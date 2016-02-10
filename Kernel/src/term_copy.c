/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: term_copy.c,v 1.1 2013/09/28 00:25:39 jschimpf Exp $
 *
 * IDENTIFICATION:	term_copy.c (was part of property.c)
 *
 * DESCRIPTION:		term copying routines
 *
 * CONTENTS:
 *			create_heapterm()
 *			get_heapterm()
 *			free_heapterm()
 *			move_heapterm()
 *			make_heapterm_persistent()
 *
 * AUTHOR:		joachim
 *
 */


#include "config.h"
#include "sepia.h"
#include "types.h"
#include "embed.h"
#include "error.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif

extern void	handle_copy_anchor(pword*,pword*,int);
extern void	mark_dids_from_pwords(pword *from, register pword *to);



/*---------------------------------------------------------
 * low level routines for copying prolog terms:
 *
 * A "root" pword must always exist. For properties it is
 * the pword in the property descriptor. If the copied term
 * is simple (or persistent), no further memory is needed.
 * Otherwise there is a single memory block connected to the
 * root pword. The root value points to the second pword of
 * the memory block, the first pword holds the data size.
 * The size is used when the term is copied back to the stack
 * to avoid the need for recursive traversal (_copy_block()).
 * CAUTION: if the root pword has the PERSISTENT bit set, it may
 * or may not point to a complete memory block with header. It
 * may be the result of copying an already persistent subterm
 * (then it probably has no header), or the result of making a proper
 * copy persistent (then the header is there, but we don't know it).
 *
 * The block which contains the actual copied term is followed
 * by a table containing pointers into the copy, marking all
 * the embedded external handles (their anchor frames, i.e.
 * the TEXTERN/TPTR pair). This table is used when freeing the
 * copy, to adjust the handle's reference counts.
 *
 *			+-------+  \
 *			|	|  |
 *			: table :   >  <num_h> words
 *			|	|  |
 *			+-------+ <
 *			|	|  |
 *			|-------|  |
 *			|	|  |
 *			: term  :   >  <size> bytes (multiple of pwords)
 * +--------+		|	|  |
 * | T...   |		|-------|  |
 * |--------|		|	|  |
 * |    -------------->	+-------+ <
 * +--------+		| num_h	|  |
 *    root		|-------|   >  1 pword header
 *			| size	|  |
 *			+-------+ <--- hg_alloc_size'd area
 * 
 * The format of heap terms must be identical to global stack terms
 * (at least for ground terms) for the following reasons:
 * - Ground heap terms can be referred to from the stacks, either as
 *   a whole or in part. They can only be freed if we know that there
 *   are no such references. The root word never has such references.
 * - Nonground heap terms must NOT be referred to from the stacks,
 *   and their variables must never be bound.  Their only use is for
 *   copying back to the global stack.  They can be freed at any time.
 * - Ground heap terms may be compared against stack terms via
 *   ec_compare_terms().
 * ---------------------------------------------------------*/

#define IsNonpersistentHeaptermRoot(root) \
 		(ISPointer((root)->tag.kernel) \
 		&& !IsSelfRef(root) \
		&& !IsPersistent((root)->tag))

#define HeaptermHeader(pw)	((pw)-1)

#define HeaptermSize(pw)	HeaptermHeader(pw)->val.nint
#define HeaptermNumHandles(pw)	HeaptermHeader(pw)->tag.kernel

#define HeaptermHandleTable(pw)	((value*)((pw) + HeaptermSize(pw)/sizeof(pword)))


/*
 * Two-pass heap copying algorithm, copying with cycles, and preserving
 * sharing of subtrees, including suspensions, buffers, variables, handles.
 * We do two depth-first traversals.
 *
 * First pass: Every node that may have multiple references is marked with
 * ALREADY_SEEN at the first encounter, and with NEED_FWD on the second.
 * We use the two GC bits for this, and don't trail these bit settings.
 *
 * Second pass: On encountering a node marked with
 *   ALREADY_SEEN
 *	copy node, reset the marker
 *   ALREADY_SEEN|NEED_FWD
 *	copy node, reset the marker, create (trailed) a forwarding pointer.
 *   FORWARDed
 *	use the forwarded pointer for the copy
 *
 * Untrail to remove the forwarding pointers.
 *
 * A nasty complication is caused by list (cons) cells: they have no header
 * which could be used for the mark bits/forwarding pointer. We have to use
 * the first (car) cell for this. Unfortunately, this could contain a simple
 * variable (TVAR_TAG+self_ref), which may need marking/forwarding itself.
 * We solve this by not marking these variables (effectively assuming they
 * are always marked ALREADY_SEEN|NEED_FWD), and using two different tags
 * for forwarding pointers, TFORWARD for variables, TFORWARD2 for lists.
 */

#define ALREADY_SEEN	MARK
#define NEED_FWD	LINK

#if 1
#define Assert(test) { \
    	if (!(test)) { \
	    p_fprintf(current_err_, "Internal error in heap copying"); \
	    ec_flush(current_err_); \
	} \
}
#else
#define Assert(test)
#endif


/* 
 * copy the given term to the pword pointed to by dest.
 * When space for structures, mutables etc is needed, it is allocated
 * using the top pointer. top is incremented and the new value is the
 * return code.
 * 
 * v, t is supposed to be dereferenced
 *
 * This routine does some (trailed) modifications of the term, so
 * don't forget to untrail after calling it!
 */

/* This macro is just an optimisation to reduce recursive calls */

#define Copy_Term_To_Heap(v, t, top, handle_slot, dest) \
	if (IsSimple(t)) {\
	    dest->val.all = v.all;\
	    dest->tag.all = t.all;\
	} else {\
	    top = _copy_term_to_heap(v, t, top, handle_slot, dest);\
	    if (!top) return top;\
	}

static pword *
_copy_term_to_heap(value v, type t, register pword *top, value **handle_slot, register pword *dest)
{
    register pword *pw, *arg_pw;
    register word arity;
    dident fdid;
    int dead;

    for(;;)			/* tail recursion loop	*/
    {
	switch(TagType(t))
	{
	case TVAR_TAG:			/* a simple variable, possibly a car */
	    arg_pw = v.ptr;
	    if (!IsTag(arg_pw->tag.kernel, TFORWARD2))
	    {
		Assert(t.kernel == arg_pw->tag.kernel);
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		Trail_(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    dest->val.ptr = dest;	/* make a new simple variable */
	    dest->tag.kernel = TREF;
	    return top;

	case TFORWARD:			/* a previously copied variable */
	    dest->val.ptr = v.ptr;
	    dest->tag.kernel = TREF;
	    return top;

	case TFORWARD2:			/* a previously copied cons cell */
	    arg_pw = v.ptr;		/* of which we need only the car */
	    Dereference_(arg_pw);	/* which could be a reference!!! */
	    *dest = *arg_pw;
	    return top;

	case TUNIV:
	case TNAME:
	    dest->val.ptr = top;
	    dest->tag.kernel = TREF;
	    arg_pw = v.ptr;
	    Assert(t.kernel == arg_pw->tag.kernel);
	    Assert(t.kernel & ALREADY_SEEN);
	    if (t.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		t.kernel = arg_pw->tag.kernel;
		Trail_Tag(arg_pw);
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		t.kernel = arg_pw->tag.kernel;
	    }
	    dest = top++;
	    dest->val.ptr = dest;
	    dest->tag.kernel = t.kernel;
	    return top;

	case TMETA:
	    dest->val.ptr = top;	/* reference to copied variable	*/
	    dest->tag.kernel = TREF;
	    arg_pw = v.ptr;
	    Assert(t.kernel == arg_pw->tag.kernel);
	    Assert(t.kernel & ALREADY_SEEN);
	    if (t.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		t.kernel = arg_pw->tag.kernel;
		Trail_Tag(arg_pw);
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		t.kernel = arg_pw->tag.kernel;
	    }
	    dest = top;
	    top += 2;
	    dest->val.ptr = dest;	/* create a self reference	*/
	    dest++->tag.kernel = t.kernel;
	    arg_pw = MetaTerm(arg_pw);	/* and copy meta information	*/
	    arity = 1;
	    break;

	case TSUSP:
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TDE));
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dead = SuspDead(arg_pw);
	    dest->val.ptr = top;
	    dest = top;
	    top += SUSP_HEADER_SIZE;
	    dest[SUSP_LD].val.ptr = (pword *) 0;
	    dest[SUSP_FLAGS].tag.all = arg_pw[SUSP_FLAGS].tag.all;
	    /* should not put the pri into the heap, but did and module */
	    dest[SUSP_PRI].val.all = arg_pw[SUSP_PRI].val.all;
	    dest[SUSP_INVOC].tag.all = 0;
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    if (dead)
		return top;
	    top += SUSP_SIZE - SUSP_HEADER_SIZE;
	    Init_Susp_State(dest, SuspPrio(arg_pw), SuspRunPrio(arg_pw));
	    dest += SUSP_GOAL;		/* copy goal and module */
	    arg_pw += SUSP_GOAL;
	    arity = SUSP_SIZE - SUSP_GOAL;
	    break;

	case THANDLE:
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TEXTERN));
	    if (!ExternalClass(arg_pw)->copy)
		goto _copy_heap_error_;
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dest->val.ptr = top;		/* make the copy */
	    *(*handle_slot)++ = dest->val; 	/* Enter into handle table */
	    handle_copy_anchor(arg_pw, top, 0);
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    return top + HANDLE_ANCHOR_SIZE;

	case TINT:
	case TNIL:
	case TDICT:
	case TPTR:
	case TPROC:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
_copy_simple_:
	    dest->val.all = v.all;	/* the simple types	*/
	    dest->tag.all = t.all;
	    return top;

	case TIVL:
	case TSTRG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    dest->tag = t;
	    t.kernel = arg_pw->tag.kernel;
	    if (IsTag(t.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		return top;
	    }
	    Assert(SameTypeC(t, TBUFFER));
	    Assert(t.kernel & ALREADY_SEEN);
	    arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
	    dest->val.ptr = top;		/* make the copy */
	    arity = BufferPwords(arg_pw);
	    Set_Buffer_Size(top, BufferSize(arg_pw));
	    top++->tag.kernel = TBUFFER;
	    pw = arg_pw + 1;
	    do				/* copy arity/sizeof(pword) pwords */
		*top++ = *pw++;
	    while(--arity > 1);
	    if (t.kernel & NEED_FWD)
	    {
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest->val.ptr;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    return top;

	case TLIST:
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    if (!(arg_pw->tag.kernel & ALREADY_SEEN))
	    {
		if (IsTag(arg_pw->tag.kernel,TFORWARD2))
		{
		    dest->val = arg_pw->val;	/* cons cell already copied */
		    dest->tag.kernel = TLIST;
		    return top;
		}
		else if (IsTag(arg_pw->tag.kernel,TFORWARD))
		{
		    /* the car was already copied and forwarded */
		    dest->val.ptr = top;	/* allocate the cons cell copy */
		    dest->tag.kernel = TLIST;
		    dest = top;
		    top += 2;

		    dest->val.ptr = arg_pw->val.ptr; /* use forwarded car */
		    dest->tag.kernel = TREF;

		    arg_pw->val.ptr = dest;	/* update forwarding pointer, no need to trail */
		    arg_pw->tag.kernel = Tag(TFORWARD2);

		    ++dest;			/* go and copy the cdr */
		    ++arg_pw;
		    arity = 1;
		}
		else
		{
		    p_fprintf(current_err_, "INTERNAL ERROR in copy_term_to_heap()\n");
		    goto _copy_heap_error_;
		}
	    }
	    else if (arg_pw->tag.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);

		pw = arg_pw;		/* get and remember the car pword */
		Dereference_(pw);
		v.all = pw->val.all;
		t.all = pw->tag.all;

		Trail_Pword(arg_pw);	/* install forwarding pointer */
		arg_pw->val.ptr = top;
		arg_pw->tag.kernel = Tag(TFORWARD2);

		dest->val.ptr = top;	/* allocate the cons cell copy */
		dest->tag.kernel = TLIST;

		dest = top;		/* copy car (already overwritten) */
		top += 2;
		Copy_Term_To_Heap(v, t, top, handle_slot, dest);

		++dest;			/* go copy the cdr */
		++arg_pw;
		arity = 1;
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
		dest->val.ptr = top;	/* allocate the cons cell copy */
		dest->tag.kernel = TLIST;
		dest = top;		/* go copy car + cdr */
		top += 2;
		arity = 2;
	    }
	    break;

	case TCOMP:
	    if (IsPersistent(t))
	    {
		Assert(!(TG_ORIG <= v.ptr && v.ptr < TG));
		goto _copy_simple_;
	    }
	    arg_pw = v.ptr;
	    if (IsTag(arg_pw->tag.kernel,TFORWARD))
	    {
		dest->val = arg_pw->val;	/* was already copied		*/
		dest->tag.kernel = TCOMP;
		return top;
	    }
	    dest->val.ptr = top;		/* begin the copy */
	    dest->tag.kernel = TCOMP;
	    fdid = arg_pw->val.did;
	    arity = DidArity(fdid);
	    dest = top;
	    top += arity +1;
	    dest->val.did = fdid;
	    dest->tag.kernel = TDICT;
	    Assert(arg_pw->tag.kernel & ALREADY_SEEN);
	    if (arg_pw->tag.kernel & NEED_FWD)
	    {
		arg_pw->tag.kernel &= ~(ALREADY_SEEN|NEED_FWD);
		Trail_Pword(arg_pw);		/* install forwarding pointer */
		arg_pw->val.ptr = dest;
		arg_pw->tag.kernel = Tag(TFORWARD);
	    }
	    else
	    {
		arg_pw->tag.kernel &= ~ALREADY_SEEN;
	    }
	    ++dest;
	    ++arg_pw;
	    break;	/* (arg_pw,arity,top,dest) */

/* EXTENSION SLOT HERE */

	default:
	    if (TagType(t) >= 0 && TagType(t) <= NTYPES)
	    {
		top = tag_desc[TagType(t)].copy_to_heap(v, t, top, dest);
		return top;
	    }
_copy_heap_error_:
	    return NULL;
	}

	for(;;)		/* copy <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (--arity == 0)
		break;
	    Copy_Term_To_Heap(pw->val, pw->tag, top, handle_slot, dest);
	    dest += 1;
	}
	v.all = pw->val.all;
	t.all = pw->tag.all;
    }
}


/*
 * Return amount of memory needed to make a heap copy of the given pword.
 * The pword itself is not counted, so simple types yield size 0.
 * The size returned is in bytes.
 * When an unknown type is encountered, the error flag *perr is set
 * and we return immediately.
 *
 * This routine leaves behind ALREADY_SEEN/NEED_FWD bits in some tags.
 * These bit settings are not trailed, therefore they must be undone
 * carefully in _copy_term_to_heap(), even in the case of an error!
 */

static word
_copy_size(value v, type t, word size, word *num_handles, int *perr)
{
    register pword *pw, *arg_pw;
    register word arity;

    for(;;)			/* tail recursion loop	*/
    {
	switch(TagType(t))
	{
	case TVAR_TAG:			/* simple variable (self ref) */
	    /* Why is this not treated like the other variables, i.e. using
	     * the ALREADY_SEEN/NEED_FWD bits? Because this variable could be
	     * the car of a cons cell, and there is a conflict between using
	     * the bits for marking the variable and marking the list cell.
	     * We need the bits for marking list cells, so we don't mark simple
	     * variables and always assume ALREADY_SEEN|NEED_FWD for them.
	     */
	    return size;

	case TUNIV:
	case TNAME:
	    if (t.kernel & ALREADY_SEEN)
	    {
		v.ptr->tag.kernel |= NEED_FWD;
		return size;
	    }
	    v.ptr->tag.kernel |= ALREADY_SEEN;
	    return size + sizeof(pword);

	case TMETA:
	    if (t.kernel & ALREADY_SEEN)
	    {
		v.ptr->tag.kernel |= NEED_FWD;
		return size;
	    }
	    v.ptr->tag.kernel |= ALREADY_SEEN;
	    size += 2 * sizeof(pword);
	    arg_pw = MetaTerm(v.ptr);
	    arity = 1;
	    break;

	case TSUSP:
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TDE));
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    if (SuspDead(arg_pw))
		return size + ((word) SUSP_HEADER_SIZE * sizeof(pword));
	    size += (word) SUSP_SIZE * sizeof(pword);
	    arity = SUSP_SIZE - SUSP_GOAL;
	    arg_pw += SUSP_GOAL;
	    break;

	case THANDLE:
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TEXTERN));
	    if (!ExternalClass(arg_pw)->copy)
		goto _copy_size_error_;
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    (*num_handles)++;
	    return size + HANDLE_ANCHOR_SIZE * sizeof(pword);

	case TINT:
	case TNIL:
	case TDICT:
	case TPTR:
	case TPROC:
#ifdef UNBOXED_DOUBLES
	case TDBL:
#endif
	    return size;

	case TIVL:
	case TSTRG:
#ifndef UNBOXED_DOUBLES
	case TDBL:
#endif
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    Assert(SameTypeC(arg_pw->tag, TBUFFER));
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    return size + BufferPwords(arg_pw) * sizeof(pword);

	case TLIST:
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    arity = 2;
	    size += 2 * sizeof(pword);
	    break;

	case TCOMP:
	    if (IsPersistent(t))
		return size;
	    arg_pw = v.ptr;
	    if (arg_pw->tag.kernel & ALREADY_SEEN)
	    {
		arg_pw->tag.kernel |= NEED_FWD;
		return size;
	    }
	    arg_pw->tag.kernel |= ALREADY_SEEN;
	    arity = DidArity(arg_pw->val.did);
	    ++arg_pw;
	    size += (arity+1)*sizeof(pword);
	    break;

/* EXTENSION SLOT HERE: compute size from v, t */

	default:
	    if (TagType(t) >= 0 && TagType(t) <= NTYPES)
		return size + tag_desc[TagType(t)].copy_size(v, t);

_copy_size_error_:
	    *perr = 1;
	    return size;
	}

	for(;;)		/* count <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (--arity == 0)
		break;
	    if (!IsSimple(pw->tag) && !IsVar(pw->tag))
	    {
		size = _copy_size(pw->val, pw->tag, size, num_handles, perr);
		if (*perr)
		    return size;
	    }
	}
	v.all = pw->val.all;
	t.all = pw->tag.all;
    }
}


/*
 * copy a consecutive heap block of <size> bytes beginning
 * at <from> to destination <to>, relocating all the pointers
 */

static void
_copy_block(register pword *from, register pword *to, word size)
{
    word offset = (char *) to - (char *) from;
    pword *start = from;
    pword *end = from + size/sizeof(pword);
    register word i;

    while(from < end)
    {
	if (ISPointer(from->tag.kernel))
	{
	    /* relocate pointers when within the copied block */
	    if (start <= from->val.ptr  &&  from->val.ptr < end) {
		to->val.str = from->val.str + offset;
		to->tag.kernel = IsRef(from->tag) ? from->tag.kernel
					: Tag(from->tag.kernel);
	    } else {
		*to = *from;
	    }
	    to++; from++;
	}
	else if (!ISSpecial(from->tag.kernel))
	    *to++ = *from++;
	else
	    switch (TagType(from->tag))
	    {
	    case TDE:
		if (!SuspDead(from))
		{
		    to[SUSP_LD].val.ptr = LD;
		    to[SUSP_FLAGS].tag.all = from[SUSP_FLAGS].tag.all;
		    to[SUSP_PRI].val.all = from[SUSP_PRI].val.all;
		    to[SUSP_INVOC].tag.all = 0;
		    Init_Susp_State(to, SuspPrio(from), SuspRunPrio(from));
		    Update_LD(to)
		    to += SUSP_GOAL;
		    from += SUSP_GOAL;
		}
		else
		{
		    to[SUSP_LD].val.ptr = (pword *) 0;
		    to[SUSP_FLAGS].tag.all = from[SUSP_FLAGS].tag.all;
		    to[SUSP_PRI].val.all = from[SUSP_PRI].val.all;
		    to[SUSP_INVOC].tag.all = 0;
		    to += SUSP_HEADER_SIZE;
		    from += SUSP_HEADER_SIZE;
		}
		break;

	    case TEXTERN:
	    {
		handle_copy_anchor(from, to, 1);
		to += HANDLE_ANCHOR_SIZE;
		from += HANDLE_ANCHOR_SIZE;
		break;
	    }

	    case TBUFFER:
		i = BufferPwords(from);
		do
		    *to++ = *from++;
		while (--i > 0);
		break;

/* EXTENSION SLOT HERE */

	    default:
		p_fprintf(current_err_,
			"INTERNAL ERROR: illegal tag (%d) in _copy_block()\n",
			from->tag.kernel);
		break;
	    }
    }
}


/*
 * Make a heapterm persistent (i.e. it will never be freed again)
 *
 * - set PERSISTENT bits in all its internal (non-variable) pointer tags
 * - mark any DIDs within the term as DICT_PERMANENT
 *   (so dictionary gc does not need to mark persistent terms)
 *
 * Heapterms can only be made persistent if:
 *	- they are fully ground
 *	- do not contain handles
 */

void
make_heapterm_persistent(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	pword *pw = root->val.ptr;
	pword *start = pw;
	pword *end = pw + HeaptermSize(pw) / sizeof(pword);

	/* CAUTION: by setting the PERSISTENT bit in root->tag we lose the
	 * information that root->val.ptr points to a complete heap-copy block
	 * (with header). It now looks identical to a persistent pointer into
	 * the middle of a persistent heap term! Hopefully we will never
	 * need this information again...  */
	root->tag.kernel |= PERSISTENT;	/* mark whole term as persistent */

	while(pw < end)
	{
	    if (ISPointer(pw->tag.kernel))
	    {
		Assert(!IsRef(pw->tag));	/* no variables */
		if (!IsPersistent(pw->tag))
		{
		    /* no pointers to non-persistent other heapterms */
		    Assert(start <= pw->val.ptr  &&  pw->val.ptr < end);
		    /* mark pointer to subterm as persistent */
		    pw->tag.kernel |= PERSISTENT;
		}
		pw++;
	    }
	    else if (IsTag(pw->tag.kernel, TBUFFER))
	    {
		pw += BufferPwords(pw);
	    }
	    else if (IsAtom(pw->tag))	/* atom or functor */
	    {
		Set_Did_Stability(pw->val.did, DICT_PERMANENT);
		pw++;
	    }
	    else if (IsString(pw->tag) && StringInDictionary(pw->val))
	    {
		dident a = check_did_n(StringStart(pw->val), StringLength(pw->val), 0);
		Assert(a != D_UNKNOWN);
		Set_Did_Stability(a, DICT_PERMANENT);
		pw++;
	    }
	    else if (IsTag(pw->tag.kernel, TPTR))
		pw++;
	    else if (IsTag(pw->tag.kernel, TEXTERN))
		pw++;
	    else
	    {
		Assert(!ISSpecial(pw->tag.kernel));
		pw++;
	    }
	}
    }
}


/*
 * Make a copy of the given term on the global stack.
 * Share ground subterm with the original.
 * Make a list of metaterm.copy pairs iff meta != 0
 */

static int
_copy_term(value v, type t, register pword *dest, register pword *meta, int marked_vars_only)
{
	register pword *pw, *arg_pw, *arg;
	register word arity;
	dident fdid;
	int	copied = 0;
	pword *save_tg = TG;

	switch(TagType(t))
	{
	case TVAR_TAG:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    dest->val.ptr = dest;
	    dest->tag.kernel = TREF;
	    Trail_(v.ptr);
	    v.ptr->val.ptr = dest;
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    return 1;

	case TFORWARD:
	    dest->val = v.ptr->val;	/* was already copied		*/
	    dest->tag.kernel = TREF;
	    return 1;

	case TUNIV:
	case TNAME:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    dest->val.ptr = TG;
	    dest->tag.kernel = TREF;
	    Trail_Tag(v.ptr);
	    v.ptr->val.ptr = TG;
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    dest = TG++;
	    Check_Gc
	    dest->val.ptr = dest;
	    dest->tag.kernel = t.kernel & ~MARK;
	    return 1;

	case TMETA:
	    if (marked_vars_only && !(t.kernel & MARK))
	    	goto _global_var_;
	    /* CAUTION: t may have MARK bit set */
	    Trail_Tag(v.ptr);			/* make forwarding pointer */
	    v.ptr->tag.kernel = Tag(TFORWARD);
	    if (meta)
	    {
		arg = TG;			/* allocate 2 list elements */
		TG += 4;
		Check_Gc

		arg[0].val.ptr = v.ptr;		/* pointer to metaterm */
		arg[0].tag.kernel = TREF;
		v.ptr->val.ptr =
		dest->val.ptr =
		arg[1].val.ptr = &arg[1];	/* free variable */
		dest->tag.kernel =
		arg[1].tag.kernel = TREF;

		arg[2].val.ptr = arg;		/* list cell holding pair */
		arg[2].tag.kernel = TLIST;
		arg[3] = *meta;
		meta->val.ptr = &arg[2];
		meta->tag.kernel = TLIST;
	    }
	    else
	    {
		v.ptr->val.ptr =
		dest->val.ptr = dest;		/* free variable */
		dest->tag.kernel = TREF;
	    }
	    return 1;

	case TSUSP:
	    dest->tag.all = t.all;
	    if (SameTypeC(v.ptr->tag, TDE))
	    {
		if (SuspDead(v.ptr))
		{
		    /* A dead suspension is ground, no need to copy it. */
		    dest->val.ptr = v.ptr;
		    return 0;
		}
		else	/* active */
		{
		    dest->val.ptr = TG;
		    arg = TG;
		    TG += SUSP_SIZE;
		    Check_Gc
		    arg[SUSP_LD].val.ptr = LD;
		    arg[SUSP_FLAGS].tag.all = v.ptr[SUSP_FLAGS].tag.all;
		    arg[SUSP_PRI].val.all = v.ptr[SUSP_PRI].val.all;
		    arg[SUSP_INVOC].tag.all = 0;
		    Init_Susp_State(arg, SuspPrio(v.ptr), SuspRunPrio(v.ptr));
		    Update_LD(arg)
		    Trail_Pword(v.ptr);
		    v.ptr->val.ptr = arg;
		    v.ptr->tag.kernel = Tag(TFORWARD);
		    arg += SUSP_GOAL;
		    arg_pw = v.ptr += SUSP_GOAL;
		    arity = SUSP_SIZE-SUSP_GOAL;
		    copied = 1;
		    /* copy remaining pwords in the suspension */
		}
	    }
	    else if (IsForward(v.ptr->tag))	/* already copied */
	    {
		dest->val.ptr = v.ptr->val.ptr;
		return 1;
	    }
	    else
	    {
		p_fprintf(current_err_,"bad type in _copy_term: 0x%x\n",t.kernel);
		return 1;
	    }
	    break;

	case TLIST:
	    dest->val.ptr = TG;
	    dest->tag.kernel = TLIST;
	    arg = TG;
	    TG += 2;
	    Check_Gc
	    arg_pw = v.ptr;
	    arity = 2;
	    break;

	case TCOMP:
	    dest->val.ptr = TG;
	    dest->tag.kernel = TCOMP;
	    arg_pw = v.ptr;
	    fdid = arg_pw++->val.did;
	    arity = DidArity(fdid);
	    arg = TG;
	    TG += arity +1;
	    Check_Gc
	    arg->val.did = fdid;
	    arg++->tag.kernel = TDICT;
	    break;

	case TEXTERN:
	case TBUFFER:
	case TPTR:
	case TDE:
	    p_fprintf(current_err_,"ECLiPSe: bad type in _copy_term: 0x%x\n",t.kernel);
	    return 1;

/* EXTENSION SLOT HERE */

_global_var_:	/* A variable that doesn't get copied */
	    dest->val.ptr = v.ptr;
	    dest->tag.kernel = TREF;
    	    return 0;

	case THANDLE:
	default:	/* simple, ground stuff */
	    dest->val.all = v.all;
	    dest->tag.all = t.all;
	    return 0;
	}

	while(arity--)	/* copy <arity> pwords beginning at <arg_pw>	*/
	{
	    pw = arg_pw++;
	    Dereference_(pw);
	    if (IsSimple(pw->tag))
		*arg = *pw;
	    else
		copied |= _copy_term(pw->val, pw->tag, arg, meta, marked_vars_only);
	    arg += 1;
	}
	if (!copied)
	{
	    dest->val.all = v.all;	/* share the original */
	    dest->tag.all = t.all;
	    TG = save_tg;		/* pop the copy */
	}
	return copied;
}


/*
 * A support function for the dictionary garbage collector
 */

void
mark_dids_from_heapterm(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	mark_dids_from_pwords(root->val.ptr,
		root->val.ptr + HeaptermSize(root->val.ptr)/sizeof(pword));
    }
    else
	mark_dids_from_pwords(root, root + 1);
}


/*---------------------------------------------------------
 * interface
 *---------------------------------------------------------*/

/*
 * free_heapterm(root,v,t) - free a heap term
 */

void
free_heapterm(pword *root)
{
    if (IsNonpersistentHeaptermRoot(root))
    {
	value *handle_slot;
	word count;

	/* free the term's embedded HANDLEs */

	/* First slot available in HANDLE table */
	handle_slot = HeaptermHandleTable(root->val.ptr);

	for( count = 0; count < HeaptermNumHandles(root->val.ptr); 
	     count++, handle_slot++ ) 
	{
            Assert(IsTag(handle_slot->ptr->tag.kernel, TEXTERN));
            if ( ExternalClass(handle_slot->ptr)->free
                && ExternalData(handle_slot->ptr))
            {
                ExternalClass(handle_slot->ptr)->free(ExternalData(handle_slot->ptr));
            }
	}	

	/* free the heap copy itself and its handle table */
	hg_free_size((generic_ptr) HeaptermHeader(root->val.ptr), 
		     HeaptermSize(root->val.ptr) + 
		     HeaptermNumHandles(root->val.ptr) * sizeof(value) + 
		     sizeof(pword));
    }
    root->tag.kernel = TEND;
}


/*
 * create_heapterm(root,v,t) - copy a prolog term to the general heap
 *
 * root points to a prolog word on the heap,
 * this is overwritten with a heap copy of v, t.
 * IMPORTANT: Stack pointers must be exported!
 */

int
create_heapterm(pword *root, value v, type t)
{
    pword **old_tt = TT;
    pword *pw = (pword*) 0, *top;
    value *handle_slot = (value *)0;
    word size, num_handles = 0;
    int err = 0;

    /* CAUTION: _copy_size() sets ALREADY_SEEN/NEED_FWD bits which are being
     * reset in _copy_term_to_heap(). We can not allow aborting in between
     * because we must not leave behind any ALREADY_SEEN/NEED_FWD bits.
     * Even if _copy_size() finds an error (err=1), we still need to call
     * _copy_term_to_heap() in order to reset all the marker bits!
     * This is ensured by wrapping into Disable_Exit()/Enable_Exit().
     */
    Disable_Exit();

    /* Find out how much space we are going to need, and allocate it */
    size = _copy_size(v, t, 0, &num_handles, &err);
    Assert(TT == old_tt);
    if (size > 0)
    {
	pw = (pword *) hg_alloc_size(size + num_handles * sizeof(value) + sizeof(pword));
	pw++;
	HeaptermSize(pw) = size;		/* the first word holds the size */
	HeaptermNumHandles(pw) = num_handles;	/* the second holds the number of HANDLEs */
        handle_slot = HeaptermHandleTable(pw);	/* First slot available in HANDLE table */
    }

    /* Now make the copy and reset the bits. The function leaves forwarding
     * pointers (trailed), which are removed by subsequent untrailing.
     * Before calling, if t is a variable's tag, we reload it from memory
     * in order to pick up any bits that were set in it by _copy_size().
     */
    top = _copy_term_to_heap(v, IsRef(t) ? v.ptr->tag : t, pw, &handle_slot, root);
    Untrail_Variables(old_tt);

    /* Mark bits are now reset, we can release the Exit-protection */
    Enable_Exit();

    /* If there was a problem, throw the incomplete copy away */
    if (err)
    {
	if (size > 0)
	{
	    hg_free_size((generic_ptr) HeaptermHeader(pw), HeaptermSize(pw) + 
				HeaptermNumHandles(pw) * sizeof(value) + sizeof(pword));
	}
	return TYPE_ERROR;
    }

    if ((int8*)top - (int8*)pw != size)
	return PERROR;

    return PSUCCEED;
}


/*
 * like create_heapterm, but takes a C string as argument
 */

void
set_string_n(pword *root, char *string, int len)		/* string\0 + length */
{
    word size = BufferSizePwords(len+1) * sizeof(pword);
    pword *pw = (pword *) hg_alloc_size(size + sizeof(pword));
    pw++;
    HeaptermSize(pw) = size;
    HeaptermNumHandles(pw) = 0;	/* no handles */
    Set_Buffer_Size(pw, len+1);
    pw->tag.kernel = TBUFFER;
    Copy_Bytes((char *)(pw + 1), string, len);
    ((char *)(pw + 1))[len] = 0;
    root->val.ptr = pw;
    root->tag.kernel = TSTRG;
}


void
set_string(pword *root, char *string)		/* NUL-terminated string */
{
    set_string_n(root, string, strlen(string));
}


/*
 * get_heapterm(root, result) - get a prolog term from the heap
 *
 * root points to the heap word representing the term.
 * The result is stored in the pword referenced by result,
 * if the term is complex, space is allocated on the global stack.
 * IMPORTANT: Stack pointers must be exported!
 */

void
get_heapterm(pword *root, pword *result)
{

    if (ISPointer(root->tag.kernel))
    {
	if (IsSelfRef(root))
	    result->val.ptr = result;		/* if free var on heap	*/
	else if (IsPersistent(root->tag))
	    result->val.all = root->val.all;
	else	/* copy back to the stack */
	{
	    pword *orig = root->val.ptr;
	    pword *dest;
	    word size = HeaptermSize(orig);

	    result->val.ptr = dest = TG;	/* push complex term	*/
	    TG += size/sizeof(pword);
	    Check_Gc;
	    _copy_block(orig, dest, size);
	}
    }
    else
    {
	result->val.all = root->val.all;
    }
    result->tag.all = root->tag.all;
}


/*
 * Move the root of a heap term
 */
void
move_heapterm(pword *root_old, pword *root_new)
{
    if (ISPointer(root_old->tag.kernel) && IsSelfRef(root_old))
	root_new->val.ptr = root_new;
    else
	root_new->val.all = root_old->val.all;
    root_new->tag.all = root_old->tag.all;
}


/*
 * copy_term(+Term, -Copy)
 * copy_term(+Term, -Copy, -MetaList)
 */

static int
p_copy_simple_term(value v, type t, value vc, type tc)
{
    pword	result;
    pword	**old_tt = TT;

    (void) _copy_term(v, t, &result, (pword *) 0, 0);
    Untrail_Variables(old_tt);
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Return_Unify_Pw(vc, tc, result.val, result.tag)
    }
    Succeed_
}


static int
p_copy_term3(value v, type t, value vc, type tc, value vl, type tl)
{
    pword	result, list;
    pword	**old_tt = TT;
    Prepare_Requests

    list.tag.kernel = TNIL;
    (void) _copy_term(v, t, &result, &list, 0);
    Untrail_Variables(old_tt);
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Request_Unify_Pw(vc, tc, result.val, result.tag)
	Return_If_Failure
    }
    Return_Unify_Pw(vl, tl, list.val, list.tag)
}


/* auxiliary function for copy_term_vars/4 */

static void
_mark_variables_trailed(value val, /* a dereferenced argument */
			type tag)
{
    register int arity;
    register pword *arg_i;

    for (;;)
    {
	if (IsRef(tag))
	{
	    if (val.ptr->tag.kernel & MARK)
	    	return;
	    if (IsVar(tag))		/* mark the variable */
		{ Trail_(val.ptr) }
	    else
		{ Trail_Tag(val.ptr) }
	    val.ptr->tag.kernel |= MARK;
	    return;
	}
	else if (IsList(tag))
	{
	    arity = 2;
	}
	else if (IsStructure(tag))
	{
	    arity = DidArity(val.ptr->val.did);
	    val.ptr++;
	}
	else
	    return;
 
	for(;arity > 1; arity--)
	{
	    arg_i = val.ptr++;
	    Dereference_(arg_i);
	    _mark_variables_trailed(arg_i->val,arg_i->tag);
	}
	arg_i = val.ptr;		/* tail recursion */
	Dereference_(arg_i);
	val.all = arg_i->val.all;
	tag.all = arg_i->tag.all;
    }
}


static int
p_copy_term_vars(value vvars, type tvars, value v, type t, value vc, type tc, value vl, type tl)
{
    pword	result, list;
    pword	**old_tt = TT;
    Prepare_Requests

    list.tag.kernel = TNIL;
    _mark_variables_trailed(vvars, tvars);
    if (TT != old_tt)
    {
	(void) _copy_term(v, t, &result, &list, 1);
	Untrail_Variables(old_tt);
    }
    else	/* nothing to do */
    {
	result.val = v;
	result.tag = t;
    }
    if (!(IsRef(result.tag) && IsSelfRef(&result)))
    {
	Request_Unify_Pw(vc, tc, result.val, result.tag)
	Return_If_Failure
    }
    Return_Unify_Pw(vl, tl, list.val, list.tag)
}


static int
p_term_size(value v, type t, value vs, type ts)
{
    word size;
    pword root;

    Check_Output_Integer(ts);
    create_heapterm(&root, v, t);
    if (ISPointer(root.tag.kernel))
    {
	if (IsSelfRef(&root))
	    size = 0;
	else if (IsPersistent(root.tag))
	    size = 0;
	else
	    size = HeaptermSize(root.val.ptr);
    }
    else
    {
	size = 0;
    }
    free_heapterm(&root);
    Return_Unify_Integer(vs, ts, size);

}


/*---------------------------------------------------------------------------
 * Init
 *---------------------------------------------------------------------------*/

void
bip_copy_init(int flags)
{
    if (!(flags & INIT_SHARED))
	return;
    (void) exported_built_in(in_dict("term_size", 2),
				p_term_size,		B_UNSAFE|U_SIMPLE);
    exported_built_in(in_dict("copy_term_vars", 4),
				p_copy_term_vars,	B_UNSAFE);
    exported_built_in(in_dict("copy_simple_term", 2),
				p_copy_simple_term,	B_UNSAFE|U_FRESH)
		->mode  = BoundArg(2, NONVAR);	/* it *could* be a nonvar */
    exported_built_in(in_dict("copy_term", 3),
				p_copy_term3,		B_UNSAFE|U_UNIFY)
		->mode  = BoundArg(2, NONVAR)	/* it *could* be a nonvar */
			| BoundArg(3, NONVAR);
}
