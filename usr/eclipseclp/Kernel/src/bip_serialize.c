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
 * VERSION	$Id: bip_serialize.c,v 1.1 2013/09/28 00:25:39 jschimpf Exp $
 */

/*
 * IDENTIFICATION:	bip_serialize.c (was part of property.c)
 *
 * DESCRIPTION:		Built-ins and functions for term serialization
 *			- dbformat (term_to_bytes, bytes_to_term)
 *			- EXDR (read_exdr, write_exdr)
 *
 * CONTENTS:
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
#include "ec_io.h"
#include "module.h"
#include "emu_export.h"

extern pword	*transf_meta_out(value val, type tag, pword *top, dident mod, pword *presult),
		*transf_meta_in(pword *pw, dident mod, int *err);

extern pword	*p_meta_arity_;

static int	_fill_procedures(pword *prev_ld, dident mod, type tmod);


/*---------------------------------------------------------------------------
 *
 * Prolog term <==> Database format conversion routines
 *
 *	pword *		term_to_dbformat(pword *term)
 *
 *	pword *		dbformat_to_term(char *buffer)
 *
 * These routines are used to convert Prolog terms into the external database
 * format and vice versa.
 * The main differences of the external format compared to standard term
 * representation are:
 *
 *	- no absolute addresses, but relative offsets
 *	- no dictionary references, but explicit strings
 *	- no alignment, more compact byte representation
 *	- a breadth-first, prefix representation
 *	- machine-independent (byte order, word size)
 *
 * Format description:
 *
 * <external_term> ::	<termsize>	<simple_term>+
 *
 * <simple_term> ::
 *	TNIL
 *	TINT		<int>
 *	TSTRG		<length>	<name>
 *	TDICT		<arity>		<length>	<name>
 *	TLIST		<offset>
 *	TCOMP		<offset>
 *	TVAR_TAG	<offset>
 *	TNAME		<offset>	<length>	<name>
 *	TMETA		<offset>	<length>	<name>
 *	TUNIV		<offset>	<length>	<name>
 *	TSUSP		<offset>
 *	TDE		<flags> 
 *
 * <flags>	::	<word>
 * <float>	::	<word>
 * <termsize>	::	<word>
 * <int>	::	<compact>
 * <arity>	::	<compact>
 * <length>	::	<compact>
 * <offset>	::	<compact>
 * <tag>	::	<byte>
 *
 * <word>	::	<byte> <byte> <byte> <byte>		(MSB first)
 * <compact>	::	<1byte>* <0byte>
 * <1byte>	::	<byte>					(byte >= 0x80)
 * <0byte>	::	<byte>					(byte <  0x80)
 * <name>	::	<byte>*
 *
 * An <offset> field holds a relative address (in words). When the term is
 * restored, the start address of the restored term is added to the relative
 * address to obtain the absolute one. Note that this is not an offset into
 * the external representation!
 *
 * During conversion to external format, in the original term the MARK bit is
 * used to mark variables that have already been encountered. Their value
 * field is temporarily overwritten with the proper <offset>. These destructive
 * modifications are trailed and are undone at the end of the conversion.
 *----------------------------------------------------------------------------*/

#define QUEUE_MASK_META		0x80000000
#define QUEUE_MASK		(QUEUE_MASK_META)
#define EnQueue_(pw, arity, mark) {					\
	if (queue_head) {						\
	    queue_tail[1].val.ptr = (pword *) hg_alloc_size(2*sizeof(pword));\
	    queue_tail = queue_tail[1].val.ptr;				\
	} else								\
	    queue_tail = queue_head = (pword *) hg_alloc_size(2*sizeof(pword));\
	queue_tail[0].val.ptr = (pw);					\
	queue_tail[0].tag.kernel = (arity|(mark));			\
	queue_tail[1].val.ptr = (pword *) 0;				\
}

#define DeQueue_(pw, arity, mark) {			\
	register pword *elem = queue_head;		\
	(pw) = elem[0].val.ptr;				\
	(arity) = elem[0].tag.kernel;			\
	(mark) = (arity) & QUEUE_MASK;			\
	(arity) = (arity) & ~QUEUE_MASK;		\
	queue_head = elem[1].val.ptr;			\
	hg_free_size((generic_ptr)elem, 2*sizeof(pword));	\
}

#define EmptyQueue() (!queue_head)


#define Reserve_Space(nbytes)				\
	if ((dest + nbytes) > (char *) TG) {		\
	    TG += (dest + nbytes + 32 - (char*)TG) / sizeof(pword);	\
	    Check_Gc;					\
	}

#define Store_Byte(byte) *dest++ = (char) (byte)
#define Store_Int32(word) {\
	    register unsigned long aux = (word);		\
	    *dest++ = (char) (aux >> 24);			\
	    *dest++ = (char) (aux >> 16);			\
	    *dest++ = (char) (aux >> 8);			\
	    *dest++ = (char) (aux);				\
	}
#ifdef OLD_FORMAT
#define Store_Int(word) \
	if ((unsigned long)(word) < 0xff) *dest++ = (char) (word);	\
	else {							\
	    *dest++ = (char) 0xff;					\
	    Store_Int32(word);					\
	}
#else
#define Store_Int(w) { \
	word aux = (word) (w); \
	if (-64 <= aux && aux <= 63) { \
	    *dest++ = aux & 0x7f; \
	} else { \
	    uword rev = 0; \
	    int k = 0; \
	    do { \
		rev = (rev << 7) | (aux & 0x7f); \
		aux >>= 7; \
		++k; \
	    } while (!(-64 <= aux && aux <= 63)); \
	    *dest++ = 0x80 | (aux & 0x7f); \
	    while (--k) { \
		*dest++ = (rev & 0x7f) | 0x80; \
		rev >>= 7; \
	    } \
	    *dest++ = rev; \
	} \
}
#endif

#ifdef OLD_FORMAT
#define Store_String(length, string) {		\
	register char *source = (string);	\
	register long ctr = (length);		\
	while (ctr-- >= 0) *dest++ = *source++;	\
}
#else
#define Store_String(length, string) {		\
	register char *source = (string);	\
	register word ctr = (length);		\
	while (ctr-- > 0) *dest++ = *source++;	\
}
#endif
#define Align() while ((word) dest % sizeof(pword)) *dest++ = (char) 0;

#define LoadByte	*buf++
#define Load_Byte(n)	(n) = LoadByte
#define Load_Int32(n) {				\
	(n) = LoadByte;				\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	(n) = ((n) << 8) | ((LoadByte) & 0xff);	\
}
#define BITS_PER_WORD (8*SIZEOF_CHAR_P)
#ifdef OLD_FORMAT
#define Load_Int(n)				\
	{ if (((n) = (unsigned char)(LoadByte)) == 0xff) Load_Int32(n); }
#else
#define Load_Int(n) { /* n must be of type (signed) word */ \
	word i = LoadByte; \
	int shift = BITS_PER_WORD-7; \
	n = i & 0x7f; \
	while (i & 0x80) { \
	    i = LoadByte; \
	    n = ((n) << 7) | (i & 0x7f); \
	    shift -= 7; \
	} \
	if (shift > 0) \
	    n = (n << shift) >> shift; /* sign extend */ \
}
#endif

/* Write an EXDR Nat */
#define Store_Nat(n) 					\
	if ((n) == (word)(char)(n)) {			\
	    *dest++ = (char)((n) | 0x80);		\
	} else {					\
	    Store_Int32((n));				\
	}

/* Combined macro for Get and Load of a Nat
 * The macro is combined since it must be responsible
 * for the loading of either a single byte or a 4 byte
 * integer/
 */
#define GetLoad_Nat(n) 					\
	Get_Next(1);					\
	(n) = LoadByte;					\
	if (n & 0x80) {					\
	    n = n & 0x7f;				\
	} else {					\
	    Get_Next(3);				\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	    (n) = ((n) << 8) | ((LoadByte) & 0xff);	\
	}

#define WordOffset(pw, offset)	((pword*)((uword*)(pw) + (offset)))
#define Words(pwords)	((sizeof(pword)/sizeof(uword))*(pwords))


/* dest is assumeed to equal buf on entry
 * res is set as the result of operations performed by the macro 
 * perr is set for non-fatal errors - a valid EXDR term is written
 */
#define Write_String_Or_Ref(nst, strhm, sval)				\
    {									\
	pword id;							\
	if (strhm) {							\
	    Make_Integer(&id, strhm->nentries);				\
	    res = store_get_else_set(strhm, sval, tstrg, &id);		\
	    if (res < PSUCCEED) {					\
		*perr = res;						\
		res = PFAIL; /* Write the 'S'tring form instead */	\
	    }								\
	} else {							\
	    res = PFAIL;						\
	}								\
	if (res == PSUCCEED) {						\
	    Store_Byte('R');						\
	    Store_Nat(id.val.nint);					\
	    res = ec_outf(nst, buf, dest - buf);			\
	} else {							\
	    Store_Byte('S');						\
	    Store_Nat(StringLength(sval));				\
	    if ((res = ec_outf(nst, buf, dest - buf)) == PSUCCEED) {	\
		res = ec_outf(nst, StringStart(sval), StringLength(sval)); \
	    }								\
	}								\
    }

/*
 * pword * term_to_dbformat(term)
 *
 * Convert a general term into external format. This is created on the global
 * stack in form of a Sepia string. The return value is a pointer to this
 * string. For the reverse conversion, only the string contents is needed,
 * not its header! The sharing of variables and suspensions is preserved.
 */

pword *
term_to_dbformat(pword *parg, dident mod)
{
    pword **save_tt = TT;
    register word arity = 1, len;
    register word curr_offset = 0, top_offset = 2;	/* in 'word's */
    register pword *queue_tail = (pword *) 0;
    pword *queue_head = (pword *) 0;
    register pword *pw;
    register char *dest, *stop;
    pword *header;
    temp_area	meta_attr;
    int		flag = 0;

    Temp_Create(meta_attr, 4 * ATTR_IO_TERM_SIZE * sizeof(pword));
    header = TG;
    dest = (char *) (header + 1) + 4;	/* space for the TBUFFER pword and for
					 * the external format header	*/

    for(;;)	/* handle <arity> consecutive pwords, starting at <parg> */
    {
	do	/* handle the pword pointed to by parg */
	{
	    pw = parg;

	    /* I need here a slightly modified version of Dereference_(pw)
	     * that stops also at MARKed words. Not very nice, I know.
	     */
	    while (IsRef(pw->tag) && !(pw->tag.kernel & MARK) && !IsSelfRef(pw))
		pw = pw->val.ptr;

	    Reserve_Space(6);

	    if (pw->tag.kernel & MARK)
	    {
		if (SameTypeC(pw->tag,TDE))		/* a suspension */
		{
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int32((pw[SUSP_FLAGS].tag.kernel & ~MARK));
		    if (SuspDead(pw)) {
			curr_offset += Words(SUSP_HEADER_SIZE-1);
			parg += SUSP_HEADER_SIZE-1;
			arity -= SUSP_HEADER_SIZE-1;
		    } else {
			Store_Byte(SuspPrio(pw) + (SuspRunPrio(pw) << 4));
			curr_offset += Words(SUSP_GOAL-1);
			parg += SUSP_GOAL-1;
			arity -= SUSP_GOAL-1;
		    }
		}
		else if (pw->val.nint == curr_offset)	/* a nonstd variable */
		{
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int(pw->val.nint);
		    if (!IsNamed(pw->tag.kernel))
		    {
			Store_Byte(0);
		    }
		    else		/* store its name */
		    {
			dident vdid = TagDid(pw->tag.kernel);
			len = DidLength(vdid);
			Store_Int(len);
			Reserve_Space(len);
			Store_String(len, DidName(vdid));
		    }
		}
		else	/* just a reference to an already encountered variable */
		{
		    Store_Byte(Tag(TVAR_TAG));
		    Store_Int(pw->val.nint);
		}
	    }
	    else switch (TagType(pw->tag))
	    {
	    case TINT:
#if SIZEOF_CHAR_P > 4
		if (pw->val.nint <  WSUF(-2147483648) || WSUF(2147483648) <= pw->val.nint)
		{
		    /* store as a bignum (to be readable on 32bit machines) */
		    len = tag_desc[pw->tag.kernel].string_size(pw->val, pw->tag, 1);
		    Store_Byte(TBIG);
		    Store_Int(len);
		    Reserve_Space(len+1);
		    stop = dest+len;
		    dest += tag_desc[pw->tag.kernel].to_string(pw->val, pw->tag,
			dest, 1);
		    while (dest <= stop)	/* pad and terminate */
		    	*dest++ = 0;
		    break;
		}
#endif
		Store_Byte(TINT);
#ifdef OLD_FORMAT
		Store_Int32(pw->val.nint);
#else
		Store_Int(pw->val.nint);
#endif
		break;

	    case TNIL:
		Store_Byte(Tag(pw->tag.kernel));
		break;

	    case TDICT:
		len = DidLength(pw->val.did);
		Store_Byte(TDICT);
		Store_Int(DidArity(pw->val.did));
		Store_Int(len);
		Reserve_Space(len);
		Store_String(len, DidName(pw->val.did));
		break;

	    case TDBL:
	    {
		ieee_double d;
		d.as_dbl = Dbl(pw->val);
		Store_Byte(TDBL);
		Store_Byte(sizeof(double)-1);	/* backward compat */
		Reserve_Space(sizeof(double));
		Store_Int32(d.as_struct.mant1);
		Store_Int32(d.as_struct.mant0);
		break;
	    }

	    case TIVL:
	    {
		ieee_double dlwb, dupb;
		dlwb.as_dbl = IvlLwb(pw->val.ptr);
		dupb.as_dbl = IvlUpb(pw->val.ptr);
		Store_Byte(TIVL);
		Reserve_Space(2*sizeof(double));
		Store_Int32(dlwb.as_struct.mant1);
		Store_Int32(dlwb.as_struct.mant0);
		Store_Int32(dupb.as_struct.mant1);
		Store_Int32(dupb.as_struct.mant0);
		break;
	    }

	    case TSTRG:
		len = StringLength(pw->val);
		Store_Byte(TSTRG);
		Store_Int(len);
		Reserve_Space(len);
		Store_String(len, StringStart(pw->val));
		break;

	    case TVAR_TAG:	/* standard variable */
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(curr_offset);
		Trail_(pw);
		pw->val.nint = curr_offset;
		pw->tag.kernel |= MARK;
		break;

	    case TNAME:
	    case TUNIV:
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(top_offset);
		Trail_Tag(pw);
		pw->val.nint = top_offset;
		pw->tag.kernel |= MARK;
		top_offset += 2;
		EnQueue_(pw, 1, 0);
		break;

	    case TMETA:
		Store_Byte(Tag(TVAR_TAG));
		Store_Int(top_offset);
		Trail_Tag(pw);
		pw->val.nint = top_offset;
		pw->tag.kernel |= MARK;
		top_offset += 4;
		EnQueue_(pw, 2, QUEUE_MASK_META);
		break;

	    case TSUSP:
		Store_Byte(Tag(TSUSP));
		pw = pw->val.ptr;
		if (pw->tag.kernel & MARK)	/* not the first encounter */
		{
		    Store_Int(pw->val.nint);
		}
		else
		{
		    Store_Int(top_offset);
		    Trail_Pword(pw);
		    pw->tag.kernel |= MARK;
		    pw->val.nint = top_offset;
		    if (SuspDead(pw))
		    {
			top_offset += Words(SUSP_HEADER_SIZE);	/* for TDE */
			EnQueue_(pw, SUSP_HEADER_SIZE, 0);
		    }
		    else
		    {
			top_offset += Words(SUSP_SIZE);	/* for TDE */
			EnQueue_(pw, SUSP_SIZE, 0);
		    }
		}
		break;

	    case TLIST:
		Store_Byte(Tag(TLIST));
		Store_Int(top_offset);
		top_offset += 4;
		EnQueue_(pw->val.ptr, 2, 0);
		break;

	    case TCOMP:
		Store_Byte(Tag(TCOMP));
		Store_Int(top_offset);
		if (flag) {
		    pword pw_out;
		    (void) transf_meta_out(pw->val, pw->tag,
			    (pword *) TempAlloc(meta_attr, ATTR_IO_TERM_SIZE * sizeof(pword)),
			    D_UNKNOWN, &pw_out);
		    pw = pw_out.val.ptr;
		    len = 1 + DidArity(pw->val.did);
		    EnQueue_(pw, len, 0);
		} else {
		    len = 1 + DidArity(pw->val.ptr->val.did);
		    EnQueue_(pw->val.ptr, len, 0);
		}
		top_offset += 2*len;
		break;

	    default:
		if (TagType(pw->tag) >= 0 && TagType(pw->tag) <= NTYPES)
		{
		    len = tag_desc[TagType(pw->tag)].string_size(pw->val, pw->tag, 1);
		    Store_Byte(Tag(pw->tag.kernel));
		    Store_Int(len);
		    Reserve_Space(len+1);
		    stop = dest+len;
		    dest += tag_desc[TagType(pw->tag)].to_string(pw->val, pw->tag,
			dest, 1);
		    while (dest <= stop)	/* pad and terminate */
		    	*dest++ = 0;
		}
		else
		{
		    p_fprintf(current_err_,
			"bad type in term_to_dbformat: 0x%x\n",
			pw->tag.kernel);
		}
		break;
	    }
	    curr_offset += Words(1);
	    ++parg;
	} while (--arity);
	if (EmptyQueue())
	    break;
	DeQueue_(parg, arity, flag);
    }
					/* # bytes of external representation */
    Store_Byte(0);			/* add a terminating 0		*/
    Set_Buffer_Size(header, dest - (char*) header - sizeof(pword));
    header->tag.kernel = TBUFFER;
    Align();				/* align the global stack pointer */
    TG = (pword *) dest;
    dest = (char *) (header + 1);	/* fill in the external format header */
    Store_Int32(top_offset);		/* (size of term after restoring) */
    Untrail_Variables(save_tt);
    Temp_Destroy(meta_attr);
    return header;
}

/*
 * pword *dbformat_to_term(buf)
 *
 * Decode a term in database format (in the buffer pointed to by buf),
 * construct it on the global stack and return its address.
 * Return NULL if there is no space to construct the term.
 */

pword *
dbformat_to_term(register char *buf, dident mod, type tmod)
{
    register pword *pw;
    pword	*p;
    pword *base, *top;
    pword *prev_ld = LD;
    pword	*r;
    pword	meta;
    word	n, t;
    int		res;

    meta.tag.kernel = TNIL;
    Load_Int32(n);
    base = pw = TG;
    TG = WordOffset(TG, n);
    if (GlobalStackOverflow)
    	return (pword *)0;
    top = TG;

    while (pw < top)
    {
	Load_Byte(t);
	switch (TagTypeC(t))
	{
	case TINT:	/* value */
#ifdef OLD_FORMAT
	    Load_Int32(n);
#else
	    Load_Int(n);
#endif
	    pw->val.nint = n;
	    pw++->tag.kernel = t;
	    break;

	case TNIL:	/* */
	    pw++->tag.kernel = t;
	    break;

	case TVAR_TAG:	/* offset */
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    pw++->tag.kernel = TREF;
	    break;

	case TUNIV:	/* offset, length, "string\0" */
	case TNAME:
	case TMETA:
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    Load_Int(n);
	    if (n)
	    {
		pw++->tag.kernel = DidTag(t, enter_dict_n(buf, n, 0));
#ifdef OLD_FORMAT
		buf += n + 1;
#else
		buf += n;
#endif
	    }
	    else
		pw++->tag.kernel = RefTag(t);	/* no name */
	    if (TagTypeC(t) == TMETA) {
		p = TG;
		TG += 2;
		Check_Gc
		p[0].val.ptr = pw;
		p[0].tag.kernel = TREF;
		p[1] = meta;
		meta.val.ptr = p;
		meta.tag.kernel = TLIST;
	    }
	    break;

	case TSUSP:
	case TCOMP:
	case TLIST:
	    Load_Int(n);
	    pw->val.ptr = WordOffset(base, n);
	    pw++->tag.kernel = t;
	    break;

	case TDICT:	/* arity, length, "string\0" */
	    Load_Int(n);
	    Load_Int(t);
	    pw->val.did = enter_dict_n(buf, t, (int) n);
	    pw++->tag.kernel = TDICT;
#ifdef OLD_FORMAT
	    buf += t + 1;
#else
	    buf += t;
#endif
	    break;

	case TDBL:	/* length, double */
	    {
		ieee_double d;
		Load_Byte(n);	/* backward compatibility */
		Load_Int32(d.as_struct.mant1);
		Load_Int32(d.as_struct.mant0);
		Make_Double(pw, d.as_dbl);
		pw++;
	    }
	    break;

	case TIVL:	/* double, double */
	    {
		ieee_double dlwb, dupb;
		Load_Int32(dlwb.as_struct.mant1);
		Load_Int32(dlwb.as_struct.mant0);
		Load_Int32(dupb.as_struct.mant1);
		Load_Int32(dupb.as_struct.mant0);
		Push_Interval(pw->val.ptr, dlwb.as_dbl, dupb.as_dbl);
		pw++->tag.kernel = TIVL;
	    }
	    break;

	case TSTRG:	/* length, "string" */
	    {
		register char *string;
		Load_Int(n);
		Make_Stack_String(n, pw->val, string);
		pw++->tag.kernel = TSTRG;
#ifdef OLD_FORMAT
		while (n-- >= 0) *string++ = *buf++;
#else
		while (n-- > 0) *string++ = *buf++;
		*string = 0;
#endif
	    }
	    break;

	case TDE:
	    pw[SUSP_LD].val.ptr = LD;
	    Update_LD(pw)
	    Load_Int32(n);
	    pw[SUSP_FLAGS].tag.kernel = n;
	    pw[SUSP_PRI].val.ptr = (pword *) 0;		/* missing */
	    pw[SUSP_INVOC].tag.kernel = 0;
	    if (!SuspDead(pw)) {
		Load_Byte(n);
		Init_Susp_State(pw, n & 0xF, (n>>4) & 0xF);
		pw += SUSP_GOAL;
	    } else {
		pw += SUSP_HEADER_SIZE;
	    }
	    break;

	default:
	    if (t >= 0 && t <= NTYPES)
	    {
		Load_Int(n);
		pw->tag.kernel = t;	/* from_string() may change tag! */
		if (tag_desc[t].from_string(buf, pw, 10) != PSUCCEED)
		{
		    /* this can happen e.g. if we try to read a bignum
		     * in an Eclipse that doesn't support them */
		    Make_Nil(pw);
		    p_fprintf(current_err_,
			"dbformat_to_term: cannot represent constant of type %s\n",
			DidName(tag_desc[t].tag_name));
		}
		++pw;
		buf += n+1;
	    }
	    else
	    {
		Make_Nil(pw);
		p_fprintf(current_err_,
			"bad type in dbformat_to_term: 0x%x\n", t);
		pw++; buf++;
	    }
	    break;
	}
    }
    p = &meta;
    while (IsList(p->tag)) {
	p = p->val.ptr;
	pw = (p++)->val.ptr;
	r = transf_meta_in(pw, mod, &res);
	if (!r) {
	    p_fprintf(current_err_,
		    "unknown attribute in dbformat_to_term: ");
	    (void) ec_pwrite(0, 2, current_err_, pw->val, pw->tag, 1200, 0,
		    mod, tdict);
	    (void) ec_newline(current_err_);
	    return (pword *) 0;
	}
	pw->val.ptr = r;
    }
    res = _fill_procedures(prev_ld, mod, tmod);
    return (res == PSUCCEED) ? base : 0;
}

/*
 * Fill in pri's in the newly read suspensions
 */
static int
_fill_procedures(pword *prev_ld, dident mod, type tmod)
{
    pword	*p, *env;
    dident	pd;
    dident	module_ref;
    pri		*proc;

    for(env=LD; env > prev_ld; env = SuspPrevious(env))
    {
	if (!(SuspDead(env))) 
	{
	    proc = SuspProc(env);
	    if (!proc) {
		p = env + SUSP_GOAL;
		Dereference_(p);
		pd = p->val.ptr->val.did;
		p = env + SUSP_MODULE;
		Dereference_(p);
		module_ref = p->val.did;
		/* Create the module if it did not exist */
		if (!IsModule(module_ref))
		    (void) ec_create_module(module_ref);
		proc = visible_procedure(pd, module_ref,
		    (module_ref == mod) ? tmod : tdict, PRI_CREATE|PRI_REFER);
		if (!proc) {
		    int err;
		    Get_Bip_Error(err);
		    p_fprintf(current_err_,
			    "locked module in dbformat_to_term: %s\n",
			    DidName(module_ref));
		    return err;
		}
		env[SUSP_PRI].val.wptr = (uword *) proc;
	    }
	}
    }
    return PSUCCEED;
}

static int
p_term_to_bytes(value v, type t, value vs, type ts, value vm, type tm)
{
    pword pw, *result;
    Check_Output_String(ts);
    Check_Atom(tm);
    pw.val.all = v.all;
    pw.tag.all = t.all;
    result = term_to_dbformat(&pw, vm.did);
    Return_Unify_String(vs, ts, result);
}

static int
p_bytes_to_term(value vs, type ts, value v, type t, value vmod, type tmod)
{
    pword *result;

    Check_Atom(tmod);
    Check_String(ts);
    result = dbformat_to_term(StringStart(vs), vmod.did, tmod);
    if (!result)
    {
	value va;
	va.did = d_.abort;
	Bip_Throw(va, tdict);
    }
    Return_Unify_Pw(v, t, result->val, result->tag);
}



/*---------------------------------------------------------------------------
 * Serialisation of ground terms for communication with other languages
 *
 * ExdrTerm	::=	'V' Version 'C'? Term
 * Term		::=	(Integer|Double|String|List|Nil|Struct|Variable)
 * Integer	::=	('B' <byte> | 'I' XDR_int | 'J' XDR_long)
 * Double	::=	'D' XDR_double
 * String	::=	('S' Length <byte>* | 'R' Index)
 * List		::=	'[' Term (List|Nil)
 * Nil		::=	']'
 * Struct	::=	'F' Arity String Term*
 * Variable	::=	'_'
 * Length	::=	XDR_nat
 * Index	::=	XDR_nat
 * Arity	::=	XDR_nat
 * Version	::=	<byte>
 * XDR_int	::=	<4 bytes, msb first>
 * XDR_long	::=	<8 bytes, msb first>
 * XDR_double	::=	<8 bytes, ieee double, exponent first>
 * XDR_nat	::=	<8 bits: 1 + seven bits unsigned value>
 *			| XDR_int			// >= 0
 *
 * NOTE: Eclipse integers are wordsized (TINT) or bignums (TBIG). 
 * Values between 2^31..2^63-1 and -2^63+1..-2^31 can be TINT or TBIG,
 * depending on machine's wordsize.
 * On the other hand, EXDR 'I' format is always 32 bits and 'J' 64 bits.
 * As an additional complication, TINT and EXDR I,J are two's complement
 * representations, but TBIGs are sign/magnitude.
 * The code must therefore deal with
 *	TINT <--> I
 *	TINT <--> J
 *	TBIG (one limb) <--> J
 *	TBIG (two limbs) <--> J
 *---------------------------------------------------------------------------*/

/*
 * write_exdr/2 fails if the term cannot be represented in EXDR format.
 * The execute_rpc/1 predicate in kernel.pl relies on that.
 * Note also that we are careful to always write a complete EXDR term,
 * even when we fail. This is to avoid the recipient of the term crashing.
 */

#define EXDR_VERSION	2

#define Negate_32_32(_lo, _hi) \
	_lo = -(_lo); \
	_hi = _lo ? ~(_hi) : -(_hi);


static int
_write_exdr(stream_id nst, pword *pw, t_heap_htable *strhm, int *perr)
{
    int		arity, res;
    pword	*arg;
    value	val;
    char	buf[10];
    char	*dest;
    ieee_double	d;

    for(;;)
    {
	Dereference_(pw);
	if (IsRef(pw->tag))
	{
	    return ec_outfc(nst, '_');
	}
	switch (TagType(pw->tag))
	{
	case TDICT:			/* like atom/0 structure */
	    dest = buf;
	    Store_Byte('F');
	    Store_Nat(0);
	    val.ptr = DidString(pw->val.did);
	    Write_String_Or_Ref(nst, strhm, val);
	    return res;

	case TCOMP:
	    dest = buf;
	    arity = DidArity(pw->val.ptr->val.did); 
	    arg = pw->val.ptr;
	    Store_Byte('F');
	    Store_Nat(arity);
	    val.ptr = DidString(arg->val.did);
	    Write_String_Or_Ref(nst, strhm, val);
	    if (res != PSUCCEED) return res;
	    ++arg;
	    break;

	case TLIST:
	    for (;;)
	    {
		if ((res = ec_outfc(nst, '[')) != PSUCCEED) return res;
		pw = pw->val.ptr;		/* write car */
		if ((res = _write_exdr(nst, pw, strhm, perr)) != PSUCCEED) return res;
		++pw;
		Dereference_(pw);		/* check cdr */
		if (IsNil(pw->tag))		/* proper end */
		{
		    return ec_outfc(nst, ']');
		}
		else if (!IsList(pw->tag))	/* improper list, truncate */
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, ']');
		}
	    }

	case TNIL:
	    return ec_outfc(nst, ']');

	case TINT:
	    dest = buf;
	    if (pw->val.nint == (word)(char)pw->val.nint) /* use 'B' format */
	    {
		Store_Byte('B');
		Store_Byte(pw->val.nint);
		return ec_outf(nst, buf, 2);
	    }
#if (SIZEOF_WORD > 4)
	    if ((int32) pw->val.nint != pw->val.nint)	/* need 'J' format */
	    {
		int32 lo, hi;
		Store_Byte('J');
		lo = (int32) pw->val.nint;
		hi = (int32) (pw->val.nint >> 32);
		Store_Int32(hi);
		Store_Int32(lo);
		return ec_outf(nst, buf, 9);
	    }
#endif
	    Store_Byte('I');
	    Store_Int32(pw->val.nint);
	    return ec_outf(nst, buf, 5);

#if SIZEOF_WORD <= 4
	case TBIG:
	{
	    int32 *limbs = (int32*) BufferStart(pw->val.ptr);
	    int32 lo, hi;
	    if (BufferSize(pw->val.ptr) > 8)
	    {
		*perr = PFAIL;
		return ec_outfc(nst, '_');
	    }
	    lo = limbs[0];
	    hi = BufferSize(pw->val.ptr) > 4 ? limbs[1] : 0;
	    if (BigNegative(pw->val.ptr))
	    {
		Negate_32_32(lo, hi);
		if (hi >= 0)
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, '_');
		}
	    }
	    else
	    {
		if (hi < 0)
		{
		    *perr = PFAIL;
		    return ec_outfc(nst, '_');
		}
	    }
	    dest = buf;
	    Store_Byte('J');
	    Store_Int32(hi);
	    Store_Int32(lo);
	    return ec_outf(nst, buf, 9);
	}
#endif

	case TSTRG:
	    dest = buf;
	    Write_String_Or_Ref(nst, strhm, pw->val);
	    return res;

	case TDBL:
	    dest = buf;
	    d.as_dbl = Dbl(pw->val);
	    Store_Byte('D');
	    Store_Int32(d.as_struct.mant1);
	    Store_Int32(d.as_struct.mant0);
	    return ec_outf(nst, buf, 9);

	default:
	    *perr = PFAIL;
	    return ec_outfc(nst, '_');
	}
	for (; arity > 1; arity--,arg++)
	{
	    if ((res = _write_exdr(nst, arg, strhm, perr)) != PSUCCEED)
	    	return res;
	}
	pw = arg;		/* tail recursion optimised */
    }
}


int p_write_exdr(value vs, type ts, value v, type t)
{
    int res, err;
    pword vt;
    char buf[2];
    char *dest = buf;
    t_heap_htable *strhm = NULL;

    stream_id nst = get_stream_id(vs, ts, SWRITE, &res);
    if (nst == NO_STREAM)
    	return res;
    if (!IsWriteStream(nst))
	return STREAM_MODE;
    Store_Byte('V');
    Store_Byte(EXDR_VERSION);
    if ((res = ec_outf(nst, buf, 2)) != PSUCCEED)
    	return res;
    if (StreamMode(nst) & SCOMPRESS)
    {
	if ((res = ec_outfc(nst, 'C')) != PSUCCEED)
	    return res;
	strhm = htable_new(HTABLE_INTERNAL);
    }
    vt.val.all = v.all;
    vt.tag.all = t.all;
    err = PSUCCEED;
    res = _write_exdr(nst, &vt, strhm, &err);
    if (strhm)
	htable_free(strhm);
    if (res != PSUCCEED)
    	return res;		/* fatal error, exdr incomplete */
    if (err != PSUCCEED)
    	return err;		/* non-fatal, exdr sane but wrong */
    Succeed_;
}


#define Get_Next(n) {					\
    buf = (char *) StreamPtr(nst);			\
    if (StreamBuf(nst) + StreamCnt(nst) >= (unsigned char*) (buf + n))	\
	StreamPtr(nst) = (unsigned char*) (buf + n);	\
    else {						\
	word _l;					\
    	buf = ec_getstring(nst, n, &_l);		\
	if (_l < n) buf = 0;				\
    }							\
}

static int
_read_exdr(stream_id nst, t_heap_htable *strhm, pword *pw)
{
    word arity, len;
    char *buf;
    ieee_double d;
    pword *arg, key, valpw;
    int res;
    dident functor;

    for (;;)
    {
	Get_Next(1);
	switch(*buf)
	{
	case '_':
	    Make_Var(pw);
	    return PSUCCEED;

	case 'B':
	    Get_Next(1);
	    Load_Byte(len);
	    Make_Integer(pw, len);
	    return PSUCCEED;

	case 'I':
	    Get_Next(4);
	    Load_Int32(len);
	    Make_Integer(pw, len);
	    return PSUCCEED;

	case 'J':
	{
	    int32 hi, lo;
	    Get_Next(8);
	    Load_Int32(hi);
	    Load_Int32(lo);
#if (SIZEOF_WORD >= 8)
	    Make_Integer(pw, ((word) hi << 32) + (uint32) lo);
#else
	    arg = TG;
	    Push_Buffer(8);
	    if (hi < 0)		/* convert to sign/magnitude */
	    {
		Negate_32_32(lo, hi);
	    	arg->tag.kernel |= BIGSIGN;
	    }
	    ((int32 *) BufferStart(arg))[0] = lo;
	    if (hi)		/* need two limbs */
	    {
		((int32 *) BufferStart(arg))[1] = hi;
	    }
	    else		/* need only one limb */
	    {
		Trim_Buffer(arg, 4);
	    }
	    pw->tag.kernel = TBIG;
	    pw->val.ptr = arg;
#endif
	    return PSUCCEED;
	}

	case 'D':
	    Get_Next(8);
	    Load_Int32(d.as_struct.mant1);
	    Load_Int32(d.as_struct.mant0);
	    Make_Float(pw, d.as_dbl);
	    return PSUCCEED;

	case ']':
	    Make_Nil(pw);
	    return PSUCCEED;

	case 'R':
	    if (!strhm) return BAD_FORMAT_STRING;
	    GetLoad_Nat(len);
	    Make_Integer(&key, len);
	    res = store_get(strhm, key.val, key.tag, pw);
            if (res != PSUCCEED) return res;
	    /* What is retrieved from the store may be a string,
	     * or a dictionary entry!
	     */
	    if (!IsString(pw->tag)) {
		pw->val.ptr = DidString(pw->val.did);
		pw->tag.kernel = TSTRG;
	    }
	    return PSUCCEED;

	case 'S':
	    GetLoad_Nat(len);
	    Get_Next(len);
	    pw->tag.kernel = TSTRG;
	    pw->val.ptr = TG;
	    Push_Buffer(len+1);
	    Copy_Bytes(StringStart(pw->val), buf, len);
	    StringStart(pw->val)[len] = 0;
	    if (strhm) {
		Make_Integer(&key, strhm->nentries);
		return store_set(strhm, key.val, key.tag, pw);
	    }
	    return PSUCCEED;

	case 'F':
	    GetLoad_Nat(arity);
	    Get_Next(1);
	    if (arity < 0 ) return BAD_FORMAT_STRING;
	    Load_Byte(len);
	    if ( len == 'S') {
		GetLoad_Nat(len);
		Get_Next(len);
		functor = enter_dict_n(buf, len, arity);
		if (strhm) {
		    Make_Integer(&key, strhm->nentries);
		    Make_Atom(&valpw, functor);
		    res = store_set(strhm, key.val, key.tag, &valpw);
		    if (res != PSUCCEED) return res;
		}
	    } else if (len == 'R') {
		if (!strhm) return BAD_FORMAT_STRING;
		GetLoad_Nat(len);
		Make_Integer(&key, len);
		res = store_get(strhm, key.val, key.tag, &valpw);
		if (res != PSUCCEED) return res;
		/* What is retrieved from the store may be a string,
		 * or a dictionary entry with correct/incorrect arity.
		 */
		if (IsString(valpw.tag)) {
		    functor = enter_dict_n(StringStart(valpw.val), 
						StringLength(valpw.val), arity);
		} else if (DidArity(valpw.val.did) == arity) {
		    functor = valpw.val.did;
		} else {
		    functor = add_dict(valpw.val.did, arity);
		}
            } else return BAD_FORMAT_STRING;
	    if (arity == 0) {
		if (functor == d_.nil) {
		    Make_Nil(pw);
		} else {
		    Make_Atom(pw, functor);
		}
		return PSUCCEED;
	    }
	    arg = TG;
	    if (functor == d_.list) {
		Make_List(pw, arg);
		Push_List_Frame();
	    } else {
		Make_Struct(pw, arg);
		Push_Struct_Frame(functor);
		++arg;
	    }
	    break;

	case '[':
	    arity = 2;
	    arg = TG;
	    Make_List(pw, arg);
	    Push_List_Frame();
	    break;

	default:
	    return BAD_FORMAT_STRING;
	}
	for (; arity > 1; arity--,arg++)
	{
	    if ((res = _read_exdr(nst, strhm, arg)) != PSUCCEED)
	    	return res;
	}
	pw = arg;		/* tail recursion optimised */
    }
}


int p_read_exdr(value vs, type ts, value v, type t)
{
    char *buf;
    pword vt;
    int res;
    t_heap_htable *strhm = NULL;

    stream_id nst = get_stream_id(vs, ts, SREAD, &res);
    if (nst == NO_STREAM)
    	return res;
    if (nst == null_)
	return PEOF;
    if (!(IsReadStream(nst)))
	return STREAM_MODE;
    Get_Next(3);
    if (!buf)
    	return PEOF;
    if (*buf++ != 'V')
    	return NOT_DUMP_FILE;
    if (*buf++ > EXDR_VERSION)
    	return BAD_DUMP_VERSION;
    if (*buf == 'C')		/* is it compressed exdr format? */
    {
	strhm = htable_new(HTABLE_INTERNAL);
    }
    else
    {
	res = ec_ungetch(nst);
	if (res != PSUCCEED) return res;
    }
    res = _read_exdr(nst, strhm, &vt);
    if (strhm)
	htable_free(strhm);
    if (res != PSUCCEED) {
    	return res;
    }
    if (!(IsRef(vt.tag) && vt.val.ptr == &vt))
    {
	Return_Unify_Pw(v, t, vt.val, vt.tag);
    }
    Succeed_
}


/*
 * Routines to convert from to simple types and xdr format
 * used by VB interface since it VB has no bit manipulation stuff
 */
void Winapi
ec_double_xdr(double *d, char *dest)
{
	ieee_double id;

	id.as_dbl = *d;
	Store_Int32(id.as_struct.mant1);
	Store_Int32(id.as_struct.mant0);
}

void Winapi
ec_xdr_double(char *buf, double *d)
{
	ieee_double id;
	
	Load_Int32(id.as_struct.mant1);
	Load_Int32(id.as_struct.mant0);
	*d = id.as_dbl;
}
void Winapi
ec_int32_xdr(int32 *l, char *dest)
{
	Store_Int32(*l);
}

void Winapi
ec_xdr_int32(char *buf, int32 *l)
{
	Load_Int32(*l);
}


/*---------------------------------------------------------------------------
 * Init
 *---------------------------------------------------------------------------*/

void
bip_serialize_init(int flags)
{
    if (!(flags & INIT_SHARED))
	return;
    (void) built_in(in_dict("write_exdr", 2),
				p_write_exdr,	B_SAFE);
    (void) built_in(in_dict("read_exdr", 2),
				p_read_exdr,	B_UNSAFE|U_FRESH);
    (void) exported_built_in(in_dict("term_to_bytes_", 3),
				p_term_to_bytes,	B_UNSAFE|U_SIMPLE);
    (void) exported_built_in(in_dict("bytes_to_term_", 3),
				p_bytes_to_term,	B_UNSAFE|U_FRESH);
}
