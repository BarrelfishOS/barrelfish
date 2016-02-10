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
 * VERSION	$Id: database.h,v 1.2 2009/02/27 21:01:04 kish_shen Exp $
 */

/*
 * IDENTIFICATION		database.h
 *
 * DESCRIPTION	
 *	
 *
 * CONTENTS:
 *
 *
 */

/* Header of the code blocks */

/* size of the prefix in (vmcode) units */
#define PROC_PREFIX_SIZE		7
#define ProcHeader(codeptr)		((vmcode *) codeptr - PROC_PREFIX_SIZE)
#define ProcBrkTableOffset(codeptr)	(*((word *) (codeptr) - 6))
#define ProcCodeSize(codeptr)		(*((word *) (codeptr) - 5))
#define ProcStruct(codeptr)		((pword *)((vmcode *) (codeptr) - 4))
#define ProcLink(codeptr)		(*ProcHeader(codeptr))
#define ProcBid(codeptr)		(*(vmcode *)ProcStruct(codeptr))
#define ProcFid(codeptr)		(*((dident *) (codeptr) - 3))
#define ProcLid(codeptr)		(*((vmcode *) (codeptr) - 2))
#define ProcCid(codeptr)		(*((vmcode *) (codeptr) - 1) & 0xffffff)
#define CodeStart(headerptr)		((vmcode *) (headerptr) + PROC_PREFIX_SIZE)
/* BlockType should be int for use in switch labels */
#define BlockType(headerptr)		(int)ProcLid(CodeStart(headerptr)) 

#define CodeArity(codeptr)		(*((vmcode *) (codeptr) - 1) >> 24)
#define Incr_Code_Arity(codeptr)	*((vmcode *) (codeptr) - 1) += 1 << 24;
#define Cid(cid, did)			((vmcode) ((cid) & 0xffffff | DidArity(did) << 24))

extern vmcode	*allocate_code_block();
#define AllocateCodeBlock(size, link, bid, fid, type, cid)	\
	allocate_code_block(size, 0, link, bid, fid, type, cid)
/* allocate with breaktable */
#define AllocateCodeBlockBTable(size, btable, link, bid, fid, type, cid)	\
	allocate_code_block(size, btable, link, bid, fid, type, cid)

#define Make_Prefix(link, btable, size, bid, fid, lid, cid)		\
	Store_d(link); Store_d(btable);  Store_d(size); Store_d(bid); Store_d(fid); Store_d(lid);Store_d(cid);
#define Make_Procedure_Prefix(link, size, bid, fid, lid, cid, did)		\
	Make_Prefix(link, 0, size, bid, fid, lid, Cid(cid, did))
/* default for system procedures */
#define Make_Default_Prefix(did)	\
	Make_Procedure_Prefix(0, 0, (uword)-1, D_UNKNOWN, DEFAULT_LINE, (uword)-1, did)

/* The Lid field stores the type for other blocks */
#define DEFAULT_LINE			 0
#define GROUND_TERM			-1
#define HASH_TABLE		        -2
#define UNDEFINED_PROC			-3
#define DYNAMIC_PROC		        -4
#define PARALLEL_TABLE			-5

/* In the usual code, only this one should occur, when the code for a new
 * procedure is being allocated. The code pointer must be 'code'
 * since the name 'code' is obligatory for the macro usage
 */
#define Allocate_Procedure(size, bid, fid, lid, cid, did)		\
	    code = AllocateCodeBlock(size, 0, bid, fid, lid, Cid(cid, did));
#define Allocate_Default_Procedure(size, did)				\
	    Allocate_Procedure(size,  (uword)-1 , D_UNKNOWN, 0, (uword)-1, did)
#define Allocate_Default_ProcedureBTable(size, did, btable)				\
	    code = AllocateCodeBlockBTable(size, btable, 0, (uword)-1, D_UNKNOWN, 0, Cid((uword)-1, did))
	    


/*
 *	by dynamic declaration or after retract_all/retract the last clause
*/
#define IsInitialized(c)	(SameCode(*c,Failure) && SameCode(*(c+1),Undefined))

/*
 *	by compilation, a declaration (local/export/global) or after abolish
*/
#define IsUndefined(c)		SameCode(*c,Undefined)

#define DYNAMIC_INSTR_SIZE	6
#define CALL_CLOCK_LIST_SIZE	100
#define FACT_SOURCE_SIZE	10


