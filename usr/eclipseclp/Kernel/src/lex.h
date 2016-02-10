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
 * Contributor(s): ECRC GmbH
 * 
 * END LICENSE BLOCK */


/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: lex.h,v 1.8 2011/04/27 05:15:50 jschimpf Exp $
 */

/*
 * IDENTIFICATION		lex.h
 *
 * DESCRIPTION		see below
 *
 */

/***************************************************************************
*
*	LEXICAL ANALIZER DEFINITIONS
*	----------------------------
*
*	All the definitions used by the lexical analyser
*	are in this file.  The parser should include this file.
*
*
*	AUTHOR:  Jorge Bocca
*	Everything changed by Pierre.
*
**************************************************************************/

/*
 * Character Classes
 */

/* unused characters */
#define 	DL	0
#define 	KI	0

/* usual atoms characters */
#define		UC	ACH-4	/* upper case alhabetic */
#define		UL	ACH-3	/* underline */
#define		LC	ACH-2	/* lower case alphabetic */
#define		N	ACH-1	/* digit */
#define 	ACH	5	/* to test the four preceeding */ 

/* ignored separators */
#define		BS	ACH	/* blank space */
#define 	NL	ACH+1	/* EOL, newline */

/* quote characters */
#define		AQ	ACH+2	/* atom quote */
#define		SQ	ACH+3	/* string quote */
#define		LQ	ACH+4	/* codes-list quote */
#define		CQ	ACH+5	/* chars-list quote */
#define		SL	ACH+6	/* solo char */
#define 	DS	ACH+7	/* special solo */
#define		CM	ACH+8	/* line comment */

#define		RA	ACH+9	/* radix quote */
#define		AS	ACH+10	/* ascii quote */
#define		TS	ACH+11	/* terminator symbol */

/* symbol characters */
#define 	SCH	ACH+12 	/* to test the five following */

#define		ES	SCH+1	/* escape char in strings, symbol */
#define 	CM1	SCH+2	/* comment external delimiter */
#define		CM2	SCH+3	/* comment internal delimiter */
#define		SY	SCH+4	/* symbol character */

#define 	NBCH	SCH+5	/* the number of types */

#define 	RE	NBCH	/* end of buffer */

#define Symbol(c)		((c) > SCH)
#define Alphanum(c)		((c) > 0 && (c) < ACH)

/* recognize an octal digit */
#define		octal(C)	(((C) >= '0') && ((C) <= '7'))


/* TOKENS */

#define LexError(token) ((token) < 0)

#define		NO_TOKEN	0
#define		BLANK_SPACE	1
#define		EOI		2
#define		EOCL		3
#define		IDENTIFIER	4
#define		QIDENTIFIER	5
#define		CODES           6
#define		COMMA		7
#define		BAR		8
#define		SOLO		9
#define		NUMBER		10
#define		STRING		11
#define		REFERENCE	12
#define		UREFERENCE	13
#define		SPACE_SOLO	14
#define		SPACE_NUMBER	15
#define		CLOSING_SOLO	16
#define		CHARS           17
#define		NBTK		18


/*
 *	USER DEFINABLE SYNTAX PARAMETERS
 */

#define NEWLINE_IN_QUOTES	0x0001	/* allow newlines in quoted objects */
#define LIMIT_ARG_PRECEDENCE	0x0002	/* limit argument precedence to 999 */
#define NO_BLANKS		0x0004	/* don't allow blanks after functor */
#define BAR_IS_NO_ATOM		0x0008	/* | is not an atom (unless quoted) */
#define BLANKS_IN_NIL		0x0010	/* allow blanks inside [] and {}    */
#define NO_ATTRIBUTES		0x0020	/* don't allow variable attributes  */
#define DOLLAR_VAR		0x0040	/* special handling of '$VAR'(N)    */
#define NESTED_COMMENTS		0x0080	/* guess what, nested comments      */
#define BASED_BIGNUMS		0x0100	/* based numbers can be bignums     */	
#define DENSE_OUTPUT		0x0200	/* avoid spaces around ops	    */
#define NO_ARRAY_SUBSCRIPTS	0x0400	/* don't allow array-like syntax    */
#define DOUBLED_QUOTE_IS_QUOTE	0x0800	/* doubled quote means quote        */
#define ISO_ESCAPES		0x1000	/* fully ISO-compliant escape seqs  */
#define ISO_BASE_PREFIX		0x2000	/* ISO-compliant number base prefix */
#define FLOATS_AS_BREALS	0x4000	/* parse floats as bounded reals    */
#define	NO_CURLY_ARGUMENTS	0x8000	/* don't allow f{...} for f with ... */
#define	BLANK_AFTER_SIGN	0x10000	/* allow blank after sign           */
#define	VAR_FUNCTOR_IS_APPLY	0x20000	/* parse X{Args} as apply(X,[Args]) */
#define	ATOM_SUBSCRIPTS		0x40000	/* allow subscripts after atoms     */
#define	GENERAL_SUBSCRIPTS	0x80000	/* allow subscripts almost anywhere */
#define	CURLY_ARGS_AS_LIST	0x100000/* parse {}(a,b,c) as {}([a,b,c])   */
#define	FLOAT_NEEDS_POINT	0x200000/* require . in float constants     */
#define BAR_IS_SEMICOLON	0x400000/* map infix |/2 to ;/2             */
#define PLUS_IS_NO_SIGN 	0x800000/* + is always functor, never sign  */
#define ISO_RESTRICTIONS	0x1000000/* other ISO restrictions	    */

#define SYNTAX_FLAGS		25	/* number of flags above	*/


/*
 * Token structure returned by the lexer
 */

typedef struct {
	int	class;		/* token class			*/

	/*
	 * Depending on the token class, we have either:
	 * - a valid tagged pword value (term)
	 * - or a char* (string) whose length is in term.val.nint
	 */
	pword	term;		/* token value			*/
	char *	string;		/* token string (if any)	*/

	source_pos_t pos;	/* source position of token	*/

} token_desc;


/*
 * Functions exported by the lexer
 */

Extern int lex_an ARGS((stream_id, syntax_desc*, token_desc *));
Extern int ec_need_quotes ARGS((dident, syntax_desc *));
Extern char *string_to_number ARGS((char *start, pword *result, stream_id nst, syntax_desc *sd));

