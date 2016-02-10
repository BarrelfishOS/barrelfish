% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% Contributor(s): Coninfer Ltd, 2013
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: format.ecl,v 1.2 2013/02/09 20:03:23 jschimpf Exp $
% Description:	Based on code factored out from quintus.pl
% ----------------------------------------------------------------------

%
% This is currently just a wrapper for printf/3 to provide the most basic
% features of Quintus-style format/3.  Comparison of capabilities:
%
%	DeFacto	ECLiPSe
%	format	printf
%	~	%
%	
%	a	a	atom
%	-	A	atom in all upper case
%	Nc	Nc	Character code
%	NE	NE	float
%	Ne	Ne	float
%	NF	NF	float
%	Nf	Nf	float
%	NG	NG	float
%	Ng	Ng	float
%	NH	-	float precise
%	Nh	-	float precise
%	-	Ad	integer (as in C)
%	-	Ao	integer in octal
%	-	Au	integer in unsigned decimal??
%	-	Ax	integer in hex a-f
%	-	AX	integer in hex A-F
%	ND	-	integer (inserting decimal point and separator commas)
%	Nd	-	integer (inserting decimal point)
%	NR	NR	radix
%	Nr	Nr	radix
%	Ns	As	code list / string
%	Ni	Ni	ignore
%	Nk	Nk	write_canonical/display
%	p	p	print
%	q	q	writeq
%	w	w	write
%	-	Cw	write/write with control chars
%	-	CW	write/write with stream settings and control chars
%	@	-	call to print
%	~	-	print ~
%	-	%	print %
%	Nn	Nn	newlines
%	N	-	newline if necessary
%	-	Nt	tabs
%	-	b	flush
%	|	-	column printing
%	+	-	column printing
%	t	-	column printing


:- module(format).

:- import printf_/8 from sepia_kernel.


:- comment(categories, ["Compatibility"]).
:- comment(summary, "Partially implements the format/2,3 predicate").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Cisco Systems, Coninfer Ltd").
:- comment(date, "$Date: 2013/02/09 20:03:23 $").

:- comment(format/2, [
    summary:"Print formatted data",
    args:["Format":"Format string (string, atom, chars or codes)",
    	"Args":"A list of terms"],
    amode:(format(++,+) is det),
    see_also:[format/3,library(quintus)]]).

:- comment(format/3, [
    summary:"Print formatted data",
    args:["Stream":"A stream handle or alias",
    	"Format":"Format string (string, atom, chars or codes)",
    	"Args":"A list of terms"],
    amode:(format(+,++,+) is det),
    see_also:[printf/3,library(quintus)],
    desc:html("<P>
    	This is a partial implementation of the format/3 predicate as
	implemented in several Prolog systems.  It simply maps the format
	string to ECLiPSe's printf/3 formats, and therefore supports only
	those formats which are compatible.
</P>")]).


:- export format/2.
:- tool(format/2, format_/3).
format_(List, ArgList, Module) :-
	format_(output, List, ArgList, Module).


:- export format/3.
:- tool(format/3, format_/4).
format_(Stream, AnyFormat, ArgList, Module) :-
	any_to_string(AnyFormat, Format),
	!,
	printf_(Stream, Format, ArgList, Module, 0'~, ErrF, ErrL, Res),
	(Res = 0 ->
	    true
	;
	    % catch the case format("~s", [ListOfChars]) and repair it
	    Res = 5,
	    substring(ErrF, "~s", 1),
	    ErrL = [Chars|More],
	    any_to_string(Chars, String)
	->
	    format_(Stream, ErrF, [String|More], Module)
	;
	    error(Res, format(Stream, ErrF, ErrL), Module)
	).
format_(Stream, AnyFormat, ArgList, Module) :-
	( ground(AnyFormat) -> E=5 ; E=4 ),
	error(E, format(Stream, AnyFormat, ArgList))@Module.


    any_to_string(X, S) :- atom(X), !,
	atom_string(X, S).
    any_to_string(X, S) :- string(X), !,
	S=X.
    any_to_string(Xs, S) :- is_list(Xs),
	( Xs=[X1|_], atom(X1) ->
	    concat_string(Xs, S)
	;
	    string_list(S, Xs)
	).

