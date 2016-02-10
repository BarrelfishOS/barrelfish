%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling Tools for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------

:- module(flatzinc_syntax).

:-(call(op(1200, fx, (:-)))).
:-(call(op(1200, xfx, (:-)))).

:- comment(date, "$Date: 2012/10/23 00:38:15 $").
:- comment(categories, ["Interfacing"]).
:- comment(summary, "Configure ECLiPSe parser to accept FlatZinc syntax").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(see_also, [library(flatzinc_parser)]).
:- comment(desc, html("
<P>
This module provides a quick way to enable ECLiPSe to read FlatZinc items.
FlatZinc Syntax is sufficiently close to ECLiPSe syntax to allow the normal
ECLiPSe parser to read FlatZinc, provided a number of syntax options are set.
The way to use this library is to load it
<PRE>
:- lib(flatzinc_syntax).
</PRE>
and then use the normal read/1,2,etc primitives with this
module context, e.g.
<PRE>
..., read(Stream, FlatZincItem)@flatzinc_syntax, ...
</PRE>
for example
<PRE>
fzn_echo(File) :-
	open(File, read, Stream),
	read(Stream, Term1)@flatzinc_syntax,
	( fromto(Term1, Term, Term2, end_of_file), param(Stream) do
	    writeln(Term),
	    read(Stream, Term2)@flatzinc_syntax
	),
	close(Stream).
</PRE>
</P><P>
Alternatively, the library exports read_item/2, which is defined as
<PRE>
read_item(Stream, Term) :-
	read(Stream, Term)@flatzinc_syntax,
	Term \\== end_of_file.
</PRE>
and is call-compatible with the predicate of the same name
exported from lib(flatzinc_parser), but faster.  Since it
works simply by modifying syntax settings for the normal
ECLiPSe parser, it is less strict than the purpose
written library(flatzinc_parser), and will detect less
syntax errors. This should however not be an issue when
processing generated FlatZinc source.
</P>
")).


:- export read_item/2.
read_item(Stream, Term) :-
	read(Stream, Term)@flatzinc_syntax,
	Term \== end_of_file.


% All syntax settings are done as initialisation
% to avoid problems while parsing this file itself!

:- local initialization((

    (
	between(32, 255, 1, Char),
	get_chtab(Char, upper_case),
	local(chtab(Char, lower_case)),
	fail
    ;
	true
    ),

    local(chtab(0'!, symbol)),
    local(chtab(0'-, solo)),    % to allow e.g. -3..-1 without spaces
    local(chtab(0'+, solo)),    % to allow e.g. 0..+1 without spaces
    local(chtab(0';, terminator)),

    local(syntax_option(iso_base_prefix)),
    local(syntax_option(not(nl_in_quotes))),
    local(syntax_option(not(no_blanks))),
    local(syntax_option(atom_subscripts)),
    local(syntax_option(curly_args_as_list)),

    % hide all global operator definitions
    (
	current_op(_, A, Op),
	local(op(0, A, Op)),
	fail
    ;
	true
    ),

    local(op(9, xfy, (::))),

    local(op(1100, xfy, (;))),		% as in Prolog
    local(op(900, xfx, (=))),
    local(op(800, xfx, (','))),
    local(op(800, xfx, (:))),
    local(op(500, xfx, (..))),
%    local(op(600, fxy, sum)),
%    local(op(600, fxy, in)),
%    local(op(200, fxy, forall)),

    % the keywords
%    local(op(1050, xfx, annotation)),
%    local(op(500, fy, any)),
    local(op(600, fx, array)),
%    local(op(0, xfy, bool)),
%    local(op(0, xfy, case)),
    local(op(1000, fx, constraint)),
%    local(op(0, xfy, default)),
%    local(op(0, xfy, else)),
%    local(op(0, xfy, elseif)),
%    local(op(0, xfy, endif)),
%    local(op(1000, fx, enum)),
%    local(op(0, xfy, false)),
%    local(op(0, xfy, float)),
%    local(op(0, xfy, function)),
%    local(op(0, xfy, if)),
%    local(op(1000, fx, include)),
%    local(op(0, xfy, int)),
%    local(op(0, xfy, let)),
    local(op(1000, xfx, maximize)),
    local(op(1000, xfx, minimize)),
    local(op(700, xfy, of)),
    local(op(1000, fy, output)),
    local(op(600, xfy, par)),
    local(op(1000, fx, predicate)),
%    local(op(1000, xfy, record)),
    local(op(1000, xf, satisfy)),
%    local(op(0, xfy, set)),
%    local(op(0, fx, solve)),
%    local(op(0, xfy, string)),
%    local(op(0, xfy, test)),
%    local(op(0, xfy, then)),
%    local(op(0, xfy, true)),
%    local(op(0, xfy, tuple)),
%    local(op(1000, fx, type)),
    local(op(700, fy, var)),
%    local(op(1000, fx, variant_record)),
%    local(op(550, xfy, where)),

    % disable struct notation macros
    local(macro((with)/2, (=)/2, [])),
    local(macro((of)/2, (=)/2, []))

    )).

