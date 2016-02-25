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

:- module(flatzinc_parser).

:- comment(categories, ["Interfacing"]).
:- comment(summary, "A parser for FlatZinc").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(date, "$Date: 2012/10/23 00:38:15 $").
:- comment(see_also, [library(flatzinc_syntax)]).
:- comment(desc, html("
<P>
A parser for FlatZinc, based on 'Specification of FlatZinc' (Nov 7 2007).
It reads and returns one item at a time, as an ECLiPSe structure which
closely resembles the FZ input.
</P>
<P>
For FlatZinc, it seems an item-wise parser is a good idea, since the
items can be processed one at a time by ECLiPSe to set up the model.
</P>
")).


%----------------------------------------------------------------------
% Parser - creates terms that are very similar to the source
%----------------------------------------------------------------------

:- export read_item/2.
:- comment(read_item/2, [
    summary:"Read one item from a FlatZinc input stream",
    amode:(read_item(+,-) is semidet),
    args:[
	"Stream":"ECLiPSe stream name or handle",
	"Item":"Output: an ECLiPSe term that describes the FlatZinc item"],
    fail_if:"Fails if end_of_file is reached",
    exceptions:[fz_abort:"syntax error"],
    desc:html("<P>
	Reads one FlatZinc item (up to and including the terminating
	semicolon) from Stream, and returns it as an ECLiPSe structure.
	Fails when end_of_file has been reached.
    </P>"),
    eg:"
    ?- read_item(input, Item).
    > float: x=3.14;
    Item = float : x = 3.14
    Yes (0.00s cpu)

    ?- read_item(input, Item).
    > solve minimize x;
    Item = minimize(solve, x)
    Yes (0.03s cpu)

    ?- read_item(input, Item).
    > var int: i::foo::bar;
    Item = var(int) : (i :: (foo :: bar))
    Yes (0.02s cpu)

    ?- read_item(input, Item).
    > constraint int_lt(x[2], 3);
    Item = constraint(int_lt(subscript(x, [2]), 3))
    Yes (0.00s cpu)

    ?- read_item(input, Item).
    > set of  int: s={1,2,3};
    Item = (set of int) : s = {[1, 2, 3]}
    Yes (0.05s cpu)

    ?- read_item(input, Item).
    > array [1..3] of var int: a;
    Item = (array([1 .. 3]) of var(int)) : a
    Yes (0.03s cpu)

    ?- read_item(input, Item).
    > var 1..5: i;
    Item = var(1 .. 5) : i
    Yes (0.03s cpu)

"]).

read_item(Stream, Term) :-
	tokenize_item(Stream, Tokens),	% fails on eof
	( item(Term, Tokens, []) ->
	    true
	;
	    syntax_error("Syntax error in: %w", [Tokens])
	).


% Items --------------------------------

item(VarDecl) -->
	[var], !,
	non_array_ti_expr_tail(Type),
	expect(:),
	ident_anns(IdentAnns),
	( [=] ->
	    { VarDecl = (var(Type):IdentAnns=Value) },
	    non_array_flat_expr(Value)
	;
	    { VarDecl = (var(Type):IdentAnns) }
	).
item(Type:IdentAnns=Value) -->
	non_array_ti_expr_tail(Type),
	[:], !,
	ident_anns(IdentAnns),
	expect(=),
	non_array_flat_expr(Value).
item(Decl) -->
	[array], !,
	expect_list(['[',i(1),(..)]),
	int_literal(Max),
	expect_list([']',of]),
	array_decl_tail(no_macro_expansion(array([1..Max]) of ElemType), ElemType, Decl).

item(constraint(ElemAnns)) -->
	[constraint], !,
	constraint_elem(Elem),
	( [::] ->
	    { ElemAnns = ::(Elem,Anns) },
	    annotations_tail(Anns)
	;
	    { ElemAnns = Elem }
	).

item(Solve) -->
	[solve], !,
	( [::] ->
	    { SolveAnns = ::(solve,Anns) },
	    annotations_tail(Anns)
	;
	    { SolveAnns = solve }
	),
	solve_kind(SolveAnns, Solve).

item(output(List)) -->
	[output], !,
	expect('['),
	nonempty_output_elem_list(List).

item(predicate(Pred)) -->
	[predicate], !,
	pred_decl(Pred).


array_decl_tail(Type, InstElemType, Decl) -->
	[var], !,
	{ InstElemType = var(ElemType) },
	non_array_ti_expr_tail(ElemType),
	expect(:),
	ident_anns(IdentAnns),
	( [=] ->
	    { Decl = (Type:IdentAnns = Value) },
	    array_literal(Value)
	;
	    { Decl = (Type:IdentAnns) }
	).
array_decl_tail(Type, ElemType, Type:IdentAnns=Value) -->
	non_array_ti_expr_tail(ElemType),
	[:], !,
	ident_anns(IdentAnns),
	expect(=),
	array_literal(Value).


constraint_elem(Elem) -->
	[ident(Ident),'('], !,
	nonempty_expr_list(Params),
	{
	    Elem =.. [Ident|Params],
	    % avoid clash with array subscript notation
	    ( functor(Elem, subscript, 2) ->
		syntax_error("Illegal constraint name: %w", [Elem])
	    ;
		true
	    )
	}.
constraint_elem(Elem) -->
	variable_expr(Elem).


solve_kind(SolveAnns, satisfy(SolveAnns)) -->
	[satisfy], !.
solve_kind(SolveAnns, minimize(SolveAnns,Expr)) -->
	[minimize], !,
	variable_expr(Expr).
solve_kind(SolveAnns, maximize(SolveAnns,Expr)) -->
	[maximize], !,
	variable_expr(Expr).


% Output-Item (obsolete)  --------------------------------

output_elem(show(Expr)) -->
	[show], !,
	expect('('),
	flat_expr(Expr),
	expect(')').
output_elem(show_cond(E1,E2,E3)) -->
	[show_cond], !,
	expect('('),
	flat_expr(E1), expect(','),
	flat_expr(E2), expect(','),
	flat_expr(E3),
	expect(')').
output_elem(Expr) -->
	[str(Expr)].

    nonempty_output_elem_list([E|Es]) -->
	output_elem(E),
	( [','] -> output_elem_list(Es) ; expect(']'), {Es = []} ).

    output_elem_list(Es) -->
	( [']'] -> {Es = []} ; nonempty_output_elem_list(Es) ).


% Predicate-Decl --------------------------------

pred_decl(Pred) -->
	[ident(Ident),'('],
	nonempty_pred_args(Params),
	{ Pred =.. [Ident|Params] }.

    nonempty_pred_args([E|Es]) -->
	pred_arg(E),
	( [','] -> pred_args(Es) ; expect(')'), {Es = []} ).

    pred_args(Es) -->
	( [')'] -> {Es = []} ; nonempty_pred_args(Es) ).

pred_arg(TypeIdent) -->
	pred_arg_type(Type),
	( [:] ->
	    {TypeIdent = Type:Ident},
	    [ident(Ident)]
	;
	    {TypeIdent = Type}
	).

pred_arg_type(var(Type)) -->
	[var], !,
	non_array_ti_expr_tail(Type).
pred_arg_type(Type) -->
	non_array_ti_expr_tail(Type).
pred_arg_type(no_macro_expansion(array([Range]) of VarElemType)) -->
	[array], !,
	expect('['),
	( [int] ->
	    {Range = int}
	;
	    {Range = 1..Max},
	    expect_list([i(1),(..)]), int_literal(Max)
	),
	expect_list([']',of]),
	( [var] ->
	    {VarElemType=var(ElemType)}
	;
	    {VarElemType=ElemType}
	),
	non_array_ti_expr_tail(ElemType).


% Type-Inst --------------------------------

non_array_ti_expr_tail(Type) -->
	( [set] ->
	    { Type = no_macro_expansion(of(set,ElemType)) },
	    expect('of'),
	    scalar_ti_expr_tail(ElemType)
	;
	    scalar_ti_expr_tail(Type)
	).


scalar_ti_expr_tail(bool) -->
	[bool], !.
scalar_ti_expr_tail(int) -->
	[int], !.
scalar_ti_expr_tail(float) -->
	[float], !.
scalar_ti_expr_tail({}(Ints)) -->
	['{'], !, nonempty_int_list(Ints).
scalar_ti_expr_tail(Min..Max) -->
	int_literal(Min), !, expect(..), int_literal(Max).
scalar_ti_expr_tail(Min..Max) -->
	float_literal(Min), !, expect(..), float_literal(Max).

    nonempty_int_list([E|Es]) -->
	int_literal(E),
	( [','] -> int_list(Es) ; expect('}'), {Es = []} ).

    int_list(Es) -->
	( ['}'] -> {Es = []} ; nonempty_int_list(Es) ).



% Expressions --------------------------------
% Rules have been reordered such that cuts do not cut valid alternatives
% (i.e. rules that match a prefix of another rule must come later).

flat_expr(Expr) -->
	non_array_flat_expr(Expr), !.
flat_expr(Expr) -->
	array_literal(Expr).


non_array_flat_expr(Expr) -->
	set_literal(Expr), !.
non_array_flat_expr(Expr) -->
	scalar_flat_expr(Expr).


scalar_flat_expr(Expr) -->
	bool_literal(Expr), !.
scalar_flat_expr(Expr) -->
	int_literal(Expr), !.
scalar_flat_expr(Expr) -->
	float_literal(Expr), !.
scalar_flat_expr(Expr) -->
	[str(Expr)], !.
scalar_flat_expr(Expr) -->
	array_access_expr(Expr), !.
scalar_flat_expr(Expr) -->
	[ident(Expr)].


int_flat_expr(Expr) -->
	int_literal(Expr), !.
int_flat_expr(Expr) -->
	array_access_expr(Expr), !.
int_flat_expr(Expr) -->
	[ident(Expr)].


variable_expr(Expr) -->
	array_access_expr(Expr), !.
variable_expr(Expr) -->
	[ident(Expr)].


array_access_expr(Ident[Index]) -->
	[ident(Ident),'['],
	int_index_expr(Index),
	expect(']').

int_index_expr(Expr) -->
	[ident(Expr)], !.
int_index_expr(Expr) -->
	int_literal(Expr).


bool_literal(false) --> [false], !.
bool_literal(true) --> [true].


int_literal(SignedInt) -->
	( [-] -> [i(Int)], {SignedInt is -Int}
	;	 [i(Int)], {SignedInt = Int} ).


float_literal(SignedFloat) -->
	( [-] -> [f(Float)], {SignedFloat is -Float}
	;	 [f(Float)], {SignedFloat = Float} ).


set_literal({}(List)) -->
	['{'], !,
	sfe_list(List).
set_literal(Min..Max) -->
	int_flat_expr(Min),
	[..], !,
	int_flat_expr(Max).

    nonempty_sfe_list([E|Es]) -->
	scalar_flat_expr(E),
	( [','] -> sfe_list(Es) ; expect('}'), {Es = []} ).

    sfe_list(Es) -->
	( ['}'] -> {Es = []} ; nonempty_sfe_list(Es) ).


array_literal(Array) -->
	['['], !,
	nafe_list(Array).

    nonempty_nafe_list([E|Es]) -->
	non_array_flat_expr(E),
	( [','] -> nafe_list(Es) ; expect(']'), {Es = []} ).

    nafe_list(Es) -->
	( [']'] -> {Es = []} ; nonempty_nafe_list(Es) ).



% Miscellaneous Elements --------------------------------

ident_anns(IdentAnns) -->
	[ident(Ident)],
	( [::] ->
	    { IdentAnns = ::(Ident,Anns) },
	    annotations_tail(Anns)
	;
	    { IdentAnns = Ident }
	).

    annotations_tail(Anns) -->
	annotation(Ann),
	( [::] ->
	    { Anns = ::(Ann,Anns1) },
	    annotations_tail(Anns1)
	;
	    { Anns = Ann }
	).


annotation(Ann) -->
	[ident(Ident)],
	( ['('] ->
	    nonempty_expr_list(Params),
	    {Ann =.. [Ident|Params]}
	;
	    {Ann = Ident}
	).

    nonempty_expr_list([E|Es]) -->
	flat_expr(E),
	( [','] -> expr_list(Es) ; expect(')'), {Es = []} ).

    expr_list(Es) -->
	( [')'] -> {Es = []} ; nonempty_expr_list(Es) ).



% Auxiliaries ------------------------------

expect_list([]) -->
	[].
expect_list([Token|Tokens]) -->
	expect(Token),
	expect_list(Tokens).

expect(Expected) -->
	[Token],
	( {Token == Expected} ->
	    []
	;
	    syntax_error("Unexpected Token, expected %w, found %w",
	    	[Expected, Token])
	).


syntax_error(Message, Culprit) -->
	{ syntax_error(Message, Culprit) }.

syntax_error(Message, Culprit) :-
	write(error, "Syntax error: "),
	printf(error, Message, Culprit),
	nl(error),
	exit_block(fz_abort).


%----------------------------------------------------------------------
% Tokenizer
% Tokenize the input stream up until the next semicolon (or eof).
% Return a list of tokens, not including the terminating semicolon.
% On end-of-file, fail.
%----------------------------------------------------------------------

tokenize_item(Stream, Tokens) :-
	tokenize_item(Stream, Tokens, []).

tokenize_item(Stream, Tokens, Tokens0) :-
	read_token(Stream, EclToken, Class)@flatzinc_parser_syntax,
	adjust_token(Class, EclToken, Token),	% fail on eof
	tokenize_item(Stream, Token, Tokens, Tokens0).

    tokenize_item(Stream, Token, Tokens, Tokens0) :-
	( Token == (;) ->
	    Tokens = Tokens0
	;
	    Tokens = [Token|Tokens1],
	    read_token(Stream, EclToken, Class)@flatzinc_parser_syntax,
	    ( adjust_token(Class, EclToken, Token1) ->
		tokenize_item(Stream, Token1, Tokens1, Tokens0)
	    ;
		unget(Stream),	% allow reading end_of_file again
		Tokens1 = Tokens0
	    )
	).


:- mode adjust_token(+,+,-).
adjust_token(anonymous, _, anonymous(_)) :- !.	% not needed in FlatZinc
adjust_token(atom, Atom, Atom) :- keyword(Atom), !.
adjust_token(atom, Atom, ident(Atom)) :- !.
adjust_token(quoted_atom, Atom, Atom) :- keyword(Atom), !.
adjust_token(quoted_atom, Atom, ident(Atom)) :- !.
adjust_token(comma, C, C) :- !.
adjust_token(integer, I, i(I)) :- !.
adjust_token(float, F, f(F)) :- !.
adjust_token(string, S, str(S)) :- !.
adjust_token(solo, S, Atom) :- !, atom_string(Atom, S).
adjust_token(open_par, S, Atom) :- !, atom_string(Atom, S).
adjust_token(end_of_file, _, _) :- !, fail.
adjust_token(_Class, EclToken, _) :-
	syntax_error("Illegal Token %w", [EclToken]).


% This differs slightly from the keyword list provided in the Flatzinc spec.
% We do not treat constraint names (like div, intersect) as keywords.
% We treat relevant punctuation as keywords, everything else is an identifier.
keyword(..).
keyword(:).
keyword(::).
keyword(;).
keyword(=).
keyword(-).
keyword(+).
keyword(annotation).
keyword(any).
keyword(array).
keyword(bool).
keyword(case).
keyword(constraint).
keyword(else).
keyword(elseif).
keyword(endif).
keyword(enum).
keyword(false).
keyword(float).
keyword(function).
keyword(if).
keyword(include).
keyword(int).
keyword(let).
keyword(list).
keyword(maximize).
keyword(minimize).
keyword(of).
keyword(output).	% obsolete
keyword(par).
keyword(predicate).
keyword(record).
keyword(satisfy).
keyword(set).
keyword(show).		% obsolete
keyword(show_cond).	% obsolete
keyword(solve).
keyword(string).
keyword(test).
keyword(then).
keyword(true).
keyword(tuple).
keyword(type).
keyword(var).
keyword(where).


%----------------------------------------------------------------------
% Auxiliary module containing syntax setting for the lexer only
%----------------------------------------------------------------------

:- module(flatzinc_parser_syntax).

:- local initialization((

    local(chtab(0'!, symbol)),
    local(chtab(0'-, solo)),    % to allow e.g. -3..-1 without spaces
    local(chtab(0'+, solo)),    % to allow e.g. 0..+1 without spaces
    local(syntax_option(iso_base_prefix)),
    local(syntax_option(not(nl_in_quotes))),

    % treat upper case letters like lower case
    (
	between(0, 255, 1, _Char),
	get_chtab(_Char, upper_case),
	local(chtab(_Char, lower_case)),
	fail
     ;
	true
    )
)).

