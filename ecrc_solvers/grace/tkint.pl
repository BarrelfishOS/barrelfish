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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH.
% 
% END LICENSE BLOCK

%
% Predicates for pasing values to Tk and displaying.
%

:- begin_module(grace).
:- call(lib(fd)).

el_to_const(I, S, 1) :-
    atomic(I),
    !,
    (I = dummy_var ->
	S = ""
    ;
	S = I
    ).
el_to_const(V, S, Size) :-
    true,
    meta(V),
    dvar_dest(V, D, Size),
    part_dom_to_const(D, S).

:- mode int_to_const(+, -).
int_to_const(I, I) :-
    integer(I).
int_to_const(A..B, C) :-
    concat_string([A, .., B], C).

:- mode int_to_const(+, ?, ?).
int_to_const(I) -->
    {integer(I)},
    [I].
int_to_const(A..B) -->
    {concat_string([A, .., B], S)},
    [S].

:- mode part_dom_to_const(++, -).
part_dom_to_const(N, N) :-
    integer(N).
part_dom_to_const(D, S) :-
    compound(D),
    term_string(D, SL),
    string_length(SL, L),
    L2 is L - 2,
    substring(SL, 2, L2, S).		% remove the list brackets

const_to_el("", _) :- !.
const_to_el(I, [I]) :-
    integer(I),
    !.
const_to_el(String, List) :-
    term_string(T, String),
    comma_to_list(T, List).

comma_to_list((A,B), [A|L]) :-
    !,
    comma_to_list(B, L).
comma_to_list(A, [A]).

make_tcl_list(From, HowMany, String) :-
    make_label_list(From, HowMany, List),
    concat_string([' {'|List], String).

delay lazy_list(_, L) if var(L).
lazy_list(_, []).
lazy_list(N, [N|L]) :-
    N1 is N + 1,
    lazy_list(N1, L).

make_label_list(I, 1, [I, '} ']) :- !.
make_label_list(I, N, [I, ' '|L]) :-
    I1 is I + 1,
    N1 is N - 1,
    make_label_list(I1, N1, L).

dvar_domlist(Var, String) :-
    Var :: DomList,
    intervals_to_list(DomList, IList),
    tcl_string(IList, String).

intervals_to_list([], []).
intervals_to_list([A..B|L], [[A,B]|L1]) :-
    !,
    intervals_to_list(L, L1).
intervals_to_list([A|L], [A|L1]) :-
    intervals_to_list(L, L1).

list_to_tcl(List, String) :-
    add_spaces(List, SpList),
    concat_string([' {'|SpList], String).

add_spaces([], ['} ']).
add_spaces([H|T], [H, ' '|L]) :-
    add_spaces(T, L).

goals_to_tcl(Goals, TclList) :-
    open(_, string, S),
    printf(S, "{", []),
    goals_to_strings(S, Goals),
    get_stream_info(S, name, TclList),
    close(S).

goals_to_strings(S, []) :-
    printf(S, "}", []).
goals_to_strings(S, [Goal|L]) :-
    our_goal(Goal),
    !,
    goals_to_strings(S, L).
goals_to_strings(S, [Goal|L]) :-
    printf(S, " \"%GDmw\"", [Goal]),
    goals_to_strings(S, L).

goal_to_string(Goal, String) :-
    open(_, string, S),
    printf(S, "%GDmw", [Goal]),
    get_stream_info(S, name, String),
    close(S).

dvar_dest(Var, D, Size) :-
    dvar_domain(Var, dom(D, Size)).

% convert a domain to list of interval pairs
var_domain_list(Var, IList) :-
    dvar_dest(Var, D, _),
    interval_list(D, IList).

interval_list([], ['}']).
interval_list([A..B|L], [S|IL]) :-
    !,
    concat_string(['{', A, ' ', B, '} '], S),
    interval_list(L, IL).
interval_list([I|L], [S|IL]) :-
    (integer(I) -> 
        concat_string([I, ' '], S)
     ; S = ' '
    ),
    interval_list(L, IL).

%
% Make a difference of two domain variables in a form which
% can be passed to Tk for colored display.
%
domain_difference(Var, EV, List, Link) :-
    dvar_domain(Var, DO),
    dvar_domain(EV, DN),
    (dom_difference(DO, DN, DomDif, _) ->
	sorted_list_to_dom(NewL, DN),
	sorted_list_to_dom(DifL, DomDif),
	merge_difs(NewL, DifL, List, Link)
    ;
	% they are equal
	List = [Const, '" "'|Link],
	el_to_const(Var, Const1, _),
	(substring(Const1, " ", _) ->
	    remove_spaces(Const1, Const)
	;
	    Const = Const1
	)
    ).

merge_difs(New, Dif, List, Link) :-
    merge_difs(New, Dif, [""|NL], NL, List, Link).

merge_difs([], [], N, []) -->
    !,
    empty_new(N).
merge_difs([New|LN], [], N, NL) -->
    !,
    {int_to_const(New, C)},
    ({LN = []} ->
	{NL = [C|NL1]}
    ;
	{NL = [C, ','|NL1]}
    ),
    merge_difs(LN, [], N, NL1).
merge_difs([], [Dif|LD], N, []) -->
    !,
    empty_new(N),
    int_to_const(Dif),
    ({LD = []} ->
	{true}
    ;
	merge_difs([], LD, [','|NL1], NL1)
    ).
merge_difs([New|LN], [Dif|LD], N, NL) -->
    ({before(New, Dif)} ->
	{int_to_const(New, C),
	NL = [C, ','|NL1]},
	merge_difs(LN, [Dif|LD], N, NL1)
    ;
	{NL = []},
	empty_new(N),
	int_to_const(Dif),
	merge_difs([New|LN], LD, [','|NL1], NL1)
    ).

empty_new([]) -->
    {true}.
empty_new([""]) -->
    !,
    ['{} '].
empty_new(N) -->
    {concat_string(N, S)},
    [S].

before(New, Dif) :-
    integer(New),
    (Dif = A.._ ->
	New < A
    ;
	New < Dif
    ).
before(_..B, Dif) :-
    (Dif = A.._ ->
	B < A
    ;
	B < Dif
    ).

%
% Convert a goal to a Tcl list in the node format
%
goal_to_node(Index, String) :-
    goal_format(Index, Goal, RealExit),
    gfunctor(Goal, F, A),
    (gfunctor(RealExit, F, A) ->
	Exit = RealExit
    ;
	printf("goal %d: %w has exit %w\n%b", [Index, Goal, RealExit]),
	Exit = Goal
    ),
    decompose_expr(Goal, List1, Vars),
    sumnodes(vars, Exit, EV, []),
    (length(Vars) =:= length(EV) ->
	EVars = EV
    ;
	% Some variables have been instantiated; we have to
	% find the corresponding terms by simultaneous lookup
    vars_from(Goal, Exit, EVars, []) ->
	true
    ;
	printf("cannot match %mw and %mw\n%b", [Goal, Exit]),
	EVars = EV
    ),
    %printf("difference %mw -> %mw\n%b", [Vars, EVars]),
    start_with_nonvar(List1, List2),
    goal_domains(List2, EVars, List3),
    list_to_tcl(List3, String).

goal_modified(Index) :-
    goal_format(Index, Goal, RealExit),
    Goal \== RealExit,
    not variant(Goal, RealExit).

vars_from(T1, T2) -->
    {var(T1),
    !},
    [T2].
vars_from(T1, T2) -->
    {T1 = [_|_],
    T2 = [_|_],
    !},
    vars_from_list(T1, T2).
vars_from(T1, T2) -->
    {T1 =.. [F|L1],
    T2 =.. [F|L2] ->
	true
    ;
    T2 = [] ->		% accept a truncated linear term
	true
    },
    vars_from_list(L1, L2).

vars_from_list([], []) --> {true}.
vars_from_list([K*V1|L1], [K*V2|L2]) -->
    ({var(V1), !},
	[V2]
    ;
    	{true}
    ),
    vars_from_list(L1, L2).
vars_from_list([_*V1|L1], L2) -->
    {nonvar(V1), !},
    vars_from_list(L1, L2).
vars_from_list([H|L1], [K|L2]) -->
    vars_from(H, K),
    vars_from_list(L1, L2).


% extended functor that matches variables
gfunctor(Var, _, 0) :-
    var(Var).
gfunctor(Term, F, A) :-
    nonvar(Term),
    functor(Term, F, A).

start_with_nonvar([H|T], L) :-
    (var(H) ->
	L = [{}, H|T]
    ;
    H = "" ->
	start_with_nonvar(T, L)
    ;
	L = [H|T]
    ).

goal_domains([], [], []).
goal_domains([Var|L], EVars, [' {', Id, ' {'| C]) :-
    var(Var),
    !,
    var_id(Var, Id),
    (EVars = [EV|EL],
    (var_id(EV, Id); atomic(EV)) ->
	domain_difference(Var, EV, C, ['}} '|T]),
	goal_domains(L, EL, T)
    ;
	printf("Cannot make difference of %Dmw with vars %Dmw, list %Dmw\n%b",
		[Var, EVars, L]),
	el_to_const(Var, S, _),
	C = [S, '}} '|T],
	goal_domains(L, EVars, T)
    ).
goal_domains([C|L], EV, [C|T]) :-
    goal_domains(L, EV, T).

% Take a Prolog term and return a list of strings and vars which,
%  when concatenated, represent the printed form of the term.
decompose_expr(Expr, List, Vars) :-
    sumnodes(vars, Expr, Vars, []),
    open(_, string, Stream),		% simulate term_string without attribs.
    printf(Stream, "%GDw", [Expr]),
    %printf("%GDw\n%b", [Expr]),
    get_stream_info(Stream, name, String),
    close(Stream),
    open(String, string, S),
    read_token(S, Tok, Class),
    scan_expression(S, Tok, Class, Vars, List, CL, CL).

scan_expression(S, _, end_of_file, [], [String], CList, []) :-
    !,
    concat_string(CList, String),
    close(S).
scan_expression(S, _, var, [Var|Vars], [String, Var|List], CList, []) :-
    !,
    concat_string(CList, String),
    read_token(S, Tok, Class),
    scan_expression(S, Tok, Class, Vars, List, CL, CL).
scan_expression(S, _, comma, Vars, List, CList, [','|CL]) :-
    !,
    read_token(S, NewTok, NewClass),
    scan_expression(S, NewTok, NewClass, Vars, List, CList, CL).
scan_expression(S, Tok, atom, Vars, List, CList, [RepTok|CL]) :-
    % We must escape backslashes
    replace_token(Tok, RepTok),
    !,
    read_token(S, NewTok, NewClass),
    scan_expression(S, NewTok, NewClass, Vars, List, CList, CL).
scan_expression(S, Tok, _, Vars, List, CList, [Tok|CL]) :-
    read_token(S, NewTok, NewClass),
    scan_expression(S, NewTok, NewClass, Vars, List, CList, CL).

vars(X) -->
    {var(X)} -> [X];[].

:- mode replace_token(+, -).
replace_token(#\=, "#\\\\=") :- !.
replace_token(#\+, "#\\\\+") :- !.
replace_token(#\/, "#\\\\/") :- !.
replace_token(#/\, "#/\\\\") :- !.

remove_spaces(SpString, Atom) :-
    name(SpString, SpList),
    delete_all(SpList, 0' , List),
    name(Atom, List).

delete_all([], _, []).
delete_all([H|T], H, L) :-
    !,
    delete_all(T, H, L).
delete_all([H|T], C, [H|L]) :-
    delete_all(T, C, L).

domain_element(I, [H|T], Val) :-
    domain_element(I, H, T, Val).

domain_element(1, L.._, _, L) :-
    !.
domain_element(1, H, _, H) :-
    !.
domain_element(I, L..U, T, Val) :-
    !,
    (I =< U - L + 1 ->
        Val is L + I - 1
    ;    
    	I1 is I - U + L - 1,
    	domain_element(I1, T, Val)
    ).    
domain_element(I, _, [H|T], Val) :-
    I1 is I - 1,
    domain_element(I1, H, T, Val).
