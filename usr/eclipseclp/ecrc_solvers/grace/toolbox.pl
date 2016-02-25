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
% A set of various predicates to be used for modifications of the
% default search
%

:- begin_module(grace).
:- call(lib(fd)).

%
% Value Selection
%
%indomainr(Var) :-  MGW
%    dvar_domain(Var, Domain),
%    dom_range(Domain, Min, Max),
%    between(Max, Min, -1, Var).

indomainr(Var) :-
    dom(Var,List),
    rev_memb(Var,List).

rev_memb(Var,[_|Tail]) :-
    rev_memb(Var,Tail).
rev_memb(Var,[Var|_]).

indomain_random(Var) :-
    dvar_domain(Var, Domain),
    Domain = dom(List, Size),
    I is (random mod Size) + 1,
    domain_element(I, List, Val),
    (Var = Val; Var ## Val, indomain_random(Var)).


:- export
	smallest_element/3,
	largest_element/3,
	random_element/3,
	halve_range_bottom/3,
	halve_range_top/3,
	halve_elements_bottom/3,
	halve_elements_top/3.

smallest_element(Var, List, List1) :-
    delete_var(Var, List, List1),
    indomain(Var).

largest_element(Var, List, List1) :-
    delete_var(Var, List, List1),
    indomainr(Var).

random_element(Var, List, List1) :-
    delete_var(Var, List, List1),
    indomain_random(Var).

halve_range_bottom(Var, List, List1) :-
    is_integer_domain(Var), !,
    dvar_domain(Var, Dom),
    dom_range(Dom, Min, Max),
    Mid is (Max + Min)//2,
    (Var #<= Mid; Var #> Mid),
    delete_var(Var, List, LD),
    append(LD, [Var], List1).
halve_range_bottom(Var,List,List1) :-
     is_domain(Var), !,
     dom(Var,DomList),
     split_low(DomList,LowDom,HighDom),
     (Var::LowDom ; Var::HighDom),
     delete_var(Var, List, LD),
     append(LD, [Var], List1).
halve_range_bottom(Val, List, List1) :-
    nonvar(Val),
    delete_var(Val, List, List1).

halve_range_top(Var, List, List1) :-
    is_integer_domain(Var), !,
    dvar_domain(Var, Dom),
    dom_range(Dom, Min, Max),
    Mid is (Max + Min)//2,
    (Var #> Mid; Var #<= Mid),
    delete_var(Var, List, LD),
    append(LD, [Var], List1).
halve_range_top(Var,List,List1) :-
     is_domain(Var), !,
     dom(Var,DomList),
     split_high(DomList,HighDom,LowDom),
     (Var::HighDom ; Var::LowDom),
     delete_var(Var, List, LD),
     append(LD, [Var], List1).
halve_range_top(Val, List, List1) :-
    nonvar(Val),
    delete_var(Val, List, List1).

halve_elements_bottom(Var, List, List1) :-
    is_integer_domain(Var), !,
    dom(Var, ElList),
    length(ElList, L),
    Mid is L//2,
    length(L1, Mid),
    append(L1, [X|_], ElList),
    (Var #< X; Var #>= X),
    delete_var(Var, List, LD),
    append(LD, [Var], List1).
halve_elements_bottom(Var, List, List1) :-
    is_domain(Var), !,
    halve_range_bottom(Var,List,List1).
halve_elements_bottom(Val, List, List1) :-
    nonvar(Val),
    delete_var(Val, List, List1).

halve_elements_top(Var, List, List1) :-
    is_integer_domain(Var), !,
    dom(Var, ElList),
    length(ElList, L),
    Mid is L//2,
    length(L1, Mid),
    append(L1, [X|_], ElList),
    (Var #>= X; Var #< X),
    delete_var(Var, List, LD),
    append(LD, [Var], List1).
halve_elements_top(Var, List, List1) :-
    is_domain(Var), !,
    halve_range_top(Var,List,List1).
halve_elements_top(Val, List, List1) :-
    nonvar(Val),
    delete_var(Val, List, List1).

split_low(List,LowDom,HighDom) :-
     sort(List,SList),
     length(SList,N),
     M is (N+1)//2,
     length(LowDom,M),
     append(LowDom,HighDom,SList).

split_high(List,HighDom,LowDom) :-
     sort(List,SList),
     length(SList,N),
     M is N//2,
     length(LowDom,M),
     append(LowDom,HighDom,SList).
%
% Variable Selection
%

:- export
	grace_var_selection/5,
	first_in_list/2,
	smallest_domain/2,
	largest_domain/2,
	smallest_minimum/2,
	largest_minimum/2,
	smallest_maximum/2,
	largest_maximum/2,
	largest_difference/2,
	smallest_difference/2,
	least_regret/2,
	most_constrained/2.

first_in_list([Var|_], Var).

/*
 * Define a variable selection predicate which looks as

 
selectpred([H|T], Var) :-
    var_size(H, Val),
    find_var(T, H, Val, Var).

find_var([], Chosen, _, Chosen).
find_var([H|T], SoFar, OldVal, Var) :-
    var_size(H, NewVal),
    (
	NewVal >= OldVal
    ->
	find_var(T, SoFar, OldVal, Var)
    ;
	find_var(T, H, NewVal, Var)
    ).

 */

grace_var_selection(Name, MinMax, Pred, PredH, PredCard) :-
    concat_atoms(find_, Name, AuxName),
    Head =.. [Name, [H|T], Var],
    AuxGoal =.. [AuxName, T, H, Card, Chosen],
    copy_term((Pred, PredH, PredCard), (Pred, H, Card)),
    compile_term((Head :- Pred, AuxGoal, Var = Chosen)),
    AuxCl1 =.. [AuxName, [], Chosen, _, Chosen],
    AuxCl2 =.. [AuxName, [Var1|L], OldVar, OldCard, Chosen],
    copy_term((Pred, PredH, PredCard), (Pred1, Var1, NewCard)),
    RecG1 =.. [AuxName, L, OldVar, OldCard, Chosen],
    RecG2 =.. [AuxName, L, Var1, NewCard, Chosen],
    (MinMax = min ->
    	Test = (NewCard @>= OldCard)
    ;
    	Test = (NewCard @=< OldCard)
    ),
    compile_term([(AuxCl1 :- !),
		(AuxCl2 :- Pred1, (Test -> RecG1; RecG2))]).

:- grace_var_selection(smallest_domain, min, dvar_size(Var, Size), Var, Size).
:- grace_var_selection(largest_domain, max, dvar_size(Var, Size), Var, Size).
:- grace_var_selection(smallest_minimum, min,
	(my_dom_range(Var, Min, _)), Var, Min).
:- grace_var_selection(largest_minimum, max,
	(my_dom_range(Var, Min, _)), Var, Min).
:- grace_var_selection(smallest_maximum, min,
	(my_dom_range(Var, _, Max)), Var, Max).
:- grace_var_selection(largest_maximum, max,
	(my_dom_range(Var, _, Max)), Var, Max).
:- grace_var_selection(smallest_difference, min,
	(my_size(Var,Size)), Var, Size).
:- grace_var_selection(largest_difference, max,
	(my_size(Var,Size)), Var, Size).
:- grace_var_selection(least_regret, max, var_regret(Var, Size), Var, Size).

my_size(Var,Size) :-
     is_integer_domain(Var), !,
     dvar_domain(Var,Dom),
     dom_range(Dom,Min,Max),
     Size is Max-Min.
my_size(Var,Size) :-
     dom(Var,List),
     length(List,Size).

most_constrained(List, Var) :-
    deleteffc(Var, List, _).

% For manual variable selection
select_only(Var) :-
    select_value(Var, [Var], _).

% For manual value selection
modify_var(Var, "=", List) :-
    var_eq(Var, List)
    ;
    var_neq(Var, List).
modify_var(Var, "#", List) :-
    var_neq(Var, List)
    ;
    var_eq(Var, List).

var_eq(Var, List) :-
    Var :: List.

var_neq(Var, List) :-
    list_to_dom(List, Dom),   % MGW
    dvar_domain(Var, DV),
    dom_difference(DV, Dom, Diff, _),
    dvar_update(Var, Diff).

not_in(_, []).
not_in(Var, [N|L]) :-
    (integer(Var) ->
	Var \== N
    ;
	Var ## N
    ),
    not_in(Var, L).

test_equal(Var, Val) :-
    stop_printing_trace,
    not test_inequal(Var, Val),
    restore_trace.

test_inequal(Var, Val) :-
    set_priority(run_prio),
    (Var = Val,
    true ->
	fail
    ;
    	true
    ).

%
% Various constraints
%
:- export
	list_max/2,
	list_min/2,
	max_var/3,
	min_var/3,
	permutation/1.

permutation(V) :-
    permutation(V, _).

% Vars is a permutation of the list 1..N
permutation(Vars, Vals) :-
    length(Vars, N),
    length(Vals, N),
    Vals :: 1..N,
    Vars :: 1..N,
    alldifferent(Vars),
    alldifferent(Vals),
    inv_vals(Vars, 1, Vals).


inv_vals([], _, _).
inv_vals([V|L], I, Vals) :-
    inv_val(V, I, Vals),
    I1 is I + 1,
    inv_vals(L, I1, Vals).


%
% V = j iff Vals(j) = i
% if j removed from the domain of V, remove i from the domain of Vals(j), redelay
% if V instantiated to j, Vals(j) = i, end
% if Vals(j) instantiated to i, V=j, end
% if Vals(j) instantiated to k#i, redelay
%
% woken when V updated or any of Vals instantiated
inv_val(V, I, Vals) :-
    nonvar(V),
    V1 is V - 1,
    list_arg(V1, Vals, I).
inv_val(V, I, Vals) :-
    var(V),
    dvar_domain(V, DV),
    dom_to_list(DV, LV),
    update_inv_vals(V, LV, 1, I, Vals),
    make_suspension(inv_val(V, I, Vals), 3, Susp),
    insert_suspension(V, Susp, any of fd, fd),
    insert_suspension(Vals, Susp, inst of suspend, suspend).
    %suspend(inv_val(V, I, Vals), 3, [V->any, Vals->inst]).

update_inv_vals(_, [], _, _, _).
update_inv_vals(Var, [V|LV], J, I, [Val|Vals]) :-
    (V = J ->
    	(nonvar(Val), Val=I ->
    	    Var = J
	;
	    true
	),
    	J1 is J + 1,
    	update_inv_vals(Var, LV, J1, I, Vals)
    ;
    	Val ## I,
    	J1 is J + 1,
    	update_inv_vals(Var, [V|LV], J1, I, Vals)
    ).

var_regret(Var, Dif) :-
    is_integer_domain(Var), !,
    Var :: List,
    var_regret1(List, Dif).
var_regret(_,1).

var_regret1([_.._|_], 1) :-
    !.
var_regret1([A,H|_], Dif) :-
    !,
    (H = B.._ ->
    	Dif is B - A
    ;
    	Dif is H - A
    ).	
var_regret1([_], 100000).

list_min([Min], Min) :- !.
list_min([Var|List], Min) :-
    list_min(Var, List, Min).

list_min(Min, [], Min).
list_min(Var, [X|L], Min) :-
    min_var(Var, X, M),
    list_min(M, L, Min).

list_max([Max], Max) :- !.
list_max([Var|List], Max) :-
    list_max(Var, List, Max).

list_max(Max, [], Max).
list_max(Var, [X|L], Max) :-
    max_var(Var, X, M),
    list_max(M, L, Max).

min_var(A, B, Min) :-
    A #>= Min,
    B #>= Min,
    min_var_test(A, B, Min).

min_var_test(A, B, Min) :-
    dvar_range(A, MinA, MaxA),
    dvar_range(B, MinB, MaxB),
    (MaxA =< MinB ->
    	Min = A
    ;
    MinA < MinB ->
	Min #>= MinA,
	Goal = min_var_test(A, B, Min),
	make_suspension(Goal, 3, Susp),
	insert_suspension(Goal, Susp, min of fd, fd),
	insert_suspension(Goal, Susp, max of fd, fd)
    ;
    	(MinA >= MaxB ->
    	    Min = B
	;
	    Min #>= MinB,
	    Goal = min_var_test(A, B, Min),
	    make_suspension(Goal, 3, Susp),
	    insert_suspension(Goal, Susp, min of fd, fd),
	    insert_suspension(Goal, Susp, max of fd, fd)
	)
    ).


max_var(A, B, Max) :-
    A #<= Max,
    B #<= Max,
    max_var_test(A, B, Max).

max_var_test(A, B, Max) :-
    dvar_range(A, MinA, MaxA),
    dvar_range(B, MinB, MaxB),
    (MinA >= MaxB ->
	Max = A
    ;
    MaxA > MaxB ->
	Max #<= MaxA,
	Goal = max_var_test(A, B, Max),
	make_suspension(Goal, 3, Susp),
	insert_suspension(Goal, Susp, min of fd, fd),
	insert_suspension(Goal, Susp, max of fd, fd)
    ;
    	(MaxA =< MinB ->
    	    Max = B
	;
	    Max #<= MaxB,
	    Goal = max_var_test(A, B, Max),
	    make_suspension(Goal, 3, Susp),
	    insert_suspension(Goal, Susp, min of fd, fd),
	    insert_suspension(Goal, Susp, max of fd, fd)
	)
    ).


