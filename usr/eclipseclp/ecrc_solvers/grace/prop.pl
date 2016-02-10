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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Predicates to trace and display
%	the propagation between labeling steps.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(grace).
:- call(lib(fd)).

%
% Deamon on the labelled variable
%
start_stepw_deamon(Inst) :-
    nonvar(Inst).
start_stepw_deamon(Var) :-
    (getval(mode, stepw) ->
	copy_term(Var, CVar),
	make_suspension(stepw_deamon(Var, CVar), 1, Susp),
	insert_suspension(Var, Susp, constrained of suspend, suspend)
    ;
	true
    ).

stepw_deamon(Val, Var) :-
    get_depth(D),
    record(label, [label(Val, D)|Var]),
    %trace
    init_propagation_trace(label(Val, D)).

%
% Recording the information
%
trace_suspension(_, _, _, grace) :- !.
trace_suspension(Port, Goal, Mark, _) :-
    %printf("%s %w %GDmVw\n%b", [Port, Mark, Goal]),
    handle_suspension_trace(Port, Goal, Mark).

handle_suspension_trace('CALL', Goal, Mark) :-
    copy_term(Goal, Copy),
    record(call, [Mark|Copy]).
handle_suspension_trace('EXIT', Goal, Mark) :-
    copy_term(Goal, Copy),
    record(exit, [Mark|Copy]).
handle_suspension_trace('REC_WAKE', Goal, Mark) :-
    copy_term(Goal, Copy),
    record(exit, [Mark|Copy]).
handle_suspension_trace('FAIL', _Goal, Mark) :-
    (getval(first_fail, 1) ->
	record(fail, Mark),
	setval(first_fail, 0)
    ;
	true
    ).

%
% We can only record goals, no suspensions, because of bug #731, otherwise
% we would duplicate them
trace_propagation(Mark, Woken, Delayed, First) :-
    %printf("+%w (%d) %VDw %VDw\n", [Mark, First, Woken, Delayed]),
    record_propagation(Mark, Woken, Delayed, First).

record_propagation(Mark, Woken, Delayed, First) :-
    woken_goals(Woken, First, WokenList),
    delayed_list(Delayed, 16'7fffffff, DelayedList),	% always new
    (WokenList = [] ->
	true
    ;
	record(wake, [Mark|WokenList])
    ),
    (DelayedList = [] ->
	true
    ;
	record(delay, [Mark|DelayedList])
    ).

%
% filter our local goals and create a list of marks
%
woken_goals(L, First, W) :-
    (woken_goals(L, First, W, -1)).

woken_goals([], _, [], _).
woken_goals([Susp|S], First, L, Last) :-
    (suspension_to_goal(Susp, _, grace) ->
	woken_goals(S, First, L, Last)
    ;
    %printf("in woken: %Vw\n", [Susp]),
    suspension_mark(Susp, First, Mark),
    Mark > 0,
    Mark \== Last ->
	L = [Mark|L1],
	woken_goals(S, First, L1, Mark)
    ;
	woken_goals(S, First, L, Last)
    ).

%
% New suspensions. Filter out our goals and mark the rest.
%
delayed_list([], _, []).
delayed_list([Susp|S], New, L) :-
    suspension_to_goal(Susp, Goal, Module),
    (Module = grace ->
	L1 = L
    ;
	(suspension_mark(Susp, New, Mark) ->
	    copy_term(Goal, Copy),
	    record(delay_goal, [Mark|Copy]),
	    L = [Mark|L1]
	;
	    L1 = L
	)
    ),
    delayed_list(S, New, L1).


init_propagation_trace(Label) :-
    get_parent(p(_, _, LS, _)),
    garbage_collect,
    new_scheduled(LS, Woken),
    get_suspension_counter(SC),
    (tcl_eval('set .tc.reg.var', 0) ->
	% Replace
	set_first_suspension(SC)
    ;
	% Add
	set_first_suspension(SC)
    ),
    %printf("----init: label %w, first counter %d\n%b", [Label, SC]),
    %printf("\twoken: %Vw\n", [Woken]),
    %printf("\tlast scheduled: %w\n", [LS]),
    %trace
    record_propagation(Label, Woken, [], SC),
    setval(first_fail, 1),
    last_scheduled(LS1),
    last_suspension(LD1),
    set_parent(p(Label, [], LS1, LD1)).

reset_propagation_trace :-
    erase_all(call),
    erase_all(exit),
    erase_all(fail),
    erase_all(wake),
    erase_all(delay),
    erase_all(delay_goal),
    erase_all(label),
    (current_array(goals(_, _), _) ->
	erase_array(goals/2)
    ;
	true
    ).

:- global pp/0.
pp :-
    recorded_list(call, CL),
    recorded_list(exit, EL),
    recorded_list(delay, DL),
    recorded_list(wake, WL),
    recorded_list(fail, FL),
    printf("\ncall=", []),
    print_array(0, CL),
    printf("\nexit=", []),
    print_array(1, EL),
    printf("\ndelay=", []),
    print_list(DL),
    printf("\nwake=", []),
    print_list(WL),
    printf("\nfail=", []),
    print_list(FL).

pl(Key) :-
    recorded_list(Key, L),
    printf("\n%s=", [Key]),
    print_list(L).

print_array(I, []) :-
    current_array(goals(N, _), _),
    N1 is N - 1,
    between(0, N1, 1, C),
    getval(goals(C, I), Goal),
    (var(Goal) ->
	true
    ;
	printf("%d\t%w\n%b", [C, Goal])
    ),
    fail; true.
print_array(_, [H|T]) :-
    print_list([H|T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Displaying the propagation tree
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Display the whole propagation. Since we don't have a good graph
% package, display it as a tree.
%
make_graph(1) :-
    woken_graph(1).
make_graph(3) :-
    simple_graph(1).

woken_graph(Eager) :-
    make_tree('.t', "Propagation Tree", 30, horizontal, Tree, 1),
    recorded_list(wake, WL),
    goal_graph(Tree, WL, Eager).

simple_graph(Eager) :-
    tcl_eval('make_simple_tree .st', Tree),
    recorded_list(wake, WL),
    simple_tree(Tree, WL, Eager).

delayed_graph(Eager) :-
    make_tree('.t', "Propagation Tree", 30, horizontal, Tree, 1),
    recorded_list(delay, DL),
    goal_graph(Tree, DL, Eager).

%
% Display the sequence of updates of a specified variable
%
display_var_updates(Id) :-
    current_array(goals(Max, _), _),
    make_list_of_var_nodes(0, Max, Id, List),
    (List = [] ->
	true
    ;
	failed_nodes_list(FL, List),
	sort(2, =<, FL, FL1),	% to get I-fail after I-Node
	label_nodes_list(LL, FL1, MD),
	keysort(LL, Sorted),
	make_tree('.tv', "Variable Updates", 10, vertical, Tree, 0),
	display_var_nodes(Tree, Sorted, MD),
	colour_failed_nodes(Tree)
    ).

make_tree(Top, Title, ParDistance, Layout, Tree, Replace) :-
    tcl_eval(['make_tree ', Top, ' "', Title, '" ',
	ParDistance, ' ', Layout, ' ', Replace], Tree).

%
% Make a list of all nodes which have modified the given variable
make_list_of_var_nodes(I, Max, Id, List) :-
    I < Max,
    !,
    I1 is I + 1,
    goal_format(I, Call, Exit),
    (var_in_term(Id, Call, CVar) ->
	(var_in_term(Id, Exit, EVar),
	same_domain(CVar, EVar),
	not(recorded(fail, I)) ->
	    make_list_of_var_nodes(I1, Max, Id, List)
	;
	    % different or not there - must be instantiated
	    getval(goals(I, 2), Order),
	    List = [Order-I|L],
	    make_list_of_var_nodes(I1, Max, Id, L)
	)
    ;
	make_list_of_var_nodes(I1, Max, Id, List)
    ).
make_list_of_var_nodes(_, _, _, []).

failed_nodes_list(List, Link) :-
    recorded_list(fail, FL),
    failed_nodes_list(FL, List, Link).

failed_nodes_list([], L, L).
failed_nodes_list([H|T], [O-fail|List], Link) :-
    getval(goals(H, 2), O),
    failed_nodes_list(T, List, Link).

label_nodes_list(List, Link, MD) :-
    recorded_list(wake, WL),
    label_nodes_list(WL, List, Link, 0, MD).

label_nodes_list([], L, L, M, M).
label_nodes_list([[label(_, D), H|_]|T], [O1-l(D)|List], Link, M, MD) :-
    !,
    getval(goals(H, 2), O),
    O1 is O,
    (D > M ->
	M1 = D
    ;
	M1 = M
    ),
    label_nodes_list(T, List, Link, M1, MD).
label_nodes_list([_|T], List, Link, M, MD) :-
    label_nodes_list(T, List, Link, M, MD).

display_var_nodes(Tree, [_-P|L], _MD) :-
    (L = [] ->
	add_successors(Tree, P, [], 1, 0)
    ;
	display_var_nodes(Tree, P, L, []),
	fail; true
    ).

display_var_nodes(_, _, [], _).
display_var_nodes(Tree, P, [_-C|L], Lab) :-
    (P = fail ->
	display_var_nodes(Tree, C, L, Lab)
    ;
    C = fail ->
	display_var_nodes(Tree, P, L, Lab)
    ;
    C = l(D) ->
	(find_stack_depth(Lab, D, LN, NewLab) ->
	    display_var_nodes(Tree, C, L, NewLab)
	;
	    display_var_nodes(Tree, C, L, [D-P|Lab])
	)
    ;
    P = l(D) ->
	(find_stack_depth(Lab, D, LN, NewLab) ->
	    true
	;
	    LN = start,
	    NewLab = [D-start|Lab]
	),
	add_successors(Tree, LN, [C], 0, 0),
	display_var_nodes(Tree, C, L, NewLab)
    ;
	add_successors(Tree, P, [C], 0, 0),
	display_var_nodes(Tree, C, L, Lab)
    ).

find_stack_depth(L, D, LN, L) :-
    L = [D-LN|_],
    !.
find_stack_depth([H-_|T], D, LN, L) :-
    H > D,
    find_stack_depth(T, D, LN, L).

/*
display_var_nodes(Tree, [_-P|L]) :-
    (L = [] ->
	add_successors(Tree, P, [], 1, 0)
    ;
	display_var_nodes(Tree, P, L, 0)
    ).

display_var_nodes(_, _, [], _).
display_var_nodes(Tree, P, [_-C|L], Fail) :-
    (P = fail ->
	display_var_nodes(Tree, C, L, Fail)
    ;
    C = fail ->
	display_var_nodes(Tree, P, L, 1)
    ;
	add_successors(Tree, P, [C], 1, Fail),
	display_var_nodes(Tree, C, L, 0)
    ).
*/

goal_graph(Tree, [], _) :-
    colour_failed_nodes(Tree).
goal_graph(Tree, [[Parent|Woken]|List], Eager) :-
    sort(Woken, Sorted),
    add_successors(Tree, Parent, Sorted, Eager, 0),
    goal_graph(Tree, List, Eager).

add_successors(Tree, Parent, Children, Eager, Fail) :-
    goals_list(Children, WList),
    list_to_tcl(WList, TclList),
    (Parent = label(V, D) ->
	concat_string(['label(', V, ',', D, ')'], ParT)
    ;
	ParT = Parent
    ),
    ((integer(Parent); Parent = label(_, _)) ->
	goal_to_node(Parent, PNode)
    ;
	PNode = Parent
    ),
    tcl_eval(['add_successors ', Tree, ' ', ParT, ' ', PNode,
			TclList, Eager, ' ', Fail]).

colour_failed_nodes(Tree) :-
    recorded_list(fail, FL),
    list_to_tcl(FL, FTcl),
    tcl_eval(['tree_failed_nodes ', Tree, ' ', FTcl]).

simple_tree(_, [], _).
simple_tree(Tree, [[Parent|Woken]|List], Eager) :-
    sort(Woken, Sorted),
    add_simple_successors(Tree, Parent, Sorted, Eager, 0),
    simple_tree(Tree, List, Eager).

add_simple_successors(Tree, Parent, Children, Eager, _Fail) :-
    simple_goal_list(Children, CL),
    list_to_tcl(CL, TclList),
    (Parent = label(V, D) ->
	concat_string(['label(', V, ',', D, ')'], ParT)
    ;
	ParT = Parent
    ),
    tcl_eval(['add_simple_successors ', Tree, ' ', ParT, ' ', 
			TclList, Eager]).

simple_goal_list([], []).
simple_goal_list([G|L], [C|T]) :-
    (goal_modified(G) ->
	list_to_tcl([G, red], C)
    ;
	list_to_tcl([G, black], C)
    ),
    simple_goal_list(L, T).


goals_list([], []).
goals_list([G|T], [W|L]) :-
    goal_to_node(G, GoalS),
    !,
    concat_string(['{', G, GoalS, '}'], W),
    goals_list(T, L).
goals_list([_|T], L) :-
    goals_list(T, L).

% Fake GC to get rid of the long list
call_number(_) :-
    recorded_list(call, CList),
    length(CList, Length),
    setval(nodes, Length),
    fail.
call_number(X) :-
    getval(nodes, X).

goal_format(label(V, D), Var, V) :-
    !,
    recorded(label, [label(V, D)|Var]).
goal_format(Index, Goal, Exit) :-
    getval(goals(Index, 0), Goal),
    getval(goals(Index, 1), Exit),
    (var(Exit) ->
	Exit = Goal
    ;
	true
    ).

%
% Process the recorded data, move records to arrays where direct
% access is necessary
%
end_propagation :-
    get_suspension_counter(SC),
    (SC > 1 ->
	(call_number(CN),
	CN > 0 ->
	    prop_register(CN, 1),
	    % Move the data from records to arrays, because we need
	    % direct access
	    goals_array(SC),
	    findall((Lab,D)-L, (recorded(delay, [label(Lab, D)|L], Ref), erase(Ref)), DL),
	    prune_delays(DL)
	;
	recorded(wake, _) ->
	    recorded_list(wake, WL),
	    flatten(WL, WLF),
	    sort(WLF, WLFS),
	    length(WLFS, N),
	    prop_register(N, 0)
	)
    ;
	tcl_eval('tc_register .tc {}')
    ).

prop_register(CN, Calls) :-
    (CN = 1 -> G = " goal"; G = " goals"),
    concat_string([CN, G, " registered"], Text),
    tcl_eval(['tc_register .tc "', Text, '" ', Calls]).

prune_delays([(Lab,D)-DL|L]) :-
    filter_delays(DL, CDL),
    (CDL = [] ->
	true
    ;
	recorda(delay, [label(Lab, D)|CDL])
    ),
    prune_delays(L).
prune_delays([]).
    
goals_array(C) :-
    C1 is C + 1,
    (current_array(goals(_, _), _) ->
	erase_array(goals/2)
    ;
	true
    ),
    make_local_array(goals(C1, 3)),
    recorded_list(call, CList),
    insert_calls(CList, 0),
    erase_all(call),
    recorded_list(exit, EList),
    insert_exits(EList),
    erase_all(exit),
    recorded_list(delay_goal, DG),
    insert_delay_goals(DG, 0),
    erase_all(delay_goal),
    true.

insert_calls([], _).
insert_calls([[M|Goal]|List], I) :-
    setval(goals(M, 0), Goal),
    setval(goals(M, 2), I),
    I1 is I + 1,
    insert_calls(List, I1).

insert_exits([]).
insert_exits([[M|Goal]|List]) :-
    setval(goals(M, 1), Goal),
    insert_exits(List).

insert_delay_goals([], _).
insert_delay_goals([[M|Goal]|List], I) :-
    getval(goals(M, I), G),
    (var(G) ->
	setval(goals(M, I), Goal)
    ;
	true
    ),
    insert_delay_goals(List, I).

filter_delays([], []).
filter_delays([D|DL], [D|CL]) :-
    recorded(delay, [D|_]),
    !,
    filter_delays(DL, CL).
filter_delays([_|DL], CL) :-
    filter_delays(DL, CL).

var_in_term(Id, Var{grace:(grace with id:I)}, V) :-
    -?->
    I = Id,
    !,
    V = Var.
var_in_term(Id, Term, Var) :-
    compound(Term),
    Term = [_|_],
    !,
    var_in_term_list(Id, Term, Var).
var_in_term(Id, Term, Var) :-
    compound(Term),
    Term =.. [_|Args],
    var_in_term_list(Id, Args, Var).

var_in_term_list(Id, [Term|_], Var) :-
    var_in_term(Id, Term, Var),
    !.
var_in_term_list(Id, [_|L], Var) :-
    var_in_term_list(Id, L, Var).

same_domain(V1, V2) :-
    dvar_domain(V1, D),
    dvar_domain(V2, D).
