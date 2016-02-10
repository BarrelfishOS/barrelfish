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
%	Labeling
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(grace).
:- call(lib(fd)).

grace_label :-
    getval(grace, on),
    !,
    all_variables(Vars),
    grace_label_list(Vars),
    grace_solution.
grace_label :-
    all_variables(Vars),
    label_nograce(Vars).

all_variables(AllRev) :-
    labeled_matrices(Matrices),
    term_variables(Matrices, AllVars),
    reverse(AllVars, AllRev).		% to have them in the original order

active_variables(Vars) :-
    active_matrices(Matrices),
    term_variables(Matrices, AllVars),
    reverse(AllVars, Vars).

grace_label(_) :-
    getval(grace, off),
    !.
grace_label(Var) :-
    tcl(disable_selections),
    label_var(Var, Label, noselect),
    next_label(Var, Label).

next_label(Var, Goal) :-
    nonvar(Goal),
    Goal = modify_var(_, _, _),
    !,
    call(Goal),
    grace_label(Var).
next_label(_, _).

grace_label(_, _) :-
    getval(grace, off),
    !.
grace_label(Var, List) :-
    tcl(enable_selections),
    label_var(Var, Label, select),
    next_label(Label, Var, List).

% If something is manually modified, it is returned in the Label goal.
% Otherwise we go on with the selection we have
next_label(V, _, _) :-
    var(V),
    !.
next_label(Goal, Var, List) :-
    call(Goal),
    grace_label(Var, List).

grace_label_list(List) :-
    check_list(List),
    grace_label_list1(List).

grace_label_list1([]).
grace_label_list1(List) :-
    List = [_|_],
    minimize_bound_check,
    select_var(List, Var),
    (getval(grace, on) ->
	label_var(Var, Label, select)
    ;
	true
    ),
    next_grace_label_list(Label, Var, List).

next_grace_label_list(G, Var, List) :-
    var(G),
    select_value(Var, List, List1),
    grace_label_list1(List1).
next_grace_label_list(Goal, _, List) :-
    nonvar(Goal),
    call(Goal),
    grace_label_list1(List).

label_nograce([]).
label_nograce(List) :-
    List = [_|_],
    minimize_bound_check,
    select_var(List, Var),
    select_value(Var, List, NewList),
    label_nograce(NewList).

label_var(V, _, _) :-
    nonvar(V).
label_var(Var, Label, Select) :-
    var(Var),
    (var_matrix(Var, Name) ->
	(matrix_option(Name, lookahead, 1) ->
	    lookahead_matrix(Name)
	;
	matrix_option(Name, lookahead_var, 1) ->
	    lookahead_var(Var)
	;
	    true
	)
    ;
    	true
    ),
    do_label_var(Var, Label, Select).

do_label_var(V, _, _) :-
    nonvar(V).
do_label_var(Var, Label, Select) :-
    var(Var),
    handle_label_failure,
    (	true
    ;
	getval(backward, 1),
	decval(choices),
	fail
    ),
    (getval(stop, Stop) ->
	(Stop = goal(G1) ->
	    getval(goal, G),
	    (G1 = G ->
		step_mode,
		(G = 1 -> reset_status; true)
	    ;
	    G1 < G ->
	    	setval(mode, retry(goal(G1))),
	    	fail
	    ;
	    	true
	    )
	;
	Stop = depth(D),
	get_depth(D) ->
	    step_mode
	;
	    true
	)
    ;
	true
    ),
    get_cut(LastChP),
    set_last_choice(LastChP),
    getval(mode, Mode),
    handle_interface(Var, Mode, Label, Select),
    inc_depth,
    inc_goal,
    setval(backward, 0).

handle_label_failure :-
    getval(choices, Ch),
    getval(failures, Bt),
    getval(goal, G),
    get_last_choice(ChP),
    (
	true
    ;
	getval(mode, Mode),
	(Mode = retry(Back) ->
	    get_depth(D),
	    (Back = depth(RD),
	    (integer(RD), D =< RD; RD=fail(RDF), D =< RDF) ->
		setval(backward, 0),
		setval(choices, Ch),
		setval(failures, Bt),
		setval(goal, G),
		step_mode,
		integer(RD),
		display_all_matrices,
		handle_label_failure,
		(D = 0 -> reset_status; true)
	    ;
	    Back = goal(G1),
	    G1 >= G ->
		setval(backward, 0),
		setval(choices, Ch),
		setval(failures, Bt),
		setval(goal, G),
		setval(stop, Back),
		run_mode,
		display_all_matrices,
		handle_label_failure
	    ;
		D1 is D - 1,
		D1 >= 0,
		cut_to(ChP),
		tcl_eval(['catch {vs_delete .vs.c ', D1, '}']),
		fail
	    )
	)
    ).
handle_label_failure :-
    incval(choices),
    setval(backward, 1),
    fail.

ppp.
    
handle_interface(_, run_fast, _, _) :-
    !.					% do nothing in the fast mode
handle_interface(Var, back_min_max(Mode), Label, Select) :-
    !,
    setval(mode, Mode),
    display_all_matrices,
    handle_interface(Var, Mode, Label, Select).
handle_interface(_, step, _, _) :-
    set_priority(default_prio, 0),
    fail.
handle_interface(Var, Mode, Label, Select) :-
    get_priority(P),
    get_depth(D),
    (Mode = run(Prio) ->
	set_priority(Prio)		% it might have been untrailed somewhere
    ;
	true
    ),
    (P > matrix_prio ->
	print_status,
	selected_variable(Var),
	(Mode = run(Prio) ->
	    true %tcl_eval(update)
	;
	    force_displays,
	    (Mode = moddom(step) ->
		message('Next Step')
	    ;
	    Mode = moddom(run) ->
		step_mode,
		message('Next Step')
	    ;
		true
	    ),
	    end_propagation,
	    (Select = select ->
		tcl(enable_selections)
	    ;
		tcl(disable_selections)
	    ),
	    wait_for_events(Var, _, Label)
	),
	/*
	(single_option(control, print_trace, 1) ->
	    do_print_trace
	;
	    do_not_print_trace
	),
	*/
	restore_selected
    ;
	true
    ),
    (compound(Label) ->
	arg(1, Label, VarNew)
    ;
	VarNew = Var
    ),
    print_selected_varstack(VarNew, D, Mode),
    %start_stepw_deamon(VarNew),
    (single_option(varstack, flush, 1) ->
	tcl_eval(update)
    ;
	true
    ).

% To be used outside labeling or not synchronously
handle_events :-
    tk_next_event([Type|Args]),
    getval(mode, Mode),
    handle_event(Type, Args, Mode, Cont),
    (Cont = cont ->
	true
    ;
    Cont = fail ->
	fail
    ;
    Cont = retry_depth ->
	get_last_choice(ChP),
	ChP \== 0,
	cut_to(ChP),
	get_depth(D),
	D1 is D - 1,
	tcl_eval(['catch {vs_delete .vs.c ', D1, '}']),
	fail
    ;
    	handle_events
    ).


wait_for_events(_Var, LabIn, LabOut) :-
    tk_next_event([Type|Args]),
    getval(mode, Mode),
    handle_event(Type, Args, Mode, Cont),
    (Cont = cont ->
	LabIn = LabOut
    ;
    Cont = fail ->
	LabOut = fail
    ;
    Cont = select(N, I, J),
    select_var(N, I, J, NewVar) ->
	wait_for_events(NewVar, select_only(NewVar), LabOut)
    ;
    Cont = select_step(N, I, J),
    select_var(N, I, J, NewVar) ->
	LabOut = select_only(NewVar)
    ;
    Cont = modify(N, I, J),
    modify_var(N, I, J, LabOut) ->
    	true
    ;
    Cont = modify_var(_, _, _) ->
    	LabOut = Cont
    ;
	Cont \= retry_depth,
	wait_for_events(_, LabIn, LabOut)
    ).

var_out(Old, New) :-
    (New == [] ->
	true
    ;
	New = Old
    ).

handle_event("step", _, _, cont) :-
    !.
    %prolog_step_mode,		% already done from Tcl
    %tcl_eval(step_mode).
handle_event("run", _, _, cont) :-
    !,
    run_mode.
handle_event("break", _, _, no) :-
    !,
    break.
handle_event("abort", _, _, no) :-
    !,
    reset_global_state,
    abort.
handle_event("restart", _, _, no) :-
    !,
    setval(mode, retry(start)),
    restore_selected,
    fail.
handle_event("exit", _, _, _) :-
    !,
    reset_global_state,
    tcl_eval('destroy .'),
    tcl_eval(exit),
    abort.
handle_event("stop_goal", [N], _, Cont) :-
    (integer(N),
    N > 0 ->
	getval(goal, G),
	(N > G ->
	    setval(stop, goal(N)),
	    run_mode,
	    Cont = cont
	;
	N < G ->
	    setval(mode, retry(goal(N))),
	    restore_selected,
	    fail
	;
	    Cont = no
	)
    ;
	message('Bad step #'),
	Cont = no
    ).
handle_event("show", [N, I, J, X, Y], _, no) :-
    !,
    show_domain(N, I, J, X, Y).
handle_event("select", [N, I, J], _, select(N, I, J)) :-
    !.
handle_event("select_step", [N, I, J], _, select_step(N, I, J)) :-
    !,
    prolog_step_mode,
    tcl_eval(step_mode).
handle_event("lookahead_cell", [N, I, J], _, Cont) :-
    !,
    (lookahead_cell(N, I, J) ->
	search_size,
	Cont = no
    ;
	Cont = fail
    ).
handle_event("propagate_cell", [N, I, J], _, Cont) :-
    !,
    (grace_propagate(N, I, J) ->
	Cont = no
    ;
	Cont = fail
    ).
handle_event("constraints", [N, I, J], _, no) :-
    !,
    list_constraints(N, I, J).
handle_event("stop", [When, N, I, J], _, no) :-
    !,
    add_breakpoint(N, I, J, When).
handle_event("print", _, _, no) :-
    !,
    print_all_matrices.
handle_event("lookahead", _, _, Cont) :-
    !,
    (lookahead_all ->
    	Cont = lookahead,
	search_size,
    	wake
    ;
    	Cont = fail
    ).

handle_event("modify", [N, I, J], _, modify(N, I, J)) :-
    !.
handle_event("bind_var", [N, I, J, Val], _, Lab) :-
    !,
    matrix_element(N, I, J, Var),
    %
    % we should not create a choice point if it fails
    (dvar_domain(Var, D),
    dom_check_in(Val, D) ->
	(nonvar(Var) ->
	    Lab = no
	;
    	test_equal(Var, Val) ->
	    Lab = modify_var(Var, "=", [Val])
	;
	    (Var ## Val ->
		Lab = no
	    ;
	    	Lab = fail
	    )
	)
    ;
    	Lab = no
    ).
handle_event("fail", _, _, fail) :-
    !.
handle_event("stepd", _, moddom(_), cont) :-
    !,
    setval(mode, moddom(step)).
handle_event("stepd", _, moddomf(_), cont) :-
    !,
    setval(mode, moddomf(step)).
handle_event("stepd", _, _, cont) :-
    !,
    tcl_eval('active_matrices', L),
    (L = "" ->
	true
    ;
	setval(mode, moddom(step)),
	install_stepd_handlers(L)
    ).
handle_event("rund", _, moddomf(_), cont) :-
    !,
    setval(mode, moddomf(run)).
handle_event("rund", _, moddom(_), cont) :-
    !,
    setval(mode, moddom(run)).
handle_event("rund", _, _, cont) :-
    !,
    tcl_eval('active_matrices', L),
    (L = "" ->
	true
    ;
	setval(mode, moddom(run)),
	install_stepd_handlers(L)
    ).
handle_event("stepw", _, _, cont) :-
    !,
    setval(mode, stepw),
    (tcl_eval('set .tc.reg.var', 0) ->
	reset_propagation_trace
    ;
	true
    ),
    trace_wake.
handle_event("retry_depth", [D], _, C) :-
    !,
    get_depth(CD),
    (CD =< D ->
	C = no
    ;
	restore_selected,
	C = retry_depth,
	printf("retrying level %d\n%b", [D]),
	setval(mode, retry(depth(D)))
    ).
handle_event("next_depth", [D], _, cont) :-
    !,
    D1 is D + 1,
    setval(stop, depth(D1)),
    printf("skip to next in level %d\n%b", [D]),
    run_mode.
handle_event("fail_depth", [D], _, C) :-
    !,
    get_depth(CD),
    (CD =< D ->
	C = no
    ;
	restore_selected,
	setval(mode, retry(depth(fail(D)))),
	printf("fail level %d\n%b", [D]),
	fail
    ).
handle_event("undo", [], _, C) :-
    !,
    get_depth(D),
    D1 is D - 1,
    (D1 >= 0 ->
	restore_selected,
	C = retry_depth,
	setval(mode, retry(depth(D1)))
    ;
	C = no
    ).
handle_event("display", [_], _, no) :-
    !.
handle_event("graph", [Eager], _, no) :-
    make_graph(Eager),
    !.
handle_event("var_prop", [Id], _, no) :-
    display_var_updates(Id),
    !.
handle_event("set_lookahead", [NameS, I], _, no) :-
    !,
    atom_string(Name, NameS),
    (I = 0 ->
	grace_option(Name, lookahead, 0),
	grace_option(Name, lookahead_var, 0)
    ;
    I = 1 ->
	grace_option(Name, lookahead, 1),
	grace_option(Name, lookahead_var, 0)
    ;
	grace_option(Name, lookahead, 0),
	grace_option(Name, lookahead_var, 1)
    ).
handle_event("set_option", [W, N, V], _, no) :-
    !,
    atom_string(WA, W),
    atom_string(NA, N),
    (string(V) ->
    	atom_string(VA, V)
    ;
    	VA = V
    ),
    grace_option(WA, NA, VA),
    handle_option(WA, NA, VA).
handle_event("handle_display", [W], _, no) :-
    !,
    tcl("handle_display ##", W).

handle_option(control, print_trace, Val) :-
    !,
    (Val = 1 ->
	do_print_trace
    ;
	do_not_print_trace
    ).
handle_option(_, _, _).

install_stepd_handlers([]) :- !.
install_stepd_handlers([Id|L]) :-
    (string(Id) -> term_string(Name, Id); Name = Id),
    matrix(Name, Sq),
    apply_matrix(Sq, Name, stepd),
    install_stepd_handlers(L).
install_stepd_handlers(Id) :-
    atomic(Id),
    (string(Id) -> term_string(Name, Id); Name = Id),
    term_string(Name, Id),
    matrix(Name, Sq),
    apply_matrix(Sq, Name, stepd).

stepd_handler(Var, W) :-
    var(Var),
    el_to_const(Var, D, _),
    make_suspension(stepd_delay(Var, D, W), 1, Susp),
    insert_suspension(Var, Susp, constrained of suspend, suspend).
stepd_handler(Var, _) :-
    nonvar(Var).

stepd_delay(Var, Old, W) :-
    getval(mode, Mode),
    (Mode = moddomf(M) ->
	setval(mode, moddom(M)),
	NewMode = moddom(M)
    ;
	NewMode = Mode
    ),
    (Mode = moddom(_) ->
	el_to_const(Var, D, _),
	(Old = D ->
	    true
	;
	    tcl_eval(['stepd_changed ', W, ' {', Old, '} {', D, '}'])
	),
	(var(Var) ->
	    make_suspension(stepd_delay(Var, D, W), 1, Susp),
	    insert_suspension(Var, Susp, constrained of suspend, suspend)
	;
	    true
	),
	(Old = D ->
	    true
	;
	    (Mode = moddom(step) ->
		handle_events
	    ;
		true
	    )
	)
    ;
	true
    ).
stepd_delay(_, Old, W) :-
    getval(mode, Mode),
    (Mode = moddom(M) ->
	setval(mode, moddomf(M)),
	tcl('stepd_failing ##', [W]),
	handle_events
    ;
	true
    ),
    tcl('stepd_restore ## {##}', [W, Old]),
    fail.

lookahead_matrix(Name) :-
    get_priority(P),
    set_priority(5, 1),
    matrix(Name, M),
    appnodes(grace_lookahead_var, M),
    set_priority(P, 1),
    wake.

lookahead_all :-
    get_priority(P),
    set_priority(5, 1),
    active_matrices(M),
    appnodes(grace_lookahead_var, M),
    set_priority(P, 1),
    wake.

grace_lookahead_var(El) :-
    var(El),
    findall(Val, (par_indomain(El), Val = El), L),
    var_eq(El, L).
grace_lookahead_var(T) :-
    nonvar(T).

lookahead_cell(N, I, J) :-
    matrix_element(N, I, J, El),
    lookahead_var(El).

lookahead_var(El) :-
    findall(Val, (par_indomain(El), Val = El), L),
    var_eq(El, L).

selected_variable(Var) :-
    (find_variable(Var, N, I, J) ->
	highlight_selected(Var, N, I, J)
    ;
	true
    ).

highlight_selected(_, N, I, J) :-
    getval(backward, Back),
    set_selection(N, I, J, Back),
    setval(selected, [N, I, J]).

select_var(N, I, J, Var) :-
    matrix_element(N, I, J, Var),
    var(Var),
    restore_selected,
    highlight_selected(Var, N, I, J).

modify_var(N, I, J, Label) :-
    matrix_element(N, I, J, Var),
    var(Var),
    el_to_const(Var, Dom, Size),
    concat_string(['modify_var {', Dom, '} ', Size, ' ', x1, ' ', y1], Show),
    tcl_eval(Show),
    tcl_eval('set new_value', NewVal),
    tcl_eval('set modify_mode', Mode),
    tcl_eval(update),
    NewVal \== "",
    Mode \== "",
    const_to_el(NewVal, List),
    Label = modify_var(Var, Mode, List).


delete_var(Var, [Var|L], R) :-
    -?->
    !,
    R = L.
delete_var(Var, [H|L], [H|T]) :-
    delete_var(Var, L, T).

restore_selected :-
    tcl_eval(restore_selected).

add_breakpoint(N, I, J, Cond) :-
    matrix_element(N, I, J, Var),
    (var(Var) ->
	(    add_breakpoint(N, I, J, Cond, Var),
	    concat_string(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 0], Cmd),
	    tcl_cut_fail(Cmd)
	;
	    printf("removing breakpont", []),
	    remove_breakpoints(Var),
	    fail
	)
    ;
	true
    ).

add_breakpoint(N, I, J, "ground", Var) :-
    !,
    remove_breakpoints(Var),
    make_suspension(breakpoint(N, I, J), 4, Susp),
    insert_suspension(Var, Susp, inst of suspend, suspend),
    tcl_eval(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 2]).
add_breakpoint(N, I, J, "modified", Var) :-
    !,
    remove_breakpoints(Var),
    new_breakpoint(Var, N, I, J),
    tcl_eval(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 1]).

remove_breakpoints(_{suspend: S}) :-
    -?->
    S = suspend with [constrained:C-_, inst:B-_],
    kill_breakpoints(C),
    kill_breakpoints(B).

% stop when ground
breakpoint(N, I, J) :-
    step_mode,
    el_label(N, I, J, Lab),
    concat_string([Lab, ' is ground'], Mod),
    message(Mod),
    tcl_eval(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 0]),
    concat_string(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 2], Cmd),
    tcl_cut_fail(Cmd).
    
    
% stop when modified
breakpoint(Var, N, I, J) :-
    step_mode,
    el_label(N, I, J, Lab),
    concat_string([Lab, ' modified'], Mod),
    message(Mod),
    (var(Var) ->
	new_breakpoint(Var, N, I, J)
    ;
	tcl_eval(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 0]),
	concat_string(['change_breakpoint ', N, ' ', I, ' ', J, ' ', 1], Cmd),
	tcl_cut_fail(Cmd)
    ).

kill_breakpoints([]) :- !.	% if list free
kill_breakpoints([S|L]) :-
    (suspension_to_goal(S, G, _),
    functor(G, breakpoint, _) ->
	kill_suspension(S, 0)
    ;
	true
    ),
    kill_breakpoints(L).
	
    
new_breakpoint(Var, N, I, J) :-
    make_suspension(breakpoint(Var, N, I, J), 4, Susp),
    insert_suspension(Var, Susp, constrained of suspend, suspend).

step_mode :-
    tcl_eval(step_mode),
    prolog_step_mode.

prolog_step_mode :-
    setval(mode, step),
    setval(stop, 0),
    % default_wake,		% reset when we collect a tree
    force_displays,
    !.
prolog_step_mode.		% if wake fails

run_mode :-
    tcl_eval('set cv_display', Disp),
    display_priority(Disp, P),
    set_priority(P, 0),
    (P =< varstack_prio ->
	setval(mode, run_fast),
	message('Running, ^C to stop')
    ;
	setval(mode, run(P)),
	message('Running')
    ),
    default_wake,
    tcl_eval('run_mode; update').

display_priority("All", 9) :- !.
display_priority("Expressions", 8) :- !.
display_priority("Stack", 7) :- !.
display_priority("None", 6) :- !.

background(N, I, J, Back) :-
    concat_string(['.', N, '.', I, '.', J, ' configure -bg ', Back], Sel),
    tcl_eval(Sel).

set_selection(N, I, J, Back) :-
    concat_string(['set_selection ', N, ' ', I, ' ', J, ' ', Back], Sel),
    tcl_eval(Sel).

show_domain(N, I, J, X, Y) :-
    matrix_element(N, I, J, El),
    X1 is X - 10,
    Y1 is Y - 30,
    el_to_const(El, Dom, Size),
    %concat_string(['show_field {', Dom, '} ', Size, ' ', X1, ' ', Y1], Show),
    tcl('show_field {##} ## ## ## {##} ## ##', [Dom, Size, X1, Y1, N, I, J]).

message(Text) :-
    tcl_eval(['status_message {', Text, '}']).

backtracks :-
    getval(choices, N),
    set_text(N, ".lbackm").

depth :-
    get_depth(N),
    tcl_eval(['set current_depth ', N]).

goal :-
    getval(goal, N),
    tcl_eval(['set goal_entry ', N]).

cost :-
    (getval(optimize, 1) ->
    	get_cost(C),
    	el_to_const(C, S, _),
    	set_text(S, '.lcostm')
    ;
    	true
    ).

solutions :-
    getval(solutions, N),
    (N = opt(NO) ->
	concat_string(["(", NO, ")"], NS)
    ;
    	NS = N
    ),
    set_text(NS, ".lsolsm").

delayed :-
    delayed_goals(L),
    sumlist(user_goals, L, 0, N),
    set_text(N, ".ldelm").

user_goals(G, I0, I) :-
    (our_goal(G) ->
    	I = I0
    ;
    	I is I0 + 1
    ).

set_text(Text, Where) :-
    tcl("## configure -text {##}", [Where, Text]).

print_selected_varstack(Var,D, _) :-
    (integer(Var) ->  Low=Var, High=Var, NewVar=Var 
     ; is_integer_domain(Var) ->
             get_attribute(Var, grace with [range:Low..High]),
             (var(Low) ->
                  dvar_domain(Var, Dom),
                  dom_range(Dom, Low, High)
              ;   true
             ),
             NewVar=Var
     ;  dom(Var,Dom),
        length(Dom,Length),
        NewVar::1..Length,
        element(NewVar,Dom,Var),
        Low=1, High=Length
     ),
     psv(NewVar,D, Low, High).
    
psv(Var, D, Low, High) :-
    (var(Var) ->
        print_stack_variable(Var, D, Low, High),
        % we want to be notified about the indomain even if it fails
        make_suspension(update_stack_variable(Var, D), 1, Susp),
        insert_suspension(Var, Susp, any of fd, fd)
    ;
        % the variable is already instantiated, e.g. by lookahead
        print_stack_variable(Var, D, Var, Var),
        tcl_eval(['update_domain .vs.c ', Var, ' ', D])
    ).

print_stack_variable(Var, D, Low, High) :-
    var_domain_list(Var, DList),
    print_stack_variable(Var, DList, D, Low, High).

print_stack_variable(Var, DList, D, Low, High) :-
    concat_string(['{'|DList], DS),
    (find_variable(Var, N, I, J) ->
        true
    ;
        var_id(Var, Id),
        N = "{}",
        I = '""',
        J = Id
    ),
    tcl_eval(['vs_display_domain .vs.c ', DS, ' ', Low, ' ', High, ' ', D,
        ' ', N, ' ', I, ' ', J]).
print_stack_variable(_, _, D, _, _) :-
    tcl_eval(['vs_delete .vs.c ', D]),
    fail.

update_stack_variable(Var, D) :-
    (D is get_depth - 1 ->
        dvar_domlist(Var, Val),
        tcl_eval(['update_domain .vs.c ', Val, ' ', D])
    ;
        % do not change variables which are not on top
        true
    ),
    (var(Var) ->
        make_suspension(update_stack_variable(Var, D), 1, Susp),
        insert_suspension(Var, Susp, any of fd, fd)
    ;
        true
    ).

interrupt :-
    step_mode.

x_handler(_, ["exit"]) :-
    !,
    tcl_eval(exit).
x_handler(N, T) :-
    error(default(N), T).

search_size :-
    labeled_matrices(M),
    term_variables(M, Vars),
    search_size(Vars, 0.0, SizeLn),
    tcl_eval(['set_size ', 0, ' ', SizeLn]).

search_size([], S, S).
search_size([V|L], S0, S) :-
    dvar_domain(V, DV),
    dom_size(DV, Size),
    S1 is S0 + ln(Size),
    search_size(L, S1, S).

check_list([]).
check_list([V|L]) :-
    (compound(V) ->
    	error(5, grace_label_list([V|L]))
    ;
    	check_list(L)
    ).	

do_print_trace :-
    trace_wake,
    set_stream(susp, debug_output).

do_not_print_trace :-
    default_wake,
    set_stream(susp_save, susp),
    set_stream(susp, null).

stop_printing_trace :-
    set_stream(susp_save, susp),
    set_stream(susp, null).

restore_trace :-
    set_stream(susp, susp_save).
