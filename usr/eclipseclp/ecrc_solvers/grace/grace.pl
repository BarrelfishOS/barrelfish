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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Graphical interface to Constraints Problems in ECLiPSe
%
% Primitives:
%	grace_start(Title)
%		called at the beginning, after creating the matrices
%		If the program fails over it, 'no solutions' is assumed
%
%	grace_add_matrix(Matrix)
%		called at the beginning, after creating the matrices
%		If the program fails over it, 'no solutions' is assumed
%		Matrices are in the format [Vars, Name, Xnames, Ynames]
%
%	grace_label
%		label all registered variables together
%
%	grace_label(Var)
%		label the variable Var, no selection is possible
%
%	grace_label(Var, Rest)
%		label the variable Var, but allow for manual
%		selections as well
%
%	grace_label_list(List)
%		Ask Grace to label the whole List using default
%		or manually selected routines.
%
%	grace_solution
%		called after a solution has been found
%
%	qg_display_value(Expr, Name)
%		trace the value of the expressions Expr
%
%	grace_stop(Message)
%		stops the execution and prints the message,
%		useful to set permanent breakpoints etc.
%
% Priorities:
%	4: breakpoints
%	5: no display mode
%	6: varstack print daemons
%	7: display print daemons
%	8: matrices print daemons
%	9: default - display all

:- module_interface(grace).

:- export
    grace/0,
    grace_label/0,		% label all matrices
    grace_label/1,		% label one variable (no manual)
    grace_label/2,		% label one variable (plus manual)
    grace_label_list/1,		% label a list, also when nograce
    grace_lookahead_var/1,
    grace_minimize/1,
    grace_minimize/2,
    grace_option/3,
    grace_propagate/3,
    grace_solution/0,
    grace_register_var/1,
    grace_start/1,
    grace_stop/1,
    nograce/0.

:- use_module(matrix_util).

:- begin_module(grace).

:- export
    get_parent/1,
    set_parent/1,
    trace_propagation/4,
    trace_suspension/4.

:- external(tcl_cut_fail/1).

:- lib(util).

:- set_flag(gc_interval, 100000).

:- import
	cut_to/1,
	dom_range/3,
	get_cut/1,
	get_flag_body/4,
	get_priority/1,
	is_predicate_/2,
	kill_suspension/2,
	last_suspension/1,
	last_scheduled/1,
	new_scheduled/2,
	set_default_error_handler/2,
	set_priority/1,
	set_priority/2,
	setarg/3,
	symbol_address/2,
	unify_attributes/2
    from sepia_kernel.

:- lib(tk).
:- call(lib(fd)).
:- call(lib(structures)).
:- lib(apply_macros).

:- pragma(expand).

:- tcl_source.

:- is_predicate(add_attribute/3) -> true;
	import add_attribute/3 from sepia_kernel.

:- get_flag(version, Ver), Ver @< '3.5' ->
	import
	    get_global_variable/2,
	    set_global_variable/2
	from sepia_kernel
    ;
	(current_array(grace_data, _) ->
	    true
	;
	    make_local_array(grace_data, global_reference)
	)
    .



:- local
	interrupt/0.

% Our attribute, It is used only to attach some data to the variable
:- meta_attribute(grace, [copy_term:copy_term_grace/2]).
:- call(define_struct(grace(range, id, matrix))).

copy_term_grace(X{AttrX}, Copy) :-
    -?->
    copy_term_grace(X, Copy, AttrX).

copy_term_grace(Var, Copy, grace with id:Id) :-
    (var(Id) ->
	var_id(Var, Id)
    ;
	true
    ),
    add_attribute(Copy, grace with [range:0, id:Id], grace).

tr_const(no_macro_expansion(default_prio), 9).
tr_const(no_macro_expansion(matrix_prio), 8).
tr_const(no_macro_expansion(display_prio), 7).
tr_const(no_macro_expansion(varstack_prio), 6).
tr_const(no_macro_expansion(run_prio), 6).

:-  call((
    define_macro(no_macro_expansion(default_prio), tr_const/2, []),
    define_macro(no_macro_expansion(matrix_prio), tr_const/2, []),
    define_macro(no_macro_expansion(display_prio), tr_const/2, []),
    define_macro(no_macro_expansion(varstack_prio), tr_const/2, []),
    define_macro(no_macro_expansion(run_prio), tr_const/2, []))).

% no 'ensure_loaded' because it is the same module
:- use_module(wake).
:- compile(matrices).
:- compile(label).
:- compile(expr).
:- compile(tkint).
:- compile(toolbox).
%:- compile(prop).
:- compile(options).

% The execution mode, one of:
%	step -	stop on every labeling call (when selecting a variable)
%	stepd -	step over every matrix domain update
%	run(P) - stop only on breakpoints, run with prio P
%	run_fast - stop only on breakpoints, no updates
:- make_local_array(mode).

:- make_local_array(solutions).
:- make_local_array(choices).
:- make_local_array(failures).
:- make_local_array(stop).
:- make_local_array(backward).
:- make_local_array(selected).
:- make_local_array(size).
:- make_local_array(goal).
:- make_local_array(de_count).
:- make_local_array(grace).
:- make_local_array(var_id).
:- make_local_array(first_fail).
:- make_local_array(cputime).
:- make_local_array(print_size), setval(print_size, 8).
:- make_local_array(optimize).
:- make_local_array(module).
:- make_local_array(startup).

:- local_record(call).
:- local_record(exit).
:- local_record(fail).
:- local_record(wake).
:- local_record(delay).
:- local_record(delay_goal).
:- local_record(label).		% to display the label node

:- set_flag(syntax_option, dense_output).
:- setval(grace, on).

:- op(700, xfx, [#=, ##, #\=, #>, #<, #>=, #<=, ::]).
:- op(600, xfx, ..).
:- op(100, fx, trace).		% for internal selective tracing


% We must redefine the unification handler to take our wake/0 when necessary
unify_handler([]) :-
    wake.
unify_handler([[Term|Attr]|List]) :-
    -?->
    unify_attributes(Term, Attr),
    unify_handler(List).

grace :-
    setval(grace, on),
    notrace.

nograce :-
    setval(grace, off),
    reset_global_state.

reset_global_state :-
    default_wake,
    reset_interrupt_handler(2).

status_init :-
    setval(mode, step),
    setval(solutions, 0),
    setval(choices, 0),
    setval(failures,0),
    setval(stop, 0),
    setval(backward, 0),
    setval(selected, 0),
    setval(goal, 1),
    setval(de_count, 0),
    setval(var_id, 1),
    setval(optimize, 0),
    setval(startup, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Initialization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- tool(grace_start/1, grace_start_body/2).

grace_start_body(Title, Module) :-
    getval(grace, off),
    !,
    set_data(d([], 0, 0, 0, 0)),
    setval(module, Module),
    default_options(Title).
grace_start_body(Title, Module) :-
    set_data(d([], 0, 0, 0, 0)),
    set_default_error_handler(11, unify_handler/1),
    set_error_handler(11, unify_handler/1),
    default_wake,
    block(start(Title, Module, cold), Tag, catch_exit(Tag, Title, Module)).

catch_exit(Tag, _, _) :-
    reset_global_state,
    printf("Resetting...\n", []),
    exit_block(Tag).

start(Title, Module, _When) :-
    status_init,
    reset_propagation_trace,
    set_suspension_counter(1),
    set_first_suspension(1),
    set_depth(0),
    (tcl_eval('wm title .', ["Grace"|_]) ->
	NewStart = fail
    ;
	check_protcl,
	NewStart = true,
	(tcl_eval('') ->
	    true
	;
	    tk([])
	),
	tcl_source(control),
	tcl_source(matrix),
	tcl_source(varstack),
	tcl_source(var),
	%tcl_source(display),
	tcl_source(constr),
	tcl_source(add_menu),
	%tcl_source(tree_ctr),
	%tcl_source(tree),
	%tcl_source(tree_smp),
	tcl_source(request),
	default_options(Title),
	init_options(Module),
	option(tk, init, InitS),
	tcl(InitS)
    ),
    (process_options ->
	true
    ;
	error(1, grace_start(Title))
    ),
    init_display(Title),
    set_priority(default_prio, 0),
    tcl_eval(['set grace_module ', Module]),
    setval(module, Module),
    message('Initializing...'),
    set_interrupt_handler(2, interrupt/0),
    reset_status,
    reset_optimization,
    print_status,
    last_scheduled(LS),
    last_suspension(LD),
    set_parent(p(label(start, 0), [], LS, LD)),
    step_mode,
    tcl_eval(update),
    message("Stopped").
start(Title, Module, _) :-
    getval(mode, M),
    step_mode,
    getval(solutions, Sol),
    (M = retry(start) ->
        message('Restarting'),
        Wait = nowait
    ;
    (Sol = 0; Sol = opt(0)) ->
	setval(solutions, 0),
	message('No solutions'),
	Wait = wait
    ;
	message('All solutions found'),
	Wait = wait
    ),
    print_status,
    tcl_eval(update),
    end_propagation,
    end_loop(Title, Wait, Module).

end_loop(Title, Wait, Module) :-
    (Wait = wait, handle_events -> true; true),
    (option(control, restart, "restart") ->
        Res = 1
    ;
	tcl_eval('request_user cv_restart "Restart" "Cancel" {Do you want to restart the program?}', Res)
    ),
    (Res = 1 ->
	start(Title, Module, restart)
    ;
	end_loop(Title, Wait, Module)
    ).

init_display(Title) :-
    option(control, geometry, CG),
    option(varstack, geometry, VG),
    tcl('grace_init {##} ## ##', [Title, CG, VG], New),
    tcl(enable_selections),
    tcl(stepd_reset),
    (New = 1 ->
	cold_init,
	selection_init,
	true
    ;
	true
    ).

cold_init :-
    option(control, display, DM),
    tcl('set cv_display ##', DM),
    (option(control, print_trace, 1) ->
	TM = 1,
	do_print_trace
    ;
	TM = 0,
	set_stream(susp, null),
	do_not_print_trace
    ).

print_status :-
    depth,
    backtracks,
    solutions,
    goal,
    delayed,
    cost,
    search_size,
    print_time.

% As long as we don't store them for each level
reset_status :-
    setval(solutions, 0),
    statistics(times, [T|_]),
    setval(cputime, T).

reset_optimization :-
    tcl('pack forget .lcost .lcostm'),
    setval(optimize, 0).

print_time :-
    statistics(times, [T|_]),
    Time is T - getval(cputime),
    tcl_eval(['.ltimem configure -text [format "%.2f" ', Time, ']']).

force_displays :-
    get_priority(P),
    set_priority(default_prio, 0),
    wake,
    true,
    set_priority(P, 0).

%
% Storing and accessing data in the global descriptor
%
store_matrices(M) :-
    set_data(1, M).

get_matrices(M) :-
    get_data(1, M).

set_depth(M) :-
    set_data(2, M).

get_depth(M) :-
    get_data(2, M).

set_parent(M) :-
    set_data(3, M).

get_parent(M) :-
    get_data(Stored),
    (functor(Stored, d, 4) ->
	arg(3, Stored, M)
    ;
	default_wake,
	printf(error,
	    "GRACE: resetting the waking routine, please restart.\n%b", []),
	abort
    ).

get_last_choice(M) :-
    get_data(4, M).

set_last_choice(M) :-
    set_data(4, M).

set_cost(C) :-
    set_data(5, C).

get_cost(C) :-
    get_data(5, C).

set_data(N, Data) :-
    get_data(Stored),
    setarg(N, Stored, Data).

get_data(N, Data) :-
    get_data(Stored),
    arg(N, Stored, Data).

:- get_flag(version, Ver), Ver @< '3.5' ->
	compile_term([

get_data(Data) :-
    get_global_variable(5, Data),

set_data(Data) :-
    set_global_variable(5, Stored)
	])
    ;
	compile_term([
get_data(Data) :-
    getval(grace_data, Data),

set_data(Data) :-
    setval(grace_data, Data)
	]).

store_matrix(M) :-
    get_matrices(L),
    store_matrices([M|L]).

inc_goal :-
    incval(goal).

inc_depth :-
    get_depth(M),
    M1 is M + 1,
    set_depth(M1).
    /*
    (M1 < 6 ->
	trace_depth(M1)
    ;
	true
    ).
    */

inc_solutions :-
    getval(solutions, S),
    (integer(S) ->
    	incval(solutions)
    ;
    	S =.. [F, N],
    	N1 is N + 1,
    	S1 =.. [F, N1],
    	setval(solutions, S1)
    ).

split_list(List, N, ListOfLists) :-
    split_list(List, 1, N, ListOfLists, L, L).

split_list([V|List], N, N, [Row|LoL], Row, [V]) :-
    !,
    split_list(List, 1, N, LoL, L, L).
split_list([V|List], I, N, LoL, Row, [V|L]) :-
    I =< N,
    !,
    I1 is I + 1,
    split_list(List, I1, N, LoL, Row, L).
split_list([], _, _, [], [], []) :-
    !.
split_list([], _, _, [Row], Row, []).

bell :-
    tcl_eval(bell).

check_protcl :-
   is_predicate_(protcl_version/1, tk),
   call(protcl_version(V), tk),
   V @>= '2.2',
   !.
check_protcl :-
    printf(error, "You need ProTcXl 2.2 or newer to run Grace\n%b", []),
    abort.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Optimisation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grace_minimize(Expression) :-
    do_minimize(all, Expression).

grace_minimize(Vars, Expression) :-
    do_minimize(Vars, Expression).

do_minimize(V, Expression) :-
    setval(optimize, 1),
    set_text('', '.lcostm'),
    tcl('pack .lcostm .lcost -in .ft -side right'),
    getval(solutions, Sol),
    setval(solutions, opt(0)),
    term_to_linear(Expression, List), 
    linear_term_range(List, Min, Max),
    Cost :: Min..Max,
    Cost #= Expression,
    set_cost(Cost),
    grace_stop("Starting optimization"),
    option(control, percent, Perc),
    option(control, branch_and_bound, BB),
    (V = all ->
    	all_variables(Vars)
    ;
    	Vars = V
    ),
    %tcl('.options.menu.optimize entryconfigure all -state disabled'),
    (BB = "continue" ->
	minimize((grace_label_list(Vars), grace_solution), Cost, Min, Max, Perc)
    ;
	min_max((display_all_matrices, grace_label_list(Vars), grace_solution), Cost, Min, Max, Perc)
    ),
    reset_optimization,
    setval(optimize, 2),
    setval(solutions, Sol),
    grace_solution.

dummy(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Explicit Propagation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default definition which is usually overriden by a user-defined one
% which is too constly to be applied on every step.
grace_propagate(_, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Solution
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grace_solution :-
    getval(grace, off), 
    !.
grace_solution :-
    getval(optimize, Opt),
    getval(mode, M),
    (M = retry(start) ->
        setval(optimize, 0),
        fail
    ;
    Opt = 0 ->
	Mess = 'Found a solution'
    ;
    	get_cost(Cost),
	(var(Cost) ->
	    dvar_domain(Cost, DC),
	    dom_range(DC, Cost, _)
	;
	    % Fix the problem with minimize
	    minimize_bound_check
	),
	(Opt = 1 ->
	    concat_string(['Solution with cost ', Cost], Mess),
	    set_text(Cost, '.lcostm')
	;
	    concat_string(['Optimal solution with cost ', Cost], Mess)
	)
    ),
    inc_solutions,
    single_option(control, display_solutions, Displ),
    (Displ = 1 ->
	force_displays,
	print_time,
	print_status
    ;
	solutions
    ),
    (
	(M = step		% stop always in step mode
	;
	getval(stop, 0),	% stop in pure run mode unless all solls
	option(control, all_solutions, 0)
	;
	Opt = 2)		% stop always on minimal solution
    ->
	display_all_matrices,
	step_mode,
	message(Mess),
	end_propagation,
	(Opt = 2 ->
	    setval(optimize, 0)
	;
	    true
	),
	handle_events
    ;
	message(Mess)
    ),
    (Displ = _ ->
	tcl_eval(update)
    ;
	true
    ),
    inc_goal,
    Opt == 1,
    (option(control, branch_and_bound, "restart") ->
	% and now reset everything because we lose our choice points
	getval(mode, Mode),
	setval(mode, back_min_max(Mode)),
	reset_varstack
    ;
    	true
    ).

%
% Default procedure to check some particular property of the solution
%
check_matrices.

grace_register_var(X) :-
    add_attributes(X, "", "").

%
% All predicates that process the grace attribute
%
get_attribute(_{X}, Attr) :-
    -?->
    Attr = X.

var_id(_{Attr}, Id) :-
    -?->
    Attr = with(grace, [id:Id]),
    (var(Id) ->
	getval(var_id, Id),
	incval(var_id)
    ;
	true
    ).

var_matrix(_{Attr}, Name) :-
    -?->
    Attr = grace with [matrix:Name],
    nonvar(Name).

reset_var(_{grace with [range: Low..High]}, N, I, J) :-
    -?->
    concat_string([Low, .., High], S),
    display_element(N, I, J, S).
reset_var(V, _, _, _) :-
    nonvar(V).

add_attributes(X, Name, Id) :-
    var(X),
    (is_domain(X) ->
	my_dom_range(X, Min, Max),      %MGW
	add_attribute(X, grace with [range: Min..Max, id:Id, matrix:Name], grace)
    ;
    	% A free vriable; suppose this is a dummy variable that should
    	% not be displayed. We ground it for convenience
    	X = dummy_var,
	tcl('m_ignore_var ##', Id)
    ).
add_attributes(X, _, _) :-
    nonvar(X).

my_dom_range(X,Min,Max) :-
      is_integer_domain(X), !,
      dvar_domain(X,Dom),
      dom_range(Dom,Min,Max).
my_dom_range(X,Min,Max) :-
      dom(X,Dom),
      sort(Dom,SDom),
      firstel(SDom,Min), 
      lastel(SDom,Max).

firstel([H|_],H).

lastel([Last],Last) :- !.
lastel([_|T],Last) :- lastel(T,Last).

reset_varstack :-
    get_depth(D),
    tcl_eval(['reset_varstack .vs.c ', D]).

reset_displays :-
    tcl_eval(['reset_displays .de']).

debug_handler_p(_, _) :- get_priority(P), writeln(P).
:- global debug_handler_p/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Stop
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grace_stop(_) :-
    getval(grace, off),
    !.
grace_stop(Mess) :-
    end_propagation,
    step_mode,
    print_status,
    message(Mess),
    handle_events.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Variable and Value Selection
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
selection_init :-
    tcl('.varsel.menu delete 0 last'),
    option(control, var_selections, VarList),
    add_var_selections(VarList),
    option(control, var_selection, VarSelect),
    set_var_selection(VarSelect),

    tcl('.valsel.menu delete 0 last'),
    option(control, value_selections, ValList),
    add_value_selections(ValList),
    option(control, value_selection, ValSelect),
    set_value_selection(ValSelect).

add_var_selections([]).
add_var_selections([[_, N]|L]) :-
    (tcl('add_var_selection {##}', [N]) -> true; true),
    add_var_selections(L).

set_var_selection(Index) :-
    single_option(control, var_selections, List),
    memberchk([Pred, Index], List),
    (Pred = M:N/2 ->
        Goal =.. [N, L, V],
        Call = call_explicit(Goal, M)
    ;
    	Pred = N/2,
    	Call =.. [N, L, V]
    ),
    compile_term(select_var(L, V) :- Call),
    (tcl('set var_selection {##}', Index) -> true; true).

add_value_selections([]).
add_value_selections([[_, N]|L]) :-
    (tcl('add_value_selection {##}', [N]) -> true; true),
    add_value_selections(L).

set_value_selection(Index) :-
    single_option(control, value_selections, List),
    memberchk([Pred, Index], List),
    (Pred = M:N/3 ->
        Goal =.. [N, V, L, L1],
        Call = call_explicit(Goal, M)
    ;
    	Pred = N/3,
    	Call =.. [N, V, L, L1]
    ),
    compile_term(select_value(V, L, L1) :- Call),
    (tcl('set var_selection {##}', Index) -> true; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% File initialisation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- status_init.

:- call_explicit(set_error_handler(333, x_handler/2), sepia_kernel).

%
% Default stuff until propagation included
%

init_propagation_trace.
reset_propagation_trace.
end_propagation.

trace_suspension(_, _, _, grace) :- !.
trace_suspension(_, Goal, _, _) :-
    our_goal(Goal),
    !.
trace_suspension(Port, Goal, _Mark, _) :-
    printf(susp, "%s %GDmw\n%b", [Port, Goal]).

