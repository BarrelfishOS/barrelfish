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
% Display Constraints
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(grace).
:- call(lib(fd)).

list_constraints(N, I, J) :-
    matrix_element(N, I, J, Var),
    setval(listed, listed(N, I, J)),
    delayed_goals(Var, DG),
    sort(DG, DGS),
    goals_to_tcl(DGS, List),
    option(constraints, geometry, CG),
    tcl('list_constraints .cl ## ##', [CG, List]),
    tk_next_event(E),
    % we don't pass the DG list because then the garbage collector could
    %  not get rid of it
    process_constraints(Var, E).

process_constraints(_, ["quit_constraints"]) :-
    !.
process_constraints(Var, ["add_constraint", I]) :-
    !,
    delayed_goals(Var, DG),
    sort(DG, DGS),
    find_goal(DGS, I, Goal),
    display_expression(Goal),
    tk_next_event(E),
    process_constraints(Var, E).
process_constraints(_, ["constraints", N, I, J]) :-
    !,
    list_constraints(N, I, J).
process_constraints(Var, _) :-			% we ignore everything else
    !,
    tk_next_event(E),
    process_constraints(Var, E).

find_goal([G|L], I, Goal) :-
    our_goal(G),
    !,
    find_goal(L, I, Goal).
find_goal([Goal|_], 0, Goal) :-
    !.
find_goal([_|L], I, Goal) :-
    I1 is I - 1,
    find_goal(L, I1, Goal).

display_expression(element(A, B, C, _, _)) :-
    !,
    qg_display_value(element(A, B, C), fd).
display_expression(fd_eq(L)) :-
    !,
    display_lin_value(L, "0 #=").
display_expression(fd_ge(L)) :-
    !,
    display_lin_value(L, "0 #>=").
display_expression(gec(X, K, Y, C)) :-
    !,
    qg_display_value(X + K*Y + C, "0 #<=").
display_expression(fd_ineq(L)) :-
    !,
    display_lin_value(L, "0 ##").
display_expression(Goal) :-
    Goal = gec_ent(_, _, _, _, _),
    !,
    qg_display_value(Goal, "#>=").
display_expression(Goal) :-
    functor(Goal, Name, _),
    qg_display_value(Goal, Name).

our_goal(print_delay(_, _, _, _)).
our_goal(stepd_delay(_, _, _)).
our_goal(breakpoint(_, _, _)).
our_goal(update_stack_variable(_, _)).
our_goal(display_inst(_, _, _)).
our_goal(constrain_max_index(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Print Expressions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qg_display_value(_, _) :-
    getval(grace, off),
    !.
qg_display_value(Expr, Name) :-
    init_de(Name, W),
    (nonvar(Expr), term_to_linear(Expr, List) ->
	display_list(List, 1, W)
    ;
	display_expression(Expr, W)
    ).

display_lin_value(List, Name) :-
    init_de(Name, W),
    display_list(List, 0, W).

init_de(Name, W) :-
    getval(de_count, DC),
    incval(de_count),
    concat_string(['.de.', DC], W),
    tcl_eval(['de_init ', W, ' {', Name, '}']).

display_list([K*Var|L], J, W) :-
    !,
    (K = 1 ->
	Cf = "+"
    ;
    K > 0 ->
	concat_string([+, K, *], Cf)
    ;
    K = -1 ->
	Cf = "-"
    ;
	concat_string([K, *], Cf)
    ),
    var_link(Var, W, J, VarId, Link),
    el_to_const(Var, DomS, _),
    tcl_eval(['de_append_str ', W, ' ', J, ' {', Cf, '}']),
    tcl_eval(['de_append_var ', W, ' ', J, ' {', DomS, '} ', Link]),
    print_delay(Var, DomS, VarId, display_prio),
    make_suspension(display_inst(Var, W, J), display_prio, Susp),
    insert_suspension(Var, Susp, inst of suspend, suspend),
    J1 is J + 1,
    display_list(L, J1, W).
display_list([K|L], J, W) :-
    integer(K),
    tcl_eval(['de_update_inst ', W, ' ', K]),
    display_list(L, J, W).
display_list([], _, W) :-
    tcl_eval(['de_check_inst ', W]).

display_inst(Var, W, J) :-
    (tcl_eval(['de_var_inst_save ', W, ' ', J], [IntVal, Before]) ->
	update_de_inst(Var, W, J, IntVal, Before)
    ;
	true
    ).

update_de_inst(Var, W, J, Val, _) :-
    tcl_eval(['catch {de_var_inst ', W, ' ', J, ' ', Var, ' ', Val, '}']).
update_de_inst(_, W, J, Val, Before) :-
    tcl_eval(['catch {de_var_restore ', W, ' ', J, ' ', Val, ' ', Before, '}']),
    fail.

display_expression(Expr, W) :-
    decompose_expr(Expr, List, _),
    display_expression_list(List, 1, 1, W).

display_expression_list([], _, _, W) :-
    tcl_eval(['de_check_inst ', W]).
display_expression_list([H|T], Jv, Ji, W) :-
    (var(H) ->
	var_link(H, W, Jv, VarId, Link),
	el_to_const(H, DomS, _),
	tcl_eval(['de_append_var ', W, ' ', Jv, ' {', DomS, '} ', Link]),
	print_delay(H, DomS, VarId, display_prio),
	Jv1 is Jv + 1,
	Ji1 = Ji
    ;
	tcl_eval(['de_append_str ', W, ' ', Ji, ' {', H, '}']),
	Ji1 is Ji + 1,
	Jv1 = Jv
    ),
    display_expression_list(T, Jv1, Ji1, W).

var_link(Var, W, J, VarId, Link) :-
    concat_string([W, ., 'v', J], VarId),
    (find_variable(Var, Type, X, Y) ->
	concat_string([., Type, ., X, ., Y], Link),
	get_attribute(Var, grace with [id:Link])
    ;
	(get_attribute(Var, grace with [id:Link]) ->
	    (nonvar(Link) ->
		true
	    ;
		VarId = Link
	    )
	;
	    VarId = Link,
	    add_attribute(Var, grace with [id:Link], grace)
	)
    ).

