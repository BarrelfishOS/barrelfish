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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Description:	ECLiPSe best-first search library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Andrew Eremin, IC-Parc
% Version:      $Id: bfs.ecl,v 1.1 2012/07/31 02:17:06 jschimpf Exp $
%
% ----------------------------------------------------------------------


% ----------------------------------------------------------------------
:- module(bfs).

:- lib(constraint_pools).
:- lib(hash).
:- lib(eplex).
:- lib(n_trees).

% ----------------------------------------------------------------------
%
% Meta-attribute
%
% ----------------------------------------------------------------------

:- meta_attribute(bfs, [unify:unify_bfs/2]).

:- local struct(
                bfs(
                    optimal_val,
                    type,
                    node_info,
                    pseudocost:pseudocost,
                    solver,
                    next
                   )
               ).

% ----------------------------------------------------------------------
%
% Bfs var node info structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_node_info(
                              id,   % int: node number
                              lo,   % float: node lower bound
                              hi,   % float: node upper bound
                              val,  % float: node optimal solution value
                              rc    % float: node reduced cost
                             )
               ).

% ----------------------------------------------------------------------
%
% Pools
%
% ----------------------------------------------------------------------

:- export bfs_instance/1.	 % bfs_instance(+PoolName)

:- local struct(bfs_constraint_type(branch, global_cut)).

% ----------------------------------------------------------------------
%
% Best-first search tree structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_tree(
                         pool,           % the pool with which the
                                         % solver is associated
                         module,         % caller module
                         
                         data,           % arbitrary prob data that
                                         % branches may want to access:
                                         % probably variables in some
                                         % structured form since
                                         % branches are stored over
                                         % failure and cannot contain
                                         % explicit vars
                         feas_check,     % the arbitrary global
                                         % feasibility check
                                         % (intention is other than
                                         % variable integrality)
                         
                         shelf,          % ECLiPSe shelf for storing
                                         % various results over failures
                         cost,           % var: variable for the
                                         %      optimal cost
                         integral_obj,   % atom: yes or no, is the cost
                                         %       of all feasible
                                         %       solutions integral?
                         int_tolerance,  % float: integrality tolerance
                         sense,          % min or max: optimisation sense
                         node_susp,      % suspension list containing
                                         % the suspension for the node 
                                         % solver 
                         vars,           % []: list of all vars currently
                                         %     belonging to this tree
                         root_node,      % n_tree: the root of the
                                         %         search tree
                                         %         represented as a
                                         %         n_tree
                         next_node_no,   % int: the id for the next
                                         %      node created
                         best_bound,     % float: current best global
                                         %        feasible solution
                                         %        objective value
                         gubs,           % [GUB1,...,GUBm]: list of
                                         %    bfs_gub structures for
                                         %    the GUBs of this problem
                         stats:stats,    % structure holding
                                         % statistics info
                         info_messages,  % on, off: info message status
                                         % Search Control: node selection
                         node_select,    % atom: best_first, depth_first,
                                         %       best_estimate
                                         %       node selection criteria
                                         % Search Control: problem division
                         alpha_min,      % float: constants used in
                         alpha_max,      % float: utilising variable
                                         %        up/down estimations
                                         %        for branching:
                                         %        variable estimation =
                                         %         alpha_min *
                                         %          min(up/down est) +
                                         %         alpha_max *
                                         %          max(up/down est)
                         beta_const,     % float: constants used in
                         beta_pc,        % float: calculating variable
                         beta_lb,        % float: up/down estimations:
                                         %        up/down estimation = 
                                         %         beta_const + 
                                         %         beta_pc * pseudocost + 
                                         %         beta_lb * lowerbound
                         pcd_average,    % float: average observed
                                         %        down-branch pseudocost
                         pcd_count,      % int: number of observed
                                         %      down-branches
                         pcu_average,    % float: average observed
                                         %        up-branch pseudocost
                         pcu_count,      % int: number of observed
                                         %      up-branches
                         pc_init,        % atom: average,calculated,cost:
                                         %       pseudocost initialization
                                         %       method:
                                         %       set to current average,
                                         %       calulated,
                                         %       set to objective cost
                                         %       coefficient
                         pc_ratio,       % float: ratio in (0.0..1.0)
                                         %        of max work in calculating
                                         %        pseudocosts to root node
                                         %        solution work
                         pc_update,      % atom: average,first,last:
                                         %       pseudocost update method
                                         %       set to average/first/last
                                         %       of observed degradations
                         lb_time         % int: time limit in seconds
                                         %      for lower bound calculation
                        )
               ).

% ----------------------------------------------------------------------
%
% bfs_tree shelf structure
%
% ----------------------------------------------------------------------

:- local struct(bfs_shelf(branches, global_cuts, info)).

% ----------------------------------------------------------------------
%
% Branch-and-Price search tree node structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_node(
                         id,          % int: node number
                         state,       % arbitrary local state for the
                                      % node solver
                         rank,        % node ranking for node selection
                         objval,      % float: relaxed optimal
                                      %        solution value
                         parent_obj,  % float: relaxed optimal
                                      %        solution value of
                                      %        parent node
                                      % these two are needed when
                                      % using pseudocost-based
                                      % methods
                         pc_update,   % pseudocost to update after
                                      % solving node: [GUBNo, BPNo, Dir]
                         frac_vars,   % []: integer vars with fractional
                                      %     node optimal solution 
                         branches     % []: current branching decisions
                        )
               ).

% ----------------------------------------------------------------------
%
% Generalised Upper/Lower bound structure
%
% ----------------------------------------------------------------------

:- local struct(
                bfs_gub(
                        vars,
                        refs,
                        pseudocosts
                       )
               ).

% ----------------------------------------------------------------------
%
% Pseudocost structure
%
% ----------------------------------------------------------------------

:- local struct(
                pseudocost(
                           pcd,
                           pcd_count,
                           pcu,
                           pcu_count
                          )
               ).

% ----------------------------------------------------------------------
%
% Statistics structure
%
% ----------------------------------------------------------------------

:- local struct(
                stats(
                      start_time,
                      total_time,
                      first_sol_time,
                      opt_sol_time,
                      no_sols,
                      solve_time,
                      separate_time,
                      nodes_separated,
                      nodes_solved
                     )
               ).

% ----------------------------------------------------------------------
%
% exports
%
% ----------------------------------------------------------------------

:- export bfs_integers/2.
:- export bfs_node_info/3.
:- export bfs_node_info/6.
:- export bfs_statistics/1.
:- export bfs_node_cost/2.
:- export bfs_var_get/4.
:- export bfs_get/3.
:- export bfs_set/3.
:- export bfs_impose_node_state/2.
:- export bfs_solver_setup/3.
:- export bfs_solver_setup/4.
:- export bfs_solve/2.
:- export bfs_branch/2.
:- export bfs_branch/3.
:- export bfs_global_cut/2.
:- export bfs_minimize_eplex_node/2.
:- export bfs_update_pseudocosts/1.
:- export bfs_deg_est/2.
:- export bfs_strong/2.
:- export bfs_enhanced/2.
:- export bfs_fracvar/1.

% ----------------------------------------------------------------------
%
% bfs attribute handlers
%
% ----------------------------------------------------------------------

unify_bfs(_, Attr) :-
        var(Attr).                   % Ignore if not a bfs var
unify_bfs(Term, Attr) :-
        compound(Attr),
        unify_term_bfs(Term, Attr).

:- mode unify_term_bfs(?, +).
unify_term_bfs(X, Attr) :-
        nonvar(X),                   % bfs var and NONVAR - instantiated
        instantiation_deviates_for_handle(Attr, X).
unify_term_bfs(Y{bfs:AttrY}, AttrX) :-
        -?->
        unify_bfs_bfs(Y, AttrY, AttrX).

unify_bfs_bfs(_, AttrY, AttrX) :-
        var(AttrY),	            % No attribute for this extension
        AttrX = AttrY.	            % Transfer the attribute
unify_bfs_bfs(_, AttrY, AttrX) :-
        nonvar(AttrY),              % bfs var and bfs var
        unify_bfs_handles(AttrX, AttrY).

instantiation_deviates_for_handle(ThisAttr, X) :-
        ( compound(ThisAttr) ->
              ThisAttr = bfs with [
                                   solver:Handle,
                                   optimal_val:Val,
                                   next:NextAttr
                                  ],
              ( X = Val -> % instantiated to its optimal_val
                    true
              ; bfs_next_node(Handle, _SubTree) ->
                  % Still processing: the optimal val is only
                  % current optimal in the tree, and quite
                  % likely the instantiation has been caused
                  % by the tree search at another node, so
                  % do not rewake the solver now because it
                  % could fail incorrectly.
                  true
              ;
                  % finished solving so this was a true optimal,
                  % wake solver
                  schedule_suspensions(node_susp of bfs_tree, Handle),
                  wake
              ),
              instantiation_deviates_for_handle(NextAttr, X)
	;    
              % chain terminated by atom 'end'
              true
        ).

unify_bfs_handles(ThisAttrX, AttrY) :-
        ThisAttrX = bfs with [
                              solver:Handle,
                              next:NextAttrX
                             ],
        remove_bfs_attr_for_handle(Handle, AttrY, ThisAttrY, NextAttrY),
        ( compound(ThisAttrY) ->
              % two variables in the same solver are unified,
              % send an equality constraint to the solver and wake it
              % Note to AE: how do I get back the vars?
              schedule_suspensions(node_susp of bfs_tree, Handle),
              wake
        ;
              % Y has no attribute for Pool
              ThisAttrY = end
        ),
        % continue with the rest of X and Ys chains
        unify_bfs_handles(NextAttrX, ThisAttrX, NextAttrY).

unify_bfs_handles(ThisAttrX, Attr0, AttrY) :-
        ( compound(ThisAttrX) ->
              ( compound(AttrY) ->
                    ThisAttrX = bfs with [
                                          solver:Handle,
                                          next:NextAttrX
                                         ],
                    remove_bfs_attr_for_handle(Handle, AttrY, ThisAttrY, NextAttrY),
                    ( compound(ThisAttrY) ->
                          % two variables in the same solver are unified,
                          % send an equality constraint for the two columns
                          % to the external solver and wake it
                          % Note to AE: how do I get back the vars?
                          schedule_suspensions(node_susp of bfs_tree, Handle),
                          wake
                    ;
                          % Y has no attribute for Pool
                          ThisAttrY = end
                    ),
                    % continue with the rest of X and Ys chains
                    unify_bfs_handles(NextAttrX, ThisAttrX, NextAttrY)
              ;
                    % Ys chain terminated by atom'end'
                    true
              )
        ;
              % Xs chain terminated by atom 'end'
              % put the rest of Ys chain here
              setarg(next of bfs, Attr0, AttrY)
        ).

% From a bfs_attr, removes the attribute corresponding to that for the
% first argument form the chain and returns it. Fails if none found. 
remove_bfs_attr_for_handle(Handle, ThisAttr, Attr, RestAttr) :-
        % no need to test for var(ThisAttr) in chain
        ThisAttr = bfs with [solver:ThisHandle, next:NextAttr],
	(ThisHandle == Handle ->
             RestAttr = NextAttr,
             setarg(next of bfs, ThisAttr, end),
	     Attr = ThisAttr
	;    
             RestAttr = ThisAttr,
	     dechain_bfs_attr_for_handle1(Handle, NextAttr, ThisAttr, Attr)
	).
        
dechain_bfs_attr_for_handle1(Handle, ThisAttr, Attr0, Attr) :-
        % no need to test for var(ThisAttr) in chain
        ( ThisAttr = bfs with [solver:ThisHandle, next:NextAttr] ->
              (ThisHandle == Handle ->
                   setarg(next of bfs, Attr0, NextAttr),
                   setarg(next of bfs, ThisAttr, end),
                   Attr = ThisAttr
              ;    
                   dechain_bfs_attr_for_handle1(Handle, NextAttr, ThisAttr, Attr)
              )
        ;     % chain terminated by atom 'end'
              ThisAttr = Attr
        ).

% ----------------------------------------------------------------------

get_bfs_attr(X{bfs:Attr0}, Handle, Attr) ?-
        ( var(Attr0) ->
              new_bfs_attr(X, Handle, Attr)
        ;
              Attr0 = bfs with [solver:Handle0,next:Next],
              % should not fail unless Attr0 incorrect
              ( Handle0 == Handle ->
                    Attr = Attr0
              ;
                    get_bfs_attr1(Next, Attr0, Handle, Attr)
              )
        ).
get_bfs_attr(X, Handle, Attr) :-           % make a new bfs variable
        free(X),
        new_bfs_attr(X, Handle, Attr).

get_bfs_attr1(ThisAttr, Attr0, Handle, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_bfs_attrstruct(Handle, Attr),
	setarg(next of bfs, Attr0, Attr).
get_bfs_attr1(ThisAttr, _Attr0, Handle, Attr) :-
        ThisAttr = bfs with [solver:Handle0,next:Next],
        ( Handle0 == Handle ->
              Attr = ThisAttr
        ;
              get_bfs_attr1(Next, ThisAttr, Handle, Attr)
        ).

new_bfs_attr(X, Handle, Attr) :-                       % make a new bfs variable:
        new_bfs_attrstruct(Handle, Attr),
        add_attribute(X, Attr, bfs),                 % add a bfs attribute
        Handle = bfs_tree{vars:Vars},
        ( var(Vars) -> Vars0 = [X] ; Vars0 = [X|Vars] ),
        setarg(vars of bfs_tree, Handle, Vars0), % add it to the bfs tree
        true.

:- mode new_bfs_attrstruct(+,-).
new_bfs_attrstruct(Handle, Attr) :-
        hash_create(NodeInfoHashTable),
        Attr = bfs with [
                         type: real,
                         node_info:NodeInfoHashTable,
                         pcu: 0,
                         pcu_count: none,
                         pcd: 0,
                         pcd_count: none,
                         solver:Handle,
                         next:end
                        ].

% From a bfs_attr, searches for the attribute corresponding to that for the
% first argument. Fails if none found. 
get_bfs_attr_for_handle(Handle, Attr0, Attr) :-
        compound(Attr0), 
	get_bfs_attr_for_handle1(Handle, Attr0, Attr).

get_bfs_attr_for_handle1(Handle, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = bfs with [solver:Handle0,next:NextAttr],
	(Handle0 == Handle ->
	     Attr0 = Attr
	;    
	     get_bfs_attr_for_handle1(Handle, NextAttr, Attr)
	).

bfs_get_node_info(Var, NodeId, Val, _Handle) :-
        nonvar(Var), !,
        Val = bfs_node_info with [
                                  id:NodeId,
                                  lo:Var,
                                  hi:Var,
                                  val:Var,
                                  rc:0
                                 ].
bfs_get_node_info(Var{bfs:Attr0}, NodeId, Val, Handle) ?-
        get_bfs_attr_for_handle(Handle, Attr0, Attr),
        Attr = bfs with [
                         type:Type,
                         node_info:Vals
                        ],
        nonvar(NodeId),
        ( hash_get(Vals, NodeId, Val) ->
            % found a bfs_node_info struct for this node
            true
        ;
            % no bfs_node_info struct for this node,
            % implicitly has original bounds, 0 val and rc
            Val = bfs_node_info with [
                                      id:NodeId,
                                      lo:NLo,
                                      hi:NHi,
                                      val:0,
                                      rc:0
                                     ],
            get_var_bounds(Var, Lo, Hi),
            ( Type == integer ->
                  NLo is ceiling(Lo),
                  NHi is floor(Hi)
            ;
                  NLo = Lo,
                  NHi = Hi
            )
        ).

update_node_info(Val, V) :-
        Val = bfs_node_info with [
                                  lo:Lo,
                                  hi:Hi,
                                  val:T,
                                  rc:RC
                                 ],
        V = bfs_node_info with [
                                lo:L,
                                hi:H
                               ],
        ( var(Lo) -> true
        ; var(L) -> setarg(lo of bfs_node_info, V, Lo)
        ; NL is max(Lo, L), setarg(lo of bfs_node_info, V, NL) ),
        ( var(Hi) -> true
        ; var(H) -> setarg(hi of bfs_node_info, V, Hi)
        ; NH is min(Hi, H), setarg(hi of bfs_node_info, V, NH) ),
        ( var(T) -> true ; setarg(val of bfs_node_info, V, T) ),
        ( var(RC) -> true ; setarg(rc of bfs_node_info, V, RC) ).

bfs_new_node_info(Var, Node, NodeInfo, Handle) :-
	get_bfs_attr(Var, Handle, Attr),
        Attr = bfs{type:T, node_info:NodeInfoHash},
        Node = bfs_node{id:Id, frac_vars:Vars},
        NodeInfo = bfs_node_info{id:Id, val:Val},
        ( ( nonvar(Val), T == integer,
            Handle = bfs_tree with int_tolerance:IntTol,
            abs(Val-round(Val)) > IntTol ) ->
              setarg(frac_vars of bfs_node, Node, [Var:Val|Vars])
        ;
              true
        ),
        ( hash_get(NodeInfoHash, Id, NodeInfo0) ->
            update_node_info(NodeInfo, NodeInfo0)
        ;
            hash_add(NodeInfoHash, Id, NodeInfo)
        ).

% bfs_node_info/6 : for low-level handles
bfs_node_info(Handle, Var, Lo, Hi, Val, RC) :-
        ( var(Handle) ->
            error(4, bfs_node_info(Handle, Var, Lo, Hi, Val, RC))
        ; Handle = bfs_tree with [] ->
            Info = bfs_node_info with [lo:Lo, hi:Hi, val:Val, rc:RC],
            bfs_node_info_body(Handle, Var, Info)
        ;
            % not a proper bfs handle
            error(5, bfs_node_info(Handle, Var, Lo, Hi, Val, RC))
        ).

% node_info/5 : for pools
bfs_node_info1(Var, Lo, Hi, Val, RC, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:node_info(Var, Lo, Hi, Val, RC)]),
            abort
        ;
            Info = bfs_node_info with [lo:Lo, hi:Hi, val:Val, rc:RC],
            bfs_node_info_body(Handle, Var, Info)
        ).

% bfs_node_info/3 : for low-level handles
bfs_node_info(Handle, Var, Info) :-
        ( var(Handle) ->
            error(4, bfs_node_info(Handle, Var, Info))
        ; Handle = bfs_tree with [] ->
            bfs_node_info_body(Handle, Var, Info)
        ;
            % not a proper bfs handle
            error(5, bfs_node_info(Handle, Var, Info))
        ).

% node_info/2 : for pools
bfs_node_info1(Var, Info, Pool) :-
        get_pool_item(Pool, Handle), 
	( Handle == 0 ->
              printf(error, "Bfs instance has no tree set up in %w%n",
                     [Pool:node_info(Var, Info)]),
              abort
	;
              bfs_node_info_body(Handle, Var, Info)
        ).

bfs_node_info_body(Handle, Var, Val) :-
	( bfs_next_node(Handle, SubTree),
          n_trees:n_tree_get(data, SubTree, Node) ->
              ( var(Val) ->
                    Node = bfs_node with id:Id,
                    bfs_get_node_info(Var, Id, Val, Handle)
              ;
                    bfs_new_node_info(Var, Node, Val, Handle)
              )
        ;
              printf(error, "Bfs tree has no current node in %w%n",
                     [bfs_node_info_body(Handle, Var, Val)]),
              abort
        ).

% bfs_var_get/4: for low-level handles
bfs_var_get(Handle, Var, What, Val) :-
        ( var(Handle) ->
            error(4, bfs_var_get(Handle, Var, What, Val))
        ; var(What) ->
            error(4, bfs_var_get(Handle, Var, What, Val))
        ; Handle = bfs_tree{} ->
            bfs_var_get_body(Handle, Var, What, Val)
        ;
            error(5, bfs_var_get(Handle, Var, What, Val))
        ).

% var_get/3: for pools
bfs_var_get1(Var, What, Val, Pool) :-
        ( var(What) ->
            error(4, Pool:var_get(Var, What, Val))
        ;
            get_pool_item(Pool, Handle), 
            ( Handle == 0 ->
                printf(error, "Bfs instance has no tree set up in %w%n",
                       [Pool:var_get(Var, What, Val)]),
                abort
            ;
                bfs_var_get_body(Handle, Var, What, Val)
            )
        ).

bfs_var_get_body(Handle, Var, node_val, Val) ?- !,
        ( bfs_next_node(Handle, SubTree),
          n_trees:n_tree_get(data, SubTree, bfs_node{id:Id}) ->
              ( number(Var) ->
                  Val = Var
              ;
                  bfs_get_node_info(Var, Id, bfs_node_info{val:Val},
                                    Handle)
              )
        ;
              printf(error, "Bfs tree has no current node in %w%n",
                     [bfs_var_get_body(Handle, Var, node_val, Val)]),
              abort
        ).
bfs_var_get_body(Handle, Var, node_bounds, Val) ?- !,
        ( bfs_next_node(Handle, SubTree),
          n_trees:n_tree_get(data, SubTree, bfs_node{id:Id}) ->
              ( number(Var) ->
                  Val = Var..Var
              ;
                  bfs_get_node_info(Var, Id, bfs_node_info{lo:Lo,hi:Hi},
                                    Handle),
                  Val = Lo..Hi
              )
        ;
              printf(error, "Bfs tree has no current node in %w%n",
                     [bfs_var_get_body(Handle, Var, node_bounds, Val)]),
              abort
        ).
bfs_var_get_body(Handle, Var, node_rc, Val) ?- !,
        ( bfs_next_node(Handle, SubTree),
          n_trees:n_tree_get(data, SubTree, bfs_node{id:Id}) ->
              ( number(Var) -> Val = 0
              ; bfs_get_node_info(Var, Id, bfs_node_info{rc:Val}, Handle) )
        ;
              printf(error, "Bfs tree has no current node in %w%n",
                     [bfs_var_get_body(Handle, Var, node_rc, Val)]),
              abort
        ).
bfs_var_get_body(Handle, Var, optimal_val, Val) ?- !,
        ( number(Var) ->
              Val = Var
        ;
              get_bfs_attr(Var, Handle, bfs{optimal_val:Val})
        ).
bfs_var_get_body(Handle, Var, type, Val) ?- !,
        ( integer(Var) ->
              Val = integer
        ; number(Var) ->
              Val = real
        ;
              get_bfs_attr(Var, Handle, bfs{type:Val})
        ).
bfs_var_get_body(Handle, Var, What, Val) :-
	error(6, bfs_var_get_body(Handle, Var, What, Val)).

bfs_var_set(_{bfs:Attr0}, optimal_val, Val, Handle) ?- !,
        get_bfs_attr_for_handle(Handle, Attr0, Attr),
        ( Attr = bfs with type: integer ->
              Opt is fix(round(float(Val)))
        ;
              Opt = Val
        ),
        setarg(optimal_val of bfs, Attr, Opt).
bfs_var_set(Var, What, Val, Handle) :-
	error(6, bfs_var_set(Var, What, Val, Handle)).

bfs_integers1(Ints, Pool) :-
        get_pool_handle(Handle, Pool),
        bfs_integers(Handle, Ints).

bfs_integers(Handle, Ints) :-
        ( var(Ints) ->
              get_bfs_attr(Ints, Handle, Attr),
              setarg(type of bfs, Attr, integer)
        ;
              (
                  foreach(Var, Ints),
                  param(Handle)
              do
                  get_bfs_attr(Var, Handle, Attr),
                  setarg(type of bfs, Attr, integer)
              )
        ).

% bfs_branch/2: for low-level handles
bfs_branch(Handle, Branch) :-
        ( var(Handle) ->
            error(4, bfs_branch(Handle, Branch))
        ; Handle = bfs_tree{} ->
            bfs_branch_body(Handle, [0, Branch])
        ;
            error(5, bfs_branch(Handle, Branch))
        ).

% bfs_branch/1: for pools
bfs_branch1(Branch, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:bfs_branch(Branch)]),
            abort
        ;
            bfs_branch_body(Handle, [0, Branch])
        ).

% bfs_branch/3: for low-level handles
bfs_branch(Handle, Score, Branch) :-
        ( var(Handle) ->
            error(4, bfs_branch(Handle, Score, Branch))
        ; Handle = bfs_tree{} ->
            bfs_branch_body(Handle, [Score, Branch])
        ;
            error(5, bfs_branch(Handle, Score, Branch))
        ).

% branch/2: for pools
bfs_branch1(Score, Branch, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:branch(Score, Branch)]),
            abort
        ;
            bfs_branch_body(Handle, [Score, Branch])
        ).

bfs_branch_body(bfs_tree{shelf:Shelf}, Branch) :-
        shelf_get(Shelf, branches of bfs_shelf, Branches),
        shelf_set(Shelf, branches of bfs_shelf, [Branch|Branches]).

% bfs_global_cut/2: for low-level handles
bfs_global_cut(Handle, GlobalCut) :-
        ( var(Handle) ->
            error(4, bfs_global_cut(Handle, GlobalCut))
        ; Handle = bfs_tree{} ->
            bfs_global_cut_body(Handle, GlobalCut)
        ;
            error(5, bfs_global_cut(Handle, GlobalCut))
        ).

% global_cut/1: for pools
bfs_global_cut1(GlobalCut, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:global_cut(GlobalCut)]),
            abort
        ;
            bfs_global_cut_body(Handle, GlobalCut)
        ).

bfs_global_cut_body(bfs_tree{shelf:Shelf}, GlobalCut) :-
        shelf_get(Shelf, global_cuts of bfs_shelf, GlobalCuts),
        shelf_set(Shelf, global_cuts of bfs_shelf, [GlobalCut|GlobalCuts]).

bfs_info_message(bfs_tree with info_messages:OnOff, String, Vars) :-
        ( OnOff == on -> printf(String, Vars) ; true ).

% bfs_statistics/1: for low-level handles
bfs_statistics(Handle) :-
        ( var(Handle) ->
            error(4, bfs_statistics(Handle))
        ; Handle = bfs_tree{} ->
            bfs_statistics_body(Handle)
        ;
            error(5, bfs_statistics(Handle))
        ).

% /0: for pools
bfs_statistics1(Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:statistics]),
            abort
        ;
            bfs_statistics_body(Handle)
        ).

bfs_statistics_body(bfs_tree{next_node_no:NTotal, stats:Stats}) :-
        Stats = stats{total_time:TTotal,
                      nodes_solved:NSolve,
                      solve_time:TSolve,
                      nodes_separated:NSep,
                      separate_time:TSep,
                      no_sols:NSol,
                      first_sol_time:TFirst,
                      opt_sol_time:TOpt
                     },
        printf("%n%nbfs statistics:%n%tTotal nodes:"
               " %d%n%tTotal time: %w" 
               "%n%tNodes solved: %d%n%tNode solution time: %w"
               "%n%tNodes separated: %d%n%tNode separation time: %w"
               "%n%tSolutions found: %d%n%tFirst solution time: %w"
               "%n%tOptimal solution time: %w%n%n",
               [NTotal, TTotal, NSolve, TSolve,
                NSep, TSep, NSol, TFirst, TOpt]).

% bfs_node_cost/2: for low-level handles
bfs_node_cost(Handle, Val) :-
        ( var(Handle) ->
            error(4, bfs_node_cost(Handle, Val))
        ; var(Val) ->
            error(4, bfs_node_cost(Handle, Val))
        ; Handle = bfs_tree{} ->
            bfs_node_cost_body(Handle, Val)
        ;
            error(5, bfs_node_cost(Handle, Val))
        ).

% node_cost/1: for pools
bfs_node_cost1(Val, Pool) :-
        ( var(Val) ->
            error(4, Pool:node_cost(Val))
        ;
            get_pool_item(Pool, Handle), 
            ( Handle == 0 ->
                printf(error, "Bfs instance has no tree set up in %w%n",
                       [Pool:node_cost(Val)]),
                abort
            ;
                bfs_node_cost_body(Handle, Val)
            )
        ).

bfs_node_cost_body(Handle, NodeCost) :-
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        setarg(objval of bfs_node, Node, NodeCost).

% bfs_get/3: for low-level handles
bfs_get(Handle, What, Val) :-
        ( var(Handle) ->
            error(4, bfs_get(Handle, What, Val))
        ; var(What) ->
            error(4, bfs_get(Handle, What, Val))
        ; Handle = bfs_tree{} ->
            bfs_get_body(Handle, What, Val)
        ;
            error(5, bfs_get(Handle, What, Val))
        ).

% get/2: for pools
bfs_get1(What, Val, Pool) :-
        ( var(What) ->
            error(4, Pool:get(What, Val))
        ;
            get_pool_item(Pool, Handle), 
            ( Handle == 0 ->
                printf(error, "Bfs instance has no tree set up in %w%n",
                       [Pool:get(What, Val)]),
                abort
            ;
                bfs_get_body(Handle, What, Val)
            )
        ).

bfs_next_node(Handle, Node) :-
        Handle = bfs_tree{
                          root_node:Root,
                          best_bound:Bound,
                          node_select:Method,
                          integral_obj:IntObj
                         },
        ( Method == depth_first ->
            n_trees:next(dfs, Root, Node0)
        ; Method == two_phase ->
            n_trees:next(dfs, Root, Node0)
        ; Method == best_first ->
            n_trees:next(bfs, Root, Node0)
        ; Method == best_estimate ->
            n_trees:next(bfs, Root, Node0)
        ),
        ( %n_trees:n_tree_get(data, Node0, bfs_node{parent_obj:ParentObj}),
          %fathom_by_bounds(IntObj, ParentObj, Bound) ->
          %  n_trees:n_tree_fathom(Node0),
          %  bfs_next_node(Handle, Node)
        %;
            Node = Node0
        ).

bfs_get_body(Handle, frac_vars, Val) ?- !,
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, bfs_node{frac_vars:Vars}),
        ( foreach(Var:_, Vars), foreach(Var, Val) do true ).
bfs_get_body(Handle, branches, Val) ?- !,
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, bfs_node{branches:Val}).
bfs_get_body(Handle, data, Val) ?- !,
        Handle = bfs_tree{data:Val}.
bfs_get_body(Handle, node_count, Val) ?- !,
        Handle = bfs_tree{next_node_no:Val}.
bfs_get_body(Handle, What, Val) :-
	error(6, bfs_get_body(Handle, What, Val)).

% bfs_set/3: for low-level handles
bfs_set(Handle, What, Val) :-
        ( var(Handle) ->
            error(4, bfs_set(Handle, What, Val))
        ; var(What) ->
            error(4, bfs_set(Handle, What, Val))
        ; var(Val) ->
            error(4, bfs_set(Handle, What, Val))
        ; Handle = bfs_tree{} ->
            bfs_set_body(Handle, What, Val)
        ;
            error(5, bfs_set(Handle, What, Val))
        ).

% set/2: for pools
bfs_set1(What, Val, Pool) :-
        ( var(What) ->
            error(4, Pool:set(What, Val))
        ; var(Val) ->
            error(4, Pool:set(What, Val))
        ;
            get_pool_item(Pool, Handle), 
            ( Handle == 0 ->
                printf(error, "Bfs instance has no tree set up in %w%n",
                       [Pool:set(What, Val)]),
                abort
            ;
                bfs_set_body(Handle, What, Val)
            )
	).

bfs_set_body(Handle, feas_check, Val) ?- !,
        functor(Val, Functor, Arity),
        Handle = bfs_tree with module:Module,
        current_predicate(Functor/Arity)@Module,
        setarg(feas_check of bfs_tree, Handle, Val).
bfs_set_body(Handle, data, Val) ?- !,
        setarg(data of bfs_tree, Handle, Val).
bfs_set_body(Handle, node_select, Val) ?- !,
        setarg(node_select of bfs_tree, Handle, Val).
bfs_set_body(Handle, alpha_min, Val) ?- !,
        setarg(alpha_min of bfs_tree, Handle, Val).
bfs_set_body(Handle, alpha_max, Val) ?- !,
        setarg(alpha_max of bfs_tree, Handle, Val).
bfs_set_body(Handle, beta_const, Val) ?- !,
        setarg(beta_const of bfs_tree, Handle, Val).
bfs_set_body(Handle, beta_pc, Val) ?- !,
        setarg(beta_pc of bfs_tree, Handle, Val).
bfs_set_body(Handle, beta_lb, Val) ?- !,
        setarg(beta_lb of bfs_tree, Handle, Val).
bfs_set_body(Handle, pc_init, Val) ?- !,
        setarg(pc_init of bfs_tree, Handle, Val).
bfs_set_body(Handle, pc_update, Val) ?- !,
        setarg(pc_update of bfs_tree, Handle, Val).
bfs_set_body(Handle, pc_ratio, Val) ?- !,
        setarg(pc_ratio of bfs_tree, Handle, Val).
bfs_set_body(Handle, lb_time, Val) ?- !,
        setarg(lb_time of bfs_tree, Handle, Val).
bfs_set_body(Handle, int_tolerance, Val) ?- !,
        setarg(int_tolerance of bfs_tree, Handle, Val).
bfs_set_body(Handle, info_messages, Val) ?- !,
        setarg(info_messages of bfs_tree, Handle, Val).
bfs_set_body(Handle, What, Val) :-
	error(6, bfs_set_body(Handle, What, Val)).


% bfs_solver_setup/3,4: for low-level handles
:- tool(bfs_solver_setup/3, bfs_solver_setup2/4).

bfs_solver_setup2(Handle, Sense, Solver, Module) :-
        bfs_solver_setup(Handle, Sense, Solver, [], Module).

:- tool(bfs_solver_setup/4, bfs_solver_setup/5).

bfs_solver_setup(Handle, Sense, Solver, OptionList, Module) :-
        bfs_solver_setup_body(Handle, Sense, Solver, OptionList, _, Module).

% solver_setup/2,3: for pools
:- tool(bfs_solver_setup1/3, bfs_solver_setup0/4).

bfs_solver_setup0(Sense, Solver, Pool, Module) :-
        bfs_solver_setup0(Sense, Solver, [], Pool, Module).

:- tool(bfs_solver_setup1/4, bfs_solver_setup0/5).

bfs_solver_setup0(Sense, Solver, OptionList, Pool, Module) :-
        get_pool_handle(Handle, Pool),
        bfs_solver_setup_body(Handle, Sense, Solver, OptionList, Pool, Module).

bfs_solver_setup_body(Handle, Sense, Solver, OptionList, Pool, Module) :-
        ( var(Solver) ->
              error(4, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ;
              true
        ),
        ( var(Sense) ->
              error(4, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ; Sense == min ->
              true
        ; Sense == max ->
              true
        ; error(6, bfs_solver_setup(Sense, Solver, OptionList, Pool))
        ),
        % setup tree
        Handle = bfs_tree with [
                                module:Module,
                                shelf:Shelf,
                                sense:Sense,
                                cost:CostVar,
                                best_bound:1.0Inf,
                                root_node:Root,
                                next_node_no:1,
                                pool:Pool
                               ],
        set_var_bounds(CostVar, -1.0Inf, 1.0Inf),
        % create a shelf for storing info over failures
        shelf_create(bfs_shelf{branches:[],global_cuts:[],info:[]}, Shelf),
        % create a root node bfs_node struct
        RootNode = bfs_node with [
                                  id:0,
                                  rank: Rank,
                                  parent_obj: -1.0Inf,
                                  branches:[]
                                 ],
        % create a n_tree for the search tree and place the bfs_node
        % struct in its data
        n_trees:n_tree((<), Root),
        n_trees:n_tree_set(data, Root, RootNode),
        ( atom(Solver), is_constraint_pool(Solver) ->
              get_pool_item(Solver, Item),
              % if the solver supplied is a constraint pool name we
              % have built-in node optimization and separation
              % predicates for eplex problems, otherwise
              % they must be user-supplied
              %( Item = prob with [vars:VarArr, objcoeffs:Expr] ->
              ( functor(Item, prob, _) ->
                    Solver:eplex_get(vars, VarArr),
                    Solver:eplex_get(objective, Objective),
                    Objective =.. [Sense, Expr],
                    Solve = bfs_minimize_eplex_node(Handle, Item),
                    lp_set(Item, reduced_cost, yes),
                    lp_set(Item, keep_basis, yes),
                    RootNode = bfs_node with state:([],[]),
                    DefaultSeparation = bfs_deg_est(Handle, eplex(Item)),
                    %( bfs_integral_objective(Expr, VarArr, Handle) ->
                    ( bfs_integral_objective(Handle, Expr) ->
                          setarg(integral_obj of bfs_tree, Handle, yes)
                    ;
                          true
                    ),
                    % process option list and fill in default values
                    process_options(OptionList, Handle, Item, Separation),
                    fill_in_defaults(Handle, DefaultSeparation, Separation),
                    % try to identify GUB constraints to improve branching
                    gub_constraints(Item, Handle)
              ;
                    concat_string([Pool,
                                   ": no built-in node optimization or"
                                   " separation predicates for pool ",
                                   Solver], Message),
                    writeln(warning_output, Message),
                    abort
              )
        %; Solver = prob with [vars:VarArr, objcoeffs:Expr] ->
        ; functor(Solver, prob, _) ->
              lp_get(Solver, vars, VarArr),
              lp_get(Solver, objective, Objective),
              Objective =.. [Sense, Expr],
              Solve = bfs_minimize_eplex_node(Handle, Solver),
              lp_set(Solver, reduced_cost, yes),
              lp_set(Solver, keep_basis, yes),
              RootNode = bfs_node with state:([],[]),
              DefaultSeparation = bfs_deg_est(Handle, eplex(Solver)),
              %( bfs_integral_objective(Expr, VarArr, Handle) ->
              ( bfs_integral_objective(Handle, Expr) ->
                    setarg(integral_obj of bfs_tree, Handle, yes)
              ;
                    true
              ),
              % process option list and fill in default values
              process_options(OptionList, Handle, Solver, Separation),
              fill_in_defaults(Handle, DefaultSeparation, Separation),
              % try to identify GUB constraints to improve branching
              gub_constraints(Solver, Handle)
        ;
              Solve = Solver,
              % process option list and fill in default values
              process_options(OptionList, Handle, Separation),
              fill_in_defaults(Handle, DefaultSeparation, Separation)
        ),
        Handle = bfs_tree with node_select:NodeSelect,
        ( ( functor(Solve, bfs_minimize_eplex_node, _),
            lp_get(presolve, 1),
            lp_get(optimizer, xpress),
            (Separation = bfs_deg_est(_, _); Separation = bfs_strong(_, _)) ) ->
              % We disallow the use of XPRESS with estimate-based
              % methods on pre-solved problems because IF there is an
              % abort (probably hitting iteration limit before finding
              % a feasible basis) AND the external solver is XPRESS-MP
              % AND the problem was pre-solved, THEN the variable
              % column numbers get returned in the wrong order causing
              % incorrect solutions at future nodes
              Separation1 = bfs_fracvar(Handle),
              ( NodeSelect == best_estimate ->
                    % have to change the node selection method also
                    % since best_estimate requires some sort of
                    % estimate-based spearation method
                    setarg(node_select of bfs_tree, Handle, best_first),
                    NewNS = best_first
              ;
                    NewNS = NodeSelect
              ),
              bfs_info_message(Handle, "cannot use lower-bounding based"
                               " estimate methods for variable/branch"
                               " selection with pre-solved XPRESS problems,"
                               " parameters changed to%n%tseparation: fracvar"
                               " %n%tnode_select: %w%n", [NewNS])
        ;
              Separation1 = Separation
        ),
        Handle = bfs_tree with node_select:NodeSelect0,
        ( NodeSelect0 == best_first ->
              Rank = -1.0Inf
        ; NodeSelect0 == depth_first ->
              Rank = ""
        ; NodeSelect0 == best_estimate ->
              Rank = -1.0Inf
        ; NodeSelect0 == two_phase ->
              Rank = ""
        ;
              error(4, bfs_solver_setup(Solver, OptionList, Pool))
        ),
        % make a suspension for the node processing
        % and insert it in the node_susp suspension list of Handle
        make_suspension(bfs_body(Handle, Solve, Separation1, Pool),
                        0, Susp),
        enter_suspension_list(node_susp of bfs_tree, Handle, Susp).

bfs_solver_cleanup(Pool) :-
        get_pool_handle(Handle, Pool),
        Handle = bfs_tree with node_susp:SuspList,
        ( foreach(Susp, SuspList) do kill_suspension(Susp) ).

% bfs_solve/2: for low-level handles
bfs_solve(Handle, Obj) :-
        ( var(Handle) ->
            error(4, bfs_solve(Handle, Obj))
        ; Handle = bfs_tree{} ->
            bfs_solve_body(Handle, Obj)
        ;
            error(5, bfs_solve(Handle, Obj))
        ).

% solve/1: for pools
bfs_solve1(Obj, Pool) :-
        get_pool_handle(Handle, Pool),
        bfs_solve_body(Handle, Obj).

bfs_solve_body(Handle, Obj) :-
        cputime(StartTime),
        setarg(start_time of bfs_tree, Handle, StartTime),
        setarg(no_sols of bfs_tree, Handle, 0),
        Handle = bfs_tree with sense:Sense,
        ( Sense == min ->
            setarg(best_bound of bfs_tree, Handle, 1.0Inf)
        ; Sense == max ->
            setarg(best_bound of bfs_tree, Handle, -1.0Inf)
        ),
        schedule_suspensions(node_susp of bfs_tree, Handle),
        wake,
        cputime(EndTime),
        TotalTime is EndTime - StartTime,
        setarg(total_time of bfs_tree, Handle, TotalTime),
        % return the optimal solution value
        Handle = bfs_tree{node_select:NodeSelect,
                          best_bound:Obj},
        ( Sense == min ->
              Obj < 1.0Inf
        ; Sense == max ->
              Obj > -1.0Inf
        ),
        % replace the root node for later resolution
        ( NodeSelect == best_first ->
              Rank = -1.0Inf
        ; NodeSelect == depth_first ->
              Rank = ""
        ; NodeSelect == best_estimate ->
              Rank = -1.0Inf
        ; NodeSelect == two_phase ->
              Rank = ""
        ),
        RootNode = bfs_node with [
                                  id:0,
                                  rank: Rank,
                                  parent_obj: -1.0Inf,
                                  branches:[]
                                 ],
        % create a n_tree for the search tree and place the bfs_node
        % struct in its data
        n_trees:n_tree((<), Root),
        n_trees:n_tree_set(data, Root, RootNode),
        setarg(root_node of bfs_tree, Handle, Root).

gub_constraints(EplexHandle, Handle) :-
        % we need to find gub constraints in the problem and
        % identify suitable reference rows for them
        Handle = bfs_tree{int_tolerance:IntTol},
        lp_get(EplexHandle, constraints_norm, NormCstrs),
        (
            foreach(Type:[Const*One|VarTerms], NormCstrs),
            fromto(OtherCstrs, Out, In, []),
            fromto(GUBSets, GOut, GIn, []),
            param(IntTol, Handle)
        do
            ( Type == (>=) ->
                  % not a GUB
                  Out = [Type:[Const*One|VarTerms]|In],
                  GOut = GIn
            ; gub_structure(VarTerms, GUBSet, Const, IntTol, Handle) ->
                  % found a GUB constraint
                  Out = In,
                  GOut = [GUBSet|GIn]
            ;
                  % not a GUB
                  Out = [Type:[Const*One|VarTerms]|In],
                  GOut = GIn
            )
        ),
        (
            foreach(GUBSet, GUBSets),
            fromto(GUBs, Out, In, []),
            param(OtherCstrs)
        do
            ( reference_row(OtherCstrs, GUBSet, Vars, RefTerms, PCStructs) ->
                  % found a suitable reference row for this GUB
                  % create a bfs_gub structure
                  GUB = bfs_gub with [
                                      vars:Vars,
                                      refs:RefTerms,
                                      pseudocosts:PCStructs
                                     ],
                  Out = [GUB|In]
            ;
                  % cannot find a suitable reference row for
                  % this GUB
                  Out = In
            )
        ),
        ( GUBs == [] ->
              true
        ;
              GUBStruct =.. [[]|GUBs],
              setarg(gubs of bfs_tree, Handle, GUBStruct),
              Handle = bfs_tree{root_node:Root},
              n_trees:n_tree_get(data, Root, RootNode),
              setarg(branches of bfs_node, RootNode,
                     [lp_add(EplexHandle, [], [])])
        ).

process_options([], _, _, _) ?- !, true.
process_options([O|Os], Handle, EplexHandle, Separation) ?- !,
	process_option(O, Handle, EplexHandle, Separation),
	process_options(Os, Handle, EplexHandle, Separation).
process_options(_NonList, Handle, _, _) :-
        Handle = bfs_tree with pool:Pool,
        bfs_info_message(Handle, "%w : options not proper list."
                         " Ignored.%n,", [Pool]).

process_option(separation(S), Handle, Eplex, Separation) ?- !,
        process_separation(S, Handle, Eplex, Separation).
process_option(O, Handle, _, Separation) ?- !,
        process_option(O, Handle, Separation).

process_separation(Val, Handle, Eplex, Separation) :- !,
        ( Val = deg_est -> Separation = bfs_deg_est(Handle, eplex(Eplex))
        ; Val = fracvar -> Separation = bfs_fracvar(Handle)
        ; Val = strong -> Separation = bfs_strong(Handle, eplex(Eplex))
        ; Val = enhanced -> Separation = bfs_enhanced(Handle, eplex(Eplex))
        % this doesn't exist!
        %; Val = gub -> Separation = bfs_gub(Handle)
        ; Separation = Val ).

process_options([], _, _) ?- !, true.
process_options([O|Os], Handle, Separation) ?- !,
	process_option(O, Handle, Separation),
	process_options(Os, Handle, Separation).
process_options(_NonList, Handle, _) :-
        Handle = bfs_tree with pool:Pool,
        bfs_info_message(Handle, "%w : options not proper list."
                         " Ignored.%n,", [Pool]).

process_option(separation(Separation), _Handle, Separation) :- !.
process_option(global_feasibility_check(Val), Handle, _) ?- !,
        bfs_set_body(Handle, feas_check, Val).
process_option(data(Val), Handle, _) ?- !,
        bfs_set_body(Handle, data, Val).
process_option(node_select(Val), Handle, _) ?- !,
        bfs_set_body(Handle, node_select, Val).
process_option(alpha(AlphaMin, AlphaMax), Handle, _) ?- !,
	bfs_set_body(Handle, alpha_min, AlphaMin),
	bfs_set_body(Handle, alpha_max, AlphaMax).
process_option(beta(BetaConst, BetaPC, BetaLB), Handle, _) ?- !,
	bfs_set_body(Handle, beta_const, BetaConst),
	bfs_set_body(Handle, beta_pc, BetaPC),
	bfs_set_body(Handle, beta_lb, BetaLB).
process_option(pseudo_cost(PCInit, PCUpdate, PCRatio), Handle, _) ?- !,
	bfs_set_body(Handle, pc_init, PCInit),
	bfs_set_body(Handle, pc_update, PCUpdate),
        ( var(PCRatio) -> true
        ; 0 < PCRatio, PCRatio < 1 -> bfs_set_body(Handle, pc_ratio, PCRatio)
        ; bfs_info_message(Handle, "Invalid pc_ratio (ignored): %w%n", [PCRatio]) ).
process_option(lower_bound(Val), Handle, _) ?- !,
	bfs_set_body(Handle, lb_time, Val).
process_option(int_tolerance(Val), Handle, _) ?- !,
        ( 0 =< Val, Val =< 0.5 -> bfs_set_body(Handle, int_tolerance, Val)
        ; bfs_info_message(Handle, "Invalid int_tolerance (ignored): %w%n", [Val]) ).
process_option(info_messages(Val), Handle, _) ?- !,
        bfs_set_body(Handle, info_messages, Val).
process_option(NoOpt, Handle, _) :-
	bfs_info_message(Handle, "Invalid option (ignored): %w%n", [NoOpt]).

fill_in_defaults(Handle, DefaultSeparation, Separation) :-
        Handle = bfs_tree with [
                                data:Data,
                                feas_check:FeasCheck,
                                integral_obj:IntObj,
                                int_tolerance:IntTol,
                                vars:Vars,
                                node_select:NodeSelect,
                                pcd_count:0-0,
                                pcu_count:0-0,
                                alpha_min:AlphaMin,
                                alpha_max:AlphaMax,
                                beta_const:BetaConst,
                                beta_pc:BetaPC,
                                beta_lb:BetaLB,
                                pc_init:PCInit,
                                pc_update:PCUpdate,
                                pc_ratio:PCRatio,
                                lb_time:LB,
                                total_time:0,
                                no_sols:0,
                                solve_time:0,
                                separate_time:0,
                                nodes_separated:0,
                                nodes_solved:0,
                                gubs:GUBs,
                                info_messages:OnOff,
                                pool:Pool
                               ],
        ( var(Data) -> Data = 0 ; true ),
        ( var(FeasCheck) -> FeasCheck = true ; true ),
        ( var(IntObj) -> IntObj = no ; true ),
        ( var(IntTol) -> IntTol = 1e-05 ; true ),
        ( var(Vars) -> Vars = [] ; true ),
        ( var(Separation) ->
              ( var(DefaultSeparation) ->
                    concat_string([Pool,
                                   ": no node separation method"
                                   " specified."], Message),
                    writeln(warning_output, Message),
                    abort
              ;
                    Separation = DefaultSeparation
              )
        ;
              true
        ),
        ( var(NodeSelect) -> NodeSelect = best_first ; true ),
        ( var(AlphaMin) -> AlphaMin = 2 ; true ),
        ( var(AlphaMax) -> AlphaMax = 1 ; true ),
        ( var(BetaConst) -> BetaConst = 0 ; true ),
        ( var(BetaPC) -> BetaPC = 1 ; true ),
        ( var(BetaLB) -> BetaLB = 1 ; true ),
        ( var(PCInit) -> PCInit = calculated ; true ),
        ( var(PCUpdate) -> PCUpdate = average ; true ),
        ( var(PCRatio) -> PCRatio = 0.05 ; true ),
        ( var(LB) -> LB = 1 ; true ),
        ( var(OnOff) -> OnOff = off ; true ),
        ( var(GUBs) -> GUBs = [] ; true ).

bfs_integral_objective(Handle, Expr1+Expr2) :-
        bfs_integral_objective(Handle, Expr1),
        bfs_integral_objective(Handle, Expr2).
bfs_integral_objective(Handle, C*Var) :-
        integer(C),
        bfs_var_get_body(Handle, Var, type, integer).
bfs_integral_objective(Handle, Var) :-
        bfs_var_get_body(Handle, Var, type, integer).

bfs_integral_objective([], _, _).
bfs_integral_objective([J:C|Rest], VarArr, Handle) :-
        integer(C),
        J1 is J + 1,
        arg(J1, VarArr, Var),
        bfs_var_get_body(Handle, Var, type, integer),
        bfs_integral_objective(Rest, VarArr, Handle).

:- demon bfs_body/4.
:- set_flag(bfs_body/4, priority, 8).
:- set_flag(bfs_body/4, run_priority, 8).
bfs_body(Handle, Solve, Separation, Pool) :-
        Handle = bfs_tree{module:Module,
                          solve_time:TSolve,
                          nodes_solved:NSolve,
                          shelf:Shelf
                         },
        ( bfs_next_node(Handle, SubTree),
          n_trees:n_tree_get(data, SubTree, Node) ->
              Node = bfs_node{id:NodeId},
              bfs_info_message(Handle, "%w:Processing node %w%n%tsolving relaxation ... ",
                               [Pool, NodeId]),
              cputime(T1),
              % call node minimization goal
              setarg(frac_vars of bfs_node, Node, []),
              ( call(Solve)@Module ->
                    Node = bfs_node{objval:ObjVal},
                    bfs_info_message(Handle, " done, z = %w%n", [ObjVal])
              ;
                    setarg(objval of bfs_node, Node, 1.0Inf),
                    bfs_info_message(Handle, " infeasible%n", [])
              ),
              % do some statistics collection
              cputime(T2),
              TSolve1 is TSolve + T2 - T1,
              setarg(solve_time of bfs_tree, Handle, TSolve1),
              NSolve1 is NSolve + 1,
              setarg(nodes_solved of bfs_tree, Handle, NSolve1),
              bfs_info_message(Handle, "%tAdding global cuts ... ", []),
              % note to AE: should really check all nodes in the tree
              % to ensure they are feasible with these new global cuts
              shelf_get(Shelf, global_cuts of bfs_shelf, GlobalCuts),
              (
                  foreach(GlobalCut, GlobalCuts),
                  param(Module)
              do
                  call(GlobalCut)@Module
              ),
              shelf_set(Shelf, global_cuts of bfs_shelf, []),
              bfs_info_message(Handle, " done%n", []),
              bfs_info_message(Handle, "%tseparating ... ", []),
              cputime(T3),
              Handle = bfs_tree{feas_check:FeasCheck,
                                shelf:Shelf,
                                sense:Sense,
                                vars:Vars,
                                integral_obj:IntObj,
                                cost:Cost,
                                best_bound:BestBound,
                                node_select:NodeSelect,
                                next_node_no:Id1,
                                separate_time:TSep,
                                nodes_separated:NSep,
                                start_time:TStart,
                                no_sols:NSols
                               },
              Node = bfs_node{id:NodeId,
                              objval:NodeCost,
                              rank:Rank,
                              frac_vars:FracVars,
                              state:State,
                              branches:Branches
                             },
              ( NodeCost == 1.0Inf ->
                    % fathomed by infeasibility
                    bfs_info_message(Handle, "%tfathomed by infeasibility%n", []),
                    n_trees:n_tree_fathom(SubTree)
              ; fathom_by_bounds(IntObj, NodeCost, BestBound) ->
                    % fathomed by bounds
                    bfs_info_message(Handle, "%tfathomed by bounds%n", []),
                    n_trees:n_tree_fathom(SubTree)
              ; global_feasible(Node, FeasCheck, Module) ->
                    % globally feasible solution,
                    % optimal within tolerance bound
                    bfs_info_message(Handle, "%tglobally feasible%n", []),
                    % do some statistics collection
                    cputime(T),
                    TSolution is T - TStart,
                    setarg(opt_sol_time of bfs_tree, Handle, TSolution),
                    % update incumbent and prune nodes
                    set_var_bounds(Cost, -1.0Inf, NodeCost),
                    ( IntObj == yes ->
                          NewBound is fix(NodeCost),
                          PruneBound is NodeCost - 1
                    ;
                          NewBound = NodeCost,
                          PruneBound = NodeCost
                    ),
                    setarg(best_bound of bfs_tree, Handle, NewBound),
                    n_trees:n_tree_fathom(SubTree),
                    Handle = bfs_tree with vars:Vars,
                    (
                        foreach(Var, Vars),
                        param(NodeId, Handle)
                    do
                        ( var(Var) ->
                            bfs_get_node_info(Var, NodeId,
                                              bfs_node_info with val:Val, Handle),
                            bfs_var_set(Var, optimal_val, Val, Handle)
                        ;
                            true
                        )
                    ),
                    ( NSols = 0 ->
                          % first feasible solution
                          % if we are using a two-phase strategy
                          % we should switch
                          ( NodeSelect == two_phase ->
                              setarg(node_select of bfs_tree, Handle, best_first)
                          ;
                              true
                          ),
                          setarg(first_sol_time of bfs_tree, Handle, TSolution),
                          setarg(no_sols of bfs_tree, Handle, 1)
                    ;
                          % new optimal solution
                          NSols1 is NSols + 1,
                          setarg(no_sols of bfs_tree, Handle, NSols1)
                    )
              ;
                    (
                        foreach(Var, Vars),
                        foreach(Lo..Hi, LoHis),
                        param(NodeId, Handle)
                    do
                        bfs_get_node_info(Var, NodeId,
                                          bfs_node_info with [lo:Lo, hi:Hi],
                                          Handle)
                    ),
                    shelf_set(Shelf, branches of bfs_shelf, []),
                    call(Separation)@Module,
                    shelf_get(Shelf, branches of bfs_shelf, ScoredBranches),
                    (
                        foreach([Score, Branch], ScoredBranches),
                        foreach(Diff-NewNode, ChildRanks),
                        count(Id, Id1, Id2),
                        param(NodeSelect, Sense, NodeCost, Rank, State, Branches,
                              FracVars, Vars, LoHis, Handle, SubTree)
                    do
                        NewNode = bfs_node with [
                                                 id:Id,
                                                 rank:ChildRank,
                                                 parent_obj:NodeCost,
                                                 state:State,
                                                 branches:ChildBranches
                                                ],
                        (
                            foreach(Var, Vars),
                            foreach(Lo..Hi, LoHis),
                            param(NewNode, Handle)
                        do
                            bfs_new_node_info(Var, NewNode,
                                              bfs_node_info with [lo:Lo, hi:Hi],
                                              Handle)
                        ),
                        ( number(Score) ->
                              Diff = Score
                        ;
                              Score = [_PCIdxs, PCStruct, Diff, UpDown],
                              setarg(pc_update of bfs_node, NewNode,
                                     [PCStruct, Diff, UpDown])
                        ),
                        ( Branch = NewNode ->
                            true
                        ; Branch = bounds(Idx, Lo..Hi) ->
                            % it was a variable bound update
                            FracVarArr =.. [[]|FracVars],
                            arg(Idx, FracVarArr, Var:_Sol),
                            PCVars = [Var],
                            bfs_new_node_info(Var, NewNode,
                                              bfs_node_info with [lo:Lo, hi:Hi],
                                              Handle),
                            ChildBranches = Branches
                        ; Branch = gub_bound(GN, NVars, Rel, Rhs),
                          Branches = [lp_add(EplexHandle, Cstrs, [])] ->
                            % it was a gub and we have an eplex
                            % handle to add to
                            Handle = bfs_tree{gubs:GUBs},
                            arg(GN, GUBs, GUB),
                            GUB = bfs_gub{vars:GUBVars},
                            (
                                for(_, 1, NVars),
                                foreach(Term, Terms),
                                fromto(GUBVars, [Term|Rest], Rest, _)
                            do
                                true
                            ),
                            term_variables(Terms, PCVars),
                            Expr =.. [Rel, sum(PCVars), Rhs],
                            normalise_cstrs([Expr], [Cstr], []),
                            ChildBranches = [lp_add(EplexHandle, [Cstr|Cstrs], [])]
                        ;
                            % it was some user defined branch
                            ChildBranches = [Branch|Branches]
                        ),
                        ( NodeSelect == best_first ->
                              % using a static node selection method
                              % so order among children by fractional change
                              % the rank given to a node here is the bound
                              % on the node objective obtained from the parent
                              % and the frac change Diff from parent to node,
                              % standard ordering ensures we choose among them
                              % in Bound then Frac order
                              ( Rank = PCost-_ -> true ; Rank = PCost ),
                              ( Sense = min ->
                                  NodeCost0 is max(PCost, NodeCost),
                                  ChildRank = NodeCost0-Diff
                              ; % Sense = max
                                  NodeCost0 is min(PCost, NodeCost),
                                  Diff1 is 1 - Diff,
                                  ChildRank = NodeCost0-Diff1
                              ),
                              n_trees:n_tree_expand(SubTree,
                                                    ChildRank,
                                                    ChildNode),
                              n_trees:n_tree_set(data, ChildNode, NewNode)
                        ; NodeSelect == depth_first ->
                              true
                        ; NodeSelect == two_phase ->
                              true
                        ; NodeSelect == best_estimate ->
                              % using an estimate based selection method, so the order
                              % of the nodes is taken care of automatically
                              % Note to AE: this may not give the correct
                              % estimates with gub branching?
                              ( Sense = min ->
                                    Order = (@<),
                                    Functor = (+)
                              ; % Sense = max
                                    Order = (@>),
                                    Functor = (-)
                              ),
                              PCStruct = pseudocost with [pcd:PCD, pcu:PCU],
                              (
                                  foreach(Var:Val, FracVars),
                                  fromto(NodeCost, In, Out, CommonRank),
                                  param(Handle, PCVars, Functor)
                              do
                                  ( var_member(Var, PCVars) ->
                                        Out = In
                                  ;
                                        get_bfs_attr(Var, Handle,
                                                     bfs with [pcd:D, pcu: U]),
                                        Delta is min(D*(Val-floor(Val)),
                                                     U*(ceiling(Val)-Val)),
                                        Goal =.. [Functor, In, Delta, Out],
                                        call(Goal)
                                  )
                              ),
                              ( UpDown == d -> Delta is PCD * Diff
                              ; Delta is PCU * Diff ), % UpDown == u
                              Goal =.. [Functor, CommonRank, Delta, ChildRank],
                              call(Goal),
                              n_trees:n_tree_expand(SubTree,
                                                    ChildRank,
                                                    ChildNode),
                              n_trees:n_tree_set(data, ChildNode, NewNode)
                        ;
                              printf(error, "unknown node_select type: %w%n",
                                     [NodeSelect]),
                              abort
                        )
                    ),
                    ( ( NodeSelect == depth_first ; NodeSelect == two_phase ) ->
                          sort(1, =<, ChildRanks, Sorted),
                          (
                              foreach(_-NewNode, Sorted),
                              count(N, 0, _),
                              param(SubTree, Pool, Rank)
                          do
                              ( N =< 255 ->
                                    char_code(C, N),
                                    concat_string([Rank, C], ChildRank),
                                    NewNode = bfs_node with rank:ChildRank,
                                    n_trees:n_tree_expand(SubTree,
                                                          ChildRank,
                                                          ChildNode),
                                    n_trees:n_tree_set(data, ChildNode, NewNode)
                              ;
                                    printf(error, "%w: maximum number"
                                           " of children at a node"
                                           " exceeded (256)%n",
                                           [Pool]),
                                    abort
                              )
                          )
                    ; true ),
                    NextId is Id2 + 1,
                    cputime(T4),
                    TSep1 is TSep + T4 - T3,
                    setarg(separate_time of bfs_tree, Handle, TSep1),
                    NSep1 is NSep + 1,
                    setarg(nodes_separated of bfs_tree, Handle, NSep1),
                    setarg(next_node_no of bfs_tree, Handle, NextId),
                    bfs_info_message(Handle, "done, nodes %d..%d created%n",
                                     [Id1, Id2])
              ),
              schedule_suspensions(node_susp of bfs_tree, Handle),
              wake
        ;
              true
        ).

var_member(Var, [H|T]) :-
        ( Var == H -> true ; var_member(Var, T) ).

% bfs_minimize_eplex_node/2: for low-level handles
bfs_minimize_eplex_node(Handle, EplexHandle) :-
        ( var(Handle) ->
            error(4, bfs_minimize_eplex_node(Handle, EplexHandle))
        ; Handle = bfs_tree{} ->
            bfs_minimize_eplex_node_body(Handle, EplexHandle)
        ;
            error(5, bfs_minimize_eplex_node(Handle, EplexHandle))
        ).

% minimize_eplex_node/1: for pools
bfs_minimize_eplex_node1(EplexHandle, Pool) :-
        get_pool_handle(Handle, Pool),
        bfs_minimize_eplex_node_body(Handle, EplexHandle).

bfs_minimize_eplex_node_body(Handle, EplexHandle) :-
        Handle = bfs_tree with [
                                cost:Cost,
                                shelf:Shelf,
                                pc_ratio:PCRatio,
                                no_sols:NSols
                               ],
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node with [
                              id:NodeId,
                              state:(CBasis, RBasis),
                              branches:Branches
                             ],
        ( NodeId = 0 ->
              % root node, no need to block/fail
              lp_solve(EplexHandle, Obj),
              lp_get(EplexHandle, vars, VarArr),
              VarArr =.. [_|Vars],
              ( 
                  foreach(Var, Vars),
                  param(EplexHandle, Handle)
              do
                  lp_var_get(EplexHandle, Var, solution, Val),
                  lp_var_get(EplexHandle, Var, reduced_cost, RC),
                  get_var_bounds(Var, Lo, Hi),
                  Info = bfs_node_info with [
                                             lo:Lo,
                                             hi:Hi,
                                             val:Val,
                                             rc:RC
                                            ],
                  bfs_node_info_body(Handle, Var, Info)
              ),
              lp_get(EplexHandle, simplex_iterations, Its),
              Node = bfs_node with frac_vars:FracVars,
              length(FracVars, NFrac),
              PCIt is fix(ceiling((PCRatio*Its)/(2*NFrac))),
              setarg(pc_ratio of bfs_tree, Handle, PCIt),
              lp_get(EplexHandle, cbasis, NewCBasis),
              lp_get(EplexHandle, rbasis, NewRBasis),
              setarg(state of bfs_node, Node, (NewCBasis, NewRBasis))
        ;
              shelf_get(Shelf, info of bfs_shelf, OldInfo),
              \+ \+ ( lp_set(EplexHandle, cbasis, CBasis),
                      lp_set(EplexHandle, rbasis, RBasis),
                      lp_get(EplexHandle, vars, VarArr),
                      VarArr =.. [_|Vars],
                      call_priority(( 
                                        foreach(Var, Vars),
                                        param(Handle, EplexHandle)
                                    do
                                        bfs_node_info_body(Handle, Var, Info),
                                        Info = bfs_node_info with [lo:Lo, hi:Hi],
                                        set_var_bounds(Var, Lo, Hi),
                                        lp_var_set_bounds(EplexHandle, Var, Lo, Hi)
                                    ), 2),
                      ( foreach(Branch, Branches) do call(Branch) ),
                      lp_solve(EplexHandle, Obj),
                      ( NSols > 0 ->
                            reduced_cost_pruning(EplexHandle, Cost)
                      ;
                            true
                      ),
                      (
                          foreach(Var, Vars),
                          foreach(Info, Vals),
                          param(EplexHandle)
                      do
                          lp_var_get(EplexHandle, Var, solution, Val),
                          lp_var_get(EplexHandle, Var, reduced_cost, RC),
                          get_var_bounds(Var, Lo, Hi),
                          Info = bfs_node_info with [
                                                        lo:Lo,
                                                        hi:Hi,
                                                        val:Val,
                                                        rc:RC
                                                    ]
                      ),
                      lp_get(EplexHandle, cbasis, NewCBasis),
                      lp_get(EplexHandle, rbasis, NewRBasis),
                      shelf_set(Shelf, info of bfs_shelf, [Obj, Vals, (NewCBasis, NewRBasis)])
              ),
              lp_get(EplexHandle, vars, VarArr),
              VarArr =.. [_|Vars],
              shelf_get(Shelf, info of bfs_shelf, [Obj, Vals, NewState]),
              shelf_set(Shelf, info of bfs_shelf, OldInfo),
              ( 
                  foreach(Var, Vars),
                  foreach(Info, Vals),
                  param(Handle)
              do
                  bfs_node_info_body(Handle, Var, Info)
              ),
              setarg(state of bfs_node, Node, NewState)
        ),
        setarg(objval of bfs_node, Node, Obj).

% bfs_update_pseudocosts/1: for low-level handles
bfs_update_pseudocosts(Handle) :-
        ( var(Handle) ->
            error(4, bfs_update_pseudocosts(Handle))
        ; Handle = bfs_tree{} ->
            bfs_update_pseudocosts_body(Handle)
        ;
            error(5, bfs_update_pseudocosts(Handle))
        ).

% update_pseudocosts/0: for pools
bfs_update_pseudocosts1(Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:update_pseudocosts]),
            abort
        ;
            bfs_update_pseudocosts_body(Handle)
        ).

bfs_update_pseudocosts_body(Handle) :-
        Handle = bfs_tree{
                          pc_update:PCUpdate,
                          pcd_count:PCDCount-PCDTotal,
                          pcu_count:PCUCount-PCUTotal
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node{
                        id:Id,
                        objval:Obj,
                        parent_obj:ObjP,
                        pc_update:[PCStruct, Frac, Direction]
                       },
        ( Id == 0 ->
            true
        ;
            PseudoCost is (Obj - ObjP)/Frac,
            % update pseudocosts
            PCStruct = pseudocost{
                                  pcd:Pcd,
                                  pcd_count:PcdTerm,
                                  pcu:Pcu,
                                  pcu_count:PcuTerm
                                 },
            ( Direction == u ->
                % update the global pcu counters
                % for pcu initialisation
                NewPCUCount is PCUCount + 1,
                NewPCUTotal is PCUTotal + PseudoCost,
                NewPCUAverage is NewPCUTotal/NewPCUCount,
                setarg(pcu_average of bfs_tree, Handle,
                       NewPCUAverage),
                setarg(pcu_count of bfs_tree, Handle,
                       NewPCUCount-NewPCUTotal),
                % update the pcu counters involved in the branch
                ( PcuTerm = 0-Pcu ->
                    NewPcuCount = 1,
                    NewPcuTotal = PseudoCost,
                    NewPcu = PseudoCost
                ;
                    PcuTerm = PcuCount-PcuTotal,
                    NewPcuCount is PcuCount + 1,
                    NewPcuTotal is PcuTotal + PseudoCost,
                    ( PCUpdate = first ->
                        NewPcu = Pcu
                    ; PCUpdate = last ->
                        NewPcu = PseudoCost
                    ; % PCUpdate = average
                        NewPcu is NewPcuTotal/NewPcuCount
                    )
                ),
                setarg(pcu_count of pseudocost, PCStruct,
                       NewPcuCount-NewPcuTotal),
                setarg(pcu of pseudocost, PCStruct, NewPcu)
            ; Direction == d ->
                % update the global pcd counters
                % for pcd initialisation
                NewPCDCount is PCDCount + 1,
                NewPCDTotal is PCDTotal + PseudoCost,
                NewPCDAverage is NewPCDTotal/NewPCDCount,
                setarg(pcd_average of bfs_tree, Handle,
                       NewPCDAverage),
                setarg(pcd_count of bfs_tree, Handle,
                       NewPCDCount-NewPCDTotal),
                % update the pcd counters involved in the branch
                ( PcdTerm = 0-Pcd ->
                    NewPcdCount = 1,
                    NewPcdTotal = PseudoCost,
                    NewPcd = PseudoCost
                ;
                    PcdTerm = PcdCount-PcdTotal,
                    NewPcdCount is PcdCount + 1,
                    NewPcdTotal is PcdTotal + PseudoCost,
                    ( PCUpdate = first ->
                        NewPcd = Pcd
                    ; PCUpdate = last ->
                        NewPcd = PseudoCost
                    ; % PCUpdate = average
                        NewPcd is NewPcdTotal/NewPcdCount
                    )
                ),
                setarg(pcd_count of pseudocost, PCStruct,
                       NewPcdCount-NewPcdTotal),
                setarg(pcd of pseudocost, PCStruct, NewPcd)
            )
        ).

fathom_by_bounds(IntObj, Obj, Bound) :-
        ( IntObj == yes ->
              ceiling(Obj) > Bound
        ;
              Obj > Bound
        ).

global_feasible(bfs_node with frac_vars:[], FeasCheck, Module) :-
        call(FeasCheck)@Module.

depth_first(Ranks, Base) :-
        sort(1, >=, Ranks, Sorted),
        (
            foreach(_-Rank, Sorted),
            count(N, 1, _),
            param(Base)
        do
            concat_string([Base, N], Rank)
        ).

gub_structure([], [], _Handle).
gub_structure([Val*Var|Rest], [Var|Terms], Handle) :-
        Val =:= 1,
        bfs_var_get_body(Handle, Var, type, integer),
        gub_structure(Rest, Terms, Handle).

gub_structure([], [], Val, IntTol, _Handle) :-
        abs(Val + 1) =< IntTol.
gub_structure([Val*Var|Rest], Terms, Sum, IntTol, Handle) :-
        ( (Val =:= 1, bfs_var_get_body(Handle, Var, type, integer)) ->
              Terms = [Var|Terms0],
              Sum0 = Sum
        ;
              Terms = Terms0,
              get_var_bounds(Var, Lo, Hi),
              Sum0 is Sum + min(Val*Lo, Val*Hi)
        ),
        gub_structure(Rest, Terms0, Sum0, IntTol, Handle).

reference_row([_Type:[_Const*_One|Terms]|Cstrs], GUBSet,
              Vars, RefTerms, PCStructs) :-
        ( reference_row1(GUBSet, Terms, RefTerms0) ->
              % Note to AE: should we commit to just the first
              % possible reference 
              % row we find or collect them all as generators for
              % branching constraints? Probably that would then be too
              % expensive to check them all each time, but it might
              % create better branches
              sort(RefTerms0, RefTerms),
              (
                  foreach(_Coeff*Var, RefTerms),
                  foreach(1*Var, Vars),
                  foreach(pseudocost with [pcd_count:none,
                                           pcu_count:none], PCList)
              do
                  true
              ),
              PCStructs =.. [[]|PCList]
        ;
              reference_row(Cstrs, GUBSet, Vars, RefTerms, PCStructs)
        ).

reference_row1([], _Terms, []).
reference_row1([Var|GUBSet], Terms, [RefTerm|RefTerms]) :-
        reference_row(Var, Terms, RefTerm, Terms0),
        reference_row1(GUBSet, Terms0, RefTerms).

reference_row(Var, [Val0*Var0|Terms], RefTerm, Terms0) :-
        ( Var == Var0 ->
              RefTerm= Val0*Var0,
              Terms0 = Terms
        ;
              Terms0 = [Val0*Var0|Terms1],
              reference_row(Var, Terms, RefTerm, Terms1)
        ).

branchpoint([Coeff:VarCount:VarSum|BranchPoints], RefTotal, NVars, Diff) :-
        ( Coeff > RefTotal ->
              % this branch set includes vars with coeff above the
              % suggested ref val
              branchpoint(BranchPoints, RefTotal, NVars, Diff)
        ;
              % maximal branch set with no coeffs above the suggested
              % ref val
              NVars = VarCount,
              Diff = VarSum
        ).

% bfs_deg_est/2: for low-level handles
bfs_deg_est(Handle, Node) :-
        ( var(Handle) ->
            error(4, bfs_deg_est(Handle, Node))
        ; Handle = bfs_tree{} ->
            bfs_deg_est_body(Handle, Node)
        ;
            error(5, bfs_deg_est(Handle, Node))
        ).

% deg_est/1: for pools
bfs_deg_est1(Node, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:deg_est(Node)]),
            abort
        ;
            bfs_deg_est_body(Handle, Node)
        ).

% Note to AE: try to get rid of the eplex handle form this call
bfs_deg_est_body(Handle, eplex(EplexHandle)) :-
        Handle = bfs_tree{shelf:Shelf,
                          gubs:GUBs
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        % check the shelf is empty just in case
        shelf_get(Shelf, branches of bfs_shelf, []),
        Node = bfs_node with id:NodeId,
        % update pseudocosts for this node
        ( NodeId > 0 ->
              bfs_update_pseudocosts_body(Handle)
        ;
              true
        ),
        % find the var to branch on
        shelf_get(Shelf, info of bfs_shelf, OldInfo),
        bfs_deg_est(Handle, eplex(EplexHandle), Result, PseudoCosts),
        shelf_set(Shelf, info of bfs_shelf, OldInfo),
        ( Result = g(GN, NVars, Down) ->
              % initialize any GUB pseudocosts
              (
                  foreacharg(bfs_gub{pseudocosts:PCStructs}, GUBs),
                  foreach(PseudoCost, PseudoCosts)
              do
                  ( PseudoCost == not_stored ->
                        true
                  ;
                        PseudoCost = BP-[PCD, PCU],
                        arg(BP, PCStructs, PCStruct),
                        setarg(pcd_count of pseudocost, PCStruct, 0-PCD),
                        setarg(pcd of pseudocost, PCStruct, PCD),
                        setarg(pcu_count of pseudocost, PCStruct, 0-PCU),
                        setarg(pcu of pseudocost, PCStruct, PCU)
                  )
              ),
              arg(GN, GUBs, GUB),
              GUB = bfs_gub{pseudocosts:PCStructs},
              arg(NVars, PCStructs, PCStruct),
              Up is 1 - Down,
              % store the branches
              shelf_set(Shelf, branches of bfs_shelf,
                        [
                         [[g(GN, NVars), PCStruct, Down, d], % score
                          gub_bound(GN, NVars, =<, 0)],      % branch
                         [[g(GN, NVars), PCStruct, Up, u],   % score
                          gub_bound(GN, NVars, >=, 1)]       % branch
                        ])
        ;
              Result = v(Idx),
              Node = bfs_node with frac_vars:Vars,
              % initialize any variable pseudocosts
              (
                  foreach(Var:_, Vars),
                  foreach(PseudoCost, PseudoCosts),
                  param(Handle)
              do
                  ( PseudoCost == not_stored ->
                        true
                  ;
                        PseudoCost = [PCD, PCU],
                        get_bfs_attr(Var, Handle, Attr),
                        setarg(pcd_count of bfs, Attr, 0-PCD),
                        setarg(pcd of bfs, Attr, PCD),
                        setarg(pcu_count of bfs, Attr, 0-PCU),
                        setarg(pcu of bfs, Attr, PCU)
                  )
              ),
              VarArr =.. [[]|Vars],
              arg(Idx, VarArr, Var:Val),
              Lo is ceiling(Val),
              Up is Lo - Val,
              Hi is floor(Val),
              Down is Val - Hi,
              get_bfs_attr(Var, Handle, bfs with pseudocost:PCStruct),
              % store the branches
              shelf_set(Shelf, branches of bfs_shelf,
                        [
                         [[v(Idx), PCStruct, Down, d],
                          bounds(Idx, -1.0Inf..Hi)],
                         [[v(Idx), PCStruct, Up, u],
                          bounds(Idx, Lo..1.0Inf)]
                        ])
        ).
        
bfs_deg_est(Handle, NodeType, _, _) :-
        % branching on best estimate
        bfs_impose_node_state_body(NodeType, Handle),
        Handle = bfs_tree{int_tolerance:IntTol,
                          gubs:GUBs,
                          shelf:Shelf,
                          alpha_min:AlphaMin,
                          alpha_max:AlphaMax,
                          beta_const:BetaConst,
                          beta_pc:BetaPC,
                          beta_lb:BetaLB,
                          pc_init:PCInit,
                          pc_ratio:PCIt,
                          lb_time:LBIt
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node{id:NodeId,
                        objval:NodeCost,
                        frac_vars:Vars
                       },
        ( ( PCInit == calculated, PCIt >= LBIt ) ->
              PCOverLB = true
        ;
              PCOverLB = fail
        ),
        (
            foreacharg(GUB, GUBs),
            foreach(PseudoCost, GPseudoCosts),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(_, ScoreIn, ScoreOut, _),
            param(Handle, NodeType, IntTol, NodeId, NodeCost,
                  AlphaMin, AlphaMax,
                  BetaConst, BetaPC, BetaLB, PCOverLB)
        do
            GUB = bfs_gub{vars:Vars,
                          refs:Refs,
                          pseudocosts:PCStructs
                         },
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Handle, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info with val:Val, Handle),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  PseudoCost = not_stored,
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, and calculate score
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  ( BetaPC =:= 0 ->
                        % only lower bounds in estimate
                        PCD = 0,
                        PCU = 0,
                        PseudoCost = not_stored
                  ;
                        % otherwise get pseudocosts
                        get_gub_pseudocosts(Handle, NodeType,
                                            NodeCost, GUB, NVars, Sum,
                                            PCD, PCU, Initializing),
                        ( call(Initializing) ->
                              PseudoCost = NVars-[PCD, PCU]
                        ;
                              PseudoCost = not_stored
                        )
                  ),
                  ( BetaLB =:= 0 ->
                        % only pseudocosts in estimate
                        LD = 0,
                        LU = 0
                  ;
                        ( call(PCOverLB),
                          arg(NVars, PCStructs, 
                              pseudocost with [pcd_count:0-LD,
                                               pcu_count:0-LU]) ) ->
                            % if pseudocosts have just been calculated and are
                            % stronger, use them as lower bounds
                            true
                  ;
                            % otherwise get lower bounds
                            (
                                for(_, 1, NVars),
                                foreach(Term, Terms),
                                fromto(Vars, [Term|Rest], Rest, _)
                            do
                                true
                            ),
                            get_gub_lowerbounds(Handle, NodeType,
                                                NodeCost, Terms,
                                                LD, LU)
                  ),
                  DownScore is BetaConst + (BetaPC * PCD * Sum) +
                               (BetaLB * LD),
                  UpScore is BetaConst + (BetaPC * PCU * (1-Sum)) +
                             (BetaLB * LU),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = g(GN, NVars, Sum)
                  )
            )
            
        ),
        ( nonvar(Result) ->
              PseudoCosts = GPseudoCosts
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var using deg_est
              (
                  foreach(Var:Val, Vars),
                  count(I, 1, _),
                  fromto(_, ScoreIn, ScoreOut, _),
                  fromto(_, In, Out, Result),
                  foreach(PseudoCost, PseudoCosts),
                  param(Handle, NodeType, NodeCost,
                        AlphaMin, AlphaMax,
                        BetaConst, BetaPC, BetaLB, PCOverLB)
              do
                  Up is ceiling(Val) - Val,
                  Down is Val - floor(Val),
                  ( BetaPC =:= 0 ->
                        % only lower bounds in estimate
                        PCD = 0,
                        PCU = 0,
                        PseudoCost = not_stored
                  ;
                        % otherwise get pseudocosts
                        get_pseudocosts(Handle, NodeType,
                                        NodeCost, Var, Val,
                                        PCD, PCU, Initializing),
                        ( call(Initializing) ->
                              PseudoCost = [PCD, PCU]
                        ;
                              PseudoCost = not_stored
                        )
                  ),
                  ( BetaLB =:= 0 ->
                        % only pseudocosts in estimate
                        LD = 0,
                        LU = 0
                  ;
                        ( call(PCOverLB),
                          get_bfs_attr(Var, Handle,
                                       bfs with [pcd_count:0-LD,
                                                 pcu_count:0-LU]) ) ->
                            % if pseudocosts have just been calculated and are
                            % stronger, use them as lower bounds
                            true
                  ;
                            % otherwise get lower bounds
                            get_lowerbounds(Handle, NodeType,
                                            NodeCost, Var, Val,
                                            LD, LU)
                  ),
                  DownScore is BetaConst + (BetaPC * PCD * Down) +
                               (BetaLB * LD),
                  UpScore is BetaConst + (BetaPC * PCU * Up) +
                             (BetaLB * LU),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = v(I)
                  )
              )
        ),
        shelf_set(Shelf, info of bfs_shelf, Result:PseudoCosts),
        fail.
bfs_deg_est(bfs_tree{shelf:Shelf}, _, Result, PseudoCosts) :-
        shelf_get(Shelf, info of bfs_shelf, Result:PseudoCosts).

% bfs_fracvar/1: for low-level handles
bfs_fracvar(Handle) :-
        ( var(Handle) ->
            error(4, bfs_fracvar(Handle))
        ; Handle = bfs_tree{} ->
            bfs_fracvar_body(Handle)
        ;
            error(5, bfs_fracvar(Handle))
        ).

% fracvar/0: for pools
bfs_fracvar1(Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:fracvar]),
            abort
        ;
            bfs_fracvar_body(Handle)
        ).

bfs_fracvar_body(Handle) :-
        % branching on gub if available or most fractional var
        Handle = bfs_tree{int_tolerance:IntTol,
                          shelf:Shelf,
                          gubs:GUBs
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node{id:NodeId,
                        frac_vars:Vars
                       },
        % check the shelf is empty just in case
        shelf_get(Shelf, branches of bfs_shelf, []),
        (
            foreacharg(GUB, GUBs),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(IntTol, ScoreIn, ScoreOut, _),
            param(Handle, IntTol, NodeId)
        do
            GUB = bfs_gub{refs:Refs},
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Handle, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info{val:Val}, Handle),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  Score is min(Sum, (1 - Sum)),
                  ( Score =< ScoreIn ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = GN:NVars:Sum
                  )
            )
        ),
        ( nonvar(Result) ->
            Result = GN:NVars:Down,
            Up is 1 - Down,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down, % score
                        gub_bound(GN, NVars, =<, 0)],      % branch
                       [Up,   % score
                        gub_bound(GN, NVars, >=, 1)]       % branch
                      ])
        ;
            % there were no GUBs with enough frac vars to be worth
            % branching on, choose a single var
            (
                foreach(_Var:Val, Vars),
                count(I, 1, _),
                fromto(IntTol, DiffIn, DiffOut, _),
                fromto(_, In, Out, Idx:V)
            do
                Diff is abs(round(Val) - Val),
                ( Diff > DiffIn ->
                    DiffOut = Diff,
                    Out = I:Val
                ;
                    DiffOut = DiffIn,
                    Out = In
                )
            ),
            Lo is ceiling(V),
            Up is Lo - V,
            Hi is floor(V),
            Down is V - Hi,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down,
                        bounds(Idx, -1.0Inf..Hi)],
                       [Up,
                        bounds(Idx, Lo..1.0Inf)]
                      ])
        ).

% bfs_enhanced/2: for low-level handles
bfs_enhanced(Handle, Node) :-
        ( var(Handle) ->
            error(4, bfs_enhanced(Handle, Node))
        ; Handle = bfs_tree{} ->
            bfs_enhanced_body(Handle, Node)
        ;
            error(5, bfs_enhanced(Handle, Node))
        ).

% enhanced/1: for pools
bfs_enhanced1(Node, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:enhanced(Node)]),
            abort
        ;
            bfs_enhanced_body(Handle, Node)
        ).

bfs_enhanced_body(Handle, eplex(EplexHandle)) :-
        Handle = bfs_tree{int_tolerance:IntTol,
                          shelf:Shelf,
                          gubs:GUBs
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node{id:NodeId,
                        frac_vars:Vars
                       },
        % check the shelf is empty just in case
        shelf_get(Shelf, branches of bfs_shelf, []),
        (
            foreacharg(GUB, GUBs),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(IntTol, ScoreIn, ScoreOut, _),
            param(Handle, IntTol, NodeId)
        do
            GUB = bfs_gub{refs:Refs},
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Handle, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info{val:Val}, Handle),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  Score is min(Sum, (1 - Sum)),
                  ( Score =< ScoreIn ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = GN:NVars:Sum
                  )
            )
        ),
        ( nonvar(Result) ->
            Result = GN:NVars:Down,
            Up is 1 - Down,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down, % score
                        gub_bound(GN, NVars, =<, 0)],      % branch
                       [Up,   % score
                        gub_bound(GN, NVars, >=, 1)]       % branch
                      ])
        ;
            % there were no GUBs with enough frac vars to be worth
            % branching on, choose a single var
            (
                foreach(Var:Val, Vars),
                foreach(Frac-Val-Var, FracVars),
                fromto(0.0, LIn, LOut, L),
                fromto(1.0, UIn, UOut, U)
            do
                Frac is Val - floor(Val),
                ( Frac < 0.5 ->
                    LOut is max(Frac, LIn),
                    UOut = UIn
                ; Frac > 0.5 ->
                    LOut = LIn,
                    UOut is min(Frac, UIn)
                ;
                    LOut = 0.5,
                    UOut = 0.5
                )
            ),
            LBound is 0.8 * L,
            UBound is 0.2 + 0.8 * U,
            (
                foreach(FracPart-Val-Var, FracVars),
                count(I, 1, _),
                fromto(-1.0Inf, ScoreIn, ScoreOut, _),
                fromto(_, In, Out, Idx:V),
                param(EplexHandle, LBound, UBound)
            do
                ( (FracPart >= LBound, FracPart =< UBound) ->
                    lp_get(EplexHandle, objective, Objective),
                    Objective =.. [_MinMax, Expr],
                    ( bfs_objective_coeff(Expr, Var, Score) ->
                        %EplexHandle = prob with objcoeffs:ObjCoeffs,
                        %lp_var_occurrence(Var, EplexHandle, J),
                        %( member(J:Score, ObjCoeffs) ->
                        true
                    ;
                        Score = 0
                    ),
                    ( Score =< ScoreIn ->
                        ScoreOut = ScoreIn,
                        Out = In
                    ;
                        ScoreOut = Score,
                        Out = I:Val
                    )
                ;
                    ScoreOut = ScoreIn,
                    Out = In
                )
            ),
            Lo is ceiling(V),
            Up is Lo - V,
            Hi is floor(V),
            Down is V - Hi,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down,
                        bounds(Idx, -1.0Inf..Hi)],
                       [Up,
                        bounds(Idx, Lo..1.0Inf)]
                      ])
        ).

% bfs_strong/2: for low-level handles
bfs_strong(Handle, Node) :-
        ( var(Handle) ->
            error(4, bfs_strong(Handle, Node))
        ; Handle = bfs_tree{} ->
            bfs_strong_body(Handle, Node)
        ;
            error(5, bfs_strong(Handle, Node))
        ).

% strong/1: for pools
bfs_strong1(Node, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:strong(Node)]),
            abort
        ;
            bfs_strong_body(Handle, Node)
        ).

% Note to AE: try to get rid of the eplex handle form this call
bfs_strong_body(Handle, eplex(EplexHandle)) :-
        Handle = bfs_tree{shelf:Shelf},
        shelf_get(Shelf, info of bfs_shelf, OldInfo),
        bfs_strong(Handle, eplex(EplexHandle), Result),
        shelf_set(Shelf, info of bfs_shelf, OldInfo),
        % check the shelf is empty just in case
        shelf_get(Shelf, branches of bfs_shelf, []),
        ( Result = g(GN, NVars, Down) ->
            Up is 1 - Down,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down, % score
                        gub_bound(GN, NVars, =<, 0)],      % branch
                       [Up,   % score
                        gub_bound(GN, NVars, >=, 1)]       % branch
                      ])
        ;
            Result = v(Idx),
            bfs_next_node(Handle, SubTree),
            n_trees:n_tree_get(data, SubTree, bfs_node{frac_vars:Vars}),
            VarArr =.. [[]|Vars],
            arg(Idx, VarArr, _Var:Val),
            Lo is ceiling(Val),
            Up is Lo - Val,
            Hi is floor(Val),
            Down is Val - Hi,
            % store the branches
            shelf_set(Shelf, branches of bfs_shelf,
                      [
                       [Down,
                        bounds(Idx, -1.0Inf..Hi)],
                       [Up,
                        bounds(Idx, Lo..1.0Inf)]
                      ])
        ).

bfs_strong(Handle, NodeType, _) :-
        bfs_impose_node_state_body(NodeType, Handle),
        Handle = bfs_tree{int_tolerance:IntTol,
                          gubs:GUBs,
                          shelf:Shelf,
                          alpha_min:AlphaMin,
                          alpha_max:AlphaMax
                         },
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node{id:NodeId,
                        objval:NodeCost,
                        frac_vars:Vars
                       },
        (
            foreacharg(GUB, GUBs),
            count(GN, 1, _),
            fromto(_, In, Out, Result),
            fromto(_, ScoreIn, ScoreOut, _),
            param(Handle, NodeType, IntTol, NodeId, NodeCost,
                  AlphaMin, AlphaMax)
        do
            GUB = bfs_gub{vars:Vars,
                          refs:Refs
                         },
            (
                foreach(Coeff*Var, Refs),
                count(NVars, 1, _),
                fromto([], BPIn, [Coeff:NVars:VTOut|BPIn], BranchPoints),
                fromto(0, VTIn, VTOut, _),
                fromto(0, RTIn, RTOut, RefTotal),
                fromto(0, FIn, FOut, Frac),
                param(Handle, IntTol, NodeId)
            do
                bfs_get_node_info(Var, NodeId,
                                  bfs_node_info{val:Val}, Handle),
                VTOut is VTIn + Val,
                RTOut is RTIn + Coeff*Val,
                ( abs(min(Val-floor(Val),ceiling(Val)-Val)) =< IntTol ->
                      FOut = FIn
                ;
                      FOut is FIn + 1
                )
            ),
            % if there are less than Limit fractional vars in the GUB it
            % is likely better to choose a good single var to branch
            % on, so we ignore this GUB
            % Note to AE: should not be hardwired Limit
            Limit = 2,
            ( Frac =< Limit ->
                  Out = In,
                  ScoreOut = ScoreIn
            ;
                  % find the branchpoint for the GUB, if the sum is
                  % more fractional than the current choice, use it
                  branchpoint(BranchPoints, RefTotal, NVars, Sum),
                  (
                      for(_, 1, NVars),
                      foreach(Term, Terms),
                      fromto(Vars, [Term|Rest], Rest, _)
                  do
                      true
                  ),
                  get_gub_lowerbounds(Handle, NodeType,
                                      NodeCost, Terms,
                                      DownScore, UpScore),
                  Score is AlphaMin * min(DownScore, UpScore) +
                           AlphaMax * max(DownScore, UpScore),
                  ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                        ScoreOut = ScoreIn,
                        Out = In
                  ;
                        ScoreOut = Score,
                        Out = g(GN, NVars, Sum)
                  )
            )
            
        ),
        ( nonvar(Result) ->
              true
        ;
              % there were no GUBs with enough frac vars to be worth
              % branching on, choose a single var
              (
                  foreach(Var:Val, Vars),
                  count(I, 1, _),
                  foreach(Frac-I-Val-Var, FracVars),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U)
              do
                  Frac is Val - floor(Val),
                  ( Frac < 0.5 ->
                        LOut is max(Frac, LIn),
                        UOut = UIn
                  ; Frac > 0.5 ->
                        LOut = LIn,
                        UOut is min(Frac, UIn)
                  ;
                        LOut = 0.5,
                        UOut = 0.5
                  )
              ),
              LBound is 0.8 * L,
              UBound is 0.2 + 0.8 * U,
              (
                  foreach(FracPart-Idx-Val-Var, FracVars),
                  fromto(_, ScoreIn, ScoreOut, _),            
                  fromto(_, In, Out, Result),
                  param(Handle, NodeType, NodeCost,
                        AlphaMin, AlphaMax, LBound, UBound)
              do
                  ( (FracPart >= LBound, FracPart =< UBound) ->
                        get_lowerbounds(Handle, NodeType,
                                        NodeCost, Var, Val,
                                        DownScore, UpScore),
                        Score is AlphaMin * min(DownScore, UpScore) +
                                 AlphaMax * max(DownScore, UpScore),
                        ( (nonvar(ScoreIn), Score =< ScoreIn) ->
                              ScoreOut = ScoreIn,
                              Out = In
                        ;
                              ScoreOut = Score,
                              Out = v(Idx)
                        )
                  ;
                        ScoreOut = ScoreIn,
                        Out = In
                  )
              )
        ),
        shelf_set(Shelf, info of bfs_shelf, Result),
        fail.
bfs_strong(bfs_tree with shelf:Shelf, _, Result) :-
        shelf_get(Shelf, info of bfs_shelf, Result).

% bfs_impose_node_state/2 : for low-level handles
bfs_impose_node_state(NodeType, Handle) :-
        ( var(Handle) ->
            error(4, bfs_impose_node_state(NodeType, Handle))
        ; Handle = bfs_tree with [] ->
            bfs_impose_node_state_body(NodeType, Handle)
        ;
            % not a proper bfs handle
            error(5, bfs_impose_node_state(NodeType, Handle))
        ).

% impose_node_state/1 : for pools
bfs_impose_node_state1(NodeType, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Bfs instance has no tree set up in %w%n",
                   [Pool:impose_node_state(NodeType)]),
            abort
        ;
            bfs_impose_node_state_body(NodeType, Handle)
        ).

bfs_impose_node_state_body(NodeType, Handle) :-
        Handle = bfs_tree{vars:Vars},
        bfs_next_node(Handle, SubTree),
        n_trees:n_tree_get(data, SubTree, Node),
        Node = bfs_node with [
                              id:NodeId,
                              state:(CBasis, RBasis),
                              branches:Branches
                             ],
        % apply branching decisions & propagate
        % call them all as atomic op to limit wakings
        % since we will be doing this often
        call_priority((
                          foreach(Var, Vars),
                          param(NodeId, Handle)
                      do
                          bfs_get_node_info(Var, NodeId,
                                            bfs_node_info with [lo:Lo, hi:Hi],
                                            Handle),
                          set_var_bounds(Var, Lo, Hi)
                      ), 2),
        call_priority((
                          foreach(Branch, Branches)
                      do
                          call(Branch)
                      ), 2),
        ( NodeType = eplex(EplexHandle) ->
              lp_set(EplexHandle, cbasis, CBasis),
              lp_set(EplexHandle, rbasis, RBasis)
        ;
              true
        ).

get_gub_pseudocosts(Handle, NodeType, NodeCost, GUB, BranchPoint, Sum,
                    PseudoCostDown, PseudoCostUp, Initializing) :-
        Handle = bfs_tree{pcd_average:PcDown,
                          pcu_average:PcUp,
                          pc_init:PcInit,
                          pc_ratio:Limit,
                          shelf:Shelf
                         },
        NodeType = eplex(EplexHandle),
        GUB = bfs_gub{vars:Vars,
                      pseudocosts:PCStructs
                     },
        arg(BranchPoint, PCStructs, PseudoCosts),
        PseudoCosts = pseudocost{pcd:PCD,
                                 pcd_count:PCD_Count,
                                 pcu:PCU
                                },
        ( PCD_Count = none ->
              ( PcInit == calculated ->
                  % explicitly calculating initial pseudocosts:
                  shelf_get(Shelf, info of bfs_shelf, OldInfo),
                  estimate_degradation(Handle, NodeType,
                                       lp_add(EplexHandle,
                                              [(=:=):[0*1|Vars]], []),
                                       Limit, ObjValDown),
                  PseudoCostDown is abs((ObjValDown - NodeCost)/Sum),
                  estimate_degradation(Handle, NodeType,
                                       lp_add(EplexHandle,
                                              [(=:=):[-1*1|Vars]], []),
                                       Limit, ObjValUp),
                  PseudoCostUp is abs((ObjValUp - NodeCost)/(1 - Sum)),
                  shelf_set(Shelf, info of bfs_shelf, OldInfo)
              ;
                PcInit == average ->
                    % initializing to current average:
                    PseudoCostDown = PcDown,
                    PseudoCostUp = PcUp
              ;
                    printf(error, "unknown GUB pseudocost"
                           " initialization method %w%n",
                           [PcInit]),
                    flush(ouput),
                    abort
              ),
              Initializing = true,
              setarg(pcd_count of pseudocost, PseudoCosts, 0-PseudoCostDown),
              setarg(pcd of pseudocost, PseudoCosts, PseudoCostDown),
              setarg(pcu_count of pseudocost, PseudoCosts, 0-PseudoCostUp),
              setarg(pcu of pseudocost, PseudoCosts, PseudoCostUp)
        ;
              Initializing = fail,
              PseudoCostDown = PCD,
              PseudoCostUp = PCU
        ).

get_pseudocosts(Handle, NodeType, NodeCost, Var, Val,
		PseudoCostDown, PseudoCostUp, Initializing) :-
        Handle = bfs_tree{pcd_average:PcDown,
                          pcu_average:PcUp,
                          pc_init:PcInit,
                          pc_ratio:Limit,
                          shelf:Shelf
                         },
        get_bfs_attr(Var, Handle, Attr),
        Attr = bfs{pcd:PCD,
                   pcd_count:PCD_Count,
                   pcu:PCU
                  },
        ( PCD_Count = none ->
              ( PcInit == calculated ->
                  % explicitly calculating initial pseudocosts:
                  shelf_get(Shelf, info of bfs_shelf, OldInfo),
                  Hi is floor(Val),
                  estimate_degradation(Handle, NodeType,
                                       set_var_bounds(Var, -1.0Inf, Hi),
                                       Limit, ObjValDown),
                  PseudoCostDown is abs((ObjValDown - NodeCost)/(Val - Hi)),
                  Lo is ceiling(Val),
                  estimate_degradation(Handle, NodeType,
                                       set_var_bounds(Var, Lo, 1.0Inf),
                                       Limit, ObjValUp),
                  PseudoCostUp is abs((ObjValUp - NodeCost)/(Lo - Val)),
                  shelf_set(Shelf, info of bfs_shelf, OldInfo)
              ;
                PcInit == cost ->
                    % initializing to cost coefficient:
                    ( NodeType = eplex(EplexHandle) ->
                         lp_get(EplexHandle, objective, Objective),
                         Objective =.. [_MinMax, Expr],
                         ( bfs_objective_coeff(Expr, Var, Cost) ->
                         %EplexHandle = prob with objcoeffs:ObjCoeffs,
                         %lp_var_occurrence(Var, EplexHandle, J),
                         %( member(J:Cost, ObjCoeffs) ->
                               true
                         ;
                               Cost = 0
                         )
                    ;
                         printf(error, "Don't know how to get the cost"
                                " coefficient for %d in get_pseudocosts/8%n",
                                [Var]),
                         abort
                    ),
                    % Note to AE:
                    % so we have to be able to GET the cost
                    % coefficient from somewhere
                    PseudoCostDown = Cost,
                    PseudoCostUp = Cost
              ;
                PcInit == average ->
                    % initializing to current average:
                    PseudoCostDown = PcDown,
                    PseudoCostUp = PcUp
              ;
                    printf(error, "unknown pseudocost"
                           " initialization method %w%n",
                           [PcInit]),
                    flush(ouput),
                    abort
              ),
              
              Initializing = true,
              
              setarg(pcd_count of bfs, Attr, 0-PseudoCostDown),
              setarg(pcd of bfs, Attr, PseudoCostDown),
              setarg(pcu_count of bfs, Attr, 0-PseudoCostUp),
              setarg(pcu of bfs, Attr, PseudoCostUp)
        ;
              
              Initializing = fail,
              
              PseudoCostDown = PCD,
              PseudoCostUp = PCU
        ).

bfs_objective_coeff(Expr1+Expr2, X, C) :-
        ( bfs_objective_coeff(Expr1, X, C) -> true
        ; bfs_objective_coeff(Expr2, X, C) -> true
        ; fail ).
%bfs_objective_coeff(Expr1+_Expr2, X, C) :-
%        bfs_objective_coeff(Expr1, X, C).
%bfs_objective_coeff(_Expr1+Expr2, X, C) :-
%        bfs_objective_coeff(Expr2, X, C).
bfs_objective_coeff(C*Var, X, C) :-
        Var == X, !.
bfs_objective_coeff(Var, X, 1) :-
        Var == X, !.

get_gub_lowerbounds(Handle, eplex(EplexHandle), NodeCost, Terms, LD, LU) :-
        Handle = bfs_tree{lb_time:Limit, shelf:Shelf},
        shelf_get(Shelf, info of bfs_shelf, OldInfo),
        length(Terms, NVars),
        GLimit is NVars * Limit,
        normalise_cstrs([sum(Terms) =:= 0, sum(Terms) =:= 1],
                        [Cstr1, Cstr2], []),
        estimate_degradation(Handle, NodeType,
                             lp_add(EplexHandle, Cstr1, []),
                             GLimit, ObjValDown),
        LD is abs(ObjValDown - NodeCost),
        estimate_degradation(Handle, NodeType,
                             lp_add(EplexHandle, Cstr2, []),
                             GLimit, ObjValUp),
        LU is abs(ObjValUp - NodeCost),
        shelf_set(Shelf, info of bfs_shelf, OldInfo).

get_lowerbounds(Handle, NodeType, NodeCost, Var, Val, LD, LU) :-
        Handle = bfs_tree{lb_time:Limit, shelf:Shelf},
        shelf_get(Shelf, info of bfs_shelf, OldInfo),
        Hi is floor(Val),
        estimate_degradation(Handle, NodeType,
                             set_var_bounds(Var, -1.0Inf, Hi),
                             Limit, ObjValDown),
        LD is abs(ObjValDown - NodeCost),
        Lo is ceiling(Val),
        estimate_degradation(Handle, NodeType,
                             set_var_bounds(Var, Lo, 1.0Inf),
                             Limit, ObjValUp),
        LU is abs(ObjValUp - NodeCost),
        shelf_set(Shelf, info of bfs_shelf, OldInfo).

estimate_degradation(bfs_tree{shelf:Shelf}, eplex(EplexHandle),
                     Bound, Limit, _DegVal) :-
        shelf_set(Shelf, info of bfs_shelf, 1.0Inf),
        call(Bound),
        lp_get(optimizer, Optimizer),
        ( Optimizer == cplex ->
              lp_get(optimizer_param(iteration_limit), ItLim),
              lp_set(optimizer_param(iteration_limit), Limit)
        ;
              lp_get(EplexHandle, optimizer_param(iteration_limit), ItLim),
              lp_set(EplexHandle, optimizer_param(iteration_limit), Limit)
        ),
        lp_get(EplexHandle, method, Method),
        lp_set(EplexHandle, method, dual),
        set_event_handler(eplex_suboptimal, true/0),
        set_event_handler(eplex_abort, exit_abort/0),
        block( ( lp_solve(EplexHandle, Obj) -> true ; Obj = 1.0Inf ),
               abort,
               % IF there was an abort (probably hit iteration limit
               % before finding a feasible basis) AND presolve was
               % turned on AND the external solver is XPRESS-MP, THEN
               % the problem is not postsolved, and row/column numbers
               % can be completely wrong resulting in solution
               % information being returned in the incorrect order and
               % possibly variable bounds getting out of sync from now
               % on. Currently we avoid this by just disallowing the
               % use of a presolved XPRESS problem with estimate
               % degradation but we could alternatively check here and
               % reload the problem and bounds if necessary
               Obj = 1.0Inf
             ),
        set_event_handler(eplex_suboptimal, eplex_result_handler/2),
        set_event_handler(eplex_abort, eplex_result_handler/2),
        shelf_set(Shelf, info of bfs_shelf, Obj),
        ( Optimizer == cplex ->
              lp_set(optimizer_param(iteration_limit), ItLim)
        ;
              lp_set(EplexHandle, optimizer_param(iteration_limit), ItLim)
        ),
        lp_set(EplexHandle, method, Method),
        fail.
estimate_degradation(bfs_tree with shelf:Shelf, _, _, _, DegVal) :-
        shelf_get(Shelf, info of bfs_shelf, DegVal).

exit_abort :- exit_block(abort).

%-----------------------------------------------------------------------
% Pools
%-----------------------------------------------------------------------

:- local record(bfs_pools). % list of bfs pool names

create_bfs_pool(Pool) :-
	create_constraint_pool(Pool, property(arity) of bfs_constraint_type,
                               [
                                 deg_est/1 -> bfs_deg_est1/2,
                                 strong/1 -> bfs_strong1/2,
                                 enhanced/1 -> bfs_enhanced1/2,
                                 fracvar/0 -> bfs_fracvar1/1,
                                 node_cost/1 -> bfs_node_cost1/2,
                                 impose_node_state/1 -> bfs_impose_node_state1/2,
                                 update_pseudocosts/0 -> bfs_update_pseudocosts1/1,
                                 minimize_eplex_node/1 -> bfs_minimize_eplex_node1/2,
                                 var_get/3 -> bfs_var_get1/4,
                                 integers/1 -> bfs_integers1/2,
                                 get/2 -> bfs_get1/3,
                                 set/2 -> bfs_set1/3,
                                 node_info/2 -> bfs_node_info1/3,
                                 node_info/5 -> bfs_node_info1/6,
                                 statistics/0 -> bfs_statistics1/1,
                                 solve/1 -> bfs_solve1/2,
                                 solver_setup/2 -> bfs_solver_setup1/3,
                                 solver_setup/3 -> bfs_solver_setup1/4,
                                 solver_cleanup/0 -> bfs_solver_cleanup/1,
                                 bfs_branch/1 -> bfs_branch1/2,
                                 branch/2 -> bfs_branch1/3,
                                 global_cut/1 -> bfs_global_cut1/2
                               ]).


bfs_instance(PoolName) :-
        ( lp_get(optimizer, xpress), lp_get(optimizer_version, 13) ->
              % warn the user that there are bugs when using
              % XPRESS-MP 13.26 as external solver
              % I am unsure whether this is related to the bugs
              % already reported to Dash, or something new
              printf(warning_output, "Warning: XPRESS-MP 13.26 is known to give"
                     " incorrect (suboptimal) solutions on some test"
                     " instances%n", [])
        ; true ),
	( is_constraint_pool(PoolName),
	  recorded(bfs_pools, PoolName) % is a bfs pool
	->
            % if pool exists, check if it is currently empty 
	    ( pool_is_empty(PoolName),
	      get_pool_item(PoolName, 0) % has no associated solver
	    ->
		true
	    ;
                printf(error, "Bfs instance still in use in %w%n", [bfs_instance(PoolName)]),
		abort
	    )
	;
%	    ( current_module(PoolName) ->
%		  error(6, bfs_instance(PoolName))
%	    ;
		  record(bfs_pools, PoolName),
		  create_bfs_pool(PoolName)
%	    )
	).

get_pool_handle(Handle, Pool) :-
        get_pool_item(Pool, Handle0),
        ( Handle0 == 0 ->
            Handle = bfs_tree with [pool:Pool],
            init_suspension_list(node_susp of bfs_tree, Handle),
            set_pool_item(Pool, Handle)
        ;
            Handle0 = bfs_tree with [],
            Handle = Handle0
        ).

% ----------------------------------------------------------------------

:- comment(categories, ["Algorithms","Constraints"]).
:- comment(summary, "Best-first search library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").
:- comment(status, prototype).

:- comment(include, bfs_comments).
