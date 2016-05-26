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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK

% TODO:
% 	- many integer 0 are used, which should probably be 0.0
% 	- replace +/2 expressions by lin/1 terms

:- lib(constraint_pools).
:- lib(linearize).
:- lib(hash).
:- lib(eplex).
:- lib(dual_var).
:- lib(bfs).

:- import
     lp_var_non_monotonic_set_bounds/4
   from eplex_.

:- inline(verify/1, t_verify/2).
t_verify(verify(Goal), (Goal->true;writeln(error,verify_failed(Goal)),abort)).
verify(Goal) :- verify(Goal).

% ----------------------------------------------------------------------
%
% Meta-attribute related stuff
% This is the attribute of the generated MP variables
% ----------------------------------------------------------------------

:- export cg_var_print/2.

:- meta_attribute(colgen, [print:cg_var_print/2, unify:unify_colgen/2,
                           get_bounds:cg_get_bounds/3,
                           set_bounds:cg_set_bounds/3]).

:- export struct(
                 colgen(
                        mp_val,		% float (sometimes int 0?)
                        cost,		% float: MP objective coefficient
                        coeffs,		% [cstrid-coeff], keysorted
                        aux,		% 'artificial', 'stabilisation', [],
					%  or defined by subproblem sp_sol{}
                        lo,		% float, possibly int??
                        hi,		% float, possibly int??
                        type,		% 'real' or 'integer'
                        master_prob,    % suspension list
                        solver,		% cg_prob{}
                        next		% colgen{} or []
                       )
                ).

% ----------------------------------------------------------------------
%
% Pools
%
% ----------------------------------------------------------------------

:- export colgen_instance/1.	 % colgen_instance(+PoolName)

% ----------------------------------------------------------------------
%
% Predicates with pool argments (don't reexport these in colgen!)
%
% ----------------------------------------------------------------------

:- export var_dual1/7.              % var_dual/6
:- export get_dual1/3.              % get_dual/2
:- export get_coeff1/3.             % get_coeff/2
:- export get_idx1/3.               % get_idx/2
:- export get_rhs1/3.               % get_rhs/2
:- export always_set_dual1/3.       % always_set_dual/2
:- export set_dual1/3.              % set_dual/2

:- export bp_solve1/2.              % solve/1
:- export cg_solver_setup/3.        % solver_setup/2
:- export cg_solver_setup/4.        % solver_setup/3
:- export cg_integers1/2.           % integers/1
:- export add_cg_pool_constraint/3. % identified_constraint/2
:- export cg_eq/3.		    % =:=/2
:- export cg_ge/3.		    % >=/2
:- export cg_le/3.		    % =</2
:- export cg_sp_count1/2.           % cg_subproblem_count/1
:- export cg_sp_sol/2.              % cg_subproblem_solution/1
:- export cg_valid_columns1/2.      % valid_columns/1
:- export cg_sp_rc_sum/2.           % cg_subproblem_rc_sum/1
:- export cg_optimal_rc1/2.         % optimal_rc/1
:- export cg_minimize/5.            % minimize/4
:- export cg_minimize/4.            % minimize/3
:- export cg_var_get1/4.            % var_get/3
:- export cg_get1/3.                % get/2
:- export cg_branch1/2.             % branch/1
:- export cg_branch1/3.             % branch/2

% ----------------------------------------------------------------------
%
% Predicates with handle argments
%
% ----------------------------------------------------------------------

:- reexport var_dual/7,             % var_dual/6
            get_dual/3,             % get_dual/2
            get_coeff/3,            % get_coeff/2
            get_idx/3,              % get_idx/2
            get_rhs/3,              % get_rhs/2
            always_set_dual/3,      % always_set_dual/2
            set_dual/3              % set_dual/2
            from dual_var.

:- export bp_solve/2.
:- export cg_solver_setup/8.
:- export cg_solver_setup/9.
:- export cg_integers/2.            % should this be a handle predicate?
%:- export cg_add_constraints/3.
:- export cg_sp_count/2.
:- export cg_valid_columns/2.
:- export cg_optimal_rc/2.
:- export cg_minimize/9.
:- export cg_minimize/8.
:- export cg_var_get/4.
:- export cg_get/3.
:- export cg_branch/2.
:- export cg_branch/3.

% ----------------------------------------------------------------------

:- local struct(cg_constraint_type(mp_only, mp_sp, mp_branch)).

:- export op(700, xfx, [$>=, $=, $=<, $::]).

% ----------------------------------------------------------------------
%
% Problem handle structure
%
% ----------------------------------------------------------------------

:- export struct(
     cg_prob(

    % Global State:

	     master_prob,       % the master problem eplex handle
	     bfs_tree,          % bfs_tree for branch-and-price
	     mp_susp,           % suspension list containing the
				% suspension for the MP solver
	     const_obj,         % real: the constant part of
				%       the cost funciton
	     phase1_obj,        % Expr: the artificial variable cost fn
				%       for phase 1 of two-phase
	     sp_solution_call,  % the user supplied subproblem
				%      solution predicate
	     pool,              % the associated constraint pool
	     tolerance,         % float: tolerance for optimality
	     branch_tolerance,  % float: tolerance for optimality
	     info_messages,     % on, off: info message status
	     on_degeneracy,     % stop, continue: should we halt when
				%     we find degeneracy, or continue
				%     (if so the sp solver is assumed
				%      to deal with it)
	     stabilisation,     % off,
				% on(BoundIter, BoundUpdate,
				%     CoeffIter, CoeffUpdate),
				% stab_pred(UpdatePred, StoppingPred):
				%     the policy to perform basis
				%     stabilisation - if off then no
				%     stabilisation is performed,
				%     if on(...) then the default policy
				%     is used with var bounds/coefficients
				%     updated by BoundUpdate/CoeffUpdate
				%     after BoundIter/CoeffIter iterations
				%     resp, otherwise a user defined
				%     policy is employed and UpdatePred/
				%     StoppingPred should be predicates
				%     that perform the updates and test
				%     for stopping conditions.
	     stab_terms,        % [StabTerm1,...,StabTermM]:
				%     list of stabilisation terms 
	     stab_iter_counts,  % stab_counters struct:
				%     record of how many iterations
				%     since stabilisation var bound and
				%     coeff update
	     disallow,          % lp, clp, off:
				%     policy for active prevention of
				%     duplicate columns
	     basis_perturbation,% on, off:
				%     should we try and perturb external
				%     solver basis when appear to be at
				%     optimal when external solver returns
				%     same basis after adding columns:
				%     off - no
				%     on - temporarily set the external
				%          solver to always perturb
	     upper_bound,       % float: current bounds on solution
	     lower_bound,       %     objective value
	     integral_obj,      % atom: yes or no, is the cost of all
	     			%     feasible solutions integral? (currently
				%     unused, but included for later)
	     duals,             % array: array of current
				%     master problem dual values
	     idx_lookup,        % hash table: lookup table to
				%     convert master problem constraint
				%     ids into external solver row ids
	     sp_obj_terms,      % array: array of implicit sum terms
				%     forming the obj function of subproblems
				%     in same order as the duals array
				%     that are their coeffs in it
	     mp_cols_added,     % int: total number of columns added to MP
	     mp_vars,           % [Var1,...,Varj]: list of all mp vars
	     sep_call,          % the user supplied separation predicate
             module,            % module: module in which to call the various
                                %     user-defined predicates

    % Column Management:

	     col_del,           % atom: ?,none:
				%     column deletion strategy (currently
				%     unused, but included for later)
	     shelf,             % cg_shelf{}: store for data that needs to
                                %     be preserved across failure
	     phase,             % indicator for current phase in two-phase
				%     method:
				%        0 : phase 1
				%       -1 : phase 2
				% this is also used as the "dual" multiplier
				%     for cost coefficient of a column in the
				%     subproblem objective
	     new_columns        % record: columns waiting to be
				%     added to the master problem
	    )
	).

:- local struct(cg_shelf(
            info,               % data stored around a bp_node
            optimal_rc,         % best bound on the reduced cost
                                %       of a new column (or 'none')
            sp_sol_cnt          % number SP solutions posted via
                                %      cg_subproblem_solution/1
        )).

% ----------------------------------------------------------------------
%
% Basis stabilisation term structure
%
% ----------------------------------------------------------------------

:- export struct(
                 stab_term(
                           idx,         % the index of the associated
                                        % constraint
                           plus_var,    % the Y^{+} stabilisation var,
                           plus_coeff,  % the current objective coeff
                           plus_bound,  % and upper bound
                           minus_var,   % the Y^{-} stabilisation var,
                           minus_coeff, % the current objective coeff
                           minus_bound  % and upper bound
                          )
                ).

% ----------------------------------------------------------------------
%
% Basis stabilisation counters structure
%
% ----------------------------------------------------------------------

:- export struct(
                 stab_counters(
                               bound_counter, % # iterations
                                              % since last bound
                                              % updates
                               coeff_counter  % # iterations
                                              % since coeff bound
                                              % updates
                              )
                ).
% ----------------------------------------------------------------------
%
% Subproblem handle structure
%
% Note: this info should be moved into the cg_prob structure and accessed
% from there instead, so that we can quietly ignore the sp_prob struct
% and eventually drop altogether.
%
%
% ----------------------------------------------------------------------

:- export struct(
                 sp_prob(
                         master_pool,       % atom: the MP pool to which to
                                            % post cg_subproblem_solution/1 
                         cutoff,            % float: the bound for termination
                                            %        of column generation
                         cost,              % var: dual_var for SP solution
                                            %      cost coefficient in MP
                         coeff_vars,        % [Ai,...,An]: list of dual_var
                                            %      vars for SP solution
                                            %      coefficients in original
                                            %      constraints of MP
                         aux,               % term: arbitrary additional
                                            %       data stored in colgen
                                            %       attribute of MP vars
                         disallow,          % list of cstrs to post if
                                            % actively preventing
                                            % duplicate columns
					    % 2-elem list [Count,Templates]
                         status,            % phase1, phase2, degenerate
                         module,            % obsolete
                         lo, hi,            % implicit variable default bounds
                         type               % implicit variable default type
                         )
                ).
                         
% ----------------------------------------------------------------------
%
% Subproblem solution structure
%
% Note: this should have a boolean "optimal" field defaulting to false
% if not supplied to allow communication of sp_rc_sum without recourse
% to separate predicate as now.
%
% ----------------------------------------------------------------------

:- export struct(
                 sp_sol(
                        cost,              % number: cost coefficient in MP
                        coeff_vars,        % [A1,...,An] list of reals
                                           % or sparse list [Idi-Ai,...]:
                                           %      coefficients in original
                                           %      constraints of MP
                        aux,               % term: arbitrary additional
                                           %       data
                        lo,                % lower bound
                        hi,                % upper bound
                        type               % type integer or real
                       )
                ).

% ----------------------------------------------------------------------
%
% Temporary var info structure
%
% ----------------------------------------------------------------------

:- local struct(
                cg_var_info(
                            lo,
                            hi,
                            val,
                            reduced_cost,
                            type,
                            attr
                           )
               ).

% ----------------------------------------------------------------------
%
% cg attribute handlers
%
% ----------------------------------------------------------------------

unify_colgen(_, Attr) :-
        var(Attr).                   % Ignore if not a colgen var
unify_colgen(Term, Attr) :-
        compound(Attr),
        unify_term_colgen(Term, Attr).

:- mode unify_term_colgen(?, +).
unify_term_colgen(X, Attr) :-
        nonvar(X),                   % colgen var and NONVAR - instantiated
        instantiation_deviates_for_handle(Attr, X).
unify_term_colgen(Y{colgen:AttrY}, AttrX) :-
        -?->
        unify_colgen_colgen(Y, AttrY, AttrX).

unify_colgen_colgen(_, AttrY, AttrX) :-
        var(AttrY),	            % No attribute for this extension
        AttrX = AttrY.	            % Transfer the attribute
unify_colgen_colgen(_, AttrY, AttrX) :-
        nonvar(AttrY),              % colgen var and colgen var
        unify_handles(AttrX, AttrY).

instantiation_deviates_for_handle(ThisAttr, X) :-
        ( compound(ThisAttr) ->
              ThisAttr = colgen{mp_val:Val,
                                cost:Cost,
                                solver:Handle,
                                next:NextAttr},
              ( float(X) =:= 0 ->
                  true
              ; var(Cost) ->
                  printf(warning_output,
                         "Warning: instantiating a variable for"
                         " %w with unspecified cost coefficient to"
                         " %w - assuming zero cost%n", [Handle, X])
              ;
                  Handle = cg_prob{const_obj:Const*One},
                  Const0 is Const + Cost * float(X),
                  setarg(const_obj of cg_prob, Handle, Const0*One)
              ),
              ( X =:= Val -> % instantiated to its mp_val
                    true
              ;     % otherwise wake the mp
                    % should probably post a constraint
                    % to the sp disallowing the
                    % corresponding sp solution
                    schedule_suspensions(master_prob of colgen, ThisAttr),
                    wake
              ),
              instantiation_deviates_for_handle(NextAttr, X)
	;    
              % chain terminated by atom 'end'
              true
        ).

unify_handles(ThisAttrX, AttrY) :-
        ThisAttrX = colgen{solver:Handle,
                           cost:Cost,
                           coeffs:Coeffs,
                           aux:Aux,
                           next:NextAttrX},
        remove_cg_attr_for_handle(Handle, AttrY, ThisAttrY, NextAttrY),
        (   % if Y has an attribute for Handle they must match
            ThisAttrY = colgen{cost:Cost,
                               coeffs:Coeffs,
                               aux:Aux},
            % two variables in the same solver are unified,
            % send an equality constraint for the two columns
            % to the external solver and wake it
            schedule_suspensions(master_prob of colgen, ThisAttrX),
            wake
        ;
            % Y has no attribute for 
            ThisAttrY = end
        ),
        % continue with the rest of X and Ys chains
        unify_handles(NextAttrX, ThisAttrX, NextAttrY).

unify_handles(ThisAttrX, Attr0, AttrY) :-
        ( compound(ThisAttrX) ->
              ( compound(AttrY) ->
                    ThisAttrX = colgen{solver:Handle,
                                       cost:Cost,
                                       coeffs:Coeffs,
                                       aux:Aux,
                                       next:NextAttrX},
                    remove_cg_attr_for_handle(Handle, AttrY, ThisAttrY, NextAttrY),
                    (   % if Y has an attribute for Handle they must match
                        ThisAttrY = colgen{cost:Cost,
                                           coeffs:Coeffs,
                                           aux:Aux},
                        % two variables in the same solver are unified,
                        % send an equality constraint for the two columns
                        % to the external solver and wake it
                        schedule_suspensions(master_prob of colgen, ThisAttrX),
                        wake
                    ;
                        % Y has no attribute for Handle
                        ThisAttrY = end
                    ),
                    % continue with the rest of X and Ys chains
                    unify_handles(NextAttrX, ThisAttrX, NextAttrY)
              ;
                    % Ys chain terminated by atom'end'
                    true
              )
        ;
              % Xs chain terminated by atom 'end'
              % put the rest of Ys chain here
              setarg(next of colgen, Attr0, AttrY)
        ).

% From a cg_attr, removes the attribute corresponding to that for the
% first argument form the chain and returns it. Fails if none found. 
remove_cg_attr_for_handle(Handle, ThisAttr, Attr, RestAttr) :-
        % no need to test for var(ThisAttr) in chain
        ThisAttr = colgen{solver:ThisHandle, next:NextAttr},
	(ThisHandle == Handle ->
             RestAttr = NextAttr,
             setarg(next of colgen, ThisAttr, end),
	     Attr = ThisAttr
	;    
             RestAttr = ThisAttr,
	     dechain_cg_attr_for_handle1(Handle, NextAttr, ThisAttr, Attr)
	).
        
dechain_cg_attr_for_handle1(Handle, ThisAttr, Attr0, Attr) :-
        % no need to test for var(ThisAttr) in chain
        ( ThisAttr = colgen{solver:ThisHandle, next:NextAttr} ->
              (ThisHandle == Handle ->
                   setarg(next of colgen, Attr0, NextAttr),
                   setarg(next of colgen, ThisAttr, end),
                   Attr = ThisAttr
              ;    
                   dechain_cg_attr_for_handle1(Handle, NextAttr, ThisAttr, Attr)
              )
        ;     % chain terminated by atom 'end'
              ThisAttr = Attr
        ).

% get_bounds handler

cg_get_bounds(_Var{colgen:Attr}, Lwb, Upb) ?-
        ( var(Attr) -> true
        ; cg_get_attr_bounds(Attr, -1.0Inf, 1.0Inf, Lwb, Upb) ).

cg_get_attr_bounds(colgen{lo:Lo1, hi:Hi1, next:Next},
                   Lo0, Hi0, Lo, Hi) ?-
        Lo1 =< Hi0,
        Hi1 >= Lo0,
	Lo2 is max(Lo0, Lo1),
	Hi2 is min(Hi0, Hi1),
        cg_get_attr_bounds(Next, Lo2, Hi2, Lo, Hi).
cg_get_attr_bounds(end, Lo0, Hi0, Lo, Hi) ?-
        Lo0 = Lo, Hi0 = Hi.

cg_set_bounds(Var{colgen:Attr}, Lwb, Upb) ?-
        ( var(Attr) -> true
        ; cg_set_attr_bounds(Var, Attr, Lwb, Upb) ).

pos_inf(1e+20) :- !.
pos_inf(1.0Inf) :- !.

neg_inf(-1e+20) :- !.
neg_inf(-1.0Inf) :- !.

cg_set_attr_bounds(Var, Attr, Lwb, Upb) :-
        ( compound(Attr) ->
            Attr = colgen{coeffs:Coeffs,
                          solver:Handle,
                          lo:Lwb0,
                          hi:Upb0,
                          type:Type,
                          next:NextAttr},
            ( Type == real ->
                Lwb1 = Lwb,
                Upb1 = Upb
            ; Type == integer ->
                ( neg_inf(Lwb) -> Lwb1 = Lwb ; Lwb1 is fix(ceiling(Lwb)) ),
                ( pos_inf(Upb) -> Upb1 = Upb ; Upb1 is fix(floor(Upb)) )
            ),
            ( Lwb1 > Lwb0 ->
                ( (neg_inf(Lwb0) ; neg_inf(Lwb1)) ->
                    Handle = cg_prob{master_prob:MPHandle},
                    ( var(MPHandle) ->
                        % master problem lp not set up yet, will be
                        % taken care of in cg_solver_setup
                        true
                    ; lp_var_occurrence(Var, MPHandle, _I) ->
                        lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                    ;
                        % Var not yet added to master problem lp, will be
                        % taken care of when added
                        true
                    ),
                    LwbDiff = 0
                ;
                    LwbDiff is Lwb1 - Lwb0
                ),
                setarg(lo of colgen, Attr, Lwb1)
            ;
                LwbDiff = 0
            ),
            ( Upb1 < Upb0 ->
                ( (pos_inf(Upb0) ; pos_inf(Upb1)) ->
                    Handle = cg_prob{master_prob:MPHandle},
                    ( var(MPHandle) ->
                        % master problem lp not set up yet, will be
                        % taken care of in cg_solver_setup
                        true
                    ; lp_var_occurrence(Var, MPHandle, _I) ->
                        lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                    ;
                        % Var not yet added to master problem lp, will be
                        % taken care of when added
                        true
                    ),
                    UpbDiff = 0
                ;
                    UpbDiff is Upb1 - Upb0
                ),
                setarg(hi of colgen, Attr, Upb1)
            ;
                UpbDiff = 0
            ),
            ( LwbDiff =:= 0, UpbDiff =:= 0 ->
                true
            ;
                Handle = cg_prob{master_prob:MPHandle,
                                 sp_solution_call:SolveSubProblem},
                ( var(MPHandle) ->
                    % master problem lp not set up yet, will be
                    % taken care of in cg_solver_setup
                    true
                ; lp_var_occurrence(Var, MPHandle, _I) ->
                    lp_var_set_bounds(MPHandle, Var, Lwb1, Upb1)
                ;
                    % Var not yet added to master problem lp, will be
                    % taken care of when added
                    true
                ),
                ( var(SolveSubProblem) ->
                    true
                ; var(Coeffs) ->
                    true
                ;
                    arg(1, SolveSubProblem, sp_prob{coeff_vars:DualVars}),
                    (
                        foreach(DualVar, DualVars),
                        param(Handle, Coeffs, LwbDiff, UpbDiff)
                    do
                        get_idx(DualVar, Ident, Handle),
                        ( once member(Ident-Val, Coeffs) ->
                            get_lhs_range(DualVar, Lo0, Hi0),
                            Lo1 is Lo0 + Val*LwbDiff,
                            Hi1 is Hi0 + Val*UpbDiff,
                            set_lhs_range(DualVar, Lo1, Hi1)
                        ;
                            true
                        )
                    )
                )
            ),
            cg_set_attr_bounds(Var, NextAttr, Lwb, Upb)
        ;    
            % chain terminated by atom 'end'
            true
        ).

cg_var_print(_{colgen:Attr}, Printed) ?-
        nonvar(Attr), 
        printed_cg_attributes(Attr, Printed).

printed_cg_attributes(Attr, Printed) :-
        ( compound(Attr) ->
              Attr = colgen{solver:Handle,
                            mp_val:Val,
                            cost:Cost,
                            coeffs:Coeffs,
                            aux:Aux,
                            lo:Lo,
                            hi:Hi,
                            next:NextAttr},
              Printed = [Handle:[mp_val:Val, cost:Cost, coeffs:Coeffs,
                               aux:Aux, lo:Lo, hi:Hi]|Rest],
              printed_cg_attributes(NextAttr, Rest)
	;    
              % chain terminated by atom 'end'
              Printed = []
        ).

get_cg_attr(X{colgen:Attr0}, Handle, Attr) ?-
        ( var(Attr0) ->
              new_cg_attr(X, Handle, Attr)
        ;
              Attr0 = colgen{solver:Handle0,next:Next},
              % should not fail unless Attr0 incorrect
              ( Handle0 == Handle ->
                    Attr = Attr0
              ;
                    get_cg_attr1(Next, Attr0, Handle, Attr)
              )
        ).
get_cg_attr(X, Handle, Attr) :-           % make a new colgen variable
        free(X),
        new_cg_attr(X, Handle, Attr).

get_cg_attr1(ThisAttr, Attr0, Handle, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_cg_attrstruct(Handle, Attr),
	setarg(next of colgen, Attr0, Attr).
get_cg_attr1(ThisAttr, _Attr0, Handle, Attr) :-
        ThisAttr = colgen{solver:Handle0,next:Next},
        ( Handle0 == Handle ->
              Attr = ThisAttr
        ;
              get_cg_attr1(Next, ThisAttr, Handle, Attr)
        ).

new_cg_attr(X, Handle, Attr) :-           % make a new colgen variable:
        new_cg_attrstruct(Handle, Attr),
        add_attribute(X, Attr, colgen). % and add a colgen attribute

:- mode new_cg_attrstruct(+,?).
new_cg_attrstruct(Handle, Attr) :-
        new_cg_attrstruct(Handle, _, _, _, _, _, _, Attr).


%:- mode new_cg_attrstruct(+,?,?,?,?,?,?,-).
new_cg_attrstruct(Handle, Obj, Coeffs, Lo, Hi, Type, Info, Attr) :-
        ( Lo = -1.0Inf -> true ; true ),
        ( Hi = 1.0Inf -> true ; true ),
        ( Type = real -> true ; true ),
        Attr = colgen{mp_val:0,
                      solver:Handle,
                      cost:Obj,
                      coeffs:Coeffs,
                      lo:Lo,
                      hi:Hi,
                      type:Type,
                      aux:Info,
                      next:end},
        init_suspension_list(master_prob of colgen, Attr).


% From a cg_attr, searches for the attribute corresponding to that for the
% first argument. Fails if none found. 
get_cg_attr_for_handle(Handle, Attr0, Attr) :-
        compound(Attr0), 
	get_cg_attr_for_handle1(Handle, Attr0, Attr).

get_cg_attr_for_handle1(Handle, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = colgen{solver:Handle0,next:NextAttr},
	(Handle0 == Handle ->
	     Attr0 = Attr
	;    
	     get_cg_attr_for_handle1(Handle, NextAttr, Attr)
	).

% var_dual/6: for pools
var_dual1(Var, Dual, Coeff, Idx, Type, Rhs, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:var_dual(Var, Dual, Coeff, Idx, Type, Rhs)]),
	    abort
	;
            % make sure we consistently have floats in the attribute
            FDual is float(Dual),
            FRhs is float(Rhs),
	    var_dual(Var, FDual, Coeff, Idx, Type, FRhs, Handle)
	).        

% get_dual/2: for pools
get_dual1(Var, Dual, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:get_dual(Var, Dual)]),
	    abort
	;
	    get_dual(Var, Dual, Handle)
	).

% get_coeff/2: for pools
get_coeff1(Var, Coeff, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:get_coeff(Var, Coeff)]),
	    abort
	;
	    get_coeff(Var, Coeff, Handle)
	).

% get_idx/2: for pools
get_idx1(Var, Idx, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:get_idx(Var, Idx)]),
	    abort
	;
	    get_idx(Var, Idx, Handle)
	).

% get_rhs/2: for pools
get_rhs1(Var, Rhs, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:get_rhs(Var, Rhs)]),
	    abort
	;
	    get_rhs(Var, Rhs, Handle)
	).

% always_set_dual/2: for pools
always_set_dual1(Var, Dual, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:always_set_dual(Var, Dual)]),
	    abort
	;
	    always_set_dual(Var, Dual, Handle)
	).

% set_dual/2: for pools
set_dual1(Var, Dual, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:set_dual(Var, Dual)]),
	    abort
	;
	    set_dual(Var, Dual, Handle)
	).

% cg_var_get/4: for low-level handles
cg_var_get(Handle, X, What, Val) :-
        ( var(Handle) ->
            error(4, cg_var_get(Handle, X, What, Val))
        ; Handle = cg_prob{} ->
            cg_var_get_body(Handle, X, What, Val)
        ;
            error(5, cg_var_get(Handle, X, What, Val))
        ).

% var_get/3: for pools
cg_var_get1(X, What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_var_get(X, What, Val)]),
	    abort
	;
	    cg_var_get_body(Handle, X, What, Val)
	).

cg_var_get_body(Handle, X, node_val, Val) ?- !,
        Handle = cg_prob{bfs_tree:BfsHandle},
        bfs_var_get(BfsHandle, X, node_val, Val).
cg_var_get_body(Handle, X, reduced_cost, Val) ?- !,
        Handle = cg_prob{bfs_tree:BfsHandle},
        bfs_var_get(BfsHandle, X, node_rc, Val).
cg_var_get_body(Handle, X, mp_val, Val) ?- !,
        cg_var_mp_val(Handle, X, Val).
cg_var_get_body(Handle, X, cost, Val) ?- !,
        cg_var_cost(Handle, X, Val).
cg_var_get_body(Handle, X, coeffs, Val) ?- !,
        cg_var_coeffs(Handle, X, Val).
cg_var_get_body(Handle, X, aux, Val) ?- !,
        cg_var_aux(Handle, X, Val).
cg_var_get_body(Handle, X, What, Val) ?-
	error(6, cg_var_get_body(Handle, X, What, Val)).

cg_var_mp_val(Handle, _{colgen:Attr0}, Sol) ?-
	get_cg_attr_for_handle(Handle, Attr0, colgen{mp_val:Sol}).
cg_var_mp_val(_, X, Sol) :-
	integer(X),
	Sol is float(X).
cg_var_mp_val(_, X, Sol) :-
	real(X),
        X = Sol.

cg_var_cost(Handle, _{colgen:Attr0}, Cost) ?-
	get_cg_attr_for_handle(Handle, Attr0, colgen{cost:Cost}).

cg_var_coeffs(Handle, _{colgen:Attr0}, Coeffs) ?-
	get_cg_attr_for_handle(Handle, Attr0, colgen{coeffs:Coeffs}).

cg_var_aux(Handle, _{colgen:Attr0}, Aux) ?-
	get_cg_attr_for_handle(Handle, Attr0, colgen{aux:Aux}).

% cg_integers/2: for low-level handles
cg_integers(Handle, Ints) :-
        ( var(Handle) ->
            error(4, cg_integers(Handle, Ints))
        ; Handle = cg_prob{} ->
            cg_integers_body(Handle, Ints)
        ;
            error(5, cg_integers(Handle, Ints))
        ).

% integers/1: for pools
cg_integers1(Ints, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:integers(Ints)]),
	    abort
	;
	    cg_integers_body(Handle, Ints)
	).

cg_integers_body(Handle, Ints) :-
        ( Handle = cg_prob{bfs_tree:[],info_messages:OnOff} ->
              bfs_solver_setup(BfsHandle, min, bp_node(Handle),
                               [info_messages(OnOff),
                                separation(bp_separate(Handle))]),
              setarg(bfs_tree of cg_prob, Handle, BfsHandle)
        ; Handle = cg_prob{bfs_tree:BfsHandle} ),
        ( var(Ints) ->
              get_cg_attr(Ints, Handle, Attr),
              setarg(type of colgen, Attr, integer)
        ;
              (
                  foreach(Int, Ints),
                  param(Handle)
              do
                  get_cg_attr(Int, Handle, Attr),
                  setarg(type of colgen, Attr, integer)
              )
        ),
        bfs_integers(BfsHandle, Ints).

% ----------------------------------------------------------------------
% The user-level constraints
% ----------------------------------------------------------------------

% what should this do? look at cg_solver_setup_body
%cg_add_constraints(Handle, Cstrs, Ids).

cg_range(X, Lo..Hi, Pool) :-
        get_pool_handle(Handle, Pool),
        get_cg_attr(X, Handle, _Attr), % make sure it is a var for Pool
        set_var_bounds(X, Lo, Hi).

cg_eq(X, Y, Pool) :- add_cg_pool_constraint(X$=Y, _Id, Pool).
cg_ge(X, Y, Pool) :- add_cg_pool_constraint(X$>=Y, _Id, Pool).
cg_le(X, Y, Pool) :- add_cg_pool_constraint(X$=<Y, _Id, Pool).

add_cg_pool_constraint(Cstr, Ident, Pool) :-
	cg_normalise_cstr(Cstr, Norm0, CoeffVar, Coeff),
	!,
        get_pool_handle(Handle, Pool),
        Handle = cg_prob{idx_lookup:Lookup,master_prob:MPHandle},
        ( nonvar(Ident) ->
            true
        ;
            Id = Ident
        ),
        suspend(hash_set(Lookup, Ident, Id), 1, Id->inst),
        ( nonvar(CoeffVar) ->
            % only involves existing MP vars
            try_propagate_bounds(Norm0, Norm),
            ( var(Norm) ->
                true     % constraint simplified away
            ; nonvar(MPHandle) ->
                lp_add_indexed(MPHandle, [Norm], [], [Id])
            ;
                post_typed_pool_constraint(Pool,
                                           mp_only of cg_constraint_type,
                                           Id:Norm)
            )
        ;
            % involves MP vars to be generated by SPs
            % cannot propagate bounds yet
            % give the CoeffVar of the Lambda vars to be generated
            % a dual_var attribute
            Norm0 = Sense:[Val*_One|_],
            Rhs is float(-Val),
            var_dual(CoeffVar, 0.0, Coeff, Ident, Sense, Rhs, Handle),
            post_typed_pool_constraint(Pool,
                                       mp_sp of cg_constraint_type,
                                       [CoeffVar, Id]:Norm0)
	).
add_cg_pool_constraint(Cstr, _Id, Pool) :-
	error(5, Pool:Cstr).

try_propagate_bounds(NormIn, NormOut) :-
	NormIn = Sense:[Cst*_|Lhs],
	( Lhs == [] ->			% ground: check immediately
	    satisfied(Sense, Cst)
	; Lhs = [C*X] ->		% single var: update its bound
	    Bound is -Cst/C,
	    swap_sense(C, Sense, Sense1),
            ( Sense1 == (=<) -> set_var_bounds(X, -1.0Inf, Bound)
            ; Sense1 == (>=) -> set_var_bounds(X, Bound, 1.0Inf)
            ; set_var_bounds(X, Bound, Bound) ), % may bind X!
	    wake
	;				
	    NormIn = NormOut
	).

    satisfied((=<), C) :- C =< 0.
    satisfied((>=), C) :- C >= 0.
    satisfied((=:=), C) :- C =:= 0.

    swap_sense(C, (=<), (>=)) :- C < 0, !.
    swap_sense(C, (>=), (=<)) :- C < 0, !.
    swap_sense(_, S, S).

% cg_sp_count/2: for low-level handles
%:- deprecated(cg_sp_count/2, "No longer necessary").
cg_sp_count(Handle, P) :-
        ( var(Handle) ->
            error(4, cg_sp_count(Handle, P))
        ; var(P) ->
            error(4, cg_sp_count(Handle, P))
        ; Handle = cg_prob{}, integer(P) ->
            cg_sp_count_body(Handle, P)
        ;
            error(5, cg_sp_count(Handle, P))
        ).

% cg_subproblem_count/1: for pools
cg_sp_count1(P, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_subproblem_count(P)]),
	    abort
	;
	    cg_sp_count_body(Handle, P)
	).

cg_sp_count_body(_Handle, _P).

:- deprecated(cg_sp_sol/2, "Use valid_columns/1").
% cg_subproblem_solution/1: for pools
cg_sp_sol(ColSpecs, Pool) :-
        cg_valid_columns1(ColSpecs, Pool).

% valid_columns/1: for pools
cg_valid_columns1(ColSpecs, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:valid_columns(ColSpecs)]),
	    abort
	;
            Handle = cg_prob{shelf:Shelf},
            shelf_inc(Shelf, sp_sol_cnt of cg_shelf),	% suppress auto-posting
	    cg_valid_columns(Handle, ColSpecs)
	).

% cg_valid_columns/2: for low-level handles
cg_valid_columns(Handle, ColSpecs) :-
        ( var(Handle) ->
            error(4, cg_valid_columns(Handle, ColSpecs))
        ; var(ColSpecs) ->
            error(4, cg_valid_columns(Handle, ColSpecs))
        ; Handle = cg_prob{new_columns:NewColRec} ->
            % recorda for compatibility, should be recordz
            ( ColSpecs = sp_sol{} ->
                recorda(NewColRec, ColSpecs)
            ;
                ( foreach(ColSpec, ColSpecs), param(NewColRec) do
                    ColSpec = sp_sol{},
                    recorda(NewColRec, ColSpecs)
                )
            )
        ;
            error(5, cg_valid_columns(Handle, ColSpecs))
        ).


% Set optimal RC (during SP solution process): the SP solver should
% do that iff it computes an RC-optimal column, because we can derive
% an MP lower bound from it.

% cg_optimal_rc/2: for low-level handles
cg_optimal_rc(Handle, RCOpt) :-
        ( var(Handle) ->
            error(4, cg_optimal_rc(Handle, RCOpt))
        ; var(RCOpt) ->
            error(4, cg_optimal_rc(Handle, RCOpt))
        ; Handle = cg_prob{shelf:Shelf}, number(RCOpt) ->
            shelf_set(Shelf, optimal_rc of cg_shelf, RCOpt)
        ;
            error(5, cg_optimal_rc(Handle, RCOpt))
        ).

% optimal_rc/1: for pools
cg_optimal_rc1(RCOpt, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:optimal_rc(RCOpt)]),
	    abort
	;
	    cg_optimal_rc(Handle, RCOpt)
	).

:- deprecated(cg_sp_rc_sum/2, "Use optimal_rc/1").
% subproblem_rc_sum/1: for pools
cg_sp_rc_sum(SPRCSum, Pool) :-
        cg_optimal_rc1(SPRCSum, Pool).


% cg_branch/2: for low-level handles
cg_branch(Handle, Branch) :-
        ( var(Handle) ->
            error(4, cg_branch(Handle, Branch))
        ; Handle = cg_prob{} ->
            cg_branch_body(Handle, 0, Branch)
        ;
            error(5, cg_branch(Handle, Branch))
        ).

% cg_branch/3: for low-level handles
cg_branch(Handle, Score, Branch) :-
        ( var(Handle) ->
            error(4, cg_branch(Handle, Score, Branch))
        ; Handle = cg_prob{} ->
            cg_branch_body(Handle, Score, Branch)
        ;
            error(5, cg_branch(Handle, Score, Branch))
        ).

% branch/1: for pools
cg_branch1(Branch, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:branch(Branch)]),
	    abort
	;
	    cg_branch_body(Handle, 0, Branch)
	).

% branch/2: for pools
cg_branch1(Score, Branch, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:branch(Score, Branch)]),
	    abort
	;
	    cg_branch_body(Handle, Score, Branch)
	).

cg_branch_body(cg_prob{shelf:Shelf}, Score, Branch) :-
        shelf_get(Shelf, info of cg_shelf, Branches),
        shelf_set(Shelf, info of cg_shelf, [Score:Branch|Branches]).

cg_info_message(cg_prob{info_messages:OnOff}, String, Vars) :-
        ( OnOff == on -> printf(String, Vars), flush(output) ; true ).

% cg_get/3: for low-level handles
cg_get(Handle, What, Val) :-
        ( var(Handle) ->
            error(4, cg_get(Handle, What, Val))
        ; Handle = cg_prob{} ->
            cg_get_body(Handle, What, Val)
        ;
            error(5, cg_get(Handle, What, Val))
        ).

% get/2: for pools
cg_get1(What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_get(What, Val)]),
	    abort
	;
	    cg_get_body(Handle, What, Val)
	).

cg_get_body(Handle, obj_val, ObjVal) ?- !,
        Handle = cg_prob{master_prob:MPHandle,
                         mp_vars:Vars,
                         const_obj:Const*_One},
        (
            foreach(Var, Vars),
            fromto(Const, In, Out, ObjVal),
            param(Handle, MPHandle)
        do
            ( nonvar(Var) ->
                  Out = In
            ;
                  cg_var_cost(Handle, Var, Cost),
                  lp_var_get(MPHandle, Var, solution, VarVal),
                  Out is In + Cost*VarVal
            )
        ).
cg_get_body(Handle, sp_obj, Val) :- !,
        cg_get_body(Handle, sp_obj(all), Val).
cg_get_body(Handle, sp_obj(Idents), Val) :- !,
        Handle = cg_prob{duals:DualArr,
                         idx_lookup:Lookup,
                         sp_obj_terms:TermArr},
        ( Idents == all ->
              (
                  for(I,1,arity(TermArr)),
                  fromto(Val, Out, In, []),
                  param(TermArr,DualArr)
              do
                  arg(I, TermArr, Term),
                  arg(I, DualArr, Dual),
                  ( nonvar(Term), Term =:= 0 -> Out = In
                  ; Out = [Dual*Term|In] )
              )
        ; Idents = [_|_] ->
              (
                  foreach(Ident, Idents),
                  foreach(Dual*Term, Val),
                  param(Lookup, DualArr, TermArr)
              do
                  hash_get(Lookup, Ident, Id),
                  Id1 is Id + 1,
                  arg(Id1, DualArr, Dual),
                  arg(Id1, TermArr, Term)
              )
        ;
              hash_get(Lookup, Idents, Id),
              Id1 is Id + 1,
              arg(Id1, DualArr, Dual),
              arg(Id1, TermArr, Term),
              Val = Dual*Term
        ).
cg_get_body(Handle, dual(Idents), Val) :- !,
        Handle = cg_prob{duals:DualArr,
                         idx_lookup:Lookup},
        ( Idents == all ->
              DualArr =.. [[]|Val]
        ; Idents = [_|_] ->
              (
                  foreach(Ident, Idents),
                  foreach(V, Val),
                  param(Lookup, DualArr)
              do
                  hash_get(Lookup, Ident, Id),
                  Id1 is Id + 1,
                  arg(Id1, DualArr, V)
              )
        ;
              hash_get(Lookup, Idents, Id),
              Id1 is Id + 1,
              arg(Id1, DualArr, Val)
        ).
cg_get_body(Handle, column_count, Val) ?- !,
        Handle = cg_prob{mp_cols_added:Val}.
cg_get_body(Handle, unsatisfiable_cstrs, Val) ?- !,
        Handle = cg_prob{sp_solution_call:SolveSubProblem},
        arg(1, SolveSubProblem, sp_prob{coeff_vars:DualVars}),
        (
            foreach(DualVar, DualVars),
            fromto(Val, Out, In, []),
            param(Handle)
        do
            ( satisfiable_primal_cstr(DualVar, Handle) -> Out = In ; Out = [DualVar|In] )
        ).
cg_get_body(Handle, satisfiable_cstrs, Val) ?- !,
        Handle = cg_prob{sp_solution_call:SolveSubProblem},
        arg(1, SolveSubProblem, sp_prob{coeff_vars:DualVars}),
        (
            foreach(DualVar, DualVars),
            fromto(Val, Out, In, []),
            param(Handle)
        do
            ( satisfiable_primal_cstr(DualVar, Handle) -> Out = [DualVar|In] ; Out = In )
        ).
cg_get_body(Handle, frac_vars, Val) ?- !,
        Handle = cg_prob{bfs_tree:BfsHandle},
        bfs_get(BfsHandle, frac_vars, Val).
cg_get_body(Handle, generated_non_zero_vars, Val) ?- !,
        Handle = cg_prob{mp_vars:Vars},
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Handle)
        do
            ( nonvar(Var) ->
                  ( Var > 1e-05 -> Out = [Var|In] ; Out = In )
            ;
                  get_cg_attr(Var, Handle, colgen{mp_val:Sol, coeffs:Coeffs}),
                  ( abs(Sol) =< 1e-05 -> Out = In
                  ; Coeffs == [] -> Out = In
                  ; Out = [Var|In] )
            )
        ).
cg_get_body(Handle, non_zero_vars, Val) ?- !,
        Handle = cg_prob{mp_vars:Vars},
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Handle)
        do
            ( nonvar(Var) ->
                  ( Var > 1e-05 -> Out = [Var|In] ; Out = In )
            ;
                  get_cg_attr(Var, Handle, colgen{mp_val:Sol}),
                  ( abs(Sol) =< 1e-05 -> Out = In ; Out = [Var|In] )
            )
        ).
cg_get_body(Handle, generated_vars, Val) :- !,
        Handle = cg_prob{mp_vars:Vars},
        (
            foreach(Var, Vars),
            fromto(Val, Out, In, []),
            param(Handle)
        do
            ( nonvar(Var) ->
                  Out = In
            ;
                  get_cg_attr(Var, Handle, colgen{coeffs:Coeffs}),
                  ( Coeffs == [] -> Out = In
                  ; Out = [Var|In] )
            )
        ).
cg_get_body(Handle, vars, Val) ?- !,
        Handle = cg_prob{mp_vars:Val}.
cg_get_body(Handle, sep_goal, Val) ?- !,
        Handle = cg_prob{sep_call:(call(Val)@_Module)}.
cg_get_body(Handle, sp_solver, Val) ?- !,
        Handle = cg_prob{sp_solution_call:Val}.
cg_get_body(Handle, stab_coeff_minus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, stab_term{minus_coeff:Val}).
cg_get_body(Handle, stab_coeff_plus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, stab_term{plus_coeff:Val}).
cg_get_body(Handle, stab_bound_minus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, stab_term{minus_bound:Val}).
cg_get_body(Handle, stab_bound_plus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, stab_term{plus_bound:Val}).
cg_get_body(Handle, What, Val) :-
	error(6, cg_get_body(Handle, What, Val)).

% cg_set/3: for low-level handles
cg_set(Handle, What, Val) :-
        ( var(Handle) ->
            error(4, cg_set(Handle, What, Val))
        ; Handle = cg_prob{} ->
            cg_set_body(Handle, What, Val)
        ;
            error(5, cg_set(Handle, What, Val))
        ).

% set/2: for pools
cg_set1(What, Val, Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:cg_set(What, Val)]),
	    abort
	;
	    cg_set_body(Handle, What, Val)
	).

cg_set_body(Handle, disallow, Val) ?- !,
        setarg(disallow of cg_prob, Handle, Val).
cg_set_body(Handle, int_tolerance, Val) ?- !,
        setarg(tolerance of cg_prob, Handle, Val).
cg_set_body(Handle, basis_perturbation, Val) ?- !,
        setarg(basis_perturbation of cg_prob, Handle, Val).
cg_set_body(Handle, info_messages, Val) ?- !,
        setarg(info_messages of cg_prob, Handle, Val),
        Handle = cg_prob{bfs_tree:BfsHandle},
        ( BfsHandle == [] -> true ; bfs_set(BfsHandle, info_messages, Val) ).
cg_set_body(Handle, on_degeneracy, Val) ?- !,
        setarg(on_degeneracy of cg_prob, Handle, Val).
cg_set_body(Handle, stabilisation, Val) ?- !,
        setarg(stabilisation of cg_prob, Handle, Val).
cg_set_body(Handle, stab_coeff_minus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, StabTerm),
        setarg(minus_coeff of stab_term, StabTerm, Val).
cg_set_body(Handle, stab_coeff_plus(Ident), Val) :- !,
        Handle = cg_prob{stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, StabTerm),
        setarg(plus_coeff of stab_term, StabTerm, Val).
cg_set_body(Handle, stab_bound_minus(Ident), Val) :- !,
        Handle = cg_prob{master_prob:MPHandle,
                         stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, StabTerm),
        StabTerm = stab_term{minus_var:Yminus},
        lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Val),
        setarg(minus_bound of stab_term, StabTerm, Val).
cg_set_body(Handle, stab_bound_plus(Ident), Val) :- !,
        Handle = cg_prob{master_prob:MPHandle,
                         stab_terms:StabTerms,
                         idx_lookup:Lookup},
        hash_get(Lookup, Ident, Id),
        get_stab_term(StabTerms, Id, StabTerm),
        StabTerm = stab_term{plus_var:Yplus},
        lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Val),
        setarg(plus_bound of stab_term, StabTerm, Val).
cg_set_body(Handle, What, Val) :-
	error(6, cg_set_body(Handle, What, Val)).

% cg_statistics/1: for low-level handles
cg_statistics(Handle) :-
        ( var(Handle) ->
            error(4, cg_statistics(Handle))
        ; Handle = cg_prob{} ->
            cg_statistics_body(Handle)
        ;
            error(5, cg_statistics(Handle))
        ).

% statistics/0: for pools
cg_statistics1(Pool) :-
        get_pool_handle(Handle, Pool), 
	( Handle == 0 ->
	    printf(error, "Colgen instance has no solver set up in %w%n",
		[Pool:statistics]),
	    abort
	;
	    cg_statistics_body(Handle)
	).

cg_statistics_body(Handle) :-
        Handle = cg_prob{bfs_tree:BfsHandle},
        ( BfsHandle == [] -> true ; bfs_statistics(BfsHandle) ).

get_stab_term([StabTerm|StabTerms], Id, Val) :-
        StabTerm = stab_term{idx:Idx},
        ( Idx == Id ->
            Val = StabTerm
        ;
            get_stab_term(StabTerms, Id, Val)
        ).

% ----------------------------------------------------------------------
% The optimisation predicates
% ----------------------------------------------------------------------

% cg_minimize/8,9: for low-level handles
:- tool(cg_minimize/8, cg_minimize0/9).

cg_minimize0(MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars, SolveSubProblem, Obj, ObjVal, Module) :-
        cg_minimize(MPCstrs, MPIdxs,
                    MPSPCstrs, MPSPIdxs, CoeffVars,
                    SolveSubProblem, Obj, [],
                    ObjVal, Module).

:- tool(cg_minimize/9, cg_minimize/10).

cg_minimize(MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                SolveSubProblem, Obj, OptionList, ObjVal, Module) :-
        cg_minimize_body(_Handle, MPCstrs, MPIdxs,
                         MPSPCstrs, MPSPIdxs, CoeffVars,
                         SolveSubProblem, Obj, OptionList, ObjVal,
                         _, Module).

% minimize/4,5: for pools
:- tool(cg_minimize/4, cg_minimize0/5).

cg_minimize0(SolveSubProblem, Obj, ObjVal, Pool, Module) :-
        cg_minimize(SolveSubProblem, Obj, [], ObjVal, Pool, Module).

:- tool(cg_minimize/5, cg_minimize/6).

cg_minimize(SolveSubProblem, Obj, OptionList, ObjVal, Pool, Module) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Colgen instance has no solver in %w%n",
                   [Pool:minimize(SolveSubProblem, Obj, OptionList, ObjVal)]),
            abort
        ;
            cg_pool_collect_mp_sp_cstrs(Pool, MPCstrs, MPIdxs,
                                        MPSPCstrs, MPSPIdxs, CoeffVars),
            cg_minimize_body(Handle, MPCstrs, MPIdxs,
                             MPSPCstrs, MPSPIdxs, CoeffVars,
                             SolveSubProblem, Obj, OptionList, ObjVal,
                             Pool, Module)
        ).

cg_pool_collect_mp_sp_cstrs(Pool, MPCstrs, MPIdxs,
                            MPSPCstrs, MPSPIdxs, CoeffVars) :-
        % collect any constraints only involving known MP vars
        collect_typed_pool_constraints(Pool, mp_only of cg_constraint_type,
                                       MPIdxCstrs),
        (
            foreach(Id:MPCstr, MPIdxCstrs),
            foreach(Id, MPIdxs),
            foreach(MPCstr, MPCstrs)
        do
            true
        ),
        % collect any constraints constraints which involve generated vars
        collect_typed_pool_constraints(Pool, mp_sp of cg_constraint_type,
                                       MPSPIdxCstrs),
        (
            foreach([CoeffVar, Id]:MPSPCstr, MPSPIdxCstrs),
            foreach(CoeffVar, CoeffVars),
            foreach(Id, MPSPIdxs),
            foreach(MPSPCstr, MPSPCstrs)
        do
            true
        ).

cg_minimize_body(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                 SolveSubProblem, Obj, OptionList, ObjVal, Pool, Module) :-
        % setup original MP problem
        cg_solver_setup_body(Handle, MPCstrs, MPIdxs,
                             MPSPCstrs, MPSPIdxs, CoeffVars,
                             SolveSubProblem, Obj, OptionList,
                             Pool, Module),
        % solve the initial MP
        cg_iteration(Handle),
        Handle = cg_prob{master_prob:MP,
                         upper_bound:ObjVal},
        lp_get(MP, vars, AllVarArr),
        (
            foreacharg(Var, AllVarArr),
            param(MP, Handle)
        do
            ( nonvar(Var) ->
                  true
            ;
                  lp_var_get(MP, Var, solution, Val),
                  get_cg_attr(Var, Handle, Attr),
                  setarg(mp_val of colgen, Attr, Val)
            )
        ).

% bp_solve/2: for low-level handles
bp_solve(Handle, Obj) :-
        ( var(Handle) ->
            error(4, bp_solve(Handle, Obj))
        ; Handle = cg_prob{} ->
            bp_solve_body(Handle, Obj)
        ;
            error(5, bp_solve(Handle, Obj))
        ).

% solve/1: for pools
bp_solve1(Obj, Pool) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Colgen instance has no solver set up in %w%n",
                   [Pool:solve(Obj)]),
            abort
        ;
            bp_solve_body(Handle, Obj)
        ).

bp_solve_body(Handle, Obj) :-
        Handle = cg_prob{bfs_tree:BfsHandle0,info_messages:OnOff},
        ( BfsHandle0 == [] ->
            bfs_solver_setup(BfsHandle, min, bp_node(Handle),
                             [info_messages(OnOff),
                              separation(bp_separate(Handle))]),
            setarg(bfs_tree of cg_prob, Handle, BfsHandle)
        ;
            BfsHandle = BfsHandle0
        ),
        bfs_solve(BfsHandle, Obj),
        Handle = cg_prob{mp_vars:Vars},
        (
            foreach(Var, Vars),
            param(Handle, BfsHandle)
        do
            ( var(Var) ->
                bfs_var_get(BfsHandle, Var, optimal_val, Val),
                ( var(Val) -> Val = 0 ; true ),
                get_cg_attr(Var, Handle, Attr),
                setarg(mp_val of colgen, Attr, Val)
            ;
                true
            )
        ).

bp_node(Handle) :-
        add_new_solver_rowcols(Handle),
        Handle = cg_prob{master_prob:MP,
                         bfs_tree:BfsHandle,
                         idx_lookup:Lookup,
                         shelf:Shelf},
        \+ \+ ( bfs_impose_node_state(other, BfsHandle),
                ( cg_get(Handle, unsatisfiable_cstrs, []) -> D = -1 ; D = 0 ),
                setarg(phase of cg_prob, Handle, D),
                cg_get(Handle, vars, OldVars),
                cg_iteration(Handle),
                cg_get(Handle, non_zero_vars, Vars),
                (
                    foreach(Var, OldVars),
                    foreach(Info, OldVals),
                    param(Handle, MP)
                do
                    cg_var_get(Handle, Var, mp_val, Val),
                    % note that if the optimisation has been stopped
                    % because we have "close enough" bounds there may
                    % be columns that have been added to the MP since
                    % the last lp_solve and any attempt to get a
                    % reduced cost will fail
                    ( lp_var_get(MP, Var, reduced_cost, RC) ->
                        true
                    ;
                        RC = 0
                    ),
                    get_var_bounds(Var, Lo, Hi),
                    Info = cg_var_info{lo: Lo,
                                       hi: Hi,
                                       val: Val,
                                       reduced_cost: RC}
                ),
                (
                    foreach(Var, Vars),
                    fromto(NewVals, Out, In, []),
                    param(OldVars, Handle, MP)
                do
                    % if it was added to the cg_prob before
                    % this call to cg_iteration/1 its
                    % solution info has been found above,
                    % otherwise it must have been generated
                    % in this call, so we have to save its
                    % colgen attr as well as the solution info
                    % to recreate the var after the \+ \+
                    ( var_member(Var, OldVars) ->
                          Out = In
                    ;
                          cg_var_get(Handle, Var, mp_val, Val),
                          % note that if the optimisation has been stopped
                          % because we have "close enough" bounds there may
                          % be columns that have been added to the MP since
                          % the last lp_solve and any attempt to get a
                          % reduced cost will fail
                          ( lp_var_get(MP, Var, reduced_cost, RC) ->
                              true
                          ;
                              RC = 0
                          ),
                          get_var_bounds(Var, Lo, Hi),
                          get_cg_attr(Var, Handle, Attr),
                          Attr = colgen{mp_val:Val,
                                        type:Type,
                                        cost:Cost,
                                        coeffs:Coeffs,
                                        aux:Aux},
                          Attr1 = colgen{cost:Cost,
                                         coeffs:Coeffs,
                                         aux:Aux},
                          Info = cg_var_info{lo: Lo,
                                             hi: Hi,
                                             val: Val,
                                             reduced_cost: RC,
                                             type: Type,
                                             attr: Attr1},
                          Out = [Info|In]
                    )
                ),
                Handle = cg_prob{upper_bound:NodeCost},
                shelf_set(Shelf, info of cg_shelf, [NodeCost, OldVals, NewVals])
        ),
        shelf_get(Shelf, info of cg_shelf, [NodeCost, OldVals, NewVals]),
        shelf_set(Shelf, info of cg_shelf, []),
        cg_get(Handle, vars, OldVars),
        (
            foreach(Var, OldVars),
            foreach(Info, OldVals),
            param(BfsHandle)
        do
            ( var(Var) ->
                Info = cg_var_info{lo: Lo,
                                   hi: Hi,
                                   val: Val,
                                   reduced_cost: RC},
                bfs_node_info(BfsHandle, Var, Lo, Hi, Val, RC)
            ;
                true
            )
        ),
        (
            foreach(Info, NewVals),
            foreach(Var:ObjCol, VarCols),
            fromto(NewVars, [Var|Vars], Vars, OldVars),
            param(BfsHandle, Lookup, Handle)
        do
            Info = cg_var_info{lo: Lo,
                               hi: Hi,
                               val: Val,
                               reduced_cost: RC,
                               type: Type,
                               attr: Attr},
            get_cg_attr(Var, Handle, Attr),
            Attr = colgen{cost:Obj, coeffs:Coeffs},
            ( Obj =:= 0 -> ObjCol = BCol ; ObjCol = [obj:Obj|BCol] ),
            BCol = [lo:Lo, hi:Hi|Col],
            (
                foreach(Ident-V, Coeffs),
                foreach(Id:V, Col),
                param(Lookup)
            do
                hash_get(Lookup, Ident, Id)
            ),
            ( Type == integer ->
                bfs_integers(BfsHandle, Var), cg_integers(Handle, Var)
            ;
                true
            ),
            bfs_node_info(BfsHandle, Var, Lo, Hi, Val, RC)
        ),
        setarg(mp_vars of cg_prob, Handle, NewVars),
        lp_add_columns(MP, VarCols),
        lp_get(MP, vars, VarArr),
        arity(VarArr, ColsAdded),
        setarg(mp_cols_added of cg_prob, Handle, ColsAdded),
        bfs_node_cost(BfsHandle, NodeCost).


var_member(Var, [H|T]) :-
        ( Var == H -> true ; var_member(Var, T) ).

% cg_solver_setup/8,9: for low-level handles
:- tool(cg_solver_setup/8, cg_solver_setup0/9).

cg_solver_setup0(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                 SubProblemSolver, Obj, Module) :-
        cg_solver_setup(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                        SubProblemSolver, Obj, [], Module).

:- tool(cg_solver_setup/9, cg_solver_setup/10).

cg_solver_setup(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                SubProblemSolver, Obj, OptionList, Module) :-
        ( var(Handle) ->
            error(4, cg_solver_setup(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                                     SubProblemSolver, Obj, OptionList))
        ; Handle = cg_prob{} ->
            cg_solver_setup_body(Handle, MPCstrs, MPIdxs,
                                 MPSPCstrs, MPSPIdxs, CoeffVars,
                                 SubProblemSolver, Obj, OptionList,
                                 _, Module)
        ;
            error(5, cg_solver_setup(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                                     SubProblemSolver, Obj, OptionList))
        ).

% solver_setup/2,3: for pools
:- tool(cg_solver_setup/3, cg_solver_setup0/4).

cg_solver_setup0(SubProblemSolver, Obj, Pool, Module) :-
        cg_solver_setup(SubProblemSolver, Obj, [], Pool, Module).

:- tool(cg_solver_setup/4, cg_solver_setup/5).

cg_solver_setup(SubProblemSolver, Obj, OptionList, Pool, Module) :-
        get_pool_item(Pool, Handle), 
        ( Handle == 0 ->
            printf(error, "Colgen instance has no solver in %w%n",
                   [Pool:solver_setup(SubProblemSolver, Obj, OptionList)]),
            abort
        ;
            cg_pool_collect_mp_sp_cstrs(Pool, MPCstrs, MPIdxs,
                                        MPSPCstrs, MPSPIdxs, CoeffVars),
            cg_solver_setup_body(Handle, MPCstrs, MPIdxs,
                                 MPSPCstrs, MPSPIdxs, CoeffVars,
                                 SubProblemSolver, Obj, OptionList,
                                 Pool, Module)
        ).

%:- mode cg_solver_setup_body(-, +, ?, +, ?, ?, +, ?, +, ?, ++).
cg_solver_setup_body(Handle, MPCstrs, MPIdxs, MPSPCstrs, MPSPIdxs, CoeffVars,
                     SolveSubProblem, Obj, OptionList, Pool, Module) :-
        linearize(Obj, [ConstTerm|LinObj], NonLinObj),
        ( NonLinObj == [] ->
              % the variables to be generated do not appear in the
              % objective, set OptVar in the SPs to 0
              NormObj = LinObj,
              OptVar = 0
        ;
              % the variables to be generated do appear in the
              % objective, but give them a 0 dual val for now
              % in case we need to use two phase
              NonLinObj = [AuxVar = implicit_sum(OptVar)],
              filter_auxvar(AuxVar, LinObj, NormObj, ObjCoeff),
              var_dual(OptVar, 0.0, ObjCoeff, obj, _, 0.0, Handle)
        ),
        % process option list and fill in defaults
        process_options(OptionList, Handle, Module, SepGoal, EplexOptions),
        fill_in_defaults(Handle),
        arg(1, SolveSubProblem, SPHandle),
        fill_in_defaults_sp(Pool, SPHandle, Module),
        Handle = cg_prob{master_prob:MPHandle,
			 module:Module,
                         const_obj:ConstTerm,
                         sp_solution_call:SolveSubProblem,
                         phase1_obj:Phase1Obj,
                         upper_bound:1.0Inf,
                         lower_bound: -1.0Inf,
                         mp_vars:Vars,
                         stab_terms:StabTerms,
                         stab_iter_counts:StabIterCounts,
                         sep_call:(call(SepGoal)@Module),
                         mp_cols_added:ColsAdded,
                         pool:Pool,
                         idx_lookup:Lookup,
                         shelf:Shelf,
                         phase:0},
        StabIterCounts = stab_counters{bound_counter:1,
                                       coeff_counter:1},
        hash_create(Lookup),
        verify(nonvar(Shelf)),
        % create the eplex handle and setup the fixed part of the obj
        ( NormObj == [] ->
              % need a dummy var in the obj fn to force
              % a CPLEX handle to be created properly
              % (or a dummy constraint)
              get_cg_attr(Dummy, Handle, DummyAttr),
              setarg(cost of colgen, DummyAttr, 0),
              setarg(coeffs of colgen, DummyAttr, []),
              setarg(aux of colgen, DummyAttr, artificial),
              MPObj=[ConstTerm, 1*Dummy]
        ;
              MPObj=[ConstTerm|NormObj]
        ),
        lp_setup([], min(sum(MPObj)),
                 EplexOptions,
                 MPHandle),
        lp_set(MPHandle, dual_solution, yes),
        lp_set(MPHandle, keep_basis, yes),
        lp_set(MPHandle, reduced_cost, yes),
        lp_set(MPHandle, slack, yes),
        % add any constraints only involving known MP vars:
        % if basis stabilisation is being used we will need to add
        % plus/minus stabilisation variables to each constraint,
        % we give them some default bounds of 0.0..1.0e+3 and
        % objective coefficient 0 initially
        % actually we always add them even if it is not used, to
        % make it easier to re-solve with it enabled if necessary
        %
        % note: the initial upper bound of 1.0e+3 is a bit arbitrary.
        % Adding these variables to the problem gives us a relaxation
        % of the problem. If the upper bounds are inifinite the
        % relaxation is always feasible, while fixing them to zero
        % gives the relaxation identical to the original problem.
        % Their intended purpose is to avoid the wildly oscillating
        % optimal dual values often encountered in column generation
        % we want is to solve
        (
            foreach(MPCstr, MPCstrs),
            foreach(MPCstr1, MPNormCstrs),
            fromto(StabTerms, [StabTerm|Rest],
                   Rest, StabTerms0),
            foreach(MPIdx, MPIdxs),
            param(Handle)
        do
            MPCstr = Type:[ConstTerm|LinTerms],
            get_cg_attr(Yminus, Handle, AttrM),
            setarg(cost of colgen, AttrM, 0),
            setarg(coeffs of colgen, AttrM, [Idx - -1]),
            setarg(aux of colgen, AttrM, stabilisation),
            get_cg_attr(Yplus, Handle, AttrP),
            setarg(cost of colgen, AttrP, 0),
            setarg(coeffs of colgen, AttrP, [Idx - 1]),
            setarg(aux of colgen, AttrP, stabilisation),
            MPCstr1 = Type:[ConstTerm, -1*Yminus, 1*Yplus|LinTerms],
            StabTerm = stab_term{idx: MPIdx,
                                 plus_var: Yplus,
                                 plus_coeff: 0,
                                 plus_bound: 1.0e+3,
                                 minus_var: Yminus,
                                 minus_coeff: 0,
                                 minus_bound: 1.0e+3}
        ),
        lp_add_indexed(MPHandle, MPNormCstrs, [], MPIdxs),
        % now add the constraints which involve generated vars and
        % setup the dual_var attributes of the subproblem vars with
        % index of MP constraint
        % note: we add in artificial variables in case the first
        % restricted MP at any node is infeasible
        % again we add plus/minus stabilisation variables to each
        % constraint and give them some default bounds of 0.0..1.0e+3
        % and objective coefficient 0 initially
        (
            foreach(MPSPCstr, MPSPCstrs),
            foreach(StabTerm, StabTerms0),
            foreach(MPSPCstr1, MPSPNormCstrs),
            fromto(ArtVars, AVOut, AVIn, []),
            foreach(Idx, MPSPIdxs),
            param(Handle)
        do
            StabTerm = stab_term{idx:Idx,
                                 plus_var: Yplus,
                                 plus_coeff: 0,
                                 plus_bound: 1.0e+3,
                                 minus_var: Yminus,
                                 minus_coeff: 0,
                                 minus_bound: 1.0e+3},
            MPSPCstr = Type:[ConstTerm|LinTerms],
            ( Type == (=<) ->
                  get_cg_attr(Art, Handle, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - -1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Handle, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Handle, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  MPSPCstr1 = Type:[ConstTerm, -1*Art,
                                    -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art|AVIn]
            ; Type == (>=) ->
                  get_cg_attr(Art, Handle, Attr),
                  setarg(cost of colgen, Attr, 0),
                  setarg(coeffs of colgen, Attr, [Idx - 1]),
                  setarg(aux of colgen, Attr, artificial),
                  get_cg_attr(Yminus, Handle, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Handle, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  MPSPCstr1 = Type:[ConstTerm, 1*Art,
                                    -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art|AVIn]
            ; Type == (=:=) ->
                  get_cg_attr(Art1, Handle, Attr1),
                  setarg(cost of colgen, Attr1, 0),
                  setarg(coeffs of colgen, Attr1, [Idx - -1]),
                  setarg(aux of colgen, Attr1, artificial),
                  get_cg_attr(Art2, Handle, Attr2),
                  setarg(cost of colgen, Attr2, 0),
                  setarg(coeffs of colgen, Attr2, [Idx - 1]),
                  setarg(aux of colgen, Attr2, artificial),
                  get_cg_attr(Yminus, Handle, AttrM),
                  setarg(cost of colgen, AttrM, 0),
                  setarg(coeffs of colgen, AttrM, [Idx - -1]),
                  setarg(aux of colgen, AttrM, stabilisation),
                  get_cg_attr(Yplus, Handle, AttrP),
                  setarg(cost of colgen, AttrP, 0),
                  setarg(coeffs of colgen, AttrP, [Idx - 1]),
                  setarg(aux of colgen, AttrP, stabilisation),
                  MPSPCstr1 = Type:[ConstTerm, -1*Art1, 1*Art2,
                                    -1*Yminus, 1*Yplus|LinTerms],
                  AVOut = [Art1, Art2|AVIn]
            )
        ),
        Phase1Obj = min(sum(ArtVars)),
        lp_add_indexed(MPHandle, MPSPNormCstrs, [], MPSPIdxs),
        expand_sp_obj_terms(MPIdxs, MPSPIdxs, CoeffVars, Handle),
        % give the known MP vars a colgen attribute
        % and set their objective cost
        % this has to be done after both types of constraints
        % above are added in case there are known MP vars
        % appearing in the type 2 constraints that do not
        % appear in the type 1 constraints
        % however, now the artificial variables have been added
        % to the MP so we have to check if each var has an attribute
        % already and only add attribute/include in vars of cg_prob
        % if it does not
        lp_get(MPHandle, vars, VarArr),
        (
            foreacharg(Var, VarArr),
            fromto(Vars, Out, In, []),
            param(MPHandle, Handle)
        do
            get_cg_attr(Var, Handle, Attr),
            Attr = colgen{aux:Aux, lo:Lo, hi:Hi},
            % set initial mp eplex bounds
            lp_var_set_bounds(MPHandle, Var, Lo, Hi),
            ( Aux == artificial ->
                % artificial variable
                Out = In
            ; Aux == stabilisation ->
                % stabilisation variable
                Out = In
            ;
                % known MP variable
                Attr = colgen{cost:0, coeffs:[], aux:[]},
                Out = [Var|In]
            )
        ),
        (
            foreach(Const*Var, NormObj),
            param(Handle)
        do
            get_cg_attr(Var, Handle, Attr),
            setarg(cost of colgen, Attr, Const)
        ),
        % finally add initial SP solution column set to MP
        cg_new_MP_columns(Handle, VarCols),
        ( VarCols == [] ->
              % no initial solution columns
              true
        ;
              % add initial solution columns to MP
              lp_add_columns(MPHandle, VarCols),
              % record the total number of columns now in the mp
              lp_get(MPHandle, vars, MPVarArr),
              arity(MPVarArr, ColsAdded)
        ),
        (
            foreach(Var, ArtVars)
        do
            set_var_bounds(Var, 0, 1.0Inf)
        ),
        (
            foreach(StabTerm, StabTerms),
            param(MPHandle)
        do
            StabTerm = stab_term{plus_var: Yplus,
                                 plus_bound: Boundplus,
                                 minus_var: Yminus,
                                 minus_bound: Boundminus},
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus),
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus)
        ),
        ( NormObj == [] -> Dummy = 0 ; true ),
        % make a suspension for the MP iterator
        % and insert it in the suspension list of Handle
        make_suspension(cg_iteration(Handle), 0, MPSusp),
        enter_suspension_list(mp_susp of cg_prob, Handle, MPSusp),
        % make a suspension for the SP iterator
        % and insert it in the suspension lists of the dual val vars
        make_suspension(solveSPs(Handle), 0, SPSusp),
        SPHandle = sp_prob{cost:OptVar, coeff_vars:DualVars},
        insert_suspension([OptVar|DualVars], SPSusp, susps of dual_var,
                          dual_var).

add_new_solver_rowcols(Handle) :-
        Handle = cg_prob{master_prob:MPHandle,
                         phase1_obj:Phase1Obj,
                         mp_vars:OldVars,
                         stab_terms:OldStabTerms,
                         pool:Pool},
        ( var(Pool) ->
            % there is no associated colgen pool
            MPIdxCstrs = [],
            MPSPNormCstrs = []
        ;
            % collect any new constraints from the pool
            collect_typed_pool_constraints(Pool, mp_only of cg_constraint_type,
                                           MPIdxCstrs),
            collect_typed_pool_constraints(Pool, mp_sp of cg_constraint_type,
                                           MPSPNormCstrs)
        ),
        ( ( MPIdxCstrs == [], MPSPNormCstrs == [] ) ->
            % there were no new constraints,
            % done
            true
        ;
            % there were some, add them to the MP solver
            get_pool_handle(Handle, Pool),
            % process any constraints only involving known MP vars
            (
                foreach(MPIdx:MPIdxCstr, MPIdxCstrs),
                fromto(NormExprs, [Expr|RestExprs], RestExprs, NormExprs0),
                foreach(MPIdxCstr1, MPIdxNormCstrs),
                fromto(StabTerms, [StabTerm|Rest],
                       Rest, StabTerms0),
                fromto(NewStabTerms, [StabTerm|Rest0],
                       Rest0, StabTerms00),
                foreach(MPIdx, MPIdxs),
                param(Handle)
            do
                % add a stabilisation term to the constraint before
                % sending to MPHandle
                MPIdxCstr = Type:Expr,
                MPIdxCstr1 = Type:Expr1,
                add_stabilisation_term(MPIdx, Handle, Expr, Expr1, StabTerm)
            ),
            % now process the constraints which involve generated vars and
            % setup the dual_var attributes of the subproblem vars with
            % index of MP constraint
            % note: we add in artificial variables in case the first
            % restricted MP at any node is infeasible
            Phase1Obj = min(sum(OldArtVars)),
            (
                foreach([CoeffVar, Idx]:NormCstr, MPSPNormCstrs),
                foreach(Expr, NormExprs0),
                fromto(StabTerms0, [StabTerm|Rest],
                       Rest, OldStabTerms),
                foreach(StabTerm, StabTerms00),
                foreach(CoeffVar, CoeffVars),
                foreach(Cstr, Cstrs),
                fromto(NewArtVars, NAVOut, NAVIn, []),
                fromto(ArtVars, AVOut, AVIn, OldArtVars),
                foreach(Idx, Idxs),
                param(Handle)
            do
                % add a stabilisation term and artificial vars to the
                % constraint before sending to MPHandle
                NormCstr = Type:Expr,
                add_stabilisation_term(Idx, Handle, Expr, [ConstTerm|LinTerms], StabTerm),
                ( Type == (=<) ->
                    get_cg_attr(Art, Handle, Attr),
                    setarg(cost of colgen, Attr, 0),
                    setarg(coeffs of colgen, Attr, [Idx - -1]),
                    setarg(aux of colgen, Attr, artificial),
                    Cstr = Type:[ConstTerm, -1*Art|LinTerms],
                    NAVOut = [Art|NAVIn],
                    AVOut = [Art|AVIn]
                ; Type == (>=) ->
                    get_cg_attr(Art, Handle, Attr),
                    setarg(cost of colgen, Attr, 0),
                    setarg(coeffs of colgen, Attr, [Idx - 1]),
                    setarg(aux of colgen, Attr, artificial),
                    Cstr = Type:[ConstTerm, 1*Art|LinTerms],
                    NAVOut = [Art|NAVIn],
                    AVOut = [Art|AVIn]
                ; Type == (=:=) ->
                    get_cg_attr(Art1, Handle, Attr1),
                    setarg(cost of colgen, Attr1, 0),
                    setarg(coeffs of colgen, Attr1, [Idx - -1]),
                    setarg(aux of colgen, Attr1, artificial),
                    get_cg_attr(Art2, Handle, Attr2),
                    setarg(cost of colgen, Attr2, 0),
                    setarg(coeffs of colgen, Attr2, [Idx - 1]),
                    setarg(aux of colgen, Attr2, artificial),
                    Cstr = Type:[ConstTerm, -1*Art1, 1*Art2|LinTerms],
                    NAVOut = [Art1, Art2|NAVIn],
                    AVOut = [Art1, Art2|AVIn]
                )
            ),
            NewPhase1Obj = min(sum(ArtVars)),
            setarg(phase1_obj of cg_prob, Handle, NewPhase1Obj),
            setarg(stab_terms of cg_prob, Handle, StabTerms),
            % give the new MP variables a colgen attribute
            % this has to be done for variables in both types of constraints
            % however, some variables may already have been added
            % to the MP so we have to check if each variable occurs
            % already and only add attribute/include in vars of cg_prob
            % if it does not
            term_variables(NormExprs, ExprVars),
            (
                foreach(ExprVar, ExprVars),
                fromto(Vars, Out, In, OldVars),
                param(MPHandle, Handle)
            do
                ( lp_var_occurrence(ExprVar, MPHandle, _) ->
                    % already added to MPHandle,
                    % it must be a pre-existing variable for Handle
                    Out = In
                ;
                    % not yet in MPHandle,
                    % it is a new variable for Handle
                    get_cg_attr(ExprVar, Handle, colgen{lo:Lo, hi:Hi}),
                    % set initial mp eplex bounds
                    lp_var_set_bounds(MPHandle, ExprVar, Lo, Hi),
                    Out = [ExprVar|In]
                )
            ),
            lp_add_indexed(MPHandle, MPIdxNormCstrs, [], MPIdxs),
            lp_add_indexed(MPHandle, Cstrs, [], Idxs),
            expand_sp_obj_terms(MPIdxs, Idxs, CoeffVars, Handle),
            % finally add any new SP solution columns to MP
            cg_new_MP_columns(Handle, VarCols),
            ( VarCols == [] ->
                % no initial solution columns
                true
            ;
                % add initial solution columns to MP
                lp_add_columns(MPHandle, VarCols),
                (
                    foreach(Var:_, VarCols),
                    fromto(NewVars, [Var|Rest], Rest, Vars)
                do
                    true
                ),
                % record the total number of columns now in the mp
                lp_get(MPHandle, vars, MPVarArr),
                arity(MPVarArr, NewColsAdded),
                setarg(mp_vars of cg_prob, Handle, NewVars),
                setarg(mp_cols_added of cg_prob, Handle, NewColsAdded)
            ),
            (
                foreach(Var, NewArtVars)
            do
                set_var_bounds(Var, 0, 1.0Inf)
            ),
            (
                foreach(StabTerm, NewStabTerms),
                param(MPHandle)
            do
                StabTerm = stab_term{plus_var: Yplus,
                                     plus_bound: Boundplus,
                                     minus_var: Yminus,
                                     minus_bound: Boundminus},
                lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus),
                lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus)
            ),
            % insert the SP iterator suspension in the suspension lists of
            % the dual val vars
            % TODO: this looks wrong - why a new suspension???
            % also: sp_prob's coeff_vars field hasn't been updated?
            make_suspension(solveSPs(Handle), 0, SPSusp),
            insert_suspension(CoeffVars, SPSusp, susps of dual_var,
                              dual_var)
        ).

add_stabilisation_term(Idx, Handle, Expr, StabExpr, StabTerm) :-
        Expr = [ConstTerm|LinTerms],
        % add stabilisation var terms to the linear expression
        StabExpr = [ConstTerm, -1*Yminus, 1*Yplus|LinTerms],
        % create a new stab_term structure for them
        StabTerm = stab_term{idx: Idx,
                             plus_var: Yplus,
                             plus_coeff: 0,
                             plus_bound: 1.0e+3,
                             minus_var: Yminus,
                             minus_coeff: 0,
                             minus_bound: 1.0e+3},
        % add colgen attributes to the new stabilisation vars
        get_cg_attr(Yminus, Handle, AttrM),
        setarg(cost of colgen, AttrM, 0),
        setarg(coeffs of colgen, AttrM, [Idx - -1]),
        setarg(aux of colgen, AttrM, stabilisation),
        get_cg_attr(Yplus, Handle, AttrP),
        setarg(cost of colgen, AttrP, 0),
        setarg(coeffs of colgen, AttrP, [Idx - 1]),
        setarg(aux of colgen, AttrP, stabilisation).

expand_sp_obj_terms([], [], [], _Handle) :- !.
expand_sp_obj_terms(MPIdxs, Idxs, CoeffVars, Handle) :-
        ( MPIdxs == [] ->
            NCstrs is max(Idxs)
        ; Idxs == [] ->
            NCstrs is max(MPIdxs)
        ;
            NCstrs is max(max(MPIdxs), max(Idxs))
        ),
        NCstrs1 is NCstrs + 1,
        dim(ExprArr, [NCstrs1]),
        Handle = cg_prob{sp_obj_terms:OldExprArr},
        ( var(OldExprArr) ->
            OldExprArr = ExprArr
        ;
            dim(OldExprArr, [OldNCstrs]),
            NCstrs1 > OldNCstrs,
            (
                for(Idx, 1, OldNCstrs),
                param(OldExprArr, ExprArr)
            do
                arg(Idx, OldExprArr, Arg),
                arg(Idx, ExprArr, Arg)
            ),
            setarg(sp_obj_terms of cg_prob, Handle, ExprArr)
        ),
        (
            foreach(CoeffVar, CoeffVars),
            foreach(Idx, Idxs),
            param(ExprArr)
        do
            Idx1 is Idx + 1,
            arg(Idx1, ExprArr, CoeffVar)
        ),
        (
            foreach(MPIdx, MPIdxs),
            param(ExprArr)
        do
            MPIdx1 is MPIdx + 1,
            arg(MPIdx1, ExprArr, 0)
        ).

process_options([], _, _, Separation, []) :- !,
        ( var(Separation) -> Separation = true ; true ).
process_options([O|Os], Handle, Module, Separation, EplexOptions) :- !,
	process_option(O, Handle, Module, Separation, EplexOptions, EplexOptions0),
	process_options(Os, Handle, Module, Separation, EplexOptions0).
process_options(_NonList, Handle, _, _, _) :-
        Handle = cg_prob{pool:Pool},
        cg_info_message(Handle, "%w : options not proper list."
                        " Ignored.%n,", [Pool]).

process_option(separate(Separation0), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Separation0) ->
            error(4, process_option(separate(Separation0), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ;
            Separation = Separation0,
            EplexOptions = EplexOptions0
        ).
process_option(node_select(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(node_select(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ;
            ( Handle = cg_prob{bfs_tree:[], info_messages:OnOff} ->
                bfs_solver_setup(BfsHandle, min, bp_node(Handle),
                                      [info_messages(OnOff),
                                       node_select(Val),
                                       separation(bp_separate(Handle))]),
                setarg(bfs_tree of cg_prob, Handle, BfsHandle)
            ;
                Handle = cg_prob{bfs_tree:BfsHandle},
                bfs_set(BfsHandle, node_select, Val)
            ),
            EplexOptions = EplexOptions0
        ).
process_option(int_tolerance(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(int_tolerance(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; float(Val) ->
            ( 0 =< Val, Val =< 0.5 ->
                cg_set(Handle, int_tolerance, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(int_tolerance(Val), Handle, Module, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; error(5, process_option(int_tolerance(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(disallow(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(disallow(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; atom(Val) ->
            ( valid_disallow_setting(Val) ->
                cg_set(Handle, disallow, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(disallow(Val), Handle, Module, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; error(5, process_option(disallow(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(info_messages(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(info_messages(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; atom(Val) ->
            ( valid_setting(Val) ->
                cg_set(Handle, info_messages, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(info_messages(Val), Handle, Module, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; error(5, process_option(info_messages(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(on_degeneracy(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(on_degeneracy(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; atom(Val) ->
            ( valid_degeneracy_setting(Val) ->
                cg_set(Handle, on_degeneracy, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(on_degeneracy(Val), Handle, Module, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; error(5, process_option(on_degeneracy(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(stabilisation(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(stabilisation(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; valid_stabilisation_setting(Val) ->
            cg_set(Handle, stabilisation, Val),
            EplexOptions = EplexOptions0
        ; error(6, process_option(stabilisation(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(basis_perturbation(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(basis_perturbation(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ; atom(Val) ->
            ( valid_setting(Val) ->
                cg_set(Handle, basis_perturbation, Val),
                EplexOptions = EplexOptions0
            ; error(6, process_option(basis_perturbation(Val), Handle, Module, Separation,
                                      EplexOptions, EplexOptions0))
            )
        ; error(5, process_option(basis_perturbation(Val), Handle, Module, Separation,
                                  EplexOptions, EplexOptions0))
        ).
process_option(eplex_option(Val), Handle, Module, Separation, EplexOptions, EplexOptions0) ?- !,
	( var(Val) ->
            error(4, process_option(eplex_option(Val), Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ;
            EplexOptions = [Val|EplexOptions0]
        ).
process_option(Option, Handle, Module, Separation, EplexOptions, EplexOptions0) :-
        ( var(Option) ->
            error(4, process_option(Option, Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ;
            error(6, process_option(Option, Handle, Module, Separation,
                                    EplexOptions, EplexOptions0))
        ).

valid_degeneracy_setting(continue).
valid_degeneracy_setting(stop).

valid_stabilisation_setting(on(BoundIter, BoundUpdate,
                               CoeffIter, CoeffUpdate)) ?-
        integer(BoundIter),
        BoundIter >= 1,
        number(BoundUpdate),
        BoundUpdate > 0,
        integer(CoeffIter),
        CoeffIter >= 1,
        number(CoeffUpdate),
        CoeffUpdate > 0.
valid_stabilisation_setting(off).
valid_stabilisation_setting(stab_pred(_, _)).

valid_disallow_setting(clp).
valid_disallow_setting(lp).
valid_disallow_setting(off).

valid_setting(on).
valid_setting(off).

fill_in_defaults(Handle) :-
        Handle = cg_prob{disallow:DisOnOff,
                         tolerance:Tol,
                         branch_tolerance:BranchTol,
                         info_messages:MessageOnOff,
                         on_degeneracy:OnDegeneracy,
                         stabilisation:Stabilisation,
                         basis_perturbation:PerturbOnOff},
        ( var(DisOnOff) -> DisOnOff = off ; true ),
        ( var(Tol) -> Tol = 1e-05 ; true ),
        ( var(BranchTol) -> BranchTol = 0 ; true ),
        ( var(MessageOnOff) -> MessageOnOff = off ; true ),
        ( var(OnDegeneracy) -> OnDegeneracy = stop ; true ),
        ( var(Stabilisation) -> Stabilisation = off ; true ),
        ( var(PerturbOnOff) -> PerturbOnOff = off ; true ).

fill_in_defaults_sp(Pool, SPHandle, Module) :-
        SPHandle = sp_prob{master_pool:Pool,
                           cutoff:CutOff,
                           disallow:Disallow,
                           status: phase1,
                           module:Module,
                           lo:Lo, hi:Hi, type:Type
               },
        ( var(CutOff) -> CutOff = 1e-05 ; true ),
        ( var(Disallow) -> Disallow = [0, []] ; true ),
        ( var(Lo) -> Lo = 0.0 ; true ),         % or -1.0Inf, but 0 more likely
        ( var(Hi) -> Hi = 1.0Inf ; true ),
        ( var(Type) -> Type = real ; true ).


% ----------------------------------------------------------------------
% most general instantiation templates to disallow duplicate columns
% ----------------------------------------------------------------------

% repeatedly amalgamate a new coefficient list with
% the coefficient list of a member of a list of
% linearisation-coefficient list pairs, extract
% linearisation of new most general amalgamation
% and return new linearisation-coefficient list pair list
lin_amalgamate(N, List, Coeffs, Vars, NewN, [[N1, Cstrs]-NewCoeffs|NewList]) :-
    % try to amalgamate Coeffs with others in list
    % repeatedly until we have most general disallow template
    (
	fromto(no, _, Done, yes),
	fromto(Coeffs, CIn, COut, NewCoeffs),
	fromto(N, NIn, NOut, N0),
	fromto(List, LIn, LOut, NewList)
    do
	lin_amalgamate(LIn, CIn, NIn, LOut, COut, NOut, Done)
    ),
    % linearise new template
    linearize_template(Vars, NewCoeffs, 1, N1, LCstrs, Flags, Done),
    ( Done == yes -> NewN = N0, N1 = 0, Cstrs = []
    ; NewN is N0+N1, Cstrs = [(>=):[-1*1|Flags]|LCstrs] ).

linearize_template([], [], N, N, [], [], no).
linearize_template([Var|Vars], [Coeff|Coeffs], N0, N, Cstrs, Flags, Done) :-
    get_var_bounds(Var, Lo, Hi),
    ( Coeff = [_|_] -> CList = Coeff ; CList = [Coeff] ),
    lin_not_in(CList, Var, Lo, Hi, N0, N1, Cstrs, Cstrs1, Flags, Flags1, Done),
    ( Done == yes ->         % Var cannot match, no Cstrs needed
	true
    ;
	linearize_template(Vars, Coeffs, N1, N, Cstrs1, Flags1, Done)
    ).

lin_not_in([], _Var, _VLo, _VHi, _N, _N1, _Cstrs, _Cstrs1, _Flags, _Flags1, yes).
lin_not_in([H|T], Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1, Done) :-
    ( integer(H) -> Lo = H, Hi = H ; H = Lo..Hi ),
    ( VHi < Lo ->            % cannot be in this or later ranges
	Done = yes
    ; VLo < Lo, VHi =< Hi -> % may be in this range, cannot be in later
	% Var =< VHi-(VHi+1-Lo)*Flag
	Alpha is fix(VHi+1-Lo),
	Beta is fix(-1*VHi),
	N1 is N+1,
	Cstrs = [(=<):[Beta*1, 1*Var, Alpha*Flag]|Cstrs1],
	Flags = [1*Flag|Flags1],
	Flag::0..1
    ; VHi =< Hi ->           % is in this range, no cstr for this var
	N = N1,
	Cstrs = Cstrs1,
	Flags = Flags1
    ; VLo =< VHi ->          % may be in this or later but cannot be earlier
	lin_not_in1(T, Hi, Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1)
    ;                        % may be in later ranges but cannot be in this
	lin_not_in(T, Var, VLo, VHi, N, N1, Cstrs, Cstrs1, Flags, Flags1, Done)
    ).

lin_not_in1([], Hi, Var, VLo, _VHi, N, N1,
	    [(>=):[Delta*1,1*Var,Gamma*Flag]|Cstrs1], Cstrs1,
	    [1*Flag|Flags1], Flags1) :-
    % Var >= VLo+(Hi+1-VLo)*Flag
    N1 is N+1,
    Flag::0..1,
    Gamma is fix(VLo-1-Hi),
    Delta is fix(-1*VLo).
lin_not_in1([H|T], Hi, Var, VLo, VHi, N, N1,
	    [(>=):[Delta*1,1*Var,Gamma*Flag],
	     (=<):[Beta*1,1*Var,Alpha*Flag]|Cstrs], Cstrs1,
	    [1*Flag|Flags], Flags1) :-
    % Var >= VLo+(Hi+1-VLo)*Flag
    % Var =< VHi-(VHi+1-Lo1)*Flag
    ( integer(H) -> Lo1 = H, Hi1 = H ; H = Lo1..Hi1 ),
    N0 is N+2,
    Flag::0..1,
    Alpha is fix(VHi+1-Lo1),
    Beta is fix(-1*VHi),
    Gamma is fix(VLo-1-Hi),
    Delta is fix(-1*VLo),
    lin_not_in1(T, Hi1, Var, VLo, VHi, N0, N1, Cstrs, Cstrs1, Flags, Flags1).

% amalgamate a coefficient list with the coefficient
% list of a member of a list of linearisation-coefficient
% list pairs and return remaining list and amalgamated coefficients
lin_amalgamate([], Coeffs, N, [], Coeffs, N, yes).
lin_amalgamate([[L, Cstrs]-Coeffs|List], NewCoeffs, N,
	       NewList, NewCoeffs1, NewN, Done) :-
    ( amalg(NewCoeffs, Coeffs, 0, 0, NewCoeffs1) ->
	% new coeff list can be amalgamated with this one
	% to create a more general disallow template
	% return rest of list and new template
	NewList = List,
	NewN is N-L,
	Done = no
    ;
	% could not be amalgamated
	NewList = [[L, Cstrs]-Coeffs|NewList1],
	lin_amalgamate(List, NewCoeffs, N, NewList1, NewCoeffs1, NewN, Done)
    ).

% repeatedly amlagamate a new coefficient list with
% the coefficient list of a member of a list of
% suspension-coefficient list pairs killing any
% associated suspensions, suspend disallow demons for
% the most general amalgamation and return new
% suspension-coefficient list pair list
% The coefficients are numbers or ../2 ranges.
amalgamate(N, List, Coeffs, Vars, NewN, [[N1, Susps]-NewCoeffs|NewList]) :-
    % try to amalgamate Coeffs with others in list
    % repeatedly until we have most general disallow template
    (
	fromto(no, _, Done, yes),
	fromto(Coeffs, CIn, COut, NewCoeffs),
	fromto(N, NIn, NOut, N0),
	fromto(List, LIn, LOut, NewList)
    do
	amalgamate(LIn, CIn, NIn, LOut, COut, NOut, Done)
    ),
    % suspend disallow demons for new template
    (
	foreach(Var, Vars),
	foreach(Coeff, NewCoeffs),
	foreach(Flag, AndFlags),
	foreach(Susp, AndSusps),
	count(_,2,NSusps),
	param(OrFlag)
    do
	( Coeff = [_|_] -> ListTerm = l(Coeff) ; ListTerm = l([Coeff]) ),
	suspend(not_among(Var, ListTerm, Flag, OrFlag, Susp), 0, [Var->constrained, [Flag, OrFlag]->inst], Susp),
	not_among(Var, ListTerm, Flag, OrFlag, Susp)
    ),
    ( AndFlags = [AndFlag] ->
	AndFlag = 1,
	NewN is N0+1,
	N1 = 1,
	Susps = AndSusps
    ;
	NewN is N0+NSusps,
	N1 = NSusps,
	Susps = [OrSusp|AndSusps],
	AndFlagTerm = l(AndFlags),
	suspend(or(OrFlag, AndFlagTerm, OrSusp), 0, [OrFlag|AndFlags]->inst, OrSusp),
	or(OrFlag, AndFlagTerm, OrSusp)
    ).

% amalgamate a coefficient list with the coefficient list of
% a member of a list of suspension-coefficient list pairs
% killing any associated suspensions and return remaining
% list and amalgamated coefficients
amalgamate([], Coeffs, N, [], Coeffs, N, yes).
amalgamate([[L, Susps]-Coeffs|List], NewCoeffs, N,
	   NewList, NewCoeffs1, NewN, Done) :-
    ( amalg(NewCoeffs, Coeffs, 0, 0, NewCoeffs1) ->
	% new coeff list can be amalgamated with this one
	% to create a more general disallow template
	% kill the suspension for this disallow goal
	% return rest of list and new template
	(
	    foreach(Susp, Susps)
	do
	    kill_suspension(Susp)
	),
	NewList = List,
	NewN is N-L,
	Done = no
    ;
	% could not be amalgamated
	NewList = [[L, Susps]-Coeffs|NewList1],
	amalgamate(List, NewCoeffs, N, NewList1, NewCoeffs1, NewN, Done)
    ).

% amalgamate two coefficient lists to create a _most general amalgamation_
% succeed if: they are pairwise disjoint at exactly one coeff and
% pairwise indentical at all others
% abort if: they are equal/intersecting at all coeffs (since we have a
% duplicate coeff list which should have been disallowed)
% fail: otherwise
amalg([], [], D, I, []) :-
    ( D = 0 -> printf("lists not disjoint in amalg/5%n", []),
               flush(output), abort
    ; D = 1, I = 0 ).
amalg([A|As], [B|Bs], D, I, Amalg) :-
    ( A == B ->
	% pairwise identical here
	D1 = D, I1 = I, Amalg = [A|Amalg1]
    ;
	% otherwise make both coeff templates into
	% disallowed domain lists
	% and check for disjointness/intersection
	( A = [_|_] -> List1 = A ; List1 = [A] ),
	( B = [_|_] -> List2 = B ; List2 = [B] ),
	amalg(List1, List2, D, D1, I, I1, List3), Amalg = [List3|Amalg1]
    ),
    amalg(As, Bs, D1, I1, Amalg1).

amalg(List1, List2, D, D1, I, I1, Amalg) :-
    amalg1(List1, List2, D, D1, I, I1, List3),
    ( List3 = [Val] -> Amalg = Val
    ; Amalg = List3 ).

amalg1([], List, 0, 1, 0, 0, List) :- !.
amalg1(List, [], 0, 1, 0, 0, List) :- !.
amalg1([H1|T1], [H2|T2], D, D1, I, I1, Amalg) :-
    ( integer(H1) -> Lo1 = H1, Hi1 = H1 ; H1 = Lo1..Hi1 ),
    ( integer(H2) -> Lo2 = H2, Hi2 = H2 ; H2 = Lo2..Hi2 ),
    ( overlapping_ranges(Lo1, Hi1, Lo2, Hi2) ->
	% lists intersect here,
	% fail immediately if already disjoint
	% otherwise succeed with
	% no need to continue amalgamating
	D = 0, D1 = 0, I1 = 1
    ;
	% no intersection between H1 and H2,
	% continue with T1 and T2,
	% amalgamating H1 and H2 if necessary
	( Lo1 is Hi2 + 1 -> T3 = [Lo2..Hi1|T1], T4 = T2, Amalg = Amalg1
	; Lo1 > Hi2 + 1 -> T3 = [H1|T1], T4 = T2, Amalg = [H2|Amalg1]
	; Lo2 is Hi1 + 1 -> T3 = T1, T4 = [Lo1..Hi2|T2], Amalg = Amalg1
	; T3 = T1, T4 = [H2|T2], Amalg = [H1|Amalg1] ),
	amalg1(T3, T4, D, D1, I, I1, Amalg1)
    ).

overlapping_ranges(Lo1, Hi1, Lo2, Hi2) :-
    (Lo1 >= Lo2, Lo1 =< Hi2) ; (Lo2 >= Lo1, Lo2 =< Hi1).

% suspend this on [OrFlag|AndFlags]->inst
:- demon or/3.
:- set_flag(or/3, priority, 3).
or(OrFlag, AndFlagTerm, Susp) :-
    ( OrFlag == 1 ->
	kill_suspension(Susp)
    ;
	% an AndFlag was set to 0
	AndFlagTerm =.. [_, AndFlags],
	(
	    foreach(AndFlag, AndFlags),
	    fromto(NewFlags, Out, In, [])
	do
	    ( AndFlag == 0 -> Out = In ; Out = [AndFlag|In] )
	),
	( NewFlags = [AndFlag] -> AndFlag = 1, kill_suspension(Susp)
	; setarg(1, AndFlagTerm, NewFlags) )
    ).

% suspend this on Var->any, [Flag, OrFlag]->inst
:- demon not_among/5.
:- set_flag(not_among/5, priority, 4).
not_among(Var, ListTerm, Flag, OrFlag, Susp) :-
    ( OrFlag == 1 ->
	% some other variable has been
	% instantiated to a non-matching
	% value, done
	kill_suspension(Susp)
    ;
	% change in domain of Var
	% or all other variables have been
	% instantiated to matching values,
	% Var cannot match ListTerm
	get_var_bounds(Var, Lo, Hi),
	ListTerm =.. [_, List],
	not_among_body(List, Lo, Hi, NewList, Flag),
	( Flag == 0 ->
	    % matches the list, done
	    kill_suspension(Susp)
	; NewList == [] ->
	    % does not match, done
	    OrFlag = 1, Flag = 1, kill_suspension(Susp)
	; Flag == 1 ->
	    % all other variables have been
	    % instantiated to matching values,
	    % Var cannot match ListTerm

	    % is there a way to remove intervals from ic var domains?

	    % for now

	    % remove the top and bottom overlaps
	    % if it can be done without creating holes
	    NewList = [H|Rest],
	    ( integer(H) -> Lo1 = H, Hi1 = H ; H = Lo1..Hi1 ),
	    ( Lo1 =< Lo -> NewLo is Hi1 + 1 ; NewLo = Lo ),
	    ( Rest == [] -> NewHi = Hi
	    ;
		( fromto(Rest, [_|Out], Out, [T]) do true ),
		( integer(T) -> Lo2 = T, Hi2 = T ; T = Lo2..Hi2 ),
		( Hi2 >= Hi -> NewHi is Lo2 - 1 ; NewHi = Hi )
	    ),
	    set_var_bounds(Var, NewLo, NewHi),
	    setarg(1, ListTerm, NewList)
	; setarg(1, ListTerm, NewList) )
    ).

% not_among_body(+RangeList, +VLo, +VHi, ?List, ?Flag)
% succeeds if VLo..VHi is a subset of a member of RangeList and Flag = 0
% or if List is the intersection of VLo..VHi and RangeList
not_among_body([], _VLo, _VHi, [], _Flag).
not_among_body([H|T], VLo, VHi, List, Flag) :-
    ( integer(H) -> Lo = H, Hi = H ; H = Lo..Hi ),
    ( VHi < Lo ->            % cannot be in this or later ranges
	List = []
    ; VLo < Lo, VHi =< Hi -> % may be in this range, cannot be in later
	List = [H]
    ; VHi =< Hi ->           % is in this range, set flag false
	Flag = 0
    ;                        % may be in this and/or later ranges
	( VLo =< Hi -> List = [H|List1] ; List = List1 ),
	not_among_body(T, VLo, VHi, List1, Flag)
    ).

:- mode cg_new_MP_columns(+,-).
cg_new_MP_columns(Handle, VarCols) :-
        Handle = cg_prob{sp_solution_call:SolveSubProblem,
                         disallow:DisOnOff,
                         mp_vars:OldVars,
                         idx_lookup:Lookup,
                         new_columns:NewColRec},
        recorded_list(NewColRec, Solns),
        erase_all(NewColRec),
        arg(1, SolveSubProblem, SPHandle),
        SPHandle = sp_prob{coeff_vars:DualVars,
                           disallow:[Count, Templates]},
        (
            foreach(Soln, Solns),
            fromto(VarCols, [Var:ObjCol|Rest], Rest, []),
            fromto(NewVars, [Var|Vars], Vars, OldVars),

            % most general inst templates for disallowing cols
            % for thesis results tables
            fromto(Count, CountIn, CountOut, Count0),
            fromto(Templates, TemplatesIn, TemplatesOut, NewTemplates),
            param(DisOnOff),
            
            param(Handle, DualVars, Lookup)
        do
            Soln = sp_sol{cost:Obj,
                          coeff_vars:Coeffs,
                          aux:Info,
                          lo:Lo,
                          hi:Hi,
                          type:Type},
            ( Obj =:= 0 -> ObjCol = BCol ; ObjCol = [obj:Obj|BCol] ),
            BCol = [lo:Lo, hi:Hi|Col],
            new_cg_attrstruct(Handle, Obj, HashCol, Lo, Hi, Type, Info, Attr),
            add_attribute(Var, Attr, colgen),
            ( Coeffs = [_Id-_V|_] ->
                  % Coeffs is a sparse coefficient list (unordered)

                  % most general inst templates for disallowing cols
                  % for thesis results tables
                  ( DisOnOff == off ->
                        CountOut = CountIn, TemplatesOut = TemplatesIn
                  ;
                        % reconstruct full coefficient list from sparse one
                        (
                            foreach(Var, DualVars),     %+
                            foreach(TVal, TCoeffs),     %-
                            param(Handle, Coeffs)
                        do
                            get_idx(Var, Id, Handle),
                            ( member(Id-TVal, Coeffs) -> true
                            ; TVal = 0 )
                        ),
                        ( DisOnOff == lp ->
                              lin_amalgamate(CountIn, TemplatesIn,
                                             TCoeffs, DualVars,
                                             CountOut, TemplatesOut)
                        ; DisOnOff == clp ->
                              amalgamate(CountIn, TemplatesIn,
                                         TCoeffs, DualVars,
                                         CountOut, TemplatesOut)
                        )
                  ),

                  % make a sparse column representation Col1
                  (
                      foreach(Id-V, Coeffs),    %+
                      foreach(I:V, Col1),       %-
                      param(Lookup)
                  do
                      hash_get(Lookup, Id, I)
                  ),
                  keysort(Coeffs, HashCol)

            ;
                  % Coeffs is a full list of coefficients (not sparse)

                  % most general inst templates for disallowing cols
                  % for thesis results tables
                  ( DisOnOff == off ->
                        CountOut = CountIn, TemplatesOut = TemplatesIn
                  ;
                        ( DisOnOff == lp ->
                              lin_amalgamate(CountIn, TemplatesIn,
                                             Coeffs, DualVars,
                                             CountOut, TemplatesOut)
                        ; DisOnOff == clp ->
                              amalgamate(CountIn, TemplatesIn,
                                         Coeffs, DualVars,
                                         CountOut, TemplatesOut)
                        )
                  ),

                  % make a sparse column representation Col1
                  (
                      foreach(Var, DualVars),
                      foreach(V, Coeffs),
                      fromto(HC, HCOut, HCIn, []),
                      fromto(Col1, Out, In, []),
                      param(Handle, Lookup)
                  do
                      (V =:= 0 ->
                           HCOut = HCIn,
                           Out = In
                      ;
                           get_idx(Var, Id, Handle),
                           HCOut = [Id-V|HCIn],
                           hash_get(Lookup, Id, I),
                           Out = [I:V|In]
                      )
                  ),
                  keysort(HC, HashCol)
            ),
            keysort(Col1, Col)
        ),

        % most general inst templates for disallowing cols
        % for thesis results tables
        NewCount is max(Count, Count0),
        setarg(disallow of sp_prob,SPHandle, [NewCount, NewTemplates]),
                                 
        setarg(mp_vars of cg_prob, Handle, NewVars).


:- demon cg_iteration/1.
:- set_flag(cg_iteration/1, priority, 7).
:- set_flag(cg_iteration/1, run_priority, 7).
%cg_iteration(+Handle)
%
% perform one column generation iteration:
% a) add any new constraints/variables and solve master problem,
% b) retrieve dual values and solve subproblems,
% c) retrieve beneficial columns and add to master problem
cg_iteration(Handle) :-
        % collect any new constraints or variables added since last
        % iteration
        %cg_info_message(Handle, "%tExtending restricted master problem ... ", []),
        add_new_solver_rowcols(Handle),
        % solve the new master problem
        cg_info_message(Handle, "%tSolving restricted master problem ... ", []),
        ( in_phase_1(Handle) ->
            solve_masterproblem_phase1(Handle, Status, ObjVal)
        ;
            solve_masterproblem_phase2(Handle, Status, ObjVal)
        ),
        % solve the subproblems
        solve_subproblems(Status, Handle),
        % retrieve beneficial columns
        cg_new_MP_columns(Handle, VarCols),
        ( stabilisation_stopping_criteria(Handle, VarCols) ->
            % cannot improve solution by adding new columns,
            % if we are in phase 1 (phase == 0) and we have hit
            % the stopping conditions we do not have a feasible
            % solution yet so we fail, while if we are in phase 2
            % (phase == -1) and we have hit the stopping
            % conditions then we have a feasible solution that we
            % cannot improve upon
            \+ in_phase_1(Handle)
        ;
            % update the master problem stabilisation terms
            update_stabilisation(Handle, VarCols),
            % update the degeneracy status
            update_degeneracy_status(Handle),
            % update the master problem lp
            update_masterproblem_lp(Handle, VarCols),
            % update the master problem lower bound
            update_lower_bound(Handle, ObjVal)
        ).

in_phase_1(cg_prob{phase:0}).

%solve_masterproblem_phase1(+Handle, -Status, -ObjVal)
%
% solve phase 1 of the two-phase method, i.e. with artifical cost
% function: we want to change to phase 2 iff the current
% problem has objective value 0. In order to avoid problems with
% rounding we do not solve and test solution value, but rather try to
% do the phase change and solve the phase 2 problem first. If that
% succeeds, then the phase 1 problem must have had objective value 0,
% so we return a phase2 Status with the value. If it fails, then the
% phase 1 problem must have had strictly positive objective value, so
% we try to solve the phase 1 problem. If that succeeds, we are still
% in phase 1 and return a phase 1 flag with the value. If both have
% failed then the phase 1 problem has become infeasible: since all
% constraints involving generated vars have an artificial variable
% they should always be satisfiable, thus any infeasibility is due to
% constraints involving no generated vars. If such a constraint is
% infeasible now then it will always be infeasible in future, so we
% fail. 
% NB: for safety we really should check which constraints were
% unsatisfiable and abort if they contain a constraint involving
% generated vars
solve_masterproblem_phase1(Handle, Status, ObjVal) ?-
        % try phase change and phase 2 solution now
        Handle = cg_prob{master_prob:MP,
                         phase1_obj:Phase1Fn,
                         stabilisation:Stabilisation,
                         stab_terms:StabTerms,
                         mp_vars:Vars},
        phase_change(Phase1Fn),
        lp_get(MP, objective, ObjExpr),
%	( lp_get(MP, cbasisarr, CB) -> true ; CB=[] ),
        solve_perturbed_objective_function(Stabilisation, StabTerms,
                                           MP, ObjExpr, ObjVal), !,
%	( lp_get(MP, cbasisarr, CB1) -> true ; CB1=[] ),
%	write_term(CB, [compact(true)]), nl,
%	write_term(CB1, [compact(true)]), nl,
        % phase change and phase 2 solution successful,
        % we have a feasible solution change the phase setting
        setarg(phase of cg_prob, Handle, -1),
        % any optimal solution of a phase 2 master problem is an upper
        % bound on the true optimal value, so update the upper bound ...
        setarg(upper_bound of cg_prob, Handle, ObjVal),
        % ... and set optimal vals
        set_optimal_mp_vals(Vars, Handle, MP),
        % no need to perturb the dual values since at the very least
        % the objective coefficient "dual" has changed from 0 to -1
        cg_info_message(Handle, "done, z_mp = %w%n", [ObjVal]),
        Status = phase2.
solve_masterproblem_phase1(Handle, Status, ObjVal) ?-
        % phase change and phase 2 solution was unsuccessful,
        % try phase 1 solution
        Handle = cg_prob{master_prob:MP,
                         phase1_obj:Phase1Fn,
                         on_degeneracy:OnDegeneracy,
                         stabilisation:Stabilisation,
                         stab_terms:StabTerms},
        ( lp_get(MP, cbasis, StartCBasis) -> true ; StartCBasis = [] ),
        ( lp_get(MP, rbasis, StartRBasis) -> true ; StartRBasis = [] ),
%	( lp_get(MP, cbasisarr, CB) -> true ; CB=[] ),
        solve_perturbed_objective_function(Stabilisation, StabTerms,
                                           MP, Phase1Fn, ObjVal), !,
        % phase 1 solution successful, still restricted infeasible
        % if we have retained the same optimal basis try to perturb
        % the dual values so that the subproblem can generate
        % different columns
        perturb_if_necessary(Handle, Phase1Fn, StartCBasis, StartRBasis),
        cg_info_message(Handle, "infeasible%n", []),
%	( lp_get(MP, cbasisarr, CB1) -> true ; CB1=[] ),
%	write_term(CB, [compact(true)]), nl,
%	write_term(CB1, [compact(true)]), nl,
        ( lp_get(MP, cbasis, StartCBasis),
          lp_get(MP, rbasis, StartRBasis) ->
            % degenerate so Status is infeasible if not handled,
            % and degenerate if it is handled by the subproblem
            ( OnDegeneracy == stop -> Status = infeasible
            ; OnDegeneracy == continue -> Status = degenerate )
        ;
            % not degenerate so Status is phase1
            Status = phase1
        ).
solve_masterproblem_phase1(Handle, _Status, _ObjVal) ?-
        % phase 1 solution unsuccessful, fail
        cg_info_message(Handle, "failed: inconsistent constraints posted%n", []),
        fail.

%solve_masterproblem_phase2(+Handle, -Status, -ObjVal)
%
% solve phase 2 of the two-phase method, i.e. with true objective
% function and all artificial variables fixed to 0
solve_masterproblem_phase2(Handle, Status, ObjVal) ?-
        % ensure the artificial variables are ground just in case
        Handle = cg_prob{master_prob:MP,
                         phase1_obj:Phase1Fn,
                         on_degeneracy:OnDegeneracy,
                         stabilisation:Stabilisation,
                         stab_terms:StabTerms,
                         mp_vars:Vars,
                         tolerance:Tolerance,
                         lower_bound:LowerBound},
        ( lp_get(MP, cbasis, StartCBasis) -> true ; StartCBasis = [] ),
        ( lp_get(MP, rbasis, StartRBasis) -> true ; StartRBasis = [] ),
%	( lp_get(MP, cbasisarr, CB) -> true ; CB=[] ),
        % TODO: don't zero out when already done
        phase_change(Phase1Fn),
        % TODO: Don't retrieve and reconstruct potentially big objective
        % TODO: at least use a sum(List) instead of nested +/2
        lp_get(MP, objective, ObjExpr),
        solve_perturbed_objective_function(Stabilisation, StabTerms,
                                           MP, ObjExpr, ObjVal),
        % any optimal solution of a phase 2 master problem is an upper
        % bound on the true optimal value, so update the upper bound ...
        setarg(upper_bound of cg_prob, Handle, ObjVal),
        % ... and set optimal vals
        set_optimal_mp_vals(Vars, Handle, MP),
        % if we have retained the same optimal basis try to perturb
        % the dual values so that the subproblem can generate
        % different columns
        perturb_if_necessary(Handle, ObjExpr, StartCBasis, StartRBasis),
        cg_info_message(Handle, "done, z_mp = %w%n", [ObjVal]),
%	( lp_get(MP, cbasisarr, CB1) -> true ; CB1=[] ),
%	write_term(CB, [compact(true)]), nl,
%	write_term(CB1, [compact(true)]), nl,
        ( ObjVal - (Tolerance*ObjVal) =< LowerBound ->
            % within Tolerance of lower bound so status is Optimal
            Status = optimal
        ; lp_get(MP, cbasis, StartCBasis),
          lp_get(MP, rbasis, StartRBasis) ->
            % degenerate so Status is suboptimal if not handled,
            % and degenerate if it is handled by the subproblem
            ( OnDegeneracy == stop -> Status = suboptimal
            ; OnDegeneracy == continue -> Status = degenerate )
        ;
            % not degenerate so Status is phase2
            Status = phase2
        ).

phase_change(min(sum(ArtVars))) :-
        % zero out the artificial variables
        (
            foreach(0, ArtVars)
        do
            true
        ).

solve_perturbed_objective_function(off, StabTerms,
                                   MP, ObjExpr, ObjVal) ?-
        % no stabilisation policy, zero out the stabilisation vars ...
        %TODO: don't do that repeatedly
        (
            foreach(StabTerm, StabTerms)
        do
            StabTerm = stab_term{plus_var:0.0,minus_var:0.0}
        ),
        % ... and solve for the existing objective function
        lp_probe(MP, ObjExpr, ObjVal).
%        lp_get(MP, constraints, _),
%        lp_probe(MP, ObjExpr, ObjVal),
%	lp_get(MP, typed_solution, Sol),
%	writeln(mp_sol:Sol).
solve_perturbed_objective_function(on(_,_,_,_), StabTerms,
                                   MP, ObjExpr, ObjVal) ?-
        solve_perturbed_objective_function_(StabTerms, MP, ObjExpr, ObjVal).
solve_perturbed_objective_function(stab_pred(_,_), StabTerms,
                                   MP, ObjExpr, ObjVal) ?-
        solve_perturbed_objective_function_(StabTerms, MP, ObjExpr, ObjVal).

solve_perturbed_objective_function_(StabTerms, MP, ObjExpr, ObjVal) :-
        % some form of stabilisation policy (either default or
        % user-defined) is in use, perturb the objective function ...
        (
            foreach(StabTerm, StabTerms),
            fromto(SExpr, Out, In, Expr)
        do
            StabTerm = stab_term{plus_var: Yplus,
                                 plus_coeff: CoeffPlus,
                                 minus_var: Yminus,
                                 minus_coeff: CoeffMinus},
            Out = CoeffPlus*Yplus - CoeffMinus*Yminus + In
        ),
        ObjExpr =.. [Sense, Expr],
        StabExpr =.. [Sense, SExpr],
        % ... and solve for the perturbed objective function
        lp_probe(MP, StabExpr, ObjVal).

set_optimal_mp_vals(Vars, Handle, MP) :-
        ( 
            foreach(Var, Vars),
            param(Handle, MP)
        do
            ( nonvar(Var) -> true
            ; lp_var_get(MP, Var, solution, Sol),
              get_cg_attr(Var, Handle, Attr),
              setarg(mp_val of colgen, Attr, Sol)
            )
        ).

%solve_subproblems(++Status, +Handle)
%
% Status in {phase1,phase2,optimal,suboptimal,infeasible,degenerate}
%
% solve subproblems dealing with degeneracy appropriately for the
% current phase and parameters
solve_subproblems(infeasible, Handle) ?- !,
        % apparently degenerate and infeasible (in phase 1),
        % no degeneracy handling -
        % fail
        cg_info_message(Handle, "%t... detected identical"
                        " external solver basis after mp"
                        " optimisation%n%t    terminating with"
                        " no solution where one may exist%n", []),
        fail.
solve_subproblems(suboptimal, Handle) ?- !,
        % apparently degenerate and feasible (in phase 2),
        % no degeneracy handling -
        % terminate column generation
        cg_info_message(Handle, "%t... detected identical"
                        " external solver basis after mp"
                        " optimisation%n%t    terminating with"
                        " potentially suboptimal solution%n", []).
solve_subproblems(optimal, _Handle) ?- !.
        % within tolerance of optimal, no need to solve subproblems
solve_subproblems(Status, Handle) :-
        % either degenerate where the user-defined subproblem solution
        % predicate will try to deal with degeneracy, or not
        % degenerate -
        % flag status, get dual values and update dual_var attributes,
        % triggers SP solution
        cg_info_message(Handle, "%tSolving subproblems ... ", []),
        Handle = cg_prob{master_prob:MP,
                         sp_solution_call:SolveSubProblem,
                         idx_lookup:Lookup,
                         shelf:Shelf,
                         new_columns:NewColRec,
                         phase:OptDual},
        lp_get(MP, dual_solution, Duals),
        DualArr =.. [[]|Duals],
        setarg(duals of cg_prob, Handle, DualArr),
        arg(1, SolveSubProblem, SPHandle),
        setarg(status of sp_prob, SPHandle, Status),
        shelf_set(Shelf, optimal_rc of cg_shelf, none),
        verify(recorded_list(NewColRec,[])),
        call_priority(update_duals(Handle, SPHandle, OptDual, Lookup, DualArr),
                      2),
	% Subproblem solver woken here!!!
        cg_info_message(Handle, "done%n", []).


stabilisation_stopping_criteria(Handle, VarCols) :-
        Handle = cg_prob{duals:Duals,
                         module:Module,
                         master_prob: MPHandle,
                         sp_solution_call: SolveSubProblem,
                         stabilisation: Stabilisation,
                         stab_terms: StabTerms,
                         tolerance: Tolerance,
                         phase:OptDual},
        ( Stabilisation == off ->
            VarCols = []
        ; Stabilisation = stab_pred(_, Goal) ->
            call(Goal)@Module
        ; Stabilisation = on(_BoundIter, _BoundUpdate,
                             _CoeffIter, _CoeffUpdate) ->
            (
                foreach(_Var:ObjCol, VarCols),
                param(Duals, OptDual, Tolerance)
            do
                % check all cols have wrong reduced cost
                ( ObjCol = [obj:Obj|Col] -> true
                ; Obj = 0, ObjCol = Col ),
                Cost is OptDual*Obj,
                (
                    foreach(I:V, Col),
                    fromto(Cost, In, Out, RC),
                    param(Duals)
                do
                    I1 is I + 1,
                    arg(I1, Duals, Dual),
                    Out is In + Dual*V
                ),
                RC =< -Tolerance
            ),
            (
                foreach(StabTerm, StabTerms),
                param(MPHandle, Tolerance)
            do
                StabTerm = stab_term{plus_var: Yplus,
                                     plus_bound: Boundplus,
                                     minus_var: Yminus,
                                     minus_bound: Boundminus},
                lp_var_get(MPHandle, Yplus, solution, YplusVal),
                abs(YplusVal) =< Tolerance,
                Boundplus =< Tolerance,
                lp_var_get(MPHandle, Yminus, solution, YminusVal),
                abs(YminusVal) =< Tolerance,
                Boundminus =< Tolerance
            )
        ).
           
update_stabilisation(Handle, VarCols) :-
        Handle = cg_prob{duals:Duals,
                         module:Module,
                         sp_solution_call: SolveSubProblem,
                         master_prob: MPHandle,
                         stabilisation: Stabilisation,
                         stab_terms: StabTerms,
                         stab_iter_counts: StabCounts,
                         tolerance: Tolerance,
                         phase:OptDual},
        ( Stabilisation == off ->
            true
        ; Stabilisation = stab_pred(Goal, _) ->
            call(Goal)@Module
        ; Stabilisation = on(BoundIter, BoundUpdate,
                             CoeffIter, CoeffUpdate) ->
            StabCounts = stab_counters{bound_counter:BoundCount,
                                       coeff_counter:CoeffCount},
            ( BoundCount < BoundIter ->
                BoundCount1 is BoundCount + 1,
                setarg(bound_counter of stab_counters, StabCounts,
                       BoundCount1)
            ;
                update_stab_bounds(StabTerms, MPHandle, BoundUpdate),
                setarg(bound_counter of stab_counters, StabCounts,
                       1)
            ),
            ( CoeffCount < CoeffIter ->
                CoeffCount1 is CoeffCount + 1,
                setarg(coeff_counter of stab_counters, StabCounts,
                       CoeffCount1)
            ; update_stab_coeffs(StabTerms, VarCols, OptDual,
                                 Duals, Tolerance, CoeffUpdate) ->
                setarg(coeff_counter of stab_counters, StabCounts,
                       1)
            ;
                true
            )
         ).

update_stab_bounds(StabTerms, MPHandle, BoundUpdate) :-
        (
            foreach(StabTerm, StabTerms),
            param(MPHandle, BoundUpdate)
        do
            StabTerm = stab_term{plus_var: Yplus,
                                 plus_bound: Boundplus,
                                 minus_var: Yminus,
                                 minus_bound: Boundminus},
            Boundplus1 is max(0, Boundplus - BoundUpdate),
            lp_var_non_monotonic_set_bounds(MPHandle, Yplus, 0, Boundplus1),
            setarg(plus_bound of stab_term, StabTerm, Boundplus1),
            Boundminus1 is max(0, Boundminus - BoundUpdate),
            lp_var_non_monotonic_set_bounds(MPHandle, Yminus, 0, Boundminus1),
            setarg(minus_bound of stab_term, StabTerm, Boundminus1)
        ).

update_stab_coeffs(StabTerms, VarCols, OptDual,
                   Duals, Tolerance,
                   CoeffUpdate) :-
        (
            foreach(_Var:ObjCol, VarCols),
            param(OptDual, Duals, Tolerance)
        do
            % check all cols have wrong reduced cost
            ( ObjCol = [obj:Obj|Col] -> true
            ; Obj = 0, ObjCol = Col ),
            Cost is OptDual*Obj,
            (
                foreach(I:V, Col),
                fromto(Cost, In, Out, RC),
                param(Duals)
            do
                I1 is I + 1,
                arg(I1, Duals, Dual),
                Out is In + Dual*V
            ),
            RC < Tolerance
        ),
        (
            foreach(StabTerm, StabTerms),
            param(Duals, CoeffUpdate)
        do
            StabTerm = stab_term{idx:Idx},
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            Coeffplus is Dual + CoeffUpdate,
            setarg(plus_coeff of stab_term, StabTerm, Coeffplus),
            Coeffminus is Dual - CoeffUpdate,
            setarg(minus_coeff of stab_term, StabTerm, Coeffminus)
        ).

update_degeneracy_status(cg_prob{sp_solution_call:SolveSubProblem, phase:CD}) :-
        ( CD == 0 -> Status = phase1 ; Status = phase2 ),
        arg(1, SolveSubProblem, SPHandle),
        setarg(status of sp_prob, SPHandle, Status).

update_masterproblem_lp(Handle, VarCols) :-
        Handle = cg_prob{master_prob:MP},
        % add new columns to MP
        lp_add_columns(MP, VarCols),
        lp_get(MP, vars, MPVarArr),
        arity(MPVarArr, NewColsAdded),              
        %%% WHY not reuse eplex's array here
        MPVarArr =.. [_|NewMPVars],
        setarg(mp_vars of cg_prob, Handle, NewMPVars),
        setarg(mp_cols_added of cg_prob, Handle, NewColsAdded).

update_lower_bound(Handle, ObjVal) :-
        % calculate the Lasdon bound
        ( lasdon_bound(Handle, ObjVal, LasdonBound) ->
            Handle = cg_prob{tolerance:Tolerance, upper_bound:UpperBound},
            ( LasdonBound >= UpperBound - (Tolerance * UpperBound) ->
                % cannot improve solution, done
                true
            ;
                % schedule the next MP iteration
                schedule_suspensions(mp_susp of cg_prob, Handle),
                wake
            )
        ;
            % no bound available
            %
            % (note: if the subproblem solver is guaranteed to return
            %  an optimal column we could just find the best reduced
            %  cost as in update_stab_coeffs/6 and use that to
            %  calculate the bound, but we allow solvers that return
            %  potentially suboptimal columns; an improvement would be
            %  to allow solvers to post an "optimality" flag along
            %  with the column so that on any given iteration we could
            %  update the bound iff the flag has been encountered for a
            %  column; for multiple subproblems we would need to take
            %  the maxium reduced cost of the "optimal" flagged
            %  columns)
            %
            % schedule the next MP iteration 
            schedule_suspensions(mp_susp of cg_prob, Handle),
            wake
        ).

lasdon_bound(Handle, ObjVal, LasdonBound) :-
        Handle = cg_prob{lower_bound:LowerBound,shelf:Shelf},
        \+ in_phase_1(Handle),
        shelf_get(Shelf, optimal_rc of cg_shelf, RCSum),
        number(RCSum),
        RCSum < 1.0Inf,
        LasdonBound is max(ObjVal - RCSum, LowerBound),
        setarg(lower_bound of cg_prob, Handle, LasdonBound),
        cg_info_message(Handle, "New lower bound: %w%n", [LasdonBound]).

%perturb_if_necessary(+Handle, +ObjExpr, ++CBasis, ++RBasis)
%
% iff the new basis retrieved from the LP is the same as the [CR]Basis
% args, and basis_perturbation is on, attempt to perturb the lp
% solution, so that we avoid identical dual values and repeated
% columns returned from the subproblems, which would result in a loop.
perturb_if_necessary(cg_prob{basis_perturbation:off}, _ObjExpr, _CBasis, _RBasis) :- !.
perturb_if_necessary(Handle, ObjExpr, CBasis, RBasis) :- !,
        Handle = cg_prob{master_prob:MPHandle},
        ( lp_get(MPHandle, cbasis, CBasis),
          lp_get(MPHandle, rbasis, RBasis) ->
            lp_get(optimizer, Optimizer),
            % the actual parameters to change are optimizer specific
            optimizer_specific_perturbation(Optimizer, ObjExpr, MPHandle),
            lp_get(MPHandle, cbasis, NewCBasis),
            lp_get(MPHandle, rbasis, NewRBasis),
            ( CBasis = NewCBasis,
              RBasis = NewRBasis -> writeln(perturbation-failed) ; true )
        ;
            true
        ).

optimizer_specific_perturbation(cplex, ObjExpr, MPHandle) ?- !,
        lp_get(optimizer_param(perind), Ind),
        lp_get(optimizer_param(perlim), Lim),
        lp_set(optimizer_param(perind), 1),
        lp_set(optimizer_param(perlim), 1),
        lp_probe(MPHandle, ObjExpr, _),
        lp_set(optimizer_param(perind), Ind),
        lp_set(optimizer_param(perlim), Lim).
optimizer_specific_perturbation(xpress, ObjExpr, MPHandle) ?- !,
        lp_get(optimizer_param(perturb), Val),
        lp_set(optimizer_param(perturb), 1.0),
        lp_probe(MPHandle, ObjExpr, _),
        lp_set(optimizer_param(perturb), Val).
%TODO: parameters for COIN
optimizer_specific_perturbation(Solver, _ObjExpr, _MPHandle) :-
	printf(error, "Colgen: %w not supported%n", [Solver]),
	abort.
	

update_duals(Handle, SPHandle, OptDual, Lookup, Duals) :-
        SPHandle = sp_prob{cost:OptVar,
                           coeff_vars:DualVars,
                           cutoff:Cutoff},
        ( nonvar(OptVar) ->
              Cutoff1 is Cutoff + OptDual*OptVar,
              setarg(cutoff of sp_prob, SPHandle, Cutoff1)
        ;
              always_set_dual(OptVar, OptDual, Handle)
        ),
        (
            foreach(DualVar, DualVars),
            param(Handle, Lookup, Duals)
        do
            get_idx(DualVar, Ident, Handle),
            hash_get(Lookup, Ident, Idx),
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            always_set_dual(DualVar, Dual, Handle)
        ).
/*
update_duals(SPHandle, Mu, Nu, OptDual, Duals, ObjVal, ZInc) :-
        SPHandle = sp_prob{cost:OptVar,
                           coeff_vars:DualVars,
                           gub_coeff_vars:Branches},
        set_dual(OptVar, OptDual),
        
        ( get_idx(Mu, MuIdx) ->
              % calculate Vanderbeck-Wolsey bound
              % Delta1 = Z - sum(PiB) - sum(MuK) - sum(NuL),
              % Delta2 = ZInc - sum(PiB) - sum(MuK) - sum(NuL),
              % Rho1 = max(Delta1/K0, Delta1/L0),
              % Rho2 = max(Delta2/K0, Delta2/L0),
              % Rho = min(Rho1, Rho2),
              % SPOpt > Mu0 + Nu0 - Rho
              % approximated by
              % SPOpt >= Mu0 + Nu0 - Rho + 1e-05
              (
                  foreach(DualVar, DualVars),
                  fromto(0, In, Out, PiB),
                  param(Duals)
              do
                  get_idx(DualVar, Idx),
                  get_rhs(DualVar, Rhs),
                  Idx1 is Idx + 1,
                  arg(Idx1, Duals, Dual),
                  Out is In + Dual*Rhs,            
                  set_dual(DualVar, Dual)
              ),
              (
                  foreach(Branch, Branches),
                  
                  % foreach(cg_gub{dual_var:GUBDualVar}, Branches),
                  fromto(PiB, In, Out, ConstTerm),
                  param(Duals)
              do
                  
                  Branch = cg_gub{type:Type,
                                  dual_var:GUBDualVar},
                  
                  ( atom(Type) ->
                  
                  get_rhs(GUBDualVar, Rhs),

                  ( (Rhs = 0, Type \= (>=)) ->
                        Out = In
                  ;
                  
                  get_idx(GUBDualVar, Idx),
                  Idx1 is Idx + 1,
                  arg(Idx1, Duals, Dual),
                  Out is In + Dual*Rhs,
                  set_dual(GUBDualVar, Dual)
                  
                  )
              
                  ;
                  (
                      foreach(T, Type),
                      foreach(GUBDV, GUBDualVar),
                      fromto(In, I, O, Out),
                      param(Duals)
                  do
                      get_rhs(GUBDV, Rhs),
                      ( (Rhs = 0, T \= (>=)) ->
                            O = I,
                            % the constraint was not explicitly
                            % added to the lp because it just
                            % fixes vars to 0,
                            % no constraint, no dual val,
                            % no contribution to ConstTerm
                            % but we MUST still give it a big
                            % negative dual val for the sps
                            set_dual(GUBDV, -1000000000)
                      ;
                            get_idx(GUBDV, Idx),
                            ( var(Idx) ->
                                  % the constraint was not
                                  % added to the lp because
                                  % it contained no vars yet
                                  % is it safe to set Dual = 0?
                                  Dual = 0
                            ;
                                  
                            Idx1 is Idx + 1,
                            arg(Idx1, Duals, Dual),
                            
                            true
                            ),
                            
                            O is I + Dual*Rhs,
                            set_dual(GUBDV, Dual)
                      )
                  )
                  )
              
              ),
              % get_idx(Mu, MuIdx),
              get_rhs(Mu, K),
              MuIdx1 is MuIdx + 1,
              arg(MuIdx1, Duals, Mu0),
              set_dual(Mu, Mu0),
              get_idx(Nu, NuIdx),
              get_rhs(Nu, L),
              NuIdx1 is NuIdx + 1,
              arg(NuIdx1, Duals, Nu0),
              set_dual(Nu, Nu0),
              Delta1 is ObjVal - ConstTerm,
              Rho1 is max(Delta1/K, Delta1/L),
              Delta2 is ZInc - ConstTerm,
              Rho2 is max(Delta2/K, Delta2/L),
              ( Rho2 =< Rho1 ->
                    % should prune node if no SP solutions
                    Rho = Rho2
              ;
                    % should terminate CG in node if no SP solutiuons
                    Rho = Rho1
              ),
              % but since
              % SPOpt = max{(pi-e)X - fW + sum(muZ) + sum(nuZ) + mu0 + nu0}
              % we can just leave Mu0, Nu0 out?
              % CGCutoff is Mu0 + Nu0 - Rho + 1e-05,
              CGCutoff is 1e-05 - Rho,
              setarg(cutoff of sp_prob, SPHandle, CGCutoff)        
        ;
              
        (
            foreach(DualVar, DualVars),
            param(Duals)
        do
            get_idx(DualVar, Idx),
            Idx1 is Idx + 1,
            arg(Idx1, Duals, Dual),
            set_dual(DualVar, Dual)
        )
        
        ).
*/

bp_separate(Handle) :-
        Handle = cg_prob{bfs_tree:BfsHandle,
                         sep_call:Goal,
                         shelf:Shelf},
        Goal = call(SepGoal)@Module,
        ( SepGoal == true ->
            true
        ;
            \+ \+ ( bfs_impose_node_state(other, BfsHandle),
                    call(Goal)
                  ),
            shelf_get(Shelf, info of cg_shelf, Branches),
            shelf_set(Shelf, info of cg_shelf, []),
            (
                foreach(Score:Branch, Branches),
                param(BfsHandle, Module)
            do
                bfs_branch(BfsHandle, [Score, call(Branch)@Module])
            )
        ).


fractional_vars(Vars, FracVars, Vals, Diffs, Fracs, L, U, Pool) :-
        Vars = [X|_],
        get_pool_handle(Handle, Pool),
        cg_var_get(Handle, X, mp_val, Sol),
        ( var(Sol) ->
              Handle = cg_prob{master_prob:MP},
              (
                  foreach(Var, Vars),
                  fromto(FracVars, VarsIn, VarsOut, []),
                  fromto(Vals, ValsIn, ValsOut, []),
                  fromto(Diffs, DiffsIn, DiffsOut, []),
                  fromto(Fracs, FracsIn, FracsOut, []),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U),
                  param(MP)
              do
                  ( nonvar(Var) ->
                        VarsIn = VarsOut,
                        ValsIn = ValsOut,
                        DiffsIn = DiffsOut,
                        FracsIn = FracsOut,
                        LIn = LOut,
                        UIn = UOut
                  ;
                        lp_var_get(MP, Var, solution, Val),
                        Diff is abs(round(Val) - Val),
                        ( Diff =< 1e-5 ->
                              VarsIn = VarsOut,
                              ValsIn = ValsOut,
                              DiffsIn = DiffsOut,
                              FracsIn = FracsOut,
                              LIn = LOut,
                              UIn = UOut
                        ;
                              Frac is Val - floor(Val),
                              ( Frac < 0.5 ->
                                    LOut is max(Frac, LIn),
                                    UOut = UIn
                              ;
                                    Frac > 0.5 ->
                                        LOut = LIn,
                                        UOut is min(Frac, UIn)
                              ;
                                        LOut = 0.5,
                                        UOut = 0.5
                              ),
                              VarsIn = [Var|VarsOut],
                              ValsIn = [Val|ValsOut],
                              DiffsIn = [Diff|DiffsOut],
                              FracsIn = [Frac|FracsOut]
                        )
                  )

              )
        ;
              (
                  foreach(Var, Vars),
                  fromto(FracVars, VarsIn, VarsOut, []),
                  fromto(Vals, ValsIn, ValsOut, []),
                  fromto(Diffs, DiffsIn, DiffsOut, []),
                  fromto(Fracs, FracsIn, FracsOut, []),
                  fromto(0.0, LIn, LOut, L),
                  fromto(1.0, UIn, UOut, U),
                  param(Handle)
              do
                  ( nonvar(Var) ->
                        VarsIn = VarsOut,
                        ValsIn = ValsOut,
                        DiffsIn = DiffsOut,
                        FracsIn = FracsOut,
                        LIn = LOut,
                        UIn = UOut
                  ;
                        cg_var_get(Handle, Var, mp_val, Val),
                        Diff is abs(round(Val) - Val),
                        ( Diff =< 1e-5 ->
                              VarsIn = VarsOut,
                              ValsIn = ValsOut,
                              DiffsIn = DiffsOut,
                              FracsIn = FracsOut,
                              LIn = LOut,
                              UIn = UOut
                        ;
                              Frac is Val - floor(Val),
                              ( Frac < 0.5 ->
                                    LOut is max(Frac, LIn),
                                    UOut = UIn
                              ;
                                Frac > 0.5 ->
                                    LOut = LIn,
                                    UOut is min(Frac, UIn)
                              ;
                                    LOut = 0.5,
                                    UOut = 0.5
                              ),
                              VarsIn = [Var|VarsOut],
                              ValsIn = [Val|ValsOut],
                              DiffsIn = [Diff|DiffsOut],
                              FracsIn = [Frac|FracsOut]
                        )
                  )
              )
        ).

:- demon solveSPs/1.
:- set_flag(solveSPs/1, priority, 6).
:- set_flag(solveSPs/1, run_priority, 6).
solveSPs(Handle) :-
        (
            Handle = cg_prob{sp_solution_call:SolveSubProblem,
                                shelf:Shelf,module:Module},
            arg(1, SolveSubProblem, SPHandle),
            % note SolveSubProblem MUST post at least one positive
            % reduced cost solution to the MP pool with
            % cg_subproblem_solution if any exist for any subproblem
            % otherwise we will terminate early with a suboptimal solution
            shelf_set(Shelf, sp_sol_cnt of cg_shelf, 0),

            call(SolveSubProblem)@Module,

            % If the SP has not posted solution(s) itself, post the current one
            ( shelf_get(Shelf, sp_sol_cnt of cg_shelf, 0) ->
                SPHandle = sp_prob{cost:Cost, coeff_vars:Coeffs,
                                    lo:Lo, hi:Hi, type:Type},
                ( ground(Cost-Coeffs) ->
                    cg_valid_columns(Handle, sp_sol{
                            cost:Cost, coeff_vars:Coeffs, lo:Lo, hi:Hi, type:Type})
                ; 
                    writeln(warning_output, "Warning: subproblem solver "
                            "succeeded with nonground variables (ignored)")
                )
            ;
                shelf_set(Shelf, sp_sol_cnt of cg_shelf, 0)
            ),
            fail
        ;
            true
        ).


%-----------------------------------------------------------------------
% Pools
%-----------------------------------------------------------------------

:- local record(colgen_pools). % list of colgen pool names

create_colgen_pool(Pool) :-
	create_constraint_pool(Pool, property(arity) of cg_constraint_type,
                               [
                                var_dual/6 -> var_dual1/7,
                                get_dual/2 -> get_dual1/3,
                                get_coeff/2 -> get_coeff1/3,
                                get_idx/2 -> get_idx1/3,
                                get_rhs/2 -> get_rhs1/3,
                                always_set_dual/2 -> always_set_dual1/3,
                                set_dual/2 -> set_dual1/3,
                                solve/1 -> bp_solve1/2,
                                solver_setup/3 -> cg_solver_setup/4,
                                solver_setup/2 -> cg_solver_setup/3,
                                var_get/3 -> cg_var_get1/4,
                                get/2 -> cg_get1/3,
                                set/2 -> cg_set/3,
                                statistics/0 -> cg_statistics/1,
                                integers/1 -> cg_integers1/2,
                                identified_constraint/2 -> add_cg_pool_constraint/3,
                                (::)/2 -> cg_range/3,
                                (=:=)/2 -> cg_eq/3,
                                (>=)/2 -> cg_ge/3,
                                (=<)/2 -> cg_le/3,
                                ($::)/2 -> cg_range/3,
                                ($=)/2 -> cg_eq/3,
                                ($>=)/2 -> cg_ge/3,
                                ($=<)/2 -> cg_le/3,
                                branch/1 -> cg_branch1/2,
                                branch/2 -> cg_branch1/3,
                                cg_subproblem_count/1 -> cg_sp_count1/2,
                                cg_subproblem_solution/1 -> cg_valid_columns1/2,
                                valid_columns/1 -> cg_valid_columns1/2,
                                cg_subproblem_rc_sum/1 -> cg_sp_rc_sum/2,
                                optimal_rc/1 -> cg_optimal_rc1/2,
                                minimize/4 -> cg_minimize/5,
                                minimize/3 -> cg_minimize/4
                               ]).


colgen_instance(PoolName) :-
        ( is_constraint_pool(PoolName),
	  recorded(colgen_pools, PoolName) % is a colgen pool
	->
            % if pool exists, check if it is currently empty 
	    ( pool_is_empty(PoolName),
	      get_pool_item(PoolName, 0) % has no associated solver
	    ->
		true
	    ;
		printf(error, "Colgen instance still in use in %w%n", [colgen_instance(PoolName)]),
		abort
	    )
	;
%	    ( current_module(PoolName) ->
%		  error(6, colgen_instance(PoolName))
%	    ;
		  record(colgen_pools, PoolName),
		  create_colgen_pool(PoolName)
%	    )
	).

get_pool_handle(Handle, Pool) :-
	( get_pool_item(Pool, Handle), Handle = cg_prob{} ->
            true
        ;
            get_pool_item(Pool, 0),
            init_cg_prob(Pool, Handle),
            set_pool_item(Pool, Handle)
        ).
        
init_cg_prob(Pool, Handle) :-
        Handle = cg_prob{
                pool:Pool,
                bfs_tree:[],
                new_columns:NewColRec,
                shelf:Shelf },
        record_create(NewColRec),
        shelf_create(cg_shelf{
                info:[],
                optimal_rc:none,
                sp_sol_cnt:0
            }, Shelf),
        init_suspension_list(mp_susp of cg_prob, Handle).


% ----------------------------------------------------------------------
% Expression simplifier (using lib(linearize))
%
% A linear expression is normalised into a list (sum) of the form
%	[C0*1, C1*X1, C2*X2, ...]
% where Ci are numbers and Xi are distinct variables.
% The first (constant) term is always present, Ci (i>=1) are nonzero.
% The expression must be built from variables, numbers, +/2, */2, -/2, -/1.
% The simplifier fails if the expression is not linear.
%
% renormalise/2 renormalises a normal form expression after variable
% bindings, unifications.
%
% A normalised constraint has the form
%	Sense:NormExpr
% where Sense is one of the atoms =:=, >= or =< and NormExpr is a
% normalised expression as above. E.g. (>=):[-5*1,3*X] encodes
% the constraint  -5 + 3*X >= 0.
% ----------------------------------------------------------------------

cg_normalise_cstr(E1 =:= E2, Norm, CoeffVar, Coeff) :- !,
        cg_normalise_cstr(E1 $= E2, Norm, CoeffVar, Coeff).
cg_normalise_cstr(E1 >= E2, Norm, CoeffVar, Coeff) :- !,
        cg_normalise_cstr(E1 $>= E2, Norm, CoeffVar, Coeff).
cg_normalise_cstr(E1 =< E2, Norm, CoeffVar, Coeff) :- !,
        cg_normalise_cstr(E1 $=< E2, Norm, CoeffVar, Coeff).
cg_normalise_cstr(E1 $= E2, (=:=):Norm, CoeffVar, Coeff) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0,
              Coeff = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm, Coeff)
        ).
cg_normalise_cstr(E1 $>= E2, (>=):Norm, CoeffVar, Coeff) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0,
              Coeff = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm, Coeff)
        ).
cg_normalise_cstr(E1 $=< E2, (=<):Norm, CoeffVar, Coeff) :- !,
	linearize(E1-E2, Lin, NonLin),
        ( NonLin == [] ->
              Norm = Lin,
              CoeffVar = 0,
              Coeff = 0
        ;
              NonLin = [AuxVar = implicit_sum(CoeffVar)],
              filter_auxvar(AuxVar, Lin, Norm, Coeff)
        ).
cg_normalise_cstr(Cstr, _, _, _) :-
	writeln(error, "Error: Unknown constraint":Cstr),
	abort. 

    filter_auxvar(Var, [C*X|Terms], Norm, Coeff) :-
        ( Var == X ->
              Norm = Terms,
              Coeff = C
        ;
              Norm = [C*X|Norm0],
              filter_auxvar(Var, Terms, Norm0, Coeff)
        ).
