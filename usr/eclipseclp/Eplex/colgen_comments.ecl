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
% Contributor(s): Andrew Eremin, IC-Parc
% 
% END LICENSE BLOCK

:- comment(categories, ["Algorithms","Constraints"]).
:- comment(summary, "Column generation library").
:- comment(author, "Andrew Eremin").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2012/07/31 02:17:06 $").
:- comment(status, prototype).

:- comment(desc, html("\
<P>

   This library lets you use hybrid column generation. Partial linear
   constraints are posted to a solver and further variables added to
   them during search as they become profitable. The generated
   variables will have a column of coefficients in the constraints of
   the colgen instance associated with them corresponding to
   particular instantiations of the variables of a subproblem. The
   predicate to find profitable subproblem variable instantiations is
   supplied by the user. When a user-defined branching predicate is
   provided, the library can also be used for hybrid branch-and-price.
</P><P>
   The library uses the eplex library to solve LP master
   problems, from which dual values are used to create cost functions
   for the user-defined subproblem. Solution of master and subproblems
   will iterate until no further subproblem solutions are posted to
   the colgen instance.
</P> ")).

:- comment(minimize/3, [
template:  "ColgenInstance:minimize(+SolveSubProblem, +Obj, -ObjVal)",
    args:  ["SolveSubProblem": "Subproblem solution predicate",
            "Obj": "The objective function to minimize",
            "ObjVal": "The optimal solution cost"
           ],
    summary: "Minimizes the problem associated with the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Minimize the partial linear expression <TT>Obj</TT> for the problem
  associated with the colgen instance <TT>ColgenInstance</TT>, using
  the user-defined predicate <TT>SolveSubProblem</TT> to provide
  profitable variables during solution. The optimal solution cost is
  unified with <TT>ObjVal</TT>.
  </P><P>
  The first argument of the subproblem solution predicate must be a
  subproblem structure, as specified in solver_setup/3.
  </P>
  "),
  see_also:[solver_setup/3]
]).

:- comment(solver_setup/2, [
template:  "ColgenInstance:solver_setup(+SolveSubProblem, +Obj)",
    args:  ["SolveSubProblem": "Subproblem solution predicate",
            "Obj": "The objective function to minimize"
           ],
    summary: "Define subproblem and objective for ColgenInstance.",
    desc: html("\
    Equivalent to solver_setup/3 with default options.
    "),
    see_also:[colgen:solver_setup/3]
]).

:- comment(solver_setup/3, [
template:  "ColgenInstance:solver_setup(+SolveSubProblem, +Obj, +Options)",
    args:  ["SolveSubProblem": "Subproblem solution predicate",
            "Obj": "The objective function to minimize",
            "Options": "A list of options"
           ],
    summary: "Define subproblem, objective and options for ColgenInstance.",
    desc: html("\
  <P>
  Define the partial linear expression <TT>Obj</TT> as the objective to
  minimize for the problem associated with the colgen instance
  <TT>ColgenInstance</TT>.  It will typically contain implicit_sum terms.
  </P><P>
  Associate the user-defined predicate <TT>SolveSubProblem</TT> to provide
  profitable variables during the solution process.
  </P><P>
  The first argument of the subproblem solution predicate must be a
  subproblem structure:
 <PRE>
      sp_prob(master_pool, cutoff, cost, coeff_vars, aux, ...)
 </PRE>
  where and <TT>master_pool</TT> will be unified with the colgen
  instance <TT>ColgenInstance</TT> so that solutions can be posted to
  it from within the solution predicate, <TT>cutoff</TT> is a minimum
  acceptable value for the cost of subproblem solutions that will be
  updated before calling the predicate, <TT>cost</TT> is the variable
  occurring in the implicit sum term of <TT>obj</TT> (if any)
  representing the contribution of new subproblem solutions to the
  master problem solution cost, <TT>coeff_vars</TT> is a list of all
  subproblem variables occurring in the implicit sum terms of master
  problem constraints.
  </P>
  <P>
  The following options are accepted:
    <DL>
    <DT><TT>separate(+SeparationGoal)</TT><DD>
	a user-specified separation goal (XXX).
    <DT><TT>node_select(+Val)</TT><DD>
	node selection criterion passed to bfs instance
	<TT>(best_first|depth_first|best_estimate)</TT>.
    <DT><TT>eplex_option(+EplexOption)</TT><DD>
	Option to be passed to the associated eplex solver instance.
    <DT><TT>disallow(+Policy)</TT><DD>
	policy for active prevention of duplicate columns <TT>(off|lp|clp)</TT>.
    <DT><TT>int_tolerance(+Tol)</TT><DD>
	tolerance for optimality <TT>(1e-5|float)</TT>.
    <DT><TT>basis_perturbation(+OffOn)</TT><DD>
	should we try and perturb the external solver basis when we appear
	to be at optimal and external solver returns same basis after adding
	columns  ('off' - no, 'on' - temporarily set the external solver
	to always perturb)
    <DT><TT>info_messages(+OffOn)</TT><DD>
	print messages while solving.
    <DT><TT>on_degeneracy(+Action)</TT><DD>
	should we halt when we find degeneracy (default 'stop'), or
	continue and let the subproblem solver deal with it ('continue').
    <DT><TT>stabilisation(+Policy)</TT><DD>
	the policy to perform basis stabilisation:
	<DL>
	<DT><TT>off</TT><DD>
	    no stabilisation is performed.
	<DT><TT>on(BoundIter, BoundUpdate, CoeffIter, CoeffUpdate)</TT><DD>
	    then the default policy is used with var bounds/coefficients
	    updated by BoundUpdate/CoeffUpdate after BoundIter/CoeffIter
	    iterations respectively.
	<DT><TT>stab_pred(UpdatePred, StoppingPred)</TT><DD>
	    a user defined policy is employed and UpdatePred/ StoppingPred
	    should be predicates that perform the updates and test for
	    stopping conditions.
	</DL>
    </DL>
  <P>
  "),
    see_also: [colgen:set/2]
]).

:- comment(cg_subproblem_solution/1, [
template:  "ColgenInstance:cg_subproblem_solution(++Value)",
    args:  ["Value": "Subproblem solution (sp_sol structure) or list"
                     " of subproblem solutions"
           ],
    summary: "Posts new subproblem solution(s) to the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Post subproblem solution(s) corresponding to a column of coefficients
  for a new master problem variable to the colgen instance
  <TT>ColgenInstance</TT>. The argument must be a <TT>sp_sol</TT>
  structure or list of such structures:
 <PRE>
      sp_sol(cost, coeff_vars, aux)
 </PRE>
  where <TT>cost</TT> is the master problem cost function coefficient
  of the solution, <TT>coeff_vars</TT> is a list of <TT>Id-Val</TT>
  pairs corresponding to the subproblem variable solution values and
  identifier of the constraint in which it occurred as an implicit sum
  term for those subproblem variables with a non-zero solution
  value. <TT>aux</TT> should contain any problem specific information
  which is of interest that is not represented uniquely by the cost
  and objective coefficients.
  </P>
  ")
]).

:- comment(identified_constraint/2, [
template:  "ColgenInstance:identified_constraint(+Cstr, ?Id)",
    args:  ["Cstr": "colgen constraint"],
    summary: "Post an identified constraint to the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Post a constraint to the colgen instance <TT>ColgenInstance</TT>
  which will be associated with the identifier <TT>Id</TT>. The
  constraint <TT>Cstr</TT> must be a valid colgen constraint of type
  <TT>>=/2,=:=/2,=</2,$>=/2,$=/2,$=</2</TT>. If <TT>Id</TT> is
  uninstantiated it will be unified with the external solver row
  number of the constraint when this is set up. The identifier can
  later be used to retrieve the dual value or subproblem cost function
  term associated with the constraint.
  </P> "),
    see_also: [(>=)/2,(=:=)/2,(=<)/2,($>=)/2,($=)/2,($=<)/2,get/2 ] ]).

:- comment(get/2, [
template:  "ColgenInstance:get(++What, -Value)",
args:      ["What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain global problem information.",
desc:      html("\
<P>
   Retrieve information about solver constraints and results, for the
   colgen instance ColgenInstance. What can take one of the following values:

<DL>
    <DT><TT>dual(+Id)</TT>
      <DD>Returns the floating-point value of the
      current dual value for the constraint having identifier Id. See
      also <TT>sp_obj</TT> below.
<P>
    <DT><TT>sp_obj(+Id)</TT>
      <DD>Returns the sub problem objective terms currently associated
      associated with the constraint having identifier Id. This will
      be a term <TT>Val*Var</TT> where Val is the current dual value
      for the constraint as also returned by
      <TT>ColgenInstance:get(dual(Id), Val)</TT> and Var is the
      subproblem variable in the implicit sum term of the
      constraint. It is the users responsibility to get all relevant
      terms of the current cost function and ensure that subproblem
      solutions posted to the colgen instance have a non-negative
      cost.
<P>
    <DT><TT>vars</TT>
      <DD>Returns a list of all variables currently
      associated with the colgen instance <TT>ColgenInstance</TT>.
<P>
    <DT><TT>non_zero_vars</TT>
      <DD>Returns a list of all variables currently associated with
      the colgen instance <TT>ColgenInstance</TT> that have a non-zero
      optimal solution. This may be more efficient than retrieving all
      problem variables after solution, since very many variables can
      be generated and most will have a zero value in the optimal
      solution.
<P>
    <DT><TT>frac_vars</TT>
      <DD>Returns a list of all variables currently associated with
      the colgen instance <TT>ColgenInstance</TT> that have a
      fractional optimal solution. This is intended for use primarily
      in user-defined problem branching predicates.
<P>
    <DT><TT>column_count</TT><DD>
      Number of generated columns.
<P>
    <DT><TT>obj_val</TT><DD>
      Current objective value.
<P>
    <DT><TT>unsatisfiable_cstrs</TT><DD>
<P>
    <DT><TT>satisfiable_cstrs</TT><DD>
<P>
    <DT><TT>generated_non_zero_vars</TT><DD>
<P>
    <DT><TT>non_zero_vars</TT><DD>
<P>
    <DT><TT>vars</TT><DD>
<P>
    <DT><TT>sep_goal</TT><DD>
      Separation goal.
<P>
    <DT><TT>sp_solver</TT><DD>
      Subproblem solver goal.
<P>
    <DT><TT>stab_coeff/bound_minus/plus(Ident)</TT><DD>
      Stabilisation parameters per constraint.

</DL>") ]).

:- comment(var_get/3, [
template:  "ColgenInstance:var_get(+Var, ++What, -Value)",
args:      ["Var":   "A solver problem variable for the solver associated with ColgenInstance",
            "What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for an individual solver problem variable Var.",
desc:      html("\
<P>
   Retrieve information about solver constraints and results related to a
   particular variable, for the colgen instance ColgenInstance.
   What can take one of the following values:
<DL>
    <DT><TT>mp_val</TT>
    <DD>Returns the floating-point solution for variable Var.
<P>
    <DT><TT>cost</TT>
    <DD>Returns the master problem objective coefficient
    associated with the variable Var.
<P>
    <DT><TT>coeffs</TT>
    <DD>Returns a list of Id-Val pairs representing the constraint
    identifiers and coefficient values for the master problem
    constraints in which the coefficient is non-zero associated
    with the variable Var.
<P>
    <DT><TT>aux</TT>
    <DD>Returns the auxiliary information associated with the
    variable Var. The intended use is for subproblem information
    not represented in the master problem constraint coefficients.
</DL>")
]).

:- comment(colgen_instance/1, [
    amode:colgen_instance(++),
    args:  ["ColgenInstance": "Colgen instance name (atom)"
           ],
    summary: "Initialises the colgen instance ColgenInstance.",
    desc: html("\
  <P>
  Initialises the colgen instance ColgenInstance. A colgen instance is an
  instance of the colgen solver, to which colgen partial linear arithmetic
  constraints can be posted, and to which an external LP solver can be
  associated and used to optimise the posted constraints with respect
  to some objective.
  </P><P>
  If ColgenInstance is not an already existing colgen instance, a new colgen
  instance will be created and initialised. If it is an existing colgen
  instance, and it is not currently being used (having no outstanding posted
  constraints and no associated solver), it is effectively reinitialised.
  Otherwise, the predicate aborts with an error. Note that a colgen instance
  is a module, and each colgen instance can be associated with at most one
  solver at any time and vice versa.
  </P>
  "),
    see_also:   [(>=)/2,(=:=)/2,(=<)/2,($>=)/2,($=)/2,($=<)/2,var_get/3]
]).

%:- comment((>=)/2,  [
%    template:  "ColgenInstance:(?X >= ?Y)",
%    args:      ["X":    "Partial linear expression",
%		"Y":    "Partial linear expression"
%	       ],
%    see_also:  [(=:=)/2,(=<)/2,($=)/2,($=<)/2,($>=)/2,var_get/3], 
%
%    summary:   "Constrains X to be greater than or equal to Y.",
%    desc:      html("\
%	Logically: Constrains X to be greater than or equal to Y. X
%	and Y are partial linear expressions. Partial linear
%	expressions may contain terms of the form
%	<TT>implicit_sum(+Var)</TT> in addition to any terms allowed
%	within a standard linear expression. Variables occurring
%	inside <TT>implicit_sum/1</TT> terms are taken to be
%	subproblem variables whose instantiation will correspond to
%	the coefficient of a generated master problem variable in this
%	constraint. Operationally, the constraint gets delayed until
%	the external solver state for ColgenInstance is invoked.")
%    ]).
%
%:- comment((=<)/2,  [
%    template:  "ColgenInstance:(?X =< ?Y)",
%    args:      ["X":    "Partial linear expression",
%		"Y":    "Partial linear expression"
%	       ],
%    see_also:  [(=:=)/2,(>=)/2,($=)/2,($=<)/2,($>=)/2,var_get/3],
%    summary:   "Constrains X to be less than or equal to Y.",
%    desc:      html("\
%	Logically: Constrains X to be less than or equal to Y. X and
%	Y are partial linear expressions. Partial linear expressions
%	may contain terms of the form <TT>implicit_sum(+Var)</TT> in
%	addition to any terms allowed within a standard linear
%	expression. Variables occurring inside <TT>implicit_sum/1</TT>
%	terms are taken to be subproblem variables whose instantiation
%	will correspond to the coefficient of a generated master
%	problem variable in this constraint. Operationally, the
%	constraint gets delayed until the external solver state for
%	ColgenInstance is invoked.")  ]).
%
%:- comment((=:=)/2,  [
%    template:  "ColgenInstance:(?X =:= ?Y)",
%    args:      ["X":    "Partial linear expression",
%		"Y":    "Partial linear expression"
%	       ],
%    see_also:  [(=<)/2,(>=)/2,($=)/2,($=<)/2,($>=)/2,var_get/3],
%    summary:   "Constrains X to be equal to Y.",
%    desc:      html("\
%	Logically: Constrains X to be equal to Y. X and Y are partial
%	linear expressions. Partial linear expressions may contain
%	terms of the form <TT>implicit_sum(+Var)</TT> in addition to
%	any terms allowed within a standard linear
%	expression. Variables occurring inside <TT>implicit_sum/1</TT>
%	terms are taken to be subproblem variables whose instantiation
%	will correspond to the coefficient of a generated master
%	problem variable in this constraint. Operationally, the
%	constraint gets delayed until the external solver state for
%	ColgenInstance is invoked.")  ]).

:- comment(($>=)/2,  [
    template:  "ColgenInstance:(?X $>= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=)/2,($=<)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3], 

    summary:   "Constrains X to be greater than or equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be greater than or equal to Y. X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P>
 ")
]).

:- comment(($=<)/2,  [
    template:  "ColgenInstance:(?X $=< ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3],
    summary:   "Constrains X to be less than or equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be less than or equal to Y.  X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P>
 ")
]).

:- comment(($=)/2,  [
    template:  "ColgenInstance:(?X $= ?Y)",
    args:      ["X":    "Partial linear expression",
		"Y":    "Partial linear expression"
	       ],
    see_also:  [($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,var_get/3],
    summary:   "Constrains X to be equal to Y.",
    desc:      html("\
    <P>
    Logically: Constrains X to be less than or equal to Y.  X and Y
    are partial linear expressions. Partial linear expressions may
    contain terms of the form <TT>implicit_sum(+Var)</TT> in addition
    to any terms allowed within a standard linear
    expression. Variables occurring inside <TT>implicit_sum/1</TT>
    terms are taken to be subproblem variables whose instantiation
    will correspond to the coefficient of a generated master problem
    variable in this constraint. Operationally, the constraint gets
    delayed until the external solver state for ColgenInstance is
    invoked.
    </P>
 ")
]).


:- comment(set/2,  [
    template:  "ColgenInstance:set(+What,++Value)",
    args:      ["What": "Parameter name",
		"Value":"Parameter value"
	       ],
    see_also:  [colgen:get/2, colgen:solver_setup/3],
    summary:   "Set parameters for column generation instance.",
    desc:      html("\
    <P>
    Set parameters for the give column generation instance:
    </P>
<DL>
<DT><TT>disallow (off|lp|clp)</TT><DD>
    policy for active preventions of duplicate columns.
<DT><TT>int_tolerance (1e-5|float)</TT><DD>
    tolerance for optimality.
<DT><TT>basis_perturbation (off|on)</TT><DD>
    should we try and perturb the external solver basis when we appear
    to be at optimal and external solver returns same basis after adding
    columns  ('off' - no, 'on' - temporarily set the external solver
    to always perturb)
<DT><TT>info_messages (off|on)</TT><DD>
    print messages while solving.
<DT><TT>on_degeneracy (stop|continue)</TT><DD>
    should we halt when we find degeneracy (default 'stop'), or
    continue and let the subproblem solver deal with it ('continue').
<DT><TT>stabilisation (off|on()|stab_pred())</TT><DD>
    the policy to perform basis stabilisation:
    <DL>
    <DT><TT>off</TT><DD>
	no stabilisation is performed.
    <DT><TT>on(BoundIter, BoundUpdate, CoeffIter, CoeffUpdate)</TT><DD>
	then the default policy is used with var bounds/coefficients
	updated by BoundUpdate/CoeffUpdate after BoundIter/CoeffIter
	iterations respectively.
    <DT><TT>stab_pred(UpdatePred, StoppingPred)</TT><DD>
	a user defined policy is employed and UpdatePred/ StoppingPred
	should be predicates that perform the updates and test for
	stopping conditions.
    </DL>
<DT><TT>stab_coeff/bound_minus/plus(Ident)</TT><DD>
    parameters for stabilisation, per constraint.
</DL>
 ")
]).



/*
:- comment(integers/1,  [
    template:  "ColgenInstance:integers(?Vars)",
    args:      ["Vars":    "Variable or a list or variables"],
    see_also:  [_:integers/1,reals/1,(::)/2],
    summary:   "Constrains Vars to integers for ColgenInstance.",
    desc:      html("<P>\
	Constrains list Vars to integers in the eplex instance
        EplexInstance. If a variable in Vars is not already a problem
        variable for EplexInstance, it will be added as a new problem
        variable. The external solver will then take the integrality into
        account, i.e. to solve a MIP/MIQP rather than a relaxed LP/QP
        problem.  Unlike integers/1 constraints from other solvers, the
        variables are not constrained to be integer type at the ECLiPSe
        level. However, when a typed_solution is retrieved (e.g. via
        eplex_var_get/3), this will be rounded to the nearest integer.
	<P>
	Note that even when problem variables have been declared as
        integers in other solvers (ic or other external solver
        states), if the integrality constraint is not made known to this
        EplexInstance, any invocation of the eplex external solver (e.g. via
        eplex_solve/1) will only solve a continuous relaxation.
	<P>
	")
    ]).

*/
