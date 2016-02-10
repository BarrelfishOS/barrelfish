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

:- comment(desc, html("\
<P>
   This library lets you use customisable branch-and-bound search. It
   is primarily intended for use with the colgen library in implementing
   branch-and-price algorithms, but can be used with arbitrary
   user-defined node solution, separation and selection goals.
   When application specific methods are not required for node
   separation and/or selection the use of built-in depth-first
   and best-first node selection, and branching on most fractional
   variable or generalised upper bound constraint may be specified.
   When the relaxed problem to be solved at a node involves an eplex
   instance branching may additionally be specified by built-in
   objective coefficient, estimate- or lower-bounding based methods,
   and best-estimate node selection may be used.
</P>
")).

:- comment(statistics/0, [
template: "BfsInstance:statistics",
summary: "Display search tree statistics for bfs instance BfsInstance.",
see_also:[solver_setup/2, solver_setup/3, solve/1
         ],
desc: html("\
<P>
  Display statistics for the search tree associated with the bfs
  instance BfsInstance: total number of nodes created, total search
  time, number of nodes solved, node solution time, number of nodes
  separated, node separation time, total number of global feasible
  solutions found, first and optimal global solution solution time.
</P>
")
]).

:- comment(solver_setup/2, [
template: "BfsInstance:solver_setup(+OptSense, +Solver)",
args:    ["OptSense":     "Optimisation direction: min or max",
          "Solver":       "Node relaxation solver"],
summary: "Setup a bfs solver tree for bfs instance BfsInstance.",
see_also:[integers/1, bfs_branch/1, node_info/5,
          solver_setup/3, solve/1,
          get/2, var_get/3, bfs:statistics/0
         ],
desc: html("\
<P>
  Setup a new solver tree for the bfs instance BfsInstance. The tree
  will be associated with BfsInstance; BfsInstance must not already
  have a solver tree associated with it. Once the solver tree is setup,
  it can be optimised via solve/1.
</P><P>
  This is a simplified version of solver_setup/3, it is equivalent to
  calling solver_setup/3 with the following default options:
<PRE>
       solver_setup(OptSense, Solver, [])
</PRE>
")
]).

:- comment(solver_setup/3, [
template: "BfsInstance:solver_setup(+OptSense, +Solver, ++ListOfOptions)",    
args:      ["OptSense":     "Optimisation direction: min or max",
            "Solver":       "Node relaxation solver",
            "ListOfOptions": "List of solver options"
           ],
summary: "Setup a bfs solver tree for bfs instance BfsInstance.",
see_also:[integers/1, bfs_branch/1, node_info/5,
          solver_setup/2, solve/1,
          get/2, var_get/3, bfs:statistics/0
         ],
desc: html("\
<P>
  Setup a new solver tree for the bfs instance BfsInstance. The tree
  will be associated with BfsInstance; BfsInstance must not already
  have a solver tree associated with it. Once the solver tree is setup,
  it can be optimised via solve/1. This predicate allow various
  options to be specified when setting up the  solver state via
  <TT>ListOfOptions</TT>.
</P><P>
  <TT>OptSense</TT> is the optimisation direction. <B>Note</B> that this
  is assumed to be the same as the sense of optimisation used in
  <TT>Solver</TT>. It is the user's responsibility to ensure 
  that this is in fact the case. <TT>OptSense</TT> is used internally
  for bound updates and pruning, and for node ordering with the built-in
  best-first and best-estimate node selection methods.
</P><P>
  <TT>Solver</TT> is the node relaxed problem solver. It is either a
  user-defined predicate or an eplex instance or handle, for which a
  built-in node relaxation solver is available.

</P><P>
ListOfOptions are:

<DL>

<P>
<DT><STRONG><TT>separation(+Separation)</TT></STRONG>
    <DD>Use the specified method to separate the current node.
    <TT>Separation</TT> is either a user-defined predicate, or one of
    the atoms <TT>fracvar, enhanced, strong, deg_est</TT>
    corresponding to the built-in separation methods. Note that the
    methods <TT>enhanced, strong, deg_est</TT> are only available when
    the node relaxation solver involves an eplex instance.
    <TT>Separation</TT> defaults to fracvar.

<P>
<DT><STRONG><TT>node_select(+Select)</TT></STRONG>
    <DD>Use the specified method (<TT>depth_first, best_first,
    best_estimate</TT>) to select the next open node for solution and
    separation. <TT>Select</TT> defaults to best_first.

<P>
<DT><STRONG><TT>alpha(?AlphaMin, ?AlphaMax)</TT></STRONG>
    <DD>When using estimate- or lower-bounding based dichotomic node
    separation methods the overall value assigned to branching on a
    particular variable or constraint is calculated as a weighted sum
    of the estimates obtained for the two branches it would produce.
    AlphaMin is the weighting given to the minimum of the two estimates
    and AlphaMax to the maximum. <TT>AlphaMin</TT> and <TT>AlphaMax</TT>
    are numbers and default to 2 and 1 respectively.

<P>
<DT><STRONG><TT>beta(?BetaConst, ?BetaPC, ?BetaLB)</TT></STRONG>
    <DD>When using estimate- or lower-bounding based node separation
    methods with a problem involving an eplex instance the estimate
    assigned to each branch produced by branching on a particular
    variable or constraint is calculated as a weighted sum  of the
    pseudo-cost estimate and the lower bound. BetaPC is the weighting
    given to the pseudo-cost estimate and BetaLB to the lower bound.
    BetaConst is a constant offset only used when linear regression is
    employed to update these values during search. <TT>BetaConst</TT>,
    <TT>BetaPC</TT>, <TT>BetaLB</TT> are numbers and default to 0, 1,
    1 respectively.

<P>
<DT><STRONG><TT>pseudo_cost(?PCInit, ?PCUpdate, ?PCRatio)</TT></STRONG>
    <DD>
    <P>When using estimate-based dichotomic node separation methods
    with a problem involving an eplex instance up and down pseudo-costs
    are assigned to each variable and generalised upper bound constraint 
    branch-point representing the estimated degradation in objective
    cost per unit change in variable or constraint value incurred on
    that branch.
    </P><P><TT>PCInit</TT> specifies the method used to initialise these
    values when a variable or constraint branch-point is first considered
    for branching:
    <DL>
    <DD><TT>average</TT> : the pseudocosts are initialised to the average
    of the observed changes in cost of all up or down branches in the
    search tree.
    <DD><TT>cost</TT> : variable pseudocosts are initialised to the
    objective cost coefficient of the variable, constraint branch-point
    pseudocosts to the average of the cost coefficients of variables
    involved.
    <DD><TT>calculated</TT> : the pseudocosts are initialised to a
    value calculated by performing a number of external solver iterations
    equal to <TT>(PCRatio * #iterations in root node)/(2* #fractional
    vars in root node)</TT>.
    </DL>
    The default is <TT>calculated</TT>.
    </P><P><TT>PCUpdate</TT> is an atom specifying the method used to
    update these values throughout the search tree once the variable
    or constraint has been branched on:
    <DL>
    <DD><TT>average</TT> : the pseudocosts are updated to the average
    of the observed changes in cost of all up or down branches in the
    search tree for that variable or constraint. 
    <DD><TT>first</TT> : the pseudocosts are fixed to the observed
    change in cost at the first up or down branch in the search tree for
    that variable or constraint.
    <DD><TT>last</TT> : the pseudocosts are fixed to the observed
    change in cost at the last up or down branch in the search tree for
    that variable or constraint.
    </DL>
    The default is <TT>average</TT>.
    </P><P><TT>PCRatio</TT> is a float between 0 and 1 and is used in
    calculating the number of external solver iterations to perform
    when explicitly calculating initial pseudo-cost estimates; the
    default value is 0.05. Setting small ratios will result in faster
    node separation, but the initial estimates for variables and
    constraints on which the branching decisions are taken will be less
    accurate. Setting larger values will result in more work being performed
    in node separation and better estimates for the branching
    decisions. The optimum value will be problem specific, although in 
    general the overhead of performing a total number of iterations
    more than a small ratio of the root node iterations will outweigh the
    benefit obtained.

<P>
<DT><STRONG><TT>lower_bound(+Limit)</TT></STRONG>
    <DD>When using lower-bounding based node separation methods with a
    problem involving an eplex instance, specify how many external solver
    iterations should be performed to calculate the lower bound.
    <TT>Limit</TT> is an integer and defaults to 1. Setting small
    values of iterations will result in faster node separation, but the
    lower bounds on which the branching decisions are taken will be less
    tight. Setting larger values will result in more work being performed
    in node separation and tighter lower bounds for the branching
    decisions. The optimum value will be problem specific, although in
    general the overhead of performing more than a few iterations will
    outweigh the benefit obtained.

<P>
<DT><STRONG><TT>int_tolerance(+IntTol)</TT></STRONG>
    <DD>Specify how far from integrality an integer variable's node
    solution can fall before it is considered for separation by the
    built-in separation methods. <TT>IntTol</TT> is a float and
    defaults to 0.00001.  
<P>
<DT><STRONG><TT>info_messages(+OnOff)</TT></STRONG>
    <DD>Specify whether information messages should be output at
    various points during solution. This option is most useful for
    debugging purposes. OnOff is one of the atoms <TT>on</TT> or
    <TT>off</TT>, the default is <TT>off</TT>.

</DL>
</P>")
]).

:- comment(bfs_branch/1, [
template:  "BfsInstance:bfs_branch(+Branch)",
args:      ["Branch":   "Prolog term"
           ],
summary:   "Post a branching constraint to the bfs instance BfsInstance.",
see_also:  [solver_setup/2, solver_setup/3, get/2],
desc:      html("\
<P>
   Post a new branching constraint <TT>Branch</TT> to the bfs instance
   BfsInstance. The constraint will be used to create a new child node
   of the current open node in the search tree for BfsInstance.
   <TT>Branch</TT> may be any prolog term, but clearly should be an
   appropriate constraint for the node relaxation solver associated with
   BfsInstance. 
</P>")
]).


:- comment(solve/1, [
template:  "BfsInstance:solve(-Cost)",
args:      ["Cost":   "The optimal solution cost of the problem"
                      " associated with BfsInstance"
           ],
summary:   "Optimise the problem associated with BfsInstance.",
fail_if:   "No solution exists satisfying the global feasibility conditions.",
see_also:  [solver_setup/2, solver_setup/3, get/2, var_get/3, bfs:statistics/0],
desc:      html("\
<P>
   A solver setup with solver_setup/2 or solver_setup/3 is triggered
   using this predicate.
</P><P>
   The node relaxation and separation solvers are applied to the next
   selected node of the problem represented by Handle repeatedly until
   no more open nodes remain. The criteria for node selection order
   depends on the options given to solver_setup/2,3.  solve/1 fails if
   there is no solution or succeeds if an optimal solution is found,
   returning the solution's cost in Cost.  After a success, various
   solution information and statistics can be retrieved using get/2,
   var_get/3 and statistics/0.
</P>")
]).

:- comment(node_cost/1, [
template:  "BfsInstance:node_cost(+Val)",
args:      ["Val":  "Solution cost for problem (number)"
           ],
summary:   "Set solution cost for the problem at a node.",
desc:      html("\
<P>
   Set the solution cost for the problem at the current open node of
   the search tree associated with the bfs instance BfsInstance.
</P>")
]).

:- comment(node_info/5, [
template:  "BfsInstance:node_info(+Var, ?Lo, ?Hi, ?Val, ?RC)",
args:      ["Var":   "A solver problem variable for solver the associated with BfsInstance",
            "Lo":  "Lower bound for Var (number)",
            "Hi":  "Upper bound for Var (number)",
            "Val":  "Solution value for Var (number)",
            "RC":  "Reduced cost for Var (number)"
           ],
summary:   "Get or set node bounds and solution information for an individual"
           " solver problem variable Var.",
desc:      html("\
<P>
   Retrieve or update bounds and solution information for an
   individual solver problem variable Var for the current open node of
   the search tree associated with the bfs instance BfsInstance. If Var
   was not already a problem variable for BfsInstance it will now be
   considered one. If reduced costs are available in problems
   involving eplex instances supplying these to the bfs solver can lead
   to improved pruning.
</P>")
]).

:- comment(var_get/3, [
template:  "BfsInstance:var_get(+Var, ++What, -Value)",
args:      ["Var":   "A solver problem variable for the solver associated with BfsInstance",
            "What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for an individual solver problem variable Var.",
exceptions: [6: "What is not a valid value."],
desc:      html("\
<P>
   Retrieve information about solver results related to a particular
   variable, for the bfs instance BfsInstance. Fails if Var is not a
   problem variable for BfsInstance. What can take one of the following
   values: 

<DL>
    <DT><TT>optimal_val</TT>
    <DD>Returns the floating-point solution for variable Var.
<P>

    <DT><TT>node_val</TT>
    <DD>Returns the floating-point solution for variable Var for the
    current node in the search tree of this instance.
<P>

    <DT><TT>type</TT>
    <DD>Returns the type real or integer of Var in this instance.
</DL>")
]).

:- comment(get/2, [
template:  "BfsInstance:get(++What, -Value)",
args:      ["What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for the problem associated with BfsInstance.",
exceptions: [6: "What is not a valid value."],
desc:      html("\
<P>
   Retrieve information about the problem associated with the bfs
   instance BfsInstance. What can take one of the following values:

<DL>
    <DT><TT>frac_vars</TT>
    <DD>Returns the list of variables declared as integer for
    BfsInstance which have fractional solution values in the current node
    of the search tree.
<P>

    <DT><TT>branches</TT>
    <DD>Returns the branching decisions taken from the root to the
    current node of the search tree.
<P>

    <DT><TT>data</TT>
    <DD>Returns the user-defined data associated with the instance.
<P>

    <DT><TT>node_count</TT>
    <DD>Returns the total number of nodes in the search tree.
</DL>")
]).

:- comment(integers/1, [
template:  "BfsInstance:integers(+Ints)",
args:      ["Ints":   "A variable or list of variables"
           ],
summary:   "Declare Ints as integers in BfsInstance.",
desc:      html("\
<P>
   The variable or list of variables Ints will be considered integers
   when solving the problem associated with BfsInstance. Note that
   this does not mean that they will be treated as integers by the
   specified node relaxation solver, rather that they will be considered
   as candidates for branching decisions in nodes where their solution
   value is not integral (to within a tolerance parameter set in the
   options list of BfsInstance:solver_setup/3). The predefined
   node separation schemes will branch on these variables. User-defined
   node separation predicates may access the fractional variables
   at the current node via a call to BfsInstance:get/2"),
see_also:   [solver_setup/3, get/2]
]).

:- comment(bfs_instance/1, [
amode:bfs_instance(++),
args:  ["BfsInstance": "Bfs instance name (atom)"
       ],
summary: "Initialises the bfs instance BfsInstance.",
desc: html("\
  <P>
  Initialises the bfs instance BfsInstance. A bfs instance is an
  instance of the best first search solver, with which node
  relaxation and separation solvers can be associated and used to
  optimise the problem constraints posted to the relaxed node solver
  with respect to its objective using a specified node ordering scheme.
  In particular best-first and best-estimate search schemes are
  supported and application-specific schemes may be easily defined by
  the user.
  </P><P>
  If BfsInstance is not an already existing bfs instance, a new bfs
  instance will be created and initialised. If it is an existing bfs
  instance, and it is not currently being used (having no associated
  solvers), it is effectively reinitialised. Otherwise, the predicate
  aborts with an error. Note that a bfs instance is a module, and each
  bfs instance can be associated with at most one relaxation and one
  separation solver at any time and vice versa.
  </P>
  "),
see_also:   [integers/1, bfs_branch/1, node_info/5,
             solver_setup/2, solver_setup/3, solve/1,
             get/2, var_get/3, bfs:statistics/0]
]).




