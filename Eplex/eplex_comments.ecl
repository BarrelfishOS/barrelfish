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

:- comment(desc, html("\
<P>
   This library lets you use an external Mathematical Programming solver 
   from within ECLiPSe. This is done either directly to the solver, like
   CPLEX, XPRESS-MP or Gurobi, or indirectly via COIN-OR project's Open Solver
   Interface (OSI), giving access to the CLP, CBC and SYMPHONY solvers.
   For the commercial solvers, the library provides just the interface, 
   and does not include the solver or any required licence to use them.
</P><P>
    The constraints provided are:
    <DL>
    <DT>Vs $:: Bound</DT><DD>Bounds on variables</DD>
    <DT>X $= Y</DT><DD>equality over linear expressions</DD>
    <DT>X $&gt;= Y</DT><DD>inequality over linear expressions</DD>
    <DT>X $=&lt; Y</DT><DD>inequality over linear expressions</DD>
    <DT>integers(Xs)</DT><DD>integrality of variables</DD>
    <DT>sos1(Xs)</DT><DD>all but one are zero</DD>
    <DT>sos2(Xs)</DT><DD>all but two consecutive values are zero</DD>
    <DT>Cond=>Linear</DT><DD>indicator constraint (some solvers only)</DD>
    </DL>
    The operational behaviour of the linear constraints is as follows:
</P><UL>
    <LI>When they contain no variables, they simply succeed or fail.

    <LI>When they contain exactly one variable, they are translated into a
    bound update on that variable for the external solver instance. This
    can lead to immediate failure if the lower bound is greater than the upper.

    <LI>Otherwise, the constraint is transferred to the external solver
    immediately (or as soon as the solver is set up).
</UL><P>
The following arithmetic expression can be used inside the constraints:
<DL>
<DT><STRONG><B>X</B></STRONG>
<DD>Variables. If X is not yet a problem variable for the external solver
    instance, it  is turned into one via an implicit declaration 
    <TT>X :: -inf..inf</TT>.

<P>
<DT><STRONG><B>123, 3.4</B></STRONG>
<DD>Integer or floating point constants.

<P>
<DT><STRONG><B>+</B>Expr</STRONG>
<DD>Identity.

<P>
<DT><STRONG><B>-</B>Expr</STRONG>
<DD>Sign change.

<P>
<DT><STRONG>E1<B>+</B>E2</STRONG>
<DD>Addition.

<P>
<DT><STRONG><B>sum</B>(ListOfExpr)</STRONG>
<DD>Equivalent to the sum of all list elements.

<P>
<DT><STRONG>E1<B>-</B>E2</STRONG>
<DD>Subtraction.

<P>
<DT><STRONG>E1<B>*</B>E2</STRONG>
<DD>Multiplication.

<P>
<DT><STRONG>ListOfExpr1<B>*</B>ListOfExpr2</STRONG>
<DD>Scalar product: The sum of the products of the corresponding
elements in the two lists.  The lists must be of equal length.
</DL>
</P><P>
The external solver can either be explicitly invoked to solve the
problem represented by the constraints, or be invoked in response to
certain trigger conditions. This mechanism makes it possible to tailor
the solving behaviour for a particular application's needs.
</P>
")).

:- comment(lp_get_license/0,  [
    summary:"Get a runtime license token for the external solver.",
    fail_if:"Fails if no license can (currently) be obtained",
    see_also:[lp_get_license/2,lp_release_license/0],
    desc:html("\
    	The eplex-library interfaces to an external simplex/MIP solver
	which might require a license to run (e.g. CPLEX, XPRESS-MP, Gurobi).
	When the eplex-library is loaded, it tries to obtain a license
	immediately.  If this is not possible, for example because the
	license pool is temporarily empty, a warning is printed. The
	application program should then call lp_get_license/0 later
	to obtain a license before any eplex functionality is used.
	If a license is successfully obtained or already held,
	lp_get_license succeeds, otherwise it fails.
    <P>
    	This predicate uses information from the file eplex_lic_info.ecl
	in the ECLiPSe library directory, or solver specific environment
	variable settings to locate the licensing information.
    ")]).

:- comment(lp_get_license/2,  [
    amode:     lp_get_license(+,+),
    args:      ["LicStr":"String or Atom", "LicNum":"Integer"],
    summary:"Get a runtime license token for the external solver.",
    fail_if:"Fails if no license can (currently) be obtained",
    see_also:[lp_get_license/0,lp_release_license/0,lp_get_license_challenge/1],
    desc:html("\
    	The eplex-library interfaces to an external simplex/MIP solver
	which might require a license to run (e.g. CPLEX, XPRESS-MP, Gurobi).
	When the eplex-library is loaded, it tries to obtain a license
	immediately.  However, this might not be possible, either
	because no licenses are currently available, or because
	specific licensing information needs to be supplied.
    <P>
	In the latter case, lp_get_license/2 must be called explicitly
	by the programmer, and the relevant licensing information must
	be supplied as arguments.  The meaning of the arguments is
	specific to the solver used:
    <PRE>
                                LicStr          LicNum

        CPLEX development       unused          unused
        CPLEX runtime           licenvstring    serialnum
        XPRESS-MP development   xpress_path     unused
        XPRESS-MP runtime       xpress_path     response
        Gurobi                  license_file    unused
        OSI                     unused          unused
    </PRE>
        If LicStr is a file or directory name, it is expected to be in
	the native operating system syntax.
    ")]).

:- comment(lp_get_license_challenge/1,  [
    amode:     lp_get_license_challenge(-),
    args:      ["Challenge":"Variable"],
    summary:"Get parameter for computing license key (some external solvers only)",
    fail_if:"Fails if the solver doesn't use a challenge-response licensing system",
    see_also:[lp_get_license/2],
    eg:"
     ?- ( lp_get_license_challenge(Challenge) ->
	    magic_formula(Challenge, Response),
	    lp_get_license(RuntimeLicenseString, Response)
	;
	    lp_get_license(DevelopmentLicensePath, 0)
	).
    ",
    desc:html("\
	This is only needed for certain OEM versions of external solvers!
    <P>
    	The eplex-library interfaces to an external simplex/MIP solver
	which might require a license to run (e.g. CPLEX, XPRESS-MP).
	Certain versions of these external solvers use a challenge-
	response licensing system. Only in such cases is this predicate
	needed. lp_get_license_challenge/1 returns a value which is used to
	compute a license key. This key is then passed into lp_get_license/2.
	For the details of this procedure you will need to consult the
	documentation of the solver vendor.
    <P>
        Be aware that the magic formula supplied by the vendor is usually
        designed to be performed in a language like C, and may rely on the
        behaviour of overflowing with 32 bit integers. In ECLiPSe, no
        overflowing will takes place because of bignums, and this has to be
        taken into account if the magic formula is calculated in ECLiPSe.
    <P>
	If you have a development license of CPLEX or XPRESS-MP, this
	predicate does not need to be called. If called anyway, it fails.
    ")]).

:- comment(lp_release_license/0,  [
    summary:"Release a runtime license token for the external solver.",
    see_also:[lp_get_license/0,lp_get_license/2],
    desc:html("\
    	The eplex-library interfaces to an external simplex/MIP solver
	which might require a license to run (e.g. CPLEX, XPRESS-MP, Gurobi).
	When the eplex-library is loaded, it tries to obtain a license
	immediately, or a license can be obtained by calling
	lp_get_license/0.  A held license can then be released using
	lp_release_license/0.  The predicate always succeeds, even
	if no license was held.
    ")]).

:- comment(integers/1,  [
    template:  ["integers(?Vars)","EplexInstance:integers(?Vars)"],
    args:      ["Vars":    "Variable or number, or a list or submatrix of variables/numbers"],
    see_also:  [_:integers/1,reals/1,(::)/2],
    summary:   "Constrains Vars to integers for eplex instance EplexInstance.",
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


:- comment(sos1/1,  [
    template:  ["sos1(?Vars)","EplexInstance:sos1(?Vars)"],
    args:      ["Vars":    "A collection of variables"],
    see_also:  [_:sos1/1,sos2/1,integers/1],
    summary:   "Constrains all but one of Vars to be zero.",
    desc:      html("<P>\
	Constrains all but one of Vars to be zero.  In MIP terminology
	this is called a Special Ordered Set (SOS) of type 1.
</P><P>
	If the variables are also required to be integral, this must
	be separately declared using integers/1.  Similarly for bounds.
</P>
	")
    ]).


:- comment(sos2/1,  [
    template:  ["sos2(?Vars)","EplexInstance:sos2(?Vars)"],
    args:      ["Vars":    "A collection of variables"],
    see_also:  [_:sos2/1,sos1/1,integers/1],
    summary:   "Constrains all but two consecutive elements of Vars to be zero.",
    desc:      html("<P>\
	Constrains all but two consecutive elements of Vars to be zero.  In
	MIP terminology this is called a Special Ordered Set (SOS) of type 2.
</P><P>
	If the variables are also required to be integral, this must
	be separately declared using integers/1.  Similarly for bounds.
</P>
	")
    ]).


:- comment(reals/1,  [
    template:  ["reals(?Vars)","EplexInstance:reals(?Vars)"],
    args:      ["Vars":    "Variable or number, or a list or submatrix of variables/numbers"],
    see_also:  [_:reals/1,integers/1,(::)/2],
    summary:   "Constraints Vars to the real domain for EplexInstance.",
    fail_if: "Vars contain elements which are neither variable or number.",
    desc:      html("<P>\
	Constrains Vars to the real domain in the eplex instance
        EplexInstance. Any variables that are not problem variables
        for EplexInstance are added to the problem, with unconstrained
        bounds (except by the external solver's idea of infinity).
</P><P>
        Note that the notion of real is used here in the pure mathematical
        sense, where real numbers subsume the integers. If the variables
        are already instantiated, this call checks that the variable is
        instantiated to a number.
</P>
        ")
    ]).


:- comment((::)/2,  [
    amode:     ::(?,++),
    template: ["?Vars :: ++Lo..Hi","EplexInstance: (?Vars :: ++Lo..Hi)"],
    args: ["Vars":"Variable or number, or a list or submatrix of variables/numbers",
	   "Lo..Hi":"Lo, Hi are numbers or arithmetic expressions"
	],
    see_also:  [integers/1,reals/1,_:(::)/2,(#::)/2,($::)/2,get_var_bounds/3],
    summary:   "Constrains list elements to lie between the given bounds.",
    fail_if: "Lo is greater than Hi.",
    exceptions: [5: "Lo..Hi is not in correct form",
                 5: "Lo and Hi are of different numeric types"], 
    desc:      html("\
	Constrain a variable (or all variables in a list) to take only
	values in a given range for the eplex instance EplexInstance. Any
	variables that are not already problem variables for EplexInstance
	will be added to the problem. The range information is passed to
	the external solver state associated with EplexInstance as
	floats. No type information are implied by the bounds.
	<P>
	In particular, if the bounds are given as integers, this
	information is <STRONG>not</STRONG> passed to the external solver.
	Unless eplex:integers/1 is invoked, any
	invocation of the eplex external solver (whether via eplex_solve/1,
	lp_solve/2, lp_demon_setup/5 or optimize/2) will only solve a
	continuous relaxation, even when problem variables have been
	declared as integers in other solvers (e.g. ic).
        <P>
        No propagation is done at the ECLiPSe level with the bounds. In
        particular, the variable is not instantiated even if the lower and
        upper bounds are identical. The bounds may be inconsistent with the
        bounds given for the variables in other EplexInstances or solvers
        (i.e. this will not result in immediate failure).")
    ]).

:- comment(($::)/2,  [
    amode:     $::(?,++),
    template: ["?Vars $:: ++Lo..Hi","EplexInstance: (?Vars $:: ++Lo..Hi)"],
    args: ["Vars":"Variable or number or a list of them",
	   "Lo..Hi":"Lo, Hi are numbers or arithmetic expressions"
	],
    see_also:  [integers/1,reals/1,(::)/2,(#::)/2,_:($::)/2],
    summary:   "Constrains list elements to lie between the given bounds.",
    exceptions: [5: "Lo..Hi is not in correct form",
                 5: "Lo and Hi are of different numeric types"], 
    desc:      html("\
	Constrain a variable (or all variables in a list) to take only
	values in a given range for the eplex instance EplexInstance. Any
	variables that are not already problem variables for EplexInstance
	will be added to the problem. The range information is passed to
	the external solver state associated with EplexInstance as
	floats. No type information are implied by the bounds.
	<P>
	In particular, if the bounds are given as integers, this
	information is <STRONG>not</STRONG> passed to the external solver.
	Unless eplex:integers/1 is invoked, any
	invocation of the eplex external solver (whether via eplex_solve/1,
	lp_solve/2, lp_demon_setup/5 or optimize/2) will only solve a
	continuous relaxation, even when problem variables have been
	declared as integers in other solvers (e.g. ic).
        <P>
        No propagation is done at the ECLiPSe level with the bounds. In
        particular, the variable is not instantiated even if the lower and
        upper bounds are identical. The bounds may be inconsistent with the
        bounds given for the variables in other EplexInstances or solvers
        (i.e. this will not result in immediate failure).")
    ]).


:- comment(eplex_var_get/3, [
template:  ["eplex_var_get(+Var, ++What, -Value)","EplexInstance:eplex_var_get(+Var, ++What, -Value)"],
args:      ["Var":   "A solver problem variable for solver associated with EplexInstance",
            "What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for an individual solver problem variable Var.",
exceptions: [
             6: "What is not a valid value.",
             6: "Var is not a problem variable for EplexInstance.",
             6: "What is unavailable; the information was not requested"
                " at solver setup.",
            40: "Solver state had been previously destroyed."
            ],
desc:      html("\
<P>
   Retrieve information about the (logically) most recent solver state and
   results related to a particular variable, for the eplex instance
   EplexInstance.  Fails if no solution has been computed yet.  What can
   take one of the following values:

<DL>
    <DT><TT>solution</TT>
    <DD>Returns the floating-point solution for variable Var.
<P>

    <DT><TT>typed_solution</TT>
    <DD>Returns the properly typed (integer or float) solution for variable
        Var, depending on if the variable was constrained to be an integer
        or not for EplexInstance.
<P>

    <DT><TT>reduced_cost</TT>
    <DD>Returns the reduced cost for variable Var. If the problem is a MIP, 
        then depending on the external solver, this is either unavailable
        or is the value at the optimal LP node. 
        Note that reduced cost is defined as the minimal amount the
        objective value will be changed by if the variable's value is
        increased by 1 from its solution value.

    <DT><TT>type</TT>
    <DD>Returns the type the variable Var is constrained to in
        EplexInstance. Possible values are integer or real.
</DL>
<P>
  Note that solution or reduced_cost can only be retrieved
  when previously requested in the option list of eplex_solver_setup/4,
  lp_demon_setup/5, lp_setup/4 or with lp_set/3.
</P>")
]).

:- comment(lp_var_get/4, [
amode:     lp_var_get(+,+,++,-),
args:      ["Handle": "Handle to a solver state",
            "Var":   "A solver problem variable for Handle",
            "What":  "Specification for information wanted (atom)",
	    "Value": "Returned value of What"
           ],
summary:   "Obtain information for an individual solver problem variable Var.",
see_also: [eplex_var_get/3],
exceptions: [4: "Handle is not instantiated.",
             5: "Handle is not a compound term.",
             6: "What is not a valid value.",
             6: "Var is not a problem variable for Handle.",
             6: "What is unavailable; the information was not requested"
                " at solver setup.",
            40: "Handle not valid: solver state had been destroyed by cleanup"
            ],
desc:      html("\
<P>
   Retrieve information about the (logically) most recent solver state and
   results related to a particular variable, for the solver state
   represented by Handle. Fails if no solution has been computed yet. What
   can take the same values as those in eplex_var_get/3.")

]).


:-comment(suspend_on_change/2,
          [template:["suspend_on_change(?Var, +Susp)","EplexInstance:suspend_on_change(?Var, +Susp)"],
           args:["Var"  : "A solver problem variable for solver associated with EplexInstance",
                 "Susp" : "Suspension to schedule when the typed solution for this variable changes"],
          summary:"Record the given suspension to be scheduled whenever a solution is found for the EplexInstance."]).

:-comment(get_changeable_value/2,
          [template:["get_changeable_value(?Var, -Val)","EplexInstance:get_changeable_value(?Var, -Val)"],
           args:["Var"  : "A solver problem variable for solver associated with EplexInstance",
                 "Val"  : "The typed_solution value for this variable"],
           summary:"Interface predicate to access the <EM>changeable</EM> value for this variable."]).

:-comment(lp_suspend_on_change/3,
          [template:"lp_suspend_on_change(+Handle, ?Var, +Susp)",
           args:["Handle" : "Handle to a solver state",
                 "Var"  : "A solver problem variable for solver state"
                          " represented by Handle",
                 "Susp" : "Suspension to schedule when the typed solution for this variable changes"],
          summary:"Record the given suspension to be scheduled"
                  " whenever a solution is found for the Eplex handle."]).

:-comment(lp_get_changeable_value/3,
          [template:"lp_get_changeable_value(+Handle, ?Var, -Val)",
           args:["Handle" : "Handle to a solver state",
                 "Var"  : "A solver problem variable for solver state"
                          " represented by Handle",
                 "Val"  : "The typed_solution value for this variable"],
           summary:"Interface predicate to access the <EM>changeable</EM> value for this variable."]).


:- comment(eplex_instance/1, [
    amode:eplex_instance(++),
    args:  ["EplexInstance": "Eplex instance name (atom)"
           ],
    summary: "Initialises the eplex instance EplexInstance.",
    desc: html("\
  <P>
  Initialises the eplex instance EplexInstance. An eplex instance is an
  instance of the eplex solver, to which eplex linear arithmetic and 
  integrality constraints can be posted, and to which an external LP/MIP
  solver can be associated and used to optimise the posted constraints
  with respect to some objective. 
  </P><P>
  If EplexInstance is not an already existing eplex instance, a new eplex
  instance will be created and initialised. If it is an existing eplex
  instance, and it is not currently being used (having no outstanding posted
  constraints and no associated solver), it is effectively reinitialised.
  Otherwise, the predicate aborts with an error. Note that an eplex instance
  is a module, and each eplex instance can be associated with at most one
  solver at any time and vice versa. An eplex instance should NOT be
  erased (e.g. using erase_module/1), as this would only erase the module,
  but not the information that eplex keeps on it as an eplex instance.
  </P>
  "),
    see_also:   [($=)/2,($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,
                 ($::)/2,(::)/2,integers/1,reals/1,
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ]
]).

:- comment((>=)/2,  [
    template:  "EplexInstance:(?X >= ?Y)",
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [_:(>=)/2,(=:=)/2,(=<)/2, 
                ($>=)/2,($=)/2,($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ], 

    summary:   "Constrains X to be greater than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be greater than or equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.")

    ]).

:- comment(($>=)/2,  [
    template:  ["?X $>= ?Y","EplexInstance:(?X $>= ?Y)"],
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [(>=)/2,(=:=)/2,(=<)/2, 
                _:($>=)/2,($=)/2,($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ], 

    summary:   "Constrains X to be greater than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be greater than or equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.
        </P><P>
        The $ version of the arithmetic constraints are provided to allow 
        code to be written which does not specify the solver. They are
        equivalent to their eplex instance counterparts without the $ prefix.
")

    ]).

:- comment((=<)/2,  [
    template:  "EplexInstance:(?X =< ?Y)",
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [(=:=)/2,_:(=<)/2,(>=)/2, 
                ($>=)/2,($=)/2,($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ],
    summary:   "Constrains X to be less than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be less than or equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.")
    ]).

:- comment(($=<)/2,  [
    template:  ["?X $=< ?Y","EplexInstance:(?X $=< ?Y)"],
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [(=:=)/2,(=<)/2,(>=)/2, 
                ($>=)/2,($=)/2,_:($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ],
    summary:   "Constrains X to be less than or equal to Y.",
    desc:      html("\
	Logically: Constrains X to be less than or equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.
        </P><P>
        The $ version of the arithmetic constraints are provided to allow 
        code to be written which does not specify the solver. They are
        equivalent to their eplex instance counterparts without the $ prefix.
    ")
]).

:- comment((=:=)/2,  [
    template:  "EplexInstance:(?X =:= ?Y)",
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [_:(=:=)/2,(=<)/2,(>=)/2, 
                ($>=)/2,($=)/2,($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ],
    summary:   "Constrains X to be equal to Y.",
    desc:      html("\
	Logically: Constrains X to be equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.")
    ]).

:- comment(($=)/2,  [
    template:  ["?X $= ?Y","EplexInstance:(?X $= ?Y)"],
    args:      ["X":    "Linear expression",
		"Y":    "Linear expression"
	       ],
    see_also:  [(=:=)/2,(=<)/2,(>=)/2, 
                ($>=)/2,_:($=)/2,($=<)/2, 
                 eplex_solver_setup/1, eplex_solver_setup/4,
                 eplex_probe/2, eplex_solve/1, 
                 eplex_get/2, eplex_var_get/3, eplex_cleanup/0
                ],
    summary:   "Constrains X to be equal to Y.",
    desc:      html("\
	Logically: Constrains X to be equal to Y.
	X and Y are linear expressions.
	Operationally, the constraint gets delayed until the external
        solver state for EplexInstance is invoked.
        </P><P>
        The $ version of the arithmetic constraints are provided to allow 
        code to be written which does not specify the solver. They are
        equivalent to their eplex instance counterparts without the $ prefix.
    ")
]).

:- comment((=>)/2,  [
    template:  ["Ind => LinCon","EplexInstance:(Ind => LinCon)"],
    args:      ["Ind":    "A binary variable or negated binary variable",
		"LinCon":    "A linear constraint"
	       ],
    see_also:  [ _:(=>)/2, ($>=)/2,($=)/2,($=<)/2, 
                 eplex_add_constraints/2
                ],
    summary:   "Linear constraint conditional on binary variable",
    desc:      html("<P>\
	Logical implication Ind =&ge; LinCon.  The linear constraint LinCon
	is enforced when the indicator Ind is 1, and violation of LinCon
	forces the indicator Ind to 0.
</P><P>
	Ind can be a variable, or a subscript expression resulting in a
	variable.  It can also be negated using the neg/1 prefix operator.
	Correspondingly, LinCon is either enforced by the binary being 0 or 1.
</P><P>
	LinCon is any linear constraint accepted by eplex, i.e. an equality
	or inequality constraint over linear expressions.
</P><P>
	This constraint does not implicitly impose bounds or a type on the
	binary variable in Ind.  It will usually make sense to separately
	impose bounds of 0..1 and invoke integers/1.
</P><P>
	Indicator constraints are not supported by every external solver.
	Use lp_get(has_indicator_constraints,yes) to check availability.
</P>
    "),
    eg:"
    B => ( X1 $>= X2+8 )
    neg B => ( X1+5 $=< X2 )
    B[12] => ( sum(Costs) $= 42 )
"
]).

:- comment(eplex_add_constraints/2, [
 template:  ["eplex_add_constraints(+Constraints,+Integers)",
	     "EplexInstance: eplex_add_constraints(+Constraints,+Integers)"],
 args:      [
             "Constraints": "A (possibly empty) list of equality or inequality constraints",
             "Integers": "A (possibly empty) list of variables to be considered as integers"
            ],
 see_also:  [lp_add_constraints/3,lp_demon_setup/5,lp_add/3,lp_add_constraints/4,
             ($=)/2,($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,sos1/1,sos2/1,(=>)/2],
 resat:     no,
 fail_if: "Any ground (no variable) or bound constraints (one variable) is self-inconsistent.",
 exceptions: [4: "Constraints or Integers uninstantiated.",
              5: "Some constraint in Constraints is non-linear.",
             40: "Solver state had been previously destroyed."              
             ],
 summary:    "Add new constraints to the eplex instance EplexInstance,"
             " possibly triggering it.",
 desc:      html("\
<P>
  Add new constraints (with possibly new variables) to the eplex instance
  EplexInstance.  Cstrs is a list of equality or inequality constraints; 
  NewIntegers is a list of (possibly new) variables that should be
  consider as integers  This is logically the same as posting the
  constraints one by one to the solver, but it may be more convenient if 
  the constraints are already collected as a list.
</P><P>
  Operationally, if a solver state has been associated with the eplex
  instance, the constraints are added to the solver state in one go. 
  For some solvers, this can be more efficient than adding the constraints
  incrementally. 
</P><P>
  If the new_constraint trigger option was selected, then the solver will
  be invoked after the adding of the constraints if these constraints are 
  not already satisfied.  The constraints will be removed on backtracking.
</P><P>
  The constraints are normalised and simplified before being passed to
  the external solver. If any constraint is ground, they are tested for
  consistency.
</P><P>
  The constraints can be linear equalities and inequalities, sos/1 and
  sos2/1 constraints, and =&ge;/2 indicator constraints.
</P><P>
  Note that variables in NewIntegers can be any problem variables. In
  previous versions of ECLiPSe, there was a restriction that the variables
  be new problem variables; this restriction has been removed.
</P><P>

 </P>")
 ]).

:- comment(lp_add_constraints/3, [
 amode:     lp_add_constraints(+,+,++),
 args:      ["Handle":      "Handle to a solver state",
             "Constraints": "A (possibly empty) list of equality or inequality constraints",
             "Integers": "A (possibly empty) list of variables to be"
                         " considered as integers in the problem"
            ],
 see_also:  [lp_demon_setup/5,lp_add/3,lp_add_constraints/4,
             eplex_add_constraints/2,
             ($=)/2,($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2,sos1/1,sos2/1,(=>)/2],
 resat:     no,
 fail_if: "Any ground (no variable) or bound constraints (one variable) is self-inconsistent.",
 exceptions: [4: "Handle, Constraints or Integers uninstantiated.",
              5: "Some constraint in Constraints is non-linear.",
              40: "Handle not valid: solver state had been destroyed by cleanup"

             ],
 summary:    "Add new constraints to the solver Handle, possibly"
 " triggering it.",
 desc:      html("\
<P>
  Add new constraints (with possibly new variables) to a solver.
  NewIntegers is a list of variables that the external solver should
  consider as integers  The new constraints will be taken into
  account the next time the solver is run: if the new_constraint option of
  lp_demon_setup was turned on, then the solver will be invoked after the
  adding of the constraints, unless they are already satisfied.  The 
  constraints will be removed on backtracking.
</P><P>
  The constraints are normalised and simplified before being passed to
  the external solver. If any constraint is ground, they are tested for
  consistency.
</P><P>
  The constraints can be linear equalities and inequalities, sos/1 and
  sos2/1 constraints, and =&ge;/2 indicator constraints.
</P><P>
  Note that variables in NewIntegers can be any problem variables. In
  previous versions of ECLiPSe, there was a restriction that the variables
  be new problem variables; this restriction has been removed.
</P><P>

 </P>")
 ]).


:- comment(lp_add_constraints/4, [
 amode:     lp_add_constraints(+,+,++, -),
 args:      ["Handle":      "Handle to a solver state",
             "Constraints": "A (possibly empty) list of expandable"
                            "linear equality or inequality constraints",
             "Integers": "A (possibly empty) list of problem variables to be considered as integers",
             "Indices": "Indices for the constraints in Constraints"
            ],
 see_also:  [lp_add_constraints/3,lp_demon_setup/5,lp_add/3,lp_add_columns/2,
             ($=)/2,($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2],
 resat:     no,
 exceptions: [4: "Handle, Constraints or Integers uninstantiated.",
              5: "Some constraint in Constraints is ground.",
              5: "Some constraint in Constraints is non-linear.",
              40: "Handle not valid: solver state had been destroyed by cleanup"

             ],
 summary:    "Add new expandable constraints to the demon solver Handle.",
 desc:      html("\
<P>
  Add new expandable constraints (with possibly new variables) to a solver.
  An expandable constraint can be expanded when new variables are added to
  the problem. These constraints are in the form of having a constant on
  the right-hand side, and coefficients for the problem variables on the
  left. When new variables are added (via lp_add_columns/2), the
  coefficients for these variables can be given for the expandable
  constraints. Note that the constraints can only be expanded if there are
  no trigger conditions (i.e. the problem must be solved explicitly), or if
  the problem is not associated with an eplex instance. Otherwise, the
  added constraints are treated as normal (non-expandable) constraints.
</P><P>
  Constraints is a list of expandable constraints.  NewIntegers is a list
  of variables that the external solver should consider as integers,
  Indices is a list of the indices for the added constraints. It is
  returned by the predicate, and has the same length as Constraints. Each
  element in Indices is the index for the constraint in Constraints at the
  same position in the list. This index is used to refer to the constraint
  in lp_add_columns/2.
</P><P>
  Operationally lp_add_constraints/4 behaves like lp_add_constraints/3,
  except that it never trigger the solver as a demon:
  The new constraints will be taken into account the next time the solver
  is run, and  removed on backtracking. The constraints are normalised 
  before being passed to the external solver. Unlike lp_add_constraints/3,
  they are not simplified, and no consistency checks are performed. If any
  of the constraints are ground, a type error is raised. 
</P><P>
  Note that variables in NewIntegers can be any problem variables.
 </P>")
 ]).


:- comment(lp_add_cutpool_constraints/4, [
 amode:     lp_add_cutpool_constraints(+,+,+,-),
 args:      ["Handle":      "Handle to a solver state",
             "Constraints": "A (possibly empty) list of equality or inequality constraints",
             "Options": "A (possibly empty) list of Option:Value pairs",
             "Indices": "Indices for the constraints in CstrSpecs"
            ],
 resat:     no,
 summary:    "Add constraints to the cutpool associated  with solver state Handle.",
 exceptions:[6: "Constraints contains variable(s) that is not in the"
                " problem during setup.",
             5: "A constraint in Constraints is ground."
            ],
              
 see_also: [lp_get/3, lp_set/3],
 desc:      html("\
<P>
  Add constraints to the cutpool associated with solver state Handle.
  Unlike normal constraints, cutpool constraints are not added directly to
  the problem, and are *NOT* removed on backtracking. Logically, cutpool
  constraints are valid for all solves of the problem, regardless of when
  a constraint is added to the pool -- they are `globally valid'. The 
  purpose of the cutpool is to give the user more flexibility on how
  constraints are used by the external solver.
</P><P>
  <TT>Constraints</TT> is the list of constraints to be added to the
  cutpool. As they are not removed on backtracking, they can only involve
  variables that are present at problem setup. Otherwise, an out of range
  error is raised. Variables can be created for a problem before setup by
  posting normal constraints involving the variables, in particular, using
  reals/1. The constraints are normalised before being passed to the
  external solver.  They are not simplified, and no consistency checks
  are performed. If any of the constraints are ground, a type error is
  raised. Instantiated variables are treated as constants (i.e. the value
  they are instantiated to when the predicate is called), even when the
  execution backtracks to a point before the instantiations.
</P><P> 
  <TT>Options</TT> is a possibly empty list of Option:Value pairs specifying 
  the options. Valid options are:
<DL>
<P>
<DT><STRONG><TT>add_initially</TT></STRONG>
   <DD>specifies if the constraints should be added to the problem matrix
   before the external solver is invoked. Valid values are <TT>1</TT> for
   adding the constraints before solving, and <TT>0</TT> for not adding.
   The default is <TT>1</TT>.
<DT><STRONG><TT>active</TT></STRONG>
   <DD>specifies if the constraints should be active. Valid values are 
   <TT>1</TT> for active, and <TT>0</TT> for non-active. The default is <TT>1</TT>.
<DT><STRONG><TT>group</TT></STRONG>
   <DD>specifies the cutpool constraints group that the constraints will be
   added to. <TT>Value</TT> must be an existing group name for the cutpool,
   i.e. the default group, or a group name previously created with 
   the cutpool_group option of lp_set/3. The default is the default group
   name ([]).
</DL>
</P><P>
  Operationally, the cutpool constraints are taken into account when the
  external solver is invoked, and the user can specify how they are taken
  into account by setting the status of these constraints: by default,
  cutpool constraints are active, and are taken into account during solver
  invocation: none of these constraints will be violated in the solution
  produced: they are either part of the problem matrix solved by the
  external solver, or they have been explicitly checked for non-violation. 
  This is done as follows: active cutpool constraints that are labelled as 
  `add_initially' (the default) are added to the problem matrix, and the 
  external solver is invoked. When a solution is produced, the unadded
  active cutpool constraints are checked to see if they are violated. Any
  violated constraints are added to the problem matrix and the problem is
  resolved by the external solver. This is repeated until there are no
  violated constraints. This approach allows the user to leave out
  constraints that might be expensive (i.e. slow down the solving) unless
  they usefully constrain the problem. 
</P><P>
  Checking for violations require the solution values of the problem
  variables. Thus, if the <TT>solution</TT> option is turned off and no
  solution value is available, and there are unadded active cutpool 
  constraints, an out of range error would be raised when the solver tries
  to solve the problem.
</P><P>
  The violation checking is only done if the external solver produces a 
  solution with solution values. If the problem is unbounded (or unknown,
  i.e. the solver cannot determine if the problem is infeasible or
  unbounded), then all the active cutpool constraints are added to the
  problem without checking for violations, and the problem is resolved once
  more. This is done because adding constraints to the problem can make the
  problem become bounded.
</P><P>
  The user can change a cutpool constraint's status to non-active, in which
  case it is neither checked for violations nor added to the problem.
  For logical  correctness, the user should only make a constraint non-active
  if it is certain that the constraint will not be violated, e.g. if it is
  superceded by a stronger constraint. 
</P><P>
  Cutpool constraints are organised into named groups, allowing multiple
  constraints to be referred to as a unit, e.g. with the cutpool_info
  option of lp_get/3.
</P><P>
  Note that as the constraints are added to the cutpool, and not to the
  problem directly, this is not considered as adding new constraints, and
  will not cause the solver to trigger with the new_constraint trigger
  condition set.
  ")
]).
  
:- comment(lp_add_columns/2, [
 amode:     lp_add_columns(+,+),
 args:      ["Handle":      "Handle to a solver state",
             "Columns": "A list of Variable:ColumnSpecification pairs."
            ],
 summary: "Add new variables as columns to the external solver's matrix.",
 exceptions:[4: "Handle or Columns are uninstantiated.",
             5: "A Variable in Columns is already a problem variable for"
                " problem represented by Handle.",
             40: "Handle not valid: solver state had been destroyed by cleanup",
             abort: "Handle is an eplex instance problem, or trigger"
                  " conditions have been specified."
            ],

 desc:      html("\
<P>
   Add new variables as columns to the external solver's matrix. This 
   allows non-zero coefficients for the objective and existing constraints
   to be specified for these variables. 
</P><P>
   Each column is specified as a Variable:ColumnSpecification pair.
   Variable is the variable that is being added to the problem, and
   ColumnSpecification is the specification for the non-zero coefficients
   for the column representing the variable in the external solver matrix:
   this is a list of Index:Coefficient pairs. Where index is the index of an
   existing constraint (obtained when the constraint was added using
   lp_add_constraints/4), and coefficient is its coefficient.
</P><P>
   The ColumnSpecification list can optionally start with a special element
   'obj':Coeff, in which case the coefficient is the objective coefficient
   for the column (otherwise zero).  This can be optionally followed by two
   entries of the form 'lo':Lwb,'hi':Upb, which specify initial lower and
   upper bound on the column variable (otherwise unbounded).  Each
   constraint coefficient should only occur once. Any index not specified
   is given a zero value.
</P><P>
   Note that lp_add_columns/2 essentially modifies the problem represented
   by Handle. This is not compatible with the view of the problem as a
   compound constraint, and therefore, lp_add_columns/2 can only be used
   for problems that are not eplex instances, and/or have no trigger
   conditions for triggering the external solver. 
</P>
  ")
]).



:- comment(lp_add_vars/2, [
 amode:     lp_add_vars(+,+),
 args:      ["Handle":      "Handle to a solver state",
             "Vars": "A list of variables or numbers"
            ],
 summary: "Declare Vars to be problem variables for the solver state"
          " Handle",
 see_also: [lp_setup/4,lp_demon_setup/5],
 desc:    html("\
<P>
   Declares variables in Vars as problem variables for the solver state
   represented by Handle. Any of the variables that are not existing 
   problem variables for Handle are added to the problem. The new
   variables' bounds are constrained to -infinity..infinity (infinity as 
   defined by the external solver).
"),
 fail_if: "Vars contain elements which are neither variables or numbers."]).

                          
        
:- comment(normalise_cstrs/3, [
amode:     normalise_cstrs(+,-,-),
args:      ["Constraints":     "List of arithmetic relations",
            "NormConstraints": "Linear constraints from Constraints (normalised)",
            "NonlinConstraints": "Non-linear constraints from Constraints"
           ],
summary:   "Normalise the linear constraints in Constraints.",
see_also:  [($=)/2,($=<)/2,($>=)/2,(=:=)/2,(=<)/2,(>=)/2],
desc:      html("\
  Constraints is a list of terms of the form X $= Y, X $>= Y or X $=< Y (or
  their non-$ equivalents)  where X and Y are arithmetic expressions.  The linear
  constraints are returned in normalised form in NormConstraints, the
  nonlinear ones are returned unchanged in NonlinConstr.
")
]).

:- comment(eplex_solver_setup/1, [
template: ["eplex_solver_setup(+Objective)","EplexInstance:eplex_solver_setup(+Objective)"],
args:    ["Objective":     "Objective function: min(CostExpr) or max(CostExpr)"],
summary: "Setup an external solver state for eplex instance EplexInstance",
see_also:[($=)/2,($=<)/2,($>=)/2,(=:=)/2, (>=)/2, (=<)/2, 
          ($::)/2, (::)/2,integers/1,reals/1,
          eplex_solver_setup/4, eplex_probe/2,
          eplex_solve/1,
          eplex_get/2, eplex_var_get/3
         ],
desc: html("\
<P>
  Setup a new external solver state for the eplex instance EplexInstance. 
  The solver state will be associated with EplexInstance;
  EplexInstance must not already have a solver state associated with it.
  Once the solver state is setup, it can be invoked (e.g. via eplex_solve/1 
  or eplex_probe/2) to optimise the Objective function. 
</P><P>
    CostExpr is a linear expression
    (or quadratic if supported by the external solver).
</P><P>
  Declaratively, this can be seen as a compound constraint representing all
  the individual linear constraints that have been set so far and are going
  to be set up later for <TT>EplexInstance</TT>. The invoking of this 
  constraint is done explicitly by the user, usually via eplex_solve/1.
  Operationally, when the external solver is invoked, the delayed 
  constraints posted to <TT>EplexInstance</TT> are collected and taken into
  account.
</P><P>
  This is a simplified version of eplex_solver_setup/4, it is equivalent to
  calling eplex_solver_setup/4 with the following defaults:
<PRE>
       eplex_solver_setup(Objective, _, [], [])
</PRE>
")
]).

:- comment(eplex_solver_setup/4, [
template: ["eplex_solver_setup(+Objective, ?Cost, ++ListOfOptions, +TriggerModes)",
	   "EplexInstance:eplex_solver_setup(+Objective, ?Cost, ++ListOfOptions, +TriggerModes)"],
args:      ["Objective":     "Objective function: min(CostExpr) or max(CostExpr)",
            "Cost":          "Variable bounded by the optimal solution",
            "ListOfOptions": "List of solver options",
            "TriggerModes":  "List of conditions for re-triggering solver"
           ],
summary: "Setup an external solver state for eplex instance EplexInstance",
see_also:[($=)/2,($=<)/2,($>=)/2,(=:=)/2, (>=)/2, (=<)/2, 
          ($::)/2,(::)/2,integers/1, reals/1,
          eplex_solver_setup/1,
	  eplex_probe/2, eplex_solve/1,
          eplex_get/2, eplex_var_get/3,
          eplex_get_iis/4,
	  lp_demon_setup/5, lp_setup/4
         ],
desc: html("\
<P>
  Setup a new external solver state for the eplex instance EplexInstance. 
  The solver state will be associated with EplexInstance;
  EplexInstance must not already have a solver state associated with it.
  This predicate allow various options to be specified when setting up the
  solver state. <TT>ListOfOptions</TT> allows a list of solver options to 
  be specified, and <TT>TriggerModes</TT> allows the 
  solver state to be set up as a demon so that the external solver is 
  automatically invoked when the conditions for re-triggering specified in
  <TT>TriggerModes</TT> are met. The external solver can also be invoked 
  explicitly via eplex_solve/1. 
</P><P>
  Declaratively, this can be seen as a compound constraint representing all
  the individual linear constraints that have been set so far and are going
  to be set up later for <TT>EplexInstance</TT>. Operationally, when the
  external solver is invoked, the delayed constraints posted to 
  <TT>EplexInstance</TT> are collected and taken into account.
</P><P>
  <TT>CostExpr</TT> is a linear cost expression (or quadratic, if supported
  by the external solver).
</P><P>
  The external solver's best objective bound will be exported as a bound
  for <TT>Cost</TT>: For a minimisation problem, each solution's best bound
  becomes a lower bound, for maximisation an upper bound on Cost.  This
  technique allows for repeated re-solving with reduced bounds or added
  constraints. Note that Cost is not automatically made a problem variable
  (it can be a problem variable if there are constraints that involve it),
  and thus may not have bounds associated with in. In order for the bounds
  information not to be lost, some bounds should be given to <TT>Cost</TT>
  (e.g. making it a problem variable (but this might introduce unnecessary
  self-waking on bounds change), or via another solver with bounds
  (e.g. ic)).
</P><P>
  <TT>ListOfOptions</TT> is the same as in the low-level primitive 
  lp_demon_setup/5, except that EplexInstance is implicitly associated with
  the new external solver state, so the <TT>collect_from</TT> option of
  lp_demon_setup/5 is not allowed (it is set to <TT>
  collect_from(pool(EplexInstance))</TT> by the predicate). See 
  below for more details.
</P><P>
  <TT>TriggerModes</TT> specifies under which conditions the external solver
  will be re-triggered. If no condition is specified, then the solver must
  be explicitly triggered, usually via eplex_solve/1. The conditions are the
  same as in the low-level primitive lp_demon_setup/5 called by this predicate.
  See below (after <TT>ListOfOptions</TT> descriptions) for more details.
</P><P>
  Note: Some external solvers need to write temporary files when they
  are solving a problem. These are written to the temporary directory
  specified in ECLiPSe's tmp_dir setting (get_flag/2, set_flag/2).

</P><P>
ListOfOptions are (unless explicitly specified otherwise, each option should 
occur no more than once):

<DL>

<P>
<DT><STRONG><TT>initial_solve(+YesNo)</TT></STRONG>
    <DD>Specifies if an initial solve (call to the external solver) should
    be performed immediately after problem setup.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>yes</TT> if any trigger condition is specified in TriggerModes, 
    <TT>no</TT> if no trigger condition is specified.

<P>
<DT><STRONG><TT>method(+Method)</TT></STRONG>
    <DD>Use the specified method (<TT>default, auto, primal, dual, net,
    barrier, sifting, concurrent, concurrent_det</TT>) (representing Primal
    Simplex, Dual Simplex, Network Simplex, Barrier, Sifting etc) to solve
    the problem. For MIP problems, this specifies the start algorithm (the
    one that is used to solve the initial relaxation). See the external
    solver's manual for a description of these methods.
<P> 
    For some of the methods, an additional `auxiliary' method may be 
    specified in brackets. These are:
<DL>
<P>
      <DT><TT>net(Simplex)</TT>: 
      <DD>specifies the Simplex method (<TT>primal</TT> or <TT>dual</TT>) to 
      follow the network optimisation. For LP problems only.
<P>
      <DT><TT>barrier(Crossover)</TT>: 
      <DD>specifies how the crossover to a basic solution from the barrier
      solution is performed. <TT>Crossover</TT> can be <TT>primal</TT>, 
      <TT>dual</TT>, or <TT>none</TT>. <TT>none</TT> means no crossover is
      performed. 
<P>
      <DT><TT>sifting(SubMethod)</TT>: 
      <DD>specifies the method for solving the sifting subproblem. 
      <TT>SubMethod</TT> can be <TT>primal, dual, net, barrier</TT>.
<P>
</DL>
    For all the auxiliary methods, <TT>default</TT> can also be specified.
    This is equivalent to not specifying an auxiliary method at all. 
<P>
    Note that not every method is available on every external solver.
    The default method would use the solver's default method, or any
    selections done via solver-specific optimizer_param(_) settings.

<P>
<DT><STRONG><TT>node_method(+Method)</TT></STRONG>
    <DD>For MIP problems only. Use the specified method (<TT>default, primal,
    dual, net, barrier, sifting</TT>) to solve the subproblem at each node
    of the MIP search-tree, except the root node, which is specified by
    <TT>method</TT> option above. See method option for more description of
    the methods. Note that there are less choices in the specifications of
    the auxiliary methods that in the method option, due to limitations in
    the solvers. If a specified auxiliary method cannot be used, `default'
    will be used instead.

<P>
<DT><STRONG><TT>solution(+YesNo)</TT></STRONG>
    <DD>Make the solutions available each time the problem has been (re-)solved
    successfully.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>yes</TT>.

<P>
<DT><STRONG><TT>dual_solution(+YesNo)</TT></STRONG>
    <DD>Make the dual solutions available each time the problem has been 
    (re-)solved successfully. If the problem is a MIP, then depending on
    the external solver, this is either unavailable or are the values for
    the optimal LP node. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>slack(+YesNo)</TT></STRONG>
    <DD>Make the constraint slacks available each time the problem has been 
    (re-)solved successfully.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>reduced_cost(+YesNo)</TT></STRONG>
    <DD>Make the reduced costs available each time the problem has been 
    (re-)solved successfully. If the problem is a MIP, then depending on
    the external solver, this is either unavailable or are the values for
    the optimal LP node. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>keep_basis(+YesNo)</TT></STRONG>
    <DD>Store the basis each time the problem has been solved successfully,
    and use this basis as a starting point for re-solving next time.
    This option only affects performance.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
    <DT><STRONG><TT>mipstart(+Option)</TT></STRONG>
    <DD>Use the previous solution values as a warm-start heuristics for
    the MIP search.  This only has an effect for certain solvers (e.g.
    Gurobi), if there are integrality constraints, and if there is a 
    previous solution available.  Possible values are <TT>none</TT>
    (no mipstart values, the default), <TT>all</TT> (use previous
    solution for all variables), or <TT>integers</TT> (use previous
    solution for all variables that are now constrained to be integral).  

<P>
    <DT><STRONG><TT>cache_iis(YesNo)</TT></STRONG>
     <DD>Specify if an IIS should be computed immediately for an infeasible problem
     (if supported by the external solver), and store it so that it can bee retrieved by
     eplex_get_iis/4 or lp_get_iis/5 (called from within a user-defined infeasible 
     handler). This will be done before the problem can be modified and make the computing 
     of the IIS impossible. The IIS will only be available before the problem is solved
     again, and also before the infeasible solve is backtracked. This option has no effect  
     if the external solver does not support the computation of an IIS. Note that if this 
     option is set, eplex will always ask for an IIS to computed for an infeasible problem, 
     even if it is immediately backtracked by the infeasible handler failing, and that the 
     option is only needed if the problem instance in the external solver is modified 
     before eplex_get_iis/4 or lp_get_iis/5 is called. 
     YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>demon_tolerance(RealTol, IntTol)</TT></STRONG>
    <DD>Specify how far outside a variable's range an lp-solution
    can fall before lp_demon_setup/5 re-triggers.
    <TT>RealTol</TT> and <TT>IntTol</TT> are floats and default to
    0.00001 and 0.5 respectively.

<P>
<DT><STRONG><TT>sos1(VarList)</TT></STRONG> - deprecated, use sos1/1 constraint
    <DD><TT>VarList</TT> is a list of variables which the solver should
    treat as variables of a type 1 special ordered set (SOS), i.e. at most
    one of the variables in the set can be non-zero. This can occur multiple
    times, for different sets of variables.

<P>
<DT><STRONG><TT>sos2(VarList)</TT></STRONG> - deprecated, use sos2/1 constraint
    <DD><TT>VarList</TT> is a list of variables which the solver should
    treat as variables of a type 2 special ordered set (SOS), i.e. at most
    two of the variables in the set can be non-zero. This can occur multiple
    times, for different sets of variables.

<P>
<DT><STRONG><TT>presolve(+YesNo)</TT></STRONG>
    <DD>Specify if the external solver should perform presolve for this
    problem. With presolving, the external solver will transform the
    problem before solving it. This can lead to significantly faster times
    to find solutions. However, as the problem has been transformed, some
    external solvers have restriction on accessing or changing the problem
    state. In addition, if the solver is repeatedly called because the
    problem is frequently modified, then presolve may not be an advantage.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    determined by the global setting of <TT>presolve</TT>, which can be
    changed via <TT>lp_set/2</TT>. The initial default is <TT>yes</TT>.
    Note that the presolve setting cannot be changed for a problem once it
    is set.  If the external solver supports per-problem optimizer_params, 
    their global defaults will be used for presolve(yes).
<P>
<DT><STRONG><TT>timeout(+TimeOut)</TT></STRONG>
    <DD>Set the external solver to time-out after <TT>TimeOut</TT> seconds.
    <TT>TimeOut</TT> is a positive number. The solver will abort (in either
    the abort or suboptimal state, depending on if a suboptimal solution
    was found) if the optimal solution was not found within the time
    limit. This should be used instead of setting the solver-specific
    parameter for time-out directly. In cases where the solver expects an
    integer for the time-out interval, the time given is rounded up to the
    next integer value. The timeout is set by setting the external
    solver's timeout settings, and the exact behaviour may be solver dependent.
<P>
<DT><STRONG><TT>suboptimal_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver returned a suboptimal solution (because the problem
    was aborted). Goal would be run in place of raising the default 
    <TT>eplex_suboptimal</TT> event.
<P>
<DT><STRONG><TT>unbounded_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    problem is unbounded. Goal would be run in place of raising the  
    default <TT>eplex_unbounded</TT> event.
<P>
<DT><STRONG><TT>infeasible_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver found that the problem is infeasible. Goal would be run
    in place of raising the default <TT>eplex_infeasible</TT> event.  Note
    that the default and logically correct behaviour is to fail, this
    handler is provided to allow the user to analyse the cause of the
    infeasibility. It is recommended that the handler should also fail
    after performing the analysis.
<P>
<DT><STRONG><TT>unknown_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver was not able to determine if the problem is unbounded
    or infeasible. Goal would be run in place of raising the default 
    <TT>eplex_unknown</TT> event.
<P>
<DT><STRONG><TT>abort_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver aborted without finding any solution. Goal would be 
    run in place of raising the default <TT>eplex_abort</TT> event.
<P>
<DT><STRONG><TT>use_var_names(+YesNo)</TT></STRONG>
    <DD>Specify if variable names (set using <TT>set_var_name/2</TT> of the 
    var_name library) should be passed to the external solver. If a 
    particular variable does not have a name when it is first passed to the
    external solver, a default name determined by the solver would be used.
    Note that for XPRESS-MP, there is a limit on the length of the name,
    which can be changed between 8 and 64 in steps of 8 with the 
    parameter <TT>mpsnamelength (XPRS_MPSNAMELENGTH)</TT>. Variable 
    names longer than this limit are truncated to the limit. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.
<P>
<DT><STRONG><TT>priority(+Prio)</TT></STRONG>
  <DD><TT>Prio</TT> is the scheduling priority with which the solver gets
  woken up.  This priority determines whether the solver is run before or
  after other constraints. By default, if no priority is specified, the
  default priority (0, mapped to 5 unless changed) is used. Normally, the
  default priority should be sufficient and this option is not needed,
  unless there is a specific need to have the external solver invoked with
  higher or lower priority than some other constraints.
<P>
<DT><STRONG><TT>mip_use_copy(+YesNo)</TT></STRONG>
    <DD>Some external solvers do not allow a MIP problem to be modified
    once the MIP search has been started. Eplex works around this
    problem by making a copy of the problem and solving that, so that
    the original problem can still be modified. This can be turned off to
    avoid the overhead of making this copy, in which case the MIP
    problem cannot be modified. This option is used only when solving a
    MIP problem, and the external solver does not allow a MIP problem to
    be modified; otherwise it is ignored.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default 
    is <TT>yes</TT> so that the problem can be modified.
<P>
<DT><STRONG><TT>write_before_solve(+Format,+File)</TT></STRONG>
    <DD>This option is most useful for debugging purposes. If given, Eplex
    will ask the external solver to dump the problem each time the problem
    is solved. This allows the problem in an <TT>eplex_probe/2</TT> or
    <TT>lp_probe/3</TT> to be dumped. As in <TT>lp_write/3</TT>,
    <TT>Format</TT> is the format of the dumped file and <TT>File</TT> is
    its name. Note that the problem is dumped each time the external solver
    is invoked if the problem has cutpool constraints, where there may be
    multiple invocations of the solver per solver call. 
    The default without this option is that the problem would not be dumped.
<P>
<DT><STRONG><TT>post_equality_when_unified(+YesNo)</TT></STRONG>
    <DD>This option determines if an equality constraint between two
    variables will be posted to the solver when these variables are
    unified. Setting <TT>YesNo</TT> to no means that the constraint 
    will <EM>not</EM> be posted. Note that this can lead to the
    solver's problem becoming inconsistent with ECLiPSe's. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>yes</TT>.
<P>
<DT><STRONG><TT>sync_bounds(+YesNo)</TT></STRONG>
    <DD>This option determines if the bounds of the problems variables are
    synchronised with other solvers (i.e. the generic bounds are obtained
    with get_var_bounds/3 and then passed to the external solver) before
    the external solver is invoked. This was always done for previous
    non-standalone version of eplex. For standalone eplex, as the bounds
    are communicated directly to the external solver, the synchronisation
    of variable bounds is not needed unless the user is using eplex
    co-operatively with other solvers (e.g. ic). Even in such cases, it may
    be more efficient to communicate these bounds changes by explicitly
    programming it, especially if the problem has many variables and bounds
    changes happen only to a few of the variables. Setting <TT>YesNo</TT>
    to yes should increase compatibility with previous code (but note that
    previous eplex obtained the bounds from a specific bounds keeper like
    ic rather than the generic bounds).  YesNo is one of the atoms
    <TT>yes</TT> or <TT>no</TT>, the default is <TT>no</TT>.

</DL>
<P>
<TT>TriggerModes</TT> can be a list of the following specifiers:

<DL>

  <DT><STRONG><TT>inst</TT></STRONG>
  <DD>re-trigger if a problem variable gets instantiated.

  <DT><STRONG><TT>ModuleName:Index</TT></STRONG>
  <DD>re-trigger when the  suspension list given by ModuleName:Index is woken
  for any of the problem variables.
  The format for <TT>ModuleName:Index</TT> is the same as for specifying
  the suspension list in suspend/3,4.

  <DT><STRONG><TT>deviating_inst</TT></STRONG>
  <DD>re-trigger if a problem variable gets instantiated
      to a value that differs from its lp-solution more than a tolerance.

  <DT><STRONG><TT>bounds</TT></STRONG>
  <DD>re-trigger each time a variable bound for the solver instance changes.

  <DT><STRONG><TT>deviating_bounds</TT></STRONG>
  <DD>re-trigger each time a variable's solver instance bound changes
      such that its lp-solution gets excluded more than a tolerance.

  <DT><STRONG><TT>new_constraint</TT></STRONG>
  <DD>re-trigger each time a new (arithmetic or integral) constraint is
      added to the solver instance. Note that adding integral constraint
      on new problem variables, or adding bounds constraint, or adding 
      constraints to the cutpool will *not* re-trigger.

  <DT><STRONG><TT>trigger(Atom)</TT></STRONG>
  <DD>re-trigger each time the symbolic trigger Atom is pulled by invoking 
      schedule_suspensions/1

  <DT><STRONG><TT>pre(Goal)</TT></STRONG>
  <DD>an additional condition to be used together with other triggers. When 
      the demon is triggered, it first executes <TT>PreGoal</TT>. Only if 
      that succeeds, does the appropriate external solver get invoked.
      This provides a way of reducing the number of (possibly expensive)
      solver invocations when given preconditions are not met.

  <DT><STRONG><TT>post(Goal)</TT></STRONG>
  <DD>this is not a trigger condition, but specifies a goal to be executed
      after solver success, but before the Cost variable gets
      constrained. It is intended as a hook for exporting solution
      information, e.g. copying solutions from the solver state into
      variable attributes (eg. tentative value), or computing weights for
      labelling heuristics from the solver state.  
  <DT><STRONG><TT>suspension(Susp)</TT></STRONG>
  <DD>this is not a trigger condition, but instead is used to access the 
      demon used to trigger the solver. Susp is instantiated to
      the suspension that triggers the solver: by waking Susp, the solver
      is triggered. Susp is a demon in that it stays around after being
      woken. Accessing Susp allows the user to specify arbitrary conditions 
      for triggering the solver.
</DL>

  The tolerances mentioned can be specified in lp_setup/4 or lp_set/3
  as <TT>demon_tolerance</TT>.

</P><P>
  If several trigger conditions are specified, then any of them will trigger
  the solver.

</P><P>
  When a solver demon runs frequently on relatively small problems,
  it can be important for efficiency to switch off the presolve option
  to reduce overheads.

</P>")
]).

 
  

:- comment(lp_demon_setup/5, [
amode:lp_demon_setup(+,?,++,++,-),
args:      ["Objective":     "Objective function: min(CostExpr) or max(CostExpr)",
            "Cost":          "Variable bounded by the optimal solution",
            "ListOfOptions": "List of solver options",
            "TriggerModes":  "List of conditions for re-triggering solver",
            "Handle":        "handle to solver state"
           ],
see_also:  [lp_solve/2, lp_set/3, lp_setup/4, lp_get_iis/5,
            solution_out_of_range/1, schedule_suspensions/1,
	    library(constraint_pools)],
summary:   "Setup the external solver as a simplex demon.",
eg:        "\
   Some common invocations patterns for this predicate are the following.
   The first triggers the solver only on instantiation of variables to
   values that don't fit with the simplex solution:

      lp_demon_setup(min(Expr), C, [], [deviating_inst], H)

",
desc:      html("\
<P>
  Setup the external solver as a simplex demon. A simplex demon collects
  linear constraints and re-solves the problem whenever the triggering
  conditions in TriggerModes are met.

</P><P>
  Declaratively, this can be seen as a compound constraint representing all
  the individual linear constraints that have been set so far and are going
  to be set up later.  Operationally, the delayed constraints are collected
  and an external solver is set up (as with lp_setup/4).  Then the problem
  is solved once initially (if <TT>initial_solve</TT> option is yes) and a
  delayed goal lp_demon is set up which will re-trigger the solver when 
  certain  conditions are met.

</P><P>
  <TT>CostExpr</TT> is a linear cost expression (or quadratic, if supported
  by the external solver).

</P><P>
  <TT>Handle</TT> refers to the created solver state (as in lp_setup/4 or
  lp_read/3 described below). It can be used to access and modify the state
  of the solver, retrieve solution information etc. 

</P><P>
  Unlike with lp_solve/2, <TT>Cost</TT> will not be instantiated to a
  solution's cost, but only be bounded by the best-bound on cost:
  For a minimisation problem, each solution's best-bound becomes a lower bound,
  for maximisation an upper bound on Cost.  This technique allows for
  repeated re-solving with reduced bounds or added constraints. Note that
  Cost is not automatically made a problem variable (it can be a problem
  variable if there are constraints that involve it), and thus may not have
  bounds associated with in. In order for the bounds information not to be
  lost, some bounds should be given to <TT>Cost</TT> (e.g. making it a
  problem variable (but this might introduce unnecessary self-waking on
  bounds change), or via another solver with bounds (e.g. ic)).
</P><P>
  <TT>ListOfOptions</TT> is a list of solver options as described for
  lp_setup/4. In addition, the following extra options are also available:
   
<DL>

<DT><STRONG><TT>collect_from(+Pool)</TT></STRONG>
    <DD>Specifies if this solver state should be associated with an eplex
    instance. If Pool is <TT>none</TT>, then the solver is not associated
    with an eplex instance. If Pool is <TT>pool(Instance)</TT>, where
    Instance is the (atomic) name of an existing eplex instance, then this
    eplex instance would be associated with the solver state, i.e. when the
    solver is invoked, it will collect constraints posted to
    <TT>Instance</TT>.  Note that <TT>Instance</TT> must not be associated
    with any other solver state already.
    The default value for <TT>Pool</TT> is pool(eplex) (for backward
    compatibility).

<DT><STRONG><TT>initial_solve(+YesNo)</TT></STRONG>
    <DD>Specifies if an initial solve (call to the external solver) should
    be performed immediately after problem setup.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>yes</TT> if any trigger condition is specified in TriggerModes, 
    <TT>no</TT> if no trigger condition is specified.

<DT><STRONG><TT>priority(+Prio)</TT></STRONG>
    <DD><TT>Prio</TT> is the scheduling priority with which the solver gets
    woken up.  This priority determines whether the solver is run before or
    after other constraints. By default, if no priority is specified, the
    default priority (0, mapped to 5 unless changed) is used. Normally, the
    default priority should be sufficient and this option is not needed,
    unless there is a specific need to have the external solver invoked with
    higher or lower priority than some other constraints.
</DL>
   
</P><P>

  <TT>TriggerModes</TT> specifies under which conditions the solver demon
  will be re-triggered. It can be a list of the following specifiers

<DL>

  <DT><STRONG><TT>inst</TT></STRONG>
  <DD>re-trigger if a problem variable gets instantiated.

  <DT><STRONG><TT>ModuleName:Index</TT></STRONG>
  <DD>re-trigger when the  suspension list given by ModuleName:Index is woken
  for any of the problem variables.
  The format for <TT>ModuleName:Index</TT> is the same as for specifying
  the suspension list in suspend/3,4.

  <DT><STRONG><TT>deviating_inst</TT></STRONG>
  <DD>re-trigger if a problem variable gets instantiated
      to a value that differs from its lp-solution more than a tolerance.

  <DT><STRONG><TT>bounds</TT></STRONG>
  <DD>re-trigger each time a variable bound for the solver instance changes.

  <DT><STRONG><TT>deviating_bounds</TT></STRONG>
  <DD>re-trigger each time a variable's solver instance bound changes
      such that its lp-solution gets excluded more than a tolerance.

  <DT><STRONG><TT>new_constraint</TT></STRONG>
  <DD>re-trigger each time a new (arithmetic or integral) constraint is
      added to the solver instance. Note that adding integral constraint
      on new problem variables, or adding bounds constraint, or adding 
      constraints to the cutpool will *not* re-trigger.

  <DT><STRONG><TT>trigger(Atom)</TT></STRONG>
  <DD>re-trigger each time the symbolic trigger Atom is pulled by invoking 
      schedule_suspensions/1

  <DT><STRONG><TT>pre(Goal)</TT></STRONG>
  <DD>an additional condition to be used together with other triggers. When 
      the demon is triggered, it first executes <TT>PreGoal</TT>. Only if 
      that succeeds, does the appropriate external solver get invoked.
      This provides a way of reducing the number of (possibly expensive)
      solver invocations when given preconditions are not met.

  <DT><STRONG><TT>post(Goal)</TT></STRONG>
  <DD>this is not a trigger condition, but specifies a goal to be executed
      after solver success, but before the Cost variable gets
      constrained. It is intended as a hook for exporting solution
      information, e.g. copying solutions from the solver state into
      variable attributes (eg. tentative value), or computing weights for
      labelling heuristics from the solver state.  
  <DT><STRONG><TT>suspension(Susp)</TT></STRONG>
  <DD>this is not a trigger condition, but instead is used to access the 
      demon used to trigger the solver. Susp is instantiated to
      the suspension that triggers the solver: by waking Susp, the solver
      is triggered. Susp is a demon in that it stays around after being
      woken. Accessing Susp allows the user to specify arbitrary conditions 
      for triggering the solver.
</DL>

  The tolerances mentioned can be specified in lp_setup/4 or lp_set/3
  as <TT>demon_tolerance</TT>.

</P><P>
  If several trigger conditions are specified, then any of them will trigger
  the solver.

</P><P>
  When a solver demon runs frequently on relatively small problems,
  it can be important for efficiency to switch off the presolve option
  to reduce overheads.

</P><P>
  The solver demon calls lp_solve/2 when it wakes up. See the description
  of lp_solve/2 for the handling of exceptions.
</P><P>
  Note: Some external solvers need to write temporary files when they
  are solving a problem. These are written to the temporary directory
  specified in ECLiPSe's tmp_dir setting (get_flag/2, set_flag/2).
</P>")
]).


:- comment(instantiation_deviates/1, [
amode:     instantiation_deviates(+),
args:      ["Handle": "Handle to a solver state"],
see_also:  [lp_demon_setup/5],
summary:   "A trigger goal for lp_demon_setup/5.",
desc:      html("\
<P>

  This is intended as a useful pre(Goal) for lp_demon_setup/5 in connection
  with the <TT>inst</TT> trigger mode.  It succeeds if any of the variables
  originally involved in Handle have been instantiated to a value that is
  not within +/- tolerance from the latest simplex solution for that
  variable.  The admissible tolerances can be specified in lp_setup/4 or
  lp_set/3 as <TT>demon_tolerance</TT>.

</P>")
]).

:- comment(solution_out_of_range/1, [
amode:     solution_out_of_range(+),
args:      ["Handle": "Handle to a solver state"],
see_also:  [lp_demon_setup/5],
summary:   "A trigger goal for lp_demon_setup/5.",
desc:      html("\
<P>
  This is intended as a useful pre(Goal) for lp_demon_setup/5 in connection
  with the <TT>bounds</TT> trigger mode.  It succeeds if any of the
  solutions (computed by the most recent successful solving) of Handle are
  more than a tolerance outside the range of the corresponding variables,
  ie. couldn't be instantiated to this value.  The admissible tolerances
  can be specified in lp_setup/4 or lp_set/3 as <TT>demon_tolerance</TT>.
</P>")
]).

:- comment(lp_setup/4, [
amode:     lp_setup(+,+,++,-),
args:      ["NormConstraints": "normalised constraints",
            "Objective":       "Objective function: min(CostExpr) or max(CostExpr)",
            "ListOfOptions":   "List of solver options",
            "Handle":          "handle to solver state"
           ],
summary:   "Create a new external solver state for the constraints NormConstraints.",
see_also:  [lp_add/3, lp_set/3, lp_add_vars/2, lp_add_constraints/3,
            lp_solve/2, lp_probe/3, lp_get/3, lp_get_iis/5,
            normalise_cstrs/3, lp_write/3],
desc:      html("\
<P>
    Create a new solver state for the set of constraints NormConstraints
    (see normalise_cstrs/3 for how to obtain a set of normalised
    constraints).  Apart from the explicitly listed constraints, the
    variable's ranges will be taken into account as the variable bounds for
    the simplex algorithm.  Undeclared variables are implicitly declared as
    reals.

</P><P>
    However, when variables have been declared integers by other solvers (e.g.
    integers/1 of the ic library) that is not taken into account by the
    solver by default.  This means that the solver will only work on the
    <EM>relaxed problem</EM> (ie. ignoring the integrality constraints),
    unless specified otherwise in the options.

</P><P>
    CostExpr is a linear expression
    (or quadratic if supported by the external solver).

</P><P>
    Options is a list of options (see below). Unless explicitly specified
    otherwise, each option should occur no more than once.


</P><P>
    A solver-handle Handle is returned which is used to refer to the solver
    subsequently.

</P><P>
    Note: Some external solvers need to write temporary files when they
    are solving a problem. These are written to the temporary directory
    specified in ECLiPSe's tmp_dir setting (get_flag/2, set_flag/2).

</P><P>
The solver Options are:
<DL>

<DT><STRONG><TT>integers(+ListOfVars)</TT></STRONG>
    <DD>Consider the specified variables to be integers (whether or not
    they have been declared such).
    This option will instruct the external solver to use its own MIP solver
    (ie. branch-and-bound search happens within the external solver).
<P>
<DT><STRONG><TT>reals(+ListOfVars)</TT></STRONG>
    <DD>Consider the specified variables to be problem variables, but does
    not otherwise constrain the variables. This option allows variables to
    be added to the problem without requiring them to occur in other
    constraints. 
<P>
<DT><STRONG><TT>method(+Method)</TT></STRONG>
    <DD>Use the specified method (<TT>default, auto, primal, dual, net,
    barrier, sifting, concurrent, concurrent_det</TT>) (representing Primal
    Simplex, Dual Simplex, Network Simplex, Barrier, Sifting etc) to solve
    the problem. For MIP problems, this specifies the start algorithm (the
    one that is used to solve the initial relaxation). See the external
    solver's manual for a description of these methods.
<P> 
    For some of the methods, an additional `auxiliary' method may be 
    specified in brackets. These are:
<DL>
<P>
      <DT><TT>net(Simplex)</TT>: 
      <DD>specifies the Simplex method (<TT>primal</TT> or <TT>dual</TT>) to 
      follow the network optimisation. For LP problems only.
<P>
      <DT><TT>barrier(Crossover)</TT>: 
      <DD>specifies how the crossover to a basic solution from the barrier
      solution is performed. <TT>Crossover</TT> can be <TT>primal</TT>, 
      <TT>dual</TT>, or <TT>none</TT>. <TT>none</TT> means no crossover is
      performed. 
<P>
      <DT><TT>sifting(SubMethod)</TT>: 
      <DD>specifies the method for solving the sifting subproblem. 
      <TT>SubMethod</TT> can be <TT>primal, dual, net, barrier</TT>.
<P>
</DL>
    For all the auxiliary methods, <TT>default</TT> can also be specified.
    This is equivalent to not specifying an auxiliary method at all. 
<P>
    Note that not every method is available on every external solver.
    The default method would use the solver's default method, or any
    selections done via solver-specific optimizer_param(_) settings.

<P>
<DT><STRONG><TT>node_method(+Method)</TT></STRONG>
    <DD>For MIP problems only. Use the specified method (<TT>default, primal,
    dual, net, barrier, sifting</TT>) to solve the subproblem at each node
    of the MIP search-tree, except the root node, which is specified by
    <TT>method</TT> option above. See method option for more description of
    the methods. Note that there are less choices in the specifications of
    the auxiliary methods that in the method option, due to limitations in
    the solvers. If a specified auxiliary method cannot be used, `default'
    will be used instead.

<P>
<DT><STRONG><TT>solution(+YesNo)</TT></STRONG>
    <DD>Make the solutions available each time the problem has been (re-)solved
    successfully.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>yes</TT>.

<P>
<DT><STRONG><TT>dual_solution(+YesNo)</TT></STRONG>
    <DD>Make the dual solutions available each time the problem has been 
    (re-)solved successfully. If the problem is a MIP, then depending on
    the external solver, this is either unavailable or are the values for
    the optimal LP node. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>slack(+YesNo)</TT></STRONG>
    <DD>Make the constraint slacks available each time the problem has been 
    (re-)solved successfully.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>reduced_cost(+YesNo)</TT></STRONG>
    <DD>Make the reduced costs available each time the problem has been
    (re-)solved successfully. If the problem is a MIP, then depending on
    the external solver, this is either unavailable or are the values for
    the optimal LP node. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>keep_basis(+YesNo)</TT></STRONG>
    <DD>Store the basis each time the problem has been solved successfully,
    and use this basis as a starting point for re-solving next time.
    This option only affects performance.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
    <DT><STRONG><TT>mipstart(+Option)</TT></STRONG>
    <DD>Use the previous solution values as a warm-start heuristics for
    the MIP search.  This only has an effect for certain solvers (e.g.
    Gurobi), if there are integrality constraints, and if there is a 
    previous solution available.  Possible values are <TT>none</TT>
    (no mipstart values, the default), <TT>all</TT> (use previous
    solution for all variables), or <TT>integers</TT> (use previous
    solution for all variables that are now constrained to be integral).  

<P>
    <DT><STRONG><TT>cache_iis(YesNo)</TT></STRONG>
     <DD>Specify if an IIS should be computed immediately for an infeasible problem
     (if supported by the external solver), and store it so that it can bee retrieved by
     eplex_get_iis/4 or lp_get_iis/5 (called from within a user-defined infeasible 
     handler). This will be done before the problem can be modified and make the computing 
     of the IIS impossible. The IIS will only be available before the problem is solved
     again, and also before the infeasible solve is backtracked. This option has no effect  
     if the external solver does not support the computation of an IIS. Note that if this 
     option is set, eplex will always ask for an IIS to computed for an infeasible problem, 
     even if it is immediately backtracked by the infeasible handler failing, and that the 
     option is only needed if the problem instance in the external solver is modified 
     before eplex_get_iis/4 or lp_get_iis/5 is called. 
     YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.

<P>
<DT><STRONG><TT>demon_tolerance(RealTol, IntTol)</TT></STRONG>
    <DD>Specify how far outside a variable's range an lp-solution
    can fall before lp_demon_setup/5 re-triggers.
    <TT>RealTol</TT> and <TT>IntTol</TT> are floats and default to
    0.00001 and 0.5 respectively.

<DT><STRONG><TT>integers(+ListOfVars)</TT></STRONG>
    <DD>Consider the specified variables to be integers (whether or not
    they have been declared such).
    This option will instruct the external solver to use its own MIP solver
    (ie. branch-and-bound search happens within the external solver).
<P>
<DT><STRONG><TT>sos1(VarList)</TT></STRONG>
    <DD><TT>VarList</TT> is a list of variables which the solver should
    treat as variables of a type 1 special ordered set (SOS), i.e. at most
    one of the variables in the set can be non-zero. This can occur multiple
    times, for different sets of variables.

<P>
<DT><STRONG><TT>sos2(VarList)</TT></STRONG>
    <DD><TT>VarList</TT> is a list of variables which the solver should
    treat as variables of a type 2 special ordered set (SOS), i.e. at most
    two of the variables in the set can be non-zero. This can occur multiple
    times, for different sets of variables.

<P>
<DT><STRONG><TT>presolve(+YesNo)</TT></STRONG>
    <DD>Specify if the external solver should perform presolve for this
    problem. With presolving, the external solver will transform the
    problem before solving it. This can lead to significantly faster times
    to find solutions. However, as the problem has been transformed, some
    external solvers have restriction on accessing or changing the problem
    state. In addition, if the solver is repeatedly called because the
    problem is frequently modified, then presolve may not be an advantage.
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    determined by the global setting of <TT>presolve</TT>, which can be
    changed via <TT>lp_set/2</TT>. The initial default is <TT>yes</TT>.
    Note that the presolve setting cannot be changed for a problem once it
    is set.  If the external solver supports per-problem optimizer_params, 
    their global defaults will be used for presolve(yes).
<P>
<DT><STRONG><TT>timeout(+TimeOut)</TT></STRONG>
    <DD>Set the external solver to time-out after <TT>TimeOut</TT> seconds.
    <TT>TimeOut</TT> is a positive number. The solver will abort (in either
    the abort or suboptimal state, depending on if a suboptimal solution
    was found) if the optimal solution was not found within the time
    limit. This should be used instead of setting the solver-specific
    parameter for time-out directly. In cases where the solver expects an
    integer for the time-out interval, the time given is rounded up to the
    next integer value. The timeout is set by setting the external solver's
    timeout settings, and the exact behaviour may be solver dependent.
<P>
<DT><STRONG><TT>suboptimal_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver returned a suboptimal solution (because the problem
    was aborted). Goal would be run in place of raising the default 
    <TT>eplex_suboptimal</TT> event.
<P>
<DT><STRONG><TT>unbounded_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    problem is unbounded. Goal would be run in place of raising the  
    default <TT>eplex_unbounded</TT> event.
<DT><STRONG><TT>infeasible_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver found that the problem is infeasible. Goal would be run
    in place of raising the default <TT>eplex_infeasible</TT> event.  Note
    that the default and logically correct behaviour is to fail, this
    handler is provided to allow the user to analyse the cause of the
    infeasibility. It is recommended that the handler should also fail
    after performing the analysis.
<P>
<P>
<DT><STRONG><TT>unknown_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver was not able to determine if the problem is unbounded
    or infeasible. Goal would be run in place of raising the default 
    <TT>eplex_unknown</TT> event.
<P>
<DT><STRONG><TT>abort_handler(+Goal)</TT></STRONG>
    <DD>Specifies a user defined goal Goal to handle the case when the
    external solver aborted without finding any solution. Goal would be 
    run in place of raising the default <TT>eplex_abort</TT> event.
<P>
<DT><STRONG><TT>use_var_names(+YesNo)</TT></STRONG>
    <DD>Specify if variable names (set using <TT>set_var_name/2</TT> of the 
    var_name library) should be passed to the external solver. If a 
    particular variable does not have a name when it is first passed to the
    external solver, a default name determined by the solver would be used.
    Note that for XPRESS-MP, there is a limit on the length of the name,
    which can be changed between 8 and 64 in steps of 8 with the 
    parameter <TT>mpsnamelength (XPRS_MPSNAMELENGTH)</TT>. Variable 
    names longer than this limit are truncated to the limit. 
    YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default is 
    <TT>no</TT>.
<P>
    <DT><STRONG><TT>mip_use_copy(YesNo)</TT></STRONG>
        <DD>Some external solvers do not allow a MIP problem to be modified
        once the MIP search has been started. Eplex works around this
        problem by making a copy of the problem and solving that, so that
        the original problem can still be modified. This can be turned off to
        avoid the overhead of making this copy, in which case the MIP
        problem cannot be modified. This option is used only when solving a
        MIP problem, and the external solver does not allow a MIP problem to
        be modified; otherwise it is ignored.
        YesNo is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default 
        is <TT>yes</TT> so that the problem can be modified.
<P>
    <DT><STRONG><TT>write_before_solve(Format,File)</TT></STRONG>
        <DD>This option is most useful for debugging purposes. If given,
        Eplex will ask the external solver to dump the problem each time
        the problem is solved. This allows the problem in an
        <TT>eplex_probe/2</TT> or <TT>lp_probe/3</TT> to be dumped. As in
        <TT>lp_write/3</TT>, <TT>Format</TT> is the format of the dumped
        file and <TT>File</TT> is its name. Note that the problem is dumped
        each time the external solver is invoked if the problem has cutpool
        constraints, where there may be multiple invocations of the solver
        per solver call. 
        The default without this option is that the problem would not be 
        dumped.


    <DT><STRONG><TT>post_equality_when_unified(+YesNo)</TT></STRONG>
        <DD>This option determines if an equality constraint between two
        variables will be posted to the solver when these variables are
        unified. Setting <TT>YesNo</TT> to no means that the constraint 
        will <EM>not</EM> be posted. Note that this can lead to the
        solver's problem becoming inconsistent with ECLiPSe's. 
<P>
<DT><STRONG><TT>sync_bounds(+YesNo)</TT></STRONG>
    <DD>This option determines if the bounds of the problems variables are
    synchronised with other solvers (i.e. the generic bounds are obtained
    with get_var_bounds/3 and then passed to the external solver) before
    the external solver is invoked. This was always done for previous
    non-standalone version of eplex. For standalone eplex, as the bounds
    are communicated directly to the external solver, the synchronisation
    of variable bounds is not needed unless the user is using eplex
    co-operatively with other solvers (e.g. ic). Even in such cases, it may
    be more efficient to communicate these bounds changes by explicitly
    programming it, especially if the problem has many variables and bounds
    changes happen only to a few of the variables. Setting <TT>YesNo</TT>
    to yes should increase compatibility with previous code (but note that
    previous eplex obtained the bounds from a specific bounds keeper like
    ic rather than the generic bounds).  YesNo is one of the atoms
    <TT>yes</TT> or <TT>no</TT>, the default is <TT>no</TT>.

</DL>
")
]).

:- comment(lp_add/3, [
amode:     lp_add(+,+,+),
args:      ["Handle":      "Handle to a solver state",
            "NewNormCons": "List of new normalised constraints",
            "NewIntegers": "List of variables to be considered as integers"
           ],
see_also:  [lp_setup/4],
resat:     no,
summary:   "Add new constraints to a solver state Handle.",
desc:      html("\
<P>
  Add new normalised constraints (with possibly new variables) to a solver.
  This is a lower level predicate called by lp_add_constraints/3 and when
  constraints are collected from the constraints pool by the solver. The
  constraints are not simplified and must already be normalised, so
  constraints which would otherwise be simplified away by the higher level
  predicates can be added to the external solver. The constraints will be
  removed on backtracking. Note that the solver will not be invoked directly.
</P><P>
  Note that variables in NewIntegers can be any problem variables. In
  previous versions of ECLiPSe, there was a restriction that the variables
  be new problem variables; this restriction has been removed.
</P>
")
]).

:- comment(eplex_solve/1, [
template:  ["eplex_solve(-Cost)","EplexInstance:eplex_solve(-Cost)"],
args:      [
            "Cost":   "Value of returned solution"
           ],
summary:   "Explicitly invoke the external solver associated with EplexInstance.",
fail_if:   "External solver was unable to find a solution (default behaviour)",
see_also:  [eplex_solver_setup/4, eplex_solver_setup/1, eplex_var_get/3, eplex_get/2,
            ($=)/2,($=<)/2,($>=)/2,(=:=)/2, (>=)/2, (=<)/2, 
            ($::)/2, (::)/2,integers/1, reals/1
           ],
desc:      html("\
<P>
   Explicitly triggers the solver associated with the eplex instance 
   EplexInstance. If the solver was set up without any trigger conditions,
   then it needs to be explicitly triggered via a call to this predicate.
   The objective value of the solve is unified with Cost, which should be a
   free variable.
</P><P>
   The external solver's LP, QP or MIP solver is applied to the
   problem represented by the eplex instance.  Precisely which method is used
   depends on the options given at set up.  eplex_solve/1 fails if
   there is no solution or succeeds if an optimal solution is found,
   returning the solution's cost in Cost. After a success, various
   solution and status information can be retrieved using eplex_get/2
   and eplex_var_get/3.
</P><P>
   Normally, the external solver is invoked once per call in this
   predicate, but if the problem contain cutpool constraints (see
   lp_add_cutpool_constraints/4), then the external solver may be invoked
   repeatedly to produce a solution which does not violate the cutpool
   constraints. 
</P><P>
   When a solver is triggered repeatedly, each invocation will
   automatically take into account the current variable bounds.  The set of
   constraints considered by the solver is the one given when the solver
   was created plus any new constraints that were posted in the
   meantime.
</P><P>
   The user can define their own handlers (per eplex instance/problem) to
   handle cases where there was some  error condition, or some limits were 
   exceeded during the solving of the problem. If no handler was defined,
   by default an event would be raised. These are:
<DL>
    <DT>suboptimal handler (eplex_suboptimal event)<DD>
    	This means that a solution was found but it may be suboptimal.
	The default behaviour is to print a warning and succeed.
    <DT>unbounded_handler (eplex_unbounded event)<DD>
	This means that the problem is unbounded. The default
	behaviour is to bind Cost to infinity (positive or negative
	depending on the optimisation direction), print a warning and
	succeed.  CAUTION: No solution values are computed when the
	problem is unbounded, so unless the problem was set up with
	the solution(no) option, an error will occur when trying to
	continue as if the optimisation had succeeded.
    <DT>infeasible_handler (eplex_infeasible event)<DD>
    	This means that the problem is infeasible. The default
	behaviour is to fail. Redefining this handler allows the 
        examination of the failed problem, e.g. obtaining an IIS for it.
    <DT>unknown_handler (eplex_unknown event)<DD>
    	This means that due to the solution method chosen, it is unknown
	whether the problem is unbounded or infeasible. The default
	behaviour is to print a warning and fail (even though this
	may be logically wrong!).
    <DT>abort_handler (eplex_abort event)<DD>
    	Some other error condition occurred during optimisation.
	The default behaviour is to print an error and abort.
</DL>
</P><P>
   Note that the events are raised for the low-level primitive lp_solve/2,
   which is called by eplex_solve/1.
</P>"),
    exceptions:[
        5: "EplexInstance does not a solver setup for it.",
	eplex_suboptimal : "Solution was found, but is possibly suboptimal",
	eplex_unbounded : "Problem is unbounded, no solution values",
	eplex_unknown : "Result is unknown (infeasible or unbounded)",
	eplex_abort : "External solver aborted for some reason",
        40: "Solver state had been previously destroyed."

        ]
]).


:- comment(lp_solve/2, [
amode:     lp_solve(+,-),
args:      ["Handle": "Handle to a solver state",
            "Cost":   "Value of returned solution"
           ],
summary:   "Explicitly invoke the external solver.",
fail_if:   "External solver was unable to find a solution.",
see_also:  [lp_setup/4, lp_add/3, lp_get/3, lp_var_get/4,
            lp_add_cutpool_constraints/4],
desc:      html("\
<P>
   A solver that was setup manually with lp_solve/2 needs to be 
   triggered explicitly using this predicate. 
</P><P>
   The external solver's LP, QP or MIP solver is applied to the
   problem represented by Handle.  Precisely which method is used
   depends on the options given to lp_setup/4.  lp_solve/2 fails if
   there is no solution or succeeds if an optimal solution is found,
   returning the solution's cost in Cost (unlike with lp_demon_setup/5,
   Cost gets instantiated to a number).  After a success, various
   solution and status information can be retrieved using lp_get/3
   and lp_var_get/4.
</P><P>
   Normally, the external solver is invoked once per call in this
   predicate, but if the problem contain cutpool constraints (see
   lp_add_cutpool_constraints/4), then the external solver may be invoked
   repeatedly to produce a solution which does not violate the cutpool
   constraints. 
</P><P>
   When a solver is triggered repeatedly, each invocation will
   automatically take into account the current variable bounds.  The set of
   constraints considered by the solver is the one given when the solver
   was created plus any new constraints that were added (lp_add/3) in the
   meantime.
</P><P>
   The user can define their own handlers (per eplex instance/problem) to
   handle cases where there was some  error condition, or some limits were 
   exceeded during the solving of the problem. If no handler was defined,
   by default an event would be raised. These are:
<DL>
    <DT>suboptimal handler (eplex_suboptimal event)<DD>
    	This means that a solution was found but it may be suboptimal.
	The default behaviour is to print a warning and succeed.
    <DT>unbounded_handler (eplex_unbounded event)<DD>
	This means that the problem is unbounded.  The default
	behaviour is to bind Cost to infinity (positive or negative
	depending on the optimisation direction), print a warning and
	succeed.  CAUTION: No solution values are computed when the
	problem is unbounded, so unless the problem was set up with
	the solution(no) option, an error will occur when trying to
	continue as if the optimisation had succeeded.
    <DT>infeasible_handler (eplex_infeasible event)<DD>
    	This means that the problem is infeasible. The default
	behaviour is to fail. Redefining this handler allows the 
        examination of the failed problem, e.g. obtaining an IIS for it.
    <DT>unknown_handler (eplex_unknown event)<DD>
    	This means that due to the solution method chosen, it is unknown
	whether the problem is unbounded or infeasible. The default
	behaviour is to print a warning and fail (even though this
	may be logically wrong!).
    <DT>abort_handler (eplex_abort event)<DD>
    	Some other error condition occurred during optimisation.
	The default behaviour is to print an error and abort.
</DL>
</P>"),
    exceptions:[
	eplex_suboptimal : "Solution was found, but is possibly suboptimal",
	eplex_unbounded : "Problem is unbounded, no solution values",
	eplex_unknown : "Result is unknown (infeasible or unbounded)",
	eplex_abort : "External solver aborted for some reason",
        40: "Handle not valid: solver state had been destroyed by cleanup"
   ]
]).

:- comment(eplex_probe/2, [
template:  ["eplex_probe(+Probes, -Cost)","EplexInstance:eplex_probe(+Probes, -Cost)"],
args:      [
            "Probes": "Temporary probe specification(s)",
            "Cost":      "Value of solution"
           ],
summary:   "Invoke EplexInstance's external solver, probing the problem"
           " temporarily modified by the probe specifications.",
see_also:  [eplex_solve/1, eplex_solver_setup/1, eplex_solver_setup/4,
            eplex_get/2, eplex_var_get/3, eplex_set/2, lp_add_constraints/4],
desc:      html("\
<P>
   Similar to eplex_solve/1, but the problem is first temporarily modified
   as specified in Probes. Probes is a list of one or more probe
   specifications that specifies how the problem is modified. The
   objective value of the modified problem is unified with Cost
   after the solve. Cost should be a free variable.
</P><P>
   After the call to this predicate, the problem is restored for the 
   external solver. However, the results from the probe (obtainable from
   eplex_var_get/3 and eplex_get/2) are retained.
</P><P>
   The following probe specifications are allowed:
<DL>
    <DT>min(Expr)/max(Expr)</DT><DD>
        Minimise/maximise the problem with the objective function Expr.
        Only existing problem variables can be given in Expr. Cannot be
        used in conjunction with objsense, objexpr or perturb_obj.</DD>
    <DT>objsense(Sense)</DT><DD>
        Sense is either min or max. Solves the problem with the original
        objective function, but with the sense given in Sense rather than
        that specified at setup. If only the sense of the objective is to 
        be changed, this changes the objective more efficiently than using
        min(Expr)/max(Expr). Cannot be used in conjunction with min/max.</DD>
    <DT>objexpr(Expr)</DT><DD>
        Optimise the problem with respect to the objective function Expr,
        without changing the optimisation direction specified at problem
        setup. Cannot be used in conjunction with min/max or perturb_obj.</DD>
    <DT>perturb_obj(ObjDeltas)</DT><DD>
        Perturb the existing linear objective coefficients as specified by
        ObjDeltas. ObjDeltas is a (possibly empty) list of specifications
        Var : Delta, where Var is a problem variable and Delta is the
        amount its existing objective coefficient should be changed by. For
        example, if the existing objective is 2*X + 3*Y + Z, 
        perturb_obj([X : 1, Y : -0.5]) will modify the objective to 
        3*X + 2.5*Y + Z for the probe. If a variable occurs more than once in
        ObjDeltas, the change to the objective coefficient is cumulative.
        Perturb_obj is not strictly needed, as its effect can be achieved
        by constructing the whole objective and then probing with that.
        However, this can be expensive if most of the objective stays 
        unchanged, with only a few of its coefficients changed. It is 
        recommended that perturb_obj probe be used in such cases. 
        Cannot be used in conjunction with objexpr or min/max.</DD>
    <DT>rhscoeffs(RhsCoeffs)</DT><DD>
        Alter the rhs coefficients of the matrix as specified by RhsCoeffs.
        RhsCoeffs is a (possibly empty) list of rhs coefficient
        specifications of the form Idx : Rhs, where Idx is the index of the
        expandable constraint obtained from using lp_add_constraints/4, and
        Rhs is the value for the right-hand side constant to use during the
        probe. Any constraints not specified in RhsCoeffs remain unchanged
        during the probe. If Idx occurs more than once in RhsCoeffs, the
        result is undefined.</DD>
    <DT>bounds(BoundsChanges)</DT><DD>
        Alter the bounds of problem variables as specified by
        BoundsChanges. BoundsChanges is a (possibly empty) list of bounds
        changes of the form Var $:: Lo..Hi, where Var is a problem variable
        and Lo and Hi are the new lower and upper bounds to use for the
        variable during the probe. Unlike normal bound changes, the bounds
        can be widened as well narrowed, although failure will still occur
        if Hi &lt; Lo. Variables not specified in BoundsChanges retain their
        bounds during the probe. If Var occurs more than once in 
        BoundsChanges, the result is undefined.</DD>
    <DT>fixed</DT><DD>
        The problem is solved as an LP problem by `fixing' the integer
        variables to their optimal MIP solution values. If there is an
        existing MIP solution for the problem, eplex will try to use that;
        otherwise, a MIP solution is obtained first.
        `Fixed' probing is useful for providing reduced costs for
        MIP problems. Note that reduced costs for the variables will only be
        available if the reduced_cost option for the problem is set to yes
        (this can be done either at problem setup, or by using
        eplex_set/2). For a problem without integer variables, the original
        problem is solved without change.  Note that this probe
        specification is not available if the mip_use_copy option is set to
        no for external solvers that cannot modify a MIP problem.  Cannot
        be used in conjunction with `relaxed'.</DD>
    <DT>relaxed</DT><DD>
        The problem is solved as an LP problem by relaxing all the integer
        constraints.  For a problem with integer variables, this should be
        equivalent to the initial relaxation solved at the start of the MIP
        search. Depending on the solver, the discreteness of the integer
        variables may still be taken into account during presolve. To
        ensure that the problem solved is exactly the LP problem without
        any integer constraints, presolve should be off. For a problem
        without integer variables, the original problem is solved without
        change. Note that this probe specification is not available if the
        mip_use_copy option is set to no for external solvers that cannot
        modify a MIP problem. Cannot be used in conjunction with `fixed'.</DD>

</DL>
</P>"),
fail_if:   "External solver was unable to find a solution, or bounds probe"
           " used and the specified interval for some variable is empty",
exceptions:[
        5: "EplexInstance does not have a solver setup for it.",
	eplex_suboptimal : "Solution was found, but is possibly suboptimal",
	eplex_unbounded : "Problem is unbounded, no solution values",
	eplex_unknown : "Result is unknown (infeasible or unbounded)",
	eplex_abort : "External solver aborted for some reason",
        abort : "Incorrect specification for the probe(s)",
        6: "mip_use_copy option was set to no (solver dependent) for probes"
           " that require yes.",
        40: "Solver state had been previously destroyed."
        ]
                          
]).

:- comment(lp_probe/3, [
amode:     lp_probe(+,+,-),
args:      ["Handle":    "Handle to existing solver state",
            "Probes": "Temporary probe specification(s)",
            "Cost":      "Value of solution"
           ],
summary:   "Invoke external solver, probing the problem"
           " temporarily modified by the probe specifications.",
see_also:  [lp_solve/2, lp_setup/4, lp_var_get/4, lp_get/3, lp_set/3, lp_add_constraints/4],
desc:      html("\
<P>
   Similar to lp_solve/2, but the problem is first temporarily modified
   as specified in Probes. Probes is a list of one or more probe
   specifications that specifies how the problem is modified. After the call
   to this predicate, the problem is restored for the external
   solver. However, the results from the probe (obtainable from
   lp_var_get/4 and lp_get/3) are retained.
</P><P>
   The following probe specifications are allowed:
<DL>
    <DT>min(Expr)/max(Expr)</DT><DD>
        Minimise/maximise the problem with the objective function Expr.
        Only existing problem variables can be given in Expr. Cannot be
        used in conjunction with objsense, objexpr or perturb_obj.</DD>
    <DT>objsense(Sense)</DT><DD>
        Sense is either min or max. Solves the problem with the original
        objective function, but with the sense given in Sense rather than
        that specified at setup. If only the sense of the objective is to 
        be changed, this changes the objective more efficiently than using
        min(Expr)/max(Expr). Cannot be used in conjunction with min/max.</DD>
    <DT>objexpr(Expr)</DT><DD>
        Optimise the problem with respect to the objective function Expr,
        without changing the optimisation direction specified at problem
        setup. Cannot be used in conjunction with min/max or perturb_obj.</DD>
    <DT>perturb_obj(ObjDeltas)</DT><DD>
        Perturb the existing linear objective coefficients as specified by
        ObjDeltas. ObjDeltas is a (possibly empty) list of specifications
        Var : Delta, where Var is a problem variable and Delta is the
        amount its existing objective coefficient should be changed by. For
        example, if the existing objective is 2*X + 3*Y + Z, 
        perturb_obj([X : 1, Y : -0.5]) will modify the objective to 
        3*X + 2.5*Y + Z for the probe. If a variable occurs more than once in
        ObjDeltas, the change to the objective coefficient is cumulative.
        Perturb_obj is not strictly needed, as its effect can be achieved
        by constructing the whole objective and then probing with that.
        However, this can be expensive if most of the objective stays 
        unchanged, with only a few of its coefficients changed. It is 
        recommended that perturb_obj probe be used in such cases. 
        Cannot be used in conjunction with objexpr or min/max.</DD>
    <DT>rhscoeffs(RhsCoeffs)</DT><DD>
        Alter the rhs coefficients of the matrix as specified by RhsCoeffs.
        RhsCoeffs is a (possibly empty) list of rhs coefficient
        specifications of the form Idx : Rhs, where Idx is the index of the
        expandable constraint obtained from using lp_add_constraints/4, and
        Rhs is the value for the right-hand side constant to use during the
        probe. Any constraints not specified in RhsCoeffs remain unchanged
        during the probe. If Idx occurs more than once in RhsCoeffs, the
        result is undefined.</DD>
    <DT>bounds(BoundsChanges)</DT><DD>
        Alter the bounds of problem variables as specified by
        BoundsChanges. BoundsChanges is a (possibly empty) list of bounds
        changes of the form Var $:: Lo..Hi, where Var is a problem variable
        and Lo and Hi are the new lower and upper bounds to use for the
        variable during the probe. Unlike normal bound changes, the bounds
        can be widened as well narrowed, although failure will still occur
        if Hi &lt; Lo. Variables not specified in BoundsChanges retain their
        bounds during the probe. If Var occurs more than once in 
        BoundsChanges, the result is undefined.</DD>
    <DT>fixed</DT><DD>
        The problem is solved as an LP problem by `fixing' the integer
        variables to their optimal MIP solution values. If there is an
        existing MIP solution for the problem, eplex will try to use that;
        otherwise, a MIP solution is obtained first.
        `Fixed' probing is useful for providing reduced costs for
        MIP problems. Note that reduced costs for the variables will only be
        available if the reduced_cost option for the problem is set to yes
        (this can be done either at problem setup, or by using
        eplex_set/2). For a problem without integer variables, the original
        problem is solved without change.  Note that this probe
        specification is not available if the mip_use_copy option is set to
        no for external solvers that cannot modify a MIP problem.  Cannot
        be used in conjunction with `relaxed'.</DD>
    <DT>relaxed</DT><DD>
        The problem is solved as an LP problem by relaxing all the integer
        constraints.  For a problem with integer variables, this should be
        equivalent to the initial relaxation solved at the start of the MIP
        search. Depending on the solver, the discreteness of the integer
        variables may still be taken into account during presolve. To
        ensure that the problem solved is exactly the LP problem without
        any integer constraints, presolve should be off. For a problem
        without integer variables, the original problem is solved without
        change. Note that this probe specification is not available if the
        mip_use_copy option is set to no for external solvers that cannot
        modify a MIP problem. Cannot be used in conjunction with `fixed'.</DD>

</DL>
</P>"),
fail_if:   "External solver was unable to find a solution, or bounds probe"
           " used and the specified interval for some variable is empty",
exceptions:[
        5: "EplexInstance does not have a solver setup for it.",
	eplex_suboptimal : "Solution was found, but is possibly suboptimal",
	eplex_unbounded : "Problem is unbounded, no solution values",
	eplex_unknown : "Result is unknown (infeasible or unbounded)",
	eplex_abort : "External solver aborted for some reason",
        abort : "Incorrect specification for the probe(s)",
        6: "mip_use_copy option was set to no (solver dependent) for probes"
           " that require yes.",
        40: "Handle not valid: solver state had been destroyed by cleanup"
           ]
]).

:- comment(eplex_cleanup/0, [
template:  ["eplex_cleanup","EplexInstance:eplex_cleanup"],
summary:   "Destroy the external solver associated with EplexInstance.",
desc:      html("\
<P>
    Destroy the specified solver, free all memory, etc.  Note that ECLiPSe
    will normally do the cleanup automatically, for instance when execution
    fails across the solver setup. The solver is disassociated with 
    EplexInstance, and any outstanding constraints not yet collected by the
    solver are removed, with a warning to the user. In effect, EplexInstance
    is reinitialised and can now be associated with a new solver without
    backtracking. Note that this last behaviour is unlike lp_cleanup/1, which
    also destroy the solver and disassociate it from the eplex instance (if 
    any), but does not remove any outstanding constraints.
</P><P>
    This predicate should be used with caution as the information from the
    solver state will no longer be available. In particular, if the program
    backtracks to a point between the problem set up and clean up,
    accessing the solver state directly or indirectly (e.g. unifying a
    problem variable) will result in a stale handle error. 

</P>")
]).

:- comment(lp_cleanup/1, [
amode:     lp_cleanup(+),
args:      ["Handle":  "Handle to a solver state"],
summary:   "Destroy the specified solver Handle and clean up.",
desc:      html("\
<P>
    Destroy the specified solver, free all memory, etc.  Note that ECLiPSe
    will normally do the cleanup automatically, for instance when execution
    fails across the solver setup, or when a solver handle gets garbage
    collected.
</P><P>
    This predicate should be used with caution as the information from the
    solver state will no longer be available. In particular, if the program
    backtracks to a point between the problem set up and clean up,
    accessing the solver state directly or indirectly (e.g. unifying a
    problem variable) will result in a stale handle error. 
</P>")
]).

:- comment(lp_read/3, [
amode:     lp_read(+,++,-),
args:      ["File":   "File name",
            "Format": "lp or mps",
            "Handle": "Returns a handle to the new solver state"
           ],
see_also:  [lp_write/3, lp_setup/4, eplex_read/2, eplex_write/2, lp_probe/3],
summary:   "Read a problem from a file and setup a solver for it.",
desc:      html("
   Read a problem from a file and setup a solver for it.  Format is
   lp or mps. The result is a handle similar to the one obtained by 
   lp_setup/4. Note that minimisation of the objective is assumed 
   for mps format, as the sense of the objective is not included in 
   the mps format. Minimisation is also assumed for external solvers
   that ignore the optimisation direction of the lp file (e.g. Xpress).
   Note also that although quadratic problems can be read in correctly if
   supported by the external solver, the quadratic objective coefficients
   are not extracted from the problem by eplex. These coefficients are
   used to set/reset the objective when a problem is probed with a
   different objective in eplex_probe/2 or lp_probe/3, so the objective
   will not be changed or restored correctly in this case.
")
]).

:- comment(eplex_read/2, [
template: ["eplex_read(++Format,++File)","EplexInstance:eplex_read(++Format,++File)"],
args:      ["Format": "lp or mps",
	    "File":   "File name"
           ],
see_also:  [eplex_write/2, lp_write/3, lp_setup/4, eplex_probe/2],
summary:   "Read a problem from a file into eplex instance EplexInstance.",
exceptions: [5: "EplexInstance already has a solver setup for it."],
desc:      html("
   Read a problem from a file and setup a solver for it in EplexInstance.
   A solver should not already be setup for EplexInstance. Format is lp or
   mps. Note that minimisation of the objective is assumed for mps format,
   as the sense of the objective is not included in the mps format.
   Minimisation is also assumed for external solvers that ignore the
   optimisation direction of the lp file (e.g. Xpress).  Note also that
   although quadratic problems can be read in correctly if supported by the
   external solver, the quadratic objective coefficients are not extracted
   from the problem by eplex. These coefficients are used to set/reset the
   objective when a problem is probed with a different objective in
   eplex_probe/2 or lp_probe/3, so the objective will not be changed or
   restored correctly in this case.  
")
]).

:- comment(lp_write/3, [
amode:     lp_write(+,++,+),
args:      ["Handle": "Handle to an existing solver state",
            "Format": "lp or mps",
	    "File":   "File name"
           ],
see_also:  [lp_read/3, lp_setup/4],
summary:   "Write a solver problem to a file.",
desc:      html("
   Write the problem which corresponds to Handle to a file. The set of
   supported formats depends on the actual external solver which is used.
   All solvers support the mps format. Some solvers on some operating
   system platforms may change or append a suffix to the filename.  Note
   that the mps format does not specify the sense (min or max) of the
   objective function. Note also that any active cutpool constraints in the
   problem will be included in the dump, even though they could be left out
   of the problem that the external solver actually solves.")
]).

:- comment(eplex_write/2, [
template: ["eplex_write(++Format,++File)","EplexInstance:eplex_write(++Format,++File)"],
args:      ["Format": "lp or mps",
	    "File":   "File name"
           ],
see_also:  [eplex_read/2, lp_write/3, lp_setup/4],
summary:   "Write the problem in the solver for eplex instance"
"  EplexInstance to a file.",
exceptions: [5: "EplexInstance does not a solver setup for it.",
             40: "Solver state had been previously destroyed."
             ],
desc:      html("
   Write the problem in the solver for EplexInstance to a file. The set of
   supported formats depends on the actual external solver which is used.
   All solvers support the mps format. Some solvers on some operating
   system platforms may change or append a suffix to the filename. 
   Note that the mps format does not specify the sense (min or max) of the
   objective function. Minimisation is also assumed for external solvers
   that ignore the optimisation direction of the lp file (e.g. Xpress).
   Note also that any active cutpool constraints in the problem will be 
   included in the dump, even though they could be left out of the problem
   that the external solver actually solves.
")
]).

:- comment(eplex_get/2, [
template:  ["eplex_get(++ParamName, -Value)","EplexInstance:eplex_get(++ParamName, -Value)"],
args:      [
            "ParamName": "Name of parameter (atom)",
            "Value":     "Returned value for ParamName"
           ],
summary:   "Retrieve information about solver state and results for eplex instance EplexInstance.", 
exceptions: [5: "EplexInstance does not a solver setup for it.",
             40: "Solver state had been previously destroyed."
             ],
see_also:  [eplex_solver_setup/4, eplex_set/2, lp_set/3, lp_get/3, lp_add_constraints/4],
desc:      html("\
<P>
   Retrieve information about solver state and the logically most recent
   results for the eplex instance <TT>EplexInstance</TT>. <TT>ParamName</TT> 
   is the same as that for lp_get/3, which retrieves the same information 
   via the solver state.
</P>

<DL>
<P>  
     <DT><STRONG><TT>vars</TT></STRONG>
     <DD>Returns a term ''(X1,...,Xn) whose arity is the number of
         variables involved in the solver's constraint set, and whose
         arguments are these variables.

<P>
     <DT><STRONG><TT>ints</TT></STRONG>
     <DD>Returns a list [Xi1,...,Xik] which is the subset of the problem
         variables that the solver considers to be integers.

<P>
     <DT><STRONG><TT>constraints_norm</TT></STRONG>
     <DD>Returns a list of the problem constraints (excluding any cutpool
         constraints) in normalised form.  They may be simplified with
         respect to the originals that were passed to the problem.

<P>
     <DT><STRONG><TT>constraints</TT></STRONG>
     <DD>Returns a list of the problem constraints (excluding any cutppol
         constraints) in denormalised (readable) form.  They may be
         simplified with respect to the originals that were passed to the
         problem.

<P>
     <DT><STRONG><TT>objective</TT></STRONG>
     <DD>Returns a term min(E) or max(E), representing objective function
         and optimisation direction. E is a linear expression: any
         quadratic components will not be retrieved.

<P>
     <DT><STRONG><TT>num_cols</TT></STRONG>
     <DD>Returns the number of columns (i.e. variables) in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_rows</TT></STRONG>
     <DD>Returns the number of rows (i.e. constraints) in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_nonzeros</TT></STRONG>
     <DD>Returns the number of non-zero coefficients in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_ints</TT></STRONG>
     <DD>Returns the number of columns (i.e. variables) constrained to be
     integers in the matrix of the solver state.

<P>
     <DT><STRONG><TT>num_quads</TT></STRONG>
     <DD>Returns the number of non-zero coefficients in the quadratic
     coefficient matrix (Q-matrix) of the solver state.

<P>
     <DT><STRONG><TT>method</TT></STRONG>
     <DD>Returns the method that is specified to solve the problem. If an
     auxiliary  method can be given for the method, and this auxiliary
     method is not <TT>default</TT>, the method will be returned as 
     Method(Aux), e.g. <TT>barrier(none)</TT>. The method will be 
     <TT>default</TT> unless otherwise specified by the the user (at setup or 
     via eplex_setup/2 or lp_setup/3).  In that case, the external solver's
     defaults and any settings done via optimizer_param(_) will be used.

<P>
     <DT><STRONG><TT>node_method</TT></STRONG>
     <DD>Applicable to MIP problems only. Returns the method that is
     specified to solve the problem at the nodes of the branch-and-bound
     tree. If an auxiliary method can be given for the method, and this
     auxiliary method is not <TT>default</TT>, the method will be returned
     as Method(Aux), e.g. <TT>barrier(none)</TT>. The method will be
     <TT>default</TT> unless otherwise specified by the the user (at setup
     or via eplex_setup/2 or lp_setup/3).  In that case, the external solver's
     defaults and any settings done via optimizer_param(_) will be used.

<P>
     <DT><STRONG><TT>status</TT></STRONG>
     <DD>Status that was returned by the most recent invocation of the 
         external solver.

<P>
     <DT><STRONG><TT>cost</TT></STRONG>
     <DD>Objective value (i.e. cost/profit) of the current solution.
         Fails if no solution has been computed yet.

<P>
     <DT><STRONG><TT>best_bound</TT></STRONG>
     <DD>The best bound (for minimisation, the lower bound) on the optimal 
         objective value for the current problem. Together with the
         worst_bound, this specifies the range for the optimal objective
         value. Note that a non-empty range does not mean that the problem
         is feasible unless an objective value (cost) is also
         available. The best_bound is the same as the current objective
         value if the problem has been solved to optimality. It can be
         better than the objective value either because a) (for MIP
         problems only) the problem has been optimised to within the
         mipgap tolerance, or b) the problem was not solved to optimality,
         i.e. it was aborted before the optimal solution was found.
<P>
     <DT><STRONG><TT>worst_bound</TT></STRONG>
     <DD>The worst bound (for minimisation, the upper bound) on the optimal 
         objective value for the current problem. Together with the
         best_bound, this specifies the range for the optimal objective
         value. Note that a non-empty range does not mean that the problem
         is feasible unless an objective value (cost) is also
         available. The worst_bound is the same as the current objective
         value if a solution has been computed for the problem, whether
         the problem was solved to optimality or not. Depending on the 
         problem type and method used to solve the problem, a worst bound
         can be returned for a problem even if the solving of the problem
         was aborted before a solution was obtained. 
<P>
     <DT><STRONG><TT>typed_solution</TT></STRONG>
     <DD>Returns a term ''(X1,...,Xn) whose arguments are the properly
         typed (integer or float) solution values for the corresponding
         problem variables (<TT>vars</TT>).  The floating point solutions
         are the same as returned by <TT>solution</TT>, the integers are
         obtained by rounding the corresponding floating-point solution to
         the nearest integer.  To instantiate the problem variables to
         their solutions, unify this term with the corresponding term
         containing the variables. Note that this unification could fail
         if two problem variables Xa and Xb were unified after the solution 
         was lasted computed, as the solutions values in the Xa and Xb
         positions could be different, even though they are now represented 
         by one variable.
         Fails if no solution has been computed yet.

<PRE>
    instantiate_solution(Handle) :-
        lp_get(Handle, vars, Vars),
        lp_get(Handle, typed_solution, Values),
        Vars = Values.
</PRE>

<P>
    <DT><STRONG><TT>slack</TT></STRONG>
    <DD>Returns a list of floating-point values representing the constraint
        slacks in the logically last solve. The problem consists of normal
        constraints followed by any added cutpool constraints, and the
        order corresponds to the list order in <TT>constraints</TT> for
        normal constraints, and to cutpool_info(last_added,Info) for the
        cutpool constraints.  Fails if no solution has been computed yet.

<P>
    <DT><STRONG><TT>slack(Indexes)</TT></STRONG>
    <DD>Returns a list of floating-point values representing the slack
        values for the constraints represented by Indexes. Indexes are a
        list of constraint indices (as returned by lp_add_constraints/4 for
        normal constraints, and lp_add_cutpool_constraints/4 for cutpool
        constraints), and the order of the returned list corresponds to the
        order in <TT>Indexes</TT>.  Fails if no slack value has been
        computed yet for any of the constraints in Indexes -- note that 
        values are only computed for cutpool constraints if they were added
        to the problem.

<P>
    <DT><STRONG><TT>dual_solution</TT></STRONG>
    <DD>Returns a list of floating-point values representing the dual
        solutions in the logically last solve. The problem consists of
        normal constraints followed by any added cutpool constraints, and
        the order corresponds to the list order in <TT>constraints</TT> for
        normal constraints, and to cutpool_info(last_added,Info) for the
        cutpool constraints.  Fails if no solution has been computed yet.

<P>
    <DT><STRONG><TT>dual_solution(Indexes)</TT></STRONG>
    <DD>Returns a list of floating-point values representing the dual
        solutions for the constraints represented by Indexes. Indexes are a
        list of constraint indices (as returned by lp_add_constraints/4
        normal constraints, and lp_add_cutpool_constraints/4 for cutpool
        constraints), and the order of the returned list corresponds to the
        order in <TT>Indexes</TT>.  Fails if no dual solution has been
        computed yet for any of the constraints in Indexes -- note that
        values are only computed for cutpool constraints if they were added
        to the problem.

<P>
    <DT><STRONG><TT>constraints(Indexes)</TT></STRONG>
    <DD>Returns a list of problem constraints as specified by
        Indexes in denormalised form. The constraints can be either 
        normal constraints or cutpool constraints.
<P>
    <DT><STRONG><TT>constraints_norm(Indexes)</TT></STRONG>
    <DD>Returns a list of problem constraints as specified by
        Indexes in normalised form. The constraints can be either 
        normal constraints or cutpool constraints.
<P>
    <DT><STRONG><TT>cutpool_info(Select,Info)</TT></STRONG>
    <DD>Returns the information specified by Info for the cutpool constraints
        specified by Select. The returned information is either
        i) a pair of lists Indexes-Values where Indexes is the index for
        the constraint in the corresponding position of Values, or 
        ii) a list of Indexes if the information requested is index. Info 
        can be:
<DL>
           <DT><STRONG><TT>index</TT></STRONG>
               <DD>the indexes for the selected constraints. A list
                   of Indexes are returned.
           <DT><STRONG><TT>active</TT></STRONG>
               <DD>the current active status of the selected constraints. 
                   The status is either 0 (not active) or 1 (active).
                   A pair of lists Indexes-ActiveStatus is returned.
           <DT><STRONG><TT>add_initially</TT></STRONG>
               <DD>the current add_initially status of the selected 
                   constraints. The status is either 0 (not add) or 1 (add).
                   Note that although inactive constraints have a
                   add_initially status, they will not be added to a problem.
                   A pair of lists Indexes-AddInitially is returned.
           <DT><STRONG><TT>binding_state</TT></STRONG>
               <DD>the binding state for the selected constraints in the
                   logically last solve for the problem. A pair of lists
                   Indexes-BindingStates is returned. To get this
                   information, the slack values must be available from the
                   last solve.
                   The state can be:
                   a) binding - the constraint was satisfied and binding, i.e.
                      it is within tolerance of its RHS value in the
                      normalised form. 
                   b) satisfied - the constraint was satisfied but not
                      binding. 
                   c) inactive - the constraint was inactive.
           <DT><STRONG><TT>constraints_norm</TT></STRONG>
               <DD>the normalised form of the constraints for the selected 
                   constraints is returned in a pair of lists 
                   Indexes-Constraints.
           <DT><STRONG><TT>constraints</TT></STRONG>
               <DD>the denormalised form of the constraints for the selected 
                   constraints is returned in a pair of lists 
                   Indexes-Constraints.
</DL>

    The constraints are selected by Select, which can be:
<DL>
           <DT><STRONG><TT>cstr(Idx)</TT></STRONG>
               <DD>The cutpool constraint as specified by <TT>Idx</TT>. 
           <DT><STRONG><TT>group(Name)</TT></STRONG>
               <DD>The cutpool constraints in the group <TT>Name</TT>. Both
                   active and non-active constraints are returned. Note that
                   the name of the default group is the atom nil (<TT>[]</TT>).
           <DT><STRONG><TT>last_added</TT></STRONG>
               <DD>The cutpool constraints that were added to the
                   problem in the logically previous solve of the problem. 
                   The constraints were either added initially, or were
                   added because they were violated in an intermediate
                   invocation. 
           <DT><STRONG><TT>last_notadded</TT></STRONG>
               <DD>The cutpool constraints that were not added to the
                   problem in the logically previous solve of the problem,
                   i.e. they were not violated.
           <DT><STRONG><TT>last_inactive</TT></STRONG>
               <DD>The cutpool constraints that were inactive during the
                   logically last solve of the problem. This does not
                   include any constraints that were added since the last
                   solve. 
</DL>
<P>
    <DT><STRONG><TT>demon_tolerance</TT></STRONG>
    <DD>Returns a comma-separated pair <TT>(RealTol,IntTol)</TT> of
        floating-point values which specify how far outside a variable's
        range an lp-solution can fall before lp_demon_setup/5
        re-triggers. The tolerances differ for real (default 0.00001) and
        integer (default 0.5) variables.

<P>
    <DT><STRONG><TT>simplex_iterations</TT></STRONG>
    <DD>Returns the external solver's count of simplex iterations.

<P>
    <DT><STRONG><TT>node_count</TT></STRONG>
    <DD>Returns the external MIP solver's node count. Note that this may 
        or may not include the initial root node.
<P>
    <DT><STRONG><TT>statistics</TT></STRONG>
    <DD>Returns a list of counter values <TT>[Successes, Failures,
        Aborts]</TT>, indicating how often lp_solve/2 was invoked on the
        Handle, and how many invocations succeeded, failed and aborted
        respectively.

<P>
    <DT><TT>timeout</TT></STRONG>
    <DD>Returns the time-out value for the solver state. This is the amount
        of CPU time in seconds that the external solver will be allow to
        spend solving the problem before timing out. The value is 0 if 
        no time-out has been set.
<P>
     <DT><STRONG><TT>optimizer_param(Param)</TT></STRONG>
     <DD>Returns the value of the external solver's parameter Param
         for the problem represented by Handle. The external solver 
         has a number of parameters that affect the way they work, and 
         this queries their values. If Param is not a valid parameter for
         the solver, an out of range exception is raised. See below for 
         more details on the parameters.
<P>
    <DT><STRONG><TT>post_equality_when_unified</TT></STRONG>
    <DD>Returns the value (yes or no) if an equality constraint will be
        posted to a solver if two variables in the solver's problem are 
        unified. 
<P>
    <DT><STRONG><TT>pool</TT></STRONG>
    <DD>Returns the name of the eplex instance (if any) associated with 
        the solver state. Fails otherwise. Only useful if called with 
        lp_get/3.
<P>
    <DT><STRONG><TT>handle</TT></STRONG>
    <DD>Returns the solver state handle (if any) associated with the eplex 
        instance. Fails otherwise. Only useful if called with eplex_get/2.
<P>
</DL>
Note that reduced_cost, slack, dual_solution can only be retrieved
when previously requested in the option list of lp_setup/4 or with lp_set/3.
An out of range error will be raised otherwise.

<P>
For the external solver's control parameter specified by
optimizer_param(Param), Param must be an atom. The Value returned is
either an integer, float or atom, depending on the parameter. The parameter
is generally specific to a solver and version, and also, they may be
problem specific, or global, again depending on the solver version. In all
cases, the value returned by lp_get/3 is the current value for the parameter
for the problem Handle. Refer to the solver documentation for details on the 
parameters. The names of the parameters are derived from the names of the 
parameters in the external solver. For CPLEX, take the parameter name from 
the CPLEX manual (or cplex.h), remove the CPX_PARAM_ prefix and convert the 
rest to lower case, e.g.

<PRE>
        CPX_PARAM_NODELIM becomes nodelim. 
</PRE>
For XPRESS-MP (version 13 and newer), take the parameter name from the 
manual (or xpresso.h), remove the XPRS_ prefix (if present) and convert 
the rest to lower case, e.g.
<PRE>
	XPRS_MAXNODE becomes maxnode. 
</PRE>
<P>
For Gurobi, take the parameter name from the manual and convert it to
all lower case, e.g.
<PRE>
	IntFeasTol becomes intfeastol. 
</PRE>
<P>
For solvers used via the OSI, there are a few generic parameters supported 
via OSI, and depending on the actual solver, there may be some additional
solver-specific parameters. For the generic parameters, take the parameter
name, remove the Osi prefix and convert the rest to lower case, e.g.
<PRE>
        OsiPrimalTolerance becomes primaltolerance
</PRE>

<P>
    The following parameter names are additional aliases that work for
    several solvers:
<DL>
    <DT><TT>feasibility_tol</TT>
	<DD>CPX_PARAM_EPRHS (CPLEX) or XPRS_FEASTOL (XPRESS-MP) or
            OsiPrimalTolerance (OSI) or FeasibilityTol (GRB) - float
    <DT><TT>integrality</TT>
	<DD>CPX_PARAM_EPINT (CPLEX) or XPRS_MIPTOL (XPRESS-MP) or
            CbcIntegerTolerance (OSI,Cbc specific) or IntFeasTol (GRB) - float
    <DT><TT>iteration_limit</TT>
	<DD>CPX_PARAM_ITLIM (CPLEX) or XPRS_LPITERLIMIT (XPRESS-MP) or
            OsiMaxNumIteration (OSI) or IterationLimit (GRB) - integer
    <DT><TT>node_limit</TT>
	<DD>CPX_PARAM_NODELIM (CPLEX) or XPRS_MAXNODE (XPRESS-MP) or
            CbcMaxNumNode (OSI, Cbc specific) or NodeLimit (GRB)- integer
    <DT><TT>objdifference</TT>
	<DD>CPX_PARAM_OBJDIF (CPLEX) or XPRS_MIPADDCUTOFF (XPRESS-MP) or
            CbcCutoffIncrement (OSI, Cbc specific) - float
</DL>

            ")
]).

:- comment(lp_get/3, [
amode:     lp_get(+,++,-),
args:      ["Handle":    "Handle to an existing solver state",
            "ParamName": "Name of parameter (atom or structure)",
            "Value":     "Returned value for ParamName"
           ],
summary:   "Retrieve information about solver state and results for solver state Handle.", 
see_also:  [lp_setup/4, lp_set/3, eplex_get/2, lp_add_constraints/4],
desc:      html("\
<P>
   Retrieve information about solver state and results for the (logically) 
   most recent solved solver state represented by <TT>Handle</TT>. 
   <TT>ParamName</TT> is the same as that for eplex_get/2, which retrieves 
   the same information via the EplexInstance.
   It can  be one of:
</P>

<DL>
<P>  
     <DT><STRONG><TT>vars</TT></STRONG>
     <DD>Returns a term ''(X1,...,Xn) whose arity is the number of
         variables involved in the solver's constraint set, and whose
         arguments are these variables.

<P>
     <DT><STRONG><TT>ints</TT></STRONG>
     <DD>Returns a list [Xi1,...,Xik] which is the subset of the problem
         variables that the solver considers to be integers.

<P>
     <DT><STRONG><TT>constraints_norm</TT></STRONG>
     <DD>Returns a list of the problem constraints (excluding any cutpool
         constraints) in normalised form.  They may be simplified with
         respect to the originals that were passed to the problem.

<P>
     <DT><STRONG><TT>constraints</TT></STRONG>
     <DD>Returns a list of the problem constraints (excluding any cutpool
         constraints) in denormalised (readable) form.  They may be
         simplified with respect to the originals that were passed to the
         problem.

<P>
     <DT><STRONG><TT>objective</TT></STRONG>
     <DD>Returns a term min(E) or max(E), representing objective function
         and optimisation direction. E is a linear expression: any
         quadratic components will not be retrieved.

<P>
     <DT><STRONG><TT>num_cols</TT></STRONG>
     <DD>Returns the number of columns (i.e. variables) in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_rows</TT></STRONG>
     <DD>Returns the number of rows (i.e. constraints) in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_nonzeros</TT></STRONG>
     <DD>Returns the number of non-zero coefficients in the matrix of the
     solver state.

<P>
     <DT><STRONG><TT>num_ints</TT></STRONG>
     <DD>Returns the number of columns (i.e. variables) constrained to be
     integers in the matrix of the solver state.

<P>
     <DT><STRONG><TT>num_quads</TT></STRONG>
     <DD>Returns the number of non-zero coefficients in the quadratic
     coefficient matrix (Q-matrix) of the solver state.

<P>
     <DT><STRONG><TT>method</TT></STRONG>
     <DD>Returns the method that is specified to solve the problem. If an
     auxiliary  method can be given for the method, and this auxiliary
     method is not <TT>default</TT>, the method will be returned as 
     Method(Aux), e.g. <TT>barrier(none)</TT>. The method will be 
     <TT>default</TT> unless otherwise specified by the the user (at setup or 
     via eplex_setup/2 or lp_setup/3).  In that case, the external solver's
     defaults and any settings done via optimizer_param(_) will be used.

<P>
     <DT><STRONG><TT>node_method</TT></STRONG>
     <DD>Applicable to MIP problems only. Returns the method that is
     specified to solve the problem at the nodes of the branch-and-bound
     tree. If an auxiliary method can be given for the method, and this
     auxiliary method is not <TT>default</TT>, the method will be returned
     as Method(Aux), e.g. <TT>barrier(none)</TT>. The method will be
     <TT>default</TT> unless otherwise specified by the the user (at setup or
     via eplex_setup/2 or lp_setup/3).  In that case, the external solver's
     defaults and any settings done via optimizer_param(_) will be used.

<P>
     <DT><STRONG><TT>status</TT></STRONG>
     <DD>Status that was returned by the most recent invocation of the 
         external solver.

<P>
     <DT><STRONG><TT>cost</TT></STRONG>
     <DD>Cost of the current solution.
         Fails if no solution has been computed yet.

<P>
     <DT><STRONG><TT>best_bound</TT></STRONG>
     <DD>The best bound (for minimisation, the lower bound) on the optimal 
         objective value for the current problem. Together with the
         worst_bound, this specifies the range for the optimal objective
         value. Note that a non-empty range does not mean that the problem
         is feasible unless an objective value (cost) is also
         available. The best_bound is the same as the current objective
         value if the problem has been solved to optimality. It can be
         better than the objective value either because a) (for MIP
         problems only) the problem has been optimised to within the
         mipgap tolerance, or b) the problem was not solved to optimality,
         i.e. it was aborted before the optimal solution was found.
<P>
     <DT><STRONG><TT>worst_bound</TT></STRONG>
     <DD>The worst bound (for minimisation, the upper bound) on the optimal 
         objective value for the current problem. Together with the
         best_bound, this specifies the range for the optimal objective
         value. Note that a non-empty range does not mean that the problem
         is feasible unless an objective value (cost) is also
         available. The worst_bound is the same as the current objective
         value if a solution has been computed for the problem, whether
         the problem was solved to optimality or not. Depending on the 
         problem type and method used to solve the problem, a worst bound
         can be returned for a problem even if the solving of the problem
         was aborted before a solution was obtained. 
<P>
     <DT><STRONG><TT>typed_solution</TT></STRONG>
     <DD>Returns a term ''(X1,...,Xn) whose arguments are the properly
         typed (integer or float) solution values for the corresponding
         problem variables (<TT>vars</TT>).  The floating point solutions
         are the same as returned by <TT>solution</TT>, the integers are
         obtained by rounding the corresponding floating-point solution to
         the nearest integer.  To instantiate the problem variables to
         their solutions, unify this term with the corresponding term
         containing the variables:

<PRE>
    instantiate_solution(Handle) :-
        lp_get(Handle, vars, Vars),
        lp_get(Handle, typed_solution, Values),
        Vars = Values.
</PRE>
<P>
         Note that this unification could fail if two problem variables Xa
         and Xb were unified after the solution was lasted computed, as the
         solutions values in the Xa and Xb positions could be different,
         even though they are now represented by one variable.
         Fails if no solution has been computed yet.

</P><P>
    <DT><STRONG><TT>slack</TT></STRONG>
    <DD>Returns a list of floating-point values representing the constraint
        slacks in the logically last solve. The problem consists of normal
        constraints followed by any added cutpool constraints, and the
        order corresponds to the list order in <TT>constraints</TT> for
        normal constraints, and to cutpool_info(last_added,Info) for the
        cutpool constraints.  Fails if no solution has been computed yet.

<P>
    <DT><STRONG><TT>slack(Indexes)</TT></STRONG>
    <DD>Returns a list of floating-point values representing the slack
        values for the constraints represented by Indexes. Indexes are a
        list of constraint indices (as returned by lp_add_constraints/4 for
        normal constraints, and lp_add_cutpool_constraints/4 for cutpool
        constraints), and the order of the returned list corresponds to the
        order in <TT>Indexes</TT>.  Fails if no slack value has been
        computed yet for any of the constraints in Indexes -- note that 
        values are only computed for cutpool constraints if they were added
        to the problem.

<P>
    <DT><STRONG><TT>dual_solution</TT></STRONG>
    <DD>Returns a list of floating-point values representing the dual
        solutions in the logically last solve. The problem consists of
        normal constraints followed by any added cutpool constraints, and
        the order corresponds to the list order in <TT>constraints</TT> for
        normal constraints, and to cutpool_info(last_added,Info) for the
        cutpool constraints.  Fails if no solution has been computed yet.

<P>
    <DT><STRONG><TT>dual_solution(Indexes)</TT></STRONG>
    <DD>Returns a list of floating-point values representing the dual
        solutions for the constraints represented by Indexes. Indexes are a
        list of constraint indices (as returned by lp_add_constraints/4
        normal constraints, and lp_add_cutpool_constraints/4 for cutpool
        constraints), and the order of the returned list corresponds to the
        order in <TT>Indexes</TT>.  Fails if no dual solution has been
        computed yet for any of the constraints in Indexes -- note that
        values are only computed for cutpool constraints if they were added
        to the problem.

<P>
    <DT><STRONG><TT>constraints(Indexes)</TT></STRONG>
    <DD>Returns a list of problem constraints as specified by
        Indexes in denormalised form. The constraints can be either 
        normal constraints or cutpool constraints.
<P>
    <DT><STRONG><TT>constraints_norm(Indexes)</TT></STRONG>
    <DD>Returns a list of problem constraints as specified by
        Indexes in normalised form. The constraints can be either 
        normal constraints or cutpool constraints.
<P>
    <DT><STRONG><TT>cutpool_info(Select,Info)</TT></STRONG>
    <DD>Returns the information specified by Info for the cutpool constraints
        specified by Select. The returned information is either
        i) a pair of lists Indexes-Values where Indexes is the index for
        the constraint in the corresponding position of Values, or 
        ii) a list of Indexes if the information requested is index. Info 
        can be:
<DL>
           <DT><STRONG><TT>index</TT></STRONG>
               <DD>the indexes for the selected constraints. A list
                   of Indexes are returned.
           <DT><STRONG><TT>active</TT></STRONG>
               <DD>the current active status of the selected constraints. 
                   The status is either 0 (not active) or 1 (active).
                   A pair of lists Indexes-ActiveStatus is returned.
           <DT><STRONG><TT>add_initially</TT></STRONG>
               <DD>the current add_initially status of the selected 
                   constraints. The status is either 0 (not add) or 1 (add).
                   Note that although inactive constraints have a
                   add_initially status, they will not be added to a problem.
                   A pair of lists Indexes-AddInitially is returned.
           <DT><STRONG><TT>binding_state</TT></STRONG>
               <DD>the binding state for the selected constraints in the
                   logically last solve for the problem. A pair of lists
                   Indexes-BindingStates is returned. To get this
                   information, the slack values must be available from the
                   last solve.
                   The state can be:
                   a) binding - the constraint was satisfied and binding, i.e.
                      it is within tolerance of its RHS value in the
                      normalised form. 
                   b) satisfied - the constraint was satisfied but not
                      binding. 
                   c) inactive - the constraint was inactive.
           <DT><STRONG><TT>constraints_norm</TT></STRONG>
               <DD>the normalised form of the constraints for the selected 
                   constraints is returned in a pair of lists 
                   Indexes-Constraints.
           <DT><STRONG><TT>constraints</TT></STRONG>
               <DD>the denormalised form of the constraints for the selected 
                   constraints is returned in a pair of lists 
                   Indexes-Constraints.
</DL>

    The constraints are selected by Select, which can be:
<DL>
           <DT><STRONG><TT>cstr(Idx)</TT></STRONG>
               <DD>The cutpool constraint as specified by <TT>Idx</TT>. 
           <DT><STRONG><TT>group(Name)</TT></STRONG>
               <DD>The cutpool constraints in the group <TT>Name</TT>. Both
                   active and non-active constraints are returned. Note that
                   the name of the default group is the atom nil (<TT>[]</TT>).
           <DT><STRONG><TT>last_added</TT></STRONG>
               <DD>The cutpool constraints that were added to the
                   problem in the logically previous solve of the problem. 
                   The constraints were either added initially, or were
                   added because they were violated in an intermediate
                   invocation. 
           <DT><STRONG><TT>last_notadded</TT></STRONG>
               <DD>The cutpool constraints that were not added to the
                   problem in the logically previous solve of the problem,
                   i.e. they were not violated.
           <DT><STRONG><TT>last_inactive</TT></STRONG>
               <DD>The cutpool constraints that were inactive during the
                   logically last solve of the problem. This does not
                   include any constraints that were added since the last
                   solve. 
</DL>

<P>
    <DT><STRONG><TT>demon_tolerance</TT></STRONG>
    <DD>Returns a comma-separated pair <TT>(RealTol,IntTol)</TT> of
        floating-point values which specify how far outside a variable's
        range an lp-solution can fall before lp_demon_setup/5
        re-triggers. The tolerances differ for real (default 0.00001) and
        integer (default 0.5) variables.

<P>
    <DT><STRONG><TT>simplex_iterations</TT></STRONG>
    <DD>Returns the external solver's count of simplex iterations.

<P>
    <DT><STRONG><TT>node_count</TT></STRONG>
    <DD>Returns the external MIP solver's node count.

<P>
    <DT><STRONG><TT>statistics</TT></STRONG>
    <DD>Returns a list of counter values <TT>[Successes, Failures,
        Aborts]</TT>, indicating how often lp_solve/2 was invoked on the
        Handle, and how many invocations succeeded, failed and aborted
        respectively.

<P>
    <DT><TT>timeout</TT></STRONG>
    <DD>Returns the time-out value for the solver state. This is the amount
        of CPU time in seconds that the external solver will be allow to
        spend solving the problem before timing out. The value is 0 if 
        no time-out has been set.
<P>
     <DT><STRONG><TT>optimizer_param(Param)</TT></STRONG>
     <DD>Returns the value of the external solver's parameter Param
         for the problem represented by Handle. The external solver 
         has a number of parameters that affect the way they work, and 
         this queries their values. If Param is not a valid parameter for
         the solver, an out of range exception is raised. See below for 
         more details on the parameters.
<P>
    <DT><STRONG><TT>post_equality_when_unified</TT></STRONG>
    <DD>Returns the value (yes or no) if an equality constraint will be
        posted to a solver if two variables in the solver's problem are 
        unified. 
<P>
    <DT><STRONG><TT>pool</TT></STRONG>
    <DD>Returns the name of the eplex instance (if any) associated with 
        the solver state. Fails otherwise. Only useful if called with 
        lp_get/3.
<P>
    <DT><STRONG><TT>handle</TT></STRONG>
    <DD>Returns the solver state handle (if any) associated with the eplex 
        instance. Fails otherwise. Only useful if called with eplex_get/2.
<P>
</DL>
Note that reduced_cost, slack, dual_solution can only be retrieved
when previously requested in the option list of lp_setup/4 or with lp_set/3.
An out of range error would be raised otherwise.

<P>
For the external solver's control parameter specified by
optimizer_param(Param), Param must be an atom. The Value returned is
either an integer, float or atom, depending on the parameter. The parameter
is generally specific to a solver and version, and also, they may be
problem specific, or global, again depending on the solver version. In all
cases, the value returned by lp_get/3 is the current value for the parameter
for the problem Handle. Refer to the solver documentation for details on the 
parameters. The names of the parameters are derived from the names of the 
parameters in the external solver. For CPLEX, take the parameter name from 
the CPLEX manual (or cplex.h), remove the CPX_PARAM_ prefix and convert the 
rest to lower case, e.g.

<PRE>
        CPX_PARAM_NODELIM becomes nodelim. 
</PRE>
For XPRESS-MP (version 13 and newer), take the parameter name from the 
manual (or xpresso.h), remove the XPRS_ prefix (if present) and convert 
the rest to lower case, e.g.
<PRE>
	XPRS_MAXNODE becomes maxnode. 
</PRE>
For Gurobi, take the parameter name from the manual and convert it to
all lower case, e.g.
<PRE>
	IntFeasTol becomes intfeastol. 
</PRE>
<P>
For solvers used via the OSI, there are a few generic parameters supported 
via OSI, and depending on the actual solver, there may be some additional
solver-specific parameters. For the generic parameters, take the parameter
name, remove the Osi prefix and convert the rest to lower case, e.g.
<PRE>
        OsiPrimalTolerance becomes primaltolerance
</PRE>

<P>
    The following parameter names are additional aliases that work for
    either solver:
<DL>
    <DT><TT>feasibility_tol</TT>
	<DD>CPX_PARAM_EPRHS (CPLEX) or XPRS_FEASTOL (XPRESS-MP) or
            OsiPrimalTolerance (OSI) or FeasibilityTol (GRB) - float
    <DT><TT>integrality</TT>
	<DD>CPX_PARAM_EPINT (CPLEX) or XPRS_MIPTOL (XPRESS-MP) or
            CbcIntegerTolerance (OSI,Cbc specific) or IntFeasTol (GRB) - float
    <DT><TT>iteration_limit</TT>
	<DD>CPX_PARAM_ITLIM (CPLEX) or XPRS_LPITERLIMIT (XPRESS-MP) or
            OsiMaxNumIteration (OSI) or IterationLimit (GRB) - integer
    <DT><TT>node_limit</TT>
	<DD>CPX_PARAM_NODELIM (CPLEX) or XPRS_MAXNODE (XPRESS-MP) or
            CbcMaxNumNode (OSI, Cbc specific) or NodeLimit (GRB)- integer
    <DT><TT>objdifference</TT>
	<DD>CPX_PARAM_OBJDIF (CPLEX) or XPRS_MIPADDCUTOFF (XPRESS-MP) or
            CbcCutoffIncrement (OSI, Cbc specific) - float
</DL>

")
]).


:- comment(lp_set/3, [
amode:     lp_set(+,++,+),
args:      ["Handle": "Handle to a solver state",
            "What":   "Option to set",
            "Value":  "Value being set"
           ],
resat:     no,
summary:   "Change initial options for solver state Handle.",
see_also:  [lp_setup/4, lp_get/3, lp_var_get/4, lp_get_iis/5, eplex_set/2],
desc:      html("\
<P>
This primitive can be used to change some of the initial options
even after setup. 
<EM>Handle</EM> refers to an existing solver state,
<EM>What</EM> can be one of the following:


<DL>
    <DT><STRONG><TT>method</TT></STRONG>
        <DD>Set the method that will be used to solve the problem.  Value
        is one of <TT>default, auto, primal, dual, net, net(Simplex), barrier,
        barrier(Crossover), sifting, sifting(SubMethod), concurrent,
	concurrent_det</TT>.  Simplex can be one of <TT>default, auto, primal, dual</TT>,
	specifying the Simplex method to use in the Network Simplex algorithm.
	Crossover can be one of <TT>default, auto, primal, dual, none</TT>,
	specifying the crossover method to use for the barrier method.
	SubMethod could be one of default, auto, primal, dual, net, barrier</TT>,
	specifying the method to use for the subproblems in the sifting method.
	Note that not every method is available on every external solver.
	In case of MIP solving, this is the start algorithm (the one that
	is used to solve the initial relaxation). This method setting will
	override similar settings done via optimizer_param(_).

<P>
    <DT><STRONG><TT>node_method</TT></STRONG>
        <DD>Applicable to MIP problems only. Set the method that will be
        used to solve the problem at the nodes (except the root) of the 
        branch-and-bound tree. Note that the method for solving the root
        node is controlled by the <TT>method</TT> option. Value can be set
        to the same values as in the <TT>method</TT> option, although 
        there may be more restrictions on what the actual methods/auxiliary
        methods that are allowed in combination with the root method,
        due to limitations/restrictions from the external solver. A warning
        will be given when the problem is solved if this is the case, and 
        the default method used instead.  This node_method setting will
	override similar settings done via optimizer_param(_).

<P>
    <DT><STRONG><TT>solution</TT></STRONG>
        <DD>Make the solutions available each time the problem has been
        (re-)solved successfully.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>reduced_cost</TT></STRONG>
        <DD>Make the reduced costs available each time the problem has been
        (re-)solved successfully. If the problem is a MIP, then depending on
        the external solver, this is either unavailable or are the values for
        the optimal LP node. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>slack</TT></STRONG>
        <DD>Make the constraint slacks available each time the problem has been
        (re-)solved successfully.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>dual_solution</TT></STRONG>
       <DD>Make the dual solutions available each time the problem has been
       (re-)solved successfully.  If the problem is a MIP, then depending on
        the external solver, this is either unavailable or are the values for
        the optimal LP node. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>keep_basis</TT></STRONG>
        <DD>Store the basis each time the problem has been solved successfully,
        and use this basis as a starting point for re-solving next time.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>mipstart</TT></STRONG>
        <DD>Use the previous solution values as a warm-start heuristics for
	the MIP search.  This only has an effect for certain solvers (e.g.
	Gurobi), if there are integrality constraints, and if there is a 
	previous solution available.  Possible values are <TT>none</TT>
	(no mipstart values, the default), <TT>all</TT> (use previous
	solution for all variables), or <TT>integers</TT> (use previous
	solution for all variables that are now constrained to be integral).  

<P>
    <DT><STRONG><TT>cache_iis</TT></STRONG>
        <DD>When a problem is found to be infeasible, compute an IIS for the problem
        (if supported by the external solver), and store it so that it can bee retrieved
        by eplex_get_iis/4 or lp_get_iis/5. This will be done before the problem can
        be modified and make the computing of the IIS impossible. The IIS will oulu
        be available before the problem is solved again, and before the infeasible  
        solve is backtracked. This option has no effect if the external solver does not 
        support the finding of IIS. Note that if this option is set, eplex will always ask
        for an IIS to computed for an infeasible problem, even if it is immediately backtracked 
        by the infeasible handler failing, and that the option is only needed if the problem 
        instance in the external solver is modified before eplex_get_iis/4 or lp_get_iis/5 is called. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>demon_tolerance</TT></STRONG>
        <DD>Specify how far outside a variable's range an lp-solution
        can fall before lp_demon_setup/5 re-triggers.
        Value is a comma-separated pair <TT>(RealTol,IntTol)</TT> of 
        floating-point values (default <TT>(0.00001,0.5)</TT>).

<P>
    <DT><STRONG><TT>use_var_names</TT></STRONG>
        <DD>Specify if variable names (set using <TT>set_var_name/2</TT> of
        the var_name library) should be passed to the external solver. If a
        particular variable does not have a name, a solver's default name
        would be used. Note that for XPRESS-MP, there is a limit on the length
        of the name, which can be changed between 8 and 64 in steps of 8 with
        the parameter <TT>N_NAMLEN</TT>. Variable names longer than this limit
        are truncated to the limit.  
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>, the default 
        is <TT>no</TT>.

<P>
    <DT><STRONG><TT>timeout</TT></STRONG>
        <DD>Set the external solver to time-out after <TT>Value</TT>
        seconds.  The solver will abort (in either the abort or suboptimal
        state, depending on if a suboptimal solution was found) if the
        optimal solution was not found within the time limit. In cases
        where the solver expects an integer for the time-out interval, the
        time given is rounded up to the next integer value. This should be
        used instead of the solver specific optimizer_param(Param) for
        setting timeouts, as eplex sets these parameter(s) itself. Note
        that the exact behaviour of the timeout setting is solver dependent.
        Value is a positive number.
<P>
    <DT><STRONG><TT>cutpool_option(Idx,Option)</TT></STRONG>
        <DD>Set the option Option as specified by Value for the cutpool
        constraint with index Idx. Option is one of the following:
<DL>
           <DT><STRONG><TT>active</TT></STRONG>
               <DD>Set the active status for the constraint Idx to Value.
                   Value can be 0 or 1. 0 is non-active, 1 is active.
           <DT><STRONG><TT>add_initially</TT></STRONG>
               <DD>Specify if the constraint Idx should be added to a
                   problem before the solver is invoked. 
                   Value can be 0 or 1. 1 is to add initially, 0 not.
</DL>
</P><P>
        Note that the option is <STRONG>not</STRONG> undone on
        backtracking.
</P><P>
    <DT><STRONG><TT>cutpool_group</TT></STRONG>
        <DD>Value is an atom, and is used to specify the name of a group of
        cutpool constraints. If Name is not a current group name, a new
        group with that name is created. Once created, a named group exists
        until the problem is destroyed. The default group (with the atom
        nil (<TT>[]</TT>) as its name) is predefined and does not need to
        be created with <TT>cutpool_group</TT>.
<P>
    <DT><STRONG><TT>suboptimal_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver returned a suboptimal solution (because the problem
        was aborted). Value would replace any existing suboptimal handler,
        and would also be run in place of raising the default
        <TT>eplex_suboptimal</TT> event.
<P>
    <DT><STRONG><TT>unbounded_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        problem is unbounded. Value would replace any existing unbounded
        handler, and would be run in place of raising the default
        <TT>eplex_unbounded</TT> event.
<P>
    <DT><STRONG><TT>infeasible_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver found the problem to be infeasible. Value would
        replace any existing infeasible handler, and would be run in place
        of raising the default <TT>eplex_infeasible</TT> event. Note that
        the default and logically correct behaviour is to fail, this
        handler is provided to allow the user to analyse the cause of the
        infeasibility. It is recommended that the handler should also fail
        after performing the analysis.
<P>
    <DT><STRONG><TT>unknown_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver was not able to determine if the problem is
        unbounded Value would replace any existing unknown handler, and
        would be run in place of raising the default <TT>eplex_unknown</TT>
        event.
<P>
    <DT><STRONG><TT>abort_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver aborted without finding any solution.  Value would
        replace any existing abort handler, and would be run in place of
        raising the default <TT>eplex_abort</TT> event.
<P>
    <DT><STRONG><TT>optimizer_param(Param)</TT></STRONG>
        <DD>Set the external solver's control parameter Param for the problem
        <TT>Handle</TT>. If the solver's parameters are global and not
        problem specific, an unimplemented functionality exception would
        be raised. See lp_get/3 for more details on the external solver's
        parameters. Note that the new setting is <EM>not</EM> undone on 
        backtracking.
<P>
    <DT><STRONG><TT>write_before_solve</TT></STRONG>
       <DD>Value can be the pair (Format,File) or the atom no. If
       (Format,File) is given, Eplex will ask the external solver to dump
       the problem each time the solver is solved. This allows the problem
       in an <TT>eplex_probe/2</TT> or <TT>lp_probe/3</TT> to be dumped. As
       in <TT>lp_write/3</TT>, <TT>Format</TT> is the format of the dumped
       problem, and File is the filename. See for more details. Note that
       the problem is dumped each time the external solver is invoked if
       the problem has cutpool constraints, where there may be multiple
       invocations of the solver per solver call.  `no' for Value will turn
       off this dumping.
<P>
    <DT><STRONG><TT>post_equality_when_unified</TT></STRONG>
        <DD>Value can be the atoms yes or no. Determines if an equality
        constraint between two solver variables will be posted to the
        solver when these variables are unified. Setting Value to no means
        that the constraint will <EM>not</EM> be posted. Note that this can
        lead to the solver's problem becoming inconsistent with
        ECLiPSe's. 
<P>
</DL>
  Making solutions available means that they can be retrieved using
  lp_get/3 or lp_var_get/4 after the solver has been run successfully.
</P><P>
  All settings apart from optimizer_param(Param) and cutpool settings 
  will be undone on backtracking!
</P>")
]).

:- comment(eplex_set/2, [
template:  ["eplex_set(++ParamName, -Value)","EplexInstance:eplex_set(++ParamName, -Value)"],
args:      [
            "ParamName": "Name of parameter (atom)",
            "Value":     "New value for ParamName"
           ],
resat:     no,
summary:   "Change initial options for solver state associated with EplexInstance",
exceptions: [5: "EplexInstance does not a solver setup for it.",
             40: "Solver state had been previously destroyed."
             ],
see_also:  [eplex_solver_setup/4, eplex_set/2, eplex_get_iis/4, lp_set/3, lp_get/3],
desc:      html("\
<P>
This primitive can be used to change some of the initial options
even after setup of a solver for eplex instance <EM>EplexInstance</EM>.
<EM>What</EM> can be one of the following:

<DL>
    <DT><STRONG><TT>method</TT></STRONG>
        <DD>Set the method that will be used to solve the problem.  Value
        is one of <TT>default, auto, primal, dual, net, net(Simplex), barrier,
        barrier(Crossover), sifting, sifting(SubMethod), concurrent,
	concurrent_det</TT>.  Simplex can be one of <TT>default, auto, primal, dual</TT>,
	specifying the Simplex method to use in the Network Simplex algorithm.
	Crossover can be one of <TT>default, auto, primal, dual, none</TT>,
	specifying the crossover method to use for the barrier method.
	SubMethod could be one of default, auto, primal, dual, net, barrier</TT>,
	specifying the method to use for the subproblems in the sifting method.
	Note that not every method is available on every external solver.
	In case of MIP solving, this is the start algorithm (the one that
	is used to solve the initial relaxation). This method setting will
	override similar settings done via optimizer_param(_).

<P>
    <DT><STRONG><TT>node_method</TT></STRONG>
        <DD>Applicable to MIP problems only. Set the method that will be
        used to solve the problem at the nodes (except the root) of the 
        branch-and-bound tree. Note that the method for solving the root
        node is controlled by the <TT>method</TT> option. Value can be set
        to the same values as in the <TT>method</TT> option, although 
        there may be more restrictions on what the actual methods/auxiliary
        methods that are allowed in combination with the root method,
        due to limitations/restrictions from the external solver. A warning
        will be given when the problem is solved if this is the case, and 
        the default method used instead.  This node_method setting will
	override similar settings done via optimizer_param(_).

<P>
    <DT><STRONG><TT>solution</TT></STRONG>
        <DD>Make the solutions available each time the problem has been
        (re-)solved successfully.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>reduced_cost</TT></STRONG>
        <DD>Make the reduced costs available each time the problem has been
        (re-)solved successfully. If the problem is a MIP, then depending on
        the external solver, this is either unavailable or are the values
        for the optimal LP node. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>slack</TT></STRONG>
        <DD>Make the constraint slacks available each time the problem has been
        (re-)solved successfully.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>dual_solution</TT></STRONG>
        <DD>Make the dual solutions available each time the problem has been
        (re-)solved successfully. If the problem is a MIP, then depending on
        the external solver, this is either unavailable or are the values
        for the optimal LP node. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>keep_basis</TT></STRONG>
        <DD>Store the basis each time the problem has been solved successfully,
        and use this basis as a starting point for re-solving next time.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>mipstart</TT></STRONG>
        <DD>Use the previous solution values as a warm-start heuristics for
	the MIP search.  This only has an effect for certain solvers (e.g.
	Gurobi), if there are integrality constraints, and if there is a 
	previous solution available.  Possible values are <TT>none</TT>
	(no mipstart values, the default), <TT>all</TT> (use previous
	solution for all variables), or <TT>integers</TT> (use previous
	solution for all variables that are now constrained to be integral).  

<P>
    <DT><STRONG><TT>cache_iis</TT></STRONG>
        <DD>When a problem is found to be infeasible, compute an IIS for the problem
        (if supported by the external solver), and store it so that it can bee retrieved
        by eplex_get_iis/4 or lp_get_iis/5. This will be done before the problem can
        be modified and make the computing of the IIS impossible. The IIS will only
        be available before the problem is solved again, and before the infeasible  
        solve is backtracked. This option has no effect if the external solver does not 
        support the finding of IIS. Note that if this option is set, eplex will always ask
        for an IIS to computed for an infeasible problem, even if it is immediately backtracked 
        by the infeasible handler failing, and that the option is only needed if the problem 
        instance in the external solver is modified before eplex_get_iis/4 or lp_get_iis/5 is called. 
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.

<P>
    <DT><STRONG><TT>demon_tolerance</TT></STRONG>
        <DD>Specify how far outside a variable's range an lp-solution
        can fall before lp_demon_setup/5 re-triggers.
        Value is a comma-separated pair <TT>(RealTol,IntTol)</TT> of 
        floating-point values (default <TT>(0.00001,0.5)</TT>).

<P>
    <DT><STRONG><TT>use_var_names</TT></STRONG>
        <DD>Specify if variable names (set using <TT>set_var_name/2</TT> of the
        var_name library) should be passed to the external solver. If a
        particular variable does not have a name, a solver's default name
        would be used. Note that for XPRESS-MP, there is a limit on the
        length of the name, which can be changed between 8 and 64 in steps
        of 8 with the parameter <TT>N_NAMLEN</TT>. Variable names longer
        than this limit are truncated to the limit. Note also that only 
        new variables from constraints added after the <TT>lp_set/3</TT> call 
        will pass their names to the external solver.
        Value is one of the atoms <TT>yes</TT> or <TT>no</TT>.
<P>
    <DT><STRONG><TT>timeout</TT></STRONG>
        <DD>Set the external solver to time-out after <TT>Value</TT>
        seconds.  The solver will abort (in either the abort or suboptimal
        state, depending on if a suboptimal solution was found) if the
        optimal solution was not found within the time limit. In cases
        where the solver expects an integer for the time-out interval, the
        time given is rounded up to the next integer value. This should be
        used instead of the solver specific optimizer_param(Param) for
        setting timeouts, as eplex sets these parameter(s) itself. Note
        that the exact behaviour of the timeout setting is solver dependent.
        Value is a positive number.
<P>
<DT><STRONG><TT>suboptimal_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver returned a suboptimal solution (because the problem
        was aborted). Value would replace any existing suboptimal handler,
        and would also be run in place of raising the default
        <TT>eplex_suboptimal</TT> event.
<P>
<DT><STRONG><TT>unbounded_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        problem is unbounded. Value would replace any existing unbounded
        handler, and would be run in place of raising the default
        <TT>eplex_unbounded</TT> event.
<P>
<DT><STRONG><TT>infeasible_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver found the problem to be infeasible. Value would
        replace any existing infeasible handler, and would be run in place
        of raising the default <TT>eplex_infeasible</TT> event. Note that
        the default and logically correct behaviour is to fail, this
        handler is provided to allow the user to analyse the cause of the
        infeasibility. It is recommended that the handler should also fail
        after performing the analysis.
<P>
<DT><STRONG><TT>unknown_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver was not able to determine if the problem is
        unbounded Value would replace any existing unknown handler, and
        would be run in place of raising the default <TT>eplex_unknown</TT>
        event.
<P>
<DT><STRONG><TT>abort_handler</TT></STRONG>
        <DD>Value is a user defined goal to handle the case when the
        external solver aborted without finding any solution.  Value would
        replace any existing abort handler, and would be run in place of
        raising the default <TT>eplex_abort</TT> event.
<P>
    <DT><STRONG><TT>optimizer_param(Param)</TT></STRONG>
        <DD>Set the external solver's control parameter Param for the problem
        <TT>Handle</TT>. If the solver's parameters are global and not
        problem specific, an unimplemented functionality exception would
        be raised. See lp_get/3 for more details on the external solver's
        parameters. Note that the new setting is <EM>not</EM> undone on 
        backtracking.
<P>
    <DT><STRONG><TT>write_before_solve</TT></STRONG>
       <DD>Value can be the pair (Format,File) or the atom no. If
       (Format,File) is given, Eplex will ask the external solver to dump
       the problem each time the solver is solved. This allows the problem
       in an <TT>eplex_probe/2</TT> or <TT>lp_probe/3</TT> to be dumped. As
       in <TT>lp_write/3</TT>, <TT>Format</TT> is the format of the dumped
       problem, and File is the filename. See for more details. Note that
       the problem is dumped each time the external solver is invoked if
       the problem has cutpool constraints, where there may be multiple
       invocations of the solver per solver call.  `no' for Value will turn
       off this dumping.
<P>
    <DT><STRONG><TT>post_equality_when_unified</TT></STRONG>
        <DD>Value can be the atoms yes or no. Determines if an equality
        constraint will be posted to a solver when two of its variables are
        unified. Setting Value to no means that the constraint will not be
        posted. Note that this could mean that the
        solver's problem may become inconsistent with ECLiPSe's. 
<P>
</DL>
  Making solutions available means that they can be retrieved using
  lp_get/3 or lp_var_get/4 after the solver has been run successfully.
</P><P>
  All settings apart from optimizer_param(Param) will be undone on backtracking!
</P>")
]).



:- comment(lp_get/2, [
amode:     lp_get(++,-),
amode:     lp_get(++,+),
args:      ["ParamName":   "Atom or structure",
            "Value":       "Variable, integer, float or atom"
           ],
summary:   "Obtain the value of a global parameter.",
see_also:  [lp_set/2],
desc:      html("\
    Eplex has a number of global (i.e. not specific to a
    particular problem) parameters. These can be queried using this 
    predicate.
<DL>
    <DT><TT>has_indicator_constraints</TT>
	<DD>Returns 'yes' if the solver supports =&ge;/2 indicator constraints,
	else 'no'.
    <DT><TT>has_miqp</TT>
	<DD>Returns 'yes' if the solver supports mixed integer problems
	with quadratic objectives, else 'no'.
    <DT><TT>has_qp</TT>
	<DD>Returns 'yes' if the solver supports quadratic objectives,
	else 'no'.
    <DT><TT>optimizer</TT>
	<DD>Returns the name of the external solver, currently
	'cplex', 'xpress', 'osi' or gurobi.
    <DT><TT>optimizer_version</TT>
	<DD>Returns an integer derived from the version of the
	external solver (for osi, the version is the actual solvers
        used, including any significant third-party packages such as 
        sparse matrix ordering packages required by the barrier solver)
    <DT><TT>presolve</TT>
	<DD>Returns the default presolve setting for solver setup, i.e. the
        presolve setting that a solver state would be given if it was not
        directly specified during setup. A value of 0 disables presolving.
	A value of 1 either enables presolving (for external solvers that
	have only global parameters), or uses the default optimizer_param
	settings (for external solvers with per-problem parameters).
    <DT><TT>timeout</TT>
        <DD>Returns the default time-out setting that a solver state would
        be given if it was not directly specified during setup. Value is
        0 if there is no time-out.
    <DT><TT>optimizer_param(Param)</TT>
        <DD>Get the Value of the external solver's parameter <TT>Param</TT>.
        The exact behaviour and available parameters are dependent on the 
        external solver: if the external solver has only problem specific 
        parameters (e.g. XPRESS-MP 13 and later), this gets the default 
        setting that would be used for new problems; if the external solver
        has global parameters, this gets the global value for the parameter.
        If Param is not a valid parameter for the solver, an out of range 
        error is raised. See lp_get/3 for more details on the parameters. 
</DL>
")
]).

:- comment(lp_set/2, [
amode:     lp_set(++,++),
args:      ["ParamName": "Atom or structure",
            "Value": "Integer, float, string or structure"
           ],
see_also:  [lp_get/2],
resat:     no,
summary:   "Set a global parameter for the external solver.",
desc:      html("
<P>
    Set a global parameter. The parameter names are the same as described
    in lp_get/2. For the timeout parameter, Value can be of any numeric
    type. Note that for the external solver's parameters (set via
    optimizer_param(Param)), the exact behaviour is solver dependent: if
    the solver has global parameters, this sets the value of the parameter
    globally; otherwise, this sets the default value of the parameter that
    would be assigned to a new problem. The setting is <EM>not</EM> undone 
    on backtracking.
</P><P>
    In addition, lp_set/2 is used to control the output from the external 
    solver:
<DL>
    <DT><TT>lp_set(SolverChannel, +(Stream))</TT>
       <DD>Send output from SolverChannel to the ECLiPSe I/O stream Stream.
    <DT><TT>lp_set(SolverChannel, -(Stream))</TT>
       <DD>Stop sending output from SolverChannel to the ECLiPSe I/O stream 
       Stream.
</DL>
    SolverChannel is one of <TT>result_channel, error_channel, warning_channel, 
    log_channel</TT>, and Stream is an ECLiPSe stream identifier (e.g. 
    <TT>output</TT>, or the result of an open/3 operation).
<P>
    By default, <TT>error_channel</TT> is directed to ECLiPSe's
    <TT>error</TT> stream, <TT>warning_channel</TT> to <TT>warning_output</TT>
    while <TT>result_channel</TT> and <TT>log_channel</TT> are suppressed.
    To see the output on these channels, do
</P>
<TT><PRE>
   :- lp_set(result_channel, +output).
   :- lp_set(log_channel, +log_output).
</PRE></TT>
<P>
Similarly, to create a log file:
<TT><PRE>
   :- open('mylog.log', write, logstream), lp_set(log_channel, +logstream).
</PRE></TT>
and to stop logging:
<TT><PRE>
   :- lp_set(log_channel, -logstream), close(logstream).
</PRE></TT>
</P>")
]).

:- comment(reduced_cost_pruning/2, [
summary:"Prune bounds of all problem variables based on their reduced costs",
amode:     reduced_cost_pruning(+,?),
args:      ["Handle": "Handle to a (solved) solver state",
            "GlobalCost": "Bounded global cost variable"],
see_also:  [lp_demon_setup/5],
fail_if:   "None",
desc:html("
    <P>
    Handle is a problem handle referring to the linear relaxation of
    a more complex problem. GlobalCost is the overall cost variable
    of the complex problem.
    </P><P>
    This predicate tries to prune the bounds of all variables that
    occur in the linear relaxation, based on their reduced costs,
    the optimum of the relaxation, and the currently known bounds
    (lower if maximising, upper if minimising) on the global cost.
    </P><P>
    This predicate should be called just after the Handle has been solved.
    In particular, it can be used as the post-goal in an lp-demon.
    The solver should have been set up with the <TT>reduced_cost(yes)</TT>
    option.
    </P><P>
    Note that the bounds of GlobalCost is obtained using the generic
    get_var_bounds/3. For correct pruning, GlobalCost should only have
    bounds that are relevant to this problem. 

")]).

:- comment(lp_var_occurrence/3, [
amode:   lp_var_occurrence(?,+,-),
amode:   lp_var_occurrence(?,-,-),
args:    ["Var": "Variable",
          "Handle": "Handle to a solver state, or a variable",
	  "Index": "Column number for Var in Handle's matrix (integer)"
	 ],
summary: "Returns the column number Index for Var in the external solver represented by Handle",
resat: "Yes (if Handle is a variable).",
desc: html("
    <P>
    If Handle is a problem handle, then Index is the column number for the
    variable Var in the problem matrix of the external solver presented by
    Handle. If Handle is a variable, then the predicate returns the handles
    and index for each problem handle the variable non-determinately. 
    </P>
")]).


:- comment(eplex_var_get_bounds/3, [
 template:  ["eplex_var_get_bounds(+Var, -Lo, -Hi)","EplexInstance:eplex_var_get_bounds(+Var, -Lo, -Hi)"],
args:    [
          "Var":    "A problem variable for EplexInstance",
          "Lo":     "Lower bound for Var",
          "Hi":     "Upper bound for Var"
         ], 
summary: "Returns the bounds stored in the solver state for Var in eplex instance EplexInstance.",
resat: no,
exceptions: [5: "EplexInstance does not a solver setup for it.",
             6: "Var is not a problem variable for EplexInstance.",
             40: "Solver state had been previously destroyed."
            ],
see_also: [lp_var_get_bounds/4,eplex_solver_setup/1, eplex_solver_setup/4],
desc: html("
    <P>
    Returns the numeric bounds for Var stored in the solver state
    associated with the eplex instance EplexInstance. The bounds are
    returned as floats. Var must be an existing problem variable for
    EplexInstance. ")
]).

:- comment(lp_var_get_bounds/4, [
amode:   lp_var_get_bounds(+,?,-,-),
args:    ["Handle": "Handle to a solver state",
          "Var":    "A solver problem variable for Handle",
          "Lo":     "Lower bound for Var",
          "Hi":     "Upper bound for Var"
         ], 
summary: "Returns the bounds stored in the solver state of Handle for Var.",
resat: no,
see_also: [eplex_var_get_bounds/3, lp_var_set_bounds/4],
exceptions: [5: "Handle is not in the form of a solver handle.",
             6: "Var is not a problem variable for Handle.",
             40: "Handle not valid: solver state had been destroyed by cleanup"

            ],
desc: html("
    <P>
    Returns the numeric bounds stored in the solver state of Handle for the
    variable Var. The bounds are returned as floats. Var must be an
    existing problem variable for Handle, i.e. it must occur in the 
    constraints posted to the solver state. ")
]).

:- comment(lp_var_set_bounds/4, [
amode:   lp_var_set_bounds(+,?,+,+),
args:    ["Handle": "Handle to a solver state",
          "Var":    "A solver problem variable for Handle",
          "Lo":     "New lower bound for Var (number)",
          "Hi":     "New upper bound for Var (number)"
         ], 
summary: "Imposes new bounds for Var on the solver state of Handle.",
resat: no,
see_also: [lp_var_get_bounds/4],
fail_if: "Lo is greater than Hi.",
exceptions: [5: "Handle is not in the form of a solver handle.",
             6: "Var is not a problem variable for Handle.",
             40: "Handle not valid: solver state had been destroyed by cleanup"
            ],
desc: html("
    <P>
    Imposes numeric bounds on the solver state of Handle for the variable
    Var. Each bound is only updated if it is more narrow than the current
    bound for the variable. The bounds are converted to floats before they
    are imposed, and no typing is implied by the type of the numbers. It is
    possible to impose incompatible bounds for the same variable on
    different solver states. Var must be an existing problem variable for
    Handle, i.e. it must occur in the constraints posted to the solver
    state. ")
]).

:- comment(eplex_get_iis/4, [
        summary: "Returns an IIS for an infeasible problem associated with EplexInstance.",
        template: ["eplex_get_iis(-NumConstraints, -NumVars, -ConstraintIdxs, -VarInfos)",
		   "EplexInstance:eplex_get_iis(-NumConstraints, -NumVars, -ConstraintIdxs, -VarInfos)"],
        exceptions: [141: "External solver does not support finding IIS",
                     213: "Error in external solver while getting IIS"
                    ],
        args: ["NumConstraints" : "Number of constraints in the IIS",
               "NumVars" : "Number of variables in the IIS",
               "ConstraintIdxs": "List of Indexes of the constraints in the IIS", 
               "VarInfos": "List of Variable:Status pairs where Variables"
                          " are the variables in the IIS and Status is the"
                          " status of that variable"
              ],
        amode: eplex_get_iis(-,-,-,-),
        see_also: [lp_get_iis/5, eplex_solver_setup/4, eplex_solve/1, eplex_probe/2,
                   eplex_get/2, eplex_set/2, 
                   lp_add_constraints/4, lp_add_cutpool_constraints/4], 
        eg: "
 % simple inconsistency, get the constraints with constraints option of eplex_get/2
 [eclipse 6]: eplex:(X=:=Y),
        eplex:(X+Y>=3),
        eplex:(X+Y=<2),
        eplex_solver_setup(min(X)),
        eplex_set(cache_iis, yes),
        eplex_set(infeasible_handler,
             eplex_get_iis(NC,BV, Is, Vs),
             eplex_get(constraints(Is), Cs))),
        eplex_solve(C).
  CPLEX Error  1217: No solution exists.
 
  X = X{-1e+20 .. 1e+20}
  Y = Y{-1e+20 .. 1e+20}
  NC = 2
  BV = 0
  Is = [0, 1]
  Vs = []
  Cs = [X{-1e+20 .. 1e+20} + Y{-1e+20 .. 1e+20} =< 2.0, X + Y >= 3.0]
  C = C
  Yes (0.00s cpu)
  [eclipse 7]:

  % simple example using cutpool constraints and cache_iis option
  [eclipse 7]:         eplex:(X=:=Y), eplex_solver_setup(min(X)),
        eplex_set(cache_iis, yes), eplex_set(infeasible_handler, eplex_get_iis(NC,NV,Is,Vs)),
        eplex_get(handle, H),
        lp_add_cutpool_constraints(H, [(X+Y>=4),(2*X =:= 2*Y), (X+Y=<3)], [], Idxs),
        eplex_solve(C).
  
                                CPLEX Error  1217: No solution exists.
 
  X = X{-1e+20 .. 1e+20}
  Y = Y{-1e+20 .. 1e+20}
  NC = 2
  NV = 0
  Is = [g(2, 0), g(2, 2)]
  Vs = []
  H = lp_handle(0)
  Idxs = [g(2, 0), g(2, 1), g(2, 2)]
  C = C
  Yes (0.00s cpu)
  [eclipse 8]:
  ",         
        desc: html("\
    <P>
    If the solver found that the problem was infeasible, this predicate
    will return an IIS (Irreducible Infeasible Subsystem or Irreducible
    Inconsistent Subsystem) for the problem, if supported by the solver. 
<P>
    This predicate must be called from within a user defined 
    infeasible_handler, and with the cache_iis option set to yes (both
    can be set during solver set-up or using eplex_set/2). The reason for
    this is the IIS must be computed by the solver before the failed
    solver state changes, and backtracking pass the failure (the normal 
    behaviour when a problem is infeasible) will logically remove the failed 
    state. cache_iis is needed because the default setting for this
    option is no, as the IIS need to be computed as an extra step, so
    IIS is normally not computed.
<P>
    An IIS is a subset of the problem constraints and variables which
    defines an infeasible subproblem. It is usually irreducible in that
    any proper subset of the IIS is feasible. Finding an IIS allows the
    diagnostic analysis of the infeasible problem. Note that a problem
    may have more than one infeasibility, and thus more than one IIS, 
    but this predicate only returns one. 
    </P><P>
    NumConstraints and NumVars are the number of constraints and variables
    that are in the IIS. ConstraintIdxs is a list of the constraint indexes
    of the constraints in the IIS, these indexes are the indexes that are
    returned by lp_add_constraints/4 and lp_add_cutpool_constraints/4, and can 
    also be used to retrieve the constraint with the constraints_norm(Indexes)
    and constraints(Indexes) options of eplex_get/2 and lp_get/3. VarInfos gives
    information for the variables in the IIS, and is a list of Var:Status
    pair, where Var is a variable in the IIS, and Status is its status.
    Status is a one character string, and can be:
<DL>
       <DT>\"b\": <DD>if both bounds of the variable are involved in the infeasibility
       <DT>\"u\": <DD>if the upper bound of the variable is involved in the infeasibility
       <DT>\"l\": <DD>if the lower bound of the variable is involved in the infeasibility
       <DT>\"x\": <DD>if it is unknown which bound is involved in the infeasibility
</DL><P>
    An IIS is returned only if finding IIS is supported by the external
    solver. In addition, the solver may limit the type of problems and the
    information returned by the IIS, for example, finding an IIS may be
    limited to linear problems, and/or bound status information may be
    unavailable for the variables (hence the need for the \"x\" status).
    Consult the solver's manual entry for infeasibility analyses for more detail.   
</P><P>
    For some external solvers, and for problems on the boundary between
    feasible and infeasible, it is possible that the routine that finds
    the IIS will conclude that the problem is feasible, even though it was
    considered infeasible when the problem was solved. In such cases, an
    empty IIS will be returned.
")
]).

:- comment(lp_get_iis/5, [
        summary: "Returns an IIS for an infeasible problem.",
        args: ["Handle" : "Handle to a solver state",
               "NumConstraints" : "Number of constraints in the IIS",
               "NumVars" : "Number of variables in the IIS",
               "ConstraintIdxs": "List of Indexes of the constraints in the IIS", 
               "VarInfos": "List of Variable:Status pairs where Variables"
                          " are the variables in the IIS and Status is the"
                          " status of that variable"
              ],
        amode: lp_get_iis(+,-,-,-,-),
        exceptions: [141: "External solver does not support finding IIS"],
        see_also: [eplex_get_iis/4, lp_setup/4, lp_demon_setup/5, lp_solve/2, lp_probe/3,
                   lp_get/3, lp_set/3, lp_add_constraints/4, lp_add_cutpool_constraints/4], 
        desc: html("\
    <P>
    If the solver found that the problem was infeasible, this predicate
    will return an IIS (Irreducible Infeasible Subsystem or Irreducible
    Inconsistent Subsystem) for the problem, if supported by the solver. 
    </P><P>
    This predicate must be called from within a user defined 
    infeasible_handler, and with the cache_iis option set to yes (both
    can be set during solver set-up or using lp_set/2). The reason for
    this is the IIS must be computed by the solver before the failed
    solver state changes, and backtracking pass the failure (the normal 
    behaviour when a problem is infeasible) will logically remove the failed 
    state. cache_iis is needed because the default setting for this
    option is no, as the IIS need to be computed as an extra step, so
    IIS is normally not computed.
    </P><P>
    An IIS is a subset of the problem constraints and variables which
    defines an infeasible subproblem. It is usually irreducible in that
    any proper subset of the IIS is feasible. Finding an IIS allows the
    diagnostic analysis of the infeasible problem. Note that a problem
    may have more than one infeasibility, and thus more than one IIS, 
    but this predicate only returns one. 
    </P><P>
    NumConstraints and NumVars are the number of constraints and variables
    that are in the IIS. ConstraintIdxs is a list of the constraint indexes
    of the constraints in the IIS, these indexes are the indexes that are
    returned by lp_add_constraints/4 and lp_add_cutpool_constraints/4, and can 
    also be used to retrieve the constraint with the constraints_norm(Indexes)
    and constraints(Indexes) options of eplex_get/2 and lp_get/3. VarInfos gives
    information for the variables in the IIS, and is a list of Var:Status
    pair, where Var is a variable in the IIS, and Status is its status.
    Status is a one character string, and can be:
<DL>
       <DT>\"b\": <DD>if both bounds of the variable are involved in the infeasibility
       <DT>\"u\": <DD>if the upper bound of the variable is involved in the infeasibility
       <DT>\"l\": <DD>if the lower bound of the variable is involved in the infeasibility
       <DT>\"x\": <DD>if it is unknown which bound is involved in the infeasibility
</DL><P>
    An IIS is returned only if finding IIS is supported by the external
    solver. In addition, the solver may limit the type of problems and the
    information returned by the IIS, for example, finding an IIS may be
    limited to linear problems, and/or bound status information may be
    unavailable for the variables (hence the need for the \"x\" status).
    Consult the manual entry for infeasibility analyses for more detail.   
</P><P>
    For some external solvers, and for problems on the boundary between
    feasible and infeasible, it is possible that the routine that finds
    the IIS will conclude that the problem is feasible, even though it was
    considered infeasible when the problem was solved. In such cases, an
    empty IIS will be returned.
")
]).


:- comment(eplex_verify_solution/2, [
        summary: "Verifies the current solution for the problem associated with EplexInstance.",
        template:["eplex_verify_solution(-ViolatedCstrs,-ViolatedVars)",
		  "EplexInstance:eplex_verify_solution(-ViolatedCstrs,-ViolatedVars)"],
        args: ["ViolatedCstrs": "List of violated Constraints.", 
               "ViolatedVars": "List of violated variables."
              ],
        amode: eplex_verify_solution(-,-),
        desc: html(
"<P>
 This predicate checks the current solution for the problem associated with
 EplexInstance. It verifies that all the constraints are satisfied by the
 solution values for the problem variables, and that the solution values do
 not violate their bounds, and the values are integral for integer variables.
 Violated Constraints are returned in ViolatedCstrs, and violated variables in 
 ViolatedVars.
</P><P>
 Under normal circumstances, if the external solver produces a solution, it 
 should produce no violations. Any violation probably indicates a problem
 with the external solver. However, because the external solver is a complex
 piece of software and may contain problems, it is a good idea to verify the
 solution, and this predicate provides an easy way to do this.
</P><P>
 Each violation is returned as a structure of the following form:
 <PRE>  vio(type,delta,idx,item) </PRE>
 where
<DL>
       <DT>type:   <DD>the type of violation:
          <DL>
              <DT>norm:   <DD>the violated item is a normal constraint.
              <DT>condcp: <DD>the violated item is an active cutpool constraint.
              <DT>int:    <DD>the violated item is an integer variable whose 
                      solution value is not integral.                    
              <DT>lower:  <DD>the violated item is a variable whose solution value
                      is less than the lower bound.                     
              <DT>upper:  <DD>the violated item is a variable whose solution value
                      is greater than the upper bound.                     
           </DL>
        <DT>delta: <DD>the absolute value of the violation. 
        <DT>idx:   <DD>the index used by the external solver for the item (most
               likely the row/column number in the problem matrix). This 
               is needed to locate the violation in the solver for reporting
               or investigating the problem.
         <DT>item:  <DD>the violated item, i.e. the constraint or variable.
</DL><P>
 For checking of constraints, a fresh copy of the constraint is made with
 the solution values, as this avoids binding the original variables. Any active
 cutpool constraints in the (logically) last solve of the problem are checked,
 along with the normal constraints. A constraint is considered to be  violated
 if the difference between the sum of the left-hand side and the right-hand side 
 for the constraint is greater than the feasibility tolerance (feasibility_tol)
 parameter for the instance. For variables, a bound is considered violated if
 the solution value falls outside the bound by more than feasibility tolerance,
 and integrality is considered to be violated if the fractional part of the
 soltuion is greater than the integrality parameter for the instance. 
"),
 fail_if: "no solution values are available."
]).

:- comment(lp_verify_solution/3, [
        summary: "Verifies the current solution for the problem associated with Handle.",
        args: ["Handle": "Handle to a solver state",
               "ViolatedCstrs": "List of violated Constraints.", 
               "ViolatedVars": "List of violated variables."
              ],
        amode: lp_verify_solution(+,-,-),
        desc: html(
"<P>
 This predicate checks the current solution for the problem associated with
 Handle. It verifies that all the constraints are satisfied by the
 solution values for the problem variables, and that the solution values do
 not violate their bounds, and the values are integral for integer variables.
 Violated Constraints are returned in ViolatedCstrs, and violated variables in 
 ViolatedVars.
</P><P>
 Under normal circumstances, if the external solver produces a solution, it 
 should produce no violations. Any violation probably indicates a problem
 with the external solver. However, because the external solver is a complex
 piece of software and may contain problems, it is a good idea to verify the
 solution, and this predicate provides an easy way to do this.
</P><P>
 Each violation is returned as a structure of the following form:
 <PRE>  vio(type,delta,idx,item) </PRE>
 where
<DL>
       <DT>type:   <DD>the type of violation:
          <DL>
              <DT>norm:   <DD>the violated item is a normal constraint.
              <DT>condcp: <DD>the violated item is an active cutpool constraint.
              <DT>int:    <DD>the violated item is an integer variable whose 
                      solution value is not integral.                    
              <DT>lower:  <DD>the violated item is a variable whose solution value
                      is less than the lower bound.                     
              <DT>upper:  <DD>the violated item is a variable whose solution value
                      is greater than the upper bound.                     
           </DL>
        <DT>delta: <DD>the absolute value of the violation. 
        <DT>idx:   <DD>the index used by the external solver for the item (most
               likely the row/column number in the problem matrix). This 
               is needed to locate the violation in the solver for reporting
               or investigating the problem.
         <DT>item:  <DD>the violated item, i.e. the constraint or variable.
</DL><P>
 For checking of constraints, a fresh copy of the constraint is made with
 the solution values, as this avoids binding the original variables. Any active
 cutpool constraints in the (logically) last solve of the problem are checked,
 along with the normal constraints. A constraint is considered to be  violated
 if the difference between the sum of the left-hand side and the right-hand side 
 for the constraint is greater than the feasibility tolerance (feasibility_tol)
 parameter for the instance. For variables, a bound is considered violated if
 the solution value falls outside the bound by more than feasibility tolerance,
 and integrality is considered to be violated if the fractional part of the
 soltuion is greater than the integrality parameter for the instance. 
"),
 fail_if: "no solution values are available."
]).

:- comment(piecewise_linear_hull/3, [
template:["piecewise_linear_hull(?X, ++Points, ?Y)",
	  "EplexInstance:piecewise_linear_hull(?X, ++Points, ?Y)"],
args:["X":"Parameter/domain of the piecewise function",
      "Points":"List of points defining the piecewise function",
      "Y":"Result/range of piecewise the function"
     ],
summary: "Relates X and Y according to a piecewise linear function.",
see_also: [_:piecewise_linear/3],
desc: html("\
<P>
This predicate is intended for use with hybrid ic/eplex solving. It
imposes the constraint Y = f(X), where f is a piecewise
linear function defined by Points: the piecewise_linear/3 constraint for
 the ic solver, and a linear relaxation of it for the eplex instance
 EplexInstance. Please see the documentation for
piecewise_linear/3 for details of how the piecewise linear function is
specified.
</P><P>
This predicate extends lib(ic)'s piecewise_linear/3 by also computing the convex hull
of the portions of the constraint which are feasible with respect to the
current bounds (stored with the IC bounds) of X and Y.  Constraints (and
 bounds) defining this convex hull are then
passed to the eplex instance.  Changes in the IC bounds of the variables are
 automatically forwarded to the eplex instance, so that these constraints are updated whenever new
bounds on X or Y change the convex hull. Note that the reverse, i.e. bounds
  changes on the eplex instance,  are NOT forwarded to the IC bounds. 
</P><P>
This implementation of the piecewise
constraint accepts bounded reals in its arguments, but it
does not fully support bounded reals of non-zero width (i.e.
those which do not correspond to a single floating point value).  As a
result, use of such bounded reals is not recommended at this time.</P>
")
]).

:- comment(lp_add_indexed/4, hidden).
:- comment(eplex_solver_setup/5, hidden).


