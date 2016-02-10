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
% Contributor(s): Pascal Brisset and Thom Fruehwirth, ECRC. 
% 
% END LICENSE BLOCK

:- comment(/(chr, 1), [
	summary:"Compile File.chr into a .pl file and load the pl file.

",
	template:"chr(+File)",
	desc:html("   Compile File.chr containing constraint handling rules into a .pl file
   and load the pl file.  Note that even if the file cannot be opened by
   UNIX (Error 170), a nonempty .pl file will be produced and loaded (with
   no effect).

<P>
"),
	args:["+File" : "A file name (with extension chr)."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[5 : "File is not an atom or string.", 170 : "UNIX cannot open the file."],
	eg:"   Success:
[eclipse]: chr(minmax).
minmax.chr compiled traceable 106874 bytes in 3.37 seconds
minmax.pl  compiled traceable 124980 bytes in 1.83 seconds
yes.



",
	see_also:[/(chr2pl, 1)]]).

:- comment(/(chr2pl, 1), [
	summary:"Compile File.chr into a .pl file.

",
	template:"chr2pl(+File)",
	desc:html("   Compile File.chr containing constraint handling rules into a .pl file.
   Note that even if the file cannot be opened by UNIX (Error 170), a
   nonempty .pl file will be produced.

<P>
"),
	args:["+File" : "A file name (with extension chr)."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[5 : "File is not an atom or string.", 170 : "UNIX cannot open the file."],
	eg:"   Success:
[eclipse]: chr(minmax).
minmax.chr compiled traceable 106874 bytes in 3.37 seconds
yes.



",
	see_also:[/(chr, 1)]]).

:- comment(/(chr_get_constraint, 1), [
	summary:"Remove a constraint unifying with Constraint from the constraint store.

",
	template:"chr_get_constraint(?Constraint)",
	desc:html("   Removes a constraint unifying with Constraint from the constraint store.
   Note that if the unification with Constraint binds variables occurring
   also in other constraints in the constraint store, these constraints may
   be simplified (see last examples).  Thus it is recommended to use either
   a free variable or a term with variable arguments for Constraint.  Used
   by advanced constraint handling rules users to manipulate themselves the
   constraints defined by constraint handling rules.

<P>
"),
	args:["?Constraint" : "A constraint (callable term) defined by constraint handling                rules."],
	resat:"   Yes.",
	fail_if:"   Fails if there is no constraint (defined by constraint handling rules)\n   in the constraint store that unifies with Constraint.\n\n",
	eg:"   Example using the constraint handler for Booleans  bool.chr:
[eclipse]: chr_get_constraint(C).
no (more) solution.

[eclipse]: and(X,Y,Z), or(X,Y,Z).

Constraints:
(1) X_g745 * Y_g777 = Z_g809 % pretty print of and/3 constraint
(2) X_g745 + Y_g777 = Z_g809 % pretty print of or/3 constraint

yes.

[eclipse]: and(X,Y,Z), or(X,Y,Z), chr_get_constraint(C).

C = X * Y = Z

Constraints:
(2) X_g765 + Y_g797 = Z_g829
     More? (;)

C = X + Y = Z

Constraints:
(1) X_g765 * Y_g797 = Z_g829
     More? (;)
no (more) solution.

[eclipse]: and(X,Y,Z), or(X,Y,Z), chr_get_constraint(and(1,A,B)).
% or/3 - constraint is solved when X is bound to 1
X = 1
Y = A
Z = 1
A = A
B = 1

[eclipse]: and(X,Y,Z), or(X,Y,Z), chr_get_constraint(and(1,1,0)).
no (more) solution. % or/3 - constraint fails

[eclipse]: and(X,Y,Z), chr_get_constraint(and(1,1,0)).

X = 1
Y = 1
Z = 0


   The predicate chr_labeling/0 can be defined as:
labeling :-
   chr_get_constraint(C),
   chr_label_with(C),
   !,
   chr_resolve(C),
   labeling.

labeling.



",
	see_also:[/(chr_labeling, 0), /(chr_label_with, 1), /(chr_resolve, 1), /(chr_get_constraint, 1), /(chr_get_constraint, 2)]]).

:- comment(/(chr_get_constraint, 2), [
	summary:"Remove a constraint in which the variable Variable occurs and which unifies
with Constraint from the constraint store.

",
	template:"chr_get_constraint(Variable,?Constraint)",
	desc:html("   Removes a constraint in which the variable Variable occurs and which
   unifies with Constraint from the constraint store.  Note that if the
   unification with Constraint binds variables occurring also in other
   constraints in the constraint store, these constraints may be simplified
   (see last examples).  Thus it is recommended to use either a free
   variable or a term with variable arguments for Constraint.  Used by
   advanced constraint handling rules users to manipulate themselves the
   constraints defined by constraint handling rules.  See also
   chr_get_constraint/1 for more examples.

<P>
"),
	args:["Variable" : "A free variable.", "?Constraint" : "A constraint (callable term) defined by constraint handling                rules."],
	resat:"   Yes.",
	fail_if:"   Fails if Variable is not a free variable or if there is no constraint\n   (defined by constraint handling rules) in the constraint store that\n   unifies with Constraint and in which the variable Variable occurs.\n\n",
	eg:"   Example using the constraint handler for Booleans  bool.chr:
[eclipse]: and(X,Y,Z), or(A,B,C), chr_get_constraint(Y,Cstr).

X = X
Z = Z
A = A
B = B
Y = Y
Cstr = X * Y = Z

Constraints:
(2) A + B = C   % pretty print for or/3 - constraint

     More? (;)

no (more) solution.


   The following predicate labeling(+Varlist) labels the variables in the
   list Varlist:
labeling([X|VL]) :-
   var(X),
   chr_get_constraint(X,C),
   chr_label_with(C),
   !,
   chr_resolve(C),
   labeling([X|VL]).

labeling([X|VL]) :-
   labeling(VL).

labeling([]).




",
	see_also:[/(chr_labeling, 0), /(chr_label_with, 1), /(chr_resolve, 1), /(chr_get_constraint, 2)]]).

:- comment(/(chr_label_with, 1), [
	summary:"Checks the label_with declarations of Constraint.

",
	template:"chr_label_with(+Constraint)",
	desc:html("   Checks the label_with declarations of Constraint.  Used by advanced
   constraint handling rules users to write their own labeling procedure
   for the constraints defined by constraint handling rules.

<P>
"),
	args:["+Constraint" : "A chr constraint."],
	resat:"   Yes.",
	fail_if:"   Fails if Constraint is a variable or if Constraint does not have a\n   label_with declaration or if the guard of all unifying label_with\n   declarations fail.\n\n",
	eg:"   Given the following  label_with declaration (from the example
   constraint handler in file  time-pc.chr:
label_with path(N, X, Y, L, T, I) if N>1.

[eclipse]: chr_label_with(path(N,X,Y,L,T,I)).
no (more) solution.

[eclipse]: chr_label_with(path(1,X,Y,L,T,I)).
no (more) solution.

[eclipse]: chr_label_with(path(2,X,Y,L,T,I)).
X = X
Y = Y
L = L
T = T
I = I     More? (;)
no (more) solution.




",
	see_also:[/(chr_labeling, 0), /(chr_resolve, 1), /(chr_get_constraint, 1), /(chr_get_constraint, 2)]]).

:- comment(/(chr_labeling, 0), [
	summary:"Activates the built-in labeling feature for constraint handling rules.

",
	template:"chr_labeling",
	desc:html("   The constraint handling rule run-time system provides built-in labeling
   user-defined constraints.  The built-in labeling is invoked by calling
   the built-in predicate chr_labeling/0.  Once called, whenever no more
   constraint handling is possible, the built-in labeling will choose a
   constraint goal whose label_with declaration is satisfied for labeling.
   It will introduce choices using the clauses of the constraint.

<P>
"),
	args:[],
	resat:"   Yes.",
	fail_if:"   None, only on backtracking.\n\n",
	eg:"
   A query without and with built-in labeling:
[eclipse]: minimum(X,Y,Z), maximum(X,Y,W), Z neq W.

X = _g357
Y = _g389
Z = _g421
W = _g1227

Constraints:
(1) minimum(_g357, _g389, _g421)
(2) _g421 leq _g357
(3) _g421 leq _g389
(4) maximum(_g357, _g389, _g1227)
(5) _g357 leq _g1227
(7) _g389 leq _g1227
(10) _g421 lss _g1227

yes.

[eclipse]: minimum(X,Y,Z), maximum(X,Y,W),
           Z neq W, chr_labeling.

X = Z = _g363
Y = W = _g395

Constraints:
(10) _g363 lss _g395

     More? (;)

X = W = _g363
Y = Z = _g395

Constraints:
(17) _g395 lss _g363
yes.



",
	see_also:[/(chr_label_with, 1), /(chr_resolve, 1), /(chr_get_constraint, 1), /(chr_get_constraint, 2)]]).

:- comment(/(chr_notrace, 0), [
	summary:"Deactivates the standard or Opium debugger extension for constraint
handling rules.

",
	template:"chr_notrace",
	desc:html("   The query chr_trace.  deactivates the standard or opium debugger.  In
   case of the Opium debugger, its window remains until quited.

<P>
"),
	args:[],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"   Success:
[eclipse]: chr_notrace.
yes.
Debugger switched off



",
	see_also:[/(chr_opium, 0), /(chr_trace, 0)]]).

:- comment(/(chr_opium, 0), [
	summary:"Activates the Opium debugger and shows constraint handling.

",
	template:"chr_opium",
	desc:html("   In order to use the Opium debugger, the debug_compile flag must have
   been on (default) during compilation (chr to pl) and loading of the
   produced ECLiPSe  code.  The query chr_opium.  opens an Opium window in
   which the ECLiPSe  code will be traced.  The library chr_opium will be
   automatically loaded.  Note that the Opium debugger for constraint
   handling rules works with X graphic interface.  The OPIUM_WINDOW
   environment variable must thus be set.  An Opium execution can be
   aborted using the a.  command in Opium.

<P>
   Both debuggers display user-defined constraints and application of
   constraint handling rules.  In the Opium debugger, this information
   corresponds to additional ports of the debugger.  The additional ports
   are:

<P>
  * add:  A new constraint is added to the constraint store.

<P>
  * already_in:  A constraint to be added was already present.

<P>
   The ports related to application of rules are:

<P>
  * try_rule:  A rule is tried.

<P>
  * delay_rule:  The last tried rule cannot fire because the guard did not
    succeed.

<P>
  * fire_rule:  The last tried rule fires.

<P>
   The ports related to labeling are:
  * try_label:  A label_with declaration is checked.

<P>
  * delay_label:  The last label_with declaration delays because the guard
    did not succeed.

<P>
  * fire_label:  The last tried label_with declaration succeeds, so the
    clauses of the associated constraint will be used for built-in
    labeling.

<P>
   When displayed, each constraint is labeled with a unique integer
   identifier.  Each rule is labeled with its name as given in the chr
   source using the @ operator.  If a rule does not have a name, it is
   displayed together with a unique integer identifier.

<P>
   See the extension manual chapter on constraint handling rules for more
   information on the Opium scenario used for debugging.

<P>
"),
	args:[],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(chr_trace, 0), /(chr_notrace, 0)]]).

:- comment(/(chr_resolve, 1), [
	summary:"Uses the Prolog clauses to solve a constraint Constraint.

",
	template:"chr_resolve(+Constraint)",
	desc:html("   Uses the Prolog clauses to solve a constraint Constraint.  Used by
   advanced constraint handling rules users to program labeling procedures
   for the constraints defined by constraint handling rules.

<P>
"),
	args:["+Constraint" : "A constraint (callable term) defined by constraint handling                rules."],
	resat:"   Yes.",
	fail_if:"   Fails if there are no Prolog clauses for the constraint or if the bodies\n   of all clauses fail.\n\n",
	exceptions:[4 : "Constraint is a free variable.", 6 : "Constraint is term which is not a constraint (defined by    constraint handling rules)."],
	eg:"   Example using the constraint handler for Booleans in file  bool.chr:
[eclipse]: chr_resolve(X).
instantiation fault in is_predicate(_g671 / _g639)

[eclipse]: chr_resolve(and(X,Y)).
out of range in chr_resolve(and(X,Y))

[eclipse]: chr_resolve(and(X,Y,Z)).

X = 0
Y = Y
Z = 0     More? (;)

X = 1
Y = Z
Z = Z
yes.

[eclipse]: chr_resolve(and(a,b,c)).
no (more) solution.


   The predicate chr_labeling/0 can be defined as:
labeling :-
   chr_get_constraint(C),
   chr_label_with(C),
   !,
   chr_resolve(C),
   labeling.

labeling.



",
	see_also:[/(chr_labeling, 0), /(chr_label_with, 1), /(chr_get_constraint, 1), /(chr_get_constraint, 2)]]).

:- comment(/(chr_trace, 0), [
	summary:"Activates the standard debugger and shows constraint handling.

",
	template:"chr_trace",
	desc:html("   In order to use the standard debugger, the debug_compile must have been
   on (default) during compilation (chr to pl) and loading of the produced
   ECLiPSe  code.  The query chr_trace.  activates the standard debugger
   showing more information about the handling of constraints.  In the
   standard debugger, user-defined constraints are treated as predicates
   and the information about application of constraint handling rules is
   displayed without stopping.  The additional information displayed is:

<P>
  * add:  A new constraint is added to the constraint store.

<P>
  * already_in:  A constraint to be added was already present.

<P>
   The ports related to application of rules are:

<P>
  * try_rule:  A rule is tried.

<P>
  * delay_rule:  The last tried rule cannot fire because the guard did not
    succeed.

<P>
  * fire_rule:  The last tried rule fires.

<P>
   The ports related to labeling are:
  * try_label:  A label_with declaration is checked.

<P>
  * delay_label:  The last label_with declaration delays because the guard
    did not succeed.

<P>
  * fire_label:  The last tried label_with declaration succeeds, so the
    clauses of the associated constraint will be used for built-in
    labeling.

<P>
   When displayed, each constraint is labeled with a unique integer
   identifier.  Each rule is labeled with its name as given in the chr
   source using the @ operator.  If a rule does not have a name, it is
   displayed together with a unique integer identifier.

<P>
"),
	args:[],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"   Success:
[eclipse]: chr_trace.
yes.
Debugger switched on - creep mode
[eclipse]: notrace.     % trace only constraints
Debugger switched off
yes.
[eclipse]: minimum(X,Y,Z), maximum(X,Y,Z).
% trace edited to show only firing rules

ADD (1) minimum(X, Y, Z)
TRY (1) minimum(_g218, _g220, _g222) with propagation
RULE 'propagation' FIRED

 ADD (2) leq(_g665, _g601)

 ADD (3) leq(_g665, Var)

ADD (4) maximum(_g601, Var, _g665)
TRY (4) maximum(_g601, Var, _g665) with propagation
RULE 'propagation' FIRED

 ADD (5) leq(_g601, _g665)
 TRY (5) leq(_g601, _g665) (2) leq(_g665, _g601) with antisymmetry
 RULE 'antisymmetry' FIRED

TRY (4) maximum(_g601, Var, _g601) with max_eq
RULE 'max_eq' FIRED

 ADD (6) leq(Var, _g601)
 TRY (3) leq(_g601, Var) (6) leq(Var, _g601) with antisymmetry
 RULE 'antisymmetry' FIRED

TRY (1) minimum(_g601, _g601, _g601) with min_eq
RULE 'min_eq' FIRED

 ADD (7) leq(_g601, _g601)
 TRY (7) leq(_g601, _g601) with reflexivity
 RULE 'reflexivity' FIRED

X = Y = Z = _g558
yes.



",
	see_also:[/(chr_opium, 0), /(chr_notrace, 0)]]).
