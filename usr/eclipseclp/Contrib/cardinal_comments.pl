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
% The Original Code is  The Cardinal Constraint Solver for ECLiPSe. 
% The Initial Developer of the Original Code is  Francisco M.C.A. Azevedo. 
% Portions created by the Initial Developer are  Copyright (C) 2004.
% All Rights Reserved.
% 
% Contributor(s): Francisco M. C. A. Azevedo <fa@di.fct.unl.pt>. 
% 
% Alternatively, the contents of this file may be used under the terms of
% either of the GNU General Public License Version 2 or later (the "GPL"),
% or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
% in which case the provisions of the GPL or the LGPL are applicable instead
% of those above. If you wish to allow use of your version of this file only
% under the terms of either the GPL or the LGPL, and not to allow others to
% use your version of this file under the terms of the MPL, indicate your
% decision by deleting the provisions above and replace them with the notice
% and other provisions required by the GPL or the LGPL. If you do not delete
% the provisions above, a recipient may use your version of this file under
% the terms of any one of the MPL, the GPL or the LGPL.
% END LICENSE BLOCK

%icompile(cardinal), ecis_to_htmls('.','.','')

:-comment(desc, html("Cardinal is a sets constraints library with especial inferences
	on sets cardinality and other optional set functions (minimum and maximum for
	sets of integers, and union for sets of sets.)
	<P>
	A set is naturally used to collect distinct elements sharing some property.
	Combinatorial search problems over these data structures can thus be naturally
	modelled by high level languages with set abstraction facilities, and efficiently
	solved if constraint reasoning prunes search space when the sets are not fully
	known a priori (i.e. they are variables ranging over a set domain).
	<P>
	Many complex relations between sets can be expressed with constraints such as set
	inclusion, disjointness and equality over set expressions that may include such
	operators as intersection, union or difference of sets. Also, as it is often the
	case, one is not interested simply on these relations but on some attribute or
	function of one or more sets (e.g. the cardinality of a set). For instance, the
	goal of many problems is to maximise or minimise the cardinality of a set. Even
	for satisfaction problems, some sets, although still variables, may be constrained
	to a fixed cardinality or a stricter cardinality domain than just the one inferred
	by the domain of a set variable (for instance, the cardinality of a set may have
	to be restricted to be an even number).
	<P>
	Cardinal represents set variables by set intervals with a lower and an upper bound
	considering set inclusion as a partial ordering. Consistency techniques are then
	applied to set constraints by interval reasoning. A set domain variable S may be
	specified by an interval [A,B] where A and B are known sets ordered by set inclusion,
	representing the greatest lower bound and the lowest upper bound of S, respectively.
	<P>
	The cardinality of a set S, given as a finite domain variable C (#S=C), is not a
	bijective function since two distinct sets may have the same cardinality. Still,
	it can be constrained by the cardinalities of the set bounds.
	<P>
	A simple inference that can be done using cardinality information is to instantiate
	the set to one of the set bounds, when it is known that the set cardinality must be
	equal to the cardinality of that bound. But Cardinal does much more than that.
	For instance, consider two set variables
	S1,S2, that can assume either set value {} (empty set) or {a,b}. Their set domain
	is thus [{},{a,b}] with cardinality 0 or 2. The intersection of S1 and S2 also
	yelds set domain [{},{a,b}]. But we need a special inference to conclude that the
	intersection cardinality is also either 0 or 2 (it can not be 1). Set solvers
	other than Cardinal do not make such inferences.
	<P>
	Inferences using cardinalities can be very useful to deduce more rapidly the
	non-satisfiability of a set of constraints, thus improving efficiency of
	combinatorial search problem solving. As another simple example, if Z is known to be
	the set difference between Y and X, both contained in set {a,b,c,d}, and it is known
	that X has exactly 2 elements, it should be inferred that the cardinality of Z can
	never exceed 2 elements (i.e. from X,Y in {a,b,c,d}, #X=2, Z=Y\\X it should be
	inferred that #Z =< 2). A failure could thus be immediately detected upon the
	posting of a constraint such as #Z=3.
	<P>
	Inference capabilities such as these are particularly important when solving set
	problems where cardinality plays a special role. Cardinal thus fully uses
	constraint propagation on sets cardinality.
	<P>

<B>Intervals and Lattices</B>
	<P>
	Set intervals define a lattice of sets. The set inclusion relation between two
	sets defines a partial order on P(U), the powerset over a certain universe U,
	the set of all subsets of U.
	<P>
	Due to the transitivity rule, the top set, U, includes all sets of P(U);
	while the bottom set, {}, is included in all sets of P(U). Consequently,
	sets U and {} constitute an upper bound and a lower bound of P(U), respectively.
	In addition, they are the least upper bound (lub) or join, and the greatest lower
	bound (glb) or meet of P(U), since there is no other upper bound contained in
	(‘less’ than) U  nor other lower bound containing (‘greater’ than) the empty set {}.
	<P>
	Let us now consider for U={a,b,c,d}, the sub-lattice connecting {a,b,d} and {b}
	(thus also including sets {a,b} and {b,d}). Sets {} and {a,b,c,d} are still a
	lower and an upper bound, but this time the glb is {b} and the lub is {a,b,d}.
	<P>
	The two bounds (glb and lub) define a set interval (e.g. [{b},{a,b,d}]) and may
	form the domain of a set variable S, meaning that set S is one of those defined
	by its interval (lattice); all other sets outside this domain are excluded from
	the solution. Thus, b is definitely an element of S, while a and d are the only
	other possible elements.
	<P>
	Set interval reasoning allows us to apply consistency techniques such as Bounded
	Arc Consistency, due to the monotonic property of set inclusion.
	<P>
	Any set variable must then have a domain consisting of a set interval. In addition,
	this interval should be kept as small as possible, in order to discard all sets
	that are known not to belong to the solution, while not loosing any of the still
	possible values (sets). The smallest such domain is the one with equal glb and lub,
	i.e. a domain of the form [B,B], corresponding to a constant set B. For a set
	variable that can assume any set value from a collection of known sets, such as
	{{a,b},{a,c},{d}}, the corresponding interval is the convex closure of such
	collection (which in this case is the set interval [{},{a,b,c,d}]). In general,
	for n possible arbitrary sets S1...Sn, the corresponding set variable X has an
	interval domain [glb, lub] where glb is the intersection of all S1...Sn, and lub
	is their union.
	<P>
<B>Implementation Notes</B>
	<P>
	In Cardinal, all sets are represented as sorted lists, which eases working with
	sets and lists interchangeably.
	<P>
	Set variable bounds are represented by its glb and its lub\\glb, the set of
	additional possible elements, which we refer to as poss.
	<P>
	Cardinal implements a number of set constraints such as inclusion, equality,
	inequality, membership, disjointness, and even complement, together with set
	operations (union, intersection and difference), as built-in.
	<P>
	As mentioned, Cardinal also allows the definition and use of optional set functions
	(other than cardinality): minimum and maximum, for sets of integers, and union,
	for sets of sets. Refer to the available predicates for details.
")).





:-comment(cardinality/2, [
	amode: cardinality(?,?),
	args:  ["SetVariable": "A Set (variable or ground).",
		"Cardinality": "An integer or an FD variable."],
	summary: "Cardinality of a set",
	desc: html("Cardinality is the cardinality of SetVariable. If Cardinality is given
		(as an integer or FD variable), then SetVariable is constrained to have such cardinality.
		If Cardinality is a free variable, then it is unified with the set's cardinality as
		an FD variable or an integer (if it is already known)."),
	resat: "No.",
	fail_if: "Fails if Cardinality can not be the cardinality of SetVariable.",
	eg:"
?- S `::[]..[a,b], cardinality(S,C).
?- S `::[]..[a,b], cardinality(S,1).
?- S `::[]+[a,b]:1, cardinality(S,C).
C = 1

?- cardinality([a,b],C).
C = 2

?- S `::[c]+[a,b]:[1,3], C #> 1, cardinality(S,C).
S = [a,b,c]
C = 3",
	see_also:[(#)/2]
	]).


:-comment((#)/2, [
	amode: #(?,?),
	args:  ["SetExpression": "A Set expression.",
		"Cardinality": "An integer or an FD variable."],
	summary: "Cardinality of a set expression",
	desc: html("Cardinality is the cardinality of SetExpression, a set term possibly
		including set operators `/\\, `\\/ and `\\ (or \\). <P>
		SetExpression is first evaluated into a ground set or a set variable and then
		its Cardinality is applied as in cardinality/2."),
	resat: "No.",
	fail_if: "Fails if Cardinality can not be the cardinality of SetExpression.",
	eg:"
?- S `::[]..[a,b], #(S,C).
?- S `::[]..[a,b], #(S `/\\ [b,c],1).
?- S `::[]..[a,b], #([b,c] `\\/ S `\\ [a,z] `/\\ [g], C).
C = 2,
?- S `::[]..[a,b], #(([b,c] `\\/ S `\\ [a,z]) `/\\ [g], C).
C = 0
?- S1 `::[]..[a,b], S2 `::[]..[b,c,d], #(S1 `/\\ S2, 2).
no
",
	see_also:[cardinality/2,(`=)/2]
	]).



:-comment(glb/2, [
	amode: glb(?,-),
	args:  ["SetVariable": "A set variable.",
		"Glb": "A set."],
	summary: "Obtaining a set's glb",
	desc: html("Glb is unified with the (greatest) lower bound of SetVariable."),
	resat: "No.",
	fail_if: "Fails if Glb can not be unified with the current glb of SetVariable.",
	eg:"
?- S `::[c]+[a,b], glb(S,G).
G = [c]
",
	see_also:[glb_poss/3,domain/2,domain/3,lub/2,lub/4,poss/2]
	]).


:-comment(poss/2, [
	amode: poss(?,-),
	args:  ["SetVariable": "A set variable.",
		"Poss": "A set."],
	summary: "Obtaining the still possible elements of a set (lub\\glb)",
	desc: html("Poss is unified with the set of still possible elements of SetVariable
		(i.e. its lub\\glb).<P>
		If SetVariable is a set of sets and a union function attribute has been set,
		then each element of Poss comes annotated with its respective length."),
	resat: "No.",
	fail_if: "Fails if Poss can not be unified with the current poss (lub\\glb) of SetVariable.",
	eg:"
?- S `::[c]+[a,b], poss(S,P).
P = [a,b]

?- S `::[[c]]+[[a,b]], poss(S,P).
P = [[a,b]]

?- set(S, [],[[a,b],[b,c],[a,c],[b]],[union:[a,b,c]]), poss(S,P).
P = [[a, b] : 2, [a, c] : 2, [b] : 1, [b, c] : 2]
",
	see_also:[glb/2,glb_poss/3,domain/2,domain/3,lub/2,lub/4]
	]).


:-comment(glb_poss/3, [
	amode: glb_poss(?,-,-),
	args:  ["SetVariable": "A set variable.",
		"Glb": "A set.",
		"Poss": "A set."],
	summary: "Obtaining both the glb and the still possible elements of a set",
	desc: html("Glb is unified with the (greatest) lower bound of SetVariable.<P>
		Poss is unified with the set of still possible elements of SetVariable
		(i.e. its lub\\glb).<P>
		If SetVariable is a set of sets and a union function attribute has been set,
		then each element of Poss comes annotated with its respective length."),
	resat: "No.",
	fail_if: "Fails if Glb can not be unified with the current glb of SetVariable or
		if Poss can not be unified with the current poss (lub\\glb) of SetVariable.",
	eg:"
?- S `::[c]+[a,b], glb_poss(S,G,P).
G = [c]
P = [a,b]

?- set(S, [],[[a,b],[b,c],[a,c],[b]],[union:[a,b,c]]), glb_poss(S,G,P).
G = []
P = [[a, b] : 2, [a, c] : 2, [b] : 1, [b, c] : 2]
",
	see_also:[glb/2,poss/2,domain/2,domain/3,lub/2,lub/4]
	]).


:-comment(domain/2, [
	amode: domain(?,-),
	args:  ["SetVariable": "A set variable.",
		"Domain": "A list (pair) with glb and poss."],
	summary: "Accessing the domain of a set",
	desc: html("Domain is unified with the domain of SetVariable in the form [Glb:NIn,Poss:NMax],
		where Glb is the (greatest) lower bound of SetVariable, and NIn its length,
		Poss is the set of still possible elements of SetVariable (i.e. its lub\\glb),
		and NMax is the lub's cardinality (i.e. NIn + #(Poss)).<P>
		If SetVariable is a set of sets and a union function attribute has been set,
		then each element of Poss comes annotated with its respective length."),
	resat: "No.",
	fail_if: "Fails if Domain can not be unified with the current domain of SetVariable.",
	eg:"
?- S `::[c]+[a,b], domain(S,D).
D = [[c]:1, [a,b]:3]

?- set(S, [],[[a,b],[b,c],[a,c],[b]],[union:[a,b,c]]), domain(S,D).
D = [[]:0, [[a,b]:2, [a,c]:2, [b]:1, [b,c]:2]:4]
",
	see_also:[domain/3,glb/2,poss/2,glb_poss/3,lub/2,lub/4]
	]).


:-comment(domain/3, [
	amode: domain(?,?,?),
	args:  ["SetVariable": "A set variable.",
		"Cardinality": "An FD variable",
		"Domain": "A list (pair) with glb and poss."],
	summary: "Accessing the domain of a set",
	desc: html("Domain is unified with the domain of SetVariable (which has cardinality
		Cardinality) in the form [Glb:NIn,Poss:NMax],
		where Glb is the (greatest) lower bound of SetVariable, and NIn its length,
		Poss is the set of still possible elements of SetVariable (i.e. its lub\\glb),
		and NMax is the lub's cardinality (i.e. NIn + #(Poss)).
		<P>
		If SetVariable is a set of sets and a union function attribute has been set,
		then each element of Poss comes annotated with its respective length.
		<P>
		Use domain/3 instead of domain/2 whenever Cardinality variable is available,
		for efficiency reasons, since in the case of SetVariable being already
		ground, it is not neccessary to recalculate its length (to retrieve
		[Setvariable:Cardinality,[]:Cardinality]. This is due to the loss of
		attributes of variables when these become instantiated.
		<P>
		Cardinality should be input to domain/3. Do not use this predicate to
		retrieve the cardinality of a set, for it will only work when set is ground."),
	resat: "No.",
	fail_if: "Fails if Domain can not be unified with the current domain of SetVariable.",
	eg:"
?- S `::[c]+[a,b]:C, domain(S,C,D).
D = [[c]:1, [a,b]:3]

?- S `::[c]+[a,b]:C, S=[a,c], domain(S,C,D).
D = [[a,c]:2, []:2]

?- set(S, [],[[a,b],[b,c],[a,c],[b]],[union:[a,b,c],cardinality:C]), domain(S,C,D).
D = [[]:0, [[a,b]:2, [a,c]:2, [b]:1, [b,c]:2]:4]
",
	see_also:[domain/2,glb/2,poss/2,glb_poss/3,lub/2,lub/4,cardinality/2,(#)/2]
	]).


:-comment(lub/2, [
	amode: lub(?,-),
	args:  ["SetVariable": "A set variable.",
		"Lub": "A set."],
	summary: "Obtaining a set's lub",
	desc: html("Lub is unified with the (least) upper bound of SetVariable."),
	resat: "No.",
	fail_if: "Fails if Lub can not be unified with the current lub of SetVariable.",
	eg:"
?- S `::[c]+[a,b], lub(S,L).
L = [a,b,c]
",
	see_also:[lub/4,glb/2,poss/2,glb_poss/3,domain/2,domain/3]
	]).


:-comment(lub/4, [
	amode: lub(?,-,-,-),
	args:  ["SetVariable": "A set variable.",
		"Glb": "A set.",
		"Poss": "A set.",
		"Lub": "A set."],
	summary: "Obtaining a set's lub, together with its glb and poss (lub\\glb)",
	desc: html("Lub is unified with the (least) upper bound of SetVariable.<P>
		Glb is unified with the (greatest) lower bound of SetVariable.<P>
		Poss is unified with the set of still possible elements of SetVariable
		(i.e. its lub\\glb).<P>
		If SetVariable is a set of sets and a union function attribute has been set,
		then each element of Poss comes annotated with its respective length."),
	resat: "No.",
	fail_if: "Fails if Lub can not be unified with the current lub of SetVariable or
		if Glb can not be unified with the current glb of SetVariable or
		if Poss can not be unified with the current poss (lub\\glb) of SetVariable.",
	eg:"
?- S `::[c]+[a,b], lub(S,G,P,L).
G = [c]
P = [a,b]
L = [a,b,c]

?- set(S, [],[[a,b],[b,c],[a,c],[b]],[union:[a,b,c]]), lub(S,G,P,L).
G = []
P = [[a, b] : 2, [a, c] : 2, [b] : 1, [b, c] : 2]
L = [[a, b], [a, c], [b], [b, c]]
",
	see_also:[lub/2,glb_poss/3,glb/2,poss/2,domain/2,domain/3]
	]).


:-comment(maximum/2, [
	amode: maximum(?,?),
	args:  ["SetVariable": "A Set (variable or ground) of integers.",
		"Max": "An integer or an FD variable."],
	summary: "Maximum of a set of integers",
	desc: html("Max is the maximum (i.e. the highest element) of SetVariable.<P>
		If Max is given (as an integer or FD variable) then SetVariable is
		constrained to have such maximum.
		If Max is a free variable, then it is unified with the set's maximum as
		an FD variable or an integer (if it is already known).<P>
		maximum/2 can thus be used either to declare (or constrain) a maximum
		function or to retrieve it."),
	resat: "No.",
	fail_if: "Fails if Max can not be the maximum of SetVariable.",
	eg:"
?- S`::[]..[1,2], maximum(S,M).
?- set(S,[],[1,2],[maximum:2], maximum(S,M).
M = 2

?- S`::[]+[1,2], maximum(S,1).
S = [1]

?- set(S,[],[1,2],[maximum:1], maximum(S,M).
S = [1]
M = 1",
	see_also:[minimum/2,set/4,sets/4,cardinality/2]
	]).


:-comment(minimum/2, [
	amode: minimum(?,?),
	args:  ["SetVariable": "A Set (variable or ground) of integers.",
		"Min": "An integer or an FD variable."],
	summary: "Minimum of a set of integers",
	desc: html("Min is the minimum (i.e. the lowest element) of SetVariable.<P>
		If Min is given (as an integer or FD variable) then SetVariable is
		constrained to have such minimum.
		If Min is a free variable, then it is unified with the set's minimum as
		an FD variable or an integer (if it is already known).<P>
		minimum/2 can thus be used either to declare (or constrain) a minimum
		function or to retrieve it."),
	resat: "No.",
	fail_if: "Fails if Min can not be the minimum of SetVariable.",
	eg:"
?- S`::[]..[1,2], minimum(S,M).
?- set(S,[],[1,2],[minimum:1], minimum(S,M).
M = 1

?- S`::[]+[1,2], minimum(S,2).
S = [2]

?- set(S,[],[1,2],[minimum:2], minimum(S,M).
S = [2]
M = 2",
	see_also:[maximum/2,set/4,sets/4,cardinality/2]
	]).


:-comment(union_var/2, [
	amode: union_var(?,?),
	args:  ["SetVariable": "A Set (variable or ground) of sets.",
		"UnionVar": "A Set (variable or ground)."],
	summary: "Union of a set of sets",
	desc: html("UnionVar is the union of sets in SetVariable. If UnionVar is given
		(as a ground set or a set variable), then SetVariable is constrained to have such union.
		If UnionVar is a free variable, then it is unified with the set's union as
		a set variable or a ground set (if it is already known).<P>
		union_var/2 can thus be used either to declare (or constrain) a union
		function or to retrieve it."),
	resat: "No.",
	fail_if: "Fails if UnionVar can not be the union of SetVariable.",
	eg:"
?- S `::[]..[[a],[b]], union_var(S,U).
?- S `::[]..[[a],[b],[a,b]], union_var(S,[a,b]).
?- union_var([[a,b],[b,c]], U).
U = [a,b,c]",
	see_also:[set/4,sets/4,cardinality/2]
	]).


/*
:-comment(union_att/2, [
	amode: union_att(?,-),
	args:  ["SetVariable": "A Set (variable or ground) of sets.",
		"UnionAtt": "A Set in the form [UnionVar,GlbU+PossU,Singles,Lengths]."],
	summary: "Union Attribute of a set of sets",
	desc: html("UnionAtt is the union attribute of SetVariable, concerning its union function,
		a list with the following data:<P>
		UnionVar: union of sets in SetVariable<P>
		GlbU: set union of SetVariable's glb<P>
		PossU: set of possible union elements with counters (X:N), i.e. an ordered
			list of all elements in the sets of SetVariable's poss (lub\\glb)
			with the number of occurrences attached<P>
		Singles: elements where N=1 in PossU<P>"),
	resat: "No.",
	fail_if: "Fails if SetVariable is not a set of sets or if its union attributte
		does not unify with UnionAtt.",
	eg:"
?- S `::[]..[[a],[b]], union_att(S,UAtt).
?- union_att([[a],[b]], UAtt).
UAtt = [[a,b],[a,b]+[],[],[]]

?- S `::[]..[[a],[b],[a,b],[c]], union_att(S,[_,GlbU+PossU,Singles,Lengths]).
Glb = []
PossU = [a:2,b:2,c:1]
Singles = [c]
Lengths = [2:1, 1:3]",
	see_also:[union_var/2,union_att/3,set/4,sets/4,cardinality/2]
	]).

%:-comment(union_att/3, [...

*/




:-comment((`::)/2, [
%	amode: `::(?,+),
%	amode: (? `:: +),
	template: "?SetVariable `:: ?Domain",
	args:  ["SetVariable": "A variable.",
		"Domain": "A set domain with optional cardinality declaration."],
	summary: "Set variable declaration",
	desc: html("Declare or constrain a set domain variable to have Domain as domain.<P>
		Domain may assume 3 forms: Glb..Lub, Glb+Poss or Glb+Poss:Cardinality.<P>
		Glb is a ground set denoting the SetVariable's glb. Lub is a ground set
		denoting the SetVariable's lub. Poss is a ground set denoting the SetVariable's
		poss (lub\\glb). Cardinality is the SetVariable's cardinality, which may be
		an integer, an FD variable, or an integer domain (list or range)."),
	resat: "No.",
	fail_if: "Fails if SetVariable can not be constrained accordingly.",
	eg:"
?- S `:: []..[a,b].
?- S `:: []+[a,b].
?- S `:: []+[a,b]:1.
?- S `:: [x]+[a,b]:C.
?- S `:: []+[a,b]:[0,2].
?- S `:: [c]+[a,b,d,e,f,g,h,i,j,k]:[2,4..7].
",
	see_also:[set/4,sets/4,cardinality/2,union_var/2,minimum/2,maximum/2,set_labeling/1]
	]).



:-comment(set/4, [
	amode: set(?,++,++,+),
	args:  ["SetVariable": "A variable.",
		"Glb": "A ground set.",
		"Poss": "A ground set.",
		"Functions": "A list."],
	summary: "Set variable declaration with optional functions",
	desc: html("Declare or constrain a set domain variable to have Glb as assured
		elements and Poss as the possible additional elements.<P>
		Functions is a list of functions over SetVariable in the form
		FunctionName:FunctionValue, where FunctionName can be 'cardinality',
		'minimum', 'maximum' or 'union':
<PRE>
  cardinality: FunctionValue can be an integer, an FD variable or an integer domain (list or range)
  union: (SetVariable must be a set of sets.) FunctionValue can be a set, a set variable
	or a set domain in the form GlbUnion+PossUnion, representing the glb and poss of
	the union of SetVariable
  minimum and maximum: (SetVariable must be a nonempty set of integers.)
	FunctionValue can be an integer, an FD variable or an integer domain
</PRE>
		Cardinal inferences over SetVariable and its union, minimum and maximum
		functions will be performed only if these functions are explicitly
		declared, whereas the cardinality function and respective inferences
		will always be present even if this (cardinality) function is not
		explicitly declared. Note that a simple function declaration such as
		minimum:_ is sufficient to make it 'active'."),
	resat: "No.",
	fail_if: "Fails if SetVariable can not be constrained accordingly.",
	eg:"
?- set(S,[],[a,b],[]).
?- set(S,[],[a,b],[cardinality:1]).
?- set(S,[],[a,b],[cardinality:C]).
?- set(S,[],[a,b],[cardinality:[0,2]]).
?- set(S,[c],[a,b,d,e,f,g,h,i,j,k],[cardinality:[2,4..7]]).
?- set(S,[],[1,3,4,5,7],[minimum:Min,maximum:Max]), fd:(Max #> Min+2).
?- set(S, [], [[1,2,5],[2,4],[3,5],[1,3,4]],
	[cardinality:2, union:[1,2,3,4,5]]).   %set-covering
?- set(S, [], [[1,2,5],[2,4],[3,5],[1,3,4]], [union:[1]+[2,4,5]]).
",
	see_also:[sets/4,(`::)/2,cardinality/2,union_var/2,minimum/2,maximum/2,set_labeling/1]
	]).


:-comment(sets/4, [
	amode: sets(+,++,++,+),
	args:  ["SetVariables": "A list of variables.",
		"Glb": "A ground set.",
		"Poss": "A ground set.",
		"Functions": "A list."],
	summary: "Set variables declaration with optional functions",
	desc: html("Declare or constrain set domain variables to have Glb as assured
		elements and Poss as the possible additional elements.<P>
		Functions is a list of functions over each SetVariable in SetVariables in the form
		FunctionName:FunctionValue, where FunctionName can be 'cardinality',
		'minimum', 'maximum' or 'union':<P>
<PRE>
  cardinality: FunctionValue can be an integer, an FD variable or an integer domain (list or range)
  union: (SetVariable must be a set of sets.) FunctionValue can be a set, a set variable
	or a set domain in the form GlbUnion+PossUnion, representing the glb and poss of
	the union of SetVariable
  minimum and maximum: (SetVariable must be a nonempty set of integers.)
	FunctionValue can be an integer, an FD variable or an integer domain
</PRE>
		Cardinal inferences over SetVariable and its union, minimum and maximum
		functions will be performed only if these functions are explicitly
		declared, whereas the cardinality function and respective inferences
		will always be present even if this (cardinality) function is not
		explicitly declared. Note that a simple function declaration such as
		minimum:_ is sufficient to make it 'active'.<P>
		If a FunctionValue is given as a variable or as a fixed (integer or set)
		value, then it will be the same for all of SetVariables. If it is given
		as a domain, then function values for SetVariables may be different
		(a different domain variable is created for each SetVariable)."),
	resat: "No.",
	fail_if: "Fails if SetVariables can not be constrained accordingly.",
	eg:"
?- sets([S],[],[a,b],[]).
?- sets([S,T],[],[a,b],[cardinality:1]).
?- sets([X,Y,Z],[],[a,b],[cardinality:C]).
?- sets([X,Y,Z],[],[a,b],[cardinality:[0,2]]).
?- sets([X,Y,Z],[c],[a,b,d,e,f,g,h,i,j,k],[cardinality:[2,4..7]]).
?- sets([X,Y,Z],[],[1,3,4,5,7],[minimum:Min,maximum:1..9]), fd:(Max #> Min+2).
?- sets([X,Y,Z], [], [[1,2,5],[2,4],[3,5],[1,3,4]], [union:[1]+[2,4,5]]).
",
	see_also:[set/4,(`::)/2,cardinality/2,union_var/2,minimum/2,maximum/2,set_labeling/1]
	]).



:-comment(refine/2, [
	amode: refine(++,?),
	args:  ["UpDown": "Atom ('up' or 'down').",
		"SetVar": "A set variable."],
	summary: "Refine a set variable's domain",
	desc: html("Pick the first element of SetVar's poss (lub\\glb) and try to include it
		in its glb, or to definitely exclude it from the domain.<P>
		If heuristic UpDown is 'up' then inclusion is tried first; otherwise (down)
		exclusion is tried first."),
	resat: "Yes.",
	fail_if: "Fails if Var can not be refined (it is either ground or both the inclusion
		and exclusion of the first element of its poss leads to a failure due to
		unsatisfied constraints).",
	eg:"
?- S `:: [a]+[b,c], refine(up,S), glb_poss(S,G,P).
G = [a,b], P = [c] ;
G = [a], P = [c] ;
no

?- S `:: [a]+[b,c], refine(down,S), glb_poss(S,G,P).
G = [a], P = [c] ;
G = [a,b], P = [c] ;
no
",
	see_also:[set_labeling/2,set_labeling/1]
	]).


:-comment(set_labeling/2, [
	amode: set_labeling(++,+),
	args:  ["UpDown": "Atom: 'up' or 'down'.",
		"SetVars": "List of set variables."],
	summary: "Label set variables",
	desc: html("Instantiate all variables in SetVars from first to last, with
		consecutive refinements of their domains until they are ground.<P>
		If heuristic UpDown is 'up' then, for each set variable, for each element
		in its poss (lub\\glb), inclusion is tried first; otherwise (down)
		exclusion is tried first."),
	resat: "Yes.",
	fail_if: "Fails if SetVars can not be labeled (there is no solution to the CSP).",
	eg:"
?- S `:: [a] + [b, c], T `:: [1] + [2], set_labeling(up, [S, T]).
S = [a, b, c], T = [1, 2] ;
S = [a, b, c], T = [1] ;
S = [a, b], T = [1, 2] ;
S = [a, b], T = [1] ;
S = [a, c], T = [1, 2] ;
S = [a, c], T = [1] ;
S = [a], T = [1, 2] ;
S = [a], T = [1] ;
no

?- S `:: [a] + [b, c], T `:: [1] + [2], set_labeling(down, [S, T]).
S = [a], T = [1] ;
S = [a], T = [1, 2] ;
S = [a, c], T = [1] ;
S = [a, c], T = [1, 2] ;
S = [a, b], T = [1] ;
S = [a, b], T = [1, 2] ;
S = [a, b, c], T = [1] ;
S = [a, b, c], T = [1, 2] ;
no
",
	see_also:[set_labeling/1,refine/2]
	]).


:-comment(set_labeling/1, [
	amode: set_labeling(?),
	args:  ["SetVars": "A variable or a list of set variables."],
	summary: "Label set variable(s)",
	desc: html("Instantiate all variables in SetVars from first to last, with
		consecutive refinements of their domains until they are ground.<P>
		For each set variable, for each element in its poss (lub\\glb), inclusion
		is tried first.<P>
		SetVars can be a set variable instead of a list. Labeling a single set
		variable S can thus be done both with set_labeling([S]) or with
		set_labeling(S)."),
	resat: "Yes.",
	fail_if: "Fails if SetVars can not be labeled (there is no solution to the CSP).",
	eg:"
?- S `:: [a] + [b, c], T `:: [1] + [2], set_labeling([S, T]).
S = [a, b, c], T = [1, 2] ;
S = [a, b, c], T = [1] ;
S = [a, b], T = [1, 2] ;
S = [a, b], T = [1] ;
S = [a, c], T = [1, 2] ;
S = [a, c], T = [1] ;
S = [a], T = [1, 2] ;
S = [a], T = [1] ;
no

?- S `:: [a] + [b, c], T `:: [1] + [2], set_labeling(S).
S = [a, b, c] ;
S = [a, b] ;
S = [a, c] ;
S = [a] ;
no

",
	see_also:[set_labeling/2,refine/2]
	]).



:-comment(card_labeling/1, [
	amode: card_labeling(?),
	args:  ["SetVars": "A list of set variables."],
	summary: "Label cardinality of set variables",
	desc: html("Instantiate all cardinalities of variables in SetVars from first to
		last, using indomain/1 predicate of fd library.<P>
		card_labeling/1 is defined as:
<PRE>
  card_labeling([]).
  card_labeling([H|T]):-
	cardinality(H, C),
	indomain(C),
	card_labeling(T).
</PRE>
"),
	resat: "Yes.",
	fail_if: "Fails if cardinalities of SetVars can not be labeled (because constraint
		propagation leads to a failure).",
	eg:"
?- S `:: [a]+[b,c]:CS, T `:: [1] + [2], card_labeling([S, T]).
CS = 1, S = [a], T = [1] ;
CS = 1, S = [a], T = [1, 2] ;
CS = 2, T = [1] ;
CS = 2, T = [1, 2] ;
CS = 3, S = [a,b,c], T = [1] ;
CS = 3, S = [a,b,c], T = [1, 2] ;
no

?- S `:: [a] + [b,c,d,e,f]:[2,3,6,9], card_labeling([S]), cardinality(S,C).
C = 2 ;
C = 3 ;
C = 6, S = [a,b,c,d,e,f] ;
no

",
	see_also:[set_labeling/2,refine/2,cardinality/2]
	]).



:-comment((`@)/2, [
%	amode: `@(?,?),
%	amode: (? `@ ?),
	template: "?Element `@ ?SetVariable",
	args:  ["SetVariable": "A set variable.",
		"Element": "A ground term or a variable."],
	summary: "Set membership constraint",
	desc: html("Constrain SetVariable to include Element.<P>
		If Element is a variable then if SetVariable is a ground singleton,
		then Element is unified with its single element, otherwise the constraint
		is suspended until Element or SetVariable is ground."),
	resat: "No.",
	fail_if: "Fails if Element can not be a member of SetVariable.",
	eg:"
?- S `:: []..[a,b], a `@ S, glb_poss(S,G,P).
G = [a], P = [b]

?- S `:: []+[a,b], c `@ S.
no

?- S `:: [a]+[b,c], a `@ S, glb_poss(S,G,P).
G = [a], P = [b,c]

?- S `:: []..[a,b], X `@ S, glb_poss(S,G,P).
G = [], P = [a,b]

?- S `:: []..[a,b], X `@ S, X=b, glb_poss(S,G,P).
G = [b], P = [a]

?- S `:: [a]+[b,c]:C, X `@ S, C=1.
X = a
",
	see_also:[in/2,(`-@)/2,notin/2,(`::)/2]
	]).


:-comment(in/2, [
%	amode: in(?,?),
%	amode: (? in ?),
	template: "?Element in ?SetVariable",
	args:  ["SetVariable": "A set variable.",
		"Element": "A ground term or a variable."],
	summary: "Set membership constraint",
	desc: html("Constrain SetVariable to include Element.<P>
		in/2 is available for compatibility with conjunto library syntax.
		It is equivalent to the preferred `@/2. See its description for details."),
	resat: "No.",
	fail_if: "Fails if Element can not be a member of SetVariable.",
	see_also:[(`@)/2,(`-@)/2,notin/2,(`::)/2]
	]).



:-comment((`-@)/2, [
%	amode: `-@(?,?),
%	amode: (? `-@ ?),
	template: "?Element `-@ ?SetVariable",
	args:  ["SetVariable": "A set variable.",
		"Element": "A ground term or a variable."],
	summary: "Set non-membership constraint",
	desc: html("Constrain SetVariable to not include Element.<P>
		If Element is a variable then the constraint
		is suspended until it becomes ground."),
	resat: "No.",
	fail_if: "Fails if Element must be a member of SetVariable.",
	eg:"
?- S `:: []..[a,b], a `-@ S, glb_poss(S,G,P).
G = [], P = [b]

?- S `:: [c]+[a,b], c `-@ S.
no

?- S `:: [a]+[b,c], z `-@ S, glb_poss(S,G,P).
G = [a], P = [b,c]

?- S `:: []..[a,b], X `-@ S, glb_poss(S,G,P).
G = [], P = [a,b]

?- S `:: []..[a,b], X `-@ S, X=b, glb_poss(S,G,P).
G = [], P = [a]
",
	see_also:[(`@)/2,in/2,notin/2,(`::)/2]
	]).


:-comment(notin/2, [
%	amode: notin(?,?),
%	amode: (? notin ?),
	template: "?Element notin ?SetVariable",
	args:  ["SetVariable": "A set variable.",
		"Element": "A ground term or a variable."],
	summary: "Set non-membership constraint",
	desc: html("Constrain SetVariable to not include Element.<P>
		notin/2 is available for compatibility with conjunto library syntax.
		It is equivalent to the preferred `-@/2. See its description for details."),
	resat: "No.",
	fail_if: "Fails if Element must be a member of SetVariable.",
	see_also:[(`-@)/2,(`@)/2,in/2,(`::)/2]
	]).




:-comment((`$)/2, [
%	amode: `$(?,?),
%	amode: (? `$ ?),
	template: "?SetVar1 `$ ?SetVar2",
	args:  ["SetVar1": "A set variable.",
		"SetVar2": "A set variable."],
	summary: "Set disjointness constraint",
	desc: html("Constrain sets SetVar1 and SetVar2 to be disjoint. I.e. SetVar1 and
		SetVar2 should have no common elements (empty intersection)."),
	resat: "No.",
	fail_if: "Fails if SetVar1 and SetVar2 can not be disjoint.",
	eg:"
?- [] `$ [8], [7] `$ [8], [] `$ [].
yes

?- [7,8] `$ [8]  ; [7] `$ [7,8] ; [a] `$ [a] ; [a,b] `$ [b,a].
no

?- sets([X,Y], [],[8,9], [cardinality:1]), X `$ Y, set_labeling([X,Y]).
X = [8], Y = [9] ;
X = [9], Y = [8] ;
no

?- S `:: []+[a,b], X=S, X `$ S.
S = [], X = []

?- sets([X,Y], [],[7,8,9], [cardinality:2]), X `$ Y.
no

?- sets([X,Y], [],[7,8,9], [cardinality:[1,2]]), X `$ Y, #(X,2), #(Y,C).
C = 1

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], X `$ Y, c `@ Y, poss(X,PX), poss(Y,PY).
PX = [b,d], PY = [b,d,e,f]
",
	see_also:[all_disjoint/1,(`<>)/2,complement/2,complement/3,(`/=)/2,(`>=)/2]
	]).

:-comment((`<>)/2, [
%	amode: `<>(?,?),
%	amode: (? `<> ?),
	template: "?SetVar1 `<> ?SetVar2",
	args:  ["SetVar1": "A set variable.",
		"SetVar2": "A set variable."],
	summary: "Set disjointness constraint (obsolete)",
	desc: html("Constrain sets SetVar1 and SetVar2 to be disjoint.<P>
		Obsolete: `<>/2 is available only for compatibility with conjunto library syntax.
		It is equivalent to the preferred `$/2. See its description for details."),
	see_also:[(`$)/2,all_disjoint/1,complement/2,complement/3,(`/=)/2,(`>=)/2]
	]).


:-comment(all_disjoint/1, [
	amode: all_disjoint(+),
	args:  ["SetVars": "A list of set variables."],
	summary: "All sets disjointness global constraint",
	desc: html("Constrain all pairs of sets in SetVars to be disjoint. I.e. No two sets
		can have a common element (empty pairwise intersection).<P>
		This version of all_disjoint/1 is a weak global constraint, but stronger
		than the simple posting of all pairwise disjoint/2 constraints, since
		it posts the additional constraint that the sum of the cardinalities of
		SetVars must be less than or equal to the cardinality of the union of all
		the initial LUBs."),
	resat: "No.",
	fail_if: "Fails if SetVars can not be all disjoint.",
	eg:"
?- all_disjoint([[7],[8],[i,k]]).
yes

?- all_disjoint([[7,8],[i],[8]]).
no

?- sets([X,Y,Z], [],[1,2,7,8,9], [cardinality:2]), all_disjoint([X,Y,Z]).
no

?- sets([X,Y,Z], [],[1,2,7,8,9], [cardinality:2]), all_disjoint([X,Y,Z]), 2 `@ X, lub(Y,LubY), lub(Z,LubZ)
LubY = [1,7,8,9], LubZ = [1,7,8,9]
",
	see_also:[(`$)/2,(`<>)/2,complement/2,complement/3,(`/=)/2,(`>=),all_union/2]
	]).




:-comment((`>=)/2, [
%	amode: `>=(?,?),
%	amode: (? `>= ?),
	template: "?SetVar1 `>= ?SetVar2",
	args:  ["SetVar1": "A set variable.",
		"SetVar2": "A variable."],
	summary: "Set inclusion constraint",
	desc: html("Constrain sets SetVar1 and SetVar2 so that SetVar1 contains SetVar2.<P>
		If SetVar2 is not yet a set domain variable it is declared as such, using
		SetVar1's lub."),
	resat: "No.",
	fail_if: "Fails if SetVar1 can not contain SetVar2.",
	eg:"
?- [7,8,9] `>= [7,9], [7,8,9] `>= [7,8,9], [7,8,9] `>= [], [7,8,9] `>= [9].
yes

?- [1,7,9] `>= [7,8].
no

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], X `>= Y, poss(Y,PY).
PY = [a,b,c,d]

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], Y `>= X, glb(Y,GY).
GY = [a]

?- X `:: [a]+[b,c,d,z]:CX, Y `:: []+[a,b,c,d,e,f]:CY, X `>= Y, CX=2, fd:maxdomain(CY,MaxCY).
MaxCY = 2

?- X `:: [a]+[b,c,d,z]:CX, Y `:: []+[a,b,c,d,e,f]:CY, X `>= Y, CY=3, fd:mindomain(CX,MinCX).
MinCX = 3.
",
	see_also:[(`<)/2,(`=)/2]
	]).

:-comment((`<)/2, [
%	amode: `<(?,?),
%	amode: (? `< ?),
	template: "?SetVar1 `< ?SetVar2",
	args:  ["SetVar1": "A set variable.",
		"SetVar2": "A variable."],
	summary: "Set inclusion constraint (obsolete)",
	desc: html("Constrain sets SetVar1 and SetVar2 so that SetVar2 contains SetVar1.<P>
		Obsolete: (`<)/2 is available only for compatibility with conjunto library syntax.
		It is equivalent (with swapped arguments) to the preferred `>=/2.
		See its description for details."),
	see_also:[(`>=)/2,(`=)/2]
	]).




:-comment(all_union/2, [
	amode: all_union(+,?),
	args:  ["SetVars": "A list of variables.",
		"Union": "A variable or a ground set."],
	summary: "Union constraint of a list of sets",
	desc: html("Constraint: Union is the set union of SetVars.<P>
		Any variable in SetVars that is not yet a set domain variable, is declared
		as such using Union's lub."),
	resat: "No.",
	fail_if: "Fails if Union can not be the union of SetVars.",
	eg:"
?- all_union([[8,a,9],[i,8,o],[],[a,8,5]], U).
U = [5,8,9,a,i,o].

?- all_union([X,Z,S,Y,T], [8,9]), glb_poss(X,GX,PX), glb_poss(Y,GY,PY).
GX = [], PX = [8,9], GY = [], PY = [8,9]

?- X `:: [a]+[b,c], all_union([X,[b,n],X], U), glb_poss(X,GX,PX), glb_poss(U,GU,PU).
GX = [a], PX = [b,c], GU = [a,b,n], PU = [c]

?- sets([X,Y,Z],[a,b],[d,g,h,j],[cardinality:4]), all_union([X,Y,Z],U), #(U,C), fd:dom(C,DomC).
DomC = [4,5,6]

?- sets([X,Y,Z],[a,b],[d,g,h,j],[]), all_union([X,Y,Z],U), #(U,4), #(Y,C), fd:dom(C,DomC).
DomC = [2,3,4]
",
	see_also:[(`=)/2,all_disjoint/1]
	]).




:-comment((`/=)/2, [
%	amode: `/=(?,?),
%	amode: (? `/= ?),
	template: "?SetVar1 `/= ?SetVar2",
	args:  ["SetVar1": "A set variable.",
		"SetVar2": "A set variable."],
	summary: "Set inequality constraint",
	desc: html("Constrain sets SetVar1 and SetVar2 to be different.<P>
		This constraint is suspended until one of the two sets is bound
		to another set (variable or ground)."),
	resat: "No.",
	fail_if: "Fails if SetVar1 and SetVar2 must be the same set.",
	eg:"
?- [] `/= [8], [7] `/= [8], [7,8] `/= [8], [7] `/= [7,8].
yes

?- [] `/= [] ; [a] `/= [a] ; [a,b] `/= [b,a].
no

?- sets([X,Y], [],[8,9], [cardinality:1]), X `/= Y, set_labeling([X,Y]).
X = [8], Y = [9] ;
X = [9], Y = [8] ;
no

?- sets([X,Y], [],[8,9], []), X `/= Y, X=Y.
no

?- X `:: [8]+[8,9], [8,9] `/= X, card_labeling([X]).
X = [8] ;
no
",
	see_also:[(`$)/2,complement/2,complement/3,(`=)/2]
	]).




:-comment(complement/3, [
	amode: complement(?,++,?),
	args:  ["SetVar": "A variable.",
		"Universe": "A ground set.",
		"Complement": "A variable"],
	summary: "Set complement constraint",
	desc: html("Constrain sets so that Complement is the complement set of SetVar,
		with respect to the given Universe. I.e. Complement is Universe \\ SetVar.<P>
		If a variable (SetVar or Complement) is not yet a set domain variable,
		it is declared as such, limited by the Universe.<P>
		This constraint is usually more efficient (stronger) than posting an
		equivalent set difference constraint, due to specific inferences."),
	resat: "No.",
	fail_if: "Fails if Complement can not be the set complement of SetVar in set universe Universe.",
	eg:"
?- complement([7,8], [1,7,8,9], N).
N = [1,9]

?- complement(N, [1,7,8,9], [7,8]).
N = [1,9]

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], complement(X, [a,b,c,d,e,f,g], Y).
no

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], complement(X,[a,b,f],Y), domain(X,DX),domain(Y,DY).
DX = [[a]:1,[b]:2], DY = [[f]:1,[b]:2]

?- X `:: [a]+[b,c,d], Y `:: []+[a,b,c,d,e,f], complement(X, [a,b,c,d,e,f], Y), domain(Y,DY).
DY = [[e,f]:2,[b,c,d]:5]
",
	see_also:[complement/2,(`$)/2,(`=)/2]
	]).


:-comment(complement/2, [
	amode: complement(?,?),
	args:  ["SetVar": "A variable.",
		"Complement": "A variable"],
	summary: "Set complement constraint",
	desc: html("Constrain sets so that Complement is the complement set of SetVar.
		(The universe is taken as the union of their LUBs.)."),
	resat: "No.",
	fail_if: "Fails if Complement can not be the set complement of SetVar (in their universe).",
	eg:"
?- complement([8,9], []), complement([8,9], [t]).
yes

?- complement([8,9], [8]).
no

?- complement([8,9], N).
N = []

?- sets([X,Y], [],[7,8,9], []), complement(X,Y), 8 `@ Y, glb_poss(X,GX,PX), glb_poss(Y,GY,PY).
GX = [], PX = [7,9], GY = [8], PY = [7,9]

?- sets([X,Y], [],[7,8,9], [cardinality:C]), complement(X,Y), card_labeling([X]).
no

?- sets([X,Y], [],[7,8,9], []), complement(X,Y), X `>= Y, set_labeling(up,[Y]).
Y = [], X = [7,8,9] ;
no

?- sets([X,Y], [],[7,8,9], [minimum:Min]), complement(X,Y), refine(up,X).
no

?- sets([X,Y], [],[7,8,9], []), complement(X,Y), #(X,1), #(Y,CY).
CY = 2
",
	see_also:[complement/3,(`$)/2,(`=)/2]
	]).




:-comment((`=)/2, [
%	amode: `=(?,?),
%	amode: (? `= ?),
	template: "?SetExp1 `= ?SetExp2",
	args:  ["SetExp1": "A set expression.",
		"SetExp2": "A set expression."],
	summary: "Set equality constraint",
	desc: html("Constrain sets in both hand sides of equation so that SetExp1 and
		SetExp2 represent the same set.<P>
		A set expression is a set (variable or ground) or a set operation between
		two set expressions. Possible set operations are set union, set intersection
		and set difference, and the respective operators are `\\/, `/\\ and `\\.
		These operators are defined as:
<PRE>
:- op(500, yfx, `\\/).   %set union
:- op(400, yfx, `/\\).   %set intersection
:- op(300, yfx, `\\ ).   %set difference
</PRE>"),
	resat: "No.",
	fail_if: "Fails if SetExp1 can not be the same set as SetExp2.",
	eg:"
/*
Examples of equalities between set expressions, being S,T,U,V,W,X,Y,Z set variables:

X `= Y
S `\\/ Y `\\ Z `= T `/\\ S `\\/ U `/\\ W
V `\\ ([a,b,d] `\\/ W) `\\/ (S `\\ W) `= ((T `/\\ [3,9]) `\\/ W) `\\ U
*/

% just union:
?- [8,a,9] `\\/ [i,8,o] `\\/ [] `\\/ [a,8,5] `= U.
U = [5,8,9,a,i,o]

?- X `\\/ Y `= [8,9], glb_poss(X,GX,PX), glb_poss(Y,GY,PY).
GX = [], PX = [8,9], GY = [], PY = [8,9]

?- X `:: [a]+[b,c], X `\\/ [b,n] `= U, glb_poss(X,GX,PX), glb_poss(U,GU,PU).
GX = [a], PX = [b,c], GU = [a,b,n], PU = [c]

?- sets([A,B],[a,b],[d,g,h,j],[cardinality:4]), A`\\/B`=U, #(U,C), fd:dom(C,DomC).
DomC = [4,5,6]

?- sets([X,Y],[a,b],[d,g,h,j],[]), X`\\/Y`=U, #(U,4), #(Y,C), fd:dom(C,DomC).
DomC = [2,3,4]

?- sets([S,X], [],[a,b], [cardinality:[0,2]]), U `= X `\\/ S, #(U,C), fd:dom(C,DomC).
DomC = [0,2]

?- S `:: [a,c]+[b,g,h,j,l]:3, X`::[a]+[b,h,t,u,y]:2, U `= X `\\/ S, #(U,C), fd:dom(C,DomC).
DomC = [3,4]

?- S `:: [a,c]+[b,g,h,j,l], X`::[a]+[b,h,t,u,y], U `= X `\\/ S, #(U,C), fd:(C::0..3), #(X,CX), fd:dom(CX,DomCX).
DomCX = [1,2]


% just intersection:
?- I `= [4,6] `/\\ [3,6,8].
I = [6]

?- [a,b,c,d,e] `/\\ [a,b,c,e,f,g] `/\\ [b,d,e,f,x,y] `= I.
I = [b,e]

?- X `:: [a]+[b,c], X `/\\ [b,n] `= I, glb_poss(X,GX,PX), glb_poss(I,GI,PI).
GX = [a], PX = [b,c], GI = [], PI = [b]

?- S `:: [a]+[b,c], X`::[]+[7,8,9], I`::[]+[a,b,c,7,z,99], I `= X `/\\ S.
I = []

?- sets([A,B],[a,b],[d,g,h,j],[cardinality:5]), A`/\\B`=I, #(I,C), fd:dom(C,DomC).
DomC = [4,5]

?- S `:: [a,c]+[g,h,j,l], X`::[a]+[b,h,t,u,y], I `= X `/\\ S, #(I,C), fd:dom(C,DomC).
DomC = [1,2]


% just difference:
?- D `= [4,6] `\\ [3,6,8].
D = [4]

?- [a,b,c,d,e] `\\ [a,e,f,g] `\\ [b,d,e,f,x,y] `= D.
D = [c]

?- [a,b,c,d,e] `\\ ([a,e,f,g] `\\ [b,d,e,f,x,y]) `= D.
D = [b,c,d,e]

?- X `:: [a]+[b,c], X `\\ [b,n] `= D, glb_poss(X,GX,PX), glb_poss(D,GD,PD).
GX = [a], PX = [b,c], GD = [a], PD = [c]

?- S `:: [a]+[b,c], X`::[]+[7,8,9], D`::[]+[a,b,c,7,z,99], D `= X `\\ S, glb_poss(D,GD,PD).
GD = [], PD = [7]

?- sets([A,B],[a,b],[d,g,h,j],[cardinality:5]), A`\\B`=D, #(D,C), fd:dom(C,DomC).
DomC = [0,1]

?- sets([X,Y],[a,b],[d,g,h,j],[]), X`\\Y`=D, #(D,4).
X = [a,b,d,g,h,j], Y = [a,b]

?- S `:: [a,c,z]+[g,h], X`::[a]+[b,c,h,t], D `= S `\\ X, #(D,C), fd:dom(C,DomC).
DomC = [1,2,3,4].

?- S `:: []+[a,b], X=S, #(S `\\ X, C).
C = 0",
	see_also:[(`/=)/2,set_labeling/1,set_labeling/2]
	]).




:-comment(my_print_set_handler/2, hidden).
:-comment(my_unify_sets_handler/2, hidden).


:-comment(struct(cardinal), [
	summary: "Cardinal attributes of a set variable",
	fields:[domain: "Set domain in the form [Glb:NIn,Poss:NMax], where Glb is the set's
			glb, NIn its cardinality, Poss is its poss (i.e. its lub\\glb),
			and NMax is the lub's cardinality (i.e. NIn + #(Poss)).
			If it is a set of sets and a union function attribute has been
			declared, then each element of Poss comes annotated with its
			respective length.",
		cardinality: "Cardinality function (an integer or an FD variable).",
		minimum: "Minimum function (an integer or an FD variable),
			for sets of integers. Free variable if unused.",
		maximum: "Maximum function (an integer or an FD variable),
			for sets of integers. Free variable if unused.",
		union: html("Union function, for sets of sets. Free variable if unused;
			otherwise, a list in the form
			[UnionVar, GlbU+PossU, Singles, Lengths], where:<PRE>
UnionVar: A set (variable or ground) corresponding to the union of the set's elements
	(sets themselves);
GlbU: Set union of the set's glb;
PossU: Set of possible union elements with counters (X:N), i.e. an ordered
	list of all elements in the sets in set's poss (lub\\glb)
	with the number of occurrences attached
Singles: Set of elements where N=1 in PossU</PRE>"),
		bounded: "Suspension list.",
		glb: "Suspension list.",
		lub: "Suspension list.",
		bound: "Suspension list."
		]
	]).
