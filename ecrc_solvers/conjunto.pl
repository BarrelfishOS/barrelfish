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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Carmen Gervet and Pascal Brisset, ECRC. 
% 
% END LICENSE BLOCK

:- module(conjunto).
:- export initialization(use_module(set)).
:- use_module(set).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Finite Set Constraints Library - obsolescent, use library(fd_sets) instead").
:- comment(author, "Carmen Gervet and Pascal Brisset, ECRC").
:- comment(copyright, "1995-2006 Cisco Systems, Inc").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").
:- comment(status, deprecated).

:- comment(/(all_disjoint, 1), [
	summary:"Lsets is a list of set domain variables which are constrained to be
pairwise disjoint.

",
	template:"all_disjoint(?Lsets)",
	desc:html("   Lsets is a list of set domain variables which are constrained to be
   pairwise disjoint.  Once the unconsistent domain bounds have been
   updated.  The predicate is a programming abstraction standing for
   conjunction of disjointness constraints.  The generated disjointness
   constraints are delayed.  They are activated each time a lower bound of
   one set domain involed is modified.

<P>
"),
	args:["?Lsets" : "List of set domain variables."],
	resat:"   No.",
	fail_if:"   Fails if some elements of Lsets are not set variables or if all the set\n   variables can not be pairwise disjoint.\n\n",
	eg:"
S `:: {}.. {1,3,5}, S1 `:: {1} .. {1,3,4}, S2 `::{3,4}..{3,4,5},
all_disjoint([S,S1,S2]).

S = S{{} .. {5}}
S1 = {1}
S2 = S2{{3, 4} .. {3, 4, 5}}

Delayed goals:
        S{{} .. {5}} dis_s S2{{3, 4} .. {3, 4, 5}}
yes.
",
	see_also:[/(`::, 2), /(`<>, 2), /(`=, 2), /(all_union, 2)]]).

:- comment(/(all_union, 2), [
	summary:"Lsets is a list of set domain variables whose union is the set term Svar.

",
	template:"all_union(?Lsets, ?Svar)",
	desc:html("   Svar is the union of all the set variables appearing in Lsets.  If Svar
   is a free variable, it becomes a set variable and its attached domain is
   defined from the union of the domains or known sets appearing in Lsets.

<P>
"),
	args:["?Lsets" : "A list of set domain variables.", "?Svar" : "A free variable or a set variable."],
	resat:"   No.",
	fail_if:"   Fails if some elements of Lsets are not set variables and if Svar can\n   not be the union of the Lsets elements.\n\n",
	eg:"
[eclipse 13]: S `:: {}.. {1,2,3,5}, S1 `:: {1} .. {1,2,3,4},
all_union([S, S1],{1,2,3,4,5}).

S = S{{5} .. {1, 2, 3, 5}}
S1 = S1{{1, 4} .. {1, 2, 3, 4}}

Delayed goals:
        ground_union(S{{5} .. {1, 2, 3, 5}},
S1{{1, 4} .. {1, 2, 3, 4}}, {1, 2, 3, 4, 5})
yes.



",
	see_also:[/(`::, 2), /(`=, 2), /(`<>, 2), /(all_disjoint, 1)]]).

:- comment(/(#, 2), [
	summary:"Var is the cardinality of the set term Sterm.

",
	template:"#(?Sterm,?Var)",
	desc:html("   Var is the cardinality of the set term Sterm.  If Sterm is a known set
   and Var is free, Var is instanciated to the cardinality of Sterm.  If
   both are known, the predicate checks that Var is the cardinality of
   Sterm.  If Sterm is a set variable and Var is free, Var becomes a domain
   variable representing the set term cardinality.  The predicate is
   delayed and activated as soon as one of the bounds of Sterm or Var is
   modified.  If Sterm is a set variable and and Var is a domain variable
   it checks that Var contains the cardinality of Sterm.  It might infer a
   modification of the bounds of the set domain.

<P>
"),
	args:["?Sterm" : "A set term or a ground set.", "?Var" : "A free variable, a domain variable or an integer."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
eclipse 3]: S `:: {}..{c,d}, S1= {1,2,3}, #(S \\/ S1, V).

S = S{{} .. {c, d}}
S1 = {1, 2, 3}
V = Card{[3..5]}

Delayed goals:
        union_s({1, 2, 3}, S{{} .. {c, d}},
               R{{1, 2, 3} .. {1, 2, 3, c, d}})
        car_s(R{{1, 2, 3} .. {1, 2, 3, c, d}}, Card{[3..5]})
yes.



",
	see_also:[/(`::, 2), /(`=, 2)]]).

:- comment(/(`<>, 2), [
	summary:"Sterm and Sterm1 are disjoint.

",
	template:"?Sterm `<> ?Sterm1",
	desc:html("   This constraint states that the set terms Sterm and Sterm1 have to be
   disjoint.  If both terms are known sets, it checks the empty
   intersection between them.  If either Sterm or Sterm1 is a known set,
   this constraint checks the empty intersection or disjointness and the
   unconsistent bounds of the set variables involved are modified.  If both
   terms contain set variables, the consistency of the domain bounds is
   checked and the constraint is delayed.  It is activated as soos as the
   lower bound of one set variable involved is modified.

<P>
"),
	args:["?Sterm" : "A set term.", "?Sterm1" : "A set term."],
	resat:"   No.",
	fail_if:"   Fails if Sterm can not be a subset of Sterm1.\n\n",
	eg:"
[eclipse 3]: S `:: {}.. {1,2,3},S1 `:: {1} .. {1,2,3,4},
             S `<> S1.

S = S{{} .. {2, 3}}
S1 = S1{{1} .. {1, 2, 3, 4}}

Delayed goals:
        S{{} .. {2, 3}} dis_s S1{{1} .. {1, 2, 3, 4}}
yes.



",
	see_also:[/(`::, 2)]]).

:- comment(/(set, 1), [
	summary:"Succeeds if SVar is a ground set (not a set domain)

",
	template:"set(?SVar)",
	desc:html("   This predicate is used to test if a term is a ground set, that is a set
   variable where the domain is reduced to a singleton or a set of ground
   values defined with the symbols fg.

<P>
"),
	args:["?SVar" : "A Prolog term."],
	resat:"   No.",
	fail_if:"   Fails if SVar is not a ground set.\n\n",
	eg:"
[eclipse 3]: S = {a,{f(a,g),3},c}, set(S).

S = {a,{f(a,g),3},c}
yes.



",
	see_also:[/(`::, 2), /(glb, 2), /(lub, 2), /(set_range, 3)]]).

:- comment(/(el_weight, 2), [
	summary:"Var is an element of type e(Val,Weight) from a weighted domain.  Wvar is a
free variable unifying with the weight Weight of Var.

",
	template:"el_weight(+Var, ?Wvar)",
	desc:html("   Unifyes the weight of the term Var which should be of the form
   e(Val,Weight) with Wvar.  This predicate is convenient to select
   elements for the labeling of weighted sets.

<P>
"),
	args:["+Var" : "A weighted element.", "?Wvar" : "Term unifying with an integer."],
	resat:"   No.",
	fail_if:"   Fails if Var is not of the form e(Val,Weight) or if Wvar is not\n   unifyable with the weight.\n\n",
	eg:"
[eclipse 12]: E = e(1,2), el_weight(E, W).

E = e(1, 2)
W = 2
yes.



",
	see_also:[/(max_weight, 2), /(`::, 2)]]).

:- comment(/(glb, 2), [
	summary:"S is a set domain variable and Glb is the lower bound of its domain.

",
	template:"glb(?S, ?Glb)",
	desc:html("   Glb is the ground set corresponding to the lower bound of the domain of
   S. If Glb is a free variable, it is instanciated to the lower bound of
   the domain of S. If it is a ground set, it should be equal to the lower
   bound of the domain of S, otherwise it fails.

<P>
"),
	args:["?S" : "A set domain variable.", "?Glb" : "A free variable or a ground set."],
	resat:"   No.",
	fail_if:"   Fails if S is not a set domain variable or if Glb is a ground set not\n   equal to the lower bound of S domain.\n\n",
	eg:"
[eclipse 9]: S `:: {}.. {1,2,3,5}, glb(S,Glb).

S = S{{} .. {1, 2, 3}}
Glb = {}
yes.



",
	see_also:[/(`::, 2), /(lub, 2), /(set_range, 3)]]).

:- comment(/(in, 2), [
	summary:"E belongs to the set term Sterm.

",
	template:"?E in ?Sterm",
	desc:html("   This constraint states that E belongs to the set term Sterm.  If E is
   not a known value the constraint is delayed.  It is activated as soon as
   E becomes a known value.  If Sterm is a ground set and E is known it
   checks if E belongs to Sterm.

<P>
"),
	args:["?E" : "A Prolog term.", "?Sterm" : "A set term."],
	resat:"   No.",
	fail_if:"   Fails if E cannot belong to Sterm.\n\n",
	eg:"
[eclipse 4]: S `:: {}.. {1,2,3}, E in S.

E = E
S = S{{} .. {1, 2, 3}}

Delayed goals:
        E in_s S{{} .. {1, 2, 3}}
yes.

[eclipse 5]: S `:: {}.. {1,2,3}, 1 in S.

S = S{{1} .. {1, 2, 3}}
yes.



",
	see_also:[/(`::, 2), /(notin, 2)]]).

:- comment(/(list2set, 2), [
	summary:"Transforms a ground list List into a ground set Svar.

",
	template:"list2set(+List, ?Svar)",
	desc:html("   This predicate forces Svar to be the ground set associated with the
   ground list List.  Svar can be a free variable or a ground set.

<P>
"),
	args:["+List" : "A ground list.", "?Svar" : "A free variable or a ground set."],
	resat:"   No.",
	fail_if:"   Fails if List is not ground or if Svar is ground and different from the\n   computed set.\n\n",
	eg:"
[eclipse 5]: L = [1,3,2,6,4], list2set(L, S).

L = [1, 3, 2, 6, 4]
S = {1, 2, 3, 4, 6}
yes.



",
	see_also:[/(set2list, 2)]]).

:- comment(/(lub, 2), [
	summary:"S is a set domain variable and Lub is the lower bound of its domain.

",
	template:"lub(?S, ?Lub)",
	desc:html("   Lub is the ground set corresponding to the upper bound of the domain of
   S. If Lub is a free variable, it is instanciated to the upper bound of
   the domain of S. If it is a ground set, it should be equal to the upper
   bound of the domain of S, otherwise it fails.

<P>
"),
	args:["?S" : "A set domain variable.", "?Lub" : "A free variable or a ground set."],
	resat:"   No.",
	fail_if:"   Fails if S is not a set domain variable or if Lub is a ground set not\n   equal to the upper bound of S domain.\n\n",
	eg:"
[eclipse 9]: S `:: {}.. {1,2,3,5}, lub(S,Lub).

S = S{{} .. {1, 2, 3}}
Lub = {1, 2, 3}
yes.



",
	see_also:[/(`::, 2), /(glb, 2), /(set_range, 3)]]).

:- comment(/(max_weight, 2), [
	summary:"Svar is a weighted set domain variable whose maximal possible weight
attached to an element is Var.

",
	template:"max_weight(?Svar, ?Var)",
	desc:html("   If Svar is a weighted set domain variable, it returns the element of its
   domain which belongs to the set resulting from the difference of the
   upper bound and the lower bound and which has the greatest weight.  If
   Svar is a ground set, it just returns the element with the geratest
   weight.

<P>
"),
	args:["?Svar" : "A weighted set domain variable or a weighted set.", "?Var" : "Term unifying with a term of the form e(_,_)."],
	resat:"   No.",
	fail_if:"   Fails if Svar is not a weighted set term, or if Var can not be unified\n   with the extracted element.\n\n",
	eg:"
[eclipse 4]: S `:: {e(a,10), e(b,15)} ..{e(a,10), e(b,15),
 e(c,20)},  max_weight(S,W).

S = S{{e(a, 10), e(b, 15)} .. {e(a, 10), e(b, 15), e(c, 20)}}
W = e(c, 20)
yes.



",
	see_also:[/(`::, 2)]]).

:- comment(/(modify_bound, 3), [
	summary:"Flag is a flag which takes its value in fglb, lubg.  It indicates the bound
of the set domain variable Svar which should be updated and take the value
Var which is ground.

",
	template:"modify_bound(+Flag, ?Svar, +Var)",
	desc:html("   Flag is a flag which should take the value glb or lub, otherwise it
   fails.  If Svar is a ground set, it succeeds if we have Var equals to
   Svar.  If Svar is a set domain variable, its new lower or upper bound
   will be updated and take the value Var.  For monotonicity reasons, set
   domains can only get reduced.  So a new upper bound has to be contained
   in the old one and a new lower bound has to contain the old one.
   otherwise it fails.

<P>
"),
	args:["+Flag" : "A ground value from fglb, lubg.", "?Svar" : "A set domain variable.", "+Var" : "A ground set."],
	resat:"   No.",
	fail_if:"   Fails if Svar is ground and different from Var or if the new upper bound\n   is not contained in the old one or the new lower bound does not contain\n   the old one.\n\n",
	eg:"
[eclipse 9]: S `:: {1,2}..{1,2,3,4,6}, modify_bound(glb, S, {1,2,3}).

S = S{{1, 2, 3} .. {1, 2, 3, 4, 6}}
yes.



",
	see_also:[/(`::, 2), /(glb, 2), /(lub, 2), /(set_range, 3)]]).

:- comment(/(notin, 2), [
	summary:"E does not belong to the set term Sterm.

",
	template:"?E notin ?Sterm",
	desc:html("   This constraint states that E does not belong to the set term Sterm.  If
   E is not a known value the constraint is delayed.  It is activated as
   soon as E becomes a known value.  If Sterm is a ground set and E is
   known it checks if E does not belong to Sterm.

<P>
"),
	args:["?E" : "A Prolog term.", "?Sterm" : "A set term."],
	resat:"   No.",
	fail_if:"   Fails if E belongs to Sterm.\n\n",
	see_also:[/(`::, 2), /(in, 2)]]).

:- comment(/(refine, 1), [
	summary:"Instanciate Svar to a value in its set domain.

",
	template:"refine(?Svar)",
	desc:html("   If Svar is a set domain variable, it labels Svar to its first possible
   value (which satisfies the constraints) by taking the largest possible
   set of elements chosen from the smallest one to the biggest one.  If
   there are several possible instances of Svar, it creates choice points.
   It backtracks on the element taken.  If Svar is a ground set, nothing
   happens.  Otherwise it fails.  If List is a possible list of set domain
   variables occuring in the program, the simplest labeling procedure is:

<P>
<PRE>
            labelings([]).
            labelings([S | List]) :-
                 refine(S),
                 labelings(List).
</PRE>
"),
	args:["?Svar" : "A set domain variable or a ground set."],
	resat:"   On the taken elements.",
	fail_if:"   Fails if Svar is not a set domain variable or ground set.\n\n",
	eg:"
[eclipse 21]: S `:: {} ..{1,2,5,3,8}, refine(S).

S = {1, 2, 3, 5, 8}     More? (;)
yes



",
	see_also:[/(`::, 2)]]).

:- comment(/(`=, 2), [
	summary:"Sterm and Sterm1 are equal.

",
	template:"?Sterm `= ?Sterm1",
	desc:html("   This constraint states that the two set terms are equal.  It is
   activated whenever the upper or the lower bound of a set domain variable
   is updated.  When propagating set domain updates, the system reasons
   only over the upper and lower bound sets of values of the set domain and
   makes sure that these sets of values are consistent with those of other
   set domain variables.

<P>
   If this constraint is not solved, this predicate delays until it can be
   completely solved.

<P>
"),
	args:["?Sterm" : "A ground set, a set domain variable or a set expression                containing set operators (union \\/, intersection /\\,                difference \\).", "?Sterm1" : "A ground set, a set domain variable or a set expression."],
	resat:"   No.",
	fail_if:"   Fails if the two set terms have a different set value.\n\n",
	eg:"
[eclipse 2]: S `:: {}.. {1,2,3}, [S1,S2] `:: {}..{3,4,5},
             S \\/  S1 `= S2.

S = S{{} .. {3}}
S1 = S1{{} .. {3, 4, 5}}
S2 = S2{{} .. {3, 4, 5}}

Delayed goals:
        union_s(S{{} .. {3}}, S1{{} .. {3, 4, 5}},
S2{{} .. {3, 4, 5}})
yes.



",
	see_also:[/(`::, 2)]]).

:- comment(/(set_range, 3), [
	summary:"Svar is a set domain variable and Glb and Lub are the respective lower
bound and upper bound of its domain.

",
	template:"set_range(?Svar, ?Glb, ?Lub)",
	desc:html("   This predicate computes the lower bound Glb and the upper bound Lub of
   the domain attached to the set variable Svar.  If Glb and/or Lub are
   ground, they should be equal to the respective bounds of the domain,
   otherwise it fails.

<P>
"),
	args:["?Svar" : "A set domain variable.", "?Glb" : "A free variable or a ground set.", "?Lub" : "A free variable or a ground set."],
	resat:"   No.",
	fail_if:"   Fails if Svar is not a set domain variable or if one of the terms glb,\n   Lub, can not be unified with the respective bounds of the domain of\n   Svar.\n\n",
	exceptions:[4 : "Var is not a domain variable."],
	eg:"
[eclipse 7]: S `:: {} .. {1,2,3}, set_range(S, Glb, Lub).

S = S{{} .. {1, 2, 3}}
Glb = {}
Lub = {1, 2, 3}
yes.



",
	see_also:[/(`::, 2), /(glb, 2), /(lub, 2)]]).

:- comment(/(set2list, 2), [
	summary:"Transforms a ground set Svar into a list of ordered elements List.

",
	template:"set2list(+Svar, ?List)",
	desc:html("   This predicate forces List to be the list of ordered elements associated
   to the ground set Svar.  List can be a free variable or a ground term.
   This predicate is convenient when iterations over set elements are
   required.

<P>
"),
	args:["+Svar" : "A ground set.", "?List" : "Term unifying with a list of ground values."],
	resat:"   No.",
	fail_if:"   Fails if Svar is not ground or if List is ground and different from the\n   computed list.\n\n",
	eg:"
[eclipse 3]: S = {1,4,2,6,2,7}, set2list(S, L).

S = {1, 2, 4, 6, 7}
L = [1, 2, 4, 6, 7]
yes.



",
	see_also:[/(list2set, 2)]]).

:- comment(/(`::, 2), [
	summary:"Terms in Svar have the set domain Domain.

",
	template:"?Svar `::  +Domain",
	desc:html("   The main purpose of this predicate is to create set domain variables.
   Domain has to be a closed set interval specified as Glb..Lub.  If SVar
   is already a set domain variable, its set domain will be updated
   according to the new domain; if it is instanciated, the predicate checks
   is the set value lies in the domain.  If the upper bound equals the
   lower bound the set variable describes a ground set.  Otherwise, if SVar
   is a free variable, it is converted to a set domain variable.  If SVar
   is a list, this will be performed for all its elements.  If Domain is
   free, it fails.

<P>
"),
	args:["?SVar" : "A set variable or a list of set variables.", "+Domain" : "Lattice specified by its upper and lower bound Glb..Lub such                that Glb is a subset of Lub."],
	resat:"   No.",
	fail_if:"   Fails if Domain is free, or if SVar cannot have the domain Domain.\n\n",
	eg:"
[eclipse 3]: S `:: {}..{a,{2,3},c}.

S = S{{} .. {a, {2,3}, c}}
yes.



",
	see_also:[/(glb, 2), /(lub, 2), /(set_range, 3), /(set, 1)]]).

:- comment(/(`<, 2), [
	summary:"Sterm is a subset of Sterm1.

",
	template:"?Sterm `< ?Sterm1",
	desc:html("   This constraint states that the set term Sterm has to be a subset of the
   set term Sterm1.  If both terms are known sets, this constraint checks
   the inclusion.  If either Sterm or Sterm1 is a known set, this
   constraint checks the inclusion and the unconsistent bounds of the set
   variables involved are modified.  If both terms contain set variables
   their domain bounds are checked and the constraint is delayed.  It is
   activated as soon as the upper bound of Sterm1 or the lower bound of
   Sterm is modified.

<P>
"),
	args:["?Sterm" : "A set term.", "?Sterm1" : "A set term."],
	resat:"   No.",
	fail_if:"   Fails if Sterm can not be a subset of Sterm1.\n\n",
	eg:"
[eclipse 3]: S `:: {1}.. {1,2,3,4}, S1 `:: {} .. {1,2,3},
 S `< S1.

S1 = S1{{1} .. {1, 2, 3}}
S = S{{1} .. {1, 2, 3}}

Delayed goals:
       S1{{1} .. {1, 2, 3}} sub_s S{{1} .. {1, 2, 3}}
yes.



",
	see_also:[/(`::, 2)]]).

:- comment(/(sum_weight, 2), [
	summary:"Svar is a weighted set domain variable or a weighted ground set and Var a
free variable or a domain variable or an integer which represents the
weight of Svar.

",
	template:"sum_weight(?Svar, ?Var)",
	desc:html("   Svar is a weighted set domain variable or ground set.  Var is the weight
   of Svar.  If Var is a free variable, this constraint is a mean to access
   a set weight and attach it to Var.  If not, the weight of Svar is
   constrained to be equal to Var.  The constraint is delayed and activated
   each time a modification occurs in the domain of Svar or Var.
   Convenient when dealing with packing problems.

<P>
"),
	args:["?Svar" : "A weighted set domain variable or a weighted ground set.", "?Var" : "Term unifying with a domain variable or an integer."],
	resat:"   No.",
	fail_if:"   Fails if Svar is not a weighted set term or if Var can not represent the\n   weight of Svar.\n\n",
	eg:"
[eclipse 4]: S `:: {e(a,10), e(b,15)} ..{e(a,10), e(b,15),
                    e(c,20)}, sum_weight(S, W).

S = S{{e(a, 10), e(b, 15)} .. {e(a, 10), e(b, 15), e(c, 20)}}
W = W{[25..45]}

Delayed goals:
        weight_s(S{{e(a, 10), e(b, 15)} .. {e(a, 10),
                   e(b, 15), e(c, 20)}}, W{[25..45]})
yes.



",
	see_also:[/(`::, 2)]]).
