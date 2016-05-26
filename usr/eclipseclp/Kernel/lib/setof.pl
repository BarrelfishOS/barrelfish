% ----------------------------------------------------------------------
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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): R.A.O'Keefe and David Warren
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% System:		ECLiPSe Constraint Logic Programming System
% Version:		$Id: setof.pl,v 1.7 2013/02/18 00:42:59 jschimpf Exp $
% Identification:	setof.pl, part of module(sepia_kernel)
% Description:		Implements the all-solution predicates.
% 			This code was originally based on Richard O'Keefe's
% 			1983 public domain implementation, but now retains
% 			virtually nothing from that base, except the
% 			wording of some comments.
%
% ----------------------------------------------------------------------

:- system.

:- export
	findall/3,
	setof/3,
	coverof/3,
	bagof/3,
	(^)/2.

:- meta_predicate((
	findall(*,0,*),
	setof(*,0,*),
	bagof(*,0,*),
	coverof(*,0,*),
	^(*,0))).


%   findall(Template, Generator, List)
%   is a special case of bagof, where all free variables in the
%   generator are taken to be existentially quantified.  It is
%   described in Clocksin & Mellish on p152.  The code they give
%   has a bug (which the Dec-10 bagof and setof predicates share)
%   which this has not.

findall_body(Template, Generator, List, Module) :-
	bag_create(Ref),
	recordz_instances(Template, Generator, Module, Ref),
	bag_dissolve(Ref, List).


%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator.
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.

setof_body(Template, Filter, Set, Module) :-
	bagof_body(Template, Filter, Bag, Module),
	sort(0, <, Bag, Set).


%   coverof(Template, Generator, Set) - ECLiPSe extension
%   works like setof/3, however the list of solutions is not sorted
%   and only the most general instances are retained

coverof_body(Template, Filter, Set, Module) :-
	bagof_body(Template, Filter, Bag, Module),
	prune_instances(Bag, Set).


%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)

bagof_body(Template, QGenerator, Bag, Module) :-
	free_variables_quant(QGenerator, Template, Generator, Vars, Module),
	( Vars == [] ->
	    bag_create(Ref),
	    recordz_instances(Template, Generator, Module, Ref),
	    bag_dissolve(Ref, Bag),
	    Bag \== []
	;
	    Key =.. [.|Vars],
	    bag_create(Ref),
	    recordz_instances(Key-Template, Generator, Module, Ref),
	    bag_dissolve(Ref, KeyedSols),
	    add_stripped_keys(KeyedSols, DoubleKeyedSols, HaveAttributes),
	    ( HaveAttributes == true ->
		instance_bag_attrs(DoubleKeyedSols, Key, Bag)
	    ;
		instance_bag_plain(KeyedSols, Key, Bag)
	    )
	).


% The simpler form for "plain" solutions, which have no attributes in their
% free variable bindings, and thus are cheaper to partition into variants.

instance_bag_plain(KeyedSols, FreeVars, Bag) :-
	% Unify variables in FreeBindings. As a result, variants become
	% identical (the sharing side-effect among non-variants is unimportant).
	% The trick was suggested by Ulrich Neumerkel.
	equalise_key_variables(KeyedSols, _),
	sort(1, =<, KeyedSols, SortedKeyedSols),
	same_key_members(SortedKeyedSols, FreeVars, Bag).


% The general form, where free variable bindings can be attributed vars.
% We have a list of FreeBindingsPlain-(FreeBindingsAttr-TemplateBinding)
% We first use FreeBindingsPlain to group solutions as in the attribute-free
% case.  Within these groups we then use the naive algorithm for further
% partitioning using full attribute-aware variant testing on FreeBindingsAttr.

instance_bag_attrs(DoubleKeyedSols, FreeVars, Bag) :-
	equalise_key_variables(DoubleKeyedSols, _),
	sort(1, =<, DoubleKeyedSols, SortedDoubleKeyedSols),
	same_key_members(SortedDoubleKeyedSols, _, MultiBag),
	instance_bag_naive(MultiBag, FreeVars, Bag).


% Naive version with full variant testing, quadratic complexity
instance_bag_naive([FreeBinding1-TmplBinding1|Sols], FreeVars, Bag) :-
	(
	    foreach(Sol,Sols),
	    fromto(RemSols,RemSols1,RemSols2,[]),
	    fromto(Bag1,Bag2,Bag3,[]),
	    param(FreeBinding1)
	do
	    Sol = FreeBinding-TmplBinding,
	    ( variant(FreeBinding, FreeBinding1) ->
		FreeBinding1 = FreeBinding,
		Bag2 = [TmplBinding|Bag3],
		RemSols1 = RemSols2
	    ;
		Bag2 = Bag3,
		RemSols1 = [Sol|RemSols2]
	    )
	),
	( RemSols == [] ->
	    FreeVars = FreeBinding1, Bag = [TmplBinding1|Bag1]
	;
	    (
		FreeVars = FreeBinding1, Bag = [TmplBinding1|Bag1]
	    ;
		instance_bag_naive(RemSols, FreeVars, Bag)
	    )
	).


% Auxiliary operations on Key-Value lists

% Add keys that are copies of the original keys with their attributes stripped.

add_stripped_keys(KVs, KKVs, HaveAttributes) :-
	(
	    foreach(KV,KVs),
	    foreach(PlainKey-KV,KKVs),
	    param(HaveAttributes)
	do
	    KV = Key-_,
	    copy_term(Key, PlainKey, AttrVars),
	    ( AttrVars == [] -> true ; HaveAttributes = true )
	).


% Unify variables in the list keys.
% This is similar to using numbervars, but leaves the variables free.

equalise_key_variables([], _PositionVars).
equalise_key_variables([Key-_|KVs], PositionVars) :-
	term_variables(Key, FreeVars),
	append(FreeVars, _, PositionVars),
	equalise_key_variables(KVs, PositionVars).


% Separate the leading list elements with identical keys

same_key_prefix([Key-V|KVs], Key, [V|Vs], KVsRest) :-
	same_key_prefix1(KVs, Key, Vs, KVsRest).

    same_key_prefix1([], _Key, [], []).
    same_key_prefix1(KVs, Key, Vs, KVsRest) :- KVs = [K-V|KVs1],
    	( K == Key ->
	    Vs = [V|Vs1],
	    same_key_prefix1(KVs1, Key, Vs1, KVsRest)
	;
	    Vs = [], KVsRest = KVs
	).
    	
	
% Succeed once for each sequence of elements with identical keys

same_key_members(KVs, Key, Vs) :-
	% Pick the first sequence
	same_key_prefix(KVs, Key0, Vs0, KVsRest),
	% Succeed, and if necessary, allow backtracking for more
	( KVsRest == [] ->
	    Key = Key0, Vs = Vs0
	;
	    (
		Key = Key0, Vs = Vs0
	    ;
		same_key_members(KVsRest, Key, Vs)
	    )
	).


%    recordz_instances(Template, Generator, Module, Ref)
%    enumerates all provable instances of the Generator and records the
%    associated Template instances.  Neither argument ends up changed.

% :- sequential recordz_instances/4.
recordz_instances(Template, Generator, Module, Ref) :-
	call_local(Generator, Module),
%	true,			% force waking before recording
	bag_enter(Ref, Template),
	fail.
recordz_instances(_, _, _, _).


% Collect the free variables in QGoal.  These are those that occur
% neither in Bound nor are explicitly quantified via X^Y^...^Goal.
% If ^/2 is defined as a predicate, we use traditional quantifier-semantics,
% else ISO-semantics (where only toplevel occurrences of ^/2 are considered).
% The (toplevel-)unquantified goal is returned as well.

free_variables_quant(QGoal, Bound, Goal, VarList, _Module) :- var(QGoal), !,
	Goal = QGoal,
	free_variables(QGoal, Bound, [], VarList, false).
free_variables_quant(Vars^QGoal, Bound, Goal, VarList, Module) :- !,
	free_variables_quant(QGoal, Vars^Bound, Goal, VarList, Module).
free_variables_quant(Goal, Bound, Goal, VarList, Module) :-
	( current_built_in((^)/2)@Module -> UseQuant=true ; UseQuant=false ),
	free_variables(Goal, Bound, [], VarList, UseQuant).
	
free_variables(Term, Bound, Vars0, Vars, _) :- var(Term),
	( occurs(Term, Bound) -> Vars = Vars0
	; occurs(Term, Vars0) -> Vars = Vars0
	; Vars = [Term|Vars0]
	).
free_variables(Term, _Bound, Vars0, Vars, _) :- atomic(Term),
	Vars0 = Vars.
free_variables(Term, Bound, Vars0, Vars, UseQuant) :- compound(Term),
	( UseQuant==true, explicit_binding(Term, Bound, NewTerm, NewBound) ->
	    free_variables(NewTerm, NewBound, Vars0, Vars, UseQuant)
	;
	    (
		foreacharg(Argument,Term),
		fromto(Vars0,Vars1,Vars2,Vars),
		param(Bound,UseQuant)
	    do
		free_variables(Argument, Bound, Vars1, Vars2, UseQuant)
	    )
	).

% Traditional, non-ISO feature:
%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

:- mode	explicit_binding(+,+,-,-).
explicit_binding(\+ _,		       Bound, fail,	Bound      ) :- !.
explicit_binding(not(_),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(fail_if(_),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var) :- !.
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.
explicit_binding(coverof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.


% The tool body of ^/2

exquant_body(_, Goal, Module) :-
	untraced_call(Goal, Module).


% For proper tracing behaviour, this file must be in nodbgcomp
% and the metapredicates must be set to unskipped. This will cause
% only the metacalls of the user goals to show up in the trace.

:- unskipped
	exquant_body/3,
	setof_body/4,
	bagof_body/4,
	coverof_body/4,
	findall_body/4.

:- set_flag(setof/3, trace_meta, on).
:- set_flag(setof_body/4, trace_meta, on).
:- set_flag(bagof/3, trace_meta, on).
:- set_flag(bagof_body/4, trace_meta, on).
:- set_flag(coverof/3, trace_meta, on).
:- set_flag(coverof_body/4, trace_meta, on).
:- set_flag(findall/3, trace_meta, on).
:- set_flag(findall_body/4, trace_meta, on).
:- set_flag((^)/2, trace_meta, on).
:- set_flag(exquant_body/3, trace_meta, on).

