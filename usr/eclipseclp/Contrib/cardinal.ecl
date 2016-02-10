%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% Portions created by the Initial Developer are  Copyright (C) 1999-2004.
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
%
% cardinal.ecl      By Francisco Azevedo    1999-2004 (for ECLiPSe)
%
% Set constraints (including 'complement') with great cardinality processing.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(cardinal).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Finite Set Constraints Library").
:- comment(author, "Francisco Azevedo, CENTRIA").
:- comment(status, evolving).

:- export
	op(650, xfx, `= ), `=  /2,
	op(650, xfx, `/=), `/= /2,	%different/2
	op(650, xfx, `$ ), `$  /2,	%disjoint/2
	op(650, xfx, `@ ), `@  /2,	%member_set/2
	op(650, xfx, `-@), `-@ /2,	%not_in_set/2
	op(650, xfx, `::), `:: /2,	%set/4
	op(550, xfx, `>=), `>= /2,	%contains/2
	op(500, yfx, `\/),		%my_set_union/3
	op(400, yfx, `/\),		%my_set_intersection/3
	op(300, yfx, `\ ),		%my_set_difference/3
	cardinality/2,
	domain/2,
	domain/3,
	glb/2,
	poss/2,
	glb_poss/3,
	lub/4,
	lub/2,
%	union_att/2,
%	union_att/3,
	union_var/2,
	minimum/2,
	maximum/2,
	set/4,
	sets/4,
	complement/2,
	complement/3,
	set_labeling/1,
	set_labeling/2,
	refine/2,
	card_labeling/1,
	all_union/2,
	all_disjoint/1,
%For Conjunto compatibility:
	op(650, fx,  #     ), (#)  /2,
	op(650, xfx, `<>   ), `<>  /2,
	op(650, xfx, in    ), in   /2,
	op(650, xfx, notin ), notin/2,
	op(650, xfx, `<    ), `<   /2,
	op(300, yfx, \ )
	.

:- local
	is_list/1.	% shallow list check

:-lib(fd).
:-lib(lists).


:- export struct(cardinal(domain,cardinality,minimum,maximum,union,bounded,glb,lub,bound)).
/*
domain:        set domain in the form [Glb:NIn,Poss:NMax]
cardinality:   FD (integer) variable
minimum:       FD (integer) variable or unused
maximum:       FD (integer) variable or unused
union:         [U,GlbU+PossU,Singles,Lengths] or unused
		U: set variable
		GlbU: ordered list with assured union elements
		PossU: ordered list with possible union elements with counters (X:N)
		Singles: elements where N=1 in PossU
		Lengths: possible lengths of set elements with counters (Len:N) in decreasing order
bounded, glb,
lub, bound:    suspension lists
*/

:-comment(include, cardinal_comments).
:- include([cardinal_util,cardinal_functions]).


:- meta_attribute(cardinal, [unify:my_unify_sets_handler/2,
			print:my_print_set_handler/2]).



S1 `>= S2 :- contains(S1,S2).
S1 `/= S2 :- different(S1,S2).
S1 `$ S2 :- disjoint(S1,S2).
X `@ S :- member_set(X,S).
X `-@ S :- not_in_set(X,S).
S `:: Glb+Poss:Card :- !, set(S,Glb,Poss,[cardinality:Card]).
S `:: Glb+Poss :- set(S,Glb,Poss,[]).
S `:: Glb..Lub :- set_without(Lub,Glb,Poss), set(S,Glb,Poss,[]).
Set1 `= Exp2 :- var(Set1), !, eval_exp(Exp2, Set1).
Exp1 `= Set2 :- var(Set2), !, eval_exp(Exp1, Set2).
Exp1 `= Exp2 :- ground(Exp2), !, eval_exp(Exp2, S), eval_exp(Exp1, S).
Exp1 `= Exp2 :- ground(Exp1), !, eval_exp(Exp1, S), eval_exp(Exp2, S).
Exp1 `= Exp2 :- eval_exp(Exp1, S1), eval_exp(Exp2, S2), S1=S2.
%For Conjunto compatibility:
S1 `<> S2 :- disjoint(S1,S2).
X in S :- member_set(X,S).
X notin S :- not_in_set(X,S).
S1 `< S2 :- contains(S2,S1).

%-------
% 'Global' constraints:
%
all_union([],U):- !, U=[].
all_union([S],U):- !, U=S.
all_union(L, U):- halve(L,L1,L2),
	(is_my_set(U) -> lub(U,Lub), sets([U1,U2],[],Lub,[]) ; true),
	all_union(L1,U1), all_union(L2,U2), my_set_union(U1,U2,U).


all_disjoint(Sets):-
	all_disjoint(Sets, Lubs, Cards),
	all_sets_union(Lubs, [],LubsUnion),
	length(LubsUnion, NMax),
	sum_cards(Cards, SumCards),
	SumCards #<= NMax.

all_disjoint([], [], []).
all_disjoint([H|T], [Lub|Lubs], [Card|Cards]):-
	disjoint_set(T, H),
	lub(H, Lub), cardinality(H, Card),
	all_disjoint(T, Lubs, Cards).

disjoint_set([], _).
disjoint_set([H|T], S):- H `$ S, disjoint_set(T, S).


sets([],_,_,_).
sets([H|T],Glb,Poss,Functions):- set(H,Glb,Poss,Functions), sets(T,Glb,Poss,Functions).

% End of 'global' constraints.
%---

eval_exp(Exp, Set):- var(Exp), !, Set=Exp.
eval_exp(Exp, Set):- is_list(Exp), !, sort(Exp, OrderedList), Set=OrderedList.
eval_exp(Exp1 `\/ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_union(S1,S2,S).
eval_exp(Exp1 `/\ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_intersection(S1,S2,S).
eval_exp(Exp1 `\ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_difference(S1,S2,S).
eval_exp(Exp1 \/ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_union(S1,S2,S).
eval_exp(Exp1 /\ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_intersection(S1,S2,S).
eval_exp(Exp1 \ Exp2, S):- !, eval_exp(Exp1,S1), eval_exp(Exp2,S2), my_set_difference(S1,S2,S).



%--------
% Labeling predicates.
%
set_labeling(Var):- meta(Var), !, set_labeling(up, [Var]).
set_labeling(Vars):- set_labeling(up, Vars).

set_labeling(_,[]).
set_labeling(UpDown, [H|T]):- nonvar(H), !, set_labeling(UpDown, T).
set_labeling(UpDown, [H|T]):-
	refine(UpDown, H),
	set_labeling(UpDown, [H|T]).

refine(up, S):-
	domain(S, [Glb:NIn,[X|Poss]:NMax]), val(X,VX),
	(NIn1 is NIn+1, insert(Glb, VX, Glb1), set_domain(S, [Glb1:NIn1,Poss:NMax])
	;NMax1 is NMax-1, set_poss(S, Poss,NMax1)
	).
refine(down, S):-
	domain(S, [Glb:NIn,[X|Poss]:NMax]), val(X,VX),
	(NMax1 is NMax-1, set_poss(S, Poss,NMax1)
	;NIn1 is NIn+1, insert(Glb, VX, Glb1), set_domain(S, [Glb1:NIn1,Poss:NMax])
	).

card_labeling([]).
card_labeling([H|T]):-
	cardinality(H, C),
	indomain(C),
	card_labeling(T).
%
% End of Labeling predicates.
%--------




%--------
% my_print_set_handler(+Set, +Att)
%Operation : printing the attribute 
%
%Handler : handler(?Meta, -Attribute) 
%
%Description : Printing the attribute in printf/2, 3 when the m option is specified. Meta is the attributed variable being
%printed, Attribute is the term which will be printed as a value for this attribute, qualified by the attribute name. If no
%handler is specified for an attribute, it will not be printed. If there is only one attribute with an associated print handler,
%the attribute value is not qualified with the attribute name. 
%--
my_print_set_handler(Set, Set):- nonvar(Set), !.
my_print_set_handler(SetVar, Info):- get_attribute(cardinal, SetVar, Info).



get_attribute(cardinal, _{cardinal:Attribute}, A):- -?-> A = Attribute.

is_my_set(S):- get_attribute(cardinal, S, Att), nonvar(Att), !.
is_my_set(S):- nonvar(S), is_list(S).


%--------
% Setting cardinal attributes:
% full domain or just poss, and respective suspension lists; union; minimum, maximum
%--

t_set_domain(set_domain(Set, Domain),
	(Domain = [Glb:_,[]:_] -> Set=Glb
	;get_attribute(cardinal, Set, Att) ->
	  Att = cardinal with [domain:[OldGlb:OldNIn,OldPoss:OldNMax],cardinality:C],
	  setarg(domain of cardinal, Att, Domain),
	  Domain = [Glb:NIn,Poss:NMax],
	  (OldNIn==NIn ->
		(OldNMax==NMax -> true
		;check_union_lub(Att, NIn, OldPoss,OldNMax, Poss,NMax),
		 schedule_suspensions(lub of cardinal, Att),
		 schedule_suspensions(bounded of cardinal, Att),
		 C #<= NMax
		)
	  ;check_union_glb(Att, OldGlb, Glb),
	   (OldNMax==NMax -> C #>= NIn
	   ;check_union_lub(Att, NIn, OldPoss,OldNMax, Poss,NMax),
	    C #>= NIn, C #<= NMax,
	    schedule_suspensions(lub of cardinal, Att)
	   ),
	   schedule_suspensions(glb of cardinal, Att),
	   schedule_suspensions(bounded of cardinal, Att),
	   wake
	  )
	;Domain = [Set:_,_]
	)).
:-inline(set_domain/2, t_set_domain/2).
set_domain(Set, Domain):- set_domain(Set, Domain).  %inlined



t_set_poss(set_poss(Set, Poss, NMax),
	(get_attribute(cardinal, Set, Att) ->
	 Att = cardinal with [domain:[Glb:NIn,OldPoss:OldNMax],cardinality:C],
	 (Poss==[] -> Set=Glb
	 ;OldNMax==NMax -> setarg(domain of cardinal, Att, [Glb:NIn,Poss:NMax]) %neccessary for sets with union function, when attaching lengths to poss elements.
	 ;setarg(domain of cardinal, Att, [Glb:NIn,Poss:NMax]),
	  check_union_lub(Att, NIn, OldPoss,OldNMax, Poss,NMax),
	  C #<= NMax,
	  schedule_suspensions(lub of cardinal, Att),
	  schedule_suspensions(bounded of cardinal, Att),
	  wake
	 )
	 ;Poss=[]
	)).
:-inline(set_poss/3, t_set_poss/2).
set_poss(Set, Poss, NMax):- set_poss(Set, Poss, NMax).  %inlined



t_set_cardinality(set_cardinality(Set, Card),
	(get_attribute(cardinal, Set, Att) -> setarg(cardinality of cardinal, Att, Card)
	;length(Set, Card)
	)).
:-inline(set_cardinality/2, t_set_cardinality/2).
set_cardinality(Set, Card):- set_cardinality(Set, Card).  %inlined



t_set_union(set_union(Set, UnionData),
	(get_attribute(cardinal, Set, Att) -> setarg(union of cardinal, Att, UnionData)
	;all_sets_union(Set, [],U), UnionData=[U|_]
	)).
:-inline(set_union/2, t_set_union/2).
set_union(Set, UnionData):- set_union(Set, UnionData).  %inlined


t_set_minimum(set_minimum(Set, Min),
	(get_attribute(cardinal, Set, Att) -> setarg(minimum of cardinal, Att, Min)
	;Set=[Min|_]
	)).
:-inline(set_minimum/2, t_set_minimum/2).
set_minimum(Set, Min):- set_minimum(Set, Min).  %inlined


t_set_maximum(set_maximum(Set, Max),
	(get_attribute(cardinal, Set, Att) -> setarg(maximum of cardinal, Att, Max)
	;reverse(Set, [Max|_])
	)).
:-inline(set_maximum/2, t_set_maximum/2).
set_maximum(Set, Max):- set_maximum(Set, Max).  %inlined

% End of predicates for setting cardinal attributes.
%-------------


%--------
% set_info(+Set, +Cardinality, -Info)
%  Info is a set structure (cardinal with [domain:[Glb:NIn,Poss:NMax],cardinality:N])
% corresponding to Set.
%  If Set is an attributted variable, then Info is its set attributte.
%  If Set is ground, Info is constructed accordingly.
%--
set_info(S, _, Info):- get_attribute(cardinal, S, Att), !, Info=Att.
set_info(S, Card, cardinal with [domain:[S:N,[]:N],cardinality:N]):- var(Card),!,length(S,N).
set_info(S, Card, cardinal with [domain:[S:Card,[]:Card],cardinality:Card]).

set_info(Set, Info):- set_info(Set, _, Info).


assure_set(S, S, Info):- get_attribute(cardinal, S, Info), !.
assure_set(S, SS, cardinal with [domain:[SS:N,[]:N],cardinality:N]):-
	is_list(S), sort(S,SS), length(SS, N), !.


%--------
% Getting cardinal attributes:
% cardinality, domain, bounds; minimum, maximum; union
%--

#(Exp, C):- eval_exp(Exp, S), set_info(S, _, cardinal with cardinality:C).
cardinality(S, C):- set_info(S, _, cardinal with cardinality:C).
domain(S, Card, D):- set_info(S, Card, cardinal with domain:D).
domain(S, D):- set_info(S, _, cardinal with domain:D).
glb_poss(S, Glb, Poss):- set_info(S, n, cardinal with domain:[Glb:_,Poss:_]).
glb(S, G):- set_info(S, n, cardinal with domain:[G:_,_]).
poss(S, P):- set_info(S, n, cardinal with domain:[_,P:_]).
lub(S, Glb,Poss, Lub):- glb_poss(S,Glb,Poss), lub(Glb,Poss,Lub).

t_lub(lub(S, Lub), lub(S,_,_,Lub)).
:-inline(lub/2, t_lub/2).
lub(S, Lub):- lub(S, Lub). %inlined

minimum(S, Min):-
	get_attribute(cardinal, S, cardinal with minimum:MinAtt), !,
	(free(MinAtt) -> minimum_function(Min,S) ; Min = MinAtt).
minimum([Min|_], Min).

maximum(S, Max):-
	get_attribute(cardinal, S, cardinal with maximum:MaxAtt), !,
	(free(MaxAtt) -> maximum_function(Max,S) ; Max = MaxAtt).
maximum(S, Max):- reverse(S, [Max|_]).

%union_att(+SetVar, +UnionVar, -UnionAttribute)
union_att(S, _, U):-
	get_attribute(cardinal, S, cardinal with union:UnionAtt), !,
	(free(UnionAtt) -> U = [Union|_], union_function(Union,S), union_att(S, Union, U)
	;U = UnionAtt
	).
union_att(S, U, [U,U+[],[],[]]):- var(U), !, all_sets_union(S, [],U).
	%INEFFICIENT. When S is ground, attributes are lost
union_att(_, U, [U,U+[],[],[]]).

t_union_att(union_att(S,U), union_att(S,_,U)).
:-inline(union_att/2, t_union_att/2).
union_att(S,U):- union_att(S,U). %inlined

%t_union_var(union_var(S,U), union_att(S,_,[U|_])).
%:-inline(union_var/2, t_union_var/2).
%union_var(S,U):- union_var(S,U). %inlined
union_var(S,U):- union_att(S,_,[U|_]).

% End of predicates for getting cardinal attributes.
%-------------



%--------
% set_equality(+SetVar, +Glb,+Poss)
%  Constrain bounds of SetVar when equalled to a set with bounds Glb and Poss.
%--
t_set_equality(set_equality(S, GlbArg,PossArg),
	(glb_poss(S, Glb1,Poss1),
	verify_inclusion(GlbArg, Glb1,Poss1, Glb2), %Glb2 is the rest of Glb1 (Glb1\GlbArg)
	verify_inclusion(Glb2, [],PossArg, _), %no need to check inclusion of Glb2 in GlbArg
	set_union(GlbArg, Glb2, In, 0,NIn),
	set_intersection(Poss1, PossArg, Poss, NIn,NMax),
	set_domain(S, [In:NIn,Poss:NMax])
	)).

:-inline(set_equality/3, t_set_equality/2).

set_equality(S, GlbArg,PossArg):- set_equality(S, GlbArg,PossArg).  %inlined


%--------
% my_unify_sets_handler(+Set, +Att)
%  Handler for sets unification. A set with attributte Att was unified with Set.
%  Corresponds to the set equality constraint.
%--
my_unify_sets_handler(_, Att):- var(Att), !.
my_unify_sets_handler(Set, Att):-
	nonvar(Set), !, 
	sorted_length(Set, N), !,
	Att = cardinal with [domain:[Glb:NIn,Poss:NMax],cardinality:C],
	verify_inclusion(Set, Glb,Poss, []),
	setarg(domain of cardinal, Att, [Set:N,[]:N]),
	(N == NIn -> true ; check_union_glb(Att, Glb, Set)),
	(N == NMax -> true ; check_union_lub(Att, NIn, Poss,NMax, [],N)),
	(N == NIn -> true ; schedule_suspensions(glb of cardinal, Att)),
	(N == NMax -> true ; schedule_suspensions(lub of cardinal, Att)),
	C #= N,
	schedule_suspensions(bound of cardinal, Att),
	schedule_suspensions(bounded of cardinal, Att).
my_unify_sets_handler(SetVar, Att):-
	set_info(SetVar, AttVar),
	merge_suspension_lists(glb of cardinal, Att, glb of cardinal, AttVar),
	merge_suspension_lists(lub of cardinal, Att, lub of cardinal, AttVar),
	merge_suspension_lists(bounded of cardinal, Att, bounded of cardinal, AttVar),
	merge_suspension_lists(bound of cardinal, Att, bound of cardinal, AttVar),
	Att = cardinal with [domain:[Glb:_,Poss:_],cardinality:C1,
			minimum:Min1,maximum:Max1,union:Union1],
	AttVar = cardinal with [cardinality:C2,minimum:Min2,maximum:Max2,union:Union2],
	schedule_suspensions(bound of cardinal, AttVar),
	C1 #= C2, Min1 = Min2, Max1 = Max2,   %may wake
	((var(Union1) ; var(Union2)) -> Union1=Union2
	;Union1 = [UVar1|_],
	 Union2 = [UVar2|_],
	 UVar1 = UVar2
	),
	set_equality(SetVar, Glb,Poss).



%--------
% set(?Set, ?Glb,?Poss, +Functions)
%  Set variable declaration:
%  Set is constrained by bounds Glb and Poss, and has a list of Functions,
% including Cardinality, which can be a finite domain variable or its domain.
%--
set(S, Glb,Possible, Functions):-
	ground(S), !,
	sort(S, Set),
	(ground(Glb), ground(Possible) ->
		sort(Glb, SGlb), sort(Possible, SPossible),
		verify_inclusion(SGlb, Set,[], RestSet),  % SGlb contained in Set
		set_without(RestSet, SPossible, [])	  % Set contained in Glb U Possible
	; (var(Glb) -> Glb=Set ; sort(Glb,Set)),
	  (var(Possible) -> Possible=[] ; true)
	),
	check_ground_functions(Functions, Set).
set(S, Glb,Possible, Functions):-
	is_my_set(S), !,
	(ground(Glb), ground(Possible) ->
		my_setof(X, (member(X,Glb),ground(X)), GlbArg),
		my_setof(Y, (member(Y,Possible),ground(Y),
				\+ my_member(Y,GlbArg)), PossArg),
		set_equality(S, GlbArg,PossArg)
	; glb_poss(S, Glb1,Poss1),
	  (var(Glb) -> Glb=Glb1 ; sort(Glb,Glb1)),
	  (var(Possible) -> Possible=Poss1 ; sort(Possible,Poss1))
	),
	check_new_functions(Functions, S).
set(S, Glb,Possible, Functions):-
	(ground(Glb) -> true ; writeln('Non-ground glb'), fail),
	my_setof(X, member(X,Glb), In),
	length(In, NIn),
	(ground(Possible) -> true ; writeln('Non-ground lub'), fail),
	my_setof(Y, (member(Y,Possible), \+ my_member(Y,In)), Poss),
	length(Poss, NPoss),
	NMax is NIn+NPoss,
	SetAtt = cardinal with [domain:[In:NIn,Poss:NMax], cardinality:_,
				minimum:_, maximum:_, union:_],
	add_attribute(S, SetAtt, cardinal),
	init_suspension_list(bounded of cardinal, SetAtt),
	init_suspension_list(glb of cardinal, SetAtt),
	init_suspension_list(lub of cardinal, SetAtt),
	init_suspension_list(bound of cardinal, SetAtt),
	check_functions(Functions, S).





%--------
% different(?Set1, ?Set2)
%  Constraint: Set1 and Set2 are different sets.
%--
different(S1, S2):-
	ground(S1), ground(S2), !,
	is_list(S1), is_list(S2),
	(member(X,S1), \+ member(X,S2) ; member(X,S2), \+ member(X,S1)), !.
different(S1, S2):-
	assure_set(S1, Set1, _),
	assure_set(S2, Set2, _),
	not_same_set(Set1,Set2).

not_same_set(S1,S2):- S1 \== S2,
	(var(S1) -> suspend(not_same_set(S1,S2), 3, S1->cardinal:bound)
	;var(S2) -> suspend(not_same_set(S1,S2), 3, S2->cardinal:bound)
	;true
	).




%--------
% disjoint(?Set1, ?Set2)
%  Constraint: Set1 and Set2 are disjoint sets.
%--
disjoint(S1, S2):-
	ground(S1), ground(S2), !,
	assure_set(S1,Set1,_), assure_set(S2,Set2,_),
	set_intersection(Set1,Set2,[]).
disjoint(S1, S2):-
	assure_set(S1, Set1, cardinal with [domain:[Glb1:_,_],cardinality:C1]),
	assure_set(S2, Set2, cardinal with [domain:[Glb2:_,_],cardinality:C2]),
	set_intersection(Glb1,Glb2,[]),
	suspend_and_call(poss_without_glb(Set1,C1,Set2,SuspP1), 3,Set2->cardinal:glb, SuspP1),
	suspend_and_call(poss_without_glb(Set2,C2,Set1,SuspP2), 3,Set1->cardinal:glb, SuspP2),
		%These poss_without_glb/4 suspensions should have higher priority (lower number)
		%than the check_special_disjoint/3 demon suspension below.
	suspend_and_call(check_special_disjoint(Set1, Set2, [SuspP1,SuspP2,SuspC]), 4,
		[Set1,Set2]->cardinal:bound, SuspC),  %lower priority than both poss_without_glb/4 above
	lub(Set1,Lub1), lub(Set2,Lub2),
	set_union(Lub1,Lub2, _, 0,U),
	C1+C2 #<= U.

:- demon poss_without_glb/4.
poss_without_glb(S1,C1, S2, Susp):-
	set_info(S1, C1, cardinal with domain:[_:NIn1,Poss1:_]),
	glb(S2, Glb2),
	set_without(Poss1, Glb2, NewPoss1, 0,NNewPoss1),
	NewNMax1 is NIn1+NNewPoss1,
	set_poss(S1, NewPoss1,NewNMax1),
	(var(S2) -> true ; kill_suspension(Susp)).

:-demon check_special_disjoint/3.
check_special_disjoint(S1, S2, Susps):- S1==S2, !, kill_suspensions(Susps), S1=[].
check_special_disjoint(S1, S2, Susps):- (nonvar(S1) ; nonvar(S2)), !, kill_suspensions(Susps).
check_special_disjoint(_,_,_).





%--------
% contains(+Set1, ?Set2)
%  Constraint: Set1 contains (>=) Set2.
%--
contains(S1, S2):-
	ground(S1), ground(S2), !,
	is_list(S1), is_list(S2),
	\+ (member(X,S2), \+ member(X,S1)).
contains(S1, S2):-
	is_my_set(S2), !,
	assure_set(S1, Set1, cardinal with cardinality:C1),
	assure_set(S2, Set2, cardinal with cardinality:C2),
	C1 #>= C2,
	suspend_and_call(include_elements(Set2,Set1,C1,SuspI), 3, Set2->cardinal:glb, SuspI),
		%This include_elements/4 suspension should have higher priority (lower number)
		%than the lub_union/4 demon suspension of the union constraint
		%and the lub_arg1_diff/4 predicate suspension of set_difference constraint.
	suspend_and_call(limit_elements(Set2,C2,Set1,C1,SuspE), 3, Set1->cardinal:lub, SuspE),
	suspend_and_call(check_inclusion(Set2,Set1,[SuspI,SuspE,SuspC]), 4,
		[Set1,Set2]->cardinal:bound, SuspC). %lower priority than both suspensions above
contains(S1, S2):-
	assure_set(S1, Set1, cardinal with domain:[Glb1:_,Poss1:_]),
	lub(Glb1, Poss1, Lub1),
	set(S2, [],Lub1, []),
	contains(Set1, S2).

:- demon include_elements/4.
include_elements(In, Container,CC, Susp):-
	domain(Container, CC, [GlbC:NInC,PossC:NMaxC]),
	glb(In, GlbI),
	include_elements(_, GlbI, GlbC,PossC, GlbC1, PossC1, NInC,NInC1),
	set_domain(Container, [GlbC1:NInC1,PossC1:NMaxC]),
	(var(In) -> true ; kill_suspension(Susp)).

:- demon limit_elements/5.
limit_elements(In,CIn, Container,CC, Susp):-
	domain(Container, CC, [GlbC:_,PossC:_]),
	domain(In, CIn, [_:NInI,PossI:_]),
	limit_elements(PossI, GlbC,PossC, NewPossI, NInI,NewNMaxI),
	set_poss(In, NewPossI,NewNMaxI),
	(var(Container) -> true ; kill_suspension(Susp)).

:- demon check_inclusion/3.
check_inclusion(In, Container, Susps):-
	(In==Container ; nonvar(In) ; nonvar(Container)), !,
	kill_suspensions(Susps).
check_inclusion(_, _, _).



%--------
% include_elements(?Bound, +Set, +Glb,+Poss, -NewGlb,-NewPoss, +NIn,-NewNIn)
%
%  Definitely Include elements of Set in set represented by Glb and Poss,
% These elements must be already in Glb or be removed from Poss to Glb,
% yielding NewGlb and NewPoss. NIn is Glb's length, NewNIn is NewGlb's length.
%  All sets are ordered lists.
%  Bound just indicates which of Glb or Poss is tried to find the first
% element of Set. It is there so that in the end of last clause (when we know
% that it must be searched in Poss) we can call include_elements(poss,...
% and only the "poss" clauses will be considered, without unnecessary
% unifications and comparisons (@<).
%--
include_elements(nil, [], Glb,Poss, Glb,Poss, N,N):- !.
include_elements(glb, [H|T], [H|GlbIn],PossIn, [H|GlbOut],PossOut, Ni,No):-
	!, include_elements(_, T, GlbIn,PossIn, GlbOut,PossOut, Ni,No).
include_elements(glb, [H|T], [X|GlbIn],PossIn, [X|GlbOut],PossOut, Ni,No):-
	X @< H, !, include_elements(_, [H|T], GlbIn,PossIn, GlbOut,PossOut, Ni,No).
include_elements(poss, [H|T], GlbIn,[HP|PossIn], [H|GlbOut],PossOut, Ni,No):-
	val(HP,H), !, N1 is Ni+1,
	include_elements(_, T, GlbIn,PossIn, GlbOut,PossOut, N1,No).
include_elements(poss, [H|T], GlbIn,[HP|PossIn], GlbOut,[HP|PossOut], Ni,No):-
	val(HP,ValHP), ValHP @< H, !,
	include_elements(poss, [H|T], GlbIn,PossIn, GlbOut,PossOut, Ni,No).


%--------
% limit_elements(+InitialSet, +Set1,+Set2, -FinalSet,+Ni,-No)
%  FinalSet = InitialSet /\ (Set1 \/ Set2)
%  FinalSet is InitialSet without elements that are not present
% neither in Set1 nor in Set2. Its cardinality is No-Ni.
% All sets are ordered lists.
%--
limit_elements([], _,_, [],N,N).
limit_elements([H|T], Glb,Poss, Lo,Ni,No):-
	val(H,VH),
	remove(VH, Glb, _, Glb1, Res1),
	(Res1==true -> N1 is Ni+1, Lo=[H|L],
		limit_elements(T, Glb1,Poss, L,N1,No)
	; remove(H, Poss, _, Poss1, Res2),
	  (Res2==true -> N1 is Ni+1, Lo=[H|L],
		  limit_elements(T, Glb1,Poss1, L,N1,No)
	  ; limit_elements(T, Glb1,Poss1, Lo,Ni,No)
	  )
	).





%--------
% member_set(?Element, +Set)
%  Constraint: Element E Set.
%--
member_set(X, S):-
	ground(X), !,
	assure_set(S, Set, cardinal with domain:[Glb:NIn,Poss:NMax]),
	remove(X, Glb, _GlbButX, Front, Tail, Result),
	(Result = true -> true
	; remove(X, Poss, NewPoss),
	  Front = NewGlb-Diff,
	  Diff = [X|Tail],
	  NIn1 is NIn+1,
	  set_domain(Set, [NewGlb:NIn1,NewPoss:NMax])
	).
member_set(X, S):- nonvar(S), S=[Elem], !, X=Elem.
member_set(X, S):- suspend(member_set(X,S), 3, [X,S]->inst).


%--------
% not_in_set(?Element, +Set)
%  Constraint: Element is not in Set.
%--
not_in_set(X, S):-
	ground(X), !,
	assure_set(S, Set, cardinal with domain:[Glb:_,Poss:NMax]),
	\+ my_member(X, Glb),
	(remove(X, Poss, NewPoss) -> NMax1 is NMax-1, set_poss(Set, NewPoss,NMax1)
	;true
	).
not_in_set(X, S):- suspend(not_in_set(X,S), 3, X->inst).





%--------
% my_set_union(?Set1, ?Set2, ?SetUnion)
%  Constraint: SetUnion is the union of Set1 and Set2 (i.e. Set1 U Set2).
%--
my_set_union(Set1, Set2, Set3):-
	is_my_set(Set3), !,
	assure_set(Set3, S3, cardinal with cardinality:C3),
	contains(S3, Set1),
	contains(S3, Set2),
	assure_set(Set1, S1, cardinal with cardinality:C1),
	assure_set(Set2, S2, cardinal with cardinality:C2),
	(special_union(S1,S2,S3, Goal) -> call(Goal)
	;C3 #<= C1+C2,
	 suspend_and_call(lub_union(S1,S2,S3,SuspL), 4, [S1,S2]->cardinal:lub, SuspL),
		%lub_union/4 demon suspension should have lower priority (higher number)
		%than contains/2 suspensions, namely the include_elements/4 demon.
	 suspend_and_call(min_card_union(S1,S2,C2,C3,Susp_m1), 5,
		[S1->cardinal:glb,S2->cardinal:lub], Susp_m1),
	 suspend_and_call(min_card_union(S2,S1,C1,C3,Susp_m2), 5,
		[S1->cardinal:lub,S2->cardinal:glb], Susp_m2),
	 suspend_and_call(glb_arg_union(S1,S2,C2,S3,Susp_g1), 5, [S1,S3]->inst, Susp_g1), %[S1->cardinal:lub,S3->cardinal:glb]
	 suspend_and_call(glb_arg_union(S2,S1,C1,S3,Susp_g2), 5, [S2,S3]->inst, Susp_g2), %[S2->cardinal:lub,S3->cardinal:glb]
	 suspend_and_call(check_special_union(S1,S2,S3,[SuspC,SuspL,Susp_m1,Susp_m2,Susp_g1,Susp_g2]),
		2, [S1,S2]->cardinal:bound, SuspC),
	 lub(S1, Glb1,_Poss1, Lub1),
	 lub(S2, Glb2,_Poss2, Lub2),
	 set_intersection(Glb1, Glb2, _, 0,NGI),
	 set_intersection(Lub1, Lub2, _, 0,NLI),
	 C1::DC1,
	 C2::DC2,
	 set_without(Glb1, Lub2, _, 0,N12),
	 set_without(Glb2, Lub1, _, 0,N21),
	 or_card_dom(DC1,DC2, NGI,NLI, N12,N21, [],CDom),
	 C3::CDom
	).
my_set_union(S1, S2, S3):-
	assure_set(S1, Set1, cardinal with domain:[Glb1:_,Poss1:_]),
	assure_set(S2, Set2, cardinal with domain:[Glb2:_,Poss2:_]),
	set_union(Glb1, Glb2, Glb1u2),		%(Poss1 \ Glb2) U (Poss2 \ Glb1) =
	set_without(Poss1, Glb2, Poss3_1),	% (Lub1 \ (Glb1 U Glb2)) U
	set_without(Poss2, Glb1, Poss3_2),	% (Lub2 \ (Glb1 U Glb2)) =
	set_union(Poss3_1, Poss3_2, Poss3),	% (Lub1 U Lub2) \ (Glb1 U Glb2) =
	val_list(Poss3, ValPoss3),
	set(S3, Glb1u2,ValPoss3, []),		% Lub3 \ Glb3 = Poss3
	my_set_union(Set1, Set2, S3).


:-demon check_special_union/4.
check_special_union(S1,S2,S3, Susps):-
	special_union(S1,S2,S3, Goal), !,
	kill_suspensions(Susps),
	call(Goal).
check_special_union(_,_,_, _).

special_union(S1,S2,S3, true):- (S1==S3 ; S2==S3), !.
special_union(S1,S2,S3, S2=S3):- S1==[], !.
special_union(S1,S2,S3, S1=S3):- S2==[], !.
special_union(S1,S2,S3, S3=S1):- S1==S2, !.
special_union(S1,S2,S3, (set_union(S1,S2,U),S3=U)):- nonvar(S1), nonvar(S2), !.
special_union(S1,S2,S3, S3=S2):-
	nonvar(S1), glb(S2, Glb2),
	set_without(S1, Glb2, []), !.	% S2 >= S1
special_union(S1,S2,S3, S3=S1):-
	nonvar(S2), glb(S1, Glb1),
	set_without(S2, Glb1, []), !.	% S1 >= S2


:-demon min_card_union/5.
min_card_union(S1, S2,C2, C3, Susp):-
	glb(S1, Glb1),
	glb(S2,Glb2), poss(S2,Poss2),
	set_without(Glb1, Glb2, Glb1_Glb2),
	set_without(Glb1_Glb2, Poss2, _Glb1_U2, 0,NIn3_1),
	C3 #>= C2+NIn3_1,
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

%It is assumed that Glb1 and Glb2 are already included in Glb3, thanks to `>= constraints.
:-demon lub_union/4.
lub_union(S1, S2, S3, Susp):-
	wake,	%Force higher priority scheduled suspensions to execute
	poss(S1, Poss1),
	poss(S2, Poss2),
	domain(S3, [_:NIn3,Poss3:_]),
	limit_elements(Poss3, Poss1,Poss2, NewPoss3,0,NNewPoss3),
	NewNMax3 is NIn3+NNewPoss3,
	set_poss(S3, NewPoss3,NewNMax3),
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

:-demon glb_arg_union/5.
glb_arg_union(S1, S2,C2, S3, Susp):-
	glb_poss(S1, Glb1,Poss1),
	glb(S3, Glb3),
	set_without(Glb3, Glb1, Glb3_1),
	set_without(Glb3_1, Poss1, Glb3_Lub1),
	include_elements(Glb3_Lub1, S2,C2, _),
	((var(S1);var(S3)) -> true ; kill_suspension(Susp)).


%--------------
%or_card_dom(+DomainCX, +DomainCY, +NGI,+NLI, +NXY,+NYX, +DomainIn,-DomainOut)
% For the union of sets X and Y, DomainCX is the domain of #X,
%DomainCY is the domain of #Y, NGI=#(GlbX /\ GlbY), NLI=#(LubX /\ LubY),
%NXY=#(GlbX\LubY), NYX=#(GlbY\LubX), DomainIn is the list of possible ranges for
%the domain of #(X\/Y) so far, and DomainOut is the final one.
%--
or_card_dom([],_, _,_, _,_, D,D):- !.
or_card_dom(DCX,DCY, NGI,NLI, NXY,NYX, Di,Do):-
	remove_range(DCX, RX, TX),
	ocd(DCY, RX, NGI,NLI, NXY,NYX, Di,D1),
	or_card_dom(TX,DCY, NGI,NLI, NXY,NYX, D1,Do).

%--------------
%ocd(+DomainCY, +RangeX, +NGI,+NLI, +NXY,+NYX, +DomainIn,-DomainOut)
% For the union of sets X and Y, DomainCY is the domain of #Y,
%RangeX is a range from the domain of #X, NGI=#(GlbX /\ GlbY), NLI=#(LubX /\ LubY),
%NXY=#(GlbX\LubY), NYX=#(GlbY\LubX), DomainIn is the list of possible ranges for
%the domain of #(X\/Y) so far, and DomainOut is the final one.
%--
ocd([],_, _,_, _,_, D,D):- !.
ocd(DCY,AX..BX, NGI,NLI, NXY,NYX, Di,Do):-
	remove_range(DCY, AY..BY, TY),
	AZ1 is max(AX+NYX,AY+NXY),
	AZ is max(AZ1,AX+AY-NLI),
	BZ is BX+BY-NGI,
	(BZ >= AZ -> D1=[AZ..BZ|Di] ; D1=Di),
	ocd(TY,AX..BX, NGI,NLI, NXY,NYX, D1,Do).






%--------
% my_set_intersection(?Set1, ?Set2, ?SetIntersection)
%  Constraint: SetIntersection is the intersection of Set1 and Set2 (i.e. Set1/\Set2).
%--
my_set_intersection(Set1, Set2, Set3):-
	is_my_set(Set3), !,
	assure_set(Set3, S3, cardinal with cardinality:C3),
	contains(Set1, S3),
	contains(Set2, S3),
	assure_set(Set1, S1, cardinal with cardinality:C1),
	assure_set(Set2, S2, cardinal with cardinality:C2),
	suspend_and_call(glb_intersection(S1,S2,S3,C3,SuspG), 4,
		[S1,S2]->cardinal:glb, SuspG),
	suspend_and_call(max_card_intersection(S1,C1,S2,C3,SuspM1), 5,
		[S1->cardinal:glb,S2->cardinal:lub], SuspM1),
	suspend_and_call(max_card_intersection(S2,C2,S1,C3,SuspM2), 5,
		[S1->cardinal:lub,S2->cardinal:glb], SuspM2),
	suspend_and_call(min_card_intersection(S2,S1,S3,SuspMin), 6,
		[S1,S2]->cardinal:lub, SuspMin),
	suspend_and_call(lub_arg_intersection(S1,S2,C2,S3,SuspL1), 5,
		[S1,S3]->inst, SuspL1), %[S1->cardinal:glb,S3->cardinal:lub]
	suspend_and_call(lub_arg_intersection(S2,S1,C1,S3,SuspL2), 5,
		[S2,S3]->inst, SuspL2), %[S2->cardinal:glb,S3->cardinal:lub]
	suspend_and_call(check_special_intersection(S1,S2,S3,[SuspC,SuspG,SuspM1,SuspM2,SuspMin,SuspL1,SuspL2]),
		2, [S1,S2]->cardinal:bound, SuspC),
	lub(S1, Glb1,_Poss1, Lub1),
	lub(S2, Glb2,_Poss2, Lub2),
	set_union(Lub1, Lub2, _, 0,NU),
	C1::DC1,
	C2::DC2,
	set_without(Glb1, Lub2, _, 0,N12),
	set_without(Glb2, Lub1, _, 0,N21),
	and_card_dom(DC1,DC2, NU, N12,N21, [],CDom),
	C3::CDom.
my_set_intersection(S1, S2, S3):-
	assure_set(S1, Set1, cardinal with domain:[Glb1:_,Poss1:_]),
	assure_set(S2, Set2, cardinal with domain:[Glb2:_,Poss2:_]),
	set_intersection(Glb1, Glb2, Glb3),
	lub(Glb1, Poss1, Lub1),
	lub(Glb2, Poss2, Lub2),
	set_intersection(Lub1, Lub2, Lub3),
	set_without(Lub3, Glb3, Poss3),
	set(S3, Glb3,Poss3, []), %set(S3, Glb3,Poss3, _,_),
	my_set_intersection(Set1, Set2, S3).


:-demon check_special_intersection/4.
check_special_intersection(S1,S2,S3, Susps):- (S1==S3 ; S2==S3), !, kill_suspensions(Susps).
check_special_intersection(S1,S2,S3, Susps):- (S1==[] ; S2==[]), !, kill_suspensions(Susps), S3=[].
check_special_intersection(S1,S2,S3, Susps):- S1==S2, !, kill_suspensions(Susps), S3=S1.
check_special_intersection(S1,S2,S3, Susps):-
	nonvar(S1), nonvar(S2), !, kill_suspensions(Susps), set_intersection(S1,S2,I), S3=I.
check_special_intersection(S1,S2,S3, Susps):-
	nonvar(S1), glb(S2, Glb2), poss(S2, Poss2),
	verify_inclusion(Glb2, S1,[], S1_Glb2),	%
	set_without(Poss2, S1_Glb2, []), !,	% S1 >= S2
	kill_suspensions(Susps), S3=S2.
check_special_intersection(S1,S2,S3, Susps):-
	nonvar(S2), glb(S1, Glb1), poss(S1, Poss1),
	verify_inclusion(Glb1, S2,[], S2_Glb1),	%
	set_without(Poss1, S2_Glb1, []), !,	% S2 >= S1
	kill_suspensions(Susps), S3=S1.
check_special_intersection(_,_,_, _).


:-demon max_card_intersection/5.
max_card_intersection(S1,C1, S2, C3, Susp):-
	glb(S1, Glb1),
	glb(S2, Glb2), poss(S2, Poss2),
	set_without(Glb1, Glb2, Glb1_Glb2),
	set_without(Glb1_Glb2, Poss2, _Glb1_U2, 0,NIn1_2),
	C3 #<= C1-NIn1_2,
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

:-demon min_card_intersection/4.
min_card_intersection(S1, S2, S3, Susp):-
	set_info(S1, cardinal with [domain:[_,_:NMax1],cardinality:C1]),
	set_info(S2, cardinal with [domain:[_,_:NMax2],cardinality:C2]),
	set_info(S3, cardinal with [domain:[_,_:NMax3],cardinality:C3]),
	C3 #>= C1+C2-(NMax1+NMax2-NMax3),
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

:-demon glb_intersection/5.
glb_intersection(S1, S2, S3,C3, Susp):-
	glb(S1, Glb1),
	glb(S2, Glb2),
	set_intersection(Glb1, Glb2, Glb1i2),
	include_elements(Glb1i2, S3,C3, _),
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

:-demon lub_arg_intersection/5.
lub_arg_intersection(S1, S2,C2, S3, Susp):-
	glb(S1, Glb1),
	glb(S3, Glb3), poss(S3, Poss3),
	set_without(Glb1, Glb3, Glb1_3),
	set_without(Glb1_3, Poss3, Glb1_Lub3),
	exclude_elements(Glb1_Lub3, S2,C2),
	((var(S1);var(S3)) -> true ; kill_suspension(Susp)).

exclude_elements(Elements, Set,CS):-
	set_info(Set, CS, cardinal with domain:[Glb:NIn,Poss:_]),
	set_intersection(Elements, Glb, []),
	set_without(Poss, Elements, NewPoss, 0,NNewPoss),
	NewNMax is NIn+NNewPoss,
	set_poss(Set, NewPoss,NewNMax).


%--------------
%and_card_dom(+DomainCX, +DomainCY, +NU, +NXY,+NYX, +DomainIn,-DomainOut)
% For the intersection of sets X and Y, DomainCX is the domain of #X,
%DomainCY is the domain of #Y, NU=#(LubX \/ LubY), NXY=#(GlbX\LubY),
%NYX=#(GlbY\LubX), DomainIn is the list of possible ranges for the domain
%of #(X/\Y) so far, and DomainOut is the final one.
%--
and_card_dom([],_, _, _,_, D,D):- !.
and_card_dom(DCX,DCY, NU, NXY,NYX, Di,Do):-
	remove_range(DCX, RX, TX),
	acd(DCY, RX, NU, NXY,NYX, Di,D1),
	and_card_dom(TX,DCY, NU, NXY,NYX, D1,Do).

%--------------
%acd(+DomainCY, +RangeX, +NU, +NXY,+NYX, +DomainIn,-DomainOut)
% For the intersection of sets X and Y, DomainCY is the domain of #Y,
%RangeX is a range from the domain of #X, NU=#(LubX \/ LubY), NXY=#(GlbX\LubY),
%NYX=#(GlbY\LubX), DomainIn is the list of possible ranges for the domain
%of #(X/\Y) so far, and DomainOut is the final one.
%--
acd([],_, _, _,_, D,D):- !.
acd(DCY,AX..BX, NU, NXY,NYX, Di,Do):-
	remove_range(DCY, AY..BY, TY),
	AZ is AX+AY-NU,
	BZ is min(BX-NXY,BY-NYX),
	(BZ >= AZ -> D1=[AZ..BZ|Di] ; D1=Di),
	acd(TY,AX..BX, NU, NXY,NYX, D1,Do).

%--------------
%remove_range(+Domain, -Range, -Rest)
% Remove a Range from a cardinality Domain. Single and consecutive elements
%are converted in ranges (A..B). Rest is Domain without Range.
%--
remove_range([A..B|T], A..B, T):- !.
remove_range([R], R..R, []):- !.
remove_range([H,M..N|T], H..H, [M..N|T]):- !.
remove_range([A,B|T], A..B, T):- A =:= B-1, !.
remove_range([A,B|T], A..A, [B|T]).







%--------
% my_set_difference(?Set1, ?Set2, ?SetDifference)
%  Constraint: SetDifference is the difference between Set1 and Set2 (i.e. Set1\Set2).
%--
my_set_difference(Set1, Set2, Set3):-
	is_my_set(Set3), !,
	assure_set(Set3, S3, cardinal with cardinality:C3),
	contains(Set1, S3),
	disjoint(Set2, S3),
	assure_set(Set1, S1, cardinal with cardinality:C1),
	assure_set(Set2, S2, cardinal with cardinality:C2),
	C3 #>= C1-C2,
	suspend_and_call(glb_difference(S1,S2,S3,C3,SuspG), 4,
		[S1->cardinal:glb,S2->cardinal:lub], SuspG),
	suspend_and_call(glb_arg2_diff(S1,S2,C2,S3,SuspG2), 7,
		[S1->cardinal:glb,S3->cardinal:lub], SuspG2),
	suspend_and_call(max_card_difference_glb(S1,C1,S2,C3,SuspDG), 5,
		[[S1,S2]->cardinal:glb], SuspDG),		%,C1->fd:max,C3->fd:min
	suspend_and_call(max_card_difference_lub(S1,S2,C3,SuspDL), 7,
		[[S1,S2]->cardinal:lub], SuspDL),		%,[C2,C3]->fd:min
	suspend_and_call(check_special_difference(S1,S2,S3,[SuspC,SuspG,SuspG2,SuspDG,SuspDL,SuspL1]),
		2, [S1,S2]->cardinal:bound, SuspC),
	(var(S3) -> suspend(lub_arg1_diff(S1,C1,S2,S3), 5, [S3->inst], SuspL1) ; true),
		%Priority must be lower (higher number) than suspension of contains(S1,S3) (which inserts Glb3 in Glb1)
	lub_arg1_diff(S1,C1,S2,S3),
	lub(S1, Glb1,_Poss1, Lub1),
	lub(S2, Glb2,_Poss2, Lub2),
	set_union(Lub1, Lub2, _, 0,NU),
	C1::DC1,
	C2::DC2,
	set_intersection(Glb1, Glb2, _, 0,NGI),
	set_intersection(Lub1, Lub1, _, 0,NLI),
	diff_card_dom(DC1,DC2, NU, NGI,NLI, [],CDom),
	C3::CDom.
my_set_difference(S1, S2, S3):-
	assure_set(S1, Set1, cardinal with domain:[Glb1:_,Poss1:_]),
	assure_set(S2, Set2, cardinal with domain:[Glb2:_,Poss2:_]),
	lub(Glb1, Poss1, Lub1),
	lub(Glb2, Poss2, Lub2),
	set_without(Glb1, Lub2, Glb3),
	set_without(Lub1, Glb2, Lub3),
	set_without(Lub3, Glb3, Poss3),
	set(S3, Glb3,Poss3, []),
	my_set_difference(Set1, Set2, S3).

:-demon check_special_difference/4.
check_special_difference(S1,_S,S3, Susps):- S1==[], !, kill_suspensions(Susps), S3=[].
check_special_difference(S1,S2,S3, Susps):- S2==[], !, kill_suspensions(Susps), S1=S3.
check_special_difference(S1,S2,S3, Susps):- S1==S2, !, kill_suspensions(Susps), S3=[].
check_special_difference(S1,_S,S3, Susps):- S1==S3, !, kill_suspensions(Susps).
check_special_difference(S1,S2,S3, Susps):-
	nonvar(S1), nonvar(S2), !, kill_suspensions(Susps), set_without(S1,S2,W), S3=W.
check_special_difference(S1,S2,S3, Susps):-
	nonvar(S1), glb_poss(S2, Glb2, Poss2),
	set_intersection(Glb2, S1, []),
	set_intersection(Poss2, S1, []), !,	% disjoint(S1,S2)
	kill_suspensions(Susps), S3=S1.
check_special_difference(S1,S2,S3, Susps):-
	nonvar(S2), glb_poss(S1, Glb1, Poss1),
	set_intersection(Glb1, S2, []),
	set_intersection(Poss1, S2, []), !,	% disjoint(S1,S2)
	kill_suspensions(Susps), S3=S1.
check_special_difference(_,_,_, _).

:-demon glb_difference/5.
glb_difference(S1, S2, S3,C3, Susp):-
	glb(S1, Glb1),
	glb_poss(S2, Glb2, Poss2),
	set_without(Glb1, Glb2, Glb1_2),
	set_without(Glb1_2, Poss2, Glb1_Lub2),
	include_elements(Glb1_Lub2, S3,C3, _),
	((var(S1);var(S2)) -> true ; kill_suspension(Susp)).

:-demon glb_arg2_diff/5.
glb_arg2_diff(S1, S2,C2, S3, Susp):-
	glb(S1, Glb1),
	glb_poss(S3, Glb3, Poss3),
	set_without(Glb1, Glb3, Glb1_3),
	set_without(Glb1_3, Poss3, Glb1_Lub3),
	include_elements(Glb1_Lub3, S2,C2, _),
	((var(S1);var(S3)) -> true ; kill_suspension(Susp)).

:-demon max_card_difference_glb/5.
max_card_difference_glb(S1,C1, S2, C3, Susp):-
	glb(S1, Glb1),
	glb(S2, Glb2),
	set_intersection(Glb1, Glb2, _Glb1i2, 0,NIn1i2),
	C3 #<= C1-NIn1i2,
	((var(S1);var(S2);var(C1);var(C3)) -> true ; kill_suspension(Susp)).

:-demon max_card_difference_lub/4.
max_card_difference_lub(S1, S2, C3, Susp):-
	lub(S1, Lub1),
	lub(S2, Lub2),
	set_union(Lub1, Lub2, _, 0,NU),
	C3 #<= NU-C2,
	((var(S1);var(S2);var(C2);var(C3)) -> true ; kill_suspension(Susp)).

%It is assumed that Glb3 is already included in Glb1, thanks to the `>= constraint.
lub_arg1_diff(S1,C1, S2, S3):-
	wake,	%Force higher priority scheduled suspensions to execute
	domain(S1, C1, [_:NIn1,Poss1:_]),
	poss(S3, Poss3),
	lub(S2, Lub2),
	limit_elements(Poss1, Lub2,Poss3, NewPoss1,NIn1,NewNMax1),
	set_poss(S1, NewPoss1,NewNMax1).



%--------------
%diff_card_dom(+DomainCX, +DomainCY, +NU, +NGI,+NLI, +DomainIn,-DomainOut)
% For the difference X\Y of sets X and Y, DomainCX is the domain of #X,
%DomainCY is the domain of #Y, NU=#(LubX \/ LubY), NGI=#(GlbX /\ GlbY),
%NLI=#(LubX /\ LubY), DomainIn is the list of possible ranges for
%the domain of #(X\Y) so far, and DomainOut is the final one.
%--
diff_card_dom([],_, _, _,_, D,D):- !.
diff_card_dom(DCX,DCY, NU, NGI,NLI, Di,Do):-
	remove_range(DCX, RX, TX),
	dcd(DCY, RX, NU, NGI,NLI, Di,D1),
	diff_card_dom(TX,DCY, NU, NGI,NLI, D1,Do).

%--------------
%dcd(+DomainCY, +RangeX, +NU, +NGI,+NLI, +DomainIn,-DomainOut)
% For the difference X\Y of sets X and Y, DomainCY is the domain of #Y,
%RangeX is a range from the domain of #X, NU=#(LubX \/ LubY), NGI=#(GlbX /\ GlbY),
%NLI=#(LubX /\ LubY), DomainIn is the list of possible ranges for the domain of
%#(X\Y) so far, and DomainOut is the final one.
%--
dcd([],_, _, _,_, D,D):- !.
dcd(DCY,AX..BX, NU, NGI,NLI, Di,Do):-
	remove_range(DCY, AY..BY, TY),
	AZ is max(AX-BY,AX-NLI),
	BZ is min(BX-NGI,NU-AY),
	(BZ >= AZ -> D1=[AZ..BZ|Di] ; D1=Di),
	dcd(TY,AX..BX, NU, NGI,NLI, D1,Do).




%--------------
%complement(+Set, +Complement)
% Constraint: Complement is the complementary set of Set.
% (The universe is taken as the union of their LUBs.)
%--
complement(X, NotX):-
	(is_my_set(X) -> true ; X=[]),
	(is_my_set(NotX) -> true ; NotX=[]),
	assure_set(X, SetX, cardinal with domain:[GlbX:_,PossX:_]),
	lub(GlbX, PossX, LubX),
	assure_set(NotX, SetNX, cardinal with domain:[GlbNX:_,PossNX:_]),
	lub(GlbNX, PossNX, LubNX),
	set_union(LubX, LubNX, Universe),
	complement_sets(SetX, Universe, SetNX).


%--------------
%complement(?Set, +Universe, ?Complement)
% Constraint: Complement is the complementary set of Set (with respect to Universe.)
%--
complement(X, Universe, NotX):-
	\+ is_my_set(NotX), !,
	(is_my_set(X) -> true ; set(X, [],Universe, [])),
	assure_set(X, SetX, cardinal with domain:[GlbX:_,PossX:_]),
	lub(GlbX, PossX, LubX),
	set_without(Universe, LubX, GlbNotX),
	set_without(Universe, GlbX, LubNotX),
	set_without(LubNotX, GlbNotX, PossNotX),
	set(NotX, GlbNotX,PossNotX, []),
	complement_sets(SetX, Universe, NotX).
complement(X, Universe, NotX):-
	\+ is_my_set(X), !,
	assure_set(NotX, SetNX, cardinal with domain:[GlbNX:_,PossNX:_]),
	lub(GlbNX, PossNX, LubNX),
	set_without(Universe, LubNX, GlbX),
	set_without(Universe, GlbNX, LubX),
	set_without(LubX, GlbX, PossX),
	set(X, GlbX,PossX, []),
	complement_sets(X, Universe, SetNX).
complement(X, Universe, NotX):-
	assure_set(X, SetX, _),
	assure_set(NotX, SetNX, _),
	complement_sets(SetX, Universe, SetNX).

%--------------
%complement_sets(?Set, +Universe, ?Complement)
% Set and Complement are assured sets.
%--
complement_sets(X, U, N):-
	check_special_complement(X,U,N),
	cardinality(X, CX),
	cardinality(N, CN),
	length(U, CU),
	CN #= CU-CX,
	CN::DCN,
	rev_dom(DCN, CU, [],RDCN),
	CX::RDCN,
	CX::NewCX,
	rev_dom(NewCX, CU, [],RDCX),
	CN::RDCX,
	domain(X, CX, [GlbX:NInX,PossX:_]),
	remove_set(GlbX, U, U1-[]),
	domain(N, CN, [GlbN:NInN,PossN:_]),
	remove_set(GlbN, U1, U2-[]),
	set_intersection(PossX, U2, PossXU, 0,NPX),
	NMaxX is NInX+NPX,
	set_poss(X, PossXU, NMaxX),
	set_intersection(PossN, U2, PossNU, 0,NPN),
	NMaxN is NInN+NPN,
	set_poss(N, PossNU, NMaxN),
	complement_constraint(X, N, U2).


check_special_complement(X,U,N):-
	(X==N -> U=[]
	;var(X) -> suspend(check_special_complement(X,U,N), 4, X->cardinal:bound)
	;var(N) -> suspend(check_special_complement(X,U,N), 4, N->cardinal:bound)
	;true
	).

complement_constraint(X, N, U):-
	glb_poss(X, GlbXnb, PossXnb),
	glb_poss(N, GlbNnb, PossNnb),
	not_bounds(U, GlbXnb,PossXnb, GlbNnb,PossNnb, NewU,
		NewGlbX-[],NewPossX-[], NewGlbN-[],NewPossN-[]),
	length(NewGlbX, NGX), length(NewPossX, NPX),
	NMaxX is NGX+NPX,
	set_domain(X, [NewGlbX:NGX,NewPossX:NMaxX]),
	length(NewGlbN, NGN), length(NewPossN, NPN),
	NMaxN is NGN+NPN,
	set_domain(N, [NewGlbN:NGN,NewPossN:NMaxN]),
	(var(X) -> suspend(complement_constraint(X,N,NewU), 2, [X,N]->cardinal:bounded)
	;true
	).

%--------------
%remove_set(+Glb, +U, -DifU)
% Glb must be contained in U. DifU is the list difference for U\Glb.
%--
remove_set([], U, U-[]).
remove_set([H|T], U, Ui-DU):-
	remove(H, U, _, Ui-U1,TU, true),
	remove_set(T, TU, U1-DU).

%--------------
%not_bounds(+U, +Glb1,+Glb2, +Poss1,+Poss2, -NewU,
%	-NewGlb1DL,-NewPoss1DL, -NewGlb2DL,-NewPoss2DL)
%  For the 'not' constraint between Set1::Glb1+Poss1 and Set2::Glb2+Poss2,
% U is the initial universe possibly already without some elements of Glb1 and Glb2.
% Each element of U must belong to Set1 or Set2.
% NewU contains the elements of U still possible in both sets.
% The new domains (Glb+Poss) for Set1 and Set2 are given in difference lists in
% NewGlb1DL,NewPoss1DL, NewGlb2DL, and NewPoss2DL.
%--
not_bounds([], Glb1,Poss1, Glb2,Poss2, [], Glb1-[],Poss1-[], Glb2-[],Poss2-[]).
not_bounds([H|T], Glb1,Poss1, Glb2,Poss2, Uo, G1-DG1,P1-DP1, G2-DG2,P2-DP2):-
	remove(H, Glb1, _, G1-DG1a, TGlb1, ResG1),
	remove(H, Glb2, _, G2-DG2a, TGlb2, ResG2),
	remove(H, Poss1, _, P1-DP1a, TPoss1, ResP1),
	remove(H, Poss2, _, P2-DP2a, TPoss2, ResP2),
	(ResG1==true -> ResG2==fail, P1a=DP1a, P2a=DP2a, [H|G1a]=DG1a, G2a=DG2a, Uo=UT   %%% H already in Set1
	;ResG2==true -> ResG1==fail, P1a=DP1a, P2a=DP2a, G1a=DG1a, [H|G2a]=DG2a, Uo=UT   %%% H already in Set2
	;ResP1==true, ResP2==true -> [H|P1a]=DP1a, [H|P2a]=DP2a, G1a=DG1a, G2a=DG2a, Uo=[H|UT]   %%% H possible in both
	;ResP1==true -> P1a=DP1a, P2a=DP2a, [H|G1a]=DG1a, G2a=DG2a, Uo=UT   %%% only possible in Set1
	;ResP2==true -> P1a=DP1a, P2a=DP2a, G1a=DG1a, [H|G2a]=DG2a, Uo=UT   %%% only possible in Set2
	;fail  %%% impossible
	), not_bounds(T, TGlb1,TPoss1, TGlb2,TPoss2, UT, G1a-DG1,P1a-DP1, G2a-DG2,P2a-DP2).
