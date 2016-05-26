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

%----------------------------------------------------------------------
:- module(set).
%----------------------------------------------------------------------

:- reexport(s_lists).

:- export
	op(700, xfx, `<>),
	op(700, xfx, `=),
	op(700, xfx, in),
	op(700, xfx, notin),
	op(700, xfx, `::),
	op(700, xfx, `<),
	op(500, yfx, \),

	op(700, xfy, dis_s),     /* disjoint       */
	op(700, xfy, in_s),      /* membership     */
	op(700, xfy, nin_s),     /* non membership */
	op(650, xfx, def_s),     /* set def        */
	op(700, xfy, sub_s),     /* subset         */
	op(650, xfx, glb),
	op(650, xfx, lub).

:- export
	(`<>) /2,
	(`=) /2 ,
	(in) /2,
	(notin) /2,
	(`::) /2,
	(`<) /2,
	(#) /2,
	sum_weight/2,
	def_s /2,
	car_s /2,
	weight_s/2,
	dis_s /2,
	in_s /2,
	nin_s /2,
	sub_s /2,
	included/2,
	 set_range/3,
	glb/2,
	lub/2,
	union_s/3,
	all_union/2,
	inter_s/3,
	all_disjoint/1,
	diff_s/3,
	tr_set/2,
	svar_attribute/2,
	el_weight/2,
	max_weight/2,
	refine/1,
	par_refine/1,
	modify_bound/3.


:- export struct( set(
	del_glb,	% delayed goals woken if the glb changes
	del_lub,	% delayed goals woken if the lub changes
	del_any,	% the delayed goals woken if the variable is bound
			%	or the cardinal instantiated
	card,		% cardinal of the set
	setdom,		% the set domain itself
	weight		% sum of the set elements according to a given argument
    )).

:- meta_attribute(set, [unify: unify_set/2,
	test_unify: test_unify_set/2,
	suspensions: suspensions_set/3,
	delayed_goals_number: delayed_goals_number_set/2,
	print: tr_setvar/2]). 

:- export portray(property(functor) of set, tr_set/2, [protect_arg]).

tr_set(set with setdom:[Glb, Lub], T) :-
	-?->
	tr_s_lists(Glb, Glb1),
	tr_s_lists(Lub, Lub1),	
	T = (Glb1.. Lub1).

% ECLiPSe versions older than 3.5.2 pass only the attribute!!!
tr_setvar(_{In}, Out) ?-
	tr_set(In, Out).
tr_setvar(In, Out) :-
	tr_set(In, Out).


%----------------------------------------------------------------------
:- pragma(nodebug).

:- import suspensions_to_goals/3 from sepia_kernel.

S `:: Min .. Max :- 
	var(S),!,
	S def_s Min .. Max.
[] `:: _Min .. _Max :- !.
[S |List] `:: Min .. Max :- 
	S def_s Min .. Max,
	List `:: Min .. Max.


% seteval/2 modified a bit to avoid coicepoint creation [joachim]

%seteval(S, S) :-
%	set_range(S, _, _),!.		% subsumed by next clause
seteval(S, S1) :-
	var(S),!,
	S = S1.
seteval(S \/ S1, Re) :- !,
	seteval(S, R),
	seteval(S1, R1),
	union_s(R, R1,Re).
seteval(S /\ S1, Re) :- !,
	seteval(S, R),
	seteval(S1, R1),
	inter_s(R, R1, Re).
seteval(S \ S1, Re) :- !,
	seteval(S, R),
	seteval(S1, R1),
	diff_s(R, R1, Re).
seteval(S, S) :-
	set(S).


S `<> S1 :-
	seteval(S, R),
	seteval(S1, R1),
	dis_s(R, R1).

S `= S1 :-
	seteval(S, R),
	seteval(S1, R).

E in S :-
	seteval(S, R),
	in_s(E, R).

E notin S :-
	seteval(S, R),
	nin_s(E, R).

S `< S1 :-
	seteval(S, R),
	seteval(S1, R1),
	sub_s(R, R1).

#(S, C) :-
	seteval(S, R),
	car_s(R, C).

sum_weight(S, W) :-
	weight_s(S, W).


set_weight(Attr, Value) ?-
	setarg(weight of set, Attr, Value).

set_card_dom(Attr, C, S) :-
	setarg(card of set, Attr, C),
	setarg(setdom of set, Attr, S).

set_setdom(Attr, S) :-
	setarg(setdom of set, Attr, S).

svar_attribute(_{Attr}, Attr1) ?- Attr1 = Attr.

glb(_{set with setdom:[G,_L]}, Glb) ?- Glb = G.

lub(_{set with setdom:[_G,L]}, Lub) ?- Lub = L.

set_range(_{set with setdom:[G,L]}, Glb, Lub) ?- !, Glb = G, Lub = L.

set_or_set_range(_{set with setdom:[G,L]}, Glb, Lub) ?- !, Glb = G, Lub = L.
set_or_set_range(Set, Set1, Set1) :- set(Set), !, Set=Set1.

/*----------------------------------------------------- unification procedure*/

unify_set(_, AttrSet) :-
	var(AttrSet).
unify_set(SY, AttrSX) :-
	compound(AttrSX),
	unify_term_set(SY,AttrSX).

:- mode unify_term_set(?, +).

unify_term_set(S, Attr) :-
	compound(S),
	set(S),!,
	Attr= set with setdom:[Min,Max],
	s_included(Min,S),
	s_included(S,Max),
	schedule_inst(Attr,S).
unify_term_set(S,AttrSX) :-
	meta(S),
	svar_attribute(S,AttrSY),
	unify_set_set(S,AttrSX,AttrSY).

unify_set_set(_,AttrSX,AttrSY) :-
	var(AttrSY),
	AttrSY = AttrSX. /* change. share the attribute */
unify_set_set(SY,AttrSX,AttrSY) :-
	nonvar(AttrSY),
	AttrSY= set with [card:CardY,setdom:[MinY,MaxY], weight: WeiY],  
	AttrSX= set with [card:CardX,setdom:[MinX,MaxX], weight: WeiX],
	CardX=CardY,
	(meta(WeiX);meta(WeiY)
         -> WeiX = WeiY
         ;
	  WeiX = 0, WeiY = 0),
	%(var(WeiY), var(WeiX),  ->
	%    WeiX = 0, WeiY = 0;
	%WeiX = WeiY),
	s_included(MinX,MaxY),
	s_included(MinY,MaxX),
	s_union(MinX,MinY,NewMin),
	s_intersection(MaxX,MaxY,NewMax),
	(s_equality(NewMax,NewMin) ->
	    schedule_inst(AttrSX, NewMin),	% wake X's lists
	    SY=NewMin				% bind Y and wake its lists
	;	
	   s_card(NewMin,Cmin),
	   s_card(NewMax,Cmax),
	   Card :: Cmin..Cmax,
	   schedule_update(AttrSY,NewMin,NewMax),
	   schedule_update(AttrSX,NewMin,NewMax),
	   set_card_dom(AttrSY, Card, [NewMin,NewMax]),
	   s_weight(NewMin, Weimin),
	   s_weight(NewMax, Weimax),
	   set_weight_dom(Weimin, Weimax, Wdom),
	   Wei = Wdom,
	   set_weight(AttrSY, Wei),
	   merge_suspension_lists(del_glb of set, AttrSX, del_glb of set, AttrSY),
	   merge_suspension_lists(del_lub of set, AttrSX, del_lub of set, AttrSY),
	   merge_suspension_lists(del_any of set, AttrSX, del_any of set, AttrSY)
       ).

% schedule suspensions as a result of instantiating to Val
schedule_inst(Attr, Val) :-
	Attr = set with [del_lub:Lu, del_glb:Gl, setdom:[Glb,Lub]],
	schedule_suspensions(del_any of set, Attr),
	(s_equality(Val,Glb) ->
	    attach_suspensions(postponed,Gl) /* as late as possible */
	;
	    schedule_woken(Gl)
	),
	(s_equality(Val,Lub) ->
	    attach_suspensions(postponed,Lu)
	;
	    schedule_woken(Lu)
	).

% schedule suspensions as a result of updating the domain to NewMin..NewMax
schedule_update(Attr, NewMin, NewMax) :-
	Attr = set with [setdom:[Glb,Lub]],
	( s_equality(NewMin,Glb) ->
	    ( s_equality(NewMax,Lub) ->
	    	true
	    ;
		schedule_suspensions(del_any of set, Attr),
		schedule_suspensions(del_lub of set, Attr)
	    )
	;
	    schedule_suspensions(del_any of set, Attr),
	    schedule_suspensions(del_glb of set, Attr),
	    ( s_equality(NewMax,Lub) ->
		true
	    ;
		schedule_suspensions(del_lub of set, Attr)
	    )
	).


/*-------------------------------------------------------- test_unify -*/

test_unify_set(_, AttrSet) :-
	var(AttrSet).
test_unify_set(SY, AttrSX) :-
	compound(AttrSX),
	test_unify_term_set(SY, AttrSX).

:- mode test_unify_term_set(?, +).
test_unify_term_set(S, Attr) :-
	nonvar(S),
	set(S),
	Attr= set with setdom:[Min,Max],
	s_included(Min, S),
	s_included(S, Max).
test_unify_term_set(SY{AttrSY}, AttrSX) :-
	-?->
	test_unify_set_set(SY, AttrSX, AttrSY).

test_unify_set_set(_, _, AttrSY) :-
	var(AttrSY).
test_unify_set_set(_SY, AttrSX, AttrSY) :-
	nonvar(AttrSY),
	AttrSY = set with [card:CardY,setdom:[MinY,MaxY]],  
	AttrSX = set with [card:CardX,setdom:[MinX,MaxX]],
	dvar_domain(CardY, DomCardY),
	dvar_domain(CardX, DomCardX),
	dom_intersection(DomCardY, DomCardX, _, _),	% may fail
	s_included(MinX, MaxY),				% may fail
	s_included(MinY, MaxX).				% may fail


/*--------------------------------------------------------           -*/

suspensions_set(_{Attr}, Susps, Susps0) ?-
	( var(Attr) ->
	    Susps=Susps0
	;
	    Attr = set with [del_glb:DL, del_lub:DU, del_any:DA],
	    Susps = [DL,DU,DA|Susps0]
	).


delayed_goals_number_set(_{set with [del_any:Bound,del_glb:Glb]}, N) ?- !,	%%%%
	count_active_suspensions(Bound, 0, N1),
	count_active_suspensions(Glb, N1, N).
delayed_goals_number_set(_, 0).
	
    count_active_suspensions([Susp|Susps], N0, N) ?- !,
	( is_suspension(Susp) -> N1 is N0 + 1 ; N1 = N0 ),
	count_active_suspensions(Susps, N1, N).
    count_active_suspensions(_, N, N).


/*--------------------------------------------------Actions on domain updates-*/

modify_bound(glb, Set, Newmin) :-
	set(Set),!, Set = Newmin.
modify_bound(glb, Set, Newmin) :-
	glb(Set, Min), s_included(Min, Newmin),
	(Min = Newmin -> true;
	set_changed_glb(Set, Newmin)).
modify_bound(lub, Set, Newmax) :-
	set(Set),!, Set = Newmax.
modify_bound(lub, Set, Newmax) :-
	lub(Set, Max), s_included(Newmax, Max),
	(Max = Newmax -> true;
	set_changed_lub(Set, Newmax)).

modified_bounds(_,Set,Oldmin,Newmin) :-
	set(Set), !,
	s_equality(Newmin,Oldmin),
	s_equality(Set,Oldmin).
modified_bounds(min,_Set,Oldmin,Newmin) :-
	s_equality(Newmin,Oldmin),!.
modified_bounds(min,Set,_Oldmin,Newmin) :-
	set_changed_glb(Set, Newmin).

modified_bounds(max,_Set,Oldmax,Newmax) :-
	s_equality(Oldmax,Newmax),!.
modified_bounds(max,Set,_Oldmax,Newmax) :-
	set_changed_lub(Set, Newmax).

set_changed_lub(Var{Attr}, Value) ?-
	Attr= set with [setdom:[Glb,_Lub]],
	(s_equality(Value,Glb)
              ->
	      Var=Value
	      ;
	      set_setdom(Attr, [Glb,Value]),
	      schedule_suspensions(del_lub of set, Attr),
	      schedule_suspensions(del_any of set, Attr),
	      notify_constrained(Var)
	  ).

set_changed_glb(Var{Attr}, Value) ?-
	Attr= set with [setdom:[_Glb,Lub]],
	(s_equality(Lub,Value)
              ->
	       Var=Value
	      ;
	      set_setdom(Attr, [Value,Lub]),
	      schedule_suspensions(del_glb of set, Attr),
	      schedule_suspensions(del_any of set, Attr),
	      notify_constrained(Var)
	      ).
	      
/*------------------------------------------------------------set variable --*/
/* definition of a set variable within a domain <Glb,Lub> */

init_set_fields(Set,Glb,Lub) :-
	(s_equality(Lub,Glb)
         ->
	 s_card(Lub,Card)
         ;
	 s_card(Lub,L1),
	 s_card(Glb,L2),
	 Card ::L2..L1
        ),
	add_attribute(Set, set with [card:Card, setdom:[Glb,Lub],
	    	del_glb:[], del_lub:[], del_any:[]]).

Set def_s Glb..Glb :-
	Set = Glb,!.
Set def_s SetDom :-
	make_setdom(Set,SetDom).
make_setdom(Set,Glb..Lub) :-
	s_included(Glb,Lub),
	init_set_fields(Set,Glb,Lub).

included(S, S1) :-
	s_included(S,S1).

/*-------------------------------------------------------cardinality operator-*/

car_s(Set,Card) :-
	set(Set),!,
	s_card(Set,Card).
car_s(Set,Card) :-
	integer(Card),
	svar_attribute(Set,SAttr),
	SAttr= set with [setdom:[Glb,_Lub], card : Card1],
	s_card(Glb,Card),
	Card1 = Card,
	!,
	Set=Glb.
car_s(Set,Card) :-
	integer(Card),
	svar_attribute(Set,SAttr),
	SAttr= set with [setdom:[_Glb,Lub], card: Card1],
	s_card(Lub,Card),
	Card1= Card,
	!,
	Set=Lub.
car_s(Set,Card) :-
	integer(Card),
	Card =1,
	svar_attribute(Set,SAttr),
	SAttr= set with [setdom:[_Glb,Lub], card: Card1,weight: W],
	integer(W),!,
	get_elements(Lub, L,W),
	list2set(L,SNewmax),
	Card1 = Card,
	(s_card(SNewmax,1) ->
	    Set = SNewmax;
	    modified_bounds(max,Set,Lub,SNewmax),
	    suspend(car_s(Set,Card), 2, Set->del_any)
	).
car_s(Set,Card) :-
	svar_attribute(Set,SAttr),
	SAttr= set with [setdom:[Glb,Lub], card: Card1], 
	s_card(Lub,Length2),             
	s_card(Glb,Length1),
	Card :: Length1..Length2,
	Card1 = Card,
	attr_card_instantiate(Card,Set,SAttr,Length1,Length2).

attr_card_instantiate(Card,Set,SAttr,_Length1,Length2) :-
	SAttr =set with [setdom:[_Glb,Lub]],
	is_domain(Card),
	dvar_range(Card,Min,_Max),
	Min=Length2,!,
	Set=Lub.
attr_card_instantiate(Card,Set,SAttr,Length1,_Length2) :-
	SAttr =set with [setdom:[Glb,_Lub]],
	is_domain(Card),
	dvar_range(Card,_Min,Max),
	Max=Length1,!,
	Set=Glb.
attr_card_instantiate(Card,Set,_SAttr,_Length1,_Length2) :-
	is_domain(Card), !,
	suspend(car_s(Set,Card), 2, [Set->del_any, Card->any]).
attr_card_instantiate(Card,Set,SAttr,Length1,Length2) :-
	SAttr =set with [setdom:[Glb,Lub]],
	integer(Card),
	(set(Set),s_card(Set,Card)
         ->true;
	(Card=Length2
              -> 
	      Set=Lub ;
	      (Card=Length1
	       ->
	         Set=Glb;
		 suspend(car_s(Set,Card), 2, Set->del_any)
	     )
	 )).


/*-----------------------------------------------------------weight operator --*/ 
max_weight(S, E):-
	set(S),!,
	set2list(S, List),
	find_longest(List, E, e(_, 0)).
max_weight(S, E) :-
	set_range(S, Min, Max),
	s_delta(Max, Min, Diff),
	set2list(Diff, List),
	find_longest(List, E, e(_, 0)).

find_longest([], X, X).
find_longest([e(N, L) | List], X, e(Name, Length)) :-
	( L < Length
         ->
	  find_longest(List, X, e(Name, Length))
         ;
	  find_longest(List, X, e(N, L))
        ).

el_weight(E,W) :-
	arg(2, E, W).

% get all elements with the given weight
get_elements(Lub, L, W) :-
	set2list(Lub, LLub),
	getelements(LLub, L, W),
	L \= [].

getelements([],[],_W).
getelements([E | LLub], [E |L], W) :-
	arg(2, E, W),!,
	getelements(LLub, L, W).
getelements([_F | LLub], L, W) :-
	getelements(LLub, L, W).

set_weight_dom(Weimin, Weimax, Wdom) :-
	(Weimin= 0, Weimax = 0 -> 
	    Wdom =0;
	    Wdom :: Weimin .. Weimax).

set_weight_discrdom(NewMin, NewMax, Weimin, Wdom) :-
	s_delta(NewMax, NewMin, Sdiff),
	set2list(Sdiff, Ldiff),
	setweightdom(Weimin, Ldiff, Wdom1),
	Wdom :: [Weimin | Wdom1],
	Wdom ## 0.

setweightdom(_Weimin, [], []).
setweightdom(Weimin, [E | Ldiff], Wdom) :-
	setweightdom(Weimin, Ldiff, Wdom1),
	arg(2, E, We),
	Wee is We + Weimin,
	Wdom = [ Wee | Wdom1].
	
weight_s(S, W) :-
	set(S), !,
	s_weight(S, W).
%changes 2 weight_s predicates removed
weight_s(S, W) :-
	svar_attribute(S, SAttr),
	SAttr= set with [setdom: [Glb, Lub], card: C, weight : W],
	s_weight(Glb, W1),
	s_weight(Lub, W2),
	(C==1
         -> 
	 s_weight_discr(Lub, W22),
	 Wtemp :: W22
	%set_weight_discrdom(Glb, Lub, W1,Wtemp)
         ;
	 set_weight_dom(W1,W2, Wtemp)
        ),
	W = Wtemp, 
	attr_weight_instanciate(W, S, SAttr, W1, W2),
	( nonvar(S) ->
	           true;
		   s_delta(Lub, Glb, S_diff),
		   set_dom_change(S, S_diff, W1, W)
	).
	
% carmen changes : procedure made generic 
exist_nil_weights_indiff(Lub, Glb) :-
	s_delta(Lub, Glb, Sdiff), 
	set2list(Sdiff,LSdiff),
	exist_nil_weights(LSdiff).

exist_nil_weights([]) :-!.
exist_nil_weights([ E | Lub]) :-
	(arg(2,E,0) -> true;
	    exist_nil_weights(Lub)
	).

set_dom_change(S, Lub, W1, W) :-
	set2list(Lub, Llub),
	(is_domain(W)
         -> dvar_range(W, _, Wlocal); Wlocal = W),
	 setdomremove(S, Llub, W1, Wlocal).

setdomchange1(_S, [], _W1, _W).
setdomchange1(S, [E | Llub], W1, W) :-
	arg(2, E, We),
	((We + W1 > W;We + W1 < W)
          ->
	  E nin_s S;
	  true),
	  setdomchange1(S, Llub, W1, W).

setdomremove(_S, [], _W1, _W).
setdomremove(S, [E | Llub], W1, W) :-
	arg(2, E, We),
	(We + W1 > W
          ->
	  E nin_s S;
	  true),
	  setdomremove(S, Llub, W1, W).

%changes: two additional conditions added to test for elements with weight 0 in the last clause
attr_weight_instanciate(W, S, SAttr, _W1, W2) :-
	SAttr = set with [setdom : [_Glb, Lub]],
	is_domain(W),
	dvar_range(W, Min, _Max),
	Min = W2, !, 
	S = Lub.
attr_weight_instanciate(W, S, SAttr, W1, _W2) :-
	SAttr = set with [setdom : [Glb, _Lub]],
	is_domain(W),
	dvar_range(W, _Min, Max),
	Max = W1, !, 
	S = Glb.
attr_weight_instanciate(W, S, SAttr, _W1, _W2) :-
	is_domain(W),
	SAttr = set with [card : C],
	!,
	suspend(weight_s(S, W), 2, [S->del_any, W->any, C -> inst]).
attr_weight_instanciate(W, S, SAttr, W1, W2) :-
	SAttr = set with [setdom : [Glb, Lub]],
	integer(W),
	(W = W2
             ->
             (not(exist_nil_weights_indiff(Lub, Glb)) ->
	     S = Lub;
             add_all_not_nil(Lub, Glb, Subset),
	     Subset sub_s S
             )
             ;
	     ((W = W1,not(exist_nil_weights_indiff(Lub, Glb)))
	      -> 
	        S = Glb;
		suspend(weight_s(S, W), 2, [S->del_any, W->any])
	    )
	).

add_all_not_nil(Lub, Glb, Subset) :-
	s_delta(Lub, Glb, Sdiff),
	set2list(Sdiff, LSdiff),
	addallnotnil(LSdiff, LSubset),
	list2set(LSubset, Subset).

addallnotnil([], []) :- !.
addallnotnil([ E | Lub], NewSubset) :-
	arg(2,E,0), !,
	addallnotnil(Lub, NewSubset).
addallnotnil([ E | Lub], [E | NewSubset]) :-
	addallnotnil(Lub, NewSubset).

/*---------------------------------------------------------------CONSTRAINTS--*/

S sub_s S1 :-
	set(S),
	set(S1),
	!,
	s_included(S,S1).
S sub_s S1 :-
	set(S),
	set_range(S1,S1Min,S1Max),
	!,
	s_included(S,S1Max),
	s_union(S,S1Min,S1Newmin),
	modified_bounds(min,S1,S1Min,S1Newmin).

S sub_s S1 :-
	set(S1),
	set_range(S,SMin,SMax),
	!,
	s_included(SMin,S1),
	s_intersection(SMax,S1,SNewmax),
	modified_bounds(max,S,SMax,SNewmax).

S sub_s S1 :-
	set_range(S1,S1Min,S1Max),
	set_range(S,SMin,SMax),
	s_included(SMin,S1Max),
	s_union(SMin,S1Min,S1Newmin),
	s_intersection(SMax,S1Max,SNewmax),
	modified_bounds(min,S1,S1Min,S1Newmin),
	modified_bounds(max,S,SMax,SNewmax),
	( var(S),var(S1) -> suspend(S sub_s S1, 2, [S,S1]->del_any) ; true ),
	wake.


/*----------------------------------------------------------------membership--*/

X in_s S :-
	var(X), !,
	S \== {},	% nothing can be in the empty set
	suspend(X in_s S, 2, [X,S]->inst).
X in_s S :-                          /*X is known */
	set_range(S,SMin,SMax),!,
	s_memberchk(X,SMax),
	s_insertion(X,SMin,Newmin),
	modified_bounds(min,S,SMin,Newmin),
	wake.
X in_s S :-
	set(S),!,
	s_memberchk(X,S).


X nin_s S :-
	var(X), !,
	suspend(X nin_s S, 2, X->inst).
X nin_s S :-
	set_range(S,SMin,SMax),!,
	not s_memberchk(X,SMin),
	s_remove(X,SMax,Newmax),
	modified_bounds(max,S,SMax,Newmax),
	wake.
X nin_s S :-
	set(S),
	\+ s_memberchk(X,S).
/*----------------------------------------------------disjoint-*/

SetX dis_s SetY :-
	set(SetX),
	set(SetY),
	!,
	s_dis(SetX,SetY).
	%s_intersection(SetX,SetY,{}).
SetX dis_s SetY :-
	set(SetX),
	!,
	set_range(SetY,SYMin,SYMax),
	s_dis(SetX,SYMin),
	%s_intersection(SetX,SYMin,{}),
	s_delta(SYMax,SetX,NewSYmax),
	modified_bounds(max,SetY,SYMax,NewSYmax),
	wake.
SetY dis_s SetX :-
	set(SetX),
	!,
	set_range(SetY,SYMin,SYMax),
	s_dis(SetX,SYMin),
	%s_intersection(SetX,SYMin,{}),
	s_delta(SYMax,SetX,NewSYmax),
	modified_bounds(max,SetY,SYMax,NewSYmax),
	wake.
SetX dis_s SetY :-
	set_range(SetX,_SXMin,SXMax),
	set_range(SetY,_SYMin,SYMax),
	s_dis(SXMax,SYMax),!.
SetX dis_s SetY :-
	set_range(SetX,SXMin,SXMax),
	set_range(SetY,SYMin,SYMax),
	s_dis(SXMin,SYMin),
	%s_intersection(SXMin,SYMin,{}),
	s_delta(SXMax,SYMin,NewSXmax),
	s_delta(SYMax,SXMin,NewSYmax),
	modified_bounds(max,SetX,SXMax,NewSXmax),
	modified_bounds(max,SetY,SYMax,NewSYmax),
	( var(SetX),var(SetY) ->
	    suspend(SetX dis_s SetY, 2, [SetX, SetY]->del_glb)
	; true ),
	wake.


/* -------------------------------- --------------------All_disjoint-*/

all_disjoint([]).
all_disjoint([Set | Sets]) :-
	map_disjoint(Set, Sets),
	all_disjoint(Sets).

map_disjoint(_, []).
map_disjoint(Set1, [Set2 | Sets]) :-
	Set1 dis_s Set2,
	map_disjoint(Set1, Sets).

/*----------------------------------------------------------------OPERATORS--*/

/*-----------------------------------------------------------union (S1,S2,S3)-*/

/* X U Y = S           */
/* min(S) \ max(X) < Y */
/* max(X) < max(S)     */
set_union_domain(SMin,SMax,SXMax,SXMin,SYMax,SYMin,SXnewMin,SXnewMax,SYnewMin,
	SYnewMax) :-
	s_delta(SMin,SYMax,RemindingX),
	s_included(RemindingX, SXMax),
	s_delta(SMin,SXMax,RemindingY),
	s_included(RemindingY, SYMax),
	s_union(SXMin,RemindingX,SXnewMin),
	s_union(SYMin,RemindingY,SYnewMin),
	s_intersection(SMax, SXMax, SXnewMax),
	s_intersection(SMax, SYMax, SYnewMax).

union_set_bounds(XNewmin,XNewmax,YNewmin,YNewmax,SMin,SMax,
	Snewmin,Snewmax) :-
	s_union(XNewmin,SMin,TampSmin),      % the resulting set bounds 
	s_union(YNewmin,TampSmin,Snewmin),   % are modified (S3): add elts
	s_delta(SMax,XNewmax,Saddelts),   % from S1 and S2 min.
	s_delta(Saddelts,YNewmax,Saddelts1),
	s_delta(SMax,Saddelts1,Snewmax).

ground_union(SetX, SetY, S) :-
	set(SetX), set(SetY), !,
	s_union(SetX, SetY, S).
ground_union(SetX,SetY,S) :-
	set(SetX),
	!,
	s_delta(S,SetX,YNewMintamp),
	set_range(SetY,SYMin,SYMax),
	s_included(YNewMintamp,SYMax),
	s_union(SYMin,YNewMintamp,YNewMin),
	s_intersection(SYMax, S, SYnewMax),
	modified_bounds(min,SetY,SYMin,YNewMin),
	modified_bounds(max,SetY, SYMax, SYnewMax),
	wake.
ground_union(SetY,SetX,S) :-
	set(SetX),
	!,
	s_delta(S,SetX,YNewMintamp),
	set_range(SetY,SYMin,SYMax),
	s_included(YNewMintamp,SYMax),
	s_union(SYMin,YNewMintamp,YNewMin),
	s_intersection(SYMax, S, SYnewMax),
	modified_bounds(min,SetY,SYMin,YNewMin),
	modified_bounds(max,SetY, SYMax, SYnewMax),
	wake.
ground_union(SetX,SetY,Slist) :-  
	set_range(SetX,SXMin,SXMax),
	set_range(SetY,SYMin,SYMax),
	set_union_domain(Slist,Slist,SXMax,SXMin,SYMax,SYMin,
	XNewmin,XNewmax,YNewmin,YNewmax),
	modified_bounds(min,SetX,SXMin,XNewmin),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(max,SetY,SYMax,YNewmax),
	( var(SetX),var(SetY) ->
	    suspend(ground_union(SetX,SetY,Slist), 2, [SetX,SetY]->del_any)
	; true ),
	wake.

union_s(SetX,SetY,S) :-
	set(SetX),        % At least the first two sets are known
	set(SetY),
	!,
	s_union(SetX, SetY, S).
union_s(SetX,SetY,S) :-  
	set(S),              %S the resulting set is known
	!,
	ground_union(SetX,SetY,S).
union_s(SetX,SetY,S) :-
	set(SetX),
	set_range(S,SMin,SMax),
	set_range(SetY,SYMin,SYMax),!,
	s_delta(SMin,SetX,RemindingY),
	s_included(RemindingY, SYMax),
	s_union(SYMin,RemindingY,YNewmin),
	s_intersection(SMax, SYMax, YNewmax),
	s_union(SetX,YNewmin,SMintmp),
	s_union(SMintmp,SMin,Snewmin),   
	s_delta(SMax,SetX,Saddelts),   
	s_delta(Saddelts,YNewmax,Saddelts1),
	s_delta(SMax,Saddelts1,Snewmax),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetY,SYMax,YNewmax),
	modified_bounds(min,S,SMin,Snewmin),
	modified_bounds(max,S,SMax,Snewmax),
	wake,
	( nonground([SetY,S]) ->
	    suspend(union_s(SetX,SetY,S), 2, [SetY,S]->del_any) ; true ).
union_s(SetY,SetX,S) :-
	set(SetX),
	set_range(S,SMin,SMax),
	set_range(SetY,SYMin,SYMax),!,
	s_delta(SMin,SetX,RemindingY),
	s_included(RemindingY, SYMax),
	s_union(SYMin,RemindingY,YNewmin),
	s_intersection(SMax, SYMax, YNewmax),
	s_union(SetX,YNewmin,SMintmp),
	s_union(SMintmp,SMin,Snewmin),   
	s_delta(SMax,SetX,Saddelts),   
	s_delta(Saddelts,YNewmax,Saddelts1),
	s_delta(SMax,Saddelts1,Snewmax),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetY,SYMax,YNewmax),
	modified_bounds(min,S,SMin,Snewmin),
	modified_bounds(max,S,SMax,Snewmax),
	wake,
	( nonground([SetY,S]) ->
	    suspend(union_s(SetX,SetY,S), 2, [SetY,S]->del_any) ; true ).
union_s(SetX,SetY,S) :-    %S the resulting set is a set domain variable
	set_range(S,SMin,SMax),
	set_range(SetX,SXMin,SXMax),
	set_range(SetY,SYMin,SYMax),
	!,
	set_union_domain(SMin,SMax,SXMax,SXMin,SYMax,SYMin,
	XNewmin,XNewmax,YNewmin,YNewmax),
	union_set_bounds(XNewmin,XNewmax,YNewmin,YNewmax,SMin,SMax,
	Snewmin,Snewmax),
	modified_bounds(min,SetX,SXMin,XNewmin),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(max,SetY,SYMax,YNewmax),
	modified_bounds(min,S,SMin,Snewmin),
	modified_bounds(max,S,SMax,Snewmax),
	wake,
	( nonground([SetX,SetY,S]) ->
	    suspend(union_s(SetX,SetY,S), 2, [SetX,SetY,S]->del_any) ; true ).
union_s(SetX,SetY,S) :-   
	var(S),                 /* S is a free variable */
	set_range(SetX,SXMin,SXMax),
	set_range(SetY,SYMin,SYMax),!,
	s_union(SXMin,SYMin,SMin),
	s_union(SXMax,SYMax,SMax),
	S def_s SMin..SMax,
	suspend(union_s(SetX,SetY,S), 2, [SetX,SetY,S]->del_any).
union_s(SetX,SetY,S) :-   
	var(S),                 /* S is a free variable */
	set(SetX),
	set_range(SetY,SYMin,SYMax),!,
	s_union(SetX,SYMin,SMin),
	s_union(SetX,SYMax,SMax),
	S def_s SMin..SMax,
	suspend(union_s(SetX,SetY,S), 2, [SetY,S]->del_any).
union_s(SetY,SetX,S) :-   
	var(S),                 /* S is a free variable */
	set(SetX),
	set_range(SetY,SYMin,SYMax),
	s_union(SetX,SYMin,SMin),
	s_union(SetX,SYMax,SMax),
	S def_s SMin..SMax,
	suspend(union_s(SetX,SetY,S), 2, [SetY,S]->del_any).

/*--------------------------------------union of a conjunction (list) of sets-*/

all_union([Union1], Union2) :-
	!,
	Union1= Union2.
all_union([Set1, Set2 | Sets], Union) :-
	union_s(Set1, Set2, Set1Set2),
	all_union([Set1Set2 | Sets], Union).

/*-----------------------------------------------------intersection (S1,S2,S)-*/


inter_add_remove_elementsfromknown(SMax,[SXMin,SXMax],[SYMin,SYMax],XNewmin,
	XNewmax,YNewmin,YNewmax) :-
	inter_add_elementsfromMin(SMax,[SXMin,_SXMax],[SYMin,_SYMax],XNewmin,
	YNewmin),
	inter_remove_elementsfromMax(SMax,[SXMin,SXMax],[SYMin,SYMax],
	XNewmax,YNewmax).

inter_add_elementsfromMin(SMin,[SXMin,_SXMax],[SYMin,_SYMax],XNewmin,
	YNewmin) :-
	s_union(SMin,SXMin,XNewmin),               
	s_union(SMin,SYMin,YNewmin).

inter_remove_elementsfromMax(SNewmax,[SXMin,SXMax],[SYMin,SYMax],
	XNewmax,YNewmax) :-
	s_intersection(SXMax,SYMax,TampInter),
	s_delta(TampInter,SNewmax,RemindingInter),
	s_intersection(SXMin,RemindingInter,ElementstostayinX),
	s_intersection(SYMin,RemindingInter,ElementstostayinY),
	s_intersection(ElementstostayinX,ElementstostayinY,Inter),
	s_delta(ElementstostayinX,Inter,NotinSY), /*remove elements X from maxS2 */
	s_delta(SYMax,NotinSY,YNewmax),           /* if X in minS1, X in maxS2   */
	s_delta(ElementstostayinY,Inter,NotinSX), /* X notin S.   */
	s_delta(SXMax,NotinSX,XNewmax).

/*  the domains of the two first sets is modified */
sets_inter_newdomains(SMin,SMax,SXMax,SXMin,SYMax,SYMin,
	XNewmin,XNewmax,YNewmin,YNewmax) :-
	s_included(SMin,SXMax),
	s_included(SMin,SYMax),
	s_intersection(SXMin,SYMin,Commonmin),
	s_included(Commonmin,SMax),
	!,
	s_intersection(SXMax,SYMax,Newinter),
	s_intersection(Newinter,SMax,SNewmax),
	(s_equality(SMin,SMax) ->
	inter_add_remove_elementsfromknown(SMax,[SXMin,SXMax],[SYMin,SYMax],
	XNewmin,XNewmax,YNewmin,YNewmax);
	inter_add_elementsfromMin(SMin,[SXMin,SXMax],[SYMin,SYMax],XNewmin,
	YNewmin),
	inter_remove_elementsfromMax(SNewmax,[SXMin,SXMax],[SYMin,SYMax],
	XNewmax,YNewmax)).



% the resulting set domain is modified
result_intersectionset_bounds(XNewmin,XNewmax,YNewmin,YNewmax,
	SMin,SMax,Snewmin,Snewmax) :-
	s_intersection(XNewmin,YNewmin,TampSmin),  %Snewmin is the min union
	s_union(SMin,TampSmin,Snewmin),              %Snewmax is the max union  
	s_intersection(XNewmax,YNewmax,TampSmax),
	s_intersection(TampSmax,SMax,Snewmax).

inter_s(SetX,SetY,S) :-   %first two terms are at least known
	set(SetX),
	set(SetY),
	!,
	s_intersection(SetX, SetY, S).
inter_s(SetX,SetY,S):-
	set(S),
	set_range(SetX, SXMin,SXMax), 
	set_range(SetY,SYMin,SYMax),
	!,
	sets_inter_newdomains(S,S,SXMax,SXMin,SYMax,SYMin,
	Xnewmin,XNewmax,Ynewmin,YNewmax),
	modified_bounds(min,SetX,SXMin,Xnewmin),
	modified_bounds(min,SetY,SYMin,Ynewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(max,SetY,SYMax,YNewmax),
	wake,
	( var(SetX), var(SetY) ->
	    suspend(inter_s(SetX,SetY,S), 2, [SetX,SetY]->del_any)
	; true ).
inter_s(SetX,SetY,S):-
	set(S),
	set(SetX),
	set_range(SetY,SYMin,SYMax),
	!,
	s_intersection(SetX,SYMax,S1),
	s_included(S,S1),
	s_union(SYMin,S,Ynewmin),
	s_delta(SetX,S,STmp),
	s_delta(SYMax,STmp,YNewmax),
	modified_bounds(min,SetY,SYMin,Ynewmin),
	modified_bounds(max,SetY,SYMax,YNewmax),
	wake.
inter_s(SetX,SetY,S):-
	set(S),
	set(SetY),
	set_range(SetX,_SXMin,_SXMax),
	!,
	inter_s(SetY,SetX,S).
inter_s(SetX,SetY,S) :-          % the resulting set is a domain variable
	set_or_set_range(S,SMin,SMax),
	set_or_set_range(SetX,SXMin,SXMax),
	set_or_set_range(SetY,SYMin,SYMax),
	!,
	sets_inter_newdomains(SMin,SMax,SXMax,SXMin,SYMax,SYMin,
	XNewmin,XNewmax,YNewmin,YNewmax),
	result_intersectionset_bounds(XNewmin,XNewmax,YNewmin,YNewmax,
	SMin,SMax,Snewmin,Snewmax),
	modified_bounds(min,SetX,SXMin,XNewmin),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(max,SetY,SYMax,YNewmax),
	modified_bounds(min,S,SMin,Snewmin),
	modified_bounds(max,S,SMax,Snewmax),
	wake,
	( nonground(2, [SetX,SetY,S], _TwoVars) ->
	    suspend(inter_s(SetX,SetY,S), 2, [SetX,SetY,S]->del_any)
	; true ).
inter_s(SetX,SetY,S) :-   /* S is not specified as a domain variable */
	var(S),
	set_range(SetX,SXMin,SXMax),
	set_range(SetY,SYMin,SYMax),
	!,
	s_intersection(SXMin,SYMin,SMin),
	s_intersection(SXMax,SYMax,SMax),
	(s_equality(SMin,SMax)
         -> S=SMin
         ;
	    S def_s SMin..SMax,
	    ( nonground(2, [SetX,SetY,S], _TwoVars) ->
		suspend(inter_s(SetX,SetY,S), 2, [SetX,SetY,S]->del_any)
	    ; true )
        ).
inter_s(SetX,SetY,S) :-   /* S is not specified as a domain variable  */
	var(S),
	set(SetX),
	set_range(SetY,SYMin,SYMax),
	!,
	(s_included(SetX,SYMin)
         -> 
	 S=SetX
         ;
	 (s_dis(SYMax,SetX) %s_intersection(SYMax,SetX,{})
	     -> S={}
         ;
	     s_intersection(SetX,SYMin,SMin),
	     s_intersection(SetX, SYMax, SMax),
	     S def_s SMin..SMax,
	     ( nonground([SetY,S]) ->
		  suspend(inter_s(SetX,SetY,S), 2, [SetY,S]->del_any)
	     ; true )
	 )
	).
inter_s(SetY,SetX,S) :-
	var(S),
	set(SetX),
	set_range(SetY,_SYMin,_SYMax),
	inter_s(SetX,SetY,S).
/*---------------------------------------------------------------------diff--*/

diff_s(SetX,SetY,S) :-
	set(SetX),
	set(SetY),   /* the first two sets are known */
	!,
	s_delta(SetX, SetY,S).

diff_s(SetX,SetY,S) :-
	set(S),              /* the resulting set is known */
	!,
	set_or_set_range(SetX, SXMin,SXMax), 
	set_or_set_range(SetY,SYMin,SYMax),
	s_dis(SYMin,S),
	%s_intersection(SYMin,S,{}),
	s_included(S,SXMax),
	!,
	s_union(S,SXMin,Xnewmin1),
	s_delta(SYMin,S,Xnewmin2),
	s_union(Xnewmin1,Xnewmin2,Xnewmin),
	s_union(SYMax,S,Xnewmax1),
	s_intersection(SXMax,Xnewmax1,XNewmax),
	s_delta(SYMax,S,YNewmax),
	s_delta(Xnewmin,S,Ynewmin1),
	s_union(SYMin, Ynewmin1, Ynewmin),
	modified_bounds(min,SetX,SXMin,Xnewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(min,SetY,SYMin,Ynewmin),
	modified_bounds(max,SetY,SYMax,YNewmax),
	wake,
	( var(SetX), var(SetY) ->
	    suspend(diff_s(SetX,SetY,S), 2, [SetX,SetY]->del_any)
	; true ).
diff_s(SetX,SetY,S) :-
	set_range(S,SMin,SMax),
	!,
	set_or_set_range(SetX, SXMin,SXMax), 
	set_or_set_range(SetY,SYMin,SYMax),
	s_dis(SYMin, SMin),
	%s_intersection(SYMin,SMin,{}),
	s_included(SMin,SXMax),
	s_union(SMin,SXMin,Xnewmin1),
	s_delta(SYMin, SMax, Xnewmin2),
	s_union(Xnewmin1, Xnewmin2, XNewmin), /*+ pour X */
	s_union(SMax,SYMin,TampmaxX),
	s_intersection(SXMax, TampmaxX, XNewmax), /* remove elements from Max X */
        s_delta(XNewmin, SMax, TampminY),
	s_union(SYMin, TampminY, YNewmin), /* add elements to Min Y */
	s_delta(SYMax,SMin,YNewmax),    /* remove elements from Max Y */
	s_delta(SXMin,SYMax,Tampmin),
	s_union(Tampmin,SMin, SNewmin),  /*  add elements to Min S */
        s_delta(SXMax, SYMin, TampSmax),
	s_intersection(SMax,TampSmax, SNewmax),    /*  remove elements from Max S */
	modified_bounds(min,SetX,SXMin,XNewmin),
	modified_bounds(max,SetX,SXMax,XNewmax),
	modified_bounds(min,SetY,SYMin,YNewmin),
	modified_bounds(max,SetY,SYMax,YNewmax),
	modified_bounds(min,S,SMin,SNewmin),
	modified_bounds(max,S,SMax,SNewmax),
	wake,
	( nonground(2, [SetX,SetY,S], _TwoVars) ->
	    suspend(diff_s(SetX,SetY,S), 2, [SetX,SetY,S]->del_any)
	; true ).
diff_s(SetX,SetY,S) :-
	var(S),
	set_range(SetX,XMin,XMax),
	set_range(SetY,YMin,YMax),!,
	s_delta(XMax,YMin,SMax),
	s_delta(XMin,YMax,SMin),
	S def_s SMin..SMax,
	suspend(diff_s(SetX,SetY,S), 2, [S,SetX,SetY]->del_any).
diff_s(SetX,SetY,S) :-
	var(S),
	set(SetX),
	set_range(SetY,YMin,YMax),!,
	s_delta(SetX,YMax,SMin),
	s_delta(SetX,YMin,SMax),
	(s_equality(SMin,SMax)
         -> S=SMin
         ;
	 S def_s SMin..SMax,
	 suspend(diff_s(SetX,SetY,S), 2, [S,SetY]->del_any)
	).
diff_s(SetY,SetX,S) :-
	var(S),
	set(SetX),
	set_range(SetY,YMin,YMax),
	s_delta(YMin,SetX,SMin),
	s_delta(YMax,SetX,SMax),
	(s_equality(SMin,SMax)
         -> S=SMin
         ;
	 S def_s SMin..SMax,
	 suspend(S sub_s SetY, 2, [S,SetY]->del_any)
	).
/*---------------------------------------------------------phase de labeling -*/

refine(S) :-
	nonvar(S), set(S).
refine(S) :-
	var(S), set_range(S,Min,Max),
	s_delta(Max,Min, Diff),
	s_memberchk(X, Diff),
	( X in_s S ; X nin_s S ),
	refine(S).


par_refine(S) :-
	nonvar(S), set(S).
par_refine(S) :-
	var(S), set_range(S,Min,Max),
	s_delta(Max,Min, Diff),
	s_memberchk(X, Diff),
	in_or_notin(X, S),
	par_refine(S).

:- parallel in_or_notin/2.
in_or_notin(X, S) :- X in_s S.
in_or_notin(X, S) :- X nin_s S.
