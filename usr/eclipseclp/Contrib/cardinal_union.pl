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
% Portions created by the Initial Developer are  Copyright (C) 2000-2004.
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
% cardinal_union.pl      By Francisco Azevedo    2000-2004
%
% Set union function of Cardinal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%--------
% union_function(+Union, +SetVar)
%  According to Union function, update SetVar's union and domain attributes.
%--
union_function(Union, SetVar):-
	domain(SetVar, [In:_,Poss:NMax]),
	(var(Union) ->
		all_sets_union(In, [],InU),
		all_sets_union_count(Poss, [],UPoss, [],NUPoss),
		set_count_without(UPoss,NUPoss, InU, PossU,NPossU, Singles),
		set(Union,InU,PossU,[]), UnionVar=Union,
		attach_lengths(Poss, NewPoss, [],Lengths), NewNMax=NMax
	;Union=GlbUVar+PossUVar ->
		sort(GlbUVar,SGlbUVar), sort(PossUVar,SPossUVar),
		all_in_sets_union(In, SGlbUVar,SPossUVar, MoveSets, [],InU),
		all_sets_union(MoveSets, [],MoveElems),
		include_elements(_, MoveElems, SGlbUVar,SPossUVar,
			GlbUVar1,PossUVar1, 0,_),
%at this point:	(InU =< GlbUVar1)
		all_in_sets_union_count(Poss, GlbUVar1,PossUVar1, NewPoss,
					NMax,NewNMax, [],UPoss, [],NUPoss, [],Lengths),
		set_count_without(UPoss,NUPoss, InU, PossU1,NPossU, Singles),
		verify_inclusion(GlbUVar1, InU,PossU1, _),
%at this point:	GlbUVar =< InU+PossU1
%must assure:	PossUVar =< InU+PossU1
		set_intersection(PossUVar1, PossU1, NewPossUVar),
		set(UnionVar, GlbUVar1,NewPossUVar, [])
	;is_list(Union), sort(Union,SUnion), UnionVar=SUnion,
	 all_in_sets_union(In, SUnion,[], _, [],InU),
	 all_in_sets_union_count(Poss, SUnion,[], NewPoss, NMax,NewNMax,
				[],UPoss, [],NUPoss, [],Lengths),
	 set_count_without(UPoss,NUPoss, InU, PossU1,NPossU, Singles),
	 verify_inclusion(SUnion, InU,PossU1, _)
	),
	set_poss(SetVar, NewPoss, NewNMax),
	set_union(SetVar, [UnionVar,InU+NPossU,Singles,Lengths]),
	suspend_and_call(union_glb_singles(SetVar,UnionVar,SuspG), 5,
			UnionVar->cardinal:glb, SuspG), %,SetVar->cardinal:lub]
	suspend_and_call(union_lub_filter(SetVar,UnionVar,SuspP), 5,
			UnionVar->cardinal:lub, SuspP),  %,SetVar]->cardinal:lub
	(var(SetVar) ->
		cardinality(SetVar, CS),
		cardinality(UnionVar, CU),
		suspend_and_call(union_card_possible(SetVar,CS,CU,SuspUCP), 5,
			[SetVar->cardinal:bounded,CS->fd:max,CU->fd:min], SuspUCP)
	;true
	),
	(var(SetVar) ->
%		cardinality(SetVar, CS),
%		cardinality(UnionVar, CU),
		union_att(SetVar, [_,GlbU0+_,_,Lengths0]),
		mindomain(CU, MinCU),
		length(GlbU0, NGlbU0), MinCPossU is MinCU-NGlbU0,
		domain(SetVar, [_:NIn0,_]),
		ucp(Lengths0, MinCPossU, NIn0,MinCard),
		CS #>= MinCard
	;true
	),
	(nonvar(UnionVar) -> kill_suspensions([SuspG,SuspP])
	;suspend(kill_suspensions([SuspG,SuspP]), 7, UnionVar->inst) %lower priority
	).


%--------
% check_union_lub(+SetAtt, +NIn, +OldPoss,+OldNMax, +Poss,+NMax)
%  If SetAtt has union data, update it due to the set's lub change.
%--
check_union_lub(SetAtt, NIn, OldPoss,OldNMax, Poss,NMax):-
	SetAtt = cardinal with union:UnionData,
	nonvar(UnionData), !,
	UnionData = [UnionVar,GlbU+PossU,_Singles,Lengths],
	NPoss is NMax-NIn,
	NOut is OldNMax-NMax,
	(NOut =< NPoss ->
		set_without(OldPoss, Poss, OutSet),
		all_sets_union_count(OutSet, _,_, [],NSetOut),
		list_subtraction(PossU, NSetOut, NewPossU, Out, NewSingles),
		subtract_lengths(OutSet, Lengths, NewLengths),
		setarg(union of cardinal, SetAtt, [UnionVar,GlbU+NewPossU,NewSingles,NewLengths]),
		disjoint(UnionVar, Out)
	;all_sets_union_count(Poss, _,_, [],NewPossU),
	 find_singles(NewPossU, NewSingles),
	 lengths(Poss, [],NewLengths),
	 setarg(union of cardinal, SetAtt, [UnionVar,GlbU+NewPossU,NewSingles,NewLengths]),
	 (var(UnionVar) ->
		domain(UnionVar, [GlbUVar:NInU,PossUVar:_NMaxU]),
		glb_contained_count(_, GlbUVar, GlbU,NewPossU, RestPossU),
		poss_contained_count(PossUVar, RestPossU, NewPossUVar, NInU,NewNMaxU),
		set_domain(UnionVar, [GlbUVar:NInU,NewPossUVar:NewNMaxU])
	 ;verify_inclusion(UnionVar, GlbU,NewPossU, [])
	 )
	).
check_union_lub(_, _, _,_, _,_).

%--------
% check_union_glb(+SetAtt, +OldGlb, +Glb)
%  If SetAtt has union data, update it due to the set's Glb change.
%--
check_union_glb(SetAtt, OldGlb, Glb):-
	SetAtt = cardinal with [domain:[_,Poss:_],union:UnionData],
	nonvar(UnionData), !,
	UnionData = [UnionVar,GlbU+PossU|_], %,_Singles,Lengths],
	set_without(Glb, OldGlb, InSet),
	all_sets_union(InSet, GlbU,NewGlbU),
	set_count_without(_,PossU, NewGlbU, _,NewPossU, NewSingles),
	lengths(Poss, [],NewLengths),
	setarg(union of cardinal, SetAtt, [UnionVar,NewGlbU+NewPossU,NewSingles,NewLengths]),
	(var(UnionVar) ->
		domain(UnionVar, [GlbUVar:NInU,PossUVar:NMaxU]),
		include_elements(_, NewGlbU, GlbUVar,PossUVar, GlbO,PossO, NInU,NInO),
		set_domain(UnionVar, [GlbO:NInO,PossO:NMaxU])
	;set_without(NewGlbU, UnionVar, [])
	).
check_union_glb(_, _, _).



%--------
% all_sets_union_count(+SetsList, +SetIn,?SetAny, +NSetIn,?NSetAny)
%  SetAny is the union of all sets in (SetsList \/ SetIn).
% NSetIn and NSetAny have the same elements of SetIn and SetAny (resp.), but with counters
% attached (in the form Elem:N), representing the number of times each element has been
% counted. All sets are ordered lists.
%--
all_sets_union_count([], U,U, NU,NU).
all_sets_union_count([H|T], Ui,Uo, NUi,NUo):-
	is_list(H,L), !,
	set_union_count(L, Ui,U1, NUi,NU1),
	all_sets_union_count(T, U1,Uo, NU1,NUo).
all_sets_union_count([_|T], Ui,Uo, NUi,NUo):-
	all_sets_union_count(T, Ui,Uo, NUi,NUo).

%--------
% all_in_sets_union_count(+SetsList, +GlbUVar,+PossUVar, -NewPoss, +NMax,-NewNMax,
%			+SetIn,?SetAny, +NSetIn,?NSetAny, +LengthsIn,-LengthsOut)
% All sets in SetsList contained in GlbUVar+PossUVar constitute NewPoss and are united
% with SetIn to form SetAny. Lengths is the list of lengths of NewPoss.
% All sets are ordered lists.
%--
all_in_sets_union_count([], _,_, [], NMax,NMax, U,U, NU,NU, Lengths,Lengths).
all_in_sets_union_count([H|T], Glb,Poss, PossOut, NMaxI,NMaxO, Ui,Uo, NUi,NUo, Li,Lo):-
	is_list(H,L), !,
	set_without(L, Glb, D),
	(set_without(D, Poss, []) ->
		set_union_count(L, Ui,U1, NUi,NU1),
		length(L,Len), PossOut=[L:Len|Poss1], NMax1=NMaxI,
		add_length(Li, Len, L1)
	; U1=Ui, NU1=NUi, PossOut=Poss1, NMax1 is NMaxI-1, L1=Li
	),
	all_in_sets_union_count(T, Glb,Poss, Poss1, NMax1,NMaxO, U1,Uo, NU1,NUo, L1,Lo).
all_in_sets_union_count([H|T], Glb,Poss, [H|PossOut], NMaxI,NMaxO, Ui,Uo, NUi,NUo, Li,Lo):-
	all_in_sets_union_count(T, Glb,Poss, PossOut, NMaxI,NMaxO, Ui,Uo, NUi,NUo, Li,Lo).


%--------
% all_in_sets_union(+SetsList, +Glb,+Poss, -Move, +SetIn,?SetAny)
%  SetAny is the union of all sets in (SetsList \/ SetIn). All sets are ordered lists.
% Sets in SetsList must be completely inside Glb+Poss. Move is a list of sets of elements
% to be passed from Poss to Glb.
%--
all_in_sets_union([], _,_, [], U,U).
all_in_sets_union([H|T], Glb,Poss, [H_Glb|Move], Ui,Uo):-
	is_list(H), !,
	set_without(H, Glb, H_Glb),
	set_intersection(H_Glb, Poss, H_Glb),
	set_union(H,Ui, U1),
	all_in_sets_union(T, Glb,Poss, Move, U1,Uo).
all_in_sets_union([_|T], Glb,Poss, Move, Ui,Uo):-
	all_in_sets_union(T, Glb,Poss, Move, Ui,Uo).


%--------
% set_union_count(+Set1, +Set2,?SetAny, +NSet2,?NSetAny)
%  SetAny is the union of Set1 with Set2.
% NSet2 and NSetAny have the same elements of Set2 and SetAny (resp.), but with counters
% attached (in the form Elem:N), representing the number of times each element has been
% counted. All sets are ordered lists.
%--
set_union_count([], L,U, NU,NU):- !, U=L.
set_union_count(L, [],U, [],NU):- !, U=L, add_count(L, NU).
set_union_count([H|T1], [H|T2],L, [H:N|NT2],[H:N1|NU]):-
	!, L=[H|U], N1 is N+1,
	set_union_count(T1, T2,U, NT2,NU).
set_union_count([H1|T1], [H2|T2],L, [H2N|NT2],[H1:1|NU]):-
	H1 @< H2, !, L=[H1|U],
	set_union_count(T1, [H2|T2],U, [H2N|NT2],NU).
set_union_count(L1, [H2|T2],[H2|U], [H2N|NT2],[H2N|NU]):-
	set_union_count(L1, T2,U, NT2,NU).

%--------
% add_count(+Set, ?SetCount)
%  SetCount is Set with elements having an attached counter (initially =1).
%--
add_count([], []).
add_count([H|T], [H:1|T1]):- add_count(T, T1).


%--------
% set_count_without(+Set,+NSet, +WithoutSet, ?NewSet,?NNewSet, ?Singles)
%  NewSet is Set without WithoutSet. N-Sets have counted elements.
% Singles are the resulting elements with count=1. All sets are ordered lists.
%--
set_count_without([],[], _, W,[], []):- !, W=[].
set_count_without(L,NL, [], W,NL, Singles):- !, W=L, find_singles(NL, Singles).
set_count_without([H|T1],[H:_|NT1], [H|T2], W,NW, Singles):-
	!, set_count_without(T1,NT1, T2, W,NW, Singles).
set_count_without([H1|T1],[H1:N|NT1], [H2|T2], W,[H1:N|NW1], Singles):-
	H1 @< H2, !, W=[H1|W1],
	(N=1 -> Singles=[H1|Singles1] ; Singles=Singles1),
	set_count_without(T1,NT1, [H2|T2], W1,NW1, Singles1).
set_count_without(L1,NL1, [_|T2], W,NW, Singles):-
	set_count_without(L1,NL1, T2, W,NW, Singles).

%--------
% find_singles(+NSet, ?Singles)
% Singles are the elements of NSet with count=1. Sets are ordered lists.
%--
find_singles([], []).
find_singles([H:1|T], [H|S]):- !, find_singles(T, S).
find_singles([_|T], S):- find_singles(T, S).

%--------
% list_subtraction(+NSet, +MinusNSet, ?NewNSet, ?ElementsOut, ?Singles)
%  N-Sets have counted elements. Corresponding counts are subtracted.
% Used to update union poss. All sets are ordered lists.
% Elements in MinusNSet not in NSet are ignored (correspond to glb elements).
% ElementsOut are the elements out of NewNSet (count=0).
% Singles are the elements with count=1.
%--
list_subtraction([], _, [], [], []).
list_subtraction([H:N|T1], [H:N|T2], Sub, [H|Out], Singles):-
	!, list_subtraction(T1, T2, Sub, Out, Singles).
list_subtraction([H:N1|T1], [H:N2|T2], [H:N|Sub], Out, Singles):-
	!, N is N1-N2, (N=1 -> Singles=[H|Singles1] ; Singles1=Singles),
	list_subtraction(T1, T2, Sub, Out, Singles1).
list_subtraction([H1:N1|T1], [H2:_|T2], Sub, Out, Singles):-
	H1 @> H2, !,
	list_subtraction([H1:N1|T1], T2, Sub, Out, Singles).
list_subtraction([H:N|T], L, [H:N|Sub], Out, Singles):-
	(N=1 -> Singles=[H|Singles1] ; Singles1=Singles),
	list_subtraction(T, L, Sub, Out, Singles1).



%--------
% glb_contained_count(?Bound, +GlbUVar, +GlbU,+PossU, -RestPossU)
%  Succeeds if GlbUVar is contained in GlbU+PossU (PossU has counters).
% All sets are ordered lists.
%  Bound just indicates which of GlbU or PossU is tried to find the first
% element of GlbUVar. It is there so that in the end of last clause (when we know
% that it must be searched in PossU) we can call glb_contained_count(poss,...
% and only the "poss" clauses will be considered, without unnecessary
% unifications and comparisons (@<).
%--
glb_contained_count(nil, [], _,Poss, Poss).
glb_contained_count(glb, [H|T], [H|Glb],Poss, RestPoss):-
	!, glb_contained_count(_, T, Glb,Poss, RestPoss).
glb_contained_count(glb, [H|T], [X|Glb],Poss, RestPoss):-
	X @< H, !, glb_contained_count(_, [H|T], Glb,Poss, RestPoss).
glb_contained_count(poss, [H|T], Glb,[H:_|Poss], RestPoss):-
	!, glb_contained_count(_, T, Glb,Poss, RestPoss).
glb_contained_count(poss, [H|T], Glb,[X:N|Poss], [X:N|RestPoss]):-
	X @< H, !, glb_contained_count(poss, [H|T], Glb,Poss, RestPoss).

%--------
% poss_contained_count(+PossUVar, +PossU, -NewPossUVar, +Ni,-No)
%  PossUVar is updated so that it is contained in PossU (with counters).
% All sets are ordered lists.
%--
poss_contained_count([], _, [], N,N).
poss_contained_count([H|T], [H:_|Poss], [H|To], Ni,No):-
	!, N1 is Ni+No, poss_contained_count(T, Poss, To, N1,No).
poss_contained_count([H|T], [X:_|Poss], S, Ni,No):-
	X @< H, !, poss_contained_count([H|T], Poss, S, Ni,No).
poss_contained_count([_|T], Poss, S, Ni,No):-
	poss_contained_count(T, Poss, S, Ni,No).



%--------
% attach_lengths(+Poss, -NewPoss, +LengthsIn, -LengthsOut)
%  Attach lengths of lists in Poss. Collect and count them in Lengths.
% All sets are ordered lists. Lenghts is ordered decreasingly.
%--
attach_lengths([],[], L,L):- !.
attach_lengths([H|T],[LH:N|TL], Li,Lo):-
	is_list(H,LH), !,
	length(LH,N),
	add_length(Li, N, L1),
	attach_lengths(T,TL, L1,Lo).

%--------
% add_length(+LengthsIn, +Length, -LengthsOut)
%  Add Length to Lengths. Lenghts have counters. Greater lengths come first.
%--
add_length([Len:N|T], Len, [Len:N1|T]):- !, N1 is N+1.
add_length([LenH:N|T], Len, [LenH:N|To]):- LenH > Len, !, add_length(T, Len, To).
add_length(L, Len, [Len:1|L]).


%--------
% subtract_lengths(+Lengths, +LengthsIn, -LengthsOut)
%  Subtract Lengths from LengthsIn to yield to LengthsOut.
%--
subtract_lengths([], Lengths,Lengths):- !.
subtract_lengths([_:N|T], Li,Lo):-
	subtract_length(Li, N, L1),
	subtract_lengths(T, L1,Lo).

%--------
% subtract_length(+LengthsIn, +Length, -LengthsOut)
%  Subtract Length from Lengths (where it must be present).
%--
subtract_length([Len:1|T], Len, T):- !.
subtract_length([Len:N|T], Len, [Len:N1|T]):- !, N1 is N-1.
subtract_length([LenH:N|T], Len, [LenH:N|To]):- LenH > Len, !, subtract_length(T, Len, To).
subtract_length(L, Len, _):- write(error_sub_len:L-Len), nl, fail.


%--------
% lengths(+Poss, +LengthsIn, -LengthsOut)
%  Collect and count lengths of lists in Poss into Lengths.
% All sets are ordered lists. Lengths is ordered decreasingly.
%--
lengths([], Lengths,Lengths):- !.
lengths([_:N|T], Li,Lo):- add_length(Li, N, L1), lengths(T, L1,Lo).


%--------
% union_card_possible(+SetVar, +CardSet, +CardUnion, +Susp)
%  Demon:
% Check if SetVar with cardinality CardSet is consistent with union cardinality CardUnion,
% remove sets too small from SetVar's poss, and include mandatory large sets.
%--
:-demon union_card_possible/4.
union_card_possible(SetVar, CardSet, CardUnion, Susp):-
	domain(SetVar, CardSet, [_:NIn,Poss:NMax]),
	union_att(SetVar, [UnionVar,GlbU+_,_,Lengths]),
	maxdomain(CardSet, MaxCS),
	mindomain(CardUnion, MinCU),
	length(GlbU, NGlbU), MinCPossU is MinCU-NGlbU, MaxCPossS is MaxCS-NIn,
	union_card_possible_remove(Lengths, MinCPossU, MaxCPossS, SetVar, Poss,NMax, Change),
	(Change == true ->
		domain(SetVar, CardSet, [_:NIn1,Poss1:NMax1]),
		union_att(SetVar, UnionVar, [_,GlbU1+_,_,Lengths1]),
		length(GlbU1, NGlbU1),
		mindomain(CardUnion, MinCU1),
		maxdomain(CardSet, MaxCS1),
		MinCPossU1 is MinCU1-NGlbU1, MaxCPossS1 is MaxCS1-NIn1
	;Poss1=Poss,NMax1=NMax, Lengths1=Lengths,
	 MinCPossU1 is MinCU-NGlbU, MaxCPossS1 is MaxCS-NIn
	),
	union_card_possible_include(Lengths1, MinCPossU1, MaxCPossS1,
				SetVar,CardSet, Poss1, NMax1),
	(nonvar(SetVar) -> kill_suspension(Susp)
	;(Change == true, nonvar(UnionVar)) -> union_glb_singles(SetVar, UnionVar, _)
	;true
	).

%--------
% union_card_possible_remove(+Lengths, +CardUnionPoss, +CardSetPoss,
%				+Set, +Poss, +NMax, -Change)
%  According to still possible Lengths of Set (of sets) elements, check if it is still
% possible to obtain an extra union cardinality of at least CardUnionPoss with
% at most CardSetPoss set elements, and remove sets too small from Poss.
% Change = true if predicate sets a new Set domain (poss), and consequently triggers
% constraint propagation; otherwise, Change = false,
%--
union_card_possible_remove(Lengths, NU, NS, Set, Poss, NMax, Change):-
	NS1 is NS-1,  %Try with one less set element (itself a set)
	ucp(Lengths, NU, NS1, Lengths1, MinLength), %MinLength is a minimum length of the smallest possible set element
	(MinLength =< 0 -> Change = false
	;MinLength == 1 -> (Poss=[[]:0|P1] -> N1 is NMax-1, set_poss(Set,P1,N1), Change=true
			   ;Change = false
			   )
	;(Lengths1 = [Len:_], Len >= MinLength) -> Change = false
	;\+ (Lengths1=[MaxPossLen:_|_], MaxPossLen < MinLength),
	 remove_small_lists(Poss, MinLength, NewPoss, NMax, NewNMax),
	 set_poss(Set, NewPoss, NewNMax), Change = true
	).

remove_small_lists([], _, [], N,N):- !.
remove_small_lists([_:N|T], Min, NewPoss, NMax, NewNMax):-
	N < Min, !,
	NMax1 is NMax-1,
	remove_small_lists(T, Min, NewPoss, NMax1, NewNMax).
remove_small_lists([H|T], Min, [H|NewT], NMax, NewNMax):-
	remove_small_lists(T, Min, NewT, NMax, NewNMax).

%--------
% union_card_possible_include(+Lengths, +CardUnionPoss, +CardSetPoss,
%			+Set, +CardSet, +Poss, +NMax)
%  According to still possible Lengths of Set (of sets) elements, check if it is still
% possible to obtain an extra union cardinality of at least CardUnionPoss with
% at most CardSetPoss set elements, and include mandatory large sets from Poss in Set.
%--
union_card_possible_include(Lengths, NU, NS, Set,CardSet, Poss, NMax):-
	(Lengths == [] -> true
	;Lengths = [MaxLen:NMaxLen|Lens],
	 (NMaxLen == 1 -> Lengths1 = Lens
	 ;NMaxLen1 is NMaxLen-1, Lengths1 = [MaxLen:NMaxLen1|Lens]
	 ), %Try without a maximum length set
	 (ucp(Lengths1,NU,NS) -> true
	 ;domain(Set, CardSet, [Glb:NIn,_]),
	  include_big_lists(NMaxLen, MaxLen, Poss, Glb, NIn, NewNIn,NewGlb,NewPoss),
	  set_domain(Set, [NewGlb:NewNIn,NewPoss:NMax])
	 )
	).

include_big_lists(0, _Len, Poss, Glb, NIn, NIn,Glb,Poss):- !.
include_big_lists(N, Len, [Hp:NHp|Poss], [Hg|Glb], NIn, NInO,[Hg|GlbO],PossO):-
	Hg @< Hp, !,
	include_big_lists(N, Len, [Hp:NHp|Poss], Glb, NIn, NInO,GlbO,PossO).
include_big_lists(N, Len, [Hp:Len|Poss], Glb, NIn, NInO,[Hp|GlbO],PossO):-
	!, NIn1 is NIn+1, N1 is N-1,
	include_big_lists(N1, Len, Poss, Glb, NIn1, NInO,GlbO,PossO).
include_big_lists(N, Len, [H|Poss], Glb, NIn, NInO,GlbO,[H|PossO]):-
	include_big_lists(N, Len, Poss, Glb, NIn, NInO,GlbO,PossO).


%--------
% ucp(+Lengths, +NeccessaryCard, +NumSets, -LengthsOut, -MinLength)
%
%  According to possible Lengths of Set (of sets) elements, check if it is still
% possible to obtain an extra union cardinality of at least NeccessaryCard with
% NumSets set elements, and return in MinLength the minimum length that an additional
% possible set element would have to have to reach NeccessaryCard of union, and in
% LengthsOut the remaining lengths with counters, having consumed the NumSets larger ones.
%--
ucp([Len:N|Lengths], NU, NS, LengthsOut, NUo):-
	!, NS1 is NS-N,
	(NS1 >= 0 -> NU1 is NU-N*Len,
		(NU1 > 0 -> ucp(Lengths, NU1, NS1, LengthsOut, NUo)
		;LengthsOut=[], NUo=NU1   %NeccessaryCard already achieved
		)
	;NUo is NU-NS*Len,   %no more available sets (NS < N)
	 NLeft is N-NS, LengthsOut=[Len:NLeft|Lengths]  %NLeft is -NS1
	).
ucp([], NU, _, [], NU):- NU =< 0.


%--------
% ucp(+Lengths, +NeccessaryCard, +NumSets)
%
%  According to possible Lengths of Set (of sets) elements, check if it is still
% possible to obtain an extra union cardinality of at least NeccessaryCard with
% NumSets set elements.
%--
ucp([Len:N|Lengths], NU, NS):-
	!, NS1 is NS-N,
	(NS1 >= 0 -> NU1 is NU-N*Len,
		(NU1 > 0 -> ucp(Lengths, NU1, NS1)
		;true   %NeccessaryCard already achieved
		)
	;NS*Len >= NU   %no more available sets (NS < N)
	).
ucp([], NU, _):- NU =< 0.



%--------
% ucp(+Lengths, +NeccessaryCard, +CardIn,-CardOut)
%
%  According to possible Lengths of Set (of sets) elements, determine the
% neccessary minimum set poss cardinality (CardOut-CardIn) to be able to obtain
% an extra union cardinality of at least NeccessaryCard.
%--
ucp([Len:N|Lengths], NU, Ci,Co):-
	NU1 is NU-N*Len,
	(NU1 > 0 -> C1 is Ci+N, ucp(Lengths, NU1, C1,Co)
	;Co is Ci+fix(ceiling(NU/Len))   %NeccessaryCard already achieved
	).




%--------
% union_glb_singles(+SetVar, +UnionVar, +Suspension)
%  Demon:
% (SetVar has union data). Include in SetVar's Glb the only sets that contain each
% single element already in UnionVar's Glb. Trigered by the union's Glb change.
%--
:-demon union_glb_singles/3.
union_glb_singles(SetVar, UVar, Susp):-
	union_att(SetVar, UVar, [_UVar,_,Singles,_]),
	glb(UVar, GlbUVar),
	set_intersection(Singles, GlbUVar, SinglesInUnion),  %, 0,NSinglesInUnion),
	domain(SetVar, [Glb:NIn,Poss:NMax]),
%	NPoss is NMax-NIn,
%	((NSinglesInUnion > NPoss / 10 ; nonvar(UVar)) ->	%Heuristic
		include_sets_with(SinglesInUnion, Glb,Poss, NewGlb-[],NIn,NewNIn, NewPoss),
		set_domain(SetVar, [NewGlb:NewNIn,NewPoss:NMax]),
%	;true
%	),
	(var(UVar) -> true ; kill_suspension(Susp)).


%--------
% union_lub_filter(+SetVar, +UnionVar, +Suspension)
%  Demon:
% (SetVar has union data). Remove from SetVar's Poss the sets containing elements
% not in UnionVar's Lub. Trigered by the union's Lub change.
%--
:-demon union_lub_filter/3.
union_lub_filter(SetVar, UVar, Susp):-
	union_att(SetVar, UVar, [_UVar,_+PossU|_]), %,Singles,_],
	lub(UVar, LubUVar),
	set_without(PossU, LubUVar, NotInUnion, 0,NNotInUnion),
	domain(SetVar, [_:_NIn,Poss:NMax]),
%	NPoss is NMax-NIn,
%	((NNotInUnion > NPoss / 10 ; (nonvar(UVar), NNotInUnion > 0)) ->	%Heuristic
	(NNotInUnion > 0 ->
		val_list(NotInUnion, ValNotInUnion),
		remove_sets_with(Poss, ValNotInUnion, NewPoss, 0,NOut),
		NewNMax is NMax-NOut,
		set_poss(SetVar, NewPoss, NewNMax)
	;true
	),
	(var(UVar) -> true ; kill_suspension(Susp)).


%--------
% include_sets_with(Elements, Glb,Poss, NewGlb-DGlb,Ni,No, NewPoss)
%  Include in Glb (No-Ni) sets of Poss that intersect with Elements, to yield new glb
% (in difference list NewGlb-Dglb) and NewPoss (with removed such sets).
%--
include_sets_with([], Glb,Poss, Glb-[],N,N, Poss):- !.
include_sets_with(Elements, Glb,[H|Poss], NewGlb-DGlb,Ni,No, NewPoss):-
	val(H, ValH),
	set_intersection_difference(Elements, ValH, Intersection, RestElems),
	(Intersection=[] -> Glb1=Glb, NGlb1=NewGlb, N1=Ni, NewPoss=[H|Poss1]
	;insert(Glb, ValH, _, NewGlb-[ValH|NGlb1], Glb1), N1 is Ni+1, NewPoss=Poss1
	), include_sets_with(RestElems, Glb1,Poss, NGlb1-DGlb,N1,No, Poss1).

%--------
% remove_sets_with(Poss, Elements, NewPoss, Ni,No)
%  Remove from Poss (No-Ni) sets intersecting with Elements to yield NewPoss.
%--
%remove_sets_with(Poss, [], Poss, N,N):- !.
remove_sets_with([], _, [], N,N):- !.
remove_sets_with([H|Poss], Elements, NewPoss, Ni,No):-
	val(H, ValH),
	(set_intersection(Elements, ValH, []) -> NewPoss=[H|Poss1], N1=Ni
	;NewPoss=Poss1, N1 is Ni+1
	), remove_sets_with(Poss, Elements, Poss1, N1,No).
