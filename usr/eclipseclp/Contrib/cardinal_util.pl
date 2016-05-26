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
% cardinal_util.pl      By Francisco Azevedo
%
% Ordered lists operations and other Utility predicates of Cardinal.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

val(L:_, L):- !.
val(V,V).

val_list([L:_|T], [L|VT]):- !, val_list(T,VT).
val_list(L,L).

sum_cards([H|T], SumT + H):- sum_cards(T, SumT).
sum_cards([], 0).

%--------
% lub(+Glb, +Poss, ?Lub)
%  Lub = Glb \/ Poss
%  Lub is the Least Upper Bound of my_set with Greatest Lower Bound Glb,
% and possible extra elements Poss.
%  All sets are ordered lists.
%--
lub([], Poss, Lub):- !, val_list(Poss, Lub).
lub(Glb, [], Lub):- !, Lub=Glb.
lub([A|Glb], [B|Poss], Lub):- val(B,VB), A @< VB, !, Lub=[A|L], lub(Glb, [B|Poss], L).
lub([A|Glb], [B|Poss], [VB|Lub]):- val(B,VB), lub([A|Glb], Poss, Lub).


%--------
% all_sets_union(+SetsList, +SetIn,?SetAny)
%  SetAny is the union of all sets in (SetsList \/ SetIn). All sets are ordered lists.
%--
all_sets_union([], U,U).
all_sets_union([H|T], Ui,Uo):-
	is_list(H,L), !,
	set_union(L,Ui, U1),
	all_sets_union(T, U1,Uo).
all_sets_union([_|T], Ui,Uo):- all_sets_union(T, Ui,Uo).

%--------
% set_union(+Set1, +Set2, ?SetAny)
%  SetAny is the union of Set1 with Set2. All sets are ordered lists.
%--
set_union([],L, U):- !, U=L.
set_union(L,[], U):- !, U=L.
set_union([H|T1],[H|T2], L):- !, L=[H|U], set_union(T1,T2, U).
set_union([H1|T1],[H2|T2], L):-
	H1 @< H2, !, L=[H1|U],
	set_union(T1,[H2|T2], U).
set_union(L1,[H2|T2], [H2|U]):- set_union(L1,T2, U).

%--------
% set_union(+Set1, +Set2, ?SetAny, +Ni,-No)
%  SetAny is the union of Set1 with Set2. No is Ni plus the 
% cardinality of SetAny. All sets are ordered lists.
%--
set_union([],L, U, Ni,No):- !, U=L, length(L, Len), No is Ni+Len.
set_union(L,[], U, Ni,No):- !, U=L, length(L, Len), No is Ni+Len.
set_union([H|T1],[H|T2], L, Ni,No):-
	!, L=[H|U], N1 is Ni+1,
	set_union(T1,T2, U, N1,No).
set_union([H1|T1],[H2|T2], L, Ni,No):-
	H1 @< H2, !, L=[H1|U], N1 is Ni+1,
	set_union(T1,[H2|T2], U, N1,No).
set_union(L1,[H2|T2], [H2|U], Ni,No):-
	N1 is Ni+1,
	set_union(L1,T2, U, N1,No).


%--------
% set_intersection(+Set1, +Set2, ?SetBoth)
%  SetBoth is the intersection of Set1 with Set2. All sets are ordered lists.
%--
set_intersection([],_, I):- !, I=[].
set_intersection(_,[], I):- !, I=[].
set_intersection([H1|T1],[H2|T2], L):- val(H1,H2), !, L=[H1|I], set_intersection(T1,T2,I).
set_intersection([H1|T1],[H2|T2], I):-
	val(H1,VH1), val(H2,VH2), VH1 @< VH2, !,
	set_intersection(T1,[H2|T2], I).
set_intersection(L1,[_|T2], I):- set_intersection(L1,T2, I).

%--------
% set_intersection(+Set1, +Set2, ?SetBoth, +Ni,-No)
%  SetBoth is the intersection of Set1 with Set2. No is Ni plus the 
% cardinality of SetBoth. All sets are ordered lists.
%--
set_intersection([],_, I, N,N):- !, I=[].
set_intersection(_,[], I, N,N):- !, I=[].
set_intersection([H1|T1],[H2|T2], L, Ni,No):-
	val(H1,H2), !, N1 is Ni+1, L=[H1|I],
	set_intersection(T1,T2, I, N1,No).
set_intersection([H1|T1],[H2|T2], I, Ni,No):-
	val(H1,VH1), val(H2,VH2), VH1 @< VH2, !,
	set_intersection(T1,[H2|T2], I, Ni,No).
set_intersection(L1,[_|T2], I, Ni,No):-
	set_intersection(L1,T2, I, Ni,No).

%--------
% set_intersection_difference(+Set1, +Set2, ?SetBoth, ?Set1But2)
%  SetBoth is the intersection of Set1 with Set2.
%  Set1But2 is Set1 without Set2. All sets are ordered lists.
%--
set_intersection_difference([],_, I, D):- !, I=[], D=[].
set_intersection_difference(S,[], I,D):- !, I=[], D=S.
set_intersection_difference([H|T1],[H|T2], L, D):-
	!, L=[H|I], set_intersection_difference(T1,T2, I,D).
set_intersection_difference([H1|T1],[H2|T2], I,D):-
	H1 @< H2, !, D=[H1|D1],
	set_intersection_difference(T1,[H2|T2], I,D1).
set_intersection_difference(L1,[_|T2], I,D):-
	set_intersection_difference(L1,T2, I,D).


%--------
% set_without(+Set, +WithoutSet, ?NewSet)
%  NewSet is Set without WithoutSet. All sets are ordered lists.
%--
set_without([],_, W):- !, W=[].
set_without(L,[], W):- !, W=L.
set_without([H1|T1],[H2|T2], W):- val(H1,H), val(H2,H), !, set_without(T1,T2, W).
set_without([H1|T1],[H2|T2], W):-
	val(H1,VH1), val(H2,VH2), VH1 @< VH2, !, W=[H1|W1],
	set_without(T1,[H2|T2], W1).
set_without(L1,[_|T2], W):- set_without(L1,T2, W).

%--------
% set_without(+Set, +WithoutSet, -NewSet, +Ni,-No)
%  NewSet is Set without WithoutSet. No is Ni plus the 
% cardinality of NewSet. All sets are ordered lists.
%--
set_without([],_, W, N,N):- !, W=[].
set_without(L,[], W, Ni,No):- !, W=L, length(L, Len), No is Ni+Len.
set_without([H1|T1],[H2|T2], W, Ni,No):-
	val(H1,H), val(H2,H), !, set_without(T1,T2, W, Ni,No).
set_without([H1|T1],[H2|T2], W, Ni,No):-
	val(H1,VH1), val(H2,VH2), VH1 @< VH2, !,
	W=[H1|W1], N1 is Ni+1,
	set_without(T1,[H2|T2], W1, N1,No).
set_without(L1,[_|T2], W, Ni,No):- set_without(L1,T2, W, Ni,No).


%--------
% verify_inclusion(+Glb, +Glb1,+Poss1, -GlbOut)
%  Succeeds if each element of Glb is present in Glb1 or Poss1 (disjoint).
% GlbOut is Glb1 without these elements.
%--
verify_inclusion([], Glb,_, Glb).
verify_inclusion([H|T], [Hg|Glb],Poss, GlbOut):-
	val(H,Hg), !, verify_inclusion(T, Glb,Poss, GlbOut).
verify_inclusion([H|T], [X|Glb],Poss, [X|GlbOut]):-
	val(H,VH), X @< VH, !, verify_inclusion([H|T], Glb,Poss, GlbOut).
verify_inclusion([H|T], Glb,Poss, GlbOut):-
	remove(H, Poss, _, Poss1, true),
	verify_inclusion(T, Glb,Poss1, GlbOut).



%--------
% rev_dom(+Dom, +NU, +RevDomI,-RevDomO)
%  Dom is an integer cardinality domain (list). NU is the universe cardinality.
% RevDom is the domain corresponding to the negation.
%--
rev_dom([], _, R,R).
rev_dom([A..B|T], N, Ri,Ro):-
	!, NA is N-A,
	NB is N-B,
	rev_dom(T, N, [NB..NA|Ri],Ro).
rev_dom([H|T], N, Ri,Ro):-
	NH is N-H,
	rev_dom(T, N, [NH|Ri],Ro).





%============================
% Utility Predicates:


%--------
% is_list(+GroundTerm)
%  Succeeds if GroundTerm is a list.
%--
is_list([]).
is_list([_|_]).

%--------
% is_list(+GroundTerm, -List)
%  Succeeds if GroundTerm is a list (List or List:Length).
%--
is_list(L:_, L):- !, is_list(L).
is_list(L, L):- is_list(L).

%--------
% my_setof(Elem, Goal, -List)
%  If setof/3 fails, my_setof succeeds with List empty.
%--
my_setof(Elem, Goal, List):- setof(Elem, Goal, List), !.
my_setof(_, _, []).

%--------
% member_remove(+List, +Elem, -ListButElem)
%  Succeeds if Elem is in List, and removes it to yield ListButElem.
%--
member_remove([Elem|L], Elem, L):- !.
member_remove([H|L], Elem, [H|L1]):- member_remove(L, Elem, L1).

%--------
% till_nth(+N, +List, -ListTillNth)
%  The first N elements of List constitute ListTillNth (List's prefix of length N).
% If N > length(List) then ListTilNth = List
%--
till_nth(0, _, []):- !.
till_nth(N, [H|T], [H|R]):- !, N1 is N-1, till_nth(N1, T, R).
till_nth(_, _, []).

%--------
% from_nth(+N, +List, -ListFromNth)
%  ListFromNth is the suffix of List starting in the N'th element.
% (The first N-1 elements of List are discarded).
%--
from_nth(1, L, L):- !.
from_nth(N, [_|T], L):- N1 is N-1, from_nth(N1, T, L).

%--------
% sorted_length(+List, -Length)
%  Succeeds if List is sorted in strict increasing order. Length is its length.
%--
sorted_length([], 0).
sorted_length([H|T], N):- sort_len(T, H, 1,N).
sort_len([], _, N,N).
sort_len([H|T], X, Ni,No):- X @< H, N1 is Ni+1, sort_len(T, H, N1,No).

%--------
% my_member(?Elem, +List)
%  Check membership in ordered List.
%--
my_member(X, [X|_]):- !.
my_member(X, [H|T]):- H @< X, my_member(X, T).


%--------
% remove(?Elem, +List, -NewList)
%  Remove Elem from ordered List (must contain it) to produce NewList.
%--
t_remove3(remove(Elem, List, NewList), remove(Elem, List, NewList, _, true)).
:-inline(remove/3, t_remove3/2).
remove(Elem, List, NewList):- remove(Elem, List, NewList).  %inlined


%--------
% remove(?Elem, +List, -NewList, -Tail, -Result)
%  Remove Elem from ordered List to produce NewList.
% Tail is the sublist of List following Elem.
% Result = true if Elem originally in List (and successfully removed). Otherwise, Result=fail.
%--
t_remove5(remove(Elem, List, NewList, Tail, Result),
	remove(Elem, List, NewList, _Front, Tail, Result)).
:-inline(remove/5, t_remove5/2).
remove(Elem,List,NewList,Tail,Result):- remove(Elem,List,NewList,Tail,Result).  %inlined

%--------
% remove(?Elem, +List, -NewList, -Front, -Tail, -Result)
%  Remove Elem from ordered List to produce NewList.
% Front is a difference list of the elements before Elem.
% Tail is the sublist of List following Elem.
% Result = true if Elem originally in List (and successfully removed). Otherwise, Result=fail.
%--
remove(X, [H|T], T, F-F,T, true):- val(H,X), !.
remove(X, [H|T], [H|L], [H|F]-D,Tail, Res):-
	val(H,VH), VH @< X, !,
	remove(X, T, L, F-D,Tail, Res).
remove(_, L, L, F-F,L, fail).



%--------
% insert(+List, ?Elem, -NewList)
%  Insert Elem (if not present) in ordered List to produce NewList.
%--
t_insert(insert(List, Elem, NewList), insert(List, Elem, NewList, _Front, _Tail)).
:-inline(insert/3, t_insert/2).
insert(List, Elem, NewList):- insert(List, Elem, NewList).  %inlined

%--------
% insert(+List, ?Elem, -NewList, -FrontDL, -Tail)
%  Insert Elem (if not present) in ordered List to produce NewList.
% FrontDL is a difference list of the elements before Elem.
% Tail is the sublist of List following Elem.
%--
insert([X|T], X, [X|T], F-F, T):- !.
insert([H|T], X, [H|L], [H|F]-DF, Rest):- X @> H, !, insert(T, X, L, F-DF, Rest).
insert(L, X, [X|L], F-F, L).

%--------
% insert_count(+List, ?Elem, -NewList, -FrontDL, -Tail, +NIn,-NOut)
%  Insert Elem (if not present) in ordered List to produce NewList.
% FrontDL is a difference list of the (NOut-NIn) elements before Elem.
% Tail is the sublist of List following Elem.
%--
insert_count([H|T], X, [X|T], F-F, T, N,N):- val(H,X), !.
insert_count([H|T], X, [H|L], [H|F]-DF, Rest, Ni,No):-
	val(H,VH), X @> VH, !, N1 is Ni+1, insert_count(T, X, L, F-DF, Rest, N1,No).
insert_count(L, X, [X|L], F-F, L, N,N).


%--------
% kill_suspensions(+SuspensionsList)
%  Kill suspensions in SuspensionsList.
%--
kill_suspensions([]).
kill_suspensions([H|T]):- kill_suspension(H), kill_suspensions(T).


%--------
% suspend_and_call(+Goal, +Priority, +Conditions, -Suspension)
%  Suspend (with Priority and Conditions) and call Goal, returning Suspension.
%--
t_suspend_and_call(suspend_and_call(Goal, Priority, Conditions, Suspension),
	(suspend(Goal, Priority, Conditions, Suspension), Goal)).

:-inline(suspend_and_call/4, t_suspend_and_call/2).

suspend_and_call(Goal, Priority, Conditions, Suspension):-	   %inlined
	suspend_and_call(Goal, Priority, Conditions, Suspension).  %inlined

% end of utility predicates.
%============================
