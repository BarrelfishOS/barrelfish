%   File   : MAP.PL
%   Author : R.A.O'Keefe
%   Updated: 7 June 1984
%   Purpose: Implement finite maps.
%   Needs  : list_to_assoc from ASSOC.PL, ord_disjoint from ORDSET.PL

/*  A finite map is a function from terms to terms with a finite
    domain.  This definition actually implies that its domain
    consists of ground terms, and the code below assumes that.
    The representation is similar to the representation for bags
    (indeed a bag could be regarded as a map from keys to integers),
    that is, the empty map is 'map' and any other map is
	map(Key,Val,Map)
    where Map is a finite map and Key is @< than every key in Map.
*/

:- module(map).			% SEPIA header
:- export
	is_map/1,		%  map ->
	list_to_map/2,		%  list -> map
	map_agree/2,		%  map x map ->
	map_compose/3,		%  map x map -> map
	map_disjoint/2,		%  map x map ->
	map_domain/2,		%  map -> ordset
	map_exclude/3,		%  map x ordset -> map
	map_include/3,		%  map x ordset -> map
	map_invert/2,		%  map -> map
	map_map/3,		%  relation x map -> map
	map_range/2,		%  map -> ordset
	map_to_assoc/2,		%  map -> tree
	map_union/3,		%  map x map -> map
	map_update/3,		%  map x map -> map
	map_update/4,		%  map x key x val -> map
	map_value/3,		%  map x dom -> rng
	portray_map/1.		%  map ->

:- lib(ordset), lib(assoc), lib(apply).
:- import ord_disjoint/2 from ordset.
:- import list_to_assoc/4 from assoc.

:- mode
	is_map(+),
	    is_map(+, +),
	list_to_map(+, ?),
	    list_to_map_(+, ?),
	map_agree(+, +),
	    map_agree(+, +, +, +, +, +, +),
	map_compose(+, +, ?),
	    map_compose_(+, +, ?),
		map_compose_(+, +, +, +, +, +, +, ?),
	map_disjoint(+, +),
	map_domain(+, ?),
	map_exclude(+, +, ?),
	    map_exclude(+, +, +, +, +, +, ?),
	map_include(+, +, ?),
	    map_include(+, +, +, +, +, +, ?),
	map_invert(+, ?),
	    map_invert_(+, -),
	map_map(+, +, ?),
	map_range(+, ?),
	    map_range_(+, -),
	map_to_assoc(+, ?),
	map_to_list(+, ?),
	map_union(+, +, ?),
	    map_union(+, +, +, +, +, +, +, ?),
	map_update(+, +, ?),
	    map_update(+, +, +, +, +, +, +, ?),
	map_update(+, +, +, ?),
	    map_update(+, +, +, +, +, +, ?),
	map_value(+, +, ?),
	    map_value(+, +, +, +, ?),
	portray_map(+),
	    portray_map(+, +).

%   is_map(Thing)
%   is true when Thing is a map.  If you use the predicates in this
%   file, you have no way of constructing a map with an unbound tail,
%   so such structures are NOT recognised as bags (this avoids a
%   possible infinite loop.

is_map(map).
is_map(map(Key,_,Map)) :-
	nonvar(Map),
	is_map(Map, Key).

is_map(map, _).
is_map(map(Key,_,Map), PreviousKey) :-
	nonvar(Map),
	PreviousKey @< Key,
	is_map(Map, Key).

%   list_to_map(+KeyValList, ?Map)
%   takes a list of Key-Value pairs and orders them to form a representation
%   of a finite map.  The list may not have two elements with the same Key.

list_to_map(List, Map) :-
	keysort(List, Sorted),
	list_to_map_(Sorted, Map).

list_to_map_([], map).
list_to_map_([Key-Val|List], map(Key,Val,Map)) :-
	list_to_map_(List, Map).



%   map_agree(+Map1, Map2)
%   is true if whenever Map1 and Map2 have a key in common, they
%   agree on its value.  If they have no keys in common they agree.

map_agree(_, map) :- !.
map_agree(map, _).
map_agree(map(Key1,Val1,Map1), map(Key2,Val2,Map2)) :-
	compare(R, Key1, Key2),
	map_agree(R, Key1, Val1, Map1, Key2, Val2, Map2).

map_agree(<, _, _, Map1, Key2, Val2, Map2) :-
	map_agree(Map1, map(Key2,Val2,Map2)).
map_agree(>, Key1, Val1, Map1, _, _, Map2) :-
	map_agree(map(Key1,Val1,Map1), Map2).
map_agree(=, _, Val, Map1, _, Val, Map2) :-
	map_agree(Map1, Map2).



%   map_compose(Map1, Map2, Composition)
%   constructs Map1 o Map2.  That is, for each K-T in Map1 such that
%   there is a T-V in Map2, K-V is in Composition.  The way this is
%   done requires the range of Map1 to be ground as well as the domains
%   of both maps, but then any fast composition has the same problem.

map_compose(Map1, Map2, Composition) :-
	map_invert_(Map1, Inv0),
	keysort(Inv0, Inv1),
	map_compose_(Inv1, Map2, Mid0),
	keysort(Mid0, Mid1),
	list_to_map_(Mid1, Composition).

map_compose_(_, map, []) :- !.
map_compose_([], _, []).
map_compose_([Val1-Key1|Map1], map(Key2,Val2,Map2), Composition) :-
	compare(R, Val1, Key2),
	map_compose_(R, Val1, Key1, Map1, Key2, Val2, Map2, Composition).

map_compose_(<, _, _, Map1, Key2, Val2, Map2, Composition) :-
	map_compose_(Map1, map(Key2,Val2,Map2), Composition).
map_compose_(>, Val1, Key1, Map1, _, _, Map2, Composition) :-
	map_compose_([Val1-Key1|Map1], Map2, Composition).
map_compose_(=, Com, Key1, Map1, Com, Val2, Map2, [Key1-Val2|Composition]) :-
	map_compose_(Map1, map(Com,Val2,Map2), Composition).



%   map_disjoint(+Map1, +Map2)
%   is true when the two maps have no domain elements in common.
%   That is, if K-V1 is in Map1, there is no K-V2 in Map2 and conversely.
%   This implementation assumes you have loaded the ordered sets package.

map_disjoint(Map1, Map2) :-
	map_domain(Map1, Dom1),
	map_domain(Map2, Dom2),
	ord_disjoint(Dom1, Dom2).



%   map_domain(+Map, ?Domain)
%   unifies Domain with the ordered set representation of the domain
%   of the finite map Map.  As the keys (domain elements) of Map are
%   in ascending order and there are no duplicates, this is trivial.

map_domain(map, []).
map_domain(map(Key,_,Map), [Key|Domain]) :-
	map_domain(Map, Domain).



%   map_exclude(+Map, +Set, ?Restricted)
%   constructs a restriction of the Map by dropping members of the Set
%   from the Restricted map's domain.  That is, Restricted and Map agree,
%   but domain(Restricted) = domain(Map)\Set.
%   Set must be an *ordered* set.

map_exclude(Map, [], Map) :- !.
map_exclude(map, _, map).
map_exclude(map(Key,Val,Map), [Elt|Set], Restricted) :-
	compare(R, Key, Elt),
	map_exclude(R, Key, Val, Map, Elt, Set, Restricted).

map_exclude(<, Key, Val, Map, Elt, Set, map(Key,Val,Restricted)) :-
	map_exclude(Map, [Elt|Set], Restricted).
map_exclude(>, Key, Val, Map, _, Set, Restricted) :-
	map_exclude(map(Key,Val,Map), Set, Restricted).
map_exclude(=, _, _, Map, _, Set, Restricted) :-
	map_exclude(Map, Set, Restricted).



%   map_include(+Map, +Set, ?Restricted)
%   constructs a restriction of the Map by dropping everything which is
%   NOT a member of Set from the restricted map's domain.  That is, the
%   Restricted and original Map agree, but
%   domain(Restricted) = domain(Map) intersection Set.
%   Set must be an *ordered* set.

map_include(Map, [], Map) :- !.
map_include(map, _, map).
map_include(map(Key,Val,Map), [Elt|Set], Restricted) :-
	compare(R, Key, Elt),
	map_include(R, Key, Val, Map, Elt, Set, Restricted).

map_include(<, _, _, Map, Elt, Set, Restricted) :-
	map_include(Map, [Elt|Set], Restricted).
map_include(>, Key, Val, Map, _, Set, Restricted) :-
	map_include(map(Key,Val,Map), Set, Restricted).
map_include(=, Key, Val, Map, _, Set, map(Key,Val,Restricted)) :-
	map_include(Map, Set, Restricted).



%   map_invert(+Map, ?Inverse)
%   unifies Inverse with the inverse of a finite invertible map.
%   All we do is swap the pairs round, sort, and check that the
%   result is indeed a map.  

map_invert(Map, Inverse) :-
	map_invert_(Map, Inv0),
	keysort(Inv0, Inv1),
	list_to_map_(Inv1, Inverse).

%   map_invert_ takes a list of key-value pairs and swaps the pairs around.

map_invert_(map, []).
map_invert_(map(Key,Val,Map), [Val-Key|Inv]) :-
	map_invert_(Map, Inv).



%   map_map(+Predicate, +Map1, ?Map2)
%   composes Map1 with the Predicate, so that K-V2 is in Map2 if
%   K-V1 is in Map1 and Predicate(V1,V2).  Really, the predicate
%   should come second, but there is this convention that the
%   predicate being mapped always comes first.  It doesn't do
%   marvels for Dec-10 Prolog's indexing either.

:- tool(map_map/3, map_map/4).

map_map(_, map, map, _).
map_map(Pred, map(K,V1,Map1), map(K,V2,Map2), Module) :-
	apply(Pred, [V1,V2])@Module,
	map_map(Pred, Map1, Map2, Module).



%   map_range(+Map, ?Range)
%   unifies Range with the ordered set representation of the range of the
%   finite map Map.  Note that the cardinality (length) of the domain and
%   the range are seldom equal, except of course for invertible maps.

map_range(Map, Range) :-
	map_range_(Map, Random),
	sort(Random, Range).

map_range_(map, []).
map_range_(map(_,Val,Map), [Val|Range]) :-
	map_range_(Map, Range).



%   map_to_assoc(+Map, ?Assoc)
%   converts a finite map held as an ordered list of Key-Val pairs to
%   an ordered binary tree such as the library file ASSOC works on.
%   This predicate calls an internal routine of that file, so both
%   must be compiled or both interpreted.  Eventually the two files
%   should be combined.

map_to_assoc(Map, Assoc) :-
	map_to_list(Map, List),
	length(List, N),
	list_to_assoc(N, List, Assoc, []).



%   map_to_list(+Map, ?KeyValList)
%   converts a map from its compact form to a list of Key-Val pairs
%   such as keysort yields or list_to_assoc wants.

map_to_list(map, []).
map_to_list(map(Key,Val,Map), [Key-Val|List]) :-
	map_to_list(Map, List).



%   map_union(+Map1, +Map2, ?Union)
%   forms the union of the two given maps.  That is Union(X) =
%   Map1(X) if it is defined, or Map2(X) if that is defined.
%   But when both are defined, both must agree.  (See map_update
%   for a version where Map2 overrides Map1.)

map_union(Map, map, Map) :- !.
map_union(map, Map, Map).
map_union(map(Key1,Val1,Map1), map(Key2,Val2,Map2), Union) :-
	compare(R, Key1, Key2),
	map_union(R, Key1, Val1, Map1, Key2, Val2, Map2, Union).

map_union(<, Key1, Val1, Map1, Key2, Val2, Map2, map(Key1,Val1,Union)) :-
	map_union(Map1, map(Key2,Val2,Map2), Union).
map_union(>, Key1, Val1, Map1, Key2, Val2, Map2, map(Key2,Val2,Union)) :-
	map_union(map(Key1,Val1,Map1), Map2, Union).
map_union(=, Key, Val, Map1, Key, Val, Map2, map(Key,Val,Union)) :-
	map_union(Map1, Map2, Union).



%   map_update(+Base, +Overlay, ?Updated)
%   combines the finite maps Base and Overlay as map_union does,
%   except that when both define values for the same key, the
%   Overlay value is taken regardless of the Base value.  This
%   is useful for changing maps (you may know it as the "mu" function).

map_update(Map, map, Map) :- !.
map_update(map, Map, Map).
map_update(map(Key1,Val1,Map1), map(Key2,Val2,Map2), Updated) :-
	compare(R, Key1, Key2),
	map_update(R, Key1, Val1, Map1, Key2, Val2, Map2, Updated).

map_update(<, Key1, Val1, Map1, Key2, Val2, Map2, map(Key1,Val1,Updated)) :-
	map_update(Map1, map(Key2,Val2,Map2), Updated).
map_update(>, Key1, Val1, Map1, Key2, Val2, Map2, map(Key2,Val2,Updated)) :-
	map_update(map(Key1,Val1,Map1), Map2, Updated).
map_update(=, _, _, Map1, Key, Val, Map2, map(Key,Val,Updated)) :-
	map_update(Map1, Map2, Updated).



%   map_update(+Map, +Key, +Val, ?Updated)
%   computes an Updated map which is the same as Map except that the
%   image of Key is Val, rather than the image it had under Map if any.
%   This is an O(N) operation, not O(1).  By using trees we could get
%   O(lgN).  Eventually this package should be merged with ASSOC.PL.

map_update(map, Key, Val, map(Key,Val,map)).
map_update(map(Key1,Val1,Map), Key, Val, Updated) :-
	compare(R, Key1, Key),
	map_update(R, Key1, Val1, Map, Key, Val, Updated).

map_update(<, Key1, Val1, Map, Key, Val, map(Key1,Val1,Updated)) :-
	map_update(Map, Key, Val, Updated).
map_update(=, _, _, Map, Key, Val, map(Key,Val,Map)).
map_update(>, Key1, Val1, Map, Key, Val, map(Key,Val,map(Key1,Val1,Map))).



%   map_value(+Map, +Arg, ?Result)
%   applies the finite map Map to an argument, and unifies Result with
%   the answer.  It fails if Arg is not in the domain of Map, or if the
%   value does not unify with Result.  Note that this operation is O(N)
%   like all the others; this package is really meant for working on
%   maps as wholes.  We can achieve O(lgN) by using trees (as in ASSOC),
%   and eventually MAP and ASSOC should be merged.  In the mean time,
%   use map_to_assoc to convert a map to a tree for faster lookup.

map_value(map(Key,Val,Map), Arg, Result) :-
	compare(R, Key, Arg),
	map_value(R, Val, Map, Arg, Result).

map_value(<, _, Map, Arg, Result) :- !,
	map_value(Map, Arg, Result).
map_value(=, Result, _, _, Result).



%   portray_map(+Map)
%   writes a finite Map to the current output stream in a pretty
%   form so that you can easily see what it is.  Note that a map
%   written out this way can NOT be read back in.  The point of
%   this predicate is that you can add a clause
%	portray(X) :- is_map(X), !, portray_map(X).
%   to get maps displayed nicely by print/1.

portray_map(map) :- !,
	write('map{'), write('}').
portray_map(Map) :-
	portray_map(Map, 'map{').

portray_map(map, _) :-
	write('}').
portray_map(map(Key,Val,Map), Prefix) :-
	write(Prefix),
	print(Key), write('->'), print(Val),
	!,
	portray_map(Map, ', ').

