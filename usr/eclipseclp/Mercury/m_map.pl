%---------------------------------------------------------------------------%
% Copyright (C) 1993-1999 The University of Melbourne.
% Copyright (C) 2000 Imperial College London.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: m_map.pl.
% Main author: fjh@cs.mu.OZ.AU, conway@cs.mu.OZ.AU.
% Stability: high.
%
% This file provides the (Mercury) 'map' ADT.
% A map (also known as a dictionary or an associative array) is a collection
% of (Key,Data) pairs which allows you to look up any Data item given the
% Key.
%
% The implementation is using balanced trees, as provided by
% m_tree234.pl.  Virtually all the predicates in this file just
% forward the work to the corresponding predicate in m_tree234.pl.
%
% Modified for ECLiPSe by Warwick Harvey <wh@icparc.ic.ac.uk>, April 2000,
% based on revision 1.71 of file mercury/library/map.m from the
% Mercury CVS repository.  See http://www.cs.mu.oz.au/mercury for
% information about obtaining Mercury.
%
% The conversion included stripping out the functional versions of
% predicates and the higher-order predicates.  It also included writing
% more verbose user-level documentation.
%
% Also changed was the type of the second argument of select/3.
% Since ECLiPSe doesn't have a set ADT, a list has been used instead.
%
% This module assumes that keys are ground (so they can't be modified after
% insertion into the map), but allows the values stored to be variables.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module(m_map).

%-----------------------------------------------------------------------------%

% :- type map(_K, _V).

%-----------------------------------------------------------------------------%

:- export
	init/1,
	is_empty/1,
	contains/2,
	member/3,
	search/3,
	lookup/3,
	lower_bound_search/4,
	lower_bound_lookup/4,
	upper_bound_search/4,
	upper_bound_lookup/4,
	inverse_search/3,
	insert/4,
	det_insert/4,
	det_insert_from_corresponding_lists/4,
	det_insert_from_assoc_list/3,
	update/4,
	det_update/4,
	set/4,
	keys/2,
	sorted_keys/2,
	values/2,
	to_assoc_list/2,
	to_sorted_assoc_list/2,
	from_assoc_list/2,
	from_sorted_assoc_list/2,
	delete/3,
	delete_list/3,
	remove/4,
	det_remove/4,
	count/2,
	from_corresponding_lists/3,
	merge/3,
	overlay/3,
	select/3,
	apply_to_list/3,
	optimize/2,
	remove_smallest/4.

:- comment(categories, ["Data Structures"]).
:- comment(summary, "The `map' abstract data type.").
:- comment(author, "Fergus Henderson and Thomas Conway (Mercury) and Warwick Harvey (ECLiPSe)").
:- comment(desc, html("\
	<P>
	This module provides the `map' abstract data type.  A map, also
	known as a dictionary or an associative array, is a collection of
	key/value pairs which allows you to look up any data item given its
	key.
	</P>
	<P>
	Note that keys must be ground, but values are allowed to be 
	variables.
	</P>
	")).

% :- pred init(map(_,_)).
% :- mode init(uo) is det.
:- comment(init/1, [
	amode:		init(-),
	args:		["Map":"The new map"],
	summary:	"Initialise a new (empty) map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[is_empty/1]
]).

% :- pred is_empty(map(_,_)).
% :- mode is_empty(in) is semidet.
:- comment(is_empty/1, [
	amode:		is_empty(+),
	args:		["Map":"A map"],
	summary:	"Check whether a map is empty.",
	fail_if:	"Fails if Map is not an empty map.",
	resat:		no,
	see_also:	[init/1]
]).

% :- pred contains(map(K,_V), K).
% :- mode contains(in, in) is semidet.
:- comment(contains/2, [
	amode:		contains(+, ++),
	args:		["Map":"A map",
			"Key":"The key to look for"],
	summary:	"Check whether a map contains a key.",
	fail_if:	"Fails if Key does not appear in Map.",
	resat:		no,
	see_also:	[search/3, keys/2, sorted_keys/2],
	desc:		html("\
	<P>
	This predicate checks the map Map to see whether it contains an
	entry with key Key.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred member(map(K,V), K, V).
% :- mode member(in, out, out) is nondet.
:- comment(member/3, [
	amode:		member(+, ?, ?),
	args:		["Map":"A map",
			"Key":"A key from Map",
			"Value":"The value in Map corresponding to Key"],
	summary:	"Succeeds if Key and Value unify with a key/value pair from Map.",
	fail_if:	"Fails if Key and Value do not unify with a key/value pair from Map.",
	resat:		yes,
	see_also:	[search/3, lookup/3],
	desc:		html("\
	<P>
	Tries to unify Key and Value with key/value pairs from the map Map.
	</P>
	<P>
	If Key and Value are variables and Map is a map, then all
	members of the map Map are found on backtracking.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred search(map(K,V), K, V).
% :- mode search(in, in, in) is semidet.	% implied
% :- mode search(in, in, out) is semidet.
:- comment(search/3, [
	amode:		search(+, ++, ?),
	args:		["Map":"A map",
			"Key":"A key to search for",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for a key.",
	fail_if:	"Fails if Key does not appear in Map or if Value does not unify with the corresponding value found.",
	resat:		no,
	see_also:	[member/3, lookup/3, inverse_search/3],
	desc:		html("\
	<P>
	This predicate searches the map Map for an entry with key Key.
	If the key is found, then it attempts to unify the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred lookup(map(K,V), K, V).
% :- mode lookup(in, in, out) is det.
:- comment(lookup/3, [
	amode:		lookup(+, ++, ?),
	args:		["Map":"A map",
			"Key":"A key to search for",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for a key.",
	fail_if:	"Fails if Value does not unify with the value corresponding to Key.",
	resat:		no,
	see_also:	[member/3, search/3],
	desc:		html("\
	<P>
	This predicate searches the map Map for an entry with key Key.
	If the key is found, then it attempts to unify the corresponding
	value with Value.  If the key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred lower_bound_search(map(K,V), K, K, V).
% :- mode lower_bound_search(in, in, out, out) is semidet.
:- comment(lower_bound_search/4, [
	amode:		lower_bound_search(+, ++, ?, ?),
	args:		["Map":"A map",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for the smallest key no smaller than SearchKey.",
	fail_if:	"Fails if there are no keys at least as large as SearchKey in Map or if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_lookup/4,
			upper_bound_search/4,
			upper_bound_lookup/4],
	desc:		html("\
	<P>
	This predicate searches the map Map for the entry with the
	smallest key which is no smaller than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred lower_bound_lookup(map(K,V), K, K, V).
% :- mode lower_bound_lookup(in, in, out, out) is det.
:- comment(lower_bound_lookup/4, [
	amode:		lower_bound_lookup(+, ++, ?, ?),
	args:		["Map":"A map",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for the smallest key no smaller than SearchKey.",
	fail_if:	"Fails if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			upper_bound_search/4,
			upper_bound_lookup/4],
	desc:		html("\
	<P>
	This predicate searches the map Map for the entry with the
	smallest key which is no smaller than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.  If such a key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred upper_bound_search(map(K,V), K, K, V).
% :- mode upper_bound_search(in, in, out, out) is semidet.
:- comment(upper_bound_search/4, [
	amode:		upper_bound_search(+, ++, ?, ?),
	args:		["Map":"A map",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for the largest key no larger than SearchKey.",
	fail_if:	"Fails if there are no keys at least as large as SearchKey in Map or if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			lower_bound_lookup/4,
			upper_bound_lookup/4],
	desc:		html("\
	<P>
	This predicate searches the map Map for the entry with the
	largest key which is no larger than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred upper_bound_lookup(map(K,V), K, K, V).
% :- mode upper_bound_lookup(in, in, out, out) is det.
:- comment(upper_bound_lookup/4, [
	amode:		upper_bound_lookup(+, ++, ?, ?),
	args:		["Map":"A map",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a map for the largest key no larger than SearchKey.",
	fail_if:	"Fails if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			lower_bound_lookup/4,
			upper_bound_search/4],
	desc:		html("\
	<P>
	This predicate searches the map Map for the entry with the
	largest key which is no larger than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.  If such a key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred inverse_search(map(K,V), V, K).
% :- mode inverse_search(in, in, out) is nondet.
:- comment(inverse_search/3, [
	amode:		inverse_search(+, ?, ?),
	args:		["Map":"A map",
			"Value":"A value to search for",
			"Key":"A key corresponding to Value"],
	summary:	"Search a map for a value.",
	fail_if:	"Fails if Value does not appear in Map or if Key does not unify with any corresponding keys found.",
	resat:		yes,
	see_also:	[search/3, member/3],
	desc:		html("\
	<P>
	This predicate searches the map Map for value entries which unify
	with Value.  If such a value is found, then it attempts to unify the
	corresponding key with Key.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred insert(map(K,V), K, V, map(K,V)).
% :- mode insert(in, in, in, out) is semidet.
:- comment(insert/4, [
	amode:		insert(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"A key to insert",
			"Value":"The value corresponding to Key",
			"Map":"The map after insertion"],
	summary:	"Insert a key/value pair into a map, failing if the key already exists.",
	fail_if:	"Fails if Key already appears in Map0.",
	resat:		no,
	see_also:	[det_insert/4, update/4, det_update/4, set/4],
	desc:		html("\
	<P>
	This predicate inserts the key Key with corresponding value Value
	into the map Map0, resulting in the map Map.  If the key Key is
	already in the map, then the predicate fails.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred det_insert(map(K,V), K, V, map(K,V)).
% :- mode det_insert(in, in, in, out) is det.
:- comment(det_insert/4, [
	amode:		det_insert(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"A key to insert",
			"Value":"The value corresponding to Key",
			"Map":"The map after insertion"],
	summary:	"Insert a key/value pair into a map, aborting if the key already exists.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[insert/4, update/4, det_update/4, set/4],
	desc:		html("\
	<P>
	This predicate inserts the key Key with corresponding value Value
	into the map Map0, resulting in the map Map.  If the key Key is
	already in the map, then the predicate aborts with a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred det_insert_from_corresponding_lists(map(K,V), list(K),
%						list(V), map(K,V)).
% :- mode det_insert_from_corresponding_lists(in, in, in, out) is det.
:- comment(det_insert_from_corresponding_lists/4, [
	amode:		det_insert_from_corresponding_lists(+, ++, ?, -),
	args:		["Map0":"A map",
			"KeyList":"A list of keys to insert",
			"ValueList":"A list of values corresponding to the keys in KeyList",
			"Map":"The map after insertion"],
	summary:	"Insert key/value pairs into a map, aborting if any of the keys already exist.",
	fail_if:	"Fails if the lists aren't the same length.",
	resat:		no,
	see_also:	[det_insert/4, det_insert_from_assoc_list/3, from_corresponding_lists/3],
	desc:		html("\
	<P>
	This predicate takes a map Map0, and for each key Key in KeyList
	and corresponding value Value from ValueList, calls
	det_insert/4 to insert the Key/Value pair into the map.
	The result after all the insertions is the map Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred det_insert_from_assoc_list(map(K,V), assoc_list(K, V),
%						map(K,V)).
% :- mode det_insert_from_assoc_list(in, in, out) is det.
:- comment(det_insert_from_assoc_list/3, [
	amode:		det_insert_from_assoc_list(+, +, -),
	args:		["Map0":"A map",
			"List":"A list of Key-Value pairs to insert",
			"Map":"The map after insertion"],
	summary:	"Insert key/value pairs into a map, aborting if any of the keys already exist.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[det_insert/4, det_insert_from_corresponding_lists/4, from_assoc_list/2, from_sorted_assoc_list/2],
	desc:		html("\
	<P>
	This predicate takes a map Map0, and for each entry in List (of
	the form Key-Value), calls det_insert/4 to insert the
	Key/Value pair into the map.  The result after all the insertions
	is the map Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred update(map(K,V), K, V, map(K,V)).
% :- mode update(in, in, in, out) is semidet.
:- comment(update/4, [
	amode:		update(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"A key to update",
			"Value":"The value corresponding to Key",
			"Map":"The map after updating"],
	summary:	"Update the value corresponding to a key in a map.",
	fail_if:	"Fails if Key does not appear in Map0.",
	resat:		no,
	see_also:	[det_update/4, insert/4, det_insert/4, set/4],
	desc:		html("\
	<P>
	If the key Key already exists in the map Map0, then this predicate
	updates the corresponding value to be Value.  The resulting map is
	Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred det_update(map(K,V), K, V, map(K,V)).
% :- mode det_update(in, in, in, out) is det.
:- comment(det_update/4, [
	amode:		det_update(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"A key to update",
			"Value":"The value corresponding to Key",
			"Map":"The map after updating"],
	summary:	"Update the value corresponding to a key in a map, aborting if it doesn't exist.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[update/4, insert/4, det_insert/4, set/4],
	desc:		html("\
	<P>
	If the key Key already exists in the map Map0, then this
	predicate updates the corresponding value to be Value, resulting
	in the map Map.  If the key Key is not already in the map,
	then the predicate aborts with a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred set(map(K,V), K, V, map(K,V)).
% :- mode set(di, di, di, uo) is det.
% :- mode set(in, in, in, out) is det.
:- comment(set/4, [
	amode:		set(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"A key to set",
			"Value":"The value corresponding to Key",
			"Map":"The map after setting"],
	summary:	"Update the value corresponding to a key in a map, inserting the key if it doesn't exist already.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[insert/4, det_insert/4, update/4, det_update/4],
	desc:		html("\
	<P>
	If the key Key already exists in the map Map0, then this predicate
	updates the corresponding value to be Value.  Otherwise it inserts
	the key Key into the map with value Value.  The resulting map is
	Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred keys(map(K, _V), list(K)).
% :- mode keys(in, out) is det.
:- comment(keys/2, [
	amode:		keys(+, -),
	args:		["Map":"A map",
			"KeyList":"A list of all the keys from Map"],
	summary:	"Return all the keys from a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[sorted_keys/2, values/2],
	desc:		html("\
	<P>
	KeyList is a list of all the keys appearing in the map Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred sorted_keys(map(K, _V), list(K)).
% :- mode sorted_keys(in, out) is det.
:- comment(sorted_keys/2, [
	amode:		sorted_keys(+, -),
	args:		["Map":"A map",
			"KeyList":"A list of all the keys from Map"],
	summary:	"Return all the keys from a map, in sorted order.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[keys/2, values/2],
	desc:		html("\
	<P>
	KeyList is a list of all the keys appearing in the map Map, in
	sorted order.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred values(map(_K, V), list(V)).
% :- mode values(in, out) is det.
:- comment(values/2, [
	amode:		values(+, -),
	args:		["Map":"A map",
			"ValueList":"A list of all the values from Map"],
	summary:	"Return all the values from a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[values/2],
	desc:		html("\
	<P>
	ValueList is a list of all the values appearing in the map Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred to_assoc_list(map(K,V), assoc_list(K,V)).
% :- mode to_assoc_list(in, out) is det.
:- comment(to_assoc_list/2, [
	amode:		to_assoc_list(+, -),
	args:		["Map":"A map",
			"AssocList":"A list of the key-value pairs from Map"],
	summary:	"Converts a map into an association list.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[to_sorted_assoc_list/2, from_assoc_list/2],
	desc:		html("\
	<P>
	AssocList is a list containing the key/value pairs from the map
	Map, in the form Key-Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred to_sorted_assoc_list(map(K,V), assoc_list(K,V)).
% :- mode to_sorted_assoc_list(in, out) is det.
:- comment(to_sorted_assoc_list/2, [
	amode:		to_sorted_assoc_list(+, -),
	args:		["Map":"A map",
			"AssocList":"A sorted list of the key-value pairs from Map"],
	summary:	"Converts a map into a (sorted) association list.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[to_assoc_list/2, from_sorted_assoc_list/2],
	desc:		html("\
	<P>
	AssocList is a sorted list containing the key/value pairs from the
	map Map, in the form Key-Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred from_assoc_list(assoc_list(K,V), map(K,V)).
% :- mode from_assoc_list(in, out) is det.
:- comment(from_assoc_list/2, [
	amode:		from_assoc_list(+, -),
	args:		["AssocList":"A list of key-value pairs",
			"Map":"A map"],
	summary:	"Converts an association list into a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[to_assoc_list/2, from_sorted_assoc_list/2],
	desc:		html("\
	<P>
	AssocList is a list of key/value pairs of the form Key-Value, 
	and Map is a map containing these key/value pairs.  If a key
	appears more than once in AssocList, then its corresponding value
	in the map Map will be the last one appearing in AssocList.  
	</P>
	")
]).

% :- pred from_sorted_assoc_list(assoc_list(K,V), map(K,V)).
% :- mode from_sorted_assoc_list(in, out) is det.
:- comment(from_sorted_assoc_list/2, [
	amode:		from_sorted_assoc_list(+, -),
	args:		["AssocList":"A sorted list of key-value pairs",
			"Map":"A map"],
	summary:	"Converts a sorted association list into a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[to_sorted_assoc_list/2, from_assoc_list/2],
	desc:		html("\
	<P>
	AssocList is a sorted list of key/value pairs of the form Key-Value,
	and Map is a map containing these key/value pairs.  If a key
	appears more than once in AssocList, then its corresponding value
	in the map Map will be the last one appearing in AssocList.
	</P>
	")
]).

% :- pred delete(map(K,V), K, map(K,V)).
% :- mode delete(di, in, uo) is det.
% :- mode delete(in, in, out) is det.
:- comment(delete/3, [
	amode:		delete(+, ++, -),
	args:		["Map0":"A map",
			"Key":"The key to delete",
			"Map":"The map after deletion"],
	summary:	"Delete a key/value pair from a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[delete_list/3, remove/4],
	desc:		html("\
	<P>
	If the key Key appears in the map Map0, then remove it and its
	corresponding value, resulting in the map Map.  If the key Key
	does not appear, Map is simply bound to Map0.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred delete_list(map(K,V), list(K), map(K,V)).
% :- mode delete_list(di, in, uo) is det.
% :- mode delete_list(in, in, out) is det.
:- comment(delete_list/3, [
	amode:		delete_list(+, ++, -),
	args:		["Map0":"A map",
			"KeyList":"A list of keys to delete",
			"Map":"The map after deletions"],
	summary:	"Delete a list of key/value pairs from a map.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[delete/3, remove/4],
	desc:		html("\
	<P>
	This predicate takes a map Map0, and for each key in KeyList,
	calls delete/3 to delete the key and its corresponding
	value.  The result after all the deletions is the map Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred remove(map(K,V), K, V, map(K,V)).
% :- mode remove(in, in, out, out) is semidet.
:- comment(remove/4, [
	amode:		remove(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"The key to remove",
			"Value":"The value corresponding to Key",
			"Map":"The map after removal"],
	summary:	"Remove a key/value pair from a map, failing if the key is not present.",
	fail_if:	"Fails is Key does not appear in Map0 or if Value does not unify with the corresponding value.",
	resat:		no,
	see_also:	[delete/3, det_remove/4, remove_smallest/4],
	desc:		html("\
	<P>
	If the key Key appears in the map Map0, then remove it and attempt
	to unify its corresponding value with Value.  Map is Map0 with the
	key removed.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred det_remove(map(K,V), K, V, map(K,V)).
% :- mode det_remove(in, in, out, out) is det.
:- comment(det_remove/4, [
	amode:		det_remove(+, ++, ?, -),
	args:		["Map0":"A map",
			"Key":"The key to remove",
			"Value":"The value corresponding to Key",
			"Map":"The map after removal"],
	summary:	"Remove a key/value pair from a map, aborting if the key is not present.",
	fail_if:	"Fails if Value does not unify with the value corresponding to Key.",
	resat:		no,
	see_also:	[delete/3, remove/4],
	desc:		html("\
	<P>
	This predicate removes the key Key and its corresponding value from
	the map Map0, resulting in the map Map.  It then attempts to
	unify the removed value with Value.  If the key Key was not in the
	map Map0, then the predicate aborts with a runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred count(map(K, V), int).
% :- mode count(in, out) is det.
:- comment(count/2, [
	amode:		count(+, ?),
	args:		["Map":"A map",
			"Count":"The number of elements in Map"],
	summary:	"Count the number of elements in a map.",
	fail_if:	"Fails if Count does not unify with the number of elements in Map.",
	resat:		no,
	desc:		html("\
	<P>
	Counts the number of elements in the map Map, and attempts to
	unify the result with Count.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred from_corresponding_lists(list(K), list(V), map(K, V)).
% :- mode from_corresponding_lists(in, in, out) is det.
:- comment(from_corresponding_lists/3, [
	amode:		from_corresponding_lists(++, ?, -),
	args:		["KeyList":"A list of keys",
			"ValueList":"A list of values corresponding to the keys in KeyList",
			"Map":"The created map"],
	summary:	"Converts a corresponding pair of lists into a map.",
	fail_if:	"Fails if the lists aren't the same length.",
	resat:		no,
	see_also:	[from_assoc_list/2, det_insert_from_corresponding_lists/4],
	desc:		html("\
	<P>
	Converts a list of keys KeyList and a corresponding list of values
	ValueList into a map Map.  If a key appears more than once in
	KeyList, then its corresponding value in the map Map will be
	the last one appearing in AssocList.
	</P>
	")
]).

% :- pred merge(map(K, V), map(K, V), map(K, V)).
% :- mode merge(in, in, out) is det.
:- comment(merge/3, [
	amode:		merge(+, +, -),
	args:		["MapA":"A map to merge",
			"MapB":"The other map to merge",
			"Map":"The merged map"],
	summary:	"Merges two maps into one.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[overlay/3],
	desc:		html("\
	<P>
	The map Map is the result of merging the map MapA and the map MapB;
	i.e. Map contains all the key/value pairs from both MapA and MapB.
	If MapA and MapB have a key in common, then it is not defined
	which corresponding value will end up associated with that key
	in Map.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred overlay(map(K,V), map(K,V), map(K,V)).
% :- mode overlay(in, in, out) is det.
:- comment(overlay/3, [
	amode:		overlay(+, +, -),
	args:		["MapA":"A map",
			"MapB":"The map to overlay",
			"Map":"The resulting map"],
	summary:	"Overlays one map over another.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[merge/3],
	desc:		html("\
	<P>
	The map Map contains a key/value pair for every key that appears in
	either the map MapA or the map MapB.  If a key Key appears in MapB,
	then its corresponding value in Map is that appearing in MapB;
	otherwise it is that appearing in MapA.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% Mercury version:
% :- pred select(map(K,V), set(K), map(K,V)).
% ECLiPSe version:
% :- pred select(map(K,V), list(K), map(K,V)).
% :- mode select(in, in, out) is det.
:- comment(select/3, [
	amode:		select(+, ++, -),
	args:		["Map0":"A map",
			"KeyList":"A list of keys to select",
			"Map":"The resulting map"],
	summary:	"Creates a new map containing just those entries corresponding to a given list of keys.",
	fail_if:	"Never fails.",
	resat:		no,
	desc:		html("\
	<P>
	The map Map contains the key/value pairs from the map Map0 where
	the key appears in the list KeyList.  Keys in KeyList which do not
	appear in Map0 are ignored.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred apply_to_list(list(K), map(K, V), list(V)).
% :- mode apply_to_list(in, in, out) is det.
:- comment(apply_to_list/3, [
	amode:		apply_to_list(++, +, ?),
	args:		["KeyList":"A list of keys to map",
			"Map":"The map to apply",
			"ValueList":"The list of corresponding values"],
	summary:	"Map a list of keys to their corresponding values.",
	fail_if:	"Fails if ValueList does not unify with the list of values corresponding to KeyList.",
	resat:		no,
	see_also:	[lookup/3],
	desc:		html("\
	<P>
	This predicate applies the map Map to a list of keys KeyList to
	produce the list of values ValueList; i.e. it maps a list of keys
	to their corresponding values.  If one of the keys in KeyList is
	not found in Map, then the predicate aborts witha runtime error.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred optimize(map(K, V), map(K, V)).
% :- mode optimize(in, out) is det.
:- comment(optimize/2, [
	amode:		optimize(+, -),
	args:		["Map0":"A map to optimize",
			"Map":"The optimized map"],
	summary:	"Optimize a map for many lookups but few/no modification.",
	fail_if:	"Never fails.",
	resat:		no,
	desc:		html("\
	<P>
	Declaratively, this predicate does nothing (i.e. the map Map is
	equivalent to the map Map0).  However, operationally it suggests to
	the implementation that the representation of the map be optimised
	for lookups, with few or no modifications to be expected.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

% :- pred remove_smallest(map(K, V), K, V, map(K, V)).
% :- mode remove_smallest(in, out, out, out) is semidet.
:- comment(remove_smallest/4, [
	amode:		remove_smallest(+, ?, ?, -),
	args:		["Map0":"A map",
			"Key":"The key removed",
			"Value":"The value corresponding to Key",
			"Map":"The map after removal"],
	summary:	"Remove the smallest key and its corresponding value from a map.",
	fail_if:	"Fails if Map0 is empty or if Key and Value do not unify with the key and value removed.",
	resat:		no,
	see_also:	[remove/4],
	desc:		html("\
	<P>
	Removes the smallest key in the map Map0 (resulting in the
	map Map), and attempts to unify the removed key with Key and
	its corresponding value with Value.
	</P>
	<P>
	This predicate should only be called with maps created by other
	predicates from the map module.
	</P>
	")
]).

%-----------------------------------------------------------------------------%

:- lib(m_tree234).
:- lib(mercury).

% :- type map(K,V)	==	tree234(K,V).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

init(M) :-
	m_tree234:init(M).

is_empty(M) :-
	m_tree234:is_empty(M).

contains(Map, K) :-
	search(Map, K, _).

member(Map, K, V) :-
	m_tree234:member(Map, K, V).

search(Map, K, V) :-
	m_tree234:search(Map, K, V).

lookup(Map, K, V) :-
	( m_tree234:search(Map, K, V1) ->
		V = V1
	;
		report_lookup_error("lookup: key not found", K, V)
	).

lower_bound_search(Map, SearchK, K, V) :-
	m_tree234:lower_bound_search(Map, SearchK, K, V).

lower_bound_lookup(Map, SearchK, K, V) :-
	( m_tree234:lower_bound_search(Map, SearchK, K1, V1) ->
		K = K1,
		V = V1
	;
		report_lookup_error("lower_bound_lookup: key not found",
			SearchK, V)
	).

upper_bound_search(Map, SearchK, K, V) :-
	m_tree234:upper_bound_search(Map, SearchK, K, V).

upper_bound_lookup(Map, SearchK, K, V) :-
	( m_tree234:upper_bound_search(Map, SearchK, K1, V1) ->
		K = K1,
		V = V1
	;
		report_lookup_error("upper_bound_lookup: key not found",
			SearchK, V)
	).

insert(Map0, K, V, Map) :-
	m_tree234:insert(Map0, K, V, Map).

det_insert(Map0, K, V, Map) :-
	( m_tree234:insert(Map0, K, V, Map1) ->
		Map = Map1
	;
		report_lookup_error("det_insert: key already present",
			K, V)
	).

det_insert_from_corresponding_lists(Map0, Ks, Vs, Map) :-
	(
		foreach(K, Ks), foreach(V, Vs),
		fromto(Map0, Map_In, Map_Out, Map)
	do
		det_insert(Map_In, K, V, Map_Out)
	).

det_insert_from_assoc_list(Map, [], Map).
det_insert_from_assoc_list(Map0, [K - V | KVs], Map) :-
	det_insert(Map0, K, V, Map1),
	det_insert_from_assoc_list(Map1, KVs, Map).

update(Map0, K, V, Map) :-
	m_tree234:update(Map0, K, V, Map).

det_update(Map0, K, V, Map) :-
	( m_tree234:update(Map0, K, V, Map1) ->
		Map = Map1
	;
		report_lookup_error("det_update: key not found", K, V)
	).

set(Map0, K, V, Map) :-
	m_tree234:set(Map0, K, V, Map).

keys(Map, KeyList) :-
	m_tree234:keys(Map, KeyList).

sorted_keys(Map, KeyList) :-
	% Guaranteed to yield sorted lists.
	m_tree234:keys(Map, KeyList).

values(Map, KeyList) :-
	m_tree234:values(Map, KeyList).

to_assoc_list(M, L) :-
	m_tree234:tree234_to_assoc_list(M, L).

to_sorted_assoc_list(M, L) :-
	% Guaranteed to yield sorted lists.
	m_tree234:tree234_to_assoc_list(M, L).

from_assoc_list(L, M) :-
	m_tree234:assoc_list_to_tree234(L, M).

from_sorted_assoc_list(L, M) :-
	m_tree234:assoc_list_to_tree234(L, M).

delete(Map0, Key, Map) :-
	m_tree234:delete(Map0, Key, Map).

delete_list(Map, [], Map).
delete_list(Map0, [Key | Keys], Map) :-
	delete(Map0, Key, Map1),
	delete_list(Map1, Keys, Map).

remove(Map0, Key, Value, Map) :-
	m_tree234:remove(Map0, Key, Value, Map).

det_remove(Map0, Key, Value, Map) :-
	( m_tree234:remove(Map0, Key, Value1, Map1) ->
		Value = Value1,
		Map = Map1
	;
		report_lookup_error("det_remove: key not found",
			Key, Value)
	).

count(Map, Count) :-
	m_tree234:count(Map, Count).

%-----------------------------------------------------------------------------%

	% XXX inefficient

inverse_search(Map, V, K) :-
	m_tree234:tree234_to_assoc_list(Map, AssocList),
	assoc_list_member(K, V, AssocList).

%-----------------------------------------------------------------------------%

	% The code here is deliberately written using very simple
	% modes.
	% The reason we don't just use member/2 is that we want to
	% bootstrap this thing ASAP.

% :- pred assoc_list_member(K, V, list(pair(K,V))).
% :- mode assoc_list_member(in, out, in) is nondet.
% :- mode assoc_list_member(out, in, in) is nondet.
% :- mode assoc_list_member(in, in, in) is semidet.
assoc_list_member(K, V, [K - V | _]).
assoc_list_member(K, V, [_ | Xs]) :-
	assoc_list_member(K, V, Xs).

%-----------------------------------------------------------------------------%

from_corresponding_lists(Keys, Values, Map) :-
	%assoc_list__from_corresponding_lists(Keys, Values, AssocList),
	(
		foreach(K, Keys), foreach(V, Values), foreach(KV, AssocList)
	do
		KV = K - V
	),
	m_tree234:assoc_list_to_tree234(AssocList, Map).

%-----------------------------------------------------------------------------%

merge(M0, M1, M) :-
	to_assoc_list(M0, ML0),
	to_assoc_list(M1, ML1),
	%list__merge(ML0, ML1, ML),
	eclipse_language:merge(ML0, ML1, ML),
	from_sorted_assoc_list(ML, M).

%-----------------------------------------------------------------------------%

optimize(Map, Map).

%-----------------------------------------------------------------------------%

overlay(Map0, Map1, Map) :-
	to_assoc_list(Map1, AssocList),
	overlay_2(AssocList, Map0, Map).

% :- pred overlay_2(assoc_list(K,V), map(K,V), map(K,V)).
% :- mode overlay_2(in, in, out) is det.

overlay_2([], Map, Map).
overlay_2([K - V | AssocList], Map0, Map) :-
	set(Map0, K, V, Map1),
	overlay_2(AssocList, Map1, Map).

%-----------------------------------------------------------------------------%

select(Original, KeyList, NewMap) :-
	init(NewMap0),
	select_2(KeyList, Original, NewMap0, NewMap).

% :- pred select_2(list(K), map(K,V), map(K,V), map(K,V)).
% :- mode select_2(in, in, in, out) is det.

select_2([], _Original, New, New).
select_2([K|Ks], Original, New0, New) :-
	(
		search(Original, K, V)
	->
		set(New0, K, V, New1)
	;
		New1 = New0
	),
	select_2(Ks, Original, New1, New).

%-----------------------------------------------------------------------------%

apply_to_list([], _, []).
apply_to_list([K | Ks], Map, [V | Vs]) :-
	lookup(Map, K, V),
	apply_to_list(Ks, Map, Vs).

%-----------------------------------------------------------------------------%

remove_smallest(Map0, K, V, Map) :-
	m_tree234:remove_smallest(Map0, K, V, Map).

