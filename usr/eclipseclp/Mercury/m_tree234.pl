%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999-2000 The University of Melbourne.
% Copyright (C) 2000 Imperial College London.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB.
%---------------------------------------------------------------------------%

% m_tree234 - implements a map (dictionary) using 2-3-4 trees.
% main author: conway@cs.mu.OZ.AU.
% stability: medium.

% Modified for ECLiPSe by Warwick Harvey <wh@icparc.ic.ac.uk>, April 2000,
% based on revision 1.28 of file mercury/library/tree234.m from the
% Mercury CVS repository.  See http://www.cs.mu.oz.au/mercury for
% information about obtaining Mercury.

% The conversion included stripping out the functional versions of
% predicates and the higher-order predicates.  It also included providing
% user-level documentation.

% Note that the elimination of choicepoints in the ECLiPSe version was done
% semi-automatically; hence the names of the introduced predicates are not
% particularly imaginitive.

% This module assumes that keys are ground (so they can't be modified after
% insertion into the tree), but allows the values stored to be variables.

%---------------------------------------------------------------------------%

:- module(m_tree234).

:- export
	init/1,
	is_empty/1,
	member/3,
	search/3,
	lookup/3,
	lower_bound_search/4,
	lower_bound_lookup/4,
	upper_bound_search/4,
	upper_bound_lookup/4,
	insert/4,
	set/4,
	delete/3,
	remove/4,
	remove_smallest/4,
	keys/2,
	values/2,
	update/4,
	count/2,
	assoc_list_to_tree234/2,
	tree234_to_assoc_list/2.

:- comment(categories, ["Data Structures"]).
:- comment(summary, "A map (dictionary) implemented using 2-3-4 trees").
:- comment(author, "Thomas Conway (Mercury) and Warwick Harvey (ECLiPSe)").
:- comment(desc, html("\
	<P>
	This module implements 2-3-4 trees.  They can be used to manage and
	index a collection of key/value pairs.  You probably want to use the
	interface provided by `map.pl' instead of using this module
	directly.
	</P>
	<P>
	Note that keys must be ground, but values are allowed to be
	variables.
	</P>
	")).

% :- pred init(tree234(K, V)).
% :- mode init(uo) is det.
:- comment(init/1, [
	amode:		init(-),
	args:		["Tree":"The new tree"],
	summary:	"Initialise a new (empty) 2-3-4 tree.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[is_empty/1]
]).

% :- pred is_empty(tree234(K, V)).
% :- mode is_empty(in) is semidet.
:- comment(is_empty/1, [
	amode:          is_empty(+), 
	args:           ["Tree":"A 2-3-4 tree"],
	summary:        "Check whether a tree is empty.",
	fail_if:        "Fails if Tree is not an empty tree.",
	resat:          no,
	see_also:       [init/1]
]).

% :- pred member(tree234(K, V), K, V).
% :- mode member(in, out, out) is nondet.
:- comment(member/3, [
	amode:		member(+, ?, ?),
	args:		["Tree":"A 2-3-4 tree",
			"Key":"A key from Tree",
			"Value":"The value in Tree corresponding to Key"],
	summary:	"Succeeds if Key and Value unify with a key/value pair from Tree.",
	fail_if:	"Fails if Key and Value do not unify with a key/value pair from Tree.",
	resat:		yes,
	see_also:	[search/3, lookup/3],
	desc:		html("\
	<P>
	Tries to unify Key and Value with key/value pairs from the tree Tree.
	</P>
	<P>
	If Key and Value are variables and Tree is a 2-3-4 tree, then all
	members of the tree Tree are found on backtracking.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred search(tree234(K, V), K, V).
% :- mode search(in, in, out) is semidet.
:- comment(search/3, [
	amode:		search(+, ++, ?),
	args:		["Tree":"A 2-3-4 tree",
			"Key":"A key to search for",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for a key.",
	fail_if:	"Fails if Key does not appear in Tree or if Value does not unify with the corresponding value found.",
	resat:		no,
	see_also:	[member/3, lookup/3],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for an entry with key Key.
	If the key is found, then it attempts to unify the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred lookup(tree234(K, V), K, V).
% :- mode lookup(in, in, out) is det.
:- comment(lookup/3, [
	amode:		lookup(+, ++, ?),
	args:		["Tree":"A 2-3-4 tree",
			"Key":"A key to search for",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for a key.",
	fail_if:	"Fails if Value does not unify with the value corresponding to Key.",
	resat:		no,
	see_also:	[member/3, search/3],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for an entry with key Key.
	If the key is found, then it attempts to unify the corresponding
	value with Value.  If the key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred lower_bound_search(tree234(K, V), K, K, V).
% :- mode lower_bound_search(in, in, out, out) is semidet.
:- comment(lower_bound_search/4, [
	amode:		lower_bound_search(+, ++, ?, ?),
	args:		["Tree":"A 2-3-4 tree",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for the smallest key no smaller than SearchKey.",
	fail_if:	"Fails if there are no keys at least as large as SearchKey in Tree or if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_lookup/4,
			upper_bound_search/4,
			upper_bound_lookup/4],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for the entry with the
	smallest key which is no smaller than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred lower_bound_lookup(tree234(K, V), K, K, V).
% :- mode lower_bound_lookup(in, in, out, out) is det.
:- comment(lower_bound_lookup/4, [
	amode:		lower_bound_lookup(+, ++, ?, ?),
	args:		["Tree":"A 2-3-4 tree",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for the smallest key no smaller than SearchKey.",
	fail_if:	"Fails if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			upper_bound_search/4,
			upper_bound_lookup/4],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for the entry with the
	smallest key which is no smaller than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.  If such a key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred upper_bound_search(tree234(K, V), K, K, V).
% :- mode upper_bound_search(in, in, out, out) is semidet.
:- comment(upper_bound_search/4, [
	amode:		upper_bound_search(+, ++, ?, ?),
	args:		["Tree":"A 2-3-4 tree",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for the largest key no larger than SearchKey.",
	fail_if:	"Fails if there are no keys at least as large as SearchKey in Tree or if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			lower_bound_lookup/4,
			upper_bound_lookup/4],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for the entry with the
	largest key which is no larger than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred upper_bound_lookup(tree234(K, V), K, K, V).
% :- mode upper_bound_lookup(in, in, out, out) is det.
:- comment(upper_bound_lookup/4, [
	amode:		upper_bound_lookup(+, ++, ?, ?),
	args:		["Tree":"A 2-3-4 tree",
			"SearchKey":"A key to search for",
			"Key":"The key found",
			"Value":"The value corresponding to Key"],
	summary:	"Search a tree for the largest key no larger than SearchKey.",
	fail_if:	"Fails if Key and Value do not unify with the key and value found.",
	resat:		no,
	see_also:	[lower_bound_search/4,
			lower_bound_lookup/4,
			upper_bound_search/4],
	desc:           html("\
	<P>
	This predicate searches the tree Tree for the entry with the
	largest key which is no larger than SearchKey.  If such a key is
	found, then it attempts to unify it with Key and the corresponding
	value with Value.  If such a key is not found, then it aborts with
	a runtime error.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred insert(tree234(K, V), K, V, tree234(K, V)).
% :- mode insert(in, in, in, out) is semidet.
% % :- mode insert(di_tree234, in, in, uo_tree234) is semidet.
% % :- mode insert(in, in, in, out) is semidet.
:- comment(insert/4, [
	amode:		insert(+, ++, ?, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"A key to insert",
			"Value":"The value corresponding to Key",
			"Tree":"The tree after insertion"],
	summary:	"Insert a key/value pair into a tree, failing if the key already exists.",
	fail_if:	"Fails if Key already appears in Tree0.",
	resat:		no,
	see_also:	[update/4, set/4],
	desc:           html("\
	<P>
	This predicate inserts the key Key with corresponding value Value
	into the tree Tree0, resulting in the tree Tree.  If the key Key is
	already in the tree, then the predicate fails.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred set(tree234(K, V), K, V, tree234(K, V)).
% :- mode set(di, di, di, uo) is det.
% % :- mode set(di_tree234, in, in, uo_tree234) is det.
% :- mode set(in, in, in, out) is det.
:- comment(set/4, [
	amode:		set(+, ++, ?, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"A key to set",
			"Value":"The value corresponding to Key",
			"Tree":"The tree after setting"],
	summary:	"Update the value corresponding to a key in a tree, inserting the key if it doesn't exist already.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[insert/4, update/4],
	desc:           html("\
	<P>
	If the key Key already exists in the tree Tree0, then this predicate
	updates the corresponding value to be Value.  Otherwise it inserts
	the key Key into the tree with value Value.  The resulting tree is
	Tree.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred delete(tree234(K, V), K, tree234(K, V)).
% :- mode delete(di, in, uo) is det.
% % :- mode delete(di_tree234, in, uo_tree234) is det.
% :- mode delete(in, in, out) is det.
:- comment(delete/3, [
	amode:		delete(+, ++, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"The key to delete",
			"Tree":"The tree after deletion"],
	summary:	"Delete a key/value pair from a tree.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[remove/4],
	desc:           html("\
	<P>
	If the key Key appears in the tree Tree0, then remove it and its
	corresponding value, resulting in the tree Tree.  If the key Key
	does not appear, Tree is simply bound to Tree0.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred remove(tree234(K, V), K, V, tree234(K, V)).
% :- mode remove(di, in, uo, uo) is semidet.
% % :- mode remove(di_tree234, in, out, uo_tree234) is semidet.
% :- mode remove(in, in, out, out) is semidet.
:- comment(remove/4, [
	amode:		remove(+, ++, ?, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"The key to remove",
			"Value":"The value corresponding to Key",
			"Tree":"The tree after removal"],
	summary:	"Remove a key/value pair from a tree, failing if the key is not present.",
	fail_if:	"Fails is Key does not appear in Tree0 or if Value does not unify with the corresponding value.",
	resat:		no,
	see_also:	[delete/3, remove_smallest/4],
	desc:           html("\
	<P>
	If the key Key appears in the tree Tree0, then remove it and attempt
	to unify its corresponding value with Value.  Tree is Tree0 with the
	key removed.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred remove_smallest(tree234(K, V), K, V, tree234(K, V)).
% :- mode remove_smallest(di, uo, uo, uo) is semidet.
% % :- mode remove_smallest(di_tree234, out, out, uo_tree234)
% %	is semidet.
% :- mode remove_smallest(in, out, out, out) is semidet.
:- comment(remove_smallest/4, [
	amode:		remove_smallest(+, ?, ?, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"The key removed",
			"Value":"The value corresponding to Key",
			"Tree":"The tree after removal"],
	summary:	"Remove the smallest key and its corresponding value from a tree.",
	fail_if:	"Fails if Tree0 is empty or if Key and Value do not unify with the key and value removed.",
	resat:		no,
	see_also:	[remove/4],
	desc:           html("\
	<P>
	Removes the smallest key in the tree Tree0 (resulting in the
	tree Tree), and attempts to unify the removed key with Key and 
	its corresponding value with Value.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred keys(tree234(K, V), list(K)).
% :- mode keys(in, out) is det.
:- comment(keys/2, [
	amode:		keys(+, -),
	args:		["Tree":"A 2-3-4 tree",
			"KeyList":"A list of all the keys from Tree"],
	summary:	"Return all the keys from a tree.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[values/2],
	desc:           html("\
	<P>
	KeyList is a list of all the keys appearing in the tree Tree.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred values(tree234(K, V), list(V)).
% :- mode values(in, out) is det.
:- comment(values/2, [
	amode:		values(+, -),
	args:		["Tree":"A 2-3-4 tree",
			"ValueList":"A list of all the values from Tree"],
	summary:	"Return all the values from a tree.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[values/2],
	desc:           html("\
	<P>
	ValueList is a list of all the values appearing in the tree Tree.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred update(tree234(K, V), K, V, tree234(K, V)).
% :- mode update(in, in, in, out) is semidet.
% % :- mode update(di_tree234, in, in, uo_tree234) is det.
% % :- mode update(di, di, di, uo) is semidet.
:- comment(update/4, [
	amode:		update(+, ++, ?, -),
	args:		["Tree0":"A 2-3-4 tree",
			"Key":"A key to update",
			"Value":"The value corresponding to Key",
			"Tree":"The tree after updating"],
	summary:	"Update the value corresponding to a key in a tree.",
	fail_if:	"Fails if Key does not appear in Tree0.",
	resat:		no,
	see_also:	[insert/4, set/4],
	desc:           html("\
	<P>
	If the key Key already exists in the tree Tree0, then this predicate
	updates the corresponding value to be Value.  The resulting tree is
	Tree.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% 	% count the number of elements in a tree
% :- pred count(tree234(K, V), int).
% :- mode count(in, out) is det.
:- comment(count/2, [
	amode:		count(+, ?),
	args:		["Tree":"A 2-3-4 tree",
			"Count":"The number of elements in Tree"],
	summary:	"Count the number of elements in a tree.",
	fail_if:	"Fails if Count does not unify with the number of elements in Tree.",
	resat:		no,
	desc:           html("\
	<P>
	Counts the number of elements in the tree Tree, and attempts to
	unify the result with Count.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).

% :- pred assoc_list_to_tree234(assoc_list(K, V), tree234(K, V)).
% :- mode assoc_list_to_tree234(in, out) is det.
:- comment(assoc_list_to_tree234/2, [
	amode:		assoc_list_to_tree234(+, -),
	args:		["AssocList":"A list of key-value pairs",
			"Tree":"A 2-3-4 tree"],
	summary:	"Converts an association list into a tree.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[tree234_to_assoc_list/2],
	desc:           html("\
	<P>
	AssocList is a list of key/value pairs of the form Key-Value,
	and Tree is a tree containing these key/value pairs.  If a key
	appears more than once in AssocList, then its corresponding value
	in the tree Tree will be the last one appearing in AssocList.
	</P>
	")
]).

% :- pred tree234_to_assoc_list(tree234(K, V), assoc_list(K, V)).
% :- mode tree234_to_assoc_list(in, out) is det.
:- comment(tree234_to_assoc_list/2, [
	amode:		tree234_to_assoc_list(+, -),
	args:		["Tree":"A 2-3-4 tree",
			"AssocList":"A list of the key-value pairs from Tree"],
	summary:	"Converts a tree into an association list.",
	fail_if:	"Never fails.",
	resat:		no,
	see_also:	[assoc_list_to_tree234/2],
	desc:           html("\
	<P>
	AssocList is a list containing the key/value pairs from the tree
	Tree, in the form Key-Value.
	</P>
	<P>
	This predicate should only be called with trees created by other
	predicates from the tree234 module.
	</P>
	")
]).


%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

% :- import_module int, require, bool, std_util.

% :- type tree234(K, V)	--->
% 		empty
% 	;	two(K, V, tree234(K, V), tree234(K, V))
% 	;	three(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V))
% 	;	four(K, V, K, V, K, V, tree234(K, V), tree234(K, V),
% 			tree234(K, V), tree234(K, V)).


:- lib(mercury).

%------------------------------------------------------------------------------%

init(empty).

is_empty(Tree) :-
	Tree = empty.

%------------------------------------------------------------------------------%

member(empty, _K, _V) :- fail.
member(two(K0, V0, T0, T1), K, V) :-
	(
		K = K0,
		V = V0
	;
		member(T0, K, V)
	;
		member(T1, K, V)
	).
member(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
	(
		K = K0,
		V = V0
	;
		K = K1,
		V = V1
	;
		member(T0, K, V)
	;
		member(T1, K, V)
	;
		member(T2, K, V)
	).
member(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
	(
		K = K0,
		V = V0
	;
		K = K1,
		V = V1
	;
		K = K2,
		V = V2
	;
		member(T0, K, V)
	;
		member(T1, K, V)
	;
		member(T2, K, V)
	;
		member(T3, K, V)
	).

%------------------------------------------------------------------------------%

search(T, _K, _V) :-
	T = empty,
	fail.
search(T, K, V) :-
	T = two(K0, _, _, _),
	compare(Result, K, K0),
	search_aux4(Result, V, K, T).
search(T, K, V) :-
	T = three(K0, _, _, _, _, _, _),
	compare(Result0, K, K0),
	search_aux5(Result0, V, K, T).
search(T, K, V) :-
	T = four(_, _, K1, _, _, _, _, _, _, _),
	compare(Result1, K, K1),
	search_aux7(Result1, V, K, T).

search_aux4(Result, V, K, T) :-
	Result = (<),
	T = two(_, _, T0, _),
	search(T0, K, V).
search_aux4(Result, V, _K, T) :-
	Result = (=),
	T = two(_, V0, _, _),
	V = V0.
search_aux4(Result, V, K, T) :-
	Result = (>),
	T = two(_, _, _, T1),
	search(T1, K, V).

search_aux5(Result0, V, K, T) :-
	Result0 = (<),
	T = three(_, _, _, _, T0, _, _),
	search(T0, K, V).
search_aux5(Result0, V, _K, T) :-
	Result0 = (=),
	T = three(_, V0, _, _, _, _, _),
	V = V0.
search_aux5(Result0, V, K, T) :-
	Result0 = (>),
	T = three(_, _, K1, _, _, _, _),
	compare(Result1, K, K1),
	search_aux5_aux6(Result1, T, K, V).

search_aux5_aux6(Result1, T, K, V) :-
	Result1 = (<),
	T = three(_, _, _, _, _, T1, _),
	search(T1, K, V).
search_aux5_aux6(Result1, T, _K, V) :-
	Result1 = (=),
	T = three(_, _, _, V1, _, _, _),
	V = V1.
search_aux5_aux6(Result1, T, K, V) :-
	Result1 = (>),
	T = three(_, _, _, _, _, _, T2),
	search(T2, K, V).

search_aux7(Result1, V, K, T) :-
	Result1 = (<),
	T = four(K0, _, _, _, _, _, _, _, _, _),
	compare(Result0, K, K0),
	search_aux7_aux8(Result0, T, K, V).
search_aux7(Result1, V, _K, T) :-
	Result1 = (=),
	T = four(_, _, _, V1, _, _, _, _, _, _),
	V = V1.
search_aux7(Result1, V, K, T) :-
	Result1 = (>),
	T = four(_, _, _, _, K2, _, _, _, _, _),
	compare(Result2, K, K2),
	search_aux7_aux9(Result2, T, K, V).

search_aux7_aux8(Result0, T, K, V) :-
	Result0 = (<),
	T = four(_, _, _, _, _, _, T0, _, _, _),
	search(T0, K, V).
search_aux7_aux8(Result0, T, _K, V) :-
	Result0 = (=),
	T = four(_, V0, _, _, _, _, _, _, _, _),
	V = V0.
search_aux7_aux8(Result0, T, K, V) :-
	Result0 = (>),
	T = four(_, _, _, _, _, _, _, T1, _, _),
	search(T1, K, V).

search_aux7_aux9(Result2, T, K, V) :-
	Result2 = (<),
	T = four(_, _, _, _, _, _, _, _, T2, _),
	search(T2, K, V).
search_aux7_aux9(Result2, T, _K, V) :-
	Result2 = (=),
	T = four(_, _, _, _, _, V2, _, _, _, _),
	V = V2.
search_aux7_aux9(Result2, T, K, V) :-
	Result2 = (>),
	T = four(_, _, _, _, _, _, _, _, _, T3),
	search(T3, K, V).

lookup(T, K, V) :-
	( search(T, K, V0) ->
		V = V0
	;
		report_lookup_error("lookup: key not found.", K, V)
	).

%------------------------------------------------------------------------------%

lower_bound_search(T, _SearchK, _K, _V) :-
	T = empty,
	fail.
lower_bound_search(T, SearchK, K, V) :-
	T = two(K0, _, _, _),
	compare(Result, SearchK, K0),
	lower_bound_search_aux10(Result, K0, V, K, SearchK, T).
lower_bound_search(T, SearchK, K, V) :-
	T = three(K0, _, _, _, _, _, _),
	compare(Result0, SearchK, K0),
	lower_bound_search_aux11(Result0, K0, V, K, SearchK, T).
lower_bound_search(T, SearchK, K, V) :-
	T = four(_, _, K1, _, _, _, _, _, _, _),
	compare(Result1, SearchK, K1),
	lower_bound_search_aux13(Result1, K1, V, K, SearchK, T).

lower_bound_search_aux10(Result, _K0, V, K, SearchK, T) :-
	Result = (<),
	T = two(_, _, T0, _),
	lower_bound_search(T0, SearchK, K, V).
lower_bound_search_aux10(Result, _K0, V, K, SearchK, T) :-
	Result = (=),
	T = two(_, V0, _, _),
	K = SearchK,
	V = V0.
lower_bound_search_aux10(Result, K0, V, K, SearchK, T) :-
	Result = (>),
	T = two(_, _, _, T1),
	(
		lower_bound_search(T1, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = two(_, V0, _, _),
		K = K0,
		V = V0
	).

lower_bound_search_aux11(Result0, _K0, V, K, SearchK, T) :-
	Result0 = (=),
	T = three(_, V0, _, _, _, _, _),
	K = SearchK,
	V = V0.
lower_bound_search_aux11(Result0, K0, V, K, SearchK, T) :-
	Result0 = (>),
	T = three(_, _, K1, _, _, _, _),
	compare(Result1, SearchK, K1),
	lower_bound_search_aux11_aux12(Result1, K1, T, SearchK, K, V, K0).

lower_bound_search_aux11_aux12(Result1, _K1, T, SearchK, K, V, K0) :-
	Result1 = (<),
	T = three(_, _, _, _, _, T1, _),
	(
		lower_bound_search(T1, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = three(_, V0, _, _, _, _, _),
		K = K0,
		V = V0
	).
lower_bound_search_aux11_aux12(Result1, _K1, T, SearchK, K, V, _K0) :-
	Result1 = (=),
	T = three(_, _, _, V1, _, _, _),
	K = SearchK,
	V = V1.
lower_bound_search_aux11_aux12(Result1, K1, T, SearchK, K, V, _K0) :-
	Result1 = (>),
	T = three(_, _, _, _, _, _, T2),
	(
		lower_bound_search(T2, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = three(_, _, _, V1, _, _, _),
		K = K1,
		V = V1
	).

lower_bound_search_aux13(Result1, _K1, V, K, SearchK, T) :-
	Result1 = (<),
	T = four(K0, _, _, _, _, _, _, _, _, _),
	compare(Result0, SearchK, K0),
	lower_bound_search_aux13_aux14(Result0, K0, T, SearchK, K, V).
lower_bound_search_aux13(Result1, _K1, V, K, SearchK, T) :-
	Result1 = (=),
	T = four(_, _, _, V1, _, _, _, _, _, _),
	K = SearchK,
	V = V1.
lower_bound_search_aux13(Result1, K1, V, K, SearchK, T) :-
	Result1 = (>),
	T = four(_, _, _, _, K2, _, _, _, _, _),
	compare(Result2, SearchK, K2),
	lower_bound_search_aux13_aux15(Result2, K2, T, SearchK, K, V, K1).

lower_bound_search_aux13_aux14(Result0, _K0, T, SearchK, K, V) :-
	Result0 = (<),
	T = four(_, _, _, _, _, _, T0, _, _, _),
	lower_bound_search(T0, SearchK, K, V).
lower_bound_search_aux13_aux14(Result0, _K0, T, SearchK, K, V) :-
	Result0 = (=),
	T = four(_, V0, _, _, _, _, _, _, _, _),
	K = SearchK,
	V = V0.
lower_bound_search_aux13_aux14(Result0, K0, T, SearchK, K, V) :-
	Result0 = (>),
	T = four(_, _, _, _, _, _, _, T1, _, _),
	(
		lower_bound_search(T1, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, V0, _, _, _, _, _, _, _, _),
		K = K0,
		V = V0
	).

lower_bound_search_aux13_aux15(Result2, _K2, T, SearchK, K, V, K1) :-
	Result2 = (<),
	T = four(_, _, _, _, _, _, _, _, T2, _),
	(
		lower_bound_search(T2, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, _, _, V1, _, _, _, _, _, _),
		K = K1,
		V = V1
	).
lower_bound_search_aux13_aux15(Result2, _K2, T, SearchK, K, V, _K1) :-
	Result2 = (=),
	T = four(_, _, _, _, _, V2, _, _, _, _),
	K = SearchK,
	V = V2.
lower_bound_search_aux13_aux15(Result2, K2, T, SearchK, K, V, _K1) :-
	Result2 = (>),
	T = four(_, _, _, _, _, _, _, _, _, T3),
	(
		lower_bound_search(T3, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, _, _, _, _, V2, _, _, _, _),
		K = K2,
		V = V2
	).


lower_bound_lookup(T, SearchK, K, V) :-
	( lower_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("lower_bound_lookup: key not found.",
			SearchK, V)
	).

%------------------------------------------------------------------------------%

upper_bound_search(T, _SearchK, _K, _V) :-
	T = empty,
	fail.
upper_bound_search(T, SearchK, K, V) :-
	T = two(K0, _, _, _),
	compare(Result, SearchK, K0),
	upper_bound_search_aux16(Result, K0, V, K, SearchK, T).
upper_bound_search(T, SearchK, K, V) :-
	T = three(K0, _, _, _, _, _, _),
	compare(Result0, SearchK, K0),
	upper_bound_search_aux17(Result0, K0, V, K, SearchK, T).
upper_bound_search(T, SearchK, K, V) :-
	T = four(_, _, K1, _, _, _, _, _, _, _),
	compare(Result1, SearchK, K1),
	upper_bound_search_aux19(Result1, K1, V, K, SearchK, T).

upper_bound_search_aux16(Result, K0, V, K, SearchK, T) :-
	Result = (<),
	T = two(_, _, T0, _),
	(
		upper_bound_search(T0, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = two(_, V0, _, _),
		K = K0,
		V = V0
	).
upper_bound_search_aux16(Result, _K0, V, K, SearchK, T) :-
	Result = (=),
	T = two(_, V0, _, _),
	K = SearchK,
	V = V0.
upper_bound_search_aux16(Result, _K0, V, K, SearchK, T) :-
	Result = (>),
	T = two(_, _, _, T1),
	upper_bound_search(T1, SearchK, K, V).

upper_bound_search_aux17(Result0, K0, V, K, SearchK, T) :-
	Result0 = (<),
	T = three(_, _, _, _, T0, _, _),
	(
		upper_bound_search(T0, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = three(_, V0, _, _, _, _, _),
		K = K0,
		V = V0
	).
upper_bound_search_aux17(Result0, _K0, V, K, SearchK, T) :-
	Result0 = (=),
	T = three(_, V0, _, _, _, _, _),
	K = SearchK,
	V = V0.
upper_bound_search_aux17(Result0, _K0, V, K, SearchK, T) :-
	Result0 = (>),
	T = three(_, _, K1, _, _, _, _),
	compare(Result1, SearchK, K1),
	upper_bound_search_aux17_aux18(Result1, K1, T, SearchK, K, V).

upper_bound_search_aux17_aux18(Result1, K1, T, SearchK, K, V) :-
	Result1 = (<),
	T = three(_, _, _, _, _, T1, _),
	(
		upper_bound_search(T1, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = three(_, _, _, V1, _, _, _),
		K = K1,
		V = V1
	).
upper_bound_search_aux17_aux18(Result1, _K1, T, SearchK, K, V) :-
	Result1 = (=),
	T = three(_, _, _, V1, _, _, _),
	K = SearchK,
	V = V1.
upper_bound_search_aux17_aux18(Result1, _K1, T, SearchK, K, V) :-
	Result1 = (>),
	T = three(_, _, _, _, _, _, T2),
	upper_bound_search(T2, SearchK, K, V).

upper_bound_search_aux19(Result1, K1, V, K, SearchK, T) :-
	Result1 = (<),
	T = four(K0, _, _, _, _, _, _, _, _, _),
	compare(Result0, SearchK, K0),
	upper_bound_search_aux19_aux20(Result0, K0, T, SearchK, K, V, K1).
upper_bound_search_aux19(Result1, _K1, V, K, SearchK, T) :-
	Result1 = (=),
	T = four(_, _, _, V1, _, _, _, _, _, _),
	K = SearchK,
	V = V1.
upper_bound_search_aux19(Result1, _K1, V, K, SearchK, T) :-
	Result1 = (>),
	T = four(_, _, _, _, K2, _, _, _, _, _),
	compare(Result2, SearchK, K2),
	upper_bound_search_aux19_aux21(Result2, K2, T, SearchK, K, V).

upper_bound_search_aux19_aux21(Result2, K2, T, SearchK, K, V) :-
	Result2 = (<),
	T = four(_, _, _, _, _, _, _, _, T2, _),
	(
		upper_bound_search(T2, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, _, _, _, _, V2, _, _, _, _),
		K = K2,
		V = V2
	).
upper_bound_search_aux19_aux21(Result2, _K2, T, SearchK, K, V) :-
	Result2 = (=),
	T = four(_, _, _, _, _, V2, _, _, _, _),
	K = SearchK,
	V = V2.
upper_bound_search_aux19_aux21(Result2, _K2, T, SearchK, K, V) :-
	Result2 = (>),
	T = four(_, _, _, _, _, _, _, _, _, T3),
	upper_bound_search(T3, SearchK, K, V).

upper_bound_search_aux19_aux20(Result0, K0, T, SearchK, K, V, _K1) :-
	Result0 = (<),
	T = four(_, _, _, _, _, _, T0, _, _, _),
	(
		upper_bound_search(T0, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, V0, _, _, _, _, _, _, _, _),
		K = K0,
		V = V0
	).
upper_bound_search_aux19_aux20(Result0, _K0, T, SearchK, K, V, _K1) :-
	Result0 = (=),
	T = four(_, V0, _, _, _, _, _, _, _, _),
	K = SearchK,
	V = V0.
upper_bound_search_aux19_aux20(Result0, _K0, T, SearchK, K, V, K1) :-
	Result0 = (>),
	T = four(_, _, _, _, _, _, _, T1, _, _),
	(
		upper_bound_search(T1, SearchK, Kp, Vp)
	->
		K = Kp,
		V = Vp
	;
		T = four(_, _, _, V1, _, _, _, _, _, _),
		K = K1,
		V = V1
	).

upper_bound_lookup(T, SearchK, K, V) :-
	( upper_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("upper_bound_lookup: key not found.",
			SearchK, V)
	).

%------------------------------------------------------------------------------%

update(Tin, _K, _V, _Tout) :-
	Tin = empty,
	fail.
update(Tin, K, V, Tout) :-
	Tin = two(K0, _, _, _),
	compare(Result, K, K0),
	update_aux22(Result, K0, Tout, V, K, Tin).
update(Tin, K, V, Tout) :-
	Tin = three(K0, _, _, _, _, _, _),
	compare(Result0, K, K0),
	update_aux23(Result0, K0, Tout, V, K, Tin).
update(Tin, K, V, Tout) :-
	Tin = four(_, _, K1, _, _, _, _, _, _, _),
	compare(Result1, K, K1),
	update_aux25(Result1, K1, Tout, V, K, Tin).

update_aux22(Result, K0, Tout, V, K, Tin) :-
	Result = (<),
	Tin = two(_, _, T0, _),
	update(T0, K, V, NewT0),
	Tin = two(_, V0, _, T1),
	Tout = two(K0, V0, NewT0, T1).
update_aux22(Result, K0, Tout, V, _K, Tin) :-
	Result = (=),
	Tin = two(_, _, T0, T1),
	Tout = two(K0, V, T0, T1).
update_aux22(Result, K0, Tout, V, K, Tin) :-
	Result = (>),
	Tin = two(_, _, _, T1),
	update(T1, K, V, NewT1),
	Tin = two(_, V0, T0, _),
	Tout = two(K0, V0, T0, NewT1).

update_aux23(Result0, K0, Tout, V, K, Tin) :-
	Result0 = (<),
	Tin = three(_, _, _, _, T0, _, _),
	update(T0, K, V, NewT0),
	Tin = three(_, V0, K1, V1, _, T1, T2),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
update_aux23(Result0, K0, Tout, V, _K, Tin) :-
	Result0 = (=),
	Tin = three(_, _, K1, V1, T0, T1, T2),
	Tout = three(K0, V, K1, V1, T0, T1, T2).
update_aux23(Result0, K0, Tout, V, K, Tin) :-
	Result0 = (>),
	Tin = three(_, _, K1, _, _, _, _),
	compare(Result1, K, K1),
	update_aux23_aux24(Result1, K1, Tin, K, V, Tout, K0).

update_aux23_aux24(Result1, K1, Tin, K, V, Tout, K0) :-
	Result1 = (<),
	Tin = three(_, _, _, _, _, T1, _),
	update(T1, K, V, NewT1),
	Tin = three(_, V0, _, V1, T0, _, T2),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
update_aux23_aux24(Result1, K1, Tin, _K, V, Tout, K0) :-
	Result1 = (=),
	Tin = three(_, V0, _, _, T0, T1, T2),
	Tout = three(K0, V0, K1, V, T0, T1, T2).
update_aux23_aux24(Result1, K1, Tin, K, V, Tout, K0) :-
	Result1 = (>),
	Tin = three(_, _, _, _, _, _, T2),
	update(T2, K, V, NewT2),
	Tin = three(_, V0, _, V1, T0, T1, _),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).

update_aux25(Result1, K1, Tout, V, K, Tin) :-
	Result1 = (<),
	Tin = four(K0, _, _, _, _, _, _, _, _, _),
	compare(Result0, K, K0),
	update_aux25_aux26(Result0, K0, Tin, K, V, Tout, K1).
update_aux25(Result1, K1, Tout, V, _K, Tin) :-
	Result1 = (=),
	Tin = four(K0, V0, _, _, K2, V2, T0, T1, T2, T3),
	Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3).
update_aux25(Result1, K1, Tout, V, K, Tin) :-
	Result1 = (>),
	Tin = four(_, _, _, _, K2, _, _, _, _, _),
	compare(Result2, K, K2),
	update_aux25_aux27(Result2, K2, Tin, K, V, Tout, K1).

update_aux25_aux26(Result0, K0, Tin, K, V, Tout, K1) :-
	Result0 = (<),
	Tin = four(_, _, _, _, _, _, T0, _, _, _),
	update(T0, K, V, NewT0),
	Tin = four(_, V0, _, V1, K2, V2, _, T1, T2, T3),
	Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3).
update_aux25_aux26(Result0, K0, Tin, _K, V, Tout, K1) :-
	Result0 = (=),
	Tin = four(_, _, _, V1, K2, V2, T0, T1, T2, T3),
	Tout = four(K0, V, K1, V1, K2, V2, T0, T1, T2, T3).
update_aux25_aux26(Result0, K0, Tin, K, V, Tout, K1) :-
	Result0 = (>),
	Tin = four(_, _, _, _, _, _, _, T1, _, _),
	update(T1, K, V, NewT1),
	Tin = four(_, V0, _, V1, K2, V2, T0, _, T2, T3),
	Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3).

update_aux25_aux27(Result2, K2, Tin, K, V, Tout, K1) :-
	Result2 = (<),
	Tin = four(_, _, _, _, _, _, _, _, T2, _),
	update(T2, K, V, NewT2),
	Tin = four(K0, V0, _, V1, _, V2, T0, T1, _, T3),
	Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3).
update_aux25_aux27(Result2, K2, Tin, _K, V, Tout, K1) :-
	Result2 = (=),
	Tin = four(K0, V0, _, V1, _, _, T0, T1, T2, T3),
	Tout = four(K0, V0, K1, V1, K2, V, T0, T1, T2, T3).
update_aux25_aux27(Result2, K2, Tin, K, V, Tout, K1) :-
	Result2 = (>),
	Tin = four(_, _, _, _, _, _, _, _, _, T3),
	update(T3, K, V, NewT3),
	Tin = four(K0, V0, _, V1, _, V2, T0, T1, T2, _),
	Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

% :- inst two(K, V, T) =
% 	bound(
% 		two(K, V, T, T)
% 	).
% 
% :- inst uniq_two(K, V, T) =
% 	unique(
% 		two(K, V, T, T)
% 	).
% 
% :- inst three(K, V, T) =
% 	bound(
% 		three(K, V, K, V, T, T, T)
% 	).
% 
% :- inst uniq_three(K, V, T) =
% 	unique(
% 		three(K, V, K, V, T, T, T)
% 	).
% 
% :- inst four(K, V, T) =
% 	bound(
% 		four(K, V, K, V, K, V, T, T, T, T)
% 	).
% 
% :- inst uniq_four(K, V, T) =
% 	unique(
% 		four(K, V, K, V, K, V, T, T, T, T)
% 	).
% 
% :- mode uo_two :: out(uniq_two(unique, unique, unique)).
% :- mode suo_two :: out(uniq_two(ground, ground, uniq_tree234_gg)).
% :- mode out_two :: out(two(ground, ground, ground)).
% 
% :- mode di_two :: di(uniq_two(unique, unique, unique)).
% :- mode sdi_two :: di(uniq_two(ground, ground, uniq_tree234_gg)).
% :- mode in_two :: in(two(ground, ground, ground)).
% 
% :- mode di_three :: di(uniq_three(unique, unique, unique)).
% :- mode sdi_three :: di(uniq_three(ground, ground, uniq_tree234_gg)).
% :- mode in_three :: in(three(ground, ground, ground)).
% 
% :- mode di_four :: di(uniq_four(unique, unique, unique)).
% :- mode sdi_four :: di(uniq_four(ground, ground, uniq_tree234_gg)).
% :- mode in_four :: in(four(ground, ground, ground)).

%------------------------------------------------------------------------------%

% :- pred split_four(tree234(K, V), K, V, tree234(K, V), tree234(K, V)).
% :- mode split_four(di_four, uo, uo, uo_two, uo_two) is det.
% % :- mode split_four(sdi_four, out, out, suo_two, suo_two) is det.
% :- mode split_four(in_four, out, out, out_two, out_two) is det.

split_four(Tin, MidK, MidV, Sub0, Sub1) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	Sub0 = two(K0, V0, T0, T1),
	MidK = K1,
	MidV = V1,
	Sub1 = two(K2, V2, T2, T3).

%------------------------------------------------------------------------------%

% insert is implemented using the simple top-down
% approach described in eg Sedgwick which splits 4 nodes into
% two 2 nodes on the downward traversal of the tree as we
% search for the right place to insert the new key-value pair.
% We know we have the right place if the subtrees of the node
% are empty (in which case we expand the node - which will always
% work because we already split 4 nodes into 2 nodes), or if the
% tree itself is empty.
% This algorithm is O(lgN).

insert(Tin, K, V, Tout) :-
	Tin = empty,
	Tout = two(K, V, empty, empty).
insert(Tin, K, V, Tout) :-
	Tin = two(_, _, _, _),
	insert2(Tin, K, V, Tout).
insert(Tin, K, V, Tout) :-
	Tin = three(_, _, _, _, _, _, _),
	insert3(Tin, K, V, Tout).
insert(Tin, K, V, Tout) :-
	Tin = four(_, _, _, _, _, _, _, _, _, _),
	split_four(Tin, MidK, MidV, Sub0, Sub1),
	compare(Result1, K, MidK),
	insert_aux1(Result1, K, V, MidK, MidV, Sub0, Sub1, Tout).

insert_aux1(Result1, K, V, MidK, MidV, Sub0, Sub1, Tout) :-
	Result1 = (<),
	insert2(Sub0, K, V, NewSub0),
	Tout = two(MidK, MidV, NewSub0, Sub1).
insert_aux1(Result1, _K, _V, _MidK, _MidV, _Sub0, _Sub1, _Tout) :-
	Result1 = (=),
	fail.
insert_aux1(Result1, K, V, MidK, MidV, Sub0, Sub1, Tout) :-
	Result1 = (>),
	insert2(Sub1, K, V, NewSub1),
	Tout = two(MidK, MidV, Sub0, NewSub1).

% :- pred insert2(tree234(K, V), K, V, tree234(K, V)).
% :- mode insert2(di_two, di, di, uo) is semidet.
% % :- mode insert2(sdi_two, in, in, uo_tree234) is semidet.
% :- mode insert2(in_two, in, in, out) is semidet.

insert2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		insert2_aux1(Result, K, V, K0, V0, Tout)
	;
		compare(Result, K, K0),
		insert2_aux28(Result, Tout, V, K, T1, T0, V0, K0)
	).

insert2_aux1(Result, K, V, K0, V0, Tout) :-
	Result = (<),
	Tout = three(K, V, K0, V0, empty, empty, empty).
insert2_aux1(Result, _K, _V, _K0, _V0, _Tout) :-
	Result = (=),
	fail.
insert2_aux1(Result, K, V, K0, V0, Tout) :-
	Result = (>),
	Tout = three(K0, V0, K, V, empty, empty, empty).

insert2_aux28_aux29(K0, V0, T0, T1, K, V, Tout) :-
	T0 = empty,
	NewT0 = two(K, V, empty, empty),
	Tout = two(K0, V0, NewT0, T1).
insert2_aux28_aux29(K0, V0, T0, T1, K, V, Tout) :-
	T0 = two(_, _, _, _),
	insert2(T0, K, V, NewT0),
	Tout = two(K0, V0, NewT0, T1).
insert2_aux28_aux29(K0, V0, T0, T1, K, V, Tout) :-
	T0 = three(_, _, _, _, _, _, _),
	insert3(T0, K, V, NewT0),
	Tout = two(K0, V0, NewT0, T1).
insert2_aux28_aux29(K0, V0, T0, T1, K, V, Tout) :-
	T0 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T0, MT0K, MT0V, T00, T01),
	compare(Result1, K, MT0K),
	insert2_aux28_aux29_aux30(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0).

insert2_aux28_aux31(K0, V0, T0, T1, K, V, Tout) :-
	T1 = empty,
	NewT1 = two(K, V, empty, empty),
	Tout = two(K0, V0, T0, NewT1).
insert2_aux28_aux31(K0, V0, T0, T1, K, V, Tout) :-
	T1 = two(_, _, _, _),
	insert2(T1, K, V, NewT1),
	Tout = two(K0, V0, T0, NewT1).
insert2_aux28_aux31(K0, V0, T0, T1, K, V, Tout) :-
	T1 = three(_, _, _, _, _, _, _),
	insert3(T1, K, V, NewT1),
	Tout = two(K0, V0, T0, NewT1).
insert2_aux28_aux31(K0, V0, T0, T1, K, V, Tout) :-
	T1 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T1, MT1K, MT1V, T10, T11),
	compare(Result1, K, MT1K),
	insert2_aux28_aux31_aux32(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0).

insert2_aux28(Result, Tout, V, K, T1, T0, V0, K0) :-
	Result = (<),
	insert2_aux28_aux29(K0, V0, T0, T1, K, V, Tout).
insert2_aux28(Result, _Tout, _V, _K, _T1, _T0, _V0, _K0) :-
	Result = (=),
	fail.
insert2_aux28(Result, Tout, V, K, T1, T0, V0, K0) :-
	Result = (>),
	insert2_aux28_aux31(K0, V0, T0, T1, K, V, Tout).

insert2_aux28_aux29_aux30(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0) :-
	Result1 = (<),
	insert2(T00, K, V, NewT00),
	Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1).
insert2_aux28_aux29_aux30(Result1, _T01, _T00, _MT0V, _MT0K, _Tout, _V, _K, _T1, _V0, _K0) :-
	Result1 = (=),
	fail.
insert2_aux28_aux29_aux30(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0) :-
	Result1 = (>),
	insert2(T01, K, V, NewT01),
	Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1).

insert2_aux28_aux31_aux32(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0) :-
	Result1 = (<),
	insert2(T10, K, V, NewT10),
	Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11).
insert2_aux28_aux31_aux32(Result1, _T11, _T10, _MT1V, _MT1K, _Tout, _V, _K, _T0, _V0, _K0) :-
	Result1 = (=),
	fail.
insert2_aux28_aux31_aux32(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0) :-
	Result1 = (>),
	insert2(T11, K, V, NewT11),
	Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11).

% :- pred insert3(tree234(K, V), K, V, tree234(K, V)).
% :- mode insert3(di_three, di, di, uo) is semidet.
% % :- mode insert3(sdi_three, in, in, uo_tree234) is semidet.
% :- mode insert3(in_three, in, in, out) is semidet.

insert3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		insert3_aux34(Result0, Tout, V, K, V1, K1, V0, K0)
	;
		compare(Result0, K, K0),
		insert3_aux33(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0)
	).

insert3_aux34(Result0, Tout, V, K, V1, K1, V0, K0) :-
	Result0 = (<),
	Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty).
insert3_aux34(Result0, _Tout, _V, _K, _V1, _K1, _V0, _K0) :-
	Result0 = (=),
	fail.
insert3_aux34(Result0, Tout, V, K, V1, K1, V0, K0) :-
	Result0 = (>),
	compare(Result1, K, K1),
	insert3_aux34_aux42(Result1, K0, V0, K1, V1, K, V, Tout).

insert3_aux34_aux42(Result1, K0, V0, K1, V1, K, V, Tout) :-
	Result1 = (<),
	Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty).
insert3_aux34_aux42(Result1, _K0, _V0, _K1, _V1, _K, _V, _Tout) :-
	Result1 = (=),
	fail.
insert3_aux34_aux42(Result1, K0, V0, K1, V1, K, V, Tout) :-
	Result1 = (>),
	Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty).

insert3_aux33_aux35(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = empty,
	NewT0 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
insert3_aux33_aux35(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = two(_, _, _, _),
	insert2(T0, K, V, NewT0),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
insert3_aux33_aux35(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = three(_, _, _, _, _, _, _),
	insert3(T0, K, V, NewT0),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
insert3_aux33_aux35(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T0, MT0K, MT0V, T00, T01),
	compare(ResultM, K, MT0K),
	insert3_aux33_aux35_aux36(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0).

insert3_aux33_aux37_aux38(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = empty,
	NewT1 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
insert3_aux33_aux37_aux38(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = two(_, _, _, _),
	insert2(T1, K, V, NewT1),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
insert3_aux33_aux37_aux38(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = three(_, _, _, _, _, _, _),
	insert3(T1, K, V, NewT1),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
insert3_aux33_aux37_aux38(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T1, MT1K, MT1V, T10, T11),
	compare(ResultM, K, MT1K),
	insert3_aux33_aux37_aux38_aux39(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout).

insert3_aux33_aux37_aux40(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = empty,
	NewT2 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
insert3_aux33_aux37_aux40(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = two(_, _, _, _),
	insert2(T2, K, V, NewT2),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
insert3_aux33_aux37_aux40(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = three(_, _, _, _, _, _, _),
	insert3(T2, K, V, NewT2),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
insert3_aux33_aux37_aux40(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T2, MT2K, MT2V, T20, T21),
	compare(ResultM, K, MT2K),
	insert3_aux33_aux37_aux40_aux41(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout).

insert3_aux33(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	Result0 = (<),
	insert3_aux33_aux35(K0, V0, K1, V1, T0, T1, T2, K, V, Tout).
insert3_aux33(Result0, _Tout, _V, _K, _T2, _T1, _T0, _V1, _K1, _V0, _K0) :-
	Result0 = (=),
	fail.
insert3_aux33(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	Result0 = (>),
	compare(Result1, K, K1),
	insert3_aux33_aux37(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout).

insert3_aux33_aux37(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	Result1 = (<),
	insert3_aux33_aux37_aux38(Tout, V, K, T2, T1, T0, V1, K1, V0, K0).
insert3_aux33_aux37(Result1, _K0, _V0, _K1, _V1, _T0, _T1, _T2, _K, _V, _Tout) :-
	Result1 = (=),
	fail.
insert3_aux33_aux37(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	Result1 = (>),
	insert3_aux33_aux37_aux40(Tout, V, K, T2, T1, T0, V1, K1, V0, K0).

insert3_aux33_aux35_aux36(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0) :-
	ResultM = (<),
	insert2(T00, K, V, NewT00),
	Tout = four(MT0K, MT0V, K0, V0, K1, V1, NewT00, T01, T1, T2).
insert3_aux33_aux35_aux36(ResultM, _T01, _T00, _MT0V, _MT0K, _Tout, _V, _K, _T2, _T1, _V1, _K1, _V0, _K0) :-
	ResultM = (=),
	fail.
insert3_aux33_aux35_aux36(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0) :-
	ResultM = (>),
	insert2(T01, K, V, NewT01),
	Tout = four(MT0K, MT0V, K0, V0, K1, V1, T00, NewT01, T1, T2).

insert3_aux33_aux37_aux38_aux39(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout) :-
	ResultM = (<),
	insert2(T10, K, V, NewT10),
	Tout = four(K0, V0, MT1K, MT1V, K1, V1, T0, NewT10, T11, T2).
insert3_aux33_aux37_aux38_aux39(ResultM, _T11, _T10, _MT1V, _MT1K, _K0, _V0, _K1, _V1, _T0, _T2, _K, _V, _Tout) :-
	ResultM = (=),
	fail.
insert3_aux33_aux37_aux38_aux39(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout) :-
	ResultM = (>),
	insert2(T11, K, V, NewT11),
	Tout = four(K0, V0, MT1K, MT1V, K1, V1, T0, T10, NewT11, T2).

insert3_aux33_aux37_aux40_aux41(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout) :-
	ResultM = (<),
	insert2(T20, K, V, NewT20),
	Tout = four(K0, V0, K1, V1, MT2K, MT2V, T0, T1, NewT20, T21).
insert3_aux33_aux37_aux40_aux41(ResultM, _T21, _T20, _MT2V, _MT2K, _K0, _V0, _K1, _V1, _T0, _T1, _K, _V, _Tout) :-
	ResultM = (=),
	fail.
insert3_aux33_aux37_aux40_aux41(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout) :-
	ResultM = (>),
	insert2(T21, K, V, NewT21),
	Tout = four(K0, V0, K1, V1, MT2K, MT2V, T0, T1, T20, NewT21).

%------------------------------------------------------------------------------%

% set uses the same algorithm as used for insert,
% except that instead of failing for equal keys, we replace the value.

set(Tin, K, V, Tout) :-
	Tin = empty,
	Tout = two(K, V, empty, empty).
set(Tin, K, V, Tout) :-
	Tin = two(_, _, _, _),
	set2(Tin, K, V, Tout).
set(Tin, K, V, Tout) :-
	Tin = three(_, _, _, _, _, _, _),
	set3(Tin, K, V, Tout).
set(Tin, K, V, Tout) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	compare(Result1, K, K1),
	set_aux43(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, Tout, V, K).

set_aux43(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, Tout, V, K) :-
	Result1 = (<),
	Sub0 = two(K0, V0, T0, T1),
	Sub1 = two(K2, V2, T2, T3),
	set2(Sub0, K, V, NewSub0),
	Tout = two(K1, V1, NewSub0, Sub1).
set_aux43(Result1, T3, T2, T1, T0, V2, K2, _V1, K1, V0, K0, Tout, V, _K) :-
	Result1 = (=),
	Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3).
set_aux43(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, Tout, V, K) :-
	Result1 = (>),
	Sub0 = two(K0, V0, T0, T1),
	Sub1 = two(K2, V2, T2, T3),
	set2(Sub1, K, V, NewSub1),
	Tout = two(K1, V1, Sub0, NewSub1).

% :- pred set2(tree234(K, V), K, V, tree234(K, V)).
% :- mode set2(di_two, di, di, uo) is det.
% % :- mode set2(sdi_two, in, in, uo_tree234) is det.
% :- mode set2(in_two, in, in, out) is det.

set2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		set2_aux45(Result, Tout, V, K, T1, T0, V0, K0)
	;
		compare(Result, K, K0),
		set2_aux44(Result, Tout, V, K, T1, T0, V0, K0)
	).

set2_aux44_aux46(K0, V0, T0, T1, K, V, Tout) :-
	T0 = empty,
	NewT0 = two(K, V, empty, empty),
	Tout = two(K0, V0, NewT0, T1).
set2_aux44_aux46(K0, V0, T0, T1, K, V, Tout) :-
	T0 = two(_, _, _, _),
	set2(T0, K, V, NewT0),
	Tout = two(K0, V0, NewT0, T1).
set2_aux44_aux46(K0, V0, T0, T1, K, V, Tout) :-
	T0 = three(_, _, _, _, _, _, _),
	set3(T0, K, V, NewT0),
	Tout = two(K0, V0, NewT0, T1).
set2_aux44_aux46(K0, V0, T0, T1, K, V, Tout) :-
	T0 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T0, MT0K, MT0V, T00, T01),
	compare(Result1, K, MT0K),
	set2_aux44_aux46_aux47(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0).

set2_aux44_aux48(K0, V0, T0, T1, K, V, Tout) :-
	T1 = empty,
	NewT1 = two(K, V, empty, empty),
	Tout = two(K0, V0, T0, NewT1).
set2_aux44_aux48(K0, V0, T0, T1, K, V, Tout) :-
	T1 = two(_, _, _, _),
	set2(T1, K, V, NewT1),
	Tout = two(K0, V0, T0, NewT1).
set2_aux44_aux48(K0, V0, T0, T1, K, V, Tout) :-
	T1 = three(_, _, _, _, _, _, _),
	set3(T1, K, V, NewT1),
	Tout = two(K0, V0, T0, NewT1).
set2_aux44_aux48(K0, V0, T0, T1, K, V, Tout) :-
	T1 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T1, MT1K, MT1V, T10, T11),
	compare(Result1, K, MT1K),
	set2_aux44_aux48_aux49(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0).

set2_aux44(Result, Tout, V, K, T1, T0, V0, K0) :-
	Result = (<),
	set2_aux44_aux46(K0, V0, T0, T1, K, V, Tout).
set2_aux44(Result, Tout, V, K, T1, T0, _V0, _K0) :-
	Result = (=),
	Tout = two(K, V, T0, T1).
set2_aux44(Result, Tout, V, K, T1, T0, V0, K0) :-
	Result = (>),
	set2_aux44_aux48(K0, V0, T0, T1, K, V, Tout).

set2_aux45(Result, Tout, V, K, _T1, _T0, V0, K0) :-
	Result = (<),
	Tout = three(K, V, K0, V0, empty, empty, empty).
set2_aux45(Result, Tout, V, K, T1, T0, _V0, _K0) :-
	Result = (=),
	Tout = two(K, V, T0, T1).
set2_aux45(Result, Tout, V, K, _T1, _T0, V0, K0) :-
	Result = (>),
	Tout = three(K0, V0, K, V, empty, empty, empty).

set2_aux44_aux46_aux47(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0) :-
	Result1 = (<),
	set2(T00, K, V, NewT00),
	Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1).
set2_aux44_aux46_aux47(Result1, T01, T00, _MT0V, MT0K, Tout, V, _K, T1, V0, K0) :-
	Result1 = (=),
	Tout = three(MT0K, V, K0, V0, T00, T01, T1).
set2_aux44_aux46_aux47(Result1, T01, T00, MT0V, MT0K, Tout, V, K, T1, V0, K0) :-
	Result1 = (>),
	set2(T01, K, V, NewT01),
	Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1).

set2_aux44_aux48_aux49(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0) :-
	Result1 = (<),
	set2(T10, K, V, NewT10),
	Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11).
set2_aux44_aux48_aux49(Result1, T11, T10, _MT1V, MT1K, Tout, V, _K, T0, V0, K0) :-
	Result1 = (=),
	Tout = three(K0, V0, MT1K, V, T0, T10, T11).
set2_aux44_aux48_aux49(Result1, T11, T10, MT1V, MT1K, Tout, V, K, T0, V0, K0) :-
	Result1 = (>),
	set2(T11, K, V, NewT11),
	Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11).

% :- pred set3(tree234(K, V), K, V, tree234(K, V)).
% :- mode set3(di_three, di, di, uo) is det.
% % :- mode set3(sdi_three, in, in, uo_tree234) is det.
% :- mode set3(in_three, in, in, out) is det.

set3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		set3_aux51(Result0, Tout, V, K, V1, K1, V0, K0)
	;
		compare(Result0, K, K0),
		set3_aux50(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0)
	).

set3_aux51(Result0, Tout, V, K, V1, K1, V0, K0) :-
	Result0 = (<),
	Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty).
set3_aux51(Result0, Tout, V, _K, V1, K1, _V0, K0) :-
	Result0 = (=),
	Tout = three(K0, V, K1, V1, empty, empty, empty).
set3_aux51(Result0, Tout, V, K, V1, K1, V0, K0) :-
	Result0 = (>),
	compare(Result1, K, K1),
	set3_aux51_aux59(Result1, K0, V0, K1, V1, K, V, Tout).

set3_aux51_aux59(Result1, K0, V0, K1, V1, K, V, Tout) :-
	Result1 = (<),
	Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty).
set3_aux51_aux59(Result1, K0, V0, K1, _V1, _K, V, Tout) :-
	Result1 = (=),
	Tout = three(K0, V0, K1, V, empty, empty, empty).
set3_aux51_aux59(Result1, K0, V0, K1, V1, K, V, Tout) :-
	Result1 = (>),
	Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty).

set3_aux50_aux52(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = empty,
	NewT0 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
set3_aux50_aux52(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = two(_, _, _, _),
	set2(T0, K, V, NewT0),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
set3_aux50_aux52(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = three(_, _, _, _, _, _, _),
	set3(T0, K, V, NewT0),
	Tout = three(K0, V0, K1, V1, NewT0, T1, T2).
set3_aux50_aux52(K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	T0 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T0, MT0K, MT0V, T00, T01),
	compare(ResultM, K, MT0K),
	set3_aux50_aux52_aux53(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0).

set3_aux50_aux54_aux55(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = empty,
	NewT1 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
set3_aux50_aux54_aux55(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = two(_, _, _, _),
	set2(T1, K, V, NewT1),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
set3_aux50_aux54_aux55(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = three(_, _, _, _, _, _, _),
	set3(T1, K, V, NewT1),
	Tout = three(K0, V0, K1, V1, T0, NewT1, T2).
set3_aux50_aux54_aux55(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T1 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T1, MT1K, MT1V, T10, T11),
	compare(ResultM, K, MT1K),
	set3_aux50_aux54_aux55_aux56(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout).

set3_aux50_aux54_aux57(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = empty,
	NewT2 = two(K, V, empty, empty),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
set3_aux50_aux54_aux57(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = two(_, _, _, _),
	set2(T2, K, V, NewT2),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
set3_aux50_aux54_aux57(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = three(_, _, _, _, _, _, _),
	set3(T2, K, V, NewT2),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2).
set3_aux50_aux54_aux57(Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	T2 = four(_, _, _, _, _, _, _, _, _, _),
	split_four(T2, MT2K, MT2V, T20, T21),
	compare(ResultM, K, MT2K),
	set3_aux50_aux54_aux57_aux58(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout).

set3_aux50(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	Result0 = (<),
	set3_aux50_aux52(K0, V0, K1, V1, T0, T1, T2, K, V, Tout).
set3_aux50(Result0, Tout, V, _K, T2, T1, T0, V1, K1, _V0, K0) :-
	Result0 = (=),
	Tout = three(K0, V, K1, V1, T0, T1, T2).
set3_aux50(Result0, Tout, V, K, T2, T1, T0, V1, K1, V0, K0) :-
	Result0 = (>),
	compare(Result1, K, K1),
	set3_aux50_aux54(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout).

set3_aux50_aux54(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	Result1 = (<),
	set3_aux50_aux54_aux55(Tout, V, K, T2, T1, T0, V1, K1, V0, K0).
set3_aux50_aux54(Result1, K0, V0, _K1, _V1, T0, T1, T2, K, V, Tout) :-
	Result1 = (=),
	Tout = three(K0, V0, K, V, T0, T1, T2).
set3_aux50_aux54(Result1, K0, V0, K1, V1, T0, T1, T2, K, V, Tout) :-
	Result1 = (>),
	set3_aux50_aux54_aux57(Tout, V, K, T2, T1, T0, V1, K1, V0, K0).

set3_aux50_aux52_aux53(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0) :-
	ResultM = (<),
	set2(T00, K, V, NewT00),
	Tout = four(MT0K, MT0V, K0, V0, K1, V1, NewT00, T01, T1, T2).
set3_aux50_aux52_aux53(ResultM, T01, T00, _MT0V, MT0K, Tout, V, _K, T2, T1, V1, K1, V0, K0) :-
	ResultM = (=),
	Tout = four(MT0K, V, K0, V0, K1, V1, T00, T01, T1, T2).
set3_aux50_aux52_aux53(ResultM, T01, T00, MT0V, MT0K, Tout, V, K, T2, T1, V1, K1, V0, K0) :-
	ResultM = (>),
	set2(T01, K, V, NewT01),
	Tout = four(MT0K, MT0V, K0, V0, K1, V1, T00, NewT01, T1, T2).

set3_aux50_aux54_aux55_aux56(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout) :-
	ResultM = (<),
	set2(T10, K, V, NewT10),
	Tout = four(K0, V0, MT1K, MT1V, K1, V1, T0, NewT10, T11, T2).
set3_aux50_aux54_aux55_aux56(ResultM, T11, T10, _MT1V, MT1K, K0, V0, K1, V1, T0, T2, _K, V, Tout) :-
	ResultM = (=),
	Tout = four(K0, V0, MT1K, V, K1, V1, T0, T10, T11, T2).
set3_aux50_aux54_aux55_aux56(ResultM, T11, T10, MT1V, MT1K, K0, V0, K1, V1, T0, T2, K, V, Tout) :-
	ResultM = (>),
	set2(T11, K, V, NewT11),
	Tout = four(K0, V0, MT1K, MT1V, K1, V1, T0, T10, NewT11, T2).

set3_aux50_aux54_aux57_aux58(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout) :-
	ResultM = (<),
	set2(T20, K, V, NewT20),
	Tout = four(K0, V0, K1, V1, MT2K, MT2V, T0, T1, NewT20, T21).
set3_aux50_aux54_aux57_aux58(ResultM, T21, T20, _MT2V, MT2K, K0, V0, K1, V1, T0, T1, _K, V, Tout) :-
	ResultM = (=),
	Tout = four(K0, V0, K1, V1, MT2K, V, T0, T1, T20, T21).
set3_aux50_aux54_aux57_aux58(ResultM, T21, T20, MT2V, MT2K, K0, V0, K1, V1, T0, T1, K, V, Tout) :-
	ResultM = (>),
	set2(T21, K, V, NewT21),
	Tout = four(K0, V0, K1, V1, MT2K, MT2V, T0, T1, T20, NewT21).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

delete(Tin, K, Tout) :-
	delete_2(Tin, K, Tout, _).

	% When deleting an item from a tree, the height of the tree may be
	% reduced by one. The last argument says whether this has occurred.

% :- pred delete_2(tree234(K, V), K, tree234(K, V), bool).
% :- mode delete_2(di, in, uo, out) is det.
% :- mode delete_2(in, in, out, out) is det.

delete_2(Tin, _K, Tout, RH) :-
	Tin = empty,
	Tout = empty,
	RH = no.
delete_2(Tin, K, Tout, RH) :-
	Tin = two(K0, V0, T0, T1),
	compare(Result0, K, K0),
	delete_2_aux60(Result0, T1, T0, V0, K0, RH, Tout, K).
delete_2(Tin, K, Tout, RH) :-
	Tin = three(K0, V0, K1, V1, T0, T1, T2),
	compare(Result0, K, K0),
	delete_2_aux61(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, K).
delete_2(Tin, K, Tout, RH) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	compare(Result1, K, K1),
	delete_2_aux63(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, K).

delete_2_aux60(Result0, T1, T0, V0, K0, RH, Tout, K) :-
	Result0 = (<),
	delete_2(T0, K, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
	;
		Tout = two(K0, V0, NewT0, T1),
		RH = no
	).
delete_2_aux60(Result0, T1, T0, _V0, _K0, RH, Tout, _K) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_2node_t1(ST1K, ST1V, T0, NewT1, Tout, RH)
		;
			Tout = two(ST1K, ST1V, T0, NewT1),
			RH = no
		)
	;
		Tout = T0,
		RH = yes
	).
delete_2_aux60(Result0, T1, T0, V0, K0, RH, Tout, K) :-
	Result0 = (>),
	delete_2(T1, K, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
	;
		Tout = two(K0, V0, T0, NewT1),
		RH = no
	).

delete_2_aux61(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, K) :-
	Result0 = (<),
	delete_2(T0, K, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
		RH = no
	).
delete_2_aux61(Result0, T2, T1, T0, V1, K1, _V0, _K0, RH, Tout, _K) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_3node_t1(ST1K, ST1V, K1, V1, T0, NewT1, T2, Tout, RH)
		;
			Tout = three(ST1K, ST1V, K1, V1, T0, NewT1, T2),
			RH = no
		)
	;
		Tout = two(K1, V1, T0, T2),
		RH = no
	).
delete_2_aux61(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, K) :-
	Result0 = (>),
	compare(Result1, K, K1),
	delete_2_aux61_aux62(Result1, K, Tout, RH, K0, V0, K1, V1, T0, T1, T2).

delete_2_aux61_aux62(Result1, K, Tout, RH, K0, V0, K1, V1, T0, T1, T2) :-
	Result1 = (<),
	delete_2(T1, K, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_3node_t1(K0, V0, K1, V1, T0, NewT1, T2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, T0, NewT1, T2),
		RH = no
	).
delete_2_aux61_aux62(Result1, _K, Tout, RH, K0, V0, _K1, _V1, T0, T1, T2) :-
	Result1 = (=),
	(
		remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
	->
		(
			RHT2 = yes
		->
			fix_3node_t2(K0, V0, ST2K, ST2V, T0, T1, NewT2, Tout, RH)
		;
			Tout = three(K0, V0, ST2K, ST2V, T0, T1, NewT2),
			RH = no
		)
	;
		Tout = two(K0, V0, T0, T1),
		RH = no
	).
delete_2_aux61_aux62(Result1, K, Tout, RH, K0, V0, K1, V1, T0, T1, T2) :-
	Result1 = (>),
	delete_2(T2, K, NewT2, RHT2),
	(
		RHT2 = yes
	->
		fix_3node_t2(K0, V0, K1, V1, T0, T1, NewT2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
		RH = no
	).

delete_2_aux63(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, K) :-
	Result1 = (<),
	compare(Result0, K, K0),
	delete_2_aux63_aux64(Result0, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3).
delete_2_aux63(Result1, T3, T2, T1, T0, V2, K2, _V1, _K1, V0, K0, RH, Tout, _K) :-
	Result1 = (=),
	(
		remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
	->
		(
			RHT2 = yes
		->
			fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3, Tout, RH)
		;
			Tout = four(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3),
			RH = no
		)
	;
		Tout = three(K0, V0, K2, V2, T0, T1, T3),
		RH = no
	).
delete_2_aux63(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, K) :-
	Result1 = (>),
	compare(Result2, K, K2),
	delete_2_aux63_aux65(Result2, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3).

delete_2_aux63_aux64(Result0, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (<),
	delete_2(T0, K, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_4node_t0(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
		RH = no
	).
delete_2_aux63_aux64(Result0, _K, Tout, RH, _K0, _V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_4node_t1(ST1K, ST1V, K1, V1, K2, V2, T0, NewT1, T2, T3, Tout, RH)
		;
			Tout = four(ST1K, ST1V, K1, V1, K2, V2, T0, NewT1, T2, T3),
			RH = no
		)
	;
		Tout = three(K1, V1, K2, V2, T0, T2, T3),
		RH = no
	).
delete_2_aux63_aux64(Result0, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (>),
	delete_2(T1, K, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3),
		RH = no
	).

delete_2_aux63_aux65(Result2, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result2 = (<),
	delete_2(T2, K, NewT2, RHT2),
	(
		RHT2 = yes
	->
		fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3),
		RH = no
	).
delete_2_aux63_aux65(Result2, _K, Tout, RH, K0, V0, K1, V1, _K2, _V2, T0, T1, T2, T3) :-
	Result2 = (=),
	(
		remove_smallest_2(T3, ST3K, ST3V, NewT3, RHT3)
	->
		(
			RHT3 = yes
		->
			fix_4node_t3(K0, V0, K1, V1, ST3K, ST3V, T0, T1, T2, NewT3, Tout, RH)
		;
			Tout = four(K0, V0, K1, V1, ST3K, ST3V, T0, T1, T2, NewT3),
			RH = no
		)
	;
		Tout = three(K0, V0, K1, V1, T0, T1, T2),
		RH = no
	).
delete_2_aux63_aux65(Result2, K, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result2 = (>),
	delete_2(T3, K, NewT3, RHT3),
	(
		RHT3 = yes
	->
		fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3),
		RH = no
	).

%------------------------------------------------------------------------------%

	% We use the same algorithm as delete.

remove(Tin, K, V, Tout) :-
	remove_2(Tin, K, V, Tout, _).

% :- pred remove_2(tree234(K, V), K, V, tree234(K, V), bool).
% :- mode remove_2(di, in, uo, uo, out) is semidet.
% :- mode remove_2(in, in, out, out, out) is semidet.

remove_2(Tin, _K, _V, _Tout, _RH) :-
	Tin = empty,
	fail.
remove_2(Tin, K, V, Tout, RH) :-
	Tin = two(K0, V0, T0, T1),
	compare(Result0, K, K0),
	remove_2_aux66(Result0, T1, T0, V0, K0, RH, Tout, V, K).
remove_2(Tin, K, V, Tout, RH) :-
	Tin = three(K0, V0, K1, V1, T0, T1, T2),
	compare(Result0, K, K0),
	remove_2_aux67(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, V, K).
remove_2(Tin, K, V, Tout, RH) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	compare(Result1, K, K1),
	remove_2_aux69(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, V, K).

remove_2_aux66(Result0, T1, T0, V0, K0, RH, Tout, V, K) :-
	Result0 = (<),
	remove_2(T0, K, V, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
	;
		Tout = two(K0, V0, NewT0, T1),
		RH = no
	).
remove_2_aux66(Result0, T1, T0, V0, _K0, RH, Tout, V, _K) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_2node_t1(ST1K, ST1V, T0, NewT1, Tout, RH)
		;
			Tout = two(ST1K, ST1V, T0, NewT1),
			RH = no
		)
	;
		Tout = T0,
		RH = yes
	),
	V = V0.
remove_2_aux66(Result0, T1, T0, V0, K0, RH, Tout, V, K) :-
	Result0 = (>),
	remove_2(T1, K, V, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
	;
		Tout = two(K0, V0, T0, NewT1),
		RH = no
	).

remove_2_aux67(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, V, K) :-
	Result0 = (<),
	remove_2(T0, K, V, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
		RH = no
	).
remove_2_aux67(Result0, T2, T1, T0, V1, K1, V0, _K0, RH, Tout, V, _K) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_3node_t1(ST1K, ST1V, K1, V1, T0, NewT1, T2, Tout, RH)
		;
			Tout = three(ST1K, ST1V, K1, V1, T0, NewT1, T2),
			RH = no
		)
	;
		Tout = two(K1, V1, T0, T2),
		RH = no
	),
	V = V0.
remove_2_aux67(Result0, T2, T1, T0, V1, K1, V0, K0, RH, Tout, V, K) :-
	Result0 = (>),
	compare(Result1, K, K1),
	remove_2_aux67_aux68(Result1, K, V, Tout, RH, K0, V0, K1, V1, T0, T1, T2).

remove_2_aux67_aux68(Result1, K, V, Tout, RH, K0, V0, K1, V1, T0, T1, T2) :-
	Result1 = (<),
	remove_2(T1, K, V, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_3node_t1(K0, V0, K1, V1, T0, NewT1, T2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, T0, NewT1, T2),
		RH = no
	).
remove_2_aux67_aux68(Result1, _K, V, Tout, RH, K0, V0, _K1, V1, T0, T1, T2) :-
	Result1 = (=),
	(
		remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
	->
		(
			RHT2 = yes
		->
			fix_3node_t2(K0, V0, ST2K, ST2V, T0, T1, NewT2, Tout, RH)
		;
			Tout = three(K0, V0, ST2K, ST2V, T0, T1, NewT2),
			RH = no
		)
	;
		Tout = two(K0, V0, T0, T1),
		RH = no
	),
	V = V1.
remove_2_aux67_aux68(Result1, K, V, Tout, RH, K0, V0, K1, V1, T0, T1, T2) :-
	Result1 = (>),
	remove_2(T2, K, V, NewT2, RHT2),
	(
		RHT2 = yes
	->
		fix_3node_t2(K0, V0, K1, V1, T0, T1, NewT2, Tout, RH)
	;
		Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
		RH = no
	).

remove_2_aux69(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, V, K) :-
	Result1 = (<),
	compare(Result0, K, K0),
	remove_2_aux69_aux70(Result0, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3).
remove_2_aux69(Result1, T3, T2, T1, T0, V2, K2, V1, _K1, V0, K0, RH, Tout, V, _K) :-
	Result1 = (=),
	(
		remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
	->
		(
			RHT2 = yes
		->
			fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3, Tout, RH)
		;
			Tout = four(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3),
			RH = no
		)
	;
		Tout = three(K0, V0, K2, V2, T0, T1, T3),
		RH = no
	),
	V = V1.
remove_2_aux69(Result1, T3, T2, T1, T0, V2, K2, V1, K1, V0, K0, RH, Tout, V, K) :-
	Result1 = (>),
	compare(Result2, K, K2),
	remove_2_aux69_aux71(Result2, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3).

remove_2_aux69_aux70(Result0, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (<),
	remove_2(T0, K, V, NewT0, RHT0),
	(
		RHT0 = yes
	->
		fix_4node_t0(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
		RH = no
	).
remove_2_aux69_aux70(Result0, _K, V, Tout, RH, _K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (=),
	(
		remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
	->
		(
			RHT1 = yes
		->
			fix_4node_t1(ST1K, ST1V, K1, V1, K2, V2, T0, NewT1, T2, T3, Tout, RH)
		;
			Tout = four(ST1K, ST1V, K1, V1, K2, V2, T0, NewT1, T2, T3),
			RH = no
		)
	;
		Tout = three(K1, V1, K2, V2, T0, T2, T3),
		RH = no
	),
	V = V0.
remove_2_aux69_aux70(Result0, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result0 = (>),
	remove_2(T1, K, V, NewT1, RHT1),
	(
		RHT1 = yes
	->
		fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3),
		RH = no
	).

remove_2_aux69_aux71(Result2, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result2 = (<),
	remove_2(T2, K, V, NewT2, RHT2),
	(
		RHT2 = yes
	->
		fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3),
		RH = no
	).
remove_2_aux69_aux71(Result2, _K, V, Tout, RH, K0, V0, K1, V1, _K2, V2, T0, T1, T2, T3) :-
	Result2 = (=),
	(
		remove_smallest_2(T3, ST3K, ST3V, NewT3, RHT3)
	->
		(
			RHT3 = yes
		->
			fix_4node_t3(K0, V0, K1, V1, ST3K, ST3V, T0, T1, T2, NewT3, Tout, RH)
		;
			Tout = four(K0, V0, K1, V1, ST3K, ST3V, T0, T1, T2, NewT3),
			RH = no
		)
	;
		Tout = three(K0, V0, K1, V1, T0, T1, T2),
		RH = no
	),
	V = V2.
remove_2_aux69_aux71(Result2, K, V, Tout, RH, K0, V0, K1, V1, K2, V2, T0, T1, T2, T3) :-
	Result2 = (>),
	remove_2(T3, K, V, NewT3, RHT3),
	(
		RHT3 = yes
	->
		fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3, Tout, RH)
	;
		Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3),
		RH = no
	).

%------------------------------------------------------------------------------%

	% The algorithm we use similar to delete, except that we
	% always go down the left subtree.

remove_smallest(Tin, K, V, Tout) :-
	remove_smallest_2(Tin, K, V, Tout, _).

% :- pred remove_smallest_2(tree234(K, V), K, V, tree234(K, V), bool).
% :- mode remove_smallest_2(di, uo, uo, uo, out) is semidet.
% :- mode remove_smallest_2(in, out, out, out, out) is semidet.

remove_smallest_2(Tin, _K, _V, _Tout, _RH) :-
	Tin = empty,
	fail.
remove_smallest_2(Tin, K, V, Tout, RH) :-
	Tin = two(K0, V0, T0, T1),
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tout = T1,
		RH = yes
	;
		remove_smallest_2(T0, K, V, NewT0, RHT0),
		( RHT0 = yes ->
			fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
		;
			Tout = two(K0, V0, NewT0, T1),
			RH = no
		)
	).
remove_smallest_2(Tin, K, V, Tout, RH) :-
	Tin = three(K0, V0, K1, V1, T0, T1, T2),
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tout = two(K1, V1, T1, T2),
		RH = no
	;
		remove_smallest_2(T0, K, V, NewT0, RHT0),
		( RHT0 = yes ->
			fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
				Tout, RH)
		;
			Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
			RH = no
		)
	).
remove_smallest_2(Tin, K, V, Tout, RH) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tout = three(K1, V1, K2, V2, T1, T2, T3),
		RH = no
	;
		remove_smallest_2(T0, K, V, NewT0, RHT0),
		( RHT0 = yes ->
			fix_4node_t0(K0, V0, K1, V1, K2, V2,
				NewT0, T1, T2, T3, Tout, RH)
		;
			Tout = four(K0, V0, K1, V1, K2, V2,
				NewT0, T1, T2, T3),
			RH = no
		)
	).

%------------------------------------------------------------------------------%

	% The input to the following group of predicates are the components
	% of a two-, three- or four-node in which the height of the indicated
	% subtree is one less that it should be. If it is possible to increase
	% the height of that subtree by moving into it elements from its
	% neighboring subtrees, do so, and return the resulting tree with RH
	% set to no. Otherwise, return a balanced tree whose height is reduced
	% by one, with RH set to yes to indicate the reduced height.

% :- pred fix_2node_t0(K, V, tree234(K, V), tree234(K, V), tree234(K, V), bool).
% :- mode fix_2node_t0(di, di, di, di, uo, out) is det.
% :- mode fix_2node_t0(in, in, in, in, out, out) is det.

fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
	NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
	Node = two(K0, V0, T0, T10),
	Tout = two(K10, V10, Node, NewT1),
	RH = no.
fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = three(K10, V10, K11, V11, T10, T11, T12),
	NewT1 = two(K11, V11, T11, T12),
	Node = two(K0, V0, T0, T10),
	Tout = two(K10, V10, Node, NewT1),
	RH = no.
fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
	% move T0 one level down and combine it with the subtrees of T1
	% this reduces the depth of the tree
	T1 = two(K10, V10, T10, T11),
	Tout = three(K0, V0, K10, V10, T0, T10, T11),
	RH = yes.
fix_2node_t0(_K0, _V0, _T0, T1, _Tout, _RH) :-
	T1 = empty,
	error("unbalanced 234 tree").
	% Tout = two(K0, V0, T0, T1),
	% RH = yes

% :- pred fix_2node_t1(K, V, tree234(K, V), tree234(K, V), tree234(K, V), bool).
% :- mode fix_2node_t1(di, di, di, di, uo, out) is det.
% :- mode fix_2node_t1(in, in, in, in, out, out) is det.

fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
	% steal T0's leftmost subtree and combine it with T1
	T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
	NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
	Node = two(K0, V0, T03, T1),
	Tout = two(K02, V02, NewT0, Node),
	RH = no.
fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
	% steal T0's leftmost subtree and combine it with T1
	T0 = three(K00, V00, K01, V01, T00, T01, T02),
	NewT0 = two(K00, V00, T00, T01),
	Node = two(K0, V0, T02, T1),
	Tout = two(K01, V01, NewT0, Node),
	RH = no.
fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
	% move T1 one level down and combine it with the subtrees of T0
	% this reduces the depth of the tree
	T0 = two(K00, V00, T00, T01),
	Tout = three(K00, V00, K0, V0, T00, T01, T1),
	RH = yes.
fix_2node_t1(_K0, _V0, T0, _T1, _Tout, _RH) :-
	T0 = empty,
	error("unbalanced 234 tree").
	% Tout = two(K0, V0, T0, T1),
	% RH = yes

% :- pred fix_3node_t0(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_3node_t0(di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_3node_t0(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
	NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
	Node = two(K0, V0, T0, T10),
	Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
	RH = no.
fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = three(K10, V10, K11, V11, T10, T11, T12),
	NewT1 = two(K11, V11, T11, T12),
	Node = two(K0, V0, T0, T10),
	Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
	RH = no.
fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% move T0 one level down to become the leftmost subtree of T1
	T1 = two(K10, V10, T10, T11),
	NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
	Tout = two(K1, V1, NewT1, T2),
	RH = no.
fix_3node_t0(_K0, _V0, _K1, _V1, _T0, T1, _T2, _Tout, _RH) :-
	T1 = empty,
	error("unbalanced 234 tree").
	% Tout = three(K0, V0, K1, V1, T0, T1, T2),
	% The heights of T1 and T2 are unchanged
	% RH = no

% :- pred fix_3node_t1(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_3node_t1(di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_3node_t1(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T0's rightmost subtree and combine it with T1
	T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
	NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
	Node = two(K0, V0, T03, T1),
	Tout = three(K02, V02, K1, V1, NewT0, Node, T2),
	RH = no.
fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T0's rightmost subtree and combine it with T1
	T0 = three(K00, V00, K01, V01, T00, T01, T02),
	NewT0 = two(K00, V00, T00, T01),
	Node = two(K0, V0, T02, T1),
	Tout = three(K01, V01, K1, V1, NewT0, Node, T2),
	RH = no.
fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% move T1 one level down to become the rightmost subtree of T0
	T0 = two(K00, V00, T00, T01),
	NewT0 = three(K00, V00, K0, V0, T00, T01, T1),
	Tout = two(K1, V1, NewT0, T2),
	RH = no.
fix_3node_t1(_K0, _V0, _K1, _V1, T0, _T1, _T2, _Tout, _RH) :-
	T0 = empty,
	error("unbalanced 234 tree").
	% Tout = three(K0, V0, K1, V1, T0, T1, T2),
	% The heights of T0 and T2 are unchanged
	% RH = no

% :- pred fix_3node_t2(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_3node_t2(di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_3node_t2(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T1's rightmost subtree and combine it with T2
	T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
	NewT1 = three(K10, V10, K11, V11, T10, T11, T12),
	Node = two(K1, V1, T13, T2),
	Tout = three(K0, V0, K12, V12, T0, NewT1, Node),
	RH = no.
fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% steal T1's rightmost subtree and combine it with T2
	T1 = three(K10, V10, K11, V11, T10, T11, T12),
	NewT1 = two(K10, V10, T10, T11),
	Node = two(K1, V1, T12, T2),
	Tout = three(K0, V0, K11, V11, T0, NewT1, Node),
	RH = no.
fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	% move T2 one level down to become the rightmost subtree of T1
	T1 = two(K10, V10, T10, T11),
	NewT1 = three(K10, V10, K1, V1, T10, T11, T2),
	Tout = two(K0, V0, T0, NewT1),
	RH = no.
fix_3node_t2(_K0, _V0, _K1, _V1, _T0, T1, _T2, _Tout, _RH) :-
	T1 = empty,
	error("unbalanced 234 tree").
	% Tout = three(K0, V0, K1, V1, T0, T1, T2),
	% The heights of T0 and T1 are unchanged
	% RH = no

% :- pred fix_4node_t0(K, V, K, V, K, V,
% 	tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_4node_t0(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_4node_t0(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
	NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
	Node = two(K0, V0, T0, T10),
	Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
	RH = no.
fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T1's leftmost subtree and combine it with T0
	T1 = three(K10, V10, K11, V11, T10, T11, T12),
	NewT1 = two(K11, V11, T11, T12),
	Node = two(K0, V0, T0, T10),
	Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
	RH = no.
fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% move T0 one level down to become the leftmost subtree of T1
	T1 = two(K10, V10, T10, T11),
	NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
	Tout = three(K1, V1, K2, V2, NewT1, T2, T3),
	RH = no.
fix_4node_t0(_K0, _V0, _K1, _V1, _K2, _V2, _T0, T1, _T2, _T3, _Tout, _RH) :-
	T1 = empty,
	error("unbalanced 234 tree").
	% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	% The heights of T1, T2 and T3 are unchanged
	% RH = no

% :- pred fix_4node_t1(K, V, K, V, K, V,
% 	tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_4node_t1(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_4node_t1(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T2's leftmost subtree and combine it with T1
	T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
	NewT2 = three(K21, V21, K22, V22, T21, T22, T23),
	Node = two(K1, V1, T1, T20),
	Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
	RH = no.
fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T2's leftmost subtree and combine it with T1
	T2 = three(K20, V20, K21, V21, T20, T21, T22),
	NewT2 = two(K21, V21, T21, T22),
	Node = two(K1, V1, T1, T20),
	Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
	RH = no.
fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% move T1 one level down to become the leftmost subtree of T2
	T2 = two(K20, V20, T20, T21),
	NewT2 = three(K1, V1, K20, V20, T1, T20, T21),
	Tout = three(K0, V0, K2, V2, T0, NewT2, T3),
	RH = no.
fix_4node_t1(_K0, _V0, _K1, _V1, _K2, _V2, _T0, _T1, T2, _T3, _Tout, _RH) :-
	T2 = empty,
	error("unbalanced 234 tree").
	% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	% The heights of T0, T2 and T3 are unchanged
	% RH = no

% :- pred fix_4node_t2(K, V, K, V, K, V,
% 	tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_4node_t2(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_4node_t2(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T3's leftmost subtree and combine it with T2
	T3 = four(K30, V30, K31, V31, K32, V32, T30, T31, T32, T33),
	NewT3 = three(K31, V31, K32, V32, T31, T32, T33),
	Node = two(K2, V2, T2, T30),
	Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
	RH = no.
fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T3's leftmost subtree and combine it with T2
	T3 = three(K30, V30, K31, V31, T30, T31, T32),
	NewT3 = two(K31, V31, T31, T32),
	Node = two(K2, V2, T2, T30),
	Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
	RH = no.
fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% move T2 one level down to become the leftmost subtree of T3
	T3 = two(K30, V30, T30, T31),
	NewT3 = three(K2, V2, K30, V30, T2, T30, T31),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT3),
	RH = no.
fix_4node_t2(_K0, _V0, _K1, _V1, _K2, _V2, _T0, _T1, _T2, T3, _Tout, _RH) :-
	T3 = empty,
	error("unbalanced 234 tree").
	% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	% The heights of T0, T1 and T3 are unchanged
	% RH = no

% :- pred fix_4node_t3(K, V, K, V, K, V,
% 	tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
% 	tree234(K, V), bool).
% :- mode fix_4node_t3(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
% :- mode fix_4node_t3(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T2's rightmost subtree and combine it with T3
	T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
	NewT2 = three(K20, V20, K21, V21, T20, T21, T22),
	Node = two(K2, V2, T23, T3),
	Tout = four(K0, V0, K1, V1, K22, V22, T0, T1, NewT2, Node),
	RH = no.
fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% steal T2's rightmost subtree and combine it with T3
	T2 = three(K20, V20, K21, V21, T20, T21, T22),
	NewT2 = two(K20, V20, T20, T21),
	Node = two(K2, V2, T22, T3),
	Tout = four(K0, V0, K1, V1, K21, V21, T0, T1, NewT2, Node),
	RH = no.
fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	% move T3 one level down to become the rightmost subtree of T2
	T2 = two(K20, V20, T20, T21),
	NewT2 = three(K20, V20, K2, V2, T20, T21, T3),
	Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
	RH = no.
fix_4node_t3(_K0, _V0, _K1, _V1, _K2, _V2, _T0, _T1, T2, _T3, _Tout, _RH) :-
	T2 = empty,
	error("unbalanced 234 tree").
	% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	% The heights of T0, T1 and T2 are unchanged
	% RH = no

%------------------------------------------------------------------------------%

keys(Tree, Keys) :-
	keys_2(Tree, [], Keys).

% :- pred keys_2(tree234(K, V), list(K), list(K)).
% :- mode keys_2(in, in, out) is det.

keys_2(empty, List, List).
keys_2(two(K0, _V0, T0, T1), L0, L) :-
	keys_2(T1, L0, L1),
	keys_2(T0, [K0 | L1], L).
keys_2(three(K0, _V0, K1, _V1, T0, T1, T2), L0, L) :-
	keys_2(T2, L0, L1),
	keys_2(T1, [K1 | L1], L2),
	keys_2(T0, [K0 | L2], L).
keys_2(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L0, L) :-
	keys_2(T3, L0, L1),
	keys_2(T2, [K2 | L1], L2),
	keys_2(T1, [K1 | L2], L3),
	keys_2(T0, [K0 | L3], L).

%------------------------------------------------------------------------------%

values(Tree, Values) :-
	values_2(Tree, [], Values).

% :- pred values_2(tree234(K, V), list(V), list(V)).
% :- mode values_2(in, in, out) is det.

values_2(empty, List, List).
values_2(two(_K0, V0, T0, T1), L0, L) :-
	values_2(T1, L0, L1),
	values_2(T0, [V0 | L1], L).
values_2(three(_K0, V0, _K1, V1, T0, T1, T2), L0, L) :-
	values_2(T2, L0, L1),
	values_2(T1, [V1 | L1], L2),
	values_2(T0, [V0 | L2], L).
values_2(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L0, L) :-
	values_2(T3, L0, L1),
	values_2(T2, [V2 | L1], L2),
	values_2(T1, [V1 | L2], L3),
	values_2(T0, [V0 | L3], L).

%------------------------------------------------------------------------------%

assoc_list_to_tree234(AssocList, Tree) :-
	assoc_list_to_tree234_2(AssocList, empty, Tree).

% :- pred assoc_list_to_tree234_2(assoc_list(K, V), tree234(K, V),
% 					tree234(K, V)).
% :- mode assoc_list_to_tree234_2(in, in, out) is det.

assoc_list_to_tree234_2([], Tree, Tree).
assoc_list_to_tree234_2([K - V | Rest], Tree0, Tree) :-
	set(Tree0, K, V, Tree1),
	assoc_list_to_tree234_2(Rest, Tree1, Tree).

%------------------------------------------------------------------------------%

tree234_to_assoc_list(Tree, AssocList) :-
	tree234_to_assoc_list_2(Tree, [], AssocList).

% :- pred tree234_to_assoc_list_2(tree234(K, V), assoc_list(K, V),
% 						assoc_list(K, V)).
% :- mode tree234_to_assoc_list_2(in, in, out) is det.

tree234_to_assoc_list_2(empty, List, List).
tree234_to_assoc_list_2(two(K0, V0, T0, T1), L0, L) :-
	tree234_to_assoc_list_2(T1, L0, L1),
	tree234_to_assoc_list_2(T0, [K0 - V0 | L1], L).
tree234_to_assoc_list_2(three(K0, V0, K1, V1, T0, T1, T2), L0, L) :-
	tree234_to_assoc_list_2(T2, L0, L1),
	tree234_to_assoc_list_2(T1, [K1 - V1 | L1], L2),
	tree234_to_assoc_list_2(T0, [K0 - V0 | L2], L).
tree234_to_assoc_list_2(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
					L0, L) :-
	tree234_to_assoc_list_2(T3, L0, L1),
	tree234_to_assoc_list_2(T2, [K2 - V2 | L1], L2),
	tree234_to_assoc_list_2(T1, [K1 - V1 | L2], L3),
	tree234_to_assoc_list_2(T0, [K0 - V0 | L3], L).

%------------------------------------------------------------------------------%

	% count the number of elements in a tree
count(empty, 0).
count(two(_, _, T0, T1), N) :-
	count(T0, N0),
	count(T1, N1),
	N is 1 + N0 + N1.
count(three(_, _, _, _, T0, T1, T2), N) :-
	count(T0, N0),
	count(T1, N1),
	count(T2, N2),
	N is 2 + N0 + N1 + N2.
count(four(_, _, _, _, _, _, T0, T1, T2, T3), N) :-
	count(T0, N0),
	count(T1, N1),
	count(T2, N2),
	count(T3, N3),
	N is 3 + N0 + N1 + N2 + N3.

