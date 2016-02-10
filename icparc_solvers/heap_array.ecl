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
% Copyright (C) 2002 -2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Vassilis Liatsos, Parc Technologies
%                 Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
:- module(heap_array).

:- comment(categories, ["Data Structures"]).
:- comment(summary, "Implements a fixed size heap structure using an array.").
:- comment(desc,html("\
    This library implements a fixed size heap (priority queue).
    Heap entries consist of
    <UL>
    <LI>a key (any number)</LI>
    <LI>an identifier (integer from 1 to N)</LI>
    <LI>a datum (arbitrary term)</LI>
    </UL>
    The heap is ordered according to the key.
    The minimal element can be accessed in constant time. 
    The following operation are supported:
    <UL>
    <LI> Create a fixed size heap </LI>
    <LI> Insert elements to the heap</LI>
    <LI> Decrease key values for elements in the heap</LI>
    <LI> Increase key values for elements in the heap</LI>
    <LI> Access a minimum element (the root)</LI>
    <LI> Remove a minimum element (the root) from the heap</LI>
    <LI> Access all minimum elements as a list</LI>
    <LI> Access elements via their identifier</LI>
    <LI> Remove elements via their identifier</LI>
    <LI> Empty the heap</LI>
    </UL>
    ")
).

:- comment(author,"Vassilis Liatsos and Joachim Schimpf").
:- comment(copyright,"Cisco Systems, Inc.").
:- comment(date,"$Date: 2009/07/16 09:11:27 $").

/*

We map a binary heap to an array as follows:

        a
      /   \
     b     c
    / \   / \
   d   e f   g
 
a b c d e f g
1 2 3 4 5 6 7 8 9 10 ...

This representation has the following properties.
If a node's position in the array is P 
(a) the children of the node can be found in positions: 2*P and 2*P + 1
(b) the parent of the node can be found in position: ceiling((P - 1) / 2))

Whenever an element is added it is added to the last available position of the
array and it is "sifted up". This is an operation that swaps the element's
entry with its parent's entry if it has a smaller key value. The operation
continues until either the element's parent has a smaller key value or when
the element reaches the root of the heap.

Whenever an element's key is decreased it is "sifted up".

Whenever an element is deleted, the last entry is moved to the deleted element's
position and it is "sifted down". This is an operation that swaps the element's
entry with the smallest of its children. The operation continues until either
the element's smallest child has a greater value or when the element becomes
a leaf node.

The "sift up" and "sift down" operations take O(log(N)) time where N is the
number of elements in the heap. This is because in the worst case such an
operation will have to do log(N) swaps.

Looking up the element with the smallest key is an O(1) operation.
 
*/
% heap structure consists of
% - entries: an array storing the heap entries
% - positions: an array storing the position of an element in the heap
% - last: a positive integer >= 0 that points to the position of the last element in the heap
%         if last = 0 then the heap is empty
%         otherwise (last = N) the heap contains N elements	
:- local struct(heap(entries, positions, last)).

% entry structure consists of
% - key: the cost term of the element (any Prolog Term)
% - element: the element for which this entry applies (Integer > 0)
% - datum: the data for that element (any Prolog Term)
:- local struct(entry(key, element, datum)).

:- comment(heap_empty/1,
        [
            summary: "Given a heap structure, succeeds if it is empty",
            args: ["Heap": "the heap structure"],
	    see_also: [heap_create/2, heap_insert/4],
            amode: heap_empty(+)
        ]
	  ).

:- export heap_empty/1.
heap_empty(Heap) :-
	Heap = heap with [last:Last],
	Last == 0.


:- comment(heap_create/2,
        [
            summary: "Creates a heap structure of given size",
            args: [
		      "Size": "the size of the heap (maximum nuumber of elements)",
		      "Heap": "the heap structure"
	          ],
	    see_also: [heap_empty/1, heap_insert/4],
            amode: heap_create(++, -)
        ]
	  ).

:- export heap_create/2.

heap_create(Size, heap with [entries:Entries, positions:Positions, last:0]) :-
	dim(Entries, [Size]),
	dim(Positions, [Size]).


:- comment(heap_insert/4,
        [
            summary: "Inserts an element in the heap",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_create/2],
            amode: heap_insert(+, ++, ++, +)
        ]
	  ).

:- export heap_insert/4.
% heap_insert(+Heap, ++Key, ++ElementId, +Datum)  
heap_insert(Heap, Key, ElementId, Datum) :-
	Heap = heap with [entries: Entries, positions: Positions, last: Last],
	%% Make sure ElementId not in heap - fail if it is already in heap
	arg(ElementId, Positions, NewLast),
	var(NewLast),
	Entry = entry with [key: Key, element: ElementId, datum: Datum], 
	NewLast is Last + 1,
	setarg(NewLast, Entries, Entry),
	setarg(last of heap, Heap, NewLast),
	siftup(Heap, ElementId).


:- comment(heap_delete_all/1,
        [
            summary: "Deletes all elements from a heap structure",
            args: [
		      "Heap": "the heap structure"
	          ],
	    see_also: [heap_empty/1, heap_insert/4],
            amode: heap_delete_all(+)
        ]
	  ).

:- export heap_delete_all/1.
heap_delete_all(Heap) :-
        Heap = heap with [
                             entries:Entries,
                             positions:Positions
                         ],
        setarg(last of heap, Heap, 0),
	dim(Entries, [Size]),
        (
            for(I, 1, Size),
            param(Entries, Positions)
        do
            % we may not need to remove entries as well as positions
            % but it does no harm and I think is not much overhead
            setarg(I, Entries, _),
            setarg(I, Positions, _)
        ).


:- comment(heap_delete/3,
        [
            summary: "Deletes the element with the specified id",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete/4, heap_create/2, heap_insert/4],
	    fail_if:"There is no heap entry for ElementId",
            amode: heap_delete(+,+,-)
        ]
	  ).

:- export heap_delete/3.
heap_delete(Heap, ElementId, Datum) :-
	heap_delete(Heap, ElementId, _Key, Datum).


:- comment(heap_delete/4,
        [
            summary: "Deletes the element with the specified id",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "Key": "the key of the element",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete/3, heap_create/2, heap_insert/4],
	    fail_if:"There is no heap entry for ElementId",
            amode: heap_delete(+,+,-,-)
        ]
	  ).

:- export heap_delete/4.
% heap_delete(+Heap, +ElementId, -Key, -Datum)
heap_delete(Heap, ElementId, Key, Datum) :-
	%% get the element with the specified id
	heap_access(Heap, ElementId, Key, Datum),
	%% Now remove it and update heap
	Heap = heap with [entries: Entries, positions: Positions, last: Last],	
	%% Remove element: uninstantiate the position
        arg(ElementId, Positions, Position),
	setarg(ElementId, Positions, _),
	NewLast is Last - 1,
	setarg(last of heap, Heap, NewLast),
	( Last = Position ->
              %% The deleted element is the last
              true
        ;  Last > 1 ->
              %% Move the old last to the position of deleted
	      arg(Last, Entries, LastEntry),
	      setarg(Last, Entries, _),
	      LastEntry = entry with [key:LastKey, element: LastElementId],
	      setarg(Position, Entries, LastEntry),
	      setarg(LastElementId, Positions, Position),
              ( LastKey < Key ->
                  %% Sift old last entry up
                  siftup(Heap, LastElementId)
              ;
                  %% Sift old last entry down
                  siftdown(Heap, LastElementId)
              )
	;
	      %% Heap has become empty as a result of deleting element
	      true
	).


:- comment(heap_delete_min/3,
        [
            summary: "Deletes the element with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete_min/4,heap_create/2, heap_insert/4],
	    fail_if:"Heap is empty",
            amode: heap_delete_min(+,-,-)
        ]
	  ).

:- export heap_delete_min/3.
heap_delete_min(Heap, ElementId, Datum) :-
	heap_delete_min(Heap, _Key, ElementId, Datum).


:- comment(heap_delete_min/4,
        [
            summary: "Deletes the element with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the key of the minimal entry",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete_min/3, heap_create/2, heap_insert/4],
	    fail_if:"Heap is empty",
            amode: heap_delete_min(+,-,-,-)
        ]
	  ).

:- export heap_delete_min/4.
% heap_delete_min(+Heap, -Key, -ElementId, -Datum)
heap_delete_min(Heap, Key, ElementId, Datum) :-
	%% get the element with the smallest key
	heap_min_key(Heap, Key, ElementId, Datum),
	%% Now remove it and update heap
	Heap = heap with [entries: Entries, positions: Positions, last: Last],	
	%% Remove element: uninstantiate the position
	setarg(ElementId, Positions, _),
	NewLast is Last - 1,
	setarg(last of heap, Heap, NewLast),
	( Last > 1 ->
	      %% Move the old last to the root of heap
	      arg(Last, Entries, LastEntry),
	      setarg(Last, Entries, _),
	      LastEntry = entry with [element: LastElementId],
	      setarg(1, Entries, LastEntry),
	      setarg(LastElementId, Positions, 1),
	      %% Sift old last entry down
	      siftdown(Heap, LastElementId)
	;
	      %% Heap has become empty as a result of deleting min element
	      true
	).


:- comment(heap_min_key/3,
        [
            summary: "Returns the element with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete_min/3, heap_delete_min/4, heap_min_key/4],
	    fail_if:"Heap is empty",
            amode: heap_min_key(+,-,-)
        ]
	  ).

:- export heap_min_key/3.
% heap_min_key(+Heap, -ElementId, -Datum)
heap_min_key(Heap, ElementId, Datum) :-
	heap_min_key(Heap, _Key, ElementId, Datum).


:- comment(heap_min_key/4,
        [
            summary: "Returns the element with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the key of the minimal entry",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    see_also: [heap_delete_min/3, heap_delete_min/4, heap_min_key/3],
	    fail_if:"Heap is empty",
            amode: heap_min_key(+,-,-,-)
        ]
	  ).

:- export heap_min_key/4.
% heap_min_key(+Heap, -Key, -ElementId, -Datum)
heap_min_key(Heap, Key, ElementId, Datum) :-
	Heap = heap with [entries: Entries, last: Last],
	%% Fail if heap is empty
	Last > 0,
	%% Get element in position 1 (root of the heap tree)
	arg(1, Entries, ElementEntry),
	ElementEntry = entry with [element: ElementId, datum: Datum, key:Key].


:- comment(heap_all_min_keys/3,
        [
            summary: "Returns all elements with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "ElementIdList": "list of the element ids (Integers > 0)",
		      "DatumList": "list of the additional information stored with elements"
	          ],
	    see_also: [heap_delete_min/3, heap_delete_min/4, heap_min_key/3, heap_min_key/4, heap_all_min_keys/4],
	    fail_if:"Heap is empty",
            amode: heap_all_min_keys(+,-,-)
        ]
	  ).

:- export heap_all_min_keys/3.
% heap_all_min_keys(+Heap, -ElementIdList, -DatumList)
heap_all_min_keys(Heap, ElementIdList, DatumList) :-
	heap_all_min_keys(Heap, _Key, ElementIdList, DatumList).


:- comment(heap_all_min_keys/4,
        [
            summary: "Returns all elements with the smallest Key",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the key of the minimal entries",
		      "ElementIdList": "list of the element ids (Integers > 0)",
		      "DatumList": "list of the additional information stored with elements"
	          ],
	    see_also: [heap_delete_min/3, heap_delete_min/4, heap_min_key/3, heap_min_key/4, heap_all_min_keys/3],
	    fail_if:"Heap is empty",
            amode: heap_all_min_keys(+,-,-,-)
        ]
	  ).

:- export heap_all_min_keys/4.
heap_all_min_keys(Heap, Key, ElementIdList, DatumList) :-
	Heap = heap with [entries: Entries, last: Last],
	%% Fail if heap is empty
	Last > 0,
	%% Get element in position 1 (root of the heap tree)
	arg(1, Entries, ElementEntry),
	ElementEntry = entry with [element: ElementId, datum: Datum, key:Key],
        rest_min_keys([2, 3], Entries, Last, Key, ElementIds, Data),
        ElementIdList = [ElementId|ElementIds],
        DatumList = [Datum|Data].

rest_min_keys([], _Entries, _Last, _MinKey, [], []).
rest_min_keys([P|Rest], Entries, Last, MinKey, ElementIds, Data) :-
        ( Last >= P,
          arg(P, Entries, ElementEntry),
          ElementEntry = entry with [
                                     element:ElementId,
                                     datum:Datum,
                                     key:Key
                                    ],
          MinKey >= Key ->
            P1 is 2 * P,
            P2 is P1 + 1,
            Rest1 = [P1, P2|Rest],
            ElementIds = [ElementId|ElementIds1],
            Data = [Datum|Data1]
        ;
            Rest1 = Rest,
            ElementIds = ElementIds1,
            Data = Data1
        ),
        rest_min_keys(Rest1, Entries, Last, MinKey, ElementIds1, Data1).


:- comment(heap_increase_key/4,
        [
            summary: "Increases the key value and changes the datum for an element in the heap",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the new key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the new additional information stored with element id"
	          ],
	    see_also: [heap_create/2],
	    exceptions:[abort:"Element not in heap"],
            amode: heap_increase_key(+, ++, ++, +)
        ]
	  ).

:- export heap_increase_key/4.
% heap_increase_key(+Heap, ++Key, ++ElementId, +Datum)
heap_increase_key(Heap, Key, ElementId, Datum) :-
	find_entry(Heap, ElementId, ElementEntry),
	setarg(key of entry, ElementEntry, Key),
	setarg(datum of entry, ElementEntry, Datum),
	siftdown(Heap, ElementId).


:- comment(heap_decrease_key/4,
        [
            summary: "Decreases the key value and changes the datum for an element in the heap",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the new key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the new additional information stored with element id"
	          ],
	    see_also: [heap_create/2],
	    exceptions:[abort:"Element not in heap"],
            amode: heap_decrease_key(+, ++, ++, +)
        ]
	  ).

:- export heap_decrease_key/4.
% heap_decrease_key(+Heap, ++Key, ++ElementId, +Datum)
heap_decrease_key(Heap, Key, ElementId, Datum) :-
	find_entry(Heap, ElementId, ElementEntry),
	setarg(key of entry, ElementEntry, Key),
	setarg(datum of entry, ElementEntry, Datum),
	siftup(Heap, ElementId).


:- comment(heap_update_if_different/4,
        [
            summary: "Enter into heap if new or different than existing entry",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the new key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the new additional information stored with element id"
	          ],
	    desc:html("\
	    If the heap does not contain an element with the given identifier,
	    a new entry is created (as with heap_insert/4). If an entry with
	    an equal key value is already in the heap, the predicate just
            succeeds without changing anything. Otherwise, the existing heap
            entry is replaced (as with heap_update/4).
	    "),
	    see_also: [heap_update/4,heap_update_if_smaller/4,heap_insert/4,heap_update_datum/3],
	    amode: heap_update_if_different(+, ++, ++, +)
        ]
	  ).

:- export heap_update_if_different/4.
% heap_update_if_different(+Heap, ++Key, ++ElementId, +Datum)
heap_update_if_different(Heap, Key, ElementId, Datum) :-
        ( search_entry(Heap, ElementId, ElementEntry) ->
            ElementEntry = entry with key:OldKey,
            ( Key < OldKey ->
                setarg(key of entry, ElementEntry, Key),
                setarg(datum of entry, ElementEntry, Datum),
                siftup(Heap, ElementId)
            ; Key > OldKey ->
                setarg(key of entry, ElementEntry, Key),
                setarg(datum of entry, ElementEntry, Datum),
                siftdown(Heap, ElementId)
            ;
                true
            )
        ;
            heap_insert(Heap, Key, ElementId, Datum)
        ).


:- comment(heap_update_if_smaller/4,
        [
            summary: "Enter into heap if new or smaller than existing entry",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the new key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the new additional information stored with element id"
	          ],
	    desc:html("\
	    If the heap does not contain an element with the given identifier,
	    a new entry is created (as with heap_insert/4). If an entry with
	    an equal or better key value is already in the heap, the predicate
	    just succeeds without changing anything. Otherwise, the existing
	    heap entry is replaced (as with heap_update/4).
	    "),
	    see_also: [heap_update/4,heap_update_if_different/4,heap_insert/4,heap_update_datum/3],
            amode: heap_update_if_smaller(+, ++, ++, +)
        ]
	  ).

:- export heap_update_if_smaller/4.
heap_update_if_smaller(Heap, Key, ElementId, Datum) :-
	( search_entry(Heap, ElementId, ElementEntry) ->
	    ElementEntry = entry with key:OldKey,
	    ( Key < OldKey ->
		setarg(key of entry, ElementEntry, Key),
		setarg(datum of entry, ElementEntry, Datum),
		siftup(Heap, ElementId)
	    ;
		true
	    )
	;
	    heap_insert(Heap, Key, ElementId, Datum)
	).


:- comment(heap_update/4,
        [
            summary: "Enter into heap if smaller than existing entry",
            args: [
		      "Heap": "the heap structure",
		      "Key": "the key of the element",
		      "ElementId": "the element id (Integer > 0)",
		      "Datum": "the additional information stored with element id"
	          ],
	    desc:html("\
	    If the heap contains an entry for element with a key worse than Key,
	    the heap is updated with new values for Key and Datum.  Otherwise,
	    (no entry or better entry already in the heap) an error is raised.
	    "),
	    see_also: [heap_update_if_smaller/4,heap_insert/4,heap_update_datum/3],
            amode: heap_update(+, ++, ++, +)
        ]
	  ).

:- export heap_update/4.
heap_update(Heap, ElementId, Key, Datum) :-
	find_entry(Heap, ElementId, ElementEntry),	% error if not in heap
	ElementEntry = entry with key:OldKey,
	( Key < OldKey ->
	    setarg(key of entry, ElementEntry, Key),
	    setarg(datum of entry, ElementEntry, Datum),
	    siftup(Heap, ElementId)
	;
	    printf(error, "Heap Error: new key must be smaller than old (element %w)\n",[ElementId]),
	    abort
	).


:- comment(heap_access/4,
        [
            summary: "Access a heap entry via its identifier",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "Key": "the key of the element",
		      "Datum": "the additional information stored with element id"
	          ],
	    desc:html("\
	    If the heap contains an entry with the given identifier ElementId,
	    its key and datum are returned. This is a constant time operation.
	    "),
	    fail_if:"There is no heap entry for ElementId",
	    see_also: [heap_update_if_smaller/4,heap_insert/4,heap_update_datum/3],
            amode: heap_access(+, ++ , -, -)
        ]
      ).

:- export heap_access/4.
heap_access(Heap, ElementId, Key, Datum) :-
	search_entry(Heap, ElementId, ElementEntry),
	ElementEntry = entry with [key:Key, datum:Datum].


:- comment(heap_update_datum/3,
        [
            summary: "Change the datum associated with a heap entry",
            args: [
		      "Heap": "the heap structure",
		      "ElementId": "the element id (Integer > 0)",
		      "NewDatum": "new information to be stored with element id"
	          ],
	    desc:html("\
	    If the heap contains an entry for element, its associated datum
	    is updated. Otherwise an error is raised.
	    "),
	    see_also: [heap_update/4,heap_update_if_smaller/4,heap_insert/4],
            amode: heap_update_datum(+, ++, +)
        ]
      ).

:- export heap_update_datum/3.
heap_update_datum(Heap, ElementId, NewDatum) :-
	find_entry(Heap, ElementId, ElementEntry),
	setarg(datum of entry, ElementEntry, NewDatum).



% siftup(+Heap, ++ElementId)
% Given an element id in the heap 
% if element id has a smaller key than its parent
% swap its entry with its parent's entry
% keep doing that until either
% (a) its parent has a smaller key that itself OR
% (b) it reaches the root of the heap

siftup(Heap, ElementId) :-
	find_entry(Heap, ElementId, ElementEntry),
	find_predecessor(Heap, ElementId, PredEntry),
	ElementEntry = entry with [key: ElementKey],
	PredEntry = entry with [key: PredKey],
	PredKey > ElementKey,
	!,
	%% SWAP Predecessor with ElementId
	swap_entries(Heap, ElementEntry, PredEntry),
	siftup(Heap, ElementId).
siftup(_, _).

% siftdown(+Heap, ++ElementId)
% Given an element id in the heap
% if element id has a greater key than the minimum key of its children
% swap its entry with the minimum key child's entry
% keep doing that until either
% (a) it has a smaller key than the minimum key of its children 
% (b) it reaches a leaf of the heap (no children)

siftdown(Heap, ElementId) :-
	find_entry(Heap, ElementId, ElementEntry),
	find_min_child(Heap, ElementId, MinChild),
	ElementEntry = entry with [key: ElementKey],
	MinChild = entry with [key: MinChildKey],
	MinChildKey < ElementKey,
	!,
	%% SWAP MinChild with ElementId
	swap_entries(Heap, ElementEntry, MinChild),
	siftdown(Heap, ElementId).
siftdown(_, _).


% find_entry(+Heap, ++ElementId, -ElementEntry)
% Given a heap and an element in the heap
% Returns the entry for that element
% An exception is raised if element is not in heap
find_entry(Heap, ElementId, ElementEntry) :-
	Heap = heap with [entries: Entries, positions: Positions],
	arg(ElementId, Positions, ElementPosition),
	( nonvar(ElementPosition) ->
	     arg(ElementPosition, Entries, ElementEntry)
	;
	     printf(error, "Heap Error: element %w not in heap\n",[ElementId]),
	     abort
	).
	
search_entry(Heap, ElementId, ElementEntry) :-
	Heap = heap with [entries: Entries, positions: Positions],
	arg(ElementId, Positions, ElementPosition),
	nonvar(ElementPosition),
	arg(ElementPosition, Entries, ElementEntry).

% find_predecessor(+Heap, ++ElementId, -PredEntry)
% Given a heap and an element in the heap
% Returns the entry for its parent node
% Fails if element has no parent node
find_predecessor(Heap, ElementId, PredEntry) :-
	Heap = heap with [entries: Entries, positions: Positions],
	arg(ElementId, Positions, ElementPosition),
%	PredPosition is fix(ceiling( (ElementPosition - 1)/2 )),
	PredPosition is ElementPosition // 2,
	PredPosition > 0,
	arg(PredPosition, Entries, PredEntry).
	

% find_min_child(+Heap, ++ElementId, -MinChild)
% Given a heap and an element in the heap
% Returns the child node with the smallest key
% Fails if element is a leaf node

find_min_child(Heap, ElementId, MinChild) :-
	Heap = heap with [entries: Entries, positions: Positions, last: Last],
	arg(ElementId, Positions, ElementPosition),
	Successor1Pos is 2 * ElementPosition,
	Successor2Pos is Successor1Pos + 1,
	( Successor2Pos =< Last ->
	    arg(Successor1Pos, Entries, Successor1),
	    arg(Successor2Pos, Entries, Successor2),
	    arg(key of entry, Successor1, Key1),
	    arg(key of entry, Successor2, Key2),
	    ( Key1 < Key2 ->
	    	MinChild = Successor1
	    ;
	    	MinChild = Successor2
	    )
	;
	    Successor1Pos =< Last,
	    arg(Successor1Pos, Entries, MinChild)
	).


% swap_entries(+Heap, +Entry1, +Entry2)
% Swaps two entries in the heap
% (a) swaps the entries in the entries array
% (b) swaps the positions in the position array
swap_entries(Heap, Entry1, Entry2) :-
	Heap = heap with [entries: Entries, positions: Positions],	
	Entry1 = entry with [element: ElementId1],
	Entry2 = entry with [element: ElementId2],
	arg(ElementId1, Positions, ElementPos1),
	arg(ElementId2, Positions, ElementPos2),
	setarg(ElementPos1, Entries, Entry2),
	setarg(ElementPos2, Entries, Entry1),
	setarg(ElementId1, Positions, ElementPos2),
	setarg(ElementId2, Positions, ElementPos1).
