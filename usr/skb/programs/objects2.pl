:- local store(rh).
:- local store(sequenceTable).
:- local store(attributeIndex).

:- dynamic watch/2.
:- lib(ordset).
:- lib(lists).

%
% Output
%
print_names([]) :-
    flush(1),
    flush(2).
print_names([X]) :-
    !,
    write(X),
    flush(1),
    flush(2).
print_names([X|Rest]) :-
    write(X),
    write(', '),
    print_names(Rest).

print_object(X) :-
    format_object(X, Out),
    write(Out),
    flush(1),
    flush(2).

format_object(object(Thing, SlotList), O4) :-
    atom_string(Thing, StrThing),
    append_strings(StrThing, " { ", O2),
    format_slots(SlotList, O2, O3),
    !,
    append_strings(O3, " }", O4).

format_slots([], In, Out) :-
    append_strings(In, "", Out).
format_slots([S], In, Out) :-
    format_slot(S, In, Out).
format_slots([S|Rest], In, Out) :-
    format_slot(S, In, Out2),
    append_strings(Out2, ", ", Out3),
    format_slots(Rest, Out3, Out).

format_slot(val(Attr, X), In, Out) :-
    atom_string(Attr, StrAttr),
    append_strings(In, StrAttr, Out1),
    append_strings(Out1, ": ", Out2),
    format_slot_val(X, Out2, Out).

format_slot_val(Val, In, Out) :-
    number(Val),
    number_string(Val, StrVal),
    append_strings(In, StrVal, Out).
format_slot_val(Val, In, Out) :-
    atom(Val),
    atom_string(Val, StrVal),
    append_strings(In, StrVal, Out).
format_slot_val(Val, In, Out) :-
    string(Val),
    append_strings(In, "'", Out1),
    append_strings(Out1, Val, Out2),
    append_strings(Out2, "'", Out).

%
% Get Record
%
get_object(Name, AList, CList, Object) :-
    var(Name),
    convert_attributes(AList, ACList),
    append(ACList, CList, Constraints),
    sort(Constraints, SConstraints),
    find_candidates(Name, SConstraints, CandidateLists),
    ord_intersection(CandidateLists, Candidates),
    match_objects(Candidates, SConstraints, Objects). 


match_objects([Name|Rest], Constraints, Object) :-
    store_get(rh, Name, SList),
    Object = object(Name, SList),
    (match_constraint(Constraints, Object) ; match_objects(Rest, Constraints, Object)).

%find_candidates(Name, CList, Candidates) :-
%    var(Name), length(CList, 0), !,
%    stored_keys(rh, Candidates).
find_candidates(Name, [constraint(Attribute, _, Value)|Rest], [IdxList|Cur]) :-
    number(Value), !,
    concat_atoms('n', Attribute, Key), 
    store_get(attributeIndex, Key, IdxList),
    find_candidates(Name, Rest, Cur).
find_candidates(Name, [constraint(Attribute, _, Value)|Rest], [IdxList|Cur]) :-
    (string(Value) ; atom(Value)), !,
    concat_atoms('s', Attribute, Key), 
    store_get(attributeIndex, Key, IdxList),
    find_candidates(Name, Rest, Cur).
find_candidates(_, [], []).

iterative_ord_intersect([], X, X) :- !.
iterative_ord_intersect(Initial, New, Intersection) :-
    ord_intersect(Initial, New, Intersection).

retrieve_index(constraint(Attribute, _, Value), IdxList) :-
    number(Value),
    append_strings("n", Attribute, Key),
    store_get(attributeIndex, Key, IdxList).
retrieve_index(constraint(Attribute, _, Value), IdxList) :-
    (string(Value) ; atom(Value)),
    append_strings("s", Attribute, Key),
    store_get(attributeIndex, Key, IdxList).
retrieve_index(constraint(_, _, _), []).

    
    

%
% Attribute/Constraint Matching
%
string_compare(C, A, B) :-
    atom(A), !,
    atom_string(A, SA),
    string_compare(C, SA, B).
string_compare(C, A, B) :-
    atom(B), !,
    atom_string(B, SB),
    string_compare(C, A, SB).
string_compare(==, A, B) :- !,
    compare(=, A, B).
string_compare(=/=, A, B) :- !,
    (compare(<, A, B), ! ; compare(>, A, B), !).
string_compare(>=, A, B) :- !,
    (compare(>, A, B), ! ; compare(=, A, B), !).
string_compare(=<, A, B) :- !,
    (compare(<, A, B), ! ; compare(=, A, B), !).
string_compare(C, A, B) :- !,
    compare(C, A, B).

match_constraints(CList, Object) :-
    match_constraint(CList, Object).

match_constraint([], _).
match_constraint([name_constraint(Value)|Rest], object(Name, SList)) :-
    !, match_constraint(Rest, SList). % Ignore name constraint at this point
% Number comparison
match_constraint([constraint(Key, Comparator, Value)|Rest], object(Name, [val(Key, SVal)|SRest])) :-
    number(SVal), number(Value), !,
    FX =.. [Comparator, SVal, Value],
    call(FX),
    match_constraint(Rest, object(Name, [val(Key, SVal)|SRest])).
% String comparison
match_constraint([constraint(Key, Comparator, Value)|Rest], object(Name, [val(Key, SVal)|SRest])) :-
    ( (string(Value) ; atom(Value)), (string(SVal) ; atom(SVal)) ), !,
    string_compare(Comparator, Value, SVal),
    match_constraint(Rest, object(Name, [val(Key, SVal)|SRest])).
% Regular Expression
match_constraint([constraint(Key, match, Value)|Rest], object(Name, [val(Key, SVal)|SRest])) :-
    !, ( (string(SVal) ; atom(SVal)), (string(Value) ; atom(Value)) ),
    match(Value, SVal, []),
    match_constraint(Rest, object(Name, [val(Key, SVal)|SRest])).
% Skip to next relevant Slot in List
match_constraint([constraint(AKey, Comparator, Value)|Rest], object(Name, [val(SKey, SVal)|SRest])) :-
    compare(>, AKey, SKey), !, % continue to next entry in slotlist
    match_constraint([constraint(AKey, Comparator, Value)|Rest], object(Name, SRest)).

% Helper functions to convert attributes in constraint and match them against object
prepare_constraint(val(Key, QVal), constraint(Key, ==, QVal)).
convert_attributes(AList, CList) :-
    maplist(prepare_constraint, AList, CList).
match_attributes(AList, Object) :-
    convert_attributes(AList, CList),
    match_constraints(CList, Object).


%
% Add Record
%
next_sequence(Name, Next) :-
    store_get(sequenceTable, Name, Next),
    !,
    store_inc(sequenceTable, Name).
next_sequence(Name, 0) :-
    store_inc(sequenceTable, Name).

add_seq_object(Name, UList, CList) :-
    next_sequence(Name, Seq),
    number_string(Seq, SeqStr),
    atom_string(Name, NameStr),
    append_strings(NameStr, SeqStr, NameSeqStr),
    atom_string(NameSeq, NameSeqStr),
    add_object(NameSeq, UList, CList).
add_object(Name, UList, CList) :-
    get_object(Name, [], CList, SList),
    !, save_object(Name, UList).
add_object(Name, UList, CList) :- % record does not exist yet
    length(CList, 0), % No constraints would match in this case
    !, save_object(Name, UList).

save_object(Name, SList) :-
    transform_attributes(SList, USList),
    store_set(rh, Name, USList),
    set_attribute_index(Name, USList),
    !,
    trigger_watches(object(Name, USList), 1).
%    print_object(object(Name, SList)).


transform_attributes(AList, RNDList) :-
    sort(AList, RList),
    filter_duplicates(RList, RNDList).

filter_duplicates([], []).
filter_duplicates([X], [X]) :- !.
filter_duplicates([val(Key, X),val(Key, Y)|Rest], Out) :-
    filter_duplicates([val(Key, Y)|Rest], Out).
filter_duplicates([val(Key1, X), val(Key2, Y)|Rest], [val(Key1, X)|Out]) :-
    Key1 \= Key2,
    filter_duplicates([val(Key2, Y)|Rest], Out).

%
% Attribute Index
%
set_attribute_index(Name, SList) :-
    ( foreach(Slot, SList), param(Name) do add_index(Name, Slot) ).

del_attribute_index(Name, SList) :-
    ( foreach(Slot, SList), param(Name) do del_index(Name, Slot) ).

add_index(Name, val(Attribute, Value)) :-
   number(Value), !,
   concat_atoms('n', Attribute, Key),
   save_index(Key, Name).
add_index(Name, val(Attribute, Value)) :-
   (string(Value) ; atom(Value)), !,
   concat_atoms('s', Attribute, Key),
   save_index(Key, Name).

save_index(Idx, Name) :-
    store_get(attributeIndex, Idx, NameList),
    !,
    ord_insert(NameList, Name, NewList),
    store_set(attributeIndex, Idx, NewList).
save_index(Idx, Name) :-
    store_set(attributeIndex, Idx, [Name]).

del_index(Name, val(Attribute, Value)) :-
   number(Value), !,
   append_strings("n", Attribute, Key),
   remove_index(Key, Name).
del_index(Name, val(Attribute, Value)) :-
   (string(Value) ; atom(Value)), !,
   append_strings("s", Attribute, Key),
   remove_index(Key, Name).

remove_index(Idx, Name) :-
   store_get(attributeIndex, Idx, NameList),
   !,
   ord_del_element(NameList, Name, NewNameList),
   store_set(attributeIndex, Idx, NewNameList).


    
%
% Delete Record
%
del_object(Name, AList, CList) :-
    get_object(Name, AList, CList, Object),
    store_delete(rh, Name),
    !,
    del_attribute_index(Name, AList),
    trigger_watches(Object, 2).
    
%
% Watches
%
set_watch(Name, Attrs, Constraints, Mode, Recipient) :-
    asserta(watch(Name, triplet(template(Name, Attrs, Constraints), Mode, Recipient))).

trigger_watches(Object, Mode) :-
    find_watches(Object, Watches),
    check_watches(Object, Mode, Watches).

find_watches(object(Name, Attrs), L) :-
    coverof(X, watch(Name, X), L), !.
find_watches(_, []).

check_watches(_, _, []).
check_watches(Object, Mode, [W|Rest]) :-
    check_watch(Object, Mode, W),
    check_watches(Object, Mode, Rest).

check_watch(object(Name, Attrs), Mode, triplet(template(Name, TAttrs, TConstraint), WMode, recipient(Binding, ReplyState, WatchId))) :-
    Mode /\ WMode > 0,
    slot_vals(Name, TAttrs, Attrs),
    satisfy_constraints(TConstraint, object(Name, Attrs)),
    !,
    format_object(object(Name, Attrs), Output),
    trigger_watch(Output, Mode, ReplyState, WatchId, Retract),
    try_retract(Retract, WatchId).
check_watch(_, _, _).

try_retract(1, WatchId) :-
    retract(watch(_, triplet(_, _, recipient(_, _, WatchId)))).
try_retract(0, _). 






:- add_object(o0, [ val(a, 0), val(mix, 1.0)            ], []).
:- add_object(o1, [ val(a, 1), val(mix, 20)             ], []).
:- add_object(o2, [ val(a, 2), val(mix, 'attr1')        ], []).
:- add_object(o3, [ val(a, 3), val(mix, "attr2")        ], []).
:- add_object(o4, [ val(a, 4), val(mix, "test str")     ], []).
:- add_object(o5, [ val(a, 5), val(mix, "12")           ], []).
%:- add_object(o6, [ val(a, 6), val(mix, X)              ], []).
:- add_object(o7, [ val(a, 7), val(mix, "test str 123") ], []).
:- add_object(o8, [ val(a, 8), val(mix, "12")           ], []).
:- add_object(o9, [ val(a, 9), val(mix, 1.0992)         ], []).