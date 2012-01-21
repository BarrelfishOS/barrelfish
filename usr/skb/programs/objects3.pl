:- local store(rh).
:- local store(sequenceTable).
:- local store(attributeIndex).

:- dynamic watch/2.

%:- lib(regex).
:- lib(lists).
:- lib(ordset).

%
% Get Record
%
get_object(Name, AList, CList, Object) :-
    convert_attributes(AList, ACList),
    append(ACList, CList, Constraints),
    sort(Constraints, SConstraints),
    (atom(Name) *-> 
        get_by_name(Name, SConstraints, Object)
        ;
        get_by_constraints(Name, SConstraints, Object)
    ), !.

get_by_constraints(Name, Constraints, Object) :-
    (length(Constraints, 0) *->
        stored_keys(rh, NameKeys)
    ;
        find_candidates(Constraints, CandidateLists),
        ord_intersection(CandidateLists, NameKeys)
    ),
    (not var(Name) *-> 
        Name = name_constraint(Value),
        (foreach(X, NameKeys), fromto(Candidates,Out,In,[]), param(Value) do
            match(Value, X, []) -> Out=[X|In] ; Out=In)
    ;
        Candidates = NameKeys
    ),
    find_matching_object(Candidates, Constraints, Object).

get_by_name(Name, Constraints, Object) :-
    atom(Name),
    find_matching_object([Name], Constraints, Object).

find_matching_object([Name|Rest], Constraints, Object) :-
    match_object(Name, Constraints, Object) ; 
    find_matching_object(Rest, Constraints, Object).

match_object(Name, Constraints, object(Name, SList)) :-
    store_get(rh, Name, SList),
    match_constraints(Constraints, SList).

constraint_name(constraint(Key, _, _), Key).
get_attribute_names(CList, AList) :-
    maplist(constraint_name, CList, AList).

find_candidates(Constraints, RecordName) :-
    get_attribute_names(Constraints, AttributeList),
    
    retrieve_index(Constraint, IdxList),
    find_candidates(Rest, Cur).
find_candidates([], []).

find_next_candidate(AttributeList) :-
    index_intersect_aux(AttributeList, 0).

index_intersect_aux(AttributeList, OldState) :-
    index_intersect(ThisAttributeList, OldState, NewState),
    (
        AttributeList = ThisAttributeList
    ;
        index_intersect_aux(AttributeList, NewState)
    ). 

%
% Attribute/Constraint Matching
%
string_compare(C, A, B) :-
    atom(A), string(B), !,
    atom_string(A, SA),
    string_compare(C, SA, B).
string_compare(C, A, B) :-
    string(A), atom(B), !,
    atom_string(B, SB),
    string_compare(C, A, SB).
string_compare('==', A, B) :- !,
    compare(=, A, B).
string_compare('!=', A, B) :- !,
    (compare(<, A, B), ! ; compare(>, A, B), !).
string_compare('>=', A, B) :- !,
    (compare(>, A, B), ! ; compare(=, A, B), !).
string_compare('<=', A, B) :- !,
    (compare(<, A, B), ! ; compare(=, A, B), !).
string_compare(C, A, B) :- !,
    compare(C, A, B).

number_compare('==', A, B) :-
    !, A =:= B.
number_compare('!=', A, B) :-
    !, A =\= B.
number_compare('<=', A, B) :-
    !, A =< B.
number_compare(C, A, B) :-
    !, FX =.. [C, A, B],
    call(FX).

match_constraints([], _).
    
% Number comparison
match_constraints([constraint(Key, Comparator, Value)|Rest], [val(Key, SVal)|SRest]) :-
    number(SVal), number(Value), !,
    number_compare(Comparator, SVal, Value),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Regular Expression
match_constraints([constraint(Key, match, Value)|Rest], [val(Key, SVal)|SRest]) :-
    !, ( (string(SVal) ; atom(SVal)), (string(Value) ; atom(Value)) ),
    match(Value, SVal, []),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% String comparison
match_constraints([constraint(Key, Comparator, Value)|Rest], [val(Key, SVal)|SRest]) :-
    ( (string(Value) ; atom(Value)), (string(SVal) ; atom(SVal)) ), !,
    string_compare(Comparator, SVal, Value),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Variable
match_constraints([constraint(Key, '==', Value)|Rest], [val(Key, SVal)|SRest]) :-
    var(Value), !,
    Value = SVal,
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Skip to next relevant Slot in List
match_constraints([constraint(AKey, Comparator, Value)|Rest], [val(SKey, SVal)|SRest]) :-
    compare(>, AKey, SKey), !, % continue to next entry in slotlist
    match_constraints([constraint(AKey, Comparator, Value)|Rest], SRest).

% Helper functions to convert attributes in constraint and match them against object
prepare_constraint(val(Key, QVal), constraint(Key, ==, QVal)).
convert_attributes(AList, CList) :-
    maplist(prepare_constraint, AList, CList).


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
    get_object(Name, [], CList, object(Name, SList)),
    del_attribute_index(Name, SList),
    !, save_object(Name, UList).
add_object(Name, UList, CList) :-
    save_object(Name, UList).

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

%save_index(Idx, Name) :-
%    store_get(attributeIndex, Idx, NameList),
%    !,
%    ord_insert(NameList, Name, NewList),
%    store_set(attributeIndex, Idx, NewList).
%save_index(Idx, Name) :-
%    store_set(attributeIndex, Idx, [Name]).

del_index(Name, val(Attribute, Value)) :-
   number(Value), !,
   concat_atoms('n', Attribute, Key),
   remove_index(Key, Name).
del_index(Name, val(Attribute, Value)) :-
   (string(Value) ; atom(Value)), !,
   concat_atoms('s', Attribute, Key),
   remove_index(Key, Name).

%remove_index(Idx, Name) :-
%   store_get(attributeIndex, Idx, NameList),
%   !,
%   ord_del_element(NameList, Name, NewNameList),
%   store_set(attributeIndex, Idx, NewNameList).

retrieve_index(constraint(Attribute, _, Value), IdxList) :-
    number(Value), !,
    concat_atoms('n', Attribute, Key),
    store_get(attributeIndex, Key, IdxList).
retrieve_index(constraint(Attribute, _, Value), IdxList) :-
    (string(Value) ; atom(Value)), !,
    concat_atoms('s', Attribute, Key),
    store_get(attributeIndex, Key, IdxList).
retrieve_index(constraint(Attribute, _, Value), IdxList) :-
    var(Value), !,
    retrieve_index(constraint(Attribute, _, 0), IdxList1),
    retrieve_index(constraint(Attribute, _, "s"), IdxList2),
    ord_union(IdxList1, IdxList2, IdxList).
retrieve_index(constraint(_, _, _), []).

  
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