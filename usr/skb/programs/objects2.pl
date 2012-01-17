:- lib(hash).
:- hash_create(X), setval(rh, X).
:- hash_create(X), setval(sequenceTable, X).
:- dynamic watch/2.


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
get_object(Name, AList, CList, object(Name, SList)) :-
    getval(rh, HT),
    hash_entry(HT, Name, SList),
    match_attributes(AList, SList),
    match_constraints(CList, object(Name, SList)).
get_first_object(Name, AList, CList, object(Name, SList)) :-
    getval(rh, HT),
    hash_entry(HT, Name, SList),
    match_attributes(AList, SList),
    match_constraints(CList, object(Name, SList)), 
    !.

match_attributes(AList, SList) :-
    sort(AList, SAList),
    match_attribute(SAList, SList).
match_attribute([], _).
match_attribute([val(Key, QVal)|QRest], [val(Key, SVal)|SRest]) :-
    nonvar(QVal),
    !,
    (QVal == SVal),
    match_attribute(QRest, [val(Key, SVal)|SRest]).
match_attribute([val(Key, QVal)|QRest], [val(Key, SVal)|SRest]) :-
    var(QVal),
    !,
    QVal = SVal,
    match_attribute(QRest, [val(Key, SVal)|SRest]).
match_attribute([val(AKey, QVal)|QRest], [val(SKey, SVal)|SRest]) :-
    compare(>, AKey, SKey),
    match_attribute([val(AKey, QVal)|QRest], SRest).

match_constraints(CList, Object) :-
    sort(CList, SCList),
    match_constraint(SCList, Object).

match_constraint([], _).
match_constraint([name_constraint(Value)|Rest], object(Name, SList)) :-
    !,
    (string(Value) ; atom(Value)),
    match(Value, Name, []),
    match_constraint(Rest, SList).
match_constraint([constraint(Key, Comparator, Value)|Rest], object(Name, [val(Key, SVal)|SRest])) :-
    atom_string(Comparator, "distmatch"), % hack: does not work when put in constraint?
    !,
    (string(SVal) ; atom(SVal)),
    (string(Value) ; atom(Value)),
    match(Value, SVal, []),
    match_constraint(Rest, object(Name, [val(Key, SVal)|SRest])).
match_constraint([constraint(Key, Comparator, Value)|Rest], object(Name, [val(Key, SVal)|SRest])) :-
    number(SVal),
    number(Value),
    !,
    FX =.. [Comparator, SVal, Value],
    call(FX),
    match_constraint(Rest, object(Name, [val(Key, SVal)|SRest])).
match_constraint([constraint(AKey, Comparator, Value)|Rest], object(Name, [val(SKey, SVal)|SRest])) :-
    compare(>, AKey, SKey),
    !,
    match_constraint([constraint(AKey, Comparator, Value)|Rest], object(Name, SRest)).

%[eclipse 75]: sort([val('ab', 1), val(aaa, 2)], X).
%X = [val(aaa, 2), val(ab, 1)]
%Yes (0.00s cpu)




%
% Add Record
%
next_sequence(Name, N1) :-
    getval(sequenceTable, SQT),
    hash_update(SQT, Name, N0, N1),
    !,
    N1 is N0 + 1,
    setval(sequenceTable, SQT).
next_sequence(Name, 0) :-
    getval(sequenceTable, SQT),
    hash_add(SQT, Name, 0),
    setval(sequenceTable, SQT).

add_seq_object(Name, UList, CList) :-
    next_sequence(Name, Seq),
    number_string(Seq, SeqStr),
    atom_string(Name, NameStr),
    append_strings(NameStr, SeqStr, NameSeqStr),
    atom_string(NameSeq, NameSeqStr),
    add_object(NameSeq, UList, CList).
add_object(Name, UList, CList) :-
    getval(rh, HT),
    hash_entry(HT, Name, SList),
    !,
    match_constraints(CList, object(Name, SList)),
    transform_attributes(UList, USList),
    save_object(HT, Name, USList).
add_object(Name, UList, CList) :- % record does not exist yet
    getval(rh, HT),
    length(CList, 0), % No constraints would match in this case
    !,
    transform_attributes(UList, USList),
    save_object(HT, Name, USList).

save_object(HT, Name, SList) :-
    hash_set(HT, Name, SList),
    setval(rh, HT),
    !,
    trigger_watches(object(Name, SList), 1),
    print_object(object(Name, SList)).

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
% Delete Record
%
del_object(Name, AList, CList) :-
    get_object(Name, AList, CList, Object),
    getval(rh, HT),
    hash_delete(HT, Name),
    setval(rh, HT),
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