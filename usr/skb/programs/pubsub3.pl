:- dynamic subscribed/3.

:- local store(sh).
%:- external(bitfield_add/3, p_bitfield_add).
%:- external(bitfield_remove/3, p_bitfield_remove).
%:- external(bitfield_union/4, p_bitfield_union).

add_subscription(Id, Template, Subscriber) :-
    Template = template(Name, AList, CList),
    make_all_constraints(AList, CList, Constraints),
    store_set(sh, Id, subscription(Name, Constraints, Subscriber)),
    bitfield_add(sub, Constraints, Id).

delete_subscription(Id, Binding) :-
    store_get(sh, Id, subscription(_, Constraints, Subscriber)),
    Subscriber = subscriber(Binding, _),
    store_delete(sh, Id),
    bitfield_remove(sub, Constraints, Id).

find_subscriber(Message, Subscriber) :-
    Message = object(Name, AList),
    sort(AList, SAList),
    SMessage = object(Name, SAList),
    find_subscription_candidates(SMessage, Candidate),
    store_get(sh, Candidate, Subscription),
    match_message(SMessage, Subscription),
    Subscription = subscription(_, _, Subscriber).

find_subscription_candidates(Message, Ids) :-
    Message = object(Name, Attributes),
    (not length(Attributes, 0) ; atom(Name)), !, % TODO
    get_index_names(Attributes, IdxList),
    find_next_subscriber(IdxList, Ids).

find_next_subscriber(AttributeList, NextItem) :-
    index_union_aux(AttributeList, "s", NextItem).

% This makes our C predicate non-deterministic
index_union_aux(AttributeList, OldState, Item) :-
    bitfield_union(sh, AttributeList, OldState, NewItem),
    (
        Item = NewItem
    ;
        index_union_aux(AttributeList, NewItem, Item)
    ).

match_message(Message, Subscription) :-
    Message = object(MName, AList),
    Subscription = subscription(SName, Constraints, _),
    ( not var(SName) ->
        SName = name_constraint(Value),
        match(Value, MName, [])
    ; true ),
    match_attributes(AList, Constraints).

% Similar to match_constraints in objects code but works the other
% way around: we match attributs against constraints
% Number comparison
match_attributes(_, []).

match_attributes([val(Key, AValue)|Rest], [constraint(Key, Comparator, CValue)|CRest]) :-
    number(AValue), number(CValue), !,
    number_compare(Comparator, AValue, CValue),
    match_attributes([val(Key, AValue)|Rest], CRest).

% Regular Expression
match_attributes([val(Key, AValue)|Rest], [constraint(Key, match, CValue)|CRest]) :-
    !, ( (string(AValue) ; atom(AValue)), (string(CValue) ; atom(CValue)) ),
    match(CValue, AValue, []),
    match_attributes([val(Key, AValue)|Rest], CRest).

% String comparison
match_attributes([val(Key, AValue)|Rest], [constraint(Key, Comparator, CValue)|CRest]) :-
    ( (string(CValue) ; atom(CValue)), (string(AValue) ; atom(AValue)) ), !,
    string_compare(Comparator, AValue, CValue),
    match_attributes([val(Key, AValue)|Rest], CRest).

% Variable
match_attributes([val(Key, AValue)|Rest], [constraint(Key, '==', CValue)|CRest]) :-
    var(CValue),  !,
    CValue = AValue,
    match_attributes([val(Key, AValue)|Rest], CRest).

% Skip to next relevant Slot in List
match_attributes([val(AKey, AValue)|Rest], [constraint(CKey, Comparator, CValue)|SRest]) :-
    compare(>, CKey, AKey), !,
    match_attributes(Rest, [constraint(CKey, Comparator, CValue)|SRest]).