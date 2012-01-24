:- dynamic subscribed/3.

:- local store(sh).

% TODO we currently convert Ids to atoms (i.e. strings) as a hack to see if everything works
% need to change skiplist to support numbers as well
add_subscription(Id, Template, Subscriber) :-
    integer_atom(Id, Ids),
    Template = template(Name, AList, CList),
    make_all_constraints(AList, CList, Constraints),
    store_set(sh, Ids, subscription(Name, Constraints, Subscriber)),
    set_constraint_index(Ids, Constraints).    

delete_subscription(Id) :-
    integer_atom(Id, Ids),
    store_get(sh, Ids, subscription(_, Constraints, _)),
    store_del(sh, Ids),
    del_constraint_index(Ids, Constraints).

find_subscriber(Message, Subscriber) :-
    find_subscription_candidates(Message, Candidate),
    store_get(sh, Candidate, Subscription),
    match_message(Message, Subscription),
    Subscription = subscription(_, _, Subscriber).

find_subscription_candidates(Message, Ids) :-
    Message = object(Name, Attributes),
    (not length(Attributes, 0) ; atom(Name)), !, % TODO
    get_index_names(Attributes, IdxList),
    find_next_subscriber(IdxList, Ids).

find_next_subscriber(AttributeList, NextItem) :-
    index_union_aux(AttributeList, 0, NextItem).

% This makes our C predicate non-deterministic
index_union_aux(AttributeList, OldState, Item) :-
    index_union(sh, AttributeList, OldState, NewItem),
    (
        Item = NewItem
    ;
        index_union_aux(AttributeList, NewItem, Item)
    ).

match_message(Message, Subscription) :-
    Message = object(Name, AList),
    Subscription = subscription(_, Constraints, _),
    sort(AList, SAList),
    match_attributes(SAList, Constraints).

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


%
% Attribute Index
%
set_constraint_index(Id, SList) :-
    ( foreach(constraint(Attribute, _, _), SList), param(Id) do
        save_index(sh, Attribute, Id) ).
del_constraint_index(Id, SList) :-
    ( foreach(constraint(Attribute, _, _), SList), param(Id) do
        remove_index(sh, Attribute, Id) ).