:- include("objects.pl").
:- dynamic subscribed/3.

:- local store(sh).

add_subscription(Id, Template, Subscriber) :-
    Template = template(Name, AList, CList),
    make_all_constraints(AList, CList, Constraints),
    store_set(sh, Id, subscription(Name, Constraints, Subscriber)),
    set_constraint_index(Id, Constraints).    

delete_subscription(Id) :-
    store_get(sh, Id, subscription(_, Constraints, _)),
    store_del(sh, Id),
    del_constraint_index(Id, Constraints).

find_subscriber(Message, Subscriber) :-
    find_subscription_candidates(Message, Candidate),
    store_get(sh, Candidate, Subscription),
    match_message(Message, Subscription),
    Subscription = subscription(_, _, Subscriber).

find_subscription_candidates(Message, Id) :-
    Message = object(Name, Attributes),
    (not length(Attributes, 0) ; atom(Name)), !,
    get_index_names(Attributes, IdxList),
    find_next_subscriber(Name, IdxList, Id).

find_next_subscriber(Name, AttributeList, NextItem) :-
    subscriber_intersect_aux(Name, AttributeList, 0, NextItem).

% This makes our C predicate non-deterministic
subscriber_intersect_aux(Name, AttributeList, OldState, Item) :-
    subscriber_intersect(Name, AttributeList, OldState, NewItem),
    (
        Item = NewItem
    ;
        subscriber_intersect_aux(Name, AttributeList, NewItem, Item)
    ). 

%
% Attribute Index
%
set_constraint_index(Id, SList) :-
    ( foreach(constraint(Attribute, _, _), SList), param(Id) do 
        add_index(sh, Attribute, Id) ). % TODO c predicate
del_constraint_index(Id, SList) :-
    ( foreach(constraint(Attribute, _, _), SList), param(Id) do 
        del_index(sh, Attribute, Id) ). % TODO c predicate