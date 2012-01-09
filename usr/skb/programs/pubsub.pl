:-include("objects.pl").

:- dynamic subscribed/3.

%filter([ attr::name, weight::20 ]).

%get_subscribers(Message, Subs) :-
%    write('todo').
%subscribed(object(_, [weight::20]), other).

%subscribed(object(msg,  [ hair::[ val blonde ]]), [], subscriber(123123123, 12) ).
%subscribed(object(msg,  [ hair::_, attr::[val bla]]), [], me).
%subscribed(object(_,    [ face::[ val test ]]), [], me).
%subscribed(object(name, [ face::[ val red ], age::[ val 22 ]]), [], subs).
%subscribed(object(name, [ face::[ val red ], age::_ ] ), [ constraint(age, '>', 10) ], subs).
%subscribed(object(_, [ age::_ ] ), [ constraint(age, '=<', 10) ], subs2).
%subscribed(object(_, [ age::_ ] ), [ constraint(age, '>=', 13) ], subs3).
%subscribed(object(_, [ other::_, theval::_ ] ), [ constraint(theval, '>', 13) ], subs4).

create_object(template(Name, List), object(Name, Attributes)) :-
    add_slots(Name, List, [], Attributes).    

delete_subscription(Binding, Id) :-
    retract(subscribed(_, _, subscriber(Binding, Id))).

add_subscription(Template, Constraints, Subscriber) :-
    create_object(Template, TemplateObject),
    asserta(subscribed(TemplateObject, Constraints, Subscriber)).

find_subscriber(Message, Subscriber) :-
    find_subscription(Message, Constraints, Subscriber),
    satisfy_constraint_list(Message, Constraints).

find_subscription(Message, Constraints, Subscriber) :-
    subscribed(Template, Constraints, Subscriber),
    matches_template(Message, Template).
    
matches_template(object(OName, OSlotList), object(TName, TSlotList)) :-
    match_lists(TSlotList, OSlotList).

match_lists([], _).
match_lists([Attr::Item|TRest], OList) :-
    facet_val(req(T, Attr, val, V), Item),
    member(S::X, OList),
    (var(V)),
    match_lists(TRest, OList).
match_lists([Attr::X|TRest], OList) :-
    member(Attr::Y, OList),
    member(val(V1), X),
    !,
    (V1 == Y),
    match_lists(TRest, OList).

satisfy_constraint_list(Message, []).
satisfy_constraint_list(Message, [C|Rest]) :-
    satisfy_constraint2(Message, C),
    satisfy_constraint_list(Message, Rest).
satisfy_constraint2(object(Thing, SlotList), name_constraint(Value)) :-
    (string(Value) ; atom(Value)),
    match(Value, Thing, []).
satisfy_constraint2(object(Name, SlotList), constraint(Attr, Comparator, Value)) :-
    atom_string(Comparator, "distmatch"), % hack: does not work when put in constraint?
    !,
    member(Attr::X, SlotList),
    (string(X) ;  atom(X)),
    (string(Value) ; atom(Value)),
    match(X, Value, []).
satisfy_constraint2(object(Name, SlotList), constraint(Attr, Comparator, Value)) :-
    %get_object(Message, Attr::X),
    %slot_vals(Name, Attr::X, SlotList),
    writeln(Comparator),
    member(Attr::X, SlotList),
    number(X),
    number(Value),
    FX =.. [Comparator, X, Value],
    call(FX).