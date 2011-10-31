:-include("objects.pl").

%filter([ attr::name, weight::20 ]).

%get_subscribers(Message, Subs) :-
%    write('todo').
%subscribed(object(_, [weight::20]), other).

subscribed(object(msg,  [ hair::[ val blonde ]]), [], me).
subscribed(object(msg,  [ hair::_, attr::[val bla]]), [], me).
subscribed(object(_,    [ face::[ val test ]]), [], me).
subscribed(object(name, [ face::[ val red ], age::[ val 22 ]]), [], subs).
subscribed(object(name, [ face::[ val red ], age::_ ] ), [ constraint(age, '>', 10) ], subs).
subscribed(object(_, [ age::_ ] ), [ constraint(age, '=<', 10) ], subs2).
subscribed(object(_, [ age::_ ] ), [ constraint(age, '>=', 13) ], subs3).
subscribed(object(_, [ other::_, theval::_ ] ), [ constraint(theval, '>', 13) ], subs4).


find_subscriber(Message, Subscriber) :-
    find_subscription(Message, Constraints, Subscriber),
    satisfy_constraints(Message, Constraints).

find_subscription(Message, Constraints, Subscriber) :-
    subscribed(Template, Constraints, Subscriber),
    matches_template(Message, Template).
    
matches_template(object(OName, OSlotList), object(TName, TSlotList)) :-
    match_lists(TSlotList, OSlotList).

match_lists([], _).
match_lists([Attr::X|TRest], OList) :-
    member(Attr::Y, OList),
    (var(X)),
    match_lists(TRest, OList).
match_lists([Attr::X|TRest], OList) :-
    member(Attr::Y, OList),
    member(val(V1), X),
    member(val(V2), Y),
    !,
    (V1 == V2),
    match_lists(TRest, OList).

satisfy_constraints(Message, []).
satisfy_constraints(Message, [C|Rest]) :-
    satisfy_constraint(Message, C),
    satisfy_constraints(Message, Rest).
satisfy_constraint(object(Name, SlotList), constraint(Attr, Comparator, Value)) :-
    %get_object(Message, Attr::X),
    slot_vals(Name, Attr::X, SlotList),
    FX =.. [Comparator, X, Value],
    call(FX).    