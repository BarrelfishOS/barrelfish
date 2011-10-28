:-include("objects.pl").

%filter([ attr::name, weight::20 ]).

%get_subscribers(Message, Subs) :-
%    write('todo').
%subscribed(object(_, [weight::20]), other).

subscribed(object(msg,  [ hair::[ val blonde ]]), [], me).
subscribed(object(msg,  [ hair::[ val blonde ]]), [], me).
subscribed(object(msg,  [ hair::_, attr::[val bla]]), [], me).
subscribed(object(_,    [ face::[ val test ]]), [], me).
subscribed(object(name, [ face::[ val red ], age::_ ] ), [ constraint(age, '>', 10) ], subs).
subscribed(object(_, [ age::_ ] ), [ constraint(age, '=<', 10) ], subs2).
subscribed(object(_, [ age::_ ] ), [ constraint(age, '>=', 13) ], subs3).
% FIXME: wrong order does not match
subscribed(object(_, [ other::_, age::_ ] ), [ constraint(age, '>', 13) ], subs4).
%

find_subscribers(Message, Subscriber) :-
    subscribed(Message, Constraints, Subscriber),
    satisfy_constraints(Message, Constraints).
    
satisfy_constraints(Message, [C|Rest]) :-
    satisfy_constraint(C, Message).

satisfy_constraint(constraint(Attr, Comparator, Value), object(Name, SlotList)) :-
    %get_object(Message, Attr::X),
    slot_vals(Name, Attr::X, SlotList),
    FX =.. [Comparator, X, Value],
    call(FX).
    
    
    