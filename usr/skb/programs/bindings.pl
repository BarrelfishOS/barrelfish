:- dynamic binding/3.

notify_domain(N) :-
    write('Notify!').

check_notification(NewVal1, NewVal2) :-
    NewVal1 =\= 0,
    NewVal2 =\= 0,
    identification_complete(NewVal2).


old_binding(Id, OldEventVal, OldRPCVal) :-
    binding(Thing, OldEventVal, OldRPCVal), 
    !.
old_binding(Id, 0, 0) :-
    asserta(binding(Id, 0, 0)).    

set_event_binding(Id, EventBinding) :-
    old_binding(Id, OldEventVal, OldRPCVal),
    retract(binding(Id, OldEventVal , OldRPCVal)),
    asserta(binding(Id, EventBinding, OldRPCVal)),
    check_notification(EventBinding, OldRPCVal).
set_event_binding(Id, EventBinding) :-
    retract(binding(Id, OldEventVal , OldRPCVal)),
    asserta(binding(Id, EventBinding, OldRPCVal)).

set_rpc_binding(Id, RpcBinding) :-
    old_binding(Id, OldEventVal, OldRPCVal),
    retract(binding(Id, OldEventVal , OldRPCVal)),
    asserta(binding(Id, OldEventVal, RpcBinding)),
    check_notification(OldEventVal, RpcBinding).
set_rpc_binding(Id, RpcBinding) :-
    retract(binding(Id, OldEventVal, OldRPCVal)),
    asserta(binding(Id, OldEventVal, RpcBinding)).
    