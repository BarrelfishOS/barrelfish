:- op(500, fx, val).
:- op(500, fx, calc).
:- op(500, fx, add).
:- op(500, fx, del).
:- op(500, fx, edit).

:- op(500, xfx, ::).
:- dynamic object/2.

:- include("../data/objects.txt").

omg(X) :- write("hey").

is_empty([]).

get_object(Thing, ReqList) :-
	object(Thing, SlotList),
	slot_vals(Thing, ReqList, SlotList).

% Given name, want list of all attributes
slot_vals(_, X, Z) :-
    var(X),
    !,
    X=Z.
slot_vals(_, [], _).
slot_vals(T, [Req|Rest], SlotList) :-
	prep_req(Req, req(T, S, F, V)),
	find_slot(req(T, S, F, V), SlotList),
	!, 
	slot_vals(T, Rest, SlotList).
slot_vals(T, Req, SlotList) :-
	prep_req(Req, req(T, S, F, V)), 
	find_slot(req(T, S, F, V), SlotList).

prep_req(Slot::X, req(T, Slot, val, X)) :-
	var(X),
	!.
prep_req(Slot::X, req(T, Slot, Facet, Val)) :-
	nonvar(X), 
	X =.. [Facet, Val], 
	facet_list(FL), 
	member(Facet, FL), !.
prep_req(Slot::X, req(T, Slot, val, X)).

facet_list([val, def, calc, add, del, edit]).

find_slot(req(T, S, F, V), SlotList) :-
	nonvar(V),
	find_slot(req(T, S, F, Val), SlotList),
	!,
	(Val == V; member(V, Val)).
find_slot(req(T, S, F, V), SlotList) :-
	member(S::FacetList, SlotList),
	!, 
	facet_val(req(T, S, F, V), FacetList).
find_slot(req(T, S, F, V), SlotList) :-
	member(ako::FacetList, SlotList),
	facet_val(req(T, ako, val, Ako), FacetList), 
	(member(X, Ako); X = Ako), 
	object(X, HigherSlots), 
	find_slot(req(T, S, F, V), HigherSlots), !.
%find_slot(Req, _) :-
%	error(['object error looking for:', Req]).

facet_val(req(T, S, F, V), FacetList) :-
	FV =.. [F, V], 
	member(FV, FacetList), !.

facet_val(req(T, S, val, V), FacetList) :-
	member(val ValList, FacetList), 
	member(V, ValList), !.

facet_val(req(T, S, val, V), FacetList) :-
	member(calc Pred, FacetList), 
	Pred =.. [Functor | Args], 
	CalcPred =.. [Functor, req(T, S, val, V) | Args], 
	call(CalcPred).


add_object(Thing, UList) :-
	old_slots(Thing, SlotList), 
	add_slots(Thing, UList, SlotList, NewList), 
	retract(object(Thing, _)), 
	asserta(object(Thing, NewList)), !.

old_slots(Thing, SlotList) :-
	object(Thing, SlotList), 
	!.
old_slots(Thing, []) :-
	asserta(object(Thing, [])).

add_slots(_, [], X, X).
add_slots(T, [U|Rest], SlotList, NewList) :-
	prep_req(U, req(T, S, F, V)),
	add_slot(req(T, S, F, V), SlotList, Z),
	add_slots(T, Rest, Z, NewList).
add_slots(T, X, SlotList, NewList) :-
	prep_req(X, req(T, S, F, V)),
	add_slot(req(T, S, F, V), SlotList, NewList).

remove(_, [], []).
remove(X, [X|R], N) :- remove(X, R, N).
remove(X, [E|R], [E|N]) :- remove(X, R, N).

add_slot(req(T, S, F, V), SlotList, [S::FL2|SL2]) :-
	remove(S::FacetList, SlotList, SL2),
	add_facet(req(T, S, F, V), FacetList, FL2).

add_facet(req(T, S, F, V), FacetList, [FNew|FL2]) :-
	FX =.. [F, OldVal],
	remove(FX, FacetList, FL2),
	%add_newval(OldVal, V, NewVal),
	!,
	check_add_handler(req(T, S, F, V), FacetList),
	FNew =.. [F, V].


/*
add_newval(X, Val, Val) :- 
	var(X), 
	!.
add_newval(OldList, ValList, NewList) :-
	is_list(OldList), 
	is_list(ValList), 
	append(ValList, OldList, NewList), 
	!.
add_newval([H|T], Val, [Val, H|T]).
add_newval(Val, [H|T], [Val, H|T]).
add_newval(_, Val, Val).
*/
check_add_handler(req(T, S, F, V), FacetList) :-
	get_object(T, S::add(Add)), !,
	Add =.. [Functor | Args], 
	AddFunc =.. [Functor, req(T, S, F, V) | Args], 
	call(AddFunc).
check_add_handler(_, _).


del_object(Thing) :-
    retract(object(Thing, _)).
%del_object(Thing) :-
%    throw(['No object', Thing, 'to delete']).
del_object(Thing, UList) :-
	old_slots(Thing, SlotList), 
	del_slots(Thing, UList, SlotList, NewList),
	retract(object(Thing, _)), 
	asserta(object(Thing, NewList)).

del_slots(T, [], X, X).
del_slots(T, [U|Rest], SlotList, NewList) :-
	prep_req(U, req(T, S, F, V)),
	del_slot(req(T, S, F, V), SlotList, Z),
	del_slots(T, Rest, Z, NewList).
del_slots(T, X, SlotList, NewList) :-
	prep_req(X, req(T, S, F, V)),
	del_slot(req(T, S, F, V), SlotList, NewList).

del_slot(req(T, S, F, V), SlotList, SL2) :-
    delete(S::FacetList, SlotList, SL2),
    del_facet(req(T, S, F, V), FacetList, FL2),
    is_empty(FL2),
    !.
del_slot(req(T, S, F, V), SlotList, [S::FL2|SL2]) :-
    delete(S::FacetList, SlotList, SL2),
    del_facet(req(T, S, F, V), FacetList, FL2),
    !.
%del_slot(Req, _, _) :-
%    error(['del_slot - unable to remove', Req]).

del_facet(req(T, S, F, V), FacetList, FL) :-
	FV =.. [F, V],
	delete(FV, FacetList, FL),
	!, 
	check_del_handler(req(T, S, F, V), FacetList).
del_facet(req(T, S, F, V), FacetList, [FNew|FL]) :-
    FX =.. [F, OldVal], 
    delete(FX, FacetList, FL), 
    delete(V, OldVal, NewValList), 
    FNew =.. [F, NewValList], 
    !, 
    check_del_handler(req(T, S, F, V), FacetList).
del_facet(Req, _, _) :-
    error(['del_facet - unable to remove', Req]).

check_del_handler(req(T, S, F, V), FacetList) :-
	get_object(T, S::del(Del)), !, 
	Del =.. [Functor|Args], 
	DelFunc =.. [Functor, req(T, S, F, V)|Args], 
	call(DelFunc).
check_del_handler(_, _).
