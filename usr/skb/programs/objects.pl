:- op(500, fx, val).
:- op(500, fx, calc).
:- op(500, fx, add).
:- op(500, fx, del).
:- op(500, fx, edit).

:- op(500, xfx, ::).
:- dynamic object/2.
:- dynamic exists/2.
:- dynamic exists_not/2.


%lib(regex).
%:- include("../data/objects.txt").

is_empty([]).

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

print_slots([]).
print_slots([S]) :-
    print_slot(S).
print_slots([S|Rest]) :-
	print_slot(S),
	write(', '),
	print_slots(Rest).

print_slot(Attr::FacetList) :-
	facet_val(req(_, Attr, val, X), FacetList), 
	write(Attr),
	write(': '),
	write(X).




format_object(object(Thing, SlotList), O4) :-
    atom_string(Thing, StrThing),
    append_strings("", StrThing, O1),
    append_strings(O1, " { ", O2),
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

format_slot(Attr::FacetList, In, Out) :-
    facet_val(req(_, Attr, val, X), FacetList),
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
    append_strings(In, Val, Out).



get_object(Thing, ReqList, ConsList, object(Thing, SlotList)) :-
	object(Thing, SlotList),
	slot_vals(Thing, ReqList, SlotList),
	satisfy_constraints(ConsList, SlotList).

satisfy_constraints([], _).
satisfy_constraints([Constraint|Rest], SlotList) :-
    satisfy_constraint(Constraint, SlotList).

satisfy_constraint(constraint(Attr, Comparator, Value), SlotList) :-
    slot_vals(Name, Attr::X, SlotList),
    FX =.. [Comparator, X, Value],
    call(FX).

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
	asserta(object(Thing, NewList)),
	!,
	check_triggers(object(Thing, NewList)),
	print_object(object(Thing, NewList)).
	
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
	!,
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


del_object(Thing) :-
    old_slots(Thing, SlotList),
    retract(object(Thing, _)),
    check_triggers(object(Thing, SlotList)).
del_object(Thing, UList) :-
	old_slots(Thing, SlotList), 
	del_slots(Thing, UList, SlotList, NewList),
	retract(object(Thing, _)), 
	asserta(object(Thing, NewList)),
	check_triggers(object(Thing, SlotList)).

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
	!.
del_facet(req(T, S, F, V), FacetList, [FNew|FL]) :-
    FX =.. [F, OldVal], 
    delete(FX, FacetList, FL), 
    delete(V, OldVal, NewValList), 
    FNew =.. [F, NewValList], 
    !.
del_facet(Req, _, _) :-
    error(['del_facet - unable to remove', Req]).


check_triggers(object(T, AttrList)) :-
    find_exist_triggers(T, L1),
    !,
    check_exist_triggers(object(T, AttrList), L1),
    find_exist_not_triggers(T, L2),
    check_exist_not_triggers(object(T, AttrList), L2).
check_triggers(_).

check_exist_triggers(_, []).
check_exist_triggers(object(Name, AttrList), [triplet(Attrs, Constraints, ReplyState)|Rest]) :-
    trigger_exist(Name, Attrs, Constraint, Object, ReplyState),
    retract(exists(_, triplet(_, _, ReplyState))),
    check_exist_triggers(Name, Rest).

trigger_exist(Name, Attrs, Constraints, Object, ReplyState) :-
    get_object(Name, Attrs, Constraints, Object), !,
    format_object(Object, Output),
    notify_client(Output, ReplyState).
trigger_exist(_, _, _, _, _).

check_exist_not_triggers(_, []).
check_exist_not_triggers(object(Name, AttrList), [triplet(Attrs, Constraints, ReplyState)|Rest]) :-
    trigger_exist_not(Name, Attrs, Constraint, object(Name, AttrList), ReplyState),
    retract(exists_not(_, triplet(_, _, ReplyState))),
    check_exist_not_triggers(Name, Rest).

trigger_exist_not(Name, Attrs, Constraints, Object, ReplyState) :-
    not(get_object(Name, Attrs, Constraints, _)), !,
    format_object(Object, Output),
    notify_client(Output, ReplyState).
trigger_exist_not(_, _, _, _, _).


%notify_client(Object, ReplyState) :-
%    write(Object).

%add_watch(Thing, Attrs, Constraints, Binding) :-
%    asserta(exists(Thing, triplet(Attrs, Constraints, ReplyState))).

find_exist_triggers(Name, L) :-
    coverof(X, exists(Name, X), L).
find_exist_triggers(_, []).

find_exist_not_triggers(Name, L) :-
    coverof(X, exists_not(Name, X), L).
find_exist_not_triggers(_, []).




