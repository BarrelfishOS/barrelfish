:- op(500, fx, val).
:- op(500, fx, calc).
:- op(500, fx, add).
:- op(500, fx, del).
:- op(500, fx, edit).

:- op(500, xfx, ::).
:- dynamic object/2.
:- dynamic exists/2.
:- dynamic exists_not/2.
:- dynamic watch/2.

%:- lib(regex)    
%:- include("../data/objects.txt").

is_empty([]).
last(List, Last) :- append(_, [Last], List).

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
    append_strings(In, "'", Out1),
    append_strings(Out1, Val, Out2),
    append_strings(Out2, "'", Out).


get_object(Thing, ReqList, ConsList, object(Thing, SlotList)) :-
	object(Thing, SlotList),
	slot_vals(Thing, ReqList, SlotList),
	satisfy_constraints(ConsList, SlotList).

satisfy_constraints([], _).
satisfy_constraints([Constraint|Rest], SlotList) :-
    satisfy_constraint(Constraint, SlotList).

satisfy_constraint(constraint(Attr, Comparator, Value), SlotList) :-
    atom_string(Comparator, "distmatch"), % hack: does not work when put in constraint?
    !,
    slot_vals(Name, Attr::X, SlotList),
    (string(X) ; atom(X)),
    (string(Value) ; atom(Value)),
    match(X, Value, []).
satisfy_constraint(constraint(Attr, Comparator, Value), SlotList) :-
    slot_vals(Name, Attr::X, SlotList),
    number(X),
    number(Value),
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


next_sequence(Name, NextSeq) :-
    findall(X, get_object(X, [], [], _), L),
    get_sequences(Name, L, Seqs),
    sort(Seqs, SortedSeqs),
    last(SortedSeqs, LastSeq),
    NextSeq is LastSeq + 1,
    !.
next_sequence(Name, NextSeq) :- NextSeq is 0.

get_sequences(_, [], []). 
get_sequences(Name, [X|L], [SeqNr|Res]) :-
    atom_string(Name, StrName),
    atom_string(X, StrX),
    substring(StrX, StrName, 1),
    split(StrName, X, [], Splits),
    length(Splits, SplitLen),
    SplitLen == 3,
    last(Splits, Seq),
    number_string(SeqNr, Seq),
    get_sequences(Name, L, Res).

add_seq_object(Thing, UList) :-
    next_sequence(Thing, Seq),
    number_string(Seq, SeqStr),
    atom_string(Thing, ThingStr),
    append_strings(ThingStr, SeqStr, ThingSeqStr),
    atom_string(ThingSeq, ThingSeqStr),
    add_object(ThingSeq, UList).

add_object(Thing, UList) :-
	old_slots(Thing, SlotList),
	add_slots(Thing, UList, SlotList, NewList),
	retract(object(Thing, _)),
	asserta(object(Thing, NewList)),
	!,
    trigger_watches(object(Thing, NewList), 1),
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
    trigger_watches(object(Thing, SlotList), 2).
del_object(Thing, UList) :-
	old_slots(Thing, SlotList), 
	del_slots(Thing, UList, SlotList, NewList),
	retract(object(Thing, _)), 
	asserta(object(Thing, NewList)),
	trigger_watches(object(Thing, SlotList), 2).

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


%
% Watches
%
%trigger_watch(Object, Mode, ReplyState, WatchId, Retract) :-
%    write(Object), write(ReplyState),
%    Retract is 0.

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
    satisfy_constraints(TConstraint, Attrs),
    !,
    format_object(object(Name, Attrs), Output),
    trigger_watch(Output, Mode, ReplyState, WatchId, Retract),
    try_retract(Retract, WatchId).
check_watch(_, _, _).

try_retract(1, WatchId) :-
    retract(watch(_, triplet(_, _, recipient(_, _, WatchId)))).
try_retract(0, _).    
    
