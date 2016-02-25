% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Stefano Novello, Vassilis Liatsos,
%                  Mark Wallace and Andrew Sadler, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: generic_global_constraints.ecl,v 1.15 2015/01/14 01:31:10 jschimpf Exp $
%
%
% IDENTIFICATION:	generic_global_constraints.ecl
%
% AUTHORS:		Joachim Schimpf, IC-Parc, Imperial College, London
%			Stefano Novello, IC-Parc, Imperial College, London
%			Vassilis Liatsos, IC-Parc, Imperial College, London
%			Mark Wallace, IC-Parc, Imperial College, London
%                       Andrew Sadler, IC-Parc, Imperial College, London
%
% This file is intended to be used 'include'd into specialised modules
% which must declare certain key interface predicates (possibly via
% read-macros).
%
% See the file generic_design.txt for a description of the interface
% predicates.
% ----------------------------------------------------------------------


:- export
	alldifferent/1,		% alldifferent(List)
	alldifferent/2,		% alldifferent(List,Cap)
        bool_channeling/3,
        occurrences/3,
	ordered/2,
	ordered_sum/2,
        lex_le/2,
        lex_lt/2,
	lexico_le/2,            % backwards compatibility, alias for lex_le/2
        minlist/2,
	maxlist/2,
	sorted/2,
	sorted/3,
	sum_ge_zero/1,
	sumlist/2,
	atleast/3,
	atmost/3.
% additional constraints exported from 
% generic_flow_constraints, generic_bin_packing

:- import
	setarg/3
    from sepia_kernel.


%----------------------------------------------------------------------
% Output transformations
%----------------------------------------------------------------------

:- export tr_global_out/2.

tr_global_out(atmost(N, List, Val, _, _), atmost(N, List, Val)).
tr_global_out(atleast(N, List, Val, _, _), atleast(N, List, Val)).
tr_global_out(occurrences(Val, Vars, N, _, _), occurrences(Val, Vars, N)).
tr_global_out(lex_demon(Xs,Ys,0,_), lex_le(Xs, Ys)).
tr_global_out(lex_demon(Xs,Ys,1,_), lex_lt(Xs, Ys)).


%----------------------------------------------------------------------
% ordered(+Relation, List) -- naive implementation
%----------------------------------------------------------------------

:- comment(ordered/2, [
    summary:"Constrains List to be ordered according to Relation",
    amode:ordered(++,+),
    args:[
	"Relation":"One of the atoms <, =<, >, >=, =",
	"List":"Collection of integers or domain variables"
    ],
    kind:[constraint:[root:[ic,fd]]],
    see_also:[lex_le/2,ordered_sum/2,sorted/2,collection_to_list/2]
    ]).

ordered(Order, Xs) :- var(Xs), !,
	suspend(ordered(Order, Xs), 4, Xs->inst).
ordered(_, []) :- !.
ordered(Order, [X1|Xs]) :- !,
	ordered1(Order, X1, Xs).
ordered(Order, Xs) :-
	collection_to_list(Xs, List),
	ordered(Order, List).

ordered1(Order, X1, Xs) :- var(Xs), !,
	suspend(ordered(Order, [X1|Xs]), 4, Xs->inst).
ordered1(_Order, _X1, []).
ordered1(Order, X1, X2Xs) :-
	X2Xs = [X2|Xs],
	ordered(Order, X1, X2),
	ordered1(Order, X2, Xs).

    ordered( <, X1, X2) :- X1 #<  X2.
    ordered(=<, X1, X2) :- X1 #=< X2.
    ordered(> , X1, X2) :- X1 #>  X2.
    ordered(>=, X1, X2) :- X1 #>= X2.
    ordered(= , X1, X2) :- X1 #=  X2.


%----------------------------------------------------------------------
% minlist(+ListOfVariables, ?Minimum)
%
%    min(XiL)	is lwb for	Min
%    min(XiH)	is upb for	Min
%    MinL	is lwb for	each Xi
%
% maxlist(+ListOfVariables, ?Maximum) is completely symmetric
%----------------------------------------------------------------------

:- comment(minlist/2, [
    summary:"Min is the minimum of the values in List",
    amode:minlist(+,?),
    args:[
	"List":"Collection of integers or domain variables",
	"Min":"Variable or integer"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    	Min is the minimum of the values in List.  Operationally: 
	Min gets updated to reflect the current range of the minimum
	of variables and values in List.  Likewise, the list
	elements get constrained to the minimum given"),
    see_also:[maxlist/2,sumlist/2,collection_to_list/2]
    ]).

minlist(Xs, Min) :-
	collection_to_list(Xs, List),
	call_priority(minlist_atomic(List, Min), 2).

    minlist_atomic(Xs, Min) :-
	get_bounds(Min, MinL, MinH),
	XLMinStart is MinH+1,
	(
	    foreach(X, Xs),
	    fromto(XLMinStart, XLmin0, XLmin1, XLmin),
	    fromto(MinH, XHmin0, XHmin1, XHmin),
	    param(MinL),
	    fromto(none, MinX0, MinX1, MinX),
	    fromto(Ys, Ys1, Ys0, [])
	do
	    get_bounds(X, XL, XH),
	    ( XL < MinL ->	% Min may constrain variable in list
		lwb(X, MinL),
		% Lower bound of X has changed - need to get its new lower
		% bound in case it's greater than MinL - can't just assume
		% that the new minimum is MinL.
		get_lwb(X, XL1)
	    ;
		XL1 = XL
	    ),
	    ( XL1 < XLmin0 ->
		XLmin1 = XL1,	% it's the new minimum
		MinX1 = X
	    ;
		XLmin1 = XLmin0,
		MinX1 = MinX0
	    ),
	    ( XH < XHmin0 ->	% compute lowest XH
		XHmin1 = XH
	    ;
		XHmin1 = XHmin0
	    ),
	    ( XL > XHmin1 ->
		Ys1 = Ys0	% X can be ignored from now on
	    ;
		Ys1 = [X|Ys0]
	    )
	),
	in_between(Min, XLmin, XHmin),
	( Ys = [LastX] ->
	    LastX = Min
	; nonvar(MinX) ->	% implies nonvar(Min)
	    true
	;
	    suspend(minlist(Ys, Min), 4, [MinX->min,Ys->max,Min->[min,max]])
	).


:- comment(maxlist/2, [
    summary:"Max is the maximum of the values in List",
    amode:maxlist(+,?),
    args:[
	"List":"Collection of integers or domain variables",
	"Max":"Variable or integer"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
	Max is the maximum of the values in List.  Operationally: 
	Max gets updated to reflect the current range of the maximum
	of variables and values in List.  Likewise, the list
	elements get constrained to the maximum given."),
    see_also:[minlist/2,sumlist/2,collection_to_list/2]
    ]).

maxlist(Xs, Max) :-
	collection_to_list(Xs, List),
	call_priority(maxlist_atomic(List, Max), 2).

    maxlist_atomic(Xs, Max) :-
	get_bounds(Max, MaxL, MaxH),
	XHMaxStart is MaxL-1,
	(
	    foreach(X, Xs),
	    fromto(XHMaxStart, XHmax0, XHmax1, XHmax),
	    fromto(MaxL, XLmax0, XLmax1, XLmax),
	    param(MaxH),
	    fromto(none, MaxX0, MaxX1, MaxX),
	    fromto(Ys, Ys1, Ys0, [])
	do
	    get_bounds(X, XL, XH),
	    ( XH > MaxH ->	% Max may constrain variable in list
		upb(X, MaxH),
		% Upper bound of X has changed - need to get its new upper
		% bound in case it's less than MaxH - can't just assume
		% that the new minimum is MaxH.
		get_upb(X, XH1)
	    ;
		XH1 = XH
	    ),
	    ( XH1 > XHmax0 ->
		XHmax1 = XH1,	% it's the new maximum
		MaxX1 = X
	    ;
		XHmax1 = XHmax0,
		MaxX1 = MaxX0
	    ),
	    ( XL > XLmax0 ->	% compute highest XL
		XLmax1 = XL
	    ;
		XLmax1 = XLmax0
	    ),
	    ( XH < XLmax1 ->
		Ys1 = Ys0	% X can be ignored from now on
	    ;
		Ys1 = [X|Ys0]
	    )
	),
	in_between(Max, XLmax, XHmax),
	( Ys = [LastX] ->
	    LastX = Max
	; nonvar(MaxX) ->	% implies nonvar(Max)
	    true
	;
	    suspend(maxlist(Ys, Max), 4, [MaxX->max,Ys->min,Max->[max,min]])
	).


%----------------------------------------------------------------------
% occurrences(+Value, +List, ?N) -- Value occurs in List N times
% atmost(+N, +List, +Value) -- Value occurs in List at most N times
% atleast(+N, +List, +Value) -- Value occurs in List at least N times
%----------------------------------------------------------------------

:- comment(occurrences/3, [
    summary:"The value Value occurs in Vars N times",
    amode:occurrences(++,+,?),
    args:[
	"Value":"Atomic term",
	"Vars":"Collection (a la collection_to_list/2) of atomic terms or domain variables",
	"N":"Variable or integer"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    	  The value Value occurs in Vars N times.  Operationally:  N
	  gets updated to reflect the number of possible occurrences in the
	  collection.  Collection elements may get instantiated to Value, or
	  Value may be removed from their domain if required by N."),
    see_also:[atleast/3, atmost/3, collection_to_list/2]
    ]).

occurrences(Value, Vars, N) :-
	collection_to_list(Vars, List),
	!,
	N #>= 0,
	occurrences1(Value, List, N).
occurrences(Value, Vars, N) :-
	error(5, occurrences(Value, Vars, N)).

occurrences1(Value, List, N) :-
	var(Value),
	suspend(occurrences1(Value, List, N), 3, Value->inst).
occurrences1(Value, List, N) :-
	nonvar(Value),
	call_priority(occurrences(Value, List, N, state(0,List), _Susp), 2).


:- demon occurrences/5.
:- export portray(occurrences/5, tr_global_out/2, [goal]).

occurrences(Value, List, N, State, Susp) :-
        arg(1, State, Occ),
        arg(2, State, RestList),
	count_vars(Value, RestList, Occ, Lower, Occ, Upper, VarsWithValue),
	lwb(N, Lower),
	upb(N, Upper),
	( N == Lower ->
	    ( foreach(X, VarsWithValue), param(Value) do
		excl(X, Value)
	    ),
            kill_suspension(Susp)
	; N == Upper ->
	    ( foreach(X, VarsWithValue), param(Value) do
	    	X = Value
	    ),
            kill_suspension(Susp)
	; var(Susp) ->
	    suspend(
		    occurrences(Value, List, N, state(Lower,VarsWithValue), Susp),
		    4, [VarsWithValue->any, N->[min,max]], Susp)
	;
            setarg(1, State, Lower),
            setarg(2, State, VarsWithValue)
	).


% count_vars(+Value,+Vars,-Lower,-Upper,-VarsWithValue)
% Given an integer value and a list of finite domain variables
% Returns a lower and an upper bound of the times this value
% appear in the variable list as well as the variables which
% may or may not hold this value in the future.
% The lower bound refers to the number of times a variable was
% instantiated to the value.
% The upper bound refers to the number of uninstantiated variables
% which can still take this value.
% The VarsWithValue are the uninstantiated variables...

count_vars(_,[],Lower,Lower,Upper,Upper,[]).
count_vars(Value,[H|T],Lower1,Lower,Upper1,Upper,VarsWithValue) :-
	( H == Value ->
	    Upper2 is Upper1 + 1,		% H is instantiated to Value!
	    Lower2 is Lower1 + 1,
	    count_vars(Value,T,Lower2,Lower,Upper2,Upper,VarsWithValue)

	; check_in(Value,H) ->
	    Upper2 is Upper1 + 1,		% Value in domain
	    VarsWithValue = [H|MoreWithValue],
	    count_vars(Value,T,Lower1,Lower,Upper2,Upper,MoreWithValue)
	;
	    count_vars(Value,T,Lower1,Lower,Upper1,Upper,VarsWithValue)
	).


:- comment(atmost/3, [
	summary:"At most N elements of Vars have the value V.",
	template:"atmost(+N, ?Vars, +V)",
        kind:[constraint:[root:[ic,fd]]],
	desc:html("\
   This constraint ensures that at most N element of Vars have the value V.
   As soon as some domain variable from the collection is updated, this
   constraint is woken and it checks if the constraint is still satisfiable
   and if so, if it is already satisfied or not.
"),
	args:["+N" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
	      "+V" : "An integer"],
	fail_if:"   Fails if more than N elements of Vars are instantiated to V.",
	see_also:[atleast/3, occurrences/3, collection_to_list/2]]).

atmost(N, Vars, Value) :- var(N), !,
	suspend(atmost(N, Vars, Value), 3, N->inst).
atmost(N, Vars, Value) :- var(Value), !,
	suspend(atmost(N, Vars, Value), 3, Value->inst).
atmost(N, Vars, Value) :-
	integer(N),
	collection_to_list(Vars, List),
	!,
	call_priority(atmost(N, List, Value, state(0,List), _Susp), 2).
atmost(N, Vars, Value) :-
	error(5, atmost(N, Vars, Value)).

:- demon atmost/5.
:- export portray(atmost/5, tr_global_out/2, [goal]).
atmost(N, List, Value, State, Susp) :-
        arg(1, State, Occ),
        arg(2, State, RestList),
	count_vars(Value, N, RestList, Occ, Lower, Occ, Upper, VarsWithValue),
	( Lower > N ->
	    fail
	; Lower == N ->
	    ( foreach(X, VarsWithValue), param(Value) do
		X #\= Value
	    ),
            kill_suspension(Susp)
	; Upper =< N ->
            kill_suspension(Susp)
	; var(Susp) ->
	    suspend(
		    atmost(N, List, Value, state(Lower,VarsWithValue), Susp),
		    4, [VarsWithValue->inst], Susp)
	;
            setarg(1, State, Lower),
            setarg(2, State, VarsWithValue)
	).


:- comment(atleast/3, [
	summary:"At least N elements of Vars have the value V.",
	template:"atleast(+N, ?Vars, +V)",
        kind:[constraint:[root:[ic,fd]]],
	desc:html("\
   This constraint ensures that at least N element of Vars have the value V.
   As soon as some domain variable from the collection is updated, this
   constraint is woken and it checks if the constraint is still satisfiable
   and if so, if it is already satisfied or not.
"),
	args:["+N" : "An integer",
	      "?Vars" : "A collection (a la collection_to_list/2) of domain variables or integers",
	      "+V" : "An integer"],
	fail_if:"   Fails if less than N elements of Vars can take value V.",
	see_also:[atmost/3, occurrences/3, collection_to_list/2]]).

atleast(N, Vars, Value) :- var(N), !,
	suspend(atleast(N, Vars, Value), 3, N->inst).
atleast(N, Vars, Value) :- var(Value), !,
	suspend(atleast(N, Vars, Value), 3, Value->inst).
atleast(N, Vars, Value) :-
	integer(N),
	collection_to_list(Vars, List),
	!,
	call_priority(atleast(N, List, Value, state(0,List), _Susp), 2).
atleast(N, Vars, Value) :-
	error(5, atleast(N, Vars, Value)).

:- demon atleast/5.
:- export portray(atleast/5, tr_global_out/2, [goal]).
atleast(N, List, Value, State, Susp) :-
        arg(1, State, Occ),
        arg(2, State, RestList),
	count_vars(Value, N, RestList, Occ, Lower, Occ, Upper, VarsWithValue),
	Upper >= N,
	( Lower >= N ->
            kill_suspension(Susp)	% entailed
	; Upper == N ->
	    ( foreach(X, VarsWithValue), param(Value) do
	    	X = Value
	    ),
            kill_suspension(Susp)
	; var(Susp) ->
	    suspend(
		    atleast(N, List, Value, state(Lower,VarsWithValue), Susp),
		    4, [VarsWithValue->any], Susp)
	;
            setarg(1, State, Lower),
            setarg(2, State, VarsWithValue)
	).


count_vars(_,_,[],Lower,Lower,Upper,Upper,[]).
count_vars(Value,Max,[H|T],Lower1,Lower,Upper1,Upper,VarsWithValue) :-
	( H == Value ->
	    % H is instantiated to Value!
	    Lower2 is Lower1 + 1,
	    ( Lower2 > Max ->
		Lower = Lower2,			% stop early
		Upper = Lower, VarsWithValue = []	% don't care values
	    ;
		Upper2 is Upper1 + 1,
		count_vars(Value,Max,T,Lower2,Lower,Upper2,Upper,VarsWithValue)
	    )

	; check_in(Value,H) ->
	    Upper2 is Upper1 + 1,		% Value in domain
	    VarsWithValue = [H|MoreWithValue],
	    count_vars(Value,Max,T,Lower1,Lower,Upper2,Upper,MoreWithValue)
	; nonvar(H) ->
	    count_vars(Value,Max,T,Lower1,Lower,Upper1,Upper,VarsWithValue)
	; is_solver_var(H) ->
	    count_vars(Value,Max,T,Lower1,Lower,Upper1,Upper,VarsWithValue)
	;
	    % free variable 
	    Upper2 is Upper1 + 1,
	    VarsWithValue = [H|MoreWithValue],
	    count_vars(Value,Max,T,Lower1,Lower,Upper2,Upper,MoreWithValue)
	).


%----------------------------------------------------------------------
% sumlist(+List, ?Sum)
%----------------------------------------------------------------------

:- comment(sumlist/2, [
    summary:"The sum of the list elements is Sum",
    amode:sumlist(+,?),
    args:[
	"List":"List of integers or domain variables",
	"Sum":"Variable or integer"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("<P>\
    	  The sum of the list elements is Sum.  This constraint is
	  more efficient than the general arithmetic constraint if
	  the list is long and Sum is not constrained frequently.
	  </P><P>
	  Any input variables which do not already have finite bounds will
	  be given default bounds of -10000000 to 10000000.</P>"),
    see_also:[ordered/2,ordered_sum/2,(#=)/2]
    ]).

sumlist(List, Sum) :-
	% We have to wrap this in a call_priority in case calls to
	% get_finite_bounds/3 modify the variable, which triggers some
	% propagation which modifies another variable, triggering a
	% sumlist_update too early and throwing out our sum calculations.
	call_priority((
		(
		    foreach(X, List),
		    fromto(0, Min0, Min1, Min),		% compute Min and Max
		    fromto(0, Max0, Max1, Max),		% of the list sum
		    fromto(Vars, Vars1, Vars0, []),
		    param(ListSum)
		do
		    ( var(X) -> 
			get_finite_bounds(X, XL, XH),
			Min1 is Min0 + XL,
			Max1 is Max0 + XH,
			Vars1 = [X|Vars0],
			% put a demon on each variable
			suspend(sumlist_update(X, XL..XH, ListSum, S),
				2, [X->[min,max]], S)
		    ;
			Min1 is Min0 + X,
			Max1 is Max0 + X,
			Vars1 = Vars0
		    )
		),
		[Sum,ListSum] :: Min..Max,
		sumlist_prop(ListSum, Sum, v(Vars), _Susp)
	    ), 2).


:- demon(sumlist_update/4).
sumlist_update(X, XLXH, ListSum, Susp) :-
	XLXH = XL..XH,
	( var(X) ->
	    get_bounds(X, XL1, XH1),	% guaranteed already finite
	    setarg(1, XLXH, XL1),
	    setarg(2, XLXH, XH1)
	;
	    XL1 = X, XH1 = X,
	    kill_suspension(Susp)
	),
	% if this isn't finite something is wrong and we're screwed anyway
	get_bounds(ListSum, SumL, SumH),
	SumL1 is SumL + XL1 - XL,
	SumH1 is SumH + XH1 - XH,
	in_between(ListSum, SumL1, SumH1).


:- demon(sumlist_prop/4).
sumlist_prop(ListSum, Sum, VList, Susp) :-
	VList = v(List),
	get_bounds(ListSum, ListSumL, ListSumH),
	get_finite_bounds(Sum, SumL, SumH),
	( ListSumL > SumL ->			% List constrains Sum
	    lwb(Sum, ListSumL)
	; ListSumL < SumL ->			% Sum constrains List
	    ( foreach(X, List), param(SumL), param(ListSumH) do
		get_bounds(X, _XL, XH),
		XL1 is SumL-(ListSumH-XH),
		lwb(X, XL1)
	    )
	;
	    true
	),
	( ListSumH < SumH ->			% List constrains Sum
	    upb(Sum, ListSumH)
	; ListSumH > SumH ->			% Sum constrains List
	    ( foreach(X, List), param(SumH), param(ListSumL) do
		get_bounds(X, XL, _XH),
		XH1 is SumH-(ListSumL-XL),
		upb(X, XH1)
	    )
	;
	    true
	),
	( nonvar(ListSum) ->			% (re)suspend if necessary
	    kill_suspension(Susp)
	; nonvar(Susp) ->
	    true
	;
	    suspend(sumlist_prop(ListSum, Sum, VList, Susp), 3,
			[ListSum-Sum->[min,max]], Susp)
	).


%----------------------------------------------------------------------
% sum_ge_zero(+List)
%
% X1::1..5,X2::3..8,X3:: -20..20,sum_ge_zero([X1,X2,X3]).
%----------------------------------------------------------------------

sum_ge_zero(List) :-
	SumInfo = si(_Min,_Max,none),
	track_list_range(List, SumInfo, Vars),
	suspend(sum_ge_zero_prop(Vars, SumInfo, S), 3, [Vars->max], S),
	sum_ge_zero_prop(Vars, SumInfo, S).

% Triggered when some upper bound(s) changed.
% Lower bounds may also have changed.
% Sums in SumInfo are already updated.
:- demon sum_ge_zero_prop/3.
sum_ge_zero_prop(Vars, SumInfo, Susp) :-
	SumInfo = si(Min,Max,Free),
	Max >= 0,			% may fail
	( Min >= 0 ->
	    kill_suspension(Susp)	% entailed!
	;
	    % X guaranteed already finite
	    ( Free = free(X), get_bounds(X,XL,XH), XH >= -(Min-XL) ->
		% X is still free (upb not constraining)
		XL1 is XH-Max,
		lwb(X, XL1)
	    ;
		% do the linear thing:
		% update all lower bounds or find first free upper bound
		call_priority(
		    find_first_free_bound_and_update_lwbs(Vars, Min, Max, NewFree),
		    2),
		setarg(3, SumInfo, NewFree)
	    ),
	    wake
	).

% don't forget to wake/0 after return from this one
find_first_free_bound_and_update_lwbs([], _Min, _Max, none).
find_first_free_bound_and_update_lwbs([X|Xs], Min, Max, Free) :-
	get_bounds(X, XL, XH),	% guaranteed already finite
	XL1 is XH-Max,
	lwb(X, XL1),
	( XH >= -(Min-XL) ->
	    % there was actually a free bound, no further updates possible
	    Free=free(X)
	;
	    find_first_free_bound_and_update_lwbs(Xs, Min, Max, Free)
	).

track_list_range(List, SumInfo, Vars) :-
	arg(1, SumInfo, Min),
	arg(2, SumInfo, Max),
	(
	    foreach(X, List),
	    fromto(0, Min0, Min1, Min),		% compute Min and Max
	    fromto(0, Max0, Max1, Max),		% of the list sum
	    fromto(Vars, Vars1, Vars0, []),
	    param(SumInfo)
	do
	    ( var(X) -> 
		get_finite_bounds(X, XL, XH),
		Min1 is Min0 + XL,
		Max1 is Max0 + XH,
		Vars1 = [X|Vars0],
		suspend(range_update(X, XL..XH, SumInfo, S), 2,
			[X->[min,max]], S)	% put a demon on each variable
	    ;
		Min1 is Min0 + X,
		Max1 is Max0 + X,
		Vars1 = Vars0
	    )
	).

% Demon to maintain the range of a list sum incrementally
:- demon(range_update/4).
range_update(X, XLXH, ListSum, Susp) :-
	XLXH = XL..XH,
	arg(1, ListSum, SumL),
	arg(2, ListSum, SumH),
	( var(X) ->
	    get_bounds(X, XL1, XH1)	% guaranteed already finite
	;
	    XL1 = X, XH1 = X,
	    kill_suspension(Susp)
	),
	( XL1 > XL ->
	    setarg(1, XLXH, XL1),
	    SumL1 is SumL + (XL1 - XL),
	    setarg(1, ListSum, SumL1)
	;
	    true
	),
	( XH1 < XH ->
	    setarg(2, XLXH, XH1),
	    SumH1 is SumH + (XH1 - XH),
	    setarg(2, ListSum, SumH1)
	;
	    true
	).


%----------------------------------------------------------------------
% Auxliliary predicates
%----------------------------------------------------------------------

in_between(X, Min, Max) :-
	lwb(X, Min),
	upb(X, Max).



%----------------------------------------------------------------------
% ordered_sum(+List, ?Sum)
%
% length(L, 3), L::0..20, ordered_sum(L, 10).
%	L = [_183{[0..3]}, _196{[0..5]}, _209{[4..10]}]
% length(L, 3), L:: -20..0, ordered_sum(L, -10).
%	L = [_183{[-10..-4]}, _196{[-5..0]}, _209{[-3..0]}]
%----------------------------------------------------------------------

:- comment(ordered_sum/2, [
    summary:"The list elements are ordered and their sum is Sum",
    amode:ordered_sum(+,?),
    args:[
	"List":"List of integers or domain variables",
	"Sum":"Variable or integer"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    This constraint is declaratively equivalent to:
<PRE>
	ordered_sum(List, Sum) :-
	    ordered(=<, List),
	    sum(List) #= Sum.
</PRE>
    However, additional propagation is performed.
    </P><P>
    Any input variables which do not already have finite bounds will
    be given default bounds of -10000000 to 10000000.</P>"),
    see_also:[ordered/2,sumlist/2]
    ]).

ordered_sum(Xs, Sum) :-
	ordered(=<, Xs),
	sum(Xs) #= Sum,
	length(Xs, N),
	reverse(Xs, RXs),
	suspend(ordered_sum_u( Xs, N, Sum, S1), 4, [ Xs->min,Sum->max], S1),
	suspend(ordered_sum_l(RXs, N, Sum, S2), 4, [RXs->max,Sum->min], S2),
	schedule_woken([S1,S2]), wake.


:- demon ordered_sum_u/4.
ordered_sum_u(Xs, N, Sum, Susp) :-
	get_finite_bounds(Sum, _, S),
	(
	    foreach(X, Xs),
	    fromto(0, MinSum0, MinSum1, MinSum),
	    fromto(N, N0, N1, _),
	    param(S)
	do
	    get_finite_bounds(X, XL, _),
	    XH is fix(floor((S-MinSum0)/N0)),
%	    XH is fix(floor((S-MinSum0-N0+1)/N0)),	% if strictly ordered
	    upb(X, XH),
	    MinSum1 is MinSum0 + XL,
	    N1 is N0-1
	),
	lwb(Sum, MinSum),
	( nonground([Sum|Xs]) -> true ; kill_suspension(Susp) ),
	wake.

:- demon ordered_sum_l/4.
ordered_sum_l(Xs, N, Sum, Susp) :-
	get_finite_bounds(Sum, S, _),
	(
	    foreach(X, Xs),
	    fromto(0, MaxSum0, MaxSum1, MaxSum),
	    fromto(N, N0, N1, _),
	    param(S)
	do
	    get_finite_bounds(X, _, XH),
	    XL is fix(ceiling((S-MaxSum0)/N0)),
	    lwb(X, XL),
	    MaxSum1 is MaxSum0 + XH,
	    N1 is N0-1
	),
	upb(Sum, MaxSum),
	( nonground([Sum|Xs]) -> true ; kill_suspension(Susp) ),
	wake.


%----------------------------------------------------------------------
% lexicographic ordering between two lists
%----------------------------------------------------------------------

:- comment(lex_le/2, [
    summary:"List1 is lexicographically less or equal to List2",
    amode:lex_le(+,+),
    args:[
	"List1":"List of integers or domain variables",
	"List2":"List of integers or domain variables"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of List1 strictly smaller
	than the first element of List2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails."),
    see_also:[ordered/2,_:lex_le/2,lex_lt/2],
    eg:"\
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]).		% X::0..2
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), X=2.	% Y::0..3
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), X#>2.	% fail
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), X#<2.	% true
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), Y=3.	% X::0..2
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), Y#>3.	% X::0..1
    L=[X, Y, Z], L :: 0..9, lex_le(L, [2, 3, 1]), Y#<3.	% X::0..2
    lex_le([2, 3, 1], [3]).					% true
    " ]).

lex_le(XVector,YVector):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        lex_demon(XList,YList,0,_).


:- comment(lex_lt/2, [
    summary:"List1 is lexicographically less than  List2",
    amode:lex_lt(+,+),
    args:[
	"List1":"List of integers or domain variables",
	"List2":"List of integers or domain variables"
    ],
    kind:[constraint:[root:[ic,fd]]],
    see_also:[ordered/2,_:lex_lt/2,lex_le/2],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of List1 strictly smaller
	than the first element of List2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached) is strictly smaller than any existing element.
")
]).

lex_lt(XVector,YVector):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        lex_demon(XList,YList,1,_).


:- deprecated(lexico_le/2, "Use lex_le/2").
lexico_le(Xs, Ys) :- lex_le(Xs, Ys).  % backwards compatibility


% lex_demon(Xs, Ys, 0, Susp)	for lex_le
% lex_demon(Xs, Ys, 1, Susp)	for lex_lt
%
% This demon monitors the first two (non-identical) var-var pairs of
% list elements X1-Y1 and X2-Y2, and implements the following rules:
%
% X1-Y1 X2-Y2		lex_le			lex_lt
%
%   =    n/a 	===>	entailed		false
%   <    any 	===>	entailed		entailed	(lazy)
%   >    any 	===>	false			false
%   ?     >  	===>	X1 #<  Y1		X1 #<  Y1
%   ?     <  	===>	X1 #=< Y1		X1 #=< Y1
%   ?     =  	===>	X1 #=< Y1		X1 #<  Y1
%   ?     ?  	===>	impose X1=<Y1,suspend	impose X1=<Y1,suspend
%
% Identical X-Y pairs are dropped from the lists in recursive calls of
% lex_demon.  Note that the X1<Y1 entailment case is detected lazily,
% which also has the advantage that the demon doesn't wake itself when
% imposing upb(X1).  Strict is 0 for lex_le, 1 for lex_lt.

:- export portray(lex_demon/4, tr_global_out/2, [goal]).
:- demon lex_demon/4.
:- set_flag(lex_demon/4, priority, 3).
:- set_flag(lex_demon/4, run_priority, 2).
lex_demon(Xs, Ys, Strict, S) :-
	advance(Xs, Ys, R1, Skipped, X1Xs2, Y1Ys2, MinX1, MaxY1),
	( R1==(<) ->
	    kill_suspension(S)
	; R1==(=) ->
	    Strict == 0,		% fail for lex_lt
	    kill_suspension(S)		% entailed for lex_le
	; R1==(>) ->
	    fail
	;
	    X1Xs2 = [X1|Xs2], Y1Ys2 = [Y1|Ys2],
	    advance(Xs2, Ys2, R2, Skipped, X2Xs3, Y2Ys3, _MinX2, _MaxY2),
	    ( R2==(<) ->
		kill_suspension(S),
		X1 #=< Y1
	    ; R2==(=) ->
		kill_suspension(S),
		X1 #=< Y1-Strict	% #=< for lex_le, #< for lex_lt
	    ; R2==(>) ->
		kill_suspension(S),
		X1 #< Y1
	    ;
                ( var(Skipped), nonvar(S) ->
		    true		% identical resuspend
		;
		    kill_suspension(S),	% if any
		    X2Xs3 = [X2|_], Y2Ys3 = [Y2|_],
                    suspend(lex_demon([X1|X2Xs3], [Y1|Y2Ys3], Strict, S1), 0,
                        [[](X1,X2,Y2)->min,	% not Y1!
			 [](Y1,X2,Y2)->max,	% not X1!
			 [](X1,Y1,X2,Y2)->bound], S1)
		),
		% Impose X1=<Y1
		% This will wake lex_demon iff it leads to X1 or Y1 being bound
		upb(X1, MaxY1), lwb(Y1, MinX1)
	    )
	).


    % Skip over identical elements in Xs and Ys (indicating this in Skipped),
    % evaluate the first difference, and return information about it.
    advance([], [],    Res, _, _, _, _, _) ?- !, Res = (=).
    advance([], [_|_], Res, _, _, _, _, _) ?- !, Res = (<).
    advance([_|_], [], Res, _, _, _, _, _) ?- !, Res = (>).
    advance(XXs, YYs, Res, Skipped, XXs1, YYs1, Xmin, Ymax) :-
	XXs = [X|Xs], YYs = [Y|Ys],
	( X==Y ->
	    Skipped=true,
	    get_bounds(X, _, _),	% force default domain on X (if necessary)
	    advance(Xs, Ys, Res, _, XXs1, YYs1, Xmin, Ymax)
	;
	    get_bounds(X, Xmin, Xmax),
	    get_bounds(Y, Ymin, Ymax),
	    ( Xmax < Ymin -> Res = (<)
	    ; Xmin > Ymax -> Res = (>)
	    ; XXs1=XXs, YYs1=YYs, Res = (?)
	    )
	).


%----------------------------------------------------------------------
% sorted(?UnsortedList, ?SortedList)
%----------------------------------------------------------------------

:- comment(sorted/2, [
    summary:"Sorted is a sorted permutation of List",
    amode:sorted(+,+),
    amode:sorted(+,-),
    amode:sorted(-,+),
    args:["List":"List of domain variables or integers",
    	"Sorted":"List of domain variables or integers"],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    Declaratively: The two lists have the same length and Sorted is a
    sorted permutation of List.
<P>
    Operationally:  the elements in both lists are constrained such
    that their domains are consistent with the assumption that the
    list Sorted is the sorted version of the list List.
<P>
    One of the two arguments can be uninstantiated or partial lists
    at call time.
<P>
    Any input variables which do not already have finite bounds will
    be given default bounds of -10000000 to 10000000.
    "),
    eg:"
    ?- length(Xs,4), Xs::0..100, sorted(Xs,Ys), Xs = [8,20|_].

    Xs = [8, 20, _340{[0..100]}, _353{[0..100]}]
    Ys = [_431{[0..8]}, _413{[0..20]}, _523{[8..100]}, _621{[20..100]}]


    ?- length(Ys,4), Ys::0..100, sorted(Xs,Ys), Ys = [8,20|_].

    Xs = [_464{[8..100]}, _477{[8..100]}, _490{[8..100]}, _503{[8..100]}]
    Ys = [8, 20, _340{[20..100]}, _353{[20..100]}]
    ",
    see_also:[sorted/3,ordered/2]
    ]).

:- comment(sorted/3, [
    summary:"Sorted is a sorted permutation (described by Positions) of List",
    amode:sorted(+,?,?),
    amode:sorted(?,+,?),
    amode:sorted(?,?,+),
    args:["List":"List of domain variables or integers",
    	"Sorted":"List of domain variables or integers",
    	"Positions":"List of domain variables or integers"],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    Declaratively:  Sorted is a sorted permutation of List.  Positions
    is a list whose elements range from 1 to N (where N is the length
    of the lists) indicating the position of each unsorted list
    element within the sorted list.  The positions are all different. 
    The three lists are constrained to have the same length.
<P>
    Operationally:  the elements in all three lists are constrained
    such that their domains are consistent with the declarative
    meaning.
<P>
    Two of the three arguments can be uninstantiated or partial lists
    at call time.
<P>
    Any input variables which do not already have finite bounds will
    be given default bounds of -10000000 to 10000000.
    "),
    eg:"
    ?- length(Xs,4), Xs::0..100, sorted(Xs,Ys,Ps), Xs = [8,20|_].

    Xs = [8, 20, _346{[0..100]}, _359{[0..100]}]
    Ys = [_445{[0..8]}, _427{[0..20]}, _537{[8..100]}, _635{[20..100]}]
    Ps = [_882{[1..3]}, _895{[2..4]}, _908{[1..4]}, _921{[1..4]}]
    ",
    see_also:[sorted/2,ordered/2]
    ]).

sorted(Us, Ss) :-
	( foreach(_,Us), foreach(_,Ss) do true ),
	SsArr =.. [[]|Ss],
	functor(SsArr, [], N),
	N > 0,
	!,
	ordered(=<, Ss),
	% This gets the global lower and upper bounds while also ensuring
	% finiteness...
	arg(1,SsArr,S1), get_finite_bounds(S1,MinDom,_),
	arg(N,SsArr,Sn), get_finite_bounds(Sn,_,MaxDom),
	Us :: MinDom..MaxDom,
	suspend(sorted_demon(Us, SsArr, Susp), 4,
		[Us-Ss->[min,max]], Susp),
	sorted_demon(Us, SsArr, Susp).
sorted(Us, Ss) :-
	error(6, sorted(Us, Ss)).

% This demon does two symmetric passes, each pass can retrigger the other.
% Since both passes share the same pre-sorting, we have not separated them.

:- demon sorted_demon/3.
sorted_demon(Us, SsArr, Susp) :-
	extract_bounds(Us, Ts),

	% Pass1: traverse along ascending minima of the unsorted variables.
	% Note that all Ts must be finite integers: sort will do the wrong
	% thing if we have mixed numeric types (e.g. include infinities).
	sort(1, =<, Ts, TsAscMin),		% ascending min
	sort(2, =<, Ts, TsAscMax),		% ascending max
	TsAscMaxArr =.. [[]|TsAscMax],
	(
	    foreach(t(UiMin,_,Ui), TsAscMin),
	    count(I,1,N),
	    fromto(1, MinPos0, MinPos2, _),	% MinPos >= I
	    param(TsAscMaxArr,SsArr)
	do
	    % min(Ui) is a lower bound for the corresponding sorted variable Si
	    arg(I, SsArr, Si),			% SsArr[I] #>= UiMin,
	    lwb(Si, UiMin),
	    % increment MinPos beyond all unsorted and sorted variables that
	    % must be before Ui (i.e. whose max is below min(Ui))
	    find_first_max_above_a(UiMin, TsAscMaxArr, MinPos0, MinPos1),
	    find_first_max_above_va(UiMin, SsArr, MinPos1, MinPos2),
	    % the sorted vars may constrain the unsorted ones which
	    % can be at that position. Remember that MinPos >= I
	    arg(MinPos2, SsArr, SMinPos),
	    get_lwb(SMinPos, SMinPosMin),
	    lwb(Ui, SMinPosMin)
	),
	wake,

	% Pass2: this is completely symmetric to pass 1,
	% traverse along descending maxima of the unsorted variables
	TsAscMinArr =.. [[]|TsAscMin],		% ascending min
	reverse(TsAscMax, TsDesMax),		% descending max
	(
	    foreach(t(_,UiMax,Ui),TsDesMax),
	    for(I,N,1,-1),
	    fromto(N, MaxPos0, MaxPos2, _),	% MaxPos >= I
	    param(TsAscMinArr,SsArr)
	do
	    arg(I, SsArr, Si),			% SsArr[I] #=< UiMax,
	    upb(Si, UiMax),
	    find_first_min_below_a(UiMax, TsAscMinArr, MaxPos0, MaxPos1),
	    find_first_min_below_va(UiMax, SsArr, MaxPos1, MaxPos2),
	    arg(MaxPos2, SsArr, SMaxPos),
	    get_upb(SMaxPos, SMaxPosMax),
	    upb(Ui, SMaxPosMax)
	),
	wake,

	( nonground(SsArr-Us) -> true ; kill_suspension(Susp) ).


% This is the same with position variables

sorted(Us, Ss, Ps) :-
	( foreach(_,Us), foreach(_,Ss), foreach(_,Ps) do true ),
	SsArr =.. [[]|Ss],
	functor(SsArr, [], N),
	N > 0,
	!,
	ordered(=<, Ss),
	% This gets the global lower and upper bounds while also ensuring
	% finiteness...
	arg(1,SsArr,S1), get_finite_bounds(S1,MinDom,_),
	arg(N,SsArr,Sn), get_finite_bounds(Sn,_,MaxDom),
	Us :: MinDom..MaxDom,
	Ps :: 1..N,
	alldifferent(Ps),
	suspend(sorted_demon(Us, SsArr, Ps, Susp), 4,
		[Us-Ss-Ps->[min,max]], Susp),
	sorted_demon(Us, SsArr, Ps, Susp).
sorted(Us, Ss, Ps) :-
	error(6, sorted(Us, Ss, Ps)).


:- demon sorted_demon/4.
sorted_demon(Us, SsArr, Ps, Susp) :-
	extract_bounds(Us, Ps, Ts),

	% Pass1: traverse along ascending minima of the unsorted variables.
	% Note that all Ts must be finite integers: sort will do the wrong
	% thing if we have mixed numeric types (e.g. include infinities).
	sort(1, =<, Ts, TsAscMin),		% ascending min
	sort(2, =<, Ts, TsAscMax),		% ascending max
	TsAscMaxArr =.. [[]|TsAscMax],
	(
	    foreach(t(UiMin,_,Ui,Pi), TsAscMin),
	    count(I,1,N),
	    fromto(1, MinPos0, MinPos2, _),	% MinPos >= I
	    param(TsAscMaxArr,SsArr)
	do
	    % min(Ui) is a lower bound for the corresponding sorted variable Si
	    arg(I, SsArr, Si),			% SsArr[I] #>= UiMin,
	    lwb(Si, UiMin),
	    % increment MinPos beyond all unsorted and sorted variables that
	    % must be before Ui (i.e. whose max is below min(Ui))
	    find_first_max_above_a(UiMin, TsAscMaxArr, MinPos0, MinPos1),
	    find_first_max_above_va(UiMin, SsArr, MinPos1, MinPos2),
	    lwb(Pi, MinPos2),
	    get_lwb(Pi, MinPos),		% actual MinPos
	    % the sorted vars may constrain the unsorted ones which
	    % can be at that position. Remember that MinPos >= I
	    arg(MinPos, SsArr, SMinPos),
	    get_lwb(SMinPos, SMinPosMin),
	    lwb(Ui, SMinPosMin)
	),
	wake,

	% Pass2: this is completely symmetric to pass 1,
	% traverse along descending maxima of the unsorted variables
	TsAscMinArr =.. [[]|TsAscMin],		% ascending min
	reverse(TsAscMax, TsDesMax),		% descending max
	(
	    foreach(t(_,UiMax,Ui,Pi),TsDesMax),
	    for(I,N,1,-1),
	    fromto(N, MaxPos0, MaxPos2, _),	% MaxPos =< I
	    param(TsAscMinArr,SsArr)
	do
	    arg(I, SsArr, Si),			% SsArr[I] #=< UiMax,
	    upb(Si, UiMax),
	    find_first_min_below_a(UiMax, TsAscMinArr, MaxPos0, MaxPos1),
	    find_first_min_below_va(UiMax, SsArr, MaxPos1, MaxPos2),
	    upb(Pi, MaxPos2),
	    get_upb(Pi, MaxPos),
	    arg(MaxPos, SsArr, SMaxPos),
	    get_upb(SMaxPos, SMaxPosMax),
	    upb(Ui, SMaxPosMax)
	),
	wake,

	( nonground(SsArr-Us) -> true ; kill_suspension(Susp) ).


    extract_bounds([], []).
    extract_bounds([X|Xs], [t(Min,Max,X)|Ts]) :-
	get_bounds(X, Min, Max),
	extract_bounds(Xs, Ts).

    extract_bounds([], [], []).
    extract_bounds([X|Xs], [P|Ps], [t(Min,Max,X,P)|Ts]) :-
	get_bounds(X, Min, Max),
	extract_bounds(Xs, Ps, Ts).

    find_first_max_above_a(Min, Arr, I0, I) :-
	arg([I0,2], Arr, Max),
    	( Min =< Max ->
	    I = I0
	;
	    I1 is I0 + 1, I1 =< functor(Arr,[]),
	    find_first_max_above_a(Min, Arr, I1, I)
	).

    find_first_min_below_a(Max, Arr, I0, I) :-
	arg([I0,1], Arr, Min),
    	( Max >= Min ->
	    I = I0
	;
	    I1 is I0 - 1, I1 >= 1,
	    find_first_min_below_a(Max, Arr, I1, I)
	).

    % the same, but operating on an array of fd variables
    find_first_max_above_va(MinU, SsArr, I0, I) :-
	arg(I0, SsArr, Si),
	get_upb(Si, SiMax0),
    	( MinU =< SiMax0 ->
	    I = I0
	;
	    I1 is I0 + 1, I1 =< functor(SsArr,[]),
	    find_first_max_above_va(MinU, SsArr, I1, I)
	).

    find_first_min_below_va(MaxU, SsArr, I0, I) :-
	arg(I0, SsArr, Si),
	get_lwb(Si, SiMin0),
    	( MaxU >= SiMin0 ->
	    I = I0
	;
	    I1 is I0 - 1, I1 >= 1,
	    find_first_min_below_va(MaxU, SsArr, I1, I)
	).


%----------------------------------------------------------------------
% DESCRIPTION:
%
%	This is a strong version of alldifferent/1.
%
%	It looks at all pairs of minimum and maximum values in
% 	the domains of its variables, for each pair (min,max)
%	it finds the number of variables whose value necessary lies
%	between min and max, and the union of their domains.
%	if the number of variables equals the size of the union
%	no other variable may take a value within that domain.
%	If the number of variable is greater than the union the
%	constraint fails.
%
%	In fact, we consider slightly more subsets than just the groups
%	corresponding to (min,max) pairs - we traverse each (min,max)
%	group from smaller to larger domain sizes and detect also the
%	exhaustion of any prefix of the group.
%
%	The algorithm is based roughly on:
%	Jean Francois Puget
%	A fast algorithm for the bound consistency of the alldiff constraint
%	in AAAI 1998.
%----------------------------------------------------------------------


:- comment(alldifferent/1, [
    amode: alldifferent(+),
    args: [
	"Vars": "A collection (a la collection_to_list/2) of variables or integers"
    ],
    summary:"All members of Vars are different",
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    Constrains all elements of a collection to be pairwise different (and
    integral).  This is an implementation with the same semantics as the
    standard alldifferent/1 constraint, but with stronger propagation
    behaviour.  It checks for exhaustion of all sub-ranges of possible
    values.
    </P><P>
    The algorithm is based roughly on: Jean Francois Puget, A fast algorithm
    for the bound consistency of the alldiff constraint, AAAI 1998, but
    makes some additional inferences based on domain size.
    </P><P>
    Any input variables which do not already have finite bounds will
    be given default bounds of -10000000 to 10000000.</P>"),
    eg:"
    ?- length(Xs,5), Xs::1..4, alldifferent(Xs).
    no (more) solution.


    ?- [X1,X2]::1..2, [X3,X4,X5]::1..5, alldifferent([X1,X2,X3,X4,X5]).

    X1 = X1{[1, 2]}
    X2 = X2{[1, 2]}
    X3 = X3{[3..5]}
    X4 = X4{[3..5]}
    X5 = X5{[3..5]}

    Delayed goals:
        alldifferent([X1{[1, 2]}, X2{[1, 2]}], 1)
        alldifferent([X3{[3..5]}, X4{[3..5]}, X5{[3..5]}], 1)
    ",
    see_also:[_:alldifferent/1,alldifferent/2,collection_to_list/2]
    ]).

alldifferent(List) :-
	alldifferent(List, 1).

:- comment(alldifferent/2, [
    amode: alldifferent(+,++),
    args: [
	"Vars": "A collection (a la collection_to_list/2) of variables or integers",
	"Capacity": "Maximum number of times a value can appear in Vars"
    ],
    summary:"Vars contains at most Capacity elements of each value",
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    This is a generalization of alldifferent/1.  It allows repeated elements
    in the collection, but there can be no more than Capacity elements with
    a particular value.
    </P><P>
    Any input variables which do not already have finite bounds will
    be given default bounds of -10000000 to 10000000.</P>"),
    see_also:[alldifferent/1, collection_to_list/2]
    ]).


% We should do some optimisation for constants in the list, at least
% for the Cap=1 case.

:- local struct(dkey(max,mmin,size)).	% argument order important!
:- local struct(dvar(key:dkey,min,var)).

alldifferent([], _) ?- !.
alldifferent([_], _) ?- !.
alldifferent([X,Y], 1) ?- !,
	X #\= Y.
alldifferent(Vars, Cap) :-
	collection_to_list(Vars, List),
	!,
	( foreach(dvar{max:Max,mmin:MMin,size:Size,min:Min,var:Var},UnsortedTs),		% create the t-structures to work with
	  foreach(Max,UnsortedMaxs),
	  count(_,1,NTotal),
	  foreach(Var,List)
	do
	    get_finite_bounds(Var,Min,Max),
	    MMin is -Min,
	    get_size(Var,Size)
	),
	( Cap >= NTotal ->
	    true	% not constraining at all
	;
	    alldifferent(List, Cap, UnsortedTs, UnsortedMaxs)
	).
alldifferent(Vars, Cap) :-
	error(5, alldifferent(Vars, Cap)).

alldifferent(Vars, Cap, UnsortedTs, UnsortedMaxs) :-
	% Now sort all items in order of increasing Max,
	% then decreasing Min, then increasing domain size.
	% (the latter gives slightly better chances of detecting additional
	% exhausted sets that are smaller than the whole Min..Max group)
	sort(key of dvar,=<,UnsortedTs,TsByMax),
	% all max domains
	sort(0,<,UnsortedMaxs,Maxs),

	( fromto(Maxs,[Max|Maxs0],Maxs1,[]),			% The different maxima, increasing
	  fromto(TsByMax,TsGeMax,TsGtMax,_),			% The remaining items (maximum >= Max)
	  fromto([],TsLePrevMax,TsLeMax,_),			% Items already processed (maximum < Max)
	  fromto(0,TsLePrevMaxSize,TsLeMaxSize,_),		% ... and their number
	  fromto([],Mins0,Mins,_),				% The minima we have seen so far
	  param(Propagated,Cap)					% Flag indicating abort due to propagation
	do
	    le_than_max(Max,TsGeMax,TsEqMax,TsGtMax,		% TsEqMax is the chunk we process in this iteration
		    TsLePrevMaxSize,TsLeMaxSize),
	    merge(min of dvar,>=,TsLePrevMax,TsEqMax,TsLeMax),	% add it to the ones we already had, decreasing min, max now unordered
	    get_minima(TsEqMax,NewMins),			% compute the new set of minima for this group
	    merge(0,>,Mins0,NewMins,Mins),

	    % Don't want to have to support empty domains (inefficient in
	    % IC) so we simply extract the first domain that is going to be
	    % unioned with the empty domain, and use that instead.
	    %empty_domain(EmptyDom),
	    TsLeMax = [dvar{var:InitialDomVar} | _],
	    get_domain(InitialDomVar, InitialDom),
	    ( fromto(Mins,[Min|M0],M,[]),			% Iterate over decreasing minima (all =< fixed Max)
	      fromto(TsLeMax,TsLeMin,TsLtMin,TsLtMinRest),	% List starts with Ts for current Min followed by lower ones
	      fromto(0, NVarsIn, NVars,_),			% Accumulate number of items in range Min..Max
	      fromto(InitialDom, DomIn, Dom, UnionDom),		% ... and their domain union
	      param(PropagatedAfter,TsLeMaxSize,Cap)
	    do
		% process the list prefix which is equal to Min (this may fail)
		process_prefix_ge_min(Min, TsLeMin, TsLtMin, TsLeMaxSize, Cap,
			DomIn, Dom, NVarsIn, NVars, PropagatedAfter),

		( nonvar(PropagatedAfter) ->			% Propagation: abort loop
		    M = []
		; var(NVars) ->					% early cutoff: abort loop
		    M = []
		;
		    M = M0					% continue
		)
	    ),
	    ( nonvar(PropagatedAfter) ->
		( [TsGtMax|TsLtMinRest] == [[]|[]]  ->
		    % Exhausted subset is the whole set, abort loop & resuspend
		    true
		;
		    % Suspend the exhausted subset as a new alldifferent. We
		    % have just analysed it and know there is nothing to do!
		    vars_in_first_n(PropagatedAfter, TsLeMax, VsInMinToMax),
		    ( VsInMinToMax == [] -> true
		    ; suspend(alldifferent(VsInMinToMax,Cap), 4, VsInMinToMax->any)
		    ),

		    % Remove exhausted subset from the other items
		    call_priority((
			get_subtract_domain_rep(UnionDom,SubtractDomRep),
			remove_domain(SubtractDomRep,TsLtMinRest),
			remove_domain(SubtractDomRep,TsGtMax)
		    ), 2),

		    % Create new alldifferent for the other items.
		    % Propagated-flag will prevent original from resuspending!
		    append(TsLtMinRest,TsGtMax,NewTs),
		    ts_to_vars(NewTs,NewVars),
		    alldifferent(NewVars,Cap),
		    Propagated = true
		),
	    	Maxs1=[]					% abort loop
	    ;
	    	Maxs1=Maxs0
	    )
	),
	( nonvar(Propagated) ->
	    true
	;
	    suspend(alldifferent(Vars,Cap),4,Vars->any)
	).

	    
% process_prefix_ge_min operates on a prefix of the list TTs.
% TTs is a list of items with all the same Max and decreasing Mins.
% We process (at most) all the elements with minimum Min, which are
% at the beginning of the list.
% There are 4 possible outcomes:
% failure:
%	At some point, there are more items than the union of their domains
% var(Prop),nonvar(NVars):
%	No propagation opportunity found with the current Min.
%	RestTs: list of remaining items (with smaller minimum than Min)
%	NVars: numbers of items processed (= items with minimum Min)
%	Dom: the domain union
% var(Prop),var(NVars):
%	early cutoff, we don't need to process further Mins with this Max
%	because the domain union is already big enough for all items in
%	this range (>CutOffSize).
% nonvar(Prop),var(NVars):
%	propagation opportunity detected for the first Prop items,
%	i.e. their domain union is equal to the number of items.
%	RestTs: list of remaining items with this Max
%	Dom: the domain union

process_prefix_ge_min(_Min,[],[],_,_,_,_Dom,N,N,_Prop).
process_prefix_ge_min(Min,TTs,RestTs,CutOffSize,Cap,Dom0,Dom,NVars0,NVars,Prop) :-
	TTs = [dvar{min:MinT,var:V}|Ts],
	( MinT >= Min ->
	    NVars1 is NVars0+1,
	    get_domain(V,DomV),
	    domain_union(Dom0, DomV, Dom1, SizeDom1),
	    Dom1Cap is SizeDom1*Cap,
%%%	    incval(union_count),
	    Dom1Cap >= NVars1,			% may fail!
	    ( Dom1Cap == NVars1 ->
%%%		( Ts = [dvar{min:NextMin}|_], NextMin>= Min -> incval(extra_inference) ; true ),
	        Prop = NVars1,			% propagation opportunity found
		RestTs = Ts,
		Dom = Dom1
	    ; Dom1Cap > CutOffSize ->
		RestTs = []			% var(NVars) signals early cutoff!
	    ;
		process_prefix_ge_min(Min,Ts,RestTs,CutOffSize,Cap,Dom1,Dom,NVars1,NVars,Prop)
	    )
	;
	    RestTs = TTs,			% end of Mins, no propagation
	    NVars = NVars0,
	    Dom = Dom0
	).


	remove_domain(_DomRep,[]).
	remove_domain(DomRep,[dvar{var:Var}|Ts]) :-
	      subtract_domain(Var,DomRep),
	      remove_domain(DomRep,Ts).

	% Pre: list ordered by increasing max
	% Separate and count the initial chunk of items =< Max
	le_than_max(_Max,[],[],[],N,N).
	le_than_max(Max,TTs,TsLeThanMax,RestTs,N0,N) :-
		TTs = [T|Ts],
		T=dvar{max:MaxT},
		( MaxT =< Max ->
		    N1 is N0+1,
		    TsLeThanMax = [T|MoreTsLeThanMax],
		    le_than_max(Max,Ts,MoreTsLeThanMax,RestTs,N1,N)
		;
		    N = N0,
		    TsLeThanMax = [],
		    RestTs = TTs
		).

	vars_in_first_n(0,_,[]) :- !.
	vars_in_first_n(N,[dvar{var:V}|Ts],Vars) :- % N > 0,
	    ( var(V) ->
		Vars = [V|Vars0]
	    ;
		Vars = Vars0
	    ),
	    N1 is N-1,
	    vars_in_first_n(N1,Ts,Vars0).

	get_minima(Ts,Mins):-
		( foreach(dvar{min:Min},Ts),
		  foreach(Min,Mins)
		do
		    true
		).

	ts_to_vars(Ts,Vars):-
		( foreach(dvar{var:Var},Ts),
		  foreach(Var,Vars)
		do
		    true
		).


%
% element(Index, List, Value)
%

%element(Index, List, Value) :-
%    sort(List, DomainV),
%    LD =.. [t|List],
%    functor(LD, _, SizeI),
%    get_size(Index, SizeI),
%    Index #> 0,
%    Index #<= SizeI
%    Value::DomainV
%    get_size(Value, SizeV),
%    StartSize is max(SizeI, SizeV) + 1,
%    element(Index, LD, Value, StartSize, StartSize).
%
%element(VI, LD, VV, SI, SV) :-
%    index_values(VI, LD, VV, SI, SV, Res, NewIDom, NewVList, NSI, NSV),
%    element_res(VI, LD, VV, NSI, NSV, Res, NewIDom, NewVList).
%
%:- mode element_res(?, ++, ?, ++, ++, ++, ++, ++).
%element_res(VI, LD, VV, NSI, NSV, 0, _, _) :-			% no update
%    make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
%    insert_suspension([VI|VV], Susp, any of fd).
%element_res(VI, LD, VV, NSI, NSV, 1, NewIDom, _) :-		% Index updated
%    dvar_update_nocheck(VI, NewIDom, NSI),
%    ( NSV == 1 ->		% ONE value
%    	true
%    ;
%	make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
%	insert_suspension([VI|VV], Susp, any of fd)
%    ),
%    wake.
%element_res(VI, LD, VV, NSI, NSV, 2, _, NewV) :-		% Value updated
%    ( NSV == 1 ->		% ONE value
%	[VV] = NewV
%    ;
%	dvar_update_nocheck(VV, NewV, NSV),
%	make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
%	insert_suspension([VI|VV], Susp, any of fd),
%	wake
%    ).
%element_res(VI, LD, VV, NSI, NSV, 3, NewI, NewV) :-		% both updated
%    dvar_update_nocheck(VI, NewI, NSI),
%    ( NSV == 1 ->		% ONE value
%	[VV] = NewV
%    ;
%	(VI == VV ->
%	    var_fd(VI, dom(NewV, NSV)),
%	    element(VI, LD, VI, 0, 0)
%	;
%	    dvar_update_nocheck(VV, NewV, NSV),
%	    make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
%	    insert_suspension([VI|VV], Susp, any of fd)
%	)
%    ),
%    wake.
%element_res(VI, _, VV, _, _, 4, I, V) :-			% only one index
%    VV = V,
%    VI = I.

/************************************************************************

bool_channeling

************************************************************************/

:- comment(bool_channeling/3, [
        amode: bool_channeling(?, +, +),
        args: ["Var": "An integer domain variable",
               "DomainBools": "A collection of N 0/1 domain variables or"
                           " integers",
               "Min": "An integer"],
        summary: "Channel the domain values of Vars to the 0/1 boolean"
                 " variables in DomainBools",
        kind:[constraint:[root:[ic,fd]]],
        desc: html("\
<P>
    Var is an integer domain variable whose initial interval is Min..(Min+N),
    and this constraint links the domain values of Var with the N 0/1
    variables in DomainBools such that the i'th variable in DomainBools
    represents the value Min+i, and its value is 0 if the value is not in
    Var's domain, and 1 if Var is assigned the value [Thus, only one variable
    in DomainBools can take the value 1].
</P><P>
    A variant of this constraint, called 'domain_constraint' is in the global 
    constraint catalog. There, instead of having DomainBools and Min, there
    is a collection of Value-Bool pairs, representing a possible domain value
    and its associated 0/1 variable. The implementation here is described in
    the graph model for the domain_constraint in the catalog, and is 
    generalised arc-consistent.")]).

bool_channeling(X,Coll,F):-
        collection_to_list(Coll,B),
        length(B,N),
        X #>= F,
        X #< F+N,
        call_priority(update_bool_channeling(X,B,F,_), 2).

:-demon(update_bool_channeling/4).
update_bool_channeling(X,B,F,Susp):-
        check_bool_channeling(X,B,F),
        ( nonvar(X) ->
            kill_suspension(Susp)
        ; nonvar(Susp) ->
            unschedule_suspension(Susp)	% update is idempotent
	;
	    suspend(update_bool_channeling(X,B,F,Susp),3,
		    [B->inst, X->any],Susp)
        ).

check_bool_channeling(X,B,F):-
	( var(X) ->
	    (foreach(V,B),
	     count(J,F,_),
	     param(X) do
		(V == 0 ->
		    excl(X,J)
		; V == 1 ->
		    X = J
		; X == J ->
		    V = 1
		; check_in(J,X) ->
		    true
		;
		    V = 0
		)
	    )
	;
	    true
	),
        ( nonvar(X) ->
            (foreach(V,B),
             count(J,F,_),
             param(X) do
                (X == J ->
                    V = 1
                ;
                    V = 0
                )
            )
        ;
            true
        ),  
        wake.

/*
check_bool_channeling(X,B,F):-
        (foreach(V,B),
         count(J,F,_),
         param(X,F,B) do
            (V == 0 ->
                exclude(X,J)
            ; V == 1 ->
                X = J,
                set_to_zero_up_to_j(B,F,J)
            ; X == J ->
                V = 1
            ; is_in_domain(J,X) ->
                true
            ;
                V = 0
            )
        ),
        wake.

set_to_zero_up_to_j(_,J,J):-
        !.
set_to_zero_up_to_j([0|B],F,J):-
        F1 is F+1,
        set_to_zero_up_to_j(B,F1,J).
*/


/*****************************************************************
bin_packing constraint, contributed by Helmut Simonis
******************************************************************/

:- include(generic_bin_packing).

/*****************************************************************
Various non-domain consistent global constraints with flow-based
algorithms, implemented by Helmut Simonis, 2009
******************************************************************/

:- include(generic_flow_constraints).

