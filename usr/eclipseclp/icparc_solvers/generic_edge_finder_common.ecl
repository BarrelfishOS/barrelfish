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
% Contributor(s): Joachim Schimpf and Andrew Sadler, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: generic_edge_finder_common.ecl,v 1.2 2013/02/13 00:58:47 jschimpf Exp $
%
% Description:		Generalised Prolog toplevel for Edge-finder in C
%
% Author:		J.Schimpf, IC-Parc
%                       A.Sadler, IC-Parc
%
% This file is intended to be used 'include'd into specialised modules
% which must declare certain key interface predicates (possibly via
% read-macros).
%
% See the file generic_design.txt for a description of the interface
% predicates.
% ----------------------------------------------------------------------

:- export disjunctive_ef/3, disjunctive_ef/4, cumulative_ef/6.

:- local variable(wakes, 0).

:- import symbol_address/2 from sepia_kernel.
:-  ( symbol_address(ec_init_ef,_) ->
	true
    ;
	get_flag(hostarch, Arch),
	get_flag(object_suffix, O),
	concat_string([Arch,'/edge_finder.',O], Load),
	load(Load)
    ).

:- external(init_ef/4, ec_init_ef),
   external(init_task/9, ec_init_task),
   external(ef_disj/2, ec_ef_disj),
   external(ef_cum/1, ec_ef_cum).



:- inline(demon_suspend/4, tr_demon_suspend/2).
tr_demon_suspend(
	demon_suspend(Goal,Prior,Cond,Susp),
	(
	    suspend(Goal,Prior,Cond,Susp),
	    schedule_woken([Susp]),
	    wake
	)).

demon_suspend(Goal,Prior,Cond,Susp) :-
          suspend(Goal,Prior,Cond,Susp),
          schedule_woken([Susp]),
          wake.


%----------------------------------------------------------------------
% Disjunctive
%----------------------------------------------------------------------

disjunctive_ef(Starts, Durations, Algorithm) :-
	length(Starts, NTasks),
	init_ef(NTasks, 1, Algorithm, Handle),
	is_ground(Starts-Durations, 4, Ground),
	suspend(disjunctive_ef(Starts, Durations, [], Handle, Ground, Susp), 5,
		[Starts-Durations->min,Starts->max,Ground->inst], Susp),
	disjunctive_ef(Starts, Durations, [], Handle, Ground, Susp).

disjunctive_ef(Starts, Durations, Bools, Algorithm) :-
	length(Starts, NTasks),
	init_ef(NTasks, 1, Algorithm, Handle),
	NBools is NTasks*(NTasks-1)//2,
	functor(BoolArr, [], NBools),
	BoolArr =.. [[]|Bools],
%	Bools :: 0..1,
	disjunctive_prop_setup(Starts, Durations, Bools),
	is_ground(Starts-Durations, 4, Ground),
	suspend(disjunctive_ef(Starts, Durations, BoolArr, Handle, Ground, Susp), 5,
		[Starts-Durations->min,Starts->max,Ground->inst], Susp),
	disjunctive_ef(Starts, Durations, BoolArr, Handle, Ground, Susp).


:- demon(disjunctive_ef/6).
disjunctive_ef(Starts, Durations, BoolArr, Handle, Ground, Susp) :-
	( nonvar(Ground) -> kill_suspension(Susp) ; true ),
%	incval(wakes),
	ef_setup(Starts, Durations, 0, Handle),
%	getval(wakes, W), writeln(W), dump_handle(Starts, Handle, 0),
	call_priority((ef_disj(Handle, BoolArr),ef_get_bounds(Handle, Starts, 0)), 2).

ef_setup([], [], _I, _Handle) :- !.
ef_setup([S|Ss], [D|Ds], I, Handle) :-
	I1 is I+1,
	get_finite_bounds(S, EST, LST),
	get_finite_bounds(D, Dmin, _Dmax),
	ECT is EST+Dmin,
	LCT is LST+Dmin,
	init_task(Handle, I, EST, LST, ECT, LCT, 1, Dmin, Dmin),
	ef_setup(Ss, Ds, I1, Handle).

/*
disjunctive_back_prop(Si, Di, Sj, Dj, B) :- var(B), !,
	B::0..1,
	suspend(disjunctive_back_prop(Si,Di,Sj,Dj,B), 2, [B->inst]).
disjunctive_back_prop(Si, Di, Sj,_Dj, 0) :-
	Sj #>= Si+Di.
disjunctive_back_prop(Si,_Di, Sj, Dj, 1) :-
	Si #>= Sj+Dj.
*/

:- demon disjunctive_prop/6.
disjunctive_prop(Si,Di,Sj,Dj,B,Susp) :-
	nonvar(B),
	kill_suspension(Susp),
	( B = 0 -> Sj #>= Si+Di ; Si #>= Sj+Dj ).
disjunctive_prop(Si,Di,Sj,Dj,B,Susp) :-
	var(B),
	get_finite_bounds(Si, ESTi, LSTi),
	get_finite_bounds(Sj, ESTj, LSTj),
	get_finite_bounds(Di, MinDi, MaxDi),
	get_finite_bounds(Dj, MinDj, MaxDj),
	( ESTj >= LSTi+MaxDi ->
	    kill_suspension(Susp),
	    B = 0
	; ESTi >= LSTj+MaxDj ->
	    kill_suspension(Susp),
	    B = 1
	; LSTi < ESTj+MinDj,		% i starts before j ends
	  LSTj < ESTi+MinDi ->		% j starts before i ends
	    fail	% overlap
	;
	    true	% re-suspend
	).


disjunctive_prop_setup(Starts, Durations, Flags) :-
	length(Starts, N),
	( N >= 2 ->
	    (
		for(J,0,N-1),
		foreach(Sj, Starts),
		foreach(Dj, Durations),
		fromto(Flags, Flags3, Flags0, []),
		param(Starts, Durations)
	    do
		(
		    count(_,1,J),	% J: 0..N-1
		    fromto(Starts, [Si|Starts1], Starts1, _),
		    fromto(Durations, [Di|Durations1], Durations1, _),
		    fromto(Flags3, [F|Flags1], Flags1, Flags0),
		    param(Sj,Dj)
		do
		    suspend(disjunctive_prop(Si,Di,Sj,Dj,F,Susp),3,[[Si,Sj,Di,Dj]->min,[Si,Sj]->max,F->inst],Susp),
		    disjunctive_prop(Si,Di,Sj,Dj,F,Susp)
		)
	    )
	;
	    true
	).


%----------------------------------------------------------------------
% Cumulative
%----------------------------------------------------------------------

% If the resource limit 'Cap' is a variable, delay till it is instantiated.
% Otherwise work out the areas, as before, and call cumulative_ef1.
% Note that the new version is a demon (and I've used demon_suspend to 
% both suspend it and call it immediately).
cumulative_ef(Starts, Durations, Resources, Areas, Cap, _Alg) :- var(Starts), !,
	error(4, cumulative(Starts, Durations, Resources, Areas, Cap)).
cumulative_ef([], _Durations, _Resources, _Areas, _Cap, _Alg) :- !.
cumulative_ef(Starts, Durations, Resources, Areas, Cap, Alg) :-
	( var(Cap) -> 
            suspend(cumulative_ef(Starts, Durations, Resources, Areas, Cap, Alg),
		    5,
		    Cap->inst)
        ;
	    areas(Starts, Durations, Resources, Areas, Cap),
	    length(Starts, NTasks),
	    init_ef(NTasks, Cap, Alg, Handle),
	    is_ground(Starts-Durations-Resources, 4, Ground),
	    demon_suspend(cumulative_ef2(Starts, Durations, Resources, Areas, Handle, Ground, Susp), 
		    5,
		    [[](Starts,Durations,Resources,Areas)->min,Starts->max,Ground->inst], 
		    Susp)
        ).


    areas(Starts, Durations, Resources, Areas, Cap) :- var(Starts), !,
	error(4, cumulative(Starts, Durations, Resources, Areas, Cap)).
    areas([], [], [], [], _Cap).
    areas([_|Ss], [D|Ds], [R|Rs], [A|As], Cap) :-
	D #>= 0,
	R :: 0..Cap,
	A #= D*R,
	areas(Ss, Ds, Rs, As, Cap).


:- demon(cumulative_ef2/7).
cumulative_ef2(Starts, Durations, Resources, Areas, Handle, Ground, Susp) :-
	( nonvar(Ground) -> kill_suspension(Susp) ; true ),
%	incval(wakes),
%	getval(wakes,W), (W=24->stop;true),
	ef_setup(Starts, Durations, Resources, Areas, _Sizes, 0, Handle),
	ef_cum(Handle),
%	getval(wakes, W), writeln(W), dump_handle(Starts, Handle, 0),
	call_priority(ef_get_bounds(Handle, Starts, 0), 2).

ef_setup([], [], [], [], [], _I, _Handle) :- !.
ef_setup([S|Ss], [D|Ds], [R|Rs], [A|As], [SZ|SZs], I, Handle) :-
	I1 is I+1,
	% the bounds accessed here determine which waking conditions
	% we need when suspending cumulative_ef2/7 above
	get_finite_bounds(S, EST, LST),
	get_finite_bounds(R, SZ, _),
	get_finite_bounds(D, Dmin, _Dmax),
	get_finite_bounds(A, Amin, _Amax),
	ECT is EST+Dmin,
	LCT is LST+Dmin,
	init_task(Handle, I, EST, LST, ECT, LCT, SZ, Dmin, Amin),
	ef_setup(Ss, Ds, Rs, As, SZs, I1, Handle).


%----------------------------------------------------------------------
% Auxiliary
%----------------------------------------------------------------------

ef_get_bounds(_Handle, [], _I) :-
	wake.
ef_get_bounds(Handle, [S|Ss], I) :-
	I1 is I+1,
	xget(Handle, I, Task),
%	Task = task(_,_,_,_,_,_,_,LB,UB),
	Task = task(LB,UB),
	lwb(S, LB),
	upb(S, UB),
	ef_get_bounds(Handle, Ss, I1).

dump_handle([], _Handle, _I) :- !, nl.
dump_handle([_|Ss], Handle, I) :-
	xget(Handle, I, Task),
	Task = task(EST, LST,_ECT,_LCT,_SZ, D,_A, LB, UB),
	write(I:task(EST,LST,D,LB,UB)),
	( EST=LB,LST=UB -> nl ; writeln(***)),
	I1 is I+1,
	dump_handle(Ss, Handle, I1).


/*
is_ground(Term, Prio, Flag) :-
	term_variables(Term, Vars),
	is_ground_demon(Vars, Prio, Flag).

is_ground_demon([], _, 1).
is_ground_demon([X|Xs], Prio, Flag) :-
	is_ground_demon(Xs, Prio, Flag, X).

is_ground_demon(Xs, Prio, Flag, X) :-
	var(X),
	suspend(is_ground_demon(Xs,Prio,Flag), Prio, X->inst).
is_ground_demon(Xs, Prio, Flag, X) :-
	nonvar(X),
	is_ground_demon(Xs, Prio, Flag).
*/

:- import setarg/3 from sepia_kernel.

is_ground(Term, Prio, Flag) :-
	term_variables(Term, Vars),
	make_suspension(is_ground_demon(v(Vars), Susp, Flag), Prio, Susp),
	is_ground_demon(v(Vars), Susp, Flag).

:- demon(is_ground_demon/3).
is_ground_demon(VTerm, Susp, Flag) :-
	VTerm = v(List),
	find_var(VTerm, Susp, Flag, List).

    find_var(_VTerm, Susp, 1, []) :-
    	kill_suspension(Susp).
    find_var(VTerm, Susp, Flag, [X|Xs]) :-
	find_var(VTerm, Susp, Flag, X, Xs).

    find_var(VTerm, Susp, _Flag, X, Xs) :-
	var(X),
	setarg(1, VTerm, Xs),
	insert_suspension(X, Susp, inst of suspend, suspend).
    find_var(VTerm, Susp, Flag, X, Xs) :-
	nonvar(X),
	find_var(VTerm, Susp, Flag, Xs).


%----------------------------------------------------------------------
% Test
%----------------------------------------------------------------------

/*
test(N) :-
	length(L,N),
	is_ground(L, 4, G),
	reverse(L,R),
	cputime(T0),
	make_ground(R),
	T is cputime-T0,
	writeln(time:T).

make_ground([]).
make_ground([0|Xs]) :-
	true,
	make_ground(Xs).
*/

/*
% p47, LB1
S=[S1,S2,S3], S1::0..11, S2::1..7, S3::1..9, disjunctive(S, [6,4,3]).
% p54, LB2
S=[S1,S2,S3], S1::1..14, [S2,S3]::0..10, disjunctive(S, [4,6,6]).
S=[S1,S2,S3], S1::0..13, [S2,S3]::2..12, disjunctive(S, [4,6,6]).
% same, using cumulative
S=[S1,S2,S3], S1::1..14, [S2,S3]::0..10, cumulative(S, [4,6,6], [1,1,1], 1).
S=[S1,S2,S3], S1::0..13, [S2,S3]::2..12, cumulative(S, [4,6,6], [1,1,1], 1).

% p56, LB1
S=[S1,S2,S3,S4], S1::0..11, S2::0..6, [S3,S4]::0..5,
	cumulative(S, [8,4,5,5], [1,1,1,1], 2).
S=[S1,S2,S3,S4], S1::0..11, S2::9..15, [S3,S4]::9..14,
	cumulative(S, [8,4,5,5], [1,1,1,1], 2).

% LB3
S=[S1,S2,S3,S4], S1::0..11, S2::1..7, [S3,S4]::1..6,
	cumulative(S, [8,4,5,5], [1,1,1,1], 2).
*/
