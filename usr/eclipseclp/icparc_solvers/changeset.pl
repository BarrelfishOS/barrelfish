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
% Copyright (C) 1998 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: changeset.pl,v 1.2 2009/07/16 09:11:25 jschimpf Exp $
%
% Description:		Predicates to efficiently compute the set of
%			variables modified during a propagation sequence.
%
% Authors:		J.Schimpf, IC-Parc
%
% ----------------------------------------------------------------------

:- module(changeset).

:- comment(categories, ["Algorithms","Constraints"]).
:- comment(summary, "Compute sets of modified variables").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").

:- comment(monitor_changes_arr/5, [
    summary:"Monitor variables for modifications",
    amode:monitor_changes_arr(+,+,+,+,-),
    args:["VarArr":"A structure containing variables",
    "Prio":"Priority for the monitoring demons",
    "CondList":"Suspension list spec",
    "AttrMod":"Suspension list attribute module",
    "ChangeStream":"A lazy list of lists of changes"],
    desc:html("
    This predicate monitors an array of variables for certain
    modifications, and creates a continuous stream of lists of indices of
    modified variables, e.g.
    <PRE>
	monitor_changes_arr(VarArr, 8, [min of fd, max of fd], fd, Stream)
    </PRE>
    will monitor the variables in VarArr for modifications of their min/max
    fd-attributes. The monitor will run with a priority of 8 to 9.
    All variable modifications that occur between two wakings of the
    monitor will be detected by the monitor. It will then create a list
    of indices of the modified variables, and append this list to
    ChangeStream.
    <PRE>
	[eclipse 4]: X1::1..9, X2::1..8,
		monitor_changes_arr(v(X1,X2), 8,
				    [min of fd, max of fd], fd, Changes),
		X1 #> X2, X2 #>= 5.

	Changes = [[1], [2, 1]|More]
	X1 = X1{[6..9]}
	X2 = X2{[5..8]}
    </PRE>
    What happened here is that the first constraint X1 #> X2 caused X1 to
    change its lower bound, therefore [1] was appended to the Changes list.
    Then X2 #>= 5 raised the lower bound of X2 and (because X1 #> X2)
    the lower bound of X1, therefore both variable indices [1,2] were
    appended to the Changes list.
    <P>
    The priority of the monitor should be set up such that is is lower than
    the priority of the propagation constraints. In that case, the lists
    that get appended to ChangeStream represent exactly the set of variables
    (without duplicates) that were modified by one propagation sequence.
    <P>
    Note that the ChangeStream can be used to trigger actions whenever
    new changes get appended, for example:
    <PRE>
	delay report_changes(X) if var(X).
	report_changes([]).
	report_changes([X|T]) :-
		printf(\"The following variables changed: %Mw%n%b\", [X]),
		report_changes(T).


	[eclipse 11]: X1::1..9, X2::1..8,
		monitor_changes_arr(v(X1,X2), 8,
					[min of fd, max of fd], fd, Changes),
		report_changes(Changes),
		X1 #> X2, X2 #>= 5.
	The following variables changed: [1]
	The following variables changed: [2, 1]
	...
    <PRE>
    ")]).

:- comment(monitor_changes/6, [
    summary:"Monitor variables for modifications",
    amode:monitor_changes(+,+,+,+,+,-),
    args:["Vars":"A list containing variables",
    "Templates":"A list of terms corresponding to the variables",
    "Prio":"Priority for the monitoring demons",
    "CondList":"Suspension list spec",
    "AttrMod":"Suspension list attribute module",
    "ChangeStream":"A lazy list of lists of changes"],
    desc:html("
    Like monitor_changes_arr/5, but (instead of array indices) the
    ChangeStream contains the elements of the Templates-list that
    correspond to the modified variables, thus allowing arbitrary
    information to be conveyed to the code that processes the changes.
    <PRE>
    [eclipse 10]: X1::1..9, X2::1..8,
	    monitor_changes([X1,X2],[info(1,X1),info(2,X2)], 8,
			    [min of fd, max of fd], fd, Stream),
	    X1 #> X2, X2 #>= 5.

    Stream = [[info(1, X1{[6..9]})], [info(2, X2{[5..8]}), info(1, X1)]|More]
    X1 = X1{[6..9]}
    X2 = X2{[5..8]}
    </PRE>
    ")]).

:- export
	monitor_changes_arr/5,
	monitor_changes/6.

:- import setarg/3 from sepia_kernel.
:- import get_attribute/3 from sepia_kernel.

monitor_changes_arr(VarArr, Prio, Cond, AttrMod, ChangeStream) :-
	Store = set(Empty),
	functor(VarArr, _, N),
	setup_demons_arr(VarArr, Prio, Cond, AttrMod, Store, N),
	Prio1 is Prio+1,
	suspend(flush_change_list(Store, ChangeStream), Prio1, Empty->inst).

    setup_demons_arr(_VarArr, _Prio, _Cond, _AttrMod, _Store, 0) :- !.
    setup_demons_arr(VarArr, Prio, Cond, AttrMod, Store, N) :-
	arg(N, VarArr, X),
	N1 is N-1,
	make_suspension(change_monitor(X, N, Store, Susp), Prio, Susp),
	insert_all(X, Susp, Cond, AttrMod),
	setup_demons_arr(VarArr, Prio, Cond, AttrMod, Store, N1).


monitor_changes(Vars, Templates, Prio, Cond, AttrMod, ChangeStream) :-
	Store = set(Empty),
	setup_demons(Vars, Templates, Prio, Cond, AttrMod, Store),
	Prio1 is Prio+1,
	suspend(flush_change_list(Store, ChangeStream), Prio1, Empty->inst).

    setup_demons([], _Templates, _Prio, _Cond, _AttrMod, _Store).
    setup_demons([X|Xs], [T|Ts], Prio, Cond, AttrMod, Store) :-
	make_suspension(change_monitor(X, T, Store, Susp), Prio, Susp),
	insert_all(X, Susp, Cond, AttrMod),
	setup_demons(Xs, Ts, Prio, Cond, AttrMod, Store).

    insert_all(_, _, [], _).
    insert_all(X, Susp, [Cond|Conds], AttrMod) :-
	insert_suspension(X, Susp, Cond, AttrMod),
	insert_all(X, Susp, Conds, AttrMod).


:- demon change_monitor/4.
change_monitor(X, Template, Store, _Susp) :-
	var(X),
	Store = set(Old),
	( Old = [] -> true ; true ),
	setarg(1, Store, [Template|Old]).
change_monitor(X, Template, Store, Susp) :-
	nonvar(X),
	Store = set(Old),
	( Old = [] -> true ; true ),
	setarg(1, Store, [Template|Old]),
	kill_suspension(Susp).


flush_change_list(Store, ChangeStream) :-
	Store = set(Changes),
	get_priority(Prio1),
	suspend(flush_change_list(Store, More), Prio1, Empty->inst),
	setarg(1, Store, Empty),
	ChangeStream = [Changes|More].	% last!


/*

lib(fd).

List = [X1,X2,X3,X4,X5,X6,X7,X8,X9],
List::0..9,
Terms = [1-X1,2-X2,3-X3,4-X4,5-X5,6-X6,7-X7,8-X8,9-X9],
monitor_changes(List, Terms, 8, [min of fd, max of fd], fd, Stream),
report_changes(Stream),
call_priority((X4#>6, X7#<3, X2=5),5).

List = [X1,X2,X3,X4,X5,X6,X7,X8,X9], List::0..9, VarArr =.. [vars|List],
monitor_changes_arr(VarArr, 8, [min of fd, max of fd], fd, Stream),
report_changes(Stream),
call_priority((X4#>6, X7#<3, X2=5),5).

List = [X1,X2,X3,X4,X5,X6,X7,X8,X9], List::0..9, VarArr =.. [vars|List],
monitor_changes_arr(VarArr, 8, [min of fd, max of fd], fd, Stream),
report_changes(Stream),
X4#>6, X7#<3, X2=5.


delay report_changes(X) if var(X).
report_changes([]).
report_changes([X|T]) :-
	printf("changes: %Mw%n%b", [X]),
	report_changes(T).


re_solve(_Handle, _VarArr, []).
re_solve(Handle, VarArr, [ChangedVarIndices|More]) :-
	transfer_some_bounds(ChangedVars),
	lp_re_solve(Handle, ChangedVarIndices, ObjVal),
	suspend(lp_re_solve(Handle, More), 9, More->inst).
*/
