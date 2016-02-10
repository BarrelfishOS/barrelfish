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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: repairtest.pl,v 1.1 2006/09/23 01:53:53 snovello Exp $
% ----------------------------------------------------------------------

:- use_module(repair).
:- lib(fd).


repair :-
	( conflict_vars([C|_onflict]) ->
		indomain(C),
		repair
	; conflict_constraints([C|_onflictConstraints]) ->
		term_variables(C, Vars),
		deleteffc(Var,Vars, _),
		Var tent_get Val,
		(Var = Val ; Var ## Val), % choice
		repair
	;
		true
	).

test :-
	test(X),write(X),fail.

test(a) :-
    [X]::0..1,
    [S]::0..10,
    S #>= X + 1 r,
    X tent_set 1,
    S tent_set 5,
    write_expect(a1,[],[]),
    S #<= 7,
    write_expect(a2,[],[]),
    S#<= 4,
    write_expect(a3,[S #>= X + 1],[S]).

test(b) :-
	[X,Y]::1..10,
	X #> Y r,
	X tent_set 1,
	Y tent_set 2,
	write_expect(b1,[X #> Y],[X]),
	X tent_set 3,
	write_expect(b2,[],[]),
	X tent_set 33,
	write_expect(b3,[X #> Y],[X]).

test(c) :-
	[X,Y]::1..10,
	tent_set(X,5),
	tent_set(Y,3),
	r(X #> Y),
	write_expect(c1,[],[]),
	Y = 8,
	write_expect(c2,[X #> Y],[X]),
	repair,
	write_expect(c3,[],[]).

test(d) :-
	[X,Y,Z]::1..10,
	tent_set(X,5),
	tent_set(Y,3),
	tent_set(Z,7),
	r(Z#>X),r(Z#>Y),
	write_expect(d1,[],[]),
	Y=7,
	write_expect(d2,[Z #> Y,Z #> X],[Z]),
	repair,
	write_expect(d3,[],[]).

test(e) :-
	 X::0..1, X tent_set 0,X#>0 r,
	 ( delayed_goals([]) -> true
	 ; writeln('e1 failed')
	 ).
test(g) :-
	[X,Y]::0..2,
	tent_set(X,4),
	tent_set(Y,4),
	r(Y #>= X),
	write_expect(g1,[Y #>= X],[X,Y]),
	X=1,
	write_expect(g2,[Y #>= X],[Y]),
	Y=1,
	write_expect(g3,[],[]).

test(h) :-
	[X,Y,A,B]::0..8,
	[X,Y,A,B] tent_set [3,5,3,5],
	8 #= X+Y r,
	A #<= X r,
	B #<= Y r,
	write_expect(h1,[],[]),
	A #> 3,
	write_expect(h2,[B #<= Y,A #<=X,8 #= X+Y],[X,A,Y,B]),
	A=8,
	write_expect(h3,[],[]).

test(i) :-
	X = 1 r,
	write_expect(i1,[],[]),
	X = 1,
	write_expect(i2,[],[]).

test(j) :-
	[X,Y]::1..10,
	X #> Y r_no_prop,
	X tent_set 2,
	Y tent_set 1,
	write_expect(j1,[],[]),
	X=Y,
	write_expect(j2,[X #> X],[]).

test(k) :-
	X::1..10,
	X tent_set 5,
	suspend(X tent_set 3,3,X->ga_chg),
	X #< 5,
	write_expect(k1,[],[]).

test(l) :-
	#?(1 #> X,B) r_conflict a-b(X,B),
	X :: 1..10,
	X tent_set 0,
	expect_ccs(l1,a,[b(X,B)]),
	X tent_set 1,
	expect_ccs(l2,a,[]).
test(' -- Repair test done').

:- tool((#?)/2,(#?)/3).
#?(Goal,B,M) :- 
	call(Goal,M) -> B=1 ; B=0.

expect_ccs(Id,Key,Ds) :-
	conflict_constraints(Key,Is),
	sort(Ds,SDs),
	sort(Is,SIs),
	( SIs ==  SDs ->
	    true
	;
	    writeln(Id:expected_constraints(SIs=SDs))
	).

expect_ccs(Id,Ds) :-
	conflict_constraints(Is),
	sort(Ds,SDs),
	sort(Is,SIs),
	( SIs ==  SDs ->
	    true
	;
	    writeln(Id:expected_constraints(SIs=SDs))
	).

expect_cvs(Id,Ds) :-
	conflict_vars(Is),
	sort(Ds,SDs),
	sort(Is,SIs),
	( SIs ==  SDs ->
	    true
	;
	    writeln(Id:expected_vars(SIs=SDs))
	).

write_expect(Id,CCe,CVe) :-
	expect_ccs(Id,CCe),
	expect_cvs(Id,CVe).

% :- test.
