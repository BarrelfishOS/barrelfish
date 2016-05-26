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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Author:	Joachim Schimpf, Coninfer Ltd
% Version:	$Id: generic_prop_test_util.ecl,v 1.2 2013/01/27 23:24:26 jschimpf Exp $
%
% IDENTIFICATION:	generic_prop_test_util.pl
%
%	Support for testing global constraints with random patterns
%	Based on older module fd_test.
%
% ----------------------------------------------------------------------

%----------------------------------------------------------------------
% TODO Parameters:
%	variable range
%	number of variables
%	reductions: bounds, holes
%	max number of vars reduced in each step
%----------------------------------------------------------------------

:- comment(categories, ["Development Tools"]).
:- comment(summary, "Tools for testing global constraints").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2013/01/27 23:24:26 $").


%----------------------------------------------------------------------

:- comment(random_bound_reduction_test/2, [
    summary:"Run and keep waking a constraint until ground",
    amode:(random_bound_reduction_test(+,+) is det),
    args:[
	"Out":"An output stream for printing the execution protocol",
	"Goal":"A goal, usually a constraint with domain variables"
    ],
    see_also:[random_bound_reduction_test/3],
    desc:html("<P>
    This utility shows constraint propagation at work.  Goal should be
    a constraint involving domain variables.  The Goal is first called,
    i.e. the constraint is being set up.  Then (if it does not fail),
    some of its variables are reduced in their domains, which may lead
    to the Goal waking up and propagating the domain changes.  This
    process is repeated until either the Goal fails, or all variables
    are instantiated and the goal succeeds.
</P><P>
    Throughout the process, a protocol is printed to the Out stream.
    Output lines are marked with C (initial call), P (propagation result)
    or L (imposed domain reduction, or labeling), and display the
    corresponding state of the constraint with its variable domains.
</P><P>
    Domain reductions are chosen randomly: in each step, 1 to 3 variables
    are selected randomly, and each receives a random domain reduction
    (upper and/or lower bound).  To create a reproducible sequence, it
    is recommended to invoke seed/1 beforehand.
</P>"),
    eg:"
    ?- [X,Y]::1..3, random_bound_reduction_test(output, alldifferent([X,Y])).
    C alldifferent([_{1 .. 3}, _{1 .. 3}])
    P alldifferent([_{1 .. 3}, _{1 .. 3}])
    L alldifferent([_{1 .. 3}, _{[1, 2]}])
    L alldifferent([_{[1, 2]}, 1])
    P alldifferent([2, 1])
    yes."
]).
:- export random_bound_reduction_test/2.
:- tool(random_bound_reduction_test/2,random_bound_reduction_test_/3).

random_bound_reduction_test_(Out, Goal, Module) :-
	term_variables(Goal, Vars),
	random_bound_reduction_test_(Out, Goal, Vars, Module).


:- comment(random_bound_reduction_test/3, [
    summary:"Run and keep waking a constraint until ground",
    amode:(random_bound_reduction_test(+,+,+) is det),
    args:[
	"Out":"An output stream for printing the execution protocol",
	"Goal":"A goal, usually a constraint with domain variables",
	"Vars":"Subset of Goal's variables to be domain-reduced"
    ],
    see_also:[random_bound_reduction_test/2]
]).
:- export random_bound_reduction_test/3.
:- tool(random_bound_reduction_test/3,random_bound_reduction_test_/4).

random_bound_reduction_test_(Out, Goal, Vars, Module) :-
	printf(Out, "C %_mKW%n", [Goal]),
	(
	    call(Goal)@Module,
	    printf(Out, "P %_mKW%n", [Goal]),
	    random_reduce_until_ground(Out, Goal, Vars)
	->
	    printf(Out, "yes.%n", [])
	;
	    printf(Out, "no.%n", [])
	),
	fail.		% fail, to avoid garbage collection
random_bound_reduction_test_(_, _, _, _).


%----------------------------------------------------------------------
% apply random bound reductions to variables in a list
%----------------------------------------------------------------------

:- export random_reduce_until_ground/3.

random_reduce_until_ground(Out, OriginalVars, Remaining) :-
	term_variables(Remaining, Vars),
	( Vars = [] ->
	    true
	;
	    % reduce up to 3 variables simultaneously
	    N is 1 + random_mod(3),
	    pick_randomly(N, Vars, PickedVars),
	    call_priority((
		    random_reduce(PickedVars),
		    copy_term(OriginalVars, Before),
		    printf(Out, "L %_mKW%n", [OriginalVars])
	    ), 2),
	    % propagation happens here!
	    true,
	    ( variant(Before, OriginalVars) ->
	    	true
	    ; 
		printf(Out, "P %_mKW%n", [OriginalVars])
	    ),
	    random_reduce_until_ground(Out, OriginalVars, Vars)
	).


random_reduce_one(VarArr) :-
	functor(VarArr,_,N),
	I is random_mod(N) + 1,
	arg(I, VarArr, X),
	( var(X) ->
	    random_reduce(X)
	; nonground(VarArr) ->
	    random_reduce_one(VarArr)
	).


random_sequence(X) :-
	var(X),
	random_reduce(X),
	printf("%_mKW%n",[X]),
	random_sequence(X).
random_sequence(X) :-
	nonvar(X).


random_reduce(X) :-		% random bound reduction on X
	var(X),
	get_finite_bounds(X, Min, Max),
	get_size(X, OldDomSize),
	OldDomSizeM1 is OldDomSize-1,
	NewDomSize is 1 + random_mod(OldDomSizeM1),	% 1..OldDomSize-1
	RemovedP1 is OldDomSize-NewDomSize+1,		% 1..OldDomSize-1
	LwbIncr is random_mod(RemovedP1),		% 0..Removed
	( OldDomSize =:= Max-Min+1 ->
	    % no holes
	    NewL is Min + LwbIncr,
	    NewH is NewL + NewDomSize - 1
	;
	    % with holes
	    get_full_domain_as_list(X, Vs),
	    ( for(_,1,LwbIncr), fromto(Vs,[_|Vs1],Vs1,Vs2) do true ),
	    Vs2 = [NewL|_],
	    ( for(_,2,NewDomSize), fromto(Vs2,[_|Vs3],Vs3,Vs4) do true ),
	    Vs4 = [NewH|_]
	),
	X::NewL..NewH.
random_reduce([]) ?- true.
random_reduce([X|Xs]) ?-
	random_reduce(X),
	random_reduce(Xs).


%----------------------------------------------------------------------
% Make random domain variables
%----------------------------------------------------------------------

:- comment(make_n_random_variables/4, [
    summary:"Create random domain variables",
    amode:(make_n_random_variables(+,+,+,-) is det),
    args:[
	"NVars":"Number of variables to create (integer)",
	"Min":"Minimum lower domain bound (integer)",
	"Max":"Maximum upper domain bound (integer)",
	"Vars":"List of variables (output)"
    ]
]).
:- export make_n_random_variables/4.

make_n_random_variables(0, _, _, []) :- !.
make_n_random_variables(N, Min, Max, [X|Xs]) :-
	make_random_variable(Min, Max, X),
	N1 is N-1,
	make_n_random_variables(N1, Min, Max, Xs).

make_random_variable(Min, Max, X) :-
	Min =< Max,
	DomSizeMax is Max - Min + 1,		% 1 ..
	DomSizeMinus1 is random_mod(DomSizeMax),	% 0 .. < DomSizeMax
	LeftOver is DomSizeMax-DomSizeMinus1,	% 1 .. =< DomSizeMax
	L is Min + random_mod(LeftOver),	% Min .. Max
	H is L + DomSizeMinus1,
	X::L..H.


%----------------------------------------------------------------------
% Other random utilities
%----------------------------------------------------------------------

:- comment(random_int_between/3, [
    summary:"Create random integer",
    amode:(random_int_between(+,+,-) is det),
    args:[
	"Min":"Minimum value (integer)",
	"Max":"Maximum value (integer)",
	"X":"Random value (output integer)"
    ]
]).
:- export random_int_between/3.
random_int_between(Min, Max, X) :-
	Diff is Max-Min+1,
	X is Min + random_mod(Diff).


:- comment(make_n_random_seeds/2, [
    summary:"Create random seeds for seed/1",
    amode:(make_n_random_seeds(+,-) is det),
    args:[
	"NSeeds":"Number of seeds (integer)",
	"Seeds":"List of seeds (output)"
    ]
]).
:- export make_n_random_seeds/2.
make_n_random_seeds(N, Seeds) :-
	( for(_,1,N), foreach(Seed,Seeds) do
	    Seed is 1 + random_mod(2147483647)
	).
	

shuffle(L, R) :-
        add_random_keys(L, KL),
        keysort(KL, KR),
        rm_keys(KR, R).

        % add random key to each list element
add_random_keys([], []).
add_random_keys([A|L], [K-A|KL]) :-
        %random(K),
        frandom(K),
        add_random_keys(L, KL).

        % remove keys from association list
rm_keys([], []).
rm_keys([_K-A|KL], [A|L]) :-
        rm_keys(KL, L).


pick_randomly(N, List, PickedElems) :-
	shuffle(List, ShuffledList),
	max_prefix(N, ShuffledList, PickedElems).

    max_prefix(0, _, []) :- !.
    max_prefix(_N, [], []) :- !.
    max_prefix(N, [X|Xs], [X|Ys]) :-
	N1 is N-1,
	max_prefix(N1, Xs, Ys).


random_mod(Sup, X) :-
%	X is random mod Sup.
	X is fix(frandom * Sup).

%----------------------------------------------------------------------

