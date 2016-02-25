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
% Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Mark Wallace, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: lds.ecl,v 1.1 2006/09/23 01:53:50 snovello Exp $
%
% Limited discrepancy search and bounded backtrack search
%
% Author:	Mark Wallace, ICL/IC-Parc
% ----------------------------------------------------------------------


%:- module_interface(lds).

:- use_module(library(repair)).

%:- export
%	static_lds/3,
%	dynamic_lds/2,
%	bounded_backtrack_search/2,
%	bbs_dynamic_lds/3,
%	lds_dist/2,
%	lds_opt_dist/2.


%:- begin_module(lds).

:- coroutine.   % For checking discrepancies


/*** Static LDS: discrepancy = #variables  ***/

%We start by assuming a static heuristic, which is a complete
%assignment to the problem variables specified in advance of the
%search.  The predicate supporting static LDS takes a list of decisions 
% and results and a list of expected results.
% It is expressed in ECLiPSe as follows.

static_lds(Decisions,Vals,Discrepancy) :-
        length(Decisions,MaxDiscrepancy),              % line 1
        between(0,MaxDiscrepancy,1,Discrepancy),       % line 2
        bd_stat_lds(Decisions,Vals,0,Discrepancy).     % line 3


%The arguments to stat_lds are as follows:
%static_lds(?Vars,   The list of decisions and result variables.  Some 
%                    of the variables may already be instantiated.
%         +Vals      The list of values according to the heuristic.  It
%                    must be the same length as Vars, and the heuristic
%                    must match the value, in case the variable is
%                    already instantiated 
%         -Discrepancy  The discrepancy between the result and the
%                       heuristic.
%        )

%An example call is:
%?- static_lds([X,Y,Z],[1,2,3],Discrepancy).
%The measure of discrepancy, in this case, is simply the number of
%variables labelled differently to the heuristic.  Thus the maximum
%discrepancy (MaxDiscrepancy) is just the number of variables to be
%labelled (i.e. the length of the variable list Vars, see line 1).

%The discrepancy can be any value between 0 and the maximum (line 2).
%The search proceeds by choosing values for the discrepancy one at a
%time, starting at 0.  The between predicate instantiates Discrepancy 
%to the smallest value (0) and if this choice does not lead to a success, 
%it tries each successive value on backtracking).

%Finally an LDS search is started which seeks solutions with the given
%discrepancy from the heuristic (line 3).

%The arguments to bds_lds are as follows:
%bd_stat_lds( ?Vars,  
%         +Vals,  
%         +CurrentDiscrepancy,    (Initially zero)
%         +FinalDiscrepancy
%       )  

% If there are no more variables to label, then the current
% discrepancy IS the final discrepancy
bd_stat_lds([],[],CurrentDisc,FinalDisc) :-
        CurrentDisc=:=FinalDisc.

% Try instantiating the variable to its heuristic value
% This does not change the current discrepancy
bd_stat_lds([Var|Vars],[Val|Vals],CurrentDisc,FinalDisc) :-
        Var=Val,
        bd_stat_lds(Vars,Vals,CurrentDisc,FinalDisc).

% If the current discrepancy is less than the final one, label the
% variable to a value different from its heuristic.  As a
% result, the current discrepancy increases by one
bd_stat_lds([Var|Vars],[Val|Vals],CurrentDisc,FinalDisc) :-
       CurrentDisc<FinalDisc,
       Var~=Val,
       chooseVal(Var),
       bd_stat_lds(Vars,Vals,CurrentDisc+1,FinalDisc).

% Test to explore static limited discrepancy search

test_stat_lds(Vars,Discrepancy) :-
    Vars=[A,B,C,D,E],
    Vals=[1,2,3,4,5],
    A+B+C =:= 8, C+D+E =:= 14,
    static_lds(Vars,Vals,Discrepancy).

chooseVal(Var) :-
    between(1,10,1,Var).


/************************************************/

/*** Dynamic LDS: discrepancy = #variables  ***/


dynamic_lds(Vars,Discrepancy) :-
	length(Vars,MaxDiscrepancy),      
        between(0,MaxDiscrepancy,1,Discrepancy),
	bd_dyn_lds(Vars,0,Discrepancy).

bd_dyn_lds([],CurrentDisc,FinalDisc) :-
	CurrentDisc=:=FinalDisc.
bd_dyn_lds(Vars,CurrentDisc,FinalDisc) :-
        chooseVar(Vars,Var,Rest),
	Var tent_get Val,
        (  Var=Val,
           NewDisc=CurrentDisc 
        ;
           CurrentDisc<FinalDisc,
           Var~=Val,
           chooseVal(Var),
           NewDisc is CurrentDisc+1
        ),
        bd_dyn_lds(Rest,NewDisc,FinalDisc).

% A test to explore dynamic limited discrepancy search
test_dynamic_lds(Vars,Discrepancy) :-
    Vars=[A,B,C,D,E],
    Vals=[1,2,3,4,5],
    Vars tent_set Vals,
    A+B+C =:= 8, C+D+E =:= 14,
    dynamic_lds(Vars,Discrepancy).

chooseVar([Var|Rest],Var,Rest).

/************************************************/

/**** Bounded Backtrack Search  ********/

%ECLiPSe admits global variables which can be set and changed using
%'setval'.  These assignments are NOT undone on backtracking.  Thus 
%'setval' can be used for counting the backtracks.  This is used in the
%encoding of 'limit_backtracks' below.  Notice that when several
%successive backtrack steps are taken, without any intermediate search
%steps, this is termed a "deep" failure, and only one backtrack is
%counted.  The number of backtracks is initialised to a specified limit
%(Limit) and on every backtrack this number is decreased till it
%reaches zero. 

%At this point the ECLiPSe block exit facility is used.  On reaching
%the backtrack limit the procedure exits to a specified tag (here
%called 'exceed_limit'), and a specified procedure is executed 
%(this one outputs "Backtrack limit exceeded" and fails).

:- local variable(backtracks).
:- local variable(deep_fail).


bounded_backtrack_search(List,Limit) :-
    setval(backtracks,Limit),
    block(bbs_label(List),
          exceed_limit,
          (writeln('Backtrack limit exceeded'), fail)
         ).

bbs_label([]).
bbs_label(Vars) :-
    chooseVar(Vars,Var,Rest),
    limit_backtracks,
    chooseVal(Var),
    bbs_label(Rest).
    
limit_backtracks :-
        setval(deep_fail,false).
limit_backtracks :-
        getval(deep_fail,false),        % may fail
        setval(deep_fail,true),
        decval(backtracks),
        (getval(backtracks,0) -> exit_block(exceed_limit) ; fail).

/****************************************/

/****  BBS + Dynamic LDS   *****/

bbs_dynamic_lds(Vars,Limit,Discrepancy) :-
    length(Vars,MaxDiscrepancy),
    between(0,MaxDiscrepancy,1,Discrepancy),
    setval(backtracks,Limit),
    block(bbs_limit_lds(Vars,0,Discrepancy), 
          exceed_limit,
          (writeln('Backtrack limit exceeded'), fail)
         ). 

bbs_limit_lds(Vars,CurrentDisc,FinalDisc) :-
	limit_backtracks,
        bbs_bd_dyn_lds(Vars,CurrentDisc,FinalDisc).

bbs_bd_dyn_lds([],CurrentDisc,FinalDisc) :-
	CurrentDisc=:=FinalDisc.
bbs_bd_dyn_lds(Vars,CurrentDisc,FinalDisc) :-
        chooseVar(Vars,Var,Rest),
	Var tent_get Val,
        ( Var=Val,
          NewDisc=CurrentDisc
	  ;
          CurrentDisc<FinalDisc,
          Var~=Val,
          chooseVal(Var),
          NewDisc is CurrentDisc+1
        ),
        bbs_limit_lds(Rest,NewDisc,FinalDisc).

% A test to explore the behaviour of the combined search technique.
test_bbs_lds(Limit,List) :-
    Vars=[A,B,C,D,E],
    Vals=[1,2,3,4,5],
    Vars tent_set Vals,
    A+B+C =:= 8, C+D+E =:= 14,
    findall((Vars,Discrepancy),bbs_dynamic_lds(Vars,Limit,Discrepancy),List).

/***********************************************/

/****** lds_dist  *********/
% Find solutions with discrepancy measured in terms of
% the sum of the distances from the tentative values
% We give here an arbitrary maximum discrepancy of 10000

lds_dist(Vars,Discrepancy) :-
        between(0,10000,1,Discrepancy),        
	bd_lds_dist(Vars,0,Discrepancy).

bd_lds_dist([],CurrentDisc,FinalDisc) :-
    FinalDisc=:=CurrentDisc.

bd_lds_dist(Vars,CurrentDisc,FinalDisc) :-
    chooseVar(Vars,Var,Rest),
    (tent_num(Var,Val) ->
          (Var=Val, 
           NewDisc=CurrentDisc
           ;
           Var~=Val,
           chooseVal(Var),
           Disc is abs(Var-Val),
           NewDisc is CurrentDisc+Disc
          )
     ;
     chooseVal(Var),
     NewDisc is CurrentDisc 
    ),
    bd_lds_dist(Rest,NewDisc,FinalDisc).


% A test to explore limited discrepancy search with a measure of discrepancy 
% based on numeric difference from the heuristic value
test_dist_lds(Vars,Discrepancy) :-
    Vars=[A,B,C,D,E],
    Vals=[1,2,3,4,5],
    Vars tent_set Vals,
    A+B+C =:= 8, C+D+E =:= 14,
    lds_dist(Vars,Discrepancy).


/****** lds_opt_dist  *********/
% Illustrate flexibility, checking and optimisation.
% Same specification as lds_dist, but it
% precomputes possible discrepancies, and only explores those...

lds_opt_dist(Vars,Discrepancy) :-
    dist_sums(Vars,Discrepancies),
    member(Discrepancy,Discrepancies),
    bd_lds_dist(Vars,0,Discrepancy).   

dist_sums(Vars,Discrepancies) :-
    dyn_prog(Vars,[0],Discrepancies).

dyn_prog([],List,List).
dyn_prog([Var|Vars],List,OutList) :-
      discrepancies(Var,VarDiscrepancies),
      addup(VarDiscrepancies,List,NextList),
      sort(NextList,SortedList),
      dyn_prog(Vars,SortedList,OutList).

tent_num(Var,Val) :-
    Var tent_get Val,
    number(Val).

discrepancies(Var,[0]) :-
    not tent_num(Var,_).
discrepancies(Var,Discrepancies) :-
      tent_num(Var,Val),
% This would be simpler using finite domains!
      findall(Var,(chooseVal(Var),check_number(Var,Val)),DomList),  
      ( foreach(El,DomList), 
        foreach(Discrepancy,Discrepancies), 
        param(Val) do 
                   Discrepancy is abs(El-Val) 
      ).
check_number(Num,_) :- number(Num), !.
check_number(Val,Tent) :-
    write('The deviation of the value '), write(Val),
    write(' from the tentative value '), write(Tent),
    writeln(' cannot be computed'),
    abort.
    

addup(L1,L2,L3) :-
    (foreach(El1,L1), foreach(El1plusL2,L1plusL2), param(L2) 
    do
        (foreach(El2,L2), foreach(El1plusEl2,El1plusL2), param(El1) 
            do  El1plusEl2 is El1+El2  )
    ),
    flatten(L1plusL2,L3).

% A test to explore the optimised limited discrepancy search
lds_opt_dist_test(Vars,Discrepancy) :-
    Vars=[A,B,C,D,E],
    Vals=[1,_,3,4,5],
    tent_set_each(Vars,Vals),
    A+B+C =:= 8, C+D+E =:= 14,
    lds_opt_dist(Vars,Discrepancy).

tent_set_each(Vars,Vals) :-
    foreach(Var,Vars),
    foreach(Val,Vals)
    do
    (nonground(Val) -> true ; Var tent_set Val).




