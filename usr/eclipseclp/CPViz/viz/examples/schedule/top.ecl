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
% The Original Code is  CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are
% Copyright (C) 2009-2010 Helmut Simonis
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
:-module(top).
:-export(top/0).

:-lib(ic).
:-lib(ic_cumulative).
:-lib(ic_edge_finder).
:-lib(ic_edge_finder3).
:-lib(branch_and_bound).
:-lib(lists).
:-lib(util).
:-lib(cpviz).

% setup fails with Limit=132, setup works with Limit=133, this is
% still weak

top:-
%        top(1,"Viz_schedule_NAIVE",naive),
        top(1,"Viz_schedule_RESULT",improved).

top(Seed,Dir,Quality):-
        data(L),
        seed(Seed),
        Limit = 136,
%        M = ic_cumulative,
%        M = ic_edge_finder,
        M = ic_edge_finder3,
        generate_tasks(L,Limit,Tasks,Start,End,Dur,Total,Quality),
        length(Start,N),
        length(Ones,N),
        Ones :: 1..1,
        length(Fives,N),
        Fives :: 5..5,
       
        TotalEnd :: 0..Limit,
        (Quality \= naive ->
            S :: 0..Limit,
            D = 5,
            S+5 #=TotalEnd,
            lower_bound([D|Dur],LowerBound),
            TotalEnd #>= LowerBound
        ;
            true
        ),
        ic:max(End,TotalEnd),
        create_visualization([output:Dir,range_to:300],Handle),
        M:cumulative(End,Fives,Ones,1),
        M:cumulative(Start,Total,Ones,3),
        add_visualizer(Handle,cumulative(End,Fives,Ones,1),
                       [y:0,group:1]),
        add_visualizer(Handle,cumulative(End,Fives,Ones,1),
                       [y:5,display:gantt,group:1]),
        (Quality = naive ->
            M:cumulative(Start,Dur,Ones,2),
            add_visualizer(Handle,cumulative(Start,Dur,Ones,2,TotalEnd),
                           [y:30,group:2]),
            add_visualizer(Handle,cumulative(Start,Dur,Ones,2,TotalEnd),
                           [y:35,display:gantt,
                            group:2])
        ;
            M:cumulative([S|Start],[D|Dur],[1|Ones],2),
            add_visualizer(Handle,cumulative([S|Start],[D|Dur],
                                             [1|Ones],2,
                                             TotalEnd),
                           [y:30,group:2]),
            add_visualizer(Handle,cumulative([S|Start],[D|Dur],
                                             [1|Ones],2,
                                             TotalEnd),
                           [y:35,display:gantt,
                            group:2])
        ),

        add_visualizer(Handle,cumulative(Start,Total,Ones,3),
                       [y:60,group:3]),
        add_visualizer(Handle,cumulative(Start,Total,Ones,3),
                       [y:65,display:gantt,group:3]),
        root(Handle),
        writeln(search),
        !,
        
        search(Tasks,[0,0],3,Handle,Quality), % use 2 to prove optimality
        writeq(found(Start)),nl,
       solution(Handle),
%        draw_visualization(Handle),
        close_visualization(Handle),
        viz(Handle, _).
        

lower_bound(Dur,Bound):-
        (foreach(D,Dur),
         fromto(0,D1,D2,Total) do
            D2 is D1+D
        ),
        Bound is integer(ceiling(Total/2)).

generate_tasks(Jobs,Limit,Tasks,Start,End,Dur,Total,Quality):-
        gen_jobs(Jobs,1,Limit,Tasks,[],Start,[],End,[],Dur,[],Total,Quality).

gen_jobs([],_Nr,_Limit,[],Start,Start,End,End,Dur,Dur,Total,Total,_).
gen_jobs([job(Times,Dur)|R],Nr,Limit,[JobTask|Tasks],
         S,Send,E,End,D,Dend,T,Tend,Quality):-
        gen_job(Nr,Times,Dur,Limit,JobTask,S,S1,E,E1,D,D1,T,T1),
        (Quality \= naive ->
            order_ends(JobTask,5)
        ;
            true
        ),
        Nr1 is Nr+1,
        gen_jobs(R,Nr1,Limit,Tasks,S1,Send,E1,End,D1,Dend,T1,Tend,Quality).

gen_job(_Nr,0,_,_,[],S,S,E,E,D,D,T,T):-
        !.
gen_job(Nr,Times,Dur,Limit,[task(Start,Dur,End,Name)|Tasks],
        S,Send,E,Eend,D,Dend,T,Tend):-
        Times1 is Times-1,
        [Start,End] :: 0..Limit,
        End #= Start+Dur,
        Total is Dur+1+5,
        sprintf(Name,"t%d_%d",[Nr,Times]),
        gen_job(Nr,Times1,Dur,Limit,Tasks,[Start|S],Send,[End|E],Eend,
                [Dur|D],Dend,[Total|T],Tend).


order_ends([task(_,_,End1,_),task(_,_,End2,_)|R],Dist):-
        !,
        End2 #>= End1+Dist,
        order_ends([task(_,_,End2,_)|R],Dist).
order_ends(_,_).

/* 
labeling by selecting task and forcing at next free value; allow
 relaxation 
*/

% argument is a list of sets of tasks
% each iteration removes one task
search([],_Times,_Relax,_,_).
search([H|T],Times,Relax,Handle,Quality):-
        fixit([H|T],Rest,Times,Times1,Relax,Relax1,Handle,Quality),
%        shuffle(Rest,Rest1),
        Rest1 = Rest,
        draw_visualization(Handle),
        search(Rest1,Times1,Relax1,Handle,Quality).



% choicepoint
fixit([H|T],Rest,Times,Times1,Relax,Relax1,Handle,Quality):-
        fixit_first(H,T,Rest,Times,Times1,Relax,Relax1,Handle,Quality).
fixit([H|T],[H|Rest],Times,Times1,Relax,Relax1,Handle,Quality):-
        fixit(T,Rest,Times,Times1,Relax,Relax1,Handle,Quality).

% deterministic
% take the first of set of jobs, or don't take this set at all
% if last task of set, remove set
fixit_first([task(X,Dur,_,Name)],T,T,[A,B],[C,D],Relax,Relax1,Handle,Quality):-
        use(Name,X,Dur,A,B,C,D,Relax,Relax1,Handle,Quality),
        !.
fixit_first([task(X,Dur,_,Name)|H],T,[H|T],[A,B],[C,D],Relax,Relax1,Handle,
            Quality):-
        use(Name,X,Dur,A,B,C,D,Relax,Relax1,Handle,Quality),
        !.

% choicepoint
% assign start time, either at earliest possible time or with
% relaxation
% remember you have used up this amount of relaxation
use(_Name,X,_Dur,A,B,A,B,Relax,Relax,_Handle,Quality):-
        Quality \= naive,
        integer(X),
        !.
use(Name,X,Dur,A,B,C,D,Relax,Relax,Handle,_):-
        sprintf(Choice,"%w - %w",[Name,A]),
        (X = A,true ->
            try_c(Handle,"",-1,Choice)
        ;
            failure_c(Handle,"",-1,Choice),
            fail
        ),
        New is A+Dur,
        order(New,B,C,D).
use(Name,X,Dur,A,B,C,D,Relax,Relax1,Handle,_):-
        Relax > 0,
        between(1,Relax,Amount), % another choicepoint
        Relax1 is Relax-Amount,
        V is A+Amount,
        sprintf(Choice,"%w - %w",[Name,V]),
        (X = V, true ->
            try_c(Handle,"",-1,Choice)
        ;
            failure_c(Handle,"",-1,Choice),
            fail
        ),

        New is A+Amount+Dur,
        order(New,B,C,D).

order(A,B,A,B):-
        A =< B,
        !.
order(A,B,B,A).


/*

Data

*/

data([job(9,10),
      job(5,12),
      job(1,15),
      job(2,16),
      job(3,22)
      ]).



