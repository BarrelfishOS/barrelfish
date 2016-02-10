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
:- module(top).

:-export(top/0).

:-lib(ic).
:-lib(lists).
:-lib(cpviz).

top:-
        nqueen(naive,4,"Viz_queen4_NAIVE",no,expanded),
        nqueen(naive,8,"Viz_queen8_NAIVE",no,expanded),
%        nqueen(naive,16,"Viz_queen16_NAIVE",no,compact),
        nqueen(first_fail,16,"Viz_queen16_FF",yes,expanded),
        nqueen(middle,16,"Viz_queen16_MIDDLE",yes,expanded),
        nqueen(credit,94,"Viz_queen94_CREDIT",yes,expanded),
        true.


nqueen(Type,N,Output,IgnoreFixed,Display):-
        length(L,N),
        L :: 1..N,
        alldifferent(L),
        noattack(L),
        create_visualization([output:Output,
                              ignore_fixed:IgnoreFixed,
                              range_to:3000],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        number_variables(Handle,L,Pairs),
        root(Handle),
        (Type = credit ->
            NSq is N*N,
            middle_out(Pairs,Reordered),
            search(Reordered,1,first_fail,tree_indomain_middle(Handle,_),
                   credit(NSq,10),[])
        ; Type = naive ->
            search(Pairs,1,input_order,tree_indomain(Handle,_),complete,[])
        ; Type = first_fail ->
            search(Pairs,1,first_fail,tree_indomain_min(Handle,_),complete,[])
        ; Type = middle ->
            middle_out(Pairs,Reordered),
            search(Reordered,1,first_fail,tree_indomain_middle(Handle,_),
                   complete,[])
        ;
            writeln(wrong_type(Type)),
            abort
        ),
        solution(Handle),
        close_visualization(Handle),
	viz(Handle, [tool{show:tree,display:Display},tool{show:viz}]),
        true.


noattack([]).
noattack([H|T]):-
        noattack1(H,T,1),
        noattack(T).

noattack1(_,[],_).
noattack1(X,[Y|R],N):-
        X #\= Y+N,
        Y #\= X+N,
        N1 is N+1,
        noattack1(X,R,N1).

    
