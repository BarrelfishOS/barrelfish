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
:-lib(ic_global).
:-lib(ic_global_gac).
:-lib(graph_algorithms).
:-lib(branch_and_bound).
:-lib(util).
:-lib(cpviz).
:-use_module('data').

top:-
        seed(8),
        top(mci,100).

 
top(Name,NrDemands):-
        problem(Name,NrDemands,Network,Demands),
        route(Network,Demands,Routes),
        wave(NrDemands,Routes,LowerBound,Assignment,Max),
        writeln(LowerBound),
        writeln(Assignment),
        writeln(Max).


route(Network,Demands,Routes):-
        (foreach(demand(I,From,To),Demands),
         foreach(route(I,Path),Routes),
         param(Network) do
            single_pair_shortest_path(Network,-1,From,To,_-Path)
        ).

wave(NrDemands,Routes,LowerBound,Var,Max):-
	concat_string(["Viz_wave_",NrDemands],Output),
        UpperBound = 20,
        dim(Var,[NrDemands]),
        Var[1..NrDemands] :: 1..UpperBound,
        ic:max(Var,Max),
        setup_alldifferent(Routes,Var,LowerBound),
        Xmax is NrDemands+1,
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       vector(Var),
                       [group:1]),
        add_visualizer(Handle,
                       vector([Max]),
                       [group:2,
                        x:Xmax]),
        root(Handle),
        bb_min(assign(Var,Handle),Max,bb_options{from:LowerBound,
                                          timeout:100}),
        close_visualization(Handle),
	viz(Handle, _).


assign(Var,Handle):-
        collection_to_list(Var,List),
        number_variables(Handle,List,1,Terms),
        search(Terms,1,most_constrained,tree_indomain(Handle,_),
               complete,[]),
        solution(Handle).

        
setup_alldifferent(Routes,Var,LowerBound):-
        (foreach(route(I,Path),Routes),
         fromto([],A,A1,Pairs) do
            (foreach(Edge,Path),
             fromto(A,AA,[l(Edge,I)|AA],A1),
             param(I) do
                true
            )
        ),
        group(Pairs,1,Groups),
        (foreach(_-Group,Groups),
         fromto(0,A,A1,LowerBound),
         param(Var) do
            length(Group,N),
            A1 is eclipse_language:max(N,A),
            (foreach(l(_,I),Group),
             foreach(X,AlldifferentVars),
             param(Var) do
                subscript(Var,[I],X)
            ),
%            ic_global_gac:alldifferent(AlldifferentVars)
            ic_global:alldifferent(AlldifferentVars)
%            ic:alldifferent(AlldifferentVars)
        ).


problem(Name,NrDemands,Network,Demands):-
        network_topology(Name,NrNodes,Edges),
        make_graph(NrNodes,Edges,Directed),
        make_undirected_graph(Directed,Network),
        (for(I,1,NrDemands),
         fromto([],A,[demand(I,From,To)|A],Demands),
         param(NrNodes) do
            repeat,
            From is 1+(random mod NrNodes),
            To is 1+(random mod NrNodes),
            From \= To,
            !
        ).


% list processing auxiliary

group(List,Arg,Groups):-
        integer(Arg),
        !,
        sort(Arg,=<,List,[H|T]),
        extract_arg(Arg,H,V),
        lp(T,Arg,[H],V,Groups).
group(List,Arg1+Arg2,Groups):-
        integer(Arg1),
        integer(Arg2),
        !,
        sort(Arg1,=<,List,List1),
        sort(Arg2,=<,List1,[H|T]),
        extract_arg(Arg1+Arg2,H,V),
        lp(T,Arg1+Arg2,[H],V,Groups).

lp([],_,Last,LastV,[LastV-Last]).
lp([H|T],Arg,Old,V,Groups):-
        extract_arg(Arg,H,V),
        !,
        lp(T,Arg,[H|Old],V,Groups).
lp([H|T],Arg,Old,OldV,[OldV-Old|Groups]):-
        extract_arg(Arg,H,V),
        lp(T,Arg,[H],V,Groups).

extract_arg(0,Term,V):-
        !,
        Term = V.
extract_arg(A,Term,V):-
        integer(A),
        !,
        arg(A,Term,V).
extract_arg(A1+A2,Term,V1+V2):-
        arg(A1,Term,V1),
        arg(A2,Term,V2).
