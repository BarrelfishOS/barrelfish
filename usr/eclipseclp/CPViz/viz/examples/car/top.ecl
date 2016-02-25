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
:-lib(ic_global_gac).
:-lib(ic_global).
:-lib(util).
:-lib(timeout).
:-lib(lists).

:-use_module(structures).
:-use_module(problem).

:-lib(cpviz).

top:-
	get_flag(top/0, source_file, ThisFile),
	pathname(ThisFile, ThisDir, _),
	concat_strings(ThisDir, "data", DataDir),
        problem(DataDir,"test0.dat",Problem),
        writeq(Problem),nl,
        model(Problem,Solution0b,naive,"Viz_car_NAIVE0"),
        writeln(Solution0b),
%        problem(DataDir,"test1.dat",Problem1),
%        model(Problem1,Solution1,naive,"Viz_car_NAIVE"),
%        writeln(Solution1),
        problem(DataDir,"test1.dat",Problem2),
        model(Problem2,Solution2,regin,"Viz_car_RESULT"),
        writeln(Solution2),
        true.

model(problem{cars:Cars,
              models:Models,
              required:Required,
              using_options:List,
              value_order:Ordered},L,Method,Output):-
        length(L,Cars),
        L :: 1..Models,
        (foreach(Cnt,Required),
         count(J,1,_),
         foreach(gcc(Cnt,Cnt,J),Card) do
            true
        ),
        gcc(Card,L),
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded,
                        group:1]),

        StartPos is Models+2,
        (foreach(option{k:K,
                        n:N,
                        index_set:IndexSet,
                        binary:Binary,
                        total_use:Total,
                        pairs:Pairs},List),
         count(Group,2,_),
         fromto(StartPos,Pos,Pos1,_),
         param(L,Cars,Handle) do
            length(Binary,Cars),
            (foreach(X,L),
             count(P,1,_),
             foreach(B,Binary),
             foreach(t(B,P,group(Group,P)),Pairs),
             param(IndexSet,Group) do
                element(X,IndexSet,B)
            ),
            sequence_total(Total,Total,0,K,N,Binary),
            add_visualizer(Handle,
                           binary_vector(Binary),
                           [display:colored,
                            group:Group,
                            y:Pos]),
            Pos1 is Pos+3
/*
            add_visualizer(Handle,
                           sequence_total(Total,Total,0,K,N,Binary),
                           [display:colored,
                            group:Group,
                            y:Pos]),
            Pos1 is Pos+5+N
*/

        ),
        preference_order(L,Ordered,Pref),
        number_variables(Handle,Pref,1,VarPairs),
        !,
        root(Handle),
        timeout((Method = regin ->
                  sort(slack of option,>=,List,SortedOptions),
                  (foreach(option{pairs:Pairs},SortedOptions),
                   fromto(BinaryPairs,A1,A,[]) do
                      middle_out(Pairs,Reordered),
%                      writeln(Reordered),
                      append(Reordered,A,A1)
                  ),
%                  writeln(bin(BinaryPairs)),
                  search(BinaryPairs,1,input_order,
                         tree_indomain_max(Handle,_),complete,[]),
                  solution(Handle)
                ; Method = naive ->
                    search(VarPairs,1,input_order,
                           tree_indomain(Handle,_),
                           complete,[]),
                    solution(Handle)
                ; Method = indomain ->
                    search(VarPairs,1,input_order,
                           tree_indomain_max(Handle,_),
                           complete,[]),
                    solution(Handle)
                ;
                    writeln(wrong(Method)),
                    abort
                ),
                200,
                writeln(search_stopped)),
        close_visualization(Handle),
        viz(Handle, _).


preference_order(L,Ordered,Pref):-
        (foreach(X,L),
         foreach(Y,Pref),
         param(Ordered) do
            element(X,Ordered,Y)
        ).




