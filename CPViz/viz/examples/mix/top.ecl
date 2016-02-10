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
:-lib(ic_cumulative).
:-lib(ic_global).
:-lib(ic_global_gac).
:- import lex_le/2, lex_lt/2 from ic_global.
:-lib(cpviz).

top:-
        mix(8,"Viz_mix_RESULT").
    
mix(N,Output):-
        N1 is N+2,
        N2 is 2*N+4,
        N3 is 3*N+6,
        N4 is 4*N+8,
        length(L,N),
        L :: 1..N,
        length(K,N),
        K :: 1..N,
        length(Ones,N),
        Ones :: 1..1,
        length(Bool,N),
        Bool :: 0..1,
        length(Bins,N),
        Bins :: 0..N,
        (for(J,1,N),
         foreach(gcc(0,1,J),GCCParam),
         foreach(gcc(0,2,J),GCCParam2) do
            true
        ),
        (for(_,1,N),
         foreach(GCCParam,RowParam),
         param(GCCParam) do
            true
        ),
        (for(_,1,N),
         foreach(GCCParam2,ColParam),
         param(GCCParam2) do
            true
        ),
        dim(Matrix,[N,N]),
        Matrix[1..N,1..N] :: 1..N,
        collection_to_list(Matrix[1,1..N],L),
        collection_to_list(Matrix[2,1..N],K),
        random_list(N,RandomN),
        
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [x:0]),
        add_visualizer(Handle,
                       vector(K),
                       [x: N1]),
        add_visualizer(Handle,domain_matrix(Matrix),[x:0,y:N1]),
        
        ic_global:alldifferent(L),
        add_visualizer(Handle,alldifferent(L),[x:N2]),

        bin_packing(L,RandomN,Bins),
        add_visualizer(Handle,bin_packing(L,RandomN,Bins),[x:N3]),

        cumulative(L,Ones,Ones,1),
        add_visualizer(Handle,cumulative(L,Ones,Ones,1),[display:gantt,x:N1,y:N4]),
        add_visualizer(Handle,cumulative(L,Ones,Ones,1),[x:N2,y:N4]),
        
        lex_le(L,K),
        add_visualizer(Handle,lex_le(L,K),[x:N1,y:N1]),

        lex_lt(L,K),
        add_visualizer(Handle,lex_lt(L,K),[x:N2,y:N1]),

        inverse(L,K),
        add_visualizer(Handle,inverse(L,K),[x:N3,y:N1]),

        same(L,K),
        add_visualizer(Handle,same(L,K),[x:0,y:N2]),

        gcc(GCCParam,L),
        add_visualizer(Handle,gcc(GCCParam,L),[x:N1,y:N2]),
        
 
        build_rect(L,K,Rect),
        Na is N+1,
        W :: 1..Na,
        H :: 1..Na,
        disjoint2(Rect,W,H),
        add_visualizer(Handle,disjoint2(Rect,W,H),[x:0,y:N4]),
        

        reverse(L,LL),
        LL = [X8|_],
        element(X8,RandomN,C),
        add_visualizer(Handle,element(X8,RandomN,C),[x:N3,y:N2]),
        
        bool_channeling(X8,Bool,1),
        add_visualizer(Handle,bool_channeling(X8,Bool,1),[x:0,y:N3]),

%        alldifferent_matrix(Matrix),
        add_visualizer(Handle,alldifferent_matrix(Matrix),[x:N1,y:N3]),

%        gcc_matrix(MatrixParam,MatrixParam,Matrix),
        add_visualizer(Handle,gcc_matrix(RowParam,ColParam,Matrix),[x:N2,y:N3]),
        
        number_variables(Handle,L,Pairs),

%        extract_array(Handle,row,Matrix,NamedList),

        
        root(Handle),
        search(Pairs,1,first_fail,tree_indomain(Handle,_),complete,[]),
%        search(NamedList,1,input_order,tree_indomain(Handle,Handle),complete,[]),
        solution(Handle),
        close_visualization(Handle),
        writeln(L),
        writeln(K),
        viz(Handle, _).


random_list(N,L):-
        (for(_,1,N),
         foreach(X,L),
         param(N) do
            X is 1 + (random mod N)
        ).


build_rect(L,K,Rect):-
        (foreach(X,L),
         foreach(Y,K),
         foreach(rect(X,Y,W,H),Rect) do
            W =1,
            H=1
        ).

disjoint2(Rect,Width,Height):-
        (foreach(rect(X,Y,W,H),Rect),
         param(Width,Height) do
            X+W #=< Width,
            Y+H #=< Height
        ),
        disj(Rect).

disj([]).
disj([_]):-
        !.
disj([A|R]):-
        disj2(A,R),
        disj(R).

disj2(_,[]).
disj2(A,[B|R]):-
        disjoint_rect(A,B),
        disj2(A,R).

disjoint_rect(rect(X1,Y1,W1,H1),rect(X2,Y2,W2,H2)):-
        (X1 +W1 =< X2) or (X2+W2 =< X1) or (Y1+H1 =< Y2) or (Y2+H2 =<
                                                                   Y1).


