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
:-import lex_le/2, lex_lt/2 from ic_global.
:-lib(ic_global_gac).
:-lib(ic_sets).
:-lib(cpviz).

top:-
        ic:(T21 :: 0..21),
        top("Viz_sonet_T21",T21),
%        top("Viz_sonet_T22",22),
%        top("Viz_sonet_T23",23),
        true.


top(Output,T):-
        problem(NrNodes,NrRings,Demands,MaxRingSize,ChannelSize),
        length(Demands,NrDemands),
        dim(Flow,[NrDemands,NrRings]),
        ic:(Flow[1..NrDemands,1..NrRings] :: 0.0 .. 1.0),
        (for(I,1,NrDemands),
         param(Flow,NrRings) do
            (for(J,1,NrRings),
             fromto(0.0,A,A+F,Term),
             param(Flow,I) do
                subscript(Flow,[I,J],F)
            ),
            eval(Term) $= 1.0
        ),
        (for(I,1,NrRings),
         param(Flow,Demands,ChannelSize) do
            (foreach(demand(_,_,Size),Demands),
             count(J,1,_),
             fromto(0.0,A,A+Size*F,Term),
             param(Flow,I) do
                subscript(Flow,[J,I],F)
            ),
            eval(Term) $=< ChannelSize
        ),
        all_values(1,NrNodes,AllNodes),
        all_values(1,NrRings,AllRings),
        dim(Matrix,[NrNodes,NrRings]),
        ic:(Matrix[1..NrNodes,1..NrRings] :: 0..1),
        dim(Nodes,[NrNodes]),
        collection_to_list(Nodes[1..NrNodes],NodesList),
        (foreach(Node,NodesList),
         param(AllRings) do
            ic_sets:(Node :: []..AllRings)
        ),
        dim(Degrees,[NrNodes]),
        dim(NodeSizes,[NrNodes]),
        ic:(NodeSizes[1..NrNodes] :: 1..NrRings),
        dim(Rings,[NrRings]),
        collection_to_list(Rings[1..NrRings],RingsList),
        (foreach(Ring,RingsList),
         param(AllNodes) do
            ic_sets:(Ring :: []..AllNodes)
        ),
        dim(RingSizes,[NrRings]),
        ic:(RingSizes[1..NrRings] :: 0..MaxRingSize),
        (for(I,1,NrNodes),
         param(Matrix,Nodes,NodeSizes,NrRings,Degrees) do
            subscript(Degrees,[I],Degree),
            neighbors(I,Neighbors),
            length(Neighbors,Degree),
            subscript(Nodes,[I],Node),
            subscript(NodeSizes,[I],NodeSize),
            #(Node,NodeSize),
            collection_to_list(Matrix[I,1..NrRings],List),
            Array =..[[]|List],
            membership_booleans(Node,Array)
        ),
        (for(J,1,NrRings),
         param(Matrix,Rings,RingSizes,NrNodes) do
            subscript(Rings,[J],Ring),
            subscript(RingSizes,[J],RingSize),
            RingSize #\= 1,
            #(Ring,RingSize),
            collection_to_list(Matrix[1..NrNodes,J],List),
            Array =..[[]|List],
            membership_booleans(Ring,Array)
        ),
        (foreach(demand(I,J,_Size),Demands),
         param(Nodes,NrRings) do
            subscript(Nodes,[I],NI),
            subscript(Nodes,[J],NJ),
            ic_sets:intersection(NI,NJ,Intersect),
            ic:(NonZero :: 1..NrRings),
            #(Intersect,NonZero)
        ),
        (foreach(demand(From,To,_),Demands),
         count(I,1,_),
         param(Flow,Matrix,NrRings) do
            (for(K,1,NrRings),
             param(I,From,To,Flow,Matrix) do
                subscript(Flow,[I,K],F),
                subscript(Matrix,[From,K],X1),
                subscript(Matrix,[To,K],X2),
                F $=< X1,
                F $=< X2
            )
        ),
        needs_two_rings(NrNodes,MaxRingSize,NodeSizes,Degrees),
        three_bound(Demands,NodeSizes,MaxRingSize,Degrees),
        four_bound(Demands,NodeSizes,MaxRingSize,Degrees),
        degree_bound(NodeSizes,NrNodes,Degrees),
        non_connected_bound(NrNodes,Demands,NodeSizes,MaxRingSize,Degrees),
        collection_to_list(NodeSizes[1..NrNodes],NodeSizesList),
        create_visualization([output:Output,
                              range_to:5000,
                              var_arg:1,
                              name_arg:2,
                              focus_arg:3],Handle),
        add_visualizer(Handle,
                       binary_matrix(Matrix),
                       [group:1,
                        display:text]),
        XX is NrRings+1,
        add_visualizer(Handle,
                       vector(NodeSizes),
                       [x:XX,
                        group:2,
                        display:text]),
        sumlist(NodeSizesList,T),
        assign(T,Handle,NrNodes,Degrees,NodeSizes,Matrix),
        close_visualization(Handle),
        viz(Handle, _).

assign(T,Handle,NrNodes,Degrees,NodeSizes,Matrix):-
        root(Handle),
        tree_indomain(t(T,sum,group(0,0)),Handle,_),
        order_sizes(NrNodes,Degrees,NodeSizes,OrderedSizes),
        search(OrderedSizes,2,input_order,tree_indomain(Handle,_),
               complete,[]),
%        writeln(NodeSizes),
        order_vars(Degrees,NodeSizes,Matrix,VarAssign),
        search(VarAssign,3,input_order,tree_indomain_max(Handle,_),
               complete,[]),
        solution(Handle),
        !.
assign(_T,_Handle,_NrNodes,_Degrees,_NodeSizes,_Matrix).

% order node-size variables by increasing degree
order_sizes(NrNodes,Degrees,NodeSizes,OrderedSizes):-
        (for(I,1,NrNodes),
         foreach(t(X,Name,group(2,I),D),Terms),
         param(Degrees,NodeSizes) do
            concat_string([n,I],Name),
            subscript(Degrees,[I],D),
            subscript(NodeSizes,[I],X)
        ),
        sort(4,=<,Terms,OrderedSizes).




% order 0/1 variables by (a) increasing size (b) decreasing degree and
% (c) increasing ring number
order_vars(Degrees,NodeSizes,Matrix,VarAssign):-
        dim(Matrix,[NrNodes,NrRings]),
        dim(Reordered,[NrNodes,NrRings]),
        (for(I,1,NrNodes),
         foreach(t(Size,Y,I),Terms),
         param(Degrees,NodeSizes) do
            subscript(NodeSizes,[I],Size),
            subscript(Degrees,[I],Degree),
            Y is -Degree
        ),
        sort(0,=<,Terms,Sorted),
%        writeln(Sorted),
        (foreach(t(_,_,I),Sorted),
         count(K,1,_),
         fromto(VarAssign,A1,A,[]),
         param(NrRings,Matrix,Reordered) do
            (for(J,1,NrRings),
             fromto(A1,[t(X,Name,group(1,I-J))|AA],AA,A),
             param(I,K,Matrix,Reordered) do
                concat_string([x,I,"_",J],Name),
                subscript(Matrix,[I,J],X),
                subscript(Reordered,[K,J],X)
            )
        ),
        (for(J,1,NrRings-1),
         param(NrNodes,Reordered) do
            J1 is J+1,
            lex_le(Reordered[1..NrNodes,J1],Reordered[1..NrNodes,J])
        ).

non_connected_bound(NrNodes,Demands,NodeSizes,MaxRingSize,Degrees):-
        (for(I,1,NrNodes-1),
         param(Demands,MaxRingSize,NrNodes,NodeSizes,Degrees) do
            (for(I1,I+1,NrNodes),
             param(Demands,MaxRingSize,I,NodeSizes,Degrees) do
                one_non_connected_bound(I,I1,Demands,
                                        MaxRingSize,NodeSizes,
                                        Degrees)
            )
        ).

one_non_connected_bound(I,I1,Demands,MaxRingSize,NodeSizes,Degrees):-
        not member(demand(I,I1,_),Demands),
        subscript(Degrees,[I],DI),
        subscript(Degrees,[I1],DI1),
        DI =< MaxRingSize-1,
        DI1 =< MaxRingSize-1,
        neighbors(I,NeighI),
        neighbors(I1,NeighI1),
        eclipse_language:intersection(NeighI,NeighI1,Common),
        member(K,Common),
        subscript(Degrees,[K],DK),
        neighbors(K,NeighK),
        DK >= MaxRingSize,
        eclipse_language:union(NeighI,NeighI1,Set1),
        eclipse_language:union(Set1,NeighK,AllThree),
        length(AllThree,N),
        N > 2*MaxRingSize-1,
        !,
        subscript(NodeSizes,[I],Xi),
        subscript(NodeSizes,[I1],Xi1),
        subscript(NodeSizes,[K],Xk),
        connect_trigger(Xi,Xi1,Xk).
one_non_connected_bound(_I,_I1,_Demands,_MaxRingSize,_NodeSizes,_Degrees).
        
delay connect_trigger(X,Y,Z) if nonground(X);nonground(Y).
connect_trigger(X,Y,Z):-
%        writeln(connect_trigger(X,Y,Z)),
        (X == 1, Y ==1 ->
            Z #>= 3
        ;
            true
        ).
        

needs_two_rings(NrNodes,MaxRingSize,NodeSizes,Degrees):-
        (for(J,1,NrNodes),
         param(NodeSizes,MaxRingSize,Degrees) do
            subscript(NodeSizes,[J],NodeSize),
            subscript(Degrees,[J],D),
            Bound is integer(ceiling(D/(MaxRingSize-1))),
%            writeln(bound(J,D,Bound,NodeSize)),
            NodeSize #>= Bound
        ).

three_bound(Demands,NodeSizes,MaxRingSize,Degrees):-
        (foreach(demand(I,J,_),Demands),
         param(NodeSizes,MaxRingSize,Degrees) do
            one_three_bound(I,J,NodeSizes,MaxRingSize,Degrees)
        ).

one_three_bound(I,J,NodeSizes,MaxRingSize,Degrees):-
        subscript(Degrees,[I],DI),
        subscript(Degrees,[J],DJ),
        DI < MaxRingSize,
        DJ < MaxRingSize,
        neighbors(I,NeighI),
        neighbors(J,NeighJ),
        eclipse_language:union(NeighI,NeighJ,Union),
        length(Union,NU), % includes I and J
        NU >= MaxRingSize+1,
%        writeln(three(I,J,NeighI,NeighJ,DI,DJ,Union,NU)),
        !,
        subscript(NodeSizes,[I],VI),
        subscript(NodeSizes,[J],VJ),
        VI+VJ #>= 3.
one_three_bound(_I,_J,_NodeSizes,_,_).

four_bound(Demands,NodeSizes,MaxRingSize,Degrees):-
        (foreach(demand(I,J,_),Demands),
         param(NodeSizes,MaxRingSize,Degrees) do
            one_four_bound(I,J,NodeSizes,MaxRingSize,Degrees)
        ).

one_four_bound(I,J,NodeSizes,MaxRingSize,Degrees):-
        subscript(Degrees,[I],DI),
        subscript(Degrees,[J],DJ),
        ( 
            DI >= MaxRingSize,
            DJ < MaxRingSize
        ;
            DI < MaxRingSize,
            DJ >= MaxRingSize
        ),
        neighbors(I,NeighI),
        neighbors(J,NeighJ),
        eclipse_language:union(NeighI,NeighJ,Union),
        length(Union,NU), % includes I and J
        NU > 2*MaxRingSize-1,
%        writeln(four(I,J,NeighI,NeighJ,DI,DJ,Union,NU)),
        !,
        subscript(NodeSizes,[I],VI),
        subscript(NodeSizes,[J],VJ),
        VI+VJ #>= 4.
one_four_bound(_I,_J,_NodeSizes,_,_).

degree_bound(NodeSizes,NrNodes,Degrees):-
        (for(I,1,NrNodes),
         param(NodeSizes,Degrees) do
            subscript(Degrees,[I],DI),
            subscript(NodeSizes,[I],NodeSize),
%            writeln(degree(DI)),
            NodeSize #=< DI
        ).

neighbors(N,List):-
        problem(_,_,Demands,_,_),
        (foreach(demand(I,J,_),Demands),
         fromto([],A,A1,List),
         param(N) do
            (N = I ->
                A1 = [J|A]
            ; N = J ->
                A1 = [I|A]
            ;
                A1 = A
            )
        ).

all_values(N,M,[]):-
        N > M,
        !.
all_values(N,M,[N|R]):-
        N1 is N+1,
        all_values(N1,M,R).

/*
problem(13,7,[demand(1,2,1),
              demand(1,3,1),
              demand(1,4,1),
              demand(1,10,1),
              demand(1,11,1),
              demand(1,12,1),
              demand(2,7,1),
              demand(2,8,1),
              demand(3,4,1),
              demand(3,6,1),
              demand(3,9,1),
              demand(3,10,1),
              demand(3,12,1),
              demand(3,13,1),
              demand(5,6,1),
              demand(5,7,1),
              demand(6,7,1),
              demand(6,8,1),
              demand(6,12,1),
              demand(7,12,1),
              demand(8,11,1),
              demand(8,13,1),
              demand(9,11,1),
              demand(11,13,1)
              ],5,40).
*/

problem(13,7,[demand(1,9,8),
              demand(1,11,2),
              demand(2,3,25),
              demand(2,5,5),
              demand(2,9,2),
              demand(2,10,3),
              demand(2,13,4),
              demand(3,10,2),
              demand(4,5,4),
              demand(4,8,1),
              demand(4,11,5),
              demand(4,12,2),
              demand(5,6,5),
              demand(5,7,4),
              demand(7,9,5),
              demand(7,10,2),
              demand(7,12,6),
              demand(8,10,1),
              demand(8,12,4),
              demand(8,13,1),
              demand(9,12,5),
              demand(10,13,9),
              demand(11,13,3),
              demand(12,13,2)
              ],5,40).


% order node set variables by (a) increasing size and (b) decreasing degree
order_sets(Degrees,NodeSizes,Nodes,Sorted):-
        dim(Nodes,[NrNodes]),
        (for(I,1,NrNodes),
         foreach(t(Size,Y,Node),Terms),
         param(Degrees,NodeSizes,Nodes) do
            subscript(NodeSizes,[I],Size),
            subscript(Degrees,[I],Degree),
            subscript(Nodes,[I],Node),
            Y is -Degree
        ),
        sort(0,=<,Terms,Sorted).

assign_sets(L):-
        (foreach(t(_,_,Set),L) do
            insetdomain(Set,increasing,small_first,in_notin)
        ).

