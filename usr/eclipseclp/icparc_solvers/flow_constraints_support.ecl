:- module(flow_constraints_support).

:-lib(hash).

:- export struct(low_high(low,high,mapping)).

:- export residual_graph/5.

residual_graph(MaxFlowEdges,Edges,ResidualEdges,Residual2,FlowSolution):-
        hash_create(Hash),
        hash_create(FlowSolution),
        (foreach(Flow-Edge,MaxFlowEdges),
         fromto([],A,A1,Residual1),
         param(Hash,FlowSolution) do
            store_flow(FlowSolution,Edge,Flow),
            hash_add(Hash,Edge,1),
            forward_edge(Flow,Edge,A,AA),
            back_edge(Flow,Edge,AA,A1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,Residual2),
         param(Hash,FlowSolution) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                store_flow(FlowSolution,Edge,0),
                forward_edge(0,Edge,A,A1)
            )
        ),
        append(Residual1,Residual2,ResidualEdges).
%        writeln(res1(Residual1)),
%        writeln(res2(Residual2)),
%        writeln(res(ResidualEdges)).

store_flow(Hash,e(_From,_To,low_high{mapping:M}),Flow):-
        hash_add(Hash,M,Flow).

forward_edge(Flow,e(From,To,low_high{high:High}),
             A,[e(From,To,1)|A]):-
        High > Flow,
        !.
forward_edge(_,_,A,A).

back_edge(Flow,e(From,To,low_high{low:Low}),
             A,[e(To,From,1)|A]):-
        Flow > Low,
        !.
back_edge(_,_,A,A).

/*
extract the list of edges whose ends are in different SCC
StrongComponents is a list of list of node indices
*/

:- export mark_components/4.

mark_components(StrongComponents,NrNodes,MidEdges,NotMarked):-
        dim(Components,[NrNodes]),
        (foreach(Set,StrongComponents),
         count(J,1,_),
         param(Components) do
            (foreach(Sx,Set),
             param(J,Components) do
                subscript(Components,[Sx],J)
            )
        ),
        (foreach(Edge,MidEdges),
         fromto([],A,A1,NotMarked),
         param(Components) do
            marked(Edge,Components,A,A1)
        ).

% if ends are in same SCC, ignore the edge
% otherwise add to accumulator
marked(e(From,To,_W),Components,A,A):-
        subscript(Components,[From],X),
        subscript(Components,[To],X),
        !.
marked(Edge,_Components,A,[Edge|A]).
