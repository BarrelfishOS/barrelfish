%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% The Original Code is The Maximum Flow Library
% The Initial Developer of the Original Code is  CrossCore Optimization Ltd.
% Portions created by the Initial Developer are  Copyright (C) 2006-2007.
% All Rights Reserved.
% 
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(max_flow).
:- comment(categories, ["Algorithms"]).
:-comment(summary,"Ford-Fulkerson maximum flow algorithm").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(graph_algorithms).
:-lib(hash).

:-export(max_flow/5).
:-comment(max_flow/5,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network."),
              fail_if: "There is no feasible flow between Source and Sink"
                       " nodes (they are not connected).",
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow:max_flow_with_lb/6,
                        max_flow:max_flow_with_lb/8,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
                        
          ]
         ).

:-export(max_flow/7).
:-comment(max_flow/7,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow",
                       "MaxFlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "MaxFlowEdgesGraph": "a graph structure, original"
                                            " nodes (as in Graph) but only"
                                            " the edges that are in max flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network, a list of all"
                        " edges with non-zero flow, and a graph of the"
                        " edges with non-zero flow."),
              fail_if: "There is no feasible flow between Source and Sink"
                       " nodes (they are not connected).",
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow:max_flow_with_lb/6,
                        max_flow:max_flow_with_lb/8,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
          ]
         ).


max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue):-
		do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, _, _, _). 
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph):-
        max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
                 MaxFlowEdges, MaxFlowEdgesGraph,_).
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph, ResidualCapacities):-
        do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, 
                    N, Edges, ResidualCapacities),
        get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities,
                           MaxFlowEdges,MaxFlowEdgesGraph). 

do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, N, Edges, ResidualCapacities) :-
        graph_get_maxnode(Graph,N),
        graph_get_all_edges(Graph,Edges),
        initialize_residual_capacities(N,Edges,CapacityArg,ResidualCapacities),
        max_flow_aux(N,ResidualCapacities,SourceNode,SinkNode,0,MaxFlowValue). 

max_flow_aux(N,ResidualCapacities,SourceNode, SinkNode, 
             FlowValue, MaxFlowValue):-
        
        dim(NodeLabels,[N]),
        dim(Predecessors,[N]),
        initialize_node_labels(NodeLabels),
        initialize_predecessor_array(Predecessors),
        
        label(NodeLabels,SourceNode),
        (
            find_path(ResidualCapacities, SinkNode,
                      NodeLabels, Predecessors, [SourceNode])
        ->
            augment(ResidualCapacities,SourceNode, SinkNode,
                    Predecessors,FlowValue,NewFlowValue), 
            max_flow_aux(N,ResidualCapacities, SourceNode,
                         SinkNode, NewFlowValue, MaxFlowValue) 
        ;
            MaxFlowValue = FlowValue
        ).
        
        
find_path(_ResidualCapacities, SinkNode, NodeLabels,
          _Predecessors, 
          _List):-
        labeled(NodeLabels,SinkNode),
        !.

find_path(ResidualCapacities, SinkNode, NodeLabels,
          Predecessors, 
          List):-
        List = [I|Rest],
        get_valid_adjacent_nodes(ResidualCapacities,I,Js),
        (
            foreach(J,Js),
            fromto(Rest,In,Out,NewList),
            param(NodeLabels, Predecessors, I)
        do
            (
                unlabeled(NodeLabels,J)
            ->
                setarg(J,Predecessors,I),
                label(NodeLabels,J),
                Out = [J|In]
            ;
                In = Out
            )
        ),
        
        find_path(ResidualCapacities, SinkNode, NodeLabels,
                  Predecessors, 
                  NewList).
        
            
augment(ResidualCapacities, SourceNode, SinkNode,
        Predecessors, FlowValue, NewFlowValue):-
        arg(SinkNode,Predecessors,Pred),
        
        % residual_capacity of the last edge of the path
        get_edge_residual_capacity(ResidualCapacities,Pred,SinkNode,
                                   ResCapacity),
        
        augment_aux(ResidualCapacities,SourceNode,
                    Predecessors,Pred,ResCapacity,
                    [e(Pred,SinkNode,ResCapacity)],
                    MinResCapacity, Path), 

        update_residual_capacities(ResidualCapacities, Path, MinResCapacity),
        NewFlowValue is FlowValue + MinResCapacity.
        

augment_aux(_ResidualCapacities,SourceNode,_Predecessors,
            SourceNode,MinResCapacity,Path, MinResCapacity, Path):- 
        % source node reached, stop
        !.
augment_aux(ResidualCapacities,SourceNode,Predecessors,CurrentNode,
            CurrentMinResCapacity, PartialPath, MinResCapacity,Path):-  
        arg(CurrentNode,Predecessors,Pred),
        get_edge_residual_capacity(ResidualCapacities,Pred,CurrentNode,
                                   ResCapacity),
        
        (
            ResCapacity < CurrentMinResCapacity
        ->
            NewMinResCapacity = ResCapacity
        ;
            NewMinResCapacity = CurrentMinResCapacity
        ),
        augment_aux(ResidualCapacities,SourceNode,Predecessors,Pred,
                    NewMinResCapacity, [e(Pred,CurrentNode,
                                          ResCapacity)|PartialPath],  
                    MinResCapacity,Path).        

        

%% About the residual graph / residual capacity data structure:

%% ResidualCapacities is an array (arg = source node S) where
%% each item is a hash table (hash key = dest node D of an adjacent edge
%% for S, hash value = residual capacity for edge (S,D)

%% When finding a path, we get the valid adjacent edges from this data, and
%% no additional residual graph representation is needed.


%% Why is it done in this way instead of using residual graph as a graph?
%% Because of all tested residual graph / residual capacity
%% data structures, this appeared to be clearly the fastest representation
%% for this algorithm, where residual capacities of edges are continuously
%% updated (maybe you have a more elegant way to do it in the source of
%% graph_algorithms?) 




initialize_residual_capacities(N,Edges,CapacityArg,
                               ResidualCapacities):- 
        dim(ResidualCapacities,[N]),
        (
            foreacharg(AdjEdges,ResidualCapacities)
        do
            hash_create(AdjEdges)
        ),

        (
            foreach(e(Src,Dst,Info),Edges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            arg(Src,ResidualCapacities,AdjEdges),
            (
		hash_get(AdjEdges,Dst,0)
            ->
                % arc added already as opposite edge
                hash_set(AdjEdges,Dst,Capacity)
            ;
                % arc not added yet
                hash_add(AdjEdges,Dst,Capacity),
                % and the opposite
		arg(Dst,ResidualCapacities,OppAdjEdges),
                hash_add(OppAdjEdges,Src,0)
            )
        ).



capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.

        
get_edge_residual_capacity(ResidualCapacities,Src,Dest,ResCapacity):-
	arg(Src,ResidualCapacities,AdjEdges),
	hash_get(AdjEdges,Dest,ResCapacity).

delete_edge_residual_capacity(ResidualCapacities,Src,Dest,ResCapacity):-
        % remove both forward and backward edges
        arg(Src,ResidualCapacities,AdjEdges),
	hash_remove(AdjEdges,Dest,ResCapacity),
        arg(Dest,ResidualCapacities,IncomingEdges),
        hash_remove(IncomingEdges,Src, _).

initialize_node_labels(NodeLabels):-
        (
            foreacharg(0,NodeLabels) 
		do
            true
        ).
        

initialize_predecessor_array(Predecessors):-
        (
            foreacharg(0,Predecessors)
        do
            true
        ).
        
        
label(NodeLabels,Node):-
        setarg(Node,NodeLabels,1).

labeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 1.
unlabeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 0.
        
        
update_residual_capacities(ResidualCapacities, Path, PathSize):-      
        (
            foreach(e(S,D,ResCapacity),Path),
            param(ResidualCapacities,PathSize)
        do
            NewResCapacity is ResCapacity - PathSize,
            arg(S,ResidualCapacities,AdjEdges),
            hash_set(AdjEdges,D,NewResCapacity),

            % and _add_ PathSize to opposite direction
	    arg(D,ResidualCapacities,OppAdjEdges),
	    hash_get(OppAdjEdges,S,OppResCapacity),

            NewOppResCapacity is OppResCapacity + PathSize,
            hash_set(OppAdjEdges,S,NewOppResCapacity)
        ).
  
  
get_valid_adjacent_nodes(ResidualCapacities,Src,Neighbours):-
        arg(Src,ResidualCapacities,AdjEdges),
        hash_list(AdjEdges,Dests,ResCapacities),
        (
            fromto([],In,Out,Neighbours),
            foreach(D,Dests),
            foreach(ResCapacity,ResCapacities)
        do
            (
                ResCapacity > 0
            ->
                Out = [D|In]
            ;
                Out = In
            )
        ).

        
get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities, MaxFlowEdges, 
                   MaxFlowEdgesGraph):-
        (
            foreach(e(Src,Dst,Info),Edges),
            fromto([],In,Out,MaxFlowEdges),
            fromto([],In1,Out1,MaxFlowJustEdges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            get_edge_residual_capacity(ResidualCapacities,Src,Dst,ResCapacity),
            (
                ResCapacity < Capacity
            ->
                Flow is Capacity - ResCapacity,
                Out = [Flow-e(Src,Dst,Info)|In],
                Out1 = [e(Src,Dst,Info)|In1]
            ;
                Out = In,
                Out1 = In1
            )
        ),
        make_graph(N,MaxFlowJustEdges,MaxFlowEdgesGraph).

/************************************************************************

Maximum Flow with lower bounds on edge capacity. 

Kish Shen 2008-12

The basic Ford-Fulkerson max-flow algorithm requires all the edges to have a
capacity -- the maximum flow through the edge, but not edges which can also
have a minumum required flow. This is because the algorithm requires a
feasible flow through the graph as a starting point, which is then 
incrementally improved until the maximum flow is reached. In order to
obtain the maximum flow through a graph with minimum flow on its edges, the
predicates here implements a two phase approach:

1) Find a feasible flow

2) Find the maximum flow starting with the feasible flow

For 1), the original graph is transformed so that solving the max-flow for
the transformed graph returns a feasible flow for the original graph. This 
is done by:

a) Map the flow requirements for each edge to a zero minimum flow by taking
off the minimum flow for the edge:

F' = F - Fmin  

b) Add a new source and sink to the transformed graph, which either
 supplies from the source, or removes to the sink, the excess flows into or
 out of a (original) node due to the minimum flows on its edges. That is, 

i)  SFin > SFout -> add a new edge from the node to the new sink
 T' with capacity of SFin - SFout
ii) SFin < SFout -> add a new edge from the new source
 S' to the node with capacity of SFout - SFin

where SFin is the sum of all the minimum required flows of all the incoming
edges, and SFout the sum of all the minimum required flows of all the
outgoing edges for the node.

c) add a edge from the original sink to the original source, with infinite
 capacity, so that a circulation in the original network.

Finding the max-flow transformed graph will give a feasible solution for
the original iff the flow through all the new edges from the new source and
the new sink are saturated. In this case, the flow through the graph
matching the original is a feasible flow, with flow values on each edge 
   F = F' + Fmin

This flow (with the residual graph, with the extra edges for the
transformed graph removed) can then be used in the basic Ford-Fulkerson
algorithm to find a max-flow for the original problem.

************************************************************************/


:- export max_flow_with_lb/8.
:-comment(max_flow_with_lb/8,
          [
              summary:"Finds rhe maximum flow for a network with non-"
                      "negative lower-bounds imposed on the edge flows,"
                     "using an adapted Ford-Fulkerson maximum flow algorithm",
              amode:max_flow_with_lb(+,+,+,+,+,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "LowerBoundArg":"which argument of EdgeData to use"
                                       " as the minimum flow (lower bound)"
                                       " for edge (integer) ",
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer),",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow",
                       "MaxFlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "MaxFlowEdgesGraph": "a graph structure, original"
                                            " nodes (as in Graph) but only"
                                            " the edges that are in max flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph, modified to allow"
                        " edges to have non-negative minimum flows. It"
                        " returns the maximal achievable flow allowed by the"
                        " capacities in the network, a list of all"
                        " edges with non-zero flow, and a graph of the"
                        " edges with non-zero flow."),
              fail_if: "There is no feasible flow between Source and Sink"
                       " nodes.",
              see_also:[max_flow:max_flow_with_lb/6,
                        max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow:feas_flow_with_lb/8,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5
                       ]
          ]
         ).

max_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, MaxFlowVal, MaxFlowEdges, MaxFlowEdgesGraph) :-
        do_max_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, N, ResCaps, MaxFlowVal),
        graph_get_all_edges(Graph,Edges),
        get_max_flow_edges(N, Edges, CapacityArg,ResCaps, MaxFlowEdges,MaxFlowEdgesGraph). 

:- export max_flow_with_lb/6.
:-comment(max_flow_with_lb/6,
          [
              summary:"Finds rhe maximum flow for a network with non-"
                      "negative lower-bounds imposed on the edge flows,"
                     "using an adapted Ford-Fulkerson maximum flow algorithm",
              amode:max_flow_with_lb(+,+,+,+,+,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "LowerBoundArg":"which argument of EdgeData to use"
                                       " as the minimum flow (lower bound)"
                                       " for edge (integer) ",
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer),",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph, modified to allow"
                        " edges to have non-negative minimum flows. It"
                        " returns the maximal achievable flow allowed by the"
                        " capacities in the network."),
              fail_if: "There is no feasible flow between Source and Sink"
                       " nodes.",
              see_also:[max_flow:max_flow_with_lb/8,
                        max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow:feas_flow_with_lb/8,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5
                       ]
          ]
         ).

max_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, MaxFlowVal) :-
        do_max_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, _N, _ResCaps, MaxFlowVal).

do_max_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, N, ResCaps, MaxFlowVal) :-
        transform_graph_for_feasibility_solve(Graph, LwBArg, CapacityArg, 
                                              Src, Sink, AddedEdge, TSrc, TSink, TGraph),
	do_max_flow(TGraph, CapacityArg, TSrc, TSink, _, _TN, _Edges, ResCaps),
        (AddedEdge == 1 -> 
            delete_edge_residual_capacity(ResCaps, Sink, Src, _)
        ;
            true
        ),
        graph_get_incoming_edges(TGraph, TSink, TSinkEdges),
        (foreach(e(S,_,_), TSinkEdges), param(TSink,ResCaps) do
            delete_edge_residual_capacity(ResCaps, S, TSink, ResCapacity),
            ResCapacity =:= 0
        ),
        graph_get_incoming_edges(TGraph, Sink, SinkEdges),
        (foreach(e(S,_,TData), SinkEdges),
         param(Sink,ResCaps,CapacityArg),
         fromto(0, F0,F1, FlowVal)
        do
            get_edge_residual_capacity(ResCaps, S, Sink, ResCapacity),
            arg(CapacityArg, TData, Capacity),
            ( ResCapacity < Capacity -> F1 is F0 + Capacity - ResCapacity 
            ;
                                        F1 = F0
            )
        ),
        /* deletion of TSrc links done after getting FlowVal, as there may
           be an edge (TSrc,Sink), supplying the lower-bound flows to Sink
        */
        graph_get_adjacent_edges(TGraph, TSrc, TSrcEdges),
        (foreach(e(_,D,_), TSrcEdges), param(TSrc,ResCaps) do
            delete_edge_residual_capacity(ResCaps, TSrc, D, ResCapacity),
            ResCapacity =:= 0
        ),
        graph_get_maxnode(Graph, N),
        max_flow_aux(N,ResCaps, Src, Sink, FlowVal, MaxFlowVal).


:- export feas_flow_with_lb/8.
:-comment(feas_flow_with_lb/8,
          [
              summary:"Finds a feasible flow for a network with non-"
                      "negative lower-bounds imposed on the edge flows,"
                     "using an adapted Ford-Fulkerson maximum flow algorithm",
              amode:feas_flow_with_lb(+,+,+,+,+,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData), EdgeData must be a"
                                " structure with at least two arguments"
                                " (for the lower and upper bounds of the"
                                " edge capacity", 
                       "LowerBoundArg":"which argument of EdgeData to use"
                                       " as the minimum flow (lower bound)"
                                       " for edge (integer) ",
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " (uppoer bound) edge capacity (integer),", 
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "FlowValue": "value of the  flow",
                       "FlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "FlowEdgesGraph": "a graph structure, original"
                                            " nodes (as in Graph) but only"
                                            " the edges that are in max flow"
                   ],
              desc:html("This predicate returns a feasible flow for a"
                        " network whose edges can have a (non-negative)"
                        " lower bound imposed on the edge flows. This is"
                        " done by transforming the network to one with zero"
                        " lower-bounds and solving for feasibility. Normally"
                        " this will serve as the starting point for obtaining" 
                        " the maximal flow for the original network, but this"
                        " predicate is provided for cases where a feasible"
                        " solution is sufficient. If there is a feasible"
                        " solution, it returns the total flow  value for"
                        " this solution, a list of all edges with  flow,"
                        " and a graph of the edges with non-zero flow. It "
                        " fails if there are no feasible flow."),
              fail_if: "There is no feasible flow from Source to Sink",
              see_also:[max_flow:max_flow_with_lb/8,
                        max_flow:max_flow/5,
                        max_flow:max_flow/7
                       ]
          ]
         ).

feas_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, FeasFlowVal, FlowEdges, FlowGraph) :-
	transform_graph_for_feasibility_solve(Graph, LwBArg, CapacityArg, 
                                              Src, Sink, AddedEdge, TSrc, TSink, TGraph),
	do_max_flow(TGraph, CapacityArg, TSrc, TSink, _, N, Edges, ResCaps),
        check_if_feas_and_extract_orig(Graph, N, Edges, ResCaps, TSrc, TSink, Src,
                                       Sink, LwBArg, CapacityArg, AddedEdge,
                                       FeasFlowVal, FlowEdges, FlowGraph).  % fail if infeasible
	
transform_graph_for_feasibility_solve(Graph, LwBArg, CapacityArg, Src, Sink, AddedEdge, TSrc, TSink, TGraph) :-
	graph_get_maxnode(Graph, N),
	TSrc is N + 1,
	TSink is N + 2,
        % get incoming edges now, as we will need them (first call computes
        % them. Also, we need the edge data to create new edge data for the
        % new edges in the transformed graph. Use Sink because it should
        % have incoming nodes!
        graph_get_incoming_edges(Graph, Sink, [e(_,_,Data0)|_]), % could fail
        functor(Data0,DataName,DataArity),
        (for(I,1,N), param(Graph,TSrc,TSink,LwBArg,CapacityArg,
                           Src,Sink,DataName,DataArity, AddedEdge),
	 fromto(NewEdges0, Es0,Es, []) do
            graph_get_incoming_edges(Graph, I, InEs),	
            (foreach(e(_,_,Data), InEs), 
             param(LwBArg),
             fromto(0, In0,In1, In) do
                In1 is In0 + arg(LwBArg, Data)
            ),
            graph_get_adjacent_edges(Graph, I, OutEs),
            (foreach(e(_,_,Data), OutEs),
             param(LwBArg), 
             fromto(0, Out0,Out1, Out) do
                Out1 is Out0 + arg(LwBArg, Data)
            ),
            (I == Sink -> 
                (graph_get_edge(Graph, Sink, Src, _) ->
                    % already have an edge from Sink to Source
                    AddedEdge = 0,
                    Es0 = Es1
                ;
                    functor(Data1, DataName, DataArity),
                    arg(CapacityArg, Data1, 1.0Inf),
                    arg(LwBArg, Data1, 0),
                    AddedEdge = 1,
                    Es0 = [e(Sink, Src, Data1)|Es1]
                )
            ;
                Es0 = Es1
            ),
            Bi is In - Out,
            ( Bi > 0 ->
                functor(NData,DataName,DataArity),
                Es1 = [e(TSrc,I,NData)|Es],
                arg(CapacityArg, NData, Bi),
                arg(LwBArg, NData, 0)
            ; Bi < 0 ->
                functor(NData,DataName, DataArity),
                Bi1 is -Bi,
                Es1 = [e(I,TSink, NData)|Es],
                arg(CapacityArg, NData,  Bi1),
                arg(LwBArg, NData, 0)
            ; 
                Es1 = Es
            )
	),
        (NewEdges0 = [_,_|_] ->
            % There should be at least two new edges if any edges are added 
            % for the new source and sink (at least one each, as the excess
            % flows must balance). If not, it means the lower bound flows is
            % balanced at all nodes. In this case, add 'dummy' edges to old
            % source and sink
            NewEdges = NewEdges0
        ;
            NewEdges = [e(TSrc,Src,DData),e(Sink,TSink,DData)|NewEdges0],
            functor(DData,DataName,DataArity),
            arg(LwBArg,DData,0),
            arg(CapacityArg,DData,0)
        ),
	graph_get_all_edges(Graph, Edges0),
	(foreach(E0, Edges0),
	 param(CapacityArg,LwBArg,DataName,DataArity),
	 fromto(TEdges, TE0,TE1, NewEdges) do
		E0 = e(S,D,Data),
		arg(LwBArg, Data, Low),
		(Low > 0 ->
			TCap is arg(CapacityArg, Data) - Low,
			functor(TData, DataName, DataArity),
			arg(LwBArg, TData, Low),
			arg(CapacityArg, TData, TCap),
			TE0  = [e(S,D,TData)|TE1]
		;
			TE0 = [E0|TE1]
		)
	),
			
	make_graph(TSink, TEdges, TGraph).

check_if_feas_and_extract_orig(Graph,TN, TEdges, ResCaps, TSource, TSink, Source,
                               Sink, LwBArg, CapacityArg, AddedEdge, FeasFlowVal,
                               FlowEdges, FlowGraph) :-	
	(foreach(Edge, TEdges),
         param(Graph,TSource,TSink,Source,Sink,CapacityArg,LwBArg,ResCaps,AddedEdge),
         fromto(FlowEdges, FE0,FE1, []),
         fromto(Edges, E0,E1, []), 
		 fromto(0, FV0,FV1, FeasFlowVal) do
            Edge = e(S,D,TData),
            get_edge_residual_capacity(ResCaps,S,D,ResCapacity),
            arg(CapacityArg, TData, Capacity),
            ( ResCapacity < Capacity -> TFlow is Capacity - ResCapacity ; TFlow = 0 ),
            ( S == TSource ->
            /* added transformed edge, check feasibility */
                Capacity =:= TFlow, 
                FE0 = FE1,
                E0 = E1,
                FV1 = FV0
            ; D == TSink ->
                Capacity =:= TFlow,
                FE0 = FE1,
                E0 = E1,
                FV1 = FV0
            ; S == Sink, D == Source ->
	    /* edge from Sink to Source needs special treatment */
                ( AddedEdge == 1 ->	
                    % edge added during transformation, has no lower bound
                    FE0 = FE1,			
                    E0 = E1,
                    FV1 = FV0
                ;
                    % edge is original, get actual flow on it
                    arg(LwBArg, TData, Low),
                    Flow is TFlow + Low,
                    (Flow > 0 ->
                        (Low > 0 ->
                            % need to get original edge
                            graph_get_edge(Graph, S, D, OrigEdge)
                        ;
                            % assume reuse of original edge if Low == 0 in transformation
                            OrigEdge = Edge
                        ),
                        FE0 = [Flow-OrigEdge|FE1],
                        E0 = [OrigEdge|E1],
                        FV1 = FV0
                    ;
                        FE0 = FE1,
                        E0 = E1, 
                        FV1 = FV0
                    )
                )
            ;
            /* flow in original problem, convert it to actual flow */
                arg(LwBArg, TData, Low),
                Flow is TFlow + Low,
                (Flow > 0 ->
                    (Low > 0 ->
                        % need to get original edge
                        graph_get_edge(Graph, S, D, OrigEdge)
                    ;
                        % assume reuse of original edge if Low == 0 in transformation
                        OrigEdge = Edge
                    ),
                    FE0 = [Flow-OrigEdge|FE1],
                    E0 = [OrigEdge|E1],
                    (D == Sink -> FV1 is FV0 + Flow ; FV1 = FV0)
                ;
                    FE0 = FE1,
                    E0 = E1,
                    FV1 = FV0
                )
            )
        ),
	N is TN - 2, % original without TSrc and TSink
        make_graph(N, Edges, FlowGraph).




%% the following are exported for module all_min_cuts


:-comment(initialize_residual_capacities/4,hidden).
:-export(initialize_residual_capacities/4).

:-comment(max_flow_aux/6,hidden).
:-export(max_flow_aux/6).

:-comment(get_max_flow_edges/6,hidden).
:-export(get_max_flow_edges/6).

:-comment(max_flow/8,hidden).
:-export(max_flow/8).



