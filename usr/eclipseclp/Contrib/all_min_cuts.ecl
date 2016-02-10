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
% The Original Code is The All Minimum-cost cuts Library
% The Initial Developer of the Original Code is  CrossCore Optimization Ltd.
% Portions created by the Initial Developer are  Copyright (C)2007.
% All Rights Reserved.
% 
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(all_min_cuts).
:-comment(categories, ["Algorithms"]).
:-comment(summary,"Curet et al, algorithm for generating all minimum-cost cuts").
:-comment(desc,"Algorithm for generating all minimum-cost cuts"
               " between source and sink. From Norman D. Curet,"
               " Jason DeVinney, Matthew E. Gaston. An efficient"
               " network flow code for finding all minimum cost s-t"
               " cutsets. Computers & Operations Research 29 (2002)"
               " 205-219. ").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(max_flow).

:-lib(graph_algorithms).
:-lib(hash).



        
:- export(all_min_cuts/8).
:-comment(all_min_cuts/8,
          [
              summary:"Curet et al, algorithm for generating all minimum-"
                      "cost cuts", 
              amode:all_min_cuts(+,+,+,+,-,-,-,-),
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
                       "MinCuts": "list of all minimum cost cutsets (each"
                                  " cutset is represented by a list of"
                                  " nodes belonging to the source-side of"
                                  " the cut)",
                       "MinCutEdges": "list of all minimum cost cutsets"
                                      " (each cutset is represented by a"
                                      " list of edges that separate the"
                                      " source-side and the sink-side of"
                                      " the cut)"
                   ],
              desc:html("This predicate provides all minimal cost cutsets"
                        " for a max flow problem in a graph between a"
                        " source and a sink node. It returns the"
                        " maximal flow value, edges with non-zero flow"
                        " in the optimal solution, and a list of all"
                        " minimum cost cutsets corresponding to that"
                        " flow value. The results are given both as"
                        " node lists (nodes on the source side of the"
                        " cut), and as edge lists (edges belonging to"
                        " the cut), to simplify use of the predicate"
                        " in situations where either of the output"
                        " formats is preferred."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
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

:- export(all_min_cuts/9).
:-comment(all_min_cuts/9,
          [
              summary:"Curet et al, algorithm for generating all minimum-"
                      "cost cuts, with limit for max number of allowed cuts", 
              amode:all_min_cuts(+,+,+,+,+,-,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "Limit" : "max number of min cuts to output"
                                 " (integer), if 0 then output all possible"
                                 " mincuts",
                       "MaxFlowValue": "value of the maximum flow",
                       "MaxFlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "MinCuts": "list of max Limit minimum cost cutsets"
                                  " (each" 
                                  " cutset is represented by a list of"
                                  " nodes belonging to the source-side of"
                                  " the cut)",
                       "MinCutEdges": "list of all minimum cost cutsets"
                                      " (each cutset is represented by a"
                                      " list of edges that separate the"
                                      " source-side and the sink-side of"
                                      " the cut)"
                   ],
              desc:("This predicate uses the saem interface, and"
                    " provides the same information as all_min_cuts/8,"
                    " but adds an argument to limit the number of cuts"
                    " that are generated. This can be helpful in"
                    " graphs with a structure that creates very large"
                    " numbers of minimal cost cutsets, the Limit"
                    " argument is used to restrict the number of"
                    " solutions returned."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
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

:- export(all_min_cuts_list/5).
:-comment(all_min_cuts_list/5,
          [
              summary:"Curet et al, algorithm for generating all minimum-"
                      "cost cuts, to be used for multiple source-sink pairs"
                      " on the same graph since unnecessary repeat of"
                      " initializations" 
                      " can be avoided", 
              amode:all_min_cuts_list(+,+,+,+,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceSinkList": "list source and sink nodes"
                                         " (integers), form SourceNode-"
                                         "SinkNode" ,
                       "Limit" : "max number of min cuts to output"
                                 " (integer), if 0 then output all possible"
                                 " mincuts",
                       "MinCutEdgesList": "list where for each Source-Sink"
                                          " pair there is list of all"
                                          " minimum cost cutsets, each"
                                          " cutset is represented by a list"
                                          " of edges that separate the" 
                                          " source-side and the sink-side of" 
                                          " the cut)" 
                   ],
              desc:html("This variant of the cutset generator should"
                        " be used when one is interested in generating"
                        " cutsets for multiple source-sink pairs in"
                        " the same graph, as it avoids multiple"
                        " initialisation of the data structures leading"
                        " to an overall improved performance."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
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


%% all_min_cuts with no limit for the number of min cuts
all_min_cuts(Graph, CapacityArg, SourceNode,
             SinkNode, MaxFlowValue,MaxFlowEdges,
             MinCuts,MinCutEdges):-

        all_min_cuts(Graph, CapacityArg, SourceNode,
                     SinkNode, 0, MaxFlowValue,MaxFlowEdges,
                     MinCuts,MinCutEdges).
        

all_min_cuts(Graph, CapacityArg, SourceNode,
             SinkNode, Limit, MaxFlowValue,MaxFlowEdges,
             MinCuts,MinCutEdges):-

        %% unlike in Curet et al paper, we don't prune extraneous nodes here
        
        max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
                 MaxFlowEdges, MaxFlowEdgesGraph, ResidualCapacities),

        (
            MaxFlowValue > 0
        ->
            graph_get_maxnode(Graph,N),
            process_residual_graph_arcs(N,ResidualCapacities,ResidualGraph),

            order_nodes(N,ResidualGraph,SinkNode,List),
            
            % enumerate strongly connected components
            
            enumerate_components(N,ResidualGraph,List,Components,ComponentMap),

            % output minimal cutsets
            
            output_mincuts(Limit,N,ResidualGraph,Components, ComponentMap,
                           SourceNode, SinkNode, MinCuts, MinCutArrays),

            % get edges that separate the side of the cut
            output_mincut_edges(MinCuts,MinCutArrays,MaxFlowEdgesGraph,
                                MinCutEdges)
        ;
            MinCuts = [],
            MinCutEdges = []
        ).


%% all_min_cuts_list/5 is for multiple source-sink pairs on the same graph

%% it's much faster since we can initialize the max flow alg just once

all_min_cuts_list(Graph, CapacityArg, SourceSinkList, Limit, MinCutEdgesList):-
        graph_get_maxnode(Graph,N),
        graph_get_all_edges(Graph,Edges),
        initialize_residual_capacities(N,Edges,CapacityArg,
                                       ResidualCapacities), 
        setval(initial_residual_capacities,ResidualCapacities),
        
        (
            foreach(SourceNode-SinkNode,SourceSinkList),
            foreach(MinCutEdges,MinCutEdgesList),
            param(N,Edges,CapacityArg,Limit)
        do
            getval(initial_residual_capacities,ResidualCapacities),
            max_flow_aux(N,ResidualCapacities,SourceNode,
                         SinkNode, 0, MaxFlowValue), 
            
            (
                MaxFlowValue > 0
            ->
                get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities,
                                   _MaxFlowEdges,MaxFlowEdgesGraph),
                
                process_residual_graph_arcs(N,ResidualCapacities,
                                            ResidualGraph), 
                
                order_nodes(N,ResidualGraph,SinkNode,List),
                
                % enumerate strongly connected components
                
                enumerate_components(N,ResidualGraph,List,Components,
                                     ComponentMap), 
                
                % output minimal cutsets
                
                output_mincuts(Limit,N,ResidualGraph,Components, ComponentMap,
                               SourceNode, SinkNode, MinCuts, MinCutArrays),

                % get edges that separate the side of the cut
                output_mincut_edges(MinCuts,MinCutArrays,MaxFlowEdgesGraph,
                                    MinCutEdges)
            ;
                MinCuts = [],
                MinCutEdges = []
            )
        ).


%% make a residual graph from ResidualCapacities structure from edges that
%% have non-zero residual capacity left
process_residual_graph_arcs(N,ResidualCapacities,ResidualGraph):-
        (
            fromto([],In,Out,Edges),
            count(Src,1,_),
            foreacharg(AdjEdges,ResidualCapacities)
        do
            hash_list(AdjEdges,Dests,ResCapacities),
            (
                foreach(Dest,Dests),
                foreach(ResCapacity,ResCapacities),
                fromto(In,In1,Out1,Out),
                param(Src)
            do
                (
                    ResCapacity = 0
                ->
                    Out1 = In1
                ;
                    Out1 = [e(Src,Dest,ResCapacity)|In1]
                )
            )
        ),
        make_graph(N,Edges,ResidualGraph).



capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.


initialize_node_labels(N,NodeLabels):-
        dim(NodeLabels,[N]),
        (
            foreacharg(0,NodeLabels)
        do
            true
        ).

label(NodeLabels,Node):-
        setarg(Node,NodeLabels,1).

unlabeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 0.
        
%% order_nodes, see Curet et al paper, Figure 5
order_nodes(N,ResidualGraph,SinkNode,List):-
        initialize_node_labels(N,NodeLabels),
        label(NodeLabels,SinkNode),
        dfs(ResidualGraph,NodeLabels,SinkNode,[],List).

dfs(ResidualGraph,NodeLabels,I,In,Out):-
	graph_get_adjacent_edges(ResidualGraph,I,Edges),
	(
	  Edges = []
        ->
	  Out = [I|In]
	;
          ( foreach(e(_,J,_),Edges),
	    fromto(In,In1,Out1,Out2),
   	    param(ResidualGraph,NodeLabels)
	  do
         
            (
	       unlabeled(NodeLabels,J)
	    ->
               label(NodeLabels,J),
	       dfs(ResidualGraph,NodeLabels,J,In1,Out1)
	    ;
               Out1 = In1
	    )
	  ),
	  Out = [I|Out2]
	).  


%% enumerate_components, see Curet et al paper, Figure 6

%% Components is an array, arg = Node Id and value = the actual strongly
%% connected component where Node belongs to

%% ComponentMapList contains the strongly connected components (lists of nodes)

enumerate_components(N,ResidualGraph,List,Components,ComponentMapList):-
        dim(Components,[N]),
        ( foreacharg(0,Components) do true),        
        ( fromto(List,ListIn,ListOut,[]),
          count(Counter,1,_),
          fromto(ComponentMapList,[Mapping|Map],Map,[]),
          param(Components,ResidualGraph,N)
        do
            % remove first P with component(P)=0 from ListIn
            remove_p(ListIn,Components,P,ListTemp),
            
            setarg(P,Components,Counter),
		
            dim(Remove,[N]),
            ( fromto(Mapping,Mapping4,Mapping1,[P]),
              fromto([P],List2,List2Out,[]),
              param(Remove,Components,Counter,ResidualGraph)
            do
                List2 = [I|List2Temp],
		graph_get_incoming_edges(ResidualGraph,I,Edges),
		( foreach(e(J,_,_),Edges),
	 	  fromto([],In,Out,Js),
		  param(Components)
		do
		    (
		       arg(J,Components,0)
		    ->
		       Out = [J|In]
		    ;
		       Out = In
		    )		    
		),		
		
                ( foreach(J,Js),
                  fromto(List2Temp,List2TempIn,[J|List2TempIn],List2Out),
                  fromto(Mapping4,Mapping3,Mapping2,Mapping1),
                  param(Components,Counter,Remove)
                do
		    arg(J,Remove,1),
                    setarg(J,Components,Counter),
                    Mapping3 = [J|Mapping2]
                )
            ),

	    (
		foreach(I,ListTemp),
		fromto(ListOut,Out,In,[]),
		param(Remove)
	    do
		arg(I,Remove,X),
		(
		   nonvar(X)
	        ->
		   Out = In
		;
		   Out = [I|In]
		)
	    )
        ).
            

remove_p(ListIn,Components,P,ListOut):-
        (
            fromto(ListIn,[H|EndOut],EndOut,End),
            fromto([],StartIn,[H|StartIn],[P|StartRev]),
            fromto(false,_,Stop,true),
            param(Components)
        do
            (
                arg(H,Components,0)
            ->
                    Stop = true
            ;
                    Stop = false
            )
        ),
        reverse(StartRev,Start),
        append(Start,End,ListOut).



%% output_mincuts, see Curet et al paper, Figure 7

%% MinCuts is a list containing the source-sides (lists of nodes) of each
%% min cut

%% MinCutArrays is for boosting output_mincut_edges. It's an array of all
%% nodes, value = 1 if the node (arg) belongs to the source-side of the cut

output_mincuts(Limit,N,ResidualGraph, Components, ComponentMapList, SourceNode,
               SinkNode, MinCuts, MinCutArrays):-

        arg(SinkNode,Components,I_min),
        arg(SourceNode,Components,I_max),
        
        % total number of components Q
        length(ComponentMapList,Q),
        ComponentMap =.. [c|ComponentMapList],
        % writeln(q:Q),        
        
        
        (
            fromto(I_max,I,I1,I_min),
            count(MinCutCount,1,_),
            fromto([],MinCutsIn,MinCutsOut,MinCuts),
            fromto([],MinCutArraysIn,MinCutArraysOut,MinCutArrays),
            fromto([],CurrentSetIn,CurrentSetOut,_),
            param(ResidualGraph,Components,ComponentMap,Q,I_min,I_max,N,Limit)
        do
            CurrentSetTemp1 = [I|CurrentSetIn],
            Ip is I + 1,
            (
                for(K,Ip,Q),
                fromto(CurrentSetTemp1,CurrentSetTemp2,CurrentSetTemp3,
                       CurrentSetOut),
                param(ResidualGraph,Components,ComponentMap)
            do
                (
                    once(delete(K,CurrentSetTemp2,CurrentSetTemp3)),
                    check_k(ResidualGraph,K,Components,ComponentMap,CurrentSetTemp2)
                ->
                    true
                ;
                    CurrentSetTemp2 = CurrentSetTemp3
                )
            ),

            dim(MinCutArray,[N]),
            (
                foreach(C,CurrentSetOut),
                fromto([],In,[Mapping|In],MinCutNestedList),
                param(ComponentMap,MinCutArray)
            do
                arg(C,ComponentMap,Mapping),
                (
                    foreach(Node,Mapping),
                    param(MinCutArray)
                do
                    arg(Node,MinCutArray,1)
                )
            ),
            flatten(MinCutNestedList,MinCut),
            append(MinCutsIn,[MinCut],MinCutsOut),
            append(MinCutArraysIn,[MinCutArray],MinCutArraysOut),
            (
                fromto(I_max,In1,Out1,_),
                fromto(I_min,In2,Out2,I1temp),
                fromto(false,_,Stop,true),
                param(CurrentSetOut,I_min)
            do
                Out1 is In1 - 1,
                (
                    % writeln(Out1:CurrentSetOut),
                    not memberchk(Out1,CurrentSetOut)
                ->
                    Out2 = Out1,
                    Stop = true
                ;
                    Out2 = In2,
                    (
                        Out1 = I_min
                    ->
                        Stop = true
                    ;
                        Stop = false
                    )
                )
            ),
            
            % check whether to stop because of Limit (max allowed number of
            % mincuts) 
            
            (
                MinCutCount = Limit
            ->
                I1 = I_min % stopping condition
            ;
                I1 = I1temp
            )
        ).


check_k(ResidualGraph,K,Components,ComponentMap,CurrentSet):-
        arg(K,ComponentMap,Js),
        (
            foreach(J,Js),
            param(CurrentSet,Components,ResidualGraph,K)
        do
	    graph_get_incoming_edges(ResidualGraph,J,Edges),
            (
                foreach(e(L,_,_),Edges),
                param(CurrentSet,Components,K)
            do
                arg(L,Components,C),
                (
                    C == K
                ;
                    not memberchk(C,CurrentSet)
                )
            )
        ).
            

  
output_mincut_edges(MinCuts,MinCutArrays,MaxFlowEdgesGraph,MinCutEdges):-
        (
            foreach(NodeList,MinCuts),
            foreach(MinCutArray,MinCutArrays),
            foreach(EdgeList,MinCutEdges),
            param(MaxFlowEdgesGraph)
        do
            (
                fromto([],In,Out,EdgeList),
                foreach(Node,NodeList),
                param(MaxFlowEdgesGraph,MinCutArray)
            do
                graph_get_adjacent_edges(MaxFlowEdgesGraph,Node,Edges), 
                (
                    foreach(e(Node,D,Info),Edges),
                    fromto(In,In1,Out1,Out),
                    param(MinCutArray)
                do
                    arg(D,MinCutArray,Member),
                    (
                        nonvar(Member)
                    ->
                        Out1 = In1
                    ;
                        Out1 = [e(Node,D,Info)|In1]
                    )
                )
            )
        ).


                 