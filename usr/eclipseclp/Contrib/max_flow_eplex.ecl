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
% The Original Code is The Linear Programming Maximum Flow Library
% The Initial Developer of the Original Code is  CrossCore Optimization Ltd.
% Portions created by the Initial Developer are  Copyright (C) 2006-2007.
% All Rights Reserved.
% 
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(max_flow_eplex).
:-comment(categories, ["Algorithms"]).
:-comment(summary,"Linear programming solution for maximum flow problem").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(eplex).
:-lib(hash).
:-lib(graph_algorithms).


:-export(max_flow_eplex/5).
:-comment(max_flow_eplex/5,
          [
              summary:"Linear programming solution for maximum flow problem",
              amode:max_flow_eplex(+,+,+,+,-),
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

:-export(max_flow_eplex_dual/5).
:-comment(max_flow_eplex_dual/5,
          [
              summary:"Linear programming solution for maximum flow problem,"
                      " dual linear program",
              amode:max_flow_eplex_dual(+,+,+,+,-),
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


:-export(max_flow_eplex_dual/7).
:-comment(max_flow_eplex_dual/7,
          [
              summary:"Linear programming solution for maximum flow problem,"
                      " dual linear program. Outputs the cut as nodes and"
                      " edges.",
              amode:max_flow_eplex_dual(+,+,+,+,-,-,-),
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
                       "MinCutNodes": "List of nodes that belong to the"
                                      " source side of the minimum cost cut",
                       "MinCutEdges": "List of edges of the minimum cost cut"
                       ],
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

max_flow_eplex(Graph,CapacityArg,SourceNode,SinkNode,MaxFlowValue):-
        eplex: eplex_solver_setup(max(X_ts)),

        % circular model, we add a dummy edge from sink t to source s with
        % infinite capacity
        
        % maximize the flow on this t-s edge
        
        define_vars(Graph,CapacityArg,SourceNode,SinkNode,EdgeVars,X_ts),
        
        flow_constraints(Graph,SourceNode,SinkNode,EdgeVars),
        eplex: eplex_solve(MaxFlowValue),
        eplex: eplex_cleanup.



define_vars(Graph,CapacityArg,SourceNode,SinkNode,EdgeVars,X_ts):-

        % define edge variables and their capacity bounds
        
        graph_get_all_edges(Graph,Edges),
        hash_create(EdgeVars),        
        (
            foreach(e(S,D,Info),Edges),
            param(EdgeVars,CapacityArg)
        do
            capacity(CapacityArg,Info,Capacity),
            eplex: (X $>= 0),
            eplex: (X $=< Capacity),
            hash_add(EdgeVars,key(S,D),X)
        ),
        
        % dummy t-s edge variable with infinite capacity bound
        eplex: (X_ts $>= 0),
        (
            hash_get(EdgeVars,key(SinkNode,SourceNode),_)
        ->
            %% there is already t-s edge, replace it
            hash_set(EdgeVars,key(SinkNode,SourceNode),X_ts)
        ;
            hash_add(EdgeVars,key(SinkNode,SourceNode),X_ts)
        ).
        
        
capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.

       
             
flow_constraints(Graph,SourceNode,SinkNode,EdgeVars):-
        graph_get_maxnode(Graph,N),
        (
            for(I,1,N),
            param(Graph,SourceNode,SinkNode,EdgeVars)
        do
            get_out_edges(Graph,SourceNode,SinkNode,I,OutEdges),
            (
                foreach(e(_,J,_),OutEdges),
                foreach(X_ij,X_ijs),
                param(I,EdgeVars)
            do
                hash_get(EdgeVars,key(I,J),X_ij)
            ),
            get_in_edges(Graph,SourceNode,SinkNode,I,InEdges),
            (
                foreach(e(J,_,_),InEdges),
                foreach(X_ji,X_jis),
                param(I,EdgeVars)
            do
                hash_get(EdgeVars,key(J,I),X_ji)
            ),
            eplex: (sum(X_ijs) $= sum(X_jis))
        ).

get_out_edges(Graph,SourceNode,SinkNode,I,OutEdges):-
        graph_get_adjacent_edges(Graph,I,OutEdges1),
        (
            I == SinkNode,
            not memberchk(e(SinkNode,SourceNode,_),OutEdges1)
        ->
            OutEdges = [e(SinkNode,SourceNode,_)|OutEdges1]
        ;
            OutEdges = OutEdges1
        ).
              
            
get_in_edges(Graph,SourceNode,SinkNode,I,InEdges):-
        graph_get_incoming_edges(Graph,I,InEdges1),
        (
            I == SourceNode,
            not memberchk(e(SinkNode,SourceNode,_),InEdges1)
        ->
            InEdges = [e(SinkNode,SourceNode,_)|InEdges1]
        ;
            InEdges = InEdges1
        ).
            


max_flow_eplex_dual(Graph,CapacityArg,SourceNode,SinkNode,MaxFlowValue):- 
        
        max_flow_eplex_dual_core(Graph,CapacityArg,SourceNode,SinkNode,
                                 MaxFlowValue, 
                                 _NodeVars,_EdgeVars),
        eplex: eplex_cleanup.
        
 
max_flow_eplex_dual(Graph,CapacityArg,SourceNode,SinkNode,MaxFlowValue,
                    MinCutNodes,MinCutEdges):-  
        
        max_flow_eplex_dual_core(Graph,CapacityArg,SourceNode,SinkNode,
                                 MaxFlowValue, 
                                 NodeVars,EdgeVars),
 
        output_cut(Graph,NodeVars,EdgeVars,MinCutNodes,MinCutEdges),
        eplex: eplex_cleanup.


max_flow_eplex_dual_core(Graph,CapacityArg,SourceNode,SinkNode,MaxFlowValue,
                    NodeVars,EdgeVars):- 
        eplex: eplex_solver_setup(min(ObjFn)),

        dual_model(Graph,CapacityArg,SourceNode,SinkNode,NodeVars,
                   EdgeVars,ObjFn), 
        eplex: eplex_solve(MaxFlowValue).

        
dual_model(Graph,CapacityArg,SourceNode,SinkNode,NodeVars,
           EdgeVars,ObjFn):-
        
        graph_get_maxnode(Graph,N),
        dim(NodeVars,[N]),
        (
            foreacharg(Y,NodeVars)
        do
            eplex: (Y $>= 0),
            eplex: (Y $=< 1)
        ),
        
        graph_get_all_edges(Graph,Edges),
        hash_create(EdgeVars),        
        (
            foreach(e(I,J,Info),Edges),
            foreach(Z,EdgeVarList),
            foreach(Capacity,EdgeCapacityList),
            param(EdgeVars,CapacityArg,NodeVars)
        do
            eplex: (Z $>= 0),
            eplex: (Z $=< 1),
            hash_add(EdgeVars,key(I,J),Z),
            
            capacity(CapacityArg,Info,Capacity),
            
            arg(I,NodeVars,Y_i),
            arg(J,NodeVars,Y_j),
            eplex: (Y_i - Y_j + Z $>= 0)

        ),
        
        arg(SourceNode,NodeVars,X_s),
        arg(SinkNode,NodeVars,X_t),
        eplex: ( X_s $= 0 ),
        eplex: ( X_t $= 1 ),
             
        eplex: (ObjFn $= EdgeCapacityList * EdgeVarList).
        
        
output_cut(Graph,NodeVars,EdgeVars,MinCutNodes,MinCutEdges):-
        (
            count(I,1,_),
            foreacharg(Y,NodeVars),
            fromto([],In,Out,MinCutNodes)
        do
            eplex: eplex_var_get(Y,typed_solution,Sol),
            (
                ( Sol == 0.0 ; Sol = -0.0 )
            -> 
                Out = [I|In]
            ;
                Out = In
            )
        ),

        hash_list(EdgeVars,EdgeKeys,EdgeVarsList),
        (
            foreach(key(I,J),EdgeKeys),
            foreach(Z,EdgeVarsList),
            fromto([],In,Out,MinCutEdges),
            param(Graph)
        do
            eplex: eplex_var_get(Z,typed_solution,Sol),
            (
                Sol == 1.0
            -> 
                graph_get_edge(Graph,I,J,Edge),
                Out = [Edge|In]
            ;
                Out = In
            )
        ).
