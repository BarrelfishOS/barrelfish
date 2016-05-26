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
% The Original Code is The Integer Programming All Minimum-cost cuts Library
% The Initial Developer of the Original Code is  CrossCore Optimization Ltd.
% Portions created by the Initial Developer are  Copyright (C)2007.
% All Rights Reserved.
% 
% 
% END LICENSE BLOCK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(all_min_cuts_eplex).
:-comment(categories, ["Algorithms"]).
:-comment(summary,"Mixed integer programming solution for generating all minimum-cost cuts").
:-comment(desc,"Mixed integer programming solution for generating all"
               " minimum-cost cuts between given source and sink nodes."
               " This formulation was used as a comparison algorithm in"
               " the experimental section of [Norman D. Curet,"
               " Jason DeVinney, Matthew E. Gaston. An efficient"
               " network flow code for finding all minimum cost s-t"
               " cutsets. Computers & Operations Research 29 (2002)"
               " 205-219]. The idea is to iteratively solve dual max flow"
               " problem, and at each iteration, post an additional"
               " contraint to avoid" 
               " repeating the same cuts.").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(graph_algorithms).
:-lib(hash).
:-lib(eplex).


:- export(all_min_cuts_eplex/7).
:- comment(all_min_cuts_eplex/7,
          [
              summary:"MIP algorithm for generating all minimum-"
                      "cost cuts", 
              amode:all_min_cuts_eplex(+,+,+,+,-,-,-),
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

:- export(all_min_cuts_eplex/8).
:- comment(all_min_cuts_eplex/8,
          [
              summary:"MIP algorithm for generating all minimum-"
                      "cost cuts, with a limit for max allowed number of"
                      " generated cuts", 
              amode:all_min_cuts_eplex(+,+,+,+,+,-,-,-),
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
                                 " (integer), if Limit = 0 then output all"
                                 " possible" 
                                 " mincuts",
                       "MaxFlowValue": "value of the maximum flow"
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


all_min_cuts_eplex(Graph,CapacityArg,SourceNode,SinkNode,MaxFlowValue,
                    MinCuts,MinCutEdges):-  
        all_min_cuts_eplex(Graph,CapacityArg,SourceNode,SinkNode,0,
                           MaxFlowValue, MinCuts,MinCutEdges).

        
all_min_cuts_eplex(Graph,CapacityArg,SourceNode,SinkNode,Limit,MaxFlowValue,
                    MinCuts,MinCutEdges):-  
        
        eplex: eplex_solver_setup(min(ObjFn)),

        % setup the dual maxflow model
        dual_model(Graph,CapacityArg,SourceNode,SinkNode,NodeVars,
                   EdgeVars,ObjFn), 

        % get first solution
        eplex: eplex_solve(MaxFlowValue),
        output_cut(Graph,NodeVars,EdgeVars,MinCut1,MinCutEdgeSet1),
        
        % get more solutions if there are:
        (
            count(I,1,_),
            fromto([MinCut1],MinCutsIn,MinCutsOut,MinCuts),
            fromto([MinCutEdgeSet1],MinCutEdgesIn,MinCutEdgesOut,MinCutEdges),
            fromto(false,_,Stop,true),
            param(Graph,NodeVars,EdgeVars,MaxFlowValue,Limit)
        do
            (
                I == Limit
            ->
                % no more solutions allowed by the user
                MinCutsOut = MinCutsIn,
                MinCutEdgesOut = MinCutEdgesIn,
                Stop = true
            ;
                
                % take previous optimal solution
                MinCutEdgesIn = [PreviousCutEdges|_],
                
                % post new constraint
                post_cut_cardinality_constraint(PreviousCutEdges,EdgeVars),
                
                % solve again, still possible to get a min cut?
                
                (
                    eplex: eplex_solve(Value),
                    Value == MaxFlowValue
                ->
                    % collect the solution and try again
                    output_cut(Graph,NodeVars,EdgeVars,MinCut,MinCutEdgeSet),
                    MinCutsOut = [MinCut|MinCutsIn],
                    MinCutEdgesOut = [MinCutEdgeSet|MinCutEdgesIn],
                    Stop = false
                ;
                    % no optimal solutions left, stop
                    MinCutsOut = MinCutsIn,
                    MinCutEdgesOut = MinCutEdgesIn,
                    Stop = true
                )
            )
        ),
        eplex: eplex_cleanup.



        
dual_model(Graph,CapacityArg,SourceNode,SinkNode,NodeVars,
           EdgeVars,ObjFn):-
        
        graph_get_maxnode(Graph,N),
        dim(NodeVars,[N]),
        (
            foreacharg(Y,NodeVars)
        do
            eplex: (integers(Y)),
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
            eplex: (integers(Z)),
            eplex: (Z $>= 0),
            eplex: (Z $=< 1),
            hash_add(EdgeVars,key(I,J),Z),
            
            capacity(CapacityArg,Info,Capacity),
            
            arg(I,NodeVars,Y_i),
            arg(J,NodeVars,Y_j),
            eplex: (Y_i - Y_j + Z $>= 0)

        ),
        
        arg(SourceNode,NodeVars,Y_s),
        arg(SinkNode,NodeVars,Y_t),
        eplex: ( Y_s $= 0 ),
        eplex: ( Y_t $= 1 ),
             
        eplex: (ObjFn $= EdgeCapacityList * EdgeVarList).
        
                    
post_cut_cardinality_constraint(CutEdges,EdgeVars):-
        %% this constraint forces a different mincut
        
        (
            foreach(e(I,J,_),CutEdges),
            fromto([],In,[Var|In],CutEdgeVars),
            param(EdgeVars)
        do
            hash_get(EdgeVars,key(I,J),Var)
        ),
        
        % cardinality of previous optimal solution:
        
        length(CutEdges,Card),
        
        % next solution can contain no more than (Card-1) same edges:
        eplex: eplex_add_constraints([( sum(CutEdgeVars) $=< Card - 1)],[]).

        
output_cut(Graph,NodeVars,EdgeVars,MinCutNodes,MinCutEdges):-
        (
            count(I,1,_),
            foreacharg(Y,NodeVars),
            fromto([],In,Out,MinCutNodes)
        do
            eplex: eplex_var_get(Y,typed_solution,Sol),
            (
                Sol == 0
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
                Sol == 1
            -> 
                graph_get_edge(Graph,I,J,Edge),
                Out = [Edge|In]
            ;
                Out = In
            )
        ).


capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.

