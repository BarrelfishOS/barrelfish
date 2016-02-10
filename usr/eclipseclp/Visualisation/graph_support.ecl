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
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Andrew Sadler, IC-Parc
% 
% END LICENSE BLOCK

% Contains predicates for processing Graph and graph(fixed) type
% viewable structures

flatten_graph_property_list(_Graph, [], []).
flatten_graph_property_list(Graph,
                            [PropertyTerm|PropertyTail],
                            FlatPropertyList):-
        PropertyTerm =.. [EdgeOrNode,MarkupList],
        (foreach(Markup, MarkupList),
         foreach(FlatProperty, FlatPropertyTermList),
         param(EdgeOrNode) do
             FlatProperty =.. [EdgeOrNode,Markup]
        ),
        flatten_graph_property_list(Graph,
                                    PropertyTail,
                                    FlatPropertyListTail),
        listut:append(FlatPropertyTermList, FlatPropertyListTail, FlatPropertyList),
        true.
        

graph_to_vis_graph(Graph, NodeInfoList, EdgeInfoList, VisGraph):-
        (foreach(Name,NodeInfoList),
         fromto(1,Index,OutIndex,_),
         foreach(Node,NodeList) do
             Node = [-1,Index,Name],
             OutIndex is Index +1
        ),
        graph_get_all_edges(Graph,Edges),
        (foreach(Info,EdgeInfoList),
         foreach(e(From, To, _),Edges),
         foreach(VisEdge,VisEdgeList) do
             VisEdge = [From, To, Info]
        ),
        listut:append(NodeList, VisEdgeList, VisGraph),
        true.

% Ignore the ArgPos, simply return the node name, or a unique number
% for each node
graph_get_node_info_list(Graph, NodeNameList, _ArgPos):-
        graph_get_nodenames(Graph,NodeNameArray),
        !,
        dim(NodeNameArray,[NumNodes]),
        NodeNameList is NodeNameArray[1..NumNodes].        
graph_get_node_info_list(Graph, NodeNameList, _ArgPos):-
        graph_get_maxnode(Graph, MaxNode),
        (foreach(S,NodeNameList),
         for(I,1,MaxNode) do
             number_string(I,S)
        ).


graph_get_edge_info_list(Graph, EdgeInfoList, ArgPos):-
        graph_get_all_edges(Graph,Edges),
        (foreach(e(_S,_T,Info),Edges),
         foreach(EdgeInfo,EdgeInfoList),
         param(ArgPos) do
             (number(ArgPos),ArgPos =< 0 ->
                  EdgeInfo = Info
             ;ArgPos=[_|_] ->
                  % Handle nested structure definitions
                  (foreach(Pos,ArgPos),
                   fromto(Info,In,Out,EdgeInfo) do
                       arg(Pos,In,Out)
                  )
             ;
                  arg(ArgPos,Info,EdgeInfo)
             )
        ).
        
