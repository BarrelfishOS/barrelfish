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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: graph_algorithms.ecl,v 1.3 2010/07/25 13:29:05 jschimpf Exp $
% Contents:	Collection of graph algorithms
% ----------------------------------------------------------------------

:- module(graph_algorithms).

:- lib(hash).
:- lib(queues).
:- lib(ordset).
:- lib(heap_array).


:- comment(categories, ["Algorithms","Data Structures"]).
:- comment(summary, "Collection of graph algorithms").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2010/07/25 13:29:05 $").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(desc, html("<P>
    This library is a collection of graph algorithms.
</P><P>
    In its simplest form, a graph consists of a (possibly empty) set of
    nodes (numbered from 1 to NNodes), and zero or more directed edges
    between these nodes. An edge is represented by the data structure
<PRE>
	e(Source, Target, EdgeData)
</PRE>
    where Source and Target are integers indicating the start and end
    point of the edge, and EdgeData is an arbitrary data structure
    holding additional information about the edge, e.g. capacity,
    distance, weight, name etc.  The EdgeData field should have the
    same structure for all edges in a graph.  If there is no
    information attached to edges, the field should be set to 1
    for edges between different nodes and to 0 otherwise.
    Several library predicates inspect the EdgeData field or an argument
    of the EdgeData field, e.g. the shortest path predicate can
    use any numeric component of EdgeData as the distance criterion.
</P><P>
    The most efficient way to create a graph is to use make_graph/3
    which takes the number of nodes and a list of edges and constructs
    the graph data structure.  For example, the 13-node graph from
    Sedgewick, figure 32.1 can be created as follows:
<PRE>
    make_graph( 13,
	[ e(1,6,1),e(1,2,1),e(1,7,1),e(3,1,1),e(4,6,1),e(5,4,1),
	  e(6,5,1),e(7,5,1),e(7,10,1),e(7,3,1),e(8,7,1),e(8,9,1),e(9,8,1),
	  e(10,11,1),e(10,12,1),e(10,13,1),e(12,7,1),e(12,13,1),e(13,12,1) ],
        Graph).
</PRE>
    Often, the nodes have names or other information attached.
    This can be added to the Graph using graph_set_nodenames/2 which
    takes an existing graph and adds an array of node information
    (usually the node names, but any ground term can be used).
    For the Sedgewick-graph above, we could invoke
<PRE>
    graph_set_nodenames(Graph, [](a,b,c,d,e,f,g,h,i,j,k,l,m))
</PRE>
    If nodes have names anyway, it can be more convenient to specify
    the edges in terms of these node names rather than node numbers.
    Such symbolic edges are written in the form
<PRE>
	edge(SourceName, TargetName, EdgeData)
</PRE>
    where SourceName and TargetName should match an entry (usually the
    node name) in the graph's nodename-array.  A graph can now be
    constructed by giving a nodename-array and a list of symbolic edges
    to make_graph_symbolic/3, e.g. to build the same graph as above, use
<PRE>
    make_graph_symbolic(
	[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	[ edge(a,f,1),edge(a,b,1),edge(a,g,1),edge(c,a,1),edge(d,f,1),edge(e,d,1),
	  edge(f,e,1),edge(g,e,1),edge(g,j,1),edge(g,c,1),edge(h,g,1),edge(h,i,1),edge(i,h,1),
	  edge(j,k,1),edge(j,l,1),edge(j,m,1),edge(l,g,1),edge(l,m,1),edge(m,l,1) ],
	Graph).
</PRE>
    Note the use of the functor e/3 for the internal edge representation
    and edge/3 for the symbolic edge representation.
</P><P>
    There is no special data structure for undirected graphs - they can
    be represented by having reverse edges corresponding to every edge.
    It is allowed to have parallel edges (more than one edge from S to T)
    as long as their EdgeData fields differ.
<H4>Visualization</H4>
    You can use library(graphviz) or lib(viewable) to draw these graphs.
<H4>Overview of Shortest-Path Functionality</H4>
    This library provides a number of different variants of shortest path
    algorithms, of which the following table gives an overview:
<TABLE BORDER=\"1\">
<TR>
<TH>Predicate</TH>
    <TH>Sinks</TH>
    <TH>Paths</TH>
    <TH>Determinism</TH>
    <TH>Edge Weights</TH>
    <TH>Tolerance</TH>
</TR>
<TR>
<TD>shortest_paths/4</TD>
    <TH>all</TH>
    <TH>one</TH>
    <TH>det</TH>
    <TH>non-negative</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>shortest_paths_bellman_ford/4</TD>
    <TH>all</TH>
    <TH>one</TH>
    <TH>det</TH>
    <TH>arbitrary</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>all_short_paths_as_graph/6</TD>
    <TH>all</TH>
    <TH>all</TH>
    <TH>det</TH>
    <TH>non-negative</TH>
    <TH>yes</TH>
</TR>
<TR>
<TD>all_short_paths_as_edges/6 + possible_path/7</TD>
    <TH>all</TH>
    <TH>all</TH>
    <TH>nondet</TH>
    <TH>non-negative</TH>
    <TH>yes</TH>
</TR>
<TR>
<TD>incremental_all_shortest_paths_as_graph/6</TD>
    <TH>all</TH>
    <TH>all</TH>
    <TH>det</TH>
    <TH>positive</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>incremental_all_shortest_paths_as_edges/6 + possible_path/7</TD>
    <TH>all</TH>
    <TH>all</TH>
    <TH>nondet</TH>
    <TH>positive</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>single_pair_shortest_path/5</TD>
    <TH>single</TH>
    <TH>one</TH>
    <TH>semidet</TH>
    <TH>non-negative</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>single_pair_shortest_path_bellman_ford/5</TD>
    <TH>single</TH>
    <TH>one</TH>
    <TH>semidet</TH>
    <TH>arbitrary</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>single_pair_all_short_paths_as_graph/7</TD>
    <TH>single</TH>
    <TH>all</TH>
    <TH>det</TH>
    <TH>non-negative</TH>
    <TH>yes</TH>
</TR>
<TR>
<TD>single_pair_short_path/6</TD>
    <TH>single</TH>
    <TH>all</TH>
    <TH>nondet</TH>
    <TH>non-negative</TH>
    <TH>yes</TH>
</TR>
<TR>
<TD>incremental_single_pair_all_shortest_paths_as_graph/7</TD>
    <TH>single</TH>
    <TH>all</TH>
    <TH>det</TH>
    <TH>positive</TH>
    <TH>no</TH>
</TR>
<TR>
<TD>incremental_single_pair_shortest_path/6</TD>
    <TH>single</TH>
    <TH>all</TH>
    <TH>nondet</TH>
    <TH>positive</TH>
    <TH>no</TH>
</TR>
</TABLE>

<H4>To-do list</H4>
    The following operations should be added:
<UL>
    <LI>Graph modification: adding, removing, projecting
    <LI>Matching
    <LI>Flow
    ...
</UL>
</P>")).

%
% TODO
%
% add_nodes(+G1, +NNewNodes, -G)
% add_nodes_symbolic(+G1, +NNewNodes, -G)
% add_edges(+G1, +NewEdges, -G)
% add_edges_symbolic(+G1, +NewEdges, -G)
% del_nodes(+G1, +Nodes, -G)
% del_edges(+G1, +Edges, -G)
% compose_graphs(+G1, +G2, -G)
%	where like-numbered nodes are considered the same
% compose_graphs_symbolic(+G1, +G2, -G)
%	where like-named nodes are considered the same
%
% strong_components(+G, -NumberOfSCCs, -ComponentNumbers[NNodes], -ReducedGraph)
%
% NB
%
% incremental_all_shortest_paths_as_edges/6 assumes that we are
% dealing with STATIC graphs: i.e. the node array and incoming and
% adjacency lists for each node are unchanged since the last
% calculation and that ONLY edge data args (in particular weights)
% have been changed. If any predicate alters the graph other than this
% it MUST unset the incrementals, adj_id and in_id fields of the graph
% structure. As this library is supposed to implement static graphs
% no predicates make such changes.

:- local struct(graph(

    % required fields:
	size,		% number of nodes
	adj,		% array[size] of adjacency lists (out-edges)

    % optional fields (may be uninstantiated):
	in,		% array[size] of in-edge lists
	nodes,		% array[size] of node names
	name_node,	% hash table node_info -> node_id
        incrementals,   % array[size] of incremental structures
        adj_id,         % array[size] of adjacency lists (id_e structs
                        % for out-edges)
	in_id		% array[size] of (id_e structs for in-edges)
                        % lists
    )).

:- local struct(incremental(
        node_vals,      % array[size] of node info
        node_heap       % heap of current inconsistent nodes
    )).

:- local struct(node_val(
        value,          % current path-length estimate
        max_edge,       % number of incoming edges
        edge_heap,      % heap of under-consistent incoming edge_val
                        % structures, i.e. those that are in shorter
                        % than the current value of the node
        edge_set        % int: set of consistent incoming edge_val
                        % structures, i.e. those in shortest paths to
                        % node
    )).

:- local struct(id_e(
        id,             % the id for use in To node_val heaps and sets 
        edge            % the graph edge
    )).

% lib(queues) version is buggy before 5.0.1!
:- local list_join_queue/3.
list_join_queue(List, Front-OldBack, Front-NewBack) :-
	append(List, NewBack, OldBack).


%----------------------------------------------------------------------
% Build Graphs
%----------------------------------------------------------------------

:- export make_graph/3.
:- comment(make_graph/3, [
    summary:"Creates a graph with minimal overhead",
    amode:(make_graph(+,++,-) is det),
    args:["NNodes":"the number of nodes: integer >= 0",
    	"EdgeList":"a (possibly empty) list of e/3 structures in no particular order",
	"Graph":"will be bound to a graph structure" ],
    fail_if:"Edges contain nonexisting node numbers",
    exceptions:[6:"NNodes is negative", 4:"NNodes or EdgeList are uninstantiated", 5:"NNodes is not integral"],
    see_also:[make_graph_symbolic/3,graph_set_nodenames/2,library(graphviz),compare/3],
    eg:"
    % the 13-node undirected graph from Sedgewick:Algorithms
    make_graph( 13,
	[ e(1,6,1),e(1,2,1),e(1,7,1),e(3,1,1),e(4,6,1),e(5,4,1),
	  e(6,5,1),e(7,5,1),e(7,10,1),e(7,3,1),e(8,7,1),e(8,9,1),e(9,8,1),
	  e(10,11,1),e(10,12,1),e(10,13,1),e(12,7,1),e(12,13,1),e(13,12,1) ],
        Graph).",
    desc:html("<P>
    This predicate creates a graph data structure according to the given
    information.  A graph consists of nodes that are numbered from 1 to
    NNodes and directed edges between the nodes. An edge is represented
    by the data structure
<PRE>
	e(Source, Target, EdgeData)
</PRE>
    where Source and Target are integers indicating the start and end
    point of the edge, and EdgeData is an arbitrary data structure
    holding additional information about the edge, e.g. capacity,
    distance, weight, name etc.  The EdgeData field should have the
    same structure for all edges in the graph.  If there is no
    information attached to edges, the field should be set to 1
    for edges between different nodes and to 0 otherwise.
    Several library predicates inspect the EdgeData field or an argument
    of the EdgeData field, e.g. the shortest path predicate can
    use any numeric component of EdgeData as the distance criterion.
    Caution: the distance arguments will be compared using general term
    comparison (see compare/3) and should therefore have the same
    type in all edges (e.g. all integer or all float).
    </P>")]).

make_graph(NNodes, EdgeList, graph with [size:NNodes,adj:AdjArray]) :-
	integer(NNodes), NNodes >= 0,
	distribute_edges(NNodes, EdgeList, AdjArray),
	!.
make_graph(NNodes, EdgeList, Graph) :-
	( nonvar(NNodes), nonvar(EdgeList) ->
	    ( integer(NNodes), NNodes < 0 -> E = 6
	    ; EdgeList = [_|_] -> E = 6
	    ; E = 5 )
	;
	    E = 4
	),
	error(E, make_graph(NNodes, EdgeList, Graph)).


    distribute_edges(0, EdgeList, AdjArray) :- !,
	EdgeList == [],	% mail fail
    	AdjArray = [].
    distribute_edges(NNodes, EdgeList, AdjArray) :-
	dim(AdjArray, [NNodes]),
	sort(0, <, EdgeList, SortedEdgeList),
	arg(1, AdjArray, List1),
	distribute_edges(1, NNodes, SortedEdgeList, List1, AdjArray).

    distribute_edges(I, N, [Edge|Edges], TailI, Array) :- Edge = e(I,J,_), !,
	TailI = [Edge|TailI1],
	J =< N,	% mail fail
	distribute_edges(I, N, Edges, TailI1, Array).
    distribute_edges(I, N, Edges, [], Array) :-
	I1 is I+1,
	( I1 > N ->
	    Edges == []	% mail fail
	;
	    arg(I1, Array, ListI),
	    distribute_edges(I1, N, Edges, ListI, Array)
	).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

:- export make_sub_graph/3.
:- comment(make_sub_graph/3, [
    summary:"Creates a subgraph (projection on nodes) of a given graph",
    amode:(make_sub_graph(+,++,-) is det) ,
    args:[
	"Graph":"a graph structure",
	"Nodes":"a list of node numbers",
	"SubGraph":"will be bound to a graph structure" ],
    see_also:[is_sub_graph/2],
    desc:html("<P>
    Creates a sub-graph SubGraph of the original graph Graph. SubGraph has
    the same set of nodes as Graph, but only those edges that begin and end
    in the specified set of nodes Nodes.
    </P>")]).

make_sub_graph(Super, Nodes, Sub) :-
	nonvar(Super), nonvar(Nodes), var(Sub), !,
	Super = graph with [size:N,adj:AdjSuper],
	Sub = graph with [size:N,adj:AdjSub],
	dim(AdjSub, [N]),
	dim(IsSubNode, [N]),
	( foreach(I,Nodes), param(IsSubNode) do
	    arg(I, IsSubNode, true)
	),
	(
	    for(From,1,N),
	    param(AdjSuper,AdjSub,IsSubNode)
	do
	    arg(From, AdjSub, EdgesSub),
	    arg(From, IsSubNode, FromIsInSubGraph),
	    ( nonvar(FromIsInSubGraph) ->
		arg(From, AdjSuper, EdgesSuper),
		(
		    foreach(Edge,EdgesSuper),
		    fromto(EdgesSub,EdgesSub1,EdgesSub0,[]),
		    param(IsSubNode)
		do
		    Edge = e(_,To,_),
		    arg(To, IsSubNode, ToIsInSubGraph),
		    ( nonvar(ToIsInSubGraph) ->
			EdgesSub1 = [Edge|EdgesSub0]
		    ;
			EdgesSub1 = EdgesSub0	% ignore edge to To
		    )
		)
	    ;
	    	EdgesSub = []	% ignore edges from From
	    )
	).
make_sub_graph(Super, Nodes, Sub) :-
	error(4, make_sub_graph(Super, Nodes, Sub)).



% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% Make a new graph from an array of "incoming" edges.
% We need to construct the "outgoing" (adj) array.

make_graph_from_incoming_edges(NNodes, Incoming,
	    graph with [size:NNodes,in:Incoming,adj:AdjArray]) :-
	dim(AdjArray, [NNodes]),
	(
	    foreacharg([],AdjArray)
	do
	    true
	),
    	(
	    foreacharg(EdgeList,Incoming),
	    param(AdjArray)
	do
	    (
		foreach(Edge,EdgeList),
		param(AdjArray)
	    do
		Edge = e(From, _, _),
		arg(From, AdjArray, AdjList),
		setarg(From, AdjArray, [Edge|AdjList])
	    )
	).


% Make a graph containing all edges between SourceNode and SinkNode.
% A superset of those is in the array Incoming. We need to filter
% out the relevant ones and construct the "outgoing" (adj) array.

make_graph_from_incoming_edges_single_pair(NNodes, Incoming,
	    _SourceNode, SinkNode,
	    graph with [size:NNodes,in:RelevantIncoming,adj:AdjArray]) :-
	dim(AdjArray, [NNodes]),
	dim(RelevantIncoming, [NNodes]),
	(
	    foreacharg([],AdjArray)
	do
	    true
	),
	add_incoming(SinkNode, Incoming, RelevantIncoming, AdjArray).

    add_incoming(To, Incoming, RelevantIncoming, AdjArray) :-
	arg(To, RelevantIncoming, InEdges),
	( var(InEdges) ->
	    % InEdges are relevant since they lead to node 'To'
	    arg(To, Incoming, InEdges),
	    (
		foreach(Edge,InEdges),
		param(AdjArray,Incoming,RelevantIncoming,AdjArray)
	    do
		Edge = e(From, _To, _),
		add_incoming(From, Incoming, RelevantIncoming, AdjArray),
		arg(From, AdjArray, AdjList),
		setarg(From, AdjArray, [Edge|AdjList])
	    )
	;
	    true
	).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

:- export make_graph_symbolic/3.
:- comment(make_graph_symbolic/3, [
    summary:"Creates a graph using node names",
    amode:(make_graph_symbolic(+,++,-) is det),
    args:["NodeNameArray":"array of ground data, usually node names",
	"SymbolicEdgeList":"(possibly empty) list of edge/3 structures in no particular order",
	"Graph":"will be bound to a graph structure" ],
    see_also:[make_graph/3,library(graphviz)],
    eg:"
    % the 13-node undirected graph from Sedgewick:Algorithms
    make_graph_symbolic(
	[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	[ edge(a,f,1),edge(a,b,1),edge(a,g,1),edge(c,a,1),edge(d,f,1),edge(e,d,1),
	  edge(f,e,1),edge(g,e,1),edge(g,j,1),edge(g,c,1),edge(h,g,1),edge(h,i,1),edge(i,h,1),
	  edge(j,k,1),edge(j,l,1),edge(j,m,1),edge(l,g,1),edge(l,m,1),edge(m,l,1) ],
	Graph).
    ",
    desc:html("<P>
    This predicate is similar to make_graph/3 in that it creates a
    graph data structure according to the given information.
    If the nodes have names, then make_graph_symbolic/3 allows to
    specify the graph in a more readable way by using the node names
    rather than node numbers in the edge specifications.
    The node names are given as the array NodeNameArray, and the
    symbolic edges are written in the form
<PRE>
	edge(SourceName, TargetName, EdgeData)
</PRE>
    where SourceName and TargetName should match an entry (usually the
    node name) in NodeNameArray.  Note the use of the functor edge/3 for
    the symbolic edge representation as opposed to e/3 for the internal
    edge representation.
    </P>")]).

make_graph_symbolic(NodeInfoArray, SymbolicEdgeList, Graph) :-
	Graph = graph with [size:NNodes],
	graph_set_nodenames(Graph, NodeInfoArray),
	(
	    foreach(edge(SS,ST,D),SymbolicEdgeList),
	    foreach(e(S,T,D),EdgeList),
	    param(Graph)
	do
	    nodename_to_node(Graph, SS, S),
	    nodename_to_node(Graph, ST, T)
	),
	make_graph(NNodes, EdgeList, Graph).

   array_to_hash(NodeInfoArray, InfoNodeHash) :-
   	hash_create(InfoNodeHash),
	functor(NodeInfoArray, _, N),
	( for(I,1,N), param(NodeInfoArray,InfoNodeHash) do
	    arg(I, NodeInfoArray, Info),
	    hash_add(InfoNodeHash, Info, I)
	).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

:- export graph_set_nodenames/2.
:- comment(graph_set_nodenames/2, [
    summary:"Add node names to an existing graph",
    amode:(graph_set_nodenames(+,++) is det),
    args:[ "Graph":"a graph structure",
	"NodeNameArray":"array of ground data, usually node names"],
    see_also:[make_graph_symbolic/3,graph_get_nodenames/2],
    desc:html("<P>
    </P>")]).

graph_set_nodenames(graph with [size:NNodes,nodes:NodeInfoArray,
		name_node:InfoNodeHash], NodeInfoArray) :-
	functor(NodeInfoArray, [], NNodes),
	( NNodes > 100 ->	% build hash table for large graphs
	    array_to_hash(NodeInfoArray, InfoNodeHash)
	;
	    true
	),
	!.
graph_set_nodenames(Graph, NodeInfoArray) :-
	error(6, graph_set_nodenames(Graph, NodeInfoArray)).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

:- export make_undirected_graph/2.
:- comment(make_undirected_graph/2, [
    summary:"Creates an undirected from a directed graph",
    amode:(make_undirected_graph(+,-) is det),
    args:[
	"DirectedGraph":"a graph structure",
	"UndirectedGraph":"will be bound to a graph structure" ],
    see_also:[graph_is_bidirected/1],
    desc:html("<P>
    Creates an undirected graph from a directed graph by adding reverse
    edges for every existing edge. Moreover, all loops (edges from S to S)
    are removed from the undirected graph.
    </P>")]).

make_undirected_graph(OldGraph, NewGraph) :-
	graph_get_all_edges(OldGraph, OldEdges),
	(
	    foreach(OldEdge,OldEdges),
	    fromto(NewEdges,NewEdges1,NewEdges0,[])
	do
	    OldEdge = e(S,T,D),
	    ( S==T ->
		% remove self loops
	    	NewEdges1=NewEdges0
	    ;
		% keep old edge, add reverse one
	    	NewEdges1=[OldEdge,e(T,S,D)|NewEdges0]
	    )
	),
	graph_get_maxnode(OldGraph, NNodes),
	make_graph(NNodes, NewEdges, NewGraph).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

:- export make_random_graph/6.
:- comment(make_random_graph/6, [
    summary:"Creates a random graph with the given properties",
    amode:(make_random_graph(+,+,+,+,+,-) is det),
    args:[
	"NNodes":"integer",
	"NEdges":"integer",
	"AntiParallelFree":"one of the atoms true or false",
	"LoopFree":"one of the atoms true or false",
	"ParallelFree":"one of the atoms true or false",
	"Graph":"will be bound to a graph structure" ],
    exceptions:[6:"NEdges is too large given the chosen options and NNodes"],
    see_also:[graph_set_nodenames/2,graph_get_all_edges/2,
	graph_set_random_weights/3,
    	graph_get_adjacent_edges/3,library(graphviz)],
    desc:html("<P>
    Creates a random graph with NNodes nodes and NEdges edges.
    The other 3 options mean:
<DL>
<DT><B>AntiParallelFree</B></DT>
    <DD>if true, the graph will have no anti-parallel edges, i.e. if
    there is an edge from S to T, there won't be one from T to S</DD>
<DT><B>LoopFree</B></DT>
    <DD>if true, the graph will have no self loops, i.e. no edges
    that begin and end in the same node.</DD>
<DT><B>ParallelFree</B></DT>
    <DD>if true, the graph will have no parallel edges, i.e. there
    will be at most one edge from S to T.</DD>
</DL>
    The generated graph has no node name information and no edge data.
    Node names can be added using graph_set_nodenames/2. Edge data
    (e.g. random weights) can be added by retrieving the edges using
    graph_get_all_edges/2 or graph_get_adjacent_edges/3 and unifying
    the variable EdgeData field with the required information.
    </P>")]).

make_random_graph(NNodes, NEdges, AntiParFree, LoopFree, ParFree, Graph) :-
	NNodes >= 0, NEdges >= 0,
	check_edge_number(NNodes, NEdges, AntiParFree, LoopFree, ParFree),
	!,
   	hash_create(TabooTable),
	( LoopFree == true -> Range is NNodes-1 ; Range = NNodes ),
	(
	    for(_,1,NEdges),
	    foreach(e(S,T,_),EdgeList),
	    param(NNodes,Range,ParFree,AntiParFree,TabooTable)
	do
	    once (
		repeat,
		S is 1 + random mod NNodes,
		T is 1 + (S + random mod Range) mod NNodes,
		not hash_find(TabooTable, S-T, _)
	    ),
	    ( ParFree == true -> hash_add(TabooTable, S-T, _) ; true ),
	    ( AntiParFree == true -> hash_add(TabooTable, T-S, _) ; true )
	),
	make_graph(NNodes, EdgeList, Graph).
make_random_graph(NNodes, NEdges, AntiParFree, LoopFree, ParFree, Graph) :-
	error(6, make_random_graph(NNodes, NEdges, AntiParFree, LoopFree, ParFree, Graph)).

    check_edge_number(_NNodes, _NEdges, _, _, false) ?- !.
    check_edge_number(NNodes, NEdges, false, false, true) ?- !,
    	NEdges =< NNodes*NNodes.	% one of any pair
    check_edge_number(NNodes, NEdges, false, true, true) ?- !,
    	NEdges =< NNodes*(NNodes-1).	% minus diagonal
    check_edge_number(NNodes, NEdges, true, true, true) ?- !,
    	NEdges =< NNodes*(NNodes-1)//2.	% half minus diagonal
    check_edge_number(NNodes, NEdges, true, false, true) ?- !,
    	NEdges =< NNodes*NNodes - NNodes*(NNodes-1)//2.


:- export graph_set_random_weights/3.
:- comment(graph_set_random_weights/3, [
    summary:"Unifies all the graph's edge data fields with random numbers",
    amode:(graph_set_random_weights(+,+,+) is det),
    args:[
	"Graph":"a graph structure",
	"Min":"an integer or float",
	"Max":"an integer or float"],
    see_also:[make_random_graph/6,random/1,frandom/1],
    desc:html("<P>
    The input Graph is assumed to be a graph with uninstantiated edge
    data fields. These will be instantiated to random numbers between
    Min and Max, using random/1 if Min and Max are integers, or using
    frandom/1 if Min and Max are floats.
    </P>")]).

graph_set_random_weights(graph with adj:AdjLists, Min, Max) :-
	integer(Min), integer(Max), Min =< Max,
	Range is Max-Min+1,
	( foreacharg(Edges,AdjLists), param(Min,Range) do
	    ( foreach(e(_,_,W),Edges), param(Min,Range) do
	    	W is Min + random mod Range
	    )
	).
graph_set_random_weights(graph with adj:AdjLists, Min, Max) :-
	real(Min), real(Max), Min =< Max,
	Range is Max-Min,
	( foreacharg(Edges,AdjLists), param(Min,Range) do
	    ( foreach(e(_,_,W),Edges), param(Min,Range) do
	    	W is Min + frandom * Range
	    )
	).


:- export graph_reverse_edges/2.
:- comment(graph_reverse_edges/2, [
    summary:"Makes a graph with reversed edges",
    amode:(graph_reverse_edges(+,-) is det),
    args:[
	"Graph":"a graph structure",
	"RevGraph":"a graph structure"],
    see_also:[make_graph/3,make_graph_symbolic/3],
    desc:html("<P>
    Creates a new graph RevGraph which is identical to Graph except
    that all edges are reversed.
    </P>")]).

:- export graph_reverse_edges/2.
graph_reverse_edges(GraphIn, GraphOut) :-
	GraphIn = graph with [size:NNodes,adj:Adj,in:In],
	update_struct(graph, [adj:RevAdj,in:_], GraphIn, GraphOut),
	dim(RevAdj, [NNodes]),
	( var(In) ->
	    % we don't have the incoming edges...
	    ( foreacharg([],RevAdj) do true ),
	    (
		foreacharg(AdjEdges,Adj),
		param(RevAdj)
	    do
		(
		    foreach(e(From,To,D),AdjEdges),
		    param(RevAdj)
		do
		    arg(To, RevAdj, RevAdjEdges),
		    setarg(To, RevAdj, [e(To,From,D)|RevAdjEdges])
		)
	    )
	;
	    % if we have the incoming edges, it's easier
	    (
		for(I,1,NNodes),
		param(In,RevAdj)
	    do
	    	arg(I, In, InEdges),
	    	arg(I, RevAdj, RevAdjEdges),
		(
		    foreach(e(From,To,D),InEdges),
		    foreach(e(To,From,D),RevAdjEdges)
		do
		    true
		)
	    )
	).


%----------------------------------------------------------------------
% Retrieve graph data
%----------------------------------------------------------------------

:- export graph_get_all_edges/2.
:- comment(graph_get_all_edges/2, [
    summary:"Returns a sorted list of all edges in the graph",
    amode:(graph_get_all_edges(+,-) is det),
    args:[
	"Graph":"a graph structure",
	"EdgeList":"a list of e/3 edge structures" ],
    see_also:[graph_get_adjacent_edges/3,graph_get_incoming_edges/3,
   	 graph_get_maxnode/2],
    desc:html("<P>
    </P>")]).

graph_get_all_edges(graph with adj:AdjArr, EdgeList) :-
	( foreacharg(AdjList,AdjArr),
	    fromto(EdgeList,L3,L1,[])
	do
	    append(AdjList, L1, L3)
	).


:- export graph_get_adjacent_edges/3.
:- comment(graph_get_adjacent_edges/3, [
    summary:"Returns a sorted list of all edges starting from SourceNode",
    amode:(graph_get_adjacent_edges(+,+,-) is det),
    args:[
	"Graph":"a graph structure",
	"SourceNode":"an integer node number",
	"EdgeList":"a list of e/3 edge structures" ],
    see_also:[graph_get_all_edges/2,graph_get_incoming_edges/3,graph_get_maxnode/2],
    desc:html("<P>
    </P>")]).

graph_get_adjacent_edges(graph with adj:AdjArr, Node, EdgeList) :-
	arg(Node, AdjArr, EdgeList).


:- export graph_get_incoming_edges/3.
:- comment(graph_get_incoming_edges/3, [
    summary:"Returns a sorted list of all edges ending in TargetNode",
    amode:(graph_get_incoming_edges(+,+,-) is det),
    args:[
	"Graph":"a graph structure",
	"TargetNode":"an integer node number",
	"EdgeList":"a list of e/3 edge structures" ],
    see_also:[graph_get_all_edges/2,graph_get_adjacent_edges/3],
    desc:html("<P>
    Note on performance: By default, the graph structure only stores the
    outgoing (adjacent) edges of every node.  The incoming edge lists are
    computed lazily when graph_get_incoming_edges/3 is called for the first
    time (but then they are built for all nodes at once).  Therefore the
    first call to this predicate has O(NlogN) complexity, subsequent calls
    are only O(1). It may therefore make sense to do a dummy call to this
    predicate before starting time critical or nondeterministic computation.
    </P>")]).

graph_get_incoming_edges(Graph, Node, EdgeList) :-
	Graph = graph with [size:NNodes,adj:Adj,in:InArr],
	( nonvar(InArr) ->
	    true
	;	% first time: compute the incoming edges
	    dim(InArr, [NNodes]),
	    ( foreacharg([],InArr) do true ),
	    (
		foreacharg(EdgeList,Adj),
		param(InArr)
	    do
		(
		    foreach(Edge,EdgeList),
		    param(InArr)
		do
		    Edge = e(_, To, _),
		    arg(To, InArr, AdjList),
		    setarg(To, InArr, [Edge|AdjList])
		)
	    )
	),
	arg(Node, InArr, EdgeList).


:- export graph_get_maxnode/2.
:- comment(graph_get_maxnode/2, [
    summary:"Returns the highest node number in Graph",
    amode:(graph_get_maxnode(+,-) is det),
    args:[
	"Graph":"a graph structure",
	"MaxNode":"an integer node number"],
    see_also:[graph_get_all_edges/2,graph_get_adjacent_edges/3],
    desc:html("<P>
    Note that the smallest node number in a graph is always 1.
    </P>")]).

graph_get_maxnode(graph with size:NNodes, NNodes).


:- export graph_get_nodenames/2.
:- comment(graph_get_nodenames/2, [
    summary:"Returns the array of node names",
    amode:(graph_get_nodenames(+,-) is semidet),
    fail_if:"Fails if no node name information is available",
    args:[
	"Graph":"a graph structure",
	"NodeNameArray":"array of ground data, usually node names"],
    see_also:[graph_set_nodenames/2],
    desc:html("<P>
    </P>")]).

graph_get_nodenames(graph with nodes:Names, Names) :-
	nonvar(Names).


:- export node_to_nodename/3.
:- comment(node_to_nodename/3, [
    summary:"Retrieves the name of a node",
    amode:(node_to_nodename(+,+,-) is semidet),
    fail_if:"Fails if no node name information is available",
    args:[
	"Graph":"a graph structure",
	"Node":"an integer node number",
	"NodeName":"ground data, usually node name"],
    see_also:[nodes_to_nodenames/3,nodename_to_node/3,nodenames_to_nodes/3],
    desc:html("<P>
    </P>")]).

node_to_nodename(graph with nodes:Names, Node, NodeName) :-
	nonvar(Names),
	arg(Node, Names, NodeName).


:- export nodes_to_nodenames/3.
:- comment(nodes_to_nodenames/3, [
    summary:"Returns the names corresponding to a list of nodes",
    amode:(nodes_to_nodenames(+,+,-) is semidet),
    fail_if:"Fails if no node name information is available",
    args:[
	"Graph":"a graph structure",
	"Nodes":"a list of integer node numbers",
	"NodeNames":"a list of ground data, usually node names"],
    see_also:[node_to_nodename/3,nodename_to_node/3,nodenames_to_nodes/3],
    desc:html("<P>
    </P>")]).

nodes_to_nodenames(Graph, Nodes, NodeNames) :-
	( foreach(Node,Nodes), foreach(NodeName,NodeNames), param(Graph) do
	    node_to_nodename(Graph, Node, NodeName)
	).


:- export nodename_to_node/3.
:- comment(nodename_to_node/3, [
    summary:"Retrieves the node number given a node name",
    amode:(nodename_to_node(+,+,-) is semidet),
    fail_if:"Fails if no node name information is available",
    args:[
	"Graph":"a graph structure",
	"NodeName":"ground data, usually node name",
	"Node":"an integer node number"],
    see_also:[node_to_nodename/3,nodes_to_nodenames/3,nodenames_to_nodes/3],
    desc:html("<P>
    </P>")]).

nodename_to_node(graph with [size:N,nodes:Names,name_node:NameNodeTable], Name, Node) :-
	nonvar(Names),
	( var(NameNodeTable) ->
	    between(1, N, 1, Node),
	    arg(Node, Names, Name),
	    !
	;
	    hash_find(NameNodeTable, Name, Node)
	).


:- export nodenames_to_nodes/3.
:- comment(nodenames_to_nodes/3, [
    summary:"Returns the names corresponding to a list of nodes",
    amode:(nodenames_to_nodes(+,+,-) is semidet),
    fail_if:"Fails if no node name information is available",
    args:[
	"Graph":"a graph structure",
	"NodeNames":"a list of ground data, usually node names",
	"Nodes":"a list of integer node numbers"],
    see_also:[node_to_nodename/3,nodename_to_node/3,nodes_to_nodenames/3],
    desc:html("<P>
    </P>")]).

nodenames_to_nodes(Graph, NodeNames, Nodes) :-
	( foreach(Node,Nodes), foreach(NodeName,NodeNames), param(Graph) do
	    nodename_to_node(Graph, NodeName, Node)
	).


:- export graph_get_edge/4.
:- comment(graph_get_edge/4, [
    summary:"Finds a graph edge given source and target node",
    amode:(graph_get_edge(+,+,+,-) is semidet),
    fail_if:"Fails if no matching edge exists",
    args:[
	"Graph":"a graph structure",
	"Source":"an integer node number",
	"Target":"an integer node number",
	"Edge":"an e/3 edge structure"],
    see_also:[graph_get_all_edges/2,graph_get_adjacent_edges/3,
    	graph_edge/2,graph_adjacent_edge/3,graph_get_edges/4],
    desc:html("<P>
    This operation is linear in the number of edges adjacent to Source.
    </P>")]).

graph_get_edge(Graph, Source, Target, Edge) :-
	integer(Source), integer(Target),
	EdgeList is Graph[adj of graph, Source],
	Edge = e(Source,Target,_),
	memberchk(Edge, EdgeList).


:- export graph_get_edges/4.
:- comment(graph_get_edges/4, [
    summary:"Finds all edges between source node and target node",
    amode:(graph_get_edges(+,+,+,-) is det),
    args:[
	"Graph":"a graph structure",
	"Source":"an integer node number",
	"Target":"an integer node number",
	"Edges":"a list of  e/3 edge structures"],
    see_also:[graph_get_all_edges/2,graph_get_adjacent_edges/3,
    	graph_edge/2,graph_adjacent_edge/3,graph_get_edge/4],
    desc:html("<P>
    This operation is linear in the number of edges adjacent to Source.
    </P>")]).

graph_get_edges(Graph, Source, Target, Edges) :-
	graph_get_adjacent_edges(Graph, Source, AdjEdges),
	(
	    foreach(Edge,AdjEdges),
	    fromto(Edges,Edges1,Edges0,[]),
	    param(Target)
	do
	    ( Edge = e(_,Target,_) ->
	    	Edges1 = [Edge|Edges0]
	    ;
		Edges1 = Edges0
	    )
	).


%----------------------------------------------------------------------
% Generate Nodes and Edges
%----------------------------------------------------------------------

:- export graph_node/2.
:- comment(graph_node/2, [
    summary:"Succeeds if Node is a node of the graph",
    amode:(graph_node(+,?) is nondet),
    args:[
	"Graph":"a graph structure",
	"Node":"an integer node number"],
    see_also:[graph_get_maxnode/2],
    desc:html("<P>
    </P>")]).

graph_node(graph with size:N, X) :-
	between(1, N, 1, X).


:- export graph_adjacent_edge/3.
:- comment(graph_adjacent_edge/3, [
    summary:"Succeeds if Edge is an edge adjacent to Node in Graph",
    amode:(graph_adjacent_edge(+,+,?) is nondet),
    args:[
	"Graph":"a graph structure",
	"Node":"an integer node number",
	"Edge":"an e/3 edge structure"],
    see_also:[graph_edge/2,graph_get_adjacent_edges/3],
    desc:html("<P>
    </P>")]).

graph_adjacent_edge(Graph, Node, Edge) :-
	EdgeList is Graph[adj of graph, Node],
	member(Edge, EdgeList).


:- export graph_edge/2.
:- comment(graph_edge/2, [
    summary:"Succeeds if Edge is an edge in graph",
    amode:(graph_edge(+,?) is nondet),
    args:[
	"Graph":"a graph structure",
	"Edge":"an e/3 edge structure"],
    see_also:[graph_adjacent_edge/3,graph_get_all_edges/2],
    desc:html("<P>
    </P>")]).

graph_edge(Graph, Edge) :-
	graph_node(Graph, Node),
	graph_adjacent_edge(Graph, Node, Edge).


%----------------------------------------------------------------------
% Test Graph Properties
%----------------------------------------------------------------------

    % check sanity of the graph data structure

:- export proper_graph/1.
:- comment(proper_graph/1, [
    summary:"Tests the integrity of the given graph data structure",
    amode:(proper_graph(+) is semidet),
    args:["Graph":"a graph structure"],
    see_also:[],
    desc:html("<P>
    </P>")]).

proper_graph(graph with [size:NNodes,adj:AdjArr,nodes:Names]) :-
    	integer(NNodes),
	NNodes >= 0,
	functor(AdjArr, [], NNodes),
	( foreacharg(EdgeList,AdjArr), param(NNodes) do
	    % _DataArity instead of DataArity to suppress singleton warning
	    ( foreach(e(S,T,D),EdgeList), param(NNodes,_DataArity) do
		1 =< S, S =< NNodes,
		1 =< T, T =< NNodes,
		(nonvar(D) -> functor(D,_,_DataArity) ; true )
	    )
	),
	( var(Names) -> true ; functor(Names, [], NNodes) ).


    % algorithm from the LEDA book

:- export graph_is_bidirected/1.
:- comment(graph_is_bidirected/1, [
    summary:"Tests whether a graph is bidirected",
    amode:(graph_is_bidirected(+) is semidet),
    args:["Graph":"a graph structure"],
    see_also:[make_undirected_graph/2],
    desc:html("<P>
    A graph is bidirected if for every edge from S to T there is a
    corresponding edge from T to S. In case of loops (i.e. an edge
    from S to S) a second, different edge from S to S is required
    to satisfy the bidirectedness condition.
    </P>")]).

graph_is_bidirected(Graph) :-
	graph_get_all_edges(Graph, OrigEdges),
	sort(2, =<, OrigEdges, EdgesUp0),
	sort(1, =<, EdgesUp0, EdgesUp),
	reverse(OrigEdges, RevEdges),
	sort(1, =<, RevEdges, EdgesDown0),
	sort(2, =<, EdgesDown0, EdgesDown),
	(
	    foreach(e(S,T,D1),EdgesUp),
	    foreach(e(T,S,D2),EdgesDown)
	do
	    % don't match self loops with themselves
	    ( S==T -> D1 \== D2 ; true )
	).


:- export is_sub_graph/2.
:- comment(is_sub_graph/2, [
    summary:"Succeeds iff SubGraph is a subgraph of SuperGraph",
    amode:(is_sub_graph(+,+) is semidet),
    args:[ "SubGraph":"a graph structure",
    	"SuperGraph":"a graph structure"],
    see_also:[make_sub_graph/3],
    fail_if:"SubGraph is not a subgraph of SuperGraph",
    desc:html("
    Tests whether SubGraph is a (non-strict) subgraph of SuperGraph.
    This is the case when the nodes and edges in SubGraph are a subset
    of the nodes and edges of SuperGraph. Note that nodes are considered
    identical when they have the same node numbers (rather than the same
    node names - node name information is ignored by this predicate.).
    ")]).

is_sub_graph(Sub, Super) :-
	Sub = graph with [size:NSub,adj:AdjSub],
	Super = graph with [size:NSuper,adj:AdjSuper],
	NSub =< NSuper,
	(
	    for(I,1,NSub),
	    param(AdjSub,AdjSuper)
	do
	    arg(I, AdjSub, EdgesSub),
	    arg(I, AdjSuper, EdgesSuper),
	    sort(0, =<, EdgesSub, EdgesSubSorted),
	    sort(0, =<, EdgesSuper, EdgesSuperSorted),
	    ordset:ord_subset(EdgesSubSorted, EdgesSuperSorted)
	).


%----------------------------------------------------------------------
% topological sort and cycles
%----------------------------------------------------------------------

:- export top_sort/2.
:- comment(top_sort/2, [
    summary:"Finds a topological ordering of the graph if one exists",
    amode:(top_sort(+,-) is semidet),
    args:[ "Graph":"a graph structure",
	"Sorted":"a list of integer node numbers"],
    see_also:[graph_is_acyclic/1,graph_cycles/2],
    fail_if:"No topological ordering exists, i.e. the graph is cyclic",
    desc:html("<P>
    Finds a topological ordering of the graph, i.e. an ordering of the
    nodes such that all edges go from earlier to later nodes.
    Such an ordering exists if and only if the graph is acyclic.
    If the graph is cyclic, the predicate fails.
    </P><P>
    In general, the ordering is not unique, an arbitrary one is computed.
    The complexity is O(Nnodes + Nedges).
    </P>")]).

top_sort(G, Ordered) :-
	G = graph with [size:MaxNode, adj:Adj],
	dim(Seen, [MaxNode]),
	(
	    for(StartNode, 1, MaxNode),
	    fromto([], Ordered0, Ordered1, Ordered),
	    param(Adj,Seen)
	do
	    NodeSeen is Seen[StartNode],
	    ( var(NodeSeen) ->
		topsort_visit(Adj, Seen, [e(_,StartNode,_)], [], Ordered0, Ordered1)
	    ;
	    	Ordered0 = Ordered1
	    )
	).

    topsort_visit(_Adj, _Seen, [], [], Ordered0, Ordered) :- !,
    	Ordered = Ordered0.
    topsort_visit(Adj, Seen, [], [[e(_,Node,_)|Edges]|Stack], Ordered0, Ordered) :- !,
	arg(Node, Seen, seen(done)),	% mark done
	topsort_visit(Adj, Seen, Edges, Stack, [Node|Ordered0], Ordered).
    topsort_visit(Adj, Seen, EdgeEdges, Stack, Ordered0, Ordered) :-
	EdgeEdges = [e(_,Node,_)|Edges],
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    NodeSeen = seen(_),		% mark visited
	    arg(Node, Adj, Successors),
	    topsort_visit(Adj, Seen, Successors, [EdgeEdges|Stack], Ordered0, Ordered)
	;
	    NodeSeen = seen(Done),
	    nonvar(Done),		% must be done already, else cycle
	    topsort_visit(Adj, Seen, Edges, Stack, Ordered0, Ordered)
	).


:- export graph_is_acyclic/1.
:- comment(graph_is_acyclic/1, [
    summary:"Succeeds iff the given graph has no cycles",
    amode:(graph_is_acyclic(+) is semidet),
    args:[ "Graph":"a graph structure"],
    see_also:[top_sort/2,graph_cycles/2],
    fail_if:"No topological ordering exists, i.e. the graph is cyclic"]).

graph_is_acyclic(G) :-
	top_sort(G, _Ordered).


:- export graph_cycles/2.
:- comment(graph_cycles/2, [
    summary:"Computes a list of edges whose removal would make the graph acyclic",
    amode:(graph_cycles(+,-) is det),
    args:["Graph":"a graph structure",
	"BreakingEdges":"a list of e/3 edge structures"],
    see_also:[graph_is_acyclic/1, top_sort/2],
    desc:html("<P>
    Computes a set of edges whose removal would make the graph acyclic.
    This set is not necessarily minimal, and it contains an arbitrary
    edge from every cycle in the graph.
</P><P>
    If the list is empty, the graph is already acyclic.
    </P>")]).

graph_cycles(G, UpEdges) :-
	G = graph with [size:MaxNode, adj:Adj],
	dim(Seen, [MaxNode]),
	(
	    for(StartNode, 1, MaxNode),
	    fromto([], UpEdges0, UpEdges1, UpEdges),
	    param(Adj,Seen)
	do
	    NodeSeen is Seen[StartNode],
	    ( var(NodeSeen) ->
		cyc_visit(Adj, Seen, [e(_,StartNode,_)], [], UpEdges0, UpEdges1)
	    ;
	    	UpEdges0 = UpEdges1
	    )
	).

    cyc_visit(_Adj, _Seen, [], [], UpEdges0, UpEdges) :- !,
    	UpEdges = UpEdges0.
    cyc_visit(Adj, Seen, [], [[e(_,Node,_)|Edges]|Stack], UpEdges0, UpEdges) :- !,
	arg(Node, Seen, seen(done)),	% mark done
	cyc_visit(Adj, Seen, Edges, Stack, UpEdges0, UpEdges).
    cyc_visit(Adj, Seen, EdgeEdges, Stack, UpEdges0, UpEdges) :-
	EdgeEdges = [Edge|Edges],
	Edge = e(_,Node,_),
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    NodeSeen = seen(_),		% mark visited
	    arg(Node, Adj, Successors),
	    cyc_visit(Adj, Seen, Successors, [EdgeEdges|Stack], UpEdges0, UpEdges)
	;
	    NodeSeen = seen(Done),
	    ( nonvar(Done) ->
		cyc_visit(Adj, Seen, Edges, Stack, UpEdges0, UpEdges)
	    ;
		cyc_visit(Adj, Seen, Edges, Stack, [Edge|UpEdges0], UpEdges)
	    )
	).



:- export connected_components/2.
:- comment(connected_components/2, [
    summary:"Computes the connected components of graph",
    amode:(connected_components(+,-) is det),
    args:["Graph":"a graph structure",
	"ConnectedComponents":"a list of lists of node numbers"],
    see_also:[graph_is_bidirected/1, articulation_points/2],
    desc:html("<P>
    Computes the connected components of a (bidirected) graph. Each resulting
    connected component is represented as an (unsorted) list of nodes.
</P><P>
    This operation is only defined on bidirected graphs.
    The runtime complexity is O(Nnodes + Nedges).
    </P>")]).

connected_components(G, CC) :-		% bidirected graphs only
	G = graph with [size:MaxNode, adj:Adj],
	dim(Seen, [MaxNode]),
	(
	    for(StartNode, 1, MaxNode),
	    fromto(CC, CCs1, CCs0, []),
	    param(Adj,Seen)
	do
	    NodeSeen is Seen[StartNode],
	    ( var(NodeSeen) ->
		CCs1 = [CC|CCs0],
		NodeSeen = seen,		% mark visited
		cc_visit(Adj, Seen, StartNode, CC, [])
	    ;
	    	CCs1 = CCs0
	    )
	).

    cc_visit(Adj, Seen, From, [From|CC], CC0) :-
	arg(From, Adj, Edges),
	(
	    foreach(e(_,To,_),Edges),
	    fromto(CC,CC2,CC1,CC0),
	    param(Adj,Seen)
	do
	    arg(To, Seen, NodeSeen),
	    ( var(NodeSeen) ->
		NodeSeen = seen,		% mark visited
		cc_visit(Adj, Seen, To, CC2, CC1)
	    ;
		CC2 = CC1
	    )
	).


%----------------------------------------------------------------------
% critical_links(+Graph, -Edges)
%
% Algorithm: depth-first traversal of an undirected graph, corresponds to
% traversing a tree, plus there are some extra upward-links. As the nodes
% are visited, they are assigned labels (increasing integers) in the Labels-
% array. Undirected links either go to unvisited nodes or they are up-links.
% For every down-edge, traverse the subtree it starts. During traversal, keep
% track of the highest node reachable by an up-link. At the end of traversal,
% check whether the start node or higher was reachable from that subtree
% (but ignoring the reverse of the down-link at the root of the subtree).
% If that is not the case, then the root link of this subtree is critical.
%----------------------------------------------------------------------

:- export critical_links/2.
:- comment(critical_links/2, [
    summary:"Finds critical links in a bidirected graph",
    amode:(critical_links(+,-) is semidet),
    args:[ "Graph":"a graph structure",
	"Links":"a list of edge pairs (output)" ],
    fail_if:"Fails if the graph is not bidirected",
    see_also:[graph_is_bidirected/1,articulation_points/2],
    desc:html("<P>
    Finds all the critical links of a bidirected graph. We define an
    (bidirected) link as a pair of (oppositely directed) edges between
    two nodes. A critical link is one that, when removed, would make the
    graph more disconnected.
    <P></P>
    The result is returned as a list of links, where each link is a
    hyphenated pair of e/3 edge structures like e(I,J,Dij) - e(J,I,Dji).
    </P>")]).

critical_links(G, CL) :-
	graph_get_maxnode(G, MaxNode),
	dim(Labels, [MaxNode]),
	(
	    for(Node, 1, MaxNode),
	    fromto(0, Id0, Id1, _),
	    fromto([], CL0, CL2, CL),
	    param(G, Labels)
	do
	    Seen is Labels[Node],
	    ( var(Seen) ->
		cl_visit(G, Labels, -1, Node, Id0, Id1, _, CL0, CL2, _)
	    ;
	    	Id0 = Id1, CL0 = CL2
	    )
	).

    cl_visit(G, Labels, Parent, Node, Id0, Id4, Min, CL0, CL, UpEdge) :-
	Id is Id0 + 1,
	Id is Labels[Node],
	graph_get_adjacent_edges(G, Node, Ys),
	(
	    foreach(Edge, Ys),
	    fromto(Id, Id2, Id3, Id4),
	    fromto(Id, Min2, Min3, Min),
	    fromto(CL0, CL1, CL3, CL),
	    fromto(Parent, Parent1, Parent2, -1),
	    param(G, Labels, Id, UpEdge)
	do
	    Edge = e(Node,Y,_),
	    ( Y = Parent1 ->
	    	Parent2 = -1,	% ignore first link back to parent
		UpEdge = Edge,
		Min3 = Min2,
		CL3 = CL1,
		Id3 = Id2
	    ;
		Parent2 = Parent1,
		YLabel is Labels[Y],
		( var(YLabel) ->
		    cl_visit(G, Labels, Node, Y, Id2, Id3, YMin, CL1, CL2, YUpEdge),
		    Min3 is min(Min2, YMin),
		    ( YMin > Id ->	% found a subtree with no links back
			CL3 = [Edge-YUpEdge|CL2]
		    ;
			CL3 = CL2
		    )
		;
		    Min3 is min(Min2, YLabel),
		    CL3 = CL1,
		    Id3 = Id2
		)
	    )
	).


%----------------------------------------------------------------------
% Find articulation points
% This is only valid for bidirected graphs:
%----------------------------------------------------------------------

:- export articulation_points/2.
:- comment(articulation_points/2, [
    summary:"Finds the articulation points of the graph",
    amode:(articulation_points(+,-) is det),
    args:[ "Graph":"a graph structure",
	"Articulations":"a list of integer node numbers" ],
    see_also:[graph_is_bidirected/1,critical_links/2,biconnected_components/3],
    desc:html("<P>
    Finds the articulation points of a graph, i.e. those nodes that,
    when deleted, would break the graph into two or more disconnected
    components. If there are no articulation points, the graph is
    called biconnected.
<P></P>
    This operation is only defined for bidirected graphs.
    </P>")]).

articulation_points(G, AP) :-
	G = graph with [size:MaxNode],
	dim(Val, [MaxNode]),
	(
	    for(Node, 1, MaxNode),
	    fromto(0, Id0, Id1, _),
	    fromto([], AP0, AP2, AP),
	    param(G, Val)
	do
	    Seen is Val[Node],
	    ( var(Seen) ->
		bc_visit(G, Val, Node, Id0, Id1, _, AP0, AP1, Count),
		% root is AP if it has at least two subtrees
		( Count >= 2 -> AP2=[Node|AP1] ; AP2=AP1 )
	    ;
	    	Id0 = Id1, AP0 = AP2
	    )
	).

    % Count: the number of subtrees of Node that have no link to above Node
    bc_visit(G, Val, Node, Id0, Id4, Min, AP0, AP, Count) :-
	Id is Id0 + 1,
	Id is Val[Node],
%	writeln(Id:node(Node)),
	Ys is G[adj of graph, Node],
	(
	    foreach(e(_,Y,_), Ys),
	    fromto(Id, Id2, Id3, Id4),
	    fromto(Id, Min2, Min3, Min),
	    fromto(0, Count0, Count1, Count),
	    fromto(AP0, AP1, AP3, AP),
	    param(G, Val, Id)
	do
	    YVal is Val[Y],
	    ( var(YVal) ->
	    	bc_visit(G, Val, Y, Id2, Id3, YMin, AP1, AP2, YCount),
		% Y is AP if at least one of its subtrees has no links above Y
		( YCount >= 1 -> AP3=[Y|AP2] ; AP3=AP2 ),
%		writeln(result(visit(Y) is YMin)),
		Min3 is min(Min2, YMin),
		( YMin >= Id ->	% found a subtree with no links above Node
		    Count1 is Count0+1
		;
		    Count1 = Count0
		)
	    ;
		Min3 is min(Min2, YVal),
		Count1 = Count0,
		AP3 = AP1,
	    	Id3 = Id2
	    )
	).



:- export biconnected_components/3.
:- comment(biconnected_components/3, [
    summary:"Finds the biconnected components of the graph",
    amode:(biconnected_components(+,-,-) is det),
    args:[ "Graph":"a graph structure",
	"Articulations":"a list of integer node numbers",
    	"BCC":"list of lists of integer node numbers"],
    see_also:[graph_is_bidirected/1,articulation_points/2,strong_components/2],
    desc:html("<P>
    Computes the biconnected components of a graph, i.e. maximal subsets of
    the graph's nodes whose nodes are mutually accessible via at least two
    distinct paths (in other words, subgraphs which have no articulation
    points).
<P></P>
    Also compute a list of articulation points, i.e. those nodes that
    connect the biconnected components to each other.
<P></P>
    This operation is only defined for bidirected graphs.
<P></P>
    Note that by convention, isolated nodes and pairs of nodes connected
    by a single (bidirected) edge also form biconnected components.
    </P>")]).

% This is the same algorithms as in articulation_points/2
% but additionally collects the biconnected components
biconnected_components(G, AP, BCCs) :-
	G = graph with [size:MaxNode],
	dim(Val, [MaxNode]),
	(
	    for(Node, 1, MaxNode),
	    fromto(0, Id0, Id1, _),
	    fromto([], AP0, AP2, AP),
	    fromto(BCCs, BCCs3, BCCs1, []),
	    param(G, Val)
	do
	    Seen is Val[Node],
	    ( var(Seen) ->
		bcc_visit(G, Val, Node, Id0, Id1, _Min, AP0, AP1, BCC, [], BCCs2, BCCs1, Count),
		( Count >= 2 ->
		    % root is AP if it has at least two subtrees
		    AP2=[Node|AP1], BCCs3=BCCs2
		; Count = 1 ->
		    AP2=AP1, BCCs3=BCCs2
		;
		    % an isolated node is a bcc
		    AP2=AP1, BCCs3=[BCC|BCCs2]
		)
	    ;
	    	Id0 = Id1, AP0 = AP2, BCCs3 = BCCs1
	    )
	).

    bcc_visit(G, Val, Node, Id0, Id4, Min, AP0, AP, [Node|BCC], BCC0, BCCs, BCCs0, Count) :-
	Id is Id0 + 1,
	Id is Val[Node],
%	writeln(Id:node(Node)),
	Ys is G[adj of graph, Node],
	(
	    foreach(e(_Node,Y,_), Ys),
	    fromto(Id, Id2, Id3, Id4),
	    fromto(Id, Min2, Min3, Min),
	    fromto(0, Count0, Count1, Count),
	    fromto(AP0, AP1, AP3, AP),
	    fromto(BCC, BCC3, BCC1, BCC0),
	    fromto(BCCs, BCCs3, BCCs1, BCCs0),
	    param(G, Val, Id, Node)
	do
	    YVal is Val[Y],
	    ( var(YVal) ->
	    	bcc_visit(G, Val, Y, Id2, Id3, YMin, AP1, AP2, SubBCC, SubBCC0, BCCs3, BCCs2, YCount),
		% Y is AP if at least one of its subtrees has no links above Y
		( YCount >= 1 -> AP3=[Y|AP2] ; AP3=AP2 ),
%		writeln(result(visit(Y) is YMin)),
		Min3 is min(Min2, YMin),
		( YMin >= Id ->
		    % subtree with root Y has no links to higher up
		    % all the remaining subtree nodes form a bcc on their own
		    ( YMin = Id ->
			true	% because there is always a link back to the parent
		    ;
		    	writeln(error, "Assertion failed in biconnected_components/3")
		    ),
		    SubBCC0 = [],
		    BCCs2 = [[Node|SubBCC]|BCCs1],	% include current Node
		    BCC3 = BCC1,	% not part of "current" bcc
		    Count1 is Count0+1
		;
		    % subtree with root Y has links to higher up
		    % collect its nodes for "current" bcc
		    BCC3 = SubBCC, SubBCC0 = BCC1,
		    BCCs2 = BCCs1,
		    Count1 = Count0
		)
	    ;
		Min3 is min(Min2, YVal),
		Count1 = Count0,
		AP3 = AP1,
		BCC3 = BCC1,
		BCCs3 = BCCs1,
	    	Id3 = Id2
	    )
	).



%----------------------------------------------------------------------
% Strongly connected components
%
% Produces a list of strongly connected components of the graph,
% each represented as a list of nodes in the component.
% This is Tarjan's Algorithm, taken from Sedgewick, Algorithms.
% The algorithm is linear.
% I have introduced an additional boolean array Done[] to mark that
% a node is already in a s.c.c and can be ignored. In the original,
% this is achieved by overwriting the Id in the Val[] array with a
% big number.
%----------------------------------------------------------------------

:- export strong_components/2.
:- comment(strong_components/2, [
    summary:"Computes the strongly connected components of a graph",
    amode:(strong_components(+,-) is det),
    args:["Graph":"a graph structure",
    	"StrongComponents":"list of lists of integer node numbers"],
    see_also:[],
    desc:html("<P>
    Computes the strongly connected components, i.e. maximal subsets
    of the graph's nodes in which all nodes are mutually accessible.
    The implementation essentially uses Tarjan's algorithm with a
    complexity of O(Nnodes + Nedges).
    </P>")]).

strong_components(G, SC) :-
	G = graph with [size:MaxNode],
	dim(Val, [MaxNode]),
	dim(Done, [MaxNode]),
	(
	    for(Node, 1, MaxNode),	% iterate over the roots
	    fromto(0, Id0, Id1, _),
	    fromto([], SC0, SC1, SC),
	    param(G, Val, Done)
	do
	    Seen is Val[Node],
	    ( var(Seen) ->
		scc_visit(G, Val, Done, Node, Id0, Id1, _, [], [], SC0, SC1)
	    ;
	    	Id0 = Id1, SC0 = SC1
	    )
	).

    scc_visit(G, Val, Done, Node, Id0, Id4, Min, Stack0, Stack4, SC0, SC4) :-
	Id is Id0 + 1,
	Id is Val[Node],
%	node_to_nodename(G, Name, Node), writeln(node(Id:Name)),
	Ys is G[adj of graph, Node],
	(
	    foreach(e(_,Y,_), Ys),
	    fromto(Id, Id2, Id3, Id4),
	    fromto(Id, Min2, Min3, Min),
	    fromto(SC0, SC1, SC2, SC3),
	    fromto([Node|Stack0], Stack1, Stack2, Stack3),
	    param(G, Val, Done)
	do
	    YVal is Val[Y],
	    YDone is Done[Y],
	    ( var(YVal) ->
	    	scc_visit(G, Val, Done, Y, Id2, Id3, YMin, Stack1, Stack2, SC1, SC2),
%		writeln(result(visit(Y) is YMin)),
		Min3 is min(Min2, YMin)
	    ; var(YDone) ->		% not yet part of a scc
		Min3 is min(Min2, YVal),
		SC2 = SC1,
		Stack2 = Stack1,
	    	Id3 = Id2
	    ;				% otherwise ignore
		Min3 = Min2,
		SC2 = SC1,
		Stack2 = Stack1,
	    	Id3 = Id2
	    )
	),
	( Min == Id ->
	    pop_until_and_mark(Stack3, Node, Done, SC, Stack4),
	    SC4 = [SC|SC3]
	;
	    Stack4 = Stack3,
	    SC4 = SC3
	).

    % pop_until_and_mark(+ABCDE, +C, ?MarkArray, -ABC, -DE)
    % ABC is the front of list ABCDE up to the first occurrence of C.

    pop_until_and_mark([X|Xs], Y, Done, [X|Hs], Ts) :-
	arg(X, Done, true),
    	( X == Y ->
	    Ts = Xs, Hs = []
	;
	    pop_until_and_mark(Xs, Y, Done, Hs, Ts)
	).


%----------------------------------------------------------------------
% Single-Source Shortest Paths
% Computes a shortest path from a given node to every reachable node.
%----------------------------------------------------------------------


:- export shortest_paths/4.
:- comment(shortest_paths/4, [
    summary:"Computes one shortest path from a single source to every reachable node",
    amode:(shortest_paths(+,+,+,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number (integer)",
    	"Paths":"array of Length-EdgeList structures" ],
    see_also:[
%	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7,
	possible_path/7
    ],
    eg:"
    ?- sample_graph(G), shortest_paths(G, 0, 1, P).
    P = [](2 - [e(2, 1, 1), e(1, 2, 1)], 1 - [e(1, 2, 1)], ...)
    ",
    desc:html("<P>
    Computes one shortest path from the single source node SourceNode
    to every node which is reachable from it. In case of multiple
    shortest paths with the same length, an arbitrary one is returned.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number, and the
    numeric type (integer, float, etc) must be the same in all edges.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    The results are returned as an array ranging over all node numbers.
    For unreachable nodes the array element is uninstantiated.
    For reachable nodes, the element contains a Length-EdgeList structure
    where Length is the length of the shortest path and EdgeList is that
    path (or one of them) in reverse order, i.e. starting with the edge
    reaching the target and ending with the edge starting from SourceNode.
    </P>")]).

shortest_paths(G, DistanceArg, SourceNode, Results) :-
	G = graph with [size:MaxNode,adj:AdjLists],
	dim(Results, [MaxNode]),
	arg(SourceNode, Results, 0-[]),
	heap_create(MaxNode, PQ),
	arg(SourceNode, AdjLists, InitialEdges),
	add_edges_to_heap(DistanceArg, 0, InitialEdges, [], Results, PQ),
	sp_visit(AdjLists, DistanceArg, Results, PQ).

    sp_visit(AdjLists, DistanceArg, Results, PQ) :-
    	( heap_delete_min(PQ, Distance, Node, Path) ->
	    arg(Node, Results, Seen),
	    ( nonvar(Seen) ->		% longer or same length
		% shouldn't happen since we only add edges to unseen nodes
		% sp_visit(AdjLists, DistanceArg, Results, PQ)
		abort
	    ;
		Seen = Distance-Path,	% found shortest path to node
		arg(Node, AdjLists, EdgesFromNode),
		add_edges_to_heap(DistanceArg, Distance, EdgesFromNode, Path, Results, PQ),
		sp_visit(AdjLists, DistanceArg, Results, PQ)
	    )
	;
	    true		% we're done
	).


:- export single_pair_shortest_path/5.
:- comment(single_pair_shortest_path/5, [
    summary:"Computes one shortest path from a source to a sink node",
    amode:(single_pair_shortest_path(+,+,+,+,-) is semidet),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number (integer)",
    	"SinkNode":"sink node number (integer)",
    	"Path":"Length-EdgeList structure" ],
    see_also:[
	shortest_paths/4,
%	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7,
	possible_path/7
    ],
    fail_if:"There is no path from SourceNode to SinkNode",
    eg:"
    ?- sample_graph(G), single_pair_shortest_path(G, 0, 1, 3, P).
    P = 2 - [e(2, 3, 1), e(1, 2, 1)]
    ",
    desc:html("<P>
    Computes one shortest path from SourceNode to SinkNode.
    Fails if there is no path at all.  In case of multiple
    shortest paths with the same length, an arbitrary one is returned.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number, and the
    numeric type (integer, float, etc) must be the same in all edges.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    The shortest path is returned as a Length-EdgeList structure
    where Length is the length of the shortest path and EdgeList is that
    path (or one of them) in reverse order, i.e. starting with the edge
    reaching SinkNode and ending with the edge starting from SourceNode.
    </P>")]).

single_pair_shortest_path(G, DistanceArg, SourceNode, SinkNode, Path) :-
	G = graph with [size:MaxNode,adj:AdjLists],
	dim(Results, [MaxNode]),
	arg(SourceNode, Results, 0-[]),
	heap_create(MaxNode, PQ),
	arg(SourceNode, AdjLists, InitialEdges),
	add_edges_to_heap(DistanceArg, 0, InitialEdges, [], Results, PQ),
	spsp_visit(AdjLists, DistanceArg, SinkNode, Results, PQ),
	arg(SinkNode, Results, Path0),
	nonvar(Path0),	% fail if no path
	Path = Path0.	% then unify result

    spsp_visit(AdjLists, DistanceArg, SinkNode, Results, PQ) :-
    	( heap_delete_min(PQ, Distance, Node, Path) ->
	    arg(Node, Results, Seen),
	    ( nonvar(Seen) ->		% longer or same length
		% shouldn't happen since we only add edges to unseen nodes
		% spsp_visit(AdjLists, DistanceArg, SinkNode, Results, PQ)
		abort
	    ;
		Seen = Distance-Path,	% found shortest path to node
		arg(Node, AdjLists, EdgesFromNode),
		( Node == SinkNode ->
		    true		%  found shortest path to sink: stop
		;
		    add_edges_to_heap(DistanceArg, Distance, EdgesFromNode, Path, Results, PQ),
		    spsp_visit(AdjLists, DistanceArg, SinkNode, Results, PQ)
		)
	    )
	;
	    true		% we're done
	).


% Add paths to so far unseen nodes
% Replace old path to a node if strictly better
% Ignore alternative edges of the same length
add_edges_to_heap(DistanceArg, Distance, EdgeList, Path, Results, PQ) :-
	(
	    foreach(Edge,EdgeList),
	    param(Distance,Path,DistanceArg,Results,PQ)
	do
	    Edge = e(_,Node,Data),
	    arg(Node, Results, Seen),
	    ( var(Seen) ->
		DistanceToNode is Distance + xarg(DistanceArg, Data),
		heap_update_if_smaller(PQ, DistanceToNode, Node, [Edge|Path])
	    ;
		true
	    )
	).



    xarg(-1, _T, A) ?- !, A=1.
    xarg(0, T, A) ?- !, A=T.
    xarg(N, T, A) :- arg(N, T, A).



%----------------------------------------------------------------------
% Single-Source All Shortest Paths
% Computes all (not just one) shortest paths from a given node
% to every reachable node.
%----------------------------------------------------------------------


:- comment(all_short_paths_as_edges/6, [
    summary:"Computes all shortest paths from a single source to every reachable node",
    amode:(all_short_paths_as_edges(+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Lengths":"array of numbers (minimum path lengths)",
    	"Predecessors":"array of lists of e/3 edge structures" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
%	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7
	],
    eg:"
    ?- sample_graph(G), all_short_paths_as_edges(G, 0, 1, 0, L, E).
    L = [](0, 1, 2, 3, 2, 1, 1, _326, _327, 2, 3, 3, 3)
    E = []([], [e(1, 2, 1)], [e(7, 3, 1)], [e(5, 4, 1)],
	   [e(7, 5, 1), e(6, 5, 1)], [e(1, 6, 1)], [e(1, 7, 1)],
	   _342, _343, [e(7, 10, 1)], [e(10, 11, 1)], [e(10, 12, 1)],
	   [e(10, 13, 1)])
    ",
    desc:html("<P>
    Computes all shortest paths from the single source node SourceNode
    to every sink node which is reachable from it. The result is returned
    in the form of the Predecessors array which contains all relevant edges.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    Tolerance should be zero in order to find only the shortest paths. 
    If Tolerance is greater than zero, then all paths that are within
    this tolerance of the shortest path length will be found.
</P><P>
    The result is returned in the form of two arrays, whose indices range
    over the possible sink nodes.  The Lengths array indicates the length
    of a shortest path from SourceNode to the corresponding sink node.
    The Predecessors array is an array of edge lists, each list containing
    the alternative edges that are part of a shortest path from SourceNode
    to the corresponding sink node.
</P><P>
    If there is no path from SourceNode to a sink node J, then both
    Lengths[J] and Predecessors[J] are uninstantiated. Otherwise,
    Lengths[J] contains the length of a shortest path from SourceNode to J.
    Predecessors[J] is contains a list of alternative edges that lead from
    some predecessor node to J in a shortest path from SourceNode to J.
    Predecessors[SourceNode] is always the empty list [].
</P>
<H4>Assembling Actual Paths</H4>
<P>
    To generate an actual path from the Predecessors array, start from the
    sink node J, select one of the alternative edges in Predecessors[J]
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the parameters, the following 3 cases can occur:
    <OL>
    <LI>Tolerance is zero, and Graph did not contain zero-length edges: in this
    case, SubGraph is cycle-free and shortest paths can be found by simply
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Tolerance is zero, and Graph did contain zero-length edges: in this case,
    SubGraph may contain (zero-length) cycles which one may want to exclude
    when constructing paths.
    <LI>Tolerance is nonzero:  in this case, SubGraph may contain
    cycles (of maximum length Tolerance).  Moreover, it may be
    possible to use the edges in SubGraph to construct cycle-free paths
    whose total length is greater than the shortest path length plus
    the tolerance.  These may need to be excluded explicitly.
    </OL>
    The possible_path/7 predicate implements this path construction and
    does the necesssary checks to exclude cycles and overly long paths.
    </P>")]).

:- export all_short_paths_as_edges/6.
all_short_paths_as_edges(G, DistanceArg, SourceNode, Tolerance, Lengths, Incoming) :-
	common_all_short_paths(G, DistanceArg, SourceNode, 0, Tolerance, Lengths, Incoming).



:- comment(all_short_paths_as_graph/6, [
    summary:"Computes all shortest paths from a single source in form of a subgraph",
    amode:(all_short_paths_as_graph(+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Lengths":"array of numbers (minimum path lengths)",
    	"SubGraph":"a graph structure" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
%	all_short_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7,
	graph_get_incoming_edges/3,
	graph_set_nodenames/2
	],
    eg:"
    ?- sample_graph(G), all_short_paths_as_graph(G, 0, 1, 0, L, E).
    G = graph(13, []([e(1, 6, 1), e(1, 2, 1), e(1, 7, 1)], [], ...)
    L = [](0, 1, 2, 3, 2, 1, 1, _326, _327, 2, 3, 3, 3)
    SG = graph(13, []([e(1, 7, 1), e(1, 6, 1), e(1, 2, 1)], [], ...)
    Yes (0.00s cpu)
    ",
    desc:html("<P>
    Computes all shortest paths from the single source node SourceNode
    to every sink node which is reachable from it. The result is returned
    in the form of a sub-graph of the input graph, which contains all
    relevant edges.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    Tolerance should be zero in order to find only the shortest paths. 
    If Tolerance is greater than zero, then all paths that are within
    this tolerance of the shortest path length will be found.
</P><P>
    The result is returned in the form of SubGraph, which is a
    sub-graph of the input Graph, containing the same nodes, but only
    those edges that are needed to construct the shortest paths for
    the given parameters.  SubGraph does not inherit the nodename
    information from Graph, this can be set explicitly if required.
</P><P>
    In addition, a Lengths array is returned, whose entries indicate
    the length of a shortest path from SourceNode to the corresponding
    sink node.  If there is no path from SourceNode to a sink node J,
    then Lengths[J] is uninstantiated.
</P>
<H4>Properties of the resulting SubGraph</H4>
<P>
    To generate an actual path from the resulting SubGraph, start from the
    sink node J, select one of its incoming edges (graph_get_incoming_edges/3)
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the parameters, the following 3 cases can occur:
    <OL>
    <LI>Tolerance is zero, and Graph did not contain zero-length edges: in this
    case, SubGraph is cycle-free and shortest paths can be found by simply
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Tolerance is zero, and Graph did contain zero-length edges: in this case,
    SubGraph may contain (zero-length) cycles which one may want to exclude
    when constructing paths.
    <LI>Tolerance is nonzero:  in this case, SubGraph may contain
    cycles (of maximum length Tolerance).  Moreover, it may be
    possible to use the edges in SubGraph to construct cycle-free paths
    whose total length is greater than the shortest path length plus
    the tolerance.  These may need to be excluded explicitly.
    </OL>
    </P>")]).

:- export all_short_paths_as_graph/6.
all_short_paths_as_graph(G, DistanceArg, SourceNode, Tolerance, Lengths, ResultGraph) :-
	G = graph with [size:MaxNode],
	common_all_short_paths(G, DistanceArg, SourceNode, 0, Tolerance, Lengths, Incoming),
	make_graph_from_incoming_edges(MaxNode, Incoming, ResultGraph).



:- export single_pair_short_path/6.
:- comment(single_pair_short_path/6, [
    summary:"Computes short paths from a source to a sink node",
    amode:(single_pair_short_path(+,+,+,+,+,-) is nondet),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number (integer)",
    	"SinkNode":"sink node number (integer)",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Path":"Length-EdgeList structure" ],
    see_also:[
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
%	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7,
	possible_path/7
    ],
    fail_if:"There is no path from SourceNode to SinkNode",
    eg:"
    ?- sample_graph(G), single_pair_short_path(G, 0, 1, 3, P).
    P = 2 - [e(2, 3, 1), e(1, 2, 1)]
    ",
    desc:html("<P>
    Computes shortest (or sufficiently short) paths from SourceNode to
    SinkNode. Alternative paths are generated on backtracking.
    Fails if there is no path at all.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    If Tolerance is given as zero, all paths returned will have the same
    length and will be shortest paths from SourceNode to SinkNode.
    If Tolerance is nonzero, additional paths will be returned with
    lengths up to the length of the shortest path plus the tolerance.
    Note that the solutions are not generated in any specific order.
</P><P>
    A resulting path is returned as a Length-EdgeList structure where
    Length is the length of the path and EdgeList is the path in
    reverse order, i.e. starting with the edge reaching SinkNode and
    ending with the edge starting from SourceNode.
    </P>")]).

:- export single_pair_short_path/6.
single_pair_short_path(G, DistanceArg, SourceNode, SinkNode, Tolerance, Path) :-
	common_all_short_paths(G, DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Predecessors),
	possible_path(DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Predecessors, Path).



:- comment(single_pair_all_short_paths_as_graph/7, [
    summary:"Computes all shortest paths from source to sink in form of a subgraph",
    amode:(single_pair_all_short_paths_as_graph(+,+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
    	"SinkNode":"sink node number (integer)",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Length":"a number (minimum path length)",
    	"SubGraph":"a graph structure" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	single_pair_short_path/6,
%	single_pair_all_short_paths_as_graph/7,
	graph_get_incoming_edges/3,
	graph_set_nodenames/2
	],
    eg:"
    ?- sample_graph(G),
       single_pair_all_short_paths_as_graph(G, 0, 1, 5, 0, L, E).
    G = graph(13, []([e(1, 6, 1), e(1, 2, 1), e(1, 7, 1)], [], ...)
    L = 2
    SG = graph(13, []([e(1, 6, 1), e(1, 7, 1)], [], ...)
    ",
    desc:html("<P>
    Computes all shortest paths from source node SourceNode to sink
    node SinkNode.  The result is returned in the form of a sub-graph
    of the input graph, which contains all relevant edges.
    If there is no path, the predicate fails.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    Tolerance should be zero in order to find only the shortest paths. 
    If Tolerance is greater than zero, then all paths that are within
    this tolerance of the shortest path length will be found.
</P><P>
    The result is returned in the form of SubGraph, which is a
    sub-graph of the input Graph, containing the same nodes, but only
    those edges that are needed to construct the shortest paths for
    the given parameters.  SubGraph does not inherit the nodename
    information from Graph, this can be set explicitly if required.
</P><P>
    In addition, the Length of the shortest path from source to sink
    is returned.
</P>
<H4>Properties of the resulting SubGraph</H4>
<P>
    To generate an actual path from the resulting SubGraph, start from the
    sink node J, select one of its incoming edges (graph_get_incoming_edges/3)
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the parameters, the following 3 cases can occur:
    <OL>
    <LI>Tolerance is zero, and Graph did not contain zero-length edges: in this
    case, SubGraph is cycle-free and shortest paths can be found by simply
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Tolerance is zero, and Graph did contain zero-length edges: in this case,
    SubGraph may contain (zero-length) cycles which one may want to exclude
    when constructing paths.
    <LI>Tolerance is nonzero:  in this case, SubGraph may contain
    cycles (of maximum length Tolerance).  Moreover, it may be
    possible to use the edges in SubGraph to construct cycle-free paths
    whose total length is greater than the shortest path length plus
    the tolerance.  These may need to be excluded explicitly.
    </OL>
    </P>")]).

:- export single_pair_all_short_paths_as_graph/7.
single_pair_all_short_paths_as_graph(G, DistanceArg, SourceNode, SinkNode, Tolerance, Length, ResultGraph) :-
	G = graph with [size:MaxNode],
	common_all_short_paths(G, DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Incoming),
	arg(SinkNode, Lengths, Length),
	make_graph_from_incoming_edges_single_pair(MaxNode, Incoming, SourceNode, SinkNode, ResultGraph).



% The common code for all "all_short_paths" predicates
% - if SinkNode is set to zero we compute all sinks, otherwise we stop there
% - Tolerance is zero for proper shortest paths

common_all_short_paths(G, DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Incoming) :-
	G = graph with [size:MaxNode,adj:AdjLists],
	dim(Lengths, [MaxNode]),
	dim(Incoming, [MaxNode]),
	heap_create(MaxNode, PQ),
	TypedZero is 0*Tolerance,	% inherit numeric type from Tolerance
	arg(SourceNode, Lengths, TypedZero),
	arg(SourceNode, Incoming, []),
	arg(SourceNode, AdjLists, InitialEdges),
	process_adjacent_edges(DistanceArg, TypedZero, InitialEdges, Tolerance, Lengths, Incoming, PQ),
	asp_visit(AdjLists, DistanceArg, Tolerance, SinkNode, 1.0Inf, Lengths, Incoming, PQ).

    asp_visit(AdjLists, DistanceArg, Tolerance, SinkNode, CutOff, Lengths, Incoming, PQ) :-
    	( heap_delete_min(PQ, Distance, Node, Edges), Distance =< CutOff ->
%	    writeln(heap_delete_min(Distance, Node, Edges)),
	    arg(Node, Lengths, ShortestDistanceToNode),
	    ( var(ShortestDistanceToNode) ->
		% add the first bunch of edges, including the optimal one(s)
		ShortestDistanceToNode = Distance,
		( Tolerance > 0 ->
		    % filter out those that are too long
		    (
			foreach(Edge,Edges),
			fromto(NewEdges, E1,E2,[]),
			param(Lengths,Tolerance,DistanceArg,Node,ShortestDistanceToNode)
		    do
			Edge = e(StartNode,ToNode,Data),
			( ToNode == Node -> true ; abort ),
			arg(StartNode, Lengths, ShortestDistanceToStartNode),
			DistanceViaEdge is ShortestDistanceToStartNode + xarg(DistanceArg, Data),
			( DistanceViaEdge =< ShortestDistanceToNode + Tolerance ->
			    E1 = [Edge|E2]
			;
			    E1 = E2
			)
		    ),
		    arg(Node, Incoming, NewEdges)
		;
		    arg(Node, Incoming, Edges)
		),
		% If SinkNode reached, we still have to keep looking for
		% alternative paths, but we can compute a cutoff distance now
		( Node == SinkNode ->
		    CutOff1 is ShortestDistanceToNode + Tolerance
		;
		    CutOff1 = CutOff
		),
		arg(Node, AdjLists, EdgesFromNode),
		process_adjacent_edges(DistanceArg, Distance, EdgesFromNode, Tolerance, Lengths, Incoming, PQ)
	    ;
		printf(error, "Unexpected heap entry for known node %w%n", [Node]),
		abort
	    ),
	    asp_visit(AdjLists, DistanceArg, Tolerance, SinkNode, CutOff1, Lengths, Incoming, PQ)
	;
	    true		% we're done
	).


% - Add edges to heap which lead to nodes whose shortest path is not yet known
%   in case they are within the tolerance of the shortest found so far
% - Add alternative edges of similar length to already found results
process_adjacent_edges(_DistanceArg, _Distance, [], _Tolerance, _Lengths, _Incoming, _PQ).
process_adjacent_edges(DistanceArg, Distance, [Edge|Edges], Tolerance, Lengths, Incoming, PQ) :-
	Edge = e(_,Node,Data),
	arg(Node, Lengths, ShortestDistanceToNode),
	DistanceToNode is Distance + xarg(DistanceArg, Data),
	( var(ShortestDistanceToNode) ->
	    % no shortest path to this Node found yet
	    ( heap_access(PQ, Node, BestDistance, OldEdges) ->
		% heap already contains paths to this Node
		( DistanceToNode > BestDistance + Tolerance ->
		    % ignore the new path if it is longer
		    true
		; DistanceToNode < BestDistance ->
		    % the new path is better than what we have in the heap
		    ( Tolerance > 0 ->
			% add it to the ones we have already have (since we
			% are updating the best distance, some old edges may
			% now be beyond the tolerance and could be filtered
			% out now, but better be lazy...)
			heap_update(PQ, Node, DistanceToNode, [Edge|OldEdges])
		    ;
			% replace old edges by the better one
			heap_update(PQ, Node, DistanceToNode, [Edge])
		    )
		;
		    % same or similar length: add to existing heap entry
		    heap_update_datum(PQ, Node, [Edge|OldEdges])
		)
	    ;
		% first path to this node: make a new heap entry
		heap_insert(PQ, DistanceToNode, Node, [Edge])
	    )

	; DistanceToNode =< ShortestDistanceToNode + Tolerance ->
	    % we already have the shortest path(s), but this new one is
	    % within the tolerance, so we add it to the results.
	    % Since it is (possibly) suboptimal, it should be added to the end
	    % rather than the beginning, but that wouldn't be constant time.
	    arg(Node, Incoming, OldEdges),
	    setarg(Node, Incoming, [Edge|OldEdges])
	;
	    true
	),
	process_adjacent_edges(DistanceArg, Distance, Edges, Tolerance, Lengths, Incoming, PQ).



:- export possible_path/6.
:- deprecated(possible_path/6, "Use possible_path/7").
:- comment(possible_path/6, [
    summary:"Computes an actual path from a predecessors array",
    amode:(possible_path(+,+,+,+,+,-) is nondet),
    args:[
    	"SourceNode":"source node number",
    	"SinkNode":"sink node number",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Lengths":"array of numbers",
    	"Predecessors":"array of edge lists",
    	"Path":"Length-EdgeList structure" ],
    see_also:[
	possible_path/7
    ],
    desc:html("<P>
    This predicate is deprecated, it is equivalent to possible_path/7 with
    a DistanceArg of 0.
    </P>")]).

possible_path(SourceNode, SinkNode, Tolerance, Lengths, Predecessors, Result) :-
	possible_path(0, SourceNode, SinkNode, Tolerance, Lengths, Predecessors, Result).

:- export possible_path/7.
:- comment(possible_path/7, [
    summary:"Computes an actual path from a predecessors array",
    amode:(possible_path(+,+,+,+,+,+,-) is nondet),
    args:[
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number",
    	"SinkNode":"sink node number",
    	"Tolerance":"tolerable deviation from minimal length (non-negative number)",
    	"Lengths":"array of numbers",
    	"Predecessors":"array of edge lists",
    	"Path":"Length-EdgeList structure" ],
    see_also:[
	all_short_paths_as_edges/6
    ],
    fail_if:"There is no path to SinkNode",
    eg:"
    single_pair_shortest_path(Graph, Source, Sink, Path) :-
    	all_short_paths_as_edges(Graph, 0, Source, 0, Lengths, Preds),
    	possible_path(0, Source, Sink, 0, Lengths, Preds, Path).
    ",
    desc:html("<P>
    This predicate complements the all_short_paths_as_edges/6 predicate.
    The intended usage is that all_short_paths_as_edges/6 is used to
    precompute shortest path information from a single source node
    to all possible sink nodes, and possible_path/7 uses this
    information to enumerate actual paths to a specific sink node.
    If paths from one source to several sinks are needed, it is more
    efficient to use one call to all_short_paths_as_edges/6 and several
    calls to possible_path/7, than to use several calls to
    single_pair_all_short_paths/7.
    </P><P>
    Note that the Lengths and Predecessors arrays must have been computed
    with the same settings for DistanceArg, SourceNode and Tolerance that are
    given to possible_path/7, otherwise errors or missing paths will occur.
    </P>")]).

% possible_path(+DistanceArg, +SourceNode, +SinkNode, +Tolerance, +Lengths, +Predecessors, -Path)

possible_path(DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Predecessors, Result) :-
	arg(SinkNode, Predecessors, Edges),
	nonvar(Edges),			% fail if no path to SinkNode
	arg(SinkNode, Lengths, ShortestDistance),
	nonvar(ShortestDistance),	% fail if no path to SinkNode
	arg(SourceNode, Lengths, SourceDistance),
	( SourceDistance > 0 ->
	    printf(error, "Illegal SourceNode %d specified%n", [SourceNode]),
	    abort
	;
	    true
	),
	functor(Predecessors, F, NNodes),
	functor(Seen, F, NNodes),	% for loop checking
	arg(SinkNode, Seen, yes),
	( not Tolerance > 0 ->
	    % The length test is not needed for zero tolerance.
	    % Disable it because rounding errors may cause
	    % the test to fail with valid paths. 
	    MaxLength = 1.0Inf
	;
	    % The length test can fail because we compare lengths computed during
	    % the shortest path algorithm and lengths added up here. Use breals
	    % to achieve an optimistic test and avoid failures if possible.
	    % Note: this doesn't work reliably, some paths are still cut off...
	    MaxLength is breal(ShortestDistance) + breal(Tolerance)
	),
	BZero is breal(0),
	(
	    fromto(SinkNode, To, From, SourceNode),
	    fromto(Path, Path1, Path0, []),
	    fromto(0, Len0, Len1, Length),	% length with correct type
	    fromto(BZero, BLen0, BLen1, _),	% breal length for pruning
	    param(Predecessors,Seen,Lengths,MaxLength,DistanceArg)
	do
	    arg(To, Predecessors, Edges),
	    ( nonvar(Edges), Edges = [_|_] ->
		member(Edge, Edges),
		( Edge = e(From, To, Data) ->
		    xarg(DistanceArg, Data, Dist),
		    % cut cycles (needed for zero-length edges or nonzero tolerance)
		    arg(From, Seen, FromSeen),
		    var(FromSeen),
		    FromSeen = yes,
		    % cut edges that lead to paths that are too long
		    Len1 is Len0 + Dist,
		    BLen1 is BLen0 + Dist,
%		    Lengths[From] + Len1 =< MaxLength,
		    % use notnot and breals to fail only when sure
		    not not  Lengths[From] + BLen1 =< MaxLength,
		    Path1 = [Edge|Path0]
		;
		    printf(error, "Corrupt egde to %w: %w%n", [To,Edge]),
		    abort
		)
	    ;
		printf(error, "Node %w has no precedessors!%n"
			"Mismatched all_short_paths_as_edges/6 and possible_path/7?%n",
		    [To]),
		abort
	    )
	),
	% unify after loop, otherwise loop termination is compromised
	Result = Length-Path.


%----------------------------------------------------------------------
% Shortest Paths with negative distances (Bellman/Ford)
%----------------------------------------------------------------------

:- export single_pair_shortest_path_bellman_ford/5.
:- comment(single_pair_shortest_path_bellman_ford/5, [
    summary:"Computes one shortest path from a source to a sink node (allowing negative distances)",
    amode:(single_pair_shortest_path_bellman_ford(+,+,+,+,-) is semidet),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number",
    	"SinkNode":"sink node number",
    	"Path":"Length-EdgeList structure" ],
    see_also:[shortest_paths_bellman_ford/4,single_pair_shortest_path/5],
    fail_if:"There is no path from SourceNode to SinkNode",
    eg:"
    ?- sample_graph(G), single_pair_shortest_path_bellman_ford(G, 0, 1, 3, P).
    P = 2 - [e(2, 3, 1), e(1, 2, 1)]
    ",
    desc:html("<P>
    Computes one shortest path from SourceNode to SinkNode.
    Fails if there is no path at all.  In case of multiple
    shortest paths with the same length, an arbitrary one is returned.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. As opposed to the other
    shortest path algorithms, the Bellman-Ford algorithm can handle
    negative edge lengths, however, the implementation has currently
    no check for negative cycles and will not terminate in that case.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    The shortest path is returned as a Length-EdgeList structure
    where Length is the length of the shortest path and EdgeList is that
    path (or one of them) in reverse order, i.e. starting with the edge
    reaching SinkNode and ending with the edge starting from SourceNode.
    </P>")]).

:- export single_pair_shortest_path_bellman_ford/5.

single_pair_shortest_path_bellman_ford(Graph, DistanceArg, Source, Dest, Path) :-
	graph_get_maxnode(Graph, MaxNode),
	dim(PathsArray, [MaxNode]),
	dim(FringeArray, [MaxNode]),
	(
	    foreacharg(Fringe, FringeArray)
	do
	    Fringe = 0
	),
	setarg(Source, FringeArray, 1),
	FringeList = [Source|Tail],
	arg(Source, PathsArray, 0-[]),
	single_pair_shortest_path_bellman_ford_loop(Graph, DistanceArg, FringeList, Tail, FringeArray, PathsArray, Dest, Path).

	
single_pair_shortest_path_bellman_ford_loop(_Graph, _DistanceArg, Tail, Tail, _FringeArray, PathsArray, Dest, Path) :-
	var(Tail),
	!,
	arg(Dest, PathsArray, Path0),
	nonvar(Path0),	% fail if no path
	Path = Path0.	% then unify result

single_pair_shortest_path_bellman_ford_loop(Graph, DistanceArg, List, Tail, FringeArray, PathsArray, Dest, Path) :-
	List = [I|NewList],
	arg(I, PathsArray, Ci-Pi),
	setarg(I, FringeArray, 0),
	graph_get_adjacent_edges(Graph, I, EdgeList),
	(
	    foreach(Edge, EdgeList),
	    fromto(Tail, Tail1, Tail2, NewTail),
	    param(DistanceArg, PathsArray, FringeArray, I, Ci, Pi)
	do
	    Edge = e(I, J, Data),
	    Cj is Ci + xarg(DistanceArg, Data),
	    arg(J, PathsArray, PathJ),
	    ( var(PathJ) ->
		  PathJ = Cj-[Edge|Pi],
		  ( arg(J, FringeArray, 0) ->
			%% Need to add to Fringe List
			setarg(J, FringeArray, 1),
			Tail1 = [J|Tail2]
		  ;
			Tail1 = Tail2
		  )
	    ;
	    	  PathJ = OldCj-_OldPj,
		  ( Cj < OldCj ->
		      setarg(J, PathsArray, Cj-[Edge|Pi]),
		      ( arg(J, FringeArray, 0) ->
			    %% Need to add to Fringe List
			    setarg(J, FringeArray, 1),
			    Tail1 = [J|Tail2]
		      ;
			    Tail1 = Tail2
		      )
		  ;			 
		      Tail1 = Tail2
		  )
	    )
	),
	single_pair_shortest_path_bellman_ford_loop(Graph, DistanceArg, NewList, NewTail, FringeArray, PathsArray, Dest, Path).
	



:- export shortest_paths_bellman_ford/4.
:- comment(shortest_paths_bellman_ford/4, [
    summary:"Computes one shortest path from a single source to every reachable node (allowing negative distances)",
    amode:(shortest_paths_bellman_ford(+,+,+,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number",
    	"Paths":"array of Length-EdgeList structures" ],
    see_also:[single_pair_shortest_path_bellman_ford/5,shortest_paths/4],
    eg:"
    ?- sample_graph(G), shortest_paths_bellman_ford(G, 0, 1, P).
    P = [](2 - [e(2, 1, 1), e(1, 2, 1)], 1 - [e(1, 2, 1)], ...)
    ",
    desc:html("<P>
    Computes one shortest path from the single source node SourceNode
    to every node which is reachable from it. In case of multiple
    shortest paths with the same length, an arbitrary one is returned.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. As opposed to the other
    shortest path algorithms, the Bellman-Ford algorithm can handle
    negative edge lengths, however, the implementation has currently
    no check for negative cycles and will not terminate in that case.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    The results are returned as an array ranging over all node numbers.
    For unreachable nodes the array element is uninstantiated.
    For reachable nodes, the element contains a Length-EdgeList structure
    where Length is the length of the shortest path and EdgeList is that
    path (or one of them) in reverse order, i.e. starting with the edge
    reaching the target and ending with the edge starting from SourceNode.
    </P>")]).

:- export shortest_paths_bellman_ford/4.

%% COMPLEXITY O(M*N)
%% M : number of edges
%% N : number of nodes

shortest_paths_bellman_ford(Graph, DistanceArg, Source, PathsArray) :-
	graph_get_maxnode(Graph, MaxNode),
	dim(PathsArray, [MaxNode]),
	dim(FringeArray, [MaxNode]),
	(
	    foreacharg(Fringe, FringeArray)
	do
	    Fringe = 0
	),
	setarg(Source, FringeArray, 1),
	FringeList = [Source|Tail],
	setarg(Source, PathsArray, 0-[]),
	shortest_paths_bellman_ford_loop(Graph, DistanceArg, FringeList, Tail, FringeArray, PathsArray).

	
shortest_paths_bellman_ford_loop(_Graph, _DistanceArg, Tail, Tail, _FringeArray, _PathsArray) :-
	var(Tail),
	!.

shortest_paths_bellman_ford_loop(Graph, DistanceArg, List, Tail, FringeArray, PathsArray) :-
	List = [I|NewList],
	arg(I, PathsArray, Ci-Pi),
	setarg(I, FringeArray, 0),
	graph_get_adjacent_edges(Graph, I, EdgeList),
	(
	    foreach(Edge, EdgeList),
	    fromto(Tail, Tail1, Tail2, NewTail),
	    param(DistanceArg, PathsArray, FringeArray, I, Ci, Pi)
	do
	    Edge = e(I, J, Data),
	    Cj is Ci + xarg(DistanceArg, Data),
	    arg(J, PathsArray, OldCj-_OldPj),
	    ( var(OldCj) ->
		  setarg(J, PathsArray, Cj-[Edge|Pi]),
		  ( arg(J, FringeArray, 0) ->
			%% Need to add to Fringe List
			setarg(J, FringeArray, 1),
			Tail1 = [J|Tail2]
		  ;
			Tail1 = Tail2
		  )
	    ; Cj < OldCj ->
		  setarg(J, PathsArray, Cj-[Edge|Pi]),
		  ( arg(J, FringeArray, 0) ->
			%% Need to add to Fringe List
			setarg(J, FringeArray, 1),
			Tail1 = [J|Tail2]
		  ;
			Tail1 = Tail2
		  )
	    ;			 
		  Tail1 = Tail2
	    )
	),
	shortest_paths_bellman_ford_loop(Graph, DistanceArg, NewList, NewTail, FringeArray, PathsArray).


%----------------------------------------------------------------------
% Backward compatibility with release < 5.5
%----------------------------------------------------------------------

:- comment(all_shortest_paths/4, hidden).
:- export all_shortest_paths/4.
:- deprecated(all_shortest_paths/4, "Use all_short_paths_as_graph/6 or all_short_paths_as_edges/6 + possible_path/7").
all_shortest_paths(G, DistanceArg, SourceNode, Paths) :-
	all_short_paths_as_edges(G, DistanceArg, SourceNode, 0, Lengths, Predecessors),
	G = graph with [size:NNodes],
	dim(Paths, [NNodes]),
	(
	    for(Sink,1,NNodes),
	    param(DistanceArg,SourceNode,Lengths,Predecessors,Paths)
	do
	    findall(LP, possible_path(DistanceArg, SourceNode, Sink, 0, Lengths, Predecessors, LP), SinkPaths),
	    ( SinkPaths = [] -> true ; arg(Sink, Paths, SinkPaths) )
	).

:- comment(single_pair_all_shortest_paths/5, hidden).
:- export single_pair_all_shortest_paths/5.
:- deprecated(single_pair_all_shortest_paths/5, "Use all_short_paths_as_edges/6 + possible_path/7").
single_pair_all_shortest_paths(G, DistanceArg, SourceNode, SinkNode, Paths) :-
	all_short_paths_as_edges(G, DistanceArg, SourceNode, 0, Lengths, Predecessors),
	findall(LP, possible_path(DistanceArg, SourceNode, SinkNode, 0, Lengths, Predecessors, LP), Paths).

/* not actually in any official release
:- export single_pair_all_short_paths/6.
single_pair_all_short_paths(G, DistanceArg, SourceNode, SinkNode, Tolerance, Paths) :-
	all_short_paths_as_edges(G, DistanceArg, SourceNode, Tolerance, Lengths, Predecessors),
	findall(LP, possible_path(DistanceArg, SourceNode, SinkNode, Tolerance, Lengths, Predecessors, LP), Paths).
*/


%----------------------------------------------------------------------
% Minimum spanning tree
%----------------------------------------------------------------------

:- comment(minimum_spanning_tree/4, [
    summary:"Computes a minimum spanning tree and its weight",
    amode:(minimum_spanning_tree(+,+,-,-) is semidet),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
	"Tree":"a list of e/3 edge structures",
    	"TreeWeight":"sum of the tree's edge weights: number"],
    index:["spanning tree"],
    fail_if:"No spanning tree exists, i.e. the graph is not connected.",
    see_also:[minimum_spanning_forest/5],
    eg:"
    ?- sample_graph(G), minimum_spanning_tree(G, 0, T, W).
    T = [e(2, 10, 1), e(4, 8, 1), e(9, 2, 1), e(7, 3, 2), ...]
    W = 16
    ",
    desc:html("<P>
    Computes a minimum spanning tree for the given graph. A minimum
    spanning tree is a smallest subset of the graph's edges that still
    connects all the graph's nodes. Such a tree is not unique and of
    course exists only if the original graph is itself connected.
    However, all minimum spanning trees will have the same cost.
</P><P>
    The computed tree is returned in Tree, which is simply a list of
    the edges that form the tree. The TreeWeight is the total length
    of the tree's edges, according to DistanceArg.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number, and the
    numeric type (integer, float, etc) must be the same in all edges.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    The direction of the graph's edges is ignored by this predicate.
</P><P>
    The implementation uses Kruskal's algorithm which has a complexity
    of O(Nedges*log(Nedges)).
    </P>")]).


:- export minimum_spanning_tree/4.
minimum_spanning_tree(G, DistanceArg, Tree, TreeWeight) :-
	TreeSize is G[size of graph] - 1,
	minimum_spanning_forest(G, DistanceArg, Tree, TreeSize, TreeWeight).


:- comment(minimum_spanning_forest/5, [
    summary:"Computes a minimum spanning forest, its size and weight",
    amode:(minimum_spanning_forest(+,+,-,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
	"Forest":"a list of e/3 edge structures",
	"ForestSize":"the number of edges in the Forest list",
    	"ForestWeight":"sum of the forest's edge weights: number"],
    index:["spanning forest"],
    see_also:[minimum_spanning_tree/4],
    eg:"
    ?- sample_graph(G), minimum_spanning_forest(G, 0, T, S, W).
    T = [e(2, 10, 1), e(4, 8, 1), e(9, 2, 1), e(7, 3, 2), ...]
    S = 8
    W = 16
    ",
    desc:html("<P>
    Computes a minimum spanning forest for the given graph. A minimum
    spanning forest is a smallest subset of the graph's edges that still
    connects all the graph's connected components. Such a forest is not
    unique, but all minimum spanning forests will have the same cost.
    As opposed to a minimum spanning tree, a forest exists also if the
    original graph is not connnected. The forest will have the same
    number of connected components as the original graph.
</P><P>
    The computed forest is returned in Forest, which is simply a list of
    the edges that form the forest. The ForestSize is the number of
    edges that constitute the forest. The ForestWeight is the total
    length of the forest's edges, according to DistanceArg.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number, and the
    numeric type (integer, float, etc) must be the same in all edges.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    The direction of the graph's edges is ignored by this predicate.
</P><P>
    The implementation uses Kruskal's algorithm which has a complexity
    of O(Nedges*log(Nedges)).
    </P>")]).


:- export minimum_spanning_forest/5.
minimum_spanning_forest(G, DistanceArg, Forest, ForestSize, ForestWeight) :-
	edge_distance_arg(DistanceArg, EdgeDistanceArg),
	G = graph with [size:MaxNode],
	graph_get_all_edges(G, Edges),
	sort(EdgeDistanceArg, =<, Edges, EdgesByIncreasingWeight),
	init_union_find(MaxNode, Classes),
	TreeSize is MaxNode-1,
	(
	    fromto(EdgesByIncreasingWeight,[Edge|Edges],NextEdges,[]),
	    fromto(Forest,Res1,Res0,[]),
	    fromto(0,Count0,Count1,ForestSize),
	    fromto(0,Total0,Total1,ForestWeight),
	    param(Classes,DistanceArg,TreeSize)
	do
	    Edge = e(From,To,EdgeData),
	    find_class(Classes, From, FromClass),
	    find_class(Classes, To, ToClass),
	    ( FromClass = ToClass ->
	    	Res1 = Res0,
		Count1 = Count0,
		Total1 = Total0,
		NextEdges = Edges
	    ;
	    	Res1 = [Edge|Res0],
		Count1 is Count0+1,
		Total1 is Total0 + xarg(DistanceArg,EdgeData),
		union_classes(Classes, FromClass, ToClass),
		( Count1 < TreeSize ->
		    NextEdges = Edges	% continue
		;
		    NextEdges = []	% stop, we have a tree
		)
	    )
	).

    % edge_distance_arg(+DistanceArg,-EdgeDistanceArg)
    % Prepend a 3 (the EdgeData field index within e/3)
    % to the DistanceArg spec and take care of special cases
    :- mode edge_distance_arg(+,-).
    edge_distance_arg(N, _) :- var(N), !, fail.
    edge_distance_arg(-1, 3) :- !.	% don't care
    edge_distance_arg(0, 3) :- !.
    edge_distance_arg(N, [3,N]) :- integer(N).
    edge_distance_arg(L, [3|L]) :- L = [_|_].


%----------------------------------------------------------------------
% Union-find
%
% Used to group numbers 1..N into equivalence classes. Each class
% is represented by an arbitrary number from its class.
%
% We use path compression and weight balancing, so the complexity
% should be almost linear (Sedgewick).
%
% Data structure: an array 1..N. An element UF[I] can be
%
% Variable	node I is in its own class (initial state)
% integer J	node I is in the same class as node J
% root(W)	I represents a class with W members
%
% The array gets updated destructively on both union and find
% operations:  Find operations do path compression;
% Union operations overwrite one root to point to the other,
% and update the weight of the new common root.
%----------------------------------------------------------------------


init_union_find(N, UF) :-
	functor(UF, union_find, N).

find_class(UF, I, Root) :-
	arg(I, UF, Ri),
	( integer(Ri) ->
	    find_class(UF, Ri, Root),
	    setarg(I, UF, Root)		% path compression
	; % (var(Ri) ; Ri = root(_Weight) ) ->
	    Root = I
	).

union_classes(UF, I, J) :-
	arg(I, UF, Ri),
	arg(J, UF, Rj),
	( var(Ri) ->
	    ( var(Rj) ->		% make I the root
		arg(J, UF, I),
		arg(I, UF, root(2))
	    ; Rj = root(Wj) ->		% Wi = 1, make J the root
		W is Wj+1,
		arg(I, UF, J),
		setarg(J, UF, root(W))
	    ;
		find_class(UF, J, Jroot),
		union_classes(UF, I, Jroot)
	    )
	; Ri = root(Wi) ->
	    ( var(Rj) ->		% Wj = 1, make I the root
		W is Wi+1,
		arg(J, UF, I),
		setarg(I, UF, root(W))
	    ; Rj = root(Wj) ->
		W is Wi+Wj,
	    	( Wi > Wj ->		% make I the root
		    setarg(J, UF, I),
		    setarg(I, UF, root(W))
		;			% make J the root
		    setarg(I, UF, J),
		    setarg(J, UF, root(W))
		)
	    ;
		find_class(UF, J, Jroot),
		union_classes(UF, I, Jroot)
	    )
	;
	    find_class(UF, I, Iroot),
	    union_classes(UF, Iroot, J)
	).







% Maximal matching
:- comment(maximum_matching_hopcroft_karp/4, [
        summary:"Compute the maximum matching in a bipartite graph using Hopcroft and Karp's algorithm",
        amode:(maximum_matching_hopcroft_karp(+,++,++,-) is semidet),
        args:[
                 "G": "A directed bipartite graph, with all edges starting and ending in 'A' or 'B'",
                 "A": "List of nodes in one half of the graph",
                 "B": "List of nodes in the other half of the graph",
                 "MaximalM": "List of edges constituting the maximum matching"
             ],
        desc:html("<P>

        Computes the maximum matching in the given bipartite graph. A
        matching in a bipartite graph, is a set of edges from the
        graph such that no two edges are incident to the same node.  A
        maximum matching is a matching with the most edges possible.
        The may be more than one maximum matching, this predicate
        returns only one.

</P><P>

        The implementation uses Hopcroft and Karp's algorithm which
        has a complexity of O(Nedges*SQRT(Nnodes in A)).
       </P>"),
        fail_if:"Graph is not bipartite",
        eg:"",
        see_also:[],
        index:["matching", "bipartite matching"]]).

:-local struct(matching_graph(
                                 size,    % number of nodes
                                 from,    % array of source nodes
                                 to,      % array of destination nodes
                                 edge,    % array of original edges
                                 
                                 adj      % array[nodes] of adjacency
                                          % lists of edge ids which
                                          % are either incoming or outgoing
                             )
              ).
:-export maximum_matching_hopcroft_karp/4.
maximum_matching_hopcroft_karp(G,A,B,MaximalM):-
        G = graph{size:Size,adj:Adj},
        NewG = matching_graph{size:Size,
                              from:NewFrom,
                              to:NewTo,
                              edge:NewEdgeArray,
                              adj:NewAdj},
        % mark the nodes in A
        dim(InAB, [Size]),
        (foreach(Node,A), param(InAB) do arg(Node, InAB, a)),
        ((foreach(Node,B), param(InAB) do arg(Node, InAB, b)) ->
            true
        ;
            printf(error, "Nodes sets %w and %w are not disjoint%n", [A,B]),
            abort            
        ),
        % count the number of edges
        (foreacharg(AdjList,Adj), fromto(0,In,Out,NumEdges) do
            Out is In + length(AdjList)
        ),
        % Build matching graph structure
        dim(NewAdj, [Size]),
        dim(NewFrom, [NumEdges]),
        dim(NewTo, [NumEdges]),
        dim(NewEdgeArray, [NumEdges]),
        % initialise the adjacency lists
        (foreacharg([],NewAdj) do true),
        % populate the adjacency lists
        (foreacharg(OutEdges, Adj),
         count(SrcNode,1,_),
         fromto(1,IdIn,IdOut,_NumEdges),
         param(NewFrom,NewTo,NewEdgeArray,InAB,NewAdj) do
            arg(SrcNode,InAB,SrcIsInAB),
            ((var(SrcIsInAB),OutEdges\==[]) ->
		printf(error, "There are edges %w adjacent to node %w which is not specified as part of the bipartite partition%n", [OutEdges, SrcNode]),
		abort
            ; SrcIsInAB==b ->
                % Src not in A, hence edge is reversed
                IsReversed = true
            ;
                IsReversed = false
            ),
            % loop through outgoing edges
            (foreach(Edge, OutEdges),
             foreach(Id, NewOutEdges),
             count(Id,IdIn,TempIdOut),
             param(NewFrom, NewTo, NewEdgeArray,
                   IsReversed, InAB, SrcIsInAB, NewAdj) do
                Edge=e(Src,Dst,_Data),
                (IsReversed==true ->
                    arg(Id,NewFrom,Dst),
                    arg(Id,NewTo,Src)
                ;
                    arg(Id,NewFrom,Src),
                    arg(Id,NewTo,Dst)
                ),
                arg(Id,NewEdgeArray,Edge),
                (Dst\==Src ->
                    % need to add this edgeId to the adjacency list of
                    % "Dst" as well as "Src"
                    arg(Dst, NewAdj, OldDstEdges),
                    setarg(Dst, NewAdj, [Id|OldDstEdges])
                ;
                    true
                ),
                (arg(Dst,InAB,SrcIsInAB) ->
                    printf(error, "The graph is not bipartite%n",[]),
                    abort
                ;
                    true
                )
            ),
            % Add new "SrcNode" edges to the new adjacency list
            arg(SrcNode, NewAdj, OldOutEdges),
            append(OldOutEdges, NewOutEdges, FinalOutEdges),
            setarg(SrcNode, NewAdj, FinalOutEdges),
            IdOut is TempIdOut+1
        ),
        % perform greedy heuristic
        node_array(NewG,Free,true),
        (for(E,1,NumEdges), param(NewFrom,NewTo,NewG,Free) do
            arg(E, NewFrom, V),
            arg(E, NewTo, W),
            ((true is Free[V],true is Free[W]) ->
                setarg(V,Free,false),
                setarg(W,Free,false),
                graph_rev_edge(NewG,E)
            ;
                true
            )
        ),
        (foreach(V,A),
         fromto(FreeInA,In,Out,[]), param(Free) do
            (arg(V,Free,true)->
                In=[V|Out]
            ;
                In=Out
            )
        ),
        % define data structures
        edge_array(NewG, Useful, 0),
        node_array(NewG, Dist, 0),
        node_array(NewG, Reached, 0),
        % while there is an augmenting path
        (count(PhaseNumber,1,_),
         fromto(false,TerminateIn,TerminateOut,true),
         fromto(FreeInA,FreeInAIn,FreeInAOut,_),
         param(NewG, Free, Useful, Dist, Reached)
        do
             (bfs(NewG, FreeInAIn, Free, Useful, Dist, Reached, PhaseNumber) ->
                  % find maximal set and augment
                  node_array(NewG,Pred),
                  (foreach(V,FreeInAIn),
                   fromto([],ELIn,ELOut,EL),
                   param(NewG, Free, Useful, PhaseNumber, Pred) do
                      arg([adj of matching_graph, V], NewG, Edges),
                      (fromto(Edges,[E|EdgesRest],EdgesOut,[]),
                       param(NewG, Free, Useful, PhaseNumber, Pred,ELOne) do
                          matching_get_target(NewG, E, TargetE),
                          PredTargetE is Pred[TargetE],
                          ((var(PredTargetE),
                            arg(E, Useful, PhaseNumber))
                          ->
                              find_aug_path(NewG,E,Free,Pred,Useful,
                                            PhaseNumber,F),
                              (nonvar(F) ->
                                  ELOne = F,
                                  EdgesOut = []
                              ;
                                  EdgesOut = EdgesRest
                              )
                          ;
                              EdgesOut = EdgesRest
                          )
                      ),
                      (var(ELOne)->
                          ELOut = ELIn
                      ;
                          ELOut = [ELOne|ELIn]
                      )
                  ),
                  (foreach(E,EL),
                   fromto(FreeInAIn,FreeInAInIn,FreeInAInOut,FreeInAOut),
                   param(NewG,Free,Pred) do
                      matching_get_target(NewG, E, TargetE),
                      setarg(TargetE,Free,false),
                      (fromto(E,EIn,EOut,[]),
                       fromto(_,_,ZOut,Z),
                       param(NewG,Pred) do
                          matching_get_source(NewG, EIn, ZOut),
                          graph_rev_edge(NewG,EIn),
                          PredZOut is Pred[ZOut],
                          (var(PredZOut)-> EOut=[] ; EOut=PredZOut)
                      ),
                      setarg(Z,Free,false),
                      once(delete(Z,FreeInAInIn,FreeInAInOut))
                  ),
                  % next phase
                  TerminateOut = TerminateIn
             ;
                  TerminateOut = true
             )
        ),
        % extract results
        (for(E,1,NumEdges),
         fromto(MaximalM,MIn,MOut,[]),
         param(NewG,InAB) do
            matching_get_source(NewG, E, Src),
            (arg(Src,InAB,b) ->
                % edge is in the matching
                arg([edge of matching_graph, E], NewG, Edge),
                MIn=[Edge|MOut]
            ;
                MIn=MOut
            )
        ),
        true.

bfs(NewG, FreeInA, Free, Useful, Dist, Reached, PhaseNumber) :-
        (foreach(V,FreeInA),
         param(Dist,Reached,PhaseNumber) do
             setarg(V, Dist, 0),
             setarg(V,Reached,PhaseNumber)
        ),
        list_to_queue(FreeInA, Q),
        bfs_aux(Q,NewG,Free, Useful, Dist, Reached, PhaseNumber,AugmentingPathFound),
        % only succeed if we found a path
        nonvar(AugmentingPathFound).


bfs_aux(QIn,_NewG,_Free, _Useful, _Dist, _Reached, _PhaseNumber, _AugmentingPathFound):-
        empty_queue(QIn),!.
bfs_aux(QIn,NewG,Free, Useful, Dist, Reached, PhaseNumber,AugmentingPathFound):-
        serve_queue(QIn,V, QInPopped),
        DVPlus is Dist[V] + 1,
        arg([adj of matching_graph, V], NewG, EdgeList),
        (foreach(E,EdgeList),
         fromto(QInPopped,QInIn,QInOut,QOut),
         param(Reached,Useful,PhaseNumber,Dist, Free,
               AugmentingPathFound, DVPlus, NewG)
        do
            matching_get_target(NewG, E, W),
            (PhaseNumber is Reached[W]->
                QInIn=QInOut
            ;
                setarg(W,Dist,DVPlus),
                setarg(W,Reached,PhaseNumber),
                (true is Free[W] -> AugmentingPathFound=true ; true),
                (var(AugmentingPathFound)->
                    join_queue(W,QInIn,QInOut)
                ;
                    QInOut=QInIn
                )
            ),
            (DVPlus is Dist[W] ->
                setarg(E,Useful,PhaseNumber)
            ;
                true
            )
        ),
        bfs_aux(QOut,NewG,Free, Useful, Dist, Reached, PhaseNumber,AugmentingPathFound).

find_aug_path(NewG, F, Free, Pred, Useful, PhaseNumber, FinalEdge):-
        matching_get_target(NewG, F, W),
        setarg(W,Pred,F),
        (true is Free[W] ->
             FinalEdge = F
        ;
             arg([adj of matching_graph, W], NewG, Edges),
             (fromto(Edges,[E|EdgesRest],EdgesOut,[]),
              param(PhaseNumber,NewG,Free,Pred,Useful,FinalEdge) do
		  matching_get_target(NewG, E, Z),
                  PredZ is Pred[Z],
                  ((var(PredZ),
                    arg(E, Useful, PhaseNumber)                   ) ->
                       find_aug_path(NewG, E, Free, Pred, Useful,
                                     PhaseNumber, FinalEdge),
                       (var(FinalEdge) ->
                            EdgesOut = EdgesRest
                       ;
                            % found a path, so terminate loop
                            EdgesOut=[]
                       )
                  ;
                       EdgesOut = EdgesRest
                  )
             )
        ).

matching_get_source(matching_graph{from:SrcArray},
                    EdgeId,
                    TargetNode):-
        arg(EdgeId, SrcArray, TargetNode).
matching_get_target(matching_graph{to:DstArray},
                    EdgeId,
                    TargetNode):-
        arg(EdgeId, DstArray, TargetNode).
node_array(matching_graph{size:Size},Array):-
        dim(Array,[Size]).

node_array(matching_graph{size:Size},Array,Init):-
        dim(Array,[Size]),
        (foreacharg(Init,Array), param(Init) do true).

edge_array(matching_graph{from:EdgeArray},Array,Init):-
        dim(EdgeArray,[NumEdges]),
        dim(Array,[NumEdges]),
        (foreacharg(Init,Array), param(Init) do true).

% reverse the direction of an edge
graph_rev_edge(matching_graph{from:SrcArray,
                              to:DstArray},Edge):-
        arg(Edge, SrcArray, Src),
        arg(Edge, DstArray, Dst),
        setarg(Edge, SrcArray, Dst),
        setarg(Edge, DstArray, Src).





%----------------------------------------------------------------------
% Single-Source All Shortest Paths
% Incrementally computes all (not just one) shortest paths from a given node
% to every reachable node given a list of modified edges.
%----------------------------------------------------------------------


:- comment(incremental_all_shortest_paths_as_edges/6, [
    summary:"Incrementally computes all shortest paths from a single source to every reachable node given a list of modified edges",
    amode:(incremental_all_shortest_paths_as_edges(+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
        "Modified":"list of e/3 edge structures whose DistanceArg argument has been modified",
    	"Lengths":"array of numbers (minimum path lengths)",
    	"Predecessors":"array of lists of e/3 edge structures" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
%	incremental_all_shortest_paths_as_edges/6,
	incremental_all_shortest_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7
	],
    eg:"
    ?- sample_graph(G), incremental_all_shortest_paths_as_edges(G, 0, 1, M, L, E).
    L = [](0, 1, 2, 3, 2, 1, 1, _326, _327, 2, 3, 3, 3)
    E = []([], [e(1, 2, 1)], [e(7, 3, 1)], [e(5, 4, 1)],
	   [e(7, 5, 1), e(6, 5, 1)], [e(1, 6, 1)], [e(1, 7, 1)],
	   _342, _343, [e(7, 10, 1)], [e(10, 11, 1)], [e(10, 12, 1)],
	   [e(10, 13, 1)])
    ",
    desc:html("<P>
    Incrementally computes all shortest paths from the single source
    node SourceNode to every sink node which is reachable from it
    given a list of modified edges. The result is returned in the form
    of the Predecessors array which contains all relevant edges.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a positive number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    Modified is the list of e/3 edge structures whose DistanceArg
    argument has been modified since the last computation for this
    SourceNode.
</P><P>
    The result is returned in the form of two arrays, whose indices range
    over the possible sink nodes.  The Lengths array indicates the length
    of a shortest path from SourceNode to the corresponding sink node.
    The Predecessors array is an array of edge lists, each list containing
    the alternative edges that are part of a shortest path from SourceNode
    to the corresponding sink node.
</P><P>
    If there is no path from SourceNode to a sink node J, then both
    Lengths[J] and Predecessors[J] are uninstantiated. Otherwise,
    Lengths[J] contains the length of a shortest path from SourceNode to J.
    Predecessors[J] contains a list of alternative edges that lead from
    some predecessor node to J in a shortest path from SourceNode to J.
    Predecessors[SourceNode] is always the empty list [].
</P>
<H4>Assembling Actual Paths</H4>
<P>
    To generate an actual path from the Predecessors array, start from the
    sink node J, select one of the alternative edges in Predecessors[J]
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the parameters, the following 3 cases can occur:
    <OL>
    <LI>Graph did not contain zero-length edges: in this
    case, SubGraph is cycle-free and shortest paths can be found by simply
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Graph did contain zero-length edges: in this case,
    SubGraph may contain (zero-length) cycles which one may want to exclude
    when constructing paths.
    </OL>
    The possible_path/7 predicate implements this path construction and
    does the necesssary checks to exclude cycles.
    </P>")]).

:- export incremental_all_shortest_paths_as_edges/6.
incremental_all_shortest_paths_as_edges(G, DistanceArg, SourceNode, Modified, Lengths, Incoming) :-
        common_incremental_all_shortest_paths(G, DistanceArg, SourceNode, 0, Modified, Lengths, Incoming).

:- comment(incremental_all_shortest_paths_as_graph/6, [
    summary:"Incrementally computes all shortest paths from a single source to every reachable node given a list of modified edges",
    amode:(incremental_all_shortest_paths_as_graph(+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
        "Modified":"list of e/3 edge structures whose DistanceArg argument has been modified",
    	"Lengths":"array of numbers (minimum path lengths)",
    	"SubGraph":"a graph structure" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	incremental_all_shortest_paths_as_edges/6,
%	incremental_all_shortest_paths_as_graph/6,
	single_pair_short_path/6,
	single_pair_all_short_paths_as_graph/7
	],
    eg:"
    ?- sample_graph(G), incremental_all_shortest_paths_as_graph(G, 0, 1, 0, L, E).
    G = graph(13, []([e(1, 6, 1), e(1, 2, 1), e(1, 7, 1)], [], ...)
    L = [](0, 1, 2, 3, 2, 1, 1, _326, _327, 2, 3, 3, 3)
    SG = graph(13, []([e(1, 7, 1), e(1, 6, 1), e(1, 2, 1)], [], ...)
    Yes (0.00s cpu)
    ",
    desc:html("<P>
    Incrementally computes all shortest paths from the single source
    node SourceNode to every sink node which is reachable from it. The
    result is returned in the form of a sub-graph of the input graph,
    which contains all relevant edges.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a positive number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    SourceNode is the common starting point for the computed paths.
</P><P>
    Modified is the list of e/3 edge structures whose DistanceArg
    argument has been modified since the last computation for this
    SourceNode.
</P><P>
    The result is returned in the form of SubGraph, which is a
    sub-graph of the input Graph, containing the same nodes, but only
    those edges that are needed to construct the shortest paths for
    the given parameters.  SubGraph does not inherit the nodename
    information from Graph, this can be set explicitly if required.
</P><P>
    In addition, a Lengths array is returned, whose entries indicate
    the length of a shortest path from SourceNode to the corresponding
    sink node.  If there is no path from SourceNode to a sink node J,
    then Lengths[J] is uninstantiated.
</P>
<H4>Properties of the resulting SubGraph</H4>
<P>
    To generate an actual path from the resulting SubGraph, start from the
    sink node J, select one of its incoming edges (graph_get_incoming_edges/3)
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the parameters, the following 3 cases can occur:
    <OL>
    <LI>Graph did not contain zero-length edges: in this
    case, SubGraph is cycle-free and shortest paths can be found by simply
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Graph did contain zero-length edges: in this case,
    SubGraph may contain (zero-length) cycles which one may want to exclude
    when constructing paths.
    </P>")]).

:- export incremental_all_shortest_paths_as_graph/6.
incremental_all_shortest_paths_as_graph(G, DistanceArg, SourceNode, Modified, Lengths, ResultGraph) :-
        G = graph with [size:MaxNode],
	common_incremental_all_shortest_paths(G, DistanceArg, SourceNode, 0, Modified, Lengths, Incoming),
	make_graph_from_incoming_edges(MaxNode, Incoming, ResultGraph).

:- comment(incremental_single_pair_shortest_path/6, [
    summary:"Computes short paths from a source to a sink node",
    amode:(incremental_single_pair_shortest_path(+,+,+,+,+,-) is nondet),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance: integer",
    	"SourceNode":"source node number (integer)",
    	"SinkNode":"sink node number (integer)",
        "Modified":"list of e/3 edge structures whose DistanceArg argument has been modified",
    	"Path":"Length-EdgeList structure" ],
    see_also:[
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	incremental_all_shortest_paths_as_edges/6,
	incremental_all_shortest_paths_as_graph/6,
	single_pair_short_path/6,
%	incremental_single_pair_shortest_path/6,
	single_pair_all_short_paths_as_graph/7,
	possible_path/7
    ],
    fail_if:"There is no path from SourceNode to SinkNode",
    eg:"
    ?- sample_graph(G), incremental_single_pair_shortest_path(G, 0, 1, M, P).
    P = 2 - [e(2, 3, 1), e(1, 2, 1)]
    ",
    desc:html("<P>
    Incrementally computes shortest paths from SourceNode to SinkNode.
    Alternative paths are generated on backtracking. Fails if there is
    no path at all.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a positive number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    Modified is the list of e/3 edge structures whose DistanceArg
    argument has been modified since the last computation for this
    SourceNode.
</P><P>
    All paths returned will have the same length and will be shortest
    paths from SourceNode to SinkNode. Note that the solutions are not
    generated in any specific order.
</P><P>
    A resulting path is returned as a Length-EdgeList structure where
    Length is the length of the path and EdgeList is the path in
    reverse order, i.e. starting with the edge reaching SinkNode and
    ending with the edge starting from SourceNode.
    </P>")]).

:- export incremental_single_pair_shortest_path/6.
incremental_single_pair_shortest_path(G, DistanceArg, SourceNode, SinkNode, Modified, Path) :-
	common_incremental_all_shortest_paths(G, DistanceArg, SourceNode, SinkNode, Modified, Lengths, Predecessors),
	possible_path(DistanceArg, SourceNode, SinkNode, 0, Lengths, Predecessors, Path).



:- comment(incremental_single_pair_all_shortest_paths_as_graph/7, [
    summary:"Computes all shortest paths from source to sink in form of a subgraph",
    amode:(incremental_single_pair_all_shortest_paths_as_graph(+,+,+,+,+,-,-) is det),
    args:[ "Graph":"a graph structure",
    	"DistanceArg":"which argument of EdgeData to use as distance (integer)",
    	"SourceNode":"source node number (integer)",
    	"SinkNode":"sink node number (integer)",
    	"Modified":"list of e/3 edge structures whose DistanceArg argument has been modified",
    	"Length":"a number (minimum path length)",
    	"SubGraph":"a graph structure" ],
    see_also:[
	possible_path/7,
	shortest_paths/4,
	single_pair_shortest_path/5,
	all_short_paths_as_edges/6,
	all_short_paths_as_graph/6,
	single_pair_short_path/6,
%	single_pair_all_short_paths_as_graph/7,
	graph_get_incoming_edges/3,
	graph_set_nodenames/2
	],
    eg:"
    ?- sample_graph(G),
       incremental_single_pair_all_shortest_paths_as_graph(G, 0, 1, 5, M, L, E).
    G = graph(13, []([e(1, 6, 1), e(1, 2, 1), e(1, 7, 1)], [], ...)
    L = 2
    SG = graph(13, []([e(1, 6, 1), e(1, 7, 1)], [], ...)
    ",
    desc:html("<P>
    Incrementally computes all shortest paths from source node
    SourceNode to sink node SinkNode.  The result is returned in the
    form of a sub-graph of the input graph, which contains all
    relevant edges. If there is no path, the predicate fails.
</P><P>
    DistanceArg refers to the graph's EdgeData information that was
    specified when the graph was constructed. If EdgeData is a simple
    number, then DistanceArg should be 0 and EdgeData will be taken
    as the length of the edge. If EdgeData is a compound data structure,
    DistanceArg should be a number between 1 and the arity of that
    structure and determines which argument of the EdgeData structure
    will be interpreted as the edge's length. Important: the distance
    information in EdgeData must be a non-negative number.
</P><P>
    If DistanceArg is given as -1, then any EdgeData is ignored and
    the length of every edge is assumed to be equal to 1.
</P><P>
    Modified is the list of e/3 edge structures whose DistanceArg
    argument has been modified since the last computation for this
    SourceNode.
</P><P>
    The result is returned in the form of SubGraph, which is a
    sub-graph of the input Graph, containing the same nodes, but only
    those edges that are needed to construct the shortest paths for
    the given parameters.  SubGraph does not inherit the nodename
    information from Graph, this can be set explicitly if required.
</P><P>
    In addition, the Length of the shortest path from source to sink
    is returned.
</P>
<H4>Properties of the resulting SubGraph</H4>
<P>
    To generate an actual path from the resulting SubGraph, start from the
    sink node J, select one of its incoming edges (graph_get_incoming_edges/3)
    to find a predecessor node, and continue this process until the SourceNode
    is reached. Depending on the input graph, the following 2 cases can occur:
    <OL>
    <LI>Graph did not contain zero-length edges: in this case,
    SubGraph is cycle-free and shortest paths can be found by simply 
    selecting arbitrary incoming edges until SourceNode is reached.
    <LI>Graph did contain zero-length edges: in this case, SubGraph
    may contain (zero-length) cycles which one may want to exclude 
    when constructing paths.
    </OL>
    </P>")]).

:- export incremental_single_pair_all_shortest_paths_as_graph/7.
incremental_single_pair_all_shortest_paths_as_graph(G, DistanceArg, SourceNode, SinkNode, Modified, Length, ResultGraph) :-
	G = graph with [size:MaxNode],
	common_incremental_all_shortest_paths(G, DistanceArg, SourceNode, SinkNode, Modified, Lengths, Incoming),
	arg(SinkNode, Lengths, Length),
	make_graph_from_incoming_edges_single_pair(MaxNode, Incoming, SourceNode, SinkNode, ResultGraph).



% The common code for all "incremental_all_shortest_paths" predicates
% - if SinkNode is set to zero we compute all sinks, otherwise we stop there

common_incremental_all_shortest_paths(G, DistanceArg, SourceNode, SinkNode, Modified, Lengths, Incoming) :-
        G = graph with [
                        size: MaxN,
                        adj: Adj,
                        incrementals: IncArray,
                        adj_id: AdjId,
                        in_id: InId
                       ],
        ( var(IncArray) ->
            % need to initialize the incrementals array and the
            % adj_id and in_id arrays
            dim(IncArray, [MaxN]),
            dim(AdjId, [MaxN]),
            dim(InId, [MaxN]),
            (
                for(N, 1, MaxN),
                param(Adj, AdjId, InId)
            do
                arg(N, Adj, OutEdges),
                arg(N, AdjId, OutIdEdges),
                (
                    foreach(Edge, OutEdges),
                    foreach(IdEdge, OutIdEdges),
                    param(InId)
                do
                    Edge = e(_, X, _),
                    IdEdge = id_e with [id:Id, edge:Edge],
                    arg(X, InId, XInEdgeIds),
                    ( var(XInEdgeIds) ->
                        Id = 1,
                        XInEdgeIds = [IdEdge]
                    ;
                        XInEdgeIds = [id_e with id:Id0|_],
                        Id is Id0 + 1,
                        setarg(X, InId, [IdEdge|XInEdgeIds])
                    )
                )
            )
        ;
            true
        ),
        arg(SourceNode, IncArray, Incremental),
        ( var(Incremental) ->
            % need to initialize this incremental structure:
            % the non-incremental shortest_paths is more efficient when all
            % edge/node values have effectively been changed so run
            % that then use the results to initialise the values
            common_all_short_paths(G, DistanceArg, SourceNode, SinkNode, 0, Lengths, Incoming),
            Incremental = incremental with [
                                            node_vals: NodeVals,
                                            node_heap: NodeHeap
                                           ],
            dim(NodeVals, [MaxN]),
            heap_create(MaxN, NodeHeap),
            (
                for(N, 1, MaxN),
                param(SourceNode, NodeVals, InId, Lengths, Incoming)
            do
                arg(N, InId, InIdEdges),
                ( var(InIdEdges) ->
                    InIdEdges = [],
                    EIn0 = 0
                ; InIdEdges == [] ->
                    EIn0 = 0
                ;
                    InIdEdges = [id_e with id:EIn0|_]
                ),
                ( N = SourceNode ->
                    EIn is EIn0 + 1,
                    SetN0 is 2^EIn
                ;
                    EIn = EIn0,
                    SetN0 = 0
                ),
                heap_create(EIn, EdgeHeap),
                NodeValN = node_val with [
                                          value: ValN,
                                          max_edge: EIn,
                                          edge_heap: EdgeHeap,
                                          edge_set: SetN
                                         ],
                arg(N, Lengths, LengthN),
                ( var(LengthN) ->
                    % unreachable
                    ValN = 1.0Inf,
                    SetN = 0
                ;
                    ValN = LengthN,
                    arg(N, Incoming, IncomingN),
                    (
                        foreach(Edge, IncomingN),
                        fromto(SetN0, In, Out, SetN),
                        param(InId)
                    do
                        Edge = e(_X, Y, _Data),
                        arg(Y, InId, IdEdgePairs),
                        memberchk(id_e with [id:Id, edge:Edge], IdEdgePairs),
                        Out is setbit(In, Id)
                    )
                ),
                arg(N, NodeVals, NodeValN)
            ),
            ( SinkNode > 0 ->
                % just did a single_pair shortest path, may have
                % inconsistent nodes remaining, recompute values for
                % edges adjacent to reachable nodes in order to
                % initialize the global heap of inconsistent nodes
                (
                    for(N, 1, MaxN),
                    param(DistanceArg, NodeVals, AdjId, Incremental)
                do
                    arg(N, NodeVals, node_val with value: ValN),
                    ( ValN < 1.0Inf ->
                        % reachable
                        arg(N, AdjId, OutgoingN),
                        (
                            foreach(Edge, OutgoingN),
                            param(DistanceArg, Incremental)
                        do
                            recompute_edge_value(Edge, DistanceArg, Incremental)
                        )
                    ;
                        true
                    )
                )
            ;
                true
            )
        ;
            dynamic_ssf_g(DistanceArg, SinkNode, Modified, AdjId, InId, Incremental),
            dim(Lengths, [MaxN]),
            dim(Incoming, [MaxN]),
            Incremental = incremental with node_vals:NodeVals,
            (
                for(N, 1, MaxN),
                param(NodeVals, InId, SourceNode, Lengths, Incoming)
            do
                ( N = SourceNode ->
                    arg(N, Lengths, 0),
                    arg(N, Incoming, [])
                ;
                    arg(N, NodeVals, NodeValN),
                    NodeValN = node_val with [
                                              value:LengthN,
                                              edge_set:SetN
                                             ],
                    ( LengthN < 1.0Inf ->
                        arg(N, Lengths, LengthN),
                        arg(N, InId, InIdEdges),
                        (
                            foreach(id_e with [id:Id, edge:Edge], InIdEdges),
                            fromto(IncomingN, Out, In, []),
                            param(SetN)
                        do
                            ( getbit(SetN, Id, 1) -> Out = [Edge|In] ; Out = In )
                        ),
                        arg(N, Incoming, IncomingN)
                    ;
                        true
                    )
                )
            )
        ).

dynamic_ssf_g(DistanceArg, SinkNode, Modified, AdjId, InId, Incremental) :-
        % precondition:
        % every unmodified edge is consistent
        (
            foreach(Edge, Modified),
            param(DistanceArg, InId, Incremental)
        do
            EdgeId = id_e with edge:Edge,
            Edge = e(_, Y, _),
            arg(Y, InId, InIdEdges),
            memberchk(EdgeId, InIdEdges),
            recompute_edge_value(EdgeId, DistanceArg, Incremental)
        ),
        (
            fromto(false, In, Out, true),
            param(AdjId, InId, SinkNode, DistanceArg, Incremental)
        do
            % loop invariants:
            % 1: for every edge E = e(X, Y, _)
            % E \in SP[Y] iff d[X] + DistanceArg(E) = value(E) =< d[Y]
            % AND
            % (value(E), E) \in Heap[Y] iff value(E) < d[Y]
            % 2: for every node Y
            % (d[Y], Y) \in NodeHeap iff SP[Y] = empty_set
            % AND
            % (min_key(Heap[Y]), Y) \in NodeHeap iff Heap[Y] = empty_set
            Incremental = incremental with [
                                            node_vals: NodeVals,
                                            node_heap: NodeHeap
                                           ],
            ( next_inconsistent_node(NodeHeap, SinkNode, NodeVals, Key, X) ->
                Out = In,
                arg(X, NodeVals, NodeValX),
                NodeValX = node_val with [
                                          value:DX,
                                          max_edge:EIn,
                                          edge_heap:HeapX
                                         ],
                arg(X, AdjId, OutEdges),
                ( Key < DX ->
                    % X is over-consistent:
                    % we know that Heap[X] is non-empty
                    % and that Key = min_key(Heap[X]);
                    % since X satisfies the invariant
                    % we also know that Heap[X] contains
                    % all in_edges E with
                    % value(E) < d[X].
                    % In order to make X consistent
                    % we need to:
                    % set d[X] = Key = min_key(Heap[X])
                    node_val_set(NodeValX, value, Key),
                    % AND
                    % set SP[X] = {E|E is a edge
                    %              for X and value(E) = Key}
                    % Note that this is precisely the set
                    % of all minimum elements in HeapX
                    heap_all_min_keys(HeapX, Ids, Ids),
                    (
                        foreach(Id, Ids),
                        fromto(0, In, Out, SPX)
                    do
                        Out is setbit(In, Id)
                    ),
                    node_val_set(NodeValX, edge_set, SPX),
                    % AND
                    % set Heap[X] = empty_set
                    heap_delete_all(HeapX),
                    % X is now consistent,
                    % delete from NodeHeap:
                    heap_delete_min(NodeHeap, Key, X, X),
                    % now check if the successors of X are
                    % inconsistent:
                    (
                        foreach(Edge, OutEdges),
                        param(DistanceArg, Incremental)
                    do
                        recompute_edge_value(Edge, DistanceArg, Incremental)
                    )
                ;
                    % X is under-consistent
                    % we know that Heap[X], SP[X] are empty
                    % and that Key = d[X];
                    % since X satisfies the invariant
                    % In order to make X not under-consistent
                    % we need to:
                    % set d[X] = +inf
                    node_val_set(NodeValX, value, 1.0Inf),
                    % AND
                    % set SP[X] = {E|E is an edge for X}
                    SPX is 2^(EIn+1)-2,
                    node_val_set(NodeValX, edge_set, SPX),
                    % AND
                    % set Heap[X] = {E|E is an edge
                    %                for X and value(E) < +inf}
                    arg(X, InId, InEdges),
                    (
                        foreach(Edge, InEdges),
                        param(DistanceArg, NodeVals, HeapX)
                    do
                        Edge = id_e with [id:Id, edge:e(Y, _X, Data)],
                        Value is node_val(Y, NodeVals) + xarg(DistanceArg, Data),
                        ( Value < 1.0Inf ->
                            heap_insert(HeapX, Value, Id, Id)
                        ;
                            true
                        )
                    ),
                    % X may now be over-consistent,
                    % increase key in NodeHeap:
                    ( heap_min_key(HeapX, MinKey, _MinId, _MinId) ->
                        heap_increase_key(NodeHeap, MinKey, X, X)
                    ;
                        heap_delete_min(NodeHeap, Key, X, X)
                    ),
                    % 
                    % now check if the successors of X are
                    % inconsistent:
                    (
                        foreach(Edge, OutEdges),
                        param(DistanceArg, Incremental)
                    do
                        recompute_edge_value(Edge, DistanceArg, Incremental)
                    )
                )
            ;
                Out = true
            )
        % post-condition: every edge is consistent
        ).

next_inconsistent_node(NodeHeap, SinkNode, NodeVals, Key, X) :-
        heap_min_key(NodeHeap, Key, X, X),
        ( SinkNode = 0 ->
            % doing all sinks
            true
        ; heap_access(NodeHeap, SinkNode, _SinkKey, SinkNode) ->
            % specified sink is inconsistent
            true
        ;
            Key =< node_val(SinkNode, NodeVals) % next inconsistent
                                                % node could be in a
                                                % path to SinkNode
        ).

recompute_edge_value(EdgeId, DistanceArg, Incremental) :-
        Incremental = incremental with [
                                        node_vals:NodeVals,
                                        node_heap:NodeHeap
                                       ],
        EdgeId = id_e with [id:Id, edge:e(X, Y, Data)],
        arg(Y, NodeVals, NodeValY),
        NodeValY = node_val with [
                                  value:DY,
                                  edge_heap:HeapY,
                                  edge_set:SPY
                                 ],
        Value is node_val(X, NodeVals) + xarg(DistanceArg, Data),
        ( Value < DY ->
            heap_update_if_different(HeapY, Value, Id, Id)
        ;
            ( heap_delete(HeapY, Id, _OldValue, Id) -> true ; true )
        ),
        ( DY < Value ->
            NewSPY is clrbit(SPY, Id)
        ;  
            NewSPY is setbit(SPY, Id)
        ),
        node_val_set(NodeValY, edge_set, NewSPY),
        ( NewSPY =:= 0 ->
            % Y is under-consistent
            heap_update_if_different(NodeHeap, DY, Y, Y)
        ; heap_min_key(HeapY, MinKey, _MinId, _MinId) ->
            % Y is over-consistent
            heap_update_if_different(NodeHeap, MinKey, Y, Y)
        ;
            % Y is consistent
            ( heap_delete(NodeHeap, Y, _Key, Y) -> true ; true )
        ).

node_val(X, NodeVals, Val) :-
        arg(X, NodeVals, NodeValX),
        arg(value of node_val, NodeValX, Val).

node_val_set(NodeVal, value, Val) :- !,
        setarg(value of node_val, NodeVal, Val).
node_val_set(NodeVal, edge_set, Val) :- !,
        setarg(edge_set of node_val, NodeVal, Val).
node_val_set(NodeVal, What, Val) :-
        error(5, node_val_set(NodeVal, What, Val)).

%----------------------------------------------------------------------
% Code templates
%----------------------------------------------------------------------

end_of_file.

:- lib(util).

dfs_test :-
	 make_random_graph(1000, 10000, true, true, true, G),
%	 dfs(G).
	 time(dfs(G)), time(dfs1(G)), time(dfs2(G)).


% using stack of adjacency lists
dfs(G) :-
	G = graph with [size:MaxNode, adj:Adj],
	dim(Seen, [MaxNode]),
	(
	    for(StartNode, 1, MaxNode),
	    fromto(0, Id0, Id1, _),
	    param(Adj, Seen)
	do
	    NodeSeen is Seen[StartNode],
	    ( var(NodeSeen) ->
		dfs_visit(Adj, Seen, [e(_,StartNode,_)], [], Id0, Id1)
	    ;
	    	Id0 = Id1
	    )
	).

    dfs_visit(_Adj, _Seen, [], [], Id0, Id) :- !, Id = Id0.
    dfs_visit(Adj, Seen, [], [Edges|Stack], Id0, Id) :- !,
	dfs_visit(Adj, Seen, Edges, Stack, Id0, Id).
    dfs_visit(Adj, Seen, [e(_,Node,_)|Edges], Stack, Id0, Id2) :-
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    % Visit
	    Id is Id0 + 1,
	    arg(Node, Seen, Id),	% mark visited
%	    writeln(Id:node(Node)),
	    arg(Node, Adj, Successors),
	    dfs_visit(Adj, Seen, Successors, [Edges|Stack], Id, Id2)
	;
	    dfs_visit(Adj, Seen, Edges, Stack, Id0, Id2)
	).

dfs(G, StartNode) :-
	G = graph with [size:MaxNode, adj:Adj],
	dim(Seen, [MaxNode]),
	dfs_visit(Adj, Seen, [e(_,StartNode,_)], [], 0, _).

/***
% using stack of edges
dfs1(G) :-
	G = graph with [size:MaxNode],
	dim(Seen, [MaxNode]),
	(
	    for(StartNode, 1, MaxNode),
	    fromto(0, Id0, Id1, _),
	    param(G, Seen)
	do
	    NodeSeen is Seen[StartNode],
	    ( var(NodeSeen) ->
		dsf1_visit(G, Seen, [e(_,StartNode,_)], Id0, Id1)
	    ;
	    	Id0 = Id1
	    )
	).

dfs1(G, StartNode) :-
	G = graph with [size:MaxNode],
	dim(Seen, [MaxNode]),
	dsf1_visit(G, Seen, [e(_,StartNode,_)], 0, _).

    dsf1_visit(_G, _Seen, [], Id, Id).
    dsf1_visit(G, Seen, [e(_,Node,_)|Stack1], Id0, Id2) :-
	arg(Node, Seen, NodeSeen),
	( var(NodeSeen) ->
	    % Visit
	    Id is Id0 + 1,
	    Id is Seen[Node],	% mark visited
%	    node_to_nodename(G, Node, Name), writeln(node(Id:Name)),

	    Successors is G[adj of graph, Node],
	    append(Successors, Stack1, Stack2),
	    dsf1_visit(G, Seen, Stack2, Id, Id2)
	;
	    dsf1_visit(G, Seen, Stack1, Id0, Id2)
	).


% using recursion
dfs2(G) :-
	G = graph with [size:MaxNode],
	dim(Seen, [MaxNode]),
	(
	    for(Node, 1, MaxNode),
	    fromto(1, Id0, Id1, _),
	    param(G, Seen)
	do
	    NodeSeen is Seen[Node],
	    ( var(NodeSeen) ->
	    	dfs2_visit(G, Seen, Node, Id0, Id1)
	    ;
	    	Id0 = Id1
	    )
	).

    dfs2_visit(G, Seen, Node, Id0, Id4) :-
	Id is Id0 + 1,
	Id is Seen[Node],
	Ys is G[adj of graph, Node],
	(
	    foreach(e(_,Y,_), Ys),
	    fromto(Id, Id2, Id3, Id4),
	    param(G, Seen)
	do
	    SeenY is Seen[Y],
	    ( var(SeenY) ->
	    	dfs2_visit(G, Seen, Y, Id2, Id3)
	    ;
	    	Id3 = Id2
	    )
	).
%	node_to_nodename(G, Node, Name), writeln(node(Node:Name)).
***/


% Breadth-first search

bfs(G, StartNode) :-
	G = graph with [size:MaxNode],
	dim(Val, [MaxNode]),
	list_to_queue([e(_,StartNode,_)], StartQueue),
	bfs_visit(G, Val, StartQueue, 0, _).

    bfs_visit(G, Val, Queue0, Id0, Id2) :-
    	( serve_queue(Queue0, e(_,Node,_), Queue1) ->
	    arg(Node, Val, Seen),
	    ( var(Seen) ->
		% Visit
		Id is Id0 + 1,
		Id is Val[Node],	% mark visited
		node_to_nodename(G, Node, Name), writeln(node(Id:Name)),

		Successors is G[adj of graph, Node],
		list_join_queue(Successors, Queue1, Queue2),
		bfs_visit(G, Val, Queue2, Id, Id2)
	    ;
		bfs_visit(G, Val, Queue1, Id0, Id2)
	    )
	;
	    Id2 = Id0
	).


%----------------------------------------------------------------------
% Tests
%----------------------------------------------------------------------

test1(Graph) :-
    make_graph( 13,	% same as g3 below
	[ e(1,6,1),e(1,2,1),e(1,7,1),e(3,1,1),e(4,6,1),e(5,4,1),
	  e(6,5,1),e(7,5,1),e(7,10,1),e(7,3,1),e(8,7,1),e(8,9,1),e(9,8,1),
	  e(10,11,1),e(10,12,1),e(10,13,1),e(12,7,1),e(12,13,1),e(13,12,1) ],
        Graph),
    graph_set_nodenames(Graph, [](a,b,c,d,e,f,g,h,i,j,k,l,m)).

test2(Graph) :-
    make_graph_symbolic(
	[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	[ edge(a,f,1),edge(a,b,1),edge(a,g,1),edge(c,a,1),edge(d,f,1),edge(e,d,1),
	  edge(f,e,1),edge(g,e,1),edge(g,j,1),edge(g,c,1),edge(h,g,1),edge(h,i,1),edge(i,h,1),
	  edge(j,k,1),edge(j,l,1),edge(j,m,1),edge(l,g,1),edge(l,m,1),edge(m,l,1) ],
	Graph).

dag(Graph) :-
    make_graph_symbolic(
	[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	[ edge(a,f,1),edge(a,b,1),edge(a,g,1),edge(a,c,1), edge(e,d,1),
	  edge(f,d,1),edge(f,e,1),edge(g,c,1),edge(g,e,1),edge(g,h,1),
	  edge(h,i,1),edge(j,g,1),edge(j,k,1),edge(j,l,1),edge(j,m,1),
	  edge(l,g,1),edge(l,m,1)],
	Graph).

test3 :-
	test1(Graph),
	graph_get_all_edges(Graph, Edges), writeln(Edges),
	graph_get_adjacent_edges(Graph, 7, Edges7), writeln(Edges7),
	graph_get_maxnode(Graph, NNodes), writeln(NNodes),
	graph_get_nodenames(Graph, Names), writeln(Names),
	node_to_nodename(Graph, 7, Name7), writeln(7->Name7),
	nodes_to_nodenames(Graph, [1,3,7], Name137), writeln([1,3,7]->Name137),
	nodename_to_node(Graph, g, Node7), writeln(g->Node7),
	nodenames_to_nodes(Graph, [a,c,g], Node137), writeln([a,c,g]->Node137),
	graph_get_edge(Graph, 7, 3, Edge73), writeln(Edge73),
	findall(Node, graph_node(Graph, Node), AllNodes), writeln(AllNodes),
	findall(Edge, graph_edge(Graph, Edge), AllEdges), writeln(AllEdges),
	findall(Edge, graph_adjacent_edge(Graph, 7, Edge), Edges7), writeln(Edges7),
	true.

test4 :-
	test1(Graph1),
	make_undirected_graph(Graph1, Graph2),
	graph_is_bidirected(Graph2).

test5 :-
	repeat,
	make_random_graph(30,50,true,true,true,G),
	make_undirected_graph(G, U),
	connected_components(U, CCs),
	CCs = [C1,C2|_],
	length(C1) >= 4, length(C2) >= 4,
	graphviz:view_graph(U),
	tyi(0'y),
	!,
	( foreach(CC,CCs), param(U) do
	    make_sub_graph(U, CC, GCC),
	    is_sub_graph(GCC, U),
	    graphviz:view_graph(GCC)
	).


test_mst(N,M,G,T,TS,W) :-
	cputime(T0),
	seed(123),
	make_random_graph(N,M, true, true, true, G),
	graph_set_random_weights(G, 1, 10),
	cputime(T1),
	Tgen is T1-T0,
	writeln(gen:Tgen),
	minimum_spanning_forest(G, 0, T, TS, W),
	( TS < N-1 ->
	    writeln("Not a tree")
%	    daVinci:daVinci_draw_graph(G)
	;
	    % it's a tree
	    ( foreach(e(N1,N2,_),T), fromto(TreeNodes,[N1,N2|Ns],Ns,[]) do
	        true
	    ),
	    sort(TreeNodes, UniqueTreeNodes),
	    ( length(UniqueTreeNodes, N) ->
		true
	    ;
	    	writeln("Spanning tree error")
	    )
	),
	cputime(T2),
	Tk is T2-T1,
	writeln(kruskal:Tk).


% this is the graph in Sedgewick figure 30.2
g1(graph with [		% bidirected (=undirected)
	size:13,
	nodes:[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	adj:[](
	    	[e(1,6,1),e(1,2,1),e(1,3,1),e(1,7,1)],
		[e(2,1,1)],
		[e(3,1,1),e(3,7,1)],
		[e(4,5,1),e(4,6,1)],
		[e(5,7,1),e(5,4,1),e(5,6,1)],
		[e(6,1,1),e(6,5,1),e(6,4,1)],
		[e(7,1,1),e(7,5,1),e(7,12,1),e(7,8,1),e(7,3,1),e(7,10,1)],
		[e(8,9,1),e(8,7,1)],
		[e(9,8,1)],
		[e(10,11,1),e(10,13,1),e(10,7,1),e(10,12,1)],
		[e(11,10,1)],
		[e(12,7,1),e(12,10,1),e(12,13,1)],
		[e(13,10,1),e(13,12,1)])
	]).

g1a(graph with [		% bidirected (=undirected)
	size:13,
	nodes:[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	adj:[](
	    	[e(1,6,1),e(1,3,1),e(1,7,1)],
		[],
		[e(3,1,1),e(3,7,1)],
		[e(4,5,1),e(4,6,1)],
		[e(5,7,1),e(5,4,1),e(5,6,1)],
		[e(6,1,1),e(6,5,1),e(6,4,1)],
		[e(7,1,1),e(7,5,1),e(7,12,1),e(7,3,1),e(7,10,1)],
		[e(8,9,1)],
		[e(9,8,1)],
		[e(10,11,1),e(10,13,1),e(10,7,1),e(10,12,1)],
		[e(11,10,1)],
		[e(12,7,1),e(12,10,1),e(12,13,1)],
		[e(13,10,1),e(13,12,1)])
	]).

% this is the graph in Sedgewick figure 31.1
g2(graph with [		% weighted
	size:13,
	nodes:[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	adj:[](
	    	[e(1,2,1),e(1,6,2),e(1,7,6)],		% e(from,to,distance)
		[e(2,1,1),e(2,3,1),e(2,4,2),e(2,5,4)],
		[e(3,2,1),e(3,5,4)],
		[e(4,2,2),e(4,5,2),e(4,6,1)],
		[e(5,2,4),e(5,3,4),e(5,4,2),e(5,6,2),e(5,7,1),e(5,12,4)],
		[e(6,1,2),e(6,4,1),e(6,5,2),e(6,12,2)],
		[e(7,1,6),e(7,5,1),e(7,8,3),e(7,10,1),e(7,12,5)],
		[e(8,7,3),e(8,9,2)],
		[e(9,8,2),e(9,11,1)],
		[e(10,7,1),e(10,11,1),e(10,12,3),e(10,13,2)],
		[e(11,10,1),e(11,9,1)],
		[e(12,5,4),e(12,6,2),e(12,7,5),e(12,10,3),e(12,13,1)],
		[e(13,10,2),e(13,12,1)])
	]).

% this is the graph in Sedgewick figure 32.1
:- export g3/1.
g3(graph with [		% directed
	size:13,
%	nodes:[](a,b,c,d,e,f,g,h,i,j,k,l,m),
	adj:[](
	    	[e(1,6,1),e(1,2,1),e(1,7,1)],	% 1
		[],				% 2
		[e(3,1,1)],			% 3
		[e(4,6,1)],			% 4
		[e(5,4,1)],			% 5
		[e(6,5,1)],			% 6
		[e(7,5,1),e(7,10,1),e(7,3,1)],	% 7
		[e(8,7,1),e(8,9,1)],		% 8
		[e(9,8,1)],			% 9
		[e(10,11,1),e(10,12,1),e(10,13,1)],	% 10
		[],				% 11
		[e(12,7,1),e(12,13,1)],		% 12
		[e(13,12,1)])			% 13
	]).



% returns AP = [2,8] and 2 seems wrong
bug1(U, AP) :-
	U = graph(10, []([], [e(2, 4, _624), e(2, 9, _875)], [e(3, 8,
	_820)], [e(4, 2, _624), e(4, 8, _569), e(4, 9, _349), e(4, 10,
	_930)], [e(5, 8, _294), e(5, 9, _514)], [e(6, 8, _404)], [],
	[e(8, 3, _820), e(8, 4, _569), e(8, 5, _294), e(8, 6, _404)],
	[e(9, 2, _875), e(9, 4, _349), e(9, 5, _514), e(9, 10, _459)],
	[e(10, 4, _930), e(10, 9, _459)]), _1252, _1253, _1254),
	articulation_points(U, AP).


g4(graph with [		% directed
	size:4,
	nodes:[](a,b,c,d),
	adj:[](
	    	[e(1,2,10),e(1,3,10),e(1,4,10)],
		[e(2,3,1)],
		[e(3,4,1)],
		[])
	]).

