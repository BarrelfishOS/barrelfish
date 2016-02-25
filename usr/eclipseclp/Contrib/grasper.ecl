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
% The Original Code is The GRASPER Constraint Solver for ECLiPSe.
% The Initial Developer of the Original Code is Ruben Duarte Viegas.
% Portions created by the Initial Developer are Copyright (C) 2007.
% All Rights Reserved.
% 
% Contributor(s): Ruben Duarte Viegas <rviegas@di.fct.unl.pt>.
%
% Alternatively, the contents of this file may be used under the terms of
% either of the GNU General Public License Version 2 or later (the "GPL"),
% or the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
% in which case the provisions of the GPL or the LGPL are applicable instead
% of those above. If you wish to allow use of your version of this file only
% under the terms of either the GPL or the LGPL, and not to allow others to
% use your version of this file under the terms of the MPL, indicate your
% decision by deleting the provisions above and replace them with the notice
% and other provisions required by the GPL or the LGPL. If you do not delete
% the provisions above, a recipient may use your version of this file under
% the terms of any one of the MPL, the GPL or the LGPL.
% END LICENSE BLOCK

%%%
%
% GRASPER
% GRAph constraint Satisfaction Problem solveR
%
%%%

:- module(grasper).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Finite Graphs Constraints Library").
:- comment(author, "Ruben Duarte Viegas, CENTRIA").
:- comment(status, prototype).
:- comment(date, "$DATE$").
:- comment(desc,html("<P>
    Graph-based constraint programming can be declaratively used for path
    and circuit finding problems, to routing, scheduling and allocation
    problems, among others.  CP(Graph) was proposed by G.  Dooms et al. 
    as a general approach to solve graph-based constraint problems.  It
    provides a key set of basic constraints which represent the
    framework's core, and higher level constraints for solving path
    finding and optimization problems, and to enforce graph properties.
    </P><P>
    The GRASPER (GRAph constraint Satisfaction Problem solvER) library
    is an alternative framework for graph-based constraint solving
    based on Cardinal, a finite sets constraint solver with extra
    inferences developed in Universidade Nova de Lisboa.  It provides
    a set of basic constraints which represent the core of our
    framework and functionality for directed graphs, graph weighting,
    graph matching, graph path optimization problems and some of the
    most common graph properties.
    </P><P>
    More information in this
    <A HREF=\"http://www.springerlink.com/content/553x1733661l6k31/fulltext.pdf\">background paper</A>.
    </P>
")).
:- comment(see_also, [library(cardinal)]).

%%%
%
% Imports
%
%%%

:- lib(fd).
:- lib(ordset).
:- lib(cardinal).
:- lib(hash).

%%%
%
% Exports
%
%%%

:- export dirgraph/3.
:- export undirgraph/3.
:- export getVertexSet/2.
:- export getEdgeSet/2.
:- export order/2.
:- export size/2.

:- export weight/3.
:- export predecessors/3.
:- export successors/3.
:- export reachables/3.

:- export symmetric/1.
:- export asymmetric/1.
:- export connected/1.
:- export strongly_connected/1.
:- export weakly_connected/1.
:- export path/3.
%:- export cycle/3.

:- export subgraph/2.
:- export induced_subgraph/2.
:- export underlying_graph/2.
:- export oriented_graph/2.
:- export reverse_graph/2.
:- export complementary_graph/2.

:- export graph_labeling/1.
:- export graph_labeling/3.

:- export export_graph/2.
:- export hash_add_all/2.
:- export kill_susps/1.
:- export terminate_susps/2.

:- comment(induced_subgraph/2, hidden).
:- comment(hash_add_all/2, hidden).
:- comment(kill_susps/1, hidden).
:- comment(terminate_susps/2, hidden).

:- comment(
	dirgraph/3,
	[
		amode: dirgraph(-,+,+),
		args:
		[
			"Graph": "A directed graph.",
			"VertexSet": "The vertex-set that constitutes Graph.",
			"EdgeSet": "The edge-set that constitutes Graph."
		],
		summary: "Directed graph constructor.",
		desc: html("Creates Graph as a directed graph variable composed by the vertexes in VertexSet and the edges in EdgeSet."),
		fail_if:
			"Fails 
			 if VertexSet is not a set variable,
			 if EdgeSet is not a set variable or
			 if EdgeSet cannot be contained in (VertexSet x VertexSet).
			",
		eg:
			"
?- E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E).
No.

?- V`::[]..[1,2,3], dirgraph(G,V,E).
No.

?- V`::[]..[1,2,3], E`::[[4,5]]..[[1,2],[2,3],[3,1],[4,5]], dirgraph(G,V,E).
No.

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _543, fd:[0..3]}, _435, _436, _437, [], [], ['SUSP-_2315-susp'], ['SUSP-_1925-dead'])}
E = E{cardinal([[]:0, [[1, 2], [2, 3], [3, 1]]:3], Card{cardinal : _728, fd:[0..3]}, _620, _621, _622, [], ['SUSP-_2325-susp'], [], ['SUSP-_1641-dead'])}
G = dirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _543, fd:[0..3]}, _435, _436, _437, [], [], ['SUSP-_2315-susp'], ['SUSP-_1925-dead'])}, E{cardinal([[]:0, [[1, 2], [2, 3], [3, 1]]:3], Card{cardinal : _728, fd:[0..3]}, _620, _621, _622, [], ['SUSP-_2325-susp'], [], ['SUSP-_1641-dead'])})

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1],[4,5]], dirgraph(G,V,E).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _573, fd:[0..3]}, _465, _466, _467, [], [], ['SUSP-_2391-susp'], ['SUSP-_2001-dead'])}
E = E{cardinal([[]:0, [[1, 2], [2, 3], [3, 1]]:3], Card{cardinal : _766, fd:[0..3]}, _658, _659, _660, [], ['SUSP-_2401-susp'], [], ['SUSP-_1717-dead'])}
G = dirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _573, fd:[0..3]}, _465, _466, _467, [], [], ['SUSP-_2391-susp'], ['SUSP-_2001-dead'])}, E{cardinal([[]:0, [[1, 2], [2, 3], [3, 1]]:3], Card{cardinal : _766, fd:[0..3]}, _658, _659, _660, [], ['SUSP-_2401-susp'], [], ['SUSP-_1717-dead'])})

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E).
V = V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _573, fd:[2, 3]}, _465, _466, _467, [], [], ['SUSP-_2363-susp'], ['SUSP-_1987-dead'])}
E = E{cardinal([[[1, 2]]:1, [[2, 3], [3, 1]]:3], Card{cardinal : _763, fd:[1..3]}, _655, _656, _657, [], ['SUSP-_2373-susp'], [], ['SUSP-_1681-dead'])}
G = dirgraph(V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _573, fd:[2, 3]}, _465, _466, _467, [], [], ['SUSP-_2363-susp'], ['SUSP-_1987-dead'])}, E{cardinal([[[1, 2]]:1, [[2, 3], [3, 1]]:3], Card{cardinal : _763, fd:[1..3]}, _655, _656, _657, [], ['SUSP-_2373-susp'], [], ['SUSP-_1681-dead'])})
			"
	]
).

dirgraph(dirgraph(VertexSet, EdgeSet), VertexSet, EdgeSet) :-
	glb_poss(VertexSet, GLBVertex, PossVertex),
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	graph(dirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex, GLBEdge, PossEdge).

:- comment(
	undirgraph/3,
	[
		amode: undirgraph(-,+,+),
		args:
		[
			"Graph": "An undirected graph.",
			"VertexSet": "The vertex-set that constitutes Graph.",
			"EdgeSet": "The edge-set that constitutes Graph."
		],
		summary: "Unirected graph constructor.",
		desc: html("Creates Graph as an undirected graph variable composed by the vertexes in VertexSet and the edges in EdgeSet."),
		fail_if:
			"Fails 
			 if VertexSet is not a set variable,
			 if EdgeSet is not a set variable or
			 if EdgeSet can not be contained in (VertexSet x VertexSet).
			",
		eg:
			"
?- E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E).
No.
			 
?- V`::[]..[1,2,3], undirgraph(G,V,E).
No.
 
?- V`::[]..[1,2,3], E`::[[4,5]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2],[4,5],[5,4]], undirgraph(G,V,E).
No.
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _633, fd:[0..3]}, _525, _526, _527, [], [], ['SUSP-_2546-susp'], ['SUSP-_2156-dead'])}
E = E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _842, fd:[0..6]}, _734, _735, _736, [], ['SUSP-_2556-susp'], [], ['SUSP-_1872-dead'])}
G = undirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _633, fd:[0..3]}, _525, _526, _527, [], [], ['SUSP-_2546-susp'], ['SUSP-_2156-dead'])}, E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _842, fd:[0..6]}, _734, _735, _736, [], ['SUSP-_2556-susp'], [], ['SUSP-_1872-dead'])})
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2],[4,5],[5,4]], undirgraph(G,V,E).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _693, fd:[0..3]}, _585, _586, _587, [], [], ['SUSP-_2692-susp'], ['SUSP-_2302-dead'])}
E = E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _918, fd:[0..6]}, _810, _811, _812, [], ['SUSP-_2702-susp'], [], ['SUSP-_2018-dead'])}
G = undirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _693, fd:[0..3]}, _585, _586, _587, [], [], ['SUSP-_2692-susp'], ['SUSP-_2302-dead'])}, E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _918, fd:[0..6]}, _810, _811, _812, [], ['SUSP-_2702-susp'], [], ['SUSP-_2018-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2],[2,1]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E).
V = V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _693, fd:[2, 3]}, _585, _586, _587, [], [], ['SUSP-_3041-susp'], ['SUSP-_2651-dead'])}
E = E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _917, fd:[2..6]}, _809, _810, _811, [], ['SUSP-_3051-susp'], [], ['SUSP-_1966-dead'])}
G = undirgraph(V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _693, fd:[2, 3]}, _585, _586, _587, [], [], ['SUSP-_3041-susp'], ['SUSP-_2651-dead'])}, E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _917, fd:[2..6]}, _809, _810, _811, [], ['SUSP-_3051-susp'], [], ['SUSP-_1966-dead'])})
			"
	]
).

undirgraph(undirgraph(VertexSet, EdgeSet), VertexSet, EdgeSet) :-
	symmetricEdges(EdgeSet),
	glb_poss(VertexSet, GLBVertex, PossVertex),
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	graph(undirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex, GLBEdge, PossEdge).

:- comment(
	symmetric/1,
	[
		amode: symmetric(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Ensures Graph is symmetric",
		desc: html("Ensures that for every edge (x,y) added to the Graph the symmetric edge (y,x) is also added.<br />
					Ensures that for every edge (x,y) removed from the Graph the symmetric edge (y,x) is also removed."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable or
			 if Graph can not be enforced to be symmetric.
			",
		eg:
			"
?- symmetric(G).
No.
			 
?- V`::[]..[1,2,3], E`::[[2,3]]..[[1,2],[1,3],[2,1],[2,3],[3,1]], dirgraph(G,V,E), symmetric(G).
No.
			 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), symmetric(G).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _628, fd:[0..3]}, _520, _521, _522, [], [], ['SUSP-_2494-susp'], ['SUSP-_2104-dead'])}
E = E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [3, 1]]:4], Card{cardinal : _829, fd:[0..4]}, _721, _722, _723, [], ['SUSP-_3338-susp', 'SUSP-_2504-susp'], ['SUSP-_3347-susp'], ['SUSP-_3248-dead'])}
G = dirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _628, fd:[0..3]}, _520, _521, _522, [], [], ['SUSP-_2494-susp'], ['SUSP-_2104-dead'])}, E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [3, 1]]:4], Card{cardinal : _829, fd:[0..4]}, _721, _722, _723, [], ['SUSP-_3338-susp', 'SUSP-_2504-susp'], ['SUSP-_3347-susp'], ['SUSP-_3248-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), symmetric(G).
V = V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _688, fd:[2, 3]}, _580, _581, _582, [], [], ['SUSP-_3503-dead', 'SUSP-_2633-susp'], ['SUSP-_3584-dead'])}
E = E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _902, fd:[2..6]}, _794, _795, _796, [], ['SUSP-_4193-susp', 'SUSP-_2643-susp'], ['SUSP-_4202-susp'], ['SUSP-_4109-dead'])}
G = dirgraph(V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _688, fd:[2, 3]}, _580, _581, _582, [], [], ['SUSP-_3503-dead', 'SUSP-_2633-susp'], ['SUSP-_3584-dead'])}, E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _902, fd:[2..6]}, _794, _795, _796, [], ['SUSP-_4193-susp', 'SUSP-_2643-susp'], ['SUSP-_4202-susp'], ['SUSP-_4109-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1]], dirgraph(G,V,E), symmetric(G).
V = V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _658, fd:[2, 3]}, _550, _551, _552, [], [], ['SUSP-_3426-dead', 'SUSP-_2556-susp'], ['SUSP-_3507-dead'])}
E = E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [3, 1]]:4], Card{cardinal : _864, fd:[2..4]}, _756, _757, _758, [], ['SUSP-_4162-susp', 'SUSP-_2566-susp'], ['SUSP-_4171-susp'], ['SUSP-_4052-dead'])}
G = dirgraph(V{cardinal([[1, 2]:2, [3]:3], Card{cardinal : _658, fd:[2, 3]}, _550, _551, _552, [], [], ['SUSP-_3426-dead', 'SUSP-_2556-susp'], ['SUSP-_3507-dead'])}, E{cardinal([[[1, 2], [2, 1]]:2, [[1, 3], [3, 1]]:4], Card{cardinal : _864, fd:[2..4]}, _756, _757, _758, [], ['SUSP-_4162-susp', 'SUSP-_2566-susp'], ['SUSP-_4171-susp'], ['SUSP-_4052-dead'])})
			"
	]
).

symmetric(Graph) :-
	var(Graph),!,fail.
symmetric(dirgraph(_,EdgeSet)) :-
	symmetricEdges(EdgeSet).
symmetric(undirgraph(_,_)).

symmetricEdges(EdgeSet) :-
	add_edge_add_sym_edge(EdgeSet),
	rem_edge_rem_sym_edge(EdgeSet),

	suspend(add_edge_add_sym_edge(EdgeSet), 5, EdgeSet->cardinal:glb, GLBSusp),
	suspend(rem_edge_rem_sym_edge(EdgeSet), 5, EdgeSet->cardinal:lub, LUBSusp),
	terminate_susps(kill_susps([GLBSusp, LUBSusp]), [EdgeSet]).

%%%
% - In a symmetric graph, if an edge is added then its symmetric edge must also be added
% - O(m)
%%%

:- demon add_edge_add_sym_edge/1.

add_edge_add_sym_edge(EdgeSet) :-
	glb(EdgeSet, GLBEdge),
	findall([Y,X], member([X,Y], GLBEdge), SubEdgeSet),
	SubEdgeSet `< EdgeSet.

%%%
% - In a symmetric graph, if an edge is removed then its symmetric edge must also be removed
% - O(m)
%%%

:- demon rem_edge_rem_sym_edge/1.

rem_edge_rem_sym_edge(EdgeSet) :-
	lub(EdgeSet, LUBEdge),
	hash_create(EdgeHash),
	hash_add_all(EdgeHash, LUBEdge),
	findall([X,Y], (member([X,Y], LUBEdge), \+hash_contains(EdgeHash,[Y,X])), NotPossEdges),
	EdgeSet `$ NotPossEdges.

:- comment(
	asymmetric/1,
	[
		amode: asymmetric(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Ensures Graph is asymmetric",
		desc: html("Ensures that for every edge (x,y) added to the Graph the symmetric edge (y,x) is removed."),
		resat: "?",
		fail_if:
			"Fails 
			 if Graph is not a graph variable or
			 if Graph can not be enforced to be asymmetric.
			",
		eg:
			"
?- asymmetric(G).
No.
 
?- V`::[]..[1,2,3], E`::[[2,3],[3,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), asymmetric(G).
No.
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), asymmetric(G).
V = V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _658, fd:[0..3]}, _550, _551, _552, [], [], ['SUSP-_2571-susp'], ['SUSP-_2181-dead'])}
E = E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _867, fd:[0..6]}, _759, _760, _761, [], ['SUSP-_3006-susp', 'SUSP-_2581-susp'], [], ['SUSP-_2882-dead'])}
G = dirgraph(V{cardinal([[]:0, [1, 2, 3]:3], Card{cardinal : _658, fd:[0..3]}, _550, _551, _552, [], [], ['SUSP-_2571-susp'], ['SUSP-_2181-dead'])}, E{cardinal([[]:0, [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]:6], Card{cardinal : _867, fd:[0..6]}, _759, _760, _761, [], ['SUSP-_3006-susp', 'SUSP-_2581-susp'], [], ['SUSP-_2882-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), asymmetric(G).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _688, fd : [2, 3]}, _580, _581, _582, [], [], ['SUSP-_2633-susp'], ['SUSP-_2243-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 3], [3, 1], [3, 2]] : 5], Card{cardinal : _902, fd : [1 .. 5]}, _794, _795, _796, [], ['SUSP-_3117-susp', 'SUSP-_2643-susp'], [], ['SUSP-_2970-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _688, fd : [2, 3]}, _580, _581, _582, [], [], ['SUSP-_2633-susp'], ['SUSP-_2243-dead'])}, E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 3], [3, 1], [3, 2]] : 5], Card{cardinal : _902, fd : [1 .. 5]}, _794, _795, _796, [], ['SUSP-_3117-susp', 'SUSP-_2643-susp'], [], ['SUSP-_2970-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1]], dirgraph(G,V,E), asymmetric(G).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _658, fd : [2, 3]}, _550, _551, _552, [], [], ['SUSP-_2556-susp'], ['SUSP-_2166-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 3], [3, 1]] : 4], Card{cardinal : _864, fd : [1 .. 4]}, _756, _757, _758, [], ['SUSP-_3040-susp', 'SUSP-_2566-susp'], [], ['SUSP-_2893-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _658, fd : [2, 3]}, _550, _551, _552, [], [], ['SUSP-_2556-susp'], ['SUSP-_2166-dead'])}, E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 3], [3, 1]] : 4], Card{cardinal : _864, fd : [1 .. 4]}, _756, _757, _758, [], ['SUSP-_3040-susp', 'SUSP-_2566-susp'], [], ['SUSP-_2893-dead'])})
			"
	]
).

asymmetric(G) :-
	var(G),!,fail.
asymmetric(dirgraph(_,EdgeSet)) :-
	asymmetricEdges(EdgeSet).

asymmetricEdges(EdgeSet) :-
	add_edge_rem_sym_edge(EdgeSet),

	suspend(add_edge_rem_sym_edge(EdgeSet), 5, EdgeSet->cardinal:glb, GLBSusp),
	terminate_susps(kill_susps([GLBSusp]), [EdgeSet]).

%%%
% - In an asymmetric graph, if an edge is added then its symmetric edge must be removed
% - O(m)
%%%

:- demon add_edge_rem_sym_edge/1.

add_edge_rem_sym_edge(EdgeSet) :-
	glb(EdgeSet, GLBEdge),
	findall([Y,X], member([X,Y], GLBEdge), NotPossEdges),
	NotPossEdges `$ EdgeSet.

graph(dirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex, GLBEdge, PossEdge) :-
	setVertexSet(dirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex),
	setEdgeSet(dirgraph(VertexSet, EdgeSet), GLBEdge, PossEdge),
	graph_aux(VertexSet, EdgeSet).
graph(undirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex, GLBEdge, PossEdge) :-
	setVertexSet(undirgraph(VertexSet, EdgeSet), GLBVertex, PossVertex),
	setEdgeSet(undirgraph(VertexSet, EdgeSet), GLBEdge, PossEdge),
	graph_aux(VertexSet, EdgeSet).
	
graph_aux(VertexSet, EdgeSet) :-
	rem_vertex_rem_edges(VertexSet, EdgeSet),
	add_edge_add_vertices(VertexSet, EdgeSet),
	
	cardinality(VertexSet, VertexCard),
	cardinality(EdgeSet, EdgeCard),
	EdgeCard #<= (VertexCard * VertexCard),
	suspend(rem_vertex_rem_edges(VertexSet, EdgeSet), 4, VertexSet->cardinal:lub, VSusp),
	suspend(add_edge_add_vertices(VertexSet, EdgeSet), 4, EdgeSet->cardinal:glb, ESusp),
	terminate_susps(kill_susps([VSusp,ESusp]), [VertexSet,EdgeSet]).
	
%%%
% - The removal of a vertex imposes the removal of every edge which is incident on it
% - O(m + n)
%%%

:- demon rem_vertex_rem_edges/2.
	
rem_vertex_rem_edges(VertexSet, EdgeSet) :- 
	lub(VertexSet, LUBVertex),
	hash_create(VertexHash),
	hash_add_all(VertexHash, LUBVertex),
	lub(EdgeSet, LUBEdge),
	findall([X,Y], (member([X,Y], LUBEdge), hash_contains(VertexHash,X), hash_contains(VertexHash,Y)), PossEdges),
	EdgeSet `< PossEdges.

%%%
% - The addition of an edge imposes the addition of every vertex on which the edge is incident
% - O(m + n)
%%%

:- demon add_edge_add_vertices/2.

add_edge_add_vertices(VertexSet, EdgeSet) :-
	glb(EdgeSet, GLBEdge),
	all_union(GLBEdge, SubVertexSet),
	SubVertexSet `< VertexSet.

:- comment(
	getVertexSet/2,
	[
		amode: getVertexSet(+,?),
		args:
		[
			"Graph": "A graph.",
			"VertexSet": "Graph's vertex-set."
		],
		summary: "Obtains a graph's vertex-set.",
		desc: html("Obtains a graph's vertex-set."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable or
			 if VertexSet can not be matched with Graph's vertex-set.
			",
		eg:
			"
?- getVertexSet(G,V).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getVertexSet(G,[2,3]).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getVertexSet(G,V_1).
V = V_1{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])}
G = dirgraph(V_1{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}, E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])})
V_1 = V_1{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getVertexSet(G,[1,2]).
V = [1, 2]
E = E{cardinal([[[1, 2]] : 1, [[2, 1]] : 2], Card{cardinal : _962, fd : [1, 2]}, _854, _855, _856, [], ['SUSP-_3006-dead', 'SUSP-_2703-susp'], [], ['SUSP-_3312-dead'])}
G = dirgraph([1, 2], E{cardinal([[[1, 2]] : 1, [[2, 1]] : 2], Card{cardinal : _962, fd : [1, 2]}, _854, _855, _856, [], ['SUSP-_3006-dead', 'SUSP-_2703-susp'], [], ['SUSP-_3312-dead'])})
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), V_1`::[]..[1,2], getVertexSet(G,V_1).
V = V{cardinal([[] : 0, [1, 2] : 2], Card{cardinal : _730, fd : [0 .. 2]}, Min1, Max1, Union2, [], [], ['SUSP-_2643-susp'], [])}
E = E{cardinal([[] : 0, [[1, 2], [2, 1]] : 2], Card{cardinal : _939, fd : [0 .. 2]}, _831, _832, _833, [], ['SUSP-_3239-dead', 'SUSP-_2653-susp'], [], ['SUSP-_3547-dead'])}
G = dirgraph(V{cardinal([[] : 0, [1, 2] : 2], Card{cardinal : _730, fd : [0 .. 2]}, Min1, Max1, Union2, [], [], ['SUSP-_2643-susp'], [])}, E{cardinal([[] : 0, [[1, 2], [2, 1]] : 2], Card{cardinal : _939, fd : [0 .. 2]}, _831, _832, _833, [], ['SUSP-_3239-dead', 'SUSP-_2653-susp'], [], ['SUSP-_3547-dead'])})
V_1 = V{cardinal([[] : 0, [1, 2] : 2], Card{cardinal : _730, fd : [0 .. 2]}, Min1, Max1, Union2, [], [], ['SUSP-_2643-susp'], [])}
			"
	]
).

getVertexSet(Graph,_) :-
	var(Graph),!,fail.
getVertexSet(dirgraph(VertexSet, _), VertexSet).
getVertexSet(undirgraph(VertexSet, _), VertexSet).

setVertexSet(Graph, GLBVertex, PossVertex) :-
	getVertexSet(Graph, VertexSet),
	VertexSet`::GLBVertex..PossVertex.

:- comment(
	getEdgeSet/2,
	[
		amode: getEdgeSet(+,?),
		args:
		[
			"Graph": "A graph.",
			"EdgeSet": "Graph's edge-set."
		],
		summary: "Obtains a graph's edge-set.",
		desc: html("Obtains a graph's edge-set."),
		fail_if:
			"Fails
			 if Graph is not a graph variable or
			 if EdgeSet can not be matched with Graph's edge-set.
			",
		eg:
			"
?- getEdgeSet(G,E).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getEdgeSet(G,[[2,3],[3,2]]).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getEdgeSet(G,E_1).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}
E = E_1{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}, E_1{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])})
E_1 = E_1{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), getEdgeSet(G,[[1,2],[2,3],[3,1]]).
V = [1, 2, 3]
E = [[1, 2], [2, 3], [3, 1]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3], [3, 1]])
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), E_1`::[]..[[1,2],[2,3],[3,1]], getEdgeSet(G,E_1).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _830, fd : [2, 3]}, _722, _723, _724, [], [], ['SUSP-_2775-susp'], ['SUSP-_2385-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _1044, fd : [1 .. 3]}, Min1, Max1, Union2, [], ['SUSP-_2785-susp'], [], [])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _830, fd : [2, 3]}, _722, _723, _724, [], [], ['SUSP-_2775-susp'], ['SUSP-_2385-dead'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _1044, fd : [1 .. 3]}, Min1, Max1, Union2, [], ['SUSP-_2785-susp'], [], [])})
E_1 = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _1044, fd : [1 .. 3]}, Min1, Max1, Union2, [], ['SUSP-_2785-susp'], [], [])}
			"
	]
).

getEdgeSet(Graph, _) :-
	var(Graph),!,fail.
getEdgeSet(dirgraph(_, EdgeSet), EdgeSet).
getEdgeSet(undirgraph(_, EdgeSet), EdgeSet).

setEdgeSet(Graph, GLBEdge, PossEdge) :-
	getEdgeSet(Graph, EdgeSet),
	EdgeSet`::GLBEdge+PossEdge.

:- comment(
	order/2,
	[
		amode: order(+,?),
		args:
		[
			"Graph": "A graph.",
			"Order": "The order of the graph."
		],
		summary: "Obtains a graph's order.",
		desc: html("Determines the number of vertices composing a graph variable."),
		fail_if:
			"Fails
			 if Graph is not a graph variable or
			 if Graph can not be constrained to have a vertex-set with a cardinality delimited by Order.
			",
		eg:
			"
?- order(Graph,Order).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), order(G,0).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), order(G,4).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), order(G,Order).
V = V{cardinal([[1, 2] : 2, [3] : 3], Order{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Order{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}, E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])})
Order = Order{cardinal : _700, fd : [2, 3]}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), order(G,2).
V = [1, 2]
E = E{cardinal([[[1, 2]] : 1, [[2, 1]] : 2], Card{cardinal : _909, fd : [1, 2]}, _801, _802, _803, [], ['SUSP-_2957-dead', 'SUSP-_2650-susp'], [], ['SUSP-_3263-dead'])}
G = dirgraph([1, 2], E{cardinal([[[1, 2]] : 1, [[2, 1]] : 2], Card{cardinal : _909, fd : [1, 2]}, _801, _802, _803, [], ['SUSP-_2957-dead', 'SUSP-_2650-susp'], [], ['SUSP-_3263-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), order(G,3).
V = [1, 2, 3]
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _909, fd : [1 .. 6]}, _801, _802, _803, [], ['SUSP-_2650-susp'], [], ['SUSP-_1944-dead'])}
G = dirgraph([1, 2, 3], E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _909, fd : [1 .. 6]}, _801, _802, _803, [], ['SUSP-_2650-susp'], [], ['SUSP-_1944-dead'])})
			"
	]
).

order(Graph, _) :-
	var(Graph),!,fail.
order(dirgraph(VertexSet,_), Order) :-
	cardinality(VertexSet, Order).
order(undirgraph(VertexSet,_), Order) :-
	cardinality(VertexSet, Order).

:- comment(
	size/2,
	[
		amode: size(+,?),
		args:
		[
			"Graph": "A graph.",
			"Size": "The size of the graph."
		],
		summary: "Obtains a graph's size.",
		desc: html("Determines the number of edges composing a graph variable."),
		fail_if:
			"Fails
			 if Graph is not a graph variable or
			 if Graph can not be constrained to have an edge-set with a cardinality delimited by Size.
			",
		eg:
			"
?- size(Graph,Size).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), size(G,0).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), size(G,7).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), size(G,Size).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Size{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_2645-susp'], ['SUSP-_2255-dead'])}, E{cardinal([[[1, 2]] : 1, [[1, 3], [2, 1], [2, 3], [3, 1], [3, 2]] : 6], Size{cardinal : _914, fd : [1 .. 6]}, _806, _807, _808, [], ['SUSP-_2655-susp'], [], ['SUSP-_1949-dead'])})
Size = Size{cardinal : _914, fd : [1 .. 6]}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E), size(G,Size).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_3898-susp'], ['SUSP-_3508-dead'])}
E = E{cardinal([[[1, 2], [2, 1]] : 2, [[1, 3], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [2 .. 6]}, _806, _807, _808, [], ['SUSP-_3908-susp', 'SUSP-_1758-susp'], ['SUSP-_1767-susp'], ['SUSP-_2823-dead'])}
G = undirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _700, fd : [2, 3]}, _592, _593, _594, [], [], ['SUSP-_3898-susp'], ['SUSP-_3508-dead'])}, E{cardinal([[[1, 2], [2, 1]] : 2, [[1, 3], [2, 3], [3, 1], [3, 2]] : 6], Card{cardinal : _914, fd : [2 .. 6]}, _806, _807, _808, [], ['SUSP-_3908-susp', 'SUSP-_1758-susp'], ['SUSP-_1767-susp'], ['SUSP-_2823-dead'])})
Size = _4017{cardinal : _4022, fd : [1 .. 3]}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), size(G,1).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _695, fd : [2, 3]}, _587, _588, _589, [], [], ['SUSP-_2640-susp'], ['SUSP-_2250-dead'])}
E = [[1, 2]]
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _695, fd : [2, 3]}, _587, _588, _589, [], [], ['SUSP-_2640-susp'], ['SUSP-_2250-dead'])}, [[1, 2]])
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E), size(G,1).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _695, fd : [2, 3]}, _587, _588, _589, [], [], ['SUSP-_3893-susp'], ['SUSP-_3503-dead'])}
E = [[1, 2], [2, 1]]
G = undirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _695, fd : [2, 3]}, _587, _588, _589, [], [], ['SUSP-_3893-susp'], ['SUSP-_3503-dead'])}, [[1, 2], [2, 1]])
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(G,V,E), size(G,6).
V = [1, 2, 3]
E = [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
G = dirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]])
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E), size(G,3).
V = [1, 2, 3]
E = [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
G = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]])
			"
	]
).

size(Graph, _) :-
	var(Graph),!,fail.
size(dirgraph(_,EdgeSet), Size) :-
	cardinality(EdgeSet, Size).
size(undirgraph(_,EdgeSet), Size) :-
	cardinality(EdgeSet, Card),
	Size #= Card / 2.

:- comment(
	subgraph/2,
	[
		amode: subgraph(?,+),
		args:
		[
			"SubGraph": "A subgraph of Graph.",
			"Graph": "A graph."
		],
		summary: "Constraints SubGraph to be a subgraph of Graph.",
		desc: html("Constraints SubGraph to be a subgraph of Graph."),
		fail_if:
			"Fails
			 if Graph is not a graph variable or
			 if SubGraph can not be constrained to be contained in Graph.
			",
		eg:
			"
?- subgraph(SG,G).
No.
 
?- V`::[]..[1,2,3], SE`::[[4,5]]..[[1,2],[2,3],[3,1],[4,5]], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(SG,V,SE), dirgraph(G,V,E), subgraph(SG,G).
No.
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), subgraph(SG,G).
V = V{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _580, fd : [0 .. 3]}, _472, _473, _474, [], [], ['SUSP-_2766-susp', 'SUSP-_2352-susp'], ['SUSP-_2926-susp'])}
E = E{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _765, fd : [0 .. 3]}, _657, _658, _659, [], ['SUSP-_2362-susp'], ['SUSP-_3323-susp'], ['SUSP-_3483-susp'])}
G = dirgraph(V{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _580, fd : [0 .. 3]}, _472, _473, _474, [], [], ['SUSP-_2766-susp', 'SUSP-_2352-susp'], ['SUSP-_2926-susp'])}, E{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _765, fd : [0 .. 3]}, _657, _658, _659, [], ['SUSP-_2362-susp'], ['SUSP-_3323-susp'], ['SUSP-_3483-susp'])})
SG = dirgraph(SubVertexSet{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _2576, fd : [0 .. 3]}, _2468, _2469, _2470, [], ['SUSP-_2648-susp'], ['SUSP-_5069-susp'], ['SUSP-_4679-dead', 'SUSP-_2926-susp'])}, SubEdgeSet{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _3133, fd : [0 .. 3]}, _3025, _3026, _3027, [], ['SUSP-_5079-susp', 'SUSP-_3205-susp'], [], ['SUSP-_4395-dead', 'SUSP-_3483-susp'])})
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), subgraph(SG,G), getVertexSet(G,VSet), 1 `-@ VSet.
V = VSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _645, fd : [0 .. 2]}, _537, _538, _539, [], [], ['SUSP-_2831-susp', 'SUSP-_2417-susp'], ['SUSP-_2991-susp'])}
E = E{cardinal([[] : 0, [[2, 3]] : 1], Card{cardinal : _830, fd : [0, 1]}, _722, _723, _724, [], ['SUSP-_6162-dead', 'SUSP-_2427-susp'], ['SUSP-_3388-susp'], ['SUSP-_6513-dead', 'SUSP-_3548-susp'])}
G = dirgraph(VSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _645, fd : [0 .. 2]}, _537, _538, _539, [], [], ['SUSP-_2831-susp', 'SUSP-_2417-susp'], ['SUSP-_2991-susp'])}, E{cardinal([[] : 0, [[2, 3]] : 1], Card{cardinal : _830, fd : [0, 1]}, _722, _723, _724, [], ['SUSP-_6162-dead', 'SUSP-_2427-susp'], ['SUSP-_3388-susp'], ['SUSP-_6513-dead', 'SUSP-_3548-susp'])})
SG = dirgraph(SubVertexSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _2641, fd : [0 .. 2]}, _2533, _2534, _2535, [], ['SUSP-_2713-susp'], ['SUSP-_5134-susp'], ['SUSP-_4744-dead', 'SUSP-_2991-susp'])}, SubEdgeSet{cardinal([[] : 0, [[2, 3]] : 1], Card{cardinal : _3198, fd : [0, 1]}, _3090, _3091, _3092, [], ['SUSP-_5697-dead', 'SUSP-_5144-susp', 'SUSP-_3270-susp'], [], ['SUSP-_5949-dead', 'SUSP-_3548-susp'])})
VSet = VSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _645, fd : [0 .. 2]}, _537, _538, _539, [], [], ['SUSP-_2831-susp', 'SUSP-_2417-susp'], ['SUSP-_2991-susp'])}
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), subgraph(SG,G), getVertexSet(SG,VSet), 1 `-@ VSet.
V = V{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _645, fd : [0 .. 3]}, _537, _538, _539, [], [], ['SUSP-_2831-susp', 'SUSP-_2417-susp'], ['SUSP-_2991-susp'])}
E = E{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _830, fd : [0 .. 3]}, _722, _723, _724, [], ['SUSP-_2427-susp'], ['SUSP-_3388-susp'], ['SUSP-_3548-susp'])}
G = dirgraph(V{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _645, fd : [0 .. 3]}, _537, _538, _539, [], [], ['SUSP-_2831-susp', 'SUSP-_2417-susp'], ['SUSP-_2991-susp'])}, E{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _830, fd : [0 .. 3]}, _722, _723, _724, [], ['SUSP-_2427-susp'], ['SUSP-_3388-susp'], ['SUSP-_3548-susp'])})
SG = dirgraph(VSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _2641, fd : [0 .. 2]}, _2533, _2534, _2535, [], ['SUSP-_2713-susp'], ['SUSP-_5134-susp'], ['SUSP-_4744-dead', 'SUSP-_2991-susp'])}, SubEdgeSet{cardinal([[] : 0, [[2, 3]] : 1], Card{cardinal : _3198, fd : [0, 1]}, _3090, _3091, _3092, [], ['SUSP-_5481-dead', 'SUSP-_5144-susp', 'SUSP-_3270-susp'], [], ['SUSP-_5733-dead', 'SUSP-_3548-susp'])})
VSet = VSet{cardinal([[] : 0, [2, 3] : 2], Card{cardinal : _2641, fd : [0 .. 2]}, _2533, _2534, _2535, [], ['SUSP-_2713-susp'], ['SUSP-_5134-susp'], ['SUSP-_4744-dead', 'SUSP-_2991-susp'])}
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), subgraph(SG,G), getEdgeSet(G,ESet), [1,2] `@ ESet.
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _665, fd : [2, 3]}, _557, _558, _559, [], [], ['SUSP-_5574-dead', 'SUSP-_2851-susp', 'SUSP-_2437-susp'], ['SUSP-_5655-dead', 'SUSP-_3011-susp'])}
E = ESet{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _850, fd : [1 .. 3]}, _742, _743, _744, [], ['SUSP-_2447-susp'], ['SUSP-_3408-susp'], ['SUSP-_3568-susp'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _665, fd : [2, 3]}, _557, _558, _559, [], [], ['SUSP-_5574-dead', 'SUSP-_2851-susp', 'SUSP-_2437-susp'], ['SUSP-_5655-dead', 'SUSP-_3011-susp'])}, ESet{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _850, fd : [1 .. 3]}, _742, _743, _744, [], ['SUSP-_2447-susp'], ['SUSP-_3408-susp'], ['SUSP-_3568-susp'])})
SG = dirgraph(SubVertexSet{cardinal([[] : 0, [1, 2, 3] : 3], Card{cardinal : _2661, fd : [0 .. 3]}, _2553, _2554, _2555, [], ['SUSP-_2733-susp'], ['SUSP-_5154-susp'], ['SUSP-_4764-dead', 'SUSP-_3011-susp'])}, SubEdgeSet{cardinal([[] : 0, [[1, 2], [2, 3], [3, 1]] : 3], Card{cardinal : _3218, fd : [0 .. 3]}, _3110, _3111, _3112, [], ['SUSP-_5164-susp', 'SUSP-_3290-susp'], [], ['SUSP-_4480-dead', 'SUSP-_3568-susp'])})
ESet = ESet{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _850, fd : [1 .. 3]}, _742, _743, _744, [], ['SUSP-_2447-susp'], ['SUSP-_3408-susp'], ['SUSP-_3568-susp'])}
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), subgraph(SG,G), getEdgeSet(SG,ESet), [1,2] `@ ESet.
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _665, fd : [2, 3]}, _557, _558, _559, [], [], ['SUSP-_5768-dead', 'SUSP-_2851-susp', 'SUSP-_2437-susp'], ['SUSP-_5849-dead', 'SUSP-_3011-susp'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _850, fd : [1 .. 3]}, _742, _743, _744, [], ['SUSP-_2447-susp'], ['SUSP-_3408-susp'], ['SUSP-_3568-susp'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _665, fd : [2, 3]}, _557, _558, _559, [], [], ['SUSP-_5768-dead', 'SUSP-_2851-susp', 'SUSP-_2437-susp'], ['SUSP-_5849-dead', 'SUSP-_3011-susp'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _850, fd : [1 .. 3]}, _742, _743, _744, [], ['SUSP-_2447-susp'], ['SUSP-_3408-susp'], ['SUSP-_3568-susp'])})
SG = dirgraph(SubVertexSet{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _2661, fd : [2, 3]}, _2553, _2554, _2555, [], ['SUSP-_2733-susp'], ['SUSP-_6246-dead', 'SUSP-_5154-susp'], ['SUSP-_6327-dead', 'SUSP-_3011-susp'])}, ESet{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _3218, fd : [1 .. 3]}, _3110, _3111, _3112, [], ['SUSP-_5164-susp', 'SUSP-_3290-susp'], [], ['SUSP-_4480-dead', 'SUSP-_3568-susp'])})
ESet = ESet{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _3218, fd : [1 .. 3]}, _3110, _3111, _3112, [], ['SUSP-_5164-susp', 'SUSP-_3290-susp'], [], ['SUSP-_4480-dead', 'SUSP-_3568-susp'])}
			"
	]
).

subgraph(_, Graph) :-
	var(Graph),!,fail.
subgraph(SubGraph, dirgraph(VertexSet, EdgeSet)) :-
	SubVertexSet `< VertexSet,
	SubEdgeSet `< EdgeSet,
	dirgraph(SubGraph, SubVertexSet, SubEdgeSet).
subgraph(SubGraph, undirgraph(VertexSet, EdgeSet)) :-
	SubVertexSet `< VertexSet,
	SubEdgeSet `< EdgeSet,
	undirgraph(SubGraph, SubVertexSet, SubEdgeSet).

induced_subgraph(SubGraph, Graph) :-
	subgraph(SubGraph, Graph),
	getVertexSet(SubGraph, SubNodes),
	getEdgeSet(SubGraph, SubArcs),
	getEdgeSet(Graph, Arcs),
	
	add_node_add_all_arcs(SubNodes, Arcs, SubArcs),
	
	suspend(add_node_add_all_arcs(SubNodes, Arcs, SubArcs), 5, SubNodes->cardinal:glb, NodesSusp),
	suspend(add_node_add_all_arcs(SubNodes, Arcs, SubArcs), 5, Arcs->cardinal:glb, ArcsSusp),
	terminate_susps(kill_susps([NodesSusp,ArcsSusp]), [SubNodes,Arcs]).

%%%
% - Adding a node to an induced subgraph makes all arcs in the original graph that are incident on nodes already in the induced subgraph's glb to be added to the induced graph
% - O(m_2 + n_1)
%%%
	
:- demon add_node_add_all_arcs/3.	
	
add_node_add_all_arcs(Nodes, Arcs, InducedArcs) :-
	glb(Nodes, GLBNodes),
	hash_create(NodesHash),
	hash_add_all(NodesHash, GLBNodes),
	glb(Arcs, GLBArcs),
	findall([X,Y], (member([X,Y], GLBArcs), hash_contains(NodesHash,X), hash_contains(NodesHash,Y)), SubArcs),
	SubArcs `< InducedArcs.

:- comment(
	weight/3,
	[
		amode: weight(+,+,?),
		args:
		[
			"Graph": "A graph.",
			"WeightHash": "An hashtable an hashtable matching some elements (vertex or edge) of Graph to a positive weight. An unweighed element will be considered to have weight 0.",
			"Weight": "The sum of the graph's vertices' and edges' weights."
		],
		summary: "Calculates the Weight of Graph given a WeightHash.",
		desc: html("Calculates the Weight of Graph given a WeightHash."),
		fail_if:
			"Fails
			 if Graph is not a graph variable,
			 if Weight is not an hash variable (lib(hash)) with positive integer weights or
			 if Graph can not be constrained to have a weight delimited by Weight.
			",
		eg:
			"
?- weight(G,WH,W).
No.
 
?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), weight(G,WH,W).
instantiation fault in term_hash(1, -1, Size, _2486)
Abort
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), hash_create(WH), hash_add(WH,1,1), hash_add(WH,2,1), hash_add(WH,3,1), hash_add(WH,[1,2],1), hash_add(WH,[2,3],1), hash_add(WH,[3,1],1), weight(G,WH,0).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), hash_create(WH), hash_add(WH,1,1), hash_add(WH,2,1), hash_add(WH,3,1), hash_add(WH,[1,2],1), hash_add(WH,[2,3],1), hash_add(WH,[3,1],1), weight(G,WH,7).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), hash_create(WH), hash_add(WH,1,1), hash_add(WH,2,1), hash_add(WH,3,1), hash_add(WH,[1,2],1), hash_add(WH,[2,3],1), hash_add(WH,[3,1],1), weight(G,WH,W).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _941, fd : [2, 3]}, _833, _834, _835, [], ['SUSP-_3104-susp'], ['SUSP-_3130-susp', 'SUSP-_2731-susp'], ['SUSP-_2355-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _1131, fd : [1 .. 3]}, _1023, _1024, _1025, [], ['SUSP-_3117-susp', 'SUSP-_2741-susp'], ['SUSP-_3143-susp'], ['SUSP-_2049-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _941, fd : [2, 3]}, _833, _834, _835, [], ['SUSP-_3104-susp'], ['SUSP-_3130-susp', 'SUSP-_2731-susp'], ['SUSP-_2355-dead'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1]] : 3], Card{cardinal : _1131, fd : [1 .. 3]}, _1023, _1024, _1025, [], ['SUSP-_3117-susp', 'SUSP-_2741-susp'], ['SUSP-_3143-susp'], ['SUSP-_2049-dead'])})
WH = hash(4, 6, [[3, 1] -> 1, [2, 3] -> 1, 1 -> 1, 2 -> 1, [1, 2] -> 1, 3 -> 1])
W = W{cardinal : _3033, fd : [3 .. 6]}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), hash_create(WH), hash_add(WH,1,1), hash_add(WH,2,1), hash_add(WH,3,1), hash_add(WH,[1,2],1), hash_add(WH,[2,3],1), hash_add(WH,[3,1],1), weight(G,WH,3).
V = [1, 2]
E = [[1, 2]]
G = dirgraph([1, 2], [[1, 2]])
WH = hash(4, 6, [[3, 1] -> 1, [2, 3] -> 1, 1 -> 1, 2 -> 1, [1, 2] -> 1, 3 -> 1])
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), hash_create(WH), hash_add(WH,1,1), hash_add(WH,2,1), hash_add(WH,3,1), hash_add(WH,[1,2],1), hash_add(WH,[2,3],1), hash_add(WH,[3,1],1), weight(G,WH,6).
V = [1, 2, 3]
E = [[1, 2], [2, 3], [3, 1]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3], [3, 1]])
WH = hash(4, 6, [[3, 1] -> 1, [2, 3] -> 1, 1 -> 1, 2 -> 1, [1, 2] -> 1, 3 -> 1])
			"
	]
).

weight(Graph, _, _) :-
	var(Graph),!,fail.
weight(dirgraph(VertexSet, EdgeSet), WeightHash, Weight) :-
	my_weight_aux(VertexSet, EdgeSet, WeightHash, Weight).
weight(undirgraph(VertexSet, EdgeSet), WeightHash, Weight) :-
	my_weight_aux(VertexSet, EdgeSet, WeightHash, Weight).	
	
my_weight_aux(VertexSet, EdgeSet, WeightHash, Weight) :-
	glb_poss(VertexSet, GLBVertex, PossVertex),
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	set_weight(GLBVertex, GLBEdge, WeightHash, MinWeight),
	set_weight(PossVertex, PossEdge, WeightHash, IncWeight),
	MaxWeight is MinWeight + IncWeight,
	
	(var(Weight) ->
		Weight #>= MinWeight, Weight #<= MaxWeight;
		(
		 Weight #>= MinWeight, Weight #<= MaxWeight,
		 change_weight_change_graph(Weight, VertexSet, EdgeSet, WeightHash)
		)
	),
	
	suspend(change_weight_change_graph(Weight, VertexSet, EdgeSet, WeightHash), 3, Weight->fd:min, WeightmSusp),
	suspend(change_weight_change_graph(Weight, VertexSet, EdgeSet, WeightHash), 3, Weight->fd:max, WeightMSusp),
	suspend(change_graph_change_weight(VertexSet, EdgeSet, WeightHash, glb, Weight), 3, VertexSet->cardinal:glb, GraphGVSusp),
	suspend(change_graph_change_weight(VertexSet, EdgeSet, WeightHash, glb, Weight), 3, EdgeSet->cardinal:glb, GraphGESusp),
	suspend(change_graph_change_weight(VertexSet, EdgeSet, WeightHash, lub, Weight), 3, VertexSet->cardinal:lub, GraphLVSusp),
	suspend(change_graph_change_weight(VertexSet, EdgeSet, WeightHash, lub, Weight), 3, EdgeSet->cardinal:lub, GraphLESusp),
	terminate_susps(kill_susps([WeightmSusp,WeightMSusp,GraphGVSusp,GraphGESusp,GraphLVSusp,GraphLESusp]), [Weight,VertexSet,EdgeSet]).
	
set_weight(VertexSet, EdgeSet, WeightHash, Weight) :-
	set_weight_aux(VertexSet, WeightHash, 0, NodesWeight),
	set_weight_aux(EdgeSet, WeightHash, NodesWeight, Weight).
	
set_weight_aux([], _, Weight, Weight).
set_weight_aux([Elem|Elems], WeightHash, AcumWeight, Weight) :-
	(hash_find(WeightHash, Elem, ElemWeight); ElemWeight = 0),!,
	NewAcumWeight is AcumWeight + ElemWeight,
	set_weight_aux(Elems, WeightHash, NewAcumWeight, Weight).
	
%%%
% - When the weight is changed the vertex-set and the edge-set may be updated
% O(m + n)
%%%

:- demon change_weight_change_graph/4.	
	
change_weight_change_graph(Weight, VertexSet, EdgeSet, WeightHash) :-
	mindomain(Weight, Min),
	maxdomain(Weight, Max),
	glb_poss(VertexSet, GLBVertex, PossVertex),
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	set_weight(GLBVertex, GLBEdge, WeightHash, MinWeight),
	set_weight(PossVertex, PossEdge, WeightHash, IncWeight),
	MaxWeight is MinWeight + IncWeight,
	check(VertexSet, WeightHash, MinWeight, MaxWeight, Min, Max),
	check(EdgeSet, WeightHash, MinWeight, MaxWeight, Min, Max).
	
check(ElemSet, WeightHash, MinWeight, MaxWeight, Min, Max) :-
	poss(ElemSet, PossElem),
	check(PossElem, WeightHash, MinWeight, MaxWeight, Min, Max, [], AddList, [], RemList),
	AddList `< ElemSet,
	RemList `$ ElemSet.
	
check([], _, _, _, _, _, AddList, AddList, RemList, RemList).
check([Elem|Elems], WeightHash, MinWeight, MaxWeight, Min, Max, AcumAddList, AddList, AcumRemList, RemList) :-
	(hash_find(WeightHash, Elem, ElemWeight); ElemWeight = 0),!,
	NewMinWeight is MinWeight + ElemWeight,
	NewMaxWeight is MaxWeight - ElemWeight,
	(NewMinWeight > Max ->
		(
		 NAcumAddList = AcumAddList,
		 NAcumRemList = [Elem|AcumRemList]
		); 
		(NewMaxWeight < Min ->
			(
			 NAcumAddList = [Elem|AcumAddList],
			 NAcumRemList = AcumRemList
			); 
			(
			 NAcumAddList = AcumAddList,
			 NAcumRemList = AcumRemList
			)
		)
	),
	check(Elems, WeightHash, MinWeight, MaxWeight, Min, Max, NAcumAddList, AddList, NAcumRemList, RemList).

%%%
% - When the graph is changed the weight must be updated
% - O(m + n)
%%%

:- demon change_graph_change_weight/5.		
	
change_graph_change_weight(VertexSet, EdgeSet, WeightHash, Type, Weight) :-
	(Type = glb ->
		(
		 glb(VertexSet, GLBVertex),
		 glb(EdgeSet, GLBEdge),
		 set_weight(GLBVertex, GLBEdge, WeightHash, MinWeight),
		 Weight #>= MinWeight
		);
		(
		 lub(VertexSet, LUBVertex),
		 lub(EdgeSet, LUBEdge),
		 set_weight(LUBVertex, LUBEdge, WeightHash, MaxWeight),
		 Weight #<= MaxWeight
		)
	).	

:- comment(
	predecessors/3,
	[
		amode: predecessors(+,+,?),
		args:
		[
			"Graph": "A graph.",
			"Vertex": "A vertex of Graph.",
			"PredSet": "Set of predecessors of Vertex in Graph."
		],
		summary: "Determines the predecessor-set of a vertex in a graph.",
		desc: html("Determines the predecessor-set of a vertex in a graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable,
			 if Vertex does not belong to Graph\'s vertex-set or
			 if Graph can not be constrained to have the vertices in PredSet as predecessors of Vertex.
			",
		eg:
			"
?- predecessors(G,V,Preds).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), predecessors(G,4,Preds).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), predecessors(G,2,[1,3,4]).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), predecessors(G,2,Preds).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2921-susp', 'SUSP-_2484-susp'], ['SUSP-_3073-susp'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3506-susp', 'SUSP-_2494-susp'], ['SUSP-_3518-susp'], ['SUSP-_1802-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2921-susp', 'SUSP-_2484-susp'], ['SUSP-_3073-susp'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3506-susp', 'SUSP-_2494-susp'], ['SUSP-_3518-susp'], ['SUSP-_1802-dead'])})
Preds = Preds{cardinal([[1] : 1, [3] : 2], PredCard{cardinal : _2731, fd : [1, 2]}, _2623, _2624, _2625, [], ['SUSP-_3530-susp', 'SUSP-_2803-susp'], ['SUSP-_3542-susp'], ['SUSP-_3073-susp'])}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), predecessors(G,2,[1]).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _652, fd : [2, 3]}, _544, _545, _546, [], [], ['SUSP-_2798-dead', 'SUSP-_2489-susp'], ['SUSP-_2879-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _850, fd : [1 .. 4]}, _742, _743, _744, [], ['SUSP-_2939-susp', 'SUSP-_2499-susp'], ['SUSP-_2951-susp'], ['SUSP-_1807-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _652, fd : [2, 3]}, _544, _545, _546, [], [], ['SUSP-_2798-dead', 'SUSP-_2489-susp'], ['SUSP-_2879-dead'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _850, fd : [1 .. 4]}, _742, _743, _744, [], ['SUSP-_2939-susp', 'SUSP-_2499-susp'], ['SUSP-_2951-susp'], ['SUSP-_1807-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), predecessors(G,2,[1,3]).
V = [1, 2, 3]
E = E{cardinal([[[1, 2], [3, 2]]:2, [[2, 3], [3, 1]]:4], Card{cardinal : _860, fd:[2..4]}, _752, _753, _754, [], ['SUSP-_4324-susp', 'SUSP-_2509-susp'], ['SUSP-_4336-susp'], ['SUSP-_4141-dead'])}
G = dirgraph([1, 2, 3], E{cardinal([[[1, 2], [3, 2]]:2, [[2, 3], [3, 1]]:4], Card{cardinal : _860, fd:[2..4]}, _752, _753, _754, [], ['SUSP-_4324-susp', 'SUSP-_2509-susp'], ['SUSP-_4336-susp'], ['SUSP-_4141-dead'])})
			"
	]
).

predecessors(Graph, _, _) :-
	var(Graph),!,fail.
predecessors(dirgraph(VertexSet,EdgeSet), Vertex, PredSet) :-
	preds_aux(VertexSet, EdgeSet, Vertex, PredSet).
predecessors(undirgraph(VertexSet,EdgeSet), Vertex, PredSet) :-
	preds_aux(VertexSet, EdgeSet, Vertex, PredSet).

preds_aux(VertexSet, _, Vertex, _) :-
	lub(VertexSet, LUBVertex),
	\+ord_memberchk(Vertex,LUBVertex),
	!,fail.
preds_aux(VertexSet, EdgeSet, Vertex, PredSet) :-
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	findall(PredVertex, member([PredVertex, Vertex], GLBEdge), GLBPred),
	findall(PredVertex, member([PredVertex, Vertex], PossEdge), PossPred),
	
	(var(PredSet) ->
		(
		 PredSet`::GLBPred+PossPred,
		 PredSet `< VertexSet
		);
		(
		 PredSet`::GLBPred+PossPred,
		 PredSet `< VertexSet,
		 add_elem_add_arc(Vertex, preds, EdgeSet, PredSet),
		 rem_elem_rem_arc(Vertex, preds, EdgeSet, PredSet)
		)
	),
	
	cardinality(PredSet, PredCard),
	cardinality(EdgeSet, EdgeCard),
	PredCard #<= EdgeCard,
	
	suspend(add_arc_add_elem(Vertex, preds, EdgeSet, PredSet), 9, EdgeSet->cardinal:glb, EGLBSusp),
	suspend(rem_arc_rem_elem(Vertex, preds, EdgeSet, PredSet), 9, EdgeSet->cardinal:lub, ELUBSusp),
	suspend(add_elem_add_arc(Vertex, preds, EdgeSet, PredSet), 9, PredSet->cardinal:glb, PGLBSusp),
	suspend(rem_elem_rem_arc(Vertex, preds, EdgeSet, PredSet), 9, PredSet->cardinal:lub, PLUBSusp),
	terminate_susps(kill_susps([EGLBSusp,ELUBSusp,PGLBSusp,PLUBSusp]), [EdgeSet,PredSet]).

:- comment(
	successors/3,
	[
		amode: successors(+,+,?),
		args:
		[
			"Graph": "A graph.",
			"Vertex": "A vertex of Graph.",
			"SuccSet": "Set of successors of Vertex in Graph."
		],
		summary: "Determines the successor-set of a vertex in a graph.",
		desc: html("Determines the successor-set of a vertex in a graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable,
			 if Vertex does not belong to Graph\'s vertex-set or
			 if Graph can not be constrained to have the vertices in SuccSet as successors of Vertex.
			",
		eg:
			"
?- successors(G,V,Succs).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), successors(G,4,Succs).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), successors(G,3,[1,3,4]).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), successors(G,3,Succs).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2921-susp', 'SUSP-_2484-susp'], ['SUSP-_3073-susp'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3359-susp', 'SUSP-_2494-susp'], ['SUSP-_3371-susp'], ['SUSP-_1802-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2921-susp', 'SUSP-_2484-susp'], ['SUSP-_3073-susp'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3359-susp', 'SUSP-_2494-susp'], ['SUSP-_3371-susp'], ['SUSP-_1802-dead'])})
Succs = Succs{cardinal([[] : 0, [1, 2] : 2], SuccCard{cardinal : _2731, fd : [0 .. 2]}, _2623, _2624, _2625, [], ['SUSP-_3383-susp', 'SUSP-_2803-susp'], ['SUSP-_3395-susp'], ['SUSP-_3073-susp'])}
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), successors(G,3,[1]).
V = [1, 2, 3]
E = E{cardinal([[[1, 2], [3, 1]]:2, [[2, 3]]:3], Card{cardinal : _850, fd:[2, 3]}, _742, _743, _744, [], ['SUSP-_4597-susp', 'SUSP-_2499-susp'], ['SUSP-_4609-susp'], ['SUSP-_4406-dead'])}
G = dirgraph([1, 2, 3], E{cardinal([[[1, 2], [3, 1]]:2, [[2, 3]]:3], Card{cardinal : _850, fd:[2, 3]}, _742, _743, _744, [], ['SUSP-_4597-susp', 'SUSP-_2499-susp'], ['SUSP-_4609-susp'], ['SUSP-_4406-dead'])})
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), successors(G,3,[1,2]).
V = [1, 2, 3]
E = E{cardinal([[[1, 2], [3, 1], [3, 2]]:3, [[2, 3]]:4], Card{cardinal : _860, fd:[3, 4]}, _752, _753, _754, [], ['SUSP-_5007-susp', 'SUSP-_2509-susp'], ['SUSP-_5019-susp'], ['SUSP-_4830-dead'])}
G = dirgraph([1, 2, 3], E{cardinal([[[1, 2], [3, 1], [3, 2]]:3, [[2, 3]]:4], Card{cardinal : _860, fd:[3, 4]}, _752, _753, _754, [], ['SUSP-_5007-susp', 'SUSP-_2509-susp'], ['SUSP-_5019-susp'], ['SUSP-_4830-dead'])})			"
	]
).

successors(Graph, _, _) :-
	var(Graph),!,fail.
successors(dirgraph(VertexSet, EdgeSet), Vertex, SuccSet) :-
	succs_aux(VertexSet, EdgeSet, Vertex, SuccSet).	
successors(undirgraph(VertexSet, EdgeSet), Vertex, SuccSet) :-
	succs_aux(VertexSet, EdgeSet, Vertex, SuccSet).
	
succs_aux(VertexSet, _, Vertex, _) :-
	lub(VertexSet, LUBVertex),
	\+ord_memberchk(Vertex,LUBVertex),
	!,fail.	
succs_aux(VertexSet, EdgeSet, Vertex, SuccSet) :-
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	findall(SuccVertex, member([Vertex, SuccVertex], GLBEdge), GLBSucc),
	findall(SuccVertex, member([Vertex, SuccVertex], PossEdge), PossSucc),
	
	(var(SuccSet) ->
		(
		 SuccSet`::GLBSucc+PossSucc,
		 SuccSet `< VertexSet
		);
		(
		 SuccSet`::GLBSucc+PossSucc,
		 SuccSet `< VertexSet,
		 add_elem_add_arc(Vertex, succs, EdgeSet, SuccSet),
		 rem_elem_rem_arc(Vertex, succs, EdgeSet, SuccSet)
		)
	),

	cardinality(SuccSet, SuccCard),
	cardinality(EdgeSet, EdgeCard),
	SuccCard #<= EdgeCard,
	
	suspend(add_arc_add_elem(Vertex, succs, EdgeSet, SuccSet), 9, EdgeSet->cardinal:glb, EGLBSusp),
	suspend(rem_arc_rem_elem(Vertex, succs, EdgeSet, SuccSet), 9, EdgeSet->cardinal:lub, ELUBSusp),
	suspend(add_elem_add_arc(Vertex, succs, EdgeSet, SuccSet), 9, SuccSet->cardinal:glb, SGLBSusp),
	suspend(rem_elem_rem_arc(Vertex, succs, EdgeSet, SuccSet), 9, SuccSet->cardinal:lub, SLUBSusp),
	terminate_susps(kill_susps([EGLBSusp,ELUBSusp,SGLBSusp,SLUBSusp]), [EdgeSet,SuccSet]).

%%%
% - The indication of a precedence between X and Y imposes the addition of the edge (X,Y) to the graph
% - The indication of a sucession between Y and X imposes the addition of the edge (X,Y) to the graph
% - O(m + n)
%%%

:- demon add_elem_add_arc/4.
	
add_elem_add_arc(Vertex, Type, EdgeSet, List) :-
	glb(List, GLBList),
	(Type == preds ->
		findall([Pred,Vertex], member(Pred, GLBList), SubEdgeSet);
		findall([Vertex,Succ], member(Succ, GLBList), SubEdgeSet)
	),
	SubEdgeSet `< EdgeSet.

%%%
% - The indication of a non-precedence between X and Y imposes the removal of the edge (X,Y) from the graph
% - The indication of a non-succession between Y and X imposes the removal of the edge (X,Y) from the graph
% - O(m + n)
%%%

:- demon rem_elem_rem_arc/4.

rem_elem_rem_arc(Vertex, Type, EdgeSet, List) :-
	lub(List, LUBList),
	hash_create(ListHash),
	hash_add_all(ListHash, LUBList),
	lub(EdgeSet, LUBEdge),
	(Type == preds ->
		findall([Pred,Vertex], (member([Pred,Vertex], LUBEdge), \+hash_contains(ListHash,Pred)), NotPossEdges);
		findall([Vertex,Succ], (member([Vertex,Succ], LUBEdge), \+hash_contains(ListHash,Succ)), NotPossEdges)
	),
	NotPossEdges `$ EdgeSet.

%%%
% - The indication of the occurence of the arc (X,Y) in the graph imposes the addition of X to the predecessors of Y
% - The indication of the occurence of the arc (X,Y) in the graph imposes the addition of Y to the successors of X
% - O(m + n)
%%%

:- demon add_arc_add_elem/4.
	
add_arc_add_elem(Vertex, Type, EdgeSet, List) :-
	glb(EdgeSet, GLBEdge),
	(Type = preds ->
		findall(Pred, member([Pred,Vertex], GLBEdge), SubList);
		findall(Succ, member([Vertex,Succ], GLBEdge), SubList)),
	SubList `< List.

%%%
% - The indication of the non-occurence of the arc (X,Y) in the graph imposes the removal of X from the predecessors of Y
% - The indication of the non-occurence of the arc (X,Y) in the graph imposes the removal of Y from the successors of X
% - % O(m + n)
%%%

:- demon rem_arc_rem_elem/4.

rem_arc_rem_elem(Vertex, Type, EdgeSet, List) :-
	lub(EdgeSet, LUBEdge),
	(Type == preds ->
		findall(Pred, member([Pred,Vertex], LUBEdge), PossList);
		findall(Succ, member([Vertex,Succ], LUBEdge), PossList)),
	List `< PossList.

:- comment(
	reachables/3,
	[
		amode: reachables(+,+,?),
		args:
		[
			"Graph": "A graph.",
			"SourceVertex": "A vertex of Graph.",
			"ReachSet": "Set of vertices reachable from SourceVertex in Graph."
		],
		summary: "Determines the reachables-set of a vertex in a graph.",
		desc: html("Determines the reachables-set of a vertex in a graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable,
			 if SourceVertex does not belong to Graph\'s vertex-set or
			 if Graph can not be constrained to have the vertices in ReachSet reachable from Vertex.
			",
		eg:
			"
?- reachables(G,V,Reachs).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), reachables(G,4,Reachs).
No.
 
?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), reachables(G,3,[1,3,4]).
No.

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), reachables(G,3,Reachs).
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2484-susp'], ['SUSP-_2108-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3145-susp', 'SUSP-_2494-susp'], ['SUSP-_3133-susp'], ['SUSP-_1802-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _647, fd : [2, 3]}, _539, _540, _541, [], [], ['SUSP-_2484-susp'], ['SUSP-_2108-dead'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 1], [3, 2]] : 4], Card{cardinal : _845, fd : [1 .. 4]}, _737, _738, _739, [], ['SUSP-_3145-susp', 'SUSP-_2494-susp'], ['SUSP-_3133-susp'], ['SUSP-_1802-dead'])})
Reachs = Reachs{cardinal([[3] : 1, [1, 2] : 3], Card{cardinal : _3086, fd : [1 .. 3]}, _2978, _2979, _2980, [], ['SUSP-_3121-susp'], ['SUSP-_3110-susp'], [])}

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), reachables(G,3,Reachs), 1 `@ Reachs.
V = [1, 2, 3]
E = E{cardinal([[[1, 2], [3, 1]] : 2, [[2, 3], [3, 2]] : 4], Card{cardinal : _873, fd : [2 .. 4]}, _765, _766, _767, [], ['SUSP-_3173-susp', 'SUSP-_2522-susp'], ['SUSP-_5570-dead', 'SUSP-_3161-susp'], ['SUSP-_5651-dead'])}
G = dirgraph([1, 2, 3], E{cardinal([[[1, 2], [3, 1]] : 2, [[2, 3], [3, 2]] : 4], Card{cardinal : _873, fd : [2 .. 4]}, _765, _766, _767, [], ['SUSP-_3173-susp', 'SUSP-_2522-susp'], ['SUSP-_5570-dead', 'SUSP-_3161-susp'], ['SUSP-_5651-dead'])})
Reachs = [1, 2, 3]

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1],[3,2]], dirgraph(G,V,E), reachables(G,3,Reachs), 1 `-@ Reachs.
V = V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _675, fd : [2, 3]}, _567, _568, _569, [], [], ['SUSP-_2512-susp'], ['SUSP-_2136-dead'])}
E = E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 2]] : 3], Card{cardinal : _873, fd : [1 .. 3]}, _765, _766, _767, [], ['SUSP-_3480-dead', 'SUSP-_3173-susp', 'SUSP-_2522-susp'], ['SUSP-_3161-susp'], ['SUSP-_3665-dead'])}
G = dirgraph(V{cardinal([[1, 2] : 2, [3] : 3], Card{cardinal : _675, fd : [2, 3]}, _567, _568, _569, [], [], ['SUSP-_2512-susp'], ['SUSP-_2136-dead'])}, E{cardinal([[[1, 2]] : 1, [[2, 3], [3, 2]] : 3], Card{cardinal : _873, fd : [1 .. 3]}, _765, _766, _767, [], ['SUSP-_3480-dead', 'SUSP-_3173-susp', 'SUSP-_2522-susp'], ['SUSP-_3161-susp'], ['SUSP-_3665-dead'])})
Reachs = Reachs{cardinal([[3] : 1, [2] : 2], Card{cardinal : _3114, fd : [1, 2]}, _3006, _3007, _3008, [], ['SUSP-_4091-dead', 'SUSP-_3149-susp'], ['SUSP-_3138-susp'], ['SUSP-_4317-dead'])}
			"
	]
).

reachables(Graph, _, _) :-
	var(Graph),!,fail.
reachables(dirgraph(VertexSet,EdgeSet), SourceVertex, ReachSet) :-
	reachables_aux(VertexSet, EdgeSet, SourceVertex, ReachSet).
reachables(undirgraph(VertexSet,EdgeSet), SourceVertex, ReachSet) :-
	reachables_aux(VertexSet, EdgeSet, SourceVertex, ReachSet).

reachables_aux(VertexSet, _, SourceVertex, _) :-
	lub(VertexSet, LUBVertex),
	\+ord_memberchk(SourceVertex,LUBVertex),
	!,fail.
reachables_aux(VertexSet, EdgeSet, SourceVertex, ReachSet) :-
	lub(VertexSet, GLBVertex, _, LUBVertex),
	lub(EdgeSet, GLBEdge, _, LUBEdge),	
	reach(GLBVertex, GLBEdge, SourceVertex, GLBReachSet),
	reach(LUBVertex, LUBEdge, SourceVertex, LUBReachSet),
	ReachSet`::GLBReachSet..LUBReachSet,
	((ground(ReachSet), ground(EdgeSet)) ->
		true;
		(suspend(rem_reach_vertex_rem_edge(SourceVertex, EdgeSet, ReachSet), 10, ReachSet->cardinal:lub, RLUBSusp),
		 suspend(add_reach_vertex_add_edge(SourceVertex, EdgeSet, ReachSet), 10, ReachSet->cardinal:glb, RGLBSusp),
		 suspend(rem_edge_rem_reach_vertex(SourceVertex, VertexSet, EdgeSet, ReachSet), 10, EdgeSet->cardinal:lub, ELUBSusp),
		 suspend(add_edge_add_reach_vertex(SourceVertex, VertexSet, EdgeSet, ReachSet), 10, EdgeSet->cardinal:glb, EGLBSusp),
		 terminate_susps(kill_susps([RGLBSusp,RLUBSusp,EGLBSusp,ELUBSusp]), [ReachSet,EdgeSet])
		)
	).

reach(_, Arcs, Source, OrdReachNodes) :-
	hash_create(HashNodes),
	arc_list_to_adj_hash(Arcs, HashArcs),
	reach_aux([Source], HashNodes, HashArcs, [], ReachNodes),
	list_to_ord_set(ReachNodes, OrdReachNodes).

reach_aux([], _, _, ReachNodes, ReachNodes).
reach_aux([Node|Nodes], HashNodes, HashArcs, AcumReachs, ReachNodes) :-
	(hash_contains(HashNodes, Node) ->
		(NewNodes = Nodes, NewAcumReachs = AcumReachs);
		(
		 (
		  hash_get(HashArcs, Node, NextNodes); NextNodes = []),!,
		  append(NextNodes, Nodes, NewNodes),
		  NewAcumReachs = [Node|AcumReachs],
		  hash_add(HashNodes, Node, x)
		)
	),
	reach_aux(NewNodes, HashNodes, HashArcs, NewAcumReachs, ReachNodes).

%%%	
% - If Y is not reachable from X then the edge (X,Y) cannot occur in the graph
% - O(m + n)
%%%

:- demon rem_reach_vertex_rem_edge/3.	

rem_reach_vertex_rem_edge(SourceVertex, EdgeSet, ReachSet) :-
	lub(ReachSet, LUBReach),
	hash_create(ReachHash),
	hash_add_all(ReachHash, LUBReach),
	lub(EdgeSet, LUBEdge),
	findall([SourceVertex,Succ], (member([SourceVertex,Succ], LUBEdge), \+hash_contains(ReachHash, Succ)), NotPossEdges),
	NotPossEdges `$ EdgeSet.

%%%
% - The addition of a reachable vertex may impose the addition of an edge to the graph
% - O(m + n)
%%%

:- demon add_reach_vertex_add_edge/3.		
	
add_reach_vertex_add_edge(SourceVertex, EdgeSet, ReachSet) :-
	lub(ReachSet, GLBReach, _, LUBReach),
	findall([SourceVertex,Vertex], member(Vertex,GLBReach), Edges),
	hash_create(EdgeHash),
	hash_add_all(EdgeHash,Edges),
	hash_create(ReachHash),
	hash_add_all(ReachHash,LUBReach),
	lub(EdgeSet, LUBEdgeSet),
	prune_edges(LUBEdgeSet,SourceVertex,EdgeHash,ReachHash),
	hash_list(EdgeHash,SubEdgeSet,_),
	SubEdgeSet `< EdgeSet.
	
prune_edges([], _, _, _).
prune_edges([[X,Y]|Edges],SourceVertex,EdgeHash,ReachHash) :-
	(
	  (hash_contains(ReachHash,X),X \== SourceVertex, hash_contains(ReachHash,Y),hash_contains(EdgeHash,[SourceVertex,Y])) ->
	  hash_remove(EdgeHash,[SourceVertex,Y],_);
	  true
	),
	prune_edges(Edges,SourceVertex,EdgeHash,ReachHash).	
	
%%%
% - The addition of an edge may impose the addition of several vertices to a reachables-set
% - O(n * (m + n))
%%%

:- demon add_edge_add_reach_vertex/4.

add_edge_add_reach_vertex(SourceVertex, VertexSet, EdgeSet, ReachSet) :-
	glb(VertexSet, GLBVertex),
	glb(EdgeSet, GLBEdge),
	reach(GLBVertex, GLBEdge, SourceVertex, SubReachSet),
	SubReachSet `< ReachSet.

%%%
% - The removal of an edge may impose the removal of several vertices from a reachables-set
% - O(n * (m + n))
%%%

:- demon rem_edge_rem_reach_vertex/4.

rem_edge_rem_reach_vertex(SourceVertex, VertexSet, EdgeSet, ReachSet) :-
	lub(VertexSet, LUBVertex),
	lub(EdgeSet, LUBEdge),
	reach(LUBVertex, LUBEdge, SourceVertex, PossReachSet),
	ReachSet `< PossReachSet.

:- comment(
	connected/1,
	[
		amode: connected(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Guarantees that an undirected graph Graph is connected.",
		desc: html("Guarantees that an undirected graph Graph is connected, i.e., that each vertex is reachable from any other one."),
		fail_if:
			"Fails 
			 if Graph is not an undirected graph variable or
			 if Graph can not be constrained to be connected.
			",
		eg:
			"
?- connected(G).
No.

?- V`::[1,2]..[1,2,3,4], E`::[]..[[1,3],[2,4],[3,1],[4,2]], undirgraph(G,V,E), connected(G).
No.

?- V`::[1,2]..[1,2,3], E`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], undirgraph(G,V,E), connected(G), graph_labeling(G).
V = [1, 2]
E = [[1, 2], [2, 1]]
G = undirgraph([1, 2], [[1, 2], [2, 1]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 3], [2, 3], [3, 1], [3, 2]]
G = undirgraph([1, 2, 3], [[1, 3], [2, 3], [3, 1], [3, 2]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [2, 1], [2, 3], [3, 2]]
G = undirgraph([1, 2, 3], [[1, 2], [2, 1], [2, 3], [3, 2]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [1, 3], [2, 1], [3, 1]]
G = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [3, 1]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
G = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]])
Yes
			"
	]
).

connected(Graph) :-
	var(Graph),!,fail.
connected(undirgraph(VertexSet,EdgeSet)) :-
	strongly_connected_aux(VertexSet,EdgeSet).

:- comment(
	strongly_connected/1,
	[
		amode: strongly_connected(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Guarantees that a directed graph Graph is strongly connected.",
		desc: html("Guarantees that a directed graph Graph is strongly connected, i.e., that each vertex is reachable from any other one."),
		fail_if:
			"Fails 
			 if Graph is not a directed graph variable or
			 if Graph can not be constrained to be strongly connected.
			",
		eg:
			"
?- strongly_connected(G).
No.

?- V`::[1,2]..[1,2,3,4], E`::[]..[[1,3],[2,4],[4,1],[4,3]], dirgraph(G,V,E), strongly_connected(G).
No.

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), strongly_connected(G), graph_labeling(G).
V = []
E = []
G = dirgraph([], [])
Yes ? ;

V = [3]
E = []
G = dirgraph([3], [])
Yes ? ;

V = [2]
E = []
G = dirgraph([2], [])
Yes ? ;

V = [1]
E = []
G = dirgraph([1], [])
Yes ? ;

V = [1, 2]
E = [[1, 2], [2, 1]]
G = dirgraph([1, 2], [[1, 2], [2, 1]])
Yes
			"
	]
).

strongly_connected(Graph) :-
	var(Graph),!,fail.
strongly_connected(dirgraph(VertexSet,EdgeSet)) :-
	strongly_connected_aux(VertexSet, EdgeSet).

strongly_connected_aux(VertexSet, EdgeSet) :-
	glb(VertexSet, GLBVertex),
	hash_create(ReachHash),
	glb_strongly_connected(VertexSet, EdgeSet, GLBVertex, ReachHash),
	suspend(add_vertex_add_reach_vertices(VertexSet, EdgeSet, ReachHash), 10, VertexSet->cardinal:glb, Susp),
	terminate_susps(kill_susps([Susp]), [VertexSet]).
	
glb_strongly_connected(_, _, [], _).
glb_strongly_connected(VertexSet, EdgeSet, [Vertex|NextVertices], ReachHash) :-
	reachables_aux(VertexSet, EdgeSet, Vertex, ReachSet),
	ReachSet `= VertexSet,
	hash_add(ReachHash, Vertex, ReachSet),
	glb_strongly_connected(VertexSet, EdgeSet, NextVertices, ReachHash).
	
%%%
% - In a connected graph, adding a vertex to the graph imposes that that vertex must be reachable from all the other vertices and reach all the other vertices
% - O(n^2 * (m + n))
%%%

:- demon add_vertex_add_reach_vertices/3.

add_vertex_add_reach_vertices(VertexSet, EdgeSet, ReachHash) :-
	glb(VertexSet, GLBVertex),
	findall(Vertex, (member(Vertex, GLBVertex), \+hash_contains(ReachHash, Vertex)), NewVertices),
	glb_strongly_connected(VertexSet, EdgeSet, NewVertices, ReachHash).

:- comment(
	weakly_connected/1,
	[
		amode: weakly_connected(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Guarantees that a directed graph Graph is weakly connected.",
		desc: html("Guarantees that a directed graph Graph is weakly connected, i.e., that each vertex is reachable from any other one in the underlying graph of Graph."),
		fail_if:
			"Fails 
			 if Graph is not a directed graph variable or
			 if Graph can not be constrained to be strongly connected.
			",
		eg:
			"
?- weakly_connected(G).
No.

?- V`::[1,2]..[1,2,3,4], E`::[]..[[1,3],[2,4],[3,1],[4,2]], dirgraph(G,V,E), weakly_connected(G).
No.

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), weakly_connected(G), graph_labeling(G).
V = []
E = []
G = dirgraph([], [])
Yes ? ;

V = [3]
E = []
G = dirgraph([3], [])
Yes ? ;

V = [2]
E = []
G = dirgraph([2], [])
Yes ? ;

V = [1]
E = []
G = dirgraph([1], [])
Yes ? ;

V = [1, 2]
E = [[2, 1]]
G = dirgraph([1, 2], [[2, 1]])
Yes ? ;

V = [1, 2]
E = [[1, 2]]
G = dirgraph([1, 2], [[1, 2]])
Yes ? ;

V = [1, 2]
E = [[1, 2], [2, 1]]
G = dirgraph([1, 2], [[1, 2], [2, 1]])
Yes
			"
	]
).

weakly_connected(Graph) :-
	var(Graph),!,fail.
weakly_connected(dirgraph(VertexSet,EdgeSet)) :-
	underlying_graph(dirgraph(VertexSet,EdgeSet),undirgraph(UndirVertexSet,UndirEdgeSet)),
	strongly_connected_aux(UndirVertexSet, UndirEdgeSet).

:- comment(
	path/3,
	[
		amode: path(+,+,+),
		args:
		[
			"Graph": "A graph.",
			"OriginVertex": "Initial vertex in the path between OriginVertex and TerminusVertex in Graph.",
			"TerminusVertex": "Final vertex in the path between OriginVertex and TerminusVertex in Graph."
		],
		summary: "Ensures Graph represents a path between OriginVertex and TerminusVertex in Graph.",
		desc: html("Ensures Graph represents a path between OriginVertex and TerminusVertex in Graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable,
			 if OriginVertex does not belong to Graph\'s vertex-set,
			 if TerminusVertex does not belong to Graph\'s vertex-set or
			 if Graph can not be constrained to define a path between OriginVertex and TerminusVertex.
			",
		eg:
			"
?- path(G,Origin,Terminus).
No.

?- V`::[]..[1,2,3,4], E`::[]..[[1,2],[1,3],[1,4],[2,4],[3,4]], dirgraph(G,V,E), path(G, 1, 5).
No.

?- V`::[]..[1,2,3,4], E`::[]..[[1,2],[1,3],[1,4],[2,4],[3,4]], dirgraph(G,V,E), path(G, 5, 1).
No.

?- V`::[]..[1,2,3,4], E`::[]..[[1,2],[1,3],[1,4],[2,4],[3,4]], dirgraph(G,V,E), path(G, 4, 1).
No.

?- V`::[]..[1,2,3,4], E`::[]..[[1,2],[1,3],[1,4],[2,4],[3,4]], dirgraph(G,V,E), path(G, 1, 4), graph_labeling(G).
V = [1, 4]
E = [[1, 4]]
G = dirgraph([1, 4], [[1, 4]])
Yes ? ;

V = [1, 3, 4]
E = [[1, 3], [3, 4]]
G = dirgraph([1, 3, 4], [[1, 3], [3, 4]])
Yes ? ;

V = [1, 2, 4]
E = [[1, 2], [2, 4]]
G = dirgraph([1, 2, 4], [[1, 2], [2, 4]])
Yes

?- V`::[]..[1,2,3,4], E`::[]..[[1,2],[1,3],[1,4],[2,1],[2,4],[3,1],[3,4],[4,1],[4,2],[4,3]], undirgraph(G,V,E), path(G, 1, 4), graph_labeling(G).
V = [1, 3, 4]
E = [[1, 3], [3, 1], [3, 4], [4, 3]]
G = undirgraph([1, 3, 4], [[1, 3], [3, 1], [3, 4], [4, 3]])
Yes ? ;

V = [1, 3, 4]
E = [[1, 3], [1, 4], [3, 1], [3, 4], [4, 1], [4, 3]]
G = undirgraph([1, 3, 4], [[1, 3], [1, 4], [3, 1], [3, 4], [4, 1], [4, 3]])
Yes ? ;

V = [1, 2, 4]
E = [[1, 2], [2, 1], [2, 4], [4, 2]]
G = undirgraph([1, 2, 4], [[1, 2], [2, 1], [2, 4], [4, 2]])
Yes ? ;

V = [1, 2, 4]
E = [[1, 2], [1, 4], [2, 1], [2, 4], [4, 1], [4, 2]]
G = undirgraph([1, 2, 4], [[1, 2], [1, 4], [2, 1], [2, 4], [4, 1], [4, 2]])
Yes ? ;

V = [1, 2, 3, 4]
E = [[1, 2], [1, 3], [2, 1], [2, 4], [3, 1], [3, 4], [4, 2], [4, 3]]
G = undirgraph([1, 2, 3, 4], [[1, 2], [1, 3], [2, 1], [2, 4], [3, 1], [3, 4], [4, 2], [4, 3]])
Yes
			"
	]
).

path(Graph, _, _) :-
	var(Graph),!,fail.
path(dirgraph(VertexSet,EdgeSet), OriginVertex, TerminusVertex) :-
	quasipath(dirgraph(VertexSet,EdgeSet), OriginVertex, TerminusVertex),
	weakly_connected(dirgraph(VertexSet,EdgeSet)).
path(undirgraph(VertexSet,EdgeSet), OriginVertex, TerminusVertex) :-
	quasipath(undirgraph(VertexSet,EdgeSet), OriginVertex, TerminusVertex),
	connected(undirgraph(VertexSet,EdgeSet)).

quasipath(dirgraph(VertexSet, EdgeSet), OriginVertex, TerminusVertex) :-
	quasipath_aux(VertexSet, EdgeSet, OriginVertex, TerminusVertex, 1).
quasipath(undirgraph(VertexSet, EdgeSet), OriginVertex, TerminusVertex) :-
	quasipath_aux(VertexSet, EdgeSet, OriginVertex, TerminusVertex, 2).

quasipath_aux(VertexSet, EdgeSet, OriginVertex, TerminusVertex, Max) :-
	OriginVertex `@ VertexSet,
	TerminusVertex `@ VertexSet,
	glb_poss(VertexSet, GLBVertex, PossVertex),
	hash_create(CardHash),
	glb_quasipath(VertexSet, EdgeSet, GLBVertex, OriginVertex, TerminusVertex, 0, Max, CardHash),
	poss_quasipath(VertexSet, EdgeSet, PossVertex, OriginVertex, TerminusVertex, 0, Max, CardHash),
	cardinality(VertexSet, VertexCard),
	cardinality(EdgeSet, EdgeCard),
	EdgeCard #>= (Max * (VertexCard - 1)),
	EdgeCard #<= (Max * VertexCard),
	suspend(add_vertex_inc_card(VertexSet, OriginVertex, TerminusVertex, CardHash), 9, VertexSet->cardinal:glb, VGLBSusp),		 
	suspend(dec_card_rem_vertex(VertexSet, OriginVertex, TerminusVertex, CardHash), 9, EdgeSet->cardinal:lub, ELUBSusp),
	terminate_susps(kill_susps([VGLBSusp,ELUBSusp]), [VertexSet,EdgeSet]).

glb_quasipath(_, _, [], _, _, _, _, _).
glb_quasipath(VertexSet, EdgeSet, [Vertex|NextVertices], OriginVertex, TerminusVertex, Min, Max, CardHash) :-
	preds_aux(VertexSet, EdgeSet, Vertex, PredSet),
	cardinality(PredSet, PredCard),
	succs_aux(VertexSet, EdgeSet, Vertex, SuccSet),
	cardinality(SuccSet, SuccCard),
	(Vertex = OriginVertex ->
		(PredCard #>= Min, PredCard #<= Max, SuccCard #> Min, SuccCard #<= Max, Card = SuccCard);
		true
	),
	(Vertex = TerminusVertex ->
		(PredCard #> Min, PredCard #<= Max, SuccCard #>= Min, SuccCard #<= Max, Card = PredCard);
		true
	),
	(Vertex \= OriginVertex, Vertex \= TerminusVertex ->
		(PredCard #= Max, SuccCard #= Max, Card = Max);
		true
	),
	hash_add(CardHash, Vertex, Card),
	glb_quasipath(VertexSet, EdgeSet, NextVertices, OriginVertex, TerminusVertex, Min, Max, CardHash).

poss_quasipath(VertexSet, EdgeSet, VertexList, OriginVertex, TerminusVertex, Min, Max, CardHash) :-
	poss_quasipath_aux(VertexSet, EdgeSet, VertexList, OriginVertex, TerminusVertex, Min, Max, CardHash, [], AddList, [], RemList),
	AddList `< VertexSet,
	RemList `$ VertexSet.
	
poss_quasipath_aux(_, _, [], _, _, _, _, _,AddList,AddList,RemList,RemList).
poss_quasipath_aux(VertexSet, EdgeSet, [Vertex|NextVertices], OriginVertex, TerminusVertex, Min, Max, CardHash, AcumAddList, AddList, AcumRemList, RemList) :-
	preds_aux(VertexSet, EdgeSet, Vertex, PredSet),
	cardinality(PredSet, Card),
	succs_aux(VertexSet, EdgeSet, Vertex, SuccSet),
	cardinality(SuccSet, Card),
	Card::[Min,Max],
	mindomain(Card, CardMin),
	(CardMin > Min ->
		(NAcumAddList = [Vertex|AcumAddList], NAcumRemList = AcumRemList, Card = Max);
		(
		 maxdomain(Card, CardMax),
		 (CardMax < Max ->
			(NAcumRemList = [Vertex|AcumRemList], NAcumAddList = AcumAddList, Card = Min);
			(NAcumAddList = AcumAddList, NAcumRemList = AcumRemList)
		 )
		)
	),
	hash_add(CardHash, Vertex, Card),
	poss_quasipath_aux(VertexSet, EdgeSet, NextVertices, OriginVertex, TerminusVertex, Min, Max, CardHash, NAcumAddList, AddList, NAcumRemList, RemList).

%%%
% - If a vertex is added to a path then it must have exactly the same number of predecessors and sucessors, namely 1 if the graph is directed and 2 if it is undirected
% - O(n)
%%%
	
:- demon add_vertex_inc_card/4.

add_vertex_inc_card(VertexSet, OriginVertex, TerminusVertex, CardHash) :-
	glb(VertexSet, GLBVertex),
	findall(Vertex, (member(Vertex, GLBVertex), Vertex \= OriginVertex, Vertex \= TerminusVertex), InVertices),
	not_isolated(InVertices, CardHash).

not_isolated([], _).
not_isolated([Elem|Elems], Hash) :-
	hash_find(Hash, Elem, Card),
	Card #> 0,
	not_isolated(Elems, Hash).

%%%
% - If the cardinality of a list (predecessors or successors) decreases bellow a given number then the corresponding vertex must be removed from the graph
% - O(n)
%%%	

:- demon dec_card_rem_vertex/4.

dec_card_rem_vertex(VertexSet, _, _, CardHash) :-
	lub(VertexSet, LUBVertex),
	findall(Vertex, (member(Vertex, LUBVertex), hash_find(CardHash, Vertex, Card), Card \== 0), PossVertexSet),
	VertexSet `< PossVertexSet.

cycle(Graph, OriginVertex, TerminusVertex) :-
	path(Graph, OriginVertex, TerminusVertex),
	getEdgeSet(Graph, EdgeSet),
	[TerminusVertex,OriginVertex] `@ EdgeSet.
	
:- comment(
	underlying_graph/2,
	[
		amode: underlying_graph(+,?),
		args:
		[
			"DirectedGraph": "A directed graph.",
			"UndirectedGraph": "The underlying undirected graph of DirectedGraph."
		],
		summary: "Obtains an underlying graph of a given directed graph.",
		desc: html("Obtains an underlying graph of a given directed graph."),
		fail_if:
			"Fails 
			 if DirectedGraph is not a directed graph variable or
			 if DirectedGraph can not be contrained to have UndirectedGraph as its underlying graph (UndirectedGraph is a undirected graph variable).
			",
		eg:
			"
?- underlying_graph(DG,UG).
No.

?- V`::[]..[1,2,3], DE`::[[3,1]]..[[1,2],[2,3],[3,1]], UE`::[]..[[1,2],[2,1],[2,3],[3,2]], dirgraph(DG,V,DE), undirgraph(UG,V,UE), underlying_graph(DG,UG).
No.

?- V`::[]..[1,2,3], DE`::[[3,1]]..[[1,2],[2,3],[3,1]], dirgraph(DG,V,DE), underlying_graph(DG,UG), graph_labeling(DG).
V = [1, 3]
DE = [[3, 1]]
DG = dirgraph([1, 3], [[3, 1]])
UG = undirgraph([1, 3], [[1, 3], [3, 1]])
Yes ? ;

V = [1, 2, 3]
DE = [[3, 1]]
DG = dirgraph([1, 2, 3], [[3, 1]])
UG = undirgraph([1, 2, 3], [[1, 3], [3, 1]])
Yes ? ;

V = [1, 2, 3]
DE = [[2, 3], [3, 1]]
DG = dirgraph([1, 2, 3], [[2, 3], [3, 1]])
UG = undirgraph([1, 2, 3], [[1, 3], [2, 3], [3, 1], [3, 2]])
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2], [3, 1]]
DG = dirgraph([1, 2, 3], [[1, 2], [3, 1]])
UG = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [3, 1]])
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2], [2, 3], [3, 1]]
DG = dirgraph([1, 2, 3], [[1, 2], [2, 3], [3, 1]])
UG = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]])
Yes

?- V`::[]..[1,2,3], DE`::[]..[[1,2],[2,3],[3,1]], UE`::[]..[[1,2],[2,1],[2,3],[3,2]], dirgraph(DG,V,DE), undirgraph(UG,V,UE), underlying_graph(DG,UG), size(DG,Size), Size #> 0, graph_labeling(DG).
V = [1, 2]
DE = [[1, 2]]
UE = [[1, 2], [2, 1]]
DG = dirgraph([1, 2], [[1, 2]])
UG = undirgraph([1, 2], [[1, 2], [2, 1]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[2, 3]]
UE = [[2, 3], [3, 2]]
DG = dirgraph([1, 2, 3], [[2, 3]])
UG = undirgraph([1, 2, 3], [[2, 3], [3, 2]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2]]
UE = [[1, 2], [2, 1]]
DG = dirgraph([1, 2, 3], [[1, 2]])
UG = undirgraph([1, 2, 3], [[1, 2], [2, 1]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2], [2, 3]]
UE = [[1, 2], [2, 1], [2, 3], [3, 2]]
DG = dirgraph([1, 2, 3], [[1, 2], [2, 3]])
UG = undirgraph([1, 2, 3], [[1, 2], [2, 1], [2, 3], [3, 2]])
Size = 2
Yes
			"
	]
).

underlying_graph(Graph, _) :-
	var(Graph),!,fail.
underlying_graph(dirgraph(VertexSet, DirEdgeSet), undirgraph(VertexSet, UndirEdgeSet)) :-
	rem_dir(DirEdgeSet, UndirEdgeSet),
	undirgraph(undirgraph(VertexSet, UndirEdgeSet), VertexSet, UndirEdgeSet),
	DirEdgeSet `< UndirEdgeSet,

	suspend(rem_edge_rem_undir_edge(DirEdgeSet, UndirEdgeSet), 5, DirEdgeSet->cardinal:lub, ELUBSusp),
	suspend(add_undir_edge_add_edge(DirEdgeSet, UndirEdgeSet), 5, UndirEdgeSet->cardinal:glb, UEGLBSusp),
	terminate_susps(kill_susps([ELUBSusp,UEGLBSusp]), [DirEdgeSet,UndirEdgeSet]).

rem_dir(DirEdgeSet, UndirEdgeSet) :-
	glb_poss(DirEdgeSet, GLBDirEdge, PossDirEdge),
	hash_create(GLBHash),
	rem_dir_aux(GLBDirEdge, GLBHash, GLBUndirEdge),
	hash_create(PossHash),
	rem_dir_aux(PossDirEdge, PossHash, PossUndirEdge),
	UndirEdgeSet`::GLBUndirEdge+PossUndirEdge.

rem_dir_aux([], _, []) :- !.
rem_dir_aux([Elem|Elems], Visited, SymElems) :-
	hash_contains(Visited, Elem),!,
	rem_dir_aux(Elems, Visited, SymElems).
rem_dir_aux([[X,Y]|Elems], Visited, [[X,Y],[Y,X]|SymElems]) :-
	\+hash_contains(Visited, [X,Y]),!,
	hash_add(Visited, [X,Y], true),
	hash_add(Visited, [Y,X], true),
	rem_dir_aux(Elems, Visited, SymElems).		
	
:- comment(
	oriented_graph/2,
	[
		amode: oriented_graph(+,?),
		args:
		[
			"UndirectedGraph": "An undirected graph.",
			"DirectedGraph": "The oriented directed graph of UndirectedGraph."
		],
		summary: "Obtains an oriented graph of a given undirected graph.",
		desc: html("Obtains an oriented graph of a given undirected graph."),
		fail_if:
			"Fails 
			 if UndirectedGraph is not an undirected graph variable or
			 if UndirectedGraph can not be contrained to have DirectedGraph as an oriented graph (DirectedGraph is a directed graph variable).
			",
		eg:
			"
?- oriented_graph(UG,DG).
No.

?- V`::[]..[1,2,3], DE`::[[3,1]]..[[1,2],[2,3],[3,1]], UE`::[]..[[1,2],[2,1],[2,3],[3,2]], dirgraph(DG,V,DE), undirgraph(UG,V,UE), oriented_graph(UG,DG).
No.

?- undirgraph(UG,[1,2,3],[[1,2],[2,1]]), oriented_graph(UG,DG), graph_labeling(DG).
UG = undirgraph([1, 2, 3], [[1, 2], [2, 1]])
DG = dirgraph([1, 2, 3], [[2, 1]])
Yes ? ;

UG = undirgraph([1, 2, 3], [[1, 2], [2, 1]])
DG = dirgraph([1, 2, 3], [[1, 2]])
Yes ? ;

UG = undirgraph([1, 2, 3], [[1, 2], [2, 1]])
DG = dirgraph([1, 2, 3], [[1, 2], [2, 1]])
Yes

?- V`::[]..[1,2,3], DE`::[]..[[1,2],[3,1]], UE`::[]..[[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]], dirgraph(DG,V,DE), undirgraph(UG,V,UE), oriented_graph(UG,DG), size(DG,Size), Size #> 0, graph_labeling(DG).
V = [1, 3]
DE = [[3, 1]]
UE = [[1, 3], [3, 1]]
DG = dirgraph([1, 3], [[3, 1]])
UG = undirgraph([1, 3], [[1, 3], [3, 1]])
Size = 1
Yes ? ;

V = [1, 2]
DE = [[1, 2]]
UE = [[1, 2], [2, 1]]
DG = dirgraph([1, 2], [[1, 2]])
UG = undirgraph([1, 2], [[1, 2], [2, 1]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[3, 1]]
UE = [[1, 3], [3, 1]]
DG = dirgraph([1, 2, 3], [[3, 1]])
UG = undirgraph([1, 2, 3], [[1, 3], [3, 1]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2]]
UE = [[1, 2], [2, 1]]
DG = dirgraph([1, 2, 3], [[1, 2]])
UG = undirgraph([1, 2, 3], [[1, 2], [2, 1]])
Size = 1
Yes ? ;

V = [1, 2, 3]
DE = [[1, 2], [3, 1]]
UE = [[1, 2], [1, 3], [2, 1], [3, 1]]
DG = dirgraph([1, 2, 3], [[1, 2], [3, 1]])
UG = undirgraph([1, 2, 3], [[1, 2], [1, 3], [2, 1], [3, 1]])
Size = 2
Yes
			"
	]
).	
	
oriented_graph(Graph, _) :-
	var(Graph),!,fail.
oriented_graph(undirgraph(VertexSet, UndirEdgeSet), dirgraph(VertexSet, DirEdgeSet)) :-
	lub(UndirEdgeSet, LUBUndirEdge),
	DirEdgeSet`::[]..LUBUndirEdge,
	dirgraph(dirgraph(VertexSet, DirEdgeSet), VertexSet, DirEdgeSet),
	DirEdgeSet `< UndirEdgeSet,
	
	rem_edge_rem_undir_edge(DirEdgeSet, UndirEdgeSet),
	add_undir_edge_add_edge(DirEdgeSet, UndirEdgeSet),

	suspend(rem_edge_rem_undir_edge(DirEdgeSet, UndirEdgeSet), 5, DirEdgeSet->cardinal:lub, ELUBSusp),
	suspend(add_undir_edge_add_edge(DirEdgeSet, UndirEdgeSet), 5, UndirEdgeSet->cardinal:glb, UEGLBSusp),
	terminate_susps(kill_susps([ELUBSusp,UEGLBSusp]), [DirEdgeSet,UndirEdgeSet]).
	
%%%	
% - Adding an edge to an underlying graph may add an edge to the original graph
% - O(m) 
%%%
	
:- demon add_undir_edge_add_edge/2.

add_undir_edge_add_edge(DirEdgeSet, UndirEdgeSet) :-
	glb(UndirEdgeSet, GLBUndirEdge),
	lub(DirEdgeSet, LUBEdge),
	hash_create(DirEdgeHash),
	hash_add_all(DirEdgeHash, LUBEdge),
	findall([X,Y], (member([X,Y], GLBUndirEdge), hash_contains(DirEdgeHash, [X,Y]), \+hash_contains(DirEdgeHash, [Y,X])), SubEdgeSet),
	SubEdgeSet `< DirEdgeSet.

%%%
% - Removing an edge from a graph will remove the corresponding edges from the underlying graph
% - O(m)
%%%

:- demon rem_edge_rem_undir_edge/2.

rem_edge_rem_undir_edge(DirEdgeSet, UndirEdgeSet) :-
	lub(DirEdgeSet, LUBEdge),
	rem_dir(LUBEdge, PossEdgeSet),
	UndirEdgeSet `< PossEdgeSet.

:- comment(
	reverse_graph/2,
	[
		amode: reverse_graph(+,?),
		args:
		[
			"Graph": "A graph.",
			"ReverseGraph": "The reverse graph of Graph."
		],
		summary: "Obtains the reverse graph of a given graph.",
		desc: html("Obtains the reverse graph of a given graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable or
			 if Graph can not be contrained to have ReverseGraph as its reverse graph (ReverseGraph is a graph variable).
			",
		eg:
			"
?- reverse_graph(G,RG).
No.

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), dirgraph(RG,V,E), reverse_graph(G,RG).
No.

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), reverse_graph(G,RG), size(G,2), graph_labeling(G).
V = [1, 2, 3]
E = [[2, 3], [3, 1]]
G = dirgraph([1, 2, 3], [[2, 3], [3, 1]])
RG = dirgraph([1, 2, 3], [[1, 3], [3, 2]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [3, 1]]
G = dirgraph([1, 2, 3], [[1, 2], [3, 1]])
RG = dirgraph([1, 2, 3], [[1, 3], [2, 1]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [2, 3]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3]])
RG = dirgraph([1, 2, 3], [[2, 1], [3, 2]])
Yes

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], RE`::[[3,2]]..[[1,3],[2,1],[3,2]], dirgraph(G,V,E), dirgraph(RG,V,RE), reverse_graph(G,RG), graph_labeling(G).
V = [1, 2, 3]
E = [[1, 2], [2, 3]]
RE = [[2, 1], [3, 2]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3]])
RG = dirgraph([1, 2, 3], [[2, 1], [3, 2]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [2, 3], [3, 1]]
RE = [[1, 3], [2, 1], [3, 2]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3], [3, 1]])
RG = dirgraph([1, 2, 3], [[1, 3], [2, 1], [3, 2]])
Yes
			"
	]
).

reverse_graph(Graph, _) :-
	var(Graph),!,fail.
reverse_graph(dirgraph(VertexSet, EdgeSet), dirgraph(RevVertexSet, RevEdgeSet)) :-
	rev_graph_aux(VertexSet, EdgeSet, RevVertexSet, RevEdgeSet).
reverse_graph(undirgraph(VertexSet, EdgeSet), undirgraph(VertexSet, EdgeSet)).
	
rev_graph_aux(VertexSet, EdgeSet, VertexSet, RevEdgeSet) :-
	glb_poss(EdgeSet, GLBEdge, PossEdge),
	findall([Y,X], member([X,Y], GLBEdge), GLBRevEdge),
	findall([Y,X], member([X,Y], PossEdge), PossRevEdge),
	(var(RevEdgeSet) ->
		RevEdgeSet`::GLBRevEdge+PossRevEdge;
		(
		 RevEdgeSet`::GLBRevEdge+PossRevEdge,
		 add_edge_add_rev_edge(EdgeSet, RevEdgeSet),
		 rem_edge_rem_rev_edge(EdgeSet, RevEdgeSet),
		 add_edge_add_rev_edge(RevEdgeSet, EdgeSet),
		 rem_edge_rem_rev_edge(RevEdgeSet, EdgeSet)
		)
	),
	suspend(add_edge_add_rev_edge(EdgeSet, RevEdgeSet), 5, EdgeSet->cardinal:glb, EGLBSusp),
	suspend(rem_edge_rem_rev_edge(EdgeSet, RevEdgeSet), 5, EdgeSet->cardinal:lub, ELUBSusp),
	suspend(add_edge_add_rev_edge(RevEdgeSet, EdgeSet), 5, RevEdgeSet->cardinal:glb, REGLBSusp),
	suspend(rem_edge_rem_rev_edge(RevEdgeSet, EdgeSet), 5, RevEdgeSet->cardinal:lub, RELUBSusp),
	terminate_susps(kill_susps([EGLBSusp, ELUBSusp,REGLBSusp,RELUBSusp]), [EdgeSet,RevEdgeSet]).
	
%%%
% - If an edge is added to a graph then its symmetric edge must also be added to the reversed graph
% - O(m)
%%%

:- demon add_edge_add_rev_edge/2.

add_edge_add_rev_edge(EdgeSet, RevEdgeSet) :-
	glb(EdgeSet, GLBEdge),
	findall([Y,X], member([X,Y], GLBEdge), SubEdgeSet),
	SubEdgeSet `< RevEdgeSet.

%%%
% - If an edge is removed from a graph then its symmetric edge must also be removed from the reversed graph
% - O(m)
%%%

:- demon rem_edge_rem_rev_edge/2.

rem_edge_rem_rev_edge(EdgeSet, RevEdgeSet) :-
	lub(EdgeSet, LUBEdge),
	findall([Y,X], member([X,Y], LUBEdge), PossEdge),
	RevEdgeSet `< PossEdge.
	
:- comment(
	complementary_graph/2,
	[
		amode: complementary_graph(+,?),
		args:
		[
			"Graph": "A graph.",
			"ComplementaryGraph": "The complementary graph of Graph."
		],
		summary: "Obtains the complementary graph of a given graph.",
		desc: html("Obtains the complementary graph of a given graph."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable or
			 if Graph can not be constrained to have ComplementaryGraph as its complementary graph (ComplementarGraph is a graph variable).
			",
		eg:
			"
?- complementary_graph(G,CG).
No.

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], CE`::[]..[[1,2],[3,2],[1,3]], dirgraph(G,V,E), dirgraph(CG,V,CE), complementary_graph(G,CG).
No.

?- V`::[]..[1,2,3], E`::[[1,2]]..[[1,2],[2,3],[3,1]], dirgraph(G,V,E), complementary_graph(G,CG), graph_labeling(G).
V = [1, 2]
E = [[1, 2]]
G = dirgraph([1, 2], [[1, 2]])
CG = dirgraph([1, 2], [[1, 1], [2, 1], [2, 2]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2]]
G = dirgraph([1, 2, 3], [[1, 2]])
CG = dirgraph([1, 2, 3], [[1, 1], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1], [3, 2], [3, 3]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [3, 1]]
G = dirgraph([1, 2, 3], [[1, 2], [3, 1]])
CG = dirgraph([1, 2, 3], [[1, 1], [1, 3], [2, 1], [2, 2], [2, 3], [3, 2], [3, 3]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [2, 3]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3]])
CG = dirgraph([1, 2, 3], [[1, 1], [1, 3], [2, 1], [2, 2], [3, 1], [3, 2], [3, 3]])
Yes ? ;

V = [1, 2, 3]
E = [[1, 2], [2, 3], [3, 1]]
G = dirgraph([1, 2, 3], [[1, 2], [2, 3], [3, 1]])
CG = dirgraph([1, 2, 3], [[1, 1], [1, 3], [2, 1], [2, 2], [3, 2], [3, 3]])
Yes

?- V`::[]..[1,2,3], E`::[]..[[1,2],[2,3],[3,1]], CE`::[[2,3],[3,2]]..[[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]], dirgraph(G,V,E), dirgraph(CG,V,CE), complementary_graph(G,CG), graph_labeling(G).
V = [2, 3]
E = []
CE = [[2, 2], [2, 3], [3, 2], [3, 3]]
G = dirgraph([2, 3], [])
CG = dirgraph([2, 3], [[2, 2], [2, 3], [3, 2], [3, 3]])
Yes
			"
	]
).	
	
complementary_graph(Graph, _) :-
	var(Graph),!,fail.
complementary_graph(dirgraph(VertexSet,EdgeSet), dirgraph(CompVertexSet,CompEdgeSet)) :-
	complement_aux(VertexSet, EdgeSet, CompVertexSet, CompEdgeSet),
	dirgraph(dirgraph(CompVertexSet,CompEdgeSet), CompVertexSet, CompEdgeSet).
complementary_graph(undirgraph(VertexSet,EdgeSet), undirgraph(CompVertexSet,CompEdgeSet)) :-
	complement_aux(VertexSet, EdgeSet, CompVertexSet, CompEdgeSet),
	undirgraph(undirgraph(CompVertexSet,CompEdgeSet), CompVertexSet, CompEdgeSet).
		
complement_aux(VertexSet, EdgeSet, VertexSet, CompEdgeSet) :-
	lub(VertexSet, LUBVertex),
	findall([X,Y], (member(X, LUBVertex), member(Y, LUBVertex)), LUBCompEdge),
	(var(CompEdgeSet) ->
		(
		 CompEdgeSet `::[]..LUBCompEdge,
		 EdgeSet `$ CompEdgeSet,
		 comp_arcs(VertexSet, LUBCompEdge, EdgeSet, CompEdgeSet)
		);
		(
		 CompEdgeSet `::[]..LUBCompEdge,
		 EdgeSet `$ CompEdgeSet,
		 comp_arcs(VertexSet, LUBCompEdge, EdgeSet, CompEdgeSet),
		 comp_arcs(VertexSet, LUBCompEdge, CompEdgeSet, EdgeSet)
		)
	),
	
	suspend(comp_arcs(VertexSet, LUBCompEdge, EdgeSet, CompEdgeSet), 5, EdgeSet->cardinal:glb, EGSusp),
	suspend(comp_arcs(VertexSet, LUBCompEdge, EdgeSet, CompEdgeSet), 5, EdgeSet->cardinal:lub, ELSusp),
	suspend(comp_arcs(VertexSet, LUBCompEdge, CompEdgeSet, EdgeSet), 5, CompEdgeSet->cardinal:glb, CEGSusp),
	suspend(comp_arcs(VertexSet, LUBCompEdge, CompEdgeSet, EdgeSet), 5, CompEdgeSet->cardinal:lub, CELSusp),
	terminate_susps(kill_susps([EGSusp,ELSusp,CEGSusp,CELSusp]), [EdgeSet,CompEdgeSet]).

%%%	
% - If an edge is removed from a graph and both incident vertices are in the glb, then the edge must be added to the complement of the graph
% - if an edge is added to a graph then it must be removed from the complementary graph
% - O(m + n)
%%%	

:- demon comp_arcs/4.

comp_arcs(VertexSet, AllEdges, EdgeSet, CompEdgeSet) :-
	glb(VertexSet, GLBVertex),
	hash_create(VertexHash),
	hash_add_all(VertexHash, GLBVertex),
	lub(EdgeSet, LUBEdge),
	hash_create(EdgeHash),
	hash_add_all(EdgeHash, LUBEdge),
	findall([X,Y], (member([X,Y], AllEdges), hash_contains(VertexHash,X), hash_contains(VertexHash,Y), \+hash_contains(EdgeHash, [X,Y])), SubEdgeSet),
	SubEdgeSet `< CompEdgeSet.
		
:- comment(
	export_graph/2,
	[
		amode: export_graph(+,++),
		args:
		[
			"Graph": "A graph.",
			"File": "Name of the file where a GraphViz \'dot\' format of the graph is to be exported."
		],
		summary: "Exports Graph to Filename in a .dot format which can then be loaded by GraphViz.",
		desc: html("Exports Graph to Filename in a .dot format which can then be loaded by GraphViz.")
	]
).	
	
export_graph(Graph, _) :-
	var(Graph),!,fail.
export_graph(Graph, File) :-
	open(File, write, Stream),
	export_graph_aux(Stream, Graph),
	close(Stream).
	
export_graph_aux(Stream, dirgraph(VertexSet,EdgeSet)) :-
	writeln(Stream, 'digraph <input_name>'),
	writeln(Stream, '{'),
	export_vertices(Stream, VertexSet),
	export_edges(Stream, '->', EdgeSet, false),
	writeln(Stream, '}').
export_graph_aux(Stream, undirgraph(VertexSet,EdgeSet)) :-
	writeln(Stream, 'graph <input_name>'),
	writeln(Stream, '{'),
	export_vertices(Stream, VertexSet),
	export_edges(Stream, '--', EdgeSet, true),
	writeln(Stream, '}').

export_vertices(Stream, VertexSet) :-
	glb_poss(VertexSet, GLB, Poss),
	glb_vertices_attrs(GLBVertexAttrs),
	export_vertices_aux(Stream, GLB, GLBVertexAttrs),
	poss_vertices_attrs(PossVertexAttrs),
	export_vertices_aux(Stream, Poss, PossVertexAttrs).

glb_vertices_attrs('').
poss_vertices_attrs('[style=dotted]').

export_vertices_aux(_, [], _).
export_vertices_aux(Stream, [Elem|Elems], Attrs) :-
	write(Stream, Elem),
	write(Stream, ' '),
	write(Stream, Attrs),
	writeln(Stream, ';'),
	export_vertices_aux(Stream, Elems, Attrs).

export_edges(Stream, EdgeType, EdgeSet, Filter) :-
	glb_poss(EdgeSet, GLB, Poss),
	glb_edges_attrs(GLBEdgeAttrs),
	export_edges_aux(Stream, GLB, EdgeType, Filter, GLBEdgeAttrs),
	poss_edges_attrs(PossEdgeAttrs),
	export_edges_aux(Stream, Poss, EdgeType, Filter, PossEdgeAttrs).

export_edges_aux(_, [], _, _, _).
export_edges_aux(Stream, [[In,Out]|Elems], EdgeType, Filter, Attrs) :-
	((Filter == true, Out @< In) ->
		true;
		(write(Stream, In),
		 write(Stream, ' '),
		 write(Stream, EdgeType), 
		 write(Stream, ' '),
		 write(Stream, Out),
		 write(Stream, ' '),
		 write(Stream, Attrs),
		 writeln(Stream, ';')
		)
	),
	export_edges_aux(Stream, Elems, EdgeType, Filter, Attrs).

glb_edges_attrs('').
poss_edges_attrs('[style=dotted]').	

:- comment(
	graph_labeling/1,
	[
		amode: graph_labeling(+),
		args:
		[
			"Graph": "A graph."
		],
		summary: "Labels a graph variable.",
		desc: html("Labels a graph variable."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable.
			",
		eg:
			"
?- graph_labeling(G).
No.

?- V`::[]..[1,2], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), graph_labeling(G).
V = []
E = []
G = dirgraph([], [])
Yes ? ;

V = [2]
E = []
G = dirgraph([2], [])
Yes ? ;

V = [1]
E = []
G = dirgraph([1], [])
Yes ? ;

V = [1, 2]
E = []
G = dirgraph([1, 2], [])
Yes ? ;

V = [1, 2]
E = [[2, 1]]
G = dirgraph([1, 2], [[2, 1]])
Yes ? ;

V = [1, 2]
E = [[1, 2]]
G = dirgraph([1, 2], [[1, 2]])
Yes ? ;

V = [1, 2]
E = [[1, 2], [2, 1]]
G = dirgraph([1, 2], [[1, 2], [2, 1]])
Yes

?- V`::[]..[1,2], E`::[]..[[1,2],[2,1]], undirgraph(G,V,E), graph_labeling(G).
V = []
E = []
G = undirgraph([], [])
Yes ? ;

V = [2]
E = []
G = undirgraph([2], [])
Yes ? ;

V = [1]
E = []
G = undirgraph([1], [])
Yes ? ;

V = [1, 2]
E = []
G = undirgraph([1, 2], [])
Yes ? ;

V = [1, 2]
E = [[1, 2], [2, 1]]
G = undirgraph([1, 2], [[1, 2], [2, 1]])
Yes
			"
	]
).

graph_labeling(Graph) :-
	graph_labeling(Graph, down, down).

:- comment(
	graph_labeling/3,
	[
		amode: graph_labeling(+,++,++),
		args:
		[
			"Graph": "A graph.",
			"VertexHeuristic": "\'up\' for starting vertex inclusion first / \'down\' for starting vertex exclusion first.",
			"EdgeHeuristic": "\'up\' for starting edge inclusion first / \'down\' for starting edge exclusion first."
		],
		summary: "Labels a graph variable using VertexHeuristic and EdgeHeuristic.",
		desc: html("Labels a graph variable using VertexHeuristic and EdgeHeuristic."),
		fail_if:
			"Fails 
			 if Graph is not a graph variable,
			 if VertexHeuristic is not in {down,up} or
			 if EdgeHeuristic is not in {down,up}.
			",
		eg:
			"
?- graph_labeling(G,down,up).
No.

?- V`::[]..[1,2], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), graph_labeling(G,d,up).
No.

?- V`::[]..[1,2], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), graph_labeling(G,down,u).
No.

?- V`::[]..[1,2], E`::[]..[[1,2],[2,1]], dirgraph(G,V,E), graph_labeling(G,down,up).
V = []
E = []
G = dirgraph([], [])
Yes ? ;

V = [2]
E = []
G = dirgraph([2], [])
Yes ? ;

V = [1]
E = []
G = dirgraph([1], [])
Yes ? ;

V = [1, 2]
E = [[1, 2], [2, 1]]
G = dirgraph([1, 2], [[1, 2], [2, 1]])
Yes ? ;

V = [1, 2]
E = [[1, 2]]
G = dirgraph([1, 2], [[1, 2]])
Yes ? ;

V = [1, 2]
E = [[2, 1]]
G = dirgraph([1, 2], [[2, 1]])
Yes ? ;

V = [1, 2]
E = []
G = dirgraph([1, 2], [])
Yes
			"
	]
).

graph_labeling(Graph, _, _) :-
	var(Graph),!,fail.
graph_labeling(_, VertexHeuristic, EdgeHeuristic) :-
	(
	 (VertexHeuristic \== down, VertexHeuristic \== up);
	 (EdgeHeuristic \== down, EdgeHeuristic \== up)
	),!,fail.
graph_labeling(Graph, VertexHeuristic, EdgeHeuristic) :-
	getVertexSet(Graph, VertexSet),
	getEdgeSet(Graph, EdgeSet),
	set_labeling(VertexHeuristic, [VertexSet]),
	set_labeling(EdgeHeuristic, [EdgeSet]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
arc_list_to_adj_hash(EdgeList, EdgeHash) :-
	hash_create(EdgeHash),
	arc_list_to_adj_hash_aux(EdgeList, EdgeHash).
	
arc_list_to_adj_hash_aux([], _).
arc_list_to_adj_hash_aux([[X,Y]|Edges], EdgeHash) :-
	(hash_contains(EdgeHash, X) ->
		hash_get(EdgeHash, X, Succs);
		Succs = []
	),
	hash_set(EdgeHash, X, [Y|Succs]),
	arc_list_to_adj_hash_aux(Edges, EdgeHash).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
hash_add_all(_, []).
hash_add_all(Hash, [Elem-Value|Elems]) :-
	hash_add(Hash, Elem, Value),
	hash_add_all(hash, Elems).
hash_add_all(Hash, [Elem|Elems]) :-
	hash_add(Hash, Elem, true),
	hash_add_all(Hash, Elems).	
	
all_have_value([], _, _).
all_have_value([Elem|Elems], WeightHash, Value) :-
	hash_find(WeightHash, Elem, Weight),
	Weight = Value,
	all_have_value(Elems, WeightHash, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
kill_susps([]).
kill_susps([Susp|Susps]) :-
	kill_suspension(Susp),
	kill_susps(Susps).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

terminate_susps(Goal, []) :-
	Goal.
terminate_susps(Goal, [Cond|Conds]) :-
	(ground(Cond) ->
		terminate_susps(Goal, Conds);
		suspend(terminate_susps(Goal, [Cond|Conds]), 11, Cond->inst)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%