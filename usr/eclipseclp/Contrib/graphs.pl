
:- module(graphs).			% SEPIA header
:- export
        compose/3,
        p_member/3,
        p_to_s_graph/2,
        p_transpose/2,
        s_member/3,
        s_to_p_graph/2,
        s_to_p_trans/2,
        s_transpose/2,
        top_sort/2,
        vertices/2,
        warshall/2.

:- lib(ordset).
:- import ord_union/3 from ordset.


%   File   : GRAPHS.PL
%   Author : R.A.O'Keefe
%   Updated: 20 March 1984
%   Converted for NIP: Ken Johnson, 15-6-87
%   Purpose: Graph-processing utilities.

/*  The P-representation of a graph is a list of (from-to) vertex
    pairs, where the pairs can be in any old order.  This form is
    convenient for input/output.

    (Note [KJ] The representation of a link in the Pform,
    e.g.edinburgh-glasgow, does not imply the existence of a
    link from glasgow to edinburgh. You have to put that in separately.)

    The S-representation of a graph is a list of (vertex-neighbours)
    pairs, where the pairs are in standard order (as produced by
    keysort) and the neighbours of each vertex are also in standard
    order (as produced by sort).  This form is convenient for many
    calculations.

    p_to_s_graph(Pform, Sform) converts a P- to an S- representation.
    s_to_p_graph(Sform, Pform) converts an S- to a P- representation.

    warshall(Graph, Closure) takes the transitive closure of a graph
    in S-form.  (NB: this is not the reflexive transitive closure).

    s_to_p_trans(Sform, Pform) converts Sform to Pform, transposed.

    p_transpose transposes a graph in P-form, cost O(|E|).
    s_transpose transposes a graph in S-form, cost O(|V|^2).
*/

%   vertices(S_Graph,  Vertices)
%   strips off the neighbours lists of an S-representation to produce
%   a list of the vertices of the graph.  (It is a characteristic of
%   S-representations that *every* vertex appears, even if it has no
%   neighbours.)

vertices([], []) :- !.
vertices([Vertex-_|Graph], [Vertex|Vertices]) :-
	vertices(Graph, Vertices).



p_to_s_graph(P_Graph, S_Graph) :-
	sort(P_Graph, EdgeSet),
	p_to_s_vertices(EdgeSet, VertexBag),
	sort(VertexBag, VertexSet),
	p_to_s_group(VertexSet, EdgeSet, S_Graph).


p_to_s_vertices([], []) :- !.
p_to_s_vertices([A-Z|Edges], [A,Z|Vertices]) :-
	p_to_s_vertices(Edges, Vertices).


p_to_s_group([], _, []) :- !.
p_to_s_group([Vertex|Vertices], EdgeSet, [Vertex-Neibs|G]) :-
	p_to_s_group(EdgeSet, Vertex, Neibs, RestEdges),
	p_to_s_group(Vertices, RestEdges, G).


p_to_s_group([V-X|Edges], V, [X|Neibs], RestEdges) :- !,
	p_to_s_group(Edges, V, Neibs, RestEdges).
p_to_s_group(Edges, _, [], Edges).



s_to_p_graph([], []) :- !.
s_to_p_graph([Vertex-Neibs|G], P_Graph) :-
	s_to_p_graph(Neibs, Vertex, P_Graph, Rest_P_Graph),
	s_to_p_graph(G, Rest_P_Graph).


s_to_p_graph([], _, P_Graph, P_Graph) :- !.
s_to_p_graph([Neib|Neibs], Vertex, [Vertex-Neib|P], Rest_P) :-
	s_to_p_graph(Neibs, Vertex, P, Rest_P).



s_to_p_trans([], []) :- !.
s_to_p_trans([Vertex-Neibs|G], P_Graph) :-
	s_to_p_trans(Neibs, Vertex, P_Graph, Rest_P_Graph),
	s_to_p_trans(G, Rest_P_Graph).


s_to_p_trans([], _, P_Graph, P_Graph) :- !.
s_to_p_trans([Neib|Neibs], Vertex, [Neib-Vertex|P], Rest_P) :-
	s_to_p_trans(Neibs, Vertex, P, Rest_P).



warshall(Graph, Closure) :-
	warshall(Graph, Graph, Closure).


warshall([], Closure, Closure) :- !.
warshall([V-_|G], E, Closure) :-
	memberchk(V-Y, E),	%  Y := E(v)
	warshall(E, V, Y, NewE),
	warshall(G, NewE, Closure).


warshall([X-Neibs|G], V, Y, [X-NewNeibs|NewG]) :-
	memberchk(V, Neibs),
	!,
	ord_union(Neibs, Y, NewNeibs),
	warshall(G, V, Y, NewG).
warshall([X-Neibs|G], V, Y, [X-Neibs|NewG]) :- !,
	warshall(G, V, Y, NewG).
warshall([], _, _, []).



p_transpose([], []) :- !.
p_transpose([From-To|Edges], [To-From|Transpose]) :-
	p_transpose(Edges, Transpose).



s_transpose(S_Graph, Transpose) :-
	s_transpose(S_Graph, Base, Base, Transpose).

s_transpose([], [], Base, Base) :- !.
s_transpose([Vertex-Neibs|Graph], [Vertex-[]|RestBase], Base, Transpose) :-
	s_transpose(Graph, RestBase, Base, SoFar),
	transpose_s(SoFar, Neibs, Vertex, Transpose).

transpose_s([Neib-Trans|SoFar], [Neib|Neibs], Vertex,
		[Neib-[Vertex|Trans]|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([Head|SoFar], Neibs, Vertex, [Head|Transpose]) :- !,
	transpose_s(SoFar, Neibs, Vertex, Transpose).
transpose_s([], [], _, []).



%   p_member(X, Y, P_Graph)
%   tests whether the edge (X,Y) occurs in the graph.  This always
%   costs O(|E|) time.  Here, as in all the operations in this file,
%   vertex labels are assumed to be ground terms, or at least to be
%   sufficiently instantiated that no two of them have a common instance.

p_member(X, Y, P_Graph) :-
	nonvar(X), nonvar(Y), !,
	memberchk(X-Y, P_Graph).
p_member(X, Y, P_Graph) :-
	member(X-Y, P_Graph).

%   s_member(X, Y, S_Graph)
%   tests whether the edge (X,Y) occurs in the graph.  If either
%   X or Y is instantiated, the check is order |V| rather than
%   order |E|.

s_member(X, Y, S_Graph) :-
	var(X), var(Y), !,
	member(X-Neibs, S_Graph),
	member(Y, Neibs).
s_member(X, Y, S_Graph) :-
	var(X), !,
	member(X-Neibs, S_Graph),
	memberchk(Y, Neibs).
s_member(X, Y, S_Graph) :-
	var(Y), !,
	memberchk(X-Neibs, S_Graph),
	member(Y, Neibs).
s_member(X, Y, S_Graph) :-
	memberchk(X-Neibs, S_Graph),
	memberchk(Y, Neibs).


%   compose(G1, G2, Composition)
%   calculates the composition of two S-form graphs, which need not
%   have the same set of vertices.

compose(G1, G2, Composition) :-
	vertices(G1, V1),
	vertices(G2, V2),
	ord_union(V1, V2, V),
	compose(V, G1, G2, Composition).


compose([], _, _, []) :- !.
compose([Vertex|Vertices], [Vertex-Neibs|G1], G2, [Vertex-Comp|Composition]) :- !,
	compose1(Neibs, G2, [], Comp),
	compose(Vertices, G1, G2, Composition).
compose([Vertex|Vertices], G1, G2, [Vertex-[]|Composition]) :-
	compose(Vertices, G1, G2, Composition).


compose1([V1|Vs1], [V2-N2|G2], SoFar, Comp) :-
	compare(Rel, V1, V2), !,
	compose1(Rel, V1, Vs1, V2, N2, G2, SoFar, Comp).
compose1(_, _, Comp, Comp).


compose1(<, _, Vs1, V2, N2, G2, SoFar, Comp) :- !,
	compose1(Vs1, [V2-N2|G2], SoFar, Comp).
compose1(>, V1, Vs1, _, _, G2, SoFar, Comp) :- !,
	compose1([V1|Vs1], G2, SoFar, Comp).
compose1(=, V1, Vs1, V1, N2, G2, SoFar, Comp) :-
	ord_union(N2, SoFar, Next),
	compose1(Vs1, G2, Next, Comp).





top_sort(Graph, Sorted) :-
	vertices_and_zeros(Graph, Vertices, Counts0),
	count_edges(Graph, Vertices, Counts0, Counts1),
	select_zeros(Counts1, Vertices, Zeros),
	top_sort(Zeros, Sorted, Graph, Vertices, Counts1).


vertices_and_zeros([], [], []) :- !.
vertices_and_zeros([Vertex-_|Graph], [Vertex|Vertices], [0|Zeros]) :-
	vertices_and_zeros(Graph, Vertices, Zeros).


count_edges([], _, Counts, Counts) :- !.
count_edges([_-Neibs|Graph], Vertices, Counts0, Counts2) :-
	incr_list(Neibs, Vertices, Counts0, Counts1),
	count_edges(Graph, Vertices, Counts1, Counts2).


incr_list([], _, Counts, Counts) :- !.
incr_list([V|Neibs], [V|Vertices], [M|Counts0], [N|Counts1]) :- !,
	N is M+1,
	incr_list(Neibs, Vertices, Counts0, Counts1).
incr_list(Neibs, [_|Vertices], [N|Counts0], [N|Counts1]) :-
	incr_list(Neibs, Vertices, Counts0, Counts1).


select_zeros([], [], []) :- !.
select_zeros([0|Counts], [Vertex|Vertices], [Vertex|Zeros]) :- !,
	select_zeros(Counts, Vertices, Zeros).
select_zeros([_|Counts], [_|Vertices], Zeros) :-
	select_zeros(Counts, Vertices, Zeros).



top_sort([], [], Graph, _, Counts) :- !,
	vertices_and_zeros(Graph, _, Counts).
top_sort([Zero|Zeros], [Zero|Sorted], Graph, Vertices, Counts1) :-
	memberchk(Zero-Neibs, Graph),
	decr_list(Neibs, Vertices, Counts1, Counts2, Zeros, NewZeros),
	top_sort(NewZeros, Sorted, Graph, Vertices, Counts2).


decr_list([], _, Counts, Counts, Zeros, Zeros) :- !.
decr_list([V|Neibs], [V|Vertices], [1|Counts1], [0|Counts2], Zi, Zo) :- !,
	decr_list(Neibs, Vertices, Counts1, Counts2, [V|Zi], Zo).
decr_list([V|Neibs], [V|Vertices], [N|Counts1], [M|Counts2], Zi, Zo) :- !,
	M is N-1,
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).
decr_list(Neibs, [_|Vertices], [N|Counts1], [N|Counts2], Zi, Zo) :-
	decr_list(Neibs, Vertices, Counts1, Counts2, Zi, Zo).

