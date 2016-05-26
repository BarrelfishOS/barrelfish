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
% Copyright (C) 1995 - 2009 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			Kish Shen
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:-export(alldifferent/1).
:-export(matching/2).
:-export(gcc/2).
:-export(same/2).
:-export(inverse/2).
:-export(lex_le/2).
:-export(lex_lt/2).

%----------------------------------------------------------------------
% Output transformations
%----------------------------------------------------------------------

:-pragma(nodebug).

:-comment(tr_global_gac_out/2,hidden).
:- export tr_global_gac_out/2.

:- export portray(update_alldifferent/4, tr_global_gac_out/2, [goal]).

tr_global_gac_out(update_alldifferent(_, _,List,_), alldifferent(List)).


:- import flow_constraints_support.

/***************************************************************

Structure definitions

***************************************************************/
        
% common structure for all constraints
%:-local struct(low_high(low,high,mapping)).
:-local struct(mapping(n,
                       value2node,
                       var_array)).

:-local struct(remember(matching,
                        solved)).

/*******************************************************************************

alldifferent

*******************************************************************************/

:-comment(alldifferent/1,[summary:"GAC version of alldifferent",
                          amode:alldifferent(+),
                          args:["L":"List of integers or domain"
                                    " variables, or a collection a la collection_to_list/2"],
                          kind:[constraint:[root:[ic,fd]]],
                          desc:html("This predicate implements a GAC"
                                    " (generalized arc consistency)"
                                    " version of the alldifferent"
                                    " constraint. It uses the"
                                    " classical bitpartite matching"
                                    " implementation using the"
                                    " graph_algorithms library. This"
                                    " version often removes more"
                                    " values than the bound consistent"
                                    " alldifferent in the ic_global"
                                    " library, or the forward checking"
                                    " variant in the ic library, but"
                                    " may spend much more time doing"
                                    " this."),
                          fail_if:"fails if there is no bipartite matching"
                                  " between all variables and the"
                                  " possible values",
                          see_also:[matching/2,
                                    ic:alldifferent/1,
                                    ic_global:alldifferent/1]
                         ]).

:-comment(matching/2,[summary:"Get a matching between a list of"
                             " domain variables and their possible"
                              " values",
                      amode:matching(+,-),
                      args:["L":"A list of integers or domain"
                                " variables, or a collection a la collection_to_list/2",
                            "K":"A free variable, will be bound to a"
                                " list of integers"
                           ],
                      fail_if:"The predicate fails if no matching"
                              " exists",
                      desc:html("This predicate can be used to get the"
                                " matching into the user program."
                                " Sometimes it is a good starting"
                                " point for heuristics. It only gets"
                                " the current matching and does not do"
                                " any propagation, the matching is not"
                                " updated when values are removed, the"
                                " predicate must be called again in"
                                " the user program if this is"
                                " required"),
                      see_also:[alldifferent/1]
                     ]).

% this is the top-level entry point
% it sets up a delay on each variable, and then calls the checker
alldifferent(Vars):-
        collection_to_list(Vars,L),
        init_remember(Remember),
        call_priority((
            check_alldifferent(L,Remember),
            shrink(L,L1),
            (L1 = [] ->
                true
            ; L1 = [_] ->
                true
            ;
                suspend(update_alldifferent(L1,Remember,L,Susp),4,[L1->any],Susp)
            )
        ), 2).


shrink(L,L1):-
        (foreach(X,L),
         fromto(L1,A1,A,[]) do
            (var(X) ->
                A1 = [X|A]
            ;
                A1 = A
            )
        ).

% this is called whenever a variable changes
% it removes the constraint once it is solved
% the third argument is passed to allow output of original argument
:-demon(update_alldifferent/4).
update_alldifferent(L,Remember,_OrigArg,Susp):-  
        check_alldifferent(L,Remember),
        (is_solved(Remember) ->
            kill_suspension(Susp)
        ;
            unschedule_suspension(Susp) 
        ).

% this is the actual constraint check code
check_alldifferent(_L,Remember):-
        is_solved(Remember),
        !.
check_alldifferent(L,Remember):-
        length(L,N),
%        writeln(check),
        % create a list of the variable nodes
        % create a list of edges
        create_nodes_and_edges(L,N,Mapping,VarNodes,Edges,LastNode,Ground),
        (Ground = 1 ->
            mark_solved(Remember),
%            writeln(ground),
            true
        ;
            N1 is N+1,
            % build a list of the value nodes in the graph
            value_nodes(N1,LastNode,ValueNodes),
            % test if there are at least as many values as variables
            % if not the constraint must fail here
            LastNode - N >= N,
            % create graph data structure
            make_graph(LastNode,Edges,Graph),
            remembered_matching(Mapping,L,Remember,OldMatching,
                                SizeOld),
%            writeln(old(SizeOld,OldMatching)),
            % call the matching algorithm, if old matching does not work
            (SizeOld = N ->
                MaximalM = OldMatching
            ;
                maximum_matching_hopcroft_karp(Graph, VarNodes,
                                               ValueNodes, 
                                               MaximalM),
           
                length(MaximalM,SizeM),
%                writeln(matching(SizeM,MaximalM)),
                % check that every variable is matched, otherwise fail
                SizeM = N,
                remember_matching(N,MaximalM,Remember,Mapping)
            ),
            % invert the edges not in the matching
            invert_edges(Edges,MaximalM,InvertedEdges,Edges1),
            % build a new graph with matching and inverted edges
            make_graph(LastNode,Edges1,Graph1),
            % search for strongly connected components in new graph
            strong_components(Graph1, StrongComponents),
%            writeln(strong(StrongComponents)),
            % extract edges between nodes in different components
            mark_components(StrongComponents,LastNode,InvertedEdges,
                        NotMarked),
%            writeln(not_marked(NotMarked)),
            % find value nodes not in matching, they are possible starts
            % of alternating paths; for permutations there should be none
            unmatched_value_nodes(N1,LastNode,MaximalM,MFree),
            % MFree is a list of unmatched value nodes
            % find edges on alternate paths starting in unmatched values
            % and mark them
%            writeln(mfree(MFree)),
            alternate_paths(MFree,Graph1,NotMarked,FinalNotMarked),
            % remove the values which correspond to unmarked edges in the
            % graph; note that values are the sources of edges
            remove_unmarked_edges(FinalNotMarked,Mapping)
        ).



create_nodes_and_edges(L,N,Mapping,VarNodes,Edges,LastNode,Ground):-
        mapping_create(N,Mapping),
        hash_create(UniqueValues),
        (foreach(X,L),
         foreach(VarNode,VarNodes),
         fromto([],A,A1,Edges),
         fromto(1,B,B1,Ground),
         fromto(N,NextV,NextV1,LastNode),
         count(VarNode,1,_),
         param(Mapping,UniqueValues) do
            % create a node for each variable
            node_map(Mapping,VarNode,X),
            (integer(X) ->
                B1 = B,
                (hash_find(UniqueValues,X,_) ->
                    fail
                ;
                    hash_add(UniqueValues,X,X)
                )
            ;
                B1  = 0
            ),
            get_full_domain_as_list(X,Dom),
            % create an edge for every value in the domain of a variable
            (foreach(V,Dom),
             fromto(NextV,NextValue,NextValue1,NextV1),
             fromto(A,B,[e(VarNode,ValueNode,V)|B],A1),
             param(Mapping,VarNode) do
                % create a node for the value, if not already there
                value_insert(Mapping,V,ValueNode,NextValue,NextValue1)
            )
        ).

value_nodes(N1,LastNode,ValueNodes):-
        (for(J,N1,LastNode),
         foreach(J,ValueNodes) do
            true
        ).

unmatched_value_nodes(N1,LastNode,MaximalM,MFree):-
        dim(MNodes,[LastNode]),
        (foreach(e(_From,To,_),MaximalM),
         param(MNodes) do
            subscript(MNodes,[To],1)
        ),
        (for(J,N1,LastNode),
         fromto([],A,A1,MFree),
         param(MNodes) do
            subscript(MNodes,[J],V),
            (var(V) ->
                A1 = [J|A]
            ;
                A1 = A
            )
        ).

% special case for no unmatched value nodes = permutation
alternate_paths([],_Graph,NotMarked,NotMarked):-
        !.
alternate_paths(MFree,Graph,NotMarked,FinalNotMarked):-
        % hash table with nodes as key, dummy values
        hash_create(CheckedNodes),
        % hash table with Edges as key, dummy values 
        hash_create(MarkedEdges),
        scan_edges(MFree,CheckedNodes,Graph,MarkedEdges),
        reduce_unmarked_edges(NotMarked,MarkedEdges,FinalNotMarked).

scan_edges([],_CheckedNodes,_Graph,_MarkedEdges).
scan_edges([H|T],CheckedNodes,Graph,MarkedEdges):-
        hash_create(NewNodes),
        scan([H|T],NewNodes,CheckedNodes,Graph,MarkedEdges),
        hash_list(NewNodes,NewNodesList,_),
        scan_edges(NewNodesList,CheckedNodes,Graph,MarkedEdges).

scan([],_NewNodes,_CheckedNodes,_Graph,_MarkedEdges).
scan([H|T],NewNodes,CheckedNodes,Graph,MarkedEdges):-
        graph_get_adjacent_edges(Graph,H,Edges),
        scan_outbound(Edges,NewNodes,CheckedNodes,MarkedEdges),
        hash_add(CheckedNodes,H,H), 
        scan(T,NewNodes,CheckedNodes,Graph,MarkedEdges).

scan_outbound([],_NewNodes,_CheckedNodes,_MarkedEdges).
scan_outbound([Edge|Edge1],NewNodes,CheckedNodes,MarkedEdges):-
        consider_edge(Edge,NewNodes,CheckedNodes,MarkedEdges),
        scan_outbound(Edge1,NewNodes,CheckedNodes,MarkedEdges).

consider_edge(Edge,NewNodes,CheckedNodes,MarkedEdges):-
        Edge = e(_From,To,_Info),
        % if node has been checked before or
        % node is already on ToDo list, do not add
        ((hash_find(CheckedNodes,To,_);hash_find(NewNodes,To,_)) ->
            true
        ;
            hash_add(NewNodes,To,To)
        ),
        hash_add(MarkedEdges,Edge,0).

% if edge is in hash table, then it is marked, and should not be removed
reduce_unmarked_edges(L,Hash,K):-
        (foreach(Edge,L),
         fromto(K,A1,A,[]),
         param(Hash) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                A1 = [Edge|A]
            )
        ).

% remove values from variables corresponding to unmarked edges
remove_unmarked_edges(FinalNotMarked,Mapping):-
        (foreach(e(_ValueN,VarN,Value),FinalNotMarked),
         param(Mapping) do
            node_map(Mapping,VarN,Var),
%           writeln(rem(VarN,Var,ValueN,Value)),
            excl(Var,Value)
        ).
/*

Dealing with var/value <-> node mapping
vars are numbered 1..n
values are numbered n+1..lastnode

this allows for arbitrary values in the domains
we can not use nodenames for this as we deal with variables
*/

mapping_create(N,mapping{n:N,
                         value2node:Value2Node,
                         var_array:VarArray}):-
        hash_create(Value2Node),
        N1 is N+1,
        dim(VarArray,[N1]).

node_map(mapping{var_array:VarArray},Node,Var):-
        subscript(VarArray,[Node],Var).

% find the node for a given value
value_insert(mapping{value2node:Hash},V,ValueNode,Next,Next):-
        hash_find(Hash,V,ValueNode),
        !.
value_insert(mapping{value2node:V2N},V,Next1,Next,Next1):-
        Next1 is Next+1,
        hash_add(V2N,V,Next1).

value_lookup(mapping{value2node:Hash},V,ValueNode):-
        hash_find(Hash,V,ValueNode),
        !.


matching(L,K):-
        length(L,N),
        % create a list of the variable nodes
        % create a list of edges
        create_nodes_and_edges(L,N,_Mapping,VarNodes,Edges,LastNode,_),
        N1 is N+1,
        % build a list of the value nodes in the graph
        value_nodes(N1,LastNode,ValueNodes),
        % test if there are at least as many values as variables
        % if not the constraint must fail here
        LastNode - N >= N,
        % create graph data structure
        make_graph(LastNode,Edges,Graph),
        % call the matching algorithm
        maximum_matching_hopcroft_karp(Graph, VarNodes,ValueNodes, MaximalM),
        length(MaximalM,SizeM),
        % check that every variable is matched, otherwise fail
        SizeM = N,
        sort(1,=<,MaximalM,Sorted),
        (foreach(e(_From,_To,Value),Sorted),
         foreach(Value,K) do
            true
        ).

/*
invert the edges in a list which are not in matching
uses hash table to avoid list lookup
also returns all edges in new graph
*/
invert_edges(Edges,MaximalM,InvertedEdges,AllEdges):-
        hash_create(Match),
        (foreach(Edge,MaximalM),
         param(Match) do
            hash_add(Match,Edge,1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,InvertedEdges),
         fromto(MaximalM,B,B1,AllEdges),
         param(Match) do
            direct_edge(Edge,Match,A,A1,B,B1)
        ).

% ignore edge if it is in the matching
% otherwise invert its direction and put into accumulator
direct_edge(Edge,Match,A,A,B,B):-
        hash_find(Match,Edge,_),
        !.
direct_edge(e(From,To,W),_Match,A,[e(To,From,W)|A],
            B,[e(To,From,W)|B]).


init_remember(remember{}).

remembered_matching(_Mapping,_L,remember{matching:Array},[],0):-
        var(Array),
        !.
remembered_matching(Mapping,L,remember{matching:Array},
                    OldMatching,
                    SizeOld):-
        
        (foreach(X,L),
         count(J,1,_),
         fromto(OldMatching,A1,A,[]),
         fromto(0,B,B1,SizeOld),
         param(Mapping,Array) do
            subscript(Array,[J],Old),
            (check_in(Old,X) ->
                value_lookup(Mapping,Old,To),
                A1 = [e(J,To,Old)|A],
                B1 is B+1
            ;
                A1 = A,
                B1 = B
            )
        ).

remember_matching(N,MaximalM,Remember,_Mapping):-
        N1 is N+1,
        dim(OldMatch,[N1]),
        (foreach(e(Node,_,Value),MaximalM),
         param(OldMatch) do
            subscript(OldMatch,[Node],Value)
        ),
        setarg(1,Remember,OldMatch).

mark_solved(remember{solved:1}).

is_solved(remember{solved:Flag}):-
        integer(Flag).



/***************************************************************

GCC with fixed bounds

***************************************************************/

:- comment(gcc/2, [
        amode: gcc(++,+),
        args: ["Bounds":"A list of elements of the form gcc(Low,High,Value),"
                        " where Low, High and Value are integers, and High and"
                        " Low are non-negative (High >= Low), and Value must"
                        " be different from other Values",
               "Vars":"A collection of different variables or integers"
              ],
        summary:"Constrain the cardinality of each Value specified in Bound's"
                " gcc(Low,High,Value) to be between Low and High in Vars",
        kind:[constraint:[root:[ic,fd]]],
        desc:html("\
<P>
    This constraint ensures that the cardinality (the number of occurrences)
    of values in Vars conforms to the specifications in Bounds. Bounds is a
    list of triples in the form gcc(Low,High,Value) where Value is an integer,
    a value that Vars is to be assigned to, and must occur only once as a
    Value in Bounds, and whose cardinality |Value| is specified by 
    Low =< |Value| =< High, where Low and High are non-negative integers.
    Note that all values that Vars can take must be specified in Bounds.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is known as global_cardinality_low_up in the global
    constraint catalog. The algorithm implemented is described in 
    J.-C. Regin's paper 'Generalized Arc Consistency for Global Cardinality
    Constraint', published in AAAI-1996. 
")
                  ]).

gcc(Bounds,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
        length(Bounds,M),
        call_priority((
            check_gcc(Bounds,Variables,N,M),
            (ground(Variables) ->
                true
            ;
                suspend(update_gcc(Bounds,Variables,N,M,Susp),9,
                        [Variables->[inst,any]],Susp)

            )
         ), 2).

:-demon(update_gcc/5).
update_gcc(Bounds,Variables,N,M,Susp):-
        check_gcc(Bounds,Variables,N,M),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            unschedule_suspension(Susp)
        ).

check_gcc(Bounds,Variables,N,M):-
        create_gcc_edges(Variables,Bounds,N,M,
                          Mapping,
                          SourceNode,SinkNode,Edges),
%        writeln(Mapping),
        make_graph(SinkNode,Edges,Graph),
        feas_flow_with_lb(Graph, low of low_high, high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, MaxFlowEdges, _),
%        writeln(N-MaxFlowValue-MaxFlowEdges),
        MaxFlowValue >= N, % may fail
%        is_feasible_flow(MaxFlowEdges,Edges), % testing only, remove later
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,MidEdges,_),
        make_graph(SinkNode,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,SinkNode,MidEdges,NotMarked),
%        writeln(unmarked(NotMarked)),
        gcc_remove_unmarked_edges(NotMarked,Mapping).

create_gcc_edges(Variables,Bounds,N,M,Mapping,
                  SourceNode,SinkNode,Edges):-
        SourceNode is N+M+1,
        SinkNode is N+M+2,
        dim(Mapping,[SinkNode]),
        arg(SourceNode,Mapping,source),
        arg(SinkNode,Mapping,sink),
        hash_create(Hash),
        hash_add(Hash,node(SourceNode),source),
        hash_add(Hash,node(SinkNode),sink),
        (foreach(gcc(Low,High,Value),Bounds),
         fromto([],E,[e(J,SinkNode,
                        low_high{low:Low,high:High,mapping:0})|E],
                EdgesSink),
         count(J,N+1,_),
         param(Mapping,Hash,SinkNode) do
            (hash_find(Hash,Value,_) ->
                writeln(no_value(Value)),
                abort
            ;
                true
            ),
            hash_add(Hash,Value,J),
            arg(J,Mapping,Value)
        ),
                 
        (foreach(X,Variables),
         count(J,1,_),
         fromto([],E,E1,EdgesMain),
         fromto([],F,[e(SourceNode,J,
                        low_high{low:1,high:1,mapping:0})|F],EdgesSource),
         param(Mapping,Hash,SourceNode) do
            arg(J,Mapping,X),
            get_full_domain_as_list(X,Domain),
            (foreach(V,Domain),
             fromto(E,Edges,Edges1,E1),
             param(Hash,J,X) do
                (hash_find(Hash,V,Node) ->
                    Edges1 = [e(J,Node,
                                low_high{low:0,high:1,mapping:0})|Edges]
                ;
                    excl(X,V),
                    Edges1 = Edges
                )
            )
        ),
%        writeln(EdgesSource),
%        writeln(EdgesMain),
%        writeln(EdgesSink),
        append(EdgesSource,EdgesMain,EE),
        append(EdgesSink,EE,Edges).

% remove values from variables corresponding to unmarked edges
gcc_remove_unmarked_edges(NotMarked,Mapping):-
        (foreach(e(VarN,ValueN,_),NotMarked),
         param(Mapping) do
            gcc_node_map(Mapping,VarN,Var),
            gcc_node_map(Mapping,ValueN,Value),
            ((atom(Var);atom(Value)) ->
                true
            ;
                
%            writeln(rem(VarN,Var,ValueN,Value)),
                excl(Var,Value)
            )
        ).

gcc_node_map(Mapping,Node,Value):-
        arg(Node,Mapping,Value).



/*********************************************************************

SAME


*********************************************************************/

:- comment(same/2, [
        amode: same(+,+),
        args: ["Vars1":"A collection of N different variables or integers",
               "Vars2":"A collection of N different variables or integers"
              ],
        summary: "Vars1 and Vars2 are constrained to be a permutation of each"
                 " other in the values taken by the variables.",
        kind:[constraint:[root:[ic,fd],extra:[gccat:same]]],
        desc: html("\
<P>
    This constraint ensures that the values taken by the variables in Vars1
    and Vars2 are permutations of each other. Vars1 and Vars must be the same
    length.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
    This constraint is also known as same in the global constraint catalog. 
    The implementation is the generalised arc-consistent version described
    in the catalog. 
")]).

same(L1,L2):-
        collection_to_list(L1,Vars1),
        collection_to_list(L2,Vars2),
        length(Vars1,N),
        length(Vars2,N),
        append(Vars1,Vars2,Variables),
        call_priority((
            check_same(Vars1,Vars2,N),
            (ground(Variables) ->
                true
            ;
                suspend(update_same(Variables,Vars1,Vars2,N,Susp),
                        0,[Variables->any],Susp)
            )
        ), 2).

:-demon(update_same/5).
update_same(Variables,Vars1,Vars2,N,Susp):-
        check_same(Vars1,Vars2,N),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            unschedule_suspension(Susp)
        ).

check_same(Vars1,Vars2,N):-
        same_create_edges(Vars1,Vars2,N,NrNodes,
                          CenterEdges,SourceNode,SinkNode,Edges,
                          Mapping,ValueHash),
        make_graph(NrNodes,Edges,Graph),
        feas_flow_with_lb(Graph, low of low_high,high of low_high, 
                          SourceNode, SinkNode, 
                          MaxFlowValue, MaxFlowEdges, _),
        MaxFlowValue >= N,
        same_residual_graph(MaxFlowEdges,Edges,ResidualEdges),
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
        mark_components(StrongComponents,NrNodes,CenterEdges,NotMarked),
        remove_same_inconsistent(NotMarked,Mapping,ValueHash).

same_create_edges(Vars1,Vars2,N,NrNodes,
                  CenterEdges,
                  SourceNode,SinkNode,Edges,Mapping,Hash):-
        N2 is 2*N,
        SourceNode is N2+1,
        SinkNode is N2+2, 
        FreeNode is SinkNode+1,
        dim(Mapping,[N2]),
        hash_create(Hash),
        (foreach(X,Vars1),
         count(J,1,_),
         fromto([],A,[e(SourceNode,J,low_high{low:1,high:1,mapping:0})|A],LeftEdges),
         fromto([],B,B1,MidLeftEdges),
         fromto(FreeNode,C,C1,LastNode1),
         param(SourceNode,Mapping,Hash) do
            arg(J,Mapping,X),
            get_full_domain_as_list(X,List),
            (foreach(V,List),
             fromto(B,BB,[e(J,Node,low_high{low:0,high:1,mapping:0})|BB],B1),
             fromto(C,CC,CC1,C1),
             param(J,Hash) do
                new_node(Hash,V,Node,CC,CC1)
            )
        ),
        (foreach(X,Vars2),
         count(J,N+1,_),
         fromto([],A,[e(J,SinkNode,low_high{low:1,high:1,mapping:0})|A],RightEdges),
         fromto([],B,B1,MidRightEdges),
         fromto(LastNode1,C,C1,NrNodes),
         param(SinkNode,Mapping,Hash) do
            arg(J,Mapping,X),
            get_full_domain_as_list(X,List),
            (foreach(V,List),
             fromto(B,BB,[e(Node,J,low_high{low:0,high:1,mapping:0})|BB],B1),
             fromto(C,CC,CC1,C1),
             param(J,Hash) do
                new_node(Hash,V,Node,CC,CC1)
            )
        ),
        append(MidLeftEdges,MidRightEdges,CenterEdges),
        append(LeftEdges,CenterEdges,Edges1),
        append(RightEdges,Edges1,Edges).

new_node(Hash,V,Node,CC,CC):-
        hash_find(Hash,V,Node),
        !.
new_node(Hash,V,CC,CC,CC1):-
        hash_add(Hash,V,CC),
        hash_add(Hash,node(CC),V),
        CC1 is CC+1.


                
same_residual_graph(MaxFlowEdges,Edges,ResidualEdges):-
        (foreach(_-X,MaxFlowEdges),
         fromto(Edges,A,A1,ResidualEdges) do
            invert_edge(X,A,A1)
        ).

invert_edge(e(From,To,Cap),A,[e(To,From,Cap)|A]).


remove_same_inconsistent(NotMarked,Mapping,Hash):-
        dim(Mapping,[N2]),
        (foreach(e(From,To,_),NotMarked),
         param(Mapping,Hash,N2) do
            ((From =< N2,hash_find(Hash,node(To),Value)) ->
                arg(From,Mapping,X),
                excl(X,Value)
            ;(To =< N2,hash_find(Hash,node(From),Value)) ->
                arg(To,Mapping,X),
                excl(X,Value)
            ;
                writeln(inconsistent(From,To)),
                abort
            )
        ).

/************************************************************************

inverse

************************************************************************/

:- comment(inverse/2, [
        amode: inverse(+,+),
        args: ["Succ":"A collection of N different variables or integers",
               "Pred":"A collection  of N different variables or integers"
              ],
        summary: "Constrains elements of Succ to be the successors and"
                 " Pred to be the predecessors of nodes in a digraph",
        kind:[constraint:[root:[ic,fd]]],
        desc: html("\
<P>
     Succ and Pred are list of N elements, representing a digraph of N nodes,
     where the i'th element of Succ and Pred represents the successor and
     predecessor of the node i respectively. The constraint enforces each
     node in the digraph to have one successor and one predessor node, and
     that if node y is the successor of node x, then node x is the
     predecessor of node y.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
     This constraint is known as inverse in the global constraint catalog,
     but with implicit node index based on the position in the list.  
")]).

inverse(XL,YL):-
        collection_to_list(XL,Vars1),
        collection_to_list(YL,Vars2),
        length(Vars1,N),
        length(Vars2,N),
        append(Vars1,Vars2,Variables),
        Variables :: 1..N,
        call_priority((
            check_inverse(Vars1,Vars2,N),
            (ground(Variables) ->
                true
            ;
                suspend(update_inverse(Variables,Vars1,Vars2,N,Susp),
                        10,[Variables->any],Susp)
            )
        ), 2).


:-demon(update_inverse/5).
update_inverse(Variables,Vars1,Vars2,N,Susp):-
        check_inverse(Vars1,Vars2,N),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            unschedule_suspension(Susp)
        ).

check_inverse(Vars1,Vars2,N):-
        dim(Matrix,[N,N]),
        (for(I,1,N),
         foreach(X,Vars1),
         param(Matrix) do
            get_full_domain_as_list(X,Dom),
            (foreach(V,Dom),
             param(I,Matrix) do
                subscript(Matrix,[I,V],1-_)
            )
        ),
        (for(J,1,N),
         foreach(Y,Vars2),
         param(Matrix) do
            get_full_domain_as_list(Y,Dom),
            (foreach(V,Dom),
             param(Y,J,Matrix) do
                subscript(Matrix,[V,J],Z-1),
                (var(Z) ->
                    excl(Y,V)
                ;
                    true
                )
            )
        ),
        (for(I,1,N),
         foreach(X,Vars1),
         param(Matrix) do
            get_full_domain_as_list(X,Dom),
            (foreach(V,Dom),
             param(X,I,Matrix) do
                subscript(Matrix,[I,V],1-Z),
                (var(Z) ->
                    excl(X,V)
                ;
                    true
                )
            )
        ).

/************************************************************************

lex_le/2, lex_lt/2

************************************************************************/

:-local struct(store(alpha,beta,n)).

:- comment(lex_le/2, [
    summary:"List1 is lexicographically less or equal to List2",
    amode:lex_le(+,+),
    args:[
	"List1":"List of integers or domain variables",
	"List2":"List of integers or domain variables"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e. either is the first element of List1 strictly smaller
	than the first element of List2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
        This constraint is known as lex_lesseq in the global constraint
        catalog. The implementation here maintains generalised arc
        consistency and uses the algorithm described in:
        Z. Kiziltan, 'Symmetry Breaking Ordering Constraints, Ph.D thesis,
        Uppsala University, 2004.
")
]).

lex_le(XVector,YVector):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        setup_lex_gac(XList,YList,'=<').

:- comment(lex_lt/2, [
    summary:"List1 is lexicographically less than  List2",
    amode:lex_lt(+,+),
    args:[
	"List1":"List of integers or domain variables",
	"List2":"List of integers or domain variables"
    ],
    kind:[constraint:[root:[ic,fd]]],
    desc:html("\
    	Imposes a lexicographic ordering between the two lists. 
	I.e.  either is the first element of List1 strictly smaller
	than the first element of List2, or the first elements are
	equal and the lexicographic order holds between the two list
	tails. A non-existing element (i.e. when the end of list is 
        reached)is strictly smaller than any existing element.
</P><P>
    This is currently a prototype -- the constraint has not been tested
    very extensively and little effort has been spent to optimise performance.
    We welcome any feedback on using this constraint.
</P><P>
        This constraint is known as lex_less in the global constraint
        catalog. The implementation here maintains generalised arc
        consistency and uses the algorithm described in:
        Z. Kiziltan, 'Symmetry Breaking Ordering Constraints, Ph.D thesis,
        Uppsala University, 2004.
")
]).

lex_lt(XVector,YVector):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        call_priority(setup_lex_gac(XList,YList,'<'), 2).

setup_lex_gac(XList,YList,Variant):-
        length(XList,N),
        length(YList,N),
        find_alpha(XList,YList,1,A,XAlpha,YAlpha),
        (A =< N ->
            find_beta(XAlpha,YAlpha,A,I,-1,BB),
            ((Variant = '=<',I =:= N+1) ->
                B = 1.0Inf
            ; BB = -1 ->
                B = I
            ;
                B = BB
            ),
            A < B, % may fail
            copy_array(XList,YList,N,XArray,YArray),
            Store = store{alpha:A,beta:B,n:N},
            lex_gac(XArray,YArray,Store,A,Variant),
%            writeln(Store-XArray-YArray),
            Upper is fix(min(B-1,N)), % convert to integer required
%            writeln(Upper is min(B,N)),
            (for(J,A,Upper),
             param(XArray,YArray,Store,Variant) do
                arg(J,XArray,Xj),
                arg(J,YArray,Yj),
%                writeln(j(J,Xj,Yj)),
                (var(Xj) ->
                    suspend(update_lex_gac(Xj,XArray,YArray,
                                           Store,J,Variant,SuspX),4,
                            [Xj->[inst,min]],
                            SuspX)
                ;
                    true
                ),
                (var(Yj) ->
                    suspend(update_lex_gac(Yj,XArray,YArray,
                                           Store,J,Variant,SuspY),4,
                            [Yj->[inst,max]],
                            SuspY)
                ;
                    true
                )
            )
        ;
            % all elements equal
            (Variant = '<' ->
                fail
            ;
                true
            )
        ).

:-demon update_lex_gac/7.
update_lex_gac(Var,XArray,YArray,Store,J,Variant,Susp):-
        lex_gac(XArray,YArray,Store,J,Variant),
        (ground(Var) ->
            kill_suspension(Susp)
        ;
            unschedule_suspension(Susp)
        ).

lex_gac(XArray,YArray,Store,I,Variant):-
        Store = store{alpha:A,beta:B},
        ((A =< I,I< B) ->
            arg(I,XArray,Xi),
            arg(I,YArray,Yi),
            ((I = A, I+1 =:= B) ->
                Xi #< Yi
            ;(I = A, I+1 < B) ->
                Xi #=< Yi,
                (Xi == Yi -> % ????
                    I1 is I+1,
                    update_alpha(XArray,YArray,I1,B,Store,Variant)
                ;
                    true
                )
            ; (A < I,I<B) ->
                get_lwb(Xi,Xmin),
                get_upb(Yi,Ymax),
                (((I =:= B-1,Xmin = Ymax);Xmin>Ymax) ->
                    II is I-1,
                    update_beta(XArray,YArray,A,II,Store,Variant)
                ;
                    true
                )
            ;
                writeln(not_reached(A,I,B)),
                abort
            )
        ;
            % the update index is outside the current [a,b) range, ignore
%            writeln(outside),
            true
        ).

update_alpha(XArray,YArray,A,B,Store,Variant):-
        Store = store{n:N},
        A < B,
        (A =:= N+1 ->
            (Variant = '<' ->
                fail
            ;
                true
            )
        ;
            (A = B ->
                true
            ;
                arg(A,XArray,Xa),
                arg(A,YArray,Ya),
                (Xa == Ya ->
                    A1 is A+1,
                    update_alpha(XArray,YArray,A1,B,Store,Variant)
                ;
                    setarg(alpha of store,Store,A),
                    lex_gac(XArray,YArray,Store,A,Variant)
                )
            )
        ).

update_beta(XArray,YArray,A,B,Store,Variant):-
        B+1 =\= A,
        arg(B,XArray,Xb),
        arg(B,YArray,Yb),
        get_lwb(Xb,Xmin),
        get_upb(Yb,Ymax),
        (Xmin < Ymax ->
            B1 is B+1,
            setarg(beta of store,Store,B1),
            lex_gac(XArray,YArray,Store,B,Variant)
        ;
            (Xmin = Ymax ->
                BB is B-1,
                update_beta(XArray,YArray,A,BB,Store,Variant)
            ;
                true
            )
        ).

             
find_alpha([X|X1],[Y|Y1],I,A,Xr,Yr):-
        X == Y,
        !,
        I1 is I+1,
        find_alpha(X1,Y1,I1,A,Xr,Yr).
find_alpha(Xr,Yr,A,A,Xr,Yr).

find_beta([X|X1],[Y|Y1],I,Iend,B,Bend):-
        get_lwb(X,Xmin),
        get_upb(Y,Ymax),
        Xmin =< Ymax,
        !,
        (Xmin = Ymax ->
            (B = -1 ->
                B1 = I
            ;
                B1 = B
            )
        ;
            B1 = -1
        ),
        I1 is I+1,
        find_beta(X1,Y1,I1,Iend,B1,Bend).
find_beta(_,_,I,I,B,B).

              
copy_array(XList,YList,N,XArray,YArray):-
            dim(XArray,[N]),
            dim(YArray,[N]),
            (foreach(X,XList),
             foreach(Y,YList),
             foreacharg(X,XArray),
             foreacharg(Y,YArray) do
                true
            ).


