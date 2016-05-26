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

:- lib(graph_algorithms).
:- lib(flow_constraints_support).
:- lib(hash).
:- use_module(max_flow).

:- export sequence/4,
          sequence/5,
          sequence_total/6,
          sequence_total/7.
   
/******************************************************************************

SEQUENCE

******************************************************************************/

sequence_total(Min,Max,L,U,K,Vars,Values):-
        index_function(Vars,Values,Index),
        (foreach(X,Vars),
         foreach(Z,ZeroOne),
         param(Index) do
            element(X,Index,Z)
        ),
        sequence_total(Min,Max,L,U,K,ZeroOne).


sequence(L,U,K,Vars,Values):-
        index_function(Vars,Values,Index),
        (foreach(X,Vars),
         foreach(Z,ZeroOne),
         param(Index) do
            element(X,Index,Z)
        ),
        sequence(L,U,K,ZeroOne).

index_function(Vars,Values,Index):-
        upper_bound(Vars,Size),
        dim(Index,[Size]),
        (foreach(V,Values),
         param(Index,Size) do
            (V =< Size ->
                arg(V,Index,1)
            ;
                true
            )
        ),
        (for(J,1,Size),
         param(Index) do
            arg(J,Index,V),
            default(V,0)
        ).

default(V,V):-
        !.
default(_,_).

upper_bound([H|T],Size):-
        get_upb(H,Initial),
        (foreach(X,T),
         fromto(Initial,A,A1,Size) do
            get_upb(X,XMax),
            A1 is max(A,XMax)
        ).

sequence_total(Min,Max,L,U,K,Vars):-
        total_sequence(Min,Max,L,U,K,Vars),
        sequence(L,U,K,Vars).

total_sequence(Min,Max,L,U,K,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
%        writeln(check_total_sequence(Min,Max,L,U,K,N,Variables)),
        call_priority((
            check_total_sequence(Min,Max,L,U,K,N,Variables),
            (ground(Variables) ->
                true
            ;
                suspend(update_total_sequence(Min,Max,L,U,K,N,Variables,
                                              Susp),
                        9,[Variables->any],Susp)
            )
        ), 2).

:-demon(update_total_sequence/8).
update_total_sequence(Min,Max,L,U,K,N,Variables,Susp):-
        check_total_sequence(Min,Max,L,U,K,N,Variables),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).        
        
check_total_sequence(Min,Max,L,U,K,N,Variables):-
        create_total_sequence_edges(Min,Max,L,U,K,N,Variables,
                                    NrNodes,
                                    SourceNode,SinkNode,
                                    Edges,XEdges,
                                    CombinedEdges,
                                    Combination,Mapping),
%        writeln(SourceNode-SinkNode),
%        writeln(Combination),
        make_graph(NrNodes,CombinedEdges,Graph),
%        writeq(feas(Edges)),nl,
%        writeq(combined(CombinedEdges)),nl,
        feas_flow_with_lb(Graph,  
                          low of low_high,high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, CombinedMaxFlowEdges, _),
%        writeln(Min-Max-MaxFlowValue-CombinedMaxFlowEdges),
%        is_feasible_flow(CombinedMaxFlowEdges,CombinedEdges),
        hash_expand_edges(CombinedMaxFlowEdges,Combination,
                          MaxFlowEdges),
%        writeln(max_flow_edges(MaxFlowEdges)),
%        is_feasible_flow(MaxFlowEdges,Edges),
        MaxFlowValue >= Min,
        MaxFlowValue =< Max,
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,_MidEdges,
                       FlowSolution),
%        writeq(res(ResidualEdges)),nl,
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,NrNodes,XEdges,NotMarked),
%        writeln(unmarked(NotMarked,FlowSolution)),
        remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution).

hash_expand_edges(CombinedMaxFlowEdges,Combination,MaxFlowEdges):-
        (foreach(Edge,CombinedMaxFlowEdges),
         fromto([],A,A1,MaxFlowEdges),
         param(Combination) do
            (is_multi_edge(Edge,Flow) ->
%                writeln(Edge),
                Edge = _-Edge1,
                hash_find(Combination,Edge1,Multiples),
%                writeln(Multiples),
                distribute_flow(Multiples,Flow,A,A1)
            ;
                A1 = [Edge|A]
            )
        ).

create_total_sequence_edges(Min,Max,L,U,K,N,Variables,
                            NrNodes,
                            SourceNode,SinkNode,
                            Edges,XEdges,
                            CombinedEdges,
                            Combination,Mapping):-
        NrSets is integer(ceiling(N/K)),
        SourceNode is 2*NrSets+1,
        SinkNode is SourceNode+1,
        NrNodes = SinkNode,
%        writeln(nr(NrSets,SourceNode,SinkNode,N)),
        (for(J,1,NrSets),
         foreach(e(SourceNode,J,low_high{low:L1,high:U,mapping:0}),
                 LeftEdges),
         foreach(e(J1,SinkNode,low_high{low:L2,high:U,mapping:0}),
                 RightEdges),
         param(SourceNode,SinkNode,L,U,NrSets,N,K) do
            J1 is J+NrSets,
            ((J =:= NrSets,N mod K =\= 0)  ->
                L1 = 0
            ;
                L1 = L
            ),
            ((J =:= 1,N mod K =\= 0)  ->
                L2 = 0
            ;
                L2 = L
            )
        ),
%        writeln(LeftEdges),
        dim(Mapping,[N]),
        (for(I,1,N),
         foreach(X,Variables),
         fromto([],A,A1,XEdges),
         param(Mapping,N,K,NrSets) do
            arg(I,Mapping,X),
            From is (I+K-1)//K,
            To is 2*NrSets-(N-I)//K,
%            writeln(from(I,From,To)),
            (X == 0 ->
                A1 = A
            ; X ==1 ->
                A1 = [e(From,To,low_high{low:1,high:1,mapping:I})|A]
            ;
                A1 = [e(From,To,low_high{low:0,high:1,mapping:I})|A]
            )
        ),
%        writeln(XEdges),
        hash_create(Combination),
        merge_edges(XEdges,MergedEdges,Combination),
        append([e(SinkNode,SourceNode,
                  low_high{low:Min,high:Max,mapping:0})|LeftEdges],
               RightEdges,
               Edges1),
        append(Edges1,XEdges,Edges),
        append(Edges1,MergedEdges,CombinedEdges).

merge_edges([A|R],S,Combination):-
        merge_edges(R,A,[A],S,Combination).

merge_edges([],M,L,[M],Hash):-
        hash_add(Hash,M,L).
merge_edges([A|R],B,L,S,Hash):-
        merge_two_edges(A,B,M),
        !,
        merge_edges(R,M,[A|L],S,Hash).
merge_edges([A|R],M,L,[M|S],Hash):-
        hash_add(Hash,M,L),
        merge_edges(R,A,[A],S,Hash).

merge_two_edges(e(From,To,low_high{low:L1,high:H1}),
                e(From,To,low_high{low:L2,high:H2}),
                e(From,To,low_high{low:L,high:H,mapping: -1})):-
        L is L1+L2,
        H is H1+H2.

% this is the binary version
sequence(L,U,K,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
%        writeln(check_sequence(L,U,K,N,Variables)),
        call_priority((
            check_sequence(L,U,K,N,Variables),
            (ground(Variables) ->
                true
            ;
                suspend(update_sequence(L,U,K,N,Variables,Susp),
                        9,[Variables->[min,max]/*,
                           Variables->ic:hole*/],Susp)
            )
        ), 2).

:-demon(update_sequence/6).
update_sequence(L,U,K,N,Variables,Susp):-
        check_sequence(L,U,K,N,Variables),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).        
        
check_sequence(L,U,K,N,Variables):-
        create_edges(L,U,K,N,Variables,NrNodes,
                     SourceNode,SinkNode,Edges,XEdges,
                     CombinedEdges,MultiEdges,Mapping),
        make_graph(NrNodes,CombinedEdges,Graph),
        % ??? this needs to handle lower bounds as well
%        writeln(vars(Variables)),
%        writeq(feas(Edges)),nl,
        feas_flow_with_lb(Graph,  low of low_high,high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, CombinedMaxFlowEdges, _),
        ExpFlow is (N-K)*(U-L)+U,
%        writeln(MaxFlowValue-ExpFlow-MaxFlowEdges),
%        is_feasible_flow(CombinedMaxFlowEdges,CombinedEdges),
        expand_edges(CombinedMaxFlowEdges,MultiEdges,MaxFlowEdges),
%        is_feasible_flow(MaxFlowEdges,Edges),
        MaxFlowValue = ExpFlow,
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,_MidEdges,FlowSolution),
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,NrNodes,XEdges,NotMarked),
%        writeln(unmarked(NotMarked)),
        remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution).

/*

a lot of extra code is required to work around the restriction of -no
 multiple edges- in the max_flow library: we have to combine edges by
 adding up their capacity and then later on distribute the flow over
 that edge; it also means we can not use from-to as the hash key to
 find the correct variable belonging to an edge, we introdcue the
 mapping field in low_high to store an index number which we then use
 to look up the variable in an array Mapping

*/

% if there is no or only one MultiEdge, then there is nothing to do
expand_edges(MaxFlowEdges,[],MaxFlowEdges):-
        !.
expand_edges(MaxFlowEdges,[_],MaxFlowEdges):-
        !.
expand_edges(CombinedMaxFlowEdges,MultiEdges,MaxFlowEdges):-
        (foreach(Edge,CombinedMaxFlowEdges),
         fromto([],A,A1,MaxFlowEdges),
         param(MultiEdges) do
            expand_edge(Edge,MultiEdges,A,A1)
        ).

expand_edge(Edge,MultiEdges,A,A1):-
        is_multi_edge(Edge,Flow),
        !,
        distribute_flow(MultiEdges,Flow,A,A1).
expand_edge(Edge,_MultiEdges,A,[Edge|A]).

is_multi_edge(Flow-e(_,_,low_high{mapping: -1}),Flow).

% this only works for 0/1 flow limits, needs constraint solver in
% general case
distribute_flow(MultiEdges,Flow,In,Out):-
        (foreach(Edge,MultiEdges),
         fromto(In,A,A1,Forced),
         fromto(Flow,B,B1,Rest),
         fromto([],C,C1,Remaining) do
            (required_flow(Edge) ->
                A1 = [1-Edge|A],
                B1 is B-1,
                C1 = C
            ;
                A1 = A,
                B1 = B,
                C1 = [Edge|C]
            )
        ),
        (foreach(Edge,Remaining),
         fromto(Forced,A,A1,Out),
         fromto(Rest,B,B1,0) do
            (B > 0 ->
                A1 = [1-Edge|A],
                B1 is B-1
            ;
                A1 = A,
                B1 = B
            )
        ).

% the edge has a lower bound
required_flow(e(_,_,low_high{low:Low})):-
        Low > 0.

/*
this is different from the other flow-based constraints
edges which are marked do not indicate a value to remove, but that for
 the variable associated, only the flow solution value is feasible,
 the opposite value can be removed
*/
remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution):-
        (foreach(e(_From,_To,low_high{mapping:M}),NotMarked),
         param(Mapping,FlowSolution) do
            hash_find(FlowSolution,M,ConsistentValue),
            arg(M,Mapping,X),
            ToRemove is 1-ConsistentValue,
            excl(X,ToRemove)
        ).

/*
In the construction, the only possible parallel edges are between the first and
 last row of the matrix, whether they exist depends on the parameters
*/
create_edges(L,U,K,N,Variables,SinkNode,
             SourceNode,SinkNode,Edges,XEdges,
             CombinedAllEdges,MultiEdges,Mapping):-
        RowNodes is 2*N-2*K+3,
        SourceNode is RowNodes+1,
        SinkNode is RowNodes+2,
        UL is U-L,
%        writeln(problem(L,U,K,N,RowNodes,SourceNode,SinkNode,UL)),
        (UL > 0 ->
            (for(J,2,RowNodes-1,2),
             fromto([],A,[e(SourceNode,J,
                            low_high{low:UL,high:UL,mapping:0})|A],RowEdges1),
             param(SourceNode,UL) do
                true
            ),
            (for(J,3,RowNodes-2,2),
             fromto([],A,
                    [e(J,SinkNode,low_high{low:UL,high:UL,mapping:0})|A],
                    SinkEdges),
             param(SinkNode,UL) do
                true
            )
        ;
            RowEdges1 = [],
            SinkEdges = []
        ),
        (for(J,2,RowNodes-1,2),
         fromto(RowEdges1,
                A,[e(J,J1,low_high{low:0,high:N,mapping:0}),
                   e(J,J2,low_high{low:0,high:N,mapping:0})|A],RowEdges),
         param(N) do
            J1 is J-1,
            J2 is J+1
        ),
%        writeln(RowEdges),
%        writeln(SinkEdges),
        SpecialEdges = [e(SourceNode,1,low_high{low:L,high:L,mapping:0}),
                        e(RowNodes,SinkNode,low_high{low:U,high:U,mapping:0})],
%        writeln(SpecialEdges),
        dim(Mapping,[N]),
        (foreach(X,Variables),
         count(M,1,_),
         fromto([],A,A1,XEdges),
         fromto([],B,B1,SingleEdges),
         fromto([],C,C1,MultiEdges),
         param(RowNodes,K,Mapping) do
            From is max(1,1+2*(M-K)),
            To is min(RowNodes,1+2*M),
            arg(M,Mapping,X),
            (X == 0 ->
                A1 = A,
                B1 = B,
                C1 = C
            ; 
                (X == 1 ->
                    Edge = e(From,To,low_high{low:1,high:1,mapping:M})
                ;
                    Edge = e(From,To,low_high{low:0,high:1,mapping:M})
                ),
                ((From = 1,To = RowNodes) ->
                    A1 = [Edge|A],
                    B1 = B,
                    C1 = [Edge|C]
                ;
                    A1 = [Edge|A],
                    B1 = [Edge|B],
                    C1 = C
                )
            )
        ),
%        writeln(multi(MultiEdges)),
        length(MultiEdges,Multi),
        (Multi > 1 ->
            combine_edges(MultiEdges,CombinedEdges)
        ;
            CombinedEdges = MultiEdges
        ),
%        writeln(CombinedEdges),
        append(CombinedEdges,SingleEdges,CombinedXEdges),
        append(RowEdges,SinkEdges,E1),
        append(SpecialEdges,E1,E2),
        append(CombinedXEdges,E2,CombinedAllEdges),
        append(XEdges,E2,Edges).

% mark combined edge with mapping = -1
combine_edges([e(From,To,low_high{low:Low,high:High})|R],
              [e(From,To,low_high{low:LowEnd,high:HighEnd,mapping: -1})]):-
        (foreach(e(_,_,low_high{low:Low1,high:High1}),R),
         fromto(Low,A,A1,LowEnd),
         fromto(High,B,B1,HighEnd) do
            A1 is A+Low1,
            B1 is B+High1
        ).

% abort if infeasible edge detected
is_feasible_flow(MaxFlowEdges,Edges):-
        hash_create(Hash),
        (foreach(Flow-Edge,MaxFlowEdges),
         fromto(0,A,A1,Err),
         param(Hash) do
            hash_add(Hash,Edge,1),
            Edge = e(_,_,low_high{low:L,high:H}),
            ((Flow >= L,
              Flow =< H) ->
                A1 = A
            ;
                A1 = 1,
                writeln(infeasible(Flow,Edge))
            )
        ),
        (foreach(Edge,Edges),
         fromto(Err,A,A1,Error),
         param(Hash) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                Edge = e(_,_,low_high{low:L,high:H}),
                ((0 >= L,0 =< H) ->
                    A1 =A
                ;
                    A1 = 1,
                    writeln(infeasible(0,Edge))
                )
            )
        ),
        (Error = 1 ->
            writeln(problem),
            abort
        ;
            true
        ).

