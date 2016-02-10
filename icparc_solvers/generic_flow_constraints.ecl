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
:- lib(hash).
:- use_module(max_flow).

% gcc is from Regin
% alldifferent_matrix gcc_matrix is from Regin & Gomes
% sequence is based on CP 2008 paper
% sequence_total is also based on CP2008, includes some Regin/Puget decomposition
% same is based on description in global constraint catalog
% bool_channeling and inverse are straightforward

:-export alldifferent_matrix_internal/2,
         gcc_matrix_internal/4.

:- comment(gcc_matrix_internal/4, hidden).
:- comment(alldifferent_matrix_internal/2, hidden).

:-local variable(cnt).


/***************************************************************

Structure definitions

***************************************************************/
        
% common structure for all constraints
%:-local struct(low_high(low,high,mapping)).

:-lib(flow_constraints_support).

/*******************************************************************************

alldifferent_matrix

*******************************************************************************/

alldifferent_matrix_internal(Matrix,Lib):-
        dim(Matrix,[N,N]),
        ( foreacharg(RowVars,Matrix), param(Lib) do 
            Lib: alldifferent(RowVars)
        ),
        ( for(J,1,N), param(Lib,Matrix,N) do
            ColVars is Matrix[1..N,J],
            Lib: alldifferent(ColVars)
        ),
        call_priority((
            check_alldifferent_matrix(Matrix),
            term_variables(Matrix,List),
            (ground(List) ->
                true
            ;
                suspend(update_alldifferent_matrix(List,Matrix,Susp),
                        0,[List->any],Susp)
            )
        ), 2).


:-demon(update_alldifferent_matrix/3).
update_alldifferent_matrix(List,Matrix,Susp):-
        check_alldifferent_matrix(Matrix),
        (ground(List) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_alldifferent_matrix(Matrix):-
        dim(Matrix,[N,N]),
        NrNodes is 2*N,
        (for(I,1,N),
         foreach(I,RowNodes),
         count(J,N+1,_),
         foreach(J,ColNodes) do
            true
        ),
        (multifor([I,J],[1,1],[N,N]),
         fromto([],A,A1,Edges),
         param(N,Matrix) do
            subscript(Matrix,[I,J],X),
            get_full_domain_as_list(X,Dom),
            J1 is J+N,
            (foreach(V,Dom),
             fromto(A,C,[e(I,J1,V)|C],A1),
             param(I,J1) do
                true
            )
        ),
        sort(3,=<,Edges,Sorted),
        group_by(3,Sorted,Grouped),
        (foreach(_V-EdgeGroup,Grouped),
         param(RowNodes,ColNodes,NrNodes,N,Matrix) do
            make_graph(NrNodes,EdgeGroup,Graph),
            maximum_matching_hopcroft_karp(Graph, RowNodes,ColNodes, MaximalM),
            length(MaximalM,Size),
            Size >= N, % may fail
            invert_edges(EdgeGroup,MaximalM,InvertedEdges),
            append(MaximalM,InvertedEdges,Edges1),
            make_graph(NrNodes,Edges1,Graph1),
            strong_components(Graph1, StrongComponents),
            mark_components(StrongComponents,NrNodes,InvertedEdges,NotMarked),
            % n.b. not marked edges are inverted, i.e. arg 1 is column
            (foreach(e(Col,Row,Value),NotMarked),
             param(N,Matrix) do
                Col1 is Col-N,
                subscript(Matrix,[Row,Col1],X),
                excl(X,Value)
            )
        ).



/*
invert the edges in a list which are not in matching
uses hash table to avoid list lookup
*/
invert_edges(Edges,MaximalM,InvertedEdges):-
        hash_create(Match),
        (foreach(Edge,MaximalM),
         param(Match) do
            hash_add(Match,Edge,1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,InvertedEdges),
         param(Match) do
            direct_edge(Edge,Match,A,A1)
        ).

% ignore edge if it is in the matching
% otherwise invert its direction and put into accumulator
direct_edge(Edge,Match,A,A):-
        hash_find(Match,Edge,_),
        !.
direct_edge(e(From,To,W),_Match,A,[e(To,From,W)|A]).

group_by(Arg,[H|T],S):-
        arg(Arg,H,Key),
        group_by(Arg,T,Key,[H],S).

group_by(_,[],Key,L,[Key-L]).
group_by(Arg,[H|T],Key,L,S):-
        arg(Arg,H,Key),
        !,
        group_by(Arg,T,Key,[H|L],S).
group_by(Arg,[H|T],Key,L,[Key-L|S]):-
        arg(Arg,H,Key1),
        group_by(Arg,T,Key1,[H],S).

/******************************************************************************

GCC Matrix

******************************************************************************/

gcc_matrix_internal(Row,Col,Matrix,GccLib):-
        hash_create(Limits),
        (foreach(RowLimits,Row),
         count(I,1,MaxRows),
         param(Limits,Matrix,GccLib) do
            (foreach(gcc(Low,High,Value),RowLimits),
             param(Limits,I) do
                hash_add(Limits,row_limit(I,Value),Low-High)
            ),
            RowVars is Matrix[I],
            GccLib:gcc(RowLimits,RowVars)
        ),
        (foreach(ColLimits,Col),
         count(J,1,MaxCols),
         param(Limits,Matrix,GccLib,MaxRows) do
            (foreach(gcc(Low,High,Value),ColLimits),
             param(Limits,J) do
                hash_add(Limits,col_limit(J,Value),Low-High)
            ),
            ColVars is Matrix[1..MaxRows,J],
            GccLib:gcc(ColLimits,ColVars)
        ),
        call_priority((
            check_gcc_matrix(Matrix,Limits,MaxRows,MaxCols),
            term_variables(Matrix,List),
            (ground(List) ->
                true
            ;
                suspend(update_gcc_matrix(List,Matrix,Limits,Susp),
                        0,[List->any],Susp)
            )
        ), 2).


:-demon(update_gcc_matrix/4).
update_gcc_matrix(List,Matrix,Limits,Susp):-
        check_gcc_matrix(Matrix,Limits,_,_),
        (ground(List) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_gcc_matrix(Matrix,Limits,N,M):-
        dim(Matrix,[N,M]),
        SourceNode is N+M+1,
        SinkNode is N+M+2,
        NrNodes is N+M+2,
        (multifor([I,J],[1,1],[N,M]),
         fromto([],A,A1,AllEdges),
         param(N,Matrix) do
            subscript(Matrix,[I,J],X),
            get_full_domain_as_list(X,Dom),
            J1 is J+N,
            (foreach(V,Dom),
             fromto(A,C,[e(I,J1,V)|C],A1),
             param(I,J1) do
                true
            )
        ),
        sort(3,=<,AllEdges,Sorted),
        group_by(3,Sorted,Grouped),
        (foreach(V-EdgeGroup,Grouped),
         param(NrNodes,N,M,Matrix,
               Limits,SourceNode,SinkNode) do
%            writeln(value(V)),
%            writeln(Matrix),
%            writeln(group(EdgeGroup)),
            gcc_matrix_create_edges(EdgeGroup,V,Limits,Matrix,
                                    N,M,SourceNode,SinkNode,Edges,_MidEdges),
            make_graph(NrNodes,Edges,Graph),
%            writeq(V-make_graph(NrNodes,Edges,SourceNode,SinkNode)),nl,
            feas_flow_with_lb(Graph,  low of low_high,high of low_high,
                              SourceNode, SinkNode, 
                              _MaxFlowValue, MaxFlowEdges, _),
%            writeln(N-MaxFlowValue-MaxFlowEdges),
%            is_feasible_flow(MaxFlowEdges,Edges),
%            writeln(feasible),
            residual_graph(MaxFlowEdges,Edges,ResidualEdges,RestEdges,_),
%            writeln(residual(ResidualEdges)),
            make_graph(SinkNode,ResidualEdges,ResidualGraph),
            strong_components(ResidualGraph,StrongComponents),
%            writeln(strong(StrongComponents)),
            mark_components(StrongComponents,SinkNode,RestEdges,NotMarked),
%            writeln(unmarked(NotMarked)),
            (foreach(e(Row,Col,_),NotMarked),
             param(N,M,Matrix,V) do
                Col1 is Col-N,
                ((Row >= 1, Row =< N, Col1 >= 1, Col1 =< M) ->
                    subscript(Matrix,[Row,Col1],X),
%                    writeln(V-X-Row-Col),
%                    writeln(Matrix),
                    excl(X,V)
                ;
                    true
                )
            )
        ).

gcc_matrix_create_edges(EdgeGroup,V,Limits,Matrix,
                        N,M,SourceNode,SinkNode,Edges,MidEdges):-
        (foreach(e(From,To,_),EdgeGroup),
         foreach(e(From,To,
                   low_high{low:Low,high:1,mapping:0}),MidEdges),
         param(Matrix,N,V) do
            subscript(Matrix,[From,To-N],Entry),
            (Entry == V ->
                Low = 1
            ;
                Low = 0
            )
        ),
        (for(I,1,N),
         foreach(e(SourceNode,I,
                   low_high{low:Low,high:High,mapping:0}),
                 LeftEdges),
         param(Limits,V,SourceNode) do
            ( hash_find(Limits,row_limit(I,V),Low-High) ->
                true
            ;
                Low = 0, High = 0,
                hash_add(Limits,row_limit(I,V),Low-High)
            )
        ),
        (for(J,1,M),
         foreach(e(J1,SinkNode,
                   low_high{low:Low,high:High,mapping:0}),
                 RightEdges),
         param(Limits,V,N,SinkNode) do
            J1 is J+N,
            ( hash_find(Limits,col_limit(J,V),Low-High) ->
                true
            ;
                Low = 0, High = 0,
                hash_add(Limits,col_limit(J,V),Low-High)
            )
        ),
%        writeln(LeftEdges),
%        writeln(RightEdges),
%        writeln(mid(MidEdges)),
        append(LeftEdges,MidEdges,Edges1),
        append([e(SinkNode,SourceNode,
                  low_high{low:0,high:10000,mapping:0})|RightEdges],
               Edges1,Edges).


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
    very extensive and little effort has been spent to optimise performance.
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
            true
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


