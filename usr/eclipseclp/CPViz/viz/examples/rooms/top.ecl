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
% The Original Code is  CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are
% Copyright (C) 2009-2010 Helmut Simonis
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
:-module(top).

:-export(top/0).

:-lib(ic).
:-lib(ic_global).
:-lib(ic_global_gac).
:-lib(cpviz).

top:-
        top(1,no,no,input_order,"Viz_rooms_NAIVE"),
        top(1,no,no,first_fail,"Viz_rooms_FF"),
        top(1,yes,no,input_order,"Viz_rooms_CHANNEL"),
        top(1,no,yes,input_order,"Viz_rooms_IMPROVED"),
        true.

top(Problem,ValueSetChanneling,ImprovedHint,VariableChoice,Output):-
        hint(Problem,N,Hints),
        N1 is N-1,
        N2 is N/2,
        NrVars is N*N1//2,
        SizeDomain is N1*N1,
        length(L,NrVars),
        L :: 1..SizeDomain,
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       vector(L),
                       []),
        ic_global_gac:alldifferent(L),
        project_row_cols(L,N1,Rows,Cols),
        create_pairs(N,Contains,Names),
        separate(Contains,Rows,N,SplitRows),
        separate(Contains,Cols,N,SplitCols),

        process_hints(L,Contains,Hints),

        limit(Rows,N2,N1),
        limit(Cols,N2,N1),
        (foreach(K,SplitRows) do
            ic_global_gac:alldifferent(K)
        ),
        (foreach(K,SplitCols) do
            ic_global_gac:alldifferent(K)
        ),
        (ValueSetChanneling = yes ->
            value_set_channeling(L,Hints)
        ;
            true
        ),
        (ImprovedHint = yes ->
            improved_hints(L,Contains,Hints)
        ;
            true
        ),
        name_variables(Handle,L,Names,Pairs),
        root(Handle),
        search(Pairs,1,VariableChoice,tree_indomain(Handle,_),complete,[]),
        solution(Handle),
        writeln(L),
        close_visualization(Handle),
        viz(Handle, _).

project_row_cols(L,N,Rows,Cols):-
        generate_tables(N,RowTable,ColTable),
        (foreach(X,L),
         foreach(R,Rows),
         foreach(C,Cols),
         param(RowTable,ColTable) do
            element(X,RowTable,R),
            element(X,ColTable,C)
        ).

generate_tables(N,RowTable,ColTable):-
        (for(I,1,N),
         fromto(RowTable,A1,A,[]),
         fromto(ColTable,B1,B,[]),
         param(N) do
            (for(J,1,N),
             fromto(A1,[I|AA],AA,A),
             fromto(B1,[J|BB],BB,B),
             param(I) do
                true
            )
        ).

create_pairs(N,Contains,Names):-
        (for(I,1,N-1),
         fromto(Names,A1,A,[]),
         fromto(Contains,B1,B,[]),
         param(N) do
            (for(J,I+1,N),
             fromto(A1,[Name|AA],AA,A),
             fromto(B1,[I-J|BB],BB,B),
             param(I) do
                concat_string([I,J],Name)
            )
        ).
        
separate(Contains,Rows,Values,SplitRows):-
        (for(Value,1,Values),
         foreach(SplitRow,SplitRows),
         param(Contains,Rows) do
            (foreach(A-B,Contains),
             foreach(V,Rows),
             fromto([],R,R1,SplitRow),
             param(Value) do
                (memberchk(Value,[A,B]) ->
                    R1 = [V|R]
                ;
                    R1 = R
                )
            )
        ).

limit(L,Bound,Values):-
        (for(I,1,Values),
         foreach(gcc(Bound,Bound,I),Pattern),
         param(Bound) do
            true
        ),
        gcc(Pattern,L).

improved_hints(L,Contains,Hints):-
        (foreach(Pos-Values,Hints),
         param(L,Contains) do
            improved_hint(Pos,Values,L,Contains)
        ).

improved_hint(Pos,[A,B],L,Contains):-
        !,
        match_hint(A-B,Contains,L,X),
        X #= Pos.
improved_hint(Pos,[Value],L,Contains):-
        (foreach(X,L),
         foreach(A-B,Contains),
         fromto([],R,R1,Required),
         param(Pos,Value) do
            (not_mentioned(A,B,Value) ->
                X #\= Pos,
                R1 = R
            ;
                R1 = [X|R]
            )
        ),
        occurrences(Pos,Required,1),
        excluded_locations(Pos,Excluded),
        exclude_values(Required,Excluded).

excluded_locations(Pos,Excluded):-
        coor(Pos,X,Y),
        (for(I,1,7),
         fromto([],A,A1,E1),
         param(Y,Pos) do
            coor(K,I,Y),
            (Pos = K ->
                A1 = A
            ;
                A1 = [K|A]
            )
        ),
        (for(J,1,7),
         fromto(E1,A,A1,Excluded),
         param(X,Pos) do
            coor(K,X,J),
            (Pos = K ->
                A1 = A
            ;
                A1 = [K|A]
            )
        ).

exclude_values(Vars,Values):-
        (foreach(X,Vars),
         param(Values) do
            (foreach(Value,Values),
             param(X) do
                X #\= Value
            )
        ).

process_hints(L,Contains,Hints):-
        (foreach(Pos-Values,Hints),
         param(L,Contains) do
            process_hint(Pos,Values,L,Contains)
        ).

process_hint(Pos,[A,B],L,Contains):-
        !,
        match_hint(A-B,Contains,L,X),
        X #= Pos.
process_hint(Pos,[Value],L,Contains):-
        (foreach(X,L),
         foreach(A-B,Contains),
         fromto([],R,R1,Required),
         param(Pos,Value) do
            (not_mentioned(A,B,Value) ->
                X #\= Pos,
                R1 = R
            ;
                R1 = [X|R]
            )
        ),
        occurrences(Pos,Required,1).

not_mentioned(A,B,V):-
        A \= V,
        B \= V.

match_hint(H,[H|_],[X|_],X):-
        !.
match_hint(H,[_|T],[_|R],X):-
        match_hint(H,T,R,X).
            
value_set_channeling(L,Hints):-
        dim(Matrix,[7,7]),
        Matrix[1..7,1..7] :: 0..1,
        flatten_array(Matrix,ValueSet),
        value_set_channel(L,ValueSet,1),
        (for(I,1,7),
         param(Matrix) do
            Sum1 is Matrix[I,1..7],
            Sum2 is Matrix[1..7,I],
            sumlist(Sum1,4),
            sumlist(Sum2,4)
        ),
        (foreach(K-_,Hints),
         param(Matrix) do
            coor(K,I,J),
            subscript(Matrix,[I,J],1)
        ).
        

value_set_channel(L,ValueSet,Offset):-
        (foreach(Y,ValueSet),
         count(J,Offset,_),
         param(L) do
            (var(Y) ->
                suspend(value_set_to_var(Y,J,L),2,Y->inst)
            ;
                value_set_to_var(Y,J,L)
            )
        ),
        Array =.. [[]|ValueSet],
        (foreach(Z,L),
         param(Array) do
            (var(Z) ->
                suspend(var_to_value_set(Z,Array),2,Z->inst)
            ;
                var_to_value_set(Z,Array)
            )
        ).

value_set_to_var(1,_,_).
value_set_to_var(0,K,L):-
        (foreach(X,L),
         param(K) do
            X #\= K
        ).

var_to_value_set(K,Array):-
        arg(K,Array,1).


coor(K,I,J):-
        integer(K),
        !,
        I is 1+(K-1)//7,
        J is 1+((K-1) mod 7).
coor(K,I,J):-
        K is (I-1)*7+J.

hint(1,8,[2-[8],5-[5,7],8-[2],9-[1,5],15-[7],
        17-[8],26-[2],27-[5],28-[1],29-[8],
        34-[1],39-[4,5],43-[4],47-[1,3]]).
hint(2,8,[2-[7],3-[4],5-[3],6-[8],11-[7],12-[5],
        15-[6],22-[4,8],24-[2,7],26-[1],30-[3],
        36-[5],37-[8],41-[6],42-[2]]).
hint(3,8,[7-[2,5],11-[2,6],13-[3,5],17-[2],18-[3],
        22-[2],24-[4],27-[6],29-[1],30-[6],33-[5],
        34-[8],41-[1],45-[3],49-[1]]).

