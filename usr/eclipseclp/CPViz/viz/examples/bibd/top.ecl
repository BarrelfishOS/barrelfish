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
%:-lib(ic_global_gac).
:-lib(timeout).

:-lib(cpviz).

top:-
        all_bibd(6,10,5,3,2,col,sym,indomain_max,"Viz_bibd_COLMAX"),
        all_bibd(6,10,5,3,2,row,sym,indomain_max,"Viz_bibd_ROWMAX"),
        all_bibd(6,10,5,3,2,row,sym,indomain,"Viz_bibd_ROWMIN"),
        bibd(6,10,5,3,2,row,naive,indomain,"Viz_bibd_NAIVE"),
%        all_bibd(6,10,5,3,2,row,naive,indomain,"Viz_bibd_NAIVEALL"),
        true.

all_bibd(V,B,R,K,L,RowColumn,Method,Assign,Output):-
        model(V,B,R,K,L,Matrix,Method),
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       binary_matrix(Matrix),
                       [display:colored]),

        extract_array(Handle,RowColumn,Matrix,List),
        root(Handle),
        assign_term(Assign,Handle,AssignTerm),
        findall(x,timeout((search(List,1,input_order,
                                 AssignTerm,complete,[]),
                           solution(Handle)),
                          10,
                          fail),Sols),
        length(Sols,NrSols),
        writeln(NrSols),
        close_visualization(Handle),
	viz(Handle, _).

bibd(V,B,R,K,L,RowCol,Method,Assign,Output):-
        model(V,B,R,K,L,Matrix,Method),
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       binary_matrix(Matrix),
                       [display:colored]),
        extract_array(Handle,RowCol,Matrix,List),
        root(Handle),
        assign_term(Assign,Handle,AssignTerm),
        search(List,1,input_order,AssignTerm,complete,[]),
        solution(Handle),
        close_visualization(Handle),
	viz(Handle, _).

assign_term(Assign,Handle,AssignTerm):-
        (Assign = indomain ->
            AssignTerm = tree_indomain(Handle,_)
        ; Assign = indomain_max ->
            AssignTerm = tree_indomain_max(Handle,_)
        ;
            writeln(wrong_assign),
            abort
        ).

model(V,B,R,K,L,Matrix,Method):-
        dim(Matrix,[V,B]),
        Matrix[1..V,1..B] :: 0..1,
        (for(I,1,V),
         param(Matrix,B,R) do
            sumup(Matrix[I,1..B],R)
        ),
        (for(J,1,B),
         param(Matrix,V,K) do
            sumup(Matrix[1..V,J],K)
        ),
        (for(I,1,V-1),
         param(Matrix,V,B,L) do
            (for(I1,I+1,V),
             param(Matrix,I,B,L) do
                scalar_product(Matrix[I,1..B],Matrix[I1,1..B],L)
            )
        ),
        (Method = sym ->
            (for(I,1,V-1),
             param(Matrix,B) do
                I1 is I+1,
                strict_order(Matrix[I1,1..B],Matrix[I,1..B])
            ),
            (L = 1 ->
                (for(J,1,B-1),
                 param(Matrix,V) do
                    J1 is J+1,
                    strict_order(Matrix[1..V,J1],Matrix[1..V,J])
                )
            ;
                (for(J,1,B-1),
                 param(Matrix,V) do
                    J1 is J+1,
                    order(Matrix[1..V,J1],Matrix[1..V,J])
                )
            )
        ;
            true
        ).


sumup(A,V):-
        collection_to_list(A,List),
        check_sumup(List,V),
        (nonground(List) ->
            suspend(update_sumup(List,V,Susp),
                    4,List->inst,Susp)
        ;
            true
        ).
%        sumlist(List,V).

strict_order(XVector,YVector):-
        lex_lt(XVector,YVector).

order(XVector,YVector):-
        lex_le(XVector,YVector).
/*
order(XVector,YVector):-
        lex_leq_gac(XVector,YVector).
order(XVector,YVector):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        lexico_le(XList,YList).
*/

scalar_product(XVector,YVector,V):-
        collection_to_list(XVector,XList),
        collection_to_list(YVector,YList),
        append(XList,YList,Vars),
        check_scalar_product(XList,YList,V),
        (nonground(Vars) ->
            suspend(update_scalar_product(Vars,XList,YList,V,Susp),
                    4,Vars->inst,Susp)
        ;
            true
        ).
/*
        (foreach(X,XList),
         foreach(Y,YList),
         fromto(0,A,A1,Term) do
            A1 = A+X*Y
        ),
        eval(Term) #= V.
*/

?-demon(update_sumup/3).
update_sumup(List,V,Susp):-
        check_sumup(List,V),
        (ground(List) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_sumup(List,V):-
        (foreach(X,List),
         fromto(0,A,A1,Ones),
         fromto(0,B,B1,Potential),
         fromto([],C,C1,PList) do
            check_one_sumup(X,A,A1,B,B1,C,C1)
        ),
        Ones =< V, % may fail
        Ones+Potential >= V, % may fail,
        (Ones = V ->
            force_0(PList)
        ; Ones+Potential =:= V ->
            force_1(PList)
        ;
            true
        ),
        wake.

force_0([]).
force_0([0|R]):-
        force_0(R).

force_1([]).
force_1([1|R]):-
        force_1(R).

check_one_sumup(X,A,A,B,B1,C,[X|C]):-
        var(X),
        !,
        B1 is B+1.
check_one_sumup(1,A,A1,B,B,C,C):-
        !,
        A1 is A+1.
check_one_sumup(0,A,A,B,B,C,C).

?-demon(update_scalar_product/5).
update_scalar_product(Vars,XList,YList,V,Susp):-
        check_scalar_product(XList,YList,V),
        (ground(Vars) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_scalar_product(XList,YList,V):-
        (foreach(X,XList),
         foreach(Y,YList),
         fromto(0,B,B1,Potential),
         fromto([],C,C1,PList),
         fromto(0,A,A1,Ones) do
            one_product(X,Y,A,A1,B,B1,C,C1)
        ),
        Ones =< V, % may fail
        Ones+Potential >= V, % may fail
        (Ones = V ->
            unforce_potential(PList)
        ; Ones+Potential =:= V ->
            force_potential(PList)
        ;
            true
        ),
        wake.

unforce_potential([]).
unforce_potential([X-Y|R]):-
        not_both_one(X,Y),
%        X+Y #=< 1,
        unforce_potential(R).

not_both_one(X,Y):-
        ((var(X),var(Y)) ->
            suspend(check_not_both_one(X,Y),4,[X,Y]->inst)
        ;
            check_not_both_one(X,Y)
        ).

check_not_both_one(X,Y):-
        integer(X),
        (X = 0 ; Y = 0),
        !.
check_not_both_one(X,Y):-
        integer(Y),
        (Y = 0 ; X = 0),
        !.


force_potential([]).
force_potential([1-1|R]):-
        force_potential(R).

one_product(X,Y,A,A,B,B1,C,[X-Y|C]):-
        var(X),
        var(Y),
        !,
        B1 is B+1.
one_product(X,Y,A,A,B,B,C,C):-
        var(X),
        integer(Y),
        Y = 0,
        !.
one_product(X,Y,A,A,B,B1,C,[X-Y|C]):-
        var(X),
        !,
        B1 is B+1.
one_product(0,_Y,A,A,B,B,C,C):-
        !.
one_product(1,Y,A,A1,B,B,C,C):-
        integer(Y),
        Y = 1,
        !,
        A1 is A+1.
one_product(1,Y,A,A,B,B,C,C):-
        integer(Y),
        Y = 0,
        !.
one_product(1,Y,A,A,B,B1,C,[1-Y|C]):-
        B1 is B+1.

