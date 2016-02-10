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
:-import lex_le/2, lex_lt/2 from ic_global.
:-lib(ic_global_gac).
:-lib(cpviz).


top:-
        bibd(7,7,3,3,1),
/*
        bibd(6,10,5,3,2),
        bibd(7,14,6,3,2),
        bibd(9,12,4,3,1),
        bibd(8,14,7,4,3),
        bibd(6,20,10,3,4),
        bibd(11,11,5,5,2),
        bibd(13,13,4,4,1),
        bibd(7,21,6,2,1),
        bibd(16,20,5,4,1),
        bibd(13,26,6,3,1),
        bibd(6,10,5,3,2),
        bibd(7,7,3,3,1),
        bibd(6,20,10,3,4),
        bibd(9,12,4,3,1),
        bibd(7,14,6,3,2),
        bibd(8,14,7,4,3),
        bibd(6,30,15,3,6),
        bibd(11,11,5,5,2),
        bibd(10,15,6,4,2),
        bibd(7,21,9,3,3),
        bibd(13,13,4,4,1),
        bibd(6,40,20,3,8),
        bibd(9,18,8,4,3),
        bibd(16,20,5,4,1),
        bibd(7,28,12,3,4),
        bibd(6,50,25,3,10),
        bibd(9,24,8,3,2),
        bibd(16,16,6,6,2),
        bibd(15,21,7,5,2),
        bibd(13,26,6,3,1),
        bibd(7,35,15,3,5),
        bibd(15,15,7,7,3),
        bibd(21,21,5,5,1),
        bibd(25,30,6,5,1),
        bibd(10,18,9,5,4),
        bibd(7,42,18,3,6),
        bibd(22,22,7,7,2),
        bibd(7,49,21,3,7),

        bibd(8,28,14,4,6),
        bibd(19,19,9,9,4),
        bibd(10,30,9,3,2),
        bibd(31,31,6,6,1),
        bibd(7,56,24,3,8),
        bibd(9,36,12,3,3),
        bibd(7,63,27,3,9),
        bibd(15,35,7,3,1),
%        bibd(21,28,8,6,2),
        bibd(13,26,8,4,2),
        bibd(11,22,10,5,4),
        bibd(12,22,11,6,5),
        bibd(25,25,9,9,3),
        bibd(16,24,9,6,3),
        bibd(6,50,25,3,10),
        bibd(6,60,30,3,12),
        bibd(10,90,27,3,6),
        bibd(9,108,36,3,9),
        bibd(15,70,14,3,2),
        bibd(12,88,22,3,4),
        bibd(9,120,40,3,10),
        bibd(10,120,36,3,8),
        bibd(13,104,24,3,4),
*/
        true.


/*

find one solution

*/
bibd(V,B,R,K,L):-
        concat_atom([V,'_',B,'_',R,'_',K,'_',L],Root),
        concat_atom(['tree_',Root],TreeRoot),
%       make_configuration(sbno,"TREE",Root),
        create_visualization([tree_root:TreeRoot,
                              output:"Viz_sbno_TREE"],Handle),
        model(V,B,R,K,L,_Matrix,List),
        number_variables(Handle,List,NumberedList),
        root(Handle),
        (search(NumberedList,1,input_order,
                tree_indomain(Handle,_),complete,[]) ->
            solution(Handle)
        ;
            writeln("No solution")
        ),
        close_visualization(Handle),
        viz(Handle, _).

/*

find all solutions

*/


all_bibd(V,B,R,K,L):-
        concat_atom([V,'_',B,'_',R,'_',K,'_',L],Root),
        concat_atom(['tree_',Root],TreeRoot),
%       make_configuration(sbno,"TREE",Root),
        create_visualization([tree_root:TreeRoot,
                              output:"TREE"],Handle),
        model(V,B,R,K,L,_Matrix,List),
        number_variables(Handle,List,NumberedList),
        root(Handle),
        try(NumberedList,Handle),
        close_visualization(Handle),
        viz(Handle, _).

        
try(List,Handle):-
        search(List,1,input_order,tree_indomain(Handle,_),complete,[]),
        solution(Handle),
        fail.
try(_,_).

/*

Model

*/

model(V,B,R,K,L,Matrix,List):-
        writeln(bibd(V,B,R,K,L)),
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
        ),
        flatten_array(Matrix,List).

sumup(A,V):-
        collection_to_list(A,List),
        check_sumup(List,V),
        (nonground(List) ->
            suspend(update_sumup(List,V,Susp),
                    4,List->inst,Susp)
        ;
            true
        ).

% strict_order(XVector,YVector):-!. % TEMP
strict_order(XVector,YVector):-
        lex_lt(XVector,YVector).

% order(XVector,YVector):-!. % TEMP
order(XVector,YVector):-
        lex_le(XVector,YVector).

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


make_configuration(Example,Dir,Root):-
        concat_string([Dir,"/","configuration_",Root,".xml"],File),
        open(File,write,S),
        printf(S,"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",[]),
        printf(S,"<configuration version=\"1.0\" directory=\"examples/%w/%w\" \n",[Example,Dir]),
        printf(S,"               xsi:noNamespaceSchemaLocation=\"configuration.xsd\" \n",[]),
        printf(S,"               xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n",[]),
        printf(S,"	<tool show=\"tree\" type=\"layout\" display=\"compact\" repeat=\"final\" \n",[]),
        printf(S,"              width=\"700\" height=\"700\" fileroot=\"tree_%w\"  />\n",[Root]),
        printf(S,"</configuration>\n",[]),
        close(S).
