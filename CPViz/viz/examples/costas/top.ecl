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
:-lib(propia).
:-lib(lists).
:-lib(cpviz).

top:-
        costas(naive,12,ic,"Viz_constas_NAIVE"),
%        costas_tree(naive,13,ic,"NAIVE"),
%        costas_tree(naive,14,ic,"NAIVE"),
%        costas_tree(naive,15,ic,"NAIVE"),
%        costas_tree(naive,16,ic,"NAIVE"),
        costas(model,12,ic,"Viz_constas_MODEL"),
%        costas_tree(model,13,ic,"MODEL"),
%        costas_tree(model,14,ic,"MODEL"),
%        costas_tree(model,15,ic,"MODEL"),
%        costas_tree(model,16,ic,"MODEL"),
        costas(middle,12,ic,"Viz_constas_MIDDLE"),
%        costas_tree(middle,13,ic,"MIDDLE"),
%        costas_tree(middle,14,ic,"MIDDLE"),
%        costas_tree(middle,15,ic,"MIDDLE"),
%        costas_tree(middle,16,ic,"MIDDLE"),
%        costas_tree(middle,17,ic,"MIDDLE"),
%        costas_tree(middle,18,ic,"MIDDLE"),
        true.

costas(Type,N,Module,Output):-
        length(L,N),
        L :: 1..N,
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:compact]),
        (memberchk(Type,[model,middle]) ->
            ic_global_gac:alldifferent(L)
        ;
            Module:alldifferent(L)
        ),
        L = [_|L1],
        diffs(L,L1,Module,L,_Vars,Type),
        number_variables(Handle,L,K),
        middle_out(K,KK),
        root(Handle),
        (memberchk(Type,[middle]) ->
            search(KK,1,first_fail,tree_indomain_middle(Handle,_),complete,[])
        ;
            search(K,1,first_fail,tree_indomain(Handle,_),complete,[])
        ),
        writeln(L),
        solution(Handle),
        close_visualization(Handle),
        viz(Handle, _Default),
        !.

costas_tree(Type,N,Module,Output):-
        length(L,N),
        L :: 1..N,
        concat_atom(["tree_",N],TreeRoot),
        make_configuration(costas,Output,N),
        create_visualization([output:Output,
                              tree_root:TreeRoot,
                              root:dummy],Handle),
        (memberchk(Type,[model,middle]) ->
            ic_global_gac:alldifferent(L)
        ;
            Module:alldifferent(L)
        ),
        L = [_|L1],
        diffs(L,L1,Module,L,_Vars,Type),
        number_variables(Handle,L,K),
        middle_out(K,KK),
        root(Handle),
        (memberchk(Type,[middle]) ->
            search(KK,1,first_fail,tree_indomain_middle(Handle,_),complete,[])
        ;
            search(K,1,first_fail,tree_indomain(Handle,_),complete,[])
        ),
        writeln(L),
        solution(Handle),
        close_visualization(Handle),
        viz(Handle, []),
        !.

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

diffs(_,[],_,V,V,_Type).
diffs(L,[H|T],Module,V,Vend,Type):-
        diff_pairs(L,[H|T],Diffs,V,V1,Triples,Type),
        (memberchk(Type,[naive]) ->
            true
        ;
            impose_triples(Triples,[])
        ),
        Module:alldifferent(Diffs),
        diffs(L,T,Module,V1,Vend,Type).

diff_pairs(_,[],[],V,V,[],_Type).
diff_pairs([X|X1],[Y|Y1],[D|D1],V,Vend,[t(X,Y,D)|T],Type):-
        X #= Y+D,
        (memberchk(Type,[model]) ->
            triple(X,Y,D) infers ac
        ;
            true
        ),
        diff_pairs(X1,Y1,D1,[D|V],Vend,T,Type).

triple(X,Y,D):-
        X #= Y+D,
        labeling([X,Y,D]).

impose_triples([],_).
impose_triples([t(X,Y,D)|R],Others):-
        suspend(impose_triple(D,R),4,D->inst),
        suspend(impose_triple(D,Others),4,D->inst),
        impose_triples(R,[t(X,Y,D)|Others]).

impose_triple(D,L):-
        impose_triple1(D,L).

impose_triple1(_D,[]).
impose_triple1(D,[t(X,Y,_)|R]):-        
        (var(X) ->
            suspend(impose_one_triple12(D,X,Y),4,X->inst)
        ;
            impose_one_triple12(D,X,Y)
        ),
        (var(Y) ->
            suspend(impose_one_triple13(D,X,Y),4,Y->inst)
        ;
            impose_one_triple13(D,X,Y)
        ),
        impose_triple1(D,R).

impose_one_triple12(D,X,Y):-
        V is X-D,
        Y #\= V.

impose_one_triple13(D,X,Y):-
        V is Y+D,
        X #\= V.

