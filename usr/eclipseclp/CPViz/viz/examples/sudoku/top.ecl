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
        top(ic,'Viz_sudoku_FC'),
        top(ic_global,'Viz_sudoku_BC'),
        top(ic_global_gac,'Viz_sudoku_DC').

top(Method,Output):-
        problem(Matrix),
        model(Matrix,Method,Output),
        writeln(Matrix).

model(Matrix,Method,Output):-
        Matrix[1..9,1..9] :: 1..9,
        create_visualization([output:Output],Handle),
        add_visualizer(Handle,
                       domain_matrix(Matrix),
                       [display:expanded]),
        draw_visualization(Handle),
        (for(I,1,9),
         param(Matrix,Method,Handle) do
            Method:alldifferent(Matrix[I,1..9]),
            draw_visualization(Handle,[focus(1,row(I))]),
            Method:alldifferent(Matrix[1..9,I]),
            draw_visualization(Handle,[focus(1,col(I))])
        ),
        (multifor([I,J],[1,1],[7,7],[3,3]),
         param(Matrix,Method,Handle) do
            Method:alldifferent(flatten(Matrix[I..I+2,J..J+2])),
            draw_visualization(Handle,[focus(1,block(I,J,3,3))])
        ),
        extract_array(Handle,row,Matrix,NamedList),
        root(Handle),
        search(NamedList,1,input_order,tree_indomain(Handle,Handle),
               complete,[]),
        solution(Handle),
        close_visualization(Handle),
	viz(Handle, [tool{show:tree,display:expanded},tool{show:viz}]).
        
problem([]([](4, _, 8, _, _, _, _, _, _), 
           [](_, _, _, 1, 7, _, _, _, _), 
           [](_, _, _, _, 8, _, _, 3, 2), 
           [](_, _, 6, _, _, 8, 2, 5, _), 
           [](_, 9, _, _, _, _, _, 8, _), 
           [](_, 3, 7, 6, _, _, 9, _, _), 
           [](2, 7, _, _, 5, _, _, _, _), 
           [](_, _, _, _, 1, 4, _, _, _), 
           [](_, _, _, _, _, _, 6, _, 4))).


