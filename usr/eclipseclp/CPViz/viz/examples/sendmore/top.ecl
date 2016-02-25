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
:- module(top).
:-export(top/0).

:-lib(ic).
:-lib(cpviz).

top:-
        sendmory_complete("Viz_sendmore_ALL",no),
        sendmory_wrong("Viz_sendmore_WRONG",no),
        sendmory_split("Viz_sendmore_CARRY",no),
	true.


sendmory(L,Output,IgnoreFixed):-
	L=[S,E,N,D,M,O,R,Y],
	L :: 0..9,
        create_visualization([output:Output,
                              ignore_fixed:IgnoreFixed],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        
	alldifferent(L),
        draw_visualization(Handle),
	S #\= 0,
        draw_visualization(Handle),
	M #\= 0,
        draw_visualization(Handle),
	
	1000*S+100*E+10*N+D + 
	1000*M+100*O+10*R+E #= 
	10000*M + 1000*O+100*N+10*E+Y,

        name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],Pairs),
        root(Handle),
        search(Pairs,1,input_order,tree_indomain(Handle,_),
               complete,[]),
        solution(Handle),
        close_visualization(Handle),
        viz(Handle, _).

sendmory_complete(Output,IgnoreFixed):-
	L=[S,E,N,D,M,O,R,Y],
	L :: 0..9,
        create_visualization([output:Output,
                              ignore_fixed:IgnoreFixed],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        
	alldifferent(L),
        draw_visualization(Handle),
	S #\= 0,
        draw_visualization(Handle),
	M #\= 0,
        draw_visualization(Handle),
	
	1000*S+100*E+10*N+D + 
	1000*M+100*O+10*R+E #= 
	10000*M + 1000*O+100*N+10*E+Y,

        name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],Pairs),
        root(Handle),
        findall(x,
                (search(Pairs,1,input_order,tree_indomain(Handle,_),
                       complete,[]),
                 solution(Handle)),
                _),
        close_visualization(Handle),
        viz(Handle, _).

sendmory_wrong(Output,IgnoreFixed):-
	L=[S,E,N,D,M,O,R,Y],
	L :: 0..9,
        create_visualization([output:Output,
                              ignore_fixed:IgnoreFixed],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        
	alldifferent(L),
        draw_visualization(Handle),
	
	1000*S+100*E+10*N+D + 
	1000*M+100*O+10*R+E #= 
	10000*M + 1000*O+100*N+10*E+Y,

        name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],Pairs),
        root(Handle),
        findall(x,(search(Pairs,1,input_order,
                          tree_indomain(Handle,_),complete,[]),
                   solution(Handle)),_),
        close_visualization(Handle),
        viz(Handle, _).

sendmory_split(Output,IgnoreFixed):-
	L=[S,E,N,D,M,O,R,Y],
	L :: 0..9,
        create_visualization([output:Output,
                              ignore_fixed:IgnoreFixed],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        
        [C2,C3,C4,C5] :: 0..1,
	alldifferent(L),
        draw_visualization(Handle),
	S #\= 0,
        draw_visualization(Handle),
	M #\= 0,
        draw_visualization(Handle),
	

        M #= C5,
        draw_visualization(Handle),
        S+M+C4 #= 10*C5+O,
        draw_visualization(Handle),
        E+O+C3 #= 10*C4+N,
        draw_visualization(Handle),
        N+R+C2 #= 10*C3+E,
        draw_visualization(Handle),
        D+E    #= 10*C2+Y,

        name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],Pairs),
        root(Handle),
        findall(x,(search(Pairs,1,input_order,
                          tree_indomain(Handle,_),complete,[]),
                   solution(Handle)),_),
        close_visualization(Handle),
        viz(Handle, _).

