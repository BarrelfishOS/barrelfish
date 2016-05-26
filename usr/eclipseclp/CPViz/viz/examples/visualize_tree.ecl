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
:-module(visualize_tree).
:-comment(author,"Helmut Simonis").
:-comment(status,"experimental").
:-comment(copyright,"2010, Helmut Simonis").
:-comment(categories,["Development Tools","Visualisation"]).
:-comment(summary,"Entry point for VIZ visualisation tools").
:-comment(description,"This library is the entry point for the VIZ"
                      " visualization tools. It must be loaded to"
                      " allow generation of log files for the search"
                      " tree and the variable/constraint visuaization.").

:-comment(root/1,[summary:"Create the root node of a search tree",
                  args:["Handle":"an opaque data structure for the visualization"],
                  amode:root(+),
                  desc:html("This predicate creates the root node of a"
                            " search tree in a visualization, this"
                            " should be called before any choices are"
                            " made. It automatically calls"
                            " draw_visualization/1 to collect the"
                            " information about the state of variables"
                            " and constraints."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[solution/1,try/4,failure/4,tree_indomain/3,
                            draw_visualization/1]]).

:-export(root/1).

:-comment(solution/1,[summary:"Create a solution node for a search tree",
                  args:["Handle":"an opaque data structure for the visualization"],
                  amode:solution(+),
                  desc:html("This predicate should be calledn when a"
                            " solution of the constraint problem has"
                            " been found. It create a solution node in"
                            " the tree log, and logs the current state"
                            " of the constraint model for visualization."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,try/4,failure/4,tree_indomain/3]]).

:-export(solution/1).

:-comment(try/4,[summary:"Create a try node for the search tree",
                  args:["Handle":"an opaque data structure for the visualization",
                        "Name":"atomic value, the name of the variable"
                               " to be assigned",
                        "Size":"an integer, the size of the domain of"
                               " the variable being assigned",
                        "Value":"an integer, the value assigned to the"
                                " variable"
                        ],
                  amode:try(+,++,++,++),
                  desc:html("This is an interface that should only be"
                            " used by experienced programmers creating"
                            " their own search routines. It creates a"
                            " try node in the search tree, marking"
                            " that the assignment of a value for a"
                            " variable has succeeded. The different"
                            " versions of tree_indomain/3"
                            " automatically call this predicate, so"
                            " that users should only have to call it"
                            " if their search routine can not be"
                            " expressed with those primitives. This"
                            " predicate does not call draw_visualization/1."),
                  eg:html(""),
                  see_also:[root/1,solution/1,failure/4]]).

:-export(try/4).
:-export(try_c/4).

:-comment(failure/4,[summary:"Create a fail node for the search tree",
                  args:["Handle":"an opaque data structure for the visualization",
                        "Name":"atomic value, the name of the variable"
                               " to be assigned",
                        "Size":"an integer, the size of the domain of"
                               " the variable being assigned",
                        "Value":"an integer, the value assigned to the"
                                " variable"
                        ],
                  amode:failure(+,++,++,++),
                  desc:html("This is an interface that should only be"
                            " used by experienced programmers creating"
                            " their own search routines. It creates a"
                            " failure node in the search tree, marking"
                            " that the assignment of a value for a"
                            " variable has failed. The different"
                            " versions of tree_indomain/3"
                            " automatically call this predicate on"
                            " failure of an assignment, so"
                            " that users should only have to call it"
                            " if their search routine can not be"
                            " expressed with those primitives. This"
                            " predicate does not call draw_visualization/1."),
                  eg:html(""),
                  see_also:[root/1,solution/1,try/4]]).

:-export(failure/4).
:-export(failure_c/4).

:-comment(tree_indomain/3,[summary:"Primitive to assign a variable"
                                   " while creating a search tree",
                  args:["Term":"A term containing the variable to be assigned",
                        "Handle":"an opaque data structure for the visualization",
                        "HandleOut":"will be unified with the Handle argument"],
                  amode:tree_indomain(+,+,?),
                  desc:html("This predicate should be used instead of"
                            " indomain/1 when visualizing the"
                            " execution. Instead of a single variable,"
                            " it expects a term which describes the"
                            " name and index of the variable for"
                            " logging purposes."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,indomain/1,tree_indomain/3,
                            tree_indomain_min/3,tree_indomain_max/3,
                            tree_indomain_middle/3,tree_indomain_random/3]]).

:-export(tree_indomain/3).

:-comment(tree_indomain_min/3,[summary:"Primitive to assign a variable"
                                   " while creating a search tree",
                  args:["Term":"A term containing the variable to be assigned",
                        "Handle":"an opaque data structure for the visualization",
                        "HandleOut":"will be unified with the Handle argument"],
                  amode:tree_indomain_min(+,+,?),
                  desc:html("This predicate should be used instead of"
                            " indomain/2 when visualizing the"
                            " execution. Instead of a single variable,"
                            " it expects a term which describes the"
                            " name and index of the variable for"
                            " logging purposes."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain_min(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,indomain/2,tree_indomain/3,
                            tree_indomain_min/3,tree_indomain_max/3,
                            tree_indomain_middle/3,tree_indomain_random/3]]).

:-export(tree_indomain_min/3).

:-comment(tree_indomain_max/3,[summary:"Primitive to assign a variable"
                                   " while creating a search tree",
                  args:["Term":"A term containing the variable to be assigned",
                        "Handle":"an opaque data structure for the visualization",
                        "HandleOut":"will be unified with the Handle argument"],
                  amode:tree_indomain_max(+,+,?),
                  desc:html("This predicate should be used instead of"
                            " indomain/2 when visualizing the"
                            " execution. Instead of a single variable,"
                            " it expects a term which describes the"
                            " name and index of the variable for"
                            " logging purposes."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain_max(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,indomain/2,tree_indomain/3,
                            tree_indomain_min/3,tree_indomain_max/3,
                            tree_indomain_middle/3,tree_indomain_random/3]]).

:-export(tree_indomain_max/3).

:-comment(tree_indomain_middle/3,[summary:"Primitive to assign a variable"
                                   " while creating a search tree",
                  args:["Term":"A term containing the variable to be assigned",
                        "Handle":"an opaque data structure for the visualization",
                        "HandleOut":"will be unified with the Handle argument"],
                  amode:tree_indomain_middle(+,+,?),
                  desc:html("This predicate should be used instead of"
                            " indomain/2 when visualizing the"
                            " execution. Instead of a single variable,"
                            " it expects a term which describes the"
                            " name and index of the variable for"
                            " logging purposes."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain_middle(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,indomain/2,tree_indomain/3,
                            tree_indomain_min/3,tree_indomain_max/3,
                            tree_indomain_middle/3,tree_indomain_random/3]]).

:-export(tree_indomain_middle/3).

:-comment(tree_indomain_random/3,[summary:"Primitive to assign a variable"
                                   " while creating a search tree",
                  args:["Term":"A term containing the variable to be assigned",
                        "Handle":"an opaque data structure for the visualization",
                        "HandleOut":"will be unified with the Handle argument"],
                  amode:tree_indomain_random(+,+,?),
                  desc:html("This predicate should be used instead of"
                            " indomain/2 when visualizing the"
                            " execution. Instead of a single variable,"
                            " it expects a term which describes the"
                            " name and index of the variable for"
                            " logging purposes."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain_random(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,indomain/2,tree_indomain/3,
                            tree_indomain_min/3,tree_indomain_max/3,
                            tree_indomain_middle/3,tree_indomain_random/3]]).

:-export(tree_indomain_random/3).

:-comment(number_variables/3,[summary:"Create a list of terms for a"
                                      " search routine, numbering the variables",
                  args:["Handle":"a handle to an opaque data structure"
                                 " for the viualization",
                        "L":"a list of variables to be assigned",
                        "Terms":"a variable, will be unified to a list"
                                " of terms"],
                  amode:number_variables(+,+,-),
                  desc:html("This predicate is used to number the"
                            " variables before the search, so that the"
                            " visualizer knows at each step which variable is"
                            " currently assigned."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[number_variables/4,name_variables/4]]).

:-export(number_variables/3).

:-comment(number_variables/4,[summary:"Create a list of terms for a"
                                      " search routine, numbering the variables",
                  args:["Handle":"a handle to an opaque data structure"
                                 " for the viualization",
                        "L":"a list of variables to be assigned",
                        "Group":"an integer, the group identifier",
                        "Terms":"a variable, will be unified to a list"
                                " of terms"],
                  amode:number_variables(+,+,+,-),
                  desc:html("This predicate is used to number the"
                            " variables before the search, so that the"
                            " visualizer knows at each step which variable is"
                            " currently assigned."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[group:1]),\n"
                           "    number_variables(Handle,1,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,\n"
                           "           tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[number_variables/3,name_variables/4]]).
:-export(number_variables/4).

:-comment(name_variables/4,[summary:"Create a list of terms for a"
                                    " search routine, naming the"
                                    " variables with strings",
                            args:["Handle":"an opaque data structure for the visualization",
                                  "L":"a list of domain variables to"
                                      " be assigned",
                                  "Names":"a list of strings, the"
                                          " names of the variables",
                                  "Terms":"a variable, will be unified"
                                          " with a list of terms"
                        ],
                            amode:name_variables(+,+,++,-),
                            desc:html("This predicate creates a list of terms"
                                      " for a search routine creating a"
                                      " visualization. "),
                            eg:ascii("sendmory(L):-
    L=[S,E,N,D,M,O,R,Y],
    L :: 0..9,
    create_visualization([],Handle),
    add_visualizer(Handle,
                   vector(L),
                   []),
    alldifferent(L),
    S #\\= 0,
    M #\\= 0,
    1000*S+100*E+10*N+D + 
    1000*M+100*O+10*R+E #= 
    10000*M + 1000*O+100*N+10*E+Y,

    name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],Terms),
    root(Handle),
    search(Terms,1,input_order,tree_indomain(Handle,_),
           complete,[]),
    solution(Handle),
    close_visualization(Handle)."),
                            see_also:[number_variables/4,name_variables/5]]).

:-export(name_variables/4).

:-comment(name_variables/5,[summary:"Create a list of terms for a"
                                    " search routine, naming the"
                                    " variables with strings",
                            args:["Handle":"an opaque data structure for the visualization",
                                  "L":"a list of domain variables to"
                                      " be assigned",
                                  "Names":"a list of strings, the"
                                          " names of the variables",
                                  "Group":"id of the group these"
                                          " variables belong to",
                                  "Terms":"a variable, will be unified"
                                          " with a list of terms"
                        ],
                            amode:name_variables(+,+,++,++,-),
                            desc:html("This predicate creates a list of terms"
                                      " for a search routine creating a"
                                      " visualization. "),
                            eg:ascii("sendmory(L):-
    L=[S,E,N,D,M,O,R,Y],
    L :: 0..9,
    create_visualization([],Handle),
    add_visualizer(Handle,
                   vector(L),
                   [group:1]),
    alldifferent(L),
    S #\\= 0,
    M #\\= 0,
    1000*S+100*E+10*N+D + 
    1000*M+100*O+10*R+E #= 
    10000*M + 1000*O+100*N+10*E+Y,

    name_variables(Handle,L,['S','E','N','D','M','O','R','Y'],1,Terms),
    root(Handle),
    search(Terms,1,input_order,tree_indomain(Handle,_),
           complete,[]),
    solution(Handle),
    close_visualization(Handle)."),
                            see_also:[number_variables/4,name_variables/5]]).
:-export(name_variables/5).

:-comment(extract_array/4,[summary:"Convert a matrix into a list for"
                                   " search inside a visualization",
                  args:["Handle":"an opaque data structure for the visualization",
                        "RowCol":"atom row or col, controlling the"
                                 " order of items in the list",
                        "Matrix":"a 2D matrix",
                        "List":"a variable, will be unified with a"
                               " list of terms"
                        ],
                  amode:extract_array(+,++,+,-),
                  desc:html("Extract the elements of a matrix inot a"
                            " list in row or column order. The entries"
                            " of the list will be terms which can be"
                            " used inside a search routine with visualization."),
                  eg:ascii("model(Matrix):-
    Matrix[1..9,1..9] :: 1..9,
    create_visualization([],Handle),
    add_visualizer(Handle,
               domain_matrix(Matrix),
               []),
    (for(I,1,9),
     param(Matrix,Method) do
        alldifferent(Matrix[I,1..9]),
        alldifferent(Matrix[1..9,I])
    ),
    (multifor([I,J],[1,1],[7,7],[3,3]),
     param(Matrix) do
        alldifferent(flatten(Matrix[I..I+2,J..J+2]))
    ),
    extract_array(Handle,row,Matrix,NamedList),
    root(Handle),
    search(NamedList,1,input_order,tree_indomain(Handle,_),
           complete,[]),
    solution(Handle),
    close_visualization(Handle).
"),
                  see_also:[extract_array/5]]).

:-export(extract_array/4).

:-comment(extract_array/5,[summary:"Convert a matrix into a list for"
                                   " search inside a visualization",
                  args:["Handle":"an opaque data structure for the visualization",
                        "RowCol":"atom row or col, controlling the"
                                 " order of items in the list",
                        "Group":"integer, a group identifier",
                        "Matrix":"a 2D matrix",
                        "List":"a variable, will be unified with a"
                               " list of terms"
                        ],
                  amode:extract_array(+,++,++,+,-),
                  desc:html("Extract the elements of a matrix inot a"
                            " list in row or column order. The entries"
                            " of the list will be terms which can be"
                            " used inside a search routine with visualization."),
                  eg:ascii("model(Matrix):-
    Matrix[1..9,1..9] :: 1..9,
    create_visualization([],Handle),
    add_visualizer(Handle,
               domain_matrix(Matrix),
               [group:1]),
    (for(I,1,9),
     param(Matrix,Method) do
        alldifferent(Matrix[I,1..9]),
        alldifferent(Matrix[1..9,I])
    ),
    (multifor([I,J],[1,1],[7,7],[3,3]),
     param(Matrix) do
        alldifferent(flatten(Matrix[I..I+2,J..J+2]))
    ),
    extract_array(Handle,row,1,Matrix,NamedList),
    root(Handle),
    search(NamedList,1,input_order,tree_indomain(Handle,_),
           complete,[]),
    solution(Handle),
    close_visualization(Handle).
"),
                  see_also:[extract_array/4]]).
:-export(extract_array/5).

:-lib(ic).
:-lib(lists).
:-use_module(vis_structures).
:-use_module(visualization).
:-reexport visualization.
:-use_module(vis_options).
:-use_module(node_cnt).


root(Handle):-
        root_node(Handle),
        draw_visualization(Handle).


solution(Handle):-
        draw_visualization(Handle),
        solution_node(Handle).

tree_indomain(Term,Handle,Handle):-
        tree_indomain_generic(Term,Handle,Handle,indomain).
tree_indomain_min(Term,Handle,Handle):-
        tree_indomain_generic(Term,Handle,Handle,indomain_min).
tree_indomain_max(Term,Handle,Handle):-
        tree_indomain_generic(Term,Handle,Handle,indomain_max).
tree_indomain_middle(Term,Handle,Handle):-
        tree_indomain_generic(Term,Handle,Handle,indomain_middle).
tree_indomain_random(Term,Handle,Handle):-
        tree_indomain_generic(Term,Handle,Handle,indomain_random).

tree_indomain_generic(Term,Handle,Handle,Type):-
        Handle = visualization{ignore_fixed:IgnoreFixed,
                               var_arg:VarArg,
                               name_arg:NameArg,
                               focus_arg:FocusArg},
        arg(VarArg,Term,X),
        ((integer(X),IgnoreFixed = yes) ->
            true
        ;
            arg(NameArg,Term,Name),
            arg(FocusArg,Term,Focus),
            get_domain_as_list(X,L),
            get_domain_size(X,Size),
            reorganize_domain(X,L,Type,K),
            try_value(Handle,X,K,Name,Size,Focus,Type)
        ).


try_value(Handle,X,[V|_],Name,Size,Focus,_):-
        ((X = V, true) ->
            try(Handle,Name,Size,V),
            focus_option(Focus,FocusOption),
            draw_visualization(Handle,FocusOption)
        ;
            failure(Handle,Name,Size,V),
            fail_option(Focus,V,FailOption),
            draw_visualization(Handle,FailOption),
            fail
        ).
/*
try_value(Handle,X,[V|R],Name,Size,Focus,Type):-
        ((Type = indomain) ->
            % for indomain we don't force negation
            % just continue with remaining values
            try_value(Handle,X,R,Name,Size,Focus,Type)
        ;
            % force negation of assignment
            % if it fails then backtrack
            X #\= V,
            (integer(X) ->
                % if disequality forces the assignment
                % make a try node,
                % but no need to visualize
                try(Handle,Name,1,X),
                focus_option(Focus,FocusOption),
                draw_visualization(Handle,FocusOption)
            ;
                % value removal did succeed, but did not force
                % variable
                % so continue, try other values
                try_value(Handle,X,R,Name,Size,Focus,Type)
            )
        ).
*/
try_value(Handle,X,[V|R],Name,Size,Focus,Type):-
        % on backtracking remove previous value, not done for indomain
        ((Type = indomain) ->
            true
        ;
            % may fail
            X #\= V
        ),
        try_value(Handle,X,R,Name,Size,Focus,Type).



root_node(visualization{tree_stream:Stream}):-
        set_node_cnt(0), % we are in tree search
        printf(Stream,"<root id=\"%d\"/>\n",[0]).

solution_node(visualization{tree_stream:Stream}):-
        current_node_cnt(Id),
        printf(Stream,"<succ id=\"%d\"/>\n",[Id]).


try(Handle,Name,Size,Value):-
        new_node_cnt(Handle,Id,Parent,Stream),
        printf(Stream,"<try id=\"%d\" parent=\"%d\" name=\"%w\" size="
                      "\"%d\" value=\"%d\" />\n",
               [Id,Parent,Name,Size,Value]).

failure(Handle,Name,Size,Value):-
        new_node_cnt(Handle,Id,Parent,Stream),
        printf(Stream,"<fail id=\"%d\" parent=\"%d\" name=\"%w\" size=\"%d\" value=\"%d\" />\n",
               [Id,Parent,Name,Size,Value]).

try_c(Handle,Name,Size,Choice):-
        new_node_cnt(Handle,Id,Parent,Stream),
        printf(Stream,"<tryc id=\"%d\" parent=\"%d\" name=\"%w\" size="
                      "\"%d\" choice=\"%w\" />\n",
               [Id,Parent,Name,Size,Choice]).

failure_c(Handle,Name,Size,Choice):-
        new_node_cnt(Handle,Id,Parent,Stream),
        printf(Stream,"<failc id=\"%d\" parent=\"%d\" name=\"%w\" size=\"%d\" choice=\"%w\" />\n",
               [Id,Parent,Name,Size,Choice]).


focus_option(group(Group,V),[focus(Group,V)]):-
        !.
focus_option(V,[focus(1,V)]).

fail_option(group(Group,V),Value,[failed(Group,V,Value)]):-
        !.
fail_option(V,Value,[failed(1,V,Value)]).

reorganize_domain(_,L,indomain,L):-
        !.
reorganize_domain(_,L,indomain_min,L):-
        !.
reorganize_domain(_,L,indomain_max,K):-
        !,
        reverse(L,K).
reorganize_domain(_,L,indomain_random,K):-
        !,
        shuffle(L,K).
reorganize_domain(X,_L,order(K),Filtered):-
        !,
        (foreach(V,K),
         fromto(Filtered,A1,A,[]),
         param(X) do
            (is_in_domain(V,X) ->
                A1 = [V|A]
            ;
                A1 = A
            )
        ).
reorganize_domain(H,L,indomain_middle,K):-
        !,
        get_bounds(H,Min,Max),
        Middle is (Min+Max)//2,
        reorganize_domain(H,L,Middle,K).
reorganize_domain(X,L,Value,K):-
        integer(Value),
        get_bounds(X,Min,Max),
	( Value =< Min ->
	    % if the starting value is too small, use indomain_min
	    K = L
	; Value >= Max ->
	    % if the starting value is too large, use indomain_max
	    reverse(L,K)
	;
	    % enumerate from a starting value inside the domain
	    % is this enough in all cases ??
	    Range is 2*max(Max-Value,Value-Min)+1,
	    indomain_from(Value,1,Range,L,K)
%	    writeln(indomain_from(Value,1,Range,L,K))
	).

indomain_from(_Value,Inc,Range,_,[]):-
	Range < abs(Inc),
        !.
indomain_from(Value,Inc,Range,L,[Value|K]):-
        memberchk(Value,L),
        !,
	Value1 is Value+Inc,
	Inc1 is -sgn(Inc)*(abs(Inc)+1),
        indomain_from(Value1,Inc1,Range,L,K).
indomain_from(Value,Inc,Range,L,K):-
	Value1 is Value+Inc,
	Inc1 is -sgn(Inc)*(abs(Inc)+1),
        indomain_from(Value1,Inc1,Range,L,K).

number_variables(Handle,L,Terms):-
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:2},
        (foreach(X,L),
         foreach(t(X,J),Terms),
         count(J,1,_) do
            true
        ).

number_variables(Handle,L,Group,Terms):-
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        (foreach(X,L),
         foreach(t(X,J,group(Group,J)),Terms),
         count(J,1,_),
         param(Group) do
            true
        ).

name_variables(Handle,L,Names,Terms):-
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        (foreach(X,L),
         foreach(Y,Names),
         count(J,1,_),
         foreach(t(X,Y,J),Terms) do
            true
        ).

name_variables(Handle,L,Names,Group,Terms):-
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        (foreach(X,L),
         foreach(Y,Names),
         count(J,1,_),
         foreach(t(X,Y,group(Group,J)),Terms),
         param(Group) do
            true
        ).

extract_array(Handle,col,Matrix,List):-
        !,
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        dim(Matrix,[N,M]),
        (multifor([J,I],[1,1],[M,N]),
         count(K,1,_),
         fromto(List,[t(V,K,I-J)|A],A,[]),
         param(Matrix) do
            subscript(Matrix,[I,J],V)
        ).
extract_array(Handle,row,Matrix,List):-
        !,
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        dim(Matrix,[N,M]),
        (multifor([I,J],[1,1],[N,M]),
         count(K,1,_),
         fromto(List,[t(V,K,I-J)|A],A,[]),
         param(Matrix) do
            subscript(Matrix,[I,J],V)
        ).

extract_array(Handle,col,Group,Matrix,List):-
        !,
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        dim(Matrix,[N,M]),
        (multifor([J,I],[1,1],[M,N]),
         count(K,1,_),
         fromto(List,[t(V,K,group(Group,I-J))|A],A,[]),
         param(Matrix,Group) do
            subscript(Matrix,[I,J],V)
        ).
extract_array(Handle,row,Group,Matrix,List):-
        !,
        Handle = visualization{var_arg:1,
                               name_arg:2,
                               focus_arg:3},
        dim(Matrix,[N,M]),
        (multifor([I,J],[1,1],[N,M]),
         count(K,1,_),
         fromto(List,[t(V,K,group(Group,I-J))|A],A,[]),
         param(Matrix,Group) do
            subscript(Matrix,[I,J],V)
        ).
