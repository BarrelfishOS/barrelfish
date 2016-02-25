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
:-module(visualization).
:-comment(author,"Helmut Simonis").
:-comment(status,"experimental").
:-comment(copyright,"2010, Helmut Simonis").
:-comment(categories,["Development Tools","Visualisation"]).
:-comment(summary,"Definition of the log format for constraint and"
                  " variable visualizers").
:-comment(description,"This library describes how variable and"
                      " constraint visualizers record their"
                      " information in the log files. It also provides"
                      " predicates to start and stop the visualization"
                      " process. The library is re-exported by visualize_tree, it does not need to be loaded independently").


:-comment(create_visualization/2,[summary:"Prepare to generate"
                                          " visualization output",
                  args:["Options":"a list of option pairs of form option:value",
                        "Handle":"a free variable, will be bound to an opaque data structure for the visualization"],
                  amode:create_visualization(+,-),
                  desc:html("<P>This predicate prepares the system for"
                            " visualization output and create a data"
                            " structure Handle which is used by all"
                            " other visualization predicates. For"
                            " every run, the predicate should only be"
                            " called once. It can not be called again,"
                            " until the current visualization is"
                            " closed with close_visualization/1. The"
                            " predicate only prepares the system, it"
                            " does not record a execution state on its own."
			    "</P><P>"
			    "Possible options are:"
			    "<DL>"
			    "<DT>output</DT><DD>"
			    "   atom/string (default 'OUTPUT'), name of directory where log files will be placed"
			    "</DD>"
			    "<DT>ignore_fixed</DT><DD>"
			    "    yes/no (default yes), states if fixed assignments will be ignored and not create tree nodes"
			    "</DD>"
			    "</DL>"
			    "</P>"),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,try/4,failure/4,
                            tree_indomain/3,draw_visualization/1,
                            close_visualization/1]]).

:-export(create_visualization/2).

:-comment(add_visualizer/3,[summary:"Add a visualizer to the visualisation",
                  args:["Handle":"an opaque data structure for the"
                                 " visualization",
                        "Visualizer":"a term, defining the visualizer",
                        "Options":"a list of option:value pairs"
                                  " describing the options to be"
                                  " applied to the visualizer"],
                  amode:add_visualizer(+,++,++),
                  desc:html("<P>
This predicate is used to add a visualizer to an existing
visualization.  It can be called after the visualization has been
created with a create_visualization/2 call.  The second argument is
the description of the visualizer, either for variables or for
constraints.
</P><P>
Variable visualizers display the state and/or evolution of a
collection of variables.  At the moment this can be one of the
following entries:

<table border=1>
<tr>
<td>Variable Visualizer</td>  <td>Description</td>
</tr>
<tr>
<td>vector(L)</td>  <td>The visualizer shows the current state of a
collection of variables.  It marks which variables have been assigned,
which values have been removed and which values remain in the domain.</td>
</tr>
<tr>
<td>vector_waterfall(L)</td>  <td>This visualizer shows the changes of
the collection of variables on the path from the root node to the
current node.  It marks if a variable is assigned, changed (min and
max, min, max or size only), or if it is not modified in each step.</td>
</tr>
<tr>
<td>vector_size(L)</td>  <td>This visualizer shows the change of the
domain sizes for a collection of variables from the root node to the
current node in the search.</td>
</tr>
<tr>
<td>binary_vector(Bool)</td>  <td>This visualizer is a special variant
of the vector visualizer for a collection of 0/1 vairables.  Values
are marked either as unassigned, or as assigned to zero or to one.</td>
</tr>
<tr>
<td>domain_matrix(Matrix)</td>  <td>This visualizer shows a 2D matrix of
domain variables.  Depending on the options, it only shows the
assigned values, or displayed the values remaining in the domain.</td>
</tr>
<tr>
<td>binary_matrix(BoolMatrix)</td>  <td>A specialized version of the
matrix visualizer for 0/1 variables.</td>
</tr>
</table>

</P><P>
Constraint visualizers show the state and/or evolution of a global
constraint.  At the moment, visualizers for the following global
constraints are provided.

<table border=1>
<tr>
<td>Constraint Visualizer</td><td></td>
</tr>
<tr>
<td>alldifferent(Xs)</td><td></td>
</tr>
<tr>
<td>alldifferent_matrix(Matrix)</td><td></td>
</tr>
<tr>
<td>bin_packing(Items,Sizes,Bins)</td><td></td>
</tr>
<tr>
<td>bool_channeling(X,Bool,Start)</td><td></td>
</tr>
<tr>
<td>cumulative(Starts,Durations,Resources,Limit)</td><td></td>
</tr>
<tr>
<td>cumulative(Starts,Durationss,Resources,Limit,End)</td><td></td>
</tr>
<tr>
<td>disjoint2(Rectangles)</td><td></td>
</tr>
<tr>
<td>element(X,Vs,Y)</td><td></td>
</tr>
<tr>
<td>gcc(Limits,Vars)</td><td></td>
</tr>
<tr>
<td>gcc_matrix(RowLimits,ColLimits,Matrix)</td><td></td>
</tr>
<tr>
<td>inverse(Succ,Pred)</td><td></td>
</tr>
<tr>
<td>lex_le(Xs,Ys)</td><td></td>
</tr>
<tr>
<td>lex_lt(Xs,Ys)</td><td></td>
</tr>
<tr>
<td>same(Xs,Ys)</td><td></td>
</tr>
<tr>
<td>sequence_total(Min,Max,Low,Hi,K,ZeroOnes)</td><td></td>
</tr>
<tr>
<td></td><td></td>
</tr>
</table>

</P><P>
Possible Options are:
<DL>
<DT>display</DT><DD>
    influences how the visualizer will be drawn (expanded, text, gantt, ...),
    default: minimal
<DT>group</DT><DD>
    group id number for the visualizer (integer, or 'other')
<DT>x,y</DT><DD>
    position at which the visualizer will be placed (default 0,0)
</DD>
</DL>
</P>
"),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[alldifferent/1,
                            alldifferent_matrix/1,
                            bin_packing/3,
                            bool_channeling/3,
                            cumulative/3,
                            element/3,
                            gcc/2,
                            gcc_matrix/3,
                            inverse/2,
                            lex_le/2,
                            lex_lt/2,
                            same/2,
                            sequence_total/6,
                            create_visualization/2,
                            close_visualization/1,
                            solution/1,try/4,failure/4,tree_indomain/3,
                            draw_visualization/1]]).

:-export(add_visualizer/3).

:-comment(draw_visualization/1,[summary:"Log the current state of the constraint system",
                  args:["Handle":"an opaque data structure for the visualization"],
                  amode:draw_visualization(+),
                  desc:html("This predicate is used to explicitely log the state of the constraint systems for visualization. It is used by the application programmer to show the effect of some setup steps, before the search is started. It is also called automatically by the tree logging predicates, so that a user rarely needs to call it inside a search routine."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    draw_visualization(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,try/4,failure/4,tree_indomain/3,draw_visualization/2]]).


:-export(draw_visualization/1).

:-comment(draw_visualization/2,[summary:"Log the current state of the constraint system",
                  args:["Handle":"an opaque data structure for the visualization",
                        "Options":"a list of option:value pairs"],
                  amode:draw_visualization(+,+),
                  desc:html("This predicate is used to explicitely log the state of the constraint systems for visualization, i.e. create a visualisation time point. It is used by the application programmer to show the effect of some setup steps, before the search is started. It is also called automatically by the tree logging predicates, so that a user rarely needs to call it inside a search routine."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    draw_visualization(Handle,[]),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[root/1,solution/1,try/4,failure/4,tree_indomain/3,draw_visualization/1]]).

:-export(draw_visualization/2).

:-comment(close_visualization/1,[summary:"Stop the visualization, close all log files and flush all file output",
                  args:["Handle":"an opaque data structure for the visualization"],
                  amode:close_visualization(+),
                  desc:html("This predicate should be called at the end of a program to close the visualization logs, flush all file output and reset the internal data structures."),
                  eg:ascii("top(N,L):-\n"
                           "    length(L,N),\n"                
                           "    L :: 1..N,\n"
                           "    alldifferent(L),\n"
                           "    create_visualization([],Handle),\n"
                           "    add_visualizer(Handle,vector(L),[]),\n"
                           "    number_variables(Handle,L,Terms),\n"
                           "    root(Handle),\n"
                           "    search(Terms,1,first_fail,tree_indomain(Handle,_),complete,[]),\n"
                           "    solution(Handle),\n"
                           "    close_visualization(Handle).\n"),
                  see_also:[solution/1,try/4,failure/4,tree_indomain/3,visualization:draw_visualization/1]]).


:-export(close_visualization/1).


:-lib(ic).
:-lib(lists).

:-use_module(node_cnt).
:-use_module(vis_structures).
:-use_module(vis_options).

% counting visualizers
:-local variable(id).
% counting calls to draw_visualizer
:-local variable(number).

create_visualization(Options,Handle):-
        setval(id,0),
        setval(number,0),
        set_node_cnt(-1),
        Handle = visualization{},
        add_options(Options,visualization,Handle),
        default_options(Handle,visualization),
        open_overview(Handle).

open_overview(visualization{root:Root,
                            tree_root:TreeRoot,
                            output:Dir,
                            schema_path:SchemaPath,
                            tree_stream:TreeStream,
                            stream:Stream}):-
	( exists(Dir) -> true ; mkdir(Dir) ),
        concat_string([Dir,'/',Root,".viz"],File),
        concat_string([Dir,'/',TreeRoot,".tre"],TreeFile),
        open(File,write,Stream),
        printf(Stream,"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n",[]),
        printf(Stream,"<visualization version=\"1.0\"\n  xmlns:xsi="
                      "\"http://www.w3.org/2001/XMLSchema-instance\"\n"
                      "  xsi:noNamespaceSchemaLocation=\"%w/visualization.xsd\">\n",[SchemaPath]),
        open(TreeFile,write,TreeStream),
        printf(TreeStream,"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n",[]),
        printf(TreeStream,"<tree version=\"1.0\"\n  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n  xsi:noNamespaceSchemaLocation=\"%w/tree.xsd\">\n",[SchemaPath]).

close_visualization(Handle):-
        close_overview(Handle).

close_overview(visualization{stream:Stream,
                             tree_stream:TreeStream}):-
        printf(Stream,"</visualization>\n",[]),
        close(Stream),
        printf(TreeStream,"</tree>\n",[]),
        close(TreeStream).


add_visualizer(visualization{stream:Stream,
                             visualizers:OpenList},Type,Options):-
        incval(id),
        getval(id,Id),
        type_table(Type,TypeName),
        domain_info(Type,Width,Height,Min,Max),
        Visualizer = visualizer{id:Id,
                                type:Type,
                                type_name:TypeName},
        add_options(Options,visualizer,Visualizer),
        (memberchk(group:_,Options) ->
            true
        ;
            add_options([group:Id],visualizer,Visualizer)
        ),
        add_options([width:Width,
                     height:Height,
                     min:Min,
                     max:Max],visualizer,Visualizer),
        default_options(Visualizer,visualizer),
        store_visualizer(Stream,Visualizer),
        memberchk(Visualizer,OpenList),
        !.

store_visualizer(Stream,visualizer{id:Id,
                                   type_name:TypeName,
                                   display:Display,
                                   x:X,
                                   y:Y,
                                   width:Width,
                                   height:Height,
                                   group:Group,
                                   min:Min,
                                   max:Max}):-
        printf(Stream,"<visualizer id=\"%w\" type=\"%w\" display=\"%w\" ",
               [Id,TypeName,Display]),
        optional(Stream,"x",X,0),
        optional(Stream,"y",Y,0),
        optional(Stream,"width",Width,0),
        optional(Stream,"height",Height,0),
        optional(Stream,"group",Group,'-'),
        optional(Stream,"min",Min,0),
        optional(Stream,"max",Max,0),
        printf(Stream," />\n",[]).

optional(_Stream,_Label,Default,Default):-
        !.
optional(Stream,Label,Var,_Default):-
        printf(Stream," %w=\"%w\"",[Label,Var]).

type_table(vector(_),vector).
type_table(vector_waterfall(_),vector_waterfall).
type_table(vector_size(_),vector_size).
type_table(binary_vector(_),binary_vector).
type_table(binary_matrix(_),binary_matrix).
type_table(domain_matrix(_),domain_matrix).
% constraints
type_table(alldifferent(_),alldifferent).
type_table(alldifferent_matrix(_),alldifferent_matrix).
type_table(bin_packing(_,_,_),bin_packing).
type_table(bool_channeling(_,_,_),bool_channeling).
type_table(cumulative(_,_,_,_),cumulative).
type_table(cumulative(_,_,_,_,_),cumulative).
type_table(cumulative_cost(_,_,_,_,_),cumulative_cost).
type_table(disjoint2(_,_,_),disjoint2).
type_table(element(_,_,_),element).
type_table(gcc(_,_),gcc).
type_table(gcc_matrix(_,_,_),gcc_matrix).
type_table(inverse(_,_),inverse).
type_table(lex_le(_,_),lex_le).
type_table(lex_lt(_,_),lex_lt).
type_table(same(_,_),same).
type_table(sequence_total(_,_,_,_,_,_),sequence_total).

% visualizer specific; 
domain_info(vector(Coll),Width,Height,Min,Max):-
        !,
        collection_to_list(Coll,L),
        length(L,Width),
        get_min_max(L,Min,Max),
        Height is Max-Min+1.
domain_info(vector_waterfall(Coll),Width,Height,Min,Max):-
        !,
        collection_to_list(Coll,L),
        length(L,Width),
        get_min_max(L,Min,Max),
        Height is Width.
domain_info(vector_size(Coll),Width,Height,Min,Max):-
        !,
        collection_to_list(Coll,L),
        length(L,Width),
        Height is Width,
        Min = 0,
        Max = 1.
domain_info(binary_vector(Coll),Width,1,0,1):-
        !,
        collection_to_list(Coll,L),
        length(L,Width).
domain_info(binary_matrix(Matrix),M,N,0,1):-
        !,
        dim(Matrix,[N,M]).
domain_info(domain_matrix(Matrix),M,N,Min,Max):-
        !,
        dim(Matrix,[N,M]),
        flatten_array(Matrix,List),
        get_min_max(List,Min,Max).
domain_info(alldifferent(L),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(alldifferent_matrix(M),Width,Height,Min,Max):-
        !,
        domain_info(domain_matrix(M),Width,Height,Min,Max).
domain_info(bin_packing(Items,Sizes,Bins),Width,Height,Min,Max):-
        !,
        collection_to_list(Items,_L1),
        collection_to_list(Sizes,_L2),
        collection_to_list(Bins,L3),
        length(L3,Width),
        get_min_max(L3,Min,Max),
        Height is Max.
domain_info(bool_channeling(X,_Bool,_Start),Width,Height,Min,Max):-
        !,
        get_integer_bounds(X,_,Width),
        Height = 2,
        Min = 0,
        Max = 0.
domain_info(cumulative(Start,Dur,Res,Limit),Width,Height,Min,Max):-
        !,
        collection_to_list(Start,L1),
        collection_to_list(Dur,L2),
        collection_to_list(Res,_L3),
%        writeln(L3),
        get_integer_bounds(Limit,_,Height),
%        writeln(Height),
        get_last_end(L1,L2,0,Width),
%        writeln(Width-Height),
        Min =0,
        Max = 0.
domain_info(cumulative(Start,Dur,Res,Limit,End),Width,Height,Min,Max):-
        !,
        collection_to_list(Start,L1),
        collection_to_list(Dur,L2),
        collection_to_list(Res,_L3),
%        writeln(L3),
        get_integer_bounds(Limit,_,Height),
        get_integer_bounds(End,_,Width2),
%        writeln(Height),
        get_last_end(L1,L2,0,Width1),
        Width is max(Width1,Width2),
%        writeln(Width-Height),
        Min =0,
        Max = 0.
domain_info(cumulative_cost(Areas,Tasks,Limit,Horizon,_Cost),Width,Height,Min,Max):-
        !,
        writeln(cc(Width)),
        collection_to_list(Areas,_AreasList),
        collection_to_list(Tasks,TasksList),
        get_integer_bounds(Limit,_,Height),
        get_integer_bounds(Horizon,_,Width2),
        writeln(Height),
        writeln(get_last_end(TasksList,0,Width1)),
        get_last_end(TasksList,0,Width1),
        Width is max(Width1,Width2),
        writeln(Width-Height),
        Min =0,
        Max = 0.
domain_info(disjoint2(_L,W,H),Width,Height,Min,Max):-
        !,
        get_integer_bounds(W,_,Width),
        get_integer_bounds(H,_,Height),
        Min = 0,
        Max = 0.
domain_info(element(_X,L,_C),Width,Height,Min,Max):-
        !,
        get_min_max(L,Min,Max),
        length(L,N),
        Width is N+2,
        Height is Max-Min+3.
domain_info(gcc(_,L),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(gcc_matrix(_,_,M),Width,Height,Min,Max):-
        !,
        domain_info(domain_matrix(M),Width,Height,Min,Max).
domain_info(inverse(L,_K),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(same(L,_K),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(sequence_total(_,_,_,_,_,L),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(lex_le(L,_K),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(lex_lt(L,_K),Width,Height,Min,Max):-
        !,
        domain_info(vector(L),Width,Height,Min,Max).
domain_info(Type,_,_,_,_):-
        writeln(no_domain_info(Type)),
        abort.

get_last_end([],[],E,E).
get_last_end([A|A1],[B|B1],E,End):-
        get_integer_bounds(A,_,LastStart),
        get_integer_bounds(B,_,MaxDur),
        E1 is max(E,LastStart+MaxDur),
        get_last_end(A1,B1,E1,End).

get_last_end([],E,E).
get_last_end([Task|A1],E,End):-
        arg(1,Task,Start),
        arg(2,Task,Dur),
        get_integer_bounds(Start,_,LastStart),
        get_integer_bounds(Dur,_,MaxDur),
        E1 is max(E,LastStart+MaxDur),
        get_last_end(A1,E1,End).

binary_domain_rep(X,x):-
        var(X),
        !.
binary_domain_rep(X,X).


get_min_max([H|T],Min,Max):-
        get_integer_bounds(H,Min0,Max0),
        (foreach(X,T),
         fromto(Min0,A,A1,Min),
         fromto(Max0,B,B1,Max) do
            get_integer_bounds(X,Min1,Max1),
            A1 is min(A,Min1),
            B1 is max(B,Max1)
        ).


draw_visualization(Handle):-
        draw_visualization(Handle,[]).

draw_visualization(Handle,_Options):-
        var(Handle),
        !.
draw_visualization(Handle,Options):-
        Handle = visualization{stream:Stream,
                               visualizers:OpenList,
                               range_from:From,
                               range_to:To},
        incval(number),
        getval(number,Number),
        current_node_cnt(TreeNode),
        (Number >= From,Number =< To ->
            printf(Stream,"<state id=\"%d\" tree_node=\"%d\" >\n",
                   [Number,TreeNode]),
            draw_lp(OpenList,Options,Handle,Stream),
            printf(Stream,"</state>\n",[])
        ;
            true
        ).



draw_lp(X,_,_,_):-
        var(X),
        !.
draw_lp([H|T],Options,Handle,Stream):-
        draw_visualizer(H,Options,Handle,Stream),
        draw_lp(T,Options,Handle,Stream).

draw_visualizer(H,Options,_Handle,Stream):-
        H = visualizer{id:Id,type:Type},
        visualizer_state(Stream,Id,Type,Options).

visualizer_state(Stream,Id,Type,Options):-
        printf(Stream,"<visualizer_state id=\"%d\" >\n",[Id]),
%        writeln(draw_type(Type)),
        draw_type(Stream,Type),
        draw_option(Stream,Options),
        printf(Stream,"</visualizer_state>\n",[]).

draw_option(Stream,Options):-
        (delete(focus(Term),Options,O1a) ->
            treat_focus(Stream,'-',Term)
        ;
            O1a = Options
        ),
        (delete(focus(Group,Term),O1a,O1) ->
            treat_focus(Stream,Group,Term)
        ;
            O1 = O1a
        ),
        (delete(failed(Index,Value),O1,O2a) ->
            treat_failed(Stream,'-',Index,Value)
        ;
            O2a = O1
        ),
        (delete(failed(Group,Index,Value),O2a,O2) ->
            treat_failed(Stream,Group,Index,Value)
        ;
            O2 = O2a
        ),
        (delete(focus:Term,O2,O3) ->
            treat_focus(Stream,'-',Term)
        ;
            O3 = O2
        ),
        (O3 = [] ->
            true
        ;
            writeln(unknown_option(O2))
        ).

treat_failed(Stream,Group,Index,Value):-
        integer(Index),
        integer(Value),
        !,
        printf(Stream,"<failed",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," index=\"%w\" value=\"%w\" />\n",
               [Index,Value]).
treat_failed(Stream,Group,I-J,Value):-
        integer(I),
        integer(J),
        integer(Value),
        !,
        printf(Stream,"<failed",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," index=\"%w %w\" value=\"%w\" />\n",
               [I,J,Value]).
treat_failed(_Stream,Group,Index,Value):-
        writeln(unknown_failed(Group,Index,Value)),
        abort.



treat_focus(Stream,Group,I):-
        integer(I),
        !,
        printf(Stream,"<focus",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," index=\"%w\" />\n",
               [I]).
treat_focus(Stream,Group,I-J):-
        integer(I),
        integer(J),
        !,
        printf(Stream,"<focus",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," index=\"%w %w\" />\n",
               [I,J]).
treat_focus(Stream,Group,row(I)):-
        !,
        printf(Stream,"<focus",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," type=\"%w\" index=\"%w\" />\n",
               [row,I]).
treat_focus(Stream,Group,col(I)):-
        !,
        printf(Stream,"<focus",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," type=\"%w\" index=\"%w\" />\n",
               [col,I]).
treat_focus(Stream,Group,block(X,Y,W,H)):-
        !,
        printf(Stream,"<focus",[]),
        optional(Stream,"group",Group,'-'),
        printf(Stream," type=\"%w\" index=\"%w %w %w %w\" />\n",
               [block,X,Y,W,H]).
treat_focus(_Stream,Group,Index):-
        writeln(unknown_focus(Group,Index)),
        abort.

draw_type(Stream,bin_packing(L1,L2,L3)):-
        !,
        printf(Stream,"<argument index=\"items\" >\n",[]),
        draw_type(Stream,vector(L1)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"sizes\" >\n",[]),
        draw_type(Stream,vector(L2)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"bins\" >\n",[]),
        draw_type(Stream,vector(L3)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,bool_channeling(X,Bool,Start)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        vector_entry(Stream,X,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(Bool)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"3\" >\n",[]),
        vector_entry(Stream,Start,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,cumulative(L1,L2,L3,Limit)):-
        !,
        printf(Stream,"<argument index=\"tasks\" >\n",[]),
        draw_tuples(Stream,L1,L2,L3,["start","dur","res"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"limit\" >\n",[]),
        vector_entry(Stream,Limit,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,cumulative(L1,L2,L3,Limit,End)):-
        !,
        printf(Stream,"<argument index=\"tasks\" >\n",[]),
        draw_tuples(Stream,L1,L2,L3,["start","dur","res"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"limit\" >\n",[]),
        vector_entry(Stream,Limit,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"end\" >\n",[]),
        vector_entry(Stream,End,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,cumulative_cost(Areas,Tasks,Limit,End,Cost)):-
        !,
        writeln(draw_type),
        printf(Stream,"<argument index=\"areas\" >\n",[]),
        draw_tuples(Stream,Areas,["x","y","width","height","cost"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"tasks\" >\n",[]),
        draw_tuples(Stream,Tasks,["start","dur","res","lp"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"limit\" >\n",[]),
        vector_entry(Stream,Limit,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"end\" >\n",[]),
        vector_entry(Stream,End,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"cost\" >\n",[]),
        vector_entry(Stream,Cost,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,disjoint2(Rect,W,H)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_tuples(Stream,Rect,["x","y","w","h"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        vector_entry(Stream,W,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"3\" >\n",[]),
        vector_entry(Stream,H,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,element(X,L,C)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        vector_entry(Stream,X,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"3\" >\n",[]),
        vector_entry(Stream,C,1),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,gcc(Param,L)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_tuples(Stream,Param,["low","high","value"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,gcc_matrix(ParamRow,ParamCol,M)):-
        writeln(ParamRow),
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_collection_tuples(Stream,ParamRow,["low","high","value"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_collection_tuples(Stream,ParamCol,["low","high","value"]),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"3\" >\n",[]),
        draw_type(Stream,domain_matrix(M)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,inverse(L,K)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(K)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,lex_le(L,K)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(K)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,lex_lt(L,K)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(K)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,same(L,K)):-
        !,
        printf(Stream,"<argument index=\"1\" >\n",[]),
        draw_type(Stream,vector(L)),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"2\" >\n",[]),
        draw_type(Stream,vector(K)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,sequence_total(TotalLow,TotalHigh,Low,High,N,Binary)):-
        !,
        printf(Stream,"<argument index=\"TotalLow\" >\n",[]),
        vector_entry(Stream,TotalLow,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"TotalHigh\" >\n",[]),
        vector_entry(Stream,TotalHigh,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"Low\" >\n",[]),
        vector_entry(Stream,Low,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"High\" >\n",[]),
        vector_entry(Stream,High,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"N\" >\n",[]),
        vector_entry(Stream,N,1),
        printf(Stream,"</argument>\n",[]),
        printf(Stream,"<argument index=\"Binary\" >\n",[]),
        draw_type(Stream,vector(Binary)),
        printf(Stream,"</argument>\n",[]).
draw_type(Stream,Term):-
        functor(Term,F,1),
        memberchk(F,[vector,vector_waterfall,
                     vector_size,binary_vector,alldifferent]),
        arg(1,Term,L),
        !,
        collection_to_list(L,List),
        (foreach(X,List),
         count(J,1,_),
         param(Stream) do
            vector_entry(Stream,X,J)
        ).
draw_type(Stream,Term):-
        functor(Term,F,1),
        memberchk(F,[domain_matrix,binary_matrix,
                     alldifferent_matrix]),
        arg(1,Term,Matrix),
        !,
        (foreachelem(X,Matrix,[I,J]),
         param(Stream) do
            matrix_entry(Stream,X,I,J)
        ).
draw_type(_Stream,Type):-
        writeln(unknown_type(Type)),
        abort.


draw_collection_tuples(Stream,Lists,Keys):-
        (foreach(X,Lists),
         count(J,1,_),
         param(Stream,Keys) do
            printf(Stream,"<collection index=\"%d\" >\n",[J]),
            draw_tuples(Stream,X,Keys),
            printf(Stream,"</collection>\n",[])
        ).



draw_tuples(Stream,L,Keys):-
        (foreach(X,L),
         count(J,1,_),
         param(Stream,Keys) do
            tuple_entry(Stream,J,X,Keys)
        ).

tuple_entry(Stream,J,X,Keys):-
        printf(Stream,"<tuple index=\"%d\" >\n",[J]),
        (foreach(Key,Keys),
         count(J,1,_),
         param(Stream,X) do
            arg(J,X,V),
            vector_entry(Stream,V,Key)
        ),
        printf(Stream,"</tuple>\n",[]).

draw_tuples(Stream,L1,L2,L3,Keys):-
        (foreach(X,L1),
         foreach(Y,L2),
         foreach(Z,L3),
         count(J,1,_),
         param(Stream,Keys) do
            tuple_entry(Stream,J,X,Y,Z,Keys)
        ).

tuple_entry(Stream,J,X,Y,Z,[Key1,Key2,Key3]):-
        printf(Stream,"<tuple index=\"%d\" >\n",[J]),
        vector_entry(Stream,X,Key1),
        vector_entry(Stream,Y,Key2),
        vector_entry(Stream,Z,Key3),
        printf(Stream,"</tuple>\n",[]).

        
        
vector_entry(Stream,X,J):-
        integer(X),
        !,
        printf(Stream,"<integer index=\"%w\" value=\"%d\" />\n",
               [J,X]).
vector_entry(Stream,X,J):-
        is_solver_var(X),
        !,
        get_domain_as_white_spaced_list(X,Domain),
        printf(Stream,"<dvar index=\"%w\" domain=\"%w\" />\n",
               [J,Domain]).
vector_entry(_Stream,Var,_J):-
        var(Var),
        !.
vector_entry(Stream,List,J):-
%        writeln(other(List,J)),
        printf(Stream,"<collection index=\"%w\">\n",[J]),
        (foreach(lp(Time,Value),List),
         count(K,1,_),
         param(Stream) do
            printf(Stream,"<tuple index=\"%w\">\n",[K]),
            printf(Stream,"<integer index=\"%w\" value=\"%d\" />\n",
                   [time,Time]),
            IntValue is integer(round(Value*10000)),
            printf(Stream,"<integer index=\"%w\" value=\"%d\" />\n",
                   [value,IntValue]),
%            printf(Stream,"<number index=\"%w\" value=\"%7.5f\" />\n",
%                   [value,Value]),
            printf(Stream,"</tuple>\n",[])
        ),
        printf(Stream,"</collection>\n",[]).
matrix_entry(Stream,X,I,J):-
        integer(X),
        !,
        printf(Stream,"<integer index=\"%d %d\" value=\"%d\" />\n",
               [I,J,X]).
matrix_entry(Stream,X,I,J):-
        get_domain_as_white_spaced_list(X,Domain),
        printf(Stream,"<dvar index=\"%d %d\" domain=\"%w\" />\n",
               [I,J,Domain]).

get_domain_as_white_spaced_list(X,Domain):-
        get_domain(X,EclipseDomain),
        convert_to_white_spaced_list(EclipseDomain,Domain).

convert_to_white_spaced_list(A..B,Domain):-
        !,
        join_string([A,"..",B]," ",Domain).
convert_to_white_spaced_list(EclipseDomain,Domain):-
%        writeln(EclipseDomain),
        (foreach(A,EclipseDomain),
         foreach(B,Entries) do
            convert_entry(A,B)
        ),
        join_string(Entries," ",Domain).

convert_entry(X,X):-
        integer(X),
        !.
convert_entry(A..B,Entry):-
        join_string([A,"..",B]," ",Entry).
