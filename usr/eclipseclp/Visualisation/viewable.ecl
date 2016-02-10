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
% Copyright (C) 200 1- 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Josh Singer, Parc Technologies
% 
% END LICENSE BLOCK

:- module(viewable).

:- use_module(vc_support).

:- reexport  viewable_create/2,
             viewable_create/3,
             viewable_create/4,
             viewable/1,
             viewable_size/2,
             viewable_type/2,
             viewable_expand/3,
             viewable_expand/4 from vc_support.

:- comment(categories, ["Development Tools","Visualisation"]).
:- comment(summary, 
	   "Module for the management of viewables: arrays for visualisation").

:- comment(author, "Josh Singer").


:- comment(desc, html("<P> Module for the management of
           <EM>viewables</EM>. These are logical, multidimensional
           arrays and graph structures whose elements can be accessed
           globally for visualisation purposes. The creation,
           destruction and modification of viewables and their
           contents can be monitored by visualisation clients. </P>

           <P> The library <CODE>lib(graph_algorithms)</CODE> contains
           predicates to create and manipulate graph structures.

           <P> Viewables are created using <CODE>viewable_create/2/3/4</CODE>
            and last until this call is
           backtracked over. During its lifetime, a viewable is said to be
           <EM>existent</EM>. This can be tested with
           <CODE>viewable/1</CODE>, which can also be used to enumerate all
           existent viewables. </P>

           <P> One or more of the dimensions of a viewable can
           optionally be flexible in size while the viewable is
           existent. This allows new elements to be added to the
           viewable, using <CODE>viewable_expand/3/4</CODE>. The
           <EM>fixity</EM> of a dimension denotes whether it is fixed
           or flexible. In the case of graph typed viewables, the
           fixity refers to the ability to add new nodes and/or edges
           to the graph.  Currently only <EM>fixed</EM> graphs are
           supported.

           <P> A viewable is given a type when it is created. For
           array types, this specifies the number of dimensions, the
           fixity of each dimension and the type of viewable elements
           which the viewable can contain. For graph types, this
           simply specifies whether the graph structure (number of
           nodes, edge direction etc) may be modified during the
           viewables lifetime. An existent viewable's type can be
           retrieved using <CODE>viewable_type/2</CODE>, and its
           current sizes of the dimensions can be retrieved using
           <CODE>viewable_size/2.</code> </P>

           <h3>Optimised behaviour without visualisation</h3> Visualisation
           clients can only monitor viewables whose creation occurs
           <em>after</em> they register. When there are no registered
           visualisation clients, for efficiency purposes no data on
           viewables is stored and so the predicates in this module behave
           slightly differently from normal:

           <ul>

           <li><code>viewable_create/2/3/4</code> and
           <code>viewable_expand/3/4</code> succeed with any arguments. It is
           therefore reasonable to leave these predicates in your code even
           when not visualising.</li>

           <li><code>viewable/1</code>, <code>viewable_size/2</code> and
           <code>viewable_type/2</code> fail whatever the arguments. If
           these predicates are used (normally they should not be
           necessary) the possibility of failure should be taken into
           account.</li>

           </ul>

")).


:- comment(viewable_create/3, 
	   [amode : viewable_create(?, +, ++), 
	    args : ["ViewableName" : 
		 "A string or atom; the name of the new viewable.", 
		 "Elements" : 
		 "A list nested to at least a depth equal to the"
                 " number of dimensions, an array with sufficient"
                 " dimensions (as created for example by dim/2) or"
                 " graph structure.",
		 "Type" : "A ground term which is a valid viewable type. "],
	    summary : "Create a viewable, specifying type.",
	    eg: "

[Assuming that at least one visualisation client is registered] 

       Success:

       viewable_create(viewable1, [X, Y, Z], array([fixed], any)).

       lib(fd), fd:([X, Y, Z]::1..10), 
       viewable_create(viewable1, [X, Y, Z], array([fixed], numeric_bounds)).

       viewable_create(viewable1, [3.2, 5.00__5.01, 7], 
                       array([fixed], numeric_bounds)).

       viewable_create(\"viewable1\", [](X, Y, Z), array([flexible], any)).

       viewable_create(m, [], array([flexible], any)).

       viewable_create(var23, [[],[],[]], array([fixed, flexible], any)).

       viewable_create(var315, []([]([](R, T, [E, X, Y]), 
                                     [](W, T, grok(D))), 
				  []([](A, B, C),
				     [](R, E, W))), 
		       array([fixed, flexible, flexible], any)).

       viewable_create(m,[[],[],[]],array([fixed, flexible, flexible],any)).
       [Note: the initial size of this viewable would be 3 x 0 x 0]

       lib(graph_algorithms),
       make_graph(3,[e(1,2,e1), e(2,3,e2), e(1,3,e3)], Graph),
       viewable_create(m, Graph, graph(fixed)).
       [Note: the node will be labelled with the numbers '1', '2' and '3',
       the edges labelled 'e1', 'e2' and 'e3']
       
       lib(graph_algorithms),
       make_graph_symbolic([](n1,n2,n3),[edge(n1,n2,e1), edge(n2,n3,e2), edge(n1,n3,e3)], Graph),
       viewable_create(m, Graph, graph(fixed)).
       [Note: the node will be labelled with the strings 'n1', 'n2' and 'n3',
       the edges labelled 'e1', 'e2' and 'e3']

       lib(eplex),
       eplex_instance(my_instance),
       my_instance: eplex_solver_setup(min(X)),
       viewable_create(eplex,[X,Y],array([fixed],changeable(my_instance,numeric_bounds))),
       my_instance: (X+Y >= 3),
       my_instance: (X-2*Y =:= 0),
       my_instance: eplex_solve(Cost).       
       [Note: this creates a single viewable using the standalone eplex
        library and shows how to visualise the bounds stored in the external
        solver]


       Exceptions raised:

       viewable_create(2, [x,y,z], array([fixed], any)).
       [gives error 4] 

       viewable_create(v1, [x,y,z], array([fixed], any)), 
       viewable_create(v1, [x,y,z], array([fixed], any)).
       [gives error 1] 

       viewable_create(v1, _, array([fixed], any)).
       [gives error 1] 

       viewable_create(v1, [x,y,g], array([fixed, fixed], any)).
       [gives error 1] 

       viewable_create(v1, [x,y,x], array([], any)).
       [gives error 1] 

       viewable_create(v1, [x,y,x], slorg([fixed], any)).
       [gives error 1] 

       viewable_create(v1, [x,y,x], nurg).
       [gives error 1] 

       viewable_create(v1, [x,y,x], numeric_bounds).
       [gives error 5] 
				    ",

	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate succeeds with any arguments, and has no effect.</P>

	This predicate creates a new viewable by specifying its name,
	elements and type.  At present the type must be of the form
	array(FixityList, ElementType) or graph(fixed) where <ul>
	<li>FixityList is a list with an atom <code>fixed</code> or
	<code>flexible</code> specifying the fixity for each
	dimension. The fixity denotes whether the dimension's size is
	fixed or may vary during the time when the viewable is
	existent.</li> <li>ElementType is a term which specifies the
	type of the constituent viewable elements.  Currently there
	are two supported element types: <ul> <li><code>any</code>
	which includes any ECLiPSe term</li> <li>
	<code>numeric_bounds</code> which includes any ground number,
	integer <code>fd</code> variables, <code>ic</code> variables
	and <code>range</code> variables (including <code>eplex</code>
	and <code>ria</code> variables). </li> <li>
	<code>changeable(Module,Type)</code> which indicates that the
	value should be accessed through the <em>changeable</em>
	interface of the given Module
	(eg. changeable(eplex_instance,any) indicates that the value
	is ths solution assigned to the variable by the given
	eplex_instance).  The <code>Type</code> parameter can be any
	of the <em>other</em> ElementTypes in this list.</li> </ul>
	</li> </ul>

	Visualisation clients, if there are any, are notified of the new
	viewable when it is created. On backtracking over a call to
	<code>viewable_create/3</code>, the viewable is destroyed, and
	visualisation clients are also notified of this. </P><P>

	<EM>ViewableName</EM> is a global handle which can be used to
	access the viewable while it is existent. If <EM>ViewableName</EM>
	is a variable, it is instantiated to an atom. The atom is unique in
	the sense that no two existent viewables will share the same name
	at any point during the lifetime of the ECLiPSe engine.</P><P>

        For array types, the number of dimensions is specified by the
        length of <em>FixityList</em> in <em>Type</em>. <EM>Elements</EM>
        is a nested list or array. The depth of nesting is usually at
        least the number of dimensions specified in <em>FixityList</em>.
        However, dimensions with an initial size of 0 are also
        allowed, and these can implicitly contain an arbitrary number
        of further dimensions, also assumed to be of initial size
        0. So for example an <em>Elements</em> argument of
        <code>[[],[],[]]</code> or <code>[]([],[],[])</code> can be
        used with a <em>FixityList</em> of <code>[fixed,
        flexible]</code>, giving an initial size of 3 x 0, or used
        with a <em>FixityList</em> of <code>[fixed, flexible,
        flexible, flexible]</code>, giving an initial size of 3 x 0 x
        0 x 0. Be sure to make any initially empty dimensions
        flexible!  <em>Elements</em> must also be regular: each inner
        list / array at the same level of nesting must be the same
        length, down to the level equal to the number of dimensions.

        <p><code>viewable_create/3</code> sets location names (e.g. row/column
        names) to \"1\", \"2\", etc. For more control over location names use 
        <code>viewable_create/4</code>.</P>"), 
	    see_also : [viewable_create/2, viewable_create/4,
                        viewable_expand/3, graph_algorithms:make_graph/3],
            exceptions : [
	    4 : "ViewableName is not an atom, string or free variable.",
	    1 : "ViewableName is the name of an existent viewable.",
	    1 : "Elements is not a regular nested list or array with enough dimensions.", 
	    1 : "Type is not a ground valid type.", 
	    5 : "One of the viewable elements violates the element type."],
            resat: no
]).



:- comment(viewable_create/4, 
	   [amode : viewable_create(?, +, ++, ++), 
	    args : ["ViewableName" : 
		 "A string or atom; the name of the new viewable.", 
		 "Elements" : 
		 "A list nested to at least a depth equal to the"
                 " number of dimensions, or an array with sufficient"
                 " dimensions (as created for example by dim/2) or a"
                 " graph structure.",
		 "Type" : "A ground term which is a valid viewable type. ",
		 "LocNamesList" : "A list of lists of strings"
                                  " (location names) or a list of"
                                  " semantic markup terms from graph"
                                  " structures."],
	    summary : "Create a viewable, specifying both type and location names.",
	    eg: "

            To create a 2 x 3 viewable, you could do:

            viewable_create(v1, [[A,B,C], [D,E,F]], 
                            array([fixed, fixed], any), 
                            [[\"foo\", \"bar\"], [\"tom\", \"dick\", \"harry\"]]).

            This would have two rows (named \"foo\" and \"bar\") each
	    with three columns (named \"tom\", \"dick\" and
	    \"harry\").

            To create a graph viewable where node names are attached
            to node labels and edge info fields are attached to edge
            lables (ie info(1,foo(A)), info(2,foo(B)) and
            info(3,foo(C)) as per viewable_create/3).

            lib(graph_algorithms),
            make_graph_symbolic([](n1,n2,n3),
                                [edge(n1,n2,info(1,foo(A)),
                                 edge(n2,n3,info(2,foo(B)),
                                 edge(n1,n3,info(3,foo(C))], Graph),
            viewable_create(v2, Graph, graph(fixed),
                            [node_property([0->[name('node names'), label]]),
                             edge_property([0->[name('edge info'), label]])
                            ]).
                            

            To create a graph where the edges are labelled with the
            second argument of the edge info structure. (ie with the
            structures foo(A), foo(B) and foo(C))

            lib(graph_algorithms),
            make_graph_symbolic([](n1,n2,n3),
                                [edge(n1,n2,info(1,foo(A)),
                                 edge(n2,n3,info(2,foo(B)),
                                 edge(n1,n3,info(3,foo(C))], Graph),
            viewable_create(v2, Graph, graph(fixed),
                            [node_property([0->[name('node names'), label]]),
                             edge_property([2->[name('edge info'), label]])
                            ]).

            
            To create a graph where the edges are labelled with the
            first argument of the nested structure at the second
            argument of the edge info structure. (ie with the
            variables A, B and C)

            lib(graph_algorithms),
            make_graph_symbolic([](n1,n2,n3),
                                [edge(n1,n2,info(1,foo(A)),
                                 edge(n2,n3,info(2,foo(B)),
                                 edge(n1,n3,info(3,foo(C))], Graph),
            viewable_create(v2, Graph, graph(fixed),
                            [node_property([0->[name('node names'), label]]),
                             edge_property([[2,1]->[name('edge info'), label]])
                            ]).
            ",

	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate succeeds with any arguments, and has no effect.</P>

        <p>This predicate works exactly the same as
        <code>viewable_create/3</code> except that you have the added
        ability to set location names (e.g. row/column names) for
        arrays and to indicate which elements of the edge info
        structure should be attached to edge labels for graphs.

        <p>For array type viewables the <em>LocNamesList</em> argument
        should be a list as long as the number of dimensions in the
        viewable. The ith element <em>LocNames</em> of
        <em>LocNamesList</em> should be a list whose length is equal
        to the size of the ith dimension of the new viewable. The jth
        element of <em>LocNames</em> should be a ground string and
        this will become the name of the jth location of the ith
        dimension.  </P>

        <p>For graph type viewables the <em>LocNamesList</em> argument
        should be a list of the following form.

<pre>
[
 node_property([0->[name(NodeInfoName), label]]),
 edge_property([Index->[name(EdgeInfoName), label]])
]
</pre>

        Where <code>NodeInfoName</code> is a meaningful name for the
        information to be displayed at the nodes in the graph.  eg. If
        the graph represented a computer network, the nodes may show
        individual machine names. In such a case, <code>\"machine
        name\"</code> would be a likely candidate for the NodeInfoName
        argument.  Similarly <code>EdgeInfoName</code> refers to the
        information associated with edge labels.

        <p>The <code>Index</code> argument specifies which part of the
        edge info field should be shown on edges. 0:The whole edge
        info structure.  N:The Nth argument of the edge info
        structure.  [...,I,J,K]:The Kth argument of the Jth argument
        of the Ith argument... of the edge info structure. See below
        for examples.

        <p>For a more detailed description of creating viewables, refer to the
        documentation for <code>viewable_create/3</code>.</P>


"), 
	    see_also : [viewable_create/2, viewable_create/3,
                        viewable_expand/4, graph_algorithms:make_graph/3],
            exceptions : [
	    4 : "ViewableName is not an atom, string or free variable.",
	    1 : "ViewableName is the name of an existent viewable.",
	    1 : "Elements is not a regular nested list or array with enough dimensions.", 
	    1 : "Type is not a ground valid type.", 
	    5 : "One of the viewable elements violates the element type.",
	    5 : "LocNamesList is not a correctly-sized list of correctly-sized lists of strings."],
            resat: no
]).




:- comment(viewable_create/2, 
	   [amode : viewable_create(?, +), 
	    args : ["ViewableName" : 
		 "A string or atom; the name of the new viewable.", 
		 "Elements" : 
		 "A possibly nested list or array (as created for example by dim/2)."],
	    summary : "Create a viewable.",
	    eg: "

[Assuming that at least one visualisation client is registered]

       Success:

       viewable_create(viewable1, [X, Y, Z]).

       viewable_create(\"viewable1\", [](X, Y, Z)).

       viewable_create(m, []).

       viewable_create(var23, [[],[],[]]).

       viewable_create(var315, []([]([](R, T, [E, X, Y]), 
                                     [](W, T, grok(D))), 
				  []([](A, B, C),
				     [](R, E, W)))).



       Exceptions raised:

       viewable_create(2, [x,y,z]).
       [gives error 4] 

       viewable_create(v1, [x,y,z]), 
       viewable_create(v1, [x,y,z]).
       [gives error 1] 

       viewable_create(v1, _).
       [gives error 1] 


				    ",

	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate succeeds with any arguments, and has no effect.</P>

	This predicate creates a new viewable by specifying its name and
	elements. The predicate will try to guess the number of dimensions
	for the new viewable from the level of nesting in
	<em>Elements</em>. All dimensions are set to <code>fixed</code> so
	they cannot be expanded later. The element type is set to
	<code>any</code>. For more control over the number and fixity of
	dimensions and the element type, use
	<code>viewable_create/3</code>.

        In other ways the predicate behaves just like
        <code>viewable_create/3</code>. See the documentation for that
        predicate for more details. </P>"), 
	    see_also : [viewable_create/3, viewable_expand/3],
            exceptions : [
	    4 : "ViewableName is not an atom, string or free variable.",
	    1 : "ViewableName is the name of an existent viewable.",
	    1 : "Elements is not a nested list or array."], 
            resat: no]).



:- comment(viewable/1, 
	   [amode : viewable(?),
	    args : ["ViewableName":"Atom or string: name of a viewable."],
	    summary : "Test/enumerate names of all existent viewables.",
	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate fails with all arguments, and has no effect.</P>


       If <em>ViewableName</em> is instantiated, tests whether
       <em>ViewableName</em> is the name of an existent viewable. If not,
       <em>ViewableName</em> is successively instantiated to the names of all
       existent viewables."),
	    fail_if : "Fails if ViewableName is not / cannot be instantiated to the name of an existent viewable.",
            exceptions : [4:"ViewableName is not an atom, string or free variable"],  
	    resat : yes, 
	    eg: "

[Assuming that at least one visualisation client is registered]

     Success:

     viewable_create(my_viewable, [X,Y,Z], array([fixed], any)), 
       viewable(my_viewable).


     Failure:

     viewable(non_viewable)


     Exception raised:

     viewable(123).
     [raises error 4].
", 
	    see_also : [viewable_create/2, viewable_create/3]]).
	    



:- comment(viewable_size/2, 
	   [amode : viewable_size(++,?),
	    args : ["ViewableName" : 
		 "A string or atom: the name of an existent viewable", 
		 "SizeList" : 
		   "A list of integers: each is the current size of a dimension"],
	    summary : "Query the sizes of the dimensions of an existent viewable.",
	    desc : html(

" <p><B>NOTE:</B>When there are no registered visualisation clients, this
        predicate fails with all arguments, and has no effect.</P>

            <p>If <em>ViewableName</em> is the name of an existent viewable,
	    <em>SizeList</em> is unified with a list of integers, each
	    integer being the number of elements in one of the viewable's
	    dimensions."), fail_if : "Fails if ViewableName is not
	    instantiated to the name of an existent viewable, or if
	    SizeList fails to unify with the list of dimension sizes.

",
	    exceptions : [4:"ViewableName is not a string or an atom", 
			  5:"SizeList is not a free variable or list or contains one or more elements which is not a free variable or an integer"],
	    resat : no, 
	    eg: "

[Assuming that at least one visualisation client is registered]

	Success:

	viewable_create(v1, [], 
			array([flexible, flexible, flexible], any)), 
	viewable_size(v1, X).
        [gives X = [0,0,0]]

	viewable_create(v1, []([](R,S), [](Q,W)), 
			array([fixed, fixed], any)), 
	viewable_size(v1, X).
        [gives X = [2,2]]


	Failure:

	viewable_size(no, X).

	viewable_create(v1, []([](R,S), [](Q,W)), 
			array([fixed, fixed], any)), 
	viewable_size(v1, [1,2]).



        Exceptions raised:

	viewable_size(_, X).
        [gives error 4]

	viewable_size(123, X).
        [gives error 4]

	viewable_create(v1, [X, Y, Z]), 
	viewable_size(v1, q).
        [gives error 5]

	viewable_create(v1, [X, Y, Z]), 
	viewable_size(v1, [a]).
        [gives error 5]

", 
	    see_also : [viewable_create/3, viewable_expand/3, viewable_type/2]]).


:- comment(viewable_type/2, 
	   [amode : viewable_type(++,?),
	    args : ["ViewableName" : 
		 "A string or atom: the name of an existent viewable", 
		 "Type" : 
		   "The type of the viewable. "],
	    summary : "Query the type of an existent viewable.",
	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate fails with all arguments, and has no effect.</P>

        <p>If <em>ViewableName</em> is the name of an existent viewable,
        <em>Type</em> is unified with its type. See
        <code>viewable_create/3</code> for a discussion of valid viewable
        types.</p>


"),
	    fail_if : "Fails if ViewableName is not instantiated to the name of an existent viewable, or if Type fails to unify with the viewable's type.",
	    exceptions : [4:"ViewableName is not a string or an atom"],
	    resat : no, 
	    eg: "

[Assuming that at least one visualisation client is registered]

	Success:

	viewable_create(v1, []([](R,S), [](Q,W)), 
			array([fixed, fixed], any)), 
	viewable_type(v1, X).

        [gives X = array([fixed, fixed], any)]


	Failure:

	viewable_create(v1, []([](R,S), [](Q,W)), 
		array([fixed, fixed], any)), 
	viewable_type(v1, q).


	Exceptions raised:

	viewable_create(v1, []([](R,S), [](Q,W)), 
		array([fixed, fixed], any)), 
	viewable_type(123, _).
        [gives error 4]

", 
	    see_also : [viewable_create/3, viewable_size/2, viewable_expand/3]]).


:- comment(viewable_expand/3, 
	   [amode : viewable_expand(++,++,+),
	    args : ["ViewableName" : 
		 "A string or atom; the name of an existent viewable", 
		 "DimensionNumber" : "An integer: the number of the dimension to be enlarged.", 
		 "ExtraElements" : "A nested list or an array of the right size/dimensions, containing the new viewable elements."],
	    summary : "Expand a dimension of a viewable by adding new elements.",
	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate succeeds with any arguments, and has no effect.</P>



      <p>A viewable which has a flexible dimension (see
      <code>viewable_create/3</code>) may be expanded along that dimension
      by adding new elements. The dimension must have been declared
      flexible when the viewable was first created. The
      <em>ExtraElements</em> argument is of the same format as the
      <em>Elements</em> argument of <code>viewable_create/3</code>.</p>

      <p>The number and size of the dimensions of <em>ExtraElements</em>
      depends on the number and size of dimensions of the viewable being
      expanded. <em>ExtraElements</em> should have one less dimension than
      the viewable -- dimension <em>DimensionNumber</em> should be
      missing. The remaining dimensions should have the same sizes as the
      corresponding dimensions in the viewable.</p>

      <p>For example, a viewable of size 3 x 0 x 0 with all dimensions
      flexible, can be expanded along dimenson 3 using an
      <em>ExtraElements</em> argument of []([], [], []) which has size 3 x
      0. The viewable would then have size 3 x 0 x 1. It could then be
      expanded along dimension 1 using <code>[]</code> as
      <em>ExtraElements</em> since <code>[]</code> can implicitly have the
      size 0 x 1. The viewable would then have the size 4 x 0 x 1. Then
      dimension 2 could be expanded using <code>[[W],[X],[Y],[Z]]</code>
      for <em>ExtraElements</em> as this has size 4 x 1. The final size of
      the viewable would then be 4 x 1 x 1.</p>

      <p>The extra viewable elements must each also conform to the element
      type specified when the viewable was created.</p>

      <p> Note that when the viewable has one dimension, the
      <em>ExtraElements</em> argument will simply become the new element
      itself.</p>

      <p><code>viewable_expand/3</code> sets the name of the new location 
      (e.g. the column or row name) to its number as a string. For example, 
      adding a third row using <code>viewable_expand/3</code> will result
      in the row being named \"3\". For more control over the name of the new
      location use <code>viewable_expand/4</code>.<p>

"),
	    fail_if : "Fails if ViewableName is not the name of an existent viewable, or if the requested dimension of the viewable is not flexible.", 
	    exceptions : [4:"ViewableName is not a string or atom", 
			  5:"DimensionNumber is not a ground integer",
			  6:"DimensionNumber is not positive, or exceeds the viewable's number of dimensions", 
			  1:"ExtraElements is not a regular nested list or array of the correct size/dimensions", 
			  5:"ExtraElements contains elements which do not conform to the element type of the viewable"],
	    resat : no, 
	    eg: "

[Assuming that at least one visualisation client is registered]

       Success:

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
       	               array([flexible, fixed], any)), 
       viewable_expand(v1, 1, [R, S, T]).

       lib(fd), fd:([X, Y, Z, R] :: 1..10), 
       viewable_create(v1, [X, Y, Z], array([flexible], numeric_bounds)),
       viewable_expand(v1, 1, R).
       
       viewable_create(v1, []([](X, Y, Z), [](A, B, C)), 
       	               array([fixed, flexible], any)), 
       viewable_expand(v1, 2, [R, S]).


       viewable_create(v1, [[], [], []], 
		       array([flexible, flexible, flexible], any)),
       viewable_expand(v1, 3, []([], [], [])), 
       viewable_expand(v1, 1, []), 
       viewable_expand(v1, 2, [[W], [X], [Y], [Z]]),  
       viewable_size(v1, Size).
       [gives Size = [4,1,1]]

       viewable_create(v1, [], array([flexible], any)), 
       viewable_expand(v1, 1, F), 
       viewable_expand(v1, 1, G),
       viewable_size(v1, Size).
       [gives Size = [2]]
       
       


       Failure:

       viewable_expand(no, 2, [R, S]).


       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
	               array([fixed, flexible], any)), 
       viewable_expand(v1, 1, [R, S, T]).


       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
	               array([flexible, fixed], any)), 
       viewable_expand(v1, 2, [R, S]).



       Exceptions raised:

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
	               array([flexible, fixed], any)), 
       viewable_expand(_, 1, [R, S, T]).
       [gives error 4]

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
	               array([flexible, fixed], any)), 
       viewable_expand(v1, _, [R, S, T]).
       [gives error 5]

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
                       array([flexible, fixed], any)), 
       viewable_expand(v1, 3, [R, S, T]).
       [gives error 6]

       viewable_create(v1, []([](X, Y, Z), [](A, B, C)), 
	               array([fixed, flexible], any)), 
       viewable_expand(v1, 2, _).
       [gives error 1]

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
	               array([flexible, fixed], any)), 
       viewable_expand(v1, 1, [R, S, T, Q]).
       [gives error 1]

       lib(fd), fd:([X, Y, Z] :: 1..10), 
       viewable_create(v1, [X, Y, Z], array([flexible], numeric_bounds)),
       viewable_expand(v1, 1, an_atom)
       [gives error 5]



", 
	    see_also : [viewable_expand/4, viewable_create/3, viewable_size/2, viewable_type/2]]).


:- comment(viewable_expand/4, 
	   [amode : viewable_expand(++,++,+,++),
	    args : ["ViewableName" : 
		 "A string or atom; the name of an existent viewable", 
		 "DimensionNumber" : "An integer: the number of the dimension to be enlarged.", 
		 "ExtraElements" : "A nested list or an array of the right size/dimensions, containing the new viewable elements.",
		 "LocName" : "A string: the name of the new location."],
	    summary : "Expand a dimension of a viewable by adding new elements, specifying the name of the new location.",
	    desc : html("

        <p><B>NOTE:</B>When there are no registered visualisation clients,
        this predicate succeeds with any arguments, and has no effect.</P>

        <p>This predicate behaves exactly the same as <code>viewable_expand/3</code> except that you have the added ability to name the new location of the expanded dimension. <em>LocName</em> is a string which becomes the name for the new location. </p>

        For more details on expanding viewables, see the documentation for <code>viewable_expand/3</code>. 

"),
	    fail_if : "Fails if ViewableName is not the name of an existent viewable, or if the requested dimension of the viewable is not flexible.", 
	    exceptions : [4:"ViewableName is not a string or atom", 
			  5:"DimensionNumber is not a ground integer",
			  6:"DimensionNumber is not positive, or exceeds the viewable's number of dimensions", 
			  1:"ExtraElements is not a regular nested list or array of the correct size/dimensions", 
			  5:"ExtraElements contains elements which do not conform to the element type of the viewable",
			  5:"LocName is not a string"],
	    resat : no, 
	    eg: "

[Assuming that at least one visualisation client is registered]

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
       	               array([flexible, fixed], any)), 
       viewable_expand(v1, 1, [R, S, T], \"barg\").

       This will add to the 2 x 3 viewable a third row, named \"barg\".

       viewable_create(v1, [[X, Y, Z], [A, B, C]], 
       	               array([fixed, flexible], any)), 
       viewable_expand(v1, 2, [Q, P], \"zatch\").

       This will add to the 2 x 3 viewable a fourth column, named \"zatch\".


", 
	    see_also : [viewable_expand/3, viewable_create/4, viewable_size/2, viewable_type/2]]).



