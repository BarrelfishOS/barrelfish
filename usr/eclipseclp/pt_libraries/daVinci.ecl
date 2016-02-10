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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, Parc Technologies
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Simple interface to daVinci graph drawing tool
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Helmut Simonis, Parc Technologies Ltd
% Version:	$Id: daVinci.ecl,v 1.3 2009/12/22 02:56:23 jschimpf Exp $
%
% ----------------------------------------------------------------------

:-module(daVinci).

:-export(daVinci_begin/0).
:-export(daVinci_end/0).
:-export(daVinci_node/1).
:-export(daVinci_node/2).
:-export(daVinci_edge/3).
:-export(daVinci_node_attribute/3).
:-export(daVinci_edge_attribute/3).
:-export(daVinci_exit/0).
:-export(daVinci_draw_graph/1).

% only for testing
%:-export(da/2).

:-local variable(daVinciOut).
:-local variable(daVinciEdge).
:-local variable(daVinciAttribute).

:- comment(categories, ["Visualisation","Interfacing"]).
:-comment(summary,"This library provides a simple interface to the daVinci graph drawing tool").

:-comment(desc,html("
This library provides a simple interface to the daVinci graph drawing
tool.  It allows to specify a graph with nodes, edges and their
attributes, and draws the graph inside the daVinci tool.  The
interface does not allow callbacks back from the daVinci system, nor
does it handle the customisation of the daVinci interface that is
possible.<p> The daVinci system is a graph drawing tool developed at
the University of Bremen in Germany and is freely available for
non-profit and internal use.  Please download the software from their
WEB site <a
href=http://www.informatik.uni-bremen.de/agbkb/forschung/daVinci/daVinci.html>daVinci
Home page</a> and register with the developers.  We do not provide the
executable as part of this library.<p> The communication between
Eclipse and daVinci is handled by a socket interface, where we
exchange text messages terminated by a line-feed.  In order to gain a
bit of flexibility in the interface, we use two string streams to
buffer information about edges and attributes.  The three streams used
for communication are called daVinciOut, daVinciEdge and
daVinciAttribute.  If the stream daVinciOut exists, we assume that the
daVinci server is running and that we can send information to it.  If
the stream does not exist, then we open the daVinci program in server
mode and connect the stream to the socket connection on the port
2542.<p> ")).

:-comment(author,"H. Simonis").
:-comment(date,"$Date: 2009/12/22 02:56:23 $").

/************************************************************************

 daVinci specific code

************************************************************************/

:-comment(daVinci_begin/0,[
summary:"Start the connection to daVinci and begin the transfer of a new graph",


args:[],
desc:html("
This commands opens the connection to the daVinci system if not
already open and starts the information about a new graph.  This
predicate should be called once for each graph to be transmitted.  <p>
"),
fail_if:"no",
resat:no,
eg:"
:-use_module(daVinci).

top:-
	daVinci_begin,
	daVinci_node(1),
	daVinci_node(2,'a bigger label'),
	daVinci_node(aa),

	daVinci_edge(1,1,2),
	daVinci_edge(2,2,2),
	daVinci_edge(3,2,aa),

	daVinci_node_attribute(1,'COLOR','red'),
	daVinci_node_attribute(2,'COLOR','dodgerblue'),

	daVinci_node_attribute(2,'FONTSTYLE','italic'),
	daVinci_edge_attribute(3,'EDGECOLOR','blue'),
	daVinci_edge_attribute(3,'EDGEPATTERN','dashed'),
	daVinci_node(4),
	daVinci_edge(4,2,4),
	daVinci_edge(5,4,2),
	daVinci_end.
",

see_also:[daVinci_end/0]

]).

daVinci_begin:-
	( da_still_open ->
	    true
	;
	    da
	),
	setval(daVinciOut,0),
	setval(daVinciEdge,0),
	setval(daVinciAttribute,0),
	open(string(""),write,daVinciEdge),
	open(string(""),write,daVinciAttribute),
	da('menu(file(new))'),
	printf(daVinciOut,'graph(update([',[]),
        (
	    true
        ;
	    daVinci_end,
	    fail
	).

:-comment(daVinci_end/0,[
summary:"This commands ends the information about a graph and calls daVinci to draw it.",

args:[],
desc:html("
This commands is called once when all information about a graph have
been transmitted.  It sends the graph description to the daVinci tool
and causes it to bbe drawn.  After this command, the next daVinci
command should be daVinci_begin/0 to begin the information about the
next graph.  <p> "),
fail_if:"fails if the stream daVinciEdge does not exist",
resat:no,
eg:"
see daVinci_begin/0
",

see_also:[daVinci_begin/0]

]).

daVinci_end:-
	current_stream(daVinciEdge),
	get_stream_info(daVinciEdge,name,Edge),
	close(daVinciEdge),
	get_stream_info(daVinciAttribute,name,Attribute),
	close(daVinciAttribute),
	da_still_open,
%	da('],[]))',[]), % use this to see only nodes, comment out next lines
	da('],[%s]))',[Edge]),
        % comment next line out to check without attributes
	da('graph(change_attr([%s]))',[Attribute]), 
        true.


:-comment(daVinci_exit/0,[
summary:"This commands closes the connection and exits daVinci.",
args:[],
see_also:[daVinci_begin/0]
]).

daVinci_exit :-
	( da_still_open ->
	    da('menu(file(exit))'),
	    close(daVinciOut),
	    ( current_stream(daVinciIn) ->
		da_ack	% pipe case
	    ;
		true	% socket case
	    )
	;
	    true
	).


:-comment(daVinci_node/1,[
summary:"Define one node for the graph. The label of the node is equal to its name.",
amode:daVinci_node(?),

args:[
      "Node" : " a Prolog term, normally a number or an atom"
],
desc:html("
This command defines one node for the graph.  The label of the node is
equal to the name of the node.  Often, the name is a number or an
atom, but it can be any Prolog structure.  The node name must be
unique, and should not contain backslash, tilde or double quote
characters.<p> "),
fail_if:"no",
resat:no,
eg:"
see daVinci_begin/0
",

see_also:[daVinci_node/2,daVinci_node_attribute/3]

]).

daVinci_node(Node):-
	daVinci_node(Node,Node).


:-comment(daVinci_node/2,[
summary:"Defines a node in the graph. The label of the node can be different from the name.",
amode:daVinci_node(?,?),
args:[
      "Node" : " a Prolog term, often a number or an atom",
      "Info" : " a Prolog term"
],
desc:html("
This command defines one node for the graph.  The label of the node is
given as the second argument, and can be different from the name. 
Both arguments can be arbitrary Prolog terms, but the name often is a
number of an atom.  The name of each node must be unique, and should
not contain backslash, tilde or double quote characters.  <p> "),
fail_if:"no",
resat:no,
eg:"
see daVinci_begin/0
",

see_also:[daVinci_node/1,daVinci_node_attribute/3]

]).

daVinci_node(Node,Info):-
	da_comma(daVinciOut),
	printf(daVinciOut,'new_node("%w","",[a("OBJECT","%w")])',[Node,Info]).


:-comment(daVinci_edge/3,[
summary:"This command adds an edge between two nodes in the graph.",
amode:daVinci_edge(?,?,?),
args:[
      "Name" : "a Prolog term ",
      "Node1" : "a Prolog term ",
      "Node2" : "a Prolog term "
],
desc:html("
This command adds an edge between two nodes in the graph.  The edge is
from Node1 to Node2.  Both nodes must exist in the final graph, but do
not need to be defined before the edge is entered.  Each edge must
have a unique name, typically a number or an atom.  Names for nodes
and edges do not have to be disjoint.  <p> "),
fail_if:"no",
resat:no, eg:" see daVinci_begin/0 ",
see_also:[daVinci_edge_attribute/3]
]).

daVinci_edge(Name,Node1,Node2):-
	da_comma(daVinciEdge),
	printf(daVinciEdge,'new_edge("%w","",[],"%w","%w")',[Name,Node1,Node2]).


:-comment(daVinci_node_attribute/3,[
summary:"Set an attribute for a node in the graph",
amode:daVinci_node_attribute(?,++,++),
args:[
      "Name" : " a Prolog Term",
      "Type" : " an atom",
      "Value" : " an atom "
],
desc:html("
This predicate is used to set an attribute of a node in the graph. 
For a list of all possible attributes and their values, please refer
to the daVinci documentation <a
href=http://www.tzi.de/daVinci/docs/referenceF.html>Reference
Manual</a>.  Typical attributes are 'COLOR','FONTSYTLE','BORDER' to
set the appearance of the node, and 'OBJECT' to change the label
displayed in the node.  <p> "),
fail_if:"no",
resat:no,
eg:"
see daVinci_begin/0
",
see_also:[daVinci_node/1,daVinci_node/2]
]).

daVinci_node_attribute(Node,Type,Value):-
	da_comma(daVinciAttribute),
	printf(daVinciAttribute,'node("%w",[a("%w","%w")])',[Node,Type,Value]).


:-comment(daVinci_edge_attribute/3,[
summary:"Set an attribute for an edge in the graph",
amode:daVinci_edge_attribute(?,++,++),
args:[
      "Name" : " a Prolog term",
      "Type" : " an atom",
      "Value" : " an atom"
],
desc:html("
This predicate is used to set some attribute of an edge in the graph. 
For a list of all possible attributes and their values, please refer
to the daVinci documentation
<a href=http://www.tzi.de/daVinci/docs/referenceF.html>Reference
Manual</a>.  Typical attributes are 'EDGECOLOR' or 'EDGEPATTERN' to
set the appearance of the edge.  Note that it is currently not
possible in daVinci to specify labels for the edges.  <p> "),
fail_if:"", resat:yes, eg:" see daVinci_begin/0 ",
see_also:[daVinci_edge/3]
]).

daVinci_edge_attribute(Node,Type,Value):-
	da_comma(daVinciAttribute),
	printf(daVinciAttribute,'edge("%w",[a("%w","%w")])',[Node,Type,Value]).


/****************************************************************************

draw a graph in lib(graph_algorithms) format

****************************************************************************/

:-comment(daVinci_draw_graph/1,[
summary:"Draw a graph in lib(graph_algorithms) format",
amode:daVinci_draw_graph(+),
args:["Graph" : "a graph description"],
eg:"
:- lib(graph_algorithms).
:- lib(daVinci).

top :-
	make_random_graph(10,20,true,true,true,Graph),
	daVinci_draw_graph(Graph).
",
desc:html("
This predicate takes a graph in the format defined by the
library lib(graph_algorithms), and draws it.
")]).

daVinci_draw_graph(Graph) :-
	graph_algorithms:graph_get_maxnode(Graph, NNodes),
	graph_algorithms:graph_get_all_edges(Graph, EdgeList),

	daVinci_begin,
	( for(N,1,NNodes), param(Graph) do
	    ( graph_algorithms:node_to_nodename(Graph, N, NodeName) ->
		daVinci_node(N, NodeName)
	    ;
		daVinci_node(N)
	    )
	),
	( foreach(e(S,T,_D),EdgeList), count(E,1,_) do
	    daVinci_edge(E, S, T)
	),
	daVinci_end.


/****************************************************************************

low level interface for DaVinci

****************************************************************************/

da_comma(Stream):-
	getval(Stream,0),
	!,
	setval(Stream,1).
da_comma(Stream):-
	printf(Stream,',',[]).


da:-
	get_flag(hostarch,Arch),
	Arch = "i386_nt",
	!,
	da_executable(Executable),
	exec([Executable,'-server'],[],_P),
	socket(internet,stream,daVinciOut),
	get_flag(hostname, HostString),
	atom_string(Host, HostString),
	once (
	    % we might have to wait for daVinci to get ready to accept
	    between(1, 10, 1, _),	% try max 10 times
	    sleep(0.5),
	    block(connect(daVinciOut,Host/2542), _Tag,
	    	(writeln(error, "retrying ..."), fail))
	),
	set_stream(daVinciIn,daVinciOut),
	da_ack.
da:-
	da_executable(Executable),
	exec([Executable,'-pipe'],[daVinciOut,daVinciIn],_P),
	da_ack.

da_executable(Executable) :-
	getenv("DAVINCIHOME", DAVINCIHOME_OS),
	os_file_name(DAVINCIHOME, DAVINCIHOME_OS),
	(
	    concat_string([DAVINCIHOME,"/bin/daVinci"], Executable)
	;
	    concat_string([DAVINCIHOME,"/daVinci"], Executable)
	),
	existing_file(Executable, ["",".exe"], [executable], _),
	!.
da_executable("daVinci").	% hope it's in the PATH ...


da(X):-
	printf(daVinciOut,X,[]),
	put(daVinciOut,10),
	flush(daVinciOut),
	da_ack.

da(X,Y):-
	printf(daVinciOut,X,Y),
	put(daVinciOut,10),
	flush(daVinciOut),
	da_ack.

da_ack:-
	( read_string(daVinciIn, end_of_line, _, Reply) ->
	    ( Reply = "quit" ->
	    	close(daVinciIn)
	    ; substring(Reply, "communication_error", 1) ->
		printf(warning_output, "daVinci: %w%n", [Reply])
	    ;
		true
	    )
	;
	    close(daVinciIn)	% end of file
	).

% check whether daVinci is still there (or was exited by the user)
da_still_open :-
	current_stream(daVinciIn),
	( stream_select([daVinciIn], 0, [_]) ->
	    ( read_string(daVinciIn, end_of_line, _, "quit") ->
	    	close(daVinciIn),
	    	( current_stream(daVinciOut) -> close(daVinciOut) ; true ),
		fail
	    ;
	    	da_still_open	% read some junk: ignore and keep trying
	    )
	;
	    true		% no quit message found, succeed
	).

