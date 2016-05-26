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
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Josh Singer, Parc Technologies
% 
% END LICENSE BLOCK

:- module(tty_vc).
:- use_module(vc_support).
:- lib(hash).

:- export start_tty_vc/1.
:- export start_tty_vc/2.
:- export start_tty_vc/3.
:- export stop_tty_vc/0.


:- comment(categories, ["Interfacing","Visualisation"]).
:- comment(summary, 
	   "Simple TTY visualisation client").

:- comment(author, "Josh Singer").


:- comment(desc, html("<P>This module implements a simple TTY visualisation client. A visualisation client (VC) is an entity which monitors <em>viewables</em>. See <code>lib(viewable)</code> for more information on viewables. This module is mainly designed to help evaluate the visualisation architecture.</p>

<p>The three predicates <code>start_tty_vc/1</code>, <code>start_tty_vc/2</code> and <code>start_tty_vc/3</code>, can be used to start the TTY VC with various options. The predicate <code>stop_tty_vc/1</code> is used to stop the TTY VC. </p> 

<p>While it is running, when new 2-d viewables are created, the TTY VC starts monitoring them. The TTY VC does not monitor viewables whose number of dimensions is not 2. It reacts to events relating to monitored viewables by outputting messages to <code>stdout</code>, including pretty-printing the viewable elements when updates occur. A forward (normally constraining) update to an element is signified by \"++\", and backtracking over this is signified by \"--\".</p>   
")).




:- local variable(tty_vc_state).
:- setval(tty_vc_state, none).
:- local struct(state(name, viewables_stream, updates_stream, interest_stream, 
		      viewables, initial_view_granularity, change_condition)).


supported_vis_protocol_version(1).

:- comment(start_tty_vc/1, 
	   [amode : start_tty_vc(++), 
	    args : ["VCName" : 
		 "An atom; the name of the TTY VC."],
	    summary : "Start a TTY visualisation client.",
	    desc : html("

	    <p> This predicate starts a TTY visualisation client (VC). The change condition is <code>constrained of suspend</code>. The view granularity is <code>fine</code>. For more control over these options, use <code>start_tty_vc/2</code> or <code>start_tty_vc/3</code>. </p> The TTY VC is a non-logical entity in the sense that backtracking over the <code>start_tty_vc</code> goal does not affect the existence of the TTY VC. "),
	    fail_if : "Fails when any other TTY VC exists. Only one TTY VC can exist at a time, although in general there can be multiple VCs.",
	    see_also : [stop_tty_vc/0, start_tty_vc/2, start_tty_vc/3],
            resat: no]).


start_tty_vc(VCName):-
	start_tty_vc(VCName, fine).



:- comment(start_tty_vc/2, 
	   [amode : start_tty_vc(++, ++), 
	    args : ["VCName" : 
		 "An atom; the name of the TTY VC.", 
		 "InitialViewGranularity": "An atom: 'coarse' or 'fine'"],
	    summary : "Start a TTY visualisation client, specifying the view granularity.",
	    desc : html("

	    <p> This predicate starts a TTY visualisation client (VC), but allows the user to specify the initial view granularity. With 'fine', each propagation step is seen individually. With 'coarse' successful sequences of propagation are seen together in chunks and unsuccessful sequences are skipped. The view granularity for a particular viewable can be changed during search. Otherwise the predicate behaves exactly like <code>start_tty_vc/1</code>."),
	    see_also : [stop_tty_vc/0, start_tty_vc/1, start_tty_vc/3],
            resat: no]).


start_tty_vc(VCName, ViewGranularity):-
	start_tty_vc(VCName, ViewGranularity, 
		     change_condition(suspend, suspend, constrained)).


:- comment(start_tty_vc/3, 
	   [amode : start_tty_vc(++, ++, ++), 
	    args : ["VCName" : 
		 "An atom; the name of the TTY VC.", 
		 "InitialViewGranularity": "An atom: 'coarse' or 'fine'", 
		 "ChangeCondition": "A term of the form change_condition(<Module>, <AttributeStructName>, <SuspListName>)"],
	    summary : "Start a TTY visualisation client, specifying both the view granularity and the change condition.",
	    desc : html("

	    <p> This predicate starts a TTY visualisation client (VC), but allows the user to specify both the view granularity, as in <code>start_tty_vc/2</code> and the change condition. The change condition specifies the suspension list to use for updates. The most obvious choice is the default in the other two predicates, <code>constrained of suspend</code> in the <code>suspend</code> module. If other suspension lists are used, the value of the element displayed in the output of the TTY VC may sometimes be out of date. Otherwise the predicate behaves exactly like <code>start_tty_vc/2</code>."),
	    see_also : [stop_tty_vc/0, start_tty_vc/1, start_tty_vc/2],
            resat: no]).


start_tty_vc(VCName, ViewGranularity, ChangeCondition):-
	getval(tty_vc_state, none), % only one tty vc at a time
	initialise_vc_streams(VCName, 
			      ViewablesStream, UpdatesStream, InterestStream),
	setval(tty_vc_state, state with [name:VCName, 
					 viewables_stream:ViewablesStream, 
					 updates_stream:UpdatesStream, 
					 interest_stream:InterestStream, 
					 viewables:[], 
					 initial_view_granularity:
					     ViewGranularity, 
					 change_condition:ChangeCondition]),
	supported_vis_protocol_version(VisProtocolVersion),
	write_exdr(InterestStream, 
		   vis_protocol_version(VisProtocolVersion)),
	flush(InterestStream),
	(vis_client_register(VCName, ViewablesStream, 
			     UpdatesStream, InterestStream)
	->
	    true
	;
	    writeln(error, 
              "Visualisation module does not support protocol version"),
	    setval(tty_vc_state, none),
	    fail
	).


:- comment(stop_tty_vc/0, 
	   [amode : stop_tty_vc, 
	    summary : "Stop the TTY visualisation client.",
	    desc : html("
	    <p> This predicate stops the TTY visualisation client (VC). It will no longer react to viewables."),
	fail_if : "Fails if there is no TTY VC.", 
	see_also : [start_tty_vc/1, start_tty_vc/2, start_tty_vc/3],
            resat: no]).



stop_tty_vc:-
	getval(tty_vc_state, state with [name:VCName,
					 viewables_stream:ViewablesStream, 
					 updates_stream:UpdatesStream, 
					 interest_stream:InterestStream]), 
	vis_client_unregister(VCName), 
	close(ViewablesStream), 
	close(InterestStream), 
	close(UpdatesStream), 
	setval(tty_vc_state, none).


initialise_vc_streams(VCName, ViewablesStream, 
		      UpdatesStream, InterestStream):-
	initialise_viewables_stream(VCName, ViewablesStream), 
	initialise_updates_stream(VCName, UpdatesStream), 
	initialise_interest_stream(VCName, InterestStream).

initialise_viewables_stream(VCName, ViewablesStream):-
	concat_atoms(VCName, '_viewables', ViewablesStream),
	set_event_handler(ViewablesStream, viewables_event_handler/1), 
	open(queue(""), update, ViewablesStream, [event(ViewablesStream)]).

initialise_updates_stream(VCName, UpdatesStream):-
	concat_atoms(VCName, '_updates', UpdatesStream),
	set_event_handler(UpdatesStream, updates_event_handler/1), 
	open(queue(""), update, UpdatesStream, [event(UpdatesStream)]).

initialise_interest_stream(VCName, InterestStream):-
	concat_atoms(VCName, '_interest', InterestStream),
	open(queue(""), update, InterestStream, []).

viewables_event_handler(ViewablesStream):-
	read_exdr(ViewablesStream, Notification), 
	handle_viewables_notification(Notification).

handle_viewables_notification(viewable_create(ViewableName, 
					      Type)):-
	!, 
	getval(tty_vc_state, State),
	arg(interest_stream of state, State, InterestStream), 
	(
	    (Type = array(FixityList, _ElementType), 
	     length(FixityList, 2)) ->
		(
		    arg(name of state, State, VCName), 
		    concat_string(["TTY VC ", VCName, 
				   " now monitoring 2-d viewable ",
				   ViewableName], String), 
		    writeln(String), 
		    arg(viewables of state, State, Viewables), 
		    arg(initial_view_granularity of state, State, 
			InitialViewGranularity), 
		    (member(v(ViewableName, _), Viewables) -> true ; 
		     (setarg(viewables of state, State, 
			     [v(ViewableName, InitialViewGranularity)|
			      Viewables]),
		      setval(tty_vc_state, State))
		    ), 
		    arg(change_condition of state, State, ChangeCondition), 
		    arg(initial_view_granularity of state, 
			State, InitViewGranularity), 
		    output_viewable(ViewableName, [], interest, 
				    NewViewGranularity),
		    (var(NewViewGranularity) -> 
			 NewViewGranularity = InitViewGranularity;
			 true),
		    write_exdr(InterestStream, 
			       interest(viewable_create(ViewableName),
					yes, 
					[interest_spec(tty_vc_spec, 
						       ChangeCondition, 
						       NewViewGranularity)])),
		    flush(InterestStream)
		)
		;
		write_exdr(InterestStream, 
			   interest(viewable_create(ViewableName),
				    no, [])) 
	),
	flush(InterestStream).

handle_viewables_notification(viewable_expand(ViewableName, Dimension)):-
	!, 
	concat_string(["Viewable ", ViewableName, 
		       " expanded dimension ", Dimension], String), 
	writeln(String), 
	output_viewable(ViewableName, [], update, _).

handle_viewables_notification(viewable_contract(ViewableName)):-
	!, 
	concat_string(["Viewable ", ViewableName, 
		       " contracted."], String), 
	writeln(String), 
	(viewable(ViewableName) -> 
	     output_viewable(ViewableName, [], update, _)
	;
	     true).

handle_viewables_notification(viewable_destroy(ViewableName)):-
	!, 
	concat_string(["Viewable ", ViewableName, 
		       " destroyed."], String), 
	writeln(String).


handle_viewables_notification(_).

updates_event_handler(UpdatesStream):-
	read_exdr(UpdatesStream, Update), 
	% writeln(Update),
	handle_update(Update).

handle_update(update(ViewableName, _InterestSpecName, Direction, UpdateList)):-
	!,
	(Direction == forward ->
	     MarkerString = "++"; 	    
	     MarkerString = "--"), 
	(foreach(element([RowIndex, ColumnIndex]), 
		 UpdateList),
	 foreach(marker(MarkerString, RowIndex, ColumnIndex), MarkerList), 
	 param(MarkerString)
	do 
	    true), 
	output_viewable(ViewableName, MarkerList, update, _).
	 

handle_update(_).

output_viewable(ViewableName, MarkerList, UpdateOrInterest, 
		NewVG):-
	viewable_to_string(ViewableName, MarkerList, String), 
	writeln(String), nl,
	writeln("Type vg <Return> to change view granularity, or press <Return> to continue..."), 
	read_string("\n", _, S), 
	(S == "vg"
	->
	    (
		UpdateOrInterest == update
	    ->
		change_view_granularity(ViewableName)
	    ;
		getval(tty_vc_state, State),
		arg(viewables of state, State, Viewables), 
		eclipse_language:delete(v(ViewableName, OldVG), Viewables, 
					RestViewables), 
		swap_vg(OldVG, NewVG), 
		setarg(viewables of state, State,
		       [v(ViewableName, NewVG)|RestViewables]),
		setval(tty_vc_state, State),
		vg_change_message(NewVG, ViewableName)
	    )
	;
	    true).

change_view_granularity(ViewableName):-
	getval(tty_vc_state, State),
	arg(viewables of state, State, Viewables), 
	arg(name of state, State, Name), 
	eclipse_language:delete(v(ViewableName, OldVG), Viewables, 
				RestViewables), 
	swap_vg(OldVG, NewVG), 
	setarg(viewables of state, State,
	       [v(ViewableName, NewVG)|RestViewables]),
	setval(tty_vc_state, State),
	vis_client_interest_modify(ViewableName, Name, tty_vc_spec, 
				   view_granularity, NewVG), 
	vg_change_message(NewVG, ViewableName).

vg_change_message(NewVG, ViewableName):-
	write("View granularity on viewable "), 
	write(ViewableName),
	write(" is now "),
	writeln(NewVG),
	writeln("Press <Return> to continue..."), 
	read_string("\n", _, _).

swap_vg(fine, coarse).
swap_vg(coarse, fine).

viewable_to_string(ViewableName, MarkerList, String):-
	generate_all_strings(ViewableName, AllStrings, MarkerList), 
	find_column_widths(AllStrings, ColumnWidths), 
	write_formatted_matrix(AllStrings, ColumnWidths, String).

generate_all_strings(ViewableName, AllStrings, MarkerList):-
	viewable_size(ViewableName, [Rows, Columns]),
	R1 is Rows + 1, 
	C1 is Columns + 1, 
	dim(AllStrings, [R1, C1]),
	% fill in viewable name: (make a string if not)
	ensure_string(ViewableName, ViewableNameString), 
	arg([1,1], AllStrings, ViewableNameString), 
	(count(I, 2, R1), count(I0, 1, _), param(AllStrings) do 
	     number_string(I0, RowHeading), 
	     arg([I, 1], AllStrings, RowHeading)), 
	(count(J, 2, C1), count(J0, 1, _), param(AllStrings) do 
	     number_string(J0, ColumnHeading), 
	     arg([1, J], AllStrings, ColumnHeading)), 
	fill_in_element_strings(ViewableName, AllStrings, MarkerList).


write_formatted_matrix(AllStrings, ColumnWidths, String):-
	(foreacharg(Row, AllStrings),
	 foreach(RowString, RowStrings), 
	 param(ColumnWidths)
	do
	    write_formatted_row(Row, ColumnWidths, RowString)
	), 
	join_string(RowStrings, "\n", String).

write_formatted_row(RowArray, ColumnWidths, RowString):-
	(foreacharg(ElementStringIn, RowArray), 
	 foreach(ColumnWidth, ColumnWidths), 
	 foreach(ElementStringOut, ElementStrings)
	do
	    write_formatted_element(ElementStringIn, ColumnWidth, 
				    ElementStringOut)
	), 
	join_string(ElementStrings, " ", RowString).

write_formatted_element(ElementStringIn, ColumnWidth, ElementStringIn):-
	string_length(ElementStringIn, ColumnWidth), 
	!.

write_formatted_element(ElementStringIn, ColumnWidth, ElementStringOut):-
	string_length(ElementStringIn, StringInLength), 
	StringInLength < ColumnWidth, 
	concat_string([ElementStringIn, " "], ElementStringMid), 
	write_formatted_element(ElementStringMid, ColumnWidth, 
				ElementStringOut).

ensure_string(AtomOrString, String):-
	atom(AtomOrString), 
	!,
	atom_string(AtomOrString, String).

ensure_string(String, String).

fill_in_element_strings(ViewableName, AllStrings, MarkerList):-
	dim(AllStrings, [MaxI, MaxJ]), 
	(count(I, 2, MaxI), 
	 count(Row, 1, _), 
	 param(ViewableName, AllStrings, MaxJ, MarkerList)
	do
	    (count(J, 2, MaxJ), 
	     count(Column, 1, _), 
	     param(ViewableName, AllStrings, I, Row, MarkerList)
	    do  
		viewable_element(ViewableName, [Row, Column], VE), 
		viewable_element_to_string(VE, String),
		add_marker(MarkerList, Row, Column, String, 
			   MarkedString),  
		arg([I, J], AllStrings, MarkedString)
	    )
	).

add_marker(MarkerList, RowIndex, ColumnIndex, String, NewString):-
	member(marker(Marker, RowIndex, ColumnIndex), MarkerList), 
	!, 
	concat_strings(String, Marker, NewString).

add_marker(_MarkerList, _RowIndex, _ColumnIndex, String, String).

find_column_widths(AllStrings, ColumnWidths):-
	dim(AllStrings, [MaxI, MaxJ]), 
	(count(J, 1, MaxJ), 
	 param(AllStrings, MaxI), 
	 foreach(ColumnWidth, ColumnWidths)
	do
	    (count(I, 1, MaxI), 
	     param(AllStrings, J), 
	     foreach(StringLength, StringLengths)
	    do  
		arg([I, J], AllStrings, String),
		string_length(String, StringLength) 
	    ),  
	    maxlist(StringLengths, ColumnWidth)
	).
	

maxlist([H|T], Max):-
	maxlist_aux(T, H, Max).

maxlist_aux([], Max, Max):-
	!.

maxlist_aux([H|T], MaxSoFar, MaxFinal):-
	H > MaxSoFar, 
	!, 
	maxlist_aux(T, H, MaxFinal).

maxlist_aux([_|T], MaxSoFar, MaxFinal):-
	maxlist_aux(T, MaxSoFar, MaxFinal).





