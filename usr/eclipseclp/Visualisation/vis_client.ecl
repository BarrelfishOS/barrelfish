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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK

:- lib(hash).

:- export vis_client_register/4.
:- export vis_client_unregister/1.
:- export vis_client_request_termination/1.
:- export vis_client_interest_modify/5.
:- export vis_client_breakpoint/0.

:- local struct(vis_client(name, viewables_stream, interest_stream, 
			   updates_stream)). 

:- local struct(interest_spec(interest_identifier,  
			      change_condition, 
			      susp_list, 
			      view_granularity)).

:- local struct(interest_identifier(name, 
				    vis_client_name, 
				    viewable_name)).


:- local variable(breakpoint). % if set to 'yes' then a breakpoint
                               % will happen after the next vis_client
                               % event is flushed
:- setval(breakpoint,no).

%-------------Code to notify vis clients of viewable notifications-------

verbose(no).

vis_protocol_version(1).


valid_interest_spec_list(InterestSpecList):-

	(foreach(interest_spec(_SpecName, 
			       ChangeCondition, 
			       ViewGranularity), InterestSpecList)
	do
	    change_condition_to_susp_list(ChangeCondition, _), 
	    memberchk(ViewGranularity, [fine, coarse, timed])
	).



notify_all_interested(VCStream, Notification, ViewableName):-
	interested_vis_client_names(ViewableName, InterestedVisClientNames), 
	notify_list(VCStream, Notification, InterestedVisClientNames).

notify_all_running(VCStream, Notification):-
	running_vis_client_names(RunningVisClientNames), 
	notify_list(VCStream, Notification, RunningVisClientNames).

notify_list(VCStream, Notification, VisClientNames):-
	(foreach(VisClientName, VisClientNames),  
	 param(Notification, VCStream)
	do
	    notify(VCStream, Notification, VisClientName)).

notify(viewables, Notification, VisClientName):-
	(
	    is_registered_vis_client(VisClientName)
	->
	    retrieve_running_vis_client(VisClientName, VisClient), 
	    VisClient = vis_client with viewables_stream: ViewablesStream, 
	    write_exdr_safe(ViewablesStream, Notification, VisClientName),
            breakpoint_if_required
	;
	    true
	).


notify(updates, Notification, VisClientName):-
	(
	    is_registered_vis_client(VisClientName)
	->
	    retrieve_running_vis_client(VisClientName, VisClient), 
	    VisClient = vis_client with updates_stream: UpdatesStream, 	
	    write_exdr_safe(UpdatesStream, Notification, VisClientName),
            breakpoint_if_required
	;
	    true
	).

% called by the vis_client to indicate that the next viewable event
% should trigger a breakpoint.
vis_client_breakpoint:-
        setval(breakpoint,yes)@vc_support.

% triggers a breakpoint if the local flag 'breakpoint' is set to 'yes'
breakpoint_if_required:-
        (getval(breakpoint,yes) ->
             peer_do_multitask(vis_event),
             setval(breakpoint,no)
        ;
             true
        ).

write_exdr_safe(Stream, Notification, VisClientName):-
	block((
		  write_exdr(Stream, Notification), 
		  flush(Stream)
	      ), 
	      _AnyTag, 
	      (
		  % if the stream has been closed we deduce that the VC is
		  % terminated.
		  (is_registered_vis_client(VisClientName) -> 
		       (
			   vis_client_unregister(VisClientName),
			   concat_string(["Unregistering visualisation client ", 
					  VisClientName, "."], 
					 WarningString),
			   writeln(warning_output, WarningString), 
			   flush(warning_output)
		       );
		       true
		  )
	      )
	     ).

read_exdr_safe(Stream, TermRead, VisClientName):-
	block((
		  read_exdr(Stream, TermRead)
	      ), 
	      _AnyTag, 
	      (
		  % if the stream has been closed we deduce that the VC is
		  % terminated.
		  (is_registered_vis_client(VisClientName) -> 
		       (
			   vis_client_unregister(VisClientName),
			   concat_string(["Unregistering visualisation client ", 
					  VisClientName, "."], 
					 WarningString),
			   writeln(warning_output, WarningString), 
			   flush(warning_output)
		       );
		       true
		  )
	      )
	     ).

		  
		  
	     

read_interests(ViewableName, VisClientName):-
	retrieve_running_vis_client(VisClientName, VisClient), 
	VisClient = vis_client with interest_stream: InterestStream,
	read_exdr_safe(InterestStream, 
		       interest(viewable_create(ViewableName), 
				YesOrNo, 
				InterestSpecList), 
		       VisClientName), 
	(
	    (YesOrNo == yes, 
	     valid_interest_spec_list(InterestSpecList)) 
	->
	    (
		(foreach(interest_spec(Name, ChangeCondition, ViewGranularity), 
			 InterestSpecList), 
		 param(VisClientName, ViewableName)
		do
		    InterestIdentifier = interest_identifier with
		    [   
			name: Name, 
			viewable_name:ViewableName,
			vis_client_name:VisClientName
		    ], 
		    add_interest_spec(interest_spec with 
				     [
					 interest_identifier:InterestIdentifier,
					 change_condition:ChangeCondition, 
					 view_granularity:ViewGranularity
				     ])
		)	    
	    )
	; 
	    true
	).


contract_destroy_event_handler:-
	%writeln(contract_destroy_event_handler), 
        trigger(flush_timed_event),  % ensure that all timed updates
                                     % are sent before the viewable
                                     % is destroyed
	getval(contract_destroy_queue, StreamNumber), 
	read_fail_stream_terms(StreamNumber, BacktrackEvents), 
	(foreach(backtrack_event(Event, 
				 InterestedVCNames, 
				 QueuesToClose), 
		 BacktrackEvents)
	do
	    (Event = viewable_destroy(ViewableName)
	    ->
		retrieve_viewable_interest_specs(ViewableName, 
						 InterestSpecs), 
		(foreach(InterestSpec, InterestSpecs)
		do
		    InterestSpec = 
		       interest_spec with interest_identifier:InterestID,
		       delete_interest_spec(InterestID)
		)
	    ;
		true
	    ),
	    (foreach(QueueID, QueuesToClose) 
	    do close(QueueID)),
            % writeln(event(Event)), 
	    (
		(Event = 
		viewable_contract(ViewableName), 
		\+viewable(ViewableName))
	    ->
		true
	    ;
		notify_list(viewables, Event, InterestedVCNames)
	    )
	).



%------------------Code to manage collection of running VCs -------------

:- local variable(running_vis_clients).
:- setval(running_vis_clients, []).

is_registered_vis_client(VisClientName):-
	running_vis_client_names(RunningVisClientNames), 
	memberchk(VisClientName, RunningVisClientNames).

running_vis_client_names(RunningVisClientNames):-
	getval(running_vis_clients, RunningVisClients), 
	(foreach(RunningVisClient, RunningVisClients), 
	 foreach(RunningVisClientName, RunningVisClientNames)
	do
	    arg(name of vis_client, RunningVisClient, RunningVisClientName)
	).

add_running(NewVisClient):-
	NewVisClient = vis_client with name:VisClientName, 
	\+ retrieve_running_vis_client(VisClientName, _),
	getval(running_vis_clients, RunningVisClients), 
	setval(running_vis_clients, 
	       [NewVisClient|RunningVisClients]).

retrieve_running_vis_client(VisClientName, VisClient):-
	getval(running_vis_clients, RunningVisClients), 
	member(VisClient, RunningVisClients), 
	VisClient = vis_client with name:VisClientName, 
	!. 

remove_running(VisClientName, VisClient):-
	getval(running_vis_clients, RunningVisClients), 
	VisClient = vis_client with name:VisClientName, 
	eclipse_language:delete(VisClient, RunningVisClients, NewRunningVisClients),
	!, % only need to delete one; names are unique.
	setval(running_vis_clients, NewRunningVisClients).
	
%-----------------------------------------------------------------------

% stream number of queue recording contract/destroy events.

:- local variable(contract_destroy_queue).


:- open(queue(""), update, StreamNumber, [event(contract_destroy_event)]),
   setval(contract_destroy_queue, StreamNumber).
:- set_event_handler(contract_destroy_event, contract_destroy_event_handler/0).
:- set_event_handler(back_update_event, trigger/1).     


vis_client_register(VisClientName, ViewablesStream, 
		    UpdatesStream, InterestStream):-
	vis_protocol_version(VisProtocolVersion), 
	read_exdr(InterestStream, ProtocolVersionTerm),
	(
	    ProtocolVersionTerm == vis_protocol_version(VisProtocolVersion) 
	->
	    write_exdr(ViewablesStream, vis_protocol_supported), 
	    flush(ViewablesStream)
	;
	    write_exdr(ViewablesStream, 
		       vis_protocol_version(VisProtocolVersion)), 
	    flush(ViewablesStream), 
	    fail
	),
	add_running(vis_client with [name:VisClientName, 
				     viewables_stream:ViewablesStream, 
				     updates_stream:UpdatesStream, 
				     interest_stream:InterestStream]), 
	(verbose(yes) ->
	    (
		concat_string(["Visualisation client ",
			       VisClientName, " registered."], 
			      String), 
		writeln(String)
	    )
	;
	    true
	).


vis_client_unregister(VisClientName):-
	remove_running(VisClientName, _),
	retrieve_vis_client_interest_specs(VisClientName, InterestSpecs), 
	(foreach(InterestSpec, InterestSpecs)
	 do
	     InterestSpec = interest_spec with interest_identifier:InterestID,
	     delete_interest_spec(InterestID)
	), 
	(verbose(yes) ->
	    (
		concat_string(["Visualisation client ",
			       VisClientName, " unregistered."], 
			      String), 
		writeln(String)
	    )
	;
	    true
	).

vis_client_request_termination(VisClientName):-
	retrieve_running_vis_client(VisClientName, VisClient),
	VisClient = vis_client with viewables_stream:ViewablesStream, 
	% writing this will throw because as a consequence the VC should
	% close the queue
	block((write_exdr(ViewablesStream, terminate), 
	       flush(ViewablesStream)), 
	      _, true).
	



vis_clients:-
	retrieve_running_vis_client(_,_), 
	!.

vis_client_interest_modify(ViewableName, 
			   VisClientName, 
			   InterestSpecName, view_granularity, 
			   NewViewGranularity):-
	InterestID = interest_identifier with 
	   [
	       vis_client_name:VisClientName, 
	       viewable_name:ViewableName, 
	       name:InterestSpecName
	   ],
	remove_interest_spec(InterestID, InterestSpec), 
	setarg(view_granularity of interest_spec, InterestSpec, 
	       NewViewGranularity), 
	add_interest_spec(InterestSpec).

%--------Management of which VCs are interested in each viewable--------

:- local variable(interest_specs).
:- hash_create(InterestSpecs), setval(interest_specs, InterestSpecs).


add_interest_spec(InterestSpec):-
	InterestSpec = interest_spec with 
	    [interest_identifier:InterestID,
	     change_condition:ChangeCondition], 
	getval(interest_specs, InterestSpecs),
	\+ hash_find(InterestSpecs, InterestID, _),
	change_condition_to_susp_list(ChangeCondition, SuspList), 
	setarg(susp_list of interest_spec, InterestSpec, SuspList), 
	hash_add(InterestSpecs, InterestID, InterestSpec),
	setval(interest_specs, InterestSpecs).


remove_interest_spec(InterestID, InterestSpec):-
	\+ nonground(InterestID), 
	getval(interest_specs, InterestSpecs),
	hash_remove(InterestSpecs, InterestID, InterestSpec), 
	setval(interest_specs, InterestSpecs).

delete_interest_spec(InterestID):-
	remove_interest_spec(InterestID, _).

% nondeterministically retrieve one interest spec whose interest ID unifies
% with InterestID

% ground case, use hash find
retrieve_interest_spec(InterestID, InterestSpec):-
	\+ nonground(InterestID),
	!,
	getval(interest_specs, InterestSpecs), 
	hash_find(InterestSpecs,
		  InterestID, 
		  InterestSpec).

% nonground case, turn into list and search nondeterministically.
retrieve_interest_spec(InterestID, InterestSpec):-
	getval(interest_specs, InterestSpecs), 
	hash_list(InterestSpecs, Keys, Values), 
	(foreach(Key, Keys), 
	 foreach(Value, Values),
	 param(InterestID), 
	 fromto(none, InterestSpecIn, InterestSpecOut, finished(InterestSpec))
	do
	    (
		(Key = InterestID, InterestSpecOut = finished(Value))
	    ;
		InterestSpecIn = InterestSpecOut
	    )
	).



% retrieve all interest specs whose interest ID has viewable_name:ViewableName
retrieve_viewable_interest_specs(ViewableName, InterestSpecs):-
	findall(InterestSpec, 
		retrieve_interest_spec(interest_identifier with viewable_name:
				       ViewableName, InterestSpec), 
		InterestSpecs).

% retrieve all interest specs whose interest ID has
% vis_client_name:VisClientName
retrieve_vis_client_interest_specs(VisClientName, InterestSpecs):-
	findall(InterestSpec, 
		retrieve_interest_spec(interest_identifier with vis_client_name:
				       VisClientName, InterestSpec), 
		InterestSpecs).


interested_vis_client_names(ViewableName, InterestedVisClientNames):-
	retrieve_viewable_interest_specs(ViewableName, InterestSpecs), 
	findall(InterestedVisClientName, 
		(
		    member(InterestSpec, InterestSpecs), 
		    InterestSpec =
		      interest_spec with interest_identifier:InterestID, 
		    InterestID =
		      interest_identifier with 
		        vis_client_name:InterestedVisClientName
		),
		InterestedVisClientNames0
	       ),
	% remove duplicates by sorting
	sort(InterestedVisClientNames0, InterestedVisClientNames).


change_condition_to_susp_list(change_condition(Module, StructName, ArgName),
			      susp_list(Module, Index)):-
	current_module(Module), 
	current_struct(StructName, X) @ Module, 
	!,
	(foreacharg(Arg, X), 
	 param(Index, ArgName), 
	 count(I, 1, _)
	do
	    Arg == ArgName -> Index = I ; true
	), 
	integer(Index).


%-------- Finalization --------

:- local finalization(vis_client_cleanup).

vis_client_cleanup :-
	running_vis_client_names(RunningVisClientNames), 
	(foreach(VisClientName, RunningVisClientNames)
	  do
	    vis_client_request_termination(VisClientName)).


