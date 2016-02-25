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

%--Part of code which deals with managing suspended goals on viewable terms--

initialise_suspensions(ViewableName):-
	retrieve_viewable_interest_specs(ViewableName, InterestSpecs), 
	retrieve_viewable(ViewableName, 
			  viewable with 
			    [interest_bookkeeping_table:InterestBookkeepingTable, type:Type]), 
	!, 
	(foreach(InterestSpec, InterestSpecs), 
	 param(_ViewableName, InterestBookkeepingTable, Type)
	do
	    InterestSpec = interest_spec with interest_identifier:InterestID,
	    hash_find(InterestBookkeepingTable, 
		      InterestID, InterestBookkeeping), 
	    initialise_suspensions(Type,InterestID, InterestBookkeeping)
	).


initialise_suspensions(Type,InterestID, 
		       InterestBookkeeping):-
	InterestID = interest_identifier with viewable_name:ViewableName, 
	viewable_size(ViewableName, SizeList), 
	for_all_elements(SizeList, 1, 
			 initialise_element_suspensions(_IndexList,
                                                        Type,
							InterestID, 
							InterestBookkeeping)).


initialise_extra_suspensions(ViewableName, DimensionNumber):-
	retrieve_viewable_interest_specs(ViewableName, InterestSpecs), 
	retrieve_viewable(ViewableName, 
			  viewable with 
			    [interest_bookkeeping_table:InterestBookkeepingTable, type:Type]), 
	(foreach(InterestSpec, InterestSpecs), 	 
	 param(DimensionNumber, InterestBookkeepingTable, Type)
	do
	    InterestSpec = interest_spec with interest_identifier:InterestID,
	    hash_find(InterestBookkeepingTable, InterestID, 
		      InterestBookkeeping), 
	    initialise_extra_suspensions(Type,DimensionNumber, InterestID, InterestBookkeeping)
	). 

initialise_extra_suspensions(Type,DimensionNumber, InterestID, InterestBookkeeping):-
	InterestID = interest_identifier with viewable_name:ViewableName, 	
	viewable_size(ViewableName, SizeList), 
	length(SizeList, NDims), 
	NFirstDims is DimensionNumber - 1, 
	NLastDims is NDims - DimensionNumber,
	length(FirstSizeList, NFirstDims), 
	length(LastSizeList, NLastDims), 
	listut:append(FirstSizeList, [XDimSize | LastSizeList], SizeList),
	!, % Remove choicepoint from append
	for_all_elements(FirstSizeList, 1, 
			 initialise_extra_element_suspensions1(_,Type,
							       XDimSize, 
							       LastSizeList,
							       InterestID, 
							       InterestBookkeeping)).

initialise_extra_element_suspensions1(FirstIndexList,Type, XDimSize, 
				      LastSizeList, InterestID, InterestBookkeeping):-
	for_all_elements(LastSizeList, 3, 
			 initialise_extra_element_suspensions2(FirstIndexList,
							       XDimSize, 
							       _, 
                                                               Type,
							       InterestID,
							       InterestBookkeeping)).


initialise_extra_element_suspensions2(FirstIndexList,XDimSize, 
				      LastIndexList, Type, InterestID, InterestBookkeeping):-
	listut:append(FirstIndexList, [XDimSize|LastIndexList], FinalIndexList), 
	% writeln('initialising suspensions at '(FinalIndexList)),
	initialise_element_suspensions(FinalIndexList,Type, InterestID, 
				       InterestBookkeeping).


initialise_element_suspensions(IndexList, Type, InterestID, InterestBookkeeping):-
	retrieve_interest_spec(InterestID, InterestSpec), 
	InterestID = interest_identifier with viewable_name:ViewableName,
	!,
	initialise_element_interest_stamp_table(ViewableName, IndexList, 
						InterestID), 
	initialise_forward(Type,IndexList, InterestSpec, InterestBookkeeping).


initialise_element_interest_stamp_table(ViewableName, IndexList, InterestID):-
	retrieve_viewable(ViewableName, V),
	!, 
	V = viewable with elements:Elements,
	mixed_array_retrieve(Elements, IndexList, 
			     Element),
	arg(interest_stamp_table of viewable_element, Element, 
	    InterestStampTable),
	BacktrackRecordStamp = stamp([]), 
	timestamp_init(BacktrackRecordStamp, 1), 
	hash_add(InterestStampTable, InterestID, BacktrackRecordStamp).

initialise_forward(Type,IndexList, InterestSpec, InterestBookkeeping):-
        Type=array(_,changeable(Solver,_)),
	InterestSpec = interest_spec with 
	   [
	       interest_identifier:InterestID
	   ],
	InterestID = interest_identifier with 
	       viewable_name:ViewableName,
	retrieve_viewable(ViewableName, V), 
	V = viewable with elements:Elements,
	mixed_array_retrieve(Elements, IndexList, 
			     viewable_element with 
			    [element:Element, 
			     interest_stamp_table:InterestStampTable]),
	nonground(Element),
	hash_find(InterestStampTable, InterestID, BacktrackRecordStamp), 
	!,
        % suspend on instantiation
	suspend(forward_update_demon(Element, IndexList, InterestID, 
				     InterestBookkeeping, BacktrackRecordStamp, 
				     Susp),  
		1, Element -> inst, Susp),
        % suspend on solver change
        Solver:suspend_on_change(Element,Susp ).        
initialise_forward(_Type,IndexList, InterestSpec, InterestBookkeeping):-
	InterestSpec = interest_spec with 
	   [
	       interest_identifier:InterestID,
	       susp_list:susp_list(Module, Arg)
	   ],
	InterestID = interest_identifier with 
	       viewable_name:ViewableName,
	retrieve_viewable(ViewableName, V), 
	V = viewable with elements:Elements,
	mixed_array_retrieve(Elements, IndexList, 
			     viewable_element with 
			    [element:Element, 
			     interest_stamp_table:InterestStampTable]),
	nonground(Element),
	hash_find(InterestStampTable, InterestID, BacktrackRecordStamp), 
	!,
	suspend(forward_update_demon(Element, IndexList, InterestID, 
				     InterestBookkeeping, BacktrackRecordStamp, 
				     Susp),  
		1, Element -> Module:Arg, Susp).


% if ground, don't suspend 
initialise_forward(_, _, _, _).



% the various demons and suspended goals

:- demon forward_update_demon/6.

% if vis client is no longer registered, kill self.
forward_update_demon(_Element, _IndexList, InterestID, 
		     _InterestBookkeeping, _BacktrackRecordStamp, Susp):-
	InterestID = interest_identifier with 
	       vis_client_name:VisClientName, 
	\+ is_registered_vis_client(VisClientName), 
	!,
	kill_suspension(Susp).

forward_update_demon(Element, IndexList, InterestID, 
		     InterestBookkeeping, BacktrackRecordStamp, Susp):-
	retrieve_interest_spec(InterestID, InterestSpec), 
	InterestSpec = interest_spec with view_granularity:ViewGranularity, 
	InterestID = interest_identifier with 
	   [
	       viewable_name:ViewableName,
	       vis_client_name:VisClientName, 
	       name:InterestSpecName
	   ],
	(
	    ViewGranularity == fine
	->
	    (
		forward_fine_update(ViewableName, IndexList, 
				    InterestSpecName, 
				    VisClientName, InterestBookkeeping, 
				    BacktrackRecordStamp)
	    )
	;   ViewGranularity == coarse ->
	    (
		forward_coarse_update(ViewableName, IndexList, 
				      InterestSpecName, VisClientName, 
				      InterestBookkeeping, 
				      BacktrackRecordStamp)
	    )
        ;   % ViewGranularity == timed
            (
		forward_timed_update(ViewableName, IndexList, 
                                     InterestSpecName, VisClientName, 
                                     InterestBookkeeping, 
                                     BacktrackRecordStamp)
            )            
	), 
	(nonground(Element) -> true ; kill_suspension(Susp)).



forward_fine_update(ViewableName, IndexList, InterestSpecName, 
		    VisClientName, InterestBookkeeping, 
		    BacktrackRecordStamp):-
	InterestBookkeeping = interest_bookkeeping with 
	       backtracked_elements_queue:BacktrackedElementsQueue, 
	notify(updates, 
	       update(ViewableName, InterestSpecName, forward, 
		      [element(IndexList)]), 
	       VisClientName),
	request_fail_event(InterestBookkeeping, 
			   back_update_stamp of interest_bookkeeping, 
			   back_update_event),
	request_fail_write_term(BacktrackRecordStamp, 1, 
				BacktrackedElementsQueue, IndexList).



forward_coarse_update(ViewableName, IndexList, InterestSpecName, 
		      VisClientName, InterestBookkeeping, 
		      BacktrackRecordStamp):-
	InterestBookkeeping = interest_bookkeeping with 
	       coarse_changed_elements_set:ChangedElementsSet,
	(
	    hash_list(ChangedElementsSet, [], _)
	->
	    (
		suspend(suspended_coarse_update(ViewableName, 
						InterestSpecName, 
						VisClientName, 
						InterestBookkeeping), 
			8, 
			trigger(send_coarse_update)), 
		trigger(send_coarse_update)
	    )
	;
	    true
	), 		
	(
	    hash_find(ChangedElementsSet, IndexList, _) 
	-> 
	    true
	;
	    hash_add(ChangedElementsSet, 
		     index_stamp(IndexList, BacktrackRecordStamp), 
		     present)
	).


suspended_coarse_update(ViewableName, InterestSpecName, VisClientName, 
			InterestBookkeeping):-
	InterestBookkeeping = interest_bookkeeping with 
	[
	    backtracked_elements_queue:BacktrackedElementsQueue, 
	    coarse_changed_elements_set:ChangedElementsSet
	],
	hash_list(ChangedElementsSet, IndexStampList, _),
	hash_create(EmptyChangedElementsSet), 
	setarg(coarse_changed_elements_set of interest_bookkeeping, 
	       InterestBookkeeping, EmptyChangedElementsSet), 
	(IndexStampList == [] ->
	     true
	;
	     % for backtracking over change
	     request_fail_event(InterestBookkeeping, 
				back_update_stamp of interest_bookkeeping, 
				back_update_event),
	     (foreach(index_stamp(Index, BacktrackRecordStamp), IndexStampList), 
	      foreach(element(Index), UpdateList), 
	      param(BacktrackedElementsQueue)
	     do
              request_fail_write_term(BacktrackRecordStamp, 1, 
					 BacktrackedElementsQueue, Index)
	     ),
	     notify(updates, 
		    update(ViewableName, InterestSpecName, 
			   forward, UpdateList), 
		    VisClientName)
	).



forward_timed_update(_ViewableName, IndexList, _InterestSpecName, 
                     _VisClientName, InterestBookkeeping, 
                     BacktrackRecordStamp):-
	InterestBookkeeping = interest_bookkeeping with 
        [
	    backtracked_elements_queue:BacktrackedElementsQueue, 
            timed_changed_elements_set:ChangedElementsSet,
            timed_update_counter:UpdateCounterShelf
        ],
        (
	    sepia_kernel:store_contains(ChangedElementsSet, element(IndexList))
        ->
            % This element is already recorded in the table
            true
        ;
	    sepia_kernel:store_set(ChangedElementsSet,
                                element(IndexList), 
                                present)
        ),
        request_fail_write_term(BacktrackRecordStamp, 1, 
                                BacktrackedElementsQueue,
                                IndexList),
	( shelf_dec(UpdateCounterShelf, 1) ->
            true
        ;
            trigger(flush_timed_event)
	).

:- demon flush_timed_update/3.
% if vis client is no longer registered, kill self.
flush_timed_update(InterestID, _InterestBookkeeping, Susp):-
	InterestID = interest_identifier with 
	       vis_client_name:VisClientName, 
	\+ is_registered_vis_client(VisClientName), !,
	kill_suspension(Susp).

flush_timed_update(InterestID,
                   InterestBookkeeping, 
                   _Susp):-
	InterestID = interest_identifier with 
        [
            name:InterestSpecName, 
            viewable_name:ViewableName, 
            vis_client_name:VisClientName
        ],
	InterestBookkeeping = interest_bookkeeping with 
        [
	    %backtracked_elements_queue:BacktrackedElementsQueue, 
            timed_changed_elements_set:ChangedElementsSet,
            timed_update_counter:UpdateCounterShelf
        ],
        % reset the counter
        shelf_set(UpdateCounterShelf, 1, 100),
        % send the data
	sepia_kernel:stored_keys(ChangedElementsSet, UpdateList),
	sepia_kernel:store_erase(ChangedElementsSet),
        
        (UpdateList == [] ->
             true
        ;
             request_fail_event(InterestBookkeeping, 
                                back_update_stamp of interest_bookkeeping, 
                                back_update_event),
             notify(updates,
                    update(ViewableName, InterestSpecName, 
                           forward, UpdateList), 
                    VisClientName)
        ),
        % now send the back_updates
        trigger(back_update_event).


:- demon back_update_demon/3.


% if vis client is no longer registered, kill self.
back_update_demon(InterestID, _BacktrackedElementsQueue, Susp):-
	InterestID = interest_identifier with 
	       vis_client_name:VisClientName, 
	\+ is_registered_vis_client(VisClientName), !,
	kill_suspension(Susp).

back_update_demon(InterestID, BacktrackedElementsQueue, _Susp):-
	InterestID = interest_identifier with 
	  [name:InterestSpecName, 
	   viewable_name:ViewableName, 
	   vis_client_name:VisClientName],
	(at_eof(BacktrackedElementsQueue) ->
	     true
	;
	     (
		 read_fail_stream_terms(BacktrackedElementsQueue, Indexes), 
		 (foreach(Index, Indexes), 
		  foreach(element(Index), UpdateList)
		 do
		     true), 
                 % writeln(update_list(UpdateList)), 
		 (UpdateList \== [] ->
		      notify(updates, 
			     update(ViewableName, InterestSpecName, 
				    back, UpdateList), 
			     VisClientName)
		 ;
		      true
		 )	     
	     )
	).



	
