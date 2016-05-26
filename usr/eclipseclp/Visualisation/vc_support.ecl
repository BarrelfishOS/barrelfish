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
% Contributor(s): 
% 
% END LICENSE BLOCK

:- module(vc_support).

:- lib(logarr).
:- lib(listut).
:- lib(graph_algorithms).

:- import set_bip_error/1, get_bip_error/1 from sepia_kernel.

:- import
	timestamp_init/2,
	timestamp_age/3,
	request_fail_event/3, 
	request_fail_write/4
    from sepia_kernel.

:- export viewable_create/4.
:- export viewable_create/3.
:- export viewable_create/2.
:- export viewable/1.
:- export viewable_size/2.
:- export viewable_type/2.
:- export viewable_expand/3.
:- export viewable_expand/4.
:- export viewable_element/3.
:- export viewable_element_to_string/2.
:- export viewable_element_execute/3.
:- export viewable_changeable_execute/4.
:- export viewable_get_location_name/4.
:- export viewable_get_location_names/3.
 
% structure of a viewable + element
:- local struct(viewable(name, 
			 type, 
			 size, 
			 elements, 
			 destroy_stamp, 
			 contract_stamp,
			 interest_bookkeeping_table,
			 location_names)). 

:- local struct(interest_bookkeeping(backtracked_elements_queue, 
				     back_update_stamp, 
				     coarse_changed_elements_set,
				     timed_changed_elements_set,
                                     timed_update_counter)).


:- local struct(viewable_element(element, interest_stamp_table)).

:- include(vis_client).
:- include(suspensions).
:- include(ve_storage).
:- include(graph_support).

:- comment(categories, ["Development Tools","Visualisation"]).
:- comment(summary, 
	   "Library to support implementation of visualisation clients").

:- comment(author, "Josh Singer").

:- comment(copyright, "Cisco Systems, Inc.").



:- comment(desc, html("

           <P> This library should be used if you intend to implement a 
           new visualisation client. It should not be used for anything 
           else. 

           <P> Viewables are made up of <EM>viewable elements</EM>. A
           particular viewable element can accessed using
           <CODE>viewable_element/3</CODE>.

")).



% existent_viewables: collection of existent viewables

% reference because backtracks over viewable_create/viewable_expand should 
% restore previous state and because references should be to actual 
% variables. 
:- local reference(existent_viewables).

:- setval(existent_viewables, []).


% counter to be used for making unique viewable names

viewable_create(_,_,_,_):-
	\+vis_clients, 
	!.

viewable_create(ViewableName, Elements, Type, LocNamesList):-
	Elements \== [], % handled by other clause
	regular_nested_list(Elements, _NDimensions, DimSizes), 
	!,
	array_from_nested_list(Elements, DimSizes, ElementArray), 
	viewable_create(ViewableName, ElementArray, 
			Type, LocNamesList).


viewable_create(ViewableName, Graph, Type, PropertyList):-
        Type=graph(fixed),
        !,
        viewable_create(ViewableName, Graph, graph(fixed,graph_data), PropertyList).
viewable_create(ViewableName, Graph, Type, PropertyList):-
        Type=graph(fixed,ElemType),
        !,
        flatten_graph_property_list(Graph, PropertyList,
                                    FlatPropertyList),        
        ((member(node_property(NodeArgPos->NodeMarkupList), FlatPropertyList),
               memberchk(label,NodeMarkupList)) ->
             true
        ;
             sepia_kernel:set_bip_error(1)
        ),
        ((member(edge_property(EdgeArgPos->EdgeMarkupList), FlatPropertyList),
               memberchk(label,EdgeMarkupList)) ->
             true
        ;
             sepia_kernel:set_bip_error(1)
        ),
        graph_get_node_info_list(Graph, NodeLabelInfo, NodeArgPos),
        graph_get_edge_info_list(Graph, EdgeLabelInfo, EdgeArgPos),
        graph_to_vis_graph(Graph, NodeLabelInfo, EdgeLabelInfo,
                           VisGraph),
        % Create a 2D viewable with the special reserved type "graph_data"
        viewable_create(ViewableName,VisGraph, array([fixed,fixed],ElemType)).

viewable_create(ViewableName, Elements, Type, LocNamesList):-
	(
	    atom(ViewableName)
	-> 
	    true ; sepia_kernel:set_bip_error(5)
	),
	((\+ nonground(Type), Type = array(FixityList, ElementType))
	-> true; sepia_kernel:set_bip_error(1)), 
	((valid_fixity_list(FixityList), valid_element_type(ElementType)) 
	-> true ;
	sepia_kernel:set_bip_error(1)),
	length(FixityList, NDimensions), 
	(regular_array(Elements, NDimensions, DimSizes) -> true ; 
	 sepia_kernel:set_bip_error(1)),
	(check_element_types(Elements, DimSizes, ElementType) -> true ; 
	 sepia_kernel:set_bip_error(5)),
	(check_location_names(DimSizes, LocNamesList) -> true ;
	 sepia_kernel:set_bip_error(5)),
	(check_name(ViewableName) -> true ; 
	 sepia_kernel:set_bip_error(1)),
	!,  

	elements_array_to_ve_mixed_array(Elements, DimSizes, 
					 FixityList, StoredElements),
	hash_create(InterestBookkeepingTable),
	create_viewable_record(ViewableName, StoredElements, DimSizes, 
			       array(FixityList, ElementType), [], [],
			       InterestBookkeepingTable, LocNamesList, Struct),
	timestamp_init(Struct, destroy_stamp of viewable),
	timestamp_init(Struct, contract_stamp of viewable),
	arg(interest_bookkeeping_table of viewable, Struct, 
	    InterestBookkeepingTable), 
	store_viewable(Struct),
	% notify of creation of viewable 
	notify_all_running(viewables, 
			   viewable_create(ViewableName, 
					   array(FixityList, ElementType))),
	running_vis_client_names(InterestedVCNames), 
	(foreach(InterestedVCName, InterestedVCNames), 
	 param(ViewableName)
	do
	 read_interests(ViewableName, InterestedVCName)), 
	% set up per-interest book-keeping and back update demons
	retrieve_viewable_interest_specs(ViewableName, InterestSpecs),
	(foreach(InterestSpec, InterestSpecs), 
	 foreach(BacktrackedElementsQueue, QueuesToClose), 
	 param(InterestBookkeepingTable, _ViewableName)
	do
	    (
		InterestBookkeeping = interest_bookkeeping with
	        [
		    backtracked_elements_queue:BacktrackedElementsQueue, 
		    coarse_changed_elements_set:CoarseChangedElementsSet,
		    timed_changed_elements_set:TimedChangedElementsSet,
		    timed_update_counter:TimedUpdateCounter
		], 
		timestamp_init(InterestBookkeeping, 
			       back_update_stamp of interest_bookkeeping), 
		open(queue(""), update, BacktrackedElementsQueueHandle),
                % next line hopefully temporary fix, should not use number
                get_stream_info(BacktrackedElementsQueueHandle, physical_stream, BacktrackedElementsQueue),
		hash_create(CoarseChangedElementsSet), % used only in coarse case
		sepia_kernel:store_create(TimedChangedElementsSet), % used only in timed case
                shelf_create(count(0),TimedUpdateCounter), % used only
                                                           % in timed
                                                           % case
		arg(interest_identifier of interest_spec, InterestSpec, InterestID), 
		hash_add(InterestBookkeepingTable, InterestID, 
			 InterestBookkeeping), 
		suspend(back_update_demon(InterestID, 
					  BacktrackedElementsQueue, Susp), 
			1, trigger(back_update_event), Susp),
                
                % create the suspension which will flush the updates
                suspend(flush_timed_update(InterestID,
                                           InterestBookkeeping,
                                           FlushSusp),
                        1, [trigger(flush_timed_event),trigger(postponed)],
                        FlushSusp)
	    
	    )),
	% for destroy notification
	getval(contract_destroy_queue, StreamNumber), 
	interested_vis_client_names(ViewableName, InterestedVCs1Names),
	request_fail_write_term(Struct, destroy_stamp of viewable,
				StreamNumber, 
				backtrack_event(viewable_destroy(ViewableName),
						InterestedVCs1Names, 
						QueuesToClose)), 
	initialise_suspensions(ViewableName).

viewable_create(ViewableName, Elements, Type, LocNamesList):-
	sepia_kernel:get_bip_error(ErrorCode), 
	error(ErrorCode, 
	      viewable_create(ViewableName, Elements, Type, LocNamesList)).




% auxiliary to viewable_create/3

valid_fixity_list(FixityList):-
	(foreach(Element, FixityList) do
	     (Element == fixed -> true ; Element == flexible)). 

% 2-argument version

viewable_create(_,_):-
	\+vis_clients, 
	!.

viewable_create(ViewableName, Elements):-
	Elements \== [], % handled by other clause
	regular_nested_list(Elements, _, DimSizes),
	!, 
	array_from_nested_list(Elements, DimSizes, ElementArray),
	viewable_create(ViewableName, ElementArray).

viewable_create(ViewableName, Elements):-
	(regular_array(Elements, NDimensions, DimSizes) -> true ;
	 sepia_kernel:set_bip_error(1)),
	!,
	length(FixityList, NDimensions), 
	(foreach(Fixity, FixityList) do Fixity = fixed), 
	initialise_location_names(DimSizes, LocNamesList),
	viewable_create(ViewableName, Elements, 
			array(FixityList, any), LocNamesList).
	 
viewable_create(ViewableName, Elements):-
	sepia_kernel:get_bip_error(ErrorCode), 
	error(ErrorCode, 
	      viewable_create(ViewableName, Elements)).

% 3-argument version

viewable_create(_,_,_):-
	\+vis_clients, 
	!.

viewable_create(ViewableName, Elements, Type):-
	Elements \== [], % handled by other clause
	regular_nested_list(Elements, _NDimensions, DimSizes), 
	!,
	array_from_nested_list(Elements, DimSizes, ElementArray), 
	viewable_create(ViewableName, ElementArray, 
			Type).

viewable_create(ViewableName, Elements, Type):-
	((\+ nonground(Type), Type = array(FixityList, ElementType))
	-> true; sepia_kernel:set_bip_error(1)), 
	((valid_fixity_list(FixityList), valid_element_type(ElementType)) 
	-> true ; sepia_kernel:set_bip_error(1)),
	length(FixityList, NDimensions), 
	(regular_array(Elements, NDimensions, DimSizes) -> true ;
	 sepia_kernel:set_bip_error(1)),
	!,
	initialise_location_names(DimSizes, LocNamesList),
	viewable_create(ViewableName, Elements, 
			Type, LocNamesList).

viewable_create(ViewableName, Elements, Type):-
	((\+ nonground(Type),
          (Type = graph(fixed); Type = graph(fixed,_ElemType)))
	-> true; sepia_kernel:set_bip_error(1)), 
	!,
	viewable_create(ViewableName, Elements, Type,
                        [node_property([0->[name(nodes), label]]),
                         edge_property([0->[name(edges), label]])
                        ]).

viewable_create(ViewableName, Elements, Type):-
	sepia_kernel:get_bip_error(ErrorCode), 
	error(ErrorCode, 
	      viewable_create(ViewableName, Elements, Type)).



viewable(_):-
	\+vis_clients,
	!,
	fail.

viewable(ViewableName):-
	((atom(ViewableName) ; var(ViewableName))
	-> true ; sepia_kernel:set_bip_error(5)), 
	!, 
	retrieve_viewable(ViewableName, _).

viewable(ViewableName):-
	sepia_kernel:get_bip_error(X), error(X, viewable(ViewableName)).

:- comment(viewable_element/3, 
	   [amode : viewable_element(++, ++, ?),
	    args : ["ViewableName":"Atom: name of an existent viewable.",
		    "IndexList":"List of integers: the coordinates of the viewable element within the viewable.",
		    "ViewableElement":"Term to be unified with the viewable element."], 
	    summary : "Index a viewable element inside a viewable.",
	    desc : html("Note that this predicate throws an error if there are no current visualisation clients. Retrieves a single viewable element from a named viewable, given the element's array coordinates, and unifies it with <em>ViewableElement</em>. IndexList must be a ground list which has one positive integer for each dimension of the viewable."),
	    fail_if : "ViewableName is not instantiated to the name of an existent viewable. ViewableElement will not unify with the specified element.",
	    exceptions : [1:"There are no current visualisation clients",
			  4:"ViewableName is not an atom or string", 
			  5:"IndexList is not a ground list of integers",
			  6:"A co-ordinate in Index is out of range", 
			  8:"IndexList has the wrong number of elements"],
	    resat : no, 
	    eg : "\
       Success:

       Failure:

       Exceptions raised:
",
	    see_also : [viewable:viewable_create/3, viewable:viewable_size/2, 
			viewable_element_to_string/2, 
			viewable_element_execute/3]]).



viewable_element(ViewableName, IndexList, 
		 ViewableElement):-
	(vis_clients -> true ; 
	 sepia_kernel:set_bip_error(1)),
	((foreach(Index, IndexList) do integer(Index)) -> true ; 
	 sepia_kernel:set_bip_error(5)),
	((string(ViewableName) ; atom(ViewableName))
	-> true ; sepia_kernel:set_bip_error(4)),
	viewable_size(ViewableName, DimensionSizes), 
	length(DimensionSizes, NDimensions), 
	(length(IndexList, NDimensions) -> true ; 
	 sepia_kernel:set_bip_error(8)), 
	(foreach(Index, IndexList), 
	  foreach(DimensionSize, DimensionSizes) do
	      (Index > 0, Index =< DimensionSize) -> true ; 
	      sepia_kernel:set_bip_error(6)),
	retrieve_viewable(ViewableName, V), 
	!,
	V = viewable with [name:ViewableName, elements:Elements],
	mixed_array_retrieve(Elements, IndexList, 
			     viewable_element with element:ViewableElement).

	
viewable_element(ViewableName, IndexList, ViewableElement):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, 
	      viewable_element(ViewableName, IndexList, ViewableElement)).

viewable_get_location_names(_, _, _):-
	\+vis_clients,
	!, 
	fail.

viewable_get_location_names(ViewableName, DimNumber, LocNames):-
	viewable_size(ViewableName, Size),
	length(Size, NDims), 
	((DimNumber > 0, DimNumber =< NDims) -> true;
	 sepia_kernel:set_bip_error(6)),
	retrieve_viewable(ViewableName, 
			  V), 
	V = viewable with location_names:LocationNamesList, 
	listut:nth1(DimNumber, LocationNamesList, LocNames).
	

viewable_get_location_names(ViewableName, DimNumber, LocNames):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, 
	      viewable_get_location_names(ViewableName, DimNumber, 
					  LocNames)).


viewable_get_location_name(_, _, _, _):-
	\+vis_clients,
	!, 
	fail.

viewable_get_location_name(ViewableName, DimNumber, LocNumber, LocName):-
	viewable_get_location_names(ViewableName, DimNumber, LocNames), 
	length(LocNames, DimSize),
	((LocNumber > 0, LocNumber =< DimSize) -> true;
	 sepia_kernel:set_bip_error(6)),
	listut:nth1(LocNumber, LocNames, LocName).
	

viewable_get_location_name(ViewableName, DimNumber, LocNumber, LocName):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, 
	      viewable_get_location_name(ViewableName, DimNumber, 
					 LocNumber, LocName)).

viewable_size(_,_):-
	\+vis_clients,
	!, 
	fail.

viewable_size(ViewableName, SizeList):-
	(atom(ViewableName)
	-> true ; sepia_kernel:set_bip_error(5)),
	((var(SizeList) ; length(SizeList, _))
	-> true ; sepia_kernel:set_bip_error(5)),
	(
	    var(SizeList) -> true ; 
	    (
		(foreach(Size, SizeList) do
		     (nonvar(Size), \+ integer(Size) -> 
			  sepia_kernel:set_bip_error(5) ;
			  true))
	    ) 
	), 
	retrieve_viewable(ViewableName, 
			  V), 
	V = viewable with [size:SizeList], 
	!.

viewable_size(ViewableName, SizeList):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, viewable_size(ViewableName, SizeList)).


viewable_type(_,_):-
	\+vis_clients,
	!,
	fail.

viewable_type(ViewableName, Type):-
	(atom(ViewableName)
	-> true ; sepia_kernel:set_bip_error(5)),
	retrieve_viewable(ViewableName, 
			  V), 
	V = viewable with [type:Type], 
	!.

viewable_type(ViewableName, Type):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, viewable_type(ViewableName, Type)).

viewable_expand(_,_,_,_):-
	\+vis_clients, 
	!.

viewable_expand(ViewableName, DimensionNumber, ExtraElements, LocName):- 
	viewable_expand_checkargs(ViewableName, DimensionNumber, 
				  ExtraElements, LocName, ExtraSize, 
				  ExtraElementsArray), 
	!, 
	retrieve_viewable(ViewableName, Viewable),
	Viewable = viewable with [size:OldSizeList, 
				     elements:OldElements, 
				     type:array(FixityList, 
						_ElementType), 
				     location_names:LocNamesList],
	viewable_expand_body(DimensionNumber, ExtraElementsArray, 
			     FixityList, ExtraSize, OldElements, 
			     OldSizeList, NewElements, NewSizeList),
	add_location_name(LocNamesList, DimensionNumber, 
			  LocName, NewLocNamesList), 
	setarg(size of viewable, Viewable, NewSizeList), 
	setarg(elements of viewable, Viewable, NewElements), 
	setarg(location_names of viewable, Viewable, NewLocNamesList), 
        % for contract notification
	getval(contract_destroy_queue, StreamNumber), 
	interested_vis_client_names(ViewableName, InterestedVCNames),
	request_fail_write_term(Viewable, contract_stamp of viewable, 
				StreamNumber, 
				backtrack_event(viewable_contract(ViewableName),
						InterestedVCNames, 
						[]) 
				),
	notify_all_interested(viewables, 
			      viewable_expand(ViewableName, 
					      DimensionNumber), 
			      ViewableName),
        trigger(flush_timed_event),        
	initialise_extra_suspensions(ViewableName, DimensionNumber).

	
viewable_expand(ViewableName, DimensionNumber, ExtraElements, LocName):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, viewable_expand(ViewableName, 
				     DimensionNumber, ExtraElements, LocName)).


% 3-argument version.


viewable_expand(_,_,_):-
	\+vis_clients, 
	!.

viewable_expand(ViewableName, DimensionNumber, ExtraElements):- 
	viewable_size(ViewableName, SizeList),
	length(SizeList, NDims), 
	(integer(DimensionNumber) -> true;
	 sepia_kernel:set_bip_error(5)),
	((DimensionNumber > 0 , DimensionNumber =< NDims) -> true;
	 sepia_kernel:set_bip_error(6)),
	!,
	listut:nth1(DimensionNumber, SizeList, DimSize),
	NewLoc is DimSize + 1,
	default_location_name(NewLoc, LocName), 
	viewable_expand(ViewableName, DimensionNumber, ExtraElements, 
			LocName).
	     
	
viewable_expand(ViewableName, DimensionNumber, ExtraElements):-
	sepia_kernel:get_bip_error(Error), 
	error(Error, viewable_expand(ViewableName, 
				     DimensionNumber, ExtraElements)).

% auxiliary

add_location_name(LocNamesList, DimensionNumber, 
		  LocName, NewLocNamesList):-
	(foreach(LocNames, LocNamesList), 
	 foreach(NewLocNames, NewLocNamesList), 
	 count(I, 1, _), 
	 param(LocName, DimensionNumber)
	do
	    (I == DimensionNumber ->
		 listut:append(LocNames, [LocName], NewLocNames)
	    ;
		 NewLocNames = LocNames)).


viewable_expand_checkargs(ViewableName, DimensionNumber, ExtraElements, 
			  LocName, ExtraElementsSizeList, ExtraElements1):-
	(atom(ViewableName)
	-> true ; sepia_kernel:set_bip_error(5)),
	viewable(ViewableName), 
	(integer(DimensionNumber) -> true ;
	 sepia_kernel:set_bip_error(5)),
	viewable_type(ViewableName, array(FixityList, ElementType)), 
	length(FixityList, NDimensions), 
	((DimensionNumber > 0, DimensionNumber =< NDimensions) -> true ; 
	 sepia_kernel:set_bip_error(6)),
	((var(ExtraElements), NDimensions > 1) 
	-> sepia_kernel:set_bip_error(1)
	; true),
	(string(LocName) 
	-> true ; sepia_kernel:set_bip_error(5)),
	listut:nth1(DimensionNumber, FixityList, flexible),
	viewable_size(ViewableName, SizeList),
	N1 is DimensionNumber - 1,
	N2 is NDimensions - DimensionNumber,
	ExtraElementsNDimensions is NDimensions - 1,
	length(Dims1, N1), 
	length(Dims2, N2), 
	listut:append(Dims1, [_ExpandingSize | Dims2], SizeList), 
	listut:append(Dims1, Dims2, ExtraElementsSizeList), 
	(ExtraElementsSizeList == [] ->
	     ExtraElements1 = ExtraElements
	;
	     (
		 (
		     (
			 % other branch covers
			 \+ functor(ExtraElements, [], _), 
			 regular_nested_list(ExtraElements, 
					     ExtraElementsNDimensions, 
					     ExtraElementsSizeList), 
			 array_from_nested_list(ExtraElements, 
						ExtraElementsSizeList, 
						ExtraElements1)
		     );
		     (
			 regular_array(ExtraElements, 
				       ExtraElementsNDimensions, 
				       ExtraElementsSizeList), 
			 ExtraElements1 = ExtraElements
		     )
		 ) -> true ; 
		 sepia_kernel:set_bip_error(1)
	     )
	),
	(check_element_types(ExtraElements1, 
			     ExtraElementsSizeList, ElementType)
	-> true ;
	sepia_kernel:set_bip_error(5)).
	    
% viewable_expand_body cases

% Xdim is 1, 
% add new element, increase size of dimension 1

% Xdim is > 1, size of top dimension is > 0
% recurse on each element


% Xdim is > 1, size of top dimension = 0
% increment size of dim Xdim, leave elements unchanged


viewable_expand_body(XDim, ExtraElements, [Fixity|RestFixity], 
		     [Size|ExtraRestSize], 
		     OldElements, [Size|OldRestSize], 
		     NewElementsOut, [Size|NewRestSize]):-
	XDim > 1,
	!,
	XDim1 is XDim - 1, 
	(Size == 0 ->
	     (
		 (foreach(OldSize, OldRestSize), 
		  foreach(DimFixity, RestFixity), 
		  count(I, 2, _), 
		  foreach(NewSize, NewRestSize), 
		  param(XDim) 
		 do
		     (I == XDim -> 
			  (DimFixity = flexible, 
			   NewSize is OldSize + 1) ; 
		      NewSize = OldSize)
		 ),
		 NewElementsOut = OldElements
	     );
	     (
		 mixed_array_create_dimension(Size, Fixity, NewElementsIn), 
		 (count(Index, 1, Size),  
		  fromto(NewElementsIn, NewElements0, 
			 NewElements1, NewElementsOut),
		  param(ExtraElements, RestFixity, OldElements,  
			ExtraRestSize, OldRestSize, NewRestSize, XDim1)
		 do
		     mixed_array_retrieve(OldElements, [Index],OldElement), 
		     mixed_array_retrieve(ExtraElements, [Index],
					  ExtraElement), 
		     viewable_expand_body(XDim1, ExtraElement, 
					  RestFixity, ExtraRestSize, 
					  OldElement, OldRestSize, 
					  NewElement, NewRestSize), 
		     mixed_array_insert(NewElement, NewElements0, 
					Index, NewElements1)
		 )
	     )
	).
	    

viewable_expand_body(1, ExtraElements, [flexible|RestFixity], ExtraSize,  
		     OldElements, [XDimSize|RestSize], 
		     NewElements, [NewXDimSize|RestSize]):-
	NewXDimSize is XDimSize + 1,
	(
	    elements_array_to_ve_mixed_array(ExtraElements, ExtraSize, 
					     RestFixity, 
				   ExtraElementsStored), 
	    mixed_array_insert(ExtraElementsStored, OldElements, 
			       NewXDimSize, NewElements)
	).



% initialise location names. 
initialise_location_names(DimSizes, LocNamesList):-
	(foreach(DimSize, DimSizes), 
	 foreach(LocNames, LocNamesList)
	do
	    length(LocNames, DimSize),
	    (count(I, 1, DimSize), 
	     foreach(LocName, LocNames)
	    do
		default_location_name(I, LocName)
	    )
	).

% default location name, given location integer. 
default_location_name(Loc, LocName):-
	number_string(Loc, LocName).


% predicates for managing the set of existent viewables

create_viewable_record(ViewableName, StoredElements, DimSizes, Type, 
		       DestroyStamp, ContractStamp, InterestBookkeepingTable, 
		       LocNamesList, Struct):-
	Struct = viewable with
	[name: ViewableName, 
	 size: DimSizes, 
	 type: Type, 
	 elements: StoredElements,
	 destroy_stamp:DestroyStamp, 
	 contract_stamp:ContractStamp, 
	 interest_bookkeeping_table:InterestBookkeepingTable, 
	 location_names:LocNamesList].

retrieve_viewable(ViewableName, Viewable):-
	retrieve_all_viewables(ExistentViewables), 
	member(Viewable, ExistentViewables), 
	Viewable = viewable with name:ViewableName.

retrieve_all_viewables(AllViewables):-
	getval(existent_viewables, AllViewables).

set_all_viewables(AllViewables):-
	setval(existent_viewables, AllViewables).

remove_viewable(ViewableName, Viewable):-
	retrieve_viewable(ViewableName, Viewable),
	retrieve_all_viewables(AllViewables),
	eclipse_language:delete(Viewable, AllViewables, NewAllViewables), 
	!, 
	set_all_viewables(NewAllViewables).

store_viewable(Viewable):-
	getval(existent_viewables, ExistentViewables),
	setval(existent_viewables, [Viewable|ExistentViewables]).

% utilities
		   
% Name handling

check_name(ViewableName):-
	atom(ViewableName), 
	\+ viewable(ViewableName).

% location names check for size and type

check_location_names(DimSizes, LocNamesList):-
	length(DimSizes, Dims), 
	length(LocNamesList, Dims),
	(foreach(DimSize, DimSizes), 
	 foreach(LocNames, LocNamesList)
	do
	    length(LocNames, DimSize), 
	    (foreach(LocName, LocNames)
	    do
		string(LocName)
	    )
	).
	
% Element types

valid_element_type(any).
valid_element_type(numeric_bounds).
valid_element_type(graph_data).
valid_element_type(changeable(Solver,Type)) :-
        ground(Solver),
        valid_element_type(Type).

% if element type is 'any', no check is needed 
check_element_types(_Elements, _DimSizes, any) :- 
	!.

% if element type is 'graph_data', no check is needed 
check_element_types(_Elements, _DimSizes, graph_data) :- 
	!.

% if element type is 'changeable(Solver)', no check is needed 
check_element_types(_Elements, _DimSizes, changeable(_Solver,_Type)) :- 
	!.

% otherwise do a recursive check
check_element_types(Elements, [_Size|RestSizes], ElementType):-
	(param(RestSizes, ElementType),
	 foreacharg(Element, Elements)
	do
	    check_element_types(Element, RestSizes, ElementType)).

check_element_types(Element, [], ElementType):-
	check_element_type(Element, ElementType).

check_element_type(Element, numeric_bounds):-
	number(Element), !.

check_element_type(Element, numeric_bounds):-
	current_module(fd), 
	call(fd:is_integer_domain(Element)), !.

check_element_type(Element, numeric_bounds):-
	current_module(ic), 
	call(ic:is_solver_var(Element)), !.

check_element_type(Element, numeric_bounds):-
	current_module(range), 
	call(range:is_range(Element)), !.

	

% Writing terms to streams on failure, based on a timestamp

request_fail_write_term(StampStruct, StampArg, Stream, Term):-
	term_string(Term, 
		    String1), 
	concat_strings(String1, ".\n", String2),
	request_fail_write(StampStruct, StampArg, Stream, String2).


read_fail_stream_terms(FailStream, Terms):-
	read_string(FailStream, end_of_file, _, String),
	!, 
	split_string(String, "\n", "", SubStrings), 
	(foreach(SubString, SubStrings), 
	 fromto([], Terms0, Terms1, Terms)
	do
	    (SubString \== "" -> % an empty string is left at the end
		 (			    
		     term_string(Term, SubString),
		     Terms1 = [Term|Terms0]
		 )
	    ;
		 Terms1 = Terms0
	    )
	). 

read_fail_stream_terms(FailStream, []):-
	at_eof(FailStream).


:- comment(viewable_element_to_string/2, 
	   [amode : viewable_element_to_string(?, -),
	    args : ["ViewableElement":"Any variable term, for example a viewable element you have just extracted from a viewable.", 
		    "String": "A textual representation of the term, produced by this predicate."], 
	    summary : "Convert a variable or term to a nice string."]).

viewable_element_to_string(VE, String):-
	!,
	open(string(""), update, S),
        printf(S, "%mQPw", [VE]),
        get_stream_info(S, name, String), 
        close(S).


:- comment(viewable_element_execute/3, 
	   [amode : viewable_element_execute(++, ++, ?),
	    args : ["ViewableName":"Atom: name of an existent viewable.",
		    "ElementSpec":"Currently, this should be element(IndexList), where IndexList is a list of integers: the coordinates of the viewable element within the viewable.",
		    "Goal":"Goal to be executed, mentioning ElementSpec."], 
	    summary : "Execute a goal for a viewable element inside a viewable.",
	    desc : html("Note that this predicate throws an error if there are no current visualisation clients. The purpose of this predicate is to allow visualisation clients to call arbitrary goals which take viewable elements as input, without the visualisation clients having to retrieve the viewable elements themselves. <em>Goal</em> is the goal to be executed, but <em>Goal</em> mentions <em>ElementSpec</em> wherever it wishes to use the viewable element as input. The element specified by <em>ElementSpec</em> is retrieved from the viewable. Then all occurences of <em>ElementSpec</em> within <em>Goal</em> are replaced by the element, to give <em>NewGoal</em>. Finally, <em>NewGoal</em> is executed, and the predicate succeeds if <em>NewGoal</em> succeeds.  
<p>



"),
	    fail_if : "Fails if NewGoal fails.",
	    exceptions : [1:"There are no current visualisation clients",
			  4:"ViewableName is not an atom or string", 
			  5:"IndexList is not a ground list of integers",
			  6:"A co-ordinate in Index is out of range", 
			  8:"IndexList has the wrong number of elements"],
	    resat : yes, 
	    eg : "
An example, assumng there are visualisation clients running, would be:

[eclipse 2]: viewable_create(v1, [[X]]), 
             viewable_element_execute(v1, element([1, 1]), var(element([1, 1]))).

X = X
Yes (0.00s cpu)
[eclipse 3]: 
",
	    see_also : [viewable:viewable_create/3, viewable:viewable_size/2, 
			viewable_element_to_string/2, 
			viewable_element/3]]).


viewable_element_execute(ViewableName, element(Index), Goal):-
	viewable_element(ViewableName, Index, ElementTerm),
	replace(Goal, element(Index), ElementTerm, NewGoal), 
	call(NewGoal). 

replace(TermIn, Old, New, New):-
	TermIn == Old, 
	!.

replace(TermIn, Old, New, TermOut):-
	compound(TermIn),
	functor(TermIn, Functor, Arity), 
	functor(TermOut, Functor, Arity),
	!, 
	(foreacharg(SubArgIn, TermIn), 
	 foreacharg(SubArgOut, TermOut),
	 param(Old, New)
	do
	    replace(SubArgIn, Old, New, SubArgOut)).

replace(TermIn, _, _, TermIn). 

:- comment(viewable_changeable_execute/4,
        [amode : viewable_changeable_execute(++,++,?,++),
         args : ["ViewableName":"Atom: name of an existent viewable.",
                 "ElementSpec":"Currently, this should be element(IndexList), where IndexList is a list of integers: the coordinates of the viewable element within the viewable.",
                 "Goal":"Goal to be executed, mentioning ElementSpec.",
                 "Solver":"Solver implementing the changeable interface (eg. an eplex instance)"
                 ],
         summary : "Works as viewable_element_execute/3, but replaces the element spec with the changeable value as returned from the given Solver instead.",
         see_also:[viewable_element_execute/3]
         ]).
viewable_changeable_execute(ViewableName,element(Index),Goal,Solver):-
	viewable_element(ViewableName, Index, ElementTerm),
        Solver:get_changeable_value(ElementTerm,ChangeableTerm),
	replace(Goal, element(Index), ChangeableTerm, NewGoal), 
	call(NewGoal).







