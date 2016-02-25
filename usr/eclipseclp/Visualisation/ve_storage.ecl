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

% code managing storage of viewable elements.


% code for managing the array data structure

% The data structure for storing viewable elements is a _mixed array_
% each fixed dimension is implemented using an array (i.e. [](_, _, ...)),
% each flexible dimension is implemented using a log-array as provided by
% the library logarr 
%
% so, a viewable whose fixity list was [fixed, flexible, flexible] would be 
% implemented by an array of log-arrays of log-arrays.
%
% within the mixed array, each viewable element is stored using the 
% viewable_element struct


% elements_array_to_ve_mixed_array(++Elements, ++SizeList, ++FixityList, 
%                                  --MixedArray)
%
% creates a mixed array given some elements in a normal ECLiPSe array.
%
% Elements is a possibly multi-dimensional array of element terms. SizeList
% is the sizes of the dimensions of Elements. FixityList contains the 
% required fixities for elements in MixedArray. MixedArray is the mixed  
% array created by the predicate.
%
% This predicate is recursive.

% recursive case: create the top dimension according to head size/fixity,
% then recurse on each argument of Elements and insert the resulting
% sub-mixed array into the top dimension
elements_array_to_ve_mixed_array(Elements, [Size|RestSizes], 
				 [Fixity|RestFixity], 
				 MixedArrayOut):-
	mixed_array_create_dimension(Size, Fixity, MixedArrayIn), 
	(count(Index, 1, Size), 
	 fromto(MixedArrayIn, MixedArray0, 
		MixedArray1, MixedArrayOut), 
	 foreacharg(Element, Elements), 
	 param(RestSizes, RestFixity) do
	     elements_array_to_ve_mixed_array(Element, RestSizes, 
					      RestFixity, SubMixedArray), 
	     mixed_array_insert(SubMixedArray, MixedArray0, Index,
				MixedArray1)).

% base case (detected when sizes/fixities are empty) 1st arg must be
% element term, so initialise as a viewable_element
elements_array_to_ve_mixed_array(Element, [], [], ViewableElement):-
	viewable_element_initialise(Element, ViewableElement).


viewable_element_initialise(Element, ViewableElement):-
	ViewableElement = viewable_element with 
	[
	    element:Element, 
	    interest_stamp_table:InterestStampTable
	],
	% as good a place as any to initialise this hash table which must 
	% be stored with each elements. It maps InterestIDs to time-stamps.
	hash_create(InterestStampTable). 



% mixed_array_create_dimension(++Size, ++Fixity, --MixedArrayDimension
% creates a 1-dimensional mixed array according to Size and Fixity 

% if the fixity = fixed, use array for representation
mixed_array_create_dimension(Size, fixed, MixedArrayDimensions):-
	!, 
	dim(MixedArrayDimensions, [Size]).

% if the fixity = flexible, use log-array for representation
mixed_array_create_dimension(_Size, flexible, MixedArrayDimensions):-
	!, 
	logarr:new_array(MixedArrayDimensions).
	
% mixed_array_insert(?Element, +MixedArray, 
%                    ++Index, +NewMixedArray)
% inserts an Element into the top dimension of a MixedArray to give a 
% NewMixedArray. The representation used for the top dimension is deduced
% from the functor of MixedArray (log-arrays have functor array(..)).
mixed_array_insert(Element, array(Arr, N), Index, NewMixedArray):-
	logarr:aset(Index, array(Arr, N), Element, NewMixedArray), 
	!.

% if not a log-array, must be normal array. In this case, NewMixedArray is 
% MixedArray further instantiated.
mixed_array_insert(Element, MixedArray, Index, MixedArray):-
	dim(MixedArray, _),
	!, 
	arg(Index, MixedArray, Element).

% mixed_array_retrieve(+MixedArray, ++Index, -ElementOut)
%
% retrieves an element from a mixed array. Index is a list of integers, 
% specifying the location of the required element within MixedArray. If
% Index is shorter than the number of dimensions in MixedArray, then 
% ElementOut will itself be a mixed array. Otherwise, ElementOut will be
% a viewable element. This predicate is recursive. 

% Recursive case 1: top dimension of MixedArray is a log-array (identified
% by array(..) functor
mixed_array_retrieve(array(Arr, N), [HeadIndex|RestIndex], ElementOut):-
	!, 
	logarr:aref(HeadIndex, array(Arr, N), SubMixedArray), 
	mixed_array_retrieve(SubMixedArray, RestIndex, ElementOut).
	
% Recursive case 2: top dimension of MixedArray is a normal array
mixed_array_retrieve(MixedArray, [HeadIndex|RestIndex], ElementOut):-
	!, 
	arg(HeadIndex, MixedArray, ElementIn), 
	mixed_array_retrieve(ElementIn, RestIndex, ElementOut).

% Base case (identified by empty index) 
mixed_array_retrieve(MixedArray, [], MixedArray).

% for_all_elements(++Sizes, ++IndexListArgNumber, +Goal)
%
% for each combination of numbers between 1 and sizes, Goal is executed with 
% the combination list substituted for argument <IndexListArgNumber> 
%
% e.g. 
% [eclipse 36]: for_all_elements([2, 2], 1, writeln(_)).
% [1, 1]
% [1, 2]
% [2, 1]
% [2, 2]
%
% Yes (0.00s cpu)
%
% This is useful for doing something to all elements in a mixed array, by
% using this in conjunction with mixed_array_retrieve/3
%

% top-level wrapper
for_all_elements(Sizes, 
		 IndexListArgNumber, Goal):-
	for_all_elements([], Sizes, 
			 IndexListArgNumber, Goal).

% recursive case, for each number between 1 and Size, append to IndexListIn
% to give IndexListOut then recurse on IndexListOut and RestSizes.
for_all_elements(IndexListIn, [Size|RestSizes], 
		 IndexListArgNumber, Goal):-
	(count(Index, 1, Size),
	 param(IndexListIn, RestSizes, IndexListArgNumber, Goal)
	 do
	     listut:append(IndexListIn, [Index], IndexListOut),
	     for_all_elements(IndexListOut, RestSizes, 
			      IndexListArgNumber, Goal)).
 
% Base case (identified by empty sizes). Construct GoalOut from GoalIn, but 
% replace the one specified arg with IndexList. Then execute GoalOut. 
for_all_elements(IndexList, [], 
		 IndexListArgNumber, GoalIn):-
	functor(GoalIn, GIFunctor, GIArgs), 
	functor(GoalOut, GIFunctor, GIArgs), 
	(foreacharg(GoalInArg, GoalIn), 
	 count(Arg, 1, _), 
	 foreacharg(GoalOutArg, GoalOut),
	 param(IndexListArgNumber, IndexList)
	do
	     (Arg == IndexListArgNumber -> 
		  GoalOutArg = IndexList ;
		  GoalOutArg = GoalInArg)), 
	call(GoalOut).

% regular_nested_list(+Elements, ?NDimensions, ?Sizes)
%
% A regular nested list with N dimensions of sizes (S1, S2, ...., SN) is 
% A list of S1 lists of S2 lists of ..... lists of SN elements. 
%
% So for example [[a,b,c], [d,e,f]] is a regular nested list with 2 
% dimensions of size 2 and 3. It is also a regular nested list with 1 
% dimension of size 2 (counting the inner lists as elements).
%
% This predicate takes a list of lists Elements and succeeds if it is a 
% regular nested list with NDimensions dimensions with the corresponding
% sizes of the dimensions given in Sizes.
%
% However, there are some slightly subtle points about the behaviour of the 
% predicate:
%
% - with the number of dimensions initially a free variable, the predicate
% succeeds with the biggest dimension first. This helps some of the 
% higher-level predicates guess the dimensions of a new viewable.  
%
%
% Note that an empty list can technically be a regular nested list with
% any number of dimensions as long as the first dimension is of size 0. 
%
% - if Elements is [], NDimensions must be instantiated. In this case,
% Sizes is instantiated to a list of <NDimensions> zeroes.
%
%
% - if Elements is [] and NDimensions and Sizes are both fully ground, then 
% the predicate succeeds with Sizes being any list beginning with 0. 

% this predicate is recursive

% recursive case
%
% In this general case, enforce that Elements is as long as the head  
% of the size list, and then recurse on the tail for each element of Elements
regular_nested_list(Elements, NDimensions1, 
		    [Size1 | RestSizes]):-
	nonvar(Elements), 
	length(Elements, Size1), 
	Size1 > 0, % length zero cases covered by clauses 3 and 4
	(foreach(Element, Elements), 
	 param(NDimensions1, NDimensions0, RestSizes) 
	 do
	     (integer(NDimensions1) -> 
		  NDimensions0 is NDimensions1 - 1;
		  true),
	     regular_nested_list(Element, NDimensions0, RestSizes)
	), 
	NDimensions1 is NDimensions0 + 1.
	
% base case	
regular_nested_list(Elements, 1, [Size]):-
	nonvar(Elements), 
	length(Elements, Size), 
	!.

% case where Elements is empty, NDims is instantiated > 1 and Sizes is 
% not ground: Sizes is unified with a list of zeros of length NDims. 
regular_nested_list([], NDims, Sizes):-
	integer(NDims), 
	NDims > 1, % NDims = 1 case covered by clause 2
	nonground(Sizes), 
	length(Sizes, NDims),
	!,
	(foreach(0, Sizes) do true).

% case where Elements is empty, NDims is instantiated > 1 and Sizes is ground:
% We check that Sizes begins with a zero and that all other elements are 
% non-negative integers. 
regular_nested_list([], NDims, Sizes):-
	integer(NDims), 
	NDims > 1, % NDims=1 case covered by clause 2
	\+nonground(Sizes),
	Sizes = [0|Rest], 
	length(Sizes, NDims), 
	(foreach(R, Rest) do integer(R), R >= 0).

% array_from_nested_list(+NestedList, ++Sizes, ?Array)
%
% This converts a regular nested list with dimension sizes Sizes to a regular 
% array with the same dimensions and elements
% This predicate is recursive. 

% Recursive case
array_from_nested_list(List, [Size|RestSizes], Array):-
	dim(Array, [Size]), 
	(foreach(Element, List), 
	 foreacharg(ArrayElement, Array), 
	 param(RestSizes) do 
	     array_from_nested_list(Element, RestSizes, ArrayElement)).

% base case
array_from_nested_list(List, [], List).

% regular_array(+Elements, ?NDimensions, ?Sizes)
%
% A regular nested array with N dimensions of sizes (S1, S2, ...., SN) is 
% An array of S1 arrays of S2 arrays of ..... arrays of SN elements. 
%
% This predicate is the exact analogue of regular_nested_list/3, but  for 
% regular arrays
regular_array(Elements, NDimensions1, 
	      [Size1 | RestSizes]):-
	nonvar(Elements), 
	functor(Elements, [], Size1), 
	Size1 > 0, % Size zero cases covered by clauses 3 and 4
	(foreacharg(Element, Elements), 
	 param(NDimensions0, NDimensions1, RestSizes) 
	 do
	     (integer(NDimensions1) -> 
		  NDimensions0 is NDimensions1 - 1;
		  true), 
	     regular_array(Element, NDimensions0, RestSizes)
	),
	NDimensions1 is NDimensions0 + 1.
	
regular_array(Elements, 1, [Size]):-
	nonvar(Elements), 
	functor(Elements, [], Size), 
	!. 

regular_array(Elements, NDims, Sizes):-
	Elements == [], 
	integer(NDims), 
	nonground(Sizes), 
	!, 
	NDims > 1, % NDims = 1 case covered by clause 2
	length(Sizes, NDims), 
	(foreach(0, Sizes) do true).

regular_array(Elements, NDims, Sizes):-
	Elements == [], 
	integer(NDims), 
	\+ nonground(Sizes),
	NDims > 1, % NDims = 1 case covered by clause 2
	length(Sizes, NDims),
	Sizes = [0|Rest], 
	(foreach(R, Rest) do integer(R)).

