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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf and Andrew Sadler, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: generic_edge_finder3.ecl,v 1.2 2012/08/22 01:12:15 kish_shen Exp $
%
% Description:		Generic Edge-finder, cubic algorithm
%
% Author:		J.Schimpf, IC-Parc
%			A.Sadler, IC-Parc
%
% This file is intended to be used 'include'd into specialised modules
% which must declare certain key interface predicates (possibly via
% read-macros).
%
% See the file generic_design.txt for a description of the interface
% predicates.
% ----------------------------------------------------------------------

:- export disjunctive/2, disjunctive_bools/3, cumulative/4, cumulative/5.


:- comment(disjunctive/2, [
  amode:   disjunctive(+,+),
  args:    ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
            "Durations":   "Collection of duration for tasks (integer variables or integers)"
           ],
  summary: "Constrain the tasks with specified start times and durations to not overlap in time.",
  see_also: [disjunctive_bools/3, collection_to_list/2, _:disjunctive/2],
  kind: [constraint:[root:[ic,edge_finder3:fd]]],
  desc:    html("\
<P>
    A disjunctive scheduling constraint. StartTimes and Durations are
    collections (a la collection_to_list/2) of equal size N of integer
    variables or integers.  The declarative meaning is that the N tasks with
    the given start times and durations do not overlap at any point in time.
</P><P>
    Any input variables which do not already have finite bounds will be
    given default bounds of -10000000 to 10000000.
</P>")
]).

disjunctive(Starts, Durations) :-
	collection_to_list(Starts, StartsList),
	collection_to_list(Durations, DurationsList),
	!,
	disjunctive_ef(StartsList, DurationsList, 1).
disjunctive(Starts, Durations) :-
	error(5, disjunctive(Starts, Durations)).


:- comment(disjunctive_bools/3, [
  amode:   disjunctive_bools(+,+,+),
  amode:   disjunctive_bools(+,+,-),
  args:    ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
            "Durations":   "Collection of duration for tasks (integer variables or integers)",
            "OrderingBools":   "Variable, or list of ordering Booleans (variable or 0 or 1)"
           ],
  summary: "Constrain the tasks with specified start times and durations to not overlap in time.",
  see_also: [disjunctive/2, collection_to_list/2, _:disjunctive_bools/3],
  kind: [constraint:[root:[ic,edge_finder3:fd]]],
  desc:    html("\
<P>
    A disjunctive scheduling constraint. StartTimes and Durations are
    collections (a la collection_to_list/2) of equal size N of integer
    variables or integers.  The declarative meaning is that the N tasks with
    the given start times and durations do not overlap at any point in time.
</P><P>
    OrderingBools is a list of ordering Booleans. For each possible pair of
    tasks, there is one Boolean which describes the order of these two tasks.
    If the Tasks are numbered 1..N, and the Booleans are numbered
    1..N*(N-1)//2, then the Boolean corresponding to the task pair I,J
    (with I&gt;J) has the index (I-1)(I-2)//2 + J.  In other words, the
    OrderingBools list is a flattened version of a strictly lower
    triangular matrix of ordering Booleans, i.e.
    <PRE>
        I\\J|  1    2    3    4    5
        ---+--------------------------
        1  |  .    .    .    .    .
        2  | B[1]  .    .    .    .
        3  | B[2] B[3]  .    .    .
        4  | B[4] B[5] B[6]  .    .
        5  | B[7] B[8] B[9] B[10] .
    </PRE>
    The Boolean being set to 1 indicates that task I is before task J,
    a value of 0 indicates that task J is before task I. If uninstantiated,
    the order is not (yet) determined. Operationally, the constraint will
    both infer start time bounds from the setting of the Booleans, and
    infer Boolean settings from the start times and durations.
</P><P>
    The Booleans should be used for making search choices, typically by
    setting them such that a task is chosen to be first (or last) among
    a group of tasks.
</P><P>
    Any Start and Duration variables which do not already have finite bounds
    will be given default bounds of -10000000 to 10000000. The Booleans on
    the other hand can be domainless variables, and the only way in which
    the constraint will affect them is by instantiation to 0 or 1.
</P>")
]).

disjunctive_bools(Starts, Durations, Bools) :-
	collection_to_list(Starts, StartsList),
	collection_to_list(Durations, DurationsList),
	!,
	disjunctive_ef(StartsList, DurationsList, Bools, 1).
disjunctive_bools(Starts, Durations, Bools) :-
	error(5, disjunctive_bools(Starts, Durations, Bools)).


:- comment(cumulative/4, [
  amode: cumulative(+,+,+,++),
  args:  ["StartTimes":  "List of start times for tasks (integer variables or integers)",
          "Durations":   "List of duration for tasks (integer variables or integers)",
          "Resources":   "List of resource uages by tasks (integer variables or integers)",
          "ResourceLimit": "Maximum amount of resource available (integer)"
         ],
  summary: "Cumulative constraint on specified tasks.",
  see_also: [disjunctive/2, cumulative/5, collection_to_list/2, _:cumulative/4],
  kind: [constraint:[root:[ic,edge_finder3:fd]]],
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations and Resources
   are collections (a la collection_to_list/2) of equal size N of integer
   variables or integers.  ResourceLimit is an integer. The declarative
   meaning is:
   If there are N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time.
</P><P>
   Any input variables which do not already have finite bounds will be
   given default bounds of -10000000 to 10000000.
</P><P>
   This constraint can propagate more information than the implementation
   in library(ic_cumulative) and library(cumulative).
</P>")
]).

cumulative(Starts, Durations, Resources, Cap) :-
	collection_to_list(Starts, StartsList),
	collection_to_list(Durations, DurationsList),
	collection_to_list(Resources, ResourcesList),
	!,
	generic_cumulative(StartsList, DurationsList, ResourcesList, Cap),
	cumulative_ef(StartsList, DurationsList, ResourcesList, _Areas, Cap, 1).
cumulative(Starts, Durations, Resources, Cap) :-
	error(5, cumulative(Starts, Durations, Resources, Cap)).


:- comment(cumulative/5, [
  amode: cumulative(+,+,+,+,++),
  args:  ["StartTimes":  "Collection of start times for tasks (integer variables or integers)",
          "Durations":   "Collection of duration for tasks (integer variables or integers)",
          "Resources":   "Collection of resource usages by tasks (integer variables or integers)",
          "Areas":       "Collection of areas covered by tasks (integer variables or integers)",
          "ResourceLimit": "Maximum amount of resource available (integer)"
         ],
  summary: "Cumulative constraint on specified tasks.",
  see_also: [disjunctive/2, cumulative/4, collection_to_list/2, _:cumulative/5],
  kind: [constraint:[root:[ic,edge_finder3:fd]]],
  desc:    html("\
<P>
   In this variant, an area (the product of duration and resource usage of
   a task) can be specified, e.g. if duration or reource usage are not
   known in advance. The edge-finder algorithm can make use of this information
   to derive bound updates.
</P><P>
   Any input variables which do not already have finite bounds will be
   given default bounds of -10000000 to 10000000.
</P>
")
]).

cumulative(Starts, Durations, Resources, Areas, Cap) :-
	collection_to_list(Starts, StartsList),
	collection_to_list(Durations, DurationsList),
	collection_to_list(Resources, ResourcesList),
	collection_to_list(Areas, AreasList),
	!,
	generic_cumulative(StartsList, DurationsList, ResourcesList, Cap),
	cumulative_ef(StartsList, DurationsList, ResourcesList, AreasList, Cap, 1).
cumulative(Starts, Durations, Resources, Areas, Cap) :-
	error(5, cumulative(Starts, Durations, Resources, Areas, Cap)).

