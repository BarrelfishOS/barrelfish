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
% Contributor(s): Vassilis Liatsos, IC-Parc
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: generic_cumulative.ecl,v 1.3 2013/02/13 00:58:47 jschimpf Exp $
%
% This file is intended to be used 'include'd into specialised modules
% which must declare certain key interface predicates (possibly via
% read-macros).
%
% See the file generic_design.txt for a description of the interface
% predicates.
% ----------------------------------------------------------------------

%
% ECLiPSe LIBRARY MODULE		
%
% $Id: generic_cumulative.ecl,v 1.3 2013/02/13 00:58:47 jschimpf Exp $
%
% Emacs :      --*-prolog-*--
% File  :       cumulative.pl
% Author:      Vassilis Liatsos 
%
% Description
%    cumulative(StartTimes,Durations,Resources,ResourceLimit)
%    Preconditions:
%        StartTimes,Durations,Resources: lists of domain variables 
%                                        of same length
%        Resource Limit: positive integer
%   Postconditions:
%        Place tasks in a way so that ResourceLimit is not violated
%

:- export cumulative/4,profile/4.



:- comment(cumulative/4, [
  amode: cumulative(+,+,+,++),
  args:  ["StartTimes":  "Collection of start times for tasks (fd variables or integers)",
          "Durations":   "Collection of duration for tasks (fd variables or integers)",
          "Resources":   "Collection of resource uages by tasks (fd variables or integers)",
          "ResourceLimit": "Maximum amount of resource available (integer)"
         ],
  summary: "Cumulative constraint on specified tasks.",
  see_also: [profile/4, collection_to_list/2, _:cumulative/4],
  kind: [constraint:[root:[ic,cumulative:fd]]],
  desc:    html("\
<P>
   A cumulative scheduling constraint. StartTimes, Durations and Resources
   are collections (a la collection_to_list/2) of equal size N of finite
   domain variable or integers.  ResourceLimit is an integer.  The
   declarative meaning is:
   If there are N tasks, each starting at a certain start time, having
   a certain duration and consuming a certain (constant) amount of
   resource, then the sum of resource usage of all the tasks does not
   exceed ResourceLimit at any time.
   The (ic/fd)_edge_finder and (ic/fd)_edge_finder3 libraries implementation
   of this predicate can provide stronger propagations.
</P>")
]).
:- comment(profile/4, [
  amode: profile(+,+,+,?),
  args:  ["StartTimes":  "Collection of start times for tasks (fd variables or integers)",
          "Durations":   "Collection of duration for tasks (fd variables or integers)",
          "Resources":   "Collection of resource usages by tasks (fd variables or integers)",
          "Profile":     "List of resource usage at the start of each task"
         ],
  summary: "Cumulative constraint on specified tasks.",
  see_also: [cumulative/4, collection_to_list/2],
  kind: [constraint:[root:[ic,cumulative:fd]]],
  desc:    html("\
<P>
   StartTimes, Durations and Resources are collections (a la
   collection_to_list/2) of equal size N of finite domain variable or
   integers, with the same meaning as in cumulative/4.
   Profile is unified with a list of length N of finite domain variables or
   integers indicating the level of resource usage at the starting point of
   each task.
</P>")
]).

% cumulative(+StartTimes,+Durations,+Resources,ResourceLimit)
cumulative(S0,D0,R0,L):-
	collection_to_list(S0,S),
	collection_to_list(D0,D),
	collection_to_list(R0,R),
	!,
	cumulative0(S,D,R,L).
cumulative(S0,D0,R0,L):-
	error(5, cumulative(S0,D0,R0,L)).

cumulative0(S,D,R,L):-
	check_cumulative(S,D,R,N),
	SArray =.. [s|S],
	DArray =.. [s|D],
	RArray =.. [s|R],
	profile_aux(N,SArray,DArray,RArray,Profile),
	% Propagate constraints after setting up profile
	(
	    foreach(S,Profile), 
	    param(L)
	do
            S #=< L
	).

% Expect three lists of same length and a limit greater than 0
check_cumulative(S,D,R,N):-
	length(S,N),
	length(D,N),
	length(R,N),
	!.
check_cumulative(_S,_D,_R,_):-
	writeln(error,"Error in cumulative - make sure that the first three arguments are lists of equal length."),
	abort.

% profile(+StartPoints, +Durations, +ResourceUsage, -Profile)
% where Profile is the level of resource usage at each start point
profile(S0,D0,R0,Profile):-
	collection_to_list(S0,S),
	collection_to_list(D0,D),
	collection_to_list(R0,R),
	!,
	profile0(S,D,R,Profile).
profile(S0,D0,R0,Profile):-
	error(5, profile(S0,D0,R0,Profile)).

profile0(S,D,R,Profile):-
	check_cumulative(S,D,R,N), 	% Do not know level of resource to check (1 is a valid value)
	SArray =.. [s|S],
	DArray =.. [s|D],
	RArray =.. [s|R],
	% N activities
	profile_aux(N,SArray,DArray,RArray,Profile).

profile_aux(N,SArray,DArray,RArray,Profile):-
	(
	    count(I,1,N),
	    foreach(ResourceUsage,Profile),
	    param(SArray,DArray,RArray,N)
	do
            arg(I,SArray,Si),
	    arg(I,RArray,Ri),
	    (
		count(J,1,N),
		fromto(SumList,In,Out,[Ri]),
		param(Si,SArray,DArray,RArray,I)
	    do
	        (I == J ->
		    Out = In
		;
		    arg(J,SArray,Sj),
		    arg(J,DArray,Dj),
		    arg(J,RArray,Rj),
		    Bij :: 0..1,
		    overlap(Si,Sj,Dj,Bij,_),
		    In = [Bij * Rj | Out]
		)
	    ),
	    Sum #= sum(SumList),
	    ResourceUsage = Sum
	).

:- demon(overlap/5).
overlap(S1,S2,D2,Bool,Susp) :-
	nonvar(Bool),
	kill_suspension(Susp),
	(Bool == 1 -> % S1 contained
	    % prune values which prevent containment !
	    S2 + D2 #> S1,
	    S2 #=< S1
	;
	    % do not allow containment
	    disjunction_con(S1,S2,D2,_)
	).
overlap(S1,S2,D2,Bool,Susp) :-
	var(Bool),
	get_bounds(S1,MinS1,MaxS1),
	get_bounds(S2,MinS2,MaxS2),
	get_bounds(D2,MinD2,MaxD2),
	MaxE2 is MaxS2 + MaxD2,
	MinE2 is MinS2 + MinD2,
	( MaxS2 =< MinS1 ->
	    ( MaxS1 < MinE2 ->
		kill_suspension(Susp),
		Bool = 1
	    ; MaxE2 =< MinS1 ->
		kill_suspension(Susp),
		Bool = 0
	    ;
		V=v(S1,S2,D2),
		( nonvar(Susp) -> true
		; Goal = overlap(S1,S2,D2,Bool,Susp),
		  suspend(Goal, 4, [V->[min,max],Bool->inst], Susp))
	    )
	; MaxS1 < MinS2 ->
	    kill_suspension(Susp),
	    Bool = 0
	;
	    V=v(S1,S2,D2),
	    ( nonvar(Susp) -> true
	    ; Goal = overlap(S1,S2,D2,Bool,Susp),
	      suspend(Goal, 4, [V->[min,max],Bool->inst], Susp))
	).


:- demon(disjunction_con/4).
disjunction_con(S1,S2,D2,Susp):-
	get_bounds(S1,MinS1,MaxS1),
	get_bounds(S2,MinS2,MaxS2),
	get_bounds(D2,MinD2,_MaxD2),
	MinE2 is MinS2 + MinD2,
	( MaxS2 =< MinS1 ->
	    kill_suspension(Susp),
	    S2+D2 #=< S1
	; MaxS1 < MinE2 ->
	    kill_suspension(Susp),
	    S2 #> S1
	; var(Susp) ->
	    Goal = disjunction_con(S1,S2,D2,Susp),
	    suspend(Goal, 2, [v(S1,S2,D2)->min,v(S1,S2)->max], Susp)
	;
	    true
	).

/*

% Could use constructive disjunction 
% using dc(S1,S2,D2) infers most 
% instead of disjunction_con/4
% but it is very inefficient
% needs lib(propia)

dc(S1,S2,D2):-
	S2+D2 #<= S1.
dc(S1,S2,_D2):-
	S2 #> S1.
*/
