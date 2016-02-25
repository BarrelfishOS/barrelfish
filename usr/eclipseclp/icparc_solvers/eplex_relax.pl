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
% Contributor(s): Warwick Harvey and Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:       ECLiPSe Constraint Logic Programming System
% Version:      $Id: eplex_relax.pl,v 1.1 2006/09/23 01:53:27 snovello Exp $
%
% Linear relaxations of various constraints for use with eplex.
%
% (Currently just contains the convex hull relaxation of the piecewise
% constraint.)
%
% W.Harvey, IC-Parc
%
% Modified by K.Shen, IC-Parc, to work with standalone eplex. Based on
% ic_eplex_relax.ecl rather than the old eplex_relax.pl code.
%
% TODO:
%
% - Add support for infinite bounds in piecewise_linear_hull/3.  Currently,
%   if any bound is infinite, no convex hull is generated.
%
% - When the eplex module provides support for subsuming constraints,
%   exploit it by subsuming constraints from old convex hulls when they are
%   not part of the new convex hull.
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
:- module(eplex_relax).
% ----------------------------------------------------------------------

:- export piecewise_linear_hull/3.
:- export piecewise_linear_hull/4.

:- lib(eplex).

% ----------------------------------------------------------------------
% The piecewise linear (convex hull) constraint
% ----------------------------------------------------------------------

:- lib(ic).
:- lib(ic_constraints).	% For piecewise_linear_2/4 and struct(chunk/3).

%
% Computing the convex hull:
%
% The convex hull is found by computing the convex hull of all the points in
% the piecewise linear specification which are feasible with respect to the
% bounds on X and Y, as well as all the points where a current bound
% intersects a segment from the piecewise linear constraint.
%
% The actual convex hull computation is done using an approach similar to
% that used by the Graham scan algorithm for computing convex hulls.
% In this implementation, we compute and maintain the convex hull in two
% parts; the "upper" part and the "lower" part.  These are computed
% simultaneously, but are treated as separate entities.  Essentially,
% we process the points we wish to "wrap" in order from left to right,
% maintaining the upper and lower convex hulls of the points processed so
% far.  When processing a new point, some of the points on the existing
% convex hulls may fall inside the new ones (i.e. below the upper hull or
% above the lower one).  These are always the most recent points to have
% been added, and are fairly easy to identify (and thus remove) by examining
% gradients.
%
% When all points have been processed, one is left with a list of points
% which are on the convex hull.  It is then straightforward to generate
% appropriate constraints defining this hull.
%
%
% Updating the convex hull:
%
% To update the convex hull when a bound has changed, we simply recompute it
% and send any new constraints to eplex.  Note that it should be possible to
% optimise this by only recomputing the parts of the convex hull which are
% now infeasible (and thus changed).  However, the implementation seems
% complicated enough already.  :-)
%


	% Structure for holding information about the convex hull, namely
	% lists of the points on the upper and lower hulls and lists of the
	% corresponding constraints.
:- local struct(hull_info(upoints, lpoints, uconstraints, lconstraints)).

	% Structure for holding information about a constraint.  Currently
	% this is not used except for diagnostics, but when the eplex module
	% supports subsumption of constraints, this will store the
	% constraint identifier required to subsume the constraint.
:- local struct(constraint(dummy)).

	% The predicate to be called by the user.  It just calls other
	% predicates to set up the propagation-only constraint, and then add
	% the convex hull extension.

piecewise_linear_hull(X, Points, Y) :-
	piecewise_linear_hull(X, Points, Y, eplex).

piecewise_linear_hull(X, Points, Y, Pool) :-
	% Make sure X and Y have IC attributes before we try to suspend on
	% them...
	ic:reals([X, Y]),
        % lower priority than propagate_hull/6 
        suspend(ic_to_eplex_bounds(X,Pool,S), 7, [X->ic:min,X->ic:max], S),
	piecewise_linear_2(X, Points, Y, ChunkList),
	piecewise_linear_hull_2(X, Y, ChunkList, Pool).


	% Construct the initial hull info, set up the propagation demon, and
	% perform the initial propagation.
piecewise_linear_hull_2(X, Y, ChunkList, Pool) :-
	Chunks =.. [[] | ChunkList],
	HullInfo = hull_info with
			[upoints:[], lpoints:[],
			uconstraints:[], lconstraints:[]],
	suspend(propagate_hull(X, Y, Chunks, HullInfo, Pool, Susp), 6,
			[X->min, X->max, Y->min, Y->max], Susp),
	propagate_hull(X, Y, Chunks, HullInfo, Pool, Susp).


	% The demon which updates the convex hull whenever bounds change.
:- demon(propagate_hull/6).
propagate_hull(X, Y, Chunks, HullInfo, Pool, Susp) :-
	( var(X), var(Y) ->
	    get_bounds(X, XMin, XMax),
	    get_bounds(Y, YMin, YMax),
	    HullInfo = hull_info with
		    [upoints:UHullPoints, lpoints:LHullPoints,
		    uconstraints:UConstraints, lconstraints:LConstraints],

	    % Compute the new integer hull and update the constraints.
	    compute_hull_points(Chunks, XMin, XMax, YMin, YMax,
		    NewUHullPoints, NewLHullPoints),
	    %printf("Upper Hull Points:%n%DQw.%n", [NewUHullPoints]),
	    %printf("Lower Hull Points:%n%DQw.%n", [NewLHullPoints]),
	    update_hull_constraints(upper, Pool, X, Y, UHullPoints, NewUHullPoints,
		    UConstraints, NewUConstraints),
	    update_hull_constraints(lower, Pool, X, Y, LHullPoints, NewLHullPoints,
		    LConstraints, NewLConstraints),

	    % Update the description of the convex hull.
	    setarg(upoints of hull_info, HullInfo, NewUHullPoints),
	    setarg(lpoints of hull_info, HullInfo, NewLHullPoints),
	    setarg(uconstraints of hull_info, HullInfo, NewUConstraints),
	    setarg(lconstraints of hull_info, HullInfo, NewLConstraints)
	;
	    % If X or Y have become fixed, then we have nothing to add
	    % over what the piecewise_linear/2 constraint will infer.
	    kill_suspension(Susp)
	).


	% Compute the points on the convex hull.
	% Note: returns points in reverse order (right-to-left).
compute_hull_points(Chunks, XMin, XMax, YMin, YMax, UHullPoints, LHullPoints) :-
	functor(Chunks, _, NumChunks),

	( XMin > -1.0Inf, XMax < 1.0Inf ->
	    % Get the convex hull started.
	    compute_initial_hull_points(Chunks, XMin, XMax, YMin, YMax,
			    LeftChunkNum, NextPointNum,
			    UHullPoints0, LHullPoints0, Complete),

	    ( Complete = yes ->
		UHullPoints = UHullPoints0,
		LHullPoints = LHullPoints0
	    ;
		fx(Chunks, LeftChunkNum, NumChunks, XMax, YRight,
				right, RightChunkNum, RightSegNum),

		LeftChunk is Chunks[LeftChunkNum],
		LeftChunk = chunk with points:LeftPoints,
		functor(LeftPoints, _, NumLeftPoints),

		( LeftChunkNum = RightChunkNum ->
		    % Left and right points are both in the same chunk.
		    % Wrap the remaining complete segments in this chunk.
		    wrap_points(NextPointNum, RightSegNum, LeftPoints,
				    YMin, YMax, between,
				    UHullPoints0, UHullPoints3,
				    LHullPoints0, LHullPoints3)
		;
		    % Wrap the rest of this chunk.
		    wrap_points(NextPointNum, NumLeftPoints, LeftPoints,
				    YMin, YMax, between,
				    UHullPoints0, UHullPoints1,
				    LHullPoints0, LHullPoints1),

		    % Wrap the remaining complete chunks.
		    NextChunkNum is LeftChunkNum + 1,
		    LastChunkNum is RightChunkNum - 1,
		    wrap_chunks(NextChunkNum, LastChunkNum, Chunks,
				    YMin, YMax,
				    UHullPoints1, UHullPoints2,
				    LHullPoints1, LHullPoints2),

		    % Wrap the complete segments of the final chunk.
		    RightChunk is Chunks[RightChunkNum],
		    RightChunk = chunk with points:RightPoints,
		    wrap_points(1, RightSegNum, RightPoints,
				    YMin, YMax, between,
				    UHullPoints2, UHullPoints3,
				    LHullPoints2, LHullPoints3)
		),

		% Finish it off.
		compute_final_hull_points(Chunks, XMax, YRight, YMin, YMax,
				RightChunkNum, RightSegNum,
				UHullPoints3, UHullPoints,
				LHullPoints3, LHullPoints)
	    )
	;
	    % We don't support infinite domains yet
	    UHullPoints = [],
	    LHullPoints = []
	),
	%write("Upper hull points: "),
	%writeln(UHullPoints),
	%write("Lower hull points: "),
	%writeln(LHullPoints),
	true.


	% Compute the start of the convex hull (typically up to the first
	% point to the right of XMin).
compute_initial_hull_points(Chunks, XMin, XMax, YMin, YMax,
		LeftChunkNum, NextPointNum,
		UHullPoints, LHullPoints, Complete) :-
	functor(Chunks, _, NumChunks),
	fx(Chunks, 1, NumChunks, XMin, YLeft, left, LeftChunkNum, LeftSegNum),
	Chunk is Chunks[LeftChunkNum],
	Chunk = chunk with [points:Points, right:EndRight],
	functor(Points, _, NumPoints),

	( NumPoints = 1 ->
	    % Discontinuity --- simply check whether it should be
	    % included or not.
	    P0 is Points[1],
	    (X0, Y0) = P0,
	    ( breal_max(Y0) >= YMin, breal_min(Y0) =< YMax ->
		% XXX - for non-zero-width intervals, might be worth
		% trimming Y bounds to YMin/YMax if appropriate.
		UHullPoints = [P0],
		LHullPoints = [P0]
	    ;
		UHullPoints = [],
		LHullPoints = []
	    ),
	    NextPointNum = 2,
	    Complete = no
	;
	    P0 is Points[LeftSegNum],
	    P1 is Points[LeftSegNum+1],
	    (X0, Y0) = P0,
	    (X1, Y1) = P1,

	    ( ( XMax < breal_min(X1) ; EndRight = infinite, LeftSegNum =:= NumPoints-1 ) ->
		% We're all in the one segment, which is pretty boring...
		% Return some fake hull points that result in the correct
		% constraints and say we're done.
		% XXX - if the segment extends infinitely in either
		% direction, the constraints added by these fake points
		% could inadvertently tighten bounds due to roundoff.
		UHullPoints = [P1, P0],
		LHullPoints = [P1, P0],
		NextPointNum is LeftSegNum + 2,
		Complete = yes
	    ; XMin =:= breal_max(X1), LeftSegNum =:= NumPoints-1 ->
		% We're at the very last point in the segment --- simply
		% check whether it should be included or not.
		( breal_max(Y1) >= YMin, breal_min(Y1) =< YMax ->
		    % XXX - for non-zero-width intervals, might be worth
		    % trimming Y bounds to YMin/YMax if appropriate.
		    UHullPoints = [P1],
		    LHullPoints = [P1]
		;
		    UHullPoints = [],
		    LHullPoints = []
		),
		NextPointNum is LeftSegNum + 2,
		Complete = no
	    ;
		% General case

		% Add XMin to the hull - being careful that the added point
		% lies between the bounds (discontinuities can mean YLeft is
		% actually way outside YMin .. YMax).
		breal_bounds(YLeft, YLeftMin, YLeftMax),
		( YLeftMin < YMin ->
		    LHullPoints0 = [(XMin, YMin)]
		; YLeftMin > YMax ->
		    LHullPoints0 = [(XMin, YMax)]
		;   
		    LHullPoints0 = [(XMin, YLeftMin)]
		),
		( YLeftMax > YMax ->
		    UHullPoints0 = [(XMin, YMax)]
		; YLeftMax < YMin ->
		    UHullPoints0 = [(XMin, YMin)]
		;
		    UHullPoints0 = [(XMin, YLeftMax)]
		),

		% If (the upper bound of) this segment slopes upwards and
		% intersects YMax, we need to add this intersection point to
		% the upper hull.
		( YLeftMin < YMax, YMax < breal_max(Y1) ->
		    interpolate(Y0, X0, Y1, X1, YMax, Xa),
		    breal_min(Xa, XaMin),

		    ( XMin < XaMin, XaMin < breal_min(X1) ->
			wrap_point_upper((XaMin, YMax),
					UHullPoints0, UHullPoints)
		    ;
			UHullPoints = UHullPoints0
		    )
		;
		    UHullPoints = UHullPoints0
		),

		% If (the lower bound of) this segment slopes downwards and
		% intersects YMin, we need to add this intersection point to
		% the lower hull.
		( YLeftMax > YMin, YMin > breal_min(Y1) ->
		    interpolate(Y0, X0, Y1, X1, YMin, Xb),
		    breal_min(Xb, XbMin),

		    ( XMin < XbMin, XbMin < breal_min(X1) ->
			wrap_point_lower((XbMin, YMin),
					LHullPoints0, LHullPoints)
		    ;
			LHullPoints = LHullPoints0
		    )
		;
		    LHullPoints = LHullPoints0
		),

		NextPointNum is LeftSegNum + 1,
		Complete = no
	    )
	).

	% Compute the end of the convex hull (from the point before the
	% segment XMax is deemed to be in).
compute_final_hull_points(Chunks, XMax, YRight, YMin, YMax,
		RightChunkNum, RightSegNum,
		UHullPoints0, UHullPoints, LHullPoints0, LHullPoints) :-
	Chunk is Chunks[RightChunkNum],
	Chunk = chunk with points:Points,
	functor(Points, _, NumPoints),
	breal_bounds(YRight, YRightMin0, YRightMax0),
	YRightMin is min(YRightMin0, YMax),
	YRightMax is max(YRightMax0, YMin),

	( NumPoints = 1 ->
	    % Discontinuity --- simply check whether it should be
	    % included or not.
	    P0 is Points[1],
	    (X0, Y0) = P0,
	    ( breal_max(Y0) >= YMin, breal_min(Y0) =< YMax ->
		% XXX - for non-zero-width intervals, might be worth
		% trimming Y bounds to YMin/YMax if appropriate.
		wrap_point_upper(P0, UHullPoints0, UHullPoints),
		wrap_point_lower(P0, LHullPoints0, LHullPoints)
	    ;
		UHullPoints = UHullPoints0,
		LHullPoints = LHullPoints0
	    )
	;
	    P0 is Points[RightSegNum],
	    P1 is Points[RightSegNum+1],
	    (X0, Y0) = P0,
	    (X1, Y1) = P1,

	    % General case

	    % If (the lower bound of) this segment slopes upwards and
	    % intersects YMin, we need to add this intersection point to
	    % the lower hull.
	    ( breal_min(Y0) < YMin, YMin < breal_min(YRightMax) ->
		interpolate(Y0, X0, Y1, X1, YMin, Xa),
		breal_max(Xa, XaMax),

		( breal_max(X0) < XaMax, XaMax < XMax ->
		    wrap_point_lower((XaMax, YMin),
				    LHullPoints0, LHullPoints1)
		;
		    LHullPoints1 = LHullPoints0
		)
	    ;
		LHullPoints1 = LHullPoints0
	    ),

	    % If (the upper bound of) this segment slopes downwards and
	    % intersects YMax, we need to add this intersection point to
	    % the upper hull.
	    ( breal_max(Y0) > YMax, YMax > breal_max(YRightMin) ->
		interpolate(Y0, X0, Y1, X1, YMax, Xb),
		breal_max(Xb, XbMax),

		( breal_max(X0) < XbMax, XbMax < XMax ->
		    wrap_point_upper((XbMax, YMax),
				    UHullPoints0, UHullPoints1)
		;
		    UHullPoints1 = UHullPoints0
		)
	    ;
		UHullPoints1 = UHullPoints0
	    ),

	    % Add XMax to the hull
	    wrap_point_upper((XMax, YRightMax), UHullPoints1, UHullPoints),
	    wrap_point_lower((XMax, YRightMin), LHullPoints1, LHullPoints)
	).


	% Wrap all the points in the chunks from ChunkNum to MaxChunk.
wrap_chunks(ChunkNum, MaxChunk, Chunks, YMin, YMax,
		UHullPoints0, UHullPoints, LHullPoints0, LHullPoints) :-
	( ChunkNum =< MaxChunk ->
		Chunk is Chunks[ChunkNum],
		Chunk = chunk with points:Points,
		functor(Points, _, MaxPoint),
		wrap_points(1, MaxPoint, Points, YMin, YMax, between,
				UHullPoints0, UHullPoints1,
				LHullPoints0, LHullPoints1),
		ChunkNum1 is ChunkNum + 1,
		wrap_chunks(ChunkNum1, MaxChunk, Chunks, YMin, YMax,
				UHullPoints1, UHullPoints,
				LHullPoints1, LHullPoints)
	;
		UHullPoints = UHullPoints0,
		LHullPoints = LHullPoints0
	).

	% Wrap all the points from index I to index N.
	% Prev indicates whether the previous point was `above', `below' or
	% `between' the Y bounds (for detecting crossing of a bound).
	%
	% Note: any point at YMin (YMax) cannot be on the upper (lower)
	% convex hull, unless it is also at XMin or XMax (in which case it
	% is handled elsewhere).
wrap_points(I, N, Points, YMin, YMax, Prev, UHullPoints0, UHullPoints, LHullPoints0, LHullPoints) :-
	( I =< N ->
	    Point1 is Points[I],
	    Point1 = (X1, Y1),
	    ( breal_max(Y1) > YMax ->
		( I > 1, Prev \== above ->
		    (X0, Y0) is Points[I - 1],
		    ( Prev = below ->
			% Find intersection with YMin, wrap that point
			interpolate(Y0, X0, Y1, X1, YMin, X2),
			breal_max(X2, X2Max),
			wrap_point_lower((X2Max, YMin),
					LHullPoints0, LHullPoints1)
		    ;
			LHullPoints1 = LHullPoints0
		    ),
		    % Find intersection with YMax, wrap that point
		    interpolate(Y0, X0, Y1, X1, YMax, X3),
		    breal_min(X3, X3Min),
		    ( X3Min > breal_min(X0) ->
			wrap_point_upper((X3Min, YMax),
					UHullPoints0, UHullPoints1)
		    ;
			% Don't include the intersection if the fuzz puts it
			% before the endpoint of this segment.
			UHullPoints1 = UHullPoints0
		    )
		;
		    % Do nothing
		    UHullPoints1 = UHullPoints0,
		    LHullPoints1 = LHullPoints0
		),
		Curr = above
	    ; breal_min(Y1) < YMin ->
		( I > 1, Prev \== below ->
		    (X0, Y0) is Points[I - 1],
		    ( Prev = above ->
			% Find intersection with YMax, wrap that point
			interpolate(Y0, X0, Y1, X1, YMax, X2),
			breal_max(X2, X2Max),
			wrap_point_upper((X2Max, YMax),
					UHullPoints0, UHullPoints1)
		    ;
			UHullPoints1 = UHullPoints0
		    ),
		    % Find intersection with YMin, wrap that point
		    interpolate(Y0, X0, Y1, X1, YMin, X3),
		    breal_min(X3, X3Min),
		    ( X3Min > breal_min(X0) ->
			wrap_point_lower((X3Min, YMin),
					LHullPoints0, LHullPoints1)
		    ;
			% Don't include the intersection if the fuzz puts it
			% before the endpoint of this segment.
			LHullPoints1 = LHullPoints0
		    )
		;
		    % Do nothing
		    UHullPoints1 = UHullPoints0,
		    LHullPoints1 = LHullPoints0
		),
		Curr = below
	    ;
		% Point within current Y bounds
		( Prev = above ->
		    % Find intersection with YMax, wrap that point
		    (X0, Y0) is Points[I - 1],
		    interpolate(Y0, X0, Y1, X1, YMax, X2),
		    breal_max(X2, X2Max),
		    ( X2Max < breal_max(X1) ->
			wrap_point_upper((X2Max, YMax),
					UHullPoints0, UHullPoints0a)
		    ;
			% Don't include the intersection if the fuzz puts it
			% before the endpoint of this segment.
			UHullPoints0a = UHullPoints0
		    ),
		    LHullPoints0a = LHullPoints0
		; Prev = below ->
		    % Find intersection with YMin, wrap that point
		    (X0, Y0) is Points[I - 1],
		    interpolate(Y0, X0, Y1, X1, YMin, X2),
		    breal_max(X2, X2Max),
		    ( X2Max < breal_max(X1) ->
			wrap_point_lower((X2Max, YMin),
					LHullPoints0, LHullPoints0a)
		    ;
			% Don't include the intersection if the fuzz puts it
			% before the endpoint of this segment.
			LHullPoints0a = LHullPoints0
		    ),
		    UHullPoints0a = UHullPoints0
		;
		    UHullPoints0a = UHullPoints0,
		    LHullPoints0a = LHullPoints0
		),

		wrap_point_upper(Point1, UHullPoints0a, UHullPoints1),
		wrap_point_lower(Point1, LHullPoints0a, LHullPoints1),

		Curr = between
	    ),

	    I1 is I + 1,
	    wrap_points(I1, N, Points, YMin, YMax, Curr,
			UHullPoints1, UHullPoints, LHullPoints1, LHullPoints)
	;
	    UHullPoints = UHullPoints0,
	    LHullPoints = LHullPoints0
	).


	% Add a point to the upper convex hull, dropping any points which
	% would lie "inside" (under) the hull after the addition.
wrap_point_upper(P, HullPoints0, HullPoints) :-
	( HullPoints0 = [P0 | HullPoints1] ->
	    P = (X, Y),
	    Yu is breal_max(Y),
	    P0 = (X0, Y0),
	    Y0u is breal_max(Y0),
	    ( HullPoints1 = [P1 | _Tail] ->
		P1 = (X1, Y1),
		Y1u is breal_max(Y1),
		(
		    % Figure out whether P0 should be retained.
		    ( Y0u > Y1u ->
			( Yu > Y0u ->
			    % Both segments going up.
			    % Use top left corners to compare gradients.
			    % XXX - assumes min(X1) < min(X0) < min(X) -
			    % must be updated for non-zero-width intervals.
			    % (Y0 - Y1) / (X0 - X1) > (Y - Y0) / (X - X0)
			    % (Y0 - Y1) * (X - X0) > (Y - Y0) * (X0 - X1)
			    % (Y0 - Y1) * (X - X0) - (Y - Y0) * (X0 - X1) > 0
			    Y0u1 is breal(Y0u),
			    X0l1 is breal(breal_min(X0)),
			    LHS is (Y0u1 - Y1u) * (breal_min(X) - X0l1) -
				    (Yu - Y0u1) * (X0l1 - breal_min(X1)),
			    % Assume spanning zero not good enough.
			    breal_min(LHS) > 0
			;
			    % First segment going up, second going down.
			    true
			)
		    ;
			% If first segment going down, P0 can only be
			% retained if second segment going down too.
			Yu =< Y0u,
			% Use top right corners to compare gradients.
			% XXX - assumes max(X1) < max(X0) < max(X) -
			% must be updated for non-zero-width intervals.
			% (Y0 - Y1) / (X0 - X1) > (Y - Y0) / (X - X0)
			% (Y0 - Y1) * (X - X0) > (Y - Y0) * (X0 - X1)
			% (Y0 - Y1) * (X - X0) - (Y - Y0) * (X0 - X1) > 0
			Y0u1 is breal(Y0u),
			X0u1 is breal(breal_max(X0)),
			LHS is (Y0u1 - Y1u) * (breal_max(X) - X0u1) -
				(Yu - Y0u1) * (X0u1 - breal_max(X1)),
			% Assume spanning zero not good enough.
			breal_min(LHS) > 0
		    )
		->
		    % New hull segment
		    HullPoints = [P | HullPoints0]
		;
		    % P0 is (at least approximately) inside the
		    % convex hull, so drop it and recurse.
		    wrap_point_upper(P, HullPoints1, HullPoints)
		)
	    ;
		% P0---P is the initial segment: drop the lower of P0 and P
		% if this segment is vertical (the corresponding constraint
		% is implied by the X bound).
		( breal_min(X0) == breal_min(X) ->
		    ( Y0u < Yu ->
			HullPoints = [P]
		    ;
			HullPoints = HullPoints0
		    )
		;
		    HullPoints = [P | HullPoints0]
		)
	    )
	;
	    % There aren't any points on the hull yet, so just add
	    % this one.
	    HullPoints = [P]
	).

	% Add a point to the lower convex hull, dropping any points which
	% would lie "inside" (above) the hull after the addition.
wrap_point_lower(P, HullPoints0, HullPoints) :-
	( HullPoints0 = [P0 | HullPoints1] ->
	    P = (X, Y),
	    Yl is breal_min(Y),
	    P0 = (X0, Y0),
	    Y0l is breal_min(Y0),
	    ( HullPoints1 = [P1 | _Tail] ->
		P1 = (X1, Y1),
		Y1l is breal_min(Y1),
		(
		    % Figure out whether P0 should be retained.
		    ( Y0l < Y1l ->
			( Yl < Y0l ->
			    % Both segments going down.
			    % Use bottom left corners to compare gradients.
			    % XXX - assumes min(X1) < min(X0) < min(X) -
			    % must be updated for non-zero-width intervals.
			    % (Y0 - Y1) / (X0 - X1) < (Y - Y0) / (X - X0)
			    % (Y0 - Y1) * (X - X0) < (Y - Y0) * (X0 - X1)
			    % (Y0 - Y1) * (X - X0) - (Y - Y0) * (X0 - X1) < 0
			    Y0l1 is breal(Y0l),
			    X0l1 is breal(breal_min(X0)),
			    LHS is (Y0l1 - Y1l) * (breal_min(X) - X0l1) -
				    (Yl - Y0l1) * (X0l1 - breal_min(X1)),
			    % Assume spanning zero not good enough.
			    breal_max(LHS) < 0
			;
			    % First segment going down, second going up.
			    true
			)
		    ;
			% If first segment going up, P0 can only be
			% retained if second segment going up too.
			Yl >= Y0l,
			% Use bottom right corners to compare gradients.
			% XXX - assumes max(X1) < max(X0) < max(X) -
			% must be updated for non-zero-width intervals.
			% (Y0 - Y1) / (X0 - X1) < (Y - Y0) / (X - X0)
			% (Y0 - Y1) * (X - X0) < (Y - Y0) * (X0 - X1)
			% (Y0 - Y1) * (X - X0) - (Y - Y0) * (X0 - X1) < 0
			Y0l1 is breal(Y0l),
			X0u1 is breal(breal_max(X0)),
			LHS is (Y0l1 - Y1l) * (breal_max(X) - X0u1) -
				(Yl - Y0l1) * (X0u1 - breal_max(X1)),
			% Assume spanning zero not good enough.
			breal_max(LHS) < 0
		    )
		->
		    % New hull segment
		    HullPoints = [P | HullPoints0]
		;
		    % P0 is (at least approximately) inside the
		    % convex hull, so drop it and recurse.
		    wrap_point_lower(P, HullPoints1, HullPoints)
		)
	    ;
		% P0---P is the initial segment: drop the higher of P0 and P
		% if this segment is vertical (the corresponding constraint
		% is implied by the X bound).
		( breal_min(X0) == breal_min(X) ->
		    ( Y0l > Yl ->
			HullPoints = [P]
		    ;
			HullPoints = HullPoints0
		    )
		;
		    HullPoints = [P | HullPoints0]
		)
	    )
	;
	    % There aren't any points on the hull yet, so just add
	    % this one.
	    HullPoints = [P]
	).


	% Update the hull constraints, based on the new points found for the
	% convex hull.
	% UL indicates whether the `upper' or `lower' hull is being updated,
	% so that the hull-defining constraints can be given the correct
	% sense.
update_hull_constraints(UL, Pool, X, Y, OldPoints, NewPoints,
			OldConstraints, NewConstraints) :-
	% Find the first point in common between the old and the new hulls,
	% if it exists.
	( find_first_common_point(OldPoints, NewPoints, OldN, NewN,
			OldPoints1, NewPoints1) ->
		% Find the last point in common.
		find_last_common_point(OldPoints1, NewPoints1, N,
				_OldPoints2, NewHullPoints2),
		
		% Split the old constraints into those which have been
		% subsumed, and those which are in common with the new hull.
		take_drop_n(OldN, OldConstraints, SubsumedConstraints1,
				OldConstraints1),
		take_drop_n(N, OldConstraints1, CommonConstraints,
				SubsumedConstraints2),

		% Extract the points corresonding to the new constraints
		% which need to be added at the start of the hull.
		NewN1 is NewN + 1,
		take_n(NewN1, NewPoints, NewHullPoints1),

		% Subsume the constraints no longer needed.
		subsumed_constraints(SubsumedConstraints1),
		subsumed_constraints(SubsumedConstraints2),

		% Add the new constraints required.
		hull_points_to_constraints(UL, NewHullPoints1, X, Y, Pool,
				NewConstraints1),
		hull_points_to_constraints(UL, NewHullPoints2, X, Y, Pool,
				NewConstraints2),

		% Construct the new list of constraints.
		append(CommonConstraints, NewConstraints2, NewConstraints3),
		append(NewConstraints1, NewConstraints3, NewConstraints)
	;
		% No common points, so replace the hull wholesale.
		subsumed_constraints(OldConstraints),
		hull_points_to_constraints(UL, NewPoints, X, Y, Pool,
				NewConstraints)
	).

	% Returns the number of mismatching points in each list, as well the
	% tails of these lists with the mismatching points stripped off.
find_first_common_point(OldPoints, NewPoints, OldN, NewN, OldTail, NewTail) :-
	OldPoints = [OldPoint | OldPoints1],
	NewPoints = [NewPoint | NewPoints1],
	% XXX - assumes X coordinates are always bounded reals, and
	% both the lower and upper bounds are non-decreasing (as enforced by
	% unpack_point/3 and check_sorted_and_trim/2, respectively).
	compare(R, OldPoint, NewPoint),
	% Remember: points are in reverse X order.
	( R = = ->
		OldN = 0,
		NewN = 0,
		OldTail = OldPoints,
		NewTail = NewPoints
	; R = < ->
		find_first_common_point(OldPoints, NewPoints1,
				OldN, NewN0, OldTail, NewTail),
		NewN is NewN0 + 1
	;
		find_first_common_point(OldPoints1, NewPoints,
				OldN0, NewN, OldTail, NewTail),
		OldN is OldN0 + 1
	).

	% Strips off all matching points but the last from each list,
	% returning the number of points removed.
find_last_common_point(OldPoints, NewPoints, N, OldTail, NewTail) :-
	OldPoints = [OldPoint | OldPoints1],
	NewPoints = [NewPoint | NewPoints1],
	find_last_common_point_2(OldPoint, OldPoints1, NewPoint, NewPoints1, N,
			OldTail, NewTail).

find_last_common_point_2(PrevOldPoint, OldPoints, PrevNewPoint, NewPoints, N,
			OldTail, NewTail) :-
	(
		OldPoints = [OldPoint | OldPoints1],
		NewPoints = [NewPoint | NewPoints1],
		OldPoint = NewPoint
	->
		find_last_common_point_2(OldPoint, OldPoints1,
				NewPoint, NewPoints1, N0, OldTail, NewTail),
		N is N0 + 1
	;
		N = 0,
		OldTail = [PrevOldPoint | OldPoints],
		NewTail = [PrevNewPoint | NewPoints]
	).


	% Converts a list of hull points to constraints on X and Y.
hull_points_to_constraints(upper, Points, X, Y, Pool, Constraints) :-
	upper_points_to_constraints(Points, X, Y, Pool, Constraints).
hull_points_to_constraints(lower, Points, X, Y, Pool, Constraints) :-
	lower_points_to_constraints(Points, X, Y, Pool, Constraints).

upper_points_to_constraints([], _, _, _, []).	% For infinite hulls
upper_points_to_constraints([Point | Points], X, Y, Pool, Constraints) :-
	upper_points_to_constraints_2(Point, Points, X, Y, Pool, Constraints).

upper_points_to_constraints_2(_, [], _, _, _, []).
upper_points_to_constraints_2(PrevP, [P | Ps], X, Y, Pool, [C | Cs]) :-
	% Remember: points are in reverse order.
	upper_points_to_constraint(P, PrevP, X, Y, Pool, C),
	upper_points_to_constraints_2(P, Ps, X, Y, Pool, Cs).

	% Assumes X0 =< X1
upper_points_to_constraint((X0, Y0), (X1, Y1), X, Y, Pool, C) :-
	% Y =< Y0 + (Y1 - Y0) / (X1 - X0) * (X - X0)
	% (X1 - X0) * Y - (Y1 - Y0) * X =< Y0 * (X1 - X0) - X0 * (Y1 - Y0)
	Y0Max is breal_max(Y0),
	Y1Max is breal_max(Y1),
	XCoef0 is breal(Y1Max) - breal(Y0Max),
	breal_bounds(XCoef0, XCoefMin, XCoefMax),
	( XCoefMin > 0 ->
	    % Segment going up.  Use top left points.
	    XCoef = XCoef0,
	    X0a is breal_min(X0),
	    X1a is breal_min(X1)
	; XCoefMax < 0 ->
	    % Segment going down.  Use top right points.
	    XCoef = XCoef0,
	    X0a is breal_max(X0),
	    X1a is breal_max(X1)
	;
	    % Segment is horizontal (or close enough).
	    % Constraint should be implied by variable bounds, so just
	    % return something which yields a tautology.
	    XCoef = 0.0__0.0,
	    X0a = 0.0,
	    X1a = 0.0
	),
	YCoef is breal(X1a) - breal(X0a),
	RHSMax is breal_max(Y0Max * YCoef - X0a * XCoef),

	% Which bound of the X and Y coefficients to use (for safety)
	% depends on the sign of the X and Y values its going to be used
	% with...  Probably we don't have to be too paranoid about this
	% because:
	%   a) the coefficient is almost certainly a very narrow interval
	%      (it's a simple difference between two floats),
	%   b) we're passing them to an external solver which is going to
	%      use approximating arithmetic a lot anyway, and presumably
	%      includes its own safeguards,
	%   c) we get some fuzz protection from the RHS bound.
	% So for segments which span a zero coordinate (which would
	% otherwise require special processing) we just choose the bound
	% which is appropriate for the majority of the segment...
	( X0a + X1a >= 0.0 ->
	    XCoef1 is breal_max(XCoef)
	;
	    XCoef1 is breal_min(XCoef)
	),
	( Y0Max + Y1Max >= 0.0 ->
	    YCoef1 is breal_min(YCoef)
	;
	    YCoef1 is breal_max(YCoef)
	),
	Rep = (YCoef1 * Y - XCoef1 * X =< RHSMax),

	C = constraint with [dummy:Rep],
	%printf("Adding constraint: %DQw%n", [Rep]),
	add_pool_constraint(Rep, Pool).

lower_points_to_constraints([], _, _, _, []).	% For infinite hulls
lower_points_to_constraints([Point | Points], X, Y, Pool, Constraints) :-
	lower_points_to_constraints_2(Point, Points, X, Y, Pool, Constraints).

lower_points_to_constraints_2(_, [], _, _, _, []).
lower_points_to_constraints_2(PrevP, [P | Ps], X, Y, Pool, [C | Cs]) :-
	% Remember: points are in reverse order.
	lower_points_to_constraint(P, PrevP, X, Y, Pool, C),
	lower_points_to_constraints_2(P, Ps, X, Y, Pool, Cs).

	% Assumes X0 =< X1
lower_points_to_constraint((X0, Y0), (X1, Y1), X, Y, Pool, C) :-
	% Y >= Y0 + (Y1 - Y0) / (X1 - X0) * (X - X0)
	% (X1 - X0) * Y - (Y1 - Y0) * X >= Y0 * (X1 - X0) - X0 * (Y1 - Y0)
	Y0Min is breal_min(Y0),
	Y1Min is breal_min(Y1),
	XCoef0 is breal(Y1Min) - breal(Y0Min),
	breal_bounds(XCoef0, XCoefMin, XCoefMax),
	( XCoefMax < 0 ->
	    % Segment going down.  Use bottom left points.
	    XCoef = XCoef0,
	    X0a is breal_min(X0),
	    X1a is breal_min(X1)
	; XCoefMin > 0 ->
	    % Segment going up.  Use bottom right points.
	    XCoef = XCoef0,
	    X0a is breal_max(X0),
	    X1a is breal_max(X1)
	;
	    % Segment is horizontal (or close enough).
	    % Constraint should be implied by variable bounds, so just
	    % return something which yields a tautology.
	    XCoef = 0.0__0.0,
	    X0a = 0.0,
	    X1a = 0.0
	),
	YCoef is breal(X1a) - breal(X0a),
	RHSMin is breal_min(Y0Min * YCoef - X0a * XCoef),

	% Which bound of the X and Y coefficients to use (for safety)
	% depends on the sign of the X and Y values its going to be used
	% with...  Probably we don't have to be too paranoid about this
	% because:
	%   a) the coefficient is almost certainly a very narrow interval
	%      (it's a simple difference between two floats),
	%   b) we're passing them to an external solver which is going to
	%      use approximating arithmetic a lot anyway, and presumably
	%      includes its own safeguards,
	%   c) we get some fuzz protection from the RHS bound.
	% So for segments which span a zero coordinate (which would
	% otherwise require special processing) we just choose the bound
	% which is appropriate for the majority of the segment...
	( X0a + X1a >= 0.0 ->
	    XCoef1 is breal_min(XCoef)
	;
	    XCoef1 is breal_max(XCoef)
	),
	( Y0Min + Y1Min >= 0.0 ->
	    YCoef1 is breal_max(YCoef)
	;
	    YCoef1 is breal_min(YCoef)
	),
	Rep = (YCoef1 * Y - XCoef1 * X >= RHSMin),

	C = constraint with [dummy:Rep],
	%printf("Adding constraint: %DQw%n", [Rep]),
	add_pool_constraint(Rep, Pool).


	% Subsume the given list of constraints.
	% XXX - Since eplex doesn't support this yet and it's safe to leave
	% them active, for now we just do nothing.
subsumed_constraints(_SubsumedConstraints) :-
/*
	(
		foreach(Constraint, SubsumedConstraints)
	do
		Constraint = constraint with dummy:Rep,
		write("Subsuming constraint "),
		writeln(Rep)
	).
*/
	true.


	% Evaluate the piecewise linear function at the given point.
	% ChunkLo and ChunkHi give upper and lower bounds on the chunk
	% number in which to find X.  ChunkNum and SegNum return the chunk
	% and segment in which X was found, and Y is the function result.
fx(Chunks, ChunkLo, ChunkHi, X, Y, LeftRight, ChunkNum, SegNum) :-
	( ChunkLo = ChunkHi ->
	    Chunk is Chunks[ChunkLo],
	    Chunk = chunk with points:Points,
	    functor(Points, _, N),
	    N_1 is N - 1,	% Number of _segments_
	    fx(Points, 1, N_1, X, Y, LeftSegNum, RightSegNum),
	    ( LeftRight = left ->
		SegNum = LeftSegNum
	    ;
		SegNum = RightSegNum
	    ),
	    ChunkNum = ChunkLo
	;
	    % Do a binary chop.
	    ChunkMid is (ChunkLo + ChunkHi + 1) // 2,
	    Chunk is Chunks[ChunkMid],
	    Chunk = chunk with [points:Points, left:Left],
	    (Xp, _) is Points[1],
	    select_bound(LeftRight, Xp, Xp0),
	    ( go_left(Left, X, Xp0) ->
		NewChunkHi is ChunkMid - 1,
		fx(Chunks, ChunkLo, NewChunkHi, X, Y, LeftRight, ChunkNum, SegNum)
	    ;
		fx(Chunks, ChunkMid, ChunkHi, X, Y, LeftRight, ChunkNum, SegNum)
	    )
	).

    % Select the bound of Xp to use as the cut-off point, depending on
    % whether we're looking for the leftmost X or rightmost X.
select_bound(left, Xp, Xp0) :-
	Xp0 is breal_max(Xp).
select_bound(right, Xp, Xp0) :-
	Xp0 is breal_min(Xp).

    % Decide whether to go left or right in the binary chop, depending on
    % whether the discontinuity at the left end of the chunk is open or
    % closed.
go_left(open, X, Xp0) :-
	X =< Xp0.
go_left(closed, X, Xp0) :-
	X < Xp0.


take_n(N, List, Head) :-
	take_drop_n(N, List, Head, _).

take_drop_n(N, List, Head, Tail) :-
	(
		count(I, 1, N),
		fromto(List, [H | ListT], ListT, Tail),
		fromto(Head, [H | TakenT], TakenT, [])
	do
		true
	).

% ----------------------------------------------------------------------
% Bounds transfer
% ----------------------------------------------------------------------

:- demon ic_to_eplex_bounds/3.
ic_to_eplex_bounds(V, Pool, S) :-
        var(V),
        ic: get_bounds(V, Min, Max),
        Pool: (V:: Min..Max).
ic_to_eplex_bounds(V, _, S) :-
        nonvar(V),
        kill_suspension(S).
