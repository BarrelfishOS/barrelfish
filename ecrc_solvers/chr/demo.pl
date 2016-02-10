:- nodbgcomp.

:- module_interface(demo).


:- begin_module(demo).

:- export point_intensity/4,
	  senders_threshold_points/3,
	  covered_rectangle/3,
	  covered_rectangles/2,
	  chr_demo/0.

:- [data].
:- [graphic].


/*** point_intensity(ListSender, Point, Sender, Intensity) ***/
point_intensity([], _, _, 0).
point_intensity([S1 | ListSender], Point, Sender, Intensity) :-
	point_intensity_from_one(S1, Point, I1),
	point_intensity(ListSender, Point, S2, I2),
	( I2 > I1
         ->
	  Sender = S2, Intensity = I2
         ;
	  Sender = S1, Intensity = I1
        ).


senders_threshold_points(ListSender, NbOfPoints, LLPoint) :-
	threshold_points(ListSender, ListSender, NbOfPoints, LLPoint).

threshold_points([], _ListSender, _NbOfPoints, []).
threshold_points([S | ListS], ListSender, NbOfPoints, [area(S, ListPoint) | LLPoint]) :-
	Increment is 2*pi / NbOfPoints,
	do_threshold_points(NbOfPoints, 0, Increment, S, ListSender, ListPoint),
	threshold_points(ListS, ListSender, NbOfPoints, LLPoint).

do_threshold_points(0, _Angle, _Increment, _Point, _ListSender, []) :- !.
do_threshold_points(N, Angle, Increment, Point, ListSender, [FarthestPoint | ListPoint]) :-
	Farthest is max_threshold_distance,
	farthest_point(Point, ListSender, Angle, 0, Farthest, FarthestPoint),
	N1 is N - 1,
	Angle1 is Angle + Increment,
	do_threshold_points(N1, Angle1, Increment, Point, ListSender, ListPoint).


farthest_point(X#Y, _ListSender, Angle, Inf, Sup, X0#Y0) :-
	Sup - Inf < 0.25, !,
	X0 is X + cos(Angle)*Sup,
	Y0 is Y + sin(Angle)*Sup.
farthest_point(X#Y, ListSender, Angle, Inf, Sup, X0#Y0) :-
	Mid is (Inf + Sup) / 2,
	X1 is X + cos(Angle)*Mid,
	Y1 is Y + sin(Angle)*Mid,
	( point_intensity(ListSender, X1#Y1, X#Y, I),
	  I >= sender_intensity * threshold
	 ->
	  farthest_point(X#Y, ListSender, Angle, Mid, Sup, X0#Y0)
         ;
	  farthest_point(X#Y, ListSender, Angle, Inf, Mid, X0#Y0)
        ).

/***
farthest_point(X#Y, ListSender, Angle, Farthest, Previous, X0#Y0) :-
	X1 is X + cos(Angle)*Farthest,
	Y1 is Y + sin(Angle)*Farthest,
	point_intensity(ListSender, X1#Y1, X#Y, I),
	I >= sender_intensity * threshold,
	!,
	( (Previous - Farthest) < 10
         ->
	  X0 = X1, Y0 = Y1
         ;
	  Farthest1 is (Farthest + Previous) / 2,
	  farthest_point(X#Y, ListSender, Angle, Farthest1, Previous, X0#Y0)
        ).
farthest_point(Point, ListSender, Angle, Farthest, Previous, FarthestPoint) :-
	Farthest1 is Farthest / 2,
	farthest_point(Point, ListSender, Angle, Farthest1, Previous, FarthestPoint).
***/


max_threshold_distance(D) :-
	D is sqrt(1/threshold).


point_intensity_from_one(Sender, Point, I) :-
	( Sender == Point
         ->
	  I is sender_intensity
         ;
	  getval(current_walls, ListWall),
	  number_of_intersections(ListWall, Sender, Point, N),
	  getval(wall_absorption, Wall_Absorption),
	  ( N = 0 /*** in order to have 0^0 = 0 ***/
           ->
	   I is sender_intensity / (max(distance(Sender, Point), 1)^2)
          ;
	   I is sender_intensity / (max(distance(Sender, Point), 1)^2) * (Wall_Absorption / 100)^N
          )
        ).

distance(X1#Y1, X2#Y2, D) :-
	D is sqrt((X1 - X2)^2 + (Y1 - Y2)^2).


number_of_intersections([], _S, _P, 0).
number_of_intersections([wall(P1, P2) | ListWall], Sender, Point, N) :-
	number_of_intersections(ListWall, Sender, Point, N1),
	( intersect(P1, P2, Sender, Point)
         ->
	  N is N1 + 1
         ;
	  N = N1
        ).


intersect(X1#Y1, X2#Y2, XS#YS, XP#YP) :-
	( X1 == X2
         ->
	  ( abs(XS - XP) > 1e-3
           ->
	    (X1 - XS) * (X1 - XP) =< 0,
	    Y is (YP - YS) / (XP - XS) * (X1 - XS) + YS,
	    (Y - Y1) *  (Y - Y2) =< 0
	   ;
	    abs(XS - X1) < 1e-3
	  )
        ; ( abs(XS - XP) < 1e-3
           ->
	    (XS - X1) * (XS - X2) =< 0,
	    Y is (Y2 - Y1) / (X2 - X1) * (XS - X1) + Y1,
	    (Y - YS) *  (Y - YP) =< 0
	  ;
	    Aa is (Y2 - Y1) / (X2 - X1),
	    Ab is (YP - YS) / (XP - XS),
	    Aa =\= Ab,
	    Ba is Y1 - Aa * X1,
	    Bb is YS - Ab * XS,
	    X is (Bb - Ba) / (Aa - Ab),
	    (X - X1) * (X - X2) =< 0,
	    (X - XS) * (X - XP) =< 0
	  )
        ).


covered_rectangles(X#Y, [rect(X1#Y1, X2#Y2) | Rectangles]) :-
	covered_rectangle(X#Y, X1#Y1, X2#Y2),
        XMid  is (X2 - X1) /2 + X1, YMid  is (Y2 - Y1) /2 + Y1,
	extend_rectangle(XMid#Y2, XMid#Y2, X#Y, [right, left, up], XUp1#YUp1, XUp2#YUp2),
	extend_rectangle(XMid#Y1, XMid#Y1, X#Y, [right, left, down], XDown1#YDown1, XDown2#YDown2),
	extend_rectangle(X1#YMid, X1#YMid, X#Y, [up, down, left], XLeft1#YLeft1, XLeft2#YLeft2),
	extend_rectangle(X2#YMid, X2#YMid, X#Y, [up, down, right], XRight1#YRight1, XRight2#YRight2),
	keep_big_rectangles([rect(XUp1#YUp1, XUp2#YUp2), rect(XDown1#YDown1, XDown2#YDown2), rect(XRight1#YRight1, XRight2#YRight2), rect(XLeft1#YLeft1, XLeft2#YLeft2)], Rectangles).


keep_big_rectangles([], []).
keep_big_rectangles([rect(X1#Y1, X2#Y2) | Rectangles], BigRectangles) :-
	( (Y2 - Y1) * (X2 - X1) > 30
         ->
	   BigRectangles = [rect(X1#Y1, X2#Y2) | BigRectangles1]
         ;
	   BigRectangles1 = BigRectangles
        ),
	keep_big_rectangles(Rectangles, BigRectangles1).


covered_rectangle(X#Y, LeftDown, RightUp) :-
	LongestSide is max_threshold_distance / sqrt(2),
	largest_square(X#Y, LongestSide, Side),
	X1 is X - Side / 2,
	Y1 is Y - Side / 2,
	X2 is X + Side / 2,
	Y2 is Y + Side / 2,
	extend_rectangle(X1#Y1, X2#Y2, X#Y, [up, right, down, left], LeftDown, RightUp).


largest_square(X#Y, LongestSide, LongestSide) :-
	corners_in_Center(X#Y, LongestSide),
	!.
largest_square(X#Y, LongestSide, Side) :-
	LongestSide1 is LongestSide * 0.9,
	largest_square(X#Y, LongestSide1, Side).


corners_in_Center(X#Y, Side) :-
	check_in((X-Side/2)#(Y-Side/2), X#Y),
	check_in((X+Side/2)#(Y-Side/2), X#Y),
	check_in((X-Side/2)#(Y+Side/2), X#Y),
	check_in((X+Side/2)#(Y+Side/2), X#Y).

	    

extend_rectangle(LeftDown, RightUp, _Center, [], LeftDown, RightUp) :- !.
extend_rectangle(LD, RU, Center, [Direction | Directions], LeftDown, RightUp) :-
	( extend_one_side(LD, RU, Center, Direction, LD1, RU1)
         ->
	  reorder_directions(Direction, Directions, Directions1),
	  extend_rectangle(LD1, RU1, Center, Directions1, LeftDown, RightUp)
        ;
	  extend_rectangle(LD, RU, Center, Directions, LeftDown, RightUp)
        ).


extend_one_side(X1#Y1, X2#Y2, Center, left, XL#Y1, X2#Y2) :-
	XL is X1 - 2,
	check_in(XL#Y1, Center),
	check_in(XL#Y2, Center),
	!.
extend_one_side(X1#Y1, X2#Y2, Center, right, X1#Y1, XR#Y2) :-
	XR is X2 + 2,
	check_in(XR#Y1, Center),
	check_in(XR#Y2, Center),
	!.
extend_one_side(X1#Y1, X2#Y2, Center, down, X1#YD, X2#Y2) :-
	YD is Y1 - 2,
	check_in(X1#YD, Center),
	check_in(X2#YD, Center),
	!.
extend_one_side(X1#Y1, X2#Y2, Center, up, X1#Y1, X2#YU) :-
	YU is Y2 + 2,
	check_in(X1#YU, Center),
	check_in(X2#YU, Center),
	!.


check_in(X#Y, Center) :-
	X1 is X, Y1 is Y,
	point_intensity([Center], X1#Y1, _, I),
	I >= sender_intensity * threshold.

	



reorder_directions(Direction, Directions, Directions1) :-
	append(Directions, [Direction], Directions1).
	
	
