:- [geons].

:- [data].

:- make_callback(quit_method/2).
:- make_callback(point_method/2).
:- make_callback(senders_method/2).
:- make_callback(walls_method/2).
:- make_callback(field_method/2).
:- make_callback(refresh_method/2).
:- make_callback(reset_method/2).
:- make_callback(threshold_method/2).
:- make_callback(approximation_method/2).
:- make_callback(grid_size_method/2).
:- make_callback(solve_method/2).
:- make_callback(solve5_method/2).
:- make_callback(abort_method/2).
:- make_callback(nb_senders_method/2).
:- make_callback(absorption_method/2).
:- make_callback(on_walls_method/2).
:- make_callback(ecrc_method/2).
:- make_callback(farm_method/2).
:- make_callback(mona_method/2).

:- dynamic rectangles /5, rectangles5 /5, window_data/4.


init_ecrc5 :- assert(rectangles5(building(20 # 12, 67 # 82),
	    grid_size(23),
	    wall_absorption(65),
	    _,
	    [[rect(47.4659462 # -28.5340538, 106.534058 # 28.5340538), rect(69.0 # 28.5340538, 109.0 # 48.5340538), rect(43.0 # -40.5340538, 89.0 # -28.5340538), rect(106.534058 # -14.0, 116.534058 # 38.0), rect(37.4659462 # -38.0, 47.4659462 # 22.0)], 
[rect(50.6074181 # 21.6074181, 107.392586 # 52.3925819), rect(55.0 # 52.3925819, 105.0 # 60.3925819), rect(61.0 # 5.60741806, 101.0 # 21.6074181), rect(107.392586 # 25.0, 111.392586 # 45.0), rect(44.6074181 # 25.0, 50.6074181 # 45.0)], 
[rect(49.4659462 # 35.4659462, 98.5340576 # 90.5340576), rect(48.0 # 90.5340576, 84.0 # 100.534058), rect(58.0 # 29.4659462, 102.0 # 35.4659462), rect(98.5340576 # 41.0, 108.534058 # 75.0), rect(41.4659462 # 49.0, 49.4659462 # 85.0)],
 [rect(39.5875854 # 58.5875854, 98.4124146 # 113.412415), rect(47.0 # 113.412415, 85.0 # 123.412415), rect(53.0 # 46.5875854, 97.0 # 58.5875854), rect(98.4124146 # 62.0, 108.412415 # 100.0), rect(29.5875854 # 74.0, 39.5875854 # 108.0)], 
[rect(21.6074181 # -27.3925819, 66.3925781 # 25.3925819), rect(36.0 # 25.3925819, 60.0 # 31.3925819), rect(34.0 # -33.3925819, 52.0 # -27.3925819), rect(66.3925781 # -15.0, 80.3925781 # 13.0), rect(7.60741806 # -17.0, 21.6074181 # 11.0)], 
[rect(32.1520081 # 18.1520081, 55.8479919 # 53.8479919), rect(32.0 # 53.8479919, 50.0 # 57.8479919), rect(38.0 # 12.1520081, 52.0 # 18.1520081), rect(55.8479919 # 24.0, 63.8479919 # 44.0), rect(22.1520081 # 28.0, 32.1520081 # 44.0)], 
[rect(29.6074181 # 44.6074181, 56.3925819 # 71.3925858), rect(35.0 # 40.6074181, 51.0 # 44.6074181), rect(56.3925819 # 50.0, 62.3925819 # 66.0)], 
[rect(22.5875854 # 60.5875854, 75.4124146 # 113.412415), rect(45.0 # 113.412415, 61.0 # 123.412415), rect(4.58758545 # 59.0, 22.5875854 # 107.0)], 
[rect(-18.8806477 # -28.8806477, 36.8806458 # 30.8806477), rect(-29.0000019 # 30.8806477, 26.9999981 # 40.8806458), rect(-3.00000191 # -40.8806458, 43.0 # -28.8806477), rect(36.8806458 # -37.0, 48.8806458 # 19.0), rect(-30.8806477 # -15.0, -18.8806477 # 39.0)],
 [rect(-11.3925829 # 15.6074181, 33.3925819 # 48.3925819), rect(-7.00000095 # 48.3925819, 19.0 # 68.3925781), rect(-3.00000095 # 5.60741806, 19.0 # 15.6074181), rect(33.3925819 # 26.0, 39.3925819 # 42.0), rect(-19.3925819 # 24.0, -11.3925829 # 46.0)], 
[rect(-13.3925829 # 42.6074181, 33.3925819 # 73.3925858), rect(-4.00000095 # 73.3925858, 32.0 # 85.3925858), rect(-4.00000095 # 28.6074181, 18.0 # 42.6074181), rect(33.3925819 # 44.0, 37.3925819 # 68.0), rect(-25.3925819 # 48.0, -13.3925829 # 68.0)], 
[rect(-20.371172 # 62.62883, 42.37117 # 121.37117), rect(-3.00000191 # 121.37117, 43.0 # 133.37117), rect(-29.0000019 # 52.62883, 26.9999981 # 62.62883), rect(42.37117 # 72.0, 56.37117 # 124.0), rect(-32.37117 # 58.0, -20.371172 # 104.0)]])).


chr_demo :-
	(open_pce -> true; true),			% Point PCE server
	send(@pce, log, off),				% Do not write pce.log

%	open_2d(0,0,1,1),
	dialog,
	send(@panel, open, point(0, 0)),
	ground_size(SizeX, SizeY),
	scale_up(SizeX, SizeXX), scale_up(SizeY, SizeYY),
%	close_2d,
	global(window_data/4),
	asserta(window_data(345,70,800,800)),
	open_2d(0, 0, SizeXX, SizeYY),
	init.


init :- setval(current_senders, []),
	send(@senders, selection, 'No'),
	setval(current_walls, []),
	send(@walls, selection, 'No'),
	setval(grid_size, 20),
	send(@grid_size, selection, 20),
	setval(nb_senders, 5),
	send(@nb_senders, selection, 5),
	setval(wall_absorption, 0),
	send(@absorption, selection, 100),
	ground_size(X, Y),
	setval(building, (0#0, X#Y)),
	retract_all(rectangles(_, _, _, _, _)),
	retract_all(rectangles5(_, _, _, _, _)),
	init_ecrc5.



refresh_method(_, _) :-
	clear_view_surface(0),
	draw_building,
	draw_senders.


reset_method(_, _) :-
	init,
	refresh_method(_,_).


senders_method(_, 'No') :-
	!,
	getval(current_senders, SenderList),
	remove_senders(SenderList),
	wprintf("Senders removed", []),
	setval(current_senders, []).
senders_method(_, Example) :-
	senders_method(_, 'No'),
	( Example == 'Choose'
         ->
	  choose_senders(SenderList)
         ;
	  senders_name(Example, SenderList),
	  draw_senders(SenderList, 1)
	),
	setval(current_senders, SenderList),
	wprintf("Senders installed", []).


walls_method(_, 'No') :-
	!,
	remove_walls,
	wprintf("Walls removed", []),
	setval(current_walls, []).
walls_method(_, Example) :-
	walls_method(_, 'No'),
	( Example == 'Choose'
         ->
	  choose_walls(WallsList),
	  setval(current_walls, WallsList),
	  ground_size(MaxX, MaxY),
	  setval(building, (0#0, MaxX#MaxY))
         ;
	  building(Example, Building, WallsList),
	  setval(building, Building),
	  setval(current_walls, WallsList),
	  draw_building
	),
	wprintf("Walls installed", []).


dialog :-
	new_dialog(@panel, "Cellular Phones", panel).

point_method(_, _) :-
	getval(current_senders, ListSenderName),
	( ListSenderName \== []
         ->
	  wprintf("Click right to quit", []),
	  inactive(@quit),
	  only_positions(ListSenderName, ListSender),
	  point_loop(ListSender),
	  wprintf("Done", []),
          active(@quit)
         ;
	  wprintf("No senders\n", [])
        ).

point_loop(ListSender) :-
	get_mouse(Button, Xf#Yf),
	( Button = 3 /*** right ***/
         -> true
         ; 
	  X is fix(Xf + 0.5),	  Y is fix(Yf + 0.5),
	  send(@mouseX, label, X),
	  send(@mouseY, label, Y),
	  ( point_intensity(ListSender, Xf#Yf, Sender, Intensity) -> true
          ; Intensity is sender_intensity
          ),
	  Intensity1 is intensity_normalize(Intensity),
	  send(@mouse_intensity, label, Intensity1),
	  getval(current_senders, ListSenderName),
	  memberchk((Sender, SenderName), ListSenderName),
	  send(@mouse_sender, label, SenderName),
	  point_loop(ListSender)
        ).


choose_senders(ListSender) :-
	  wprintf("Click right to quit", []),
	  inactive(@quit),
	  choose_senders_loop(1, ListSender),
	  wprintf("Done", []),
          active(@quit).

choose_senders_loop(Name, ListSender) :-
	get_mouse(Button, Xf#Yf),
	( Button = 3 /*** right ***/
         -> ListSender = []
         ; 
	  X is fix(Xf + 0.5),	  Y is fix(Yf + 0.5),
	  send(@mouseX, label, X),
	  send(@mouseY, label, Y),
	  scale_up(X, XX), scale_up(Y, YY),
	  text(XX, YY, Name),
	  Name1 is Name + 1,
	  ListSender = [(X#Y, Name) | ListSender1],
	  choose_senders_loop(Name1, ListSender1)
        ).


choose_walls(ListWall) :-
	  wprintf("Click right to quit", []),
	  inactive(@quit),
	  choose_walls_loop(ListWall),
	  wprintf("Done", []),
          active(@quit).

choose_walls_loop(ListWall) :-
	get_mouse(Button, X1f#Y1f, X2f#Y2f),
	( Button = 3 /*** right ***/
         -> ListWall = []
         ; 
	  ListWall = [wall(X1f#Y1f, X2f#Y2f) | ListWall1],
	  choose_walls_loop(ListWall1)
        ).


threshold_method(_, _) :-
	getval(current_senders, ListSenderName),
	( ListSenderName \== []
         ->
	  wprintf("Drawing...", []),
	  only_positions(ListSenderName, ListSender),
	  nested,
	  block((do_threshold(ListSender), wprintf("Done", [])),
	        abort_block_tag,
		wprintf("Aborted", [])),
	  inactive(@abort)
         ;
	  wprintf("No senders\n", [])
        ).

do_threshold(ListSender) :-
	color_areas(LL, 1),
	senders_threshold_points(ListSender, 72, LL),
	!.

approximation_method(_, _) :-
	wprintf("(1) Click Left, (5) Click Middle", []),
	get_mouse(Button, Xf#Yf),
	X is fix(Xf + 0.5),	  Y is fix(Yf + 0.5),
	send(@mouseX, label, X),
	send(@mouseY, label, Y),
	color_areas(LL, 4),
	senders_threshold_points([Xf#Yf], 72, LL),
	interior_style(0, 1),
	perimeter_color(1),
	perimeter_width(3),
	perimeter_type(2),
	( Button = 1 %%% Left
         ->
	  covered_rectangle(Xf#Yf, P1, P2),	 
	  draw_rectangle(P1, P2)
         ;
	  covered_rectangles(Xf#Yf, ListRect),
	  l_draw_rectangle(ListRect)
        ),
	wprintf("Done", []).

l_draw_rectangle([]).
l_draw_rectangle([rect(X1#Y1, X2#Y2) | Rectangles]) :-
	draw_rectangle(X1#Y1, X2#Y2),
	l_draw_rectangle(Rectangles).

draw_rectangle(X1f#Y1f, X2f#Y2f) :-
	X1 is fix(X1f + 0.5),	X2 is fix(X2f + 0.5),
	Y1 is fix(Y1f + 0.5),	Y2 is fix(Y2f + 0.5),
	perimeter_width(2),
	scaled_rectangle(X1, Y1, X2, Y2).
	




quit_method(_, _) :-
	send(@panel, destroy),
	call(get_flag(window_data/4, definition_module, demo), kegi),
	local(window_data/4),
	close_2d.

grid_size_method(_, X) :-
	setval(grid_size, X).
	
nb_senders_method(_, X) :-
	setval(nb_senders, X).


absorption_method(_, X) :-
	Y is 100 - X,
	setval(wall_absorption, Y).

on_walls_method(_, 'Anywhere') :-
	!,
	send(@on_walls, label, 'On Walls').
on_walls_method(_, 'On Walls') :-
	!,
	send(@on_walls, label, 'Anywhere').



solve_method(_, _) :-
	nested,
	block(solve_block, abort_block_tag, wprintf("Aborted", [])),
	inactive(@abort).

solve_block :-
	wprintf("Running", []),
	compute_rectangles(ListRectangle),
	getval(nb_senders, NS),
	make_senders(NS, ListSenderName),
	setval(nb_backtracks, 0),
	send(@backtracks, label, 0),
	( solution(ListSenderName, ListRectangle)
          ->
	  remove_useless_senders(ListSenderName, ListSenderName1, N),
	  senders_method(_, 'No'),
	  setval(current_senders, ListSenderName1),
	  draw_senders,
	  wprintf("Solution Found: %d Senders", [N])
        ;
	  wprintf("No Solution", [])
        ).



compute_rectangles(ListRectangle) :-
	getval(building, (X1#Y1, X2#Y2)),
	getval(grid_size, GS),
	getval(wall_absorption, WA),
	getval(current_walls, Walls),
	rectangles(building(X1#Y1, X2#Y2), grid_size(GS), wall_absorption(WA), walls(Walls), ListRectangle),
	!,
	wprintf("Rectangles already computed", []).
compute_rectangles(ListRectangle) :-
        getval(building, (X1#Y1, X2#Y2)),
	getval(grid_size, GS),
	appr_foreachX(X1, X2, GS, Y1, Y2, []-ListRectangle),
	getval(wall_absorption, WA),
	getval(current_walls, Walls),
	assert(rectangles(building(X1#Y1, X2#Y2), grid_size(GS), wall_absorption(WA), walls(Walls), ListRectangle)),
	wprintf("Rectangles Computed", []).


compute_5rectangles(LLRectangle) :-
	getval(building, (X1#Y1, X2#Y2)),
	getval(grid_size, GS),
	getval(wall_absorption, WA),
	getval(current_walls, Walls),
	rectangles5(building(X1#Y1, X2#Y2), grid_size(GS), wall_absorption(WA), walls(Walls), LLRectangle),
	!,
	wprintf("Rectangles already computed", []).
compute_5rectangles(LLRectangle) :-
        getval(building, (X1#Y1, X2#Y2)),
	getval(grid_size, GS),
	appr5_foreachX(X1, X2, GS, Y1, Y2, []-LLRectangle),
	getval(wall_absorption, WA),
	getval(current_walls, Walls),
	assert(rectangles5(building(X1#Y1, X2#Y2), grid_size(GS), wall_absorption(WA), walls(Walls), LLRectangle)),
	wprintf("Rectangles Computed", []).



solve5_method(_, _) :-
	nested,
	block(solve5_block, abort_block_tag, wprintf("Aborted", [])),
	inactive(@abort).

solve5_block :-
	wprintf("Running", []),
	compute_5rectangles(LLRectangle),
	getval(nb_senders, NS),
	make_senders(NS, ListSenderName),
	setval(nb_backtracks, 0),
	send(@backtracks, label, 0),
	( solution5(ListSenderName, LLRectangle)
          ->
	  remove_useless_senders(ListSenderName, ListSenderName1, N),
	  senders_method(_, 'No'),
	  setval(current_senders, ListSenderName1),
	  draw_senders,
	  wprintf("Solution Found: %d Senders", [N])
        ;
	  wprintf("No Solution", [])
        ).

abort_method(_, _) :-
	exit_block(abort_block_tag).

remove_useless_senders([], [], 0).
remove_useless_senders([(X#Y, Name) | ListSender], [(X#Y, Name) | ListSender1], N) :-
	atomic(X),
	remove_useless_senders(ListSender, ListSender1, N1),
	N is N1 + 1.
remove_useless_senders([(_X#_Y, _) | ListSender], ListSender1, N) :-
	remove_useless_senders(ListSender, ListSender1, N).



make_senders(0, []) :- !.
make_senders(N, [(X#Y, N) | ListSender]) :-
	N1 is N - 1,
	make_senders(N1, ListSender).


solution(ListSenderName, ListRectangle) :-
	choose_senders(ListRectangle, [], ListSenderName),
	chr_labeling.


solution5(ListSenderName, LLRectangle) :-
	choose_senders5(LLRectangle, [], ListSenderName),
	chr_labeling.


choose_senders([], [], _) :- !.
choose_senders([], [rect(P1, P2) | Rectangles], ListSenderName) :-
	one_sender_in_the_rectangle(ListSenderName, P1, P2),
	choose_senders([], Rectangles, ListSenderName).
choose_senders([rect(P1, P2) | Rectangles], OtherRectangles, ListSenderName) :-
	( new_sender_in_the_rectangle(ListSenderName, P1, P2)
         ->
	  choose_senders(Rectangles, OtherRectangles, ListSenderName)
         ;
	  choose_senders(Rectangles, [rect(P1, P2) |  OtherRectangles], ListSenderName)
        ).


choose_senders5([], [], _).
choose_senders5([], [LRectangle | LLRectangle], ListSenderName) :-
	one_sender_in_one_rectangle(ListSenderName, LRectangle),
	choose_senders5([], LLRectangle, ListSenderName).
choose_senders5([LRectangle | LLRectangle], OtherLLRect, ListSenderName) :-
	( new_sender_in_one_rectangle(ListSenderName, LRectangle)
         ->
	  choose_senders5(LLRectangle, OtherLLRect, ListSenderName)
         ;
	  choose_senders5(LLRectangle, [LRectangle | OtherLLRect], ListSenderName)
        ).


put_sender(Sender) :-
	get(@on_walls, label, 'Anywhere'),
	!,
	getval(building, (P1, P2)),
	geon(Sender, P1, P2).
put_sender(Sender) :-
	getval(current_walls, CW),
	approx_walls(CW, ACW),
	geons(Sender, ACW).


one_sender_in_the_rectangle([(X#Y, _Name) | _Senders], X1#Y1, X2#Y2) :-
	( (atomic(X) ; constrained(X)) -> true
        ; !,
          put_sender(X#Y)
        ),
	geon(X#Y, X1#Y1, X2#Y2).
one_sender_in_the_rectangle(_, _, _) :-
	incval(nb_backtracks),
	getval(nb_backtracks, N),
	( N mod 100 =:= 0 -> send(@backtracks, label, N) ; true),
	fail.
one_sender_in_the_rectangle([_ | Senders], P1, P2) :-
	one_sender_in_the_rectangle(Senders, P1, P2).


new_sender_in_the_rectangle([(X#Y, _Name) | _Senders], X1#Y1, X2#Y2) :-
	( (atomic(X) ; constrained(X))
         -> OldSender = true
        ; 
	  put_sender(X#Y)
        ),
	geon(X#Y, X1#Y1, X2#Y2),
	!,
	var(OldSender).
new_sender_in_the_rectangle([_ | Senders], P1, P2) :-
	new_sender_in_the_rectangle(Senders, P1, P2).


one_sender_in_one_rectangle([(X#Y, _Name) | _Senders], LRectangle) :-
	( constrained(X) -> true
        ; !,
	  put_sender(X#Y)
        ),
	sender_in_one_rectangle(X#Y, LRectangle).
one_sender_in_one_rectangle(_, _) :-
	incval(nb_backtracks),
	getval(nb_backtracks, N),
	( N mod 100 =:= 0 -> send(@backtracks, label, N) ; true),
	fail.
one_sender_in_one_rectangle([_ | Senders], LRectangle) :-
	one_sender_in_one_rectangle(Senders, LRectangle).


new_sender_in_one_rectangle([(X#Y, _Name) | _Senders], LRectangle) :-
	( (atomic(X) ; constrained(X))
         ->
	  sender_in_one_rectangle(X#Y, LRectangle),
	  !,
	  fail
        ; !,
	  put_sender(X#Y),
	  sender_in_one_rectangle(X#Y, LRectangle)
        ).
new_sender_in_one_rectangle([_ | Senders], LRectangle) :-
	new_sender_in_one_rectangle(Senders, LRectangle).

/***
sender_in_one_rectangle(X#Y, [rect(X1#Y1, X2#Y2) | _LRectangle]) :-
	geon(X#Y, X1#Y1, X2#Y2).
sender_in_one_rectangle(X#Y, [_ | LRectangle]) :-
	sender_in_one_rectangle(X#Y, LRectangle).
***/
sender_in_one_rectangle(Point, LRect) :-
	geons(Point, LRect).



appr_foreachX(X, XMax, _Incr, _YMin, _YMax, LR-LR) :-
	X > XMax, !.
appr_foreachX(X, XMax, Incr, YMin, YMax, LR1-LR0) :-
	appr_foreachY(YMin, YMax, Incr, X, LR1-LR2),
	NX is X + Incr,
	appr_foreachX(NX, XMax, Incr, YMin, YMax, LR2-LR0).


appr_foreachY(Y, YMax, _, _, LR-LR) :-
	Y > YMax, !.
appr_foreachY(Y, YMax, Incr, X, LR1-[rect(X1f#Y1f, X2f#Y2f) | LR0]) :-
	covered_rectangle(X#Y, X1f#Y1f, X2f#Y2f),
	perimeter_color(10),
/***/	scaled_circle(X, Y, 1),
/***   draw_rectangle(X1#Y1, X2#Y2), ***/
	NY is Y + Incr,
	appr_foreachY(NY, YMax, Incr, X, LR1-LR0).
	


appr5_foreachX(X, XMax, _Incr, _YMin, _YMax, LR-LR) :-
	X > XMax, !.
appr5_foreachX(X, XMax, Incr, YMin, YMax, LR1-LR0) :-
	appr5_foreachY(YMin, YMax, Incr, X, LR1-LR2),
	NX is X + Incr,
	appr5_foreachX(NX, XMax, Incr, YMin, YMax, LR2-LR0).


appr5_foreachY(Y, YMax, _, _, LR-LR) :-
	Y > YMax, !.
appr5_foreachY(Y, YMax, Incr, X, LR1-[Rectangles | LR0]) :-
	covered_rectangles(X#Y, Rectangles),
	perimeter_color(10),
/***/	scaled_circle(X, Y, 1),
	NY is Y + Incr,
	appr5_foreachY(NY, YMax, Incr, X, LR1-LR0).


l_fix_rectangles([], []).
l_fix_rectangles([rect(X1f#Y1f, X2f#Y2f) | Rectanglesf], [rect(X1#Y1, X2#Y2) | Rectangles]) :-
	X1 is fix(X1f + 0.5),	X2 is fix(X2f + 0.5),
	Y1 is fix(Y1f + 0.5),	Y2 is fix(Y2f + 0.5),
	l_fix_rectangles(Rectanglesf, Rectangles).
	


draw_building :-
	getval(building, (UpLeftX#UpLeftY, DownRightX#DownRightY)),
	interior_style(0, 1),
	perimeter_color(1),
	perimeter_width(4),
	perimeter_type(0),
	scaled_rectangle(UpLeftX, UpLeftY, DownRightX, DownRightY),
	draw_walls.


draw_senders :-
	getval(current_senders, ListSender),
/*** 	character_height(50), not supported with X11 interface ***/
	draw_senders(ListSender, 1).


draw_senders([], _).
draw_senders([(X#Y, Name) | SenderList], ColorName) :-
	scale_up(X, XX), scale_up(Y, YY),
	interior_style(1, 0),
	pick_color(ColorName, Color),
	fill_color(Color),
	perimeter_color(2),
	perimeter_type(0),
	perimeter_width(2),
	XX1 is XX + 1, YY1 is YY + 2,
	scale_up(1.5, RR),
	circle(XX1, YY1, RR),
	text(XX, YY, Name),
	next_color(ColorName, ColorName1),
	draw_senders(SenderList, ColorName1).


remove_senders(SenderList) :-
	text_color(0),
	draw_senders(SenderList, 0),
	text_color(1).


get_mouse(Button, X#Y) :-
	mouse(Button, XX, YY),
	scale_up(X, XX), scale_up(Y, YY).

get_mouse(Button, X1#Y1, X2#Y2) :-
	mouse1(2, Button, XX1, YY1, XX2, YY2),
	line_color(1),
	line(XX1, YY1, XX2, YY2),
	scale_up(X1, XX1), scale_up(Y1, YY1),
	scale_up(X2, XX2), scale_up(Y2, YY2).


draw_walls :-
	getval(current_walls, WallList),
	line_color(1),
	line_width(1),	
	do_draw_walls(WallList).

remove_walls :-
	getval(current_walls, WallList),
	line_color(0),
	line_width(1),	
	do_draw_walls(WallList).

do_draw_walls([]).
do_draw_walls([wall(X1f#Y1f, X2f#Y2f) | WallList]) :-
	X1 is fix(X1f + 0.5), Y1 is fix(Y1f + 0.5),
	X2 is fix(X2f + 0.5), Y2 is fix(Y2f + 0.5),
	scaled_line(X1, Y1, X2, Y2),
	do_draw_walls(WallList).


scale(5).

scale_up(X, XX) :-
	var(XX), !,
	XX is fix(X * scale + 0.5).
scale_up(X, XX) :-
	var(X), !,
	X is XX / scale.

scaled_rectangle(X1, Y1, X2, Y2) :-
	scale_up(X1, XX1),
	scale_up(X2, XX2),
	scale_up(Y1, YY1),
	scale_up(Y2, YY2),
	rectangle(XX1, YY1, XX2, YY2).

scaled_circle(X, Y, R) :-
	scale_up(X, XX),
	scale_up(Y, YY),
	scale_up(R, RR),
	perimeter_width(10),
	fill_color(10),
	interior_style(1, 1),
/***	perimeter_type(0), ***/
	circle(XX, YY, RR).
	

scaled_line(X1, Y1, X2, Y2) :-
	scale_up(X1, XX1),
	scale_up(X2, XX2),
	scale_up(Y1, YY1),
	scale_up(Y2, YY2),
	line(XX1, YY1, XX2, YY2).

panel(@quit, button("Quit", quit_method), append, []).
panel(@abort, button("Abort", abort_method), right, [active:off, greyed:on]).
panel(@refresh, button("Refresh", refresh_method), right, []).
panel(@reset, button("Reset", reset_method), right, []).
panel(@message_label, label(x, "                          "), right, []).
panel(@senders, menu("Senders", cycle, senders_method, ['No', 'Choose' | L]), below, []) :-
	findall(Example, senders(Example, _), L).
panel(@walls, menu("Building", cycle, walls_method, ['No', 'Choose' | L]), right, []) :-
	findall(Example, building(Example, _, _), L).

panel(@absorption, slider("Wall Absorption", 0, 100, 75, absorption_method), below, []).
panel(@point, button("Point", point_method), below, []).
panel(@threshold, button("Threshold", threshold_method), right, []).
panel(@approximation, button("Approximation", approximation_method), right, []).
panel(@mouseX_label, label(x, "X: "), below, []).
panel(@mouseX, label(x, "     "), right, []).
panel(@mouseY_label, label(x, "Y: "), right, []).
panel(@mouseY, label(x, "     "), right, []).
panel(@mouse_intensity_label, label(x, "Intensity: "), right, []).
panel(@mouse_intensity, label(x, "     "), right, []).
panel(@mouse_sender_label, label(x, "Sender: "), right, []).
panel(@mouse_sender, label(x, ""), right, []).
panel(@grid_size, slider("Grid Size", 1, 40, 20, grid_size_method), below, []).
panel(@solve, button("Solve (1)", solve_method), below, []).
panel(@solve5, button("Solve (5)", solve5_method), right, []).
panel(@on_walls, button("Anywhere", on_walls_method), right, []).
panel(@backtracks_label, label(x, "Backtracks: "), right, []).
panel(@backtracks, label(x, ""), right, []).
panel(@nb_senders, slider("At most Senders", 1, 10, 5, nb_senders_method), below, []).


panel(@mona, button("Mona", mona_method), below, []).
panel(@ecrc, button("ECRC", ecrc_method), right, []).
panel(@farm, button("Farm", farm_method), right, []).


wprintf(Format,Args) :-
  open(_,string,s),		% Make output string
  printf(s,Format,Args),
  current_stream(S,_,s),
  close(s),

  send(@message_label,label,S).

active(Object) :-
  send(Object,active,on),
  send(Object,greyed,off).

inactive(Object) :-
  send(Object,active,off),
  send(Object,greyed,on).


only_positions([], []).
only_positions([(Pos, _Name) | L], [Pos | L1]) :-
	only_positions(L, L1).


senders(Example, ListSender1) :-
	senders_name(Example, ListSender),
	only_positions(ListSender, ListSender1).


intensity_normalize(Intensity, I) :-
	I is min(fix(Intensity + 0.5), sender_intensity).


delay color_areas(LA, _Color) if var(LA).
color_areas([], _) :-
	set_gc(xFunction(xCopy)),
	draw_senders,
	draw_building.
color_areas([area(X#Y, LPoint) | Areas], ColorName) :-
	pick_color(ColorName, Color),
	fill_color(Color),
	interior_style(3, 1),
	perimeter_type(0),
	perimeter_color(Color),
	scale_up(X, XX),	scale_up(Y, YY),
	link_points(XX#YY, LPoint),
	next_color(ColorName, ColorName1),
	color_areas(Areas, ColorName1).
	

delay l_link_points(LL) if var(LL).
l_link_points([]).
l_link_points([ListPoint | LLPoint]) :-
	link_points(ListPoint),
	l_link_points(LLPoint).

delay link_points(_, L) if var(L).
link_points(_, []).
link_points(X#Y, [First | ListPoint]) :-
	do_link_points(X#Y, First, ListPoint, First).

delay do_link_points(_X#_Y, _, L, _) if var(L).
delay do_link_points(_X#_Y, _, [P | _], _) if nonground(P).
do_link_points(X#Y, X1#Y1, [], X2#Y2) :- !,
	scale_up(X1, XX1), scale_up(Y1, YY1),
	scale_up(X2, XX2), scale_up(Y2, YY2),
	set_gc(xFunction(xOr)),
	polygon([X, Y, XX1, YY1, XX2, YY2]).
do_link_points(X#Y, X1#Y1, [X2#Y2 | Points], Last) :-
	scale_up(X1, XX1), scale_up(Y1, YY1),
	scale_up(X2, XX2), scale_up(Y2, YY2),
	set_gc(xFunction(xOr)),
	polygon([X, Y, XX1, YY1, XX2, YY2]),
	do_link_points(X#Y, X2#Y2, Points, Last).


nested :-
  active(@abort),	% Unblock the control panel
  reset_pce.			% Unblock PCE's output to Sepia

not_nested :-
  send(@panel, active, off).	% Block the control panel


next_color(0, 0) :- !.
next_color(C, C1) :-
	C1 is C + 1.


pick_color(0, 0).
pick_color(1, 10).
pick_color(2, 20).
pick_color(3, 30).
pick_color(4, 60).
pick_color(5, 70).
pick_color(6, 14).
pick_color(7, 23).
pick_color(8, 10).
pick_color(9, 20).
pick_color(10, 30).

ecrc_method(_, _) :-
	send(@senders, selection, 'No'),
	senders_method(_, 'No'),
	refresh_method(_,_),
	send(@walls, selection, 'ECRC'),
	walls_method(_, 'ECRC'),
	send(@absorption, selection, 35),
	absorption_method(_, 35),
	send(@grid_size, selection, 23),
	grid_size_method(_, 23),
	send(@nb_senders, selection, 5),
	nb_senders_method(_, 5),
	send(@on_walls, label, 'On Walls'),

	load_bitmap_file("ecrc.xwd"),
	pixel_array(50, 500),
	erase(bitmap, _).

farm_method(_, _) :-
	send(@senders, selection, 'No'),
	senders_method(_, 'No'),
	refresh_method(_,_),
	send(@walls, selection, 'Farm'),
	walls_method(_, 'Farm'),
	send(@absorption, selection, 100),
	absorption_method(_, 100),
	send(@grid_size, selection, 22),
	grid_size_method(_, 22),
	send(@nb_senders, selection, 3),
	nb_senders_method(_, 3),
	send(@on_walls, label, 'Anywhere'),

	load_bitmap_file("farm.xwd"),
	pixel_array(50, 500),
	erase(bitmap, _).
mona_method(_, _) :-
	send(@senders, selection, 'No'),
	senders_method(_, 'No'),
	refresh_method(_,_),
	send(@walls, selection, 'Mona'),
	walls_method(_, 'Mona'),
	send(@absorption, selection, 50),
	absorption_method(_, 50),
	send(@grid_size, selection, 17),
	grid_size_method(_, 17),
	send(@nb_senders, selection, 4),
	nb_senders_method(_, 4),
	send(@on_walls, label, 'Anywhere'),

	load_bitmap_file("mona.xwd"),
	set_gc(xForeground(222)),
	set_gc(xBackground(0)),
	pixel_array(50, 420),
	erase(bitmap, _).

:- getcwd(Cwd),
   compile_term([(
load_bitmap_file(File) :-
	concat_strings(Cwd, File, FullPath),
	load_bitmap(FullPath)
    )]).

set_gc(_).
/*
set_gc(Val) :-
	call((
	    window_fact(_, C, _, _, _, _, _, _, _, _, _, _,
		Gline, Gsolid, Gfill, _, _, Gbitmap),
	   xSetGC(C, Gbitmap, [Val]),
	   xSetGC(C, Gline, [Val]),
	   xSetGC(C, Gsolid, [Val]),
	   xSetGC(C, Gfill, [Val])
	), kegi).

	0 --> b
	1 --> 1
	0000	0001
	bbbb	0001
*/
