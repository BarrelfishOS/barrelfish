%---------------------------------------------------------------------------------------
/*
 *   display of chess board with queens 
 *   whose positions are given as a list of integers
 *   like in the nqueens program
 */

show_queens([]).
show_queens(PosList) :-
	det_maximum(PosList, M),
	coordinates(PosList, CoList),
	show_board(CoList, M, M),
	opium_nl(trace).	

det_maximum(List, M) :-
	det_maximum(List, 0, M).

det_maximum([], M, M).
det_maximum([X|Xs], A, M) :-
	X > A, 
	!,
	det_maximum(Xs, X, M).
det_maximum([X|Xs], A, M) :-
	det_maximum(Xs, A, M).
	
coordinates(PosList, CoList) :-
	coordinates(PosList, 1, CoList).

coordinates([], _, []).
coordinates([P|Ps], N, [(N,P)|Cs]) :-
	N0 is N + 1,
	coordinates(Ps, N0, Cs).

vertical_line(0) :- !.
vertical_line(S) :-
	opium_write(trace, ' ---'),
	S0 is S - 1,
	vertical_line(S0).	

show_line(S, N, _) :- 
	N > S,
	!,
	opium_write(trace, '|').
show_line(S, X, X) :-
	!,
	opium_write(trace, '| x '),
	N0 is X + 1,
	show_line(S, N0, X).
show_line(S, N, X) :-
	opium_write(trace, '|   '),
	N0 is N + 1,
	show_line(S, N0, X).	

show_board(CoList, S, 0) :-
	!, 
	opium_nl(trace), write_space,
	vertical_line(S).
show_board(CoList, S, Y) :-
	opium_nl(trace), write_space, 
	vertical_line(S), 
	opium_nl(trace), write_space,
	(member((X, Y), CoList) ->
		show_line(S, 1, X)
	;
		show_line(S, 1, 0)
	),
	Y0 is Y - 1,
	show_board(CoList, S, Y0).

write_space :-
	opium_write(trace, '                ').



