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

/********************************************************************
 *                                                                  *
 *                          mapcolour.ecl                           *
 *   ECLiPSe side code for map colouring example:                   *
 *                                                                  *
 ********************************************************************/


% fd/ic_search library is used to provide the different variable choice and value
% selection strategies
:- lib(fd_search).

:- lib(ic_search).

:- ensure_loaded(library(ic)).

% init_map(++File, -MaxSize)
% called from the external interface. It compiles in the map data file OsFile
% and returns the number of countries this map specifies (MaxSize). This
% is the maximum number of countries this map can be used for.
init_map(OsFile, MaxSize) :-
	% OsFile's name may be in an OS dependent format
	os_file_name(File, OsFile),
	compile(File),
        % the rest is just used to find the last country
	findall(Index, country(_,Index,_,_,_,_), Cs),
	sort(0, >, Cs, [MaxSize|_]).

% get_map_data(++Size)
% call from external side to cause the data for Size number of countries
% to be sent over. At the end of sending the data, the term end is sent.
get_map_data(Size) :-
	country(_, N, X1,Y1, X2,Y2),
	N =< Size,
	write_exdr(setup_map, c(N,X1,Y1,X2,Y2)),
	fail.
get_map_data(_) :-
	write_exdr(setup_map, end),
	flush(setup_map).

% colouring(++Type, ++Select, ++Choice, ++N, -Backtracks, -Time)
%
% This is the main predicate that does the map colouring, and is called from 
% the external side. Type is the solver/method used (prolog, delay or fd),
% Select is the strategy used for variable selection, which corresponds to the
% those of fd_search's search/6; Choice is the strategy used for value choice,
% again corresponding to those available in search/6, plus rotate, the map
% colouring specific strategy whereby the choice of the next colour is a
% rotation of the choice for the previous country. N is the number of countries
% to colour for this particular call, Backtracks is the number of
% backtracks performed in the search, and Time returns the total cputime
% used.
colouring(Type, Select, Choice, N, Backtracks, Time) :-
	cputime(T0),
	colouring1(Type, Select, Choice, N, Backtracks),
	Time is cputime - T0.

colouring1(Type, Select, Choice0, N, Backtracks) :-
	% setup the demons to inform external side of colour changes
        % (1 demon per country)
	functor(Countries, countries, N),
	Countries =.. [countries|CountryList],
	setup_demons(N, Countries),

        % get the colour (numbers used so that both fd and ic solvers
        % can be used (no symbolic domain in ic)
	findall(X, number_colour(X,_), ColourList),

	% collect the relevant neighbours information  
	findall(C1-C2, (neighbour(C1,C2), C1=<N,C2=<N), Neighbours),
        % make the countries both fd and ic variables. 
	[fd,ic]: (CountryList:: ColourList),
        % setting up the rotate value choice function for search/6
	add_choicearg(Choice0, Choice, ColourList),

        % colouring both set up the constraints and perform the search.
        % this is to allow both the Prolog and constraint approaches
	do_colouring(Type, Select, Choice, Neighbours, Countries,
                     CountryList, Backtracks), 
	% ask external side if another solution is required
	read_exdr(continue, Continue),
	Continue == no, !. /* otherwise fail back and get next solution */
colouring1(_, _, _, _, _). 

        
do_colouring(prolog, Select, Choice, Neighbours, Countries, CountryList,
             BTs) :-
% Prolog generate and test: first generate without any constraints, and then
% test
	fd_search: search(CountryList, 0, Select, Choice, complete,
                          [backtrack(BTs)]),
        % just use #\= as values are ground so same effect as \=
	add_fd_constraints(fd, Neighbours, Countries). 
do_colouring(delay, Select, Choice, Neighbours, Countries, CountryList, BTs) :-
% a \= constraint is woken when both arguments are ground
	add_delay_constraints(Neighbours, Countries),
	fd_search: search(CountryList, 0, Select, Choice, complete,
                          [backtrack(BTs)]).
do_colouring(fd, Select, Choice, Neighbours, Countries, CountryList, BTs) :-
% finite domain #\= (using lib(fd))

	add_fd_constraints(fd, Neighbours, Countries),
	fd_search: search(CountryList, 0, Select, Choice, complete, 
                          [backtrack(BTs)]).
do_colouring(ic, Select, Choice, Neighbours, Countries, CountryList, BTs) :-
% finite domain #\= (using lib(ic))
	add_fd_constraints(ic, Neighbours, Countries),
	ic_search: search(CountryList, 0, Select, Choice, complete, 
                          [backtrack(BTs)]).



add_fd_constraints(Solver, Neighbours, Countries) :-
	(foreach(Pair, Neighbours), param(Countries,Solver) do
	    not_same_colour(Solver, Pair, Countries)
	).

add_delay_constraints(Neighbours, Countries) :-
	(foreach(Pair, Neighbours), param(Countries) do
	    delay_not_same_colour(Pair, Countries)
	).

rotate_assign(Country, ColourList0, ColourList1) :-
	rotate(ColourList0, ColourList1),
	member(Country, ColourList0).


rotate([E|Es], R) :-
	append(Es, [E], R).


not_same_colour(Solver, C1-C2, Countries) :-
      % get the colours for the countries C1 and C2
      arg(C1, Countries, Colour1),
      arg(C2, Countries, Colour2),
      % send constraint to either the fd or ic solver
      Solver: (Colour1 #\= Colour2).


delay_not_same_colour(C1-C2, Countries) :-
      arg(C1, Countries, Colour1),
      arg(C2, Countries, Colour2),
      suspend(delay_not_same(Colour1, Colour2), 3, [Colour2->inst]).

delay_not_same(C1, C2) :-
	(var(C1) ->
	    suspend(C1\=C2, 3, [C1->inst])
	; C1 \= C2
        ).

add_choicearg(rotate,  ChoiceFunc, Values) ?- !,
	ChoiceFunc =.. [rotate_assign,Values,_].
add_choicearg(Choice, Choice, _Values).


setup_demons(0, _) ?- !.
setup_demons(N, Cs) :-
	arg(N, Cs, C),
	suspend(inform_colour(N,C), 3, [C->inst]),
	N1 is N - 1,
	setup_demons(N1, Cs).


% inform_colour is a suspended goal (one per country) that is woken when the
% country's colour is set. A choice-point is created so that the colour can
% be `undone' by the external display when it is backtracked over. 
inform_colour(N, C0) :-
        % convert number back into a colour
	number_colour(C0, C),
	write_exdr(update_map,colour(N,C)),
	flush(update_map).
inform_colour(N, _) :-
        % undo on backtrack and set colour to the neutral darkgray
	write_exdr(update_map,colour(N,darkgray)),
	flush(update_map), fail.


% the numbers should be in order to ensure same ordering in the finite
% domain solvers as in Prolog/delay solvers
number_colour(1,green).
number_colour(2,purple).
number_colour(3,red).
number_colour(4,yellow).





