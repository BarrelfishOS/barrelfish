% Lewis Carroll's Zebra (alias five houses) puzzle.


/* Problem formulation --------------------------------------------
"Five men with different nationalities live in the first five houses
of a street.  They practise five distinct professions, and each of
them has a favourite animal and a favourite drink, all of them
different.  The five houses are painted in different colours."

The Englishman lives in a red house.
The Spaniard owns a dog.
The Japanese is a painter.
The Italian drinks tea.
The Norwegian lives in the first house on the left.
The owner of the green house drinks coffee.
The green house is on the right of the white one.
The sculptor breeds snails.
The diplomat lives in the yellow house.
Milk is drunk in the middle house.
The Norwegian's house is next to the blue one.
The violinist drinks fruit juice.
The fox is in a house next to that of the doctor.
The horse is in a house next to that of the diplomat.


Q.  Who owns a Zebra, and who drinks water?

----------------------------------------------------------- */


/* Solution and performance

Only solution:

Houses = 
[
house(house1,  norwegian,   yellow,   diplomat,   fox,      water),
house(house2,  italian,     blue,     doctor,     horse,    tea  ),
house(house3,  englishman,  red,      sculptor,   snails,   milk ),
house(house4,  spaniard,    white,    violinist,  dog,      juice),
house(house4,  japanese,    green,    painter,    zebra,    coffee)]


Performance:

Prolog, same order as the program (below):              380  secs.
Prolog, optimal ordering                                  6  secs
Prolog, with delay declarations                           0.8secs

------------------------------------------------------------------*/

:- module(zebra).

% To run this program without Propia declare
%:- op(100,xfx,infers).
% Add a delay declaration here if you like.
% delay (X infers Y) if nonground(X).
%(X infers delay) :- call(X). 
:- use_module(library(propia)).

:- export(zebra/2).

% Problem representation ----------------------------------------

zebra(Houses,Language) :-
	Houses =
	[ house(house1,_,_,_,_,_),
	  house(house2,_,_,_,_,_),
	  house(house3,_,_,_,_,_),
	  house(house4,_,_,_,_,_),
	  house(house5,_,_,_,_,_)
	],

	Constraints =
	[
	house(     _,englishman,     red,         _,        _,      _ ),
	house(     _,  spaniard,       _,         _,      dog,      _ ),
	house(     _,  japanese,       _,   painter,        _,      _ ),
	house(     _,   italian,       _,         _,        _,    tea ),
	house(house1, norwegian,       _,         _,        _,      _ ),
	house(     _,         _,   green,         _,        _, coffee ),
	house(  GreH,         _,   green,         _,        _,      _ ),
	house(  WhiH,         _,   white,         _,        _,      _ ),
	house(     _,         _,       _,  sculptor,   snails,      _ ),
	house(     _,         _,  yellow,  diplomat,        _,      _ ),
	house(house3,         _,       _,        _,         _,   milk ),
	house(  NorH, norwegian,       _,        _,         _,      _ ),
	house(  BluH,         _,    blue,        _,         _,      _ ),
	house(     _,         _,       _,violinist,         _,  juice ),
	house(  FoxH,         _,       _,        _,       fox,      _ ),
	house(  DocH,         _,       _,   doctor,         _,      _ ),
	house(  HorH,         _,       _,        _,     horse,      _ ),
	house(  DipH,         _,       _, diplomat,         _,      _ ),
	house(     _,         _,       _,        _,     zebra,      _ ),
	house(     _,         _,       _,        _,         _,  water )
	],

	on_the_right(GreH,WhiH) infers Language,
	next_to(NorH,BluH)      infers Language,
	next_to(FoxH,DocH)      infers Language,
	next_to(HorH,DipH)      infers Language,

	all_member(Constraints,Houses).


% Constraint Definition ----------------------------------------

on_the_right(house2,house1).
on_the_right(house3,house2).
on_the_right(house4,house3).
on_the_right(house5,house4).

next_to(H1,H2) :- on_the_right(H1,H2).
next_to(H1,H2) :- on_the_right(H2,H1).

all_member([],_).
all_member([H|T],List) :-
	member(H,List),
	all_member(T,List).

