% LOOP46

:- module(wave).

getbug :-	
	writeln('\nCheck that you are in "module(wave).", then'),
	writeln('to start the program type "prince(david).".\n').

bug :- 
	nl,
	explanation.

explanation :-	
writeln('There is a typo in the last fact. \n\
\n\
GOAL:    prince(david). \n\
CORRECT: yes. \n\
BUGGY:   endless loop (wave). \n').


% ============================================================================

prince(X) :- 
	ancestor(Y, X), 
	king(Y).

ancestor(X,Y) :- 
	father(X,Y).
ancestor(X,Z) :- 
	ancestor(Y,Z), 
	father(X,Y).

father(john, david).
father(peter, john).
father(george, pater).   % typing error

king(george).
