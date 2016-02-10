:- module(expert_system).

getbug :- 	
	writeln('\nCheck that you are in "module(expert_system).", then'),
	writeln('to start the program type "explore(X isa carnivore).".\n').
	  

:- op(900, xfx, ':').
:- op(870, fx, if).
:- op(880, xfx, then).
:- op(550, xfy, or).
:- op(540, xfy, and).
:- op(100, xfx, [gives, eats, has, isa]).

explore(Goal) :-
	fact:Goal.

explore(Goal) :-
	Rule:if Cond then Goal,
	explore(Cond).

explore(Goal1 and Goal2) :-
	explore(Goal1),
	explore(Goal2).

explore(Goal1 or Goal2) :-
	( explore(Goal1) 
	; explore(Goal2)).

fact: sheba gives milk.
fact: sheba eats meat.

% m_rule concludes about mammals
m_rule:  if
	    A has hair
	    or
	    A gives milk
	  then
	    A isa mammal.

%c_rule concludes about carnivores
c_rule: if
	    A isa mammal
	    and 
	    A eats meat
	  then
	    A isa carnivore.




