% MISSING21

:- module(collect).

getbug :-
	writeln('\nCheck that you are in "module(collect).", then'),
	writeln('to start the program type "makelist(LA - LT).".\n').

bug :- 
	nl,
	explanation.

explanation :-	
writeln(' \n \
In the first clause the first argument in the head should be L-L. \n \
Hence the recursion can not terminate properly and fails one step \n \
before the end of the job. \n \
 \n \
GOAL:	 makelist(ListA - ListT). \n \
CORRECT: ListA = [liechtenstein, ... , germany, tail], \n \
	 ListT = tail. \n \
BUGGY:   no (more) solutions. \n \
').


% ============================================================================
/*
 p. 198   ex. 8.4 ff.

 Map Colouring Problem 
 The task is to rewrite the 'collect'-procedure on p. 191
 using difference pair representation for lists

 Lists represented by difference pairs
*/

diff_member(X,[Y|T]-T) :- 		% conflict with Opium member/2
  X \== Y,
  !,
  fail. 
diff_member(X,[X|Tail]-T).
diff_member(X,[Head|Tail]-T) :-
  diff_member(X,Tail-T).

diff_conc(A1-Z1,Z1-Z2,A1-Z2).

neighbours(austria,[italy,liechtenstein,switzerland,germany|T]-T). 
neighbours(belgium,[netherlands,luxembourg,france,germany|T]-T).
neighbours(netherlands,[germany,belgium|T]-T).
neighbours(germany,[netherlands,belgium,luxembourg,france,austria,
                switzerland,liechtenstein|T]-T).
neighbours(switzerland,[italy,france,germany,austria,liechtenstein|T]-T).
neighbours(italy,[austria,switzerland,liechtenstein|T]-T).
neighbours(luxembourg,[belgium,france,germany|T]-T).
neighbours(france,[switzerland,germany,luxembourg,belgium|T]-T).
neighbours(liechtenstein,[germany,austria,switzerland|T]-T).
   
makelist(ListA-ListT) :-
	collect([germany|[tail]]-[tail],[tail]-[tail],ListA-ListT).

/* collect(Open, Closed, Result) */

collect([]-L,ClA-ClT,ClA-ClT).			% fix: L-L instead of []-L
collect([X|OpA]-OpT,ClA-ClT,ListA-ListT) :-
  diff_member(X,ClA-ClT),			      % X has already been collected
  !,  
  collect(OpA-OpT,ClA-ClT,ListA-ListT).
collect([X|OpA]-OpT,ClA-ClT,ListA-ListT) :-  
  neighbours(X,Ngbs),                                        
  diff_conc(Ngbs,OpA-OpT,Op1A-Op1T),                       
  collect(Op1A-Op1T,[X|ClA]-ClT,ListA-ListT).

