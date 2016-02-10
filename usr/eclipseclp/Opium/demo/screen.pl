/*
 *            S C R E E N    H A N D L I N G
 *
 *  (modification of sepia library screen according to Educe)
 */


writemess(V,H,MESS) :- cursor(V,H), cleartoeol, write(MESS).
clearline(V,H) :- cursor(V,H), cleartoeol.
clearpage :- system(clear).
% clearpage :- put(27), put(91), put(50), put(74), cursor(0,0).
cleartoeol :- put(27), put(91), put(75).
cursor(R,C) :- put(27), put(91), putm(R), put(59), putm(C),
	       put(72).
putm(C) :- 
	C1 is (C+1)/10 - ((C+1) mod 10 / 10) + 48, 
	fix(C1, F1),	
	put(F1),
	C2 is ((C+1) mod 10)+48, 
	fix(C2, F2),
	put(F2).

flash :- put(27), put(91), put(53), put(109).
inverse :- put(27), put(91), put(55), put(109).
attroff :- put(27), put(91), put(109).
underline :- put(27), put(91), put(52), put(109).
bold :- put(27), put(91), put(49), put(109).
bell :- put(7).

:- skipped(cursor/2).


