% math-utilities.pl ===========================================================
% thom fruehwirth 1991-92, revised 930518,931223
% 940304 matching mistake in normalize1/2 corrected
% 940304 math_portray/2 extended with clauses for math-fourier.chr
% for math* solvers


% SETTINGS ---------------------------------------------------------------------

% for use in is/2: precision, slack variables, simulated infimum, etc.

% Code works with flag prefer_rationals on or off
% and with float_precision single or double

% adapt precision for zero/1 test
:- get_flag(float_precision,G),
   (G==single -> setval(precision,1e-06),setval(mprecision,-1e-06)
   ;
    G==double -> setval(precision,1e-12),setval(mprecision,-1e-12)
   ).

slack(X,X).	% :- X>=0.

inf(3.40282e38).
minf((-3.40282e38)).
sup(1e-45).
msup((-1e-45)).



% PRETTY PRINT ---------------------------------------------------------------

math_portray(equals(P,C),P1=:=0):- zero(C),!,
	make_poly(P,P1).
math_portray(equals(P,C),P1=:=C1):-!,
	MC is (-C),
	avoid_float(MC,C1),
	make_poly(P,P1).
math_portray(eqnonlin(X,(E)),X=:=E):-!.
% for math-fourier.chr:
math_portray(eq(P,C,R),Out):- zero(C),!,
	Out=..[R,P1,0],
	make_poly(P,P1).
math_portray(eq(P,C,R),Out):-!,
	Out=..[R,P1,C1],
	MC is (-C),
	avoid_float(MC,C1),
	make_poly(P,P1).

:- define_macro((equals)/2,math_portray/2,[write]).
:- define_macro((eqnonlin)/2,math_portray/2,[write]).
:- define_macro((eq)/3,math_portray/2,[write]).

%make_poly([],0).
make_poly([X*C],-CX):- call_kernel(C<0),!,
	C1 is (-C),
	avoid_float(C1,C2),
	make_mono(C2,X,CX).
make_poly([X*C],CX):-!,
	avoid_float(C,C1),
	make_mono(C1,X,CX).
make_poly([X*C|P],P1-CX):- call_kernel(C<0),!,
	C1 is (-C),
	avoid_float(C1,C2),
	make_mono(C2,X,CX),
	make_poly(P,P1).
make_poly([X*C|P],P1+CX):-
	avoid_float(C,C1),
	make_mono(C1,X,CX),
	make_poly(P,P1).

make_mono(C,X,CX):- nonvar(X),X=slack(Y),!,make_mono(C,Y,CX).
make_mono(C,X,CX1):- nonvar(X),number(X),!,CX is C*X,avoid_float(CX,CX1).
make_mono(1,X,X):-!.
make_mono(1_1,X,X):-!.
make_mono(C,X,C*X).


% AUXILIARY PREDICATES -------------------------------------------------------

call_kernel((A,B)) ?- !,call_kernel(A),call_kernel(B).
call_kernel(Built_in):- call_explicit(Built_in,sepia_kernel).

sort1(A,B):-
	sort(A,C),
	((C=[X*_|_],nonvar(X),X=slack(_))->A=B;B=C). % slacks unordered why?

% globalize variables into meta variables so they can be sorted
globalize(Vars):- delay(Vars-Flag,true),Flag=fired.

rev([],L,L).
rev([X|L1],L2,L3):- rev(L1,[X|L2],L3).

extract(X*C2,P0,P) ?- delete(Y*C2,P0,P),X==Y,!.

zero(C):-    
    real(C) -> 
	getval(precision,P),   % otherwise call-kernel does not work
	getval(mprecision,MP),
	call_kernel(MP < C),    % cope with imprecision
	call_kernel(C < P)      
    ;
	call_kernel(C=:=0).

nonzero(C):- not zero(C).


is_div(C1,C2,C3):- nonvar(C3),C3=slack(C4),!,is_div(C1,C2,C4). % slack-case
is_div(C1,C2,C3):- zero(C1),!,C3=0.
is_div(C1,C2,C3):- X is -(C1/C2),  % minus here to get sign needed in handlers
	avoid_float(X,C3).

is_mul(C1,C2,C3):- zero(C1),!,C3=0.
is_mul(C1,C2,C3):- zero(C2),!,C3=0.
is_mul(C1,C2,C3):- X is C1*C2, 
	avoid_float(X,C3).

avoid_float(X,C3):-
	real(X) -> Y is round(X),Z is X-Y,(zero(Z)-> C3 is fix(Y);C3=X) ; C3=X.


simplifyable(X*C,P,P1):- delete(X*C,P,P1),ground(X),!.

ground(X):- not nonground(X).


% HANDLING SLACK VARIABLES ----------------------------------------------------

all_slacks([]).
all_slacks([slack(_)*C|P]) ?-
	all_slacks(P).

all_slacks([],_).
all_slacks([slack(_)*C|P],S) ?-
	sign(C,S),
	all_slacks(P,S).

sign(C,0):- zero(C),!.
sign(C,S):- call_kernel(C>0) -> S=1 ; S=(-1).

all_zeroes([]).
all_zeroes([slack(0)*C|P]) :-
	all_zeroes(P).


% COMPUTING WITH POLYNOMIALS -------------------------------------------------

% gets rounded constant C from is_div/3
mult_const(eq0(C1,P1),C,eq0(0 ,[])):- call_kernel(C=:=0),!.
mult_const(eq0(C1,P1),C,eq0(C1,P1)):- call_kernel(C=:=1),!.
mult_const(eq0(C1,P1),C2,eq0(C,P)):-
	(zero(C1) -> C=0 ; C is C1*C2),
	mult_const1(P1,C2,P).
 mult_const1([],C,[]).
 mult_const1([Xi*Ci|Poly],C,PolyR):-
	(zero(Ci) -> PolyR=NPoly ; NCi is Ci*C,PolyR=[Xi*NCi|NPoly]),
	mult_const1(Poly,C,NPoly).

% gets input from const_mult/3
add_eq0(eq0(C1,P1),eq0(C2,P2),eq0(C,P0)):-
	Ci is C1+C2,
	(zero(Ci) -> C=0 ; C=Ci),
	add_eq1(P1,P2,P0).
%	sort(P,P0).
 add_eq1([],Poly,Poly):-!.
 add_eq1(Poly,[],Poly):-!.
 add_eq1([Xi1*Ci1|Poly1],Poly21,Poly):-
	delete(Xi2*Ci2,Poly21,Poly2),Xi2==Xi1,
	!,
	Ci is Ci1+Ci2,
	(zero(Ci) -> Poly=Poly3 ; Poly=[Xi1*Ci|Poly3]),
	add_eq1(Poly1,Poly2,Poly3).
 add_eq1([Xi1*Ci1|Poly1],Poly2,[Xi1*Ci1|Poly3]):-
	add_eq1(Poly1,Poly2,Poly3).



normalize(A,B,P2,C1):-
        normalize1(A-B,P),
	P=eq0(C1,P1),rev(P1,[],P1R),globalize(P1R),
	sort1(P1,P2).                                 

 normalize1(V,P) ?- var(V),!,
	P=eq0(0,[V*1]).
 normalize1(C,P) ?- ground(C),!,
	C1 is C,P=eq0(C1,[]).
 normalize1(slack(V),P) ?- !,
	P=eq0(0,[slack(V)*1]).
 normalize1((+E),P) ?-!,
	normalize1(E,P).
 normalize1((-E),P) ?-!,
	normalize1(E,P1),
	mult_const(P1,(-1),P).
 normalize1(A*B,C) ?- ground(A),!,
	normalize1(B,BN),
	mult_const(BN,A,C).
 normalize1(B*A,C) ?- ground(A),!,
	normalize1(B,BN),
	mult_const(BN,A,C).
 normalize1(B/A,C) ?- ground(A),!,
	normalize1(B,BN),
	A1 is 1/A,
	mult_const(BN,A1,C). 
 normalize1(A-B,C) ?- !,
	normalize1(A,AN),
	normalize1((-B),BN),
	add_eq0(AN,BN,C).
 normalize1(A+B,C) ?- !,
	normalize1(A,AN),
	normalize1(B,BN),
	add_eq0(AN,BN,C).
 normalize1(E,C) ?-
	C=eq0(0,[CX*1]),
	eqnonlin(CX,E).     % add a nonlinear equation constraint


% end of file math-utilities.pl -----------------------------------------------