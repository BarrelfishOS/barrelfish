/*
    Try the goals:

      crossword(W, consistent).
      crossword(W, unique).
      crossword(W, fc).
      crossword(W, ic_symbolic).

    Try visualising the execution by starting a Visualisation Client!

    First solution:

	|a a r o n |
	|b l o r e |
	|r a b a t |
	|i d i o t |
	|m a n n y |

*/


:- lib(ic).
:- lib(ic_symbolic).
:- import element/3 from ic_symbolic.
:- lib(propia).
:- lib(viewable).


:-local domain(letters(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)).


crossword_all(Letters,Prop) :-
	setval(count, 0),
	(
	    crossword(Letters,Prop),
	    incval(count), getval(count,C), printf("%d %b", [C]),
	    fail
	;
	    getval(count,C), nl, printf("%d solutions.%n", [C])
	).


crossword(Letters,Prop) :-
	Letters=[W11,
                 W12,W21,W22,
                 W13,W31,W23,W32,W33,
                 W14,W41,W24,W42,W34,W43,W44,
                 W15,W51,W25,W52,W35,W53,W45,W54,W55],
        Letters &:: letters,

	viewable_create(crossword, [](
                      [](W11,W12,W13,W14,W15),
                      [](W21,W22,W23,W24,W25),
                      [](W31,W32,W33,W34,W35),
                      [](W41,W42,W43,W44,W45),
                      [](W51,W52,W53,W54,W55))),

        propagate( w(W11,W12,W13,W14,W15),Prop),
        propagate( w(W21,W22,W23,W24,W25),Prop),
        propagate( w(W31,W32,W33,W34,W35),Prop),
        propagate( w(W41,W42,W43,W44,W45),Prop),
        propagate( w(W51,W52,W53,W54,W55),Prop),  
        propagate( w(W11,W21,W31,W41,W51),Prop),
        propagate( w(W12,W22,W32,W42,W52),Prop),
        propagate( w(W13,W23,W33,W43,W53),Prop),
        propagate( w(W14,W24,W34,W44,W54),Prop),
        propagate( w(W15,W25,W35,W45,W55),Prop),

	search(Letters, 0, input_order, my_indomain, complete, []).


propagate(Goal,fc) :- !,
	suspend(Goal,7,Goal->inst) infers most.
propagate(Goal,Language) :-
	Goal infers Language.


my_indomain(X):-
        ic_symbolic:indomain(X).



% dict_berghel: Lexicon for crossword benchmark from H. Berghel and R. Rankin
% The Computer Journal, April 1990


% Lexicon: 134 words
% Solutions: 72 solutions 
% Lexicon\{Aaron} makes the problem infeasible.


w(a, a, r, o, n).	% can remove 1st word to make the pb unsatisfiable
w(a, b, a, s, e).
w(a, b, b, a, s).
w(a, b, d, a, l).
w(a, b, e, a, m).
w(a, b, e, l, e).
w(a, b, n, e, r).
w(a, b, r, a, m).
w(a, b, r, i, m).
w(a, c, a, n, a).
w(a, c, a, r, a).
w(a, c, a, t, e).
w(a, c, o, i, n).
w(a, d, d, i, e).
w(a, e, g, l, e).
w(a, l, a, d, a).
w(a, l, a, t, e).
w(a, l, b, a, n).
w(a, l, l, o, w).
w(a, l, o, s, a).
w(a, l, u, l, a).
w(a, m, p, l, y).
w(a, n, a, n, a).
w(a, n, e, l, e).
w(a, n, e, n, t).
w(a, n, k, l, e).
w(a, r, a, c, a).
w(a, r, a, i, n).
w(a, r, a, m, u).
w(a, r, a, r, a).
w(a, r, d, e, b).
w(a, r, e, e, k).
w(a, r, e, n, a).
w(a, r, e, t, e).
w(a, r, o, m, a).
w(a, r, u, l, o).
w(a, w, a, r, d).
w(b, a, c, o, n).
w(b, a, d, o, n).
w(b, a, t, o, n).
w(b, e, f, i, t).
w(b, e, n, i, n).
w(b, e, t, i, s).
w(b, l, o, r, e).
w(b, o, s, u, n).
w(b, r, a, c, e).
w(b, r, a, v, a).
w(b, r, a, v, o).
w(b, r, a, z, e).
w(b, r, e, b, a).
w(b, r, e, m, e).
w(b, r, e, v, a).
w(b, r, o, m, a).
w(b, r, o, s, e).
w(b, u, r, e, t).
w(c, l, i, n, e).
w(c, l, i, t, e).
w(c, r, o, r, e).
w(d, e, m, i, t).
w(d, e, n, i, m).
w(e, m, e, n, d).
w(e, n, a, t, e).
w(e, u, r, u, s).
w(i, d, i, o, t).
w(i, n, e, r, t).
w(i, t, a, l, a).
w(l, a, d, e, r).
w(l, a, s, e, r).
w(l, a, y, e, r).
w(m, a, n, n, y).
w(m, a, r, a, l).
w(m, a, r, k, a).
w(m, e, t, a, l).
w(m, o, n, a, l).
w(n, a, m, e, r).
w(n, a, n, n, y).
w(n, a, s, a, l).
w(n, a, t, a, l).
w(n, a, t, h, e).
w(n, e, e, d, s).
w(n, e, e, l, d).
w(n, e, e, s, e).
w(n, e, n, t, a).
w(n, e, t, t, y).
w(n, e, w, e, l).
w(n, o, d, a, l).
w(n, o, n, e, t).
w(n, o, n, y, l).
w(n, o, t, a, l).
w(n, o, t, e, r).
w(o, b, e, s, e).
w(o, c, h, n, a).
w(o, m, i, n, a).
w(o, n, s, e, t).
w(o, r, a, o, n).
w(o, r, i, e, l).
w(o, s, e, l, a).
w(o, t, t, a, r).
w(o, v, i, l, e).
w(o, v, o, i, d).
w(o, v, u, l, a).
w(o, z, a, r, k).
w(r, a, b, a, t).
w(r, a, c, o, n).
w(r, a, m, e, d).
w(r, a, n, g, e).
w(r, a, s, p, y).
w(r, a, t, e, d).
w(r, a, t, e, r).
w(r, e, b, u, d).
w(r, e, b, u, t).
w(r, e, c, o, n).
w(r, e, d, a, n).
w(r, e, f, e, l).
w(r, e, g, e, s).
w(r, e, n, e, s).
w(r, e, n, k, y).
w(r, e, p, e, w).
w(r, e, r, o, w).
w(r, e, r, u, n).
w(r, e, t, a, n).
w(r, i, a, t, a).
w(r, o, b, i, n).
w(r, o, d, g, e).
w(r, o, p, e, r).
w(r, o, t, a, l).
w(r, u, d, a, s).
w(s, a, l, a, l).
w(s, a, l, a, y).
w(s, a, n, d, y).
w(s, a, y, a, l).
w(s, k, e, e, t).
w(s, o, w, e, l).
w(t, a, t, e, s).



