:- module(lambda, [
		   (^)/3, (^)/4, (^)/5, (^)/6, (^)/7, (^)/8, (^)/9,
		   (\)/1, (\)/2, (\)/3, (\)/4, (\)/5, (\)/6, (\)/7,
		   (+\)/2, (+\)/3, (+\)/4, (+\)/5, (+\)/6, (+\)/7,
		   op(201,xfx,+\)], eclipse_language).

:- export initialization(
    (
	current_module_predicate(exported,P)@lambda,
	\+current_predicate(P),
	(import P from lambda),
	printf(warning_output, "WARNING: library(lambda) redefines %Dw%n", [P]),
	fail
    ;
	true
    )).
:- local op(1150,fx,meta_predicate).

:- comment(categories, ["Programming Utilities"]).
:- comment(author, "Ulrich Neumerkel, ulrich@complang.tuwien.ac.at (ECLiPSe port: Joachim Schimpf)").
:- comment(copyright, "2009 Ulrich Neumerkel. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY Ulrich Neumerkel ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Ulrich Neumerkel OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation
are those of the authors and should not be interpreted as representing
official policies, either expressed or implied, of Ulrich Neumerkel.
").

:- comment(summary, "This library provides lambda expressions to simplify higher order
programming based on call/N.").

:- comment(desc, html("<PRE>
/** <module> Lambda expressions

This library provides lambda expressions to simplify higher order
programming based on call/N.

Lambda expressions are represented by ordinary Prolog terms.
There are two kinds of lambda expressions:

    Free+\\X1^X2^ ..^XN^Goal

         \\X1^X2^ ..^XN^Goal

The second is a shorthand for t+\\X1^X2^..^XN^Goal.

Xi are the parameters.

Goal is a goal or continuation. Syntax note: Operators within Goal
require parentheses due to the low precedence of the ^ operator.

Free contains variables that are valid outside the scope of the lambda
expression. They are thus free variables within.

All other variables of Goal are considered local variables. They must
not appear outside the lambda expression. This restriction is
currently not checked. Violations may lead to unexpected bindings.

In the following example the parentheses around X>3 are necessary.

==
?- use_module(library(lambda)).
?- use_module(library(apply)).

?- maplist(\\X^(X>3),[4,5,9]).
true.
==

In the following X is a variable that is shared by both instances of
the lambda expression. The second query illustrates the cooperation of
continuations and lambdas. The lambda expression is in this case a
continuation expecting a further argument.

==
?- Xs = [A,B], maplist(X+\\Y^dif(X,Y), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).

?- Xs = [A,B], maplist(X+\\dif(X), Xs).
Xs = [A, B],
dif(X, A),
dif(X, B).
==

The following queries are all equivalent. To see this, use
the fact f(x,y).
==
?- call(f,A1,A2).
?- call(\\X^f(X),A1,A2).
?- call(\\X^Y^f(X,Y), A1,A2).
?- call(\\X^(X+\\Y^f(X,Y)), A1,A2).
?- call(call(f, A1),A2).
?- call(f(A1),A2).
?- f(A1,A2).
A1 = x,
A2 = y.
==

Further discussions
http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/ISO-Hiord

@tbd Static expansion similar to apply_macros.
@author Ulrich Neumerkel
*/
</PRE>")).

:- meta_predicate no_hat_call(0).
:- tool(no_hat_call/1, no_hat_call_/2).

:- meta_predicate
	^(?,0,?),
	^(?,1,?,?),
	^(?,2,?,?,?),
	^(?,3,?,?,?,?),
	^(?,4,?,?,?,?,?),
	^(?,5,?,?,?,?,?,?),
	^(?,6,?,?,?,?,?,?,?).
:- tool((^)/3, '^_'/4).
:- tool((^)/4, '^_'/5).
:- tool((^)/5, '^_'/6).
:- tool((^)/6, '^_'/7).
:- tool((^)/7, '^_'/8).
:- tool((^)/8, '^_'/9).
:- tool((^)/9, '^_'/10).

'^_'(V1,Goal,V1,M) :-
   no_hat_call(Goal)@M.
'^_'(V1,Goal,V1,V2,M) :-
   call(Goal,V2)@M.
'^_'(V1,Goal,V1,V2,V3,M) :-
   call(Goal,V2,V3)@M.
'^_'(V1,Goal,V1,V2,V3,V4,M) :-
   call(Goal,V2,V3,V4)@M.
'^_'(V1,Goal,V1,V2,V3,V4,V5,M) :-
   call(Goal,V2,V3,V4,V5)@M.
'^_'(V1,Goal,V1,V2,V3,V4,V5,V6,M) :-
   call(Goal,V2,V3,V4,V5,V6)@M.
'^_'(V1,Goal,V1,V2,V3,V4,V5,V6,V7,M) :-
   call(Goal,V2,V3,V4,V5,V6,V7)@M.

:- meta_predicate
	\(0),
	\(1,?),
	\(2,?,?),
	\(3,?,?,?),
	\(4,?,?,?,?),
	\(5,?,?,?,?,?),
	\(6,?,?,?,?,?,?).
:- tool((\)/1, '\\_'/2).
:- tool((\)/2, '\\_'/3).
:- tool((\)/3, '\\_'/4).
:- tool((\)/4, '\\_'/5).
:- tool((\)/5, '\\_'/6).
:- tool((\)/6, '\\_'/7).
:- tool((\)/7, '\\_'/8).

'\\_'(FC,M) :-
   copy_term(FC,C,_),no_hat_call(C)@M.
'\\_'(FC,V1,M) :-
   copy_term(FC,C,_),call(C,V1)@M.
'\\_'(FC,V1,V2,M) :-
   copy_term(FC,C,_),call(C,V1,V2)@M.
'\\_'(FC,V1,V2,V3,M) :-
   copy_term(FC,C,_),call(C,V1,V2,V3)@M.
'\\_'(FC,V1,V2,V3,V4,M) :-
   copy_term(FC,C,_),call(C,V1,V2,V3,V4)@M.
'\\_'(FC,V1,V2,V3,V4,V5,M) :-
   copy_term(FC,C,_),call(C,V1,V2,V3,V4,V5)@M.
'\\_'(FC,V1,V2,V3,V4,V5,V6,M) :-
   copy_term(FC,C,_),call(C,V1,V2,V3,V4,V5,V6)@M.

:- meta_predicate
	+\(?,0),
	+\(?,1,?),
	+\(?,2,?,?),
	+\(?,3,?,?,?),
	+\(?,4,?,?,?,?),
	+\(?,5,?,?,?,?,?),
	+\(?,6,?,?,?,?,?,?).
:- tool((+\)/2, '+\\_'/3).
:- tool((+\)/3, '+\\_'/4).
:- tool((+\)/4, '+\\_'/5).
:- tool((+\)/5, '+\\_'/6).
:- tool((+\)/6, '+\\_'/7).
:- tool((+\)/7, '+\\_'/8).
:- tool((+\)/8, '+\\_'/9).

'+\\_'(GV,FC,M) :-
   copy_term(GV+FC,GV+C,_),no_hat_call(C)@M.
'+\\_'(GV,FC,V1,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1)@M.
'+\\_'(GV,FC,V1,V2,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1,V2)@M.
'+\\_'(GV,FC,V1,V2,V3,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1,V2,V3)@M.
'+\\_'(GV,FC,V1,V2,V3,V4,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1,V2,V3,V4)@M.
'+\\_'(GV,FC,V1,V2,V3,V4,V5,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1,V2,V3,V4,V5)@M.
'+\\_'(GV,FC,V1,V2,V3,V4,V5,V6,M) :-
   copy_term(GV+FC,GV+C,_),call(C,V1,V2,V3,V4,V5,V6)@M.


%% no_hat_call(:Goal)
%
% Like call, but issues an error for a goal (^)/2.  Such goals are
% likely the result of an insufficient number of arguments.

no_hat_call_(Goal, M) :-
   (  nonvar(Goal),
      Goal = (_^_)
   -> throw(error(existence_error(lambda_parameters,Goal),_))
   ;  call(Goal)@M
   ).

% I would like to replace this by:
% V1^Goal :- throw(error(existence_error(lambda_parameters,V1^Goal),_)).
