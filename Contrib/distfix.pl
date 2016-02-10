:- module(distfix).			% SEPIA header

:- lib(rdtok).
:- import read_tokens/2 from rdtok.

:- dynamic is_distprefix_op/5, is_distinfix_op/5.

:- local display/1.
display(X) :- write(error,X).
ttyput(X) :- put(error, X).
ttynl :- nl(error).

%   File   : DISTFI.PL
%   Author : R.A.O'Keefe
%   Updated: 10 May 1984
%   Purpose: Read Prolog terms (with extended syntax).

/*  Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.

    I have added the rule X(...) -> apply(X,[...]) for Alan Mycroft.

    Distfix operators have finally been added.  They are declared by
	distfixop(Priority, Type, Pattern, Term)
    where Priority is as usual, Type is currently only fx or fy (if
    the Pattern doesn't specify a right argument one of the types must
    still be specified but it doesn't matter which), Term is what the
    reader is to return when it sees something matching the pattern,
    and the Pattern is a list of atoms and variables whose first
    elements is an atom, and in which no two variables appear side by
    side without an intervening atom.  To avoid ambiguities, the first
    atom following each variable should NOT be an infix or postfix
    operator, but the code below does not check for that, as you could
    declare such an operator after declaring the distfix form.
    Examples:
	distfixop(950, fy, [for,each,X,show,that,Y], forall(X,Y))
	distfixop(1105, fx, [try,Goal,reverting,to,Alternative,on,failure],
				(Goal;Alternative))
	distfixop(999, fy, [there,is,a,clause,with,head,H,and,body,B],
				clause(H,B))
	distfixop(999, fy, [there,is,a,clause,with,head,H], clause(H,_))
    Infix forms are also available.  These have the side effect of
    declaring the head keyword as an infix operator; anything that did
    not do this would be significantly harder to patch into the old parser.
    Examples:
	distfixop(700, xfy, [S,is,the,set,of,X,such,that,P], setof(X,P,S))
	distfixop(700, xfy, [B,is,the,bag,of,X,such,that,P], bagof(X,P,S)),
	distfixop(700, xfy, [X,is,to,Y,as,A,is,to,B], X*B =:= A*Y),
	distfixop(700, xfx, [X,had,N,variables], numbervars(X,0,N))
*/

:- export
	distfixop/4,
	read/2.

:- mode
	after_prefix_op(+, +, +, +, +, -, -),
	ambigop(+, -, -, -, -, -),
	cant_follow_expr(+, -),
	distfixop(?, ?, ?, ?),
	distfix_head(+, +, +, -, -),
	distfix_head(+, +, +, +, -, -),
	distfix_keys(+, ?, ?),
	distfix_pass(+, +, -),
	distfix_pattern(+, +, -),
	distfix_read(+, +, -),
	expect(+, +, -),
	exprtl(+, +, +, +, -, -),
	exprtl0(+, +, +, -, -),
	infixop(+, -, -, -),
	postfixop(+, -, -),
	prefixop(+, -, -),
	prefix_is_atom(+, +),
	read(?, ?),
	read(+, +, -, -),
	read(+, +, +, -, -),
	read_args(+, -, -),
	read_list(+, -, -),
	syntax_error(+),
	syntax_error(+, +).


%   read(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

read(Answer, Variables) :-
	repeat,
	    read_tokens(Tokens, Variables),
	    (   read(Tokens, 1200, Term, Leftover), all_read(Leftover)
	    ;   syntax_error(Tokens)
	    ),
	!,
	Answer = Term.


%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op.
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	current_op(Prec, fy, Op), !.
prefixop(Op, Prec, Less) :-
	current_op(Prec, fx, Op), !,
	Less is Prec-1.


postfixop(Op, Prec, Prec) :-
	current_op(Prec, yf, Op), !.
postfixop(Op, Less, Prec) :-
	current_op(Prec, xf, Op), !, Less is Prec-1.


infixop(Op, Less, Prec, Less) :-
	current_op(Prec, xfx, Op), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	current_op(Prec, xfy, Op), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	current_op(Prec, yfx, Op), !, Less is Prec-1.


ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1), !.


%   read(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

read([Token|RestTokens], Precedence, Term, LeftOver) :-
	read(Token, RestTokens, Precedence, Term, LeftOver).
read([], _, _, _) :-
	syntax_error([expression,expected], []).


%   read(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

read(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).

read(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).

read(atom(-), [integer(Integer)|S1], Precedence, Answer, S) :-
	Negative is -Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).

read(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(atom(Keyword), S0, Precedence, Answer, S) :-
	is_distprefix_op(Keyword, Prec, Keys, Pattern, Term),
	Precedence >= Prec,
	distfix_pass(Keys, S0, S1),
	distfix_read(Pattern, S1, S2),
	!,
	exprtl(S2, Prec, Term, Precedence, Answer, S).

read(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).

read(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).

read(integer(Integer), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).

read('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).

read('[', S1, Precedence, Answer, S) :- !,
	read(S1, 999, Arg1, S2),
	read_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).

read('(', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read(' (', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).

read('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).

read('{', S1, Precedence, Answer, S) :- !,
	read(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).

read(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).

read(Token, S0, _, _, _) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   read_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

read_args([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_args(S2, Rest, S).
read_args([')'|S], [], S) :- !.
read_args(S, _, _) :-
	syntax_error([', or )',expected,in,arguments], S).


%   read_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list of terms.

read_list([','|S1], [Term|Rest], S) :- !,
	read(S1, 999, Term, S2), !,
	read_list(S2, Rest, S).
read_list(['|'|S1], Rest, S) :- !,
	read(S1, 999, Rest, S2), !,
	expect(']', S2, S).
read_list([']'|S], [], S) :- !.
read_list(S, _, _) :-
	syntax_error([', | or ]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
	syntax_error([prefix,operator,Op,in,context,
		with,precedence,Precedence], S0).

after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).

after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	read(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :-
	prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).

exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	read(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	read(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax_error([Culprit,follows,expression], [Thing|S1]).

exprtl0(S, Term, _, Term, S).


cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr(' (',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).



exprtl([infixop(F,L,O,_)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 0, C =< L,
	is_distinfix_op(F, Keys, Term, Pattern, Expr),
	distfix_pass(Keys, S1, S2),
	distfix_read(Pattern, S2, S3),
	!,	% do we want this?
	exprtl(S3, O, Expr, Precedence, Answer, S).

exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	read(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).

exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	read(S1, 1000, Next, S2), /*!,*/
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).

exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	read(S1, 1100, Next, S2), /*!,*/
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).

exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is detected,
%   we have to write out a message.  We also have to note how far it was
%   to the end of the input, and for this we are obliged to use the data-
%   base.  Then we fail all the way back to read(), and that prints the
%   input list with a marker where the error was noticed.  If subgoal_of
%   were available in compiled code we could use that to find the input
%   list without hacking the data base.  The really hairy thing is that
%   the original code noted a possible error and backtracked on, so that
%   what looked at first sight like an error sometimes turned out to be
%   a wrong decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no backtracking
%   at all.  This goal has not yet been met, and it will still occasionally
%   report an error message and then decide that it is happy with the input
%   after all.  Sorry about that.


syntax_error(Message, List) :-
	ttynl, display('**'),
	display_list(Message),
	length(List, Length),
	recorda(syntax_error, length(Length), _), !,
	fail.

display_list([Head|Tail]) :-
	ttyput(32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	ttynl.

syntax_error(List) :-
	recorded(syntax_error, length(AfterError), Ref),
	erase(Ref),
	length(List, Length),
	BeforeError is Length-AfterError,
	display_list(List, BeforeError), !,
	fail.

display_list(X, 0) :-
	display('<<here>> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	ttyput(32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	ttynl.

display_token(atom(X))	  :- !,	display(X).
display_token(var(_,X))	  :- !,	display(X).
display_token(integer(X)) :- !,	display(X).
display_token(string(X))  :- !,	display(X).
display_token(X)	  :-	display(X).

%  From here down is new stuff to handle distfix operators.

distfixop(Priority, Type, Pattern, Template) :-
	integer(Priority),
	Priority > 0,
	Priority =< 1200,
	atom(Type),
	(   (   Type = fx,	Right is Priority-1
	    ;   Type = fy,	Right = Priority
	    ),
	    distfix_keys(Pattern, [Atom|Keys], RestPattern),
	    distfix_pattern(RestPattern, Right, P_form),
	    !,
	    assert(is_distprefix_op(Atom, Priority, Keys, P_form, Template))
	;   (	Type = xfx,	Right is Priority-1
	    ;	Type = xfy,	Right = Priority
	    ;	Type = yfx,	Right is Priority-1
	    ),
	    distfix_pattern(Pattern, Right, P_form_0),
	    P_form_0 = p(Lhs,[Atom|Keys],P_form),
	    !,
	    op(Priority, Type, Atom),
	    assert(is_distinfix_op(Atom, Keys, Lhs, P_form, Template))
	).
distfixop(P, T, Pn, Te) :-
	nl(error), write(error, '! error: '),
	write(error, distfixop(P,T,Pn,Te)), nl(error),
	fail.

/*  A distfix pattern is one of
	p		  -- standing for the end of the pattern
	p(Var,Prio)	  -- standing for a right argument of that priority
	p(Var,Keys,Rest)  -- standing for Var Keyword... Restofpattern
    e.g. p(X,[by],p(Y,[giving,quotient],p(Q,[and,remainder],p(R,99)))).
    distfix_pattern(List, Prio, P_form) turns a human-readable list into
    this compact form.
*/

distfix_pattern([], _, p) :- !.
distfix_pattern([Var], Prio, p(Var,Prio)) :- !,
	var(Var).		%   
distfix_pattern([Var|List], Prio, p(Var,Keys,Rest)) :-
	var(Var),
	distfix_keys(List, Keys, RestList),
	Keys \== [], !,
	distfix_pattern(RestList, Prio, Rest).

/*  distfix_keys picks off all the atoms at the front of the list.
*/
distfix_keys([Key|List], [Key|Keys], RestList) :-
	atom(Key), !,
	distfix_keys(List, Keys, RestList).
distfix_keys(List, [], List).


distfix_read(p, S0, S) :-
	peepop(S0, S).
distfix_read(p(Variable,Priority), S0, S) :-
	read(S0, Priority, Variable, S).
distfix_read(p(Variable,Keywords,RestPattern), S0, S) :- 
	distfix_head(S0, [], Keywords, Tokens, S1),
	%  This may backtrack over ever longer Token lists
	read(Tokens, 1200, Variable, T),
	T = [],
	!,	%  not sure if I want this cut or not
	distfix_read(RestPattern, S1, S).


/*  Distfix_pass(Keys, S0, S)
    is basically append(Keys, S, S0), but Keys is a list of atoms,
    and the prefix of S0 must be atom(K1),...,atom(Kn)
*/
distfix_pass([], S, S) :- !.
distfix_pass([Key|Keys], [atom(Key)|S0], S) :-
	distfix_pass(Keys, S0, S).

/*  Distfix_head(S0, Stack, Keys, Tokens, S)
    matches S0 against Tokens & Keys & S, where Tokens is balanced
    with respect to () [] {}.  It uses the Stack to keep track of
    what brackets need balancing.
*/

distfix_head(S0, [], Keys, [], S) :-
	distfix_pass(Keys, S0, S).
distfix_head([Token|S0], Stack, Keys, [Token|Tokens], S) :-
	distfix_head(Token, Stack, S0, Keys, Tokens, S).

distfix_head('(', Stack, S0, Keys, Tokens, S) :- !,
	distfix_head(S0, [')'|Stack], Keys, Tokens, S).
distfix_head(' (',Stack, S0, Keys, Tokens, S) :- !,
	distfix_head(S0, [')'|Stack], Keys, Tokens, S).
distfix_head('[', Stack, S0, Keys, Tokens, S) :- !,
	distfix_head(S0, [']'|Stack], Keys, Tokens, S).
distfix_head('{', Stack, S0, Keys, Tokens, S) :- !,
	distfix_head(S0, ['}'|Stack], Keys, Tokens, S).
distfix_head(Token, [Token|Stack], S0, Keys, Tokens, S) :- !,
	distfix_head(S0, Stack, Keys, Tokens, S).
distfix_head(Token, _, _, _, _, _) :-
	atom(Token),
	(Token = ')' ; Token = ']' ; Token = '}'),
	!, fail.
distfix_head(_, Stack, S0, Keys, Tokens, S) :-
	distfix_head(S0, Stack, Keys, Tokens, S).

