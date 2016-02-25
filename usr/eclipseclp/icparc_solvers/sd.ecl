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
% Copyright (C) 2005 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Solver for constraints over unordered symbolic domains
%
% This variant never aliases problem variables (unless unification
% is explicitly used).
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: sd.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------


:- module(sd).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Simple solver for constraints over unordered symbolic domains").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2009/07/16 09:11:27 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- lib(ordset).

:- meta_attribute(sd, [
	print:			get_domain_as_list/2,
	unify:			unify_sd/3,
	test_unify:		test_unify_sd/2,
	copy_term:		copy_term_sd/2,
	suspensions:		suspensions_sd/3,
	delayed_goals_number:	delayed_goals_number_sd/2,
	compare_instances:	compare_instances_sd/3
    ]).

:- local struct(sd(
	    dom,		% list of allowed values (ordset)
	    any			% suspension list
    	)).

:- export
	op(700, xfx, [&::,&=,&\=]).


%----------------------------------------------------------------------
% variable declaration and manipulation
%----------------------------------------------------------------------

:- export (&::)/2.
:- tool((&::)/2, have_domain/3).
have_domain(X, DomSpec, Module) :- var(X), !,
	have_domain([X], DomSpec, Module).
have_domain(Xs, DomSpec, Module) :- Xs = [_|_], !,
	( domain_values(DomSpec, Domain, Module) ->
	    proper_list_have_values(Xs, DomSpec, Domain)
	;
	    printf(error, "Illegal right hand side in %w%n", [(Xs &:: DomSpec)]),
	    abort
	).
have_domain(subscript(Array,Index), DomSpec, Module) :- !,
	subscript(Array, Index, Elems),
	have_domain(Elems, DomSpec, Module).
have_domain(X, DomSpec, Module) :-
	have_domain([X], DomSpec, Module).


    domain_values(DomSpec, _Values, _Module) :-
	var(DomSpec), !,
	fail.
    domain_values([], Values, _Module) :- !,
    	Values = [].
    domain_values(DomSpec, Values, Module) :-	% allow declared domains
	atom(DomSpec), !,
	current_domain(DomSpec, _DefModule, ValueArray)@Module,
	ValueArray =.. [_|Symbols],
	sort(Symbols, Values).
    domain_values(Symbols, Values, _Module) :-
	valid_values(Symbols),
	list_to_ord_set(Symbols, Values).

    valid_values([]) ?- true.
    valid_values([X|Xs]) ?-
	atomic(X),
	valid_values(Xs).


    proper_list_have_values([], _DomSpec, _Values) ?- !,
    	true.
    proper_list_have_values([X|Xs], DomSpec, Values) ?- !,
	have_values(X, DomSpec, Values),
	proper_list_have_values(Xs, DomSpec, Values).
    proper_list_have_values(X, DomSpec, _Values) :-
	printf(error, "Illegal left hand side (improper list) in %w%n", [(X &:: DomSpec)]),
	abort.

    have_values(X, _DomSpec, Values) :- (var(X);atomic(X)), !,
	has_values(X, Values).
    have_values(X, DomSpec, Values) :-
    	X = [_|_], !,
	proper_list_have_values(X, DomSpec, Values).
    have_values(X, DomSpec, _Values) :-
	printf(error, "Illegal left hand side in %w%n", [(X &:: DomSpec)]),
	abort.


    % Give a domain to a variable
    has_values(X{Attr}, Values) ?-
	( var(Attr) ->
	    domainless_var_has_values(X, Values)
	;
	    domainless_var_has_values(Dummy, Values),
	    X = Dummy
	).
    has_values(X, Values) :-
	free(X),
	domainless_var_has_values(X, Values).
    has_values(X, Values) :-
    	atomic(X),
    	ord_memberchk(X, Values).


    % Give a domain to a domainless variable
domainless_var_has_values(X, Values) :-
	Values = [Value|More],		% may fail
	( More == [] ->
	    X = Value
	;
	    Attr = sd{dom:Values},
	    init_suspension_list(any of sd, Attr),
	    add_attribute(X, Attr),
	    notify_constrained(X),
	    wake
	).


    % Exclude value from a domain variable (or constant)
exclude_value(Value, X) :- atomic(X),
    	X \= Value.
exclude_value(Value, X{Attr}) ?-
	Attr = sd{dom:Values},
    	( ord_selectchk(Value, Values, ValuesRemaining) ->
	    setarg(dom of sd, Attr, ValuesRemaining),
	    ( ValuesRemaining = [LastValue] ->
		X = LastValue
	    ;
		schedule_suspensions(any of sd, Attr),
		notify_constrained(X),
		wake
	    )
	;
	    true		% value wasn't in the domain
	).


%----------------------------------------------------------------------
% Variable access and test
%----------------------------------------------------------------------

:- export is_solver_var/1.
is_solver_var(_{sd{}}) ?- true.


:- export is_exact_solver_var/1.
is_exact_solver_var(_{sd{}}) ?- true.


:- export is_solver_type/1.
is_solver_type(_{sd{}}) ?- true.
is_solver_type(X) :- atomic(X).


list_of_solver_type([]) ?- true.
list_of_solver_type([X|Xs]) ?-
	is_solver_type(X),
	list_of_solver_type(Xs).


:- export get_domain_as_list/2.		% also print handler
get_domain_as_list(_{sd{dom:Values0}}, Values) ?-
	Values = Values0.
get_domain_as_list(X, Values) :- atomic(X),
	Values = [X].


get_var_attr(_{Attr0}, Attr) ?-		% get attribute of variable, if any
	nonvar(Attr0),
	Attr = Attr0.


:- export get_domain_size/2.
get_domain_size(_{sd{dom:Values}}, Size) ?-
	length(Values, Size).
get_domain_size(X, Size) :- atomic(X),
	Size = 1.


% Pick minimum domain size element from a list (first fail principle)
% (written such that the remaining list order is not perturbed)

:- export deleteff/3.
deleteff(Best, [X1|Xs], Rest) ?-
	get_domain_size(X1, S1),
        (
	    foreach(X,Xs),
	    fromto(X1,XBestSoFar,XNextBest,Best),
	    fromto(S1,SBestSoFar,SNextBest,_),
	    fromto(Start,Rest1,Rest2,[]),
	    fromto(Start,Head1,Head2,Gap),
	    fromto(Rest,Tail1,Tail2,Gap)
	do
	    get_domain_size(X, S),
            ( S < SBestSoFar ->
                XNextBest = X,
                SNextBest = S,
                Tail1 = [XBestSoFar|Head1],
                Tail2 = Rest1,
                Head2 = Rest2
            ;
                XNextBest = XBestSoFar,
                SNextBest = SBestSoFar,
                Tail2 = Tail1,
                Head2 = Head1,
                Rest1 = [X|Rest2]
            )
        ).


%----------------------------------------------------------------------
% Search
%----------------------------------------------------------------------

:- export indomain/1.
indomain(X{sd{dom:Values}}) ?- !,
	member(X, Values).
indomain(X) :- var(X), !,
	error(4, indomain(X)).
indomain(X) :- atomic(X).


:- export labeling/1.
labeling([]) ?- true.
labeling([X|Xs]) ?-
	indomain(X),
	labeling(Xs).


%----------------------------------------------------------------------
% Attribute variable handlers
%----------------------------------------------------------------------

% copy_term handler

copy_term_sd(_{sd{dom:Values}}, Copy) ?- !,
	domainless_var_has_values(Copy, Values).
copy_term_sd(_, _).


% unify handler (variable X just got bound to Y)

unify_sd(Y, AttrX, SuspAttrX) :-
	var(AttrX),
	( nonvar(SuspAttrX), is_solver_var(Y) ->
	    % non-solver var X gets bound to solver var Y
	    schedule_suspensions(constrained of suspend, SuspAttrX)
	;
	    true
	).
unify_sd(Y, AttrX, SuspAttrX) :-
	compound(AttrX),
	unify_any_sd(Y, AttrX, SuspAttrX).

    unify_any_sd(Y{AttrY}, AttrX, SuspAttrX) ?-
	unify_sd_sd(Y, AttrX, AttrY, SuspAttrX).
    unify_any_sd(Y, AttrX, _SuspAttrX) :-
	atomic(Y),
	AttrX = sd{dom:Values},
	ord_memberchk(Y, Values),
	schedule_suspensions(any of sd, AttrX).

    unify_sd_sd(Y, AttrX, AttrY, _SuspAttrX) :-
	var(AttrY),
	AttrY = AttrX,			% transfer the attribute
	notify_constrained(Y).		% non-solver var Y unified with X
    unify_sd_sd(Y, AttrX, AttrY, SuspAttrX) :-
	compound(AttrY),
	AttrX = sd{dom:ValuesX},
	AttrY = sd{dom:ValuesY},
	ord_intersection(ValuesX, ValuesY, Common, OnlyX, OnlyY),
	Common = [Val|Vals],		% may fail with Common = []
	setarg(dom of sd, AttrY, Common),
	( Vals = [] ->
	    schedule_suspensions(any of sd, AttrX),
	    Y = Val			% wakes inst,bound,constrained,any of Y
	;
	    ( OnlyX=[] -> true ;
	    	schedule_suspensions(any of sd, AttrX),
		( var(SuspAttrX) -> true ;
		    schedule_suspensions(constrained of suspend, SuspAttrX)
		)
	    ),
	    ( OnlyY=[] -> true ;
	    	schedule_suspensions(any of sd, AttrY),
		notify_constrained(Y)
	    )
	).


% test_unify handler

test_unify_sd(_Y, AttrX) :-
	var(AttrX).
test_unify_sd(Y, AttrX) :-
	compound(AttrX),
	test_unify_term_sd(Y, AttrX).

    test_unify_term_sd(_Y{AttrY}, AttrX) ?-
	test_unify_sd_sd(AttrY, AttrX).
    test_unify_term_sd(Y, sd{dom:ValuesX}) :-
	ord_memberchk(Y, ValuesX).

    test_unify_sd_sd(AttrY, _AttrX) :-
	var(AttrY).
    test_unify_sd_sd(sd{dom:ValuesY}, sd{dom:ValuesX}) :-
	ord_intersect(ValuesY, ValuesX).


% compare_instances handler
% naming convention for the auxiliary predicates:
%	_const	nonvar
%	_free	variable without any attributes
%	_meta	variable with (possibly empty) attributes
%	_any	free;meta;const
%	_attr	(possibly uninstantiated) attribute

compare_instances_sd(Res, _X{AttrX}, Y) :- -?->
	compare_instances_attr_any(Res, AttrX, Y).
compare_instances_sd(Res, X, _Y{AttrY}) :- -?-> free(X),
	compare_instances_free_attr(Res, AttrY).	% Y must be meta!
compare_instances_sd(Res, X, _Y{AttrY}) :- -?-> nonvar(X),
	compare_instances_const_attr(Res, X, AttrY).	% Y must be meta!

    compare_instances_attr_any(Res, AttrX, Y{AttrY}) :- -?->
	compare_instances_attr_attr(Res, AttrX, AttrY).
    compare_instances_attr_any(Res, AttrX, Y) :- free(Y),
	compare_instances_attr_free(Res, AttrX).
    compare_instances_attr_any(Res, AttrX, Y) :- nonvar(Y),
	compare_instances_attr_const(Res, AttrX, Y).

    compare_instances_attr_free(Res, AttrX) :- var(AttrX),
	Res = (=).
    compare_instances_attr_free(Res, AttrX) :- nonvar(AttrX),
	Res = (<).

    compare_instances_free_attr(Res, AttrY) :- var(AttrY),
	Res = (=).
    compare_instances_free_attr(Res, AttrY) :- nonvar(AttrY),
	Res = (>).

    compare_instances_attr_attr(Res, AttrX, AttrY) :- var(AttrX),
	compare_instances_free_attr(Res, AttrY).
    compare_instances_attr_attr(Res, AttrX, AttrY) :- nonvar(AttrX),
	compare_instances_iattr_attr(Res, AttrX, AttrY).

    compare_instances_iattr_attr(Res, _AttrX, AttrY) :- var(AttrY), !,
	Res = (<).
    compare_instances_iattr_attr(Res,
		sd{dom:ValuesX}, sd{dom:ValuesY}) ?-
	ord_compare(Res, ValuesX, ValuesY).

    compare_instances_const_attr(Res, _X, AttrY) :- var(AttrY), !,
	Res = (<).
    compare_instances_const_attr(Res, X, sd{dom:ValuesY}) ?-
	ord_memberchk(X, ValuesY),
	Res = (<).

    compare_instances_attr_const(Res, AttrX, _Y) :- var(AttrX), !,
	Res = (>).
    compare_instances_attr_const(Res, sd{dom:ValuesX}, Y) ?-
	ord_memberchk(Y, ValuesX),
	Res = (>).


% suspensions handler

suspensions_sd(_{sd{any:Susps}}, Ss, Ss0) ?- !,
	Ss = [Susps|Ss0].
suspensions_sd(_, Ss, Ss).


% delayed_goals_number handler

delayed_goals_number_sd(_{sd{any:Susps}}, N) ?- !,
	( foreach(Susp,Susps), fromto(0,N0,N1,N) do
	    (is_suspension(Susp) -> N1 is N0+1 ; N1 = N0 )
	).
delayed_goals_number_sd(_, 0).


% most specific generalisation

:- export(msg/3).
msg(X, _Y, _G) :- free(X), !.
msg(X{XAttr}, Y, G) ?- !,
	msg_attr_any(XAttr, Y, G).
msg(X, Y, G) :- atomic(X), !,
	msg_const_any(X, Y, G).
msg(_X, _Y, _G).			% X has no domain (not atomic)

    msg_attr_any(_XAttr, Y, _G) :- free(Y), !.
    msg_attr_any(XAttr, Y{YAttr}, G) ?- !,
	msg_attr_attr(XAttr, YAttr, G).
    msg_attr_any(XAttr, Y, G) :- atomic(Y), !,
	msg_const_attr(Y, XAttr, G).
    msg_attr_any(_XAttr, _Y, _G).	% Y has no domain (not atomic)

    msg_const_any(_X, Y, _G) :- free(Y), !.
    msg_const_any(X, Y{YAttr}, G) ?- !,
	msg_const_attr(X, YAttr, G).
    msg_const_any(X, Y, G) :- atomic(Y), !,
	list_to_ord_set([X,Y], Values),
	has_values(G, Values).
    msg_const_any(_X, _Y, _G).		% Y has no domain (not atomic)

    msg_const_attr(_X, YAttr, _G) :- var(YAttr).
    msg_const_attr(X, sd{dom:ValuesY}, G) ?-
	ord_insert(ValuesY, X, Values),
	has_values(G, Values).

    msg_attr_attr(sd{dom:ValuesX}, sd{dom:ValuesY}, G) ?-
	ord_union(ValuesX, ValuesY, Values),
	has_values(G, Values).


%----------------------------------------------------------------------
% Constraints
%----------------------------------------------------------------------

:- export (&=)/2.
X &= Y :-
	var(X), var(Y),
	!,
	( get_var_attr(X, AttrX) ->
	    AttrX = sd{dom:Xs},
	    ( get_var_attr(Y, AttrY) ->
		AttrY = sd{dom:Ys},
		ord_intersection(Xs, Ys, Common, OnlyX, OnlyY),
		Common = [Val|Vals],		% may fail with Common = []
		setarg(dom of sd, AttrX, Common),
		setarg(dom of sd, AttrY, Common),
		( Vals = [] ->
		    X = Val, Y = Val		% instantiate both
		;
		    ( OnlyX=[] -> true ; schedule_suspensions(any of sd, AttrX) ),
		    ( OnlyY=[] -> true ; schedule_suspensions(any of sd, AttrY) ),
		    % wait for more domain information
		    suspend(X &= Y, 2, [X-Y->any]),
		    wake
		)
	    ;
		domainless_var_has_values(Y, Xs),
		X &= Y
	    )
	;
	    ( get_var_attr(Y, AttrY) ->
		AttrY = sd{dom:Ys},
		domainless_var_has_values(X, Ys),
		X &= Y
	    ;
		% wait for at least one variable to get a domain
	    	suspend(X &= Y, 2, X-Y->constrained)
	    )
	).
X &= X.


:- export (&=)/3.
&=(X, Y, Bool) :-
	var(Bool),
	!,
	( atomic(X), atomic(Y) ->
	    ( X == Y ->
		Bool = 1
	    ;
		Bool = 0
	    )
	; get_domain_as_list(X, Xs) ->
	    ( get_domain_as_list(Y, Ys) ->
	    	( ord_disjoint(Xs, Ys) ->
		    Bool = 0
		;
		    % wait for more domain information
		    suspend(&=(X, Y, Bool), 2, [X-Y->any,Bool->inst])
		)
	    ;
		var(Y),		% wait for Y to get a domain
		suspend(&=(X, Y, Bool), 2, [Y->constrained,Bool->inst])
	    )
	;
	    var(X),		% wait for X to get a domain
	    suspend(&=(X, Y, Bool), 2, [X->constrained,Bool->inst])
	).
&=(X, Y, 0) :-
	X &\= Y.
&=(X, Y, 1) :-
	X &= Y.


:- export (&\=)/2.
X &\= Y :-
	( atomic(X) ->
	    ( atomic(Y) ->
	    	X \= Y
	    ; is_solver_var(Y) ->
		exclude_value(X, Y)
	    ;
		var(Y),		% wait for Y to get a domain
		suspend(&\=(X, Y), 2, [Y->constrained])
	    )
	; is_solver_var(X) ->
	    ( atomic(Y) ->
		exclude_value(Y, X)
	    ;
		var(Y),		% wait for an instantiation
		suspend(&\=(X, Y), 2, [X-Y->inst])
	    )
	; var(X),
	    ( atomic(Y) ->
		var(X),		% wait for X to get a domain
		suspend(&\=(X, Y), 2, [X->constrained])
	    ;
		% wait for an instantiation
		suspend(&\=(X, Y), 2, [X-Y->inst])
	    )
	).


:- export (&\=)/3.
&\=(X, Y, Bool) :-
	var(Bool),
	!,
	( atomic(X), atomic(Y) ->
	    ( X == Y ->
		Bool = 0
	    ;
		Bool = 1
	    )
	; get_domain_as_list(X, Xs) ->
	    ( get_domain_as_list(Y, Ys) ->
	    	( ord_disjoint(Xs, Ys) ->
		    Bool = 1
		;
		    % wait for more domain information
		    suspend(&\=(X, Y, Bool), 2, [X-Y->any,Bool->inst])
		)
	    ;
		var(Y),		% wait for Y to get a domain
		suspend(&\=(X, Y, Bool), 2, [Y->constrained,Bool->inst])
	    )
	;
	    var(X),		% wait for X to get a domain
	    suspend(&\=(X, Y, Bool), 2, [X->constrained,Bool->inst])
	).
&\=(X, Y, 0) :-
	X &= Y.
&\=(X, Y, 1) :-
	X &\= Y.


:- export alldifferent/1.
alldifferent(Xs) :-
	list_of_solver_type(Xs),
	!,
	alldifferent(Xs, []),
	wake.
alldifferent(Xs) :-
	error(4, alldifferent(Xs)).

    alldifferent([], _Left).
    alldifferent([X | Right], Left) :-
	outof(X, Left, Right),
	alldifferent(Right, [X | Left]).

outof(X, Left, Right) :-
	var(X), !,
	suspend(outof(X, Left, Right), 3, [X->inst]).
outof(X, Left, Right) :-
	exclude_list(X, Left),
	exclude_list(X, Right).

    exclude_list(_X, []).
    exclude_list(X, [X1 | Xs]) :-
	exclude_value(X, X1),
	exclude_list(X, Xs).



%----------------------------------------------------------------------
% Documentation
%----------------------------------------------------------------------

:- comment(desc, html("
    <H4>Overview</H4>
    <P>
    This is a simple library implementing variables and constraints over
    atomic values.  Its main purpose is for first experiments with constraint
    solving.  Moreover, those interested in writing their own constraint
    solvers can use the source code of this library as a starting point.
    <H4>Domains</H4>
    Domains are declared by giving a list of possible values for a variable,
    e.g.
    <PRE>
    	?- X &amp;:: [red, green, blue].
	X = X{[blue, green, red]}
	Yes (0.00s cpu)
    </PRE>
    A variable that has been given a domain can only be instantiated to
    values from this domain. Any attempt to instantiate the variable to
    a non-domain value will cause failure:
    <PRE>
    	?- X &amp;:: [red, green, blue], X = red.
	X = red
	Yes (0.00s cpu)
    	?- X &amp;:: [red, green, blue], X = yellow.
	No (0.00s cpu)
    </PRE>
    <H4>Basic Constraints</H4>
    There are only two basic constraints, equality and disequality:
    <DL>
    <DT>X &amp;= Y</DT><DD>X is the same as Y</DD>
    <DT>X &amp;\\= Y</DT><DD>X is different from Y</DD>
    </DL>
    Both constraints exist in a reified form:
    <DL>
    <DT>&amp;=(X,Y,Bool)</DT><DD>Bool is the truth value (0/1) of X &amp;= Y</DD>
    <DT>&amp;\\=(X,Y,Bool)</DT><DD>Bool is the truth value (0/1) of X &amp;\\= Y</DD>
    </DL>
    </PRE>
    <H4>Global Constraints</H4>
    One derived, global constraint is implemented:
    <DL>
    <DT>alldifferent(List)</DT><DD>All list elements are different</DD>
    </DL>
    <H4>Search</H4>
    Domain variables can be instantiated to their domain values using
    <DL>
    <DT>indomain(X)</DT><DD>enumerate values of X</DD>
    <DT>labeling(Xs)</DT><DD>enumerate values of all elements of list Xs</DD>
    </DL>
    ")).


:- comment((&::)/2, [
    summary:"All elements of Vars have a value in the domain Domain",
    args:["Vars":"Variable or atomic value, list of them, or submatrix of them",
    	"Domain":"List of atomic values, or domain name (atom)"],
    template:"?Vars &:: +Domain",
    see_also:[indomain/1,get_domain_as_list/2,get_domain_size/2,(domain)/1],
    eg:"
    ?- X &:: [mo, tu, we, th, fr, sa, su].
    X = X{[fr, mo, sa, su, th, tu, we]}
    Yes (0.00s cpu)

    ?- [X,Y,we] &:: [mo, tu, we, th, fr, sa, su].
    X = X{[fr, mo, sa, su, th, tu, we]}
    Y = Y{[fr, mo, sa, su, th, tu, we]}
    Yes (0.00s cpu)

    ?- dim(M, [3]), M[1..3] &:: [a,e,i,o,u].
    M = [](_354{[a,e,i,o,u]}, _364{[a,e,i,o,u]}, _374{[a,e,i,o,u]})
    Yes (0.00s cpu)

    ?- [X,Y] &:: [we,fr,su].
    X = X{[we, fr, su]}
    Y = Y{[we, fr, su]}

    ?- X &:: [].
    No (0.00s cpu)

    % Using a named, pre-declared domain
    ?- local domain(colour(r, g, b)).
    Yes (0.01s cpu)

    ?- X &:: colour.
    X = X{[b, g, r]}
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	Constrains a variable (or a list of variables, or a submatrix of
	variables), to have the domain Domain.  The domain is specified
	simply as an unordered list of atomic values.  A domain variable
	can only be instantiated to values within its domain.
	</P><P>
	For compatibility with library(ic_symbolic), domains can also be
	pre-declared using domain/1, and domain variables then declared
	by just giving the domain name rather than the list of values.
	Note however that even in this case the domain is considered
	unordered, i.e. the order of domain elements in the declaration
	is not important for the purposes of this library.
	</P><P>
	Note that, on the left hand side of &amp;::/2, the atom [] is not
	interpreted as the empty list (of variables or values), but as
	a single atomic value, i.e. a potential domain element.
	This is in keeping with the behaviour of the ic_symbolic
	library, but in contrast with the behaviour of numeric solvers
	such as ic.
	</P>
")]).

:- comment((&=)/2, [
    summary:"X is the same atomic value as Y",
    args:["X":"Variable or atomic value",
    	"Y":"Variable or atomic value"],
    template:"?X &= ?Y",
    see_also:[(&=)/3,(&\=)/2],
    eg:"
    ?- X &:: [red,green,blue], X &= red.
    X = red
    Yes (0.00s cpu)

    ?- X &:: [red,green,blue], X &= yellow.
    No (0.00s cpu)

    ?- [X, Y] &:: [red,green,blue], X &= Y.
    X = X{[blue, green, red]}
    Y = Y{[blue, green, red]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X &:: [red,green,blue], X &= Y.
    X = X{[blue, green, red]}
    Y = Y{[blue, green, red]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X &:: [red,green,blue], Y &:: [blue,yellow,green], X &= Y.
    X = X{[blue, green]}
    Y = Y{[blue, green]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X &:: [red, green, blue], Y &:: [blue, yellow, brown], X &= Y.
    X = blue
    Y = blue
    Yes (0.00s cpu)

    ?- red &= red.
    Yes (0.00s cpu)

    ?- X &= red.
    X = red
    Yes (0.00s cpu)
    ",
    desc:html("
	Constrains X and Y to be the same.  This is very similar to unifying
	X and Y, except in the case where X and Y are both variables. While
	unification turns the two variables into one, this constraint preserves
	the separate variables but constrains them to be equal.
")]).


:- comment((&\=)/2, [
    summary:"X is different from Y",
    args:["X":"Variable or atomic value",
    	"Y":"Variable or atomic value"],
    template:"?X &\\= ?Y",
    see_also:[(&\=)/3,(&=)/2],
    eg:"
    ?- X &:: [red,green,blue], X &\\= red.
    X = X{[blue, green]}
    Yes (0.00s cpu)

    ?- X &:: [red, green, blue], X &\\= yellow.
    X = X{[blue, green, red]}
    Yes (0.00s cpu)

    ?- X &:: [red,green], X &\\= red.
    X = green
    Yes (0.00s cpu)

    ?- X &:: [red,green], X &\\= red, X &\\= green.
    No (0.00s cpu)

    ?- X &\\= red.
    X = X
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X &\\= red, X &:: [red,green].
    X = green
    Yes (0.00s cpu)

    ?- [X, Y] &:: [red, green, blue], X &\\= Y.
    X = X{[blue, green, red]}
    Y = Y{[blue, green, red]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- X &:: [red, green], Y &:: [blue, brown], X &\\= Y.
    X = X{[green, red]}
    Y = Y{[blue, brown]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- red &\\= green.
    Yes (0.00s cpu)

    ?- red &\\= red.
    No (0.00s cpu)
    ",
    desc:html("
	Constrains X and Y to be different. Operationally, the predicate
	delays until one side is instantiated and the other has a domain.
	The instantiated value is then removed from the domain.
")]).


:- comment((&=)/3, [
    summary:"Reified version of X &= Y",
    see_also:[(&=)/2],
    args:["X":"Variable or atomic value",
    	"Y":"Variable or atomic value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&\=)/3, [
    summary:"Reified version of X &\\= Y",
    see_also:[(&\=)/2],
    args:["X":"Variable or atomic value",
    	"Y":"Variable or atomic value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment(alldifferent/1, [
    summary:"All elements of List are different",
    args:["List":"List of domain variables or atomic values"],
    see_also:[(&\=)/2],
    eg:"
    ?- [X, Y, Z] &:: [a,b,c,d], alldifferent([X,Y,Z]), X = a.
    X = a
    Y = Y{[b, c, d]}
    Z = Z{[b, c, d]}
    There are 2 delayed goals.
    Yes (0.00s cpu)

    ?- X &:: [a,b,c], alldifferent([a,b,X]).
    X = c
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	Constrains all list elements to be different atomic values.
	At call time, the list elements must already have domains or
	be instantiated.
	</P><P>
	Operationally, the predicate delays until list elements become
	instatiated, and removes the corresponding values from the domains
	of the other list elements.
	</P>
")]).


:- comment(indomain/1, [
    summary:"Nondeterministically instantiate to domain values",
    args:["X":"Domain variable or value"],
    see_also:[(&::)/2],
    eg:"
    ?- X &:: [mo, tu, we, th, fr, sa, su], indomain(X).
    X = fr
    Yes (0.00s cpu, solution 1, maybe more)
    X = mo
    Yes (0.05s cpu, solution 2, maybe more)
    X = sa
    Yes (0.06s cpu, solution 3, maybe more)
    X = su
    Yes (0.06s cpu, solution 4, maybe more)
    X = th
    Yes (0.06s cpu, solution 5, maybe more)
    X = tu
    Yes (0.07s cpu, solution 6, maybe more)
    X = we
    Yes (0.08s cpu, solution 7)

    ?- indomain(we).
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	Nondeterministically instantiates a domain variable to its domain
	values. The order of enumeration is in increasing term order (i.e.
	alphabetic for atoms).
    </P>")]).


:- comment(msg/3, [
    summary:"MSG is the most specific generalisation of X and Y "
	"representable with domain variables from this library",
    args:["X":"Any term or variable",
	"Y":"Any term or variable",
	"MSG":"A domain variable or constant (output)"],
    amode:msg(?,?,-),
    fail_if:"None",
    see_also:[_:msg/3, library(propia), (&::)/2],
    eg:"
    ?- msg(we, fr, Z).
    Z = Z{[we, fr]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], msg(X, we, Z).
    X = X{[sa, su]}
    Z = Z{[we, sa, su]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], Y &:: [mo, tu, we], msg(X, Y, Z).
    X = X{[sa, su]}
    Y = Y{[mo, tu, we]}
    Z = Z{[mo, tu, we, sa, su]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], msg(X, _, Z).
    X = X{[sa, su]}
    Z = Z
    Yes (0.01s cpu)

    ?- msg(we, we, X).
    X = we
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	This predicate computes the most specific generalisation of X and Y
	which can be represented using this library's domain variables.
	</P><P>
	If X and Y are domain variables (or atomic constants), then MSG
	will be unified with a new domain variable whose domain
	consists of the union of the domain elements of X and Y.
	If the domain union contains only a single value, the result
	is this single value.
	</P><P>
	If X or Y are free (unconstrained) variables, then the result will
	also be a free (unconstrained) variable.
    </P>")]).

:- comment(get_domain_as_list/2, [
    summary:"Retrieves the domain of a variable (or value) as a list of values",
    amode:get_domain_as_list(?,-),
    args:["Term":"Domain variable or atomic value",
	"List":"List of atomic values"],
    see_also:[(&::)/2,indomain/1,get_domain_size/2],
    desc:html("
	Retrieves the domain of a variable (or value) as a list of values.
	The list is an ordered set in the sense of library(ordset), i.e.
	a duplicate-free list in ascending term order.
")]).

:- comment(get_domain_size/2, [
    summary:"Gives the size of the domain of a variable (or value)",
    amode:get_domain_size(?,-),
    args:["Term":"Domain variable or atomic value",
	"Size":"Variable or integer"],
    see_also:[(&::)/2,get_domain_as_list/2,deleteff/3],
    desc:html("
	Retrieves the domain size of a domain variable (or value).
	It Term is instantiated, Size is 1.
")]).

:- comment(is_solver_var/1, [
    summary:"The argument is a domain variable",
    amode:is_solver_var(?),
    args:["Term":"A term"],
    see_also:[(&::)/2,is_solver_type/1],
    desc:html("
	Tests if the argument is a domain variable from this library.
")]).

:- comment(is_exact_solver_var/1, [
    summary:"The argument is a domain variable",
    amode:is_exact_solver_var(?),
    args:["Term":"A term"],
    see_also:[is_solver_var/1],
    desc:html("
	Tests if the argument is a domain variable from this library.
	An alias for is_solver_var/1, used by lib(propia).
")]).

:- comment(is_solver_type/1, [
    summary:"The argument is a domain variable or atomic constant",
    amode:is_solver_type(?),
    args:["Term":"A term"],
    see_also:[(&::)/2,is_solver_var/1],
    desc:html("
	Tests if the argument is a domain variable from this library
	or a valid domain value (atomic term).
")]).

:- comment(labeling/1, [
    summary:"Instantiate all domain variables in a list to domain values",
    amode:labeling(+),
    args:["Term":"A list of domain variables or atomic terms"],
    see_also:[(&::)/2,indomain/1],
    desc:html("
	Instantiate all domain variables in a list to their domain values.
	Alternative instantiations are tried on backtracking. This predicate
	simply calls indomain/1 on all variable in the given list.
")]).

:- comment(deleteff/3, [
    summary:"Pick minimum domain size element from a list (first fail principle)",
    amode:deleteff(-,+,-),
    args:["Min":"Smallest list element in terms of domain size",
    	"List":"List of domain variables or constants",
    	"Rest":"Remaining list without smallest element"],
    see_also:[get_domain_size/2],
    fail_if:"List is empty",
    desc:html("
	Extracts from a list of domain variables (or constants) the first
	element with the smallest domain. This element is returned as Min,
	and the remaining list without this element is returned as Rest.
	This predicate is useful for implementing the first-fail-principle
	in a search procedure, i.e. labeling the variable with the smallest
	domain first. E.g.
	<PRE>
	labelingff(List) :-
		( deleteff(X, List, Rest) ->
		    indomain(X),
		    labelingff(Rest)
		;
		    true
		).
	</PRE>
")]).


