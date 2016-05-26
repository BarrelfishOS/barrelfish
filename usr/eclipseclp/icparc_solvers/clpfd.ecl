% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Copyright:	This file is in the public domain
% Version:	$Id: clpfd.ecl,v 1.1 2015/03/29 22:19:40 jschimpf Exp $
% Description:	Compatibility package for M.Triska's clpfd library
% ----------------------------------------------------------------------

:- module(clpfd).

:- comment(categories, ["Compatibility","Constraints"]).
:- comment(summary, "Compatibility package for M.Triska's clpfd library").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "This file is in the public domain").
:- comment(date, "$Date: 2015/03/29 22:19:40 $").
:- comment(desc, html("<P>
    This library implements the syntax of M.Triska's clpfd library
    (and to some degree SICStus's clpfd library) on top of ECLiPSe's
    library(ic).  Its features can be freely mixed with library(ic).
</P><P>
    Differences:
    <UL>
    <LI>library(ic) uses floating point arithmetic, while Triska's clpfd uses
    bignums, which implies that the behaviour with large integers will differ</LI>
    <LI>more generally, propagation details and performance may differ</LI>
    <LI>(#\\)/2 is currently not supported because of name clash</LI>
    <LI>TODO: automaton, circuit, tuples_in constraints</LI>
    </UL>
</P>")).
:- comment(see_also, [library(ic),library(ic_global),library(ic_global_gac),library(branch_and_bound)]).

:- lib(ic).
:- reexport
	(#=)/2,
	(#\=)/2,
	(#>=)/2,
	(#=<)/2,
	(#>)/2,
	(#<)/2,
	indomain/1
   from ic.

:- lib(branch_and_bound).

% ----------------------------------------------------------------------

:- export op(450,xfx,..).	% 600 in ic!

% support ?(X)
:- export op(250,yfx,?).
:- export (?)/2.
:- comment((?)/2, hidden).
?(X, X).

domain_expr(X, _, _) :- var(X), !, fail.
domain_expr({X}, [X|Ds], Ds) :- integer(X).	% SICStus feature
domain_expr(X, [X|Ds], Ds) :- integer(X).
domain_expr(L..H, [L1..H1|Ds], Ds) :-
	( L==inf, L1 = -inf ; integer(L), L1 = L ),
	( H==sup, H1 =  inf ; integer(H), H1 = H ).
domain_expr(E1\/E2, Ds, Ds0) :-
	domain_expr(E1, Ds, Ds1),
	domain_expr(E2, Ds1, Ds0).

% ----------------------------------------------------------------------

:- export op(700,xfx,in).
:- export (in)/2.
:- comment((in)/2, [ template:"?Var in +Domain", summary:"",see_also:[ic:(#::)/2]]).
X in D :-
	(var(X);integer(X)),
	domain_expr(D, Ds, []),
	X #:: Ds.

:- export op(700,xfx,ins).
:- export (ins)/2.
:- comment((ins)/2, [ template:"?Vars ins +Domain", summary:"",see_also:[ic:(#::)/2]]).
Xs ins D :-
	is_list(Xs),
	domain_expr(D, Ds, []),
	Xs #:: Ds.

% ----------------------------------------------------------------------

:- export op(760,yfx,#<==>).
:- export (#<==>)/2.
:- comment((#<==>)/2, [template:"X #<==> Y", summary:""]).
X #<==> Y :-
	B #= eval(X),
	B #= eval(Y),
	B #:: 0..1.

:- export (#<==>)/3.
:- comment((#<==>)/3, hidden).
#<==>(X,Y,B) :-
	BX #= eval(X),
	BY #= eval(Y),
	[BX,BY] #:: 0..1,
	#=(BX, BY, B).

/* commented out because of name clash with reified (#\)/1.
:- export op(730,yfx,#\).
:- export (#\)/2.
X #\ Y :-
	BX #= eval(X),
	BY #= eval(Y),
	[BX,BY] #:: 0..1,
	BX #\= BY.

:- export (#\)/3.
#\(X,Y,B) :-
	BX #= eval(X),
	BY #= eval(Y),
	[BX,BY] #:: 0..1,
	#\=(BX, BY, B).
*/

:- export op(750,yfx,#<==).
:- export (#<==)/2.
:- comment((#<==)/2, [template:"X #<== Y", summary:""]).
X #<== Y :-
	eval(Y) => eval(X).

:- export (#<==)/3.
:- comment((#<==)/3, hidden).
#<==(X, Y, B) :-
	=>(eval(Y), eval(X), B).

:- export op(750,yfx,#==>).
:- export (#==>)/2.
:- comment((#==>)/2, [template:"X #==> Y", summary:""]).
X #==> Y :-
	eval(X) => eval(Y).

:- export (#==>)/3.
:- comment((#==>)/3, hidden).
#==>(X, Y, B) :-
	=>(eval(X), eval(Y), B).

:- export op(720,yfx,#/\).
:- export (#/\)/2.
:- comment((#/\)/2, [template:"X #/\\ Y", summary:""]).
X #/\ Y :-
	eval(X) and eval(Y).

:- export (#/\)/3.
:- comment((#/\)/3, hidden).
#/\(X, Y, B) :-
	and(eval(X), eval(Y), B).

:- export op(740,yfx,#\/).
:- export (#\/)/2.
:- comment((#\/)/2, [template:"X #\\/ Y", summary:""]).
X #\/ Y :-
	eval(X) or eval(Y).

:- export (#\/)/3.
:- comment((#\/)/3, hidden).
#\/(X, Y, B) :-
	or(eval(X), eval(Y), B).

:- export op(710,fy,#\).
:- export (#\)/1.
:- comment((#\)/1, [template:"#\\ Y", summary:""]).
#\ X :-
	neg eval(X).

:- export (#\)/2.
:- comment((#\)/2, hidden).
#\(X, B) :-
	neg(eval(X), B).

% ----------------------------------------------------------------------

:- export all_different/1.
:- comment(all_different/1, [template:"all_different(+Vars)", summary:"",see_also:[ic:alldifferent/1]]).
all_different(Xs) :-
	ic:alldifferent(Xs).

:- export all_distinct/1.
:- comment(all_distinct/1, [template:"all_distinct(+Vars)", summary:"",see_also:[ic_global:alldifferent/1]]).
all_distinct(Xs) :-
	ic_global:alldifferent(Xs).

:- export sum/3.
:- comment(sum/3, [ template:"sum(+Vars,+Rel,?Expr)", summary:"" ]).
sum(Vars, #=,  Expr) ?- sum(Vars) #=  eval(Expr).
sum(Vars, #\=, Expr) ?- sum(Vars) #\= eval(Expr).
sum(Vars, #>=, Expr) ?- sum(Vars) #>= eval(Expr).
sum(Vars, #=<, Expr) ?- sum(Vars) #=< eval(Expr).
sum(Vars, #>,  Expr) ?- sum(Vars) #>  eval(Expr).
sum(Vars, #<,  Expr) ?- sum(Vars) #<  eval(Expr).

:- export scalar_product/4.
:- comment(scalar_product/4, [template:"scalar_product(+Csts,+Vars,+Rel,?Expr)", summary:"" ]).
scalar_product(Cs, Vs, #=,  Expr) :- Cs*Vs #=  eval(Expr).
scalar_product(Cs, Vs, #\=, Expr) :- Cs*Vs #\= eval(Expr).
scalar_product(Cs, Vs, #>=, Expr) :- Cs*Vs #>= eval(Expr).
scalar_product(Cs, Vs, #=<, Expr) :- Cs*Vs #=< eval(Expr).
scalar_product(Cs, Vs, #>,  Expr) :- Cs*Vs #>  eval(Expr).
scalar_product(Cs, Vs, #<,  Expr) :- Cs*Vs #<  eval(Expr).

:- export lex_chain/1.
:- comment(lex_chain/1, [template:"lex_chain(+Lists)", summary:"",see_also:[ic_global:lex_le/2]]).
lex_chain([]).
lex_chain([Xs|YsZs]) :-
	( YsZs = [Ys|_] ->
	    ic_global:lex_le(Xs, Ys),
	    lex_chain(YsZs)
	;
	    true
	).

:- export zcompare/3.
:- comment(zcompare/3, [template:"zcompare(?Rel,?X,?Y)", summary:"" ]).
zcompare(R, X, Y) :-
	SGN #= (X #> Y) - (X #< Y),
	sgn_rel(SGN, R).

    delay sgn_rel(SGN,R) if var(SGN),var(R).
    sgn_rel(-1, <).
    sgn_rel( 0, =).
    sgn_rel( 1, >).


:- export serialized/2.
:- comment(serialized/2, [template:"serialized(+Starts,+Durations)", summary:"",see_also:[ic_edge_finder:disjunctive/2]]).
serialized(Starts, Durs) :-
	ic_edge_finder:disjunctive(Starts, Durs).

:- export cumulative/2.
:- comment(cumulative/2, [template:"cumulative(+Tasks,+Options)", summary:"",see_also:[ic_edge_finder:cumulative/4]]).
cumulative(Tasks, Options) :-
	( memberchk(limit(Limit), Options) -> true ; true ),
	(
	    foreach(task(S, D, E, U, _Id), Tasks),
	    foreach(S, Starts), foreach(D, Durs), foreach(U, Usages)
	do
	    E #= S+D
	),
	ic_edge_finder:cumulative(Starts, Durs, Usages, Limit).

:- export chain/2.
:- comment(chain/2, [template:"chain(+Vars,+Relation)", summary:"" ]).
chain([], _) :- !.
chain(Zs, Relation) :-
	( fromto(Zs,[X,Y|Zs],[Y|Zs],[_]), param(Relation) do
	    call(Relation, X, Y)
	).

:- export global_cardinality/2.
:- comment(global_cardinality/2, [template:"global_cardinality(+Vars,+Pairs)", summary:"",see_also:[ic_global_gac:gcc/2]]).
global_cardinality(Xs, Pairs) :-
	global_cardinality(Xs, Pairs, []).

:- export global_cardinality/3.
:- comment(global_cardinality/3, [template:"global_cardinality(+Vars,+Pairs,+Options)", summary:"",see_also:[ic_global_gac:gcc/2]]).
global_cardinality(Xs, Pairs, Options) :-
	( foreach(Val-_,Pairs), foreach(Val,Vals) do true ),
	( memberchk(consistency(value), Options) ->
	    Xs #:: Vals,
	    ( foreach(Val-Occ,Pairs), param(Xs) do
	        ic_global:occurrences(Val, Xs, Occ)
	    )
	;
	    Xs #:: Vals,	% while gcc does not do that internally
	    ( foreach(Val-Occ,Pairs), foreach(gcc(L,H,Val),Bounds), param(Xs) do
	        get_finite_integer_bounds(Occ,L0,H),
		L is max(0,L0), L =< H,
		( nonvar(Occ) -> true ;
		   % while gcc does not handle variable OCC directly:
		   ic_global:occurrences(Val, Xs, Occ)
		)
	    ),
	    ic_global_gac:gcc(Bounds, Xs)
	),
	( memberchk(cost(Cost,Matrix), Options) ->
	    ( foreach(Row,Matrix), foreach(X,Xs), foreach(C,Cs) do
		element(X, Row, C)
	    ),
	    Cost is sum(Cs)
	;
	    true
	).


% ----------------------------------------------------------------------

:- export label/1.
:- comment(label/1, [template:"label(+Vars)", summary:"" ]).
label(Xs) :-
	labeling(Xs).

:- export labeling/2.
:- comment(labeling/2, [template:"labeling(+Options, +Vars)", summary:"",see_also:[ic:search/6,library(branch_and_bound)]]).
labeling(Options, Xs) :-
	is_list(Options),
	(
	    foreach(Option,Options),
	    fromto(Objs,Objs1,Objs2,[]),
	    param(VarSel,Order,Branching)
	do
	    ( optimization_option(Option, Cost) ->
		Objs1 = [Cost|Objs2]
	    ;
		atom(Option),
		labeling_option(Option, VarSel, Order, Branching),
		Objs1 = Objs2
	    )
	),
	varsel_default(VarSel),
	map_choice(Order, Branching, Choice),
	optimize(Objs, Xs, VarSel, Choice).

optimize([], Xs, VarSel, Choice) :-
	search(Xs, 0, VarSel, Choice, complete, []).
optimize([Cost|Objs], Xs, VarSel, Choice) :-
	% find the optimum cost
	bb_min(search(Xs, 0, VarSel, Choice, complete, []),
		Cost, [], [], Best,
		bb_options{strategy:restart,
			report_success:true/0,
			report_failure:true/0}),
	(
	    Cost = Best,
	    optimize(Objs, Xs, VarSel, Choice)
	;
	    Cost #> Best,
	    optimize([Cost|Objs], Xs, VarSel, Choice)
	).

    optimization_option(min(E),	Cost) ?- var(E) -> Cost=E ; Cost #= eval(E).
    optimization_option(max(E),	Cost) ?- Cost #= -eval(E).

    labeling_option(leftmost,	input_order, _, _).
    labeling_option(ff,		first_fail, _, _).
    labeling_option(ffc,	most_constrained, _, _).
    labeling_option(min,	smallest, _, _).
    labeling_option(max,	largest, _, _).
    labeling_option(up,		_, up, _).
    labeling_option(down,	_, down, _).
    labeling_option(step,	_, _, step).
    labeling_option(enum,	_, _, enum).
    labeling_option(bisect,	_, _, bisect).

    map_choice(down, enum,   _) ?- !, throw(error(unsupported(down+enum),_)).
    map_choice(_up,  enum,   C) ?- !, C=indomain.
    map_choice(down, bisect, C) ?- !, C=indomain_reverse_split.
    map_choice(_up,  bisect, C) ?- !, C=indomain_split.
    map_choice(down, step,   C) ?- !, C=indomain_max.
    map_choice(_up, _step,   C) ?- !, C=indomain_min.

    varsel_default(input_order) :- !.
    varsel_default(_).

% ----------------------------------------------------------------------

:- export fd_var/1.
:- comment(fd_var/1, [template:"fd_var(?Var)", summary:"",see_also:[ic:is_solver_var/1]]).
fd_var(X) :- is_solver_var(X).

:- export fd_inf/2.
:- comment(fd_inf/2, [template:"fd_inf(?Var,-Min)", summary:"",see_also:[ic:get_min/2]]).
fd_inf(X, M) :- get_min(X, M).

:- export fd_sup/2.
:- comment(fd_sup/2, [template:"fd_sup(?Var,-Max)", summary:"",see_also:[ic:get_max/2]]).
fd_sup(X, M) :- get_max(X, M).

:- export fd_size/2.
:- comment(fd_size/2, [template:"fd_size(?Var,-Size)", summary:"",see_also:[ic:get_domain_size/2]]).
fd_size(X, S) :- get_domain_size(X, S).

:- export fd_dom/2.
:- comment(fd_dom/2, [template:"fd_dom(?Var,-Dom)", summary:"",see_also:[ic:get_domain/2]]).
fd_dom(X, D) :-
	get_domain(X, [R0|Rs]),
	( foreach(R,Rs), fromto(R0,R1,R1\/R,D) do true ).

:- export transpose/2.
:- comment(transpose/2, [template:"transpose(+Rows,+Cols)", summary:"",see_also:[matrix_util:transpose/2]]).
transpose(Rows, Cols) :-
	matrix_util:transpose(Rows, Cols).
