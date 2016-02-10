% ----------------------------------------------------------------------
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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Thierry Le Provost, ECRC.
% Contributor(s): Mark Wallace, IC-Parc and ICL.
% 
% END LICENSE BLOCK
%
% System:       ECLiPSe Constraint Logic Programming System
% Version:      $Id: propia.pl,v 1.3 2009/07/16 09:11:25 jschimpf Exp $
%
% Description:          Propia
%
% Authors:      Thierry Le Provost, ECRC, 
%               Mark Wallace, IC-Parc and ICL
% ----------------------------------------------------------------------




:- coroutine.

:- module(propia).

:- comment(categories, ["Constraints","Techniques"]).
:- comment(summary, "The Generalised Propagation Library").
:- comment(author, "Thierry Le Provost, ECRC and Mark Wallace, IC-Parc and ICL").
:- comment(copyright, "1995-2006 Cisco Systems, Inc").
:- comment(date, "$Date: 2009/07/16 09:11:25 $").

:- comment(infers / 2, [
    summary:"Do generalized propagation over Goal according to the approximation Language.",
    template:"+Goal infers +Language",
    desc:html("\
	Used to turn an arbitrary, nondeterministic Goal into a
	(deterministic) constraint, whose inference power is
	determined by the approximation Language. This is done using the
	Generalised Propagation Method (see T. Le Provost and M. Wallace,
	Domain-Independent Propagation, Proceedings of the FGCS'92, Tokyo,
	June 1992).
	<P>
	The infers-predicate computes the most specific generalization
	of all the solutions of the Goal according to the approximation
	Language and delays if Goal has several non comparable solutions.
	<P>
	With the language 'most', Goal is bound to the most specific
	generalization of all its solutions.  This generalisation will
	depend what solvers are loaded - (e.g. ic, fd, sd or several of
	them).  If one of the solutions is not a variant of this
	answer, the goal is delayed until one of its variables is
	bound.  Note that the Goal may have an infinity of solutions.
	<P>
	Using other languages, an approximation of this most specific
	generalization can be computed.  With the 'unique' language,
	infers/2 instantiates the Goal only if it has a unique
	solution (or the first solution of Goal subsumes all the
	others).  With the 'consistent' language, infers/2 does not
	bind the Goal but only checks if Goal has at least one
	solution.
	<P>
	The name of a supported solver (ic, fd, ic_symbolic, sd, ...
	or a list of them) can also be used to specify the language.
	In this case, the infers-predicate computes the most specific
	generalisation expressible in terms of that solver's domain
	variables (e.g. intervals, finite domains, ...)
	<P>
	The language 'ac' implements generalised arc consistency on
	the table produced by computing all the (finitely) many
	solutions to the goal in advance. This requires that some solver
	implementing the element/3 constraint is loaded (fd, ic_global).
	"),
    args:["+Goal" : "Callable Term.",
    	"+Language" : "One of the terms most, unique, consistent, ac, or the name of a supported solver (fd, ic, sd, ic_symbolic)"],
    resat:"No.",
    fail_if:"Fails if Goal has no solution.",
    exceptions:[4 : "Goal or Language is not instantiated.", 6 : "Language is not a correct language.", 68 : "Goal is an undefined procedure."],
    eg:"
    Success:
	[eclipse]: member(X, [f(1), f(2)]) infers most.
        X = f(_g1)
        Delayed goals:
        member(f(_g1), [f(1), f(2)]) infers most
        yes.
	[eclipse]: [user].
	and(0, 0, 0).
        and(0, 1, 0).
	and(1, 0, 0).
	and(1, 1, 1).
	user       compiled traceable 528 bytes in 0.00 seconds
	yes.
	[eclipse]: and(0, X, Y).
	X = X
	Y = 0     More? (;)          % Prolog: two solutions
	X = 0
	Y = 0
	yes.
	[eclipse]: and(0, X, Y) infers most.
	X = X
	Y = 0
	yes.                         % Prolog + infers: one solution
	[eclipse]: [user].
	greater_than(succ(X), X).
	greater_than(succ(X), Y) :- greater_than(X, Y).
	user       compiled traceable 268 bytes in 0.00 seconds
	yes.
	[eclipse]: greater_than(X, zero).
	X = succ(zero)     More? (;) % Prolog: infinity of solutions
	...
	[eclipse]: greater_than(X, zero) infers most.
	X = succ(_g2)
	Delayed goals:
	    infers(greater_than(succ(_g2), zero), most, eclipse)
	yes.
        [eclipse]: lib(fd).
        ...
        [eclipse]: member(X, [f(1), f(2)]) infers most.
	X = f(_g3[1, 2])
	yes.
    Fail:
	[eclipse]: member(1, [2, 3]) infers consistent.
	no (more) solution.
    Error:
	Goal infers most. % Error 4
	true infers true.  % Error 6
    ",
    see_also:[]]).

:- export op(900, yfx, infers).

:- export (infers)/2.
:- export tr_propia/2.

:- export macro(myinfers/5, tr_propia/2, [goal,write]).
:- export macro(myinfers/6, tr_propia/2, [goal,write]).


:- tool((infers)/2, (infers)/3).

tr_propia(myinfers(G, _V, L, _M, _S), infers(G, L)).
tr_propia(myinfers(G, _L, _G, T, _M, _S), infers(G, L)) :-
	( most_type(T) -> L = most ; L = T ).

% From the constraint goal Constraint, propia extracts a single term
% which generalises all its solutions. The generalisation depends upon
% the propagation language Language

:- mode infers(+, ++, ++).
infers(Constraint, Language, Module) :-
%  Check the syntax, and handle any errors.
%  Also load solvers as a side effect
        check_errors(Constraint, Language, Module).

%  If there are no errors, 
%  and the Constraint is ground, then, ignore the propagation language,
%  and just check the constraint
infers(Constraint,Language,Module) :-
        term_variables(Constraint,Vars),
        (Vars=[] -> once(Constraint)@Module
	 ;          infers_susp(Constraint,Vars,Language,Module)
        ).
                
%  Finally, see if the user has specified a priority and wakeup condition
%  using the syntax "suspend(Constraint,Priority,Condition) infers Language"
%  If not, default Priority=5, one lower than any FD constraints, and 
%  Condition is the most general wakeup condition, 'constrained'.
infers_susp(InGoal,Vars,Language,Module) :- !,
        ( InGoal = suspend(Goal,Prior,Cond) ->
	     check_priority(Prior, InGoal, Language)
	  ;  InGoal = Goal, Prior=5, Cond= (Goal->constrained)
	),
	infers2(Goal,Vars,Language,Prior,Cond,Module).


% The language ac is handled by this clause
% Unfortunately the Priority and waking conditions for ac
% are those of the element constraint, which can't be overridden
:- mode infers2(+,+,++,++,+,++).
infers2(Goal,Vars,ac,_Prior,_Cond,Module) :- !,
	ac(Goal,Vars,Module).

% For languages 'consistent' and 'unique'
infers2(Goal,Vars,Language,Prior,Cond,Module) :-
        (Language=consistent ; Language = unique), !,
	suspend(myinfers(Goal,vars(Vars),Language,Module,Susp), Prior, Cond, Susp),
        myinfers(Goal,vars(Vars),Language,Module,Susp).

% The 'most' propagation language: 
% construct a list of one or more solver types. 
infers2(Goal,Vars,most,Prior,Cond,Module) :- !,
	most_type(Type),
	infers3(Goal,Vars,Type,Prior,Cond,Module).

infers2(Goal,Vars,Language,Prior,Cond,Module) :- 
	atom(Language), !,
	infers3(Goal,Vars,[Language],Prior,Cond,Module).

infers2(Goal,Vars,Type,Prior,Cond,Module) :- 
	infers3(Goal,Vars,Type,Prior,Cond,Module).


infers3(Goal,Vars,Type,Prior,Cond,Module) :- 
        shelf_create( propiaVar(Vars,no_delayed), GlobVar ),
        suspend( myinfers(Goal,most(Vars),GlobVar,Type,Module,Susp),
		 Prior,
		 Cond,
		 Susp
               ),
        get_msg_for_goal(Goal,most(Vars),GlobVar,Type,Module,Susp).


most_type(Type) :-
	KnownSolvers = [ic_symbolic,ic,sd,fd],
	( foreach(Solver,KnownSolvers), fromto(Type,Type1,Type2,[]) do
	    ( current_module(Solver) -> Type1=[Solver|Type2] ; Type1=Type2 )
	).


%----------------------------------------------------------------------
% Topological Branch-and-bound
%----------------------------------------------------------------------

% myinfers is the propia demon that wakes whenever the Goal becomes
% more instantiated.  It is killed either when the Goal becomes
% ground, or when the Goal has only one answer left.

:- demon myinfers/6.
:- mode myinfers(+, +, ++, ++, ++, ++).
myinfers(Goal, Most,GlobVar, Type, Module,Susp) :-
	term_variables(Most,Vars),
        (Vars=[] -> 
              kill_suspension(Susp),
              once(Goal)@Module
         ;    
              setarg(1,Most,Vars),
              get_msg_for_goal(Goal,Most,GlobVar,Type,Module,Susp)
        ).

% We make a fresh copy of the Goal (using the current, updated domains)

get_msg_for_goal(Goal,most(Vars),GlobVar,Type,Module,Susp) :-
        copy_term((Goal,Vars),(CopyGoal,CopyVars)),
	init_domains(CopyGoal, CopyVars, GlobVar, Type, Module),
        (   no_new_info(Vars,GlobVar) -> 
                   propia_answer(Vars,Type,GlobVar,Susp) ;
            extend(CopyGoal,CopyVars,Vars,NewVars,GlobVar,Type,Module) ->
                   propia_answer(Vars,Type,GlobVar,Susp),
                   call(Vars=NewVars) ;
            true ->        
                   propia_answer(Vars,Type,GlobVar,Susp)
        ).

no_new_info(Vars,GlobVar) :-
        xget(GlobVar,1,NewVars),
        instance(Vars,NewVars).

% GlobVar contains the term which currently captures the "most
% specific generalisation" (msg) of the solutions to the goal Goal found so
% far.  This msg depends upon the solver types.  The first msg is
% simply the generalisation of the first solution.  Normally the new
% msg is constructed from the previous msg and the new solution.
% However on initialisation there is only a new solution, so this is
% generalised with itself (hence Most appears twice as an argument to newmsg).
% [this basically makes a term copy, using only the copy_term handler
% for the approximation language Type].

init_domains(Goal,Vars,GlobVar,Type,Module) :-
        not not
                (subcall(once(Goal)@Module,Del),
                 newmsg(Vars,Vars,Type,NewVars,Module),
                 xset(GlobVar,1,NewVars),
                 (Del\=[] -> xset(GlobVar,2,delayed) ; true)
                ).

propia_call(Goal,Vars,GlobVar,Type,Module) :-
     subcall(Goal,Del)@Module,
     xget(GlobVar,1,OldVars),
     newmsg(Vars,OldVars,Type,NewVars,Module),
     xset(GlobVar,1,NewVars),
     (Del\=[] -> xset(GlobVar,2,delayed) ; true).

% extend first finds a solution to Goal, which instantiates Vars.  The
% previous msg OldVars is extracted from GlobCar and a new msg constructed
% from Vars and OldVars.  GlobVar is now set to this new msg.  If the
% new msg is less instantiated than the original state OrigVars, then
% no new information has been extracted by propia and 'extend'
% succeeds.  Otherwise 'instance' fails, and a new solution to Goal is
% found on backtracking.  This is used to produce a new msg and so on
% until there are no more solutions to Goal, and the first clause of
% extend fails.  Then the second clause simply extracts the current
% msg from GlobVar and instantiates NewVars.
% The suspended goal mysusp, checks each answer to Goal and fails if
% the answer is an instance of the current msg.  This is "merely" an
% optimisation, but can cut off some infinite loops in case the Goal
% has infinitely many answers (eg member(a,VarList)).
extend(Goal,Vars,OrigVars,FinalVars,GlobVar,Type,Module) :-
        not((suspend(cons_diff(Vars,GlobVar,Susp),2,Vars->constrained,Susp),
             propia_call(Goal,Vars,GlobVar,Type,Module),
             no_new_info(OrigVars,GlobVar) 
           )),
        xget(GlobVar,1,FinalVars).

% When cons_diff wakes up it checks if Vars is an instance of
% the current msg in GlobVar.  If so it fails.  Ideally cons_diff
% should be a demon, but this does not work if the Term becomes
% further instantiated in a way that introduces new variables (eg when
% X is unified with f(Y) it becaomes further instantiated, but the
% instance now contains a new variable Y.

:- demon cons_diff/3.
cons_diff(Vars,GlobVar,Susp) :-
        xget(GlobVar,1,OldVars),
        checkNotInst(Vars,OldVars),
        ( ground(Vars) -> 
                  kill_suspension(Susp)
         ;        true ).


checkNotInst(Vars,OldVars) :-
    not instance(Vars,OldVars).

% The generalisation of two terms, T1 and T2 and a type Type is made
% complicated by the possibility of an atom or variable  occurring twice in the
% same term.  eg newmsg(f(a,b,a),f(c,d,c),[fd],f(X{a,c},W{b,d},X{a,c}))
% A "Map" is used to record the msg's applied so far, eg msg(X,Y,Z))
% This map is augmented during the msg operation, and hence an input
% and output Map are the last two arguments of msgList
newmsg(T1,T2,Type,T3,Module) :-
        msgList(T1,T2,T3,Type,[],_,Module).

msgList([],[],[],_,Map,Map,_Module) :- !.
msgList([V1|V1T],[V2|V2T],[MSGV|MSGT],Type,Map,OutMap,Module) :-
        msg(V1,V2,MSGV,Type,Map,NewMap,Module),
        msgList(V1T,V2T,MSGT,Type,NewMap,OutMap,Module).

msg(T1,T2,T3,Type,Map,OutMap,Module) :- 
        nonvar(T1), nonvar(T2),
        T1=..[F|Args1], T2=..[F|Args2], !,
        msgList(Args1,Args2,Args3,Type,Map,OutMap,Module),
        T3=..[F|Args3].
msg(T1,T2,T3,_,Map,Map,_Module) :-
        lookUpMap2(Map,T1,T2,T3), !.
msg(V1,V2,MSGV,Type,Map,[map(V1,V2,MSGV)|Map],Module) :-
	Type:msg(V1,V2,MSGV)@Module.


lookUpMap2([map(V1,V2,V3)|_],V11,V22,V33) :-
        V11==V1, V22==V2, !, V33=V3.
lookUpMap2([_|Map],V1,V2,V3) :-
        lookUpMap2(Map,V1,V2,V3).



% A Goal has only one answer left, either when it is instantiated, or,
% in case the solver type includes fd, when there is just a single
% (finite domain) variable.  Clearly in this case, propia has captured
% all possible values for this variable, in its finite domain.
propia_answer(Vars,Type,GlobVar,Susp) :-
       ( propia_ans(Vars,Type),
         xget(GlobVar,2,no_delayed) ->
            kill_suspension(Susp)
       ;
            true
       ).


propia_ans([],_Type).
propia_ans([Var],Type) :-
	    member(Solver, Type),
	    Solver:is_exact_solver_var(Var),
	    !.


%----------------------------------------------------------------------
%%%%%%%% Processing languages consistent and unique  %%%%%%%%%%%
%----------------------------------------------------------------------


:- demon myinfers/5.
:- mode myinfers(+, +, ++, ++, ++).

myinfers(Goal,Vars,Language,Module,Susp) :-
        term_variables(Vars,List),
        (List=[] -> 
	        kill_suspension(Susp),
	        once(Goal)@Module
           ;    setarg(1,Vars,List),
                myinfers(Goal,Language,Module,Susp)
        ).

myinfers(Goal,consistent,Module,_Susp) :- !,
	copy_term(Goal,CopyGoal),
	subcall(once(CopyGoal)@Module,_).

myinfers(Goal,unique,Module,Susp) :- !,
	copy_term(Goal,CopyGoal),
	subcall(once(CopyGoal)@Module,Del), 
	switchUnique(Goal,CopyGoal,Del,Module,Susp).

% The language 'unique' is for committed-choice computation, for example
% p(X) :- X>0, X>=20.
% p(0).
% ?- p(X) infers unique, X=0.
% If switchUnique didn't check the delayed goals, then 
% p(X) infers unique 
% would succeed immediately, committing to the first clause for 'p'.
% This is logically unsound!
switchUnique(Goal,CopyGoal,Del,Module,Susp) :-
        (Del=[], not anotherAns(Goal,CopyGoal,Module)) ->
	        Goal=CopyGoal,  kill_suspension(Susp) 
        ;       true.

anotherAns(Goal,CopyGoal,Module) :-
	copy_term(Goal,Copy2),
        suspend(cons_diff_2(Copy2,CopyGoal,Susp),2,Copy2->constrained,Susp),
        call(Copy2)@Module.

:- demon cons_diff_2/3.
cons_diff_2(Vars,OldVars,Susp) :-
        checkNotInst(Vars,OldVars),
        ( ground(Vars) -> 
                  kill_suspension(Susp)
         ;        true ).

%%%%%%% Processing language ac              %%%%%%%%%%%%%%
%%%%%%%                                     %%%%%%%%%%%%%%

% This achieves arc consistency by finding all answers, and then
% extracting a domain for each variable corresponding to the values
% that variable can take in all the answers.  More academically it
% implements n-ary arc-consistency by applying binary arc-consistency
% on a "hidden variable" binary representation of the problem.  The
% hidden variable is I (in param(I), which means the SAME I is used to
% represent the tuple in each element constraint
ac(Goal, Args,Module) :-
     copy_term((Args,Goal),(Vars,VarGoal)),
     findall(Vars,(call(VarGoal)@Module,check_ground(Vars)),Matrix),
     transpose(Matrix,Matrix1),
     (foreach(Arg,Args), % foreach(Var,Vars), 
      foreach(List,Matrix1), param(_I,Module) do
        myelement(_I,List,Arg,Module) ).

% This is a minor efficiency enhancement to element to take account
% of the special case where the list List is simply [1,2,3,...]

myelement(I,List,Arg,Module) :-
	intlist(List,1,Max), !, 
	call(Arg::1..Max)@Module,
	I=Arg.
myelement(I,List,Arg,Module) :-
	call(element(I,List,Arg))@Module.

intlist([Max],Max,Max) :- !.
intlist([N|T],N,Max) :-
	M is N+1,
	intlist(T,M,Max).

%For a Goal with, say, three answers, 'findall' returns a matrix with
%three rows.  This is transposed into a matrix with three columns: in
%the transposed matrix each row corresponds to the values for a single
%variable in the Goal e.g.
% p(a,1).
% p(b,2).
% p(b,3).
% ?- p(X,Y) infers ac
% findall yields the matrix: [ [a,1], [b,2], [b,3] ]
% which is transposed into the matrix [ [a,b,b], [1,2,3] ]
transpose([], []).
transpose(LoL, Cols) :-
        heads_and_tails(LoL, Col, LoL1),
        ( Col == [] ->
            Cols = []
        ;
            Cols = [Col|Cols0],
            transpose(LoL1, Cols0)
        ).

heads_and_tails([], [], []).
heads_and_tails([L|Ls], Hs, Ts) :-
        ( L == [] ->
            Hs = Hs0, Ts = Ts0
        ;
            L = [H|T], Hs = [H|Hs0], Ts = [T|Ts0]
        ),
        heads_and_tails(Ls, Hs0, Ts0).



%%%%%%%%%%   Propia Syntax Check    %%%%%%%%%%%%
%%%%%%%%%%                          %%%%%%%%%%%%

%  ERRORS :  Where are their messages defined?  
%  4, 6

check_errors(Goal, Language, Module) :-
        ( var(Goal) ; var(Language) ; var(Module) ),
        !,
        error(4, infers(Goal, Language, Module)).
check_errors(_Goal, Language, Module) :-
        is_a_language(Language, Module), !, fail.
check_errors(Goal, Language, Module) :-
        error(6, infers(Goal, Language, Module)).

check_ground(Term) :-
        ( nonground(Term) ->
	      printf(error, "The answer %w should be ground%n", [Term]),
              abort
	;
	    true
	).

check_priority(Priority, Goal, Language) :-
        ( integer(Priority) ->
	    ( Priority >= 5 -> true ; error(6, Goal infers Language) )
	; var(Priority) ->
	    error(4, Goal infers Language)
	;
	    error(5, Goal infers Language)
	).

is_a_language(most, _) :- !.
is_a_language(consistent, _) :- !.
is_a_language(unique, _) :- !.
is_a_language(ac, Module) :- !,
	( is_predicate(element/3)@Module ->
	    true
	;
	    printf(error, "'infers ac' requires a visible predicate element/3%n", []),
	    fail
	).
is_a_language(Solver, _) :- atom(Solver), !,
	ensure_loaded(library(Solver)),
	( get_flag(msg/3, defined, on)@Solver -> true ;
	    printf(error, "The '%w' solver does not support generalized propagation.%n", [Solver]),
	    fail
	).
is_a_language(Solvers, Module) :-
	is_list(Solvers),
	( foreach(Solver,Solvers), param(Module) do
	    is_a_language(Solver, Module)
	).


%%%%%%%%%  Utilities              %%%%%%%%%%%%
%%%%%%%%%                         %%%%%%%%%%%%


idmemb(X,[H|_]) :- X==H, !.
idmemb(X,[_|Rest]) :- idmemb(X,Rest).
    
idremove(X,[H|T],T) :-
	X==H, !.
idremove(X,[H|T],[H|Rest]) :-
	idremove(X,T,Rest).


