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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Mark Wallace, IC-Parc
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Mark Wallaced, ICL/IC-Parc
% Version:	$Id: shadow_cons.pl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%
% Example:
% :- lib(fd).
% :- use_module(shadow_cons).
% :- shadow_call(fred,X#>2), shadow_call(fred,Y#>X), shadow_var(Y,fred,Z).
% :- shadow_call(fred,Y#>X), shadow_var(Y,fred,Y).


:- module(shadow_cons).

:- comment(categories, ["Constraints","Data Structures"]).
:- comment(summary, "A library implementing shadow, or 'local', constraint stores").
:- comment(author, "Mark Wallace, IC-Parc and ICL").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2009/07/16 09:11:27 $").

:- comment(shadow_var / 3, [
    summary:"Finds the shadow variable associated with the input
	variable in the specified shadow constraint store.  If none
	exists, it creates one.",

    desc:html("
    The shadow variable 'represents' the original variable in the
    named constraint store.  This allows it to be more (or less)
    constrained than the original variable.  When the original
    variable becomes instantiated, or has its domain/range narrowed,
    this change is propagated to the shadow variable."),
    amode:shadow_var(?,+,-),
    args:["Var" : "A variable to be shadowed",
        "StoreName" : "The name of a shadow constraint store.  This call may be the first reference to this store. It must be an atom.",
	"ShadowVar":"The associated shadow variable"],
    resat:"No.",
%    fail_if:"The original variable already has a shadow which cannot be unified with the new one, or if the input is not unifiable with its shadow.",
    exceptions:[4:"StoreName is uninstantiated",5:"StoreName is not an atom"],
    eg:"
    Success:
        [eclipse]: shadow_var(X,fred,Y).
        X = X{[fred - Y]}
        Y = Y
        yes.

        [eclipse]: lib(fd).
        ...
        [eclipse]:  X::1..10, shadow_var(X,fred,Y), X#<5, Y#>3.
        X = X{fd:[1..4], shadow_cons:[fred-4]}
        Y = 4

    Error:
        shadow_var(X,Var,Y).  % Error 4
    ",
    see_also:[shadow_call/2]]).

:- comment(shadow_call / 2, [
    summary:"\
       Invokes a constraint on (the shadow variables of) a shadow constraint store.",
    amode:shadow_call(+,?),
    desc:html("A copy of the Goal is made, replacing all the variables by their representatives in the shadow constraint store.  This copied goal is then called
        "),
    args:["StoreName" : "The name of a shadow constraint store.  This call may be the first reference to this store. It must be an atom.",
          "Goal" : "A goal (i.e. a constraint): its variables need not (yet) have shadows in the named store"],
    resat:"No.",
    fail_if:"The constraint is inconsistent with those previously imposed on the shadow store",
    exceptions:[4:"StoreName is uninstantiated",5:"StoreName is not an atom"],
    eg:"
    Success:
        [eclipse]: X::1..10, shadow_call(fred,X#>2).
        X = X{fd:[1..10], shadow_cons:[fred - ShadowVar{[3..10]}]}
        yes.
        [eclipse]: shadow_call(fred, X#>2), 
                   shadow_call(fred, Y #> X), 
                   shadow_var(Y, fred, Z).
        X = X{[fred - ShadowVar{[3..9999999]}]}
        Y = Y{[fred - Z{[4..10000000]}]}
        Z = Z{[4..10000000]}
        yes.

    Fail:
        [eclipse]: X::1..10, shadow_call(fred,X#>10).
        no (more) solution.
    Error:
        shadow_call(Var,true).  % Error 4
    ",
    see_also:[shadow_var/3]]).


:- export shadow_call/2, shadow_var/3.

:- meta_attribute(shadow_cons, [unify:unify_LC/2, print:print_LC/2]).

:- tool( shadow_call/2, shadow_call/3 ).

shadow_call(Name, Cons, Module) :-
	var(Name), !,
        error(4, shadow_call(Name,Cons,Module)).
shadow_call(Name, Cons, Module) :-
	not atom(Name), !,
        error(5, shadow_call(Name,Cons,Module)).
shadow_call(Name, Cons, Module) :-
	term_variables(Cons,Vars),
	(foreach(Var,Vars), foreach(ShadowVar, ShadowVars), param(Name) 
         do shadow_var1(Var,Name,ShadowVar)),
	% Since we put the shadow variables (which have already the correct
	% attributes) in the copy, we can now use the cheaper copy_term/3
	% that doesn't copy the attributes:
	sepia_kernel:copy_term((Cons,Vars),(ShadowCons,ShadowVars), _Metas),
	call(ShadowCons)@Module.


shadow_var(Term, Name, ShadowVar) :-
	var(Name), !,
        error(4, shadow_var(Term,Name,ShadowVar)).
shadow_var(Term, Name, ShadowVar) :-
	not atom(Name), !,
        error(5, shadow_var(Term,Name,ShadowVar)).
shadow_var(Term, Name, ShadowVar) :-
	shadow_var1(Term, Name, ShadowVar).

shadow_var1(Term, _Name, ShadowVar) :-
	nonvar(Term), ShadowVar=Term.
shadow_var1(Var, Name, ShadowVar) :-
        free(Var),
	add_attribute(Var,shadow_cons([Name-ShadowVar])),
        link(Var,ShadowVar).
shadow_var1(Var{Shadow},Name,ShadowVar) :-
	-?->
	( var(Shadow) ->
		Shadow=shadow_cons([Name-ShadowVar]),
		copy_term(Var,ShadowVar),	% copy attributes
		link(Var,ShadowVar)
          ;
		look_up(Shadow,Name,ShadowVar)
	).

link(Var,ShadowVar) :-
	suspend(update_shadow_demon(Var,ShadowVar,Susp),2,Var->constrained,Susp).


% The X==Y condition for kill_suspension prevents looping in the case
% :- shadow_call(fred,Y#>X), shadow_var(Y,fred,Y).

:- demon(update_shadow_demon/3).
update_shadow_demon(X,Y,Susp) :-
	copy_term(X,Y),
	( X==Y -> kill_suspension(Susp) ; true ).  % X==Y implies ground(X) !

look_up(ShadowCons,Name,ShadowVar) :-
	ShadowCons=shadow_cons(List),
	( memberchk(Name-Var,List) -> ShadowVar=Var
          ; setarg(1,ShadowCons,[Name-ShadowVar|List]) ).


      % unify_enum(+Term, Attribute)
unify_LC(_, Attr) :-
          /*** ANY + VAR ***/
          var(Attr).               % Ignore if no attribute for this extension
unify_LC(Term, Attr) :-
          compound(Attr),
          unify_term_LC(Term, Attr).

unify_term_LC(Value, shadow_cons(ListY)) :-
          nonvar(Value),           % The attributed variable was instantiated
          /*** NONVAR + META ***/
           (foreach(_Name-Var, ListY), param(Value) do Var=Value).
unify_term_LC(Y{AttrY}, AttrX) :-
          -?->
          unify_LC_LC(Y, AttrX, AttrY).

unify_LC_LC(_, AttrX, AttrY) :-
          var(AttrY),                         % no attribute for this extension
          /*** VAR + META ***/
          AttrX = AttrY.                      % share the attribute
unify_LC_LC(_Y, shadow_cons(ListX), AttrY) :-
          nonvar(AttrY),
          /*** META + META ***/
          AttrY = shadow_cons(ListY),
	  unionLC(ListX,ListY,ListXY),
          setarg(1, AttrY, ListXY).  

unionLC([],Y,Y).
unionLC([H|T],Y,Z) :-
	(memberchk(H,Y) -> unionLC(T,Y,Z)
         ;     Z=[H|W], unionLC(T,Y,W) ).

print_LC(_{shadow_cons(List)}, Attr) :-
          -?->
          Attr = List.



