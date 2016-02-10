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
% Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: frequency.pl,v 1.1 2006/09/23 01:53:31 snovello Exp $
% ----------------------------------------------------------------------

:- lib(fd).
:- lib(fd_global).

/*

Example

Prunes domains of variables:
L=[X1,X2,X3,X4],L::1..10,O1#>0,O2#>0,O3#>0,frequency(L,[O1,O2,O3],[1,2,3]),X1=4.

Instantiates occurences:
L=[X1,X2,X3,X4],L::1..10,O1#>0,O2#>0,O3#>1,frequency(L,[O1,O2,O3],[1,2,3]).


Instantiates all variables:
 L=[X1,X2,X3,X4],L::1..10,O1#>0,O2#>0,O3#>0,frequency(L,[O1,O2,O3],[1,2,3]),X1#<X2,X2#<X3,X3#<X4,X4=4.

*/

% frequency(+List,+Occurences,++Values)
%       An aggregate constraint of occurrences:
%          Each value in Values should occur exactly in List 
%          as many times is specified in Occurences
%          Precondition: len(Occurences) = len(Values)
%          Values should be a list of ground values
%          List should be a list of domain variables
%          Occurences should be a list of domain variables
%
%       Example:
%          
%          frequency([1,2,3,4],[1,1],[2,3]) succeeds
%          frequency([1,2,3,4],[1,0],[2,3]) fails
%          frequency([1,2,2,4],L,[1,2,3,4]) succeeds with 
%                   L = [1, 2, 0, 1]


frequency(List,Occurences,Values):-
	(   param(List),
	    foreach(V,Values),
	    foreach(O,Occurences) 
	do
            occurrences(V,List,O)
	),
	length(List,N),
	Sum #= sum(Occurences),
	Sum #<= N,
	list_to_dom(Values, ValuesDom),
	frequency_aux(List,Occurences,Values,ValuesDom,Sum).


frequency_aux(OldVars,Occs,Vals,ValuesDom,S):-
	remove_other_values(OldVars,ValuesDom,Vars,Len),
	S #<= Len,
	mindomain(S,MinS),
	(MinS == Len ->
	    % Need to filter values to find out which values have not been
	    % assigned yet in order to construct the domain of the remaining
	    % variables in the list
	    (param(Vars),foreach(Val,Vals),foreach(Occ,Occs),fromto(Domain,Out,In,[]) do
	        count_vars(Val,Vars,0,Lower,0,_Upper,_),
		mindomain(Occ,MinO),
		(Lower >= MinO -> % constraint satisfied
		    Out = In
		;
		    Out = [Val|In]
		)
	    ),
	    list_to_dom(Domain,Dom),
	    (param(Dom),foreach(V,Vars) do
	        (nonvar(V) ->
		    true
		;
		    dvar_domain(V,OldDomain),
		    dom_intersection(OldDomain,Dom,NewDom,_),
		    dvar_update(V,NewDom)
		)
	    )
	    
	;
	    Var = v(Occs,Vars),
	    suspend(frequency_aux(Vars,Occs,Vals,ValuesDom,S),4,[S->min,Var->any])
	).

% remove_other_values(OldVars,Values,NewVars,Len)

remove_other_values(OldVars,Values,NewVars,Len):-
	remove_other_values(OldVars,Values,NewVars,0,Len).

remove_other_values([H|T],Values,NewVars,Sofar,Len):-
	dvar_domain(H, HDom),
	(dom_intersection(Values,HDom,_,_) ->
	    Acc is Sofar + 1,
	    NewVars = [H|Rest]
	;
	    Acc = Sofar,
	    NewVars = Rest
	),
	remove_other_values(T,Values,Rest,Acc,Len).
remove_other_values([],_Values,[],Len,Len).

% Taken from fd_global.pl library

% count_vars(+Value,+Vars,-Lower,-Upper,-VarsWithValue)
% Given an integer value and a list of finite domain variables
% Returns a lower and an upper bound of the times this value
% appear in the variable list as well as the variables which
% may or may not hold this value in the future.
% The lower bound refers to the number of times a variable was
% instantiated to the value.
% The upper bound refers to the number of uninstantiated variables
% which can still take this value.
% The VarsWithValue are the uninstantiated variables...

%count_vars(Value,Vars,Lower,Upper,VarsWithValue):-
%	count_vars(Value,Vars,0,Lower,0,Upper,VarsWithValue).

count_vars(_,[],Lower,Lower,Upper,Upper,[]).
count_vars(Value,[H|T],Lower1,Lower,Upper1,Upper,VarsWithValue) :-
	dvar_domain(H,DH),
	( dom_check_in(Value,DH) ->
	    Upper2 is Upper1 + 1,		% Value in domain
	    ( H == Value ->
		Lower2 is Lower1 + 1,		% H is instantiated to Value!
		VarsWithValue = MoreWithValue
	    ;   
                Lower2 = Lower1,
		VarsWithValue = [H|MoreWithValue]
	    ),
	    count_vars(Value,T,Lower2,Lower,Upper2,Upper,MoreWithValue)
	;
	    count_vars(Value,T,Lower1,Lower,Upper1,Upper,VarsWithValue)
	).


	
	
%----------------------------------------------------------------------
% multi_occurrences(++Values, +List, ?Occs)
%----------------------------------------------------------------------

multi_occurrences(Values, List, Occs) :-
	nonground(Values, SomeVar), !,
	suspend(occurrences(Values, List, Occs), 3, SomeVar->inst).
multi_occurrences(Values, List, Occs) :-
	(
	    fromto(List, XXs, Xs, []),
	    foreach(Susp,Susps),
	    param(List,Values,Occs)
	do
	    XXs = [X|Xs],
	    sublist(List, XXs, ListWithoutX),
	    suspend(multi_occurrences(Values, X, ListWithoutX, Occs, Susp),
	    	4, X->any, Susp)
	),
	schedule_woken(Susps), wake.

% Ys_Zs is the list Ys_X_Zs without X (the first element of X_Zs)
sublist(Ys_X_Zs, X_Zs, Ys_Zs) :-
	( Ys_X_Zs == X_Zs ->
	    X_Zs = [_X|Ys_Zs]
	;
	    Ys_X_Zs = [Y|Ys_X_Zs1],
	    Ys_Zs = [Y|Ys_Zs1],
	    sublist(Ys_X_Zs1, X_Zs, Ys_Zs1)
	).

:- demon multi_occurrences/5.
multi_occurrences(Values, X, ListWithoutX, Occs, _Susp) :-
	( nonvar(X) -> kill_suspension(Susp) ; true ),
	dvar_domain(X, DX),
	dom_size(DX, Size),
	count_subsets(DX, ListWithoutX, Size, 1, Count, Others),
	( Count == Size ->
	    call_priority(update_domains(Others), 2)
	;
	    true
	).


not_among(X,ListWithoutX):-
	nonvar(X),
	call_priority(remove_element(ListWithoutX,X), 2).
not_among(X,ListWithoutX):-
	var(X),
	dvar_domain(X,DX),
	dom_size(DX,Size),
	count_subsets(DX,ListWithoutX,Size,1,Count,Others),
	make_suspension(not_among(X,ListWithoutX),4,Susp),
	insert_suspension(X,Susp,any of fd,fd),
	( Count == Size ->
	    call_priority(update_domains(Others), 2)
	;
	    true
	).

count_subsets(_,[],_,N,N,[]).
count_subsets(Dom,[H|T],Dom_Size,Sofar,N,Others):-
	dvar_domain(H,DH),
	(dom_difference(DH,Dom,Diff,_)->
            % DH not a subset of Dom
	    Others = [H-Diff|Others0],
	    count_subsets(Dom,T,Dom_Size,Sofar,N,Others0)
	;
            % DH subset of Dom
	    Count is Sofar + 1,
	    Count =< Dom_Size,
	    count_subsets(Dom,T,Dom_Size,Count,N,Others)
	).

update_domains([]).
update_domains([X-D|XDs]) :-
	dvar_update(X,D),
	update_domains(XDs).

remove_element([],_).
remove_element([X|Xs],E) :-
	dvar_remove_element(X,E),
	remove_element(Xs,E).



