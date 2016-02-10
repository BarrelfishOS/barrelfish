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
% Contributor(s): 
% 
% END LICENSE BLOCK

% VBS router obtained from Sakis.
% 
% Call e.g. r(2) to run sample request 2.

% Uncomment one of the lib() lines below to have a particular solver:

%:- lib(fd).		% finite domain solver only
%:- lib(lponly).		% cplex mip solver
%:- lib(mip).		% cplex lp + b&b in Prolog
:- lib(fdplex).	% fd/cplex cooperation
%:- use_module(fdplex).	% fd/cplex cooperation
%:- use_module(mip).	% fd/cplex cooperation

%:- lp_set(result_channel, +output).

% You can obtain a problem file in lp-format by using lib(lponly) and
% running the problem once. The file eclipse.cplex.lastprob
% then holds the problem in lp-format.


:- ensure_loaded([vbs_data,vbs_services]).

all :-
	get_flag((::)/2, definition_module, LibName),
	get_flag(unix_time, Stamp),
	concat_string([results_,LibName,'_',Stamp], FileName),
	open(FileName,write,rs),
	serv(N, S, D),
	N < 20,
	writeln('***** Problem':N),
	cputime(T0),
	router(S, D, _R, C),
	T is cputime-T0,
	writeln(rs, ('Problem':N, 'Cost':C, 'Time':T)),
	fail.
all :-
	close(rs).

r(N) :-
	serv(N, S, D),
	r(S, D).

r(S, D) :-
	router(S, D, R, C),
	writeln(('R'=R, 'C'=C)).

% very small examples:
r :- r(1, [10,15,20]).
s :- r(1, [10,20]).

%------------------------------------------------------------------------

solution(
[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0]).


router(Start,Dest,Route,RCost):-
	nodes_in_out(Nodes_in_out),
	make_rvars(Nodes_in_out,[],Vars,Out,[],In,UVars),
	make_rcost(Vars,RCost),
	nodes(Nodes),
	set_up_rcons(Start,Dest,Nodes,Out,In,UVars),
	disj_subnets(DSN),
	disj_subnets_check([Start|Dest],DSN,Vars),
	cputime(T1),
	strip_vars(Vars, Vs),
	minimize(naive_label(Vs,RCost), RCost),
%	min_max(naive_label(Vs,RCost), RCost),
        make_route(Vars, Route),
%%%	label_uvars(UVars),
	cputime(T2),
	Time is T2-T1,
	writeln(Time).



disj_subnets_check(_,[],_).
disj_subnets_check(SD,[D-N|Rest],Vars):-
	intersection(SD,N,Int),
	(Int\==[] ->
		true
	;	findall((I,J),(member(I,[D|N]),member(J,[D|N]),arc(I,J,_,_)),Arcs),
		remove_arcs(Arcs,Vars)),
	disj_subnets_check(SD,Rest,Vars).

remove_arcs([],_).	
remove_arcs([(I,J)|Rest],Vars):-
	memberchk((I,J)-V,Vars),
	V=0,
	remove_arcs(Rest,Vars).

strip_vars([], []).
strip_vars([_-V|Ps], [V|Vs]) :-
	strip_vars(Ps, Vs).

make_route([], []).
make_route([Arc-Bij|More], Route) :-
	( Bij == 1 -> Route = [Arc|Route0] ; Route = Route0 ),
	make_route(More, Route0).


naive_label([],_).
naive_label([V|Rest],FdCost):-
	( var(V) ->
	    indomain(V)
	;
	    true
	),
	naive_label(Rest,FdCost).


label_uvars([]).	
label_uvars([_-Var|Rest]):-
	mindomain(Var,Var),
	label_uvars(Rest).


make_rvars([],BVars,BVars,[],In,In,[]).	
make_rvars([I-_-Out|Rest],AcBVars,BVars,OutVars,AcIn,In,[I-UI|UVars]):-
	UI::0..29,
	make_vars(I,Out,IBVars,AcIn,NAcIn),
	append(IBVars,AcBVars,NAcBVars),
	OutVars=[I-IBVars|ROut],
	make_rvars(Rest,NAcBVars,BVars,ROut,NAcIn,In,UVars).

make_vars(_,[],[],In,In).	
make_vars(I,[J|Rest],[(I,J)-Bij|RVars],CIn,In):-
	Bij::0..1,
	(delete(J-InJ,CIn,RCIn)->
		NInJ=J-[(I,J)-Bij|InJ],
		NCIn=[NInJ|RCIn]
	;	NCIn=[J-[(I,J)-Bij]|CIn]),
		make_vars(I,Rest,RVars,NCIn,In).

set_up_rcons(_,_,[],_,_,_).		
set_up_rcons(S,Dest,[I|RNodes],Out,In,UVars):-
	once(delete(I-IIn,In,RIn)),
	once(delete(I-IOut,Out,ROut)),
	length(Dest,LD),
	(I==S ->
		set_to_eq(IIn,0),
		SOutSum::1..LD,
		set_to_eq(IOut,SOutSum)
	;	memberchk(I-UI,UVars),
		cycle_cons(UI,IOut,UVars),
		(memberchk(I,Dest) ->
			set_to_eq(IIn,1)
		; 	InFlag::0..1,
			OutSum::0..LD,
			set_to_eq(IIn,InFlag),
			set_to_eq(IOut,OutSum),
			geq_out(InFlag,OutSum)
%%%			,gzero_out(InFlag,OutSum)
			)),
	set_up_rcons(S,Dest,RNodes,ROut,RIn,UVars).

cycle_cons(_,[],_).
cycle_cons(UI,[(_,J)-Bij|Rest],UVars):-
	memberchk(J-UJ,UVars),
	UI-UJ+29*Bij#<=28,
	cycle_cons(UI,Rest,UVars).		

%%%gzero_out(InFlag,Var):-
%%%	mindomain(Var,Min),
%%%	(Min > 0 ->
%%%		InFlag=1
%%%	;	(var(Var) ->
%%%			make_suspension(gzero_out(InFlag,Var),3,S),
%%%			insert_suspension(Var,S, min of fd,fd)
%%%		;	InFlag=0)).	

%%%delay geq_out(Flag,OutS) if var(Flag).
%%%geq_out(Flag,OutS):-
%%%	(Flag==1 ->
%%%		OutS##0
%%%	; 	OutS=0).

geq_out(Flag,OutS):-
	Flag*1000 #>= OutS, OutS #>= Flag.


make_rcost(Vars, RCost):-
	RCost::0..68,
	set_to_eq(Vars,RCost).
	
set_to_eq([],C):-0#=C.	
set_to_eq([_-B|Rest],C):-
	make_sum(Rest,B,Sum),
	Sum#=C.
		
	
set_to_geq([],C):- 0#>=C.	
set_to_geq([_-B|Rest],C):-
	make_sum(Rest,B,Sum),
	Sum#>=C.
	
make_sum([],Sum,Sum).
make_sum([_-B|Rest],PSum,Sum):-
	NPSum=PSum+B,
	make_sum(Rest,NPSum,Sum).
	
	
	

	

