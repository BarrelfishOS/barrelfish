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
% The Original Code is The List Collection for ECLiPSe.
% The Initial Developer of the Original Code is Lukasz Domagala.
% Portions created by the Initial Developer are Copyright (C) 2009.
% All Rights Reserved.


:-module(cycle).

:- comment(categories, ["Constraints"]).
:- comment(summary, "Cycle constraint").
:- comment(author, "Lukasz Domagala").
:- comment(copyright, "www.redber.pl").
:- comment(date, "2010/06/05 21:41").

:- comment(desc,ascii("
	A configurable constraint that forces the existence of a Hamiltonian cycle in a directed graph.
	The constraint uses the ic and eplex libraries to achieve different levels of filtering. For more 
	details see cycle/4.
	Parts of the filtering algorithm have been inspired by or are implementations of ideas presented by
	John H.Hooker in \"Rossi F., van Beek P., Walsh T. (Eds.), Handbook of Constraint Programming, 
	chap. 15. 2006 Elsevier.\".
	
	The constraint will be refined and new filtering techniques will be added as time will allow to
	work on the subject.
	")).
:- comment(eg,"	
:-lib(cycle).
:-lib(ic).
:-lib(branch_and_bound).	
cycle_example:-
	%edge weight matrix, 
	%the example data has been provided by Prof. Antoni Niederliñski
	EdgeWeightMx=[](
		[](  0,384,484,214,234,267,524,656,446,371,459,561,585,683,634,751),
		[](384,  0,156,411,296,167,339,379,340,432,485,545,483,500,565,642),
		[](484,156,  0,453,323,217,213,223,281,442,452,479,394,370,500,516),
		[](214,411,453,  0,130,259,413,601,303,157,245,356,422,542,427,585),
		[](234,296,323,130,  0,129,310,491,212,178,261,335,354,465,403,517),
		[](267,167,217,259,129,  0,255,389,205,265,318,391,348,421,430,516),
		[](524,339,213,413,310,255,  0,188,134,344,319,297,181,161,295,303),
		[](656,379,223,601,491,389,188,  0,322,532,507,485,363,260,477,430),
		[](446,340,281,303,212,205,134,322,  0,204,181,196,143,242,220,306),
		[](371,432,442,157,178,265,344,532,204,  0, 86,199,300,428,268,433),
		[](459,485,452,245,261,318,319,507,181, 86,  0,113,220,382,182,347),
		[](561,545,479,356,335,391,297,485,196,199,113,  0,156,323, 75,244),
		[](585,483,394,422,354,348,181,363,143,300,220,156,  0,167,114,163),
		[](683,500,370,542,465,421,161,260,242,428,382,323,167,  0,269,170),
		[](634,565,500,427,403,430,295,477,220,268,182, 75,114,269,  0,165),
		[](751,642,516,585,517,516,303,430,306,433,347,244,163,170,165,  0)
	),
	%the cities are: Szczecin,Gdañsk,Olsztyn,Zielona Góra,Poznañ,Bydgoszcz,Warszawa,
	%Bia³ystok,£ódz,Wroc³aw,Opole,Katowice,Kielce,Lublin,Kraków,Rzeszów.
	
	dim(EdgeWeightMx,[CityCount,CityCount]),
	length(EdgeDstCityLi,CityCount),
	%the edge variables' domains, any city can be reached from any other 
	ic:(EdgeDstCityLi#::1..CityCount),	
	%the edges (i,j) where i==j will be removed by the constraint
	cycle(EdgeDstCityLi,EdgeWeightMx,SumOfEdgeWeights),	

	%search
	cputime(StartTime),
	SearchGoal=search(EdgeDstCityLi, 0, most_constrained, indomain_max, complete, []),
	bb_min(SearchGoal, SumOfEdgeWeights, bb_options{strategy:dichotomic}),
	cputime(EndTime),
	SearchTime is EndTime - StartTime,
	
	%print result
	printf(\"Search time=%2.2fs  \",[SearchTime]),printf(\"Cost=%w\",[SumOfEdgeWeights]),nl,	
	write(\"Edges=\"),writeln(EdgeDstCityLi),
	true.	
	").
	


:-lib(ic).
:-lib(ic_global).
:-lib(eplex).
:-lib(list_collection).


:- comment(cycle/4, 
	[
	    amode:(cycle(+,++,-,++) is semidet),
	    args:["Edges":"A list of ic variables", "EdgeWeights":"A square matrix of integers","CycleCost":"The cost of the cycle","Configuration":"A list of key:value configuration elements"],
	    summary:"A constraint that forces a Hamiltonian cycle in a directed graph",
	    desc:ascii("
	Edges is a list of length VertexCount of ic variables, where VertexCount is the number of 
	vertices in the graph. Each variable needs to have a domain which is the subset of 
	[1..VertexCount]. The values in the i-th variable's domain correspond to edges in the 
	graph, so the domain value j of the i-th variable corresponds to an edge (i,j). 

	EdgeWeights is a square matrix (array of arrays) of size VertexCount*VertexCount of nonnegative 
	integers. The value indexed [i,j] corresponds to the cost of the edge (i,j). Values on the diagonal
	([i,i]) are unimportant since the correspond to edges (i,i) which are automatically removed by
	the constraint.

	CycleCost is an ic variable that corresponds to the cost of the cycle. 

	Configuration is a list of key:value pairs that configure the filtering level of the constraint. 
	rc_varfix:yes/no (default is yes), enables or disables the propagation of reduced cost variable 
	fixing constraints. These constraints are based on the solution of the LP relaxation for the cycle. 
	cut_planes:yes/no (default is yes), enables or disables the iterative strengthening of the LP 
	relaxation by generation of cutting planes for the LP model. 
	bound_upd:yes/no (default is yes), enables or disables the tightening of the upper bound on cost 
	by solving a LP relaxation. 
	opt_dir:min/max (default is min), the propagation is optimised for minimisation (min) 
	or maximisation (max) of the CycleCost. 

	The default configuration for the constraint is to use the maximal available propagation 
	and cost tightening. This setting is also used by the cycle/3. 

	The most basic propagation level for cycle is achieved by setting
	Configuration=[rc_varfix:no,cut_planes:no,bound_upd:no].
	
		"),
	    %eg:"",
	    see_also:[cycle/3],
	    fail_if:"It is impossible to find any Hamiltonian cycle in the graph" ,
		exceptions:[
			1:"Wrong edge weigh matrix size.",
			1:"Wrong edge list length.",
			6:"Wrong edge domain values.",
			1:"Wrong weight value.",
			8:"Unknown options"
		]
	]).
	
:-export cycle/4.	
%cycle constraint, additionally with reduced cost variable fixing
cycle(EdgeDstVertexLi,EdgeWeightMx,CycleCost,ConfigList):-
	
	cycle_add_default_config(ConfigList,ConfigListWithDefaults),
	cycle_input_test(EdgeDstVertexLi,EdgeWeightMx,ConfigListWithDefaults),
	cycle_ic(EdgeDstVertexLi,EdgeWeightMx,CycleCost),
	cycle_lp_filter(EdgeDstVertexLi,EdgeWeightMx,CycleCost,ConfigListWithDefaults),
	true.	
:-set_flag(cycle/4, skip, on). 

:- comment(cycle/3, 
	[
	    amode:(cycle(+,++,-) is semidet),
	    args:["Edges":"A list of ic variables", "EdgeWeights":"A square matrix of integers","CycleCost":"The cost of the cycle"],
	    summary:"A constraint that forces a Hamiltonian cycle in a directed graph",
	    desc:ascii("
	Edges is a list of length VertexCount of ic variables, where VertexCount is the number of 
	vertices in the graph. Each variable needs to have a domain which is the subset of 
	[1..VertexCount]. The values in the i-th variable's domain correspond to edges in the 
	graph, so the domain value j of the i-th variable corresponds to an edge (i,j). 

	EdgeWeights is a square matrix (array of arrays) of size VertexCount*VertexCount of nonnegative 
	integers. The value indexed [i,j] corresponds to the cost of the edge (i,j). Values on the diagonal
	([i,i]) are unimportant since the correspond to edges (i,i) which are automatically removed by
	the constraint.

	CycleCost is an ic variable that corresponds to the cost of the cycle. 

	This version of the constraint uses the maximal propagation level. For more details and 
	configuration of different propagation levels see cycle/4. 
		"),
	    %eg:"",
	    see_also:[cycle/4],
	    fail_if:"It is impossible to find any Hamiltonian cycle in the graph" ,
		exceptions:[
			1:"Wrong edge weigh matrix size.",
			1:"Wrong edge list length.",
			6:"Wrong edge domain values.",
			1:"Wrong weight value.",
			8:"Unknown options"
		]
	]).
	
:-export cycle/3.	
%cycle constraint, additionally with reduced cost variable fixing
cycle(EdgeDstVertexLi,EdgeWeightMx,CycleCost):-
	cycle(EdgeDstVertexLi,EdgeWeightMx,CycleCost,[]).
:-set_flag(cycle/3, skip, on). 

	
cycle_input_test(EdgeDstVertexLi,EdgeWeightMx,ConfigList):-
	%edge weight matrix has to be square [VertexCount,VertexCount]
	(dim(EdgeWeightMx,[VertexCount,VertexCount])->
		true
	;
		writeln(error,"Wrong edge weigh matrix size. EdgeWeightMx has to be square."),
		exit_block(cycle_exception)
	),
	%EdgeDstVertexLi has to be of length VertexCount
	(length(EdgeDstVertexLi,VertexCount)->
		true
	;
		writeln(error,"Wrong edge list length. EdgeDstVertexLi has to be of length VertexCount, where VertexCount is the size of each dimension of EdgeWeightMx."),
		exit_block(cycle_exception)
	),
	%EdgeDstVertexLi edges have to be less or equal VertexCount
	(foreach(EdgeDstVertex,EdgeDstVertexLi),
	 count(VertexNr,1,VertexCount),
	 param(VertexCount)do
		get_bounds(EdgeDstVertex,EdgeDstVertexMin,EdgeDstVertexMax),
		(EdgeDstVertexMax=<VertexCount,EdgeDstVertexMin>=1->
			true
		;
			write(error,"Wrong edge domain values. The domains of vertex edge variables have to be in the range 1..VertexCount, where VertexCount is the number of vertices. Error in vertex nr "), write(error,VertexNr),writeln(error,"."),
			exit_block(cycle_exception)
		)
	),
	%edge weights have to be integer and nonnegetive
	(foreachelem(EdgeWeight, EdgeWeightMx,[SrcVertex,DstVertex])do
		(integer(EdgeWeight),EdgeWeight>=0->
			true
		;
			write(error,"Wrong weight value. Edge weights have to be integer and non negative. Error for edge "),
			write(error,[SrcVertex,DstVertex]),writeln(error,"."),
			exit_block(cycle_exception)
		),
		
		%on the diagonal weights are not taken into account 
		(SrcVertex==DstVertex,EdgeWeight=\=0->
			writeln(error,"Wrong weight value. Weights on the diagonal of the EdgeWeightMx are not used and should be always set to 0."),
			exit_block(cycle_exception)
		;true)
	),
	
	%check options in the config list
	subtract(ConfigList,[rc_varfix:yes,rc_varfix:no,cut_planes:yes,cut_planes:no,bound_upd:yes,bound_upd:no,opt_dir:min,opt_dir:max],ConfigListRest),
	(ConfigListRest\==[]->
		write(error,"Unknown options. The following options in ConfigList are not recognizable "),write(error,ConfigListRest),writeln(error,"."),
		exit_block(cycle_exception)
	;true),
	true.
	
cycle_add_default_config(ConfigList,ConfigListWithDefaults):-
	ConfigListDefaults=[rc_varfix:yes,cut_planes:yes,bound_upd:yes,opt_dir:min],
	
	(foreach(DefKey:DefVal,ConfigListDefaults),
	 fromto(ConfigListWithDefaults,CLWDIn,CLWDOut,ConfigList),
	 param(ConfigList)do
		(not(memberchk(DefKey:_UserVal,ConfigList))->
			CLWDIn = [DefKey:DefVal|CLWDOut]
		;
			CLWDIn = CLWDOut
		)
	),
	true.
	
cycle_subtour_elimination(EdgeDstVertexLi):-
	length(EdgeDstVertexLi,VertexCount),	
	dim(EdgeDstVertexAr,[VertexCount]),
    EdgeDstVertexAr=..[[]|EdgeDstVertexLi],
	
	%for each source vertex
	(count(SrcVertexNr,1,VertexCount),
	 param(EdgeDstVertexAr,VertexCount) do 
		%forbid cycles connecting less than all vertices
		arg(SrcVertexNr,EdgeDstVertexAr,EdgeDstVertex),
		CycleLength is VertexCount -2 ,
		
		%for each cycle length connecting less than all vertices
		(count(_CycleLength,1,CycleLength),
		 fromto(EdgeDstVertex,EdgeDstVertexIn,EdgeDstVertexOut,_),
		 param(SrcVertexNr,EdgeDstVertexAr) do
			%forbid cycle of this length (a subtour)
			cycle_arr_element(EdgeDstVertexIn, EdgeDstVertexAr, EdgeDstVertexOut),
			ic:(EdgeDstVertexOut #\= SrcVertexNr)
		)
	),
	true.
	

cycle_arr_element(Index,Array,Value):-
	(ground(Index)->
		arg(Index,Array,Value)
	;
	    suspend(
	        arg(Index,Array,Value),
	        0,
	        [Index->inst],
	        _ThisSusp
	    )
	),
	true.	
	
cycle_ic(EdgeDstVertexLi,EdgeWeightMx,CycleCost):-
	dim(EdgeWeightMx,[VertexCount,VertexCount]),
	
	(foreach(EdgeDstVertex,EdgeDstVertexLi),
	 %construct an array and list of all edges
	 count(VertexNr,1,VertexCount),
	 %construct a list of all edge costs
	 foreach(EdgeWeightVar,EdgeWeightVarLi),
	 param(EdgeWeightMx) do 	
		%do not allow an edge from and to the same node
		ic:(EdgeDstVertex#\=VertexNr),
		%the cost of an edge
		arg(VertexNr,EdgeWeightMx,EdgeWeightAr),
		EdgeWeightAr=..[[]|EdgeWeightLi], 
		ic:element(EdgeDstVertex, EdgeWeightLi, EdgeWeightVar)
	),
	
	%ensures that there is no more than one edge to each node 
	ic_global:alldifferent(EdgeDstVertexLi),
	
	%sum of costs of all edges
	ic_global:sumlist(EdgeWeightVarLi,CycleCost),
	
	%subtour elimination
	cycle_subtour_elimination(EdgeDstVertexLi),

	true.

%linear relaxation and reduced cost variable fixing	
cycle_lp_filter(EdgeDstVertexLi,EdgeWeightMx,CycleCost,ConfigList):-
	CycleLPInstance = cycle_lp,
	eplex_instance(CycleLPInstance),
	length(EdgeDstVertexLi,VertexCount),
	create(array(VertexCount), ToVertexLCOL),
	create(array(VertexCount), FromVertexLCOL),
	create(array(1), CostLCOL),
	
	dim(EdgeIsUsedMx,[VertexCount,VertexCount]),
	
	int_list(1,VertexCount,VertexNrLi),
	
	%for each src vertex
	(foreach(SrcVertex,EdgeDstVertexLi),
	 count(SrcVertexNr,1,_LastVertexNr),
	 param(EdgeIsUsedMx,VertexNrLi,CycleLPInstance,EdgeIsUsedMx,EdgeWeightMx,ToVertexLCOL,FromVertexLCOL,CostLCOL)do
		get_domain_as_list(SrcVertex, DstVertexNrLi),
		
		%fill cells in matrix that correspond to not allowed edges
		subtract(VertexNrLi,DstVertexNrLi,NotAllowedDstVertexNrLi),
		(foreach(NotAllowedDstVertexNr,NotAllowedDstVertexNrLi),
		 param(EdgeIsUsedMx,SrcVertexNr)do
			subscript(EdgeIsUsedMx,[SrcVertexNr,NotAllowedDstVertexNr],no)
		),
		
		%for each dst vertex
		(foreach(DstVertexNr,DstVertexNrLi),
		 param(CycleLPInstance,EdgeIsUsedMx,EdgeWeightMx,SrcVertexNr,SrcVertex,ToVertexLCOL,FromVertexLCOL,CostLCOL) do
			%get var determining if route taken
			
			%connect to the real problem variables
			(true->
				ic:(#=(SrcVertex,DstVertexNr,EdgeIsUsed))
			;
				%this cannot be used now because the reduced cost varfix will not work
				CycleLPInstance:(EdgeIsUsed>=0),
				CycleLPInstance:(EdgeIsUsed=<1)
			),
		
			subscript(EdgeIsUsedMx,[SrcVertexNr,DstVertexNr],EdgeIsUsed),
			
			append_element(ToVertexLCOL, DstVertexNr, EdgeIsUsed),
			append_element(FromVertexLCOL, SrcVertexNr, EdgeIsUsed),
			
			subscript(EdgeWeightMx,[SrcVertexNr,DstVertexNr],EdgeCost),
			append_element(CostLCOL, 1, EdgeIsUsed * EdgeCost)

		)
	),
	
	terminate_all_lists(ToVertexLCOL  , DstVertexNrLi),
	terminate_all_lists(FromVertexLCOL, SrcVertexNrLi),
	terminate_and_get_list(CostLCOL,1, CostLi),
	
	(foreach(SrcVertexNr,SrcVertexNrLi),
	 param(CycleLPInstance,FromVertexLCOL) do
		get_list(FromVertexLCOL, SrcVertexNr, EdgeIsUsedLi),
		%CycleLPInstance:integers(EdgeIsUsedLi),
		CycleLPInstance:(sum(EdgeIsUsedLi)=:=1)
	),		
	
	(foreach(DstVertexNr,DstVertexNrLi),
	 param(CycleLPInstance,ToVertexLCOL) do
		get_list(ToVertexLCOL, DstVertexNr, EdgeIsUsedLi),
		%CycleLPInstance:integers(EdgeIsUsedLi),
		CycleLPInstance:(sum(EdgeIsUsedLi)=:=1)
	),
	
	%read the optimization direction
	memberchk(opt_dir:OptDir,ConfigList),
	(OptDir==min->OppositeDir=max;OppositeDir=min),
	
	%update the bound opposite to the optimization direction, do not keep lp config and solution
	(memberchk(bound_upd:yes,ConfigList)->
		cycle_lp_solve_update_bound(CycleLPInstance,CycleCost,OppositeDir,sum(CostLi),EdgeIsUsedMx,fail,ConfigList)
	;true),
	
	%solve the lp relaxation, keep lp config and solution
	cycle_lp_solve_update_bound(CycleLPInstance,CycleCost,OptDir,sum(CostLi),EdgeIsUsedMx,true,ConfigList),
	CycleLPInstance:eplex_get(cost,OptLpRelaxCost),
	
	(memberchk(rc_varfix:yes,ConfigList)->
		%post reduced cost variable fixing constraints
		cycle_lp_get_var_data(CycleLPInstance,VarOptRcLi),
		cycle_lp_varfix_constraints(CycleCost,OppositeDir,OptLpRelaxCost,VarOptRcLi)
	;true),
	
	CycleLPInstance:eplex_cleanup,
	
	true.
	
	
cycle_lp_solve_update_bound(CycleLPInstance,CycleCost,OptDir,LpCostTe,EdgeIsUsedMx,KeepSolution,ConfigList):-
		shelf_create(bound(0), BoundSh),
		(
			%setup solver
			LpGoalTe=..[OptDir,LpCostTe],
			CycleLPInstance:eplex_solver_setup(LpGoalTe, _OptLpRelaxCost, [sync_bounds(yes), reduced_cost(yes)], []),
			%solve the linear relaxation
			(cycle_lp_solve(EdgeIsUsedMx,CycleLPInstance,ConfigList)->
				true
			;
				%impossible to find a Hamiltonion cycle in this graph, fail without an alternative
				!,fail
			),
			CycleLPInstance:eplex_get(status,LpStatus),
			look_at(LpStatus),
			CycleLPInstance:eplex_get(cost,OptLpRelaxCost),
			to_int(OptLpRelaxCost,OptLpRelaxCostInt),
			shelf_set(BoundSh,1,OptLpRelaxCostInt),
			%fail if solver setup should be backtracked over
			call(KeepSolution),
			!
		;
			true
		),
		
		shelf_get(BoundSh, 1, CycleCostBound),
		(OptDir == max->
			ic:(CycleCost #=< CycleCostBound)
		;
			ic:(CycleCost #>= CycleCostBound)
		),
		shelf_abolish(BoundSh),
	true.
	
cycle_lp_solve(EdgeIsUsedMx,CycleLPInstance,ConfigList):-	
	(memberchk(cut_planes:yes,ConfigList)->
		%strengthen the linear relaxation by cutting planes
		cycle_lp_solve_cut_planes(EdgeIsUsedMx,CycleLPInstance)
	;
		CycleLPInstance:eplex_solve(_OptLpRelaxCost)
	),
	true.
	
	
	
cycle_lp_solve_cut_planes(EdgeIsUsedMx,CycleLPInstance):-

	%solve
	CycleLPInstance:eplex_solve(_OptLpRelaxCost),
	
	%find nonhamiltonian cycles
	%a trick to prevent waking the ic propagation
	suspend(cycle_lp_find_nonhamiltonian_cycles(CycleLPInstance,EdgeIsUsedMx,CyclesLi), 
		2, trigger(cycle_lp_find_nonhamiltonian_cycles)),
	schedule_suspensions(cycle_lp_find_nonhamiltonian_cycles),wake,
	
	(CyclesLi=[_OnlyOneCycle|[]]->
		true
	;
		%add cuts
		cycle_lp_add_cuts(CycleLPInstance,EdgeIsUsedMx,CyclesLi),
		%solve again
		cycle_lp_solve_cut_planes(EdgeIsUsedMx,CycleLPInstance)
	),
	true.
	

cycle_lp_add_cuts(CycleLPInstance,EdgeIsUsedMx,CyclesLi):-
	%for each cycle find edges to the cycle 
	(foreach(Cycle,CyclesLi),
	 param(EdgeIsUsedMx,CycleLPInstance) do
		%check only paths from vertices which are not part of the cycle
		dim(EdgeIsUsedMx,[VertexCount,VertexCount]),
		int_list(1,VertexCount,VertexNrLi),
		subtract(VertexNrLi,Cycle,VertexNotInCycleLi),	 
		%for each  vertex in cycle find edges to the vertex not belonging to cycle
		(foreach(CycleVertexNr,Cycle),
		 foreach(EdgeIsUsedLi,EdgeIsUsedLiLi),
		 param(EdgeIsUsedMx,VertexNotInCycleLi) do
			%for each vertex that i s not in cycle
			(foreach(VertexNotInCycle,VertexNotInCycleLi),
			 fromto(EdgeIsUsedLi,EdgeIsUsedLiIn,EdgeIsUsedLiOut,[]),
			 param(CycleVertexNr,EdgeIsUsedMx)do
				subscript(EdgeIsUsedMx,[VertexNotInCycle,CycleVertexNr],EdgeIsUsed),
				%check if this edge can be active
				(EdgeIsUsed \==no->
					EdgeIsUsedLiIn=[EdgeIsUsed|EdgeIsUsedLiOut]
				;
					EdgeIsUsedLiIn = EdgeIsUsedLiOut
				)
			)

		),
		flatten(EdgeIsUsedLiLi,EdgeIsUsedLi),
		(EdgeIsUsedLi==[]->
			%it is impossible to eliminate this subtour, the structure if the problem implies no solution
			fail
		;true),
		%forbid subtour
		CycleLPInstance:(sum(EdgeIsUsedLi)>=1)
	),
	true.	
	
cycle_lp_find_nonhamiltonian_cycles(CycleLPInstance,EdgeIsUsedMx,CyclesLi):-

	bag_create(CyclesBag),
	
	(
		CycleLPInstance:eplex_get(vars,VarsTe),
		CycleLPInstance:eplex_get(typed_solution,VarsOptSolTe),
		(foreacharg(Var,VarsTe),
		 foreacharg(VarOptSol,VarsOptSolTe)do
			to_int(VarOptSol,VarOptSolInt),
			Var = VarOptSolInt
		),
		
		dim(EdgeIsUsedMx,[VertexCount,VertexCount]),
		int_list(1,VertexCount,VertexNrLi),
		
		%find cycles
		(fromto(VertexNrLi,VertexNrLiIn,VertexNrLiOut,[]),
		 param(EdgeIsUsedMx,CyclesBag) do
			VertexNrLiIn = [SrcVertexNr|_FreeVertexNrLi],
			
			(fromto(SrcVertexNr,CurVertexNr,NxtVertexNr,[]),
			 foreach(CurVertexNr,CycleVertexNrLi),
			 param(SrcVertexNr,EdgeIsUsedMx) do 
				cycle_lp_dst_vertex(CurVertexNr,EdgeIsUsedMx,DstVertexNr),
				(DstVertexNr == SrcVertexNr ->
					NxtVertexNr = []
				;
					DstVertexNr = NxtVertexNr
				)
			),
			subtract(VertexNrLiIn, CycleVertexNrLi, VertexNrLiOut),
			bag_enter(CyclesBag,CycleVertexNrLi)
		),
		fail
	;
		bag_dissolve(CyclesBag, CyclesLi)
	
	),
	
	true.
	
int_list(From,To,List):-
	(count(Nr,From,To),
	 foreach(Nr,List) do
		true
	),
	true.
	
look_at(_T):- true.
	
cycle_lp_dst_vertex(SrcVertexNr,EdgeIsUsedMx,DstVertexNr):-
	arg(SrcVertexNr,EdgeIsUsedMx,EdgeIsUsedAr),
	(foreacharg(EdgeIsUsed,EdgeIsUsedAr,Idx),
	 param(DstVertexNr) do
		(ground(EdgeIsUsed),EdgeIsUsed\==no,EdgeIsUsed =:= 1->
			DstVertexNr = Idx
		;true)
	),
	true.
	
cycle_lp_get_var_data(CycleLPInstance,VarOptRcLi):-

	CycleLPInstance:eplex_get(vars,VarsTe),
	
	(foreacharg(Var,VarsTe),
     fromto(VarOptRcLi,VarOptRcLiIn,VarOptRcLiOut,[]),
	 param(CycleLPInstance)do
		CycleLPInstance:eplex_var_get(Var, reduced_cost, VarRc),
		(true->
			CycleLPInstance:eplex_var_get(Var, typed_solution, VarOptVal),
			float(VarRc,VarRcFloat),
			VarOptRc = var_opt_rc(Var,VarOptVal,VarRcFloat),
			VarOptRcLiIn = [VarOptRc|VarOptRcLiOut]
		;
			VarOptRcLiIn = VarOptRcLiOut
		)
	),
	true.

	
cycle_lp_varfix_constraints(CycleCost,OppositeDir,OptLpRelaxCost,VarOptRcLi):-
	
    (foreach(VarOptRc,VarOptRcLi),
	 param(CycleCost,OptLpRelaxCost,OppositeDir)do
		VarOptRc = var_opt_rc(Var,VarOptVal,VarRc),
			(VarRc=\=0,VarOptVal=:=0->
				cycle_lp_varfix(OppositeDir,Var,VarRc,CycleCost,OptLpRelaxCost)
			;true)
	),	 
	true.	
	
%reduced cost variable fixing
cycle_lp_varfix(OppositeDir,Var,VarRc,CycleCost,OptLpRelaxCost):-
	(OppositeDir==max->
		get_max(CycleCost,CycleCostBound)
	;
		get_min(CycleCost,CycleCostBound)
	),
	
	( (1>(CycleCostBound-OptLpRelaxCost)/ VarRc)->
		Var = 0
	;
		(var(CycleCost)->
			suspend(
				cycle_lp_varfix(OppositeDir,Var,VarRc,CycleCost,OptLpRelaxCost),
				0,
				[CycleCost->ic:OppositeDir],
				_ThisSusp
			)
		;true)
	),
	true.
	
to_int(Number,NumberInt):-
	round(Number, NumberRounded),
	integer(NumberRounded,NumberInt).

	
cycle_test_generate_doc:-
	lib(document),
	get_flag(cwd, Cwd),
	concat_string([Cwd,"cycle.ecl"],File),
	document:icompile(File),
	document:eci_to_html("cycle.eci", Cwd, ""),

	true.
	