:- getcwd(D), get_flag(library_path, P), set_flag(library_path, [D|P]).
:- use_module(flatzinc).
:- use_module(minizinc).
%:- lib(flatzinc).
%:- lib(minizinc).

:- lib(document).

doc :-
	Libs = [minizinc, flatzinc, flatzinc_parser, fzn_ic, fzn_fd, fzn_eplex],
	( foreach(Lib,Libs) do icompile(Lib) ),
	ecis_to_htmls([.],doc,"").


fzn_test(OutFile) :-
	open(OutFile, write, Out),
	set_stream(output, Out),
	fzn_test,
	close(Out).

fzn_test :-
%	fzn_run("test", fzn_ic),
	fzn_run("mzn_examples/2DPacking", fzn_ic),
	fzn_run("mzn_examples/alpha", fzn_ic),
	fzn_run("mzn_examples/blocksworld_instance_1", fzn_ic),
	fzn_run("mzn_examples/blocksworld_instance_2", fzn_ic),
	fzn_run("mzn_examples/domino", fzn_ic),
	fzn_run("mzn_examples/eq20", fzn_ic),
%	fzn_run("mzn_examples/factory_planning_instance", fzn_ic),
	fzn_run("mzn_examples/golomb", fzn_ic),
	fzn_run("mzn_examples/grocery", fzn_ic),
	fzn_run("mzn_examples/jobshop2x2", fzn_ic),
	fzn_run("mzn_examples/langford", fzn_ic),
	fzn_run("mzn_examples/min_cost_flow", fzn_eplex),

	fzn_run("mzn_examples/multidimknapsack_simple", fzn_ic),
	fzn_run("mzn_examples/multidimknapsack_simple", fzn_fd),
	fzn_run("mzn_examples/multidimknapsack_simple", fzn_eplex),

	fzn_run("mzn_examples/oss", fzn_ic),
	fzn_run("mzn_examples/packing", fzn_ic),
	fzn_run("mzn_examples/perfsq", fzn_ic),
	fzn_run("mzn_examples/photo", fzn_ic),
	fzn_run("mzn_examples/product", fzn_ic),
	%fzn_run("mzn_examples/product_fd", fzn_ic),	% long
	fzn_run("mzn_examples/product_lp", fzn_eplex),

	fzn_run("mzn_examples/queen_cp2", fzn_ic),
	fzn_run("mzn_examples/queen_ip", fzn_ic),
	fzn_run("mzn_examples/queen_ip", fzn_eplex),

	fzn_run("mzn_examples/radiation", fzn_ic),
	fzn_run("mzn_examples/simple_sat", fzn_ic),
	fzn_run("mzn_examples/singHoist2", fzn_ic),
	fzn_run("mzn_examples/sudoku", fzn_ic),

%	fzn_run("mzn_examples/timetabling", fzn_ic), % long

	fzn_run("mzn_examples/tiny_tsp", fzn_ic),
	fzn_run("mzn_examples/warehouses", fzn_ic),
	fzn_run("mzn_examples/zebra", fzn_ic),

	true.

mzn_run(File, Options) :-
	printf("---------- %s ----------%n", [File]),
	subcall(minizinc:mzn_run(File, Options), DG),
	length(DG, NDG),
	( NDG > 0 -> printf("There are %d delayed goals%n", [NDG]) ; true ).

mzn_test :-
%	IntSolver = zn_options{solver:fzn_ic,setup_prio:2},
	IntSolver = zn_options{solver:fzn_ic,var_names:on},
%	IntSolver = fzn_ic,
%	IntSolver = fzn_fd,
	mzn_run("mzn_examples/2DPacking", IntSolver),
	mzn_run("mzn_examples/alpha", IntSolver),
%	mzn_run("mzn_examples/blocksworld_instance_1", IntSolver),
%	mzn_run("mzn_examples/blocksworld_instance_2", IntSolver),
%	mzn_run("mzn_examples/domino", IntSolver),
	mzn_run("mzn_examples/eq20", IntSolver),
%	mzn_run("mzn_examples/factory_planning_instance", IntSolver),
	mzn_run("mzn_examples/golomb", IntSolver),
	mzn_run("mzn_examples/grocery", IntSolver),
	mzn_run("mzn_examples/jobshop2x2", IntSolver),
	mzn_run("mzn_examples/langford", IntSolver),
	mzn_run("mzn_examples/min_cost_flow", fzn_eplex),

	mzn_run("mzn_examples/multidimknapsack_simple", IntSolver),
	mzn_run("mzn_examples/multidimknapsack_simple", fzn_eplex),

	mzn_run("mzn_examples/oss", IntSolver),
	mzn_run("mzn_examples/packing", IntSolver),
	mzn_run("mzn_examples/perfsq", IntSolver),
	mzn_run("mzn_examples/photo", IntSolver),
	mzn_run("mzn_examples/product", fzn_ic),	% floats
	%mzn_run("mzn_examples/product_fd", IntSolver),	% long
%	mzn_run("mzn_examples/product_lp", fzn_eplex),

	mzn_run("mzn_examples/queen_cp2", IntSolver),
	mzn_run("mzn_examples/queen_ip", IntSolver),
%	mzn_run("mzn_examples/queen_ip", fzn_eplex),

	mzn_run("mzn_examples/radiation", fzn_ic),
	mzn_run("mzn_examples/simple_sat", IntSolver),
	mzn_run("mzn_examples/singHoist2", IntSolver),
	mzn_run("mzn_examples/sudoku", IntSolver),

%	mzn_run("mzn_examples/timetabling", IntSolver), % long

	mzn_run("mzn_examples/tiny_tsp", fzn_ic),
	mzn_run("mzn_examples/warehouses", IntSolver),
	mzn_run("mzn_examples/zebra", IntSolver),

	true.


fzn_parse(File) :-
	open(File, read, Stream),
	repeat,
%	( flatzinc_parser:read_item(Stream, Item) ->
	( read(Stream, Item)@flatzinc_syntax, Item \= end_of_file ->
	    fail
	;
	    !,
	    close(Stream)
	).



mzn_test1 :-
	mzn_run_string(
	    "
		include \"globals.mzn\";

		int: m = 4;
		int: n = m*m;

		array [1..m] of var 0..n: mark;

		array[1..(m*(m-1)) div 2] of var 0..n: differences =
		    [ mark[j] - mark[i] | i in 1..m, j in i+1..m];

		constraint mark[1] = 0;

		constraint forall ( i in 1..m-2 ) ( mark[i] < mark[i+1] );

		constraint all_different(differences);

		constraint mark[2] - mark[1]  <  mark[m] - mark[m-1];

		solve minimize mark[m];

		output [\"golomb \", show(mark), \"\\n\"];
	    ",
	    fzn_ic).


mzn_test2 :-
	mzn_load_string(
	    '
		include "globals.mzn";

		int: m = 4;
		int: n = m*m;

		array [1..m] of var 0..n: mark;

		array[1..(m*(m-1)) div 2] of var 0..n: differences =
		    [ mark[j] - mark[i] | i in 1..m, j in i+1..m];

		constraint mark[1] = 0;

		constraint forall ( i in 1..m-2 ) ( mark[i] < mark[i+1] );

		constraint all_different(differences);

		constraint mark[2] - mark[1]  <  mark[m] - mark[m-1];

		solve minimize mark[m];	% dummy

		output ["golomb ", show(mark), "\\n"];
	    ',
	    fzn_ic, [], [], State),
	fzn_search(State),
	fzn_output(State),
	fzn_output(State).


:- lib(ic).
:- lib(branch_and_bound).

mzn_test3(M, Mark) :-

	% MiniZinc model with ECLiPSe parameter M inserted (deprecated)
	mzn_load_string(['
		include \"globals.mzn\";

		int: m = ',
	    M, ';
		int: n = m*m;

		array [1..m] of var 0..n: mark;

		array[1..(m*(m-1)) div 2] of var 0..n: differences =
		    [ mark[j] - mark[i] | i in 1..m, j in i+1..m];

		constraint mark[1] = 0;

		constraint forall ( i in 1..m-2 ) ( mark[i] < mark[i+1] );

		constraint all_different(differences);

		constraint mark[2] - mark[1]  <  mark[m] - mark[m-1];

		solve minimize mark[m];	% only for defining cost var

		output ["golomb ", show(mark), "\\n"];
	    '],
	    zn_options{solver:fzn_ic,parser:strict,var_names:on},
	    [],
	    [],
	    State
	),

	% Search in ECLiPSe
	fzn_var_lookup(State, mark, Mark),
	fzn_obj_lookup(State, Cost),

	minimize(labeling(Mark), Cost),

	% Output in ECLiPSe
	fzn_var_lookup(State, n, N),
	printf("For M=%d, N=%d, optimum is %d%n", [M,N,Cost]),

	% Standard MiniZinc output
	fzn_output(State).


mzn_test4(M, Mark) :-

	% MiniZinc model with ECLiPSe parameter M inserted
	mzn_load_string(['
		include \"globals.mzn\";

		int: m;
		int: n = m*m;

		array [1..m] of var 0..n: mark;

		array[1..(m*(m-1)) div 2] of var 0..n: differences =
		    [ mark[j] - mark[i] | i in 1..m, j in i+1..m];

		constraint mark[1] = 0;

		constraint forall ( i in 1..m-2 ) ( mark[i] < mark[i+1] );

		constraint all_different(differences);

		constraint mark[2] - mark[1]  <  mark[m] - mark[m-1];

		solve minimize mark[m];	% only for defining cost var

		output ["golomb ", show(mark), "\\n"];
	    '],
	    zn_options{solver:fzn_ic,parser:strict},
	    [m=M],
	    [],
	    State
	),

	% Search in ECLiPSe
	fzn_var_lookup(State, mark, Mark),
	fzn_obj_lookup(State, Cost),

	minimize(labeling(Mark), Cost),

	% Output in ECLiPSe
	fzn_var_lookup(State, n, N),
	printf("For M=%d, N=%d, optimum is %d%n", [M,N,Cost]),

	% Standard MiniZinc output
	fzn_output(State).


queens(N, Q) :-
	mzn_load(queens, fzn_ic, [n=N], [q=Q], FznState),
	labeling(Q),
	fzn_output(FznState).

queen8(Q) :-
	mzn_load("mzn_examples/queen_cp2",
		zn_options{solver:fzn_ic,var_names:on},
		[], [q=Q], FznState),
	labeling(Q),
	fzn_output(FznState).

queens8 :-
	mzn_run_string("
		int: n = 8;
		array [1..3,1..n] of var 1..n: q1 :: is_output;
		array [1..n] of var 1..n: q :: is_output;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\
			q[i] + i != q[j] + j /\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    ", fzn_ic).


steiner :-
	mzn_run_string("
	    int : n = 9;
	    int : nb = n * (n-1) div 6 ;
	    array [1..nb] of var set of 1..n : sets;
	    constraint forall (i in 1..nb) (card(sets[i]) = 3);
	    constraint forall (i in 1..nb, j in (i+1)..nb) (card(sets[i] intersect sets[j]) <= 1);
	    solve::set_search(sets,input_order,indomain_min,complete) satisfy;
%	    solve::set_search(sets,\"input_order\",\"indomain_min\",\"complete\") satisfy;
%	    solve satisfy;	% searches on cardinality first, slow
	", _).



% TODO: output currently does not print according to type!

params :-
	mzn_load_string("
		int : n;
		bool : b;
		set of 1..9 : s;
		array[1..3] of bool : ab;
		solve satisfy;
		output [
		    \"\\n n=\",show(n),
		    \"\\n b=\",show(b),
		    \"\\n s=\",show(s),
		    \"\\n ab=\",show(ab),
		    \"\\n\"];
	    ", _,
	    [
		n = 3,
		bool : b = 0,
		(set of int) : s = [3,5,7],
		(array([int]) of bool):ab = [](0,1,1)
	    ],
	    [],
	FznState),
	fzn_search(FznState),
	fzn_output(FznState).


% minizinc 0.8 extensions

mzn_test5 :-
	mzn_run_string(
	    "
		var int: m = 3;

		solve satisfy;

	    ",
	    fzn_ic).

fzn_test6 :-
	open(string("
	    var int: x :: is_defined_var;
	    constraint int_plus(3, 4, x) :: defines_var(x);
	    solve satisfy;

	    "), read, Stream),
	fzn_run_stream(Stream, fzn_ic).

fzn_test7:-
	open(string("
	    predicate all_different(array[int] of var int:xs);
	    solve satisfy;
	    "), read, Stream),
	fzn_run_stream(Stream, fzn_ic).

out1 :-
	mzn_run_string("
		annotation third;
		annotation foo;
		annotation bar(int:i);
		annotation mya(ann:a1,int:i,ann:a3);
		var 1..3: x :: is_output :: mya(foo,29,bar(9)) :: third;
		solve satisfy;
	    ", zn_options{solver:fzn_ic,solutions:all}).

out2 :-
	mzn_run_string("
		var 1..3: x :: is_output;
		var 1..3: y :: is_output;
		solve minimize y-x;
	    ", zn_options{solver:fzn_ic,solutions:0}).


clause :-
	open(string("
		array[1..3] of var bool : p :: output_array([1..3]);
		array[1..3] of var bool : n :: output_array([1..3]);
		var int: cost;
		constraint bool_clause(p, n);
		constraint int_lin_eq([1,1,1],n,cost);
		solve maximize cost;
	    "), read, Stream),
	fzn_run_stream(Stream, zn_options{solver:fzn_ic,parser:fast}).
