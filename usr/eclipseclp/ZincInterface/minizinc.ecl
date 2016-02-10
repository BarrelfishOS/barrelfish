%----------------------------------------------------------------------
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
% The Original Code is  The Zinc Modelling interface for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf
% 
% END LICENSE BLOCK
%----------------------------------------------------------------------

:- module(minizinc).

:- comment(date, "$Date: 2012/10/23 00:38:15 $").
:- comment(categories, ["Interfacing","Constraints"]).
:- comment(summary, "Utilities for using MiniZinc with ECLiPSe").
:- comment(author, "Joachim Schimpf, supported by Cisco Systems and NICTA Victoria").
:- comment(copyright, "Cisco Systems Inc, licensed under CMPL").
:- comment(see_also, [
	flatzinc:struct(zn_options),
	library(flatzinc),
	library(fzn_ic),
	library(fzn_fd),
	library(fzn_eplex)
    ]).
:- comment(status, prototype).
:- comment(desc, html("
<H3>
Overview
</H3>
<P>
This module allows to run MiniZinc models with ECLiPSe.
MiniZinc models can be either read from a file or stream,
or they can be embedded as strings into ECLiPSe code.
The implementation relies on an external MiniZinc-to-FlatZinc converter,
e.g. mzn2fzn, and on the FlatZinc interpreter lib(flatzinc).
Mappings to different ECLiPSe solvers are possible via the solver
mapping libraries fzn_ic, fzn_fd, fzn_eplex, etc.
</P>

<H3>
Running MiniZinc Models without using this Library
</H3>
<P>
You can run a MiniZinc model by first converting it to FlatZinc yourself,
and then using the lib(flatzinc) library. This can be done either via
an intermediate .fzn file, or by piping the resulting FlatZinc model
into the ECLiPSe-FlatZinc interpreter using e.g.
<PRE>
% mzn2fzn --output-to-stdout model.mzn | eclipse -e \"flatzinc:fzn_run(fzn_ic)\"
</PRE>
This should work as long as the mzn2fzn command is in your PATH.
Note that mzn2fzn is currently not included with ECLiPSe but comes
with the Melbourne MiniZinc distribution.  You must also make sure that
the correct specialised global constraint definitions are used,
by including e.g. lib/fzn_ic in mzn2fzn's search path via its -I option.
For more details see lib(flatzinc).
</P>

<H3>
Running MiniZinc Models using this Library
</H3>
<P>
This library allows you to do everything from within ECLiPSe and let ECLiPSe
invoke the MiniZinc to FlatZinc translator (mzn2fzn) internally with the
correct arguments.  The model can be contained in a file:
<PRE>
?- mzn_run(\"model.mzn\", fzn_ic).
</PRE>
or, if a data instance file is used
<PRE>
?- mzn_run(\"model.mzn\", \"instance.dzn\", fzn_ic).
</PRE>
Since MiniZinc models are typically small, they can also be embedded as
a string into ECLiPSe code. For example:
<PRE>
    queens8 :-
	mzn_run_string(\"
		int: n = 8;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \", fzn_ic).
</PRE>
Note that, because of the rules for escaping characters within
ECLiPSe strings, the backslashes had to be doubled!
</P>

<H3>
Installation
</H3>
<P>
This version is intended to to work with Minizinc 1.1 or later!
<P>
In order to be found by lib(minizinc), the Melbourne Minizinc-to-Flatzinc
converter mzn2fzn must be installed in a directory called <CODE>minizinc-&lt;version&gt;</CODE>
in one of the following locations (where we write &lt;ECLIPSEDIR&gt; for
the ECLiPSe installation directory, and &lt;ECLIPSEARCH&gt; for
the name for the machine architecture, e.g. i386_nt for Windows, i386_linux
for Linux):
<OL>
<LI>Directory specified by <CODE>$ECLIPSEMZN</CODE> environment variable</LI>
<LI>The user's home directory, as indicated by $HOME or $HOMEPATH</LI>
<LI><CODE>&lt;location of lib(minizinc)&gt;/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;/lib_public/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;/lib/&lt;ECLIPSEARCH&gt;</CODE></LI>
<LI><CODE>&lt;ECLIPSEDIR&gt;</CODE></LI>
<LI>Parent of <CODE>&lt;ECLIPSEDIR&gt;</CODE> (e.g. \"C:/Program Files\" on Windows)</LI>
<LI>Directory specified by <CODE>$PROGRAMFILES</CODE> environment variable</LI>
</OL>
<P>
You can also set the environment variable ECLIPSEMZN (on Windows alternatively
the registry entry HKLM/SOFTWARE/IC-Parc/Eclipse/<version>/ECLIPSEMZN)
to the Minizinc installation directory (or to its parent).


<H3>
Combining a MiniZinc model with Search or I/O in ECLiPSe
</H3>
<P>
There are several reasons why one might want to embed a MiniZinc model
into an ECLiPSe program:
<UL>
<LI>Passing parameters from the ECLiPSe program to the MiniZinc model</LI>
<LI>Getting the model solutions back into ECLiPSe</LI>
<LI>Programming custom search in ECLiPSe</LI>
<LI>Doing custom output beyond what the Zinc output annotations can do</LI>
</UL>
</P><P>
To pass a parameter into a MiniZinc model, a generic MiniZinc model must
be provided, together with a parameter map.
This map is an ECLiPSe list that corresponds to a MiniZinc (actually
FlatZinc) instance file:
<PRE>
queens(N) :-
	mzn_run_string(\"
		int: n;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \",
	    [n=N],	% parameter map: ZincId=EclipseValue
	    fzn_ic).
</PRE>
Alternatively, the generic model can be kept separately in a MiniZinc file:
<PRE>
queens(N) :-
	mzn_run(\"n_queens.mzn\", [n=N], fzn_ic).
</PRE>
<P>
With the above exmples, search and output are still completely specified
in MiniZinc.
</P><P>
To add your own search routine and/or output, use mzn_load_string/5 or
mzn_load/5. This has the effect of only loading the MiniZinc model
(i.e. setting up the constraints), but then returning to ECLiPSe without
executing any MiniZinc solve or output primitives.  The rest of the work
can then be done in ECLiPSe:
</P>
<PRE>
queens(N, Q) :-
	mzn_load(\"n_queens.mzn\", fzn_ic, [n=N], [q=Q], FznState),
	labeling(Q),
	fzn_output(FznState).
</PRE>
The [q=Q] mapping gives access to the ECLiPSe array Q corresponding to
the MiniZinc array q. This is a normal ECLiPSe array of lib(ic) domain
variables, and can be used for doing search, or outputting the results.
In the example however, we have fallen back onto the FlatZinc output
routine to display the results after search has finished.
</P><P>
Note that even if you do your own search in ECLiPSe, your MiniZinc model
must contain a solve item to be syntactically correct (and to specify
the objective, if any).
</P>

<H3>
Options
</H3>
Instead of just the name of the solver mapping (<CODE>fzn_ic</CODE> in
our examples), a <CODE>zn_options{}</CODE> structure can be given to
customize the behaviour further, e.g.
<PRE>
	mzn_run(File, zn_options{solver:fzn_eplex,var_names:on}.
</PRE>
<DL>
<DT>solver (default: fzn_ic)</DT><DD>
    Determines which ECLiPSe solvers are used.  The name is the
    name of a library implementing the mapping, e.g. fzn_ic,
    fzn_fd or fzn_eplex.
</DD>
<DT>solutions (default: 1)</DT><DD>
    The maximum number of solutions computed. Only effective if using
    builtin search and not optimizing. (0 or all = all solutions)
</DD>
<DT>setup_prio (default: 0)</DT><DD>
    The priority under which the constraint setup will be executed
    (see call_priority/2 and get_priority/1). Possible values are
    the ECLiPSe priorities 1 to 12, or 0 (the default) which stands
    for the current priority of the calling code.  A sensible value
    for this option is 2, which means that the setup code is executed
    under high priority (still allowing debug/visualisation goals).
    The effect of such a setting is that no propagation occurs until
    all constraints are fully set up, possibly leading to time savings.
</DD>
<DT>parser (default: fast)</DT><DD>
    Whether to use a 'strict' or 'fast' parser for FlatZinc input.
</DD>
<DT>var_names (default: off)</DT><DD>
    Use lib(var_name) to label ECLiPSe variables with their Zinc names.
    This is useful for debugging.
</DD>
<DT>fzn_tmp (default: file)</DT><DD>
    Use a 'pipe' or intermediate 'file' for FlatZinc.
</DD>
</DL>

<H3>
Mapping between MiniZinc/FlatZinc Data and ECLiPSe Data
</H3>
<P>
When using ECLiPSe with a Mini/FlatZinc model, one needs to be aware of
the mapping from MiniZinc to FlatZinc (e.g. flattening of arrays),
and the representation of FlatZinc data in ECLiPSe.
</P><P>
Note that the ECLiPSe-side representation depends in part on the chosen
solver mapping. The following table shows the mapping used with fzn_ic
(which employs the lib(ic) and lib(ic_sets) solver libraries):
<PRE>
	FlatZinc Type/Syntax		ECLiPSe Type/Syntax
	-----------------------------------------------------------
	string				string
	e.g.	\"abc\"			\"abc\"

	bool (false/true)		integer (0/1)
	e.g.	false			0

	int				integer
	e.g.	33			33

	float				float or breal
	e.g.	3.4			3.399__3.401

	set of int			ordered list of integer
	e.g.	{1,5,4}			[1,4,5]
		1..3			[1,2,3]

	array[1..N] of T		structure with functor []/N
	e.g.	[23,54,0]		[](23,54,0)

	var bool			lib(ic) integer variable

	var int				lib(ic) integer variable

	var float			lib(ic) continuous variable

	var set of int			lib(ic_sets) set variable
</PRE>
</P>
")).


% The location of this file, when loaded.
% Used to find the ECLiPSe/Solver specific globals.mzn file
:- local variable(here).
?- getcwd(Cwd), setval(here, Cwd).

% Location of MiniZinc installation (with bin and lib subdirectories)
% We try a couple of locations heuristically
:- local
	variable(minizinc_dir, ''),
	variable(mzn2fzn_exe, "mzn2fzn"),
	initialization((
	    get_flag(installation_directory, EclDir),
	    get_flag(hostarch, Arch),
	    getval(here, Here),
	    findall(Dir2, (
		    ( Dir1 = "$ECLIPSEMZN/"
		    ; Dir1 = "$HOME/"
		    ; Dir1 = "$HOMEPATH/"
		    ; concat_string([Here,Arch,/], Dir1)
		    ; concat_string([EclDir,"/lib_public/",Arch,/], Dir1)
		    ; concat_string([EclDir,"/lib/",Arch,/], Dir1)
		    ; concat_string([EclDir,"/"], Dir1)
		    ; concat_string([EclDir,"/../"], Dir1)
		    ; Dir1 = "$PROGRAMFILES/"
		    ),
		    canonical_path_name(Dir1, Dir2)
		), Dirs),
	    (
                (
		    canonical_path_name("$ECLIPSEMZN/", MznDir)
                ;
                    member(Dir, Dirs),
                    exists(Dir),
                    read_directory(Dir, "", SubDirs0, _),
                    sort(0, >=, SubDirs0, SubDirs),	% attempt to prefer newer ones
                    member(Sub, SubDirs),
                    member(Prefix, ["minizinc-","MiniZinc "]),
                    substring(Sub, Prefix, 1),
                    concat_string([Dir,Sub,/], MznDir)
                ),
                ( ( substring(MznDir, "minizinc-0", _)
                  ; substring(MznDir, "minizinc-1.0", _)) -> % require 1.1 at least
                    printf(warning_output, "Ignoring old version %w%n", [MznDir]),
                    fail
                ;
                    true
                ),
		( concat_string([MznDir,"bin/private/mzn2fzn"], Mzn2Fzn)
		; concat_string([MznDir,"bin/actual/mzn2fzn"], Mzn2Fzn)
		; concat_string([MznDir,"bin/private/mzn2fzn-actual"], Mzn2Fzn)
		),
		existing_file(Mzn2Fzn, ["",".exe"], [readable], _Mzn2FznExe)
	    ->
		setval(minizinc_dir, MznDir),
		setval(mzn2fzn_exe, Mzn2Fzn),
		os_file_name(MznDir, MznDirOS),
		printf(log_output, "Using minizinc installation at %w%n", [MznDirOS])
	    ;
		printf(log_output, "No usable minizinc installation found in either of:%n", []),
		( foreach(Dir,Dirs) do writeln(log_output, Dir) ),
		printf(log_output, "Will rely on PATH instead%n", [])
	    )
	)).

% Global counter for generating temp file name
:- local variable(tmpcnt, 1).

:- lib(lists).
:- use_module(flatzinc).
:- reexport struct(_) from flatzinc.


% redefined the of/2 expansion to avoid warnings for "array/set of ..."
:- export macro((of)/2, tr_of/2, []).
tr_of(OfTerm, Expanded) :-
	OfTerm =..[of,_,B],
	atom(B),
	\+ fzn_simple_type(B),
	expand_macros(OfTerm, Expanded)@eclipse_language.


%----------------------------------------------------------------------
% Top level predicates
%----------------------------------------------------------------------

:- export mzn_run/2.
:- comment(mzn_run/2, [
    summary:"Run a MiniZinc model from a given file",
    amode:(mzn_run(+,++) is det),
    args:["File":"File name (extension defaults to .mzn)",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[fzn_run/2, mzn_run/3, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Reads a MiniZinc model from a file, and interprets it using
	the solver mapping defined in SolverOrOptions.  At the end of
	solving, results are printed to the output stream, timing and
	progress messages are printed to the log_output stream, warnings
	to the warning_output stream, and error messages the error stream.
	This predicate always succeeds.
    </P>"),
    eg:"
    ?- mzn_run(\"mymodel.mzn\", fzn_ic).
    Found a solution with cost 10
    Found no solution with cost 7.0 .. 9.0
    end = 10
    b1 = 1
    b2 = 0
    b3 = 1
    b4 = 0
    Objective value = 10
    Total time 0.031s cpu (0.016 setup + 0.000 search)

    ?- mzn_run(queens8, zn_options{solver:fzn_ic,solutions:3}).
    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
    q = [1,6,8,3,7,4,2,5]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
    q = [1,7,4,6,8,2,5,3]
    Total time 0.016s cpu (0.016 setup + 0.000 search)
"]).
mzn_run(ModelFile, SolverOrOptions) :-
	mzn_run(ModelFile, [], SolverOrOptions).


:- export mzn_run/3.
:- comment(mzn_run/3, [
    summary:"Run a MiniZinc model from a given model and instance file",
    amode:(mzn_run(+,+,++) is det),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"InstFileOrParMap":"Instance file name (extension defaults to .dzn, then .mzn), or list of Id=Term correspondences",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[mzn_run/2, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Reads a MiniZinc model (given a model file and an instance
	file) and interprets it using the solver mapping defined in
	SolverOrOptions.  At the end of solving, results are printed
	to the output stream, timing and progress messages are printed
	to the log_output stream, warnings to the warning_output
	stream, and error messages the error stream.  This predicate
	always succeeds.
    </P>"),
    eg:"
    ?- mzn_run(\"mymodel.mzn\", \"myinstance.mzn\", fzn_ic).
    Found a solution with cost 10
    Found no solution with cost 7.0 .. 9.0
    end = 10
    b1 = 1
    b2 = 0
    b3 = 1
    b4 = 0
    Objective value = 10
    Total time 0.031s cpu (0.016 setup + 0.000 search)

    ?- mzn_run(\"queens.mzn\", [n=8], fzn_ic).
    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.015s cpu (0.000 setup + 0.000 search)
"]).
mzn_run(ModelFile0, ParMapOrFile, SolverOrOptions) :-
	zn_options(SolverOrOptions, Options),
	( is_list(ParMapOrFile) ->
	    pars_to_instancefile(ParMapOrFile, MznInstFile, Options),
	    mzn2fzn(ModelFile0, MznInstFile, Options, FznStream, PidOrFile),
	    delete_file(MznInstFile)
	;
	    mzn2fzn(ModelFile0, ParMapOrFile, Options, FznStream, PidOrFile)
	),
%        Options = zn_options{output:SolOut},
%        exec([solns2out,'msq.ozn'], [SolOut], _Pid),
	fzn_run_stream(FznStream, Options),
%        close(SolOut),
	mzn2fzn_cleanup(PidOrFile).


:- export mzn_run_string/2.
:- comment(mzn_run_string/2, [
    summary:"Run a MiniZinc model given as a string or list",
    amode:(mzn_run_string(++,++) is det),
    args:["MznModel":"String, Atom or List of constants",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure"],
    see_also:[mzn_run/2, mzn_run/3, struct(zn_options)],
    desc:html("<P>
	Solves the MiniZinc model MznModel, given in the simplest form
	as a string in MiniZInc syntax.  The problem is solved using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Search and output are done according
	to the model's solve and output items.
    </P><P>
	Note that, because of the rules for escaping characters within
	ECLiPSe strings, any backslashes in the MiniZinc source have
	to be doubled, and double quotes must be escaped with a backslash!
    </P><P>
	Obviously, one would like to pass parameters into a model.  The
	model can therefore  be given as a list of strings in MiniZinc
	syntax, interleaved with ECLiPSe ground terms that serve as 
	parameter instantiations.  The actual MiniZinc model then
	consists of the concatenation of all these parts.
    </P>"),
    eg:"
    ?- mzn_run_string(\"
		int: n = 8;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \", fzn_ic).

    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.020s cpu (0.020 setup+ 0.000 search)
    Yes (0.02s cpu, solution 1, maybe more)


    ?- N=8, mzn_run_string([\"
		int: n = \",
	    N, \";
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \"], fzn_ic).

    Starting search
    q = [1,5,8,6,3,7,2,4]
    Total time 0.020s cpu (0.020 setup+ 0.000 search)
    N = 8
    Yes (0.02s cpu, solution 1, maybe more)
</PRE>
"]).

mzn_run_string(MznModel, SolverOrOptions) :-
	mzn_run_string(MznModel, SolverOrOptions, []).

mzn_run_string(MznModel, SolverOrOptions, ParMap) :-
	(
	    mzn_load_string(MznModel, SolverOrOptions, ParMap, [], State),
	    fzn_search(State),
	    fzn_output(State),
	    writeln(----------),
	    fzn_last(State),
	    !
	;
	    writeln(==========)
	).


:- export mzn_load_string/5.
:- comment(mzn_load_string/5, [
    summary:"Load a MiniZinc model given as a string or list",
    amode:(mzn_load_string(++,++,++,+,-) is semidet),
    args:["MznModel":"String, Atom or List of constants",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"ParMap":"List of FznId=ECLiPSeGroundTerm correspondences",
	"VarMap":"List of FznId=ECLiPSeVarTerm correspondences",
	"FznState":"FlatZinc state descriptor"],
    fail_if:"Fails if the constraint setup fails",
    see_also:[mzn_run/2, mzn_run/3, mzn_run_string/2, struct(zn_options)],
    desc:html("<P>
	Loads the MiniZinc model MznModel, given in the simplest form
	as a string in MiniZinc syntax.  The problem is set up using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Neither search nor output are done.
    </P><P>
	Note that, because of the rules for escaping characters within
	ECLiPSe strings, any backslashes in the MiniZinc source have
	to be doubled, and double quotes must be escaped with a backslash!
    </P><P>
	To pass parameters into the model, a ParMap can be given, consisting
	of a list of FznId=ECLiPSeGroundTerm correspondences.  Here, FznId
	is an atom (the FlatZinc parameter identifier within the model),
	and ECLiPSeGroundTerm is the corresponding ECLiPSe constant.
    </P><P>
    	To access the ECLiPSe variables corresponding to the model's
	variables, VarMap can be given, consisting of a list of
	FznId=ECLiPSeTerm correspondences.  Here, FznId is an atom
	(the FlatZinc variable identifier within the model), and
	ECLiPSeTerm is the corresponding ECLiPSe constant, variable
	or array.
    </P><P>
    	The mzn_load_string/5 predicate returns a FlatZinc solver
	state which can be used to lookup further information about
	the model (fzn_var_lookup/3, fzn_obj_lookup/2), to perform
	the standard search (fzn_search/1), or to perform the model's
	output actions (fzn_output/1).
    </P>"),
    eg:"
    ?- mzn_load_string(\"
		int: n;
		array [1..n] of var 1..n: q;
		constraint
		    forall (i in 1..n, j in i+1..n) (
			q[i]     != q[j]     /\\\\
			q[i] + i != q[j] + j /\\\\
			q[i] - i != q[j] - j
		    );
		solve satisfy;
	    \",
	    fzn_ic,
	    [n=8],
	    [q=Q],
	    FznState).

    Q = [](_2492{1..8}, _2512{1..8}, _2532{1..8}, _2552{1..8}, ...]
    FznState = state(...)
    There are 84 delayed goals.
    Yes (0.02s cpu)


    ?- mzn_load_string(\"...\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q).

    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)


    ?- mzn_load_string(\"...\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q),
       fzn_output(FznState).

    % output from fzn_output:
    q = [1,5,8,6,3,7,2,4];
    % Total time 0.030s cpu (0.020 setup)

    % output from ECLiPSe toplevel:
    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)
</PRE>
"]).
mzn_load_string(MznModel, SolverOrOptions, ParMap, VarMap, State) :-
	zn_options(SolverOrOptions, Options),
	model_to_modelfile(MznModel, MznFile),
	pars_to_instancefile(ParMap, MznInstFile, Options),
	mzn2fzn(MznFile, MznInstFile, Options, FznStream, PidOrFile),
	fzn_init(Options, State),
	( block(fzn_load_stream(FznStream, State), Tag,
		(mzn_load_cleanup(PidOrFile, MznInstFile),
		delete_file(MznFile),
		exit_block(Tag)))
	->
	    mzn_load_cleanup(PidOrFile, MznInstFile),
	    delete_file(MznFile)
	;
	    mzn_load_cleanup(PidOrFile, MznInstFile),
	    delete_file(MznFile),
	    fail
	),
	fzn_ids_to_ecl_vars(VarMap, State).


:- export mzn_load/5.
:- comment(mzn_load/5, [
    summary:"Load a MiniZinc model from a file",
    amode:(mzn_load(++,++,++,+,-) is semidet),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"ParMap":"List of FznId=ECLiPSeGroundTerm correspondences",
	"VarMap":"List of FznId=ECLiPSeVarTerm correspondences",
	"FznState":"FlatZinc state descriptor"],
    fail_if:"Fails if the constraint setup fails",
    see_also:[mzn_run/2, mzn_run/3, mzn_load_string/5, struct(zn_options)],
    desc:html("<P>
	Loads a MiniZinc from ModelFile.  The problem is set up using
	a mapping to a concrete ECLiPSe solver, as specified in the
	SolverOrOptions argument.  Neither search nor output are done.
    </P><P>
	To pass parameters into the model, a ParMap can be given, consisting
	of a list of FznId=ECLiPSeGroundTerm correspondences.  Here, FznId
	is an atom (the FlatZinc parameter identifier within the model),
	and ECLiPSeGroundTerm is the corresponding ECLiPSe constant.
    </P><P>
    	To access the ECLiPSe variables corresponding to the model's
	variables, VarMap can be given, consisting of a list of
	FznId=ECLiPSeTerm correspondences.  Here, FznId is an atom
	(the FlatZinc variable identifier within the model), and
	ECLiPSeTerm is the corresponding ECLiPSe constant, variable
	or array.
    </P><P>
    	The mzn_load/5 predicate returns a FlatZinc solver
	state which can be used to lookup further information about
	the model (fzn_var_lookup/3, fzn_obj_lookup/2), to perform
	the standard search (fzn_search/1), or to perform the model's
	output actions (fzn_output/1).
    </P>"),
    eg:"
    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState).

    Q = [](_2492{1..8}, _2512{1..8}, _2532{1..8}, _2552{1..8}, ...]
    FznState = state(...)
    There are 84 delayed goals.
    Yes (0.02s cpu)


    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q).

    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)


    ?- mzn_load(\"queens\", fzn_ic, [n=8], [q=Q], FznState),
       ic:labeling(Q),
       fzn_output(FznState).

    % output from fzn_output:
    q = [1,5,8,6,3,7,2,4];
    % Total time 0.030s cpu (0.020 setup)

    % output from ECLiPSe toplevel:
    Q = [](1, 5, 8, 6, 3, 7, 2, 4)
    FznState = state(...)
    Yes (0.03s cpu, solution 1, maybe more)
</PRE>
"]).
mzn_load(MznFile, SolverOrOptions, ParMap, VarMap, State) :-
	zn_options(SolverOrOptions, Options),
	pars_to_instancefile(ParMap, MznInstFile, Options),
	mzn2fzn(MznFile, MznInstFile, Options, FznStream, PidOrFile),
	fzn_init(Options, State),
	( block(fzn_load_stream(FznStream, State), Tag,
		(mzn_load_cleanup(PidOrFile, MznInstFile), exit_block(Tag)))
	->
	    mzn_load_cleanup(PidOrFile, MznInstFile)
	;
	    mzn_load_cleanup(PidOrFile, MznInstFile),
	    fail
	),
	fzn_ids_to_ecl_vars(VarMap, State).


    model_to_modelfile(MznModel, MznFile) :-
	make_tmpfile(mod, MznFile),
	open(MznFile, write, MznStream),
	(
	    ( MznModel = [_|_] ->
		% Crude way to insert ECLiPSe parameters into MiniZinc source
		( foreach(Part,MznModel), param(MznStream) do
		    fzn_write(MznStream, Part)
		)
	    ;
		(string(MznModel);atom(MznModel)),
		write(MznStream, MznModel)
	    )
	->
	    close(MznStream)
	;
	    close(MznStream),
	    delete_file(MznFile),
	    fzn_error("Malformed Model", [MznModel])
	).


    pars_to_instancefile([], _MznInstFile, _Options) ?- !.
    pars_to_instancefile(ParMap, MznInstFile, zn_options{solver:Solver}) :-
	% Check the ParMap
	ground(ParMap),
	is_list(ParMap),
	( foreach(TyId=_Value,ParMap) do
	    ( TyId = (Ty:Id) ->
	    	atom(Id),
		fzn_type(Ty)
	    ;
		atom(TyId)
	    )
	),
	!,
	make_tmpfile(inst, MznInstFile),
	open(MznInstFile, write, Stream),
	writeln(Stream, "% Generated instance file"),
	( foreach(TyId=Value,ParMap), param(Stream,Solver) do
	    ( TyId = (Ty:Id) ->
		printf(Stream, "%w = ", [Id]),
		fzn_write(Stream, Value, Ty, Solver)
	    ;
		printf(Stream, "%w = ", [TyId]),
		fzn_write(Stream, Value)
	    ),
	    writeln(Stream, ";")
	),
	close(Stream).
    pars_to_instancefile(ParMap, _MznInstFile, _Options) :-
	fzn_error("Illegal ParMap: %w", [ParMap]).

    % 
    fzn_type(no_macro_expansion(array(_) of T)) ?- !,
	fzn_simple_type(T).
    fzn_type(no_macro_expansion(set of T)) ?- !,
	fzn_scalar_type(T).
    fzn_type(T) :-
	fzn_simple_type(T).

    fzn_scalar_type(bool).
    fzn_scalar_type(int).

    fzn_simple_type(bool).
    fzn_simple_type(int).
    fzn_simple_type(float).


    mzn2fzn_cleanup(PidOrFile) :-
	( number(PidOrFile) ->
	    ( wait(PidOrFile, _Status) -> true ; true )
	;
	    delete_file(PidOrFile)
	).

    make_tmpfile(What, File) :-
	getval(tmpcnt, I),
	incval(tmpcnt),
	get_flag(tmp_dir, Dir),
	get_flag(pid, Pid),
	get_flag(unix_time, Time),
	concat_string([Dir,ecl_,What,I,"_",Time,"_",Pid,".mzn"], File).


    delete_file(File) :-
	( nonvar(File), exists(File) -> delete(File) ; true ).

    fzn_ids_to_ecl_vars(VarMap, State) :-
	( foreach(Id=EclVar,VarMap), param(State) do
	    ( fzn_var_lookup(State, Id, EclVar) ->
	    	true
	    ;
		fzn_error("No such id in the model: %w", [Id])
	    )
	).

    mzn_load_cleanup(PidOrFile, MznFile) :-
	mzn2fzn_cleanup(PidOrFile),
	delete_file(MznFile).


%----------------------------------------------------------------------
% Invoke the MiniZinc->FlatZinc converter
% ModelFile should be file name with or without .mzn extension.
% DataFile can be a variable, or like ModelFile.
% Pipe FlatZinc output into FznStream, or produce intermediate .fzn file,
% depending on the flag UseFznFile.
% If output is piped, PidOrFile is process id to be waited for.
% If output is via file, PidOrFile is .fzn file to be deleted.
%----------------------------------------------------------------------

mzn2fzn(ModelFile0, DataFile0, zn_options{solver:Solver,fzn_tmp:OutFlag}, FznStream, PidOrFile) :-
	( existing_file(ModelFile0, ["",".mzn"], [readable], ModelFile) ->
	    os_file_name(ModelFile, ModelFileOS)
	;
	    fzn_error("No such file: %w", [ModelFile0])
	),
	( (atom(DataFile0);string(DataFile0)) ->
	    ( existing_file(DataFile0, ["",".dzn",".mzn"], [readable], DataFile) ->
		os_file_name(DataFile, DataFileOS),
		Params0 = ["--data",DataFileOS,ModelFileOS]
	    ;
		fzn_error("No such file: %w", [DataFile0])
	    )
	;
	    Params0 = [ModelFileOS]
	),
	getval(here, EclZincLib),
	concat_string([EclZincLib,Solver], EclZincSolverSpecificLib),
	os_file_name(EclZincSolverSpecificLib, EclZincSolverSpecificLibOS),
	getval(minizinc_dir, MznDir),
	getval(mzn2fzn_exe, Mzn2Fzn),
	( MznDir == '' ->
	    % Hope the exectuable knows its stdlib-dir
	    Params = ["-I",EclZincSolverSpecificLibOS|Params0]
	;
	    % Assume we are calling the mzn2fzn-actual exectuable
	    % without any environment variable setting
	    concat_string([MznDir, "lib/minizinc"], ZincDefaultLib),
	    os_file_name(ZincDefaultLib, ZincDefaultLibOS),
	    Params = ["-I",EclZincSolverSpecificLibOS,
		      "--stdlib-dir",ZincDefaultLibOS|Params0]
	),
	( OutFlag==file ->
	    % use intermediate fzn file, and echo any stderr on error
	    ( var(PidOrFile) ->
		pathname(ModelFile, Path, Base, _Mzn),
		concat_string([Path,Base,".fzn"], PidOrFile)
	    ;
		true
	    ),
	    os_file_name(PidOrFile, FznFileOS),
%	    writeln(exec([Mzn2Fzn,"--output-to-file",FznFileOS|Params], [null,null,Err], Pid)),
	    exec([Mzn2Fzn,"--output-to-file",FznFileOS|Params], [null,null,Err], Pid),
	    read_stream(Err, Message),
	    write(error, Message),
	    wait(Pid, Status),
	    ( Message \== "" ->
		fzn_error("mzn2fzn unsuccessful", [])
	    ; Status \== 0 ->
		fzn_error("mzn2fzn exited with status %16r", [Status])
	    ;
		true
	    ),
	    open(PidOrFile, read, FznStream)
	;
	    % pipe the fzn - we can't easily handle the error output
	    % without running the risk of blocking
%	    writeln(exec([Mzn2Fzn,"--output-to-stdout"|Params], [null,FznStream], PidOrFile)),
	    exec([Mzn2Fzn,"--output-to-stdout"|Params], [null,FznStream], PidOrFile)
	).

    read_stream(Stream, String) :-
	( read_string(Stream, end_of_file, _, String) ->
	    close(Stream)
	;
	    close(Stream),
	    String = ""
	).


:- export mzn2fzn/4.
:- comment(mzn2fzn/4, [
    summary:"Convert a MiniZinc model into a FlatZinc model",
    amode:(mzn2fzn(+,+,++,?) is det),
    args:["ModelFile":"File name (extension defaults to .mzn)",
	"InstFileOrParMap":"Instance file name (extension defaults to .dzn, then .mzn), or list of Id=Term correspondences",
	"SolverOrOptions":"Name of solver mapping module, or zn_options-structure",
	"FznFile":"Name of generated FlatZinc file (will be generated if variable)"],
    see_also:[mzn_run/3, flatzinc:fzn_run/2, struct(zn_options)],
    desc:html("<P>
	Converts a MiniZinc model (given a model file and an instance
	file or parameter map) into a FlatZinc model, by invoking the
	external mzn2fzn converter with the appropriate arguments.
	If no output file name is specified (FznFile uninstantiated),
	the name of the output file is the same as the input file, with
	the extension changed to .fzn.  The options should specify the
	solver that is intended to be used on the FlatZinc model (so that
	the correct version of globals.mzn is used), and the fzn_tmp
	option should be set to 'file' (the default).
    </P>"),
    eg:"
    ?- mzn2fzn(mymodel, [], zn_options{solver:fzn_ic,fzn_tmp:file}, FznFile).
    FznFile = \"mymodel.fzn\"
    Yes (0.00s cpu)
"]).

mzn2fzn(MznFile, ParMapOrFile, SolverOrOptions, FznFile) :-
	zn_options(SolverOrOptions, Options),
	Options = zn_options{fzn_tmp:OutFlag},
	( OutFlag == file ->
	    ( is_list(ParMapOrFile) ->
		pars_to_instancefile(ParMapOrFile, MznInstFile, Options),
		mzn2fzn(MznFile, MznInstFile, Options, FznStream, FznFile),
		delete_file(MznInstFile)
	    ;
		mzn2fzn(MznFile, ParMapOrFile, Options, FznStream, FznFile)
	    ),
	    close(FznStream)
	;
	    fzn_error("Unsupported option fzn_tmp:%w", [OutFlag])
	).

