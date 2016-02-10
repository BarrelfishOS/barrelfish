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
% Contributor(s): Joachim Schimpf and Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%
% Description:	ECLiPSe/CPLEX interface
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
%               Kish Shen,       IC-Parc
% Version:	$Id: eplex_.ecl,v 1.17 2015/01/14 01:31:08 jschimpf Exp $
%
% TODO:
%	- cplex_change_col_type: accept list
%	- elimiate cplex_type_code/2 (use atoms)
%	- review role of our own probtype flag (only for probes?)


:- lib(linearize).
%:- lib(var_name).	% load only if needed!
:- lib(hash).
:- lib(constraint_pools).

%:- set_flag(toplevel_module,eplex).	% debugging


% Ranged Variables ----------

:- export deletemostfract/4.	% deletemostfract(-Var, +List, +Handle, -Rest)
:- export deletefract/5.	% deletefract(-Var, +List, +Handle, +Tolerance, -Rest)
:- export int_tolerance/1.

% Constraints ----------

:- export add_constraint/1.	% add_constraint(+Constraint)
:- export lp_add_constraints/3.	% lp_add_constraints(+Handle, +Constraints, +Integers)
:- export lp_add_constraints/4. % lp_add_constraints(+Handle, +Constraints, +Int, -Indices)
:- export lp_add_cutpool_constraints/4. % lp_add_cutpool_constraints(+Handle, +Constraints, +Options, -Indices).
:- tool(add_constraint/1, add_pool_constraint/2).

% Versions and licensing ----------

:- export lp_get_license/0.	% lp_get_license
:- export lp_get_license/2.	% lp_get_license(+LicStr, +LicNum)
:- export lp_release_license/0.	% lp_release_license
:- export lp_get_license_challenge/1. % lp_get_license_challenge(-Challenge)

:- skipped lp_get_license/2.	% hide licensing-related stuff
:- untraceable lp_get_license/2.
:- untraceable lp_get_license_challenge/1.

% Simplified high-level solver invocation ----------

:- export optimize/2.		% optimize(+Objective, -Cost)
:- export optimize/3.		% optimize(+Objective, +Options, -Cost)

:- tool(optimize/2, optimize_body/3).
:- tool(optimize/3, optimize_body/4).

% Constraints manipulation ----------

:- export normalise_cstrs/3.	% normalise_cstrs(+Constraints,
				%	-NormConstraints, -NonlinConstraints)
:- export collect_lp_constraints_norm/1.
				% collect_lp_constraints_norm(-NormConstraints)
:- export collect_integers/1.	% collect_integers(-Integers)
:- export collect_reals/1.	% collect_reals(-Reals)
:- export normalise_cstr/2.
:- export renormalise_cstrs/2.
:- export renormalise_cstr/2.
:- export denormalise_cstr/2.

% Explicit solver handling ----------

:- export lp_setup/4.		% lp_setup(+NormConstraints, +Objective,
				%		+Options, -Handle)
:- export lp_add/3.		% lp_add(+Handle, +NormConstraints, +Integers)
:- export lp_cleanup/1.		% lp_cleanup(+Handle)
:- export lp_get/3.		% lp_get(+Handle, +What, -Data)
:- export lp_set/3.		% lp_set(+Handle, +What, +Data).
:- export lp_solve/2.		% lp_solve(+Handle, -Cost)
:- export lp_probe/3.		% lp_probe(+Handle, +TmpObjective, -Cost).

:- export lp_add_indexed/4.     % lp_add_indexed(+Handle, +NormConstraints, +Ints, -Indices)
:- export lp_add_vars/2.        % lp_add_vars(+Handle, +Vars)
:- export lp_add_columns/2.     % lp_add_columns(+Handle, +VarCols)

:- export lp_demon/5.		% lp_demon(+Handle, ?Cost, +PreGoal, +PostGoal, +Module)
:- export lp_demon_setup/5.     % lp_demon_setup(+Objective, ?Cost, +Options,
                                %	+TriggerModes, -Handle[,+Module])
:- export lp_demon_setup/6.     % obsolete
:- export lp_write/3.		% lp_write(+Handle, +Format, +File)
:- export lp_read/3.		% lp_read(+File, +Format, -Handle)

:- export lp_set/2.		% lp_set(+Param, +Value)
:- export lp_get/2.		% lp_get(+Param, -Value)
:- export lp_var_get/3.		% lp_var_get(+Var, +What, -Value) obsolete
:- export lp_var_get/4.		% lp_var_get(+Handle, +Var, +What, -Value)
:- export lp_var_solution/2.	% lp_var_solution(+Var,-Value) obsolete
:- export lp_var_solution/3.	% lp_var_solution(+Handle, +Var, -Value)
:- export lp_var_occurrence/3.	% lp_var_occurrence(+Var, -Handle, -Index)

:- export lp_var_set_bounds/4.  % lp_var_set_bounds(+Handle, +Var, +Lo, +Hi)
:- export lp_var_non_monotonic_set_bounds/4.
                                % lp_var_non_monotonic_set_bounds(+Handle, +Var, +Lo, +Hi)
:- export lp_var_get_bounds/4.  % lp_var_get_bounds(+Handle, +Var, -Lo, -Hi)

:- export lp_get_iis/5.		% lp_get_iis(+Handle, -NCstrs, -NCVars, -CCstrIdxs, -CVarStats)
:- export lp_verify_solution/3. % lp_verify_solution(+Handle, -ViolatedCstrs, -ViolatedVars)

:- tool(lp_demon_setup/6,lp_demon_setup_body/7).
:- tool(lp_demon_setup/5,lp_demon_setup_body/6).
:- tool(lp_setup/4,lp_setup_body/5).

:- export solution_out_of_range/1.	% solution_out_of_range(+Handle)
:- export instantiation_deviates/1.	% instantiation_deviates(+Handle)


% Handler ----------

:- export lp_var_print/2.

:- meta_attribute(eplex, [
	print:lp_var_print/2,
	unify:unify_eplex/2,
	get_bounds:lp_attr_get_bounds/3,
	set_bounds:lp_attr_set_bounds/3,
	suspensions:suspensions_eplex/3]).


:- set_event_handler(lp_obj_nobounds_warning, lp_obj_nobounds_warning/0).


% Change variable support
:-export suspend_on_change/3.    % (+Var, +Susp, +Pool)
:-export get_changeable_value/3. % (+Var,-Val, +Pool)
:-export lp_suspend_on_change/3.    % (+Handle, +Var, +Susp)
:-export lp_get_changeable_value/3. % (+Handle, +Var, -Val,)

% Pools  -----------

:- export eplex_instance/1.	% eplex_instance(+PoolName)

% Predicates with pool argments (don't reexport these in eplex!)
:- export lp_eq/3.		% =:=/2
:- export lp_ge/3.		% >=/2
:- export lp_le/3.		% =</2
:- export lp_interval/3.        % ::/2, $::/2
:- export integers/2.		% integers/1
:- export reals/2.              % reals/1
:- export eplex_get/3.		% eplex_get/2
:- export eplex_set/3.		% eplex_set/2
:- export eplex_read/3.		% eplex_read/2
:- export eplex_write/3.	% eplex_write/2
:- export eplex_cleanup/1.	% eplex_cleanup/0
:- export eplex_probe/3.	% eplex_probe/2
:- export eplex_solve/2.	% eplex_solve/1
:- export eplex_var_get/4.	% eplex_var_get/3
:- export eplex_var_get_bounds/4.  % eplex_var_get_bounds/3.
:- export eplex_add_constraints/3. % eplex_add_constraints/2
:- export eplex_get_iis/5.	% eplex_get_iis/4
:- export eplex_verify_solution/3. % eplex_verify_solution/2.

:- local struct(constraint_type(integers,reals,linear,sos,idc)).
:- local struct(probes(obj,sense,ints,rhs,bounds)). % types of probe in lp/eplex_probe

% These operators are a subset of those in lib(ic)
:- export op(700, xfx, [$>=, $=, $=<, $::]).
:- export op(780, yfx, [=>]).
:- export op(750, fx, [neg]).

:- local struct(cp_options(group,active,add_initially)). % cutpool cstrs options

valid_cp_opt(group, group of cp_options) :- !.
valid_cp_opt(active, active of cp_options) :- !.
valid_cp_opt(add_initially, add_initially of cp_options).

valid_cp_optval(group, Name, Handle, PIdx) ?-
        get_named_cp_index(Handle, Name, PIdx).
valid_cp_optval(active, InVal, _, OutVal) ?- 
        ( InVal == 1 ; InVal == 0 ), !,
        InVal = OutVal.
valid_cp_optval(add_initially, InVal, _, OutVal) ?-
        ( InVal == 1 ; InVal == 0 ), !,
        InVal = OutVal.

%----------------------------------------------------------------------
% Initialisation and licencing
%
% When lib(eplex) is loaded, we also load one of the solver interfaces.
%
% Several solver interfaces (cplex/xpress, different versions) might
% be installed. Only one can be loaded, using the following criteria:
%
% Preferences can be specified by loading one of the specific
% dummy libraries eplex_<solver>_<version> or eplex_<solver>.
% lib(eplex) tests for the existence of modules of this name.
%
% When there is an eplex_lic_info.ecl file, we load the solver that
% corresponds to the first matching entry in this file.
%
% If there is no eplex_lic_info.ecl file (runtime systems for example),
% then we load any of the installed solvers.
% 
% After loading, we try to obtain a license for the loaded solver.
% If that is not possible, a warning is printed.
%----------------------------------------------------------------------

:- import
	symbol_address/2,
	replace_attribute/3
    from sepia_kernel.

:- local variable(licence_data, none).	% 'none' or held(LicStr,LicNum)
:- local variable(loaded_solver, none).	% 'none' or loaded(Solver,Version)
:- local variable(presolve_default, 1).	% 1 or 0
:- local variable(timeout_default, 0.0). % non-negative float, 0.0 no timeout

:- pragma(nodebug).		% the licensing code should be untraceable!

optimizer_code(cplex).
optimizer_code(xpress).
optimizer_code(osi).  % treat the solver used as versions of OSI
optimizer_code(gurobi).

% selected_solver(-Solver, -Version) is det.

selected_solver(Solver, Version) :-
	current_module(EplexSolverVersion),
	atom_string(EplexSolverVersion, EplexSolverVersionString),
	split_string(EplexSolverVersionString, "_", "_", ["eplex",SolverS,VersionS]),
	atom_string(Solver, SolverS),
	optimizer_code(Solver),
	atom_string(Version, VersionS),
	!.
selected_solver(Solver, _Version) :-
	current_module(EplexSolver),
	atom_string(EplexSolver, EplexSolverString),
	split_string(EplexSolverString, "_", "_", ["eplex",SolverS]),
	atom_string(Solver, SolverS),
	optimizer_code(Solver),
	!.
selected_solver(_Solver, _Version).


% licensed_solver(?Solver, ?Version, -LicStr, -LicNum) is nondet.
% Return licensed solvers on backtracking (in order of preference)

licensed_solver(Solver, Version, LicStr, LicNum) :-
	get_flag(hostname, HostnameString),
	atom_string(Hostname, HostnameString),
	get_flag(installation_directory, EclipseDir),
	concat_string([EclipseDir,'/lib/eplex_lic_info.ecl'], LicInfo),
	get_file_info(LicInfo,readable,on),	% may fail
	open(LicInfo,read,S),
	repeat,
	read(S, T),
	( T == end_of_file -> close(S), !, fail ; true ),
	T = licence(Hostname, Solver, Version, LicStr, LicNum).


% installed_solver(?Solver, ?Version, -File) is nondet.
% Return matching installed solvers on backtracking
% secplexXY.so or sexpressXY.so are expected in the Arch subdirectory.
% Must be called with the current directory being the library directory.

installed_solver(Solver, Version, File) :-
	get_flag(hostarch, Arch),
	get_flag(object_suffix, O),
	optimizer_code(Solver),
	concat_string(["se",Solver], ESolver),
	concat_string([ESolver,"*.",O], Pattern),
	read_directory(Arch, Pattern, _, Files),
	member(File, Files),
	pathname(File, _, Base, _),
	once append_strings(ESolver, VersionString, Base),
	atom_string(Version, VersionString).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Try to load a solver interface
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

% If the external solver is a dynamic library,
% try to load it explicitly to avoid PATH problems (otherwise fail)

load_external_solver(Arch, Solver, Version, Suffix) :-
	( Solver = cplex ; Solver == gurobi ), !,
        concat_string([Solver,Version,".",Suffix], SolverLibName),
        ( concat_string([Arch,/,SolverLibName], SolverLib)
        ; concat_string([Arch,"/e",Solver,Version,/,SolverLibName], SolverLib) ),
	exists(SolverLib),
        catch(load(SolverLib), abort, fail).
load_external_solver(Arch, xpress, Version, "dll") ?- !,
	atom_string(Version, VersionString),
	( substring(VersionString, "13", 1) ->
	    concat_string([Arch,"/express",Version,"/xprs.dll"], SolverLib),
	    exists(SolverLib),
	    catch(load(SolverLib), abort, fail)
	;
	    concat_string([Arch,"/express",Version,"/xprl.dll"], SolverLib1),
	    exists(SolverLib1),
	    catch(load(SolverLib1), abort, fail),
	    concat_string([Arch,"/express",Version,"/xprs.dll"], SolverLib2),
	    exists(SolverLib2),
	    catch(load(SolverLib2), abort, fail)
	).
load_external_solver(Arch, xpress, Version, "so") ?-
	atom_string(Version, VersionString),
	\+ substring(VersionString, "13", 1), !, % assume this means > 13
        % unfortunately Solaris needs the version specific .so files 
        % (i.e. *.so.<Version>). So the directory should contain only
        % one version of each lib file 
        concat_string([Arch, "/express",Version], SolverDir),
        read_directory(SolverDir, "libxprl.so*", _, [LicLib|_]),
        concat_string([SolverDir, "/", LicLib], LicPath),
        load(LicPath),
        read_directory(SolverDir, "libxprs.so*", _, [SolverLib|_]),
        concat_string([SolverDir, "/", SolverLib], SolverPath),
        load(SolverPath).
load_external_solver(_, _, _, _).

:-
    % we want one of the solvers that have been asked for
    % in order of preference (if specified in eplex_lic_info.ecl)
    findall(solver(Solver, Version), (
	    selected_solver(Solver, Version),
	    ( licensed_solver(Solver,Version,_LS,_LN) ; true )
	),
	Candidates),
    (
	% find the first installed candidate
	member(solver(Solver, Version), Candidates),
	installed_solver(Solver, Version, ObjFile),

	get_flag(hostarch, Arch),
	concat_string([Arch,/,ObjFile], DirObjFile),

	( symbol_address(p_cpx_init, _) ->
	    true	% already loaded
	;
	    printf(log_output, "loading %A %s ... %b", [Solver,Version]),
	    get_flag(object_suffix, O),
	    ( load_external_solver(Arch, Solver, Version, O) -> true ; true ),
	    ( O = "o" ->
	       concat_string([Arch,/,ObjFile,' -lm'], Load)
	    ;
	       concat_string([Arch,/,ObjFile], Load)
	    ),
            % loading might abort because of stupid library
            % incompatibilities, etc., so fail to try other alternatives
	    catch(load(Load), abort, fail),
	    writeln(log_output, done)
	)
    ->
	external(lp_get_license_challenge/1, p_cpx_challenge),
	external(cplex_init/3, p_cpx_init),
	external(cplex_exit/0, p_cpx_exit),
	external(cplex_prob_init/8, p_cpx_prob_init),
	external(cplex_lo_hi/2, p_cpx_lo_hi),
	external(cplex_set_rhs_coeff/4, p_cpx_set_rhs_coeff),
	external(cplex_set_obj_coeff/3, p_cpx_set_obj_coeff),
	external(cplex_set_qobj_coeff/4, p_cpx_set_qobj_coeff),
	external(cplex_init_type/3, p_cpx_init_type),
	external(cplex_init_bound/4, p_cpx_init_bound),
	external(cplex_new_obj_coeff/3, p_cpx_new_obj_coeff),
	external(cplex_new_qobj_coeff/4, p_cpx_new_qobj_coeff),
	external(cplex_flush_obj/1, p_cpx_flush_obj),
	external(cplex_get_param/3, p_cpx_get_param),
	external(cplex_get_prob_param/3, p_cpx_get_prob_param),
	external(cplex_set_param/3, p_cpx_set_param),
	external(cplex_set_matbeg/4, p_cpx_set_matbeg),
	external(cplex_set_matval/4, p_cpx_set_matval),
	external(cplex_get_row/4, p_cpx_get_row),
	external(cplex_get_col_coef/5, p_cpx_get_col_coef),
	external(cplex_get_obj_coef/3, p_cpx_get_obj_coef),
	external(cplex_get_rhs/5, p_cpx_get_rhs),
	external(cplex_loadprob/1, p_cpx_loadprob),
	external(cplex_loadbase/3, p_cpx_loadbase),
	external(cplex_loadorder/3, p_cpx_loadorder),
	external(cplex_add_new_sos/4, p_cpx_add_new_sos),
	external(cplex_flush_sos/1, p_cpx_flush_sos),
	external(cplex_optimise/10, p_cpx_optimise),
	external(cplex_get_objval/2, p_cpx_get_objval),
	external(cplex_cleanup/1, p_cpx_cleanup),
	external(cplex_lpwrite/3, p_cpx_lpwrite),
	external(cplex_lpread/4, p_cpx_lpread),
	external(cplex_output_stream/3, p_cpx_output_stream),
	external(cplex_add_coeff/4, p_cpx_add_coeff),
	external(cplex_flush_new_rowcols/2, p_cpx_flush_new_rowcols),
	external(cplex_load_varname/3, p_cpx_load_varname),
	external(cplex_change_col_type/3, p_cpx_change_col_type),	% bt
	external(cplex_change_lp_to_mip/1, p_cpx_change_lp_to_mip),	% bt
        external(cplex_change_obj_sense/2, p_cpx_change_obj_sense),
        external(cplex_change_rhs/4, p_cpx_change_rhs),
        external(cplex_change_cols_bounds/5, p_cpx_change_cols_bounds),
        external(cplex_set_problem_type/3, p_cpx_set_problem_type),

	external(create_darray/2, p_create_darray),
	external(darray_size/2, p_darray_size),
	external(get_darray_element/3, p_get_darray_element),
	external(set_darray_element/3, p_set_darray_element),
	external(darray_list/3, p_darray_list),

        % column generation (adapted from AE 25/10/02)
	external(cplex_new_row/5, p_cpx_new_row),

	external(cplex_new_row_idc/5, p_cpx_new_row_idc),
	external(cplex_flush_idcs/1, p_cpx_flush_idcs),

        external(create_extended_iarray/3, p_create_extended_iarray),
        external(create_extended_darray/3, p_create_extended_darray),
        external(copy_extended_column_basis/4, p_copy_extended_column_basis),
        external(copy_extended_arrays/6, p_copy_extended_arrays),
        external(decode_basis/2, p_decode_basis),
        
        % external(cplex_get_feas_result_array/3, p_cpx_get_feas_result_array),
        external(create_iarray/2, p_create_iarray),
        external(iarray_size/2, p_iarray_size),
        external(get_iarray_element/3, p_get_iarray_element),
        external(set_iarray_element/3, p_set_iarray_element),
        external(iarray_list/2, p_iarray_list),

        % standalone eplex
        external(cplex_matrix_base/1, p_cpx_matrix_base),
        external(cplex_matrix_offset/1, p_cpx_matrix_offset),
	external(cplex_get_col_type/3, p_cpx_get_col_type),
	external(cplex_impose_col_lwb/5, p_cpx_impose_col_lwb),		% bt
	external(cplex_impose_col_upb/5, p_cpx_impose_col_upb),		% bt
	external(cplex_impose_col_bounds/7, p_cpx_impose_col_bounds),	% bt
	external(cplex_get_col_bounds/4, p_cpx_get_col_bounds),
        external(cplex_set_new_cols/6, p_cpx_set_new_cols),

        % global constraint pools
        external(cplex_get_cutpool_size/3, p_cpx_get_cutpool_size),
        external(cplex_reset_cutpool_size/3, p_cpx_reset_cutpool_size),
        external(cplex_init_cpcstr/5, p_cpx_init_cpcstr),
        external(cplex_get_named_cp_index/4, p_cpx_get_named_cp_index),
        external(cplex_get_named_cpcstr_indices/3, p_cpx_get_named_cpcstr_indices),
        external(cplex_get_cpcstr_info/4, p_cpx_get_cpcstr_info),
        external(cplex_set_cpcstr_cond/4, p_cpx_set_cpcstr_cond),
        
        setval(loaded_solver, loaded(Solver,Version))
    ;
	writeln(error, "Eplex error: Could not find any of the solver interfaces:"),
	writeln(error, Candidates)
	% we used to abort here - it's no longer done to allow the library
	% to fcompile/icompile during installation despite missing solver
    ).


% lp_get_license(+LS, +LN)
% LS is 'default' or path in OS syntax
lp_get_license(default, LN) ?-
	% If LS is 'default' we set it to point to the directory
	% where we keep the XPRESS-MP OEM version password files.
	getval(loaded_solver, loaded(Solver,Version)),
	!,
	get_flag(installation_directory, EclipseDir),
	get_flag(hostarch, Arch),
	concat_atom([EclipseDir,/,lib,/,Arch,/,e,Solver,Version], LS),
	os_file_name(LS, OSLS),
	lp_get_license(OSLS, LN).
lp_get_license(LS, LN) :-
	getval(licence_data, Held),		% already holding this one?
	Held == held(LS,LN),
	!.
lp_get_license(LS, LN) :-
	getval(loaded_solver, loaded(_Solver,_Version)),
	!,
	lp_release_license,			% possibly free other licence
	unchecked_get_license(LS, LN).		% may fail
lp_get_license(LS, LN) :-
	printf(error, "Eplex error: No solver loaded in %w%n", lp_get_license(LS, LN)),
	fail.


lp_get_license :-
	getval(licence_data, held(_,_)),	% already holding one?
	!.
lp_get_license :-
	getval(loaded_solver, loaded(Solver,Version)),
	!,
	( licensed_solver(Solver,Version,LS,LN) ->
	    lp_get_license(LS, LN)
	;
            (lp_get_license("",0) ->
                true /* initialised successfully with no license info */
            ;
                get_flag(installation_directory, EclipseDir),
                concat_string([EclipseDir,'/lib/eplex_lic_info.ecl'], LicInfo),
                writeln(warning_output, "Eplex warning: No licensing information available."),
                writeln(warning_output, "Use lp_get_license/2 or update license info database"),
                writeln(warning_output, LicInfo),
                fail
            )
	).
lp_get_license :-
	printf(error, "Eplex error: No solver loaded in %w%n", lp_get_license),
	% don't abort here because lp_get_license/0 is called in a query
	% below, and we don't want to abort loading the whole library.
	fail.

    get_solver_subdir(xpress, Version, SubDir) ?- !,
        get_flag(installation_directory, EclipseDir),
        get_flag(hostarch, Arch),
        concat_string([EclipseDir,"/lib/",Arch,"/express",Version], SubDir).
    get_solver_subdir(_, _, "").

    unchecked_get_license(LS, LN) :-
        getval(loaded_solver, loaded(Solver,Version)),
        get_solver_subdir(Solver,Version, SD),
        may_start_license_manager(Solver, Version, LS, SD),
        % SD needed for the Xpress 15 sh command ran during initialisation
        os_file_name(SD, OSSD),
	cplex_init(LS,LN,OSSD),			% may fail
        setval(licence_data, held(LS,LN)),

	% Initialisation that can only be done after getting a license:

%	get_stream(output, OutS),
%	cplex_output_stream(0, 1, OutS),	% cpxresults
	get_stream(error, ErrS),
	cplex_output_stream(1, 1, ErrS),	% cpxerror
%	get_stream(log_output, LogS),
%	cplex_output_stream(3, 1, LogS),	% log
	get_stream(warning_output, WarnS),
	cplex_output_stream(2, 1, WarnS).	% cpxwarning

    may_start_license_manager(xpress, Version, LicLoc, SolverDir) :-
        atom_string(Version, VersionString),
	substring(VersionString, "15", 1), !,
        get_flag(tmp_dir, TDir),
        get_user_name(User),
        concat_string([TDir, "eclipse_xpress_",User,".log"], XPLog),
        os_file_name(XPLog, OSXPLog),
        concat_string([SolverDir, "/lmgrd"], LicMan),
        % LicLoc is already in OS-specific form
        exec([LicMan, "-c", LicLoc, "-l", OSXPLog], []).
    may_start_license_manager(_, _, _, _).

    get_user_name(User) :- /* Unix */
        getenv("USER", User), !.
    get_user_name(User) :- /* Windows */
        getenv("USERNAME", User), !.
    get_user_name("user").

lp_release_license :-
	getval(licence_data, held(_LS,_LN)),
	!,
	cplex_exit,
	setval(licence_data, none).
lp_release_license.

% cleanup when exiting
:- local finalization(is_predicate(lp_release_license/0) -> lp_release_license ; true).


:- pragma(debug).	% allow debugging for the rest of the code

% ----------------------------------------------------------------------
% Problem handle structure
%
%	cplex_handle - pointer to C problem handle. This must be the
%		first argument (assumed by cplex_prob_init and cplex_cleanup)!
%	vars - a list of all variables involved
%	ints - a list of variables that the solver should consider
%		to be integers
%	sols,pis,slacks,djs,cbase,rbase - arrays holdings results
%		exported from cplex
%	obj - constant offset of the objective function
%	objcoeffs - objective coefficients
%	method, node_method - cplex main solving method (default, primal ...)
%       aux_method, node_aux_method - cplex auxiliary solving method
% The interface works as follows: As constraints occur in the program,
% they are first delayed (except when they are already ground).
% When later lp_setup is called, the linear ones are collected
% and processed, i.e. bounds and coefficients are computed for the
% variables, rows and colunms are counted, the right hand sides extracted.
%
% For the result arrays we use:
%	_	the field should not be retrieved from CPLEX
%	[]	no value yet
%	else	array of values
% ----------------------------------------------------------------------

% ***When adding new fields, remember to check both lp_setup *AND* lp_read!!
:- export struct(
	prob(
% The first 2 fields are accessed by the C code and must be here in this order
	    cplex_handle,	% handle to C data structure
	    stamp,		% timestamp of last add/del trail
% Input:
	    vars,		% list of problem vars (reverse col order)
	    ints,		% [Xi1,...Xik]: list of integer variables
	    objsense,		% 1 for minimize, -1 for maximize
	    presolve,           % 1 for yes, 0 for no
	    obj,		% float: constant part of objective function
	    linobj,		% list of C*X pairs (for delayed setup)
	    quadobj,		% list of [C,X,Y] triplets (for delayed setup)
	    objcoeffs,		% list of Index:Coeff pairs (for resetting)
	    qobjcoeffs,		% list of q(I,J,Coeff) triplets (for resetting)
	    method,		% int: cplex solving method (primal, dual...)
            aux_method,         % int: cplex auxiliary solving method 
            node_method,        % int: cplex solving method (for MIP)
            node_aux_method,    % int: cplex auxiliary solving method (MIP)
	    demon_tol_int,	% float: tolerable deviation of instantiation
	    demon_tol_real,	% float:  ... to simplex solution.
	    option_vnames,      % atom: yes,no
            timeout,            % float: seconds, 0.0 no timeout

            post_equality,      % atom: yes,no: post equality constr
            option_dump,        % dump problem before solving (no or
                                % write_before_solve/2)
            option_mipstart,    % in [none,all,integers]
% Solver demon
	    solver_id,          % int: Eplex's id for solver
	    suspension,		% suspension or variable
            sync_bounds,        % atom: yes,no
	    nc_trigger,         % atom: yes,no (yes implies nonvar(suspension))
            bd_trigger,         % atom: no,bounds,deviating_bounds
	    triggermodes,	% list of atoms
	    aux_susps,          % extra suspensions associated with handle
	    pool,		% associated pool if any, else var
            change_suspensions, % list of suspensions to schedule on solution
% Handler Goal
            caller_module,      % module from which eplex/lp setup was called
            subopth,            % suboptimal state handler, else var
            unboundh,           % unbounded state handler, else var
            unkh,               % unknown state handler, else var
            aborth,             % abort state handler, else var
            infeash,		% infeasible state handler, else var
% Output (from last solve):
            mr,                 % int: number of user rows 
                                % m = (mr + nccpr)
%            ncpr,               % int: number of uncond. cutpool cstrs
%            nccpr,              % int: number of active cond. cutpool cstrs
            % results from last solve
            status,		% return status of last successful invocation
	    cost,		% float: objval of the current solution
            bestbound,          % float: best bound on objval for current sol.
            worstbound,         % float: worst bound on objval for current sol.
% Output arrays (must start with sols and remain consecutive - accessed from C!)
            sols,		% array[n] of raw solutions (or [] or _)
	    pis,		% array[m] of dual values (or [] or _)
	    slacks,		% array[m] of slacks (or [] or _)
	    djs,		% array[n] of reduced costs (or [] or _)
	    cbase,		% iarray[n] of basis status (or [] or _)
	    rbase,		% iarray[m] of basis status (or [] or _)
            cp_cond_map,        % array of mapping from conditional pool
                                % to added conditional constraints index
            iis_rows,		% iarray[cm] of conflict row ids (or [] or _)
            iis_cols,		% iarray[cn] of conflict col ids (or [] or _)
            iis_colstats	% string[cn] of conflict col status (or [] or _)
	)
    ).

% temporary data needed only during solver setup
:-  local struct(temp_prob(
	    sos,		% list of sos1(Vars) or sos2(Vars)
            extra_vars,         % list of extra problem variables
            use_copy            % 1 for yes, 0 for no
        )).

:- export portray(prob/(property(arity) of prob), lp_handle_tr_out/2, []).
:- export lp_handle_tr_out/2.
lp_handle_tr_out(prob{solver_id: Id}, lp_handle(Id)).


array(X) :- string(X).


% ----------------------------------------------------------------------
% The variables
%
%----------------------------------------------------------------------

:- export struct(
	eplex(
            stamp,      % time stamp for variable in this solver
	    solver,	% A solver that this variable occurs in (prob Handle)
	    idx,	% The column number in that solver's matrix
	    intol_inst,	% Suspension list to be woken on intolerable
	    		%   instantiation, for simplex demons
	    next        % can point to a different eplex attribute for another
	                % handler, or the atom 'end'
	)
    ).


get_lp_attr(X{eplex:Attr0}, Handle, Attr) ?-
	Handle = prob{solver_id:SId},
	( var(Attr0) -> 
	     new_lp_attr(X, Handle, Attr)
	;
             Attr0 = eplex{solver:prob{solver_id:SId0},next:Next},
             % should not fail unless Attr0 incorrect
             (SId0 == SId ->    
                  Attr = Attr0
             ;
                  get_lp_attr1(Next, Attr0, Handle, SId, Attr)
             )
	).
get_lp_attr(X, Handle, Attr) :-			% make a new eplex-variable
	free(X),
	new_lp_attr(X, Handle, Attr).

    get_lp_attr1(ThisAttr, Attr0, Handle, _SId, Attr) :-
	atom(ThisAttr), !, % chain terminated by atom 'end'
	new_lp_attrstruct(Handle, Attr),
	setarg(next of eplex, Attr0, Attr).
    get_lp_attr1(ThisAttr, _Attr0, Handle, SId, Attr) :-
	compound(ThisAttr), 
        ThisAttr = eplex{solver:prob{solver_id:SId0},next:NextAttr},
	(SId0 = SId ->
	     Attr = ThisAttr
	;    
	     get_lp_attr1(NextAttr, ThisAttr, Handle, SId, Attr)
        ).

    new_lp_attr(X, Handle, Attr) :-	% make a new eplex-variable:
	new_lp_attrstruct(Handle, Attr),
	add_attribute(X, Attr, eplex).	% and add an eplex-attribute

:- mode new_lp_attrstruct(+,-).
new_lp_attrstruct(Handle, Attr) :-
	Attr = eplex{solver:Handle,next:end},
        sepia_kernel: timestamp_update(Attr, stamp of eplex),
	init_suspension_list(intol_inst of eplex, Attr).


% From an lp_attr, searches for the attribute corresponding to that for the
% first argument. Fails if none found. 
get_lp_attr_for_handle(Handle, Attr0, Attr) :-
	Handle = prob{solver_id:SId},
        compound(Attr0), 
	get_lp_attr_for_sid(SId, Attr0, Attr).

get_lp_attr_for_sid(SId, Attr0, Attr) :-
        % no need to test for var(Attr0) in chain
        Attr0 = eplex{solver:prob{solver_id:SId0},next:NextAttr},
	(SId0 == SId ->
	     Attr0 = Attr
	;    
	     get_lp_attr_for_sid(SId, NextAttr, Attr)
	).


% Update bounds

% the user accessible lp_var_set_bounds/4 raises a range error if variable
% not in problem represented by Handle
lp_var_set_bounds(Handle, V, Lo0, Hi0) :- var(Handle), !,
	error(4, lp_var_set_bounds(Handle, V, Lo0, Hi0)).
lp_var_set_bounds(Handle, V, Lo0, Hi0) :-
        Handle = prob{solver_id:SId},
	!,
	( get_var_index(V, SId, I) ->
            Lo is float(Lo0),
            Hi is float(Hi0),
            get_lp_attr(V, Handle, Attr),
            lp_impose_col_bounds(Handle, Attr, I, Lo, Hi),
            wake
	; number(V) ->
            V >= Lo0,
            V =< Hi0
        ;
            printf(error, "Eplex error: variable %w is not a problem"
                          " variable for handle %w:%n",[V,Handle]),
            error(6, lp_var_set_bounds(Handle, V, Lo0, Hi0))

        ).
lp_var_set_bounds(Handle, V, Lo0, Hi0) :-
        printf(error, "Eplex error: invalid problem handle %w:%n",[Handle]), 
        error(5, lp_var_set_bounds(Handle, V, Lo0, Hi0)).


% Update bounds

% the user accessible lp_var_non_monotonic_set_bounds/4 raises a range
% error if variable not in problem represented by Handle
% note we do not allow this for instances or handles with trigger
% conditions and demons, so there is no need to schedule wakings after
% the bound change as there can be none
lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0) :- var(Handle), !,
	error(4, lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0)).
lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0) :-
        Handle = prob{solver_id:SId, pool: Pool, suspension:S},
        ((is_suspension(S) ; nonvar(Pool)) ->
             printf(error, "Eplex error: problem cannot be modified by %w "
                    "(has trigger conditions or is an eplex instance).%n",
                    [lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0)]),
             abort
        ;
             true
        ),
        !,
	(get_unique_var_index(V, SId, I) ->
              Lo is float(Lo0),
	      Hi is float(Hi0),
	      get_lp_attr(V, Handle, Attr),
	      cplex_impose_col_bounds(Handle, Attr, I, 0, Lo, Hi, _Changed)
	;
              % raise an error if variable is not a problem variable or
              % has merged columns
              printf(error, "Eplex error: %w is either not a problem"
                     " variable or it represents more than one column:%n", [V]),
              error(6, lp_var_non_monotonic_set_bounds(Handle, V, Lo0,Hi0))
	).
lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0) :-
        printf(error, "Eplex error: invalid problem handle %w:%n", [Handle]),
        error(5, lp_var_non_monotonic_set_bounds(Handle, V, Lo0, Hi0)).


eplex_var_get_bounds(V, Lo, Hi, Pool) :-
        get_pool_handle(Handle,Pool), !,
        lp_var_get_bounds(Handle, V, Lo, Hi).
eplex_var_get_bounds(V, Lo, Hi, Pool) :-
	printf(error, "Eplex error: instance %w has no associated solver:%n",[Pool]),
        error(5, eplex_var_get_bounds(V, Lo, Hi)).

lp_var_get_bounds(Handle, V, Lo, Hi) :- var(Handle), !,
	error(4, lp_var_get_bounds(Handle, V, Lo, Hi)).
lp_var_get_bounds(Handle, V, Lo, Hi) :-
        Handle = prob{cplex_handle:CPH,solver_id:SId},
	!,
	( get_first_var_index(V, SId, I) -> % first index has the tightest bounds
            cplex_get_col_bounds(CPH, I, Lo, Hi)
	; number(V) ->
            Lo is float(V), 
            Hi is float(V)
        ;
            printf(error, "Eplex error: %w is not a problem variable for"
                   " handle %w:%n", [V,Handle]),
            error(6, lp_var_get_bounds(Handle, V, Lo,Hi))
        ).
lp_var_get_bounds(Handle, V, Lo, Hi) :-
        printf(error, "Eplex error: Invalid problem handle %w:%n", [Handle]),
        error(5, lp_var_get_bounds(Handle, V, Lo, Hi)).


% may throw(abort) indicating error
lp_impose_col_bounds(Handle, Attr, [I|_], Lo, Hi) :-
        cplex_impose_col_bounds(Handle, Attr, I, 1, Lo, Hi, Changed),
        schedule_demon_if_bounds_changed(Handle, I, Changed).


    impose_col_bounds(Handle, Attr, I, Bound, Changed, (>=))  :-
        cplex_impose_col_lwb(Handle, Attr, I, Bound, Changed).
    impose_col_bounds(Handle, Attr, I, Bound, Changed, (=<)) :-
        cplex_impose_col_upb(Handle, Attr, I, Bound, Changed).
    impose_col_bounds(Handle, Attr, I, Bound, Changed, (=:=)) :-
        cplex_impose_col_bounds(Handle, Attr, I, 1, Bound, Bound, Changed).


% Number the variables from the end of list, from Col0 to ColN-1

set_var_indices(Xs, Handle, Col0, Length, ColN) :-
        length(Xs, Length), % create or count Xs
	ColN is Col0 + Length,
	( foreach(X, Xs),
	  for(I, ColN-1, Col0, -1),
	  param(Handle) do
	    set_var_index(X, Handle, I)
	).

    set_var_index(X, _Handle, _) :- nonvar(X).
    set_var_index(X, Handle, J) :- var(X),
	get_lp_attr(X, Handle, A),
        % type error if already has an index (should not happen)
	( arg(idx of eplex, A, [J]) -> true
        ; error(5, set_var_index(X, Handle, J))
        ).



% Retrieve the variable's column for problem represented by SId. 
% Index will be in the form of a list

get_first_var_index(V, SId, Index) :-
        get_var_index(V, SId, Is), Is = [Index|_].

get_unique_var_index(V, SId, Index) :-
        get_var_index(V, SId, Is), Is = [Index].

get_var_index(_{eplex:eplex{idx:I,next:Next,solver:prob{solver_id:SId0}}}, 
              SId, Index) ?- 
	(SId0 == SId -> I = Index ; get_var_index1(Next, SId, Index)).

    get_var_index1(eplex{idx:I,solver:prob{solver_id:SId0},next:Next}, SId, Index) ?-
	(SId0 == SId -> I=Index ; get_var_index1(Next, SId, Index)).



lp_var_occurrence(X, Handle, Index) :-
        var(Handle),
        lp_var_occurrence1(X, Handle, Index).
lp_var_occurrence(X, Handle, Index) :-
        % returns the first col index
        nonvar(Handle),
        Handle = prob{solver_id:SId},
	get_first_var_index(X, SId, Index).


   lp_var_occurrence1(_{eplex:Attr}, Handle, Index) ?-
	lp_var_attrs(Attr, Handle, Index).

   :- mode lp_var_attrs(+,-,-).
   lp_var_attrs(eplex{solver:H,idx:[I|_],next:Next}, Handle, Index) ?-
	(H=Handle, 
	 I = Index 
	;
	 lp_var_attrs(Next, Handle, Index)
	).



lp_var_solution(Handle, _{eplex:Attr0}, Sol) ?-
	get_lp_attr_for_handle(Handle, Attr0, Attr),
	lp_attr_solution(Attr, Sol).
lp_var_solution(_, X, Sol) :-
	integer(X),
	Sol is float(X).
lp_var_solution(_, X, Sol) :-
	real(X),
	X = Sol.

lp_var_typed_solution(Handle, _{eplex:Attr0}, TSol) ?-
	get_lp_attr_for_handle(Handle, Attr0, Attr),
        Attr = eplex{idx:[J|_],solver:prob{cplex_handle:CPH,sols:SolArr}},
	array(SolArr),
        darray_size(SolArr, Size), J < Size,
        get_typed_solution_for_col(CPH, J, SolArr, TSol).
lp_var_typed_solution(_, X, Sol) :-
	integer(X),
	X = Sol.
lp_var_typed_solution(_, X, Sol) :-
	real(X),
	X = Sol.


get_typed_solution_for_col(CPH, J, SolArr, TSol) :-
        get_darray_element(SolArr, J, Sol),
        cplex_get_col_type(CPH, J, TypeCode),
        cplex_type_code(T, TypeCode),
	( T == integer ->
	    TSol is fix(round(Sol))
	;
	    % The following is necessary due to solver's limited precision...
            % (assumes col bounds have not changed; otherwise solution
            % value may be changed slightly if it is close to the new bounds)
            cplex_get_col_bounds(CPH, J, XL, XH),
            % use cplex_get_param here to access the feasibility_tol for
            % the (possibly local) value for this CPH 
            ( Sol < XL,Sol >= XL-cplex_get_param(CPH,feasibility_tol) ->
                TSol=XL
            ; Sol > XH,Sol =< XH+cplex_get_param(CPH,feasibility_tol) ->
                TSol=XH
            ; 
                TSol=Sol 
            )
	).


lp_var_type(Handle, _{eplex:Attr0}, Type) ?-
	get_lp_attr_for_handle(Handle, Attr0, Attr),
	Attr = eplex{idx:[J|_],solver:prob{cplex_handle:CPH}},
	cplex_get_col_type(CPH, J, TCode),
	cplex_type_code(Type, TCode).
lp_var_type(_, X, Type) :-
	integer(X),
	Type = integer.
lp_var_type(_, X, Type) :-
	real(X),
	Type = real.

lp_var_redcost(Handle, _{eplex:Attr}, Val) ?-
	get_lp_attr_for_handle(Handle, Attr, eplex{idx:Idxs,solver:prob{djs:Array}}),
	array(Array),
        (Idxs = [J] -> 
            % one column only, can get reduced cost for column
            darray_size(Array, Size), J < Size,
            get_darray_element(Array, J, Val)
        ;
            % more than one column presented by variable, the simple safe 
            % reduced cost to return is 0
            Val = 0.0
        ).
lp_var_redcost(_Handle, X, Val) :-
	number(X),
	Val = 0.0.

lp_attr_itol(eplex{solver:prob{demon_tol_int:Tol0}}, Tol) ?-
	Tol=Tol0.

lp_attr_rtol(eplex{solver:prob{demon_tol_real:Tol0}}, Tol) ?-
	Tol=Tol0.

lp_attr_solution(eplex{idx:[J|_],solver:prob{sols:SolArr}}, Sol) ?-
        % solution value is the same for unified columns, just get from one
        array(SolArr),
        darray_size(SolArr, Size), J < Size,
	get_darray_element(SolArr, J, Sol).


eplex_var_get(X, What, Val, Pool) :-
	get_pool_item(Pool, Handle), 
	( Handle == 0 ->
	    printf(error, "Eplex error: instance has no solver set up in %w%n",
		[Pool:eplex_var_get(X, What, Val)]),
	    abort
	;
	    lp_var_get1(Handle, X, What, Val)
	).


lp_var_get(Handle, X, What, Val) :- var(Handle), !,
	error(4, lp_var_get(Handle, X, What, Val)).
lp_var_get(Handle, X, What, Val) :- compound(Handle), !,
	 lp_var_get1(Handle, X, What, Val).
lp_var_get(Handle, X, What, Val) :-
	error(5, lp_var_get(Handle, X, What, Val)).


    lp_var_get1(Handle, X, solution, Val) :- !,
	(lp_var_solution(Handle, X, Val) ->
             true 
        ; 
             fail_if_valid_var_get(lp_var_get(Handle,X,solution,Val),
                                   X, Handle, sols of prob)
        ).
    lp_var_get1(Handle, X, typed_solution, TVal) :- !,
	(lp_var_typed_solution(Handle, X, TVal) ->
	     true
        ;
             fail_if_valid_var_get(lp_var_get(Handle,X,typed_solution,TVal),
                                   X, Handle, sols of prob)
        ).
    lp_var_get1(Handle, X, type, Type) :- !,
	(lp_var_type(Handle, X, Type) ->
	     true
        ;
             printf(error, "Eplex error: %w not a problem variable:%n", [X]), 
             error(6, lp_var_get(Handle, X, type, Type))
        ).
    lp_var_get1(Handle, X, reduced_cost, Val) :- !,
 	(lp_var_redcost(Handle, X, Val) ->
	      true
	;
	      fail_if_valid_var_get(lp_var_get(Handle,X,reduced_cost,Val),
				    X, Handle, djs of prob)
        ).
    lp_var_get1(Handle, X, What, Val) :-
	error(6, lp_var_get(Handle, X, What, Val)).


    % fails only if the variable is a problem variable for Handle and
    % there is a value array at Pos waiting to be filled. Error otherwise
    fail_if_valid_var_get(Goal, X, Handle, Pos) :-
        arg(solver_id of prob, Handle, SId),
        (get_var_index(X, SId, _) ->
             arg(Pos, Handle, Array),
             var(Array),  % fails silently otherwise
             writeln(error, "Eplex error: information not requested at"
                    " solver setup:"),
             error(6, Goal)
        ;
             writeln(error, "Eplex error: not accessing an eplex problem variable:"), 
             error(6, Goal)
        ).

% obsolete
lp_var_get(X{eplex:eplex{solver:Handle,next:Next}}, What, Val) ?- !,
	( Next == end ->
	    lp_var_get(Handle, X, What, Val)
	;
	    writeln(error, "Eplex error: more than one solver handle in lp_var_get/3 - use lp_var_get/4"),
	    abort
	).
lp_var_get(X, What, Sol) :-
	number(X),
	lp_var_get1(_Handle, X, What, Sol).

% obsolete
lp_var_solution(X, Val) :-
	lp_var_get(X, solution, Val).


/***
% Debugging
checkvar(X) :-
	var(X), !,
	var_type(X, Type),
	var_range(X, Lo, Hi),
	(
	    Type == integer,
	    (abs(Lo-round(Lo)) > 0.000001 ; abs(Hi-round(Hi)) > 0.000001)
	->
	    writeq(error, checkvar(X)),nl(error)
%	;
%	    Lo >= Hi
%	->
%	    writeq(error, checkvar(X)),nl(error)
	;
	    true
	).
checkvar([]) :- !.
checkvar([X|Xs]) :- !,
	checkvar(X),
	checkvar(Xs).
checkvar(_).
***/


% Unify handler:
% With var/var binding of two eplex variables, their eplex attributes are
% merged. Existing eplex attribute chains should each represent different
% solver. On merging, the two variables may contain attributes for different
% columns of the same solver, in which case, the attribute representing one
% of the columns is lost.

unify_eplex(_Term, Attr) :-
	var(Attr).		% Ignore if no attribute for this extension
unify_eplex(Term, Attr) :-
	compound(Attr),
	unify_term_eplex(Term, Attr).

:- mode unify_term_eplex(?, +).
unify_term_eplex(Y{eplex:AttrY}, AttrX) :-
	-?->
	unify_eplex_eplex(Y, AttrX, AttrY).
unify_term_eplex(X, Attr) :-
	integer(X),		% The variable was instantiated
	unify_number_eplex(X, Attr).
unify_term_eplex(X, Attr) :-
	real(X),		% The variable was instantiated
	unify_number_eplex(X, Attr).

unify_eplex_eplex(Y, AttrX, AttrY) :-
	var(AttrY),		% No attribute for this extension
	AttrX = AttrY,		% Transfer the attribute
	notify_constrained(Y).
unify_eplex_eplex(Y, AttrX, AttrY) :-
	nonvar(AttrY),
	( nonvar(AttrX) ->
	     AttrY = eplex{idx:IdxY,solver:prob{solver_id:SIdY}},
	     (AttrX = eplex{idx:IdxY,solver:prob{solver_id:SIdY}} ->
		  true   % same attribute, do nothing
	     ;
                  % merge X's attributes into Y
		  hash_create(HashY),
		  attributes_to_hash(AttrY, HashY, LastAttrY),
		  setarg(next of eplex, LastAttrY, AttrX),
		  remove_and_merge_hashed_occurences(AttrX, LastAttrY, Y, HashY),
		  notify_constrained(Y)
	     )
	;    
	   notify_constrained(Y)
	).


unify_number_eplex(X0, Attr) :-
        compound(Attr),
        Attr = eplex{solver:Handle, idx:Is},
        X is float(X0),
        % impose the column bounds the first column only
        lp_impose_col_bounds(Handle, Attr, Is, X, X),
	( lp_attr_solution(Attr, S) ->
            arg(cplex_handle of prob, Handle, CPH),
	    lp_attr_tol(CPH, Is, Attr, Tol),
	    ( abs(S-X) =< Tol ->
		true
	    ;
		schedule_suspensions(intol_inst of eplex, Attr)
	    )
	;
	    true
	),
	Attr = eplex{next:NextAttr},
        unify_number_eplex(X, NextAttr).
unify_number_eplex(_X, end) ?- true.

   lp_attr_tol(CPH, [I|_], Attr, Tol) :-
        % look at first col only: col types of all merged col are the same
        cplex_get_col_type(CPH, I, TC),
        (cplex_type_code(integer, TC) -> 
             lp_attr_itol(Attr, Tol)
        ;
             lp_attr_rtol(Attr, Tol)
        ).

attributes_to_hash(Attr, Hash, LastAttr) :-
        Attr = eplex{next:Next,solver:prob{solver_id:SId}},
        hash_add(Hash, SId, Attr),
        (Next == end ->
             LastAttr = Attr ; attributes_to_hash(Next, Hash, LastAttr)
        ).

remove_and_merge_hashed_occurences(Attr, PrevAttr, Var, Hash) :-
        Attr = eplex{next:Next,idx:Idx,solver:Handle},
        Handle = prob{solver_id:SId,cplex_handle:CPH,post_equality:PostEq},
        (hash_find(Hash, SId, MergedAttr) ->
            % MergedAttr represents other column(s) in the matrix. Merge
            % the information from Attr into it
            (PostEq == no ->
                Scheduled = no
             ;
                add_equality_constraint(Var, Attr, Scheduled)
            ),
            merge_and_check_cols(CPH, Handle, Idx, MergedAttr, Scheduled),
            merge_suspension_lists(intol_inst of eplex, Attr, 
                                   intol_inst of eplex, MergedAttr),
            % remove Attr from chain
            setarg(next of eplex, PrevAttr, Next),
            PrevAttr1 = PrevAttr
        ;
            PrevAttr1 = Attr
        ),
        remove_and_merge_hashed_occurences(Next, PrevAttr1, Var, Hash).
remove_and_merge_hashed_occurences(end, _, _, _) ?- true.


    merge_and_check_col_bounds(CPH, Handle, [I0|_], [I1|_], Attr0, Scheduled) :-
        cplex_get_col_bounds(CPH, I0, Lo0, Hi0),
        cplex_get_col_bounds(CPH, I1, Lo1, Hi1),

        (Lo0 =:= Lo1 -> 
            NewLo = Lo0
        ;
            NewLo is max(Lo0, Lo1),
            Changed = 1
        ),
        (Hi0 =:= Hi1 -> 
            NewHi = Hi0
        ;
            NewHi is min(Hi0, Hi1),
            Changed = 1
        ),
        NewHi >= NewLo,
        (Changed == 1 ->
            % impose new bound on first column
            cplex_impose_col_bounds(Handle, Attr0, I0, 1, NewLo, NewHi, _),
            (Scheduled \== yes ->
                % not already scheduled due to new_constraints trigger.
                schedule_demon_if_bounds_changed(Handle, 1.0Inf, 1)
                % with col number set to 1.0Inf, trigger test for deviating
                % bounds will fail and so not trigger
            ;
                true
            )
        ;
            true
        ).

    % merge the column(s) represented by Idx into Attr0
    merge_and_check_cols(CPH, Handle, Idx1, Attr0, Scheduled) :-
        Attr0 = eplex{idx:Idx0},
        merge_col_type(CPH, Handle, Idx0, Idx1),
        merge_and_check_col_bounds(CPH, Handle, Idx0, Idx1, Attr0, Scheduled),
        % Idx0 should be in front in the MergedIdx to preserve the same
        % first idx for Attr0
        append(Idx0, Idx1, MergedIdx),
        setarg(idx of eplex, Attr0, MergedIdx).

    merge_col_type(CPH, Handle, Idx0, Idx1) :-
        % make sure all merged columns have the same type.
        % note that as we do this every time we merge columns, so
        % if Idx0 and Idx1 are multiple columns, they must have the same
        % type across the multiple columns 
        Idx0 = [I0|_], 
        Idx1 = [I1|_],
        cplex_get_col_type(CPH, I0, TypeCode0),
        cplex_type_code(Type0, TypeCode0),
        cplex_get_col_type(CPH, I1, TypeCode1),
        cplex_type_code(Type1, TypeCode1),
        (Type0 \== Type1 ->
            % one of the column is of integer type. Make the other column(s)
            % the same integer type
            (Type0 == integer -> /* Idx0 is integer */
                Idxs = Idx1,
                TypeCode = TypeCode0
            ; /* assume Idx1 is integer */
                Idxs = Idx0,
                TypeCode = TypeCode1
            ),
            (foreach(I, Idxs), param(Handle,TypeCode) do
                cplex_change_col_type(Handle, I, TypeCode)	% backtrackable
            )
        ;
            true /* column types same, no need to change column types */
        ).


    add_equality_constraint(X1, X2Att, Trigger) :-
	% two variables in the same solver are unified, send an equality
        % constraint for the two columns to the external solver.
	% Use a dummy variable as `container' for the attribute X2Att,
        % as original X2 is not available to the unification handler
        % X1 must contain an attribute which has the same solver
        % as X2Att. 
	normalise_cstr((X1 =:= X2), Norm),
	add_attribute(X2, X2Att, eplex),
	X2Att = eplex{solver:Handle},
	Handle = prob{solver_id:SId, cplex_handle:CPH, nc_trigger:Trigger,
                      suspension:Susp},
	setup_new_rows(CPH, SId, 0, _, [Norm], _),
        clear_result(Handle, cbase of prob), % added a new row
	clear_result(Handle, rbase of prob),
	cplex_flush_new_rowcols(Handle, 0),
        (Trigger == yes -> 
            schedule_suspensions(1, s([Susp]))
        ;
            true
        ).


suspensions_eplex(_{eplex:Attr}, Susps, Susps0) ?-
	( var(Attr) -> 
	      Susps0=Susps ; collect_suspensions_eplex(Attr, Susps, Susps0)
	).

collect_suspensions_eplex(end, Susps, Susps0) ?- Susps=Susps0.
collect_suspensions_eplex(eplex{intol_inst:S, next:NextAttr}, 
                          Susps, Susps0) ?-
        Susps = [S|Susps1],
        collect_suspensions_eplex(NextAttr, Susps1, Susps0).


schedule_demon_if_bounds_changed(prob{
	    bd_trigger:TrigCond,cplex_handle:CPH,suspension:Susp,
	    demon_tol_real:RT,demon_tol_int:IT,sols:SolArr}, I, 1) ?-
        check_if_should_trigger(TrigCond, CPH, RT, IT, SolArr, I),
        !,
        schedule_suspensions(1, s([Susp])).
schedule_demon_if_bounds_changed(_,_,_).


bound_change_requires_scheduling(prob{
	    bd_trigger:TrigCond,cplex_handle:CPH,
	    demon_tol_real:RT,demon_tol_int:IT,sols:SolArr}, I) ?-
        check_if_should_trigger(TrigCond, CPH, RT, IT, SolArr, I).


check_if_should_trigger(bounds,_,_,_,_,_).
check_if_should_trigger(deviating_bounds, CPH, RT, IT, SolArr, I) :-
        array(SolArr),
        darray_size(SolArr, Size), I < Size,
        get_darray_element(SolArr, I, S),
        cplex_get_col_bounds(CPH, I, Lo, Hi),
        \+ tolerable_range_var(S, Lo, Hi, CPH, I, RT, IT).





% Print handler
lp_var_print(_{eplex:Attr}, Printed) ?-
	nonvar(Attr),
	lp_var_print1(Attr, Sols),
        % fails and print nothing if Sols is []
        (Sols = [Printed] -> true ;  Sols = [_|_], Sols = Printed).

   lp_var_print1(Attr, SolsIn0) :-
        Attr = eplex{next:NextAttr, idx:[I|_],
                           solver:prob{cplex_handle:CPH}},
        nonvar(CPH),  % we do have the low-level information
        catch(cplex_get_col_bounds(CPH, I, L, H),_,
              (L='?', H='?') ), /* catch case when var not yet in solver */
	(lp_attr_solution(Attr, Sol) ->
	     SolsIn0 = [L..H@Sol|SolsIn1] 
        ; 
             SolsIn0 = [L..H|SolsIn1]
	),
        lp_var_print1(NextAttr, SolsIn1).
   lp_var_print1(end, SolsIn1) ?- SolsIn1 = [].


% Find the variable with the most fractional lp-solution.
% Never fails on a non-empty list.

deletemostfract(BestX, [X|Xs], Handle, Rest) :-
	nonvar(Handle),
        lp_var_solution(Handle, X, Val),
        Diff is abs(Val - round(Val)),
        deletemostfract(Xs, X, Handle, Diff, BestX, Rest).
deletemostfract(BestX, Xs, Handle, Rest) :-
        var(Handle),
        error(4, deletemostfract(BestX, Xs, Handle, Rest)).


deletemostfract([], BestX, _Handle, _BestDiff, BestX, []).
deletemostfract([X|Xs], BestX, Handle, BestDiff, Res, Rest) :-
	lp_var_solution(Handle, X, Val),
	Diff is abs(Val - round(Val)),
	( Diff > BestDiff ->
	    Rest = [BestX|Rest0],
	    deletemostfract(Xs, X, Handle, Diff, Res, Rest0)
	;
	    Rest = [X|Rest0],
	    deletemostfract(Xs, BestX, Handle, BestDiff, Res, Rest0)
	).


% Find the first variable whose lp-solution deviates more than Tolerance
% from the nearest integer. Fails if there is no such variable in the list.

deletefract(BestX, Xs, Handle, Tolerance, Rest) :-
	nonvar(Handle),
        deletefract1(BestX, Xs, Handle, Tolerance, Rest).
deletefract(BestX, Xs, Handle, Tolerance, Rest) :-
        var(Handle),
	error(4, deletefract(BestX, Xs, Handle, Tolerance, Rest)).

   deletefract1(BestX, [X|Xs], Handle, Tolerance, Rest) :-
	lp_var_solution(Handle, X, Val),
	( abs(Val - round(Val)) > Tolerance ->
	    BestX = X,
	    Rest = Xs
	;
	    Rest = [X|Rest0],
	    deletefract1(BestX, Xs, Handle, Tolerance, Rest0)
	).

% set_bounds handler

lp_attr_set_bounds(_{eplex:Attr}, Lo, Hi) ?-
	% Attr guaranteed to be nonvar
        lp_attr_set_bounds1(Attr, Lo, Hi).

    lp_attr_set_bounds1(Attr, Lo0, Hi0) ?-
        Attr = eplex{solver:Handle,idx:I, next:Next},
        Lo is float(Lo0),
        Hi is float(Hi0),
        lp_impose_col_bounds(Handle, Attr, I, Lo, Hi),
        lp_attr_set_bounds1(Next, Lo, Hi).
    lp_attr_set_bounds1(end, _, _) ?-
    	wake.


% get_bounds handler

lp_attr_get_bounds(_{eplex:Attr}, Lo, Hi) ?-
	% Attr guaranteed to be nonvar
        lp_attr_get_bounds1(Attr, -1.0Inf, 1.0Inf, Lo, Hi).

    lp_attr_get_bounds1(eplex{solver:prob{cplex_handle:CPH},idx:[I|_],next:Next},
                        Lo0, Hi0, Lo, Hi) ?-
        cplex_get_col_bounds(CPH, I, Lo1, Hi1),
	Lo2 is max(Lo0, Lo1),
	Hi2 is min(Hi0, Hi1),
        lp_attr_get_bounds1(Next, Lo2, Hi2, Lo, Hi).
    lp_attr_get_bounds1(end, Lo0, Hi0, Lo, Hi) ?-
        Lo0 = Lo, Hi0 = Hi.

% ----------------------------------------------------------------------
% The user-level constraints
% ----------------------------------------------------------------------

:- local reference(lp_info).
:- local struct(lp_info(newid,    % int: next handler id  
			pending   % suspension: lp_pending's suspension
	 )).

lp_impose_interval(Vs, Interval, Pool) :-
	(range(Interval, Lo, Hi) -> Hi >= Lo 
        ;  throw(abort)
        ),
        extract_vars(Vs, Lo, Hi, no, [], VList), % may abort
        get_pool_item(Pool, Handle), 
        (Handle == 0 ->
             (foreach(V, VList), param(Pool, Lo, Hi) do
                  % just post constraints (no solver to add to yet)
                  cplex_lo_hi(MInf, PInf),
                  (Lo < MInf -> Lo1 = MInf ; Lo1 = Lo),
                  (Hi > PInf -> Hi1 = PInf ; Hi1 = Hi),
                  (Lo1 == Hi1 ->
                       add_pool_constraint(V=:=Lo1, Pool)
                  ;
                       add_pool_constraint(V>=Lo1, Pool),
                       add_pool_constraint(V=<Hi1, Pool)
                  )
             )
        ;
	     lp_add_vars_interval(Handle, VList, Lo, Hi)
        ).


lp_interval(Vs, Interval, Pool) :-
        catch(lp_impose_interval(Vs, Interval, Pool), Tag,
		colon_interval_warning((::), Vs, Interval, Tag)).

    % warn about: poolname:X::1..N
    colon_interval_warning(Functor, Vs, Interval, abort) :- !,
	Goal =.. [Functor,Vs,Interval],
	( nonvar(Vs), Vs = (_:_) ->
	    printf(error, "Eplex error: invalid syntax detected in %w; missing"
		     " brackets perhaps?%n", [Goal]),
	    throw(abort)
	;
            error(5, Goal)
        ).
    colon_interval_warning(_Functor, _Vs, _Interval, Tag) :-
	throw(Tag).

    :- mode range(?,-,-).
    range(L..H, Lo, Hi) ?- 
	bound(L, Lo),
	bound(H, Hi).
    range([L..H], Lo, Hi) ?-  % for IC/FD compatability
	bound(L, Lo),
	bound(H, Hi).
    
    bound(L, _) :- var(L), !, fail.
    bound(L, Lo) :- integer(L), !, Lo is float(L).
    bound(L, Lo) :- rational(L), !, Lo is float(L).
    bound(L, Lo) :- real(L), !, Lo=L.
    bound(-inf, Lo) :- !, Lo = -1.0Inf.
    bound(inf, Hi) :- !, Hi = 1.0Inf.
    bound(+inf, Hi) :- !, Hi = 1.0Inf.
    bound(E, Bnd) :-
        catch((Bnd0 is E), _, fail),
	Bnd is float(Bnd0).

% may throw(abort) indicating error
extract_vars(V, _, _, _, VList0, VList) :-
        var(V), !,
        VList = [V|VList0].
extract_vars(V, Lo, Hi, CheckInt, VList0, VList) :-
        number(V), !,
        (CheckInt == yes -> integer(V) ; true),
        V >= Lo, V =< Hi,
        VList = VList0.
extract_vars([], _, _, _, VList0, VList) :- !, VList0 = VList.
extract_vars([X|Xs], Lo, Hi, CheckInt, VList0, VList) :- !,
        extract_vars(X, Lo, Hi, CheckInt, VList0, VList1),
        extract_vars(Xs, Lo, Hi, CheckInt, VList1, VList).
extract_vars(subscript(Array,Index), Lo, Hi, CheckInt, VList0, VList) :- 
        subscript(Array, Index, E), !,
        extract_vars(E, Lo, Hi, CheckInt, VList0, VList).
extract_vars(_,_,_,_,_,_) :- throw(abort). 


% Add constraint of any type (linear,sos,idc) to Pool
add_pool_constraint(Cstr, Pool) :-
	( get_pool_handle(Handle, Pool) ->
	    lp_add_constraints(Handle, [Cstr], [])
	;
	    % We don't have a solver yet: post constraints to pool.
	    % For linear constraints, normalise and ground check them first.
	    post_constraint_to_pool(Cstr, Pool)
	).

% Add constraints of any type (linear,sos,idc) to Pool
eplex_add_constraints(Cstrs, Ints, Pool) :-
        (get_pool_handle(Handle, Pool) ->
            lp_add_constraints(Handle, Cstrs, Ints)
        ;
            (foreach(C, Cstrs), param(Pool) do
		post_constraint_to_pool(C, Pool)
	    ),
            integers(Ints, Pool)
        ).


    % Post linear, sos or idc constraints to Pool.
    % Linear ones get normalised and ground checked first.
    post_constraint_to_pool(Cstr, Pool) :- var(Cstr), !,
	error(4, post_constraint_to_pool(Cstr, Pool)).
    post_constraint_to_pool(Cstr, Pool) :- Cstr=sos1(_), !,
	post_typed_pool_constraint(Pool, sos of constraint_type, Cstr),
	set_lp_pending.
    post_constraint_to_pool(Cstr, Pool) :- Cstr=sos2(_), !,
	post_typed_pool_constraint(Pool, sos of constraint_type, Cstr),
	set_lp_pending.
    post_constraint_to_pool(Cstr, Pool) :- Cstr=(_=>_), !,
	post_typed_pool_constraint(Pool, idc of constraint_type, Cstr),
	set_lp_pending.
    post_constraint_to_pool(Cstr, Pool) :-
	normalise_cstr(Cstr, Norm0),
	!,
	try_ground_check(Norm0, Norm),	% may fail
	( var(Norm) ->
	    true			% simplified away
	;
	    post_typed_pool_constraint(Pool, linear of constraint_type, Norm),
	    set_lp_pending
	).
    post_constraint_to_pool(Cstr, Pool) :-
	error(5, post_constraint_to_pool(Cstr, Pool)).


lp_eq(X, Y, Pool) :- add_pool_constraint(X=:=Y, Pool).

lp_ge(X, Y, Pool) :- add_pool_constraint(X>=Y, Pool).

lp_le(X, Y, Pool) :- add_pool_constraint(X=<Y, Pool).

sos1(Xs, Pool) :- add_pool_constraint(sos1(Xs), Pool).

sos2(Xs, Pool) :- add_pool_constraint(sos2(Xs), Pool).

indicator_constraint(Cond, Cstr, Pool) :- add_pool_constraint(Cond=>Cstr, Pool).


% Can handle only linear constraints, returns indexes
lp_add_constraints(Handle, Cstrs, Ints, Idxs) :-
	( nonvar(Handle), nonvar(Cstrs), nonvar(Ints) -> true ;
	    error(4, lp_add_constraints(Handle,Cstrs,Ints,Idxs))
	),
	normalise_and_check_linear_nonground(Cstrs, NormCs, lp_add_constraints),
        lp_add_indexed(Handle, NormCs, Ints, Idxs).


% Can handle only linear constraints, returns indexes
lp_add_cutpool_constraints(Handle, Cstrs, Opts, Idxs) :-
	( nonvar(Handle), nonvar(Cstrs) -> true ;
            error(4, lp_add_cutpool_constraints(Handle, Cstrs, Opts, Idxs))
	),
	normalise_and_check_linear_nonground(Cstrs, NormCs, lp_add_cutpool_constraints),
	OptSet = cp_options{group:Name,active:Act,add_initially:Add},
	( (foreach(O:V, Opts), param(OptSet,Handle) do
            valid_cp_opt(O, OPos),
            valid_cp_optval(O, V, Handle, OVal),
            arg(OPos, OptSet, OVal)
	  ) ->
            true
        ;
            printf(error, "Eplex error: Invalid option for"
                   " lp_add_cutpool_constraints: %w%n", [Opts]),
            abort
        ),
	(var(Name) -> Name = [] ; true),
	(var(Act)  ->  Act =  1 ; true),
	(var(Add)  ->  Add =  1 ; true),
        Handle = prob{solver_id:SId,cplex_handle:CPH},
        cplex_get_cutpool_size(CPH, NRows, NNzs), 
        cplex_get_prob_param(CPH, 15, MaxVIdx),
        constraint_type_code(condcp, CType),
        get_named_cp_index(Handle, Name, NIdx),
        (setup_new_rows(CPH, SId, CType, MaxVIdx, NormCs, Idxs) ->
            (foreach(I,Idxs), param(CPH,NIdx,Act,Add) do
                rawidx_cstridx(_, RawI, I),
                cplex_init_cpcstr(CPH, RawI, NIdx, Act, Add)
            )
        ;
            % NormCs has non-orginal variables - reset global constraints
            % pool and abort
            cplex_reset_cutpool_size(CPH, NRows, NNzs),
            printf(error, "Eplex error: trying to post cutpool constraints"
                   " that contain new variables in %w.%n",
                   [lp_add_cutpool_constraints(Handle, Cstrs, Opts, Idxs)]),
            abort
        ).


normalise_and_check_linear_nonground(Cstrs, NormCs, PredCall) :-
        ( normalise_cstrs(Cstrs, NormCs, NonLins) -> true ;
            printf(error, "Eplex error: unknown constraint in %w:%n%w%n", [PredCall,Cstrs]),
	    abort
	),
	( NonLins = [Example|_] ->
            printf(error, "Eplex error: no nonlinear constraints allowed in %w:%n%w%n", [PredCall,Example]),
            abort
	;
	    true
	),
        % check that there are no ground constraints
        ( (foreach(_:[_,_|_], NormCs) do true) -> true ;
            printf(error, "Eplex error: no ground constraints allowed in %w:%n%w%n", [PredCall,Cstrs]),
	    abort
	).

all_nonground(NormCs) :-
        ( foreach(_:[_,_|_], NormCs) do true ).


% Add constraints of any type (linear,sos,idc) to existing Handle
lp_add_constraints(_Handle, [], []) :- !.
lp_add_constraints(Handle, Cstrs, Ints) :-
	( nonvar(Handle), nonvar(Ints), nonvar(Cstrs) -> true ;
	     error(4, lp_add_constraints(Handle,Cstrs,Ints)) 
	),
	normalise_cstrs(Cstrs, NormCs0, NonLins),
	!,
	% analyse the nonlinear part
	(
	    foreach(NonLin,NonLins),
	    fromto(SOSs,SOSs1,SOSs2,[]),
	    fromto(IDCs3,IDCs1,IDCs2,[]),
	    fromto(NLs,NLs1,NLs2,[])
	do
	    ( var(NonLin) ->
	    	SOSs1=SOSs2, IDCs1=IDCs2, NLs1 = [NonLin|NLs2]
	    ; NonLin = sos1(_) ->
	    	SOSs1 = [NonLin|SOSs2], IDCs1=IDCs2, NLs1=NLs2
	    ; NonLin = sos2(_) ->
	    	SOSs1 = [NonLin|SOSs2], IDCs1=IDCs2, NLs1=NLs2
	    ; NonLin = (_=>_), normalise_idc(NonLin, NormIDC) ->
	    	SOSs1=SOSs2, IDCs1 = [NormIDC|IDCs2], NLs1=NLs2
	    ; SOSs1=SOSs2, IDCs1=IDCs2, NLs1 = [NonLin|NLs2]
	    )
	),
	( NLs = [Example|_] ->
	    printf(error, "Unknown or nonlinear constraint in lp_add_constraints/3:%n%w%n", [Example]),
	    abort
	;
	    true
	),
	% preprocess the indicator constraints (may lead to new linear ones)
	(
	    foreach(IDC,IDCs3),
	    fromto(IDCs,IDCs4,IDCs5,[]),
	    fromto(NormCs3,NormCs2,NormCs1,NormCs0)
	do
	    preprocess_idc(IDC, IDCs4, IDCs5, NormCs2, NormCs1)
	),
	% preprocess the linear constraints
	(
	    foreach(Norm0, NormCs3),
	    fromto(NormCs,NC0,NC1,[]),
	    fromto(BoundCs,BC0,BC1,[])
	do
	    preprocess_norm_cstr(Norm0, NC0, NC1, BC0, BC1)
	),
        lp_add_normalised(Handle, NormCs, BoundCs, Ints, SOSs, IDCs, _, OldInts, ChangedCols),
	wake_solver_if_needed(Handle, NormCs, OldInts, ChangedCols).
lp_add_constraints(Handle, Cstr, Ints) :-
	error(5, lp_add_constraints(Handle, Cstr, Ints)).


integers(Xs, Pool) :-
        catch(extract_vars(Xs, -1.0Inf, 1.0Inf, yes, VarsTail, Vars), abort,
              error(5, integers(Xs, Pool))),
	store_integers(Vars, VarsTail, Pool).

reals(Xs, Pool) :-
        catch(extract_vars(Xs, -1.0Inf, 1.0Inf, no, VsTail, Vs), abort,
              error(5, reals(Xs, Pool))),
        get_pool_item(Pool, Handle),
        add_pool_vars(Handle, Pool, Vs, VsTail).


add_pool_vars(0, Pool, Xs, Tail) :- !, 
% solver instance not yet created
        get_typed_pool_constraints(Pool, reals of constraint_type, Tail),
        set_typed_pool_constraints(Pool, reals of constraint_type, Xs).
add_pool_vars(Handle, _Pool, Xs, []) :-
        lp_add_vars(Handle, Xs).

lp_add_vars(Handle, Xs) :-
        nonvar(Handle),
        Handle = prob{solver_id:SId, cplex_handle:CPH,option_vnames:VNames},
        filter_and_index_new_vars(Handle, Xs, VarList, _OldXs, OldCols, NAdded),
        setup_new_cols(Handle, [], [], [], [], OldCols, NAdded, 0),
        cplex_flush_new_rowcols(Handle, 0),
	% Technically, variables without constraints do not need to trigger
	% a solver, but it would be difficult to add the triggers later...
	lp_add_var_triggers(Handle, VarList, NAdded),
        load_varnames(VNames, NAdded, CPH, SId, VarList).


% Add bound constraints to new or old variables. Lo and Hi are floats.
lp_add_vars_interval(Handle, Xs, Lo, Hi) :-
        nonvar(Handle),
	Handle = prob{solver_id:SId, cplex_handle:CPH,option_vnames:VNames},
	filter_and_index_new_vars(Handle, Xs, VarList, OldVs, OldCols, NAdded),

	% Add new variables with bounds and names
	% (we could pass bounds right here, but would need lists)
	setup_new_cols(Handle, [], [], [], [], OldCols, NAdded, 0),
	% set bounds on new variables
	(
	    count(_,1,NAdded),
	    fromto(VarList,[X|Xs1],Xs1,_),
	    param(SId,CPH,Lo,Hi)
	do
	    get_unique_var_index(X, SId, J),
	    cplex_init_bound(CPH, J, (>=), Lo),
	    cplex_init_bound(CPH, J, (=<), Hi)
	),
	cplex_flush_new_rowcols(Handle, 0),
	lp_add_var_triggers(Handle, VarList, NAdded),
	load_varnames(VNames, NAdded, CPH, SId, VarList),

	% update intervals for old columns
	( foreach(X, OldVs), param(Handle, Lo, Hi) do
	    get_lp_attr(X, Handle, Attr),
	    Attr = eplex{idx: Is},
	    lp_impose_col_bounds(Handle, Attr, Is, Lo, Hi)
	),
	wake.


% Constraints with 1 or more variables and Integers are collected in 
% constraint pool(s). lp_pending/0 is used to indicate that there might
% be pending constraints in the pools.
% The auxiliary lp_pending_cleanup/1 delays on 'postponed' and kills
% itself and the lp_pending/0 when woken.


lp_pending.

set_lp_pending :-   % setup lp_pending suspension if needed
	get_lp_info(LPInfo),
	LPInfo = lp_info{pending:Pending},
	(is_suspension(Pending) -> 
	     true
	;
	     make_suspension(lp_pending, 12, NewPending),
	     setarg(pending of lp_info, LPInfo, NewPending),
	     suspend(lp_pending_cleanup(S), 11, trigger(postponed), S)
	).

:- demon lp_pending_cleanup/1.
lp_pending_cleanup(Susp) :-
	recorded(lp_pools, PoolName),
	\+ pool_is_empty(PoolName),
	!,
	% some constraints pending, kill only cleanup's suspension.
	kill_suspension(Susp).
lp_pending_cleanup(Susp) :-
	kill_suspension(Susp),
	get_lp_info(lp_info{pending:PSusp}), 
	( is_suspension(PSusp) -> kill_suspension(PSusp) ; true ).

:- local portray(lp_pending/0, tr_out/2, [goal]).
:- export tr_out/2.
tr_out(lp_pending, Goals) :-
	recorded_list(lp_pools, PoolNames),
	(foreach(Pool, PoolNames), fromto(GoalList, GsIn,GsOut, []) do
	     get_typed_pool_constraints(Pool, integers of constraint_type, Integers),
             get_typed_pool_constraints(Pool, reals of constraint_type, Reals),
	     get_typed_pool_constraints(Pool, linear of constraint_type, NormCstrs),
	     get_typed_pool_constraints(Pool, sos of constraint_type, SOSs),
	     get_typed_pool_constraints(Pool, idc of constraint_type, IDCs),
             ( Integers == [] -> GsIn=Gs0 ; GsIn=[Pool:integers(Integers)|Gs0]),
	     ( Reals == [] -> Gs0=Gs1 ; Gs0=[Pool:reals(Reals)|Gs1]),
	     ( foreach(SOS,SOSs), fromto(Gs1,[Goal|Goal1],Goal1,Gs2), param(Pool) do
		    Goal = Pool:SOS
	     ),
	     ( foreach(IDC,IDCs), fromto(Gs2,[Goal|Goal1],Goal1,Gs3), param(Pool) do
		    Goal = Pool:IDC
	     ),
	     ( foreach(NormCstr,NormCstrs), fromto(Gs3,[Goal|Goal1],Goal1,GsOut), param(Pool) do
		    denormalise_cstr(NormCstr, Cstr),
		    Goal = Pool:Cstr
	     )
	),
	(fromto(GoalList, [Goal|GL0],GL0, [LastGoal]), 
	 fromto(Goals, (Goal,Goals0),Goals0, LastGoal) do true
	).
		     

% collect variables declared by reals/1
collect_reals(Vars) :-
	collect_typed_pool_constraints(eplex, reals of constraint_type, Vars).

collect_integers(Integers) :-
	collect_typed_pool_constraints(eplex, integers of constraint_type, Integers).

store_integers(NewIntegers, Tail, Pool) :-
        (get_pool_handle(Handle, Pool) -> 
             % if there is a solver store the integer constraint
             Tail = [],
	     lp_add_normalised(Handle, [], [], NewIntegers, [], [], _, OldInts, _BdChgs),

             Handle = prob{suspension:Susp, nc_trigger: NCTrigger},
	     % wake demon if a old variable is made integer
	     % and new_constraint trigger condition is defined
	     ( OldInts \== [], NCTrigger == yes ->
		    schedule_suspensions(1, s([Susp])),
		    wake
	     ;
		    true
	     )
        ;
	     set_lp_pending,
             get_typed_pool_constraints(Pool, integers of constraint_type, OldIntegers),
             set_typed_pool_constraints(Pool, integers of constraint_type, NewIntegers),
             Tail = OldIntegers
        ).

collect_lp_constraints_norm(CstrsNorm) :-
	collect_typed_pool_constraints(eplex, linear of constraint_type, CstrsNorm).



% ----------------------------------------------------------------------
% The black-box solver
% ----------------------------------------------------------------------

optimize_body(OptExpr, ObjVal, Caller) :-
	optimize_body(OptExpr, [], ObjVal, Caller).

optimize_body(OptExpr, Options, ObjVal, Caller) :-
	get_pool_item(eplex, 0),	% may fail
	!,
	collect_lp_constraints_norm(Cstr),
	collect_integers(Ints),
	lp_setup_body(Cstr, OptExpr, [integers(Ints)|Options], Handle, Caller),
%	lp_write(Handle, lp, 'eplex_lastprob'),
	lp_solve(Handle, ObjVal),
	lp_get(Handle, vars, VArr),
	lp_get(Handle, typed_solution, SolutionVector),
	VArr = SolutionVector,			% do the bindings
	lp_cleanup(Handle).
optimize_body(OptExpr, _Options, ObjVal, _) :-
	printf(error, "Eplex error: instance 'eplex' already has an associated solver in %w%n",
	    [optimize(OptExpr, ObjVal)]),
	abort.

/*
% for the manual:
optimize(OptExpr, ObjVal) :-
	eplex:eplex_solver_setup(OptExpr),
	eplex:eplex_solve(ObjVal),
	eplex:eplex_get(vars, VArr),
	eplex:eplex_get(typed_solution, SolutionVector),
	VArr = SolutionVector,			% do the bindings
	eplex:eplex_cleanup.
*/

% ----------------------------------------------------------------------
% Invoking the LP-solver as a demon
% ----------------------------------------------------------------------
:- local struct(demon_opts(collect_from,initial_solve,priority)).


:- tool(eplex_solver_setup_cbody/6, eplex_solver_setup_body/7). % compatibility
eplex_solver_setup_body(OptExpr, Cost, Options, Prio, TriggerModes, Pool, CallerModule) :-
        eplex_solver_setup_body(OptExpr, Cost, [priority(Prio)|Options],
                                TriggerModes, Pool, CallerModule).


:- tool(eplex_solver_setup_cbody/5, eplex_solver_setup_body/6).
eplex_solver_setup_body(OptExpr, Cost, Options, TriggerModes, Pool, CallerModule) :-
	lp_demon_setup_body(OptExpr, Cost, [collect_from(pool(Pool))|Options], TriggerModes, _Handle, CallerModule).

:- tool(eplex_solver_setup/2, eplex_solver_setup/3).
eplex_solver_setup(OptExpr, Pool, CallerModule) :-
	lp_demon_setup_body(OptExpr, _Cost, [priority(0),collect_from(pool(Pool))], [], _Handle, CallerModule).


% compatibility 
lp_demon_setup_body(OptExpr, Cost, Options, Prio, TriggerModes, Handle, CallerModule) :-
        lp_demon_setup_body(OptExpr, Cost, [priority(Prio)|Options], 
                            TriggerModes, Handle, CallerModule).

lp_demon_setup_body(OptExpr, Cost, Options0, TriggerModes, Handle, CallerModule) :-
	set_default_demon_options(DemonOptions, TriggerModes),
	clean_options(Options0, Options, DemonOptions),
	DemonOptions = demon_opts{collect_from:CollectCstrs,priority:Prio,
					initial_solve:InitSolve},
	(CollectCstrs = pool(Pool) ->
	     % Pool should not already have an associated solver
             pool_has_no_solver(Pool),
	     collect_typed_pool_constraints(Pool, linear of constraint_type, Cstr),
	     collect_typed_pool_constraints(Pool, integers of constraint_type, Ints),
             collect_typed_pool_constraints(Pool, reals of constraint_type, PVars),
             collect_typed_pool_constraints(Pool, sos of constraint_type, SOSs),
             collect_typed_pool_constraints(Pool, idc of constraint_type, IDCs)
        ;
	     Ints = [], Cstr = [], PVars = [], SOSs = [], IDCs = []
	),
	append(SOSs, Options, SOSsOptions),
	lp_setup_body(Cstr, OptExpr, [reals(PVars),integers(Ints)|SOSsOptions], 
                      Handle, CallerModule),
	% indicator constraints not accepted by lp_setup, add now
	lp_add_constraints(Handle, IDCs, []),

        % associate solver with pool (must be done after lp_setup)
	(CollectCstrs = pool(Pool) -> lp_pool_associate_solver(Pool, Handle) ; true),

        % rearranged code so that various initialisations are still done
        % with TriggerModes = [] as PostGoal must be initialised for
        % any initial solving 
        Handle = prob{vars:VList,suspension:Susp},
        (TriggerModes \== [] ->
             (cannot_impose_bound(Cost) ->
                  event(lp_obj_nobounds_warning)
             ;
                  true
             ),
                  
             make_suspension(lp_demon(Handle, Cost, PreGoal, PostGoal, CallerModule), Prio, Susp)
        ;
             % No trigger modes, no need to create the demon!
             true
        ),

        ( global_triggers(TriggerModes, Handle, Susp, PreGoal, PostGoal, [], VarTriggerModes),	
          var_triggers(VarTriggerModes, Handle, Susp, VList)
        ->
            true
        ;
            printf(error, "Eplex error: invalid trigger conditions"
                   " specified while setting up problem:%w%n", [TriggerModes]),
            abort
        ),
        setarg(triggermodes of prob, Handle, VarTriggerModes),
        
	!,
	(InitSolve == yes ->
	     lp_solve(Handle, _LpCost),
             post_lp_demon_solve(Handle, Cost, PostGoal, CallerModule)
	;
	     true
	).


    :- mode global_triggers(?,+,+,-,-,+,-).
    global_triggers(X, _, _, _, _, _, _) :- var(X), !, fail.
    global_triggers([], _, _, Pre, Post, VTM, VTM) :- !,
    	( Pre=true -> true ; true ),
    	( Post=true -> true ; true ).
    global_triggers([TM|TMs], H, S, Pre, Post, VTMs0, VTMs) :- !,
	global_triggers(TM, H, S, Pre, Post, VTMs0, VTMs1),
	global_triggers(TMs, H, S, Pre, Post, VTMs1, VTMs).
    global_triggers(new_constraint, H, _, _, _, VTM, VTM) :- !,
    	setarg(nc_trigger of prob, H, yes).
    global_triggers(pre(Goal), _, _, Goal, _, VTM, VTM) :- !.
    global_triggers(post(Goal), _, _, _, Goal, VTM, VTM) :- !.
    global_triggers(trigger(Atom), _, S, _, _, VTM, VTM) :- !,
	attach_suspensions(Atom, S).
    global_triggers(suspension(S), _, S, _, _, VTM, VTM) :- !.
    global_triggers(VTM, _, _, _, _, VTMs, [VTM|VTMs]).

    var_triggers([], _H, _S, _Vars) ?- true.
    var_triggers([TM|TMs], H, S, Vars) ?-
	var_triggers(TM, H, S, Vars),
	var_triggers(TMs, H, S, Vars).
    var_triggers(inst, _H, S, Vars) ?- 
	insert_suspension(Vars, S, inst of suspend, suspend).
    var_triggers(deviating_inst, H, S, Vars) ?-
	(foreach(V, Vars), param(S, H) do
	     (var(V) ->
		  get_lp_attr(V, H, Attr),
		  enter_suspension_list(intol_inst of eplex, Attr, S)
	     ;    true
	     )
	).
    var_triggers(bounds, Handle, _S, _Vars) ?-
        setarg(bd_trigger of prob, Handle, bounds).
    var_triggers(deviating_bounds, Handle, _S, _Vars) ?-
        setarg(bd_trigger of prob, Handle, deviating_bounds).
    var_triggers(Module:Field, _H, S, Vars) ?-
	( atom(Field) ->
	    sepia_kernel:tr_of(no_macro_expansion(Field of Module), Index, Module)
	;
	    integer(Field), Index = Field
	),
	insert_suspension(Vars, S, Index, Module).

    set_default_demon_options(demon_opts{collect_from:pool(eplex),
    		initial_solve:InitialSolve, priority:0}, TriggerModes) :-
    	( TriggerModes == [] -> InitialSolve = no ; InitialSolve = yes ).

:- demon(lp_demon/5).
:- set_flag(lp_demon/5, priority, 5).
lp_demon(Handle, Cost, PreGoal, PostGoal, CallerModule) :-
	( call(PreGoal)@CallerModule ->
	    lp_solve(Handle, _LpCost),
            post_lp_demon_solve(Handle, Cost, PostGoal, CallerModule)
	;
	    true
	).

post_lp_demon_solve(Handle, Cost, PostGoal, CallerModule) :-
        call(PostGoal)@CallerModule,
        arg(bestbound of prob, Handle, BestBound),
        % bestbound is always a valid bestbound on Cost 
        ( lp_get(Handle, sense, min) ->
            LpBound is BestBound - lp_get(Handle, optimizer_param(feasibility_tol)),
              
            set_var_bounds(Cost, LpBound, 1.0Inf)
        ;
            LpBound is BestBound + lp_get(Handle, optimizer_param(feasibility_tol)),
            set_var_bounds(Cost, -1.0Inf, LpBound)
        ),
        wake.


lp_obj_nobounds_warning :- 
        writeln(warning_output, "Warning: Setting up an lp_demon with a cost variable that cannot"),
        writeln(warning_output, "have bounds imposed. Consider using a domain variable.").

% succeed if the argument cannot have a bound imposed (from branch_and_bound)
cannot_impose_bound(X) :- free(X).
cannot_impose_bound(X) :- meta(X),
	get_var_bounds(X, L, H),
	H =:= 1.0Inf,
	L =:= -1.0Inf,
	% try imposing a bound and check if it worked
	call_priority((
		set_var_bounds(X, L, 0),
		get_var_bounds(X, _, H1),
		H1 =:= 1.0Inf
	    ), 1).





instantiation_deviates(Handle) :-
	Handle = prob{vars:VList, sols:OldSolution,
				demon_tol_real:RT, demon_tol_int:IT},
	not tolerable_inst(VList, OldSolution, _, RT, IT).

    tolerable_inst([], _, N, _, _) :- !, N = 0.
    tolerable_inst([X|Xs], SArr, N1, RT, IT) :-
        tolerable_inst(Xs, SArr, N, RT, IT),
        N1 is N + 1,
	get_darray_element(SArr, N, S),
	tolerable_inst(X, S, RT, IT).

    tolerable_inst(X, S, _, IT) :-
	integer(X),
	X-IT =< S, S =< X+IT.
    tolerable_inst(X, S, RT, _) :-
	real(X),
	X-RT =< S, S =< X+RT.
    tolerable_inst(X, _S, _RT, _IT) :-
	var(X).


solution_out_of_range(Handle) :-
	Handle = prob{vars:VList, sols:OldSolution,
                      cplex_handle:CPH, 
                      demon_tol_real:RT, demon_tol_int:IT},
	not tolerable_range(VList, OldSolution, CPH, Handle, _, RT, IT).


tolerable_range([], _, _, _, N, _, _) :- !, cplex_matrix_base(N).
tolerable_range([X|Xs], SArr, CPH, Handle, N1, RT, IT) :-
        tolerable_range(Xs, SArr, CPH, Handle, N, RT, IT),
        % cannot rely on getting col. no. from X as it may be non-var 
	get_darray_element(SArr, N, S),
        lp_var_get_bounds(Handle, X, Min, Max),
        N1 is N + 1,
	tolerable_range_var(S, Min, Max, CPH, N, RT, IT).


% Bounds Min, Max supplied to allow caller of tolerable_range_var to chose
% how bounds are obtained
tolerable_range_var(S, Min, Max, CPH, I, RT, IT) :-
	cplex_get_col_type(CPH, I, TC),
        cplex_type_code(T, TC),
	( T = integer ->
	    Min-IT =< S, S =< Max+IT
	;
	    Min-RT =< S, S =< Max+RT
	).


% If Handle has a delayed solver, check 4 waking conditions:
% - We have the 'new_constraint' trigger (nc_trigger=yes) and
%   not all constraints are satisfied with their current solutions.
% - We have the 'new_constraint' trigger (nc_trigger=yes) and
%   any variable had its type changed (integrality imposed)
% - We have the 'bounds' trigger and any bound changed
% - We have the 'deviating_bounds' trigger and any bound change
%   excluded the current solution
wake_solver_if_needed(Handle, NormCstrs, TypeChangedVars, ChangedCols) :-
	Handle = prob{suspension:Susp, nc_trigger:NcTrigger},
	(
	    is_suspension(Susp),
	    (
		NcTrigger == yes,
		TypeChangedVars = [_|_]
	    ;
		NcTrigger == yes,
		member(Norm, NormCstrs),
		\+satisfied_now(Norm, Handle)
	    ;
		member(I, ChangedCols),
		bound_change_requires_scheduling(Handle, I)
	    )
	->
	    schedule_suspensions(1, s([Susp])),
	    wake
	;
	    true
	).


% ----------------------------------------------------------------------
% Preprocessing - treat the trivial constraints:
% Check the ground ones, turn single-variable constraints into bound updates.
% ----------------------------------------------------------------------


preprocess_norm_cstr(NormIn, Linears, Linears0, Bounds, Bounds0) :-
	NormIn = Sense:[Cst*_|Lhs],
	( Lhs = [] ->			        % ground: check immediately
	    satisfied(Sense, Cst),
	    Bounds = Bounds0,
	    Linears = Linears0
	; Lhs = [_C*_X] ->			% single var
	    Bounds = [NormIn|Bounds0],
	    Linears = Linears0
	;				
	    Linears = [NormIn|Linears0],
	    Bounds = Bounds0
	).

try_ground_check(NormIn, NormOut) :-
	NormIn = Sense:[Cst*_|Lhs],
	( Lhs = [] ->			        % ground: check immediately
	    satisfied(Sense, Cst)
	;				
	    NormIn = NormOut
	).


% Filter out degenerate indicator constraints (Bool 0 or 1)
% We do not further check the Bool variable here: must be done later.
% May fail if Bool instantiated, but not to 0 or 1 (of any numerical type)
preprocess_idc(IDC, IDCs, IDCs0, NormCs, NormCs0) :-
	IDC = idc(Cpl,BoolExpr,LinNorm),
	( var(BoolExpr) ->
	    IDCs = [IDC|IDCs0], NormCs = NormCs0
	;
	    Bool is BoolExpr,		% evaluate subscripts etc
	    ( var(Bool) ->
		IDCs = [idc(Cpl,Bool,LinNorm)|IDCs0], NormCs = NormCs0
	    ; Bool =:= Cpl ->		% constraint is void
		IDCs = IDCs0, NormCs = NormCs0
	    ; Bool =:= 1-Cpl ->		% degenerates to plain linear
		IDCs = IDCs0, NormCs = [LinNorm|NormCs0]
	    ;
		fail
	    )
	).


    renormalise_and_check_simple([], [], []).
    renormalise_and_check_simple([Sense:Expr|Cstrs], RemCstrs, BdCstrs) :-
	linrenorm(Expr, NormExpr0),
        preprocess_norm_cstr(Sense:NormExpr0, RemCstrs, RemCstrs1, BdCstrs, BdCstrs1), % may fail
	renormalise_and_check_simple(Cstrs, RemCstrs1, BdCstrs1).


% Process a list of normalised constraints and turn them into matrix
% coefficients (filled into the Cols array) and a list of the right hand sides.
% Bounds (single var) constraints are extracted into a separate list.
% Ground constraints are checked for consistency and may lead to failure.
% We also count the true constraints (rows) and the nonzero coefficients.

%process_constraints(+Cstrs, -Rhss, -Bs, +SId, +RowNr0, -RowNr, +NZ0, -NZ, +Colz)
process_constraints([], [], [], _, Rows, Rows, NonZeros, NonZeros, _).
process_constraints([Cstr|Cstrs], Rhss, Bs, SId, Rows0, Rows, N0, N, Cols) :-
	Cstr = Sense:[Cst*_|Lhs],
	Rhs is -Cst,
	( Lhs = [] ->  % ground: check immediately
              Rows1 = Rows0, 
              N1 = N0,
              Bs1 = Bs,
              Rhss1 = Rhss,
              satisfied(Sense, Cst)
        ; Lhs = [_] ->	% collect bounds constraints for later
              Rows1 = Rows0, 
              N1 = N0,
              Rhss = Rhss1,
	      Bs = [Cstr|Bs1]
	;
              Rows1 is Rows0+1,
              add_coeffs(Lhs, Cols, SId, Rows0, N0, N1),
              %	writeln(Lhs-Sense-Rhs)
              Rhss = [Sense:Rhs|Rhss1],
              Bs1 = Bs
        ),
	process_constraints(Cstrs, Rhss1, Bs1, SId, Rows1, Rows, N1, N, Cols).

    satisfied((=<), C) :- C =< 0.
    satisfied((>=), C) :- C >= 0.
    satisfied((=:=), C) :- C =:= 0.

    swap_sense(C, (=<), (>=)) :- C < 0, !.
    swap_sense(C, (>=), (=<)) :- C < 0, !.
    swap_sense(_, S, S).


    add_coeffs([], _, _, _, N, N).
    add_coeffs([C*X|More], Cols, SId, Row, N0, N) :-
        % if there are merged cols, the coeff C should be put on
        % *one* of the columns only
        get_first_var_index(X, SId, J),
	J1 is J+cplex_matrix_offset,
	arg(J1, Cols, Col0),
	( var(Col0) -> arg(J1, Cols, [Row:C])
	; setarg(J1, Cols, [Row:C|Col0]) ),
	N1 is N0+1,
	add_coeffs(More, Cols, SId, Row, N1, N).


satisfied_now(Sense:Lhs, Handle) :-
	eval_lhs(0.0, Handle, Lhs, ValLhs),
	satisfied(Sense, ValLhs).

    eval_lhs(Cst,_,[],Cst).
    eval_lhs(Cst0,H,[C*V|Vs],N) :-
	lp_var_solution(H, V, Val),    % can fail if V has no attribute
	Cst is Cst0 + C * Val,
	eval_lhs(Cst,H,Vs,N).

% ----------------------------------------------------------------------
% Setup the LP solver
%
% lp_setup - All the constraints are passed explicitly in the list Cstr.
%	Also, the variables may have bounds already. The constraints are
%	first simplified and converted into coefficients (and possibly
%	bound updates). This data is then copied into cplex arrays.
%
%	Accepts normalised linear constraints, and option integers(Ints).
%	For backward compatibility, it also accepts sos[12](Xs) options.
%	We don't accept indicator constraints, they must be added later.
% ----------------------------------------------------------------------

:- pragma(noexpand).	% because of compiler bug

lp_setup_body(Cstr, OptExpr, Options, Handle, CallerModule) :-
	( getval(licence_data, none) ->
            writeln(error, "Eplex error: cannot use solver: no license"
                    " information available."),
            abort
        ; var(OptExpr) -> error(4, lp_setup(Cstr, OptExpr, Options, Handle))
	; cplex_objsense(OptExpr, Sense, Expr) -> true
	; printf(error, "Eplex error: optimisation direction not specified "
                 "for objective.%n Should be min(%w) or max(%w)%n",
                 [OptExpr,OptExpr]),
          error(5, lp_setup(Cstr, OptExpr, Options, Handle))
        ),

        Handle = prob{vars:Vars, ints:Ints, obj:ObjConst, objsense:Sense, 
                      linobj:LinObjFunct, quadobj:QuadObjFunct,
                      solver_id:SId, caller_module:CallerModule,
                      mr:0, sols:[]},
        TempData = temp_prob{sos:[],extra_vars:[],use_copy:1},
        % Initialise some fields in the handle...
	new_solver_id(SId), 
	init_suspension_list(aux_susps of prob, Handle),

	catch(process_options(Options, Handle, TempData), % read options
               abort, error(6, lp_setup(Cstr, OptExpr, Options, Handle)) 
        ),
        fill_in_defaults(Handle),

	renormalise_cstrs(Cstr, CstrNorm),
	quadnorm(Expr, ObjConst, LinObjFunct, QuadObjFunct),	% After propagate!
        arg(extra_vars of temp_prob, TempData, ExtraVars),
        % Ints extracted from Options: could contain vars not in Cstr
        % use the *ObjFunct instead of Expr to avoid large structres
        % which may crash term_variables/2 
	term_variables([ExtraVars,Ints,LinObjFunct,QuadObjFunct|CstrNorm], Vars),

        % setup change_suspension list
        init_suspension_list(change_suspensions of prob, Handle),
	low_level_setup(Handle, TempData, CstrNorm).
        


new_solver_id(SId) :-
	get_lp_info(LPInfo),
	LPInfo = lp_info{newid:SId},
	SId1 is SId + 1,
	setarg(newid of lp_info, LPInfo, SId1).


get_lp_info(LPInfo) :-
	getval(lp_info, LPInfo0),
	(LPInfo0 = lp_info{} -> 
	     LPInfo0 = LPInfo 
        ; 
             LPInfo = lp_info{newid:0,pending:none},
             setval(lp_info, LPInfo)
	).
 
:- pragma(expand).	% because of compiler bug


% Low-level solver setup

low_level_setup(Handle, TempData, CstrNorm) :-
	Handle = prob{  vars:VarList,	        % in
			ints:Ints,		% in
			linobj:LinObjFunct,	% in
			quadobj:QuadObjFunct,	% in
			objsense:Sense,		% in
			presolve:PreSolve,	% in
                        option_vnames:VNames,   % in 
			solver_id:SId,          % in
			objcoeffs:ObjCoeffs,		% out
			qobjcoeffs:QuadObjCoeffs	% out
		},
        TempData = temp_prob{use_copy:UseCopy,sos:SOSs},
                                
        % convert constraints into matrix coefficients and rhs
        cplex_matrix_base(MBase), % starting column number
        set_var_indices(VarList, Handle, MBase, _, Cols),
        functor(ColCoeffs, '', Cols),
        process_constraints(CstrNorm, Rhs, BdCstrs, SId, MBase, Rows, 0, NonZeros, ColCoeffs),
        get_flag(tmp_dir, TDir),
        os_file_name(TDir,OSTDir),
        % solver setup, will cleanup on failure
        cplex_prob_init(PreSolve, UseCopy, Rows, Cols, NonZeros, OSTDir,
                        Sense, Handle),
        arg(cplex_handle of prob, Handle, CPH),	% after cplex_prob_init
	set_initial_bounds(CPH, SId, BdCstrs),	% may fail
        obj_coeffs(LinObjFunct, SId, ObjCoeffs),
        set_obj_coeffs(CPH, ObjCoeffs),
        qobj_coeffs(QuadObjFunct, SId, QuadObjCoeffs),
        set_qobj_coeffs(CPH, QuadObjCoeffs),
        set_rhs(CPH, Rhs, 0),
        set_type_integer(CPH, SId, Ints),	% may change prob type to MIP
        set_mat(CPH, ColCoeffs, 0, 0, Cols),
        set_sos(Handle, SOSs),			% may change prob type to MIP
        cplex_loadprob(CPH),
        load_varnames(VNames, Cols, CPH, SId, VarList).


% ----------------------------------------------------------------------
% Adding constraints and variables
% ----------------------------------------------------------------------

lp_add(Handle, Cstr, Integers) :-
        (var(Handle) ; var(Cstr) ; var(Integers)),  !, 
        error(4, lp_add(Handle, Cstr, Integers)).
lp_add(Handle, Cstr, Integers) :-
	renormalise_and_check_simple(Cstr, CstrNorm, BdCstrs),
        lp_add_normalised(Handle, CstrNorm, BdCstrs, Integers, [], [], _RowIdxs, _TypeChgs, _BdChgs).
	% don't wake here (as documented - but why?)


lp_add_var_triggers(Handle, AllVarsNewFirst, NAdded) :-
	nonvar(Handle), Handle = prob{suspension:Susp, triggermodes:VarTriggerModes},
	( var(Susp) ->
	    true
	; VarTriggerModes = [] ->
	    true
	;
	    ( count(_,1,NAdded), fromto(AllVarsNewFirst,[X|Xs],Xs,_), foreach(X,NewVars) do
		true
	    ),
	    var_triggers(VarTriggerModes, Handle, Susp, NewVars)
	).


% This is the core predicate for adding constraints.
% It will add CstrNorm as proper rows (without further checking
% or simplification, even when they are ground or single-variable).
% The single-variable BdCstrs will be added as bound updates.
% Integers will be added as integrality constraints.
% All of {CstrNorm,BdCstrs,Integers} may contain new variables.
% Returns indices of added rows, new variables, and a list of
% old variables that had integrality imposed.
% No waking is done here (but the necessary information is returned).
% New variables get row indices, but no suspensions.

% lp_add_normalised(+Handle,+CstrNorm,+BdCstrs,+Integers,+SOSs,+IDCs,-RowIdxs,-TypeChanges,-ChangedCols)
lp_add_normalised(Handle, CstrNorm, BdCstrs, Integers, SOSs, IDCs, RowIdxs, TypeChanges, ChangedCols) :-
	Handle = prob{cplex_handle:CPH, solver_id:SId, 
		     ints:ExistingInts,option_vnames:VNames},

	% Split single-var constraints into new and old var ones
	% this must be done before new vars are given indices!
	filter_new_vars(SId, Integers, NewIntegers, TypeChanges),
	filter_new_vars_bc(SId, BdCstrs, NewBCs, OldBCs),
	% make a list that contains at least all new variables
        term_variables([](CstrNorm,NewBCs,NewIntegers,SOSs,IDCs), Vars),
	% give indices to the new variables (OldCols..OldCols+AddedCols-1)
	% Varlist will start with AddedCols new variables
	filter_and_index_new_vars(Handle, Vars, VarList, _, OldCols, AddedCols),

	% destroy saved basis, if necessary
	( AddedCols == 0, CstrNorm == [] -> true ;
	    clear_result(Handle, cbase of prob),
	    clear_result(Handle, rbase of prob)
	),

	% allocate and initialise buffer arrays (for columns)
	setup_new_cols(Handle, [], [], [], [], OldCols, AddedCols, 0),
	% set bounds on new variables
	set_initial_bounds(CPH, SId, NewBCs),	% may fail

	% update bounds on old variables
	update_bounds(Handle, OldBCs, ChangedCols),	% may fail

	( Integers == [] -> true ;
	    % we need to change problem type first before adding
	    % integers; as cplex_init_type/3 also sets the
	    % problem type to MIP (non-backtrackably)
	    cplex_change_lp_to_mip(Handle),	% backtrackable

	    % set integer type on new variables
	    set_type_integer(CPH, SId, NewIntegers),

	    % change old variables to integers
	   (foreach(Old, TypeChanges), param(Handle,CPH,SId),
	    % OldIntegers1: existing prob. vars. which were not
	    % of type integer already
	    fromto([], OldInts0,OldInts1, OldIntegers1) do
		get_var_index(Old, SId, Is),	% must exist
		Is = [FirstI|_],
		cplex_get_col_type(CPH, FirstI, OldTypeCode),
		cplex_get_col_bounds(CPH, FirstI, Lo, Hi),
		cplex_type_code(OldType, OldTypeCode),
		(OldType \== integer -> 
		     OldInts1 = [Old|OldInts0],
		     type_to_cplex_type(integer, Lo, Hi, TypeCode),
		     (foreach(I, Is), param(Handle,TypeCode) do
			 cplex_change_col_type(Handle, I, TypeCode)	% backtrackable
		     )
		;
		     OldInts1 = OldInts0 
		)
	   ),
	   append(NewIntegers, ExistingInts, AllIntegers0),
	   append(OldIntegers1, AllIntegers0, AllIntegers),
	   setarg(ints of prob, Handle, AllIntegers)
	),

	% new constraints and number of new variables
	setup_new_rows(CPH, SId, 0, _, CstrNorm, RowIdxs),
	cplex_flush_new_rowcols(Handle, 0),

	% flush SOSs after new columns
	( SOSs == [] -> true ;
	    cplex_change_lp_to_mip(Handle),	% backtrackable
	    set_sos(Handle, SOSs),
	    cplex_flush_sos(Handle)
	),
	( IDCs == [] -> true ;
	    cplex_change_lp_to_mip(Handle),	% backtrackable
	    setup_new_idcs(Handle, IDCs),
	    cplex_flush_idcs(Handle)
	),
	load_varnames(VNames, AddedCols, CPH, SId, VarList),
	lp_add_var_triggers(Handle, VarList, AddedCols).


    % Split var list into new and old (known by solver SId) variables
    :- mode filter_new_vars(+,+,-,-).
    filter_new_vars(_, [], [], []).
    filter_new_vars(SId, [X|Xs], NewXs, OldXs) :-
	( get_var_index(X, SId, _) ->
	    OldXs = [X|OldXs0],
	    filter_new_vars(SId, Xs, NewXs, OldXs0)
	;
	    NewXs = [X|NewXs0],
	    filter_new_vars(SId, Xs, NewXs0, OldXs)
	).

    % Split bound-constraint list into new and old (known by solver SId)
    :- mode filter_new_vars_bc(+,+,-,-).
    filter_new_vars_bc(_, [], [], []).
    filter_new_vars_bc(SId, [BC|BCs], NewBCs, OldBCs) ?-
	BC = _Sense:[_Rhs,_C*X],
	( get_var_index(X, SId, _) ->
	    OldBCs = [BC|OldBCs0],
	    filter_new_vars_bc(SId, BCs, NewBCs, OldBCs0)
	;
	    NewBCs = [BC|NewBCs0],
	    filter_new_vars_bc(SId, BCs, NewBCs0, OldBCs)
	).

% filter_and_index_new_vars(+Handle, +Xs, -NewVarList, -OldXs, -Index, -NAdded)
%   separate the variables in Xs into new and old variables (OldXs). Index
%   the new variables and add them (in reverse order) to the Handle's 
%   variable list (NewVarList). NAdded is the number of added variables;
%   Index is the start index of the added variables (original number of cols)
filter_and_index_new_vars(Handle, Xs, VarList, OldXs, I0, NAdded) :-
        Handle = prob{vars:VarList0, cplex_handle:CPH, solver_id:SId},
        cplex_get_prob_param(CPH, 1, I0),
        ( foreach(X, Xs), fromto(I0, I1,I2, I),
          fromto(VarList0, NewXs0,NewXs1, VarList),
          fromto(OldXs, OldXs1,OldXs0, []),
          param(SId), param(Handle) do
              ( get_var_index(X, SId, _) ->
                    % old variable
                    NewXs0 = NewXs1,
                    I1 = I2,
                    OldXs1 = [X|OldXs0]
              ; var(X) ->
                    % new variable
                    set_var_index(X, Handle, I1), 
                    NewXs1 = [X|NewXs0],
                    I2 is I1 + 1,
                    OldXs1 = OldXs0
              ; number(X),
                    % ignore
                    NewXs0 = NewXs1,
                    I1 = I2,
                    OldXs1 = OldXs0
              )
        ),
        NAdded is I - I0,
        setarg(vars of prob, Handle, VarList).

% setup_new_rows(+CPH, +SId, +CType, +MaxIdx, +CstrNorm, -RowIdxs)
% setup new constraints (rows in matrix) of type CType (normal,
% unconditional + conditional cutpools) and return their row indices
% MaxIdx is used to test if the variables are `original' problem variables
% (for cutpools only)
setup_new_rows(_CPH, _SId, _CType, _MaxIdx, [], []).
setup_new_rows(CPH, SId, CType, MaxIdx, [Cstr|Cstrs], [Idx|Idxs]) :-
	Cstr = Sense:[Cst*_|Lhs],
	Rhs is -Cst,
	cplex_new_row(CPH, Sense, Rhs, CType, Idx0),
        rawidx_cstridx(CType, Idx0, Idx),
	add_row_coeffs(CPH, SId, CType, MaxIdx, Lhs),
	setup_new_rows(CPH, SId, CType, MaxIdx, Cstrs, Idxs).

    add_row_coeffs(_CPH, _SId, _CType, _MaxIdx, []).
    add_row_coeffs(CPH, SId, CType, MaxIdx, [C*X|CXs]) :-
        get_valid_var_index(CType, MaxIdx, X, SId, J),
	cplex_add_coeff(CPH, J, C, CType),
	add_row_coeffs(CPH, SId, CType, MaxIdx, CXs).

    get_valid_var_index(0, _, X, SId, J) :- !, /* normal constraints */
        % if there are merged cols, the coeff C should be put on
        % *one* of the columns only
	get_first_var_index(X, SId, J).
    get_valid_var_index(_CType, MaxIdx, X, SId, J) :- /* global cut pool */
        get_var_index(X, SId, Js),
        % fails if X is not an original problem variable -- need to check
        % all merged column indices. Return in J a valid original index
        check_original_index(Js, MaxIdx, J).

    check_original_index([J|Js], MaxIdx, J0) ?-
        /* < MaxIdx: MaxIdx is actual maximum used index + 1 */
        (J < MaxIdx -> J0 = J ; check_original_index(Js, MaxIdx, J0)).


setup_new_idcs(Handle, IDCs) :-
	Handle = prob{cplex_handle:CPH,solver_id:SId},
	% Set up indcator row data (similar to setup_new_rows)
	( foreach(idc(Cpl,Bool,Cstr),IDCs), param(CPH,SId) do
	    Cstr = Sense:[Cst*_|Lhs],
	    Rhs is -Cst,
	    CType = 0,
	    get_first_var_index(Bool, SId, J),
	    cplex_new_row_idc(CPH, Sense, Rhs, Cpl, J),
	    add_row_coeffs(CPH, SId, CType, _MaxIdx, Lhs)
	).


% added by AE 25/10/02
% this is for adding rows whose index in the external
% solver we want to know for sure - when we get duals
% in colgen we really have to know we are getting the right
% ones associated with the sp cost function vars

lp_add_indexed(Handle, Cstr, Ints, Indices) :-
        (var(Handle) ; var(Cstr) ; var(Ints)), !,
        error(4, lp_add_indexed(Handle, Cstr, Ints, Indices)).
lp_add_indexed(Handle, Cstr, Ints, Indices) :-
	lp_add_normalised(Handle, Cstr, [], Ints, [], [], Indices, _, _).


/*  Added by AE 22/10/02
 *  VarCols should now be a list of [Var:Col]s, where Col is a list of 
 *  RowIdx:Value pairs. For the first element, RowIdx can be obj (objective)
 */
lp_add_columns(Handle, VarCols) :-
        ((nonvar(Handle), nonvar(VarCols)) -> true 
        ; error(4, lp_add_columns(Handle, VarCols))),
        Handle = prob{cbase:CBase,
                      cplex_handle:CPH, solver_id:SId,
                      vars:Vars0, objcoeffs:ObjCoeffs0, option_vnames:VNames,
                      pool: Pool, suspension:S},
        ((is_suspension(S) ; nonvar(Pool)) ->
             printf(error, "Eplex error: problem cannot be modified by %w "
                    "(has trigger conditions or is an eplex instance).%n",
                    [lp_add_columns(Handle, VarCols)]),
             abort
        ;
             true
        ),
        cplex_get_prob_param(CPH, 1,NCols0),
        (
            foreach(Var:Col0, VarCols),
	    foreach(Obj, NewObjCoeffs),
            foreach(Lo, NewLos),
            foreach(Hi, NewHis),
	    foreach(Col, NewColCoeffs),
            fromto(Vars0, Vs,[Var|Vs], Vars1),
            fromto(ObjCoeffs0, OCs0, OCs1, ObjCoeffs),
            fromto(0, NZ0,NZ1, NonZeros),
            fromto(NCols0, I0,I1, NCols),
            param(SId, Handle, VarCols)
        do
            ( get_var_index(Var, SId, _) ->
                  printf(error, "Eplex error: adding existing variable %w"
                         " in%n", [Var]),
                  error(5, lp_add_columns(Handle, VarCols))
            ;
                  (Col0 = [obj:Obj|Col1] -> true ; Obj = 0.0, Col0 = Col1),
                  (Col1 = [lo:Lo, hi:Hi|Col] -> true ; Lo = -1.0Inf, Hi = 1.0Inf, Col1 = Col ),
                  set_var_index(Var, Handle, I0),
                  (Obj =\= 0 -> OCs1 = [I0:Obj|OCs0] ; OCs1 = OCs0),
                  I1 is I0 + 1,
                  NZ1 is NZ0 + length(Col)
            )
        ),
        AddedCols is NCols - NCols0,
        setarg(vars of prob, Handle, Vars1),
        setarg(objcoeffs of prob, Handle, ObjCoeffs),
        setup_new_cols(Handle, NewObjCoeffs, NewLos, NewHis, NewColCoeffs, NCols0, AddedCols, NonZeros),
        cplex_flush_new_rowcols(Handle, 1),
        load_varnames(VNames, AddedCols, CPH, SId, Vars1),
        ( array(CBase) ->
              extend_array(CBase, AddedCols, NewLos, NewHis, NewCBase),
              setarg(cbase of prob, Handle, NewCBase)
        ;
              true
        ).

        
% setup_new_cols(+Handle, +NewObjCoeffs, +NewLos, +NewHis, +NewColCoeffs, 
%                +OldCols, +AddedCols, +NonZeros)
%  setups the buffer arrays for the new columns. The new variables have
%  already been indexed and added to the Handle.
%    NewObjCoeffs, NewLos, NewHis, NewColCoeffs are either all nil or
%    length AddedCols 
%    NonZeros must correspond to the number of coeffs in NewColCoeffs 
%    (i.e. length of flattened NewColCoeffs)
setup_new_cols(Handle, NewObjCoeffs, NewLos, NewHis, NewColCoeffs, OldCols, AddedCols, NonZeros) :-
        Handle = prob{cplex_handle:CPH},

        (AddedCols > 0 ->
             clear_result(Handle, cbase of prob)
        ;
             true
        ),
        % allocate the buffer arrays
        cplex_set_new_cols(CPH, AddedCols, NewObjCoeffs, NewLos, NewHis, NonZeros),
        % fill the buffer arrays
        (foreach(ColCs, NewColCoeffs), 
         count(I, OldCols, _), 
         fromto(0, K0,K3, _NonZeros), 
         param(CPH) do
             (foreach(J:C, ColCs), fromto(K0, K1,K2, K3), param(CPH) do
                  cplex_set_matval(CPH, K1, J, C),
                  K2 is K1 + 1
             ),
             cplex_set_matbeg(CPH, I, K0, K3)
        ).


    extend_array(Array, Added, NewLos, NewHis, ExtendedArray) :-
        create_extended_iarray(Array, Added, ExtendedArray),
        copy_extended_column_basis(Array, NewLos, NewHis, ExtendedArray).

    extend_primal_arrays(OldBase, OldSol, OldDjs, AddedCols,
                         ExtendedBase, ExtendedSol, ExtendedDjs) :-
        create_extended_iarray(OldBase, AddedCols, ExtendedBase),
        create_extended_darray(OldSol, AddedCols, ExtendedSol),
        create_extended_darray(OldDjs, AddedCols, ExtendedDjs),
        copy_extended_arrays(OldBase, OldSol, OldDjs,
                             ExtendedBase, ExtendedSol, ExtendedDjs).

% ----------------------------------------------------------------------
% Solving
% lp_solve - cplex optimisation is invoked. When it finds a
%	solution, succeed, return cost and store solution in handle.
% ----------------------------------------------------------------------

eplex_solve(Result, Pool) :-
	get_pool_handle(Handle, Pool), 
	!,
	lp_solve(Handle, Result).
eplex_solve(Result, Pool) :-
	printf(error, "Eplex instance %w has no associated solver.%n", [Pool]),
	error(5, eplex_solve(Result)).

lp_solve(Handle, ObjVal) :- var(Handle), !,
        error(4, lp_solve(Handle, ObjVal)).
lp_solve(Handle, ObjVal) :-
	Handle = prob{vars:VList,cplex_handle:CPH, timeout:TO,
                      option_dump:DumpOpt, sync_bounds:SyncBounds,
                      option_mipstart:MipStartOpt,
                      method:M, aux_method:AM, 
                      node_method:NM, node_aux_method:NAM,
                      obj:ObjConst,cbase:CBase,rbase:RBase,
                      iis_rows:IISRows},
        cplex_get_prob_param(CPH, 10, ProbState),
        ( ProbState = 0 ->			% DESCR_EMPTY
            printf(error, "Eplex error: no problem loaded in %w.%n",
                   [lp_solve(Handle, ObjVal)]),
            abort
        ; sync_bounds_mat(SyncBounds, VList, Handle, CPH)
        ),
        ( array(CBase) ->			% use basis if available
%	    writeln(log_output, 'loading basis'),
            cplex_loadbase(CPH, CBase, RBase)
        ;
            true
        ),
        ( nonvar(IISRows) ->
            /* have a previous iis result from failure, clear it */
            clear_result(Handle, iis_rows of prob),
            clear_result(Handle, iis_cols of prob),
            clear_result(Handle, iis_colstats of prob)
        ;
            true
        ),
        
        cplex_optimise(Handle, m(M,AM,NM,NAM), TO, DumpOpt, MipStartOpt,
		sols of prob, Result, Stat, Worst, Best), 
        set_lp_handle_value(Handle, status of prob, Stat),
        BestBound is ObjConst + Best,
        WorstBound is ObjConst + Worst,
        set_lp_handle_value(Handle, worstbound of prob, WorstBound),
        set_lp_handle_value(Handle, bestbound of prob, BestBound),
        cplex_get_prob_param(CPH, 0, MRows),
        set_lp_handle_value(Handle, mr of prob, MRows),
/*        cplex_get_prob_param(CPH, 16, NCP),
        set_lp_handle_value(Handle, ncpr of prob, NCP),

        cplex_get_prob_param(CPH, 18, NAct),
        set_lp_handle_value(Handle, occpr of prob, NAct),
*/
        consider_status(Result, Handle, ObjVal).


set_lp_handle_value(Handle, ArgPos, New) :-
        arg(ArgPos, Handle, Old),
        ( var(Old) -> Old = New
        ; setarg(ArgPos, Handle, New)).

    % Note: redundant cuts here because of compiler indexing problem
    :- mode consider_status(++,+,?).
    consider_status(2, Handle, ObjVal) :- !,
        Handle = prob{cplex_handle:CPH,obj:ObjConst},
	ObjVal is ObjConst + cplex_get_objval(CPH),	% DESCR_SOLVED_SOL
        set_lp_handle_value(Handle, cost of prob, ObjVal),
        % Schedule variable suspensions
        schedule_suspensions(change_suspensions of prob, Handle),
        wake.

    consider_status(3, Handle, _) :- !,			% DESCR_SOLVED_NOSOL
        Handle = prob{infeash:G,caller_module:Caller},
        set_lp_handle_value(Handle, cost of prob, _), % not known!
        (nonvar(G) ->
             call(G)@Caller
        ;
             error(eplex_infeasible, lp_solve(Handle, _ObjVal))
        ).

    consider_status(4, Handle, ObjVal) :- !,		% DESCR_ABORTED_SOL
    	Handle = prob{cplex_handle:CPH,obj:ObjConst,
                      subopth:G,caller_module:Caller},
	ObjVal is ObjConst + cplex_get_objval(CPH),
        % set the cost etc. in the handle *before* the handler G is called
        % so that the handler can get at the cost, best/worst bounds etc.
        set_lp_handle_value(Handle, cost of prob, ObjVal),
        schedule_suspensions(change_suspensions of prob, Handle),
        (nonvar(G) ->
             call(G)@Caller
        ;
             error(eplex_suboptimal, lp_solve(Handle, ObjVal))
        ).

    consider_status(6, Handle, ObjVal) :- !,		% DESCR_UNBOUNDED_NOSOL
    	Handle = prob{objsense:Sense,unboundh:G,caller_module:Caller},
	( cplex_objsense(min, Sense) -> ObjVal = -1.0Inf ; ObjVal = 1.0Inf ),
        set_lp_handle_value(Handle, cost of prob, ObjVal),
        (nonvar(G) ->
             call(G)@Caller
        ;
             error(eplex_unbounded, lp_solve(Handle, ObjVal))
        ).

    consider_status(7, Handle, ObjVal) :- !,		% DESCR_UNKNOWN_NOSOL
    	Handle = prob{unkh:G,caller_module:Caller},
        set_lp_handle_value(Handle, cost of prob, _), % not known!
        (nonvar(G) ->
             call(G)@Caller
        ;
             error(eplex_unknown, lp_solve(Handle, ObjVal))
        ).

    consider_status(5, Handle, ObjVal) :-		% DESCR_ABORTED_NOSOL
    	Handle = prob{aborth:G,caller_module:Caller},
        set_lp_handle_value(Handle, cost of prob, _), % not known!
        (nonvar(G) ->
             call(G)@Caller
        ;
             error(eplex_abort, lp_solve(Handle, ObjVal))
        ).


% default behaviour for the unclear results

eplex_result_handler(eplex_suboptimal, lp_solve(prob{status:Stat}, _)) :-
	printf(warning_output, "event(eplex_suboptimal): Suboptimal solution (optimizer status = %d)%n", [Stat]).

eplex_result_handler(eplex_unbounded, lp_solve(prob{status:Stat}, _)) :-
	printf(warning_output, "event(eplex_unbounded): Problem unbounded, no solution values! (optimizer status = %d)%n", [Stat]).

eplex_result_handler(eplex_unknown, lp_solve(prob{status:Stat}, _)) :-
	printf(warning_output, "event(eplex_unknown): Infeasible or unbounded - failing (optimizer status = %d)%n", [Stat]),
	fail.

eplex_result_handler(eplex_abort, lp_solve(prob{status:Stat}, _)) :-
	printf(error, "event(eplex_abort): Optimization aborted (optimizer status = %d)%n", [Stat]),
	throw(abort).

eplex_result_handler(eplex_infeasible, lp_solve(_,_)) :-
        fail.

:- set_event_handler(eplex_suboptimal,	eplex_result_handler/2).
:- set_event_handler(eplex_unbounded,	eplex_result_handler/2).
:- set_event_handler(eplex_unknown,	eplex_result_handler/2).
:- set_event_handler(eplex_abort,	eplex_result_handler/2).
:- set_event_handler(eplex_infeasible,	eplex_result_handler/2).


% ----------------------------------------------------------------------
% Run the solver with a temporary altered problem, fail if no bound
% can be computed (infeasible or unbounded).
%
% The alternative solution would be to allow problem to be modified
% explicitly. But we couldn't allow that with a solver demon because that would
% make the cost non-monotonic. So it seems safer to encapsulate probing.
% ----------------------------------------------------------------------

eplex_probe(ProbeSpec, Result, Pool) :-
	get_pool_handle(Handle, Pool), 
	!,
	lp_probe(Handle, ProbeSpec, Result).
eplex_probe(ProbeSpec, Result, Pool) :-
	printf(error, "Eplex error: instance %w has no associated"
               " solver:%n", [Pool]),
        error(5, eplex_probe(ProbeSpec, Result)).


lp_probe(Handle, ProbeSpec, Result) :-
	( var(ProbeSpec) -> error(4, lp_probe(Handle, ProbeSpec, Result))
        ; var(Handle) ->  error(4, lp_probe(Handle, ProbeSpec, Result))
	; true
        ),
        extract_probes(ProbeSpec, Probes, Extracted),
        set_probes(Handle, Probes, Extracted, SetProbes),
        block_with_probes(lp_solve(Handle, Result), Handle, SetProbes),
        unset_probes(Handle, SetProbes).

extract_probes(ProbeSpec, Probes, ExtractSorted) :-
        ( ProbeSpec = [_|_] ->
              ( foreach(Probe, ProbeSpec), param(Probes), 
                fromto(Extracted, Extracted0,Extracted1, []) do
                    nonvar(Probe), 
                    extract_one_probe(Probe, Probes, Extracted0, Extracted1)
              )
        ;
              extract_one_probe(ProbeSpec, Probes, Extracted, [])
        ), !,
        % Extracted Probes should be done in order of their priorities
        sort(2, =<, Extracted, ExtractSorted).
extract_probes(ProbeSpec, _, _) :-
        printf(error, "Eplex error: incorrect probe specification(s) found"
               " in: %w%n", [ProbeSpec]),
        abort.

    % Extracted is a list of the extracted probe types in the form 
    % Type - Prio, where Prio is used to determine the order the
    % probe will be set: probes with a lower value for Prio are set 
    % first. Currently only two priority levels are used: 1 and 3,
    % as the only ordering requirement is that ints probe need to
    % be done last, as they can change the problem to the fixed/relaxed
    % type that the other probes do not deal with, and also because
    % CPLEX does not allow a fixed problem to be changed further.
    extract_one_probe(ObjExpr, Probes, Extracted0, Extracted) :-
        cplex_objsense(ObjExpr, Sense, Expr),
        !,
        % we need to do this before the ints probe. Can change problem type
        Extracted0 = [obj-1,sense-1|Extracted],
        Probes = probes{obj:objexpr(Expr), sense:Sense}.
    extract_one_probe(objsense(ObjSense), Probes, Extracted0, Extracted) :-
        cplex_objsense(ObjSense, Sense), 
        Extracted0 = [sense-1|Extracted],
        Probes = probes{sense:Sense}.
    extract_one_probe(objexpr(Expr), Probes, Extracted0, Extracted) :-
        % we need to do this before the ints probe. Can change problem type
        Extracted0 = [obj-1|Extracted],
        Probes = probes{obj:objexpr(Expr)}.
    extract_one_probe(perturb_obj(Deltas), Probes, Extracted0, Extracted) :-
        is_list_or_nil(Deltas),
        % linear coeffs only for now -- cannot change problem type
        Extracted0 = [obj-1|Extracted],
        Probes = probes{obj:objdeltas(Deltas)}.
    extract_one_probe(bounds(Bounds), Probes, Extracted0, Extracted) :-
        is_list_or_nil(Bounds), 
        Extracted0 = [bounds-1|Extracted],
        Probes = probes{bounds:Bounds}.
    extract_one_probe(fixed, Probes, Extracted0, Extracted) :- 
        Probes = probes{ints:fixed},
        Extracted0 = [ints-3|Extracted].
    extract_one_probe(relaxed, Probes, Extracted0, Extracted) :- 
        Probes = probes{ints:relaxed},
        Extracted0 = [ints-3|Extracted].
    extract_one_probe(rhscoeffs(Rhs), Probes, Extracted0, Extracted) :-
        is_list_or_nil(Rhs), 
        Probes = probes{rhs:Rhs},
        Extracted0 = [rhs-1|Extracted].


set_probes(Handle, Probes, Extracted, SetProbes) :-
        (foreach(ProbeType - _, Extracted), param(Probes, Handle),
         fromto([], Set0,Set1, SetProbes) do
               block_with_probes(set_one_probe(ProbeType, Handle, Probes, Set0, Set1),
                                 Handle, Set0)
        ).
            

block_with_probes(Goal, Handle, SetProbes) :-
        ( catch(Goal, Tag,
                   ( unset_probes(Handle, SetProbes),
                     throw(Tag)
                   )
               )
             ->
                 true
             ;
                 unset_probes(Handle, SetProbes),
                 fail
        ).


set_one_probe(obj, Handle, Probes, Set0, Set1) ?-
        Probes = probes{obj:Obj},
        set_obj_probe(Handle, Obj, Set0, Set1).
set_one_probe(sense, Handle, Probes, Set0, Set1) ?-
        Probes = probes{sense:Sense},
        set_objsense_probe(Handle, Sense, Set0, Set1).
set_one_probe(ints, Handle, Probes, Set0, Set1) ?-
        Probes = probes{ints:IntProbe},
        set_ints_probe(Handle, IntProbe, Set0, Set1).
set_one_probe(rhs, Handle, Probes, Set0, Set1) ?-
        Probes = probes{rhs:Rhs},
        set_rhs_probe(Handle, Rhs, Set0, Set1).
set_one_probe(bounds, Handle, Probes, Set0, Set1) ?-
        Probes = probes{bounds:Bounds},
        set_bounds_probe(Handle, Bounds, Set0, Set1).


set_obj_probe(Handle, objexpr(Expr), Set0, Set1) ?- !,
        Handle = prob{cplex_handle:CPH, obj:OldObjConst, solver_id:SId, 
                      objcoeffs:OldLinObjCoeffs, qobjcoeffs:OldQuadObjCoeffs},

        quadnorm(Expr, NewObjConst, NewLinObj, NewQuadObj),

        obj_coeffs(NewLinObj, SId, NewLinObjCoeffs),
        qobj_coeffs(NewQuadObj, SId, NewQuadObjCoeffs),
        change_objective(CPH, OldLinObjCoeffs, NewLinObjCoeffs, OldQuadObjCoeffs, NewQuadObjCoeffs),
        Set1 = [obj(OldObjConst, NewLinObjCoeffs, OldLinObjCoeffs, 
                    NewQuadObjCoeffs, OldQuadObjCoeffs)|Set0],
        setarg(obj of prob, Handle, NewObjConst).	% needed by lp_solve/2
%	lp_get(Handle, objective, TmpObj),
%	writeln(TmpObj),
set_obj_probe(Handle, objdeltas(ObjDeltas), Set0, Set1) ?- !,
        Handle = prob{cplex_handle:CPH,solver_id:SId},
        hash_create(Hash),
        (foreach(DSpec, ObjDeltas), param(SId,Hash) do
            (nonvar(DSpec), DSpec = V:Delta1,
             get_first_var_index(V, SId, J), number(Delta1) -> 
                true
            ;
                printf(error, "Eplex error: incorrect probe specification(s)"
                       " found for perturb_obj probe: %w%n", [DSpec]),
                abort
            ),
            (hash_get(Hash, J, Delta0) ->
                Delta is Delta0 + Delta1  % accumulate the deltas
            ;
                Delta = Delta1
            ),
            hash_set(Hash, J, Delta)
        ),
        hash_list(Hash, Idxs, Deltas),
        (foreach(J, Idxs), foreach(Delta, Deltas), param(CPH),
         foreach(J:NewC, NewObjs), foreach(J:OldC,OldObjs) do
            cplex_get_obj_coef(CPH, J, OldC),
            NewC is OldC + Delta
        ),
        Set1 = [objc(OldObjs)|Set0],
        chg_obj_coeffs(CPH, NewObjs).


set_objsense_probe(Handle, NewSense, Set0, Set1) :- 
        Handle = prob{cplex_handle:CPH},
        Set1 = [sense|Set0],
        cplex_change_obj_sense(CPH, NewSense).


set_bounds_probe(Handle, Bounds, Set0, [bounds(N,Js,OldLs,OldHs)|Set0]) :-
        Handle = prob{cplex_handle:CPH,solver_id:SId},
        (foreach(BSpec, Bounds), param(SId,CPH), 
         fromto(Js, Js0,Js1, []), fromto(0, N0,N1, N),
         fromto(OldLs, OldLs0,OldLs1, []),
         fromto(OldHs, OldHs0,OldHs1, []),
         fromto(NewLs, NewLs0,NewLs1, []),
         fromto(NewHs, NewHs0,NewHs1, []) do
            
            (nonvar(BSpec), BSpec = (V $:: Range),
             range(Range, L, H), get_var_index(V, SId, J0) ->
                (foreach(J, J0), count(_, 1, NumJ0s), param(CPH,L,H),
                 fromto(Js0, JTail0,JTail1, Js1), 
                 fromto(OldLs0, OldLTail0,OldLTail1, OldLs1),
                 fromto(OldHs0, OldHTail0,OldHTail1, OldHs1),
                 fromto(NewLs0, NewLTail0,NewLTail1, NewLs1),
                 fromto(NewHs0, NewHTail0,NewHTail1, NewHs1)
                do 
                    JTail0 = [J|JTail1],
                    NewLTail0 = [L|NewLTail1],
                    NewHTail0 = [H|NewHTail1],
                    cplex_get_col_bounds(CPH, J, OldL, OldH),
                    OldLTail0 = [OldL|OldLTail1],
                    OldHTail0 = [OldH|OldHTail1]
                ),
                N1 is N0 + NumJ0s
            ;
                printf(error, "Eplex error: incorrect probe specification(s)"
                       " found for bounds probe: %w%n", [BSpec]),
                abort
            )
        ),
        cplex_change_cols_bounds(CPH, N, Js, NewLs, NewHs).
            

set_rhs_probe(Handle, Rhs, Set0, [rhs(N,Is,OldRs)|Set0]) :-
        Handle = prob{cplex_handle: CPH},
        (foreach(RSpec, Rhs), foreach(I, Is), foreach(R, Rs), 
         foreach(OldR, OldRs), count(_, 1, N), param(CPH) do
            (nonvar(RSpec), RSpec = I:R,
             integer(I), number(R) ->
                true
            ;
                printf(error, "Eplex error: incorrect probe specification(s)"
                       " found for rhscoeffs probe: %w%n", [RSpec]),
                abort
            ),
            cplex_get_rhs(CPH, 0, I, _, OldR)
        ), 
        cplex_change_rhs(CPH, N, Is, Rs).


set_ints_probe(Handle, Probe, SetProbes0, SetProbes) :-
        arg(cplex_handle of prob, Handle, CPH),
        cplex_get_prob_param(CPH, 4, OldProbCode),
        SetProbes = [ints(OldProbCode)|SetProbes0],
        cplex_problem_code(OldProbType, OldProbCode),
        (ints_problem_code(OldProbType, Probe,  ProbeProbCode) ->
             true
        ;
             printf(error, "Eplex error: cannot use %w probe with %w"
                    " problems.%n", [Probe, OldProbType]),
             abort
        ),
        cplex_set_problem_type(CPH, ProbeProbCode, 0).

     ints_problem_code(mip, ProbeType, ProbCode) ?-
        ( ProbeType == fixed ->
            cplex_problem_code(fixedlp, ProbCode)
        ; ProbeType == relaxed ->
            cplex_problem_code(relaxedlp, ProbCode)
        ;
            fail
        ).
     ints_problem_code(miqp, ProbeType, ProbCode) ?-
        ( ProbeType == fixed ->
            cplex_problem_code(fixedqp, ProbCode)
        ; ProbeType == relaxed ->
            cplex_problem_code(relaxedqp, ProbCode)
        ;
            fail
        ).
     /* for qp/lp problems, just solve as qp/lp */
     ints_problem_code(lp, ProbeType, ProbCode) ?-
        ( ProbeType == fixed ->
            cplex_problem_code(lp, ProbCode)
        ; ProbeType == relaxed ->
            cplex_problem_code(lp, ProbCode)
        ;
            fail
        ).
     ints_problem_code(qp, ProbeType, ProbCode) ?-
        ( ProbeType == fixed ->
            cplex_problem_code(qp, ProbCode)
        ; ProbeType == relaxed ->
            cplex_problem_code(qp, ProbCode)
        ;
            fail
        ).

unset_probes(Handle, SetProbes) :-
        ( foreach(Set, SetProbes), param(Handle) do
              unset_one_probe(Handle, Set)
        ).

        
     unset_one_probe(Handle, obj(OldObjConst, NewLinObjCoeffs, OldLinObjCoeffs,
                                NewQuadObjCoeffs, OldQuadObjCoeffs)) :- 
        !,
        arg(cplex_handle of prob, Handle, CPH),
        setarg(obj of prob, Handle, OldObjConst),
        change_objective(CPH, NewLinObjCoeffs, OldLinObjCoeffs,
                         NewQuadObjCoeffs, OldQuadObjCoeffs).
     unset_one_probe(Handle, objc(OldObjsC)) :-
        !,
        arg(cplex_handle of prob, Handle, CPH),
        chg_obj_coeffs(CPH, OldObjsC).
     unset_one_probe(Handle, sense) :-
        !,
        Handle = prob{cplex_handle:CPH, objsense:OldSense},
        cplex_change_obj_sense(CPH, OldSense).
     unset_one_probe(Handle, rhs(N,Is,OldRs)) :-
        !,
        arg(cplex_handle of prob, Handle, CPH),
        cplex_change_rhs(CPH, N, Is, OldRs).
     unset_one_probe(Handle, ints(OldType)) :-
        !,
        arg(cplex_handle of prob, Handle, CPH),
        cplex_set_problem_type(CPH, OldType, 1).
     unset_one_probe(Handle, bounds(N,Idxs,Ls,Hs)) :-
        arg(cplex_handle of prob, Handle, CPH),
        cplex_change_cols_bounds(CPH, N,Idxs, Ls, Hs).

    change_objective(CPH, OldLinCoeffs, NewLinCoeffs, OldQuadCoeffs, NewQuadCoeffs) :-
	clr_obj(CPH, OldLinCoeffs),
	chg_obj_coeffs(CPH, NewLinCoeffs),
	clr_qobj(CPH, OldQuadCoeffs),
        % change prob. type here as CPLEX only allows changing qobj for
        % quadratic problems
        change_prob_type_if_needed(CPH, OldQuadCoeffs, NewQuadCoeffs),
	chg_qobj_coeffs(CPH, NewQuadCoeffs).

change_prob_type_if_needed(CPH, [], [_|_]) ?- !,
        % no existing quad. coeffs -> is currently linear
        cplex_get_prob_param(CPH, 4, LinCode),
        cplex_problem_code(LinType, LinCode),
        linear_quadratic(LinType, QuadType),
        cplex_problem_code(QuadType, QuadCode),
        cplex_set_problem_type(CPH, QuadCode, 1).
change_prob_type_if_needed(CPH, [_|_], []) ?- !,
        cplex_get_prob_param(CPH, 4, QuadCode),
        cplex_problem_code(QuadType, QuadCode),
        linear_quadratic(LinType, QuadType),
        cplex_problem_code(LinType, LinCode),
        cplex_set_problem_type(CPH, LinCode, 1).
change_prob_type_if_needed(_CPH, _, _).  % no change


% ----------------------------------------------------------------------
% Various predicates to access the solver's state
% ----------------------------------------------------------------------

eplex_write(Format, File, Pool) :-
	get_pool_handle(Handle, Pool), 
	!,
	lp_write(Handle, Format, File).
eplex_write(Format, File, Pool) :-
	printf(error, "Eplex error: instance %w has no associated"
               " solver:%n", [Pool]),
        error(5, eplex_write(Format, File)).


lp_write(prob{cplex_handle:CPH}, Format, File) ?-
        cplex_lpwrite(File, Format, CPH),
	!.
lp_write(Handle, Format, File) :-
	error(6, lp_write(Handle, Format, File)).


eplex_cleanup(Pool) :-
	get_pool_handle(Handle, Pool),
	!,
	collect_all_pool_constraints(Pool, Cstrs),	% empty the pool, just in case
	( Cstrs = [] ->
	    true
	;
	    printf(warning_output, "Eplex warning: constraint pool not empty in %w%n",
	    	[eplex_cleanup(Pool)])
	),
	lp_cleanup(Handle).
eplex_cleanup(Pool) :-
        % pool still needs to be emptied even if there is no solver!
        collect_all_pool_constraints(Pool, Cstrs),	% empty the pool, just in case
	( Cstrs = [] ->
	    true
	;
	    printf(warning_output, "Eplex warning: constraint pool not empty in %w%n",
	    	[eplex_cleanup(Pool)])
	).



lp_cleanup(Prob) :-
        Prob = prob{vars:VList,suspension:Susp,
                    solver_id:SId,aux_susps:AuxSusps,pool:Pool}, 
	kill_suspension(Susp),	% if any
	( atom(Pool) -> set_pool_item(Pool, 0) ; true ),
	(foreach(AS, AuxSusps) do kill_suspension(AS)), 
	cleanup_attributes(VList,SId), 
        % delay any possible garbage of Prob until after cplex_cleanup/1 call
        arg(cplex_handle of prob, Prob, CPH),
        cplex_cleanup(CPH).

   cleanup_attributes(Vars, SId) :-
	(foreach(V, Vars), param(SId) do 
	     cleanup_lp_attribute_chain(V, SId)
        ).

   % Some variables may share the same attribute if they've been unified
   cleanup_lp_attribute_chain(V{eplex:Attr}, SId) ?- 
	(nonvar(Attr) -> 
	     Attr = eplex{next:Next0,solver:prob{solver_id:ThisId}},
	     (SId == ThisId ->
		  (compound(Next0) -> Next0 = Next ; true/*Next is var */),
		  replace_attribute(V, Next, eplex)
	     ;
		  remove_lp_attribute_from_chain(Next0, Attr, SId)
	     )
	;
	     true % var(Attr), attr already removed
	).
   cleanup_lp_attribute_chain(V, _) :-
        free(V).
   cleanup_lp_attribute_chain(N, _) :-
        number(N).

   remove_lp_attribute_from_chain(ThisAt, PrevAt, SId) :-
	(compound(ThisAt) ->
	     ThisAt = eplex{next:NextAt,solver:prob{solver_id:ThisId}},
	     (SId == ThisId ->
		  setarg(next of eplex, PrevAt, NextAt)
	     ;
		  remove_lp_attribute_from_chain(NextAt, ThisAt, SId)
	     )
	;
	     true % attribute not found, already removed
	).
	

% check that options is a proper list, remove and warn over obsolete options,
% and separate out options which applies to lp_demon_setup only
clean_options([], CO, _DOpts) ?- !, CO = [].
clean_options([integers(_)|Os], COs, DOpts) ?- !,
	writeln(warning_output, "Eplex warning: integers(...) option no longer supported (ignored)"),
	writeln(warning_output, "	use eplex:integers(...) instead."),
	clean_options(Os, COs, DOpts).
clean_options([collect_from(none)|Os], COs, DOpts) ?- !,
	setarg(collect_from of demon_opts, DOpts, none),
        clean_options(Os, COs, DOpts).
clean_options([collect_from(pool(Pool))|Os], COs, DOpts) ?- !,
	setarg(collect_from of demon_opts, DOpts, pool(Pool)),
        clean_options(Os, COs, DOpts).
clean_options([initial_solve(YesNo)|Os], COs, DOpts) ?- 
        (YesNo == yes ; YesNo == no), !,
	setarg(initial_solve of demon_opts, DOpts, YesNo),
        clean_options(Os, COs, DOpts).
clean_options([priority(N)|Os], COs, DOpts) ?- 
        integer(N), N>=0, N =< 12, !,
	setarg(priority of demon_opts, DOpts, N),
        clean_options(Os, COs, DOpts).
clean_options([O|Os], Options, DOpts) ?- !,
	Options = [O|COs],
	clean_options(Os, COs, DOpts).
clean_options(_Options0, Options, _DOpts) :-
	writeln(warning_output, "Eplex warning: demon solver setup options not proper list. Ignored."),
	Options = [].


process_options([], _, _) ?- !, true.
process_options([O|Os], Handle, Temp) ?- !,
	process_option(O, Handle, Temp),
	process_options(Os, Handle, Temp).
process_options(_NonList, _, _) :-
	writeln(warning_output, "Eplex warning: solver setup options not proper list. Ignored.").

process_option(solution(YesNo), Handle, _) ?- !,
	lp_set(Handle, solution, YesNo).
process_option(dual_solution(YesNo), Handle, _) ?- !,
	lp_set(Handle, dual_solution, YesNo).
process_option(slack(YesNo), Handle, _) ?- !,
	lp_set(Handle, slack, YesNo).
process_option(reduced_cost(YesNo), Handle, _) ?- !,
	lp_set(Handle, reduced_cost, YesNo).
process_option(keep_basis(YesNo), Handle, _) ?- !,
	lp_set(Handle, keep_basis, YesNo).
process_option(cache_iis(YesNo), Handle, _) ?- !,
	lp_set(Handle, cache_iis, YesNo).
process_option(timeout(Lim0), Handle, _) ?- !,
        lp_set(Handle, timeout, Lim0).
process_option(sync_bounds(YesNo), Handle, _) ?- !,
	lp_set(Handle, sync_bounds, YesNo).
process_option(presolve(yes), Handle, _) ?- !,
        arg(presolve of prob, Handle, 1).
process_option(presolve(no), Handle, _) ?- !,
        arg(presolve of prob, Handle, 0).
process_option(mip_use_copy(yes), _Handle, Temp) ?- !,
        setarg(use_copy of temp_prob, Temp, 1).
process_option(mip_use_copy(no), _Handle, Temp) ?- !,
        setarg(use_copy of temp_prob, Temp, 0).
process_option(integers(Ints0), Handle, _) ?- 
        is_list_or_nil(Ints0), !, 
	Handle = prob{cplex_handle:CPH,ints:Ints},
        (foreach(X, Ints0), param(CPH) do % loop may fail, so need to cut first
            ( var(X) -> true
            ; integer(X) -> true
            ; number(X) ->
                % number: make sure it's sufficiently integral
	        abs(round(X) - X) =< cplex_get_param(CPH, integrality)
            ;
                printf(error, "Eplex error: integer variable unified to"
                            " a non-number: %w%n", [X]),
                throw(abort)

            )
        ),
        term_variables(Ints0, Ints). % eliminate duplicates and numbers
process_option(reals(Vars), _Handle, Temp) ?-
        (foreach(V, Vars) do 
            ( number(V) -> true
            ; var(V)    -> true
            ;
                printf(error, "Eplex error: problem variable unified to"
                            " a non-number: %w%n", [V]),
                throw(abort)
            )
        ), !,
        setarg(extra_vars of temp_prob, Temp, Vars).
process_option(method(M), Handle, _) ?- !,
	lp_set(Handle, method, M).
process_option(node_method(M), Handle, _) ?- !,
	lp_set(Handle, node_method, M).
process_option(demon_tolerance(RT,IT), Handle, _) ?- !,
	lp_set(Handle, demon_tolerance, (RT,IT)).
process_option(use_var_names(YesNo), Handle, _) ?- !,
	lp_set(Handle, use_var_names, YesNo).
process_option(write_before_solve(Format,File), Handle, _) ?- !,
        setarg(option_dump of prob, Handle, write_before_solve(Format,File)).
process_option(sos1(Vars), _Handle, Temp) ?- !,
	arg(sos of temp_prob, Temp, SosList),
	setarg(sos of temp_prob, Temp, [sos1(Vars)|SosList]).
process_option(sos2(Vars), _Handle, Temp) ?- !,
	arg(sos of temp_prob, Temp, SosList),
	setarg(sos of temp_prob, Temp, [sos2(Vars)|SosList]).
process_option(post_equality_when_unified(YesNo), Handle, _) ?- !,
	lp_set(Handle, post_equality_when_unified, YesNo).
process_option(suboptimal_handler(Goal), Handle, _) ?- !,
        lp_set(Handle, suboptimal_handler, Goal).
process_option(unbounded_handler(Goal), Handle, _) ?- !,
        lp_set(Handle, unbounded_handler, Goal).
process_option(unknown_handler(Goal), Handle, _) ?- !,
        lp_set(Handle, unknown_handler, Goal).
process_option(abort_handler(Goal), Handle, _) ?- !,
        lp_set(Handle, abort_handler, Goal). 
process_option(infeasible_handler(Goal), Handle, _) ?- !,
        lp_set(Handle, infeasible_handler, Goal). 
process_option(mipstart(MipStart), Handle, _) ?- !,
        lp_set(Handle, mipstart, MipStart). 
process_option(NoOpt, _Handle, _) :-
	writeln(error, "Eplex error: Invalid option for setup":NoOpt),
        abort.


fill_in_defaults(prob{ints:Ints, method:Method, aux_method:AuxMethod, 
                node_method:NMethod, node_aux_method:NAuxMethod, timeout:TO,
                sync_bounds:SyncBds, bd_trigger:BdTrigger, triggermodes:TModes,
                option_vnames:VNames, presolve:PreSolve, option_dump:Dump,
		nc_trigger:NCTrigger, post_equality:PostEq,
		option_mipstart:MipStartOpt,
		demon_tol_real:RT, demon_tol_int:IT, cp_cond_map:CPMap}) :-
	( var(RT) -> RT = 0.00001 ; true ),	%%% preliminary
	( var(IT) -> IT = 0.5 ; true ),
	( var(Ints) -> Ints = [] ; true ),
        ( var(CPMap) -> CPMap = "" ; true), % no solved state yet
        ( var(TO) -> getval(timeout_default,TO) ; true ),  
	( var(PreSolve) -> getval(presolve_default,PreSolve) ; true),
	( var(VNames) -> VNames = no ; true),
	( var(NCTrigger) -> NCTrigger = no ; true),
	( var(BdTrigger) -> BdTrigger = no ; true),
        ( var(TModes) -> TModes = [] ; true),
	( var(Method) -> cplex_method_codes(default, Method, AuxMethod) ; true ),
	( var(NMethod) -> cplex_method_codes(default, NMethod, NAuxMethod) ; true ),
        ( var(PostEq) -> PostEq = yes ; true),
        ( var(Dump) -> Dump = no ; true),
	( var(MipStartOpt) -> mipstart_code(none, MipStartOpt) ; true ),
	( var(SyncBds) -> SyncBds = no ; true).


% Accessing external solver's parameters
% The external solvers organises their parameters in two ways:
%       1. Global to all problems (CPLEX, pre-13 XPRESS)
%       2. Local to each problems (XPRESS 13 onwards)
% In Eplex we allow the access of the parameters without specifying a
% problem handle (`global', lp_get/set/2) 
% and via a problem handle (local, lp_get/set/3).
%
% A problem handle may be a C-level handle if the the problem has been
% setup at the external solver, or a variable if the external solver does
% not yet have the problem (because the problem was empty at setup time).
% 
% All the accesses to the external parameters are done with a handle
% parameter in cplex_get/set_param/3. This handle can be in 3 states:
%  1.    positive integer  (essentially pointer to C-level representation)
%  2.    variable     (no C level problem representation)
%  3.    0            (`global')
%  
%  `global' values are handled differently by Eplex depending on if the
%  solver's parameters are global or not: if they are, then the global
%  values are accessed, otherwise, the `global default', the values 
%  that will be given to a new problem upon setup, will be accessed.
%
%  Summary of the type of values accessed:

%  Handle               Local parameters      Global parameters
% =============================================================
%  positive int         problem's value       global
%     0                 global default        global
%  variable             global default        global
%
%  Note:
%  1. We raise an exception at the C level for *setting* local parameters
%     if the solver has global parameters. 
%  2. `presolve' is a special parameter that is always local to a problem,
%      regardless of if the solver's parameters are global or local.
%      lp_get/set/2 will set the global default value.   
%      It does not map directly onto the external solver's presolve parameter
%      (XPRESS 13+ has more than one, and they may take multiple values;
%       CPLEX does not have local parameters. The solver's presolve 
%       parameter(s) can be accessed directly as with other parameters.


% lp_get(+Handle, +What, -Data)  ------------------------------

eplex_get(What, Data, Pool) :-
	get_pool_handle(Handle, Pool), 
	!,
	lp_get(Handle, What, Data).
eplex_get(What, Data, Pool) :-
	printf(error, "Eplex error: instance %w has no associated"
               " solver:%n", [Pool]),
        error(5, eplex_get(What, Data)).


lp_get(Handle, What, Value) :- var(Handle), !,
	error(4, lp_get(Handle, What, Value)).
lp_get(Handle, What, Value) :- 
        lp_get1(Handle, What, Value).

lp_get1(Handle, What, Value) :- var(What), !,
	error(4, lp_get(Handle, What, Value)).
lp_get1(Handle, vars, VArr) :- !,
	Handle = prob{vars:Vars0},
        reverse(Vars0, Vars), % Vars0 are stored in reverse order from arrays
        VArr =.. [''|Vars].
lp_get1(Handle, ints, Ints) :- !,
	Handle = prob{ints:Ints}.
lp_get1(Handle, solution, RawArr) :- !,		% undocumented
	Handle = prob{sols:RawArr},
	array(RawArr).
lp_get1(Handle, cbasisarr, Arr) :- !,		% undocumented
	Handle = prob{cbase:RawArr},
	array(RawArr),
	decode_basis(RawArr, Arr).
lp_get1(Handle, cbasis, RawArr) :- !,		% undocumented
	Handle = prob{cbase:RawArr},
	array(RawArr).
lp_get1(Handle, rbasis, RawArr) :- !,		% undocumented
	Handle = prob{rbase:RawArr},
	array(RawArr).
lp_get1(Handle, typed_solution, SolArr) :- !,
	Handle = prob{cplex_handle:CPH, sols:RawArr},
	array(RawArr),
        darray_size(RawArr, N),
	functor(SolArr, '', N),
	raw_to_typed_solution(CPH, N, RawArr, SolArr).
lp_get1(Handle, reduced_cost, Array) :- !,		% undocumented
	Handle = prob{djs:Array},
	nonvar(Array).
lp_get1(Handle, constraints, Constraints) :- !,
	lp_get1(Handle, constraints_norm, NC),
	denormalise_cstr(NC, Constraints).
lp_get1(Handle, constraints_norm, Constraints) :- !,
	Handle = prob{cplex_handle:CPH},
        cplex_get_prob_param(CPH, 0, Rows),
        retrieve_constraints(Handle, Rows, Constraints).
lp_get1(Handle, constraints_norm(Is), Constraints) :- !,
	Handle = prob{cplex_handle:CPH,vars:VList},
        VArr =.. [''|VList],
        (foreach(I,Is), param(CPH,VArr), foreach(NC,Constraints) do
	    rawidx_cstridx(CType, RawI, I),
            construct_one_constraint(CPH, CType, RawI, VArr, NC)
	).
lp_get1(Handle, constraints(Is), Constraints) :- !,
        lp_get1(Handle, constraints_norm(Is), NCs),
        denormalise_cstr(NCs, Constraints).
lp_get1(Handle, cutpool_info(Select,IType), Info) :- 
        cutpool_selection_to_rawidxs(Select, Handle, RawIdxs), 
	constraint_type_code(condcp, CType),
        lp_get_cutpool_info(IType, CType, RawIdxs, Handle, Info), !.
lp_get1(Handle, slack, List) :- !,
	Handle = prob{slacks:Array,mr:MRows},
	(array(Array) ->
            darray_list(Array, MRows, List)
        ;
            var(Array), 
            write(error, "Eplex error: information not requested at solver setup: "),
            error(6, lp_get(Handle, slack, List))
        ).
lp_get1(Handle, slack(Indices), List) :- 
        is_list_or_nil(Indices), 
        !,
        Handle = prob{slacks:Array},
        (array(Array) ->
            darray_size(Array, Size),
            (
                foreach(Idx, Indices),
                foreach(Pi, List),
                param(Array, Handle, Size)
            do
                convert_to_row_index(Idx, Handle, RIdx),
                RIdx < Size,
                get_darray_element(Array, RIdx, Pi)
            )
        ;
            var(Array),
            write(error, "Eplex error: information not requested at solver setup: "),
            error(6, lp_get(Handle, slack(Indices), List))
        ).
lp_get1(Handle, dual_solution, List) :- !,
	Handle = prob{pis:Array,mr:MRows},
	(array(Array) ->
            darray_list(Array, MRows, List)
        ;
            var(Array),
            write(error, "Eplex error: information not requested at solver setup: "),
            error(6, lp_get(Handle, dual_solution, List))
        ).         
lp_get1(Handle, dual_solution(Indices), List) :- 
        is_list_or_nil(Indices), 
        !,
        Handle = prob{pis:Array},
        (array(Array) ->
            darray_size(Array, Size),
            (
                foreach(Idx, Indices),
                foreach(Pi, List),
                param(Array, Handle, Size)
            do
                convert_to_row_index(Idx, Handle, RIdx),
                RIdx < Size,
                get_darray_element(Array, RIdx, Pi)
            )
        ;
            var(Array),
            write(error, "Eplex error: information not requested at solver setup: "),
            error(6, lp_get(Handle, dual_solution(Indices), List))
        ).
lp_get1(Handle, objective, Obj) :- !,
	Handle = prob{obj:ObjConst, objsense:Sense},
	retrieve_objective(Handle, NExpr),
	linrenorm([ObjConst*1|NExpr], NExpr1),
	delinearize(NExpr1, Expr),
	cplex_objsense(Obj, Sense, Expr).
lp_get1(Handle, norm_objective, Obj) :- !,
	Handle = prob{obj:ObjConst, objsense:Sense},
	retrieve_objective(Handle, NExpr),
	cplex_objsense(Obj, Sense, [ObjConst*1|NExpr]).
lp_get1(Handle, sense, MinMax) :- !,			% undocumented
	Handle = prob{objsense:Code},
	cplex_objsense(MinMax, Code).
lp_get1(Handle, status, Stat) :- !,
	Handle = prob{status:Stat}.
lp_get1(Handle, method, M) :- !,
	Handle = prob{method:Code, aux_method:AuxCode},
	cplex_method_codes(M, Code, AuxCode).
lp_get1(Handle, node_method, M) :- !,
	Handle = prob{node_method:Code, node_aux_method:AuxCode},
	cplex_method_codes(M, Code, AuxCode).
lp_get1(Handle, demon_tolerance, (RT,IT)) :- !,
	Handle = prob{demon_tol_real:RT, demon_tol_int:IT}.
lp_get1(Handle, cost, C) :- !,
        % do not unify directly as C may be a number of different type from C0
	Handle = prob{cost:C0},
        number(C0),
	(number(C) -> C =:= C0 ; C = C0).
lp_get1(Handle, best_bound, B) :- !,
        Handle = prob{bestbound:B0},
        nonvar(B0),
        B = B0.
lp_get1(Handle, worst_bound, B) :- !,
        Handle = prob{worstbound:B0},
        nonvar(B0),
        B = B0.
lp_get1(Handle, statistics, List) :- !,
	Handle = prob{cplex_handle:CPH},
        List = [Successes,Failures,Aborts],
        cplex_get_prob_param(CPH, 5, Successes),
        cplex_get_prob_param(CPH, 6, Failures),
        cplex_get_prob_param(CPH, 7, Aborts).
lp_get1(Handle, simplex_iterations, N) :- !,
	Handle = prob{cplex_handle:CPH},
	cplex_get_prob_param(CPH, 8, N).
lp_get1(Handle, node_count, N) :- !,
	Handle = prob{cplex_handle:CPH},
	cplex_get_prob_param(CPH, 9, N).
lp_get1(Handle, problem_type, Value) :- !,
	Handle = prob{cplex_handle:CPH},
        cplex_get_prob_param(CPH, 4, Code),
        cplex_problem_code(Value, Code).
lp_get1(prob{cplex_handle:CPH}, num_rows, N) :- !,
	cplex_get_prob_param(CPH, 0, N).
lp_get1(prob{cplex_handle:CPH}, num_cols, N) :- !,
	cplex_get_prob_param(CPH, 1, N).
lp_get1(prob{cplex_handle:CPH}, num_nonzeros, N) :- !,
	cplex_get_prob_param(CPH, 12, N).
lp_get1(prob{cplex_handle:CPH}, num_ints, N) :- !,
	cplex_get_prob_param(CPH, 13, N).
lp_get1(prob{cplex_handle:CPH}, num_quads, N) :- !,
	cplex_get_prob_param(CPH, 14, N).
lp_get1(prob{cplex_handle:CPH}, optimizer_param(Param), Value) ?- 
        atom(Param), 
        cplex_get_param(CPH, Param, Value), !.
lp_get1(prob{timeout:Value0}, timeout, Value) ?- !,
	Value = Value0.
lp_get1(Handle, post_equality_when_unified, Value) ?- !,
	Handle = prob{post_equality:Value}.
lp_get1(Handle, handle, Value) :- !,
	Value = Handle.
lp_get1(Handle, pool, Pool) :- !,
	Handle = prob{pool:Pool},
	nonvar(Pool).
lp_get1(prob{option_mipstart:MipStartOpt}, mipstart, MipStart) :- !,
	mipstart_code(MipStart, MipStartOpt).
lp_get1(Handle, What, Value) :-
	error(6, lp_get(Handle, What, Value)).


% convert the index to an actual row index in the last solved matrix 
convert_to_row_index(Idx0, Handle, RowIdx) :- % normal row index 
        rawidx_cstridx(TypeCode, RawIdx, Idx0),
        constraint_type_code(CType, TypeCode),
        convert_rawidx_to_row_index(CType, Handle, RawIdx, RowIdx).

    convert_rawidx_to_row_index(norm, Handle, RawIdx, Idx) ?-
        arg(mr of prob, Handle, MR),
        RawIdx =< MR, 
        RawIdx = Idx.
/*
    convert_rawidx_to_row_index(permcp, Handle, RawIdx, Idx) ?-
        Handle = prob{mr:MR,ncpr:NCP},
        RawIdx =< NCP, 
        Idx is RawIdx + MR.
*/
    convert_rawidx_to_row_index(condcp, Handle, RawIdx, Idx) ?-
        Handle = prob{mr:MR,cp_cond_map:Map},
        iarray_size(Map, Size),
        RawIdx < Size,
        get_iarray_element(Map, RawIdx, Delta),
        Delta >= 0,  % -ve if row was not added
        Idx is MR + Delta.



% lp_get(+Handle, +What, +Index, -Data)  ------------------------------
%
%lp_get(Handle, solution, I, Value) :- !, 
%	Handle = prob{sols:Array}, array(Array),
%	get_darray_element(Array, I, Value).
%lp_get(Handle, reduced_cost, I, Value) :- !, 
%	Handle = prob{djs:Array}, array(Array),
%	get_darray_element(Array, I, Value).
%lp_get(Handle, slack, I, Value) :- !, 
%	Handle = prob{slacks:Array}, array(Array),
%	get_darray_element(Array, I, Value).
%lp_get(Handle, dual_solution, I, Value) :- !, 
%	Handle = prob{pis:Array}, array(Array),
%	get_darray_element(Array, I, Value).
%lp_get(Handle, What, I, Value) :-
%	error(6, lp_get(Handle, What, I, Value)).


% lp_set(+Handle, +What, +Data)  ------------------------------

eplex_set(What, Data, Pool) :-
        get_pool_handle(Handle, Pool), 
	!,
	lp_set(Handle, What, Data).
eplex_set(What, Data, Pool) :-
	printf(error, "Eplex error: instance %w has no associated solver.%n", [Pool]),
	error(5, eplex_set(What, Data)).


lp_set(Handle, What, Value) :-
        var(What), !,
        error(4, lp_set(Handle, What, Value)).
lp_set(Handle, What, Value) :- 
        lp_set1(Handle, What, Value).

lp_set1(Handle, What, Value) :-
        var(Value), !,
        error(4, lp_set(Handle, What, Value)).
lp_set1(Handle, method, M) :-
	cplex_method_codes(M, Code, AuxCode), !,
	setarg(method of prob, Handle, Code),
        setarg(aux_method of prob, Handle, AuxCode).
lp_set1(Handle, node_method, M) :-
	cplex_method_codes(M, Code, AuxCode), !,
	setarg(node_method of prob, Handle, Code),
	setarg(node_aux_method of prob, Handle, AuxCode).
lp_set1(Handle, demon_tolerance, (RT,IT)) :- -?->
	float(RT), float(IT), !,
	setarg(demon_tol_real of prob, Handle, RT),
	setarg(demon_tol_int of prob, Handle, IT).
lp_set1(Handle, slack, YesNo) :-
	select_result(Handle, slacks of prob, YesNo), !.
lp_set1(Handle, solution, YesNo) :-
	select_result(Handle, sols of prob, YesNo), !.
lp_set1(Handle, dual_solution, YesNo) :-
	select_result(Handle, pis of prob, YesNo), !.
lp_set1(Handle, reduced_cost, YesNo) :-
	select_result(Handle, djs of prob, YesNo), !.
lp_set1(Handle, keep_basis, YesNo) :-
	select_result(Handle, cbase of prob, YesNo),
	select_result(Handle, rbase of prob, YesNo), !.
lp_set1(Handle, cache_iis, YesNo) :- !,
        select_result(Handle, iis_rows of prob, YesNo),
        select_result(Handle, iis_cols of prob, YesNo),
        select_result(Handle, iis_colstats of prob, YesNo).
lp_set1(Handle, sync_bounds, yes) :- !, 
	setarg(sync_bounds of prob, Handle, yes).
lp_set1(Handle, sync_bounds, no) :- !, 
	setarg(sync_bounds of prob, Handle, no).
lp_set1(Handle, use_var_names, yes) :- !,
	setarg(option_vnames of prob, Handle, yes).
lp_set1(Handle, use_var_names, no) :- !,
	setarg(option_vnames of prob, Handle, no).
lp_set1(Handle, write_before_solve, no) ?- !,
	setarg(option_dump of prob, Handle, no).
lp_set1(Handle, write_before_solve, (Format,File)) ?- !,
	setarg(option_dump of prob, Handle, write_before_solve(Format,File)).
lp_set1(Handle, post_equality_when_unified, yes) :- !, 
	setarg(post_equality of prob, Handle, yes).
lp_set1(Handle, post_equality_when_unified, no) :- !, 
	setarg(post_equality of prob, Handle, no).
lp_set1(Handle, mipstart, MipStart) :-
	mipstart_code(MipStart, MipStartOpt), !,
	setarg(option_mipstart of prob, Handle, MipStartOpt).
lp_set1(Handle, order, SpecList) :-
	Handle = prob{cplex_handle:CPH,solver_id:SId},
	make_order_list(SpecList, SId, OrderList, 0, Length),
	!,
	cplex_loadorder(CPH, Length, OrderList).
lp_set1(Handle, cbasis, RawArr) :-		% undocumented
	array(RawArr), !,
	( arg(cbase of prob, Handle, RawArr) -> true
	; setarg(cbase of prob, Handle, RawArr) ).
lp_set1(Handle, rbasis, RawArr) :-		% undocumented
	array(RawArr), !,
	( arg(rbase of prob, Handle, RawArr) -> true
	; setarg(rbase of prob, Handle, RawArr) ).
lp_set1(Handle, suboptimal_handler, Spec) :- !,
        lp_set_state_handler(Handle, subopth of prob, Spec).
lp_set1(Handle, unbounded_handler, Spec) :- !,
        lp_set_state_handler(Handle, unboundh of prob, Spec).
lp_set1(Handle, unknown_handler, Spec) :- !,
        lp_set_state_handler(Handle, unkh of prob, Spec).
lp_set1(Handle, abort_handler, Spec) :- !,
        lp_set_state_handler(Handle, aborth of prob, Spec).
lp_set1(Handle, infeasible_handler, Spec) :- !,
        lp_set_state_handler(Handle, infeash of prob, Spec).
lp_set1(Handle, timeout, Value0) :-
	number(Value0),
	Value is float(Value0),
        Value >= 0,
        !,
        setarg(timeout of prob, Handle, Value).
lp_set1(Handle, optimizer_param(Param), Value) :-
        Handle = prob{cplex_handle:CPH},
        atom(Param), !,
        (cplex_set_param(CPH, Param, Value) ->
             true
        ;
             printf(warning_output, "Eplex warning: unknown parameter, setting ignored in %w%n",
		[lp_set(Handle, optimizer_param(Param), Value)])
        ).
lp_set1(Handle, cutpool_option(Idx,Opt), Value) :-
        nonvar(Idx), nonvar(Value), nonvar(Opt),
        lp_set_cp_option(Opt, Handle, Idx, Value), !.
lp_set1(Handle, cutpool_name, Name) :-
        atom(Name),
        arg(cplex_handle of prob, Handle, CPH), !,
        % create cutpool name if new
        cplex_get_named_cp_index(CPH, Name, 1, _). 
lp_set1(Handle, Param, Value) :-
	error(6, lp_set(Handle, Param, Value)).

    select_result(Handle, Arg, yes) :- !,
	( arg(Arg, Handle, []) -> true ; true ).
    select_result(Handle, Arg, clear) :- !,
	( arg(Arg, Handle, []) -> true ; setarg(Arg, Handle, []) ).
    select_result(Handle, Arg, no) :- !,
	arg(Arg, Handle, Old),
	( nonvar(Old) -> setarg(Arg, Handle, _) ; true ).

    clear_result(Handle, Arg) :-
	arg(Arg, Handle, Arr),
	( var(Arr) -> true ; Arr = [] -> true ; setarg(Arg, Handle, []) ).

    make_order_list([], _, [], N, N).
    make_order_list([order(Vars,Prio,Dir)|In], SId, OutList, N0, N) :-
	integer(Prio), integer(Dir),
	vars_to_colnos(Vars, SId, Prio, Dir, OutList, OutList0, N0, N1),
	make_order_list(In, SId, OutList0, N1, N).

    :- mode vars_to_colnos(+,+,+,+,+,-,+,-).
    vars_to_colnos([], _, _, _, L, L, N, N).
    vars_to_colnos([V|Vs], SId, Prio, Dir, OutList, OutList0, N0, N) :-
	( var(V) ->
	    get_first_var_index(V, SId, Index),
	    OutList = [order(Index,Prio,Dir)|OutList1],
	    N1 is N0+1,
	    vars_to_colnos(Vs, SId, Prio, Dir, OutList1, OutList0, N1, N)
	;
	    vars_to_colnos(Vs, SId, Prio, Dir, OutList, OutList0, N0, N)
	).

lp_set_state_handler(Handle, Pos, Goal) :-
        ( Goal == default -> 
              arg(Pos, Handle, S),
              ( var(S) -> true ; setarg(Pos, Handle, _))
        ; 
              setarg(Pos, Handle, Goal)
        ).


% lp_set(+ParameterName, +Value)  ------------------------------

lp_set(Param, Value) :- var(Param), !,
	error(4, lp_set(Param, Value)).
lp_set(result_channel, S) :- !, lp_set_channel(0, S).
lp_set(error_channel, S) :- !, lp_set_channel(1, S).
lp_set(warning_channel, S) :- !, lp_set_channel(2, S).
lp_set(log_channel, S) :- !, lp_set_channel(3, S).
lp_set(presolve, YesNo) :- !,
	((YesNo == 1 ; YesNo == 0) ->
	     setval(presolve_default, YesNo)
	;
	     error(6, lp_set(presolve, YesNo))
	).
lp_set(timeout, Value0) :-
	number(Value0),
	Value is float(Value0),
        Value >= 0,
        !,
        setval(timeout_default, Value).
lp_set(optimizer_param(Param), Value) :-
        atom(Param), !,
        (cplex_set_param(0, Param, Value) ->
             true
        ;
             printf(warning_output, "Eplex warning: unknown parameter, setting ignored in %w%n",
		[lp_set(optimizer_param(Param), Value)])
        ).
lp_set(Param, Value) :-
	error(5, lp_set(Param, Value)).

    lp_set_channel(Ch, +(S)) ?- !,
	get_stream(S, SNr),
	cplex_output_stream(Ch, 1, SNr).
    lp_set_channel(Ch, -(S)) ?- !,
	get_stream(S, SNr),
	cplex_output_stream(Ch, 0, SNr).
    lp_set_channel(Ch, S) :-
	get_stream(S, SNr),
	cplex_output_stream(Ch, 1, SNr).


% lp_get(+ParameterName, -Value)  ------------------------------

lp_get(optimizer, Value) ?-		!, cplex_get_param(0, -1, Value).
lp_get(optimizer_version, Value) ?-	!, cplex_get_param(0, -2, Value).
lp_get(has_qp, Value) ?-		!, cplex_get_param(0, -3, Value).
lp_get(has_miqp, Value) ?-		!, cplex_get_param(0, -4, Value).
lp_get(has_indicator_constraints, Value) ?- !, cplex_get_param(0, -5, Value).
lp_get(standalone, Value) ?-		!, Value = yes.
lp_get(presolve, Value) ?-		!, getval(presolve_default, Value).
lp_get(timeout, Value) ?-		!, getval(timeout_default, Value).
lp_get(optimizer_param(Param), Value) ?-
	atom(Param), 
	cplex_get_param(0, Param, Value), !.
lp_get(Param, Value) :-
	error(6, lp_get(Param, Value)).


% ----------------------------------------------------------------------
% Code for reading problems from files
% ----------------------------------------------------------------------

:- tool(eplex_read/3, eplex_read_body/4).

eplex_read_body(Format, File, Pool, Caller) :-
        \+ get_pool_handle(_, Pool), !,
        lp_read_body(File, Format, Handle, Caller),
        lp_pool_associate_solver(Pool, Handle).
eplex_read_body(Format, File, Pool, _Caller) :-
        printf(error, "Eplex error: instance %w already has an associated solver.%n",
	    [Pool]),
	error(5, eplex_read(Format, File)).

:- tool(lp_read/3, lp_read_body/4).

lp_read_body(File, Format, Handle, Caller) :-
	var(Handle),
	getval(presolve_default, PreSolve),

        % fill in fields that are different/not filled in by
        % fill_in_defaults/1
	Handle = prob{vars:VList, ints:Ints, sols:[], 
		objsense:ObjSense, obj:0.0, objcoeffs:ObjCoeffs,
		solver_id:SId, 
		presolve:PreSolve, % fill in as we have it anyway
		% linobj,quadobj only used during setup
		qobjcoeffs:[], % incorrect for quadratic problems! (b435)
                caller_module:Caller
                % pool: set by eplex_read/2
                % subopth:_,unboundh:_,unkh:_,aborth:_,
		% suspension:_,status:_,cost:_,
                % pis:_,slacks:_,djs:_,cbase:_,rbase:_
		},
	new_solver_id(SId), 
	init_suspension_list(aux_susps of prob, Handle),
        init_suspension_list(change_suspensions of prob, Handle),
	cplex_lpread(File, Format, PreSolve, Handle),

	arg(cplex_handle of prob, Handle, CPH),
        cplex_get_prob_param(CPH, 1, NCols),
        cplex_matrix_base(Base),
	set_var_indices(VList, Handle, Base, NCols, _Cols),
	retrieve_ints(CPH, SId, VList, Ints),

	fill_in_defaults(Handle),  % needs to be after Ints filled in
	cplex_get_prob_param(CPH, 3, ObjSense),
	retrieve_objective(Handle, ObjNorm),
	obj_coeffs(ObjNorm, SId, ObjCoeffs).


retrieve_constraints(_Handle, 0, []) :- !.
retrieve_constraints(Handle, Rows, Constraints) :-
	Handle = prob{cplex_handle:CPH,vars:VList},
        VArr =.. [''|VList], % need random access to variables
        cplex_matrix_base(Base),
        MaxRow is Rows - cplex_matrix_offset,
        constraint_type_code(norm, CType),
        (for(I,Base,MaxRow), param(VArr,CPH,CType),
	 foreach(C, Constraints) do 
            construct_one_constraint(CPH, CType, I, VArr, C)
        ).


construct_one_constraint(CPH, CType, I, VArr, Cstr) :-
        cplex_get_row(CPH, CType, I, Delta),
        retrieve_one_constraint(CPH, CType, Delta, VArr, Lhs),
        cplex_get_rhs(CPH, CType, I, Sense, Rhs),
        Const is -Rhs,
        Cstr = Sense: [Const*1|Lhs].

    retrieve_one_constraint(CPH, CType, Base, VArr, Term) :-
	( cplex_get_col_coef(CPH, CType, Base, J, C) ->
	    arg(J, VArr, X), Term = [C*X|Term0],
	    retrieve_one_constraint(CPH, CType, Base, VArr, Term0)
	;
	    Term = []
	).

retrieve_objective(Handle, Term) :-
	Handle = prob{cplex_handle:CPH,vars:VList},
        warn_if_quadratic(CPH),
        MaxIdx is cplex_get_prob_param(CPH, 1) - 1,
        (foreach(X,VList), fromto(MaxIdx, Idx0, Idx1, _),
         % don't get Idx directly from X as X may be instantiated
         fromto([], Term0,Term1, Term), param(CPH) do
             cplex_get_obj_coef(CPH, Idx0, C),
             Idx1 is Idx0 - 1,
             ( C = 0.0 -> Term1 = Term0 ; Term1 = [C*X|Term0] )
        ).

% retrieve integer variables from a newly read-in problem from lp_read
retrieve_ints(CPH, SId, VList, Ints) :-
        (foreach(X, VList), fromto([], Ints0,Ints1, Ints), param(CPH,SId) do
            get_unique_var_index(X, SId, J), % these are new vars => no merged cols
             cplex_get_col_type(CPH, J, Tcode), 
             cplex_type_code(Type, Tcode),
             ( Type = integer -> Ints1 = [X|Ints0] ; Ints1 = Ints0 )
	).

   warn_if_quadratic(CPH) :-
        cplex_get_prob_param(CPH, 4, Code),
        cplex_problem_code(Type, Code),
        (problem_is_quadratic(Type) ->
            writeln(warning_output, "Eplex warning: the quadratic component of the objective is not retrieved.")
        ;
            true
        ).
            

% ----------------------------------------------------------------------
% Extras
% ----------------------------------------------------------------------

:- export reduced_cost_pruning/2.
reduced_cost_pruning(Handle, IpCost) :-
	call_priority(reduced_cost_pruning1(Handle, IpCost), 2).

% called under priority to make bound updates atomic
reduced_cost_pruning1(Handle, IpCost) :-
%	writeln(reduced_cost_pruning(IpCost)),
	Handle = prob{
%		cplex_handle:CPH,
		objsense:Sense,		% min:1, max:-1
		djs:Djs,
		vars:VList},
	get_var_bounds(IpCost, IpCostL, IpCostH),
	lp_get(Handle, cost, LpCost),
	% The gap size is rounded up by FeasibilityTol (a bit arbitrary).
	% When the reduced cost/objective function gradient is very shallow,
	% small rounding errors can have a large effect on the bound pruning.
	% By increasing the gap, we also allow the more uncertainty the
	% shallower the gradient is, which should be sensible.
	lp_get(Handle, optimizer_param(feasibility_tol), FeasibilityTol),
	( Sense > 0 ->	% minimizing
	    Gap is IpCostH-LpCost+FeasibilityTol
	;		% maximizing
	    Gap is LpCost-IpCostL+FeasibilityTol
	),
	( Gap < FeasibilityTol -> 
	    % Gap should be reasonably big to do this
	    true
	;
	    ( nonvar(Djs) ->		% we have the reduced costs anyway
                MFeasibilityTol is -FeasibilityTol,
                (% loop through the variable list
                    foreach(Var, VList),
                    param(FeasibilityTol,MFeasibilityTol,Gap,Handle)
                do
                    lp_var_get(Handle, Var, reduced_cost, RC),
                    ( RC > FeasibilityTol ->	% at lower bound
                          %		    var_range(Var, L, H),
                          lp_var_get_bounds(Handle, Var, L, H),
                          NewH is L + Gap/RC,
                          ( NewH >= H ->
                                true
                          ; NewH >= L ->
                                %			writeln(log_output, (x(I):(L..H),sol=Sol,rc=RC,obj=ObjC)),
                                %		    	writeln(log_output, update_possible:(H->NewH)),
                                lp_var_set_bounds(Handle, Var, L, NewH)
                          ;
                                writeln(warning_output, "Eplex warning: reduced_cost_pruning would cause failure"),
                                writeln(warning_output, (Var:(L..H)->(L..NewH),gap=Gap,rc=RC))
                          )
                    ; RC < MFeasibilityTol ->	% at upper bound
                          %		    var_range(Var, L, H),
                          lp_var_get_bounds(Handle, Var, L, H),
                          NewL is H + Gap/RC,
                          ( NewL =< L ->
                                true
                          ; NewL =< H ->
                                %			writeln(log_output, (x(I):(L..H)->(NewL..H),sol=Sol,rc=RC,obj=ObjC)),
                                %			writeln(log_output, update_possible:(L->NewL)),
                                lp_var_set_bounds(Handle, Var, NewL, H)
                          ;
                                writeln(warning_output, "Eplex warning: reduced_cost_pruning would cause failure"),
                                writeln(warning_output, (Var:(L..H)->(NewL..H),gap=Gap,rc=RC))
                          )
                    ;
                          true
                    )
                )
            ;
                writeln(warning_output, "Eplex warning: Reduced costs not available; cannot do reduced_cost_pruning...")
            )
        ).

eplex_verify_solution(VCs, VVs, Pool) :-
        get_pool_handle(Handle, Pool), !,
        lp_verify_solution(Handle, VCs, VVs).
eplex_verify_solution(VCs, VVs, Pool) :-
        printf(error, "Eplex error: instance %w has no associated"
                      " solver:%n", [Pool]),
        error(5, eplex_verify(VCs, VVs, Pool)).


lp_verify_solution(Handle, VCs, VVs) :-
        Handle = prob{cplex_handle:CPH,vars:VList,sols:SolArr},
        cplex_get_param(CPH, feasibility_tol, Tol),
        cplex_get_param(CPH, integrality, IntTol),
        cplex_get_prob_param(CPH, 0, Rows),
        VArr =.. [''|VList], % need random access to variables
        cplex_matrix_base(Base),
        MaxRow is Rows - cplex_matrix_offset,
        ( for(I,Base,MaxRow), param(VArr,CPH,Tol,Handle), 
          fromto(VCs, VC0,VC1, VCT1)
        do
            construct_and_verify_one_constraint(I,VArr,CPH,norm,Tol,Handle,VC0,VC1)
        ),
        get_last_solved_rawidxs(Handle, CPIdxs, _, _),
        ( foreach(I, CPIdxs), param(VArr,CPH,Tol,Handle), 
          fromto(VCT1, VC0,VC1, [])
        do
            construct_and_verify_one_constraint(I,VArr,CPH,condcp,Tol,Handle,VC0,VC1)
        ),
        cplex_get_prob_param(CPH, 1, Cols),
        ( foreacharg(V, VArr, I), param(CPH, Cols, Base, Tol, IntTol, SolArr), 
          fromto(VVs, VV0,VV1, []) 
        do
            VIdx is Cols - I + Base, %Vars are in reverse column order
            cplex_get_col_bounds(CPH, VIdx, Lo, Hi),
            get_darray_element(SolArr, VIdx, Val),
            (Val >= Lo - Tol, Val =< Hi + Tol -> 
                cplex_get_col_type(CPH, VIdx, TypeCode),
                cplex_type_code(T, TypeCode),
                (T == integer ->
                    Diff is abs(round(Val) - Val),
                    (Diff > IntTol -> VV0 = [vio(int,Diff,VIdx,V)|VV1] ; VV1 = VV0)
                ;
                    VV0 = VV1 
                )
            ; 
                (Val < Lo - Tol -> VioBound = lower ; VioBound = upper),
                (Val < Lo -> Diff is Lo - Val ; Diff is Val - Hi),
                VV0 = [vio(VioBound,Diff,VIdx,V)|VV1]
            )
        ).

   construct_and_verify_one_constraint(I, VArr, CPH, Type, Tol, Handle, VC0, VC1) :-
        constraint_type_code(Type, TypeCode),
        construct_one_constraint(CPH, TypeCode, I, VArr, C),
        term_variables(C, CVs),
        copy_term((C,CVs), (CCopy,CVsCopy), _), % omit attributes!
        ( foreach(V, CVs), foreach(VCopy, CVsCopy),
          param(Handle) do
            lp_var_solution(Handle, V, VCopy)
        ),
        verify_one_constraint(CCopy, Tol, Diff),
        (Diff == satisfied -> 
            VC0 = VC1 
        ; 
            convert_rawidx_to_row_index(Type, Handle, I, RowIdx),
            VC0 = [vio(Type,Diff,RowIdx,C)|VC1]
        ).

   verify_one_constraint(Sense:[Cst*1|Lhs], Tol, Diff) :-
        Rhs is -Cst,
        LhsSum is sum(Lhs),
        Expr =.. [Sense,LhsSum,Rhs],
        (call(Expr) ->
            Diff = satisfied
        ;
            Diff0 is abs(LhsSum - Rhs),
            (Diff0 > Tol -> Diff = Diff0 ; Diff = satisfied)
            
        ).    

% ----------------------------------------------------------------------
% Interface to the C procedures
% ----------------------------------------------------------------------

cplex_objsense(min(Expr), 1, Expr).
cplex_objsense(max(Expr), -1, Expr).

cplex_objsense(min, 1).
cplex_objsense(max, -1).

% V <Op> Bound
cplex_bound_code((=<),  1).
cplex_bound_code((=:=), 0).
cplex_bound_code((>=), -1).

cplex_problem_code(lp,   0).
cplex_problem_code(mip,  1).
cplex_problem_code(qp,   2).
cplex_problem_code(miqp, 3).
cplex_problem_code(fixedlp, 4).
cplex_problem_code(fixedqp, 5).
cplex_problem_code(relaxedlp, 6).
cplex_problem_code(relaxedqp, 7).

cplex_mip_code(1).	% mip
cplex_mip_code(3).	% miqp

% correspondance between linear and quad. problems
linear_quadratic(lp, qp).
linear_quadratic(mip, miqp).
linear_quadratic(fixedlp, fixedqp).
linear_quadratic(relaxedlp, relaxedqp).

% quadratic problem types
problem_is_quadratic(qp).
problem_is_quadratic(miqp).
problem_is_quadratic(fixedqp).
problem_is_quadratic(relaxedqp).
        

% Superset of possible methods (i.e. supported by at least one solver)
% cplex_method_codes(+Name, -MethCode, -AuxMethCode)
% cplex_method_codes(-Name, +MethCode, +AuxMethCode)
% Code must correspond to METHOD_XXX definitions in C
cplex_method_codes(default,		-1, -1) :- !.
cplex_method_codes(auto,		0, -1) :- !.
cplex_method_codes(primal,		1, -1) :- !.
cplex_method_codes(dual,		2, -1) :- !.
cplex_method_codes(net,			3, -1) :- !.
cplex_method_codes(net(default),	3, -1) :- !.
cplex_method_codes(net(auto),		3,  0) :- !.
cplex_method_codes(net(primal),		3,  1) :- !.
cplex_method_codes(net(dual),		3,  2) :- !.
cplex_method_codes(net_primal,		3,  1) :- !.	% backward compatbility
cplex_method_codes(net_dual,		3,  2) :- !.	% backward compatbility
cplex_method_codes(barrier,		4, -1) :- !.
cplex_method_codes(barrier(default),	4, -1) :- !.
cplex_method_codes(barrier(auto),	4,  0) :- !.
cplex_method_codes(barrier(primal),	4,  1) :- !.
cplex_method_codes(barrier(dual),	4,  2) :- !.
cplex_method_codes(barrier(none),	4,  8) :- !.
cplex_method_codes(barrier_primal,	4,  1) :- !.	% backward compatbility
cplex_method_codes(barrier_dual,	4,  2) :- !.	% backward compatbility
cplex_method_codes(sifting,		5, -1) :- !.
cplex_method_codes(sifting(default),	5, -1) :- !.
cplex_method_codes(sifting(auto),	5,  0) :- !.
cplex_method_codes(sifting(primal),	5,  1) :- !.
cplex_method_codes(sifting(dual),	5,  2) :- !.
cplex_method_codes(sifting(net),	5,  3) :- !.
cplex_method_codes(sifting(barrier),	5,  4) :- !.
cplex_method_codes(concurrent,		6, -1) :- !.
cplex_method_codes(concurrent_det,	7, -1) :- !.


:- mode cplex_type_code(-, ++).
cplex_type_code(integer, 0'B).
cplex_type_code(integer, 0'I).
cplex_type_code(real, 0'C).

:- mode type_to_cplex_type(?,++,++,-).
type_to_cplex_type(T, _, _, 0'C) :- var(T), !.
type_to_cplex_type(integer, Lo, Hi, T) :-
	( Lo=0.0, Hi=1.0 ->
	    T = 0'B
	;
	    T = 0'I
	).
type_to_cplex_type(real, _, _, 0'C).


constraint_type_code(norm,    0).  % normal problem constraint
constraint_type_code(permcp,  1).  % unconditional cutpool constraint
constraint_type_code(condcp,  2).  % conditional cutpool constraint

% status code for cutpool constraints -- must correspond to the CSTR_STATE_*
% macros in the C code. These apply to cutpool constraints that have not 
% been added to the problem.
cp_cstr_state_code(violated, -1).
cp_cstr_state_code(satisfied, -2).
cp_cstr_state_code(binding, -3).
cp_cstr_state_code(invalid, -4).
cp_cstr_state_code(inactive, -5).
                      
% cutpool constraint conditions that can be set. Code (second arg)
% must correspond to C code in cplex_set_cpcstr_cond()
cp_cond_code(active, 1).
cp_cond_code(add_initially, 2).

mipstart_code(none, 0).
mipstart_code(all, 1).
mipstart_code(integers, 2).


:- mode set_qobj_coeffs(+,++).
set_qobj_coeffs(CPH, QObjCoeffs0) :-
        (lp_get(optimizer, osi) ->
            % sort for OSI, as quadratic terms need to be in sparse matrix form
            sort(1, =<, QObjCoeffs0, QObjCoeffs)
        ;
            QObjCoeffs0 = QObjCoeffs
        ),
        set_qobj_coeffs_r(CPH, QObjCoeffs).


:- mode set_qobj_coeffs(+,++).
set_qobj_coeffs_r(_CPH, []).
set_qobj_coeffs_r(CPH, [q(I,J,C)|CXs]) :-
	cplex_set_qobj_coeff(CPH, I, J, C),
	set_qobj_coeffs_r(CPH, CXs).

:- mode obj_coeffs(+,+,-).
obj_coeffs([], _,[]).
obj_coeffs([C*X|CXs], SId, [J:C|JCs]) :-
        % need to place new obj. coeffs to one column only
        (get_first_var_index(X, SId, J) -> 
             true
        ;
             printf(error, "Eplex error: a non-problem variable %w occurs"
                    " in the objective.%n", [X]),
             flush(error),
             abort
        ),
	obj_coeffs(CXs, SId, JCs).

:- mode set_obj_coeffs(+,++).
set_obj_coeffs(_, []).
set_obj_coeffs(CPH, [J:C|JCs]) :-
	cplex_set_obj_coeff(CPH, J, C),
	set_obj_coeffs(CPH, JCs).

:- mode chg_obj_coeffs(+,++).
chg_obj_coeffs(CPH, []) :-
	cplex_flush_obj(CPH).
chg_obj_coeffs(CPH, [J:C|JCs]) :-
	cplex_new_obj_coeff(CPH, J, C),
	chg_obj_coeffs(CPH, JCs).

:- mode clr_obj(+,++).
clr_obj(CPH, []) :-
	cplex_flush_obj(CPH).
clr_obj(CPH, [J:_C|JCs]) :-
	cplex_new_obj_coeff(CPH, J, 0),
	clr_obj(CPH, JCs).

:- mode qobj_coeffs(+,+,-).
qobj_coeffs([], _, []).
qobj_coeffs([[C,X,Y]|CXs], SId, [q(I,J,C)|JCs]) :-
        % qobj coeffs added to one pair of cols only
        (get_first_var_index(X, SId, I),
         get_first_var_index(Y, SId, J) ->
             qobj_coeffs(CXs, SId, JCs)
        ;
             printf(error, "Eplex error: a non-problem variable %w or %w occurs"
                    " in the quadratic objective.%n", [X,Y]),
             flush(error),
             abort
        ).

:- mode chg_qobj_coeffs(+,++).
chg_qobj_coeffs(_CPH, []).
chg_qobj_coeffs(CPH, [q(I,J,C)|JCs]) :-
	cplex_new_qobj_coeff(CPH, I, J, C),
	chg_qobj_coeffs(CPH, JCs).

:- mode clr_qobj(+,++).
clr_qobj(_CPH, []).
clr_qobj(CPH, [q(I,J,_C)|JCs]) :-
	cplex_new_qobj_coeff(CPH, I, J, 0),
	clr_qobj(CPH, JCs).

        
set_rhs(_, [], _).
set_rhs(CPH, [Sense:Val|Cs], I) :-
	I1 is I+1,
	cplex_set_rhs_coeff(CPH, I, Sense, Val),
	set_rhs(CPH, Cs, I1).

% initialise the type of a new column to an integer type
set_type_integer(_, _, []).
set_type_integer(CPH, SId, [X|Xs]) :-
        % new var: no merged cols
        ( get_unique_var_index(X, SId, J) ->
              cplex_get_col_bounds(CPH, J, Lo, Hi),
              type_to_cplex_type(integer, Lo, Hi, TypeCode),
              cplex_init_type(CPH, J, TypeCode)
	; var(X) ->
              printf(warning_output, "Eplex warning: integer variable not a problem variable (ignored): %mVw%n", [X])
	; integer(X) ->
	      true
	; number(X) ->
	      % nonvar: make sure it's sufficiently integral
	      abs(round(X) - X) =< cplex_get_param(CPH, integrality)
	;
              printf(error, "Eplex error: integer variable unified to"
                            " a non-number:.%w%n", [X]),
              throw(abort)
        ),
	set_type_integer(CPH, SId, Xs).

% Set initial bounds for new variables from one-variable constraints
set_initial_bounds(_, _, []).
set_initial_bounds(CPH, SId, [Sense:[Cst*_,C*X]|BdCstrs]) ?-
	Bound is float(-Cst/C),
	swap_sense(C, Sense, Sense1),
	get_unique_var_index(X, SId, J),
	cplex_init_bound(CPH, J, Sense1, Bound),
	set_initial_bounds(CPH, SId, BdCstrs).

% Update bounds for old variables from one-variable constraints.
% Return list of changed columns.
update_bounds(_, [], []).
update_bounds(Handle, [Sense:[Cst*_,C*X]|BdCstrs], CCs) ?-
	Bound is float(-Cst/C),
	swap_sense(C, Sense, Sense1),
	get_lp_attr(X, Handle, Attr),
	Attr = eplex{idx: [I|_]},	%%% enough?
	impose_col_bounds(Handle, Attr, I, Bound, Changed, Sense1),
	( Changed==1 -> CCs = [I|CCs1] ; CCs=CCs1 ),
	update_bounds(Handle, BdCstrs, CCs1).

% synchronise bounds if requested
sync_bounds_mat(yes, Vars, Handle, CPH) :-
        cplex_get_prob_param(CPH, 1, Max),
        (foreach(V,Vars), fromto(Max, I0,I, _), param(Handle) do
             I is I0 - 1,
             (number(V) ->
                  % if V is a number, no need to do anything here
                  % (bounds for column imposed by eplex unify handler)
                  true
             ;
                  get_var_bounds(V, Lo, Hi), 
                  % no need to check if bounds changed or not; as we are
                  % are in the process of invoking the solver here.
                  get_lp_attr(V, Handle, Attr),
                  cplex_impose_col_bounds(Handle, Attr, I, 1, Lo, Hi, _)
             )
        ).
sync_bounds_mat(no, _, _, _).
             

set_mat(CPH, Cols, K, J, Jmax) :-
	( J >= Jmax ->
	    true
	;
	    J1 is J+1,
	    arg(J1, Cols, Col),
	    set_mat_col(CPH, Col, K, K1),	% move to set_bounds_mat
	    cplex_set_matbeg(CPH, J, K, K1),	%
	    set_mat(CPH, Cols, K1, J1, Jmax)
	).

set_mat_col(_, T, K, K) :- var(T), !.
set_mat_col(_, [], K, K).
set_mat_col(CPH, [I:CIJ|More], K0, K) :-
	cplex_set_matval(CPH, K0, I, CIJ),
	K1 is K0+1,
	set_mat_col(CPH, More, K1, K).


set_sos(prob{cplex_handle:CPH,solver_id:SId}, SosList) :-
	set_sos_list(CPH, SId, SosList).

    set_sos_list(_CPH, _, []).
    set_sos_list(CPH, SId, [Sos|SosList]) :-
	( Sos = sos1(Vars), clean_sos1(Vars, PureVars) ->
	    vars_to_cols(PureVars, SId, Cols, 0, N),
	    cplex_add_new_sos(CPH, 1, N, Cols)
	; Sos = sos2(Vars), clean_sos2(Vars, PureVars) ->
	    vars_to_cols(PureVars, SId, Cols, 0, N),
	    cplex_add_new_sos(CPH, 2, N, Cols)
	;
	    printf(error, "Eplex error: error in SOS setup: %w%n", [Sos]),
            abort
	),
	set_sos_list(CPH, SId, SosList).

    vars_to_cols([], _, [], N, N).
    vars_to_cols([X|Xs], SId, [J|Js], N0, N) :-
	( get_first_var_index(X, SId, J) ->
	    N1 is N0+1,
	    vars_to_cols(Xs, SId, Js, N1, N)
	; var(X) ->
	    writeln(error, "Eplex error: SOS contains non-problem variable during setup"),
	    abort
	;
	    writeln(error, "Eplex error: SOS contains non-variable during setup"),
	    abort
	).

    % Preprocess an SOS1:
    %  - drop zeros, keep only variables
    %  - single nonzero: set all others to zero
    %  - several nonzeros: fail
    %
    % we could do something similar for SOS2, but much more tricky:
    %  - X=0: drop the 0, make sos2 + sos1 with left/right neighbour
    %  - X=1: make sos1 with left/right neighbour, others 0

    clean_sos1(ElemsColl, NewElems) :-
	collection_to_list(ElemsColl, Elems),
	(
	    foreach(E,Elems),
	    fromto(Vars,Vars1,Vars0,[]),
	    param(Nonzero)
	 do
	     ( var(E) ->
		 Vars1 = [E|Vars0]
	     ;
	     	X is E,		% evaluate subscripts etc
		( var(X) ->
		    Vars1 = [X|Vars0]
		 ; X =:= 0 ->
		     Vars1 = Vars0	% drop zeros
		 ;
		     var(Nonzero),   % else fail (more than one nonzero)
		     Nonzero = X,
		     Vars1 = Vars0
		 )
	     )
	 ),
	 ( var(Nonzero) ->
	     NewElems = Vars
	 ;
	     NewElems = [],
	     ( foreach(V,Vars) do V = 0 )
	 ).

    % preliminary
    clean_sos2(ElemsColl, Vars) :-
	collection_to_list(ElemsColl, Elems),
	(
	    foreach(E,Elems),
	    fromto(Vars,Vars1,Vars0,[])
	do
	    ( var(E) ->
		Vars1 = [E|Vars0]
	    ;
	     	X is E,		% evaluate subscripts etc
		Vars1 = [X|Vars0]
	    )
	).


raw_to_typed_solution(CPH, ArrSize, RawArr, SolArr) :-
        % SolArr: structure in column order from 1..ArrSize
        cplex_matrix_offset(OffSet), 
        (for(I, 1, ArrSize),
         param(CPH,RawArr,SolArr,OffSet) do
            ColIdx is I - OffSet,
            get_typed_solution_for_col(CPH, ColIdx, RawArr, TVal),
            arg(I, SolArr, TVal)
        ).



% ----------------------------------------------------------------------
% Variable names
%-----------------------------------------------------------------------

load_varnames(yes, NAdded, CPH, SId, Vars) ?-
        (count(_, 1, NAdded), fromto(Vars, [Var|Vars0],Vars0, _), 
         param(CPH, SId) do
             (get_unique_var_index(Var, SId, ColJ), % new var
              var_name:get_var_name(Var, Name) ->
                  cplex_load_varname(CPH, ColJ, Name)
             ;
                  true
             )
        ).
load_varnames(no, _,  _, _, _).


% ----------------------------------------------------------------------
% Expression simplifier (using lib(linearize))
%
% A linear expression is normalised into a list (sum) of the form
%	[C0*1, C1*X1, C2*X2, ...]
% where Ci are numbers and Xi are distinct variables.
% The first (constant) term is always present, Ci (i>=1) are nonzero.
% The expression must be built from variables, numbers, +/2, */2, -/2, -/1.
% The simplifier fails if the expression is not linear.
%
% renormalise/2 renormalises a normal form expression after variable
% bindings, unifications.
%
% A normalised constraint has the form
%	Sense:NormExpr
% where Sense is one of the atoms =:=, >= or =< and NormExpr is a
% normalised expression as above. E.g. (>=):[-5*1,3*X] encodes
% the constraint  -5 + 3*X >= 0.
% ----------------------------------------------------------------------

% Fails for var, unknown, and nonlinear constraints 
normalise_cstr(Cstr, _) :- var(Cstr), !,
	fail.
normalise_cstr(Cstr, (=:=):Norm) :- 
        Cstr = (E1 =:= E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).
normalise_cstr(Cstr, (>=):Norm) :- 
        Cstr = (E1 >= E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).
normalise_cstr(Cstr, (=<):Norm) :- 
        Cstr = (E1 =< E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).
normalise_cstr(Cstr, (=:=):Norm) :- 
        Cstr = (E1 $= E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).
normalise_cstr(Cstr, (>=):Norm) :- 
        Cstr = (E1 $>= E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).
normalise_cstr(Cstr, (=<):Norm) :- 
        Cstr = (E1 $=< E2),
	linearize(E1-E2, Norm, Residue),
        warn_nonlinear(Residue, Cstr).


normalise_cstrs(Cs, _, _) :- var(Cs), !,
	fail.
normalise_cstrs([], [], []).
normalise_cstrs([C|Cs], NormLins, NonLins) :-
	( normalise_cstr(C, Cnorm) ->
	    NormLins = [Cnorm|NormLins1],
	    normalise_cstrs(Cs, NormLins1, NonLins)
	;
	    NonLins = [C|NonLins1],
	    normalise_cstrs(Cs, NormLins, NonLins1)
	).

renormalise_cstr(Sense:Denorm, Sense:Norm) :-
	linrenorm(Denorm, Norm).

renormalise_cstrs([], []).
renormalise_cstrs([C|Cs], [N|Ns]) :-
        renormalise_cstr(C, N),
        renormalise_cstrs(Cs, Ns).


    warn_nonlinear(Residue, Cstr) :-
        (Residue = [] -> 
             true
        ;
             printf(warning_output, "Eplex warning: Unable to linearise %w%n", [Cstr]),
	     writeln(warning_output, "Constraint is either non-linear, or has incorrect syntax (missing brackets?)"),
             fail
        ).


% Normalise indicator constraint, fail if invalid
% idc(Complemented:0..1, Bool:UncheckedExpression, LinNorm:NormConstraint),
normalise_idc(Indicator=>Lin, Norm) ?-
	Norm = idc(Complemented, Bool, LinNorm),
	( nonvar(Indicator), Indicator=neg(Bool) -> Complemented=1
	; Bool=Indicator, Complemented=0
	),
	normalise_cstr(Lin, LinNorm).


% ----------------------------------------------------------------------
% Normal form -> standard expressions
% ----------------------------------------------------------------------

denormalise_cstr([], []).
denormalise_cstr([Norm|Norms], [Cstr|Cstrs]) :-
	denormalise_cstr(Norm, Cstr),
	denormalise_cstr(Norms, Cstrs).
denormalise_cstr(Sense:SimpEx0, Cstr) :-
	linrenorm(SimpEx0, SimpEx1),
	SimpEx1 = [Cst*_|Lhs],
	delinearize(Lhs, LhsExpr),
	Rhs is -Cst,
	Cstr =.. [Sense, LhsExpr, Rhs].


% ----------------------------------------------------------------------
% Interface to the polynomial simplifier
% ----------------------------------------------------------------------

	% Call polynomial simplifier and return separately:
	% - constant
	% - linear part in [C1*X1|...] format as defined above
	% - quadratic part
	% fails if not quadratic
quadnorm(Expr, Const, Lin, Quad) :-
	quadnorm(Expr, Const, LinNew, Quad, [], []),
	( foreach([C,X], LinNew), foreach(C*X, Lin) do true ).


%-----------------------------------------------------------------------
% Change variable support
%-----------------------------------------------------------------------
lp_suspend_on_change(Handle, _Var, Susp):-
        nonvar(Handle), Handle = prob{},
        suspend_on_change_handle(Handle, Susp).
suspend_on_change(_Var, Susp, Pool):-
        get_pool_handle(Handle, Pool),
        suspend_on_change_handle(Handle, Susp).

    suspend_on_change_handle(Handle, Susp):-
        enter_suspension_list(change_suspensions of prob, Handle, Susp).

lp_get_changeable_value(Handle, Var, Val):-
        nonvar(Handle), Handle = prob{},
        get_changeable_value_handle(Handle, Var, Val).
get_changeable_value(Var, Val, Pool):-
        get_pool_handle(Handle, Pool),!,
        get_changeable_value_handle(Handle, Var, Val).
get_changeable_value_handle(Handle, Var, Val):-
        (catch(lp_var_get(Handle, Var, typed_solution, Val),abort,true)->
             true
        ;
             true
        ).

%-----------------------------------------------------------------------
% Infeasible analysis
%-----------------------------------------------------------------------

eplex_get_iis(NCRows, NCCols, CIdxs, CVs, Pool) :-
        get_pool_handle(Handle, Pool),
        !,
        lp_get_iis(Handle, NCRows, NCCols, CIdxs, CVs).
eplex_get_iis(NCRows, NCCols, CIdxs, CVs, Pool) :-
	printf(error, "Eplex error: instance %w has no associated"
               " solver:%n", [Pool]),
        error(5, eplex_get_iis(NCRows, NCCols, CIdxs, CVs, Pool)).

lp_get_iis(Handle, NCRows, NCCols, CstrIdxs, CVs) :-
        Handle = prob{mr:MR,cp_cond_map:Map,iis_rows:CRowIdxs,iis_cols:CColIdxs,iis_colstats:CColStatus},
        ( array(CRowIdxs) ->
            % IIS computed eagerly on failure and associated with the IIS arrays
            iarray_size(CRowIdxs, NCRows),
            iarray_size(CColIdxs, NCCols),
            iarray_list(CRowIdxs, CRowIdxLst),
            ( foreach(RIdx, CRowIdxLst), param(Map,MR),
              foreach(CIdx, CstrIdxs)  do
                matidx_cstridx(RIdx, Map, MR, CIdx)
            ),
	
            %        lp_get1(Handle, constraints_norm(CRowIdxLst), Cstrs),
            iarray_list(CColIdxs, CColIdxLst),
            (CColIdxLst \== [] ->
                lp_get1(Handle, vars, VArr),
                ( foreach(ColIdx, CColIdxLst), param(VArr,CColStatus),
                  foreach(V:StatusAtom, CVs), count(I, 1,_) do
                    string_code(CColStatus, I, StatusCode),
                    char_code(StatusAtom, StatusCode),
                    Pos is ColIdx + 1,
                    arg(Pos, VArr, V)
                )
            ;
                CVs = []
            )
        ; CRowIdxs == [] ->
            NCRows = 0,
            NCCols = 0,
            CstrIdxs = [],
            CVs = [],
            writeln(warning_output, 
                    "Eplex warning: no IIS information is available."
                    " This is either because 1) the current solve did"
                    " not fail, or 2) the solver does not support IIS.")
        ;
            writeln(error, "Eplex error: iis must be requested via"
                         " cache_iis  option before solving."),
            error(6, lp_get_iis(Handle, NCRows, NCCols, CstrIdxs, CVs))
	).

%-----------------------------------------------------------------------
% cutpool support
%-----------------------------------------------------------------------

% rawidx_cstridx(+CType, +RawIdx, -CstrIdx)
% rawidx_cstridx(-CType, -RawIdx, ++CstrIdx)
% mapping between constraint index to the `raw' index (i.e. index into
% the correct constraint type as specified by CType)
rawidx_cstridx(0, Idx, Idx) :- integer(Idx), !.
rawidx_cstridx(1, Idx, g(1,Idx)) :- !, integer(Idx).
rawidx_cstridx(2, Idx, g(2,Idx)) :- integer(Idx).

matidx_cstridx(MatIdx, Map, MR, CIdx) :-
        ( MatIdx >= MR ->
            Idx is MatIdx - MR,
            % find each pool cstr independently -- not efficient if need 
            % to find many
            find_cstridx(Idx, Map, 0, CIdx)
	;
            CIdx = MatIdx
	).

find_cstridx(Idx, Map, RIdx0, CIdx) :-
	get_iarray_element(Map, RIdx0, Delta),
	( Idx =:= Delta ->
            constraint_type_code(condcp, CType),
            rawidx_cstridx(CType, RIdx0, CIdx)
	;
            RIdx1 is RIdx0 + 1,
            find_cstridx(Idx, Map, RIdx1, CIdx)
	).
/*
matidx_cstridx(Idx, CIdx, Map) :-
	cplex_get_prob_param(CPH, 0, NRows),
	NCPRows is NRows - MC,
	functor(MCMap, '', NCPRows),
	iarray_size(Map, MSize),
	Last is MSize - 1,
	(for(J,0,Last), param(Map, MCMap) do
		get_iarray_element(Map, J, Delta),
		( Delta >= 0 ->
			MIdx is Delta + 1,
			arg(MIdx, MCMap, J)
		;
			true
		)
	)
*/

get_named_cp_index(Handle, Name, Idx) :-
        atom(Name),
        arg(cplex_handle of prob, Handle, CPH),
        cplex_get_named_cp_index(CPH, Name, 0, Idx).
        

cutpool_selection_to_rawidxs(cstr(Idx0), _Handle, Idxs) ?-
        rawidx_cstridx(_CType, RIdx, Idx0),
        Idxs = [RIdx].
cutpool_selection_to_rawidxs(group(Name), Handle, Idxs) ?-
        arg(cplex_handle of prob, Handle, CPH),
        cplex_get_named_cp_index(CPH, Name, 0, PIdx),
        cplex_get_named_cpcstr_indices(CPH, PIdx, Idxs).
cutpool_selection_to_rawidxs(last_added, Handle, Idxs) ?-
        get_last_solved_rawidxs(Handle, Idxs, _, _).
cutpool_selection_to_rawidxs(last_notadded, Handle, Idxs) ?-
        get_last_solved_rawidxs(Handle, _, Idxs, _).
cutpool_selection_to_rawidxs(last_inactive, Handle, Idxs) ?-
        get_last_solved_rawidxs(Handle, _, _, Idxs).


% Added Index, Unadded Index, Inactive Index
get_last_solved_rawidxs(Handle, AIdxs, UIdxs, IIdxs) ?-
        arg(cp_cond_map of prob, Handle, Map),
        iarray_size(Map, Size),
        Last is Size - 1,
        (for(J,0,Last), param(Map),
         fromto([], AIdxs0,AIdxs1, AIdxs), 
         fromto([], IIdxs0,IIdxs1, IIdxs),
         fromto([], UIdxs0,UIdxs1, UIdxs)  do
            get_iarray_element(Map, J, Delta),
            ( Delta >= 0 -> % active, added
                AIdxs1 = [J|AIdxs0],
		UIdxs0 = UIdxs1,
                IIdxs0 = IIdxs1
            ;
                % unadded -- can be active or inactive
                cp_cstr_state_code(State, Delta),
                (State == inactive ->
                    IIdxs1 = [J|IIdxs0],
                    AIdxs0 = AIdxs1,
                    UIdxs0 = UIdxs1
                ;
                    % active, unadded
                    UIdxs1 = [J|UIdxs0],
                    AIdxs0 = AIdxs1,
                    IIdxs0 = IIdxs1
                )
            )
        ).

    get_cp_cstrs_info(IType, CType, RawIdxs, Handle, Info) :-
        arg(cplex_handle of prob, Handle, CPH),
        Info = Idxs-Is,
	cp_cond_code(IType, ICode),
        (foreach(RI,RawIdxs), param(ICode,CType,CPH), 
         foreach(Idx,Idxs), foreach(I, Is) do
            rawidx_cstridx(CType, RI, Idx),
            cplex_get_cpcstr_info(CPH, RI, ICode, I)
        ).

lp_get_cutpool_info(index, CType, RawIdxs, _Handle, Idxs) ?-
        (foreach(RI, RawIdxs), param(CType), foreach(Idx, Idxs) do
            rawidx_cstridx(CType, RI, Idx)
        ).
lp_get_cutpool_info(add_initially, CType, RawIdxs, Handle, Info) ?-
        get_cp_cstrs_info(add_initially, CType, RawIdxs, Handle, Info).
lp_get_cutpool_info(active, CType, RawIdxs, Handle, Info) ?-
        get_cp_cstrs_info(active, CType, RawIdxs, Handle, Info).
lp_get_cutpool_info(binding_state, CType, RawIdxs, Handle, Info) ?-
        Handle = prob{cplex_handle:CPH,cp_cond_map:Map,slacks:Slacks,mr:MR},
        (var(Slacks) ->
            writeln(error, "Eplex error: Slack values required to get"
                    " binding state information for cutpool constraints."),
            abort
        ;
            true
        ),
        cplex_get_param(CPH, feasibility_tol, Tol),
        Info = Idxs-States,
        (foreach(RI,RawIdxs),param(Map,Slacks,MR,Tol,CType),
         foreach(Idx,Idxs), foreach(BS, States) do
            rawidx_cstridx(CType, RI, Idx),
            get_iarray_element(Map, RI, D),
            ( D >= 0 -> 
                /* constraint was added - check its slack value */
                RowI is MR + D,
                get_darray_element(Slacks, RowI, S),
                (abs(S) =< Tol -> BS = binding ; BS = satisfied)
            ; 
                /* constraint was not added */
                cp_cstr_state_code(BS, D)
            )
        ).
lp_get_cutpool_info(constraints_norm, CType, RawIdxs, Handle, Info) ?-
	Handle = prob{cplex_handle:CPH,vars:VList},
        Info = Idxs-NCs,
        VArr =.. [''|VList], % need random access to variables
        (foreach(RI, RawIdxs), param(CType,VArr,CPH),
         foreach(Idx, Idxs), foreach(NC, NCs) do
                rawidx_cstridx(CType, RI, Idx),
                construct_one_constraint(CPH, CType, RI, VArr, NC)
        ).
lp_get_cutpool_info(constraints, CType, RawIdxs, Handle, Info) ?-
        lp_get_cutpool_info(constraints_norm, CType, RawIdxs, Handle, 
            Idxs-NCs),
        Info = Idxs-Cs,
        denormalise_cstr(NCs, Cs).

% currently allows only setting of states, may allow more complex
% conditions in the future
lp_set_cp_option(Option, Handle, Idx, Value) ?-
	rawidx_cstridx(CType, RIdx, Idx),
	constraint_type_code(condcp, CType),
        valid_cp_optval(Option, Value, Handle, ActVal),
	cp_cond_code(Option, OType),
        arg(cplex_handle of prob, Handle, CPH),
	cplex_set_cpcstr_cond(CPH, RIdx, OType, ActVal).


%-----------------------------------------------------------------------
% utility predicates
%-----------------------------------------------------------------------

is_list_or_nil([_|_]) ?- true.
is_list_or_nil([])    ?- true.

%-----------------------------------------------------------------------
% Pools
%-----------------------------------------------------------------------

:- local record(lp_pools). % list of lp pool names

create_eplex_pool(Pool) :-
	create_constraint_pool(Pool, property(arity) of constraint_type, [
	    (=:=)/2 -> lp_eq/3,
	    (>=)/2 -> lp_ge/3,
	    (=<)/2 -> lp_le/3,
	    ($=)/2 -> lp_eq/3,
	    ($>=)/2 -> lp_ge/3,
	    ($=<)/2 -> lp_le/3,
            ($::)/2 -> lp_interval/3,
            (::)/2 ->  lp_interval/3,
            integers/1 -> integers/2,
            reals/1 -> reals/2,
            sos1/1 -> sos1/2,
            sos2/1 -> sos2/2,
            (=>)/2 -> indicator_constraint/3,
            piecewise_linear_hull/3 -> piecewise_linear_hull/4,
            suspend_on_change/2 -> suspend_on_change/3,
            get_changeable_value/2 -> get_changeable_value/3,
	    eplex_solver_setup/1 -> eplex_solver_setup/2,
	    eplex_solver_setup/4 -> eplex_solver_setup_cbody/5, 
	    eplex_solver_setup/5 -> eplex_solver_setup_cbody/6, % obsolete
	    eplex_probe/2 -> eplex_probe/3,
	    eplex_solve/1 -> eplex_solve/2,
	    eplex_get/2 -> eplex_get/3,
            eplex_set/2 -> eplex_set/3,
	    eplex_var_get/3 -> eplex_var_get/4,
            eplex_var_get_bounds/3 -> eplex_var_get_bounds/4,
            eplex_add_constraints/2 -> eplex_add_constraints/3,
            eplex_read/2 -> eplex_read/3,
            eplex_write/2 -> eplex_write/3,
            eplex_get_iis/4 -> eplex_get_iis/5,
            eplex_verify_solution/2 -> eplex_verify_solution/3,
	    eplex_cleanup/0 -> eplex_cleanup/1
	]).


eplex_instance(PoolName) :-
	( is_constraint_pool(PoolName),
	  recorded(lp_pools, PoolName) % is a lp pool
	->
            % if pool exists, check if it is currently empty 
	    ( pool_is_empty(PoolName),
	      get_pool_item(PoolName, 0) % has no associated solver
	    ->
		true
	    ;
		printf(error, "Eplex error: instance still in use in %w%n", [eplex_instance(PoolName)]),
		abort
	    )
	;

            (recorded(lp_pools, PoolName) -> 
                 printf(error, "Eplex error: cannot create the eplex instance in %w as it may"
                        " have been erased earlier.%n", [eplex_instance(PoolName)]),
                 abort
            ;    record(lp_pools, PoolName)
            ),
            create_eplex_pool(PoolName)
	).


lp_pool_associate_solver(PoolName, Handle) :-
	get_pool_item(PoolName, 0), % does not already have a solver
	set_pool_item(PoolName, Handle),
	Handle = prob{pool:PoolName}.


get_pool_handle(Handle, Pool) :-
	get_pool_item(Pool, Handle),
	Handle = prob{}.


pool_has_no_solver(Pool) :-
        ( is_constraint_pool(Pool), 
          recorded(lp_pools, Pool) ->  
              ( get_pool_item(Pool, 0) -> true
              ; 
                printf(error, "Eplex error: Attempting to associate a"
                       " solver with eplex instance %w, which already has"
                       " an associated solver.%n", [Pool]),
                abort
              )
        ;
              printf("Eplex error: Invalid eplex instance %w specified"
                     " during set up.%n", [Pool]),
              abort
        ).


    % Dummy predicate for forwarding piecewise_linear_hull/3 to the
    % appropriate solver.  It would be nice if create_constraint_pool/3
    % allowed module qualification in the spec list, so that this extra
    % indirection were not needed...  We could just import the piecewise
    % module, but then it would always be loaded, rather than only when it's
    % actually used.
piecewise_linear_hull(X, Points, Y, Pool) :-
        eplex_relax:piecewise_linear_hull(X, Points, Y, Pool).


%-----------------------------------------------------------------------
% Compatibility: solver's idea of integer tolerance
%-----------------------------------------------------------------------
int_tolerance(Tol) :-
	cplex_get_param(0, integrality, Tol).


% ----------------------------------------------------------------------
% Try to grab a licence
% ----------------------------------------------------------------------

?- ( is_predicate(lp_get_license/0), lp_get_license -> true ; true).


