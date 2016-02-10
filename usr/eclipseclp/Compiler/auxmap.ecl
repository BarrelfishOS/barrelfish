% Compiling this file installs macros which rename
% the compiler modules during compilation, prefixing 'aux_'

rename_module(Name, AuxName) :-
	concat_atom(['aux_',Name], AuxName).

:- global
	macro(compiler_map,		rename_module/2, []),
	macro(compiler_analysis,	rename_module/2, []),
	macro(compiler_codegen,		rename_module/2, []),
	macro(compiler_common,		rename_module/2, []),
	macro(compiler_indexing,	rename_module/2, []),
	macro(compiler_normalise,	rename_module/2, []),
	macro(compiler_peephole,	rename_module/2, []),
	macro(compiler_regassign,	rename_module/2, []),
	macro(compiler_varclass,	rename_module/2, []),
	macro(ecl_compiler,		rename_module/2, []),
	macro(source_processor,		rename_module/2, []).
