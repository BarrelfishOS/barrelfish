%---------------------------------------------------------------------------%
% Copyright (C) 2000 Imperial College London.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB.
%---------------------------------------------------------------------------%

% This module implements a few generic Mercury predicates, useful for
% porting Mercury library modules to ECLiPSe.

:- module(mercury).

:- export
	error/1,
	report_lookup_error/3.

error(String) :-
	writeln(error, String),
	abort.

report_lookup_error(String, Key, Value) :-
	writeln(error, String),
	write(error, "Key: "), writeq(error, Key), nl(error),
	write(error, "Value: "), writeq(error, Value), nl(error),
	abort.

:- comment(categories, ["Compatibility"]).
:- comment(summary, "Mercury compatibility predicates.").
:- comment(author, "Warwick Harvey").
:- comment(desc, html("\
	<P>
	This module provides (a few) Mercury compatibility predicates,
	useful for when porting Mercury modules to ECLiPSe.
	</P>
	")).

:- comment(error/1, [
	amode:		error(++),
	args:		["Message":"A string giving the error message to display"],
	summary:	"Abort, printing an error message.",
	fail_if:	"Never fails.",
	resat:		no,
	desc:		html("\
	<P>
	Corresponds to Mercury's require:error/1 predicate.
	</P>
	")
]).

:- comment(report_lookup_error/3, [
	amode:		report_lookup_error(++, ++, ?),
	args:		["Message":"A string giving the error message to display",
			"Key":"The key that wasn't found",
			"Value":"The corresponding value, if supplied"],
	summary:	"Report a lookup error and abort.",
	fail_if:	"Never fails.",
	resat:		no,
	desc:		html("\
	<P>
	Corresponds to Mercury's require:report_lookup_error/3 predicate.
	</P>
	")
]).

