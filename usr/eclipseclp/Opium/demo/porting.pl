/*
 *  predicates helpful to port buggy programs from Muprolog to
 *  Sepia.
 */

% Mireille.
% these are predicates copied almost straight away from ~sepia/workdir/sepia/lib/cio.pl
% loaded that way they are local. I think it is better.

:- import
	is_predicate_/2,
	put0/1,
	see1/1,
	tell1/1,
	write_/2
   from sepia_kernel.

see(File) :-
	see1(File),
	(File \== user ->
		set_stream(File, input)
	;
		true
	).

seeing(File) :-
	current_stream(File, _, input).

seen :-
	seeing(File),
	(File \== user -> close(File); true),
	set_stream(input, stdin).

tell(File) :-
	tell1(File),
	(File \== user ->
		set_stream(File, output)
	;
		true
	).

telling(File) :-
	current_stream(File, _, output).

told :-
	telling(File),
	(File \== user -> close(File); true),
	set_stream(output, stdout).

/*
:- skipped
	see/1,
	seeing/1,
	seen/0,
	skip/1,
	tab/1,
	tell/1,
	telling/1,
	told/0.
*/

% for the LOB programs

% tells the parser to read strings as lists of ascii
% to mimic the muprolog's way of handling strings
% !!! this is a global change (in all modules)
set_all_strings_to_lists :-
	set_chtab(128, string_quote),		% make something else the string quote
	set_chtab(0'", list_quote).		% make double quote the list quote

% Sets back " " to quote strings
set_back_strings_to_strings:- 
	set_chtab(0'", string_quote).


strings_to_lists([], []).
strings_to_lists([S|SL], [L|LL]) :-
	string_to_list(S, L),
	strings_to_lists(SL, LL).

string_to_list("", []) :- !.
string_to_list(String, [HI | Tail]) :-
	substring(String, 1, 1, Head),
	char_int(Head, HI),
	string_length(String, L),
	TL is L - 1,
	substring(String, 2, TL, ST),
	string_to_list(ST, Tail).

lists_to_strings([], []).
lists_to_strings([L|LL], [S|SL]) :-
	list_to_string(L, S),
	lists_to_strings(LL, SL).

list_to_string([], "") :- !.
list_to_string([H | T], String) :-
	char_int(HS, H),
	list_to_string(T, TS),
	concat_strings(HS, TS, String).
