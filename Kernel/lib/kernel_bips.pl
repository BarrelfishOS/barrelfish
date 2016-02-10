% ----------------------------------------------------------------------
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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% 
% ECLiPSe kernel built-ins
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: kernel_bips.pl,v 1.6 2014/07/11 02:30:18 jschimpf Exp $
%
% ----------------------------------------------------------------------

% Part of module sepia_kernel

:- system.

:- export
	substring/4,
	substring/5,
	sub_string/5,
	string_concat/3,
	append_strings/3,
	keysort/2,
	sort/2,
	number_sort/2,
	msort/2,
	merge/3,
	number_merge/3,
	prune_instances/2,
        wait/2.


%----------------------------------------------------------------------
% String builtins
%----------------------------------------------------------------------


:- export string_code/3.
string_code(Index, String, Code) :-
	string_code(Index, String, Code, 1).	% nondet


:- export string_char/3.
string_char(Index, String, Char) :-
	( var(Index) ->
	    string_length(String, Length),
	    between(1, Length, 1, Index), 
	    get_string_code(Index, String, Code),
	    char_code(Char, Code)
	; integer(Index) ->
	    get_string_code(Index, String, Code),
	    char_code(Char, Code)
	;
	    error(5, string_code(Index, String, Char))
	).



/* append_strings(S1, S2, S3) iff String3 is the concatenation of
* String1 and String2
* Periklis Tsahageas/18-8-88
* implements BSI's specification :
* all arguments strings or variables, otherwise type error
* if var(S3) and [var(S1) or var(S2)] : instantiation fault.
* i.e. the normal prolog relation without infinite backtracking.
*/

% alias for compatibility with SWI
string_concat(X,Y,Z) :-
	append_strings(X,Y,Z).

append_strings(X,Y,Z) :-
	( var(Z) ->
	    concat_strings(X,Y,Z)

	; string(Z) ->
	    ( var(X) ->
		( var(Y) ->
		    string_list(Z, ZL),
		    append(XL, YL, ZL),
		    string_list(X, XL),
		    string_list(Y, YL)

		; string(Y) ->
		    string_length(Y, Ylen),
		    Xlen is string_length(Z) - Ylen,
		    Ypos is Xlen + 1,
		    first_substring(Z, Ypos, Ylen, Y),	% may fail
		    first_substring(Z, 1, Xlen, X)
		;
		    error(5, append_strings(X,Y,Z))
		)
	    ; string(X) ->
		( var(Y) ->
		    string_length(X, Xlen),
		    first_substring(Z, 1, Xlen, X),	% may fail
		    Ypos is Xlen + 1,
		    Ylen is string_length(Z) - Xlen,
		    first_substring(Z, Ypos, Ylen, Y)

		; string(Y) ->
		    concat_strings(X,Y,Z)
		;
		    error(5, append_strings(X,Y,Z))
		)
	    ;
	    	error(5, append_strings(X,Y,Z))
	    )
	;
	    error(5, append_strings(X,Y,Z))
	).


% substring(+String, ?Pos, ?Len, ?SubStr) :-
%
% This predicate conforms to the BSI substring/4 specification.
% That's why all the error checks are there.
% We implement it using the deterministic builtin
% first_substring(+String, +Pos, +Len, ?SubStr).

substring(String, Pos, Len, SubStr) :-
	check_string(String),
	( var(Pos) -> 
	    true
	;
	    integer(Pos) ->
	    ( (Pos > 0) -> 
		true
	    ;
		set_bip_error(6)
	    )
	;
	    set_bip_error(5)
	),
	check_var_or_arity(Len),
	check_var_or_string(SubStr),
	!,
	(string(SubStr)->string_length(SubStr, Len); true),
	Total is string_length(String) + 1,
	( integer(Pos) ->
	    ( integer(Len) -> 
		true
	    ;
		MaxLen is Total - Pos,
		between(0, MaxLen, 1, Len)
	    )
	;
	    ( integer(Len) ->
		MaxPos is Total - Len,
		between(1, MaxPos, 1, Pos)
	    ;
		between(1, Total, 1, Pos),
		MaxLen is Total - Pos,
		between(0, MaxLen, 1, Len)
	    )
	),
	first_substring(String, Pos, Len, SubStr).

substring(String, Pos, Len, SubStr) :-
	get_bip_error(ErrorCode),
	error(ErrorCode, substring(String, Pos, Len, SubStr)).


% substring(+String, ?Before, ?Length, ?After, ?SubString) :-
%
% This predicate is true iff string 'String' can be split
% into three pieces, 'StringL', 'SubString' and 'StringR'. 
% In addition it must be split so that 'Before' is the length 
% of string 'StringL', 'Length' is the length of string 
% 'SubString' and 'After' is the length of the string 'StringR'.
% We implement it using the deterministic builtin
% first_substring(+String, +Pos, +Len, ?SubStr).

% alias for compatibility with SWI
sub_string(String, Before, Length, After, SubString) :-
	substring(String, Before, Length, After, SubString).

substring(String, Before, Length, After, SubString) :-
	check_string(String),
	check_var_or_arity(Before),
	check_var_or_arity(Length),
	check_var_or_arity(After),
	check_var_or_string(SubString),
	!,
	(string(SubString)->string_length(SubString, Length); true),
	StringLength is string_length(String),
	( integer(Before) ->
	    ( integer(Length) ->
		( integer(After) ->
		    StringLength =:= Before + Length + After 
		; % 'After' is a var!
		    After is StringLength - Before - Length,
		    After >= 0
		)
	    ; % 'Length' is a var!
		( integer(After) ->
		    Length is StringLength - Before - After,
		    Length >= 0
		; % 'Length' and 'After' are vars!
		    MaxLength is StringLength - Before,
		    between(0, MaxLength, 1, Length),
		    After is MaxLength - Length
		)
	    )
	; % 'Before' is a var!
	    ( integer(Length) ->
		( integer(After) ->
		    Before is StringLength - Length - After,
		    Before >= 0
		; % 'Before' and 'After' are vars!
		    MaxBefore is StringLength - Length,
		    between(0, MaxBefore, 1, Before),
		    After is MaxBefore - Before
		)
	    ; % 'Before' and 'Length' are vars!
		( integer(After) ->
		    MaxBefore is StringLength - After,
		    between(0, MaxBefore, 1, Before),
		    Length is MaxBefore - Before
		; % 'Before', 'Length' and 'After' are vars!
		    between(0, StringLength, 1, Before),
		    MaxLength is StringLength - Before,
		    between(0, MaxLength, 1, Length),
		    After is StringLength - Before - Length
		)
	    )
	),
        % first_substring/4 uses position, not index, so add 1.
	Pos is Before + 1,
	first_substring(String, Pos, Length, SubString).

substring(String, Before, Length, After, SubString) :-
	get_bip_error(ErrorCode),
	error(ErrorCode, substring(String, Before, Length, After, SubString)).


:- export string_list/3.
string_list(String, List, Format) :- var(Format), !,
	error(4, string_list(String, List, Format)).
string_list(String, List, utf8) :- !,
	utf8_list(String, List).
string_list(String, List, bytes) :- !,
	string_list(String, List).
string_list(String, List, octet) :- !,
	string_list(String, List).
string_list(String, List, codes) :- !,
	string_list(String, List).
string_list(String, List, chars) :- !,
	string_chars(String, List).
string_list(String, List, Format) :-
	error(6, string_list(String, List, Format)).


:- export string_codes/2.	% SWI compatibility
string_codes(String, Codes) :-
	string_list(String, Codes).

:- export string_chars/2.	% SWI compatibility
string_chars(String, List) :-
	( var(String) ->
	    check_chars_list(List),
	    !,
	    concat_string(List, String)
	; string(String) ->
	    !,
	    ( for(I,1,string_length(String)), foreach(C,List), param(String) do
		string_code(String, I, Code),
		char_code(C, Code)
	    )
	;
	    error(5, string_chars(String, List))
	).
string_chars(String, List) :-
	bip_error(string_chars(String, List)).

    check_chars_list(X) :- var(X), !, set_bip_error(4).
    check_chars_list([]) :- !.
    check_chars_list([H|T]) :- !,
	check_char(H),
	check_chars_list(T).
    check_chars_list(_) :-
	set_bip_error(5).

    check_char(X) :- var(X), !, set_bip_error(4).
    check_char(X) :- atom(X), !,
	( atom_length(X, 1) -> true ; set_bip_error(6) ).
    check_char(_) :-
	set_bip_error(5).


%----------------------------------------------------------------------
% Sort builtins
%----------------------------------------------------------------------

:- skipped
	keysort/2,
	sort/2,
	number_sort/2,
	msort/2,
	merge/3,
	number_merge/3,
	prune_instances/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
:- mode
	keysort(+, -),	
	merge(+, +, -),
	msort(+, -),
	sort(+, -),
	number_sort(+, -),
	prune_instances(+, -).
*/


keysort(R, S) :-
	sort(1, =<, R, S).


msort(R, S) :-
	sort(0, =<, R, S).


sort(R, S) :-
	sort(0, <, R, S).


number_sort(R, S) :-
	number_sort(0, =<, R, S).


merge(A, B, M) :-
	merge(0, =<, A, B, M).


number_merge(A, B, M) :-
	number_merge(0, =<, A, B, M).


prune_instances(List, Pruned) :-
	% sorting the list first is not necessary, but likely to reduce
	% the number of instance checks because duplicates are removed,
	% identical functors are grouped together, and variables are
	% moved to the front.
	sort(List, PreSortedList),
	prune_instances(PreSortedList, [], Pruned).

:- mode prune_instances(+,+,?).
prune_instances([First|Rest], SoFar, Result) :-
	insert_pruned(First, SoFar, NewSoFar),
	prune_instances(Rest, NewSoFar, Result).
prune_instances([], Result, Result).

% insert elem into the list (which is itself pruned)
:- mode insert_pruned(?,+,-).
insert_pruned(Elem, [], [Elem]).
insert_pruned(Elem, [First|Rest], Result) :-
	( instance(Elem, First) ->
	    Result = [First|Rest]		% already subsumed by list
	; instance(First, Elem) ->
	    Result = [Elem|Res0],		% replace first instance
	    remove_instances(Elem, Rest, Res0)	% remove any others
	;
	    Result = [First|Res0],
	    insert_pruned(Elem, Rest, Res0)	% keep checking
	).

:- mode remove_instances(?,+,-).
remove_instances(_Elem, [], []).
remove_instances(Elem, [First|Rest], Result) :-
	( instance(First, Elem) ->
	    remove_instances(Elem, Rest, Result)
	;
	    Result = [First|Res0],
	    remove_instances(Elem, Rest, Res0)
	).

%----------------------------------------------------------------------
% OS builtins
%----------------------------------------------------------------------

wait(Pid, Status) :-
	wait(Pid, Status, hang).
