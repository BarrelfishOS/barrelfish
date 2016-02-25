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
%
% eclipse -e "[makerefs],do"
%
% preprocesses files to insert (or update) hyperlinks to bip pages:
%
%	\bip{n/a}
%	{\bf n/a}
%	\bipref{n/a}{oldurl}
%
%  -->	\bipref{n/a}{url}
%
% and
%	\biptxt{text}{n/a}
%     	\biptxtref{text}{n/a}{oldurl}
%
%  -->	\biptxtref{text}{n/a}{url}
%
% and
%       \txtbip{text}{n/a}
%       \txtbipref{text}{n/a}{oldurl}
%  -->  \txtbipref{text}{n/a}{url}
%
% txtbip* and biptxt* differs in what gets put into the index:
% In txtbip*, it is text that is put into the index; 
% In biptxt*, it is n/a that is put into the index.
% Both uses n/a to find the url.

:- ['../../doc/bips/index'].
:- set_chtab(0'`, list_quote).

do :-
	read_directory(., "*.tex", _, Files),
%	argv(all, [_|Files]),
	do(Files).

do([]).
do([F|Fs]) :-
	do(F),
	do(Fs).
do(File) :-
	string(File),
	printf("%s ... %b", [File]),
	open(File, read, S),
	read_string(S, "", _, String),
	close(S),
	string_list(String, List),

	get_flag(pid, Pid),
	concat_string(["tmp",Pid], TmpFile),
	open(TmpFile, write, out),
	( top(List, []) ->
	    concat_string([File,".bak"],BakFile),
	    rename(File,BakFile),
	    rename(TmpFile,File)
	;
	    printf("**** FAILED on file %s\n", [File])
	),
	writeln(done),
	close(out).


top -->
	`\\bip{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    emit_bipref([], "bipref", BipString)
	},
	!,
	top.
top -->
	`\\biptxt{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref([], TextString, "biptxtref", BipString)
	},
	!,
	top.
top -->
	`\\txtbip{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref([], TextString, "txtbipref", BipString)
	},
	!,
	top.
%top -->
%	`{\\bf `,
%	latexarg(BipList),	% consumes closing }
%	{≈o
%	    string_list(BipString, BipList),
%	    emit_bipref([], BipString)
%	},
%	!,
%	top.
top -->
	`\\bipref{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(OldUrlList),
	{
	    string_list(BipString, BipList),
	    emit_bipref(OldUrlList, "bipref", BipString)
	},
	!,
	top.
top -->
	`\\biprefni{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(OldUrlList),
	{
	    string_list(BipString, BipList),
	    emit_bipref(OldUrlList, "biprefni", BipString)
	},
	!,
	top.
top -->
	`\\biprefnoidx{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(OldUrlList),
	{
	    string_list(BipString, BipList),
	    emit_bipref(OldUrlList, "biprefnoidx", BipString)
	},
	!,
	top.
top -->
	`\\biptxtref{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(OldUrlList),
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(OldUrlList, TextString, "biptxtref", BipString)
	},
	!,
	top.
top -->
	`\\biptxtrefni{`,
	latexarg(TextList),	% consumes closing }
	`{`,
	latexarg(BipList),	% consumes closing }
	`{`,
	latexarg(OldUrlList),
	{
	    string_list(BipString, BipList),
	    string_list(TextString, TextList),
	    emit_bipref(OldUrlList, TextString, "biptxtrefni", BipString)
	},
	!,
	top.
top -->
	[C],
	{ put(out, C) },
	top.
top --> [].

latexarg([]) --> `}`, !.
latexarg([0'{|Cs]) --> `{`, !, latexarg1(Cs, Cs1), latexarg(Cs1). 
latexarg([C|Cs]) --> [C], latexarg(Cs).

% consume recursively to a matching }
latexarg1([0'}|Cs], Cs) --> `}`, !.
latexarg1([0'{|Cs0], Cs) --> `{`, latexarg1(Cs0, Cs1), latexarg1(Cs1, Cs).  
latexarg1([C|Cs0], Cs) --> [C], latexarg1(Cs0, Cs).

process_bip_spec((N0/A0), N, A, _) ?- !, N0 = N, A0 = A.
process_bip_spec((N0/A0,_), N, A, _) ?- !, N0 = N, A0 = A.
process_bip_spec((SubGroup0:N0/A0), N, A, SubGroup) ?- !, 
        N0 = N, A0 = A, SubGroup0 = SubGroup.
process_bip_spec(!(N0/A0,SubGroup0), N, A, SubGroup) ?- !, 
        N0 = N, A0 = A, SubGroup0 = SubGroup.
process_bip_spec(library(Name), N, A, _) ?- !,
        N = Name, A = index.
process_bip_spec(lib(Name), N, A, _) ?- !,
        N = Name, A = index.
process_bip_spec(Name, N, A, _) ?- atomic(Name), !,
        N = Name, A = index.

emit_bipref(OldUrl, BipTxtType, BipString) :-
	try_term_string(Bip, BipString),
        process_bip_spec(Bip, N, A, SubGroup), % may fail
	nonvar(N), nonvar(A),
	findbip(N, A, SubGroup, BipInfo),
	!,
        select_bip_info(N, A, OldUrl, SubGroup, BipInfo, HtmlFile),
	printf(out, "\\%s{%s}{%s}", [BipTxtType,BipString,HtmlFile]).
emit_bipref(_, BipString) :-
	printf("*** Could not find %s%n", [BipString]),
	fail.

emit_bipref(OldUrl, Text, BipTxtType, BipString) :-
	try_term_string(Bip, BipString),
        process_bip_spec(Bip, N, A, SubGroup), % may fail
	nonvar(N), nonvar(A),
%        findbip(N, A, Group, SubGroup, File0),
        findbip(N, A, SubGroup, BipInfo),
	!,
        select_bip_info(N, A, OldUrl, SubGroup, BipInfo, HtmlFile),
	printf(out, "\\%s{%s}{%s}{%s}", [BipTxtType,Text,BipString,HtmlFile]).
emit_bipref(_, _, _, BipString) :-
	printf("*** Could not find %s%n", [BipString]),
	fail.

select_bip_info(N, A, OldUrl, SubGroup, BipInfo, HtmlFile) :-
        ( OldUrl == [] ->
            select_one_bip_info_and_warn(N, A, SubGroup, BipInfo, HtmlFile)
        ;
            string_list(OldUrlString, OldUrl),
            ( match_bip_info(BipInfo, OldUrlString) ->
                % found a match in BipInfo to OldUrl, use the OldUrl
                HtmlFile = OldUrlString
            ;
                select_one_bip_info_and_warn(N, A, SubGroup, BipInfo, HtmlFile)
            )
        ).

match_bip_info([f(File,Group,SubGroup)|Tail], OldUrlString) :-
        make_html_file(File, Group, SubGroup, HtmlFile),
        ( HtmlFile == OldUrlString -> 
            true
        ;
            match_bip_info(Tail, OldUrlString)
        ).

make_html_file(File0, Group, SubGroup, HtmlFile) :-
        ( File0=='' -> File = index ; File = File0 ),
        concat_string(["../bips/",Group,/,SubGroup,/,File,".html"], HtmlFile).
            
select_one_bip_info_and_warn(N, A, SubGroup0, [f(File,Group,SubGroup)|Tail], HtmlFile) :-
        (var(SubGroup0) -> UnkSubG = yes ; UnkSubG = no),
        (Tail == [] -> true
        ;
         UnkSubG == no -> printf("*** More than one predicate match %w:%w/%w%n*** Group %w used.%n", [SubGroup,N,A,Group])
        ;
         UnkSubG == yes, printf("*** More than one predicate match %w/%w%n*** Group %w-%w used.%n", [N,A,Group,SubGroup])
        ),
        make_html_file(File, Group, SubGroup, HtmlFile).
        
        
findbip(N, A, SubGroup, BipInfo) :-   % Group, SubGroup, File) :-
        findall(f(File0,Group0,SubGroup), bip(N, A, Group0, SubGroup, File0), BipInfo),
        BipInfo = [_|_]. % at least one match

try_term_string(T, S) :-
	set_error_handler(114, fail/0),
	set_error_handler(115, fail/0),
	set_error_handler(117, fail/0),
	set_error_handler(119, fail/0),
	set_error_handler(198, fail/0),
	set_error_handler(7, fail/0),
	current_op(Prec1,Assoc1,(',')),
	current_op(Prec2,Assoc2,(/)), !,
	current_op(Prec3,Assoc3,(:)), !,
	op(1200,yfx,(',')),
	op(1199,yfx,(:)),
	op(1199,yfx,(!)),
	op(1198,yfx,(/)),
	set_flag(macro_expansion,off),
	( term_string(T, S) ->
%	    printf("Parsed: %q\n", [T]),
	    true
	;
	    printf("*** Couldn't parse: %q\n", [S]),
	    T=S
	),
	op(Prec1,Assoc1,(',')),
	op(Prec2,Assoc2,(/)),
	op(Prec3,Assoc3,(:)),
	reset_error_handler(7),
	reset_error_handler(114),
	reset_error_handler(115),
	reset_error_handler(117),
	reset_error_handler(198),
	reset_error_handler(119).


