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
% The Original Code is  The csv library for ECLiPSe.
% The Initial Developer of the Original Code is  Joachim Schimpf.
% Portions created by the Initial Developer are
% Copyright (C) 2010.  All Rights Reserved.
% 
% END LICENSE BLOCK

%
% Utilities for working with comma-separated format (csv).
%
% We have used as reference http://www.rfc-editor.org/rfc/rfc4180.txt
%
% Grammar from rfc4180:
%
%   file = [header CRLF] record *(CRLF record) [CRLF]
%   header = name *(COMMA name)
%   record = field *(COMMA field)
%   name = field
%   field = (escaped / non-escaped)
%   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
%   non-escaped = *TEXTDATA
%   COMMA = %x2C
%   CR = %x0D ;as per section 6.1 of RFC 2234 [2]
%   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]
%   LF = %x0A ;as per section 6.1 of RFC 2234 [2]
%   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]
%   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
%
% Notes:
% - Line ends are supposed to be CRLF, but e.g. OpenOffice has only LF,
%   so we allow that as well.
% - Can have optional header line with field names (special file type),
%   but there is no way of indicating this in the format
% - Spaces are part of the field (not sure about ..., "a" ,...)
% - Fields containing line breaks (CRLF), double quotes, and commas
%       should be enclosed in double-quotes
% - Otherwise no double quotes allowed
%

:- module(csv).

:- comment(summary, "Utilities to manipulate comma-separated (csv) format").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2015/01/14 01:31:08 $").
:- comment(copyright, "2010 by author").
:- comment(categories, ["Interfacing"]).

:- comment(csv_read/3, [
    summary:"Read a file containing comma separated values (csv format)",
    args:["File":"File name (string or atom)",
        "Rows":"List of lists (output)",
        "Options":"List of options"],
    amode:(csv_read(+,-,+) is det),
    desc:html("<p>
        Reads a file containing comma separated values, and returns the
        file content as a list.  The file may have an optional .csv suffix.
    </p><p>
        The result list contains one element for each record in the file.
        Each list element is itself a list, representing a data record.
        The data elements are either numbers (if they can be interpreted
        as numbers by ECLiPSe's number_string/2 predicate), or otherwise
        strings.
    </p><p>
        No options are currently supported.
    </p>
    "),
    see_also:[number_string/2,csv_read_row/2]
]).

:- export csv_read/3.

csv_read(File, Rows, _Options) :-
        ( existing_file(File, ["",".csv"], [readable], XFile) ->
            open(XFile, read, S),
            ( read_string(S, end_of_file, _, String) ->
                close(S),
                % add a terminating LF, if it is missing...
                string_length(String, Len),
                ( substring(String, "\012", Len) ->
                    string_list(String, Line, utf8)
                ;
                    append_strings(String, "\012", StringLf),
                    string_list(StringLf, Line, utf8)
                ),
                ( records(Rows, Line, []) ->
                    true
                ;
                    printf(error, "Invalid csv file: %w%n", [File]),
                    abort
                )
            ;
                close(S),
                Rows = []
            )
        ;
            printf(error, "No such file: %w%n", [File]),
            abort
        ).


records(Records) -->
        ( record(Record) ->
            { Records = [Record|Records1] },
            records(Records1)
        ;
            { Records = [] }
        ).

record([Field|Fields]) -->
        field(Field),
        ( [0',] ->
            record(Fields)
        ; crlf,
            { Fields = [] }
        ).

field(Field) -->
        ( [0'"] ->
            escaped(FieldChars)
        ;
            non_escaped(FieldChars)  % may be empty field
        ),
        {
            % convert to number if possible (should be an option)
            string_list(FieldString, FieldChars),
            split_string(FieldString, "", " \t", [StrippedString]),
            ( number_string(Field, StrippedString) ->
                true
            ;
                Field = FieldString
            )
        }.

escaped(Cs) -->
        ( [16'22] ->
            ( [16'22] ->
                { Cs = [16'22|Cs1] },
                escaped(Cs1)
            ;
                { Cs = [] }
            )
        ;
            [C],
            { Cs = [C|Cs1] },
            escaped(Cs1)
        ).


non_escaped(Cs) -->
        ( textdata(C) ->
            { Cs = [C|Cs1] },
            non_escaped(Cs1)
        ;
            { Cs = [] }
        ).
 
textdata(C) -->
        [C],
        { C >= 16'20, C \== 16'22, C \== 16'2C }.

crlf --> [16'D, 16'A], !.
crlf --> [16'A].              % occurs in OpenOffice csv output



%----------------------------------------------------------------------
% Approximate reading of single rows, using ECLiPSe parser
% We do this by declaring LF a terminator, changing character classes,
% and reading as a term with read/2.
% Limitations:
%       - cannot handle empty fields ,,
%----------------------------------------------------------------------

:- comment(csv_read_row/2, [
    summary:"Read one row of comma separated values (approximate)",
    args:["Stream":"Stream name or handle",
        "RowList":"List of numbers and strings, or 'end_of_file' (output)"],
    amode:(csv_read_row(+,-) is det),
    desc:html("<p>
        Reads one row of comma separated values from Stream, and returns
        the result as a list.  On end of file, returns the atom 'end_of_file'.
    </p><p>
        The result list contains one element for each field in the record.
        The data elements are either numbers (if they can be interpreted
        as numbers by ECLiPSe's number_string/2 predicate), or otherwise
        strings.
    </p><p>
        Shortcomings: as oppsed to csv_read/3, this predicate here uses the
        ECLiPSe parser to read rows, and does not implement the cvs format
        fully.  E.g. empty fields are not handled and yield a syntax error.
    </p>
    "),
    see_also:[number_string/2,csv_read/3]
]).

:- export csv_read_row/2.
csv_read_row(Stream, RowList) :-
        read(Stream, RowTerm)@csv_syntax,
        ( RowTerm == end_of_file ->
            RowList = RowTerm
        ;
            comma_to_list(RowTerm, RowList)
        ).

comma_to_list((A,BC), ABCs) ?- !, ABCs = [AN|BCs],
        convert_type(A, AN),
        comma_to_list(BC, BCs).
comma_to_list(A, [AN]) :-
        convert_type(A, AN).

    convert_type(AS, NS) :-
        ( atom(AS) -> atom_string(AS, S) ; S = AS ),
        ( number_string(N, S) -> NS=N ; NS=S ).


:- module(csv_syntax).

:- local initialization((
        (
            % make dummy underline and atom_quote characters, because
            % ECLiPSe doesn't allow them to be undefined.
            local(chtab(0,underline)),
            local(chtab(1,atom_quote)),
            between(2,255,1,C),
            char_table(C, Class),
            local(chtab(C,Class)),
            fail
        ;
            true
        ),
        local(syntax_option(doubled_quote_is_quote))
    )).

char_table(0'\n, terminator) :- !.
char_table(0'\r, blank_space) :- !.	% ignored
char_table(0',, special) :- !.
char_table(0'", string_quote) :- !.
char_table(_Other, lower_case).		% treat everything else like a letter

