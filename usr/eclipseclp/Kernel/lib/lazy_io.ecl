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
% The Original Code is  The lazy_io library for ECLiPSe.
% The Initial Developer of the Original Code is  Coninfer Ltd.
% Portions created by the Initial Developer are
% Copyright (C) 2010.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------

:- module(lazy_io).

:- comment(categories, ["Programming Utilities"]).
:- comment(summary, "Mapping between lists and I/O streams").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Joachim Schimpf, Coninfer Ltd").
:- comment(date, "$Date: 2011/04/09 23:10:53 $").

%----------------------------------------------------------------------
% Input
%----------------------------------------------------------------------

:- comment(stream_to_lazy_list/2, [
    summary:"Map an input stream to a lazy list",
    args:["Stream":"A stream handle",
    	"Codes":"A variable or (possibly partial) list"],
    amode:(stream_to_lazy_list(+,?) is semidet),
    see_also:[list_to_stream/3,char_code/2,string_list/2],
    desc:html("<P>
    	This predicate makes the content of Stream available as a list of
	character codes, similar to reading the whole stream content into
	a list, and then using the list.  However, in this lazy version
	the list is materialized in an incremental fashion, i.e.  only
	when its elements are created by the list processing code, are
	the character codes read from the stream and filled in.  For most
	purposes, this lazy list can be used like a normal list, e.g. in
	parsing with DCGs.  The advantage of the lazy version is that at
	no time is it necessary to have the whole list in memory: the tail
	is created lazily from the stream, and the front will be garbage
	collected when no longer needed.
	</P><P>
	The stream must be open in read-mode.  The list will reflect the
	stream content starting from the position the stream is at when
	stream_to_lazy_list/2 is called.  The list will end with the last
	character in the stream.  If the stream content changes while the
	list is partly materialised, the list content is undefined.
	</P><P>
	The stream should not be closed as long as it is possible to backtrack
	to a point between the call to stream_to_lazy_list/2 and the
	termination of the list.  In most cases, it is not necessary to close
	the stream explicitly at all, because it will be closed automatically
	on garbage collection or on failure.
	</P><P>
	NOTE about cuts in current ECLiPSe versions: If a unification is
	followed immediately by a cut, and the unification causes waking,
	the waking occurs only after the cut.  A unification that creates
	an element of the lazy list may therefore need a dummy 'true' goal
	before a subsequent cut in order to force waking.
    </P>"),
    eg:"
    copy_file(Old, New) :-
	open(Old, read, In),
	open(New, write, Out),
	stream_to_lazy_list(In, Codes),
	( foreach(C,Codes), param(Out) do put(Out, C) ),
	close(In),
	close(Out).
    "
]).

:- export stream_to_lazy_list/2.

stream_to_lazy_list(Stream, Codes) :-
	at(Stream, Pos),
	stream_to_lazy_list(Stream, 1024, Pos, Codes).
%	stream_to_lazy_list(Stream, Pos, Codes).

delay stream_to_lazy_list(_Stream, _ChunkSize, _Pos, Codes) if var(Codes).
stream_to_lazy_list(Stream, ChunkSize, Pos, Codes) :-
	seek(Stream, Pos),
	( read_string(Stream, "", ChunkSize, String) ->
	    string_list(String, List),
	    append(List, Codes1, Codes),
	    at(Stream, Pos1),
	    stream_to_lazy_list(Stream, ChunkSize, Pos1, Codes1)
	;
	    Codes = []
	).

/*
% The same, but reading only one character at a time
% (slow because it wakes up for every list element)
delay stream_to_lazy_list(_Stream, _Pos, Codes) if var(Codes).
stream_to_lazy_list(Stream, Pos, Codes) :-
	seek(Stream, Pos),
	get(Stream, C),
	( C < 0 ->
	    Codes = []
	;
	    Codes = [C|Codes1],
	    at(Stream, Pos1),
	    stream_to_lazy_list(Stream, Pos1, Codes1)
	).
*/

:- export stream_to_list/3.
stream_to_list(Stream, Codes, eager) ?-
	( read_string(Stream, end_of_file, _, String) ->
	    string_list(String, Codes)
	;
	    Codes = []
	).
stream_to_list(Stream, Codes, lazy) ?-
	stream_to_lazy_list(Stream, Codes).
	

%----------------------------------------------------------------------
% Output
%----------------------------------------------------------------------

:- comment(list_to_stream/3, [
    summary:"Map a list to an output stream",
    args:["Codes":"A (possibly partial) list of character codes",
    	"Stream":"A stream handle",
	"Mode":"One of the atoms 'incremental' or 'delayed'"],
    amode:(list_to_stream(?,+,+) is det),
    see_also:[stream_to_lazy_list/2,open/4,char_code/2,string_list/2],
    desc:html("<P>
	This predicates maps a list of character codes to an output
	stream.  When list_to_stream/3 is called, the list Codes may
	be uninstantiated, or it may be a partial list.  Once the list
	is terminated, and the Stream is closed, the file will reflect
	the last state of the list.  As long as the stream is not
	closed, the list may be arbitrarily extended, shortened on
	backtracking, terminated, un-terminated on backtracking, etc. 
	Only once the stream is closed using close/1, is the stream
	contents committed to reflect the list (provided the list has
	been terminated).  This behaviour is independent of the Mode
	argument.
</P><P>
	Mode 'incremental' should be used if the list may become very long.
	In this mode the list is copied into the stream incrementally, such
	that the front of the list can be garbage collected.
</P><P>
	Mode 'delayed' may be slightly more efficient, since it copies the
	list contents to the stream only once the list is terminated.  This
	will however use more memory, since the whole list needs to be held
	in memory until it is terminated.
</P><P>
	The Stream must be open in write-mode.  In addition it is advisable
	to set the delete_file(when_lost) option when opening - this will
	have the effect of eventually deleting the associated file in case
	the stream is never explicitly closed.
</P><P>
	If the stream is closed without the list being terminated, the stream
	content is essentially undefined.  In practice, with 'incremental'
	mode the stream will reflect the partial list as it was when the
	stream was closed.  With 'delayed' mode, the stream will be empty
	or reflect an earlier terminated state that was backtracked over.
</P>
    "),
    eg:"
    write_list(File, N, Code) :-
	open(File, write, Out, [delete_file(when_lost)]),
	list_to_stream(Codes, Out, incremental),
	( for(_,1,N), foreach(Code,Codes), param(Code) do true ),
	close(Out).
    "
]).

% Transfer a list to a stream, 

:- export list_to_stream/3.

list_to_stream(Codes, Stream, Mode) :-
	( get_stream_info(Stream, delete_file, when_lost) -> true
	; printf(warning_output, "Stream should have %w option%n", [delete_file(when_lost)])
	),
	at(Stream, Pos),
	( Mode == incremental ->
	    incr_list_to_stream(Codes, Pos, Stream)
	; Mode == delayed ->
	    delayed_list_to_stream(Codes, Codes, Pos, Stream)
	).


% Transfer a list to a stream as we go along.  The list will never
% get long, as it is eagerly transferred to the stream (buffer).
% When backtracking and reconstructing the list in a different way,
% the file will be updated (modulo buffering and flushing).
% The file is only committed when the stream is closed.

delay incr_list_to_stream(Codes,_,_) if var(Codes).
incr_list_to_stream([], Pos, Stream) :-
	%flush(Stream),
	seek(Stream, Pos),
	stream_truncate(Stream).
incr_list_to_stream([C|Cs], Pos, Stream) :-
	seek(Stream, Pos),	% possibly back in file (after failure)
	put(Stream, C),
	at(Stream, Pos1),
	%( var(Cs) -> flush(Stream) ; true ),
	incr_list_to_stream(Cs, Pos1, Stream).


% Transfer a list to a stream, but only once the list is terminated.
% You can backtrack over the termination, re-build the list and terminate
% again, and the file will be updated.  This works as long as the stream
% is not closed (at which time it is committed for good).
% The list can get long as it is fully constructed.

delay delayed_list_to_stream(_,Codes,_,_) if var(Codes).
delayed_list_to_stream(Codes, [], Pos, Stream) :-
	seek(Stream, Pos),
	( foreach(C,Codes), param(Stream) do put(Stream, C) ),
	stream_truncate(Stream).
delayed_list_to_stream(Codes, [_C|Cs], Pos, Stream) :-
	delayed_list_to_stream(Codes, Cs, Pos, Stream).


end_of_file.

% ----------------------------------------------------------------------
% Test code
% ----------------------------------------------------------------------

% Sample use case: file copy

copy_file(Old, New) :-
	open(Old, read, In),
	open(New, write, Out),
	stream_to_lazy_list(In, Codes),
	( foreach(C,Codes), param(Out) do put(Out, C) ),
	close(In),
	close(Out).


read_test(File) :-
	open(File, read, In),
	stream_to_lazy_list(In, Codes),
	( foreach(_,Codes) do true ).


write_test(File, N, Mode) :-
	open(File, write, Out, [delete_file(when_lost)]),
	list_to_stream(Codes, Out, Mode),
	( for(_,1,N), foreach(0'x,Codes) do true ),
	close(Out).
	

go(Mode) :-
	open(foo, write, S, [delete_file(when_lost)]),
	list_to_stream(Codes, S, Mode),
	Codes = [0'a,0'b,0'c|T],
	%get(_),
	(
	    T = [0'x,0'x,0'x],
	    %get(_),
	    fail
	;
	    T = [0'y,0'y,0'y,0'y],
	    %get(_),
	    fail
	;
	    T = [0'g,0'h,0'i|T1]
	),
	T1 = [0'j],
	%get(_),
	close(S).


