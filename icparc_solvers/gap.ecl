%
% Module for interfacing with the GAP computational algebra package.
%
% Third generation interface: uses named pipes for most communication in
% order to:
%
%   (a) be more robust than the original interface; and
%   (b) avoid the crippling overhead encountered when using the xgap
%	interface for robustness.
%
% $Id: gap.ecl,v 1.3 2009/07/16 09:11:27 jschimpf Exp $
%

%
% Copyright (C) 2002-2004  The SBDS Group
%
% This library is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
% Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this library; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%

%
% TODO
%
% - Use GAP saved sessions for faster start-up times.
%
% - Catch signal chld to handle the case where GAP dies unexpectedly.
%
% - Add timeouts for shutting down GAP and resort to more extreme measures.
%
% - Add support for multiple concurrent GAP sessions through handles, which
%   may have a user-specified name if desired.
%
% - XXX Users of this module may be vulnerable to a symlink attack from a
%   malicious local user.
%
% - Add options for logging to a file rather than a stream, etc.
%

:- module(gap).

%
% General interface:
%
% gap_shutdown	End a GAP session; should be called before quitting ECLiPSe
% gap_restart	Restart a GAP session; like gap_shutdown then gap_start
% gap_ensure_started	Start a GAP session if one not already running
% gap_running	Test whether a GAP session is active
%
% gap_command	Send a command to GAP (no output expected)
% gap_query	Send a query to GAP (returns output of last command as term)
% gap_query_string	As above, but returned as string (for non-ECLiPSe terms)
%

:- comment(categories, ["Interfacing"]).
:- comment(summary, "Library for interfacing with the GAP computational algebra system").
:- comment(desc, html("\
   This library provides an interface to the GAP computational algebra
   system (see <A HREF=\"http://www.gap-system.org/\">www.gap-system.org</A>).  GAP must be installed and appear in the
   user's path in order for this library to work.<P>

   Commands can be sent to GAP using the gap_command/2 predicate.  If there
   are results that need to be returned to ECLiPSe, use gap_query/3 or
   gap_query_string/3.  Normally GAP will be started automatically when it
   is needed, and shut down automatically when the ECLiPSe session ends, but
   some manual control is available through gap_ensure_started/0,
   gap_shutdown/0 and gap_restart/0.  gap_running/0 can be used to determine
   whether a GAP session is currently active.<P>

   The current implementation of the library only supports one GAP session
   at a time, to be shared amongst all software components that use the
   library.  In future we expect to allow multiple concurrent independent
   sessions through the use of handles.<P>

   This library supports the following options (see library(config_opts)):
   <DL>
   <DT>gap_reporting_level</DT>
       <DD>Controls how much reporting output the GAP interface generates.
       Valid levels are:<P>
       <DL>
       <DT>silent</DT>
           <DD>Generate no reporting output under any circumstances.</DD>
       <DT>error</DT>
           <DD>Generate reporting output only when an error occurs.</DD>
       <DT>standard</DT>
           <DD>Generate basic reporting output.</DD>
       <DT>verbose</DT>
           <DD>Generate lots of reporting output, including all commands and
           queries sent to/from GAP.</DD>
       </DL><P>
       The default is standard.  Verbose can be useful for debugging.<P></DD>

   <DT>gap_reporting_stream</DT>
       <DD>Specifies the stream to send reporting output to.  By default,
       ECLiPSe's standard output stream is used.</DD>
   </DL>

   This library is still experimental and is expected to evolve.  Feedback,
   suggestions, problem reports, etc. welcome.<P>

   Please note that this library currently does not work on Windows machines
   since it relies on named pipes, which are currently unsupported by
   cygwin.<P>
")).
:- comment(see_also, [library(config_opts)]).
:- comment(index, [
	"gap_reporting_level",
	"gap_reporting_stream"
    ]).
:- comment(author, "Warwick Harvey, Steve Linton, Ian Gent and others").
:- comment(status, "evolving").


:- export
	gap_running/0,
	gap_ensure_started/0,
	gap_shutdown/0,
	gap_restart/0,
	gap_command/2,
	gap_query/3,
	gap_query_string/3.


:- local variable(gap_pid, -1).		% No GAP running right now.

:- local variable(pipe_to_gap, "").	% Name of pipe to use when talking to GAP.
:- local variable(stream_to_gap, null).	% Stream to use when talking to GAP.
:- local variable(pipe_from_gap, "").	% Name of pipe to use when listening to GAP.


    % Record which directory this file resides in, since we want to look for
    % GAP code in the same directory.
:- local variable(src_dir).
:- get_flag(cwd, SrcDir), setval(src_dir, SrcDir).


    % Check whether we can use ECLiPSe's new finalization facilities to
    % automatically shut GAP down when ECLiPSe terminates.
:- ( get_flag(version_as_list, VersionList), VersionList @>= [5,8,64] ->
	local finalization(gap_shutdown)
    ;
	printf(warning_output, "Your ECLiPSe is too old to support automatic shutdown and clean-up of GAP%nwhen you quit - remember to call gap_shutdown/0 when you're done!%n", [])
    ).


:- lib(config_opts).


    % -------------
    % gap_running/0
    % -------------

:- comment(gap_running/0, [
	summary: "Query whether a GAP session is currently active",
	see_also: [gap_ensure_started/0, gap_shutdown/0, gap_restart/0],
	desc: html("\
   This predicate succeeds if there is a GAP session currently active and
   fails if there is not.
")
    ]).

gap_running :-
	getval(gap_pid, PID),
	PID >= 0.


    % --------------------
    % gap_ensure_started/0
    % --------------------

:- comment(gap_ensure_started/0, [
	summary: "Make sure there is a GAP session active",
	see_also: [gap_shutdown/0, gap_restart/0, gap_running/0],
	desc: html("\
   Makes sure that there is a GAP session currently active, by starting GAP
   if required.  It is normally not necessary to call this predicate
   directly, since sending any GAP command or query will automatically start
   GAP if GAP is not currently running.  However, there may be circumstances
   where one wishes to force GAP to start, to avoid the start-up delay later
   when the first command is sent.
")
    ]).

% Kish 2008-06-16: added a test for checking that gap can be run.
% Without this test, gap_ensure_started/0 will succeed because exec/3
% running gap will succeed even if gap is undefined. 
% exec/2 should abort when there is an error with the command, and "gap -h"
% should print help information and quit.
can_run_gap :-
        block(( exec(["gap",
                      "-h"	% print info and quit
                     ], [I,O,E]),
                close(I), close(O), close(E)
              ),
              _,
              fail % error occurred - unable to run gap
        ).
              
gap_ensure_started :-
	( gap_running ->
	    true
	;
            ( can_run_gap -> 
                true 
            ; 
                writeln(error, "Unable to run gap."),
                abort
            ),
            exec(["gap",
		    "-q",	% Be quiet - don't print banner, prompts.
		    "-A",	% Don't load packages.
		    "-r",	% Don't read any .gaprc file.
		    %"-b",	% Don't print opening banner (covered by -q).
		    "-n",	% Disable line editing.
		    "-T"	% Disable break loop behaviour.
		], [GapInput], PID),
	    report(standard, "GAP started with PID %q.%n", [PID]),
	    (
		block(initialise_gap(GapInput, PID), Tag,
			( close(GapInput), gap_cleanup, exit_block(Tag) ))
	    ->
		true
	    ;
		report(error, "GAP initialisation failed.%n", []),
		close(GapInput),
		gap_cleanup,
		fail
	    )
	).

initialise_gap(GapInput, PID) :-
	setval(gap_pid, PID),
	get_flag(tmp_dir, TmpDir),
	get_flag(tmp_dir, TmpDir),
	concat_string([TmpDir, "/eclipse_pipe_to_gap.", PID], ToGapPipeName),
	concat_string([TmpDir, "/eclipse_pipe_from_gap.", PID], FromGapPipeName),
	setval(pipe_to_gap, ToGapPipeName),
	setval(pipe_from_gap, FromGapPipeName),
	exec(["mkfifo", "-m", "600", ToGapPipeName], []),
	exec(["mkfifo", "-m", "600", FromGapPipeName], []),
	% Load some useful utilities.
	getval(src_dir, SrcDir),
	printf(GapInput, "while true do Read(%q); od;%n", [ToGapPipeName]),
	close(GapInput),
	open(ToGapPipeName, write, ToGapStream),
	setval(stream_to_gap, ToGapStream),
	gap_command("Read(\"%s/RuntimesList.g\")", [SrcDir]).


    % --------------
    % gap_shutdown/0
    % --------------

:- comment(gap_shutdown/0, [
	summary: "Shut down a GAP session",
	see_also: [gap_ensure_started/0, gap_restart/0, gap_running/0],
	desc: html("\
   Shuts down the currently active GAP session if there is one.  With recent
   versions of ECLiPSe it is not normally necessary to call this explicitly;
   GAP will be shut down automatically before the ECLiPSe session ends.
   However, there may be circumstances where one wishes to force GAP to shut
   down before the session ends, and on older versions of ECLiPSe a manual
   shutdown is required in order to terminate the GAP process and clean up
   the named pipes used for communication.
")
    ]).

gap_shutdown :-
	( gap_running ->
	    gap_command("quit;", []),
	    getval(stream_to_gap, ToGapStream),
	    close(ToGapStream),
	    gap_cleanup
	;
	    true
	).

gap_cleanup :-
	getval(gap_pid, PID),
	wait(PID, Status),
	report(standard, "GAP process (PID %q) terminated with status %q.%n",
		[PID, Status]),
	setval(gap_pid, -1),
	getval(pipe_to_gap, ToGapPipeName),
	( ToGapPipeName == "" ->
	    true
	;
	    delete(ToGapPipeName),
	    setval(pipe_to_gap, "")
	),
	setval(stream_to_gap, null),
	getval(pipe_from_gap, FromGapPipeName),
	( FromGapPipeName == "" ->
	    true
	;
	    delete(FromGapPipeName),
	    setval(pipe_from_gap, "")
	).


    % -------------
    % gap_restart/0
    % -------------

:- comment(gap_restart/0, [
	summary: "Restart a GAP session",
	see_also: [gap_ensure_started/0, gap_shutdown/0, gap_running/0],
	desc: html("\
   Restarts the currently active GAP session.  This is useful if one wishes
   to discard GAP's current state and start again with a fresh session (all
   GAP state is lost).<P>

   This predicate is simply a short-hand for calling gap_shutdown/0 and then
   gap_ensure_started/0.
")
    ]).

gap_restart :-
	gap_shutdown,
	gap_ensure_started.


    % -------------
    % gap_command/2
    % -------------

:- comment(gap_command/2, [
	summary: "Send a command to GAP",
	args: [
	    "Format": "Format string",
	    "Args": "List of arguments"
	],
	amode: gap_command(++, +),
	see_also: [gap_query/3, gap_query_string/3, printf/2],
	desc: html("\
   Sends a command to GAP.  The command is specified using Format and Args a
   la printf/2.  It is not necessary to terminate the GAP command with a
   semicolon.<P>

   If there is no currently active GAP session, one is started before
   sending the command.
")
    ]).

gap_command(Format, Args) :-
	gap_ensure_started,
	report(verbose, "GAP command:%n", []),
	report(verbose, Format, Args),
	report(verbose, "%n", []),
	getval(stream_to_gap, ToGapStream),
	printf(ToGapStream, Format, Args),
	put_char(ToGapStream, ";"),
	nl(ToGapStream),
	flush(ToGapStream).


    % ------------------
    % gap_query_string/3
    % ------------------

:- comment(gap_query_string/3, [
	summary: "Query GAP",
	args: [
	    "Format": "Format string",
	    "Args": "List of arguments",
	    "Result": "Result (string)"
	],
	amode: gap_query_string(++, +, ?),
	see_also: [gap_query/3, gap_command/2, printf/2],
	desc: html("\
   Evaluates an expression in GAP and returns the result as a string.  The
   expression is specified using Format and Args a la printf/2, and should
   be a valid GAP expression (no terminating semicolon).  The result is
   returned as a string in Result.<P>

   If there is no currently active GAP session, one is started before
   sending the query.
")
    ]).

gap_query_string(Format, Args, Result) :-
	gap_ensure_started,
	getval(stream_to_gap, ToGapStream),
	getval(pipe_from_gap, FromGapPipeName),
	if_reporting(verbose, (
		report(verbose, "GAP query:%n", []),
		report(verbose, Format, Args),
		report(verbose, "%n", []),
		date(Date),
		report(verbose, "%s", [Date]),
		printf(ToGapStream, "PrintTo(\"%s\", RuntimesList());%n",
			[FromGapPipeName]),
		flush(ToGapStream),
		open(FromGapPipeName, read, FromGapStream0),
		read_string(FromGapStream0, end_of_file, _, Result0),
		close(FromGapStream0),
		term_string([TimeMS0, SysTimeMS0, _, _, _], Result0)
	    )),
	printf(ToGapStream, "eclipse_gap_result := \"eclipse_gap_error\";%n"
		"eclipse_gap_result := ( ", []),
	printf(ToGapStream, Format, Args),
	printf(ToGapStream, " );%nPrintTo(\"%s\", eclipse_gap_result);%n",
		[FromGapPipeName]),
	flush(ToGapStream),
	open(FromGapPipeName, read, FromGapStream),
	read_string(FromGapStream, end_of_file, _, Result),
	close(FromGapStream),
	if_reporting(verbose, (
		report(verbose, "GAP response:%n%s%n", [Result]),
		printf(ToGapStream, "PrintTo(\"%s\", RuntimesList());%n",
			[FromGapPipeName]),
		flush(ToGapStream),
		open(FromGapPipeName, read, FromGapStream1),
		read_string(FromGapStream1, end_of_file, _, Result1),
		close(FromGapStream1),
		term_string([TimeMS1, SysTimeMS1, _, _, _], Result1),
		TimeMS is TimeMS1 - TimeMS0,
		SysTimeMS is SysTimeMS1 - SysTimeMS0,
		report(verbose, "GAP query times: user = %q; system = %q.%n", 
			[TimeMS, SysTimeMS])
	    )),
	( Result == "eclipse_gap_error" ->
	    report(error, "Error evaluating GAP query.%n", []),
	    abort
	;
	    true
	).


    % -----------
    % gap_query/3
    % -----------

:- comment(gap_query/3, [
	summary: "Query GAP",
	args: [
	    "Format": "Format string",
	    "Args": "List of arguments",
	    "Result": "Result (term)"
	],
	amode: gap_query(++, +, ?),
	see_also: [gap_query_string/3, gap_command/2, printf/2],
	desc: html("\
   Evaluates an expression in GAP and returns the result as an ECLiPSe term.
   The expression is specified using Format and Args a la printf/2, and
   should be a valid GAP expression (no terminating semicolon).  The result
   is returned as an ECLiPSe term in Result (it is an error if the result
   returned from GAP is not a valid ECLiPSe term).<P>

   If there is no currently active GAP session, one is started before
   sending the query.
")
    ]).

gap_query(Format, Args, Result) :-
	gap_query_string(Format, Args, String),
	term_string(Result, String).


    % -----------------
    % Reporting support
    % -----------------

:- local domain(reporting_level(silent, error, standard, verbose)).

valid_reporting_level(Level) :-
	domain_index(Level, _:reporting_level, _).


:- register_option(gap_reporting_level, standard,
	    [validation_pred(valid_reporting_level/1)]).

:- register_option(gap_reporting_stream, output, []).


    % If the reporting level specified is at least as high as the currently
    % active reporting level, then print the given material to the reporting
    % stream.
    % XXX - should move as much as possible of this to a compile-time
    % transformation.
report(ReportLevel, Format, Args) :-
	( domain_index(ReportLevel, _:reporting_level, ReportIdx) ->
	    get_option(gap_reporting_level, ActualLevel),
	    ( ReportIdx =< domain_index(ActualLevel, _) ->
		get_option(gap_reporting_stream, Stream),
		printf(Stream, Format, Args),
		flush(Stream)
	    ;
		true
	    )
	;
	    printf(error, "Invalid GAP reporting level %q.%n", [ReportLevel]),
	    abort
	).

    % If the reporting level specified is at least as high as the currently
    % active reporting level, then execute the given goal.
    % XXX - should make it a compile-time transformation.
if_reporting(ReportLevel, Goal) :-
	( domain_index(ReportLevel, _:reporting_level, ReportIdx) ->
	    get_option(gap_reporting_level, ActualLevel),
	    ( ReportIdx =< domain_index(ActualLevel, _) ->
		call(Goal)
	    ;
		true
	    )
	;
	    printf(error, "Invalid GAP reporting level %q.%n", [ReportLevel]),
	    abort
	).

