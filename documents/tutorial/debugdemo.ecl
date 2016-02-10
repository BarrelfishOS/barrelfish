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

:- local variable(pid).

:- [mapcolour].

exec_mapdisplay(Pid, Port, Sock) :-
        get_flag(hostarch, ARCH),
        TclFile = "mapdebugdemo.tcl",
        (exists(TclFile) -> 
            true
        ;
            printf(error, "Cannot find %w, which is needed to run"
                           " this program.%nCheck that you are in the"
                           " right directory to run this program.\n", [TclFile]),
            (current_stream(Sock) -> close(Sock) ; true),
            abort
        ),
        ((ARCH == "i386_nt"; ARCH == "x86_64_nt") ->
             get_flag(installation_directory, ECDIR),
             concat_string([ECDIR, "/tcltk/",ARCH, "/bin/"], WISHPATH),
	     read_directory(WISHPATH, "wish*", _, [WISH|_]), 
	     concat_string([WISHPATH, WISH], WISHEXEC),
             exec([WISHEXEC, TclFile, "--", "-p", Port], [], Pid)
        ;
             exec([wish, TclFile, "--", "-p", Port], [], Pid)
        ),
        setval(pid, Pid).



disconnect_handler :-
        get_flag(hostarch, ARCH),
        ((ARCH == "i386_nt" ; ARCH == "x86_64_nt") -> 
             true ; getval(pid, Pid), wait(Pid, _)
        ).

post_attach(S) :-
        set_event_handler(S, disconnect_handler/0).


colour :-
        remote_connect_setup(localhost/Port, Peer, Sock),
        exec_mapdisplay(_Pid, Port, Sock),
        (remote_connect_accept(Peer, Sock, 10, post_attach(Peer), "", _) ->
            block(colouring1(prolog, input_order, indomain, 4, _), _,  true)
        ;
            writeln("Unable to connect to program to display the map."),
            (current_stream(Sock) -> close(Sock) ; true),
            abort
        ).


colourdelay :-
        colouring1(delay, input_order, indomain, 4, _).

