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

/**************************************************************************
 
     Demo program for peer multitasking

  Running the query init(N) will create N remote peers running copies
  of the tcl demo program. The tcl demo program is assumed to be in 
  the same directory as this program, its ECLiPSe counterpart.

  Each tcl remote peer can participate in peer multitasking. A peer
  multitasking phase is started by the query go_multi/0. To hand control
  over to a specific peer, use go_one/1.

  This program is described in the Tcl Peer Multitasking chapter of the
  Embedding Manual

 **************************************************************************/

init(N) :-
        (for(I,1,N) do 
             remote_connect_setup(localhost/Port, Peer, Sock),
             % start the remote peer program...
             exec([wish,"example_multi.tcl", "--", "-h", "localhost", "-p", Port],
                  [], _Pid),
             (remote_connect_accept(Peer, Sock, 10, post_attach(Peer), "", _) ->
                  true ; close(Sock)
             )
        ).

post_attach(Peer) :-
        recorda(multi_peers, Peer),
        printf("Created peer %w%n", [Peer]),
        set_event_handler(Peer, disconnect_handler/1).


disconnect_handler(Peer) :-
        erase(multi_peers, Peer),
        writeln(finished-Peer).

go_multi :-
        writeln("start multitasking phase...."),
        block(peer_do_multitask(demo), peer_multitask_empty,
                      writeln("No peers are registered for multitasking.")),
        writeln("end multitasking phase....").

go_one(Peer) :-
        printf("transferring to peer %w%n", [Peer]),
        once(recorded(multi_peers, Peer)), 
        remote_yield(Peer),
        printf("Return from peer %w%n", [Peer]).



