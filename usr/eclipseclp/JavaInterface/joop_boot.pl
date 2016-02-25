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

:- module(joop_boot).

:- export(jb_go/0).

jb_go:-
	% read the peer name atom specified on stdin.
	read(Control),
	set_password(Passwd),
	remote_connect_setup(localhost/Port, Control, Socket),
	% write the port and password out to stdout 
	% (java should read this)
	write(Port), 
	write("\n"),
	write(Passwd),
	write("\n"),
	flush(output),  
        % accept the connection 
        % blocks until connection is established, then 
	% waits for resume signal from remote side
	remote_connect_accept(Control, Socket, block, _, Passwd, _),
	% read a term from standard in
        read(String),
        % if the connection was successful on the java side, java writes 
        % "accept" into standard in, if unsuccessful, it writes "reject"
	(String = accept -> % if accept was written,
	session(Control);   % yield to the remote peer for a session
      	% otherwise nothing
	true). 

% yield to the remote peer until final resume is called, then disconnect. 
session(Control):-
	remote_yield(Control), 
	remote_disconnect(Control).

% set password to be random number
set_password(Passwd):-
	random(RN),
	number_string(RN, Passwd).



