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
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Josh Singer, Parc Technologies
% 
% END LICENSE BLOCK

% ----------------------------------------------------------------------
% System:       ECLiPSe Constraint Logic Programming System
% Version:      
%
% Example program for using queues with JavaInterface module 
%
% Author:       Josh Singer, Parc Technologies Ltd. 
% ----------------------------------------------------------------------

read_5_write_5:-
	count(I, 1, 5) do 
           (read_exdr(java_to_eclipse, TermIn),
	    write_exdr(eclipse_to_java, foo(TermIn, bar)),
	    flush(eclipse_to_java)).
    
