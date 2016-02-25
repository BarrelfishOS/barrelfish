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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Helmut Simonis, Parc Technologies
%                 Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Generic search routine and search utilities for fd problems
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Helmut Simonis, Parc Technologies Ltd
%               Joachim Schimpf, IC-Parc
%               Kish Shen, IC-Parc (Generised to IC)
% Version:	$Id: fd_search.ecl,v 1.2 2009/07/16 09:11:27 jschimpf Exp $
%
% ----------------------------------------------------------------------

:- module(fd_search).

:- lib(fd).
:- use_module(fd_generic_interface).

:- comment(categories, ["Constraints"]).
:-comment(summary,"This library contains a generic search routine which 
implements a number of partial search methods (complete, credit, lds, bbs, dbs)").

:-comment(desc,html("This library contains a generic search routine which 
implements a number of partial search methods (complete, credit, lds, bbs, 
dbs) and some of their combinations.")).

:-comment(author,"H. Simonis, J. Schimpf").
:-comment(copyright,"Cisco Systems, Inc.").
:-comment(date,"$Date: 2009/07/16 09:11:27 $").

:-include(generic_search).
:-comment(include, generic_search_comments).

