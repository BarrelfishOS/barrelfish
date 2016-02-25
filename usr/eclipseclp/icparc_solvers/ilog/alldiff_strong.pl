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
% Copyright (C) 1999 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: alldiff_strong.pl,v 1.1 2006/09/23 01:54:03 snovello Exp $
% ----------------------------------------------------------------------

% $Id: alldiff_strong.pl,v 1.1 2006/09/23 01:54:03 snovello Exp $

% Strong alldiff of the Ilog fd library : a powerful alldifferent is already
% provided by fd so the module is defined here only to avoid a loading
% of the real alldiff_strong.pl file.

:- module_interface(alldiff_strong).

:- lib(fd).

:- begin_module(alldiff_strong).

% Left intentionaly empty !
