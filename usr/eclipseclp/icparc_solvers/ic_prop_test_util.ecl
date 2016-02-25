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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 	Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ic_prop_test_util.ecl,v 1.1 2013/01/27 01:37:35 jschimpf Exp $
%
%
% IDENTIFICATION:	ic_prop_test_util.ecl
%
% AUTHORS:		Joachim Schimpf, Coninfer Ltd
%
% Specialise the generic code of generic_prop_test_util.ecl for lib(ic)
% ----------------------------------------------------------------------

:- module(ic_prop_test_util).

:- lib(ic).
:- use_module(ic_generic_interface).

:- include(generic_prop_test_util).
