/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Pascal Brisset
 * 
 * END LICENSE BLOCK */
// -*- mode: c++;-*-
// $Id: stdsched.h,v 1.1 2006/09/23 01:54:05 snovello Exp $

#ifndef STDSCHED_H
#define STDSCHED_H

#include "stdecil.h"

extern t_ext_type *schedule_method;
extern t_ext_type *activity_method;
extern t_ext_type *resource_method;
extern t_ext_type *altresset_method;
extern t_ext_type *constraint_method;

#endif
