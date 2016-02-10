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
// $Id: ec2il.h,v 1.1 2006/09/23 01:54:04 snovello Exp $

#ifndef EC2IL_H
#define EC2IL_H

#include "stdecil.h"

class Ec2ilException {};
class IsLongException {};
IlcConstraint ec2il_cstr(EC_word);
IlcIntExp ec2il_expr(EC_word);

extern long EC_long(EC_word); // No check
extern IlcIntVar var_or_int(EC_word); // may raise Ec2ilException

extern IlcIntArray int_array_of_list(EC_word list);
// may raise Ec2ilException or IsLongException

#endif
