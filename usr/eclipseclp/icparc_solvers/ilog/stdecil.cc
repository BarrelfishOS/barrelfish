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

// $Id: stdecil.cc,v 1.1 2006/09/23 01:54:05 snovello Exp $

#define STDECIL_CC
#include "stdecil.h"

static
t_ext_ptr
intvar_copy(t_ext_ptr p) // min_max, findall ...
{
  ec_panic("Cannot copy an EC_IlcIntVar", "stdecil.cc:intvar_copy()");
  return p; // The compiler expects something to be returned
}

t_ext_type struct_intvar_method = {NULL, intvar_copy, NULL, NULL, NULL, NULL, NULL, NULL,NULL};
t_ext_type *intvar_method = &struct_intvar_method;

IlcManager m = IlcManager(IlcNoEdit);
IlcIntExp zero = IlcIntVar(m, 0, 0);

void
ilog_fail(pword *p)
{
  m.popState();
}


