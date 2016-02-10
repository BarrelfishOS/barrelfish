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

// $Id: stdsched.cc,v 1.1 2006/09/23 01:54:05 snovello Exp $

#include "stdsched.h"

static
t_ext_ptr
trivial_copy(t_ext_ptr p) // useful for using min_max predicate
{
  return p;
}

t_ext_type struct_schedule_method =
  {NULL, trivial_copy, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
t_ext_type struct_activity_method =
  {NULL, trivial_copy, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
t_ext_type struct_resource_method =
  {NULL, trivial_copy, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};
t_ext_type struct_altresset_method =
  {NULL, trivial_copy, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};
t_ext_type struct_constraint_method =
  {NULL, trivial_copy, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

t_ext_type *schedule_method = &struct_schedule_method;
t_ext_type *activity_method = &struct_activity_method;
t_ext_type *resource_method = &struct_resource_method;
t_ext_type *altresset_method = &struct_altresset_method;
t_ext_type *constraint_method = &struct_constraint_method;
