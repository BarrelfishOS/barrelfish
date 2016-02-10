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

// $Id: sched_cstrs.cc,v 1.1 2006/09/23 01:54:05 snovello Exp $

#include <strings.h>
#include <assert.h> //;
#include <ilsched/intact.h>
#include "ec2il.h"
#include "sched_cstrs.h"

IlcCapResource
resource_of_handle(EC_word W)
{
  IlcCapResource resource;
  int is_resource = W.is_handle(resource_method, (void**)&resource);
  if (is_resource != EC_succeed) throw SchedCstrException();
  return resource;
}

IlcDiscreteResource
discrete_resource_of_handle(EC_word W)
{
  IlcDiscreteResource resource;
  int is_resource = W.is_handle(resource_method, (void**)&resource);
  if (is_resource != EC_succeed) throw SchedCstrException();
  return resource;
}

IlcAltResSet
altresset_of_handle(EC_word W)
{
  IlcAltResSet resource;
  int is_resource = W.is_handle(altresset_method, (void**)&resource);
  if (is_resource != EC_succeed) throw SchedCstrException();
  return resource;
}

IlcAltResConstraint
altresconstraint_of_handle(EC_word W)
{
  IlcAltResConstraint c;
  int is_c = W.is_handle(constraint_method, (void**)&c);
  if (is_c != EC_succeed) throw SchedCstrException();
  return c;
}

IlcIntervalActivity
activity_of_handle(EC_word W)
{
  IlcIntervalActivity activity;
  int is_activity = W.is_handle(activity_method, (void**)&activity);
  if (is_activity != EC_succeed) throw SchedCstrException();
  return activity;
}

IlcBool
is_atom(EC_word w, char *name)
{
  EC_atom a;
  return (w.is_atom(&a) == EC_succeed && strcmp(a.name(), name) == 0);
}




IlcConstraint
starts_after_end(EC_word C) // starts_after_end(A1, A2, Delay)
{
  IlcActivity activity1 = activity_of_handle(EC_argument(C,1));
  IlcActivity activity2 = activity_of_handle(EC_argument(C,2));
  IlcInt delay = EC_long(EC_argument(C,3));

  return(activity1.startsAfterEnd(activity2, delay));
}

IlcConstraint
consumes(EC_word C) // consumes(A, Resource, Capacity, cap or ars)
{
  IlcIntervalActivity activity = activity_of_handle(EC_argument(C, 1));
  IlcInt capacity = EC_long(EC_argument(C, 3));

  if (is_atom(EC_argument(C, 4), "cap")) {
    IlcCapResource resource = resource_of_handle(EC_argument(C, 2));
    return activity.consumes(resource, capacity);
  }

  assert(is_atom(EC_argument(C, 4), "ars"));
  IlcAltResSet resource = altresset_of_handle(EC_argument(C, 2));
  return activity.consumes(resource, capacity);  
}

IlcConstraint
requires(EC_word C) // requires(A, Resource, Capacity, cap or ars)
{
  IlcIntervalActivity activity = activity_of_handle(EC_argument(C, 1));
  IlcInt capacity = EC_long(EC_argument(C, 3));

  if (is_atom(EC_argument(C, 4), "cap")) {
    IlcCapResource resource = resource_of_handle(EC_argument(C, 2));
    return activity.requires(resource, capacity);
  }

  assert(is_atom(EC_argument(C, 4), "ars"));
  IlcAltResSet resource = altresset_of_handle(EC_argument(C, 2));
  return activity.requires(resource, capacity);
}

IlcConstraint
sched_cstr(EC_word C)
{
  EC_functor f;

  if (C.functor(&f) == EC_succeed) {
    switch (f.arity()) {
    case 3: {
      if (strcmp(f.name(), "starts_after_end") == 0) {
	return starts_after_end(C);
      } 

      // Unknown ternary constraints
      throw SchedCstrException();
    }
    case 4: {
      if (strcmp(f.name(), "consumes") == 0) {
	return consumes(C);
      } else if (strcmp(f.name(), "requires") == 0) {
	return requires(C);
      }

      // Unknown 4-ary constraints
      throw SchedCstrException();
    }
    default:
      // Unknown constraint
      throw SchedCstrException();
    }
  }

  // Unknown constraint
  throw SchedCstrException();
}


void
set_capacity_max(EC_word C) //set_capacity_max(Resource, Min, Max, Capacity)
{
  IlcInt min = EC_long(EC_argument(C, 2));
  IlcInt max = EC_long(EC_argument(C, 3));
  IlcInt capacity = EC_long(EC_argument(C, 4));
  discrete_resource_of_handle(EC_argument(C, 1)).setCapacityMax(min, max, capacity);
}

void
sched_void(EC_word C)
{
  EC_functor f;

  if (C.functor(&f) == EC_succeed) {
    switch (f.arity()) {
    case 1: {
      if (strcmp(f.name(), "close_resource") == 0) {
	resource_of_handle(EC_argument(C, 1)).close();
	return;
      }
      // Unknown unary constraint
      throw SchedCstrException();
    }
    case 2: {
      if (strcmp(f.name(), "set_not_possible") == 0) { // set_not_possible(C, R)
	altresconstraint_of_handle(EC_argument(C, 1)).setNotPossible(resource_of_handle(EC_argument(C, 2)));
	return;
      }
      // Unknown binary constraint
      throw SchedCstrException();
    }
    case 4: {
      if (strcmp(f.name(), "set_capacity_max") == 0) {
	set_capacity_max(C);
	return;
      }

      // Unknown 4-ary constraint
      throw SchedCstrException();
    }
    default:
      // Unknown constraint
      throw SchedCstrException();
    }
  }

  // Unknown constraint
  throw SchedCstrException();
}
