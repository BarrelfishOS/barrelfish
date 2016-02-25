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
// $Id: scheduler.cc,v 1.1 2006/09/23 01:54:05 snovello Exp $


#include <assert.h> //;
#include <ilsched/intact.h>
#include "sched_cstrs.h"
#include "ec2il.h"

IlcSchedule
schedule_of_handle(EC_word W)
{
  IlcSchedule schedule;
  int is_schedule = W.is_handle(schedule_method, (void**)&schedule);
  assert(is_schedule == EC_succeed);
  return schedule;
}


extern "C"
int
c_schedule() // c_schedule(+Min, +Max, -S)
{
  IlcSchedule schedule(m, EC_long(EC_arg(1)), EC_long(EC_arg(2)));
  return unify(EC_arg(3), handle(schedule_method, schedule.getImpl()));
}

extern "C"
int
c_interval_activity() // c_interval_activity(+S, +Start, +End, +Duration, -A)
{
  try {
    IlcSchedule schedule = schedule_of_handle(EC_arg(1));
    IlcIntVar
      start = var_or_int(EC_arg(2)),
      end = var_or_int(EC_arg(3)),
      duration = var_or_int(EC_arg(4));

    IlcIntervalActivity activity(schedule, start, end, duration);

    return unify(EC_arg(5), handle(activity_method, activity.getImpl()));
  }
  catch(Ec2ilException) { // raised by var_or_int()
    return INSTANTIATION_FAULT;
  }
  catch(IlcFailException) { // raised by activity()
    return EC_fail;
  }
}

extern "C"
int
c_schedule_add() //c_schedule_add(H, Constraint, Handle)
{
  try {
    m.pushState();
    IlcConstraint constraint = sched_cstr(EC_arg(2));
    m.add(constraint);
    trail_undo(ec_arg(1).val.ptr, ilog_fail);
    return unify(EC_arg(3), handle(constraint_method, constraint.getImpl()));
  }
  catch (IlcFailException) {
    m.popState();
    return EC_fail;
  }
  catch(SchedCstrException) {
    return INSTANTIATION_FAULT;
  }
}

extern "C"
int
c_schedule_set() //c_schedule_set(H, Setting)
{
  try {
    ReturnTryPushTrailCatchPop(sched_void(EC_arg(2)), ec_arg(1).val.ptr);
  }
  catch(SchedCstrException) {
    return INSTANTIATION_FAULT;
  } 
}

extern "C"
int
c_discrete_resource() // c_discrete_resource(S, Capacity, R)
{
  IlcSchedule schedule = schedule_of_handle(EC_arg(1));
  IlcInt capacity = EC_long(EC_arg(2));

  IlcDiscreteResource resource;

  if (capacity == 1) // More efficient: 2 times faster on bridge problem
    resource = IlcUnaryResource(schedule);
  else
    resource = IlcDiscreteResource(schedule, capacity);

  return unify(EC_arg(3), handle(resource_method, resource.getImpl()));
}

extern "C"
int
c_alt_res_set() // c_alt_res_set(S, Rs, ARS)
{
  IlcSchedule schedule = schedule_of_handle(EC_arg(1));
  EC_word Rs = EC_arg(2);

  // computes the length of the list
  EC_word p = Rs; int size = 0;
  while(p.is_nil() != EC_succeed) {
    EC_word head, tail;
    if (p.is_list(head, tail) != EC_succeed) { return INSTANTIATION_FAULT; }
    p = tail; size++;
  }

  // Make the resource
  IlcAltResSet resource(schedule, size);
  // Initialize the resource
  p = Rs;
  for(int i = 0; i < size; i++) {
    EC_word head, tail;
    p.is_list(head, tail); // check already done
    resource[i] = resource_of_handle(head);
    p = tail;
  }

  return unify(EC_arg(3), handle(altresset_method, resource.getImpl()));
}
