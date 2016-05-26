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
// $Id: ilog.cc,v 1.1 2006/09/23 01:54:04 snovello Exp $

// External predicates for the ilog.pl module

#include "classes.h"
#include "ec2il.h"

// Positions in the term attached to an Ilog Variable (object of EC_IlcIntVar)
//  Must be COHERENT with ilog_structure in ilog.cc
#define VARIABLE_INDEX 2
#define SUSPENSIONS_MIN_INDEX 3
#define SUSPENSIONS_MAX_INDEX 4
#define SUSPENSIONS_ANY_INDEX 5

#define ILOG_ALREADY_INITIALIZED PERROR
#define ILOG_NOT_INITIALIZED PERROR
#define CheckInit() if (!ilog_initialized) {return ILOG_NOT_INITIALIZED;}

static ilog_initialized = IlcFalse;


t_ext_type ilog_method = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};


extern "C" 
int
c_ilog_init()
{
  if (ilog_initialized) { m.end();}

  IlcInitFloat();
  m = IlcManager(IlcNoEdit);
  m.useExceptions();
  zero = IlcIntVar(m, 0, 0); zero.setValue(0);
  ilog_initialized = IlcTrue;
  return unify(EC_arg(1), handle(&ilog_method, m.getImpl()));
}

int
get_handle_from_arg(int arg, t_ext_type *method, void **obj)
{
  return ((EC_succeed == EC_arg(arg).is_handle(method, obj)));
}

extern "C"
c_ilog_is_intvar()
{
  void **obj = NULL;
  return EC_arg(1).is_handle(intvar_method, obj);
}
extern "C" 
int
c_ilog_print_info()
{
  CheckInit();
  m.printInformation();
  return(EC_succeed);
}

void
instantiate_ecvar(EC_IlcIntVar v)
{
  EC_word ec_term = *v.getEC_term();
  EC_word ec_var = EC_argument(ec_term, VARIABLE_INDEX);

  if (unify(ec_var, EC_word(v.getValue())) != EC_succeed) {
    throw IlcFailException();
  }
}

void
wake_range_suspensions(EC_IlcIntVar v)
{
  EC_word ec_term = *v.getEC_term();

  ec_term.schedule_suspensions(SUSPENSIONS_MIN_INDEX);
  ec_term.schedule_suspensions(SUSPENSIONS_MAX_INDEX);
  ec_term.schedule_suspensions(SUSPENSIONS_ANY_INDEX);
}

void
wake_domain_suspensions(EC_IlcIntVar v)
{
  EC_word ec_term = *v.getEC_term();

  ec_term.schedule_suspensions(SUSPENSIONS_ANY_INDEX);
}

int
suspend_and_unify_intvar(EC_IlcIntVar v, int arg)
{
  v.whenCondition(instantiate_ecvar, IlcWhenValue);
  v.whenCondition(wake_range_suspensions, IlcWhenRange);
  v.whenCondition(wake_domain_suspensions, IlcWhenDomain);
  return unify(EC_arg(arg), handle(intvar_method, v.getImpl()));
}

extern "C" 
int
c_ilog_intvar() // ilog_intvar(Term, Id, Min, Max)
{
  CheckInit();
  IlcInt min = ec_arg(3).val.nint;
  IlcInt max = ec_arg(4).val.nint;
  
  EC_IlcIntVar v(m, min, max, EC_arg(1));
  return suspend_and_unify_intvar(v, 2);
}

extern "C" 
int
c_ilog_enum_var() // ilog_intvar(Term, Id, Values)
{
  CheckInit();
  try {
    IlcIntArray values = int_array_of_list(EC_arg(3));
    
    EC_IlcIntVar v(m, values, EC_arg(1));

    return suspend_and_unify_intvar(v, 2);
  }
  catch (Ec2ilException) {
    // It's not a list
    return INSTANTIATION_FAULT;
  }
  catch (IsLongException) {
    // It's not a list of integers
    return INSTANTIATION_FAULT;
  }
}

extern "C" 
int
c_ilog_copy_var() // ilog_copy_var(Term, Copy, Original)
{
  CheckInit();
  
  IlcIntVar original;

  if (get_handle_from_arg(3, intvar_method, (void**)&original)) { 
    IlcIntVar copy = original.getCopy(m);

    EC_IlcIntVar v(m, copy, EC_arg(1));

    return suspend_and_unify_intvar(v, 2);
  } else {
    return INSTANTIATION_FAULT;
  }
}

extern "C" 
int
c_ilog_get_range() // ilog_get_range(Var, Min, Max)
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(1, intvar_method, (void**)&v)) { 
    if (v.isBound()) {ec_panic("Bound variable", "ilog_get_range");}

    return (unify(EC_arg(3), EC_word(v.getMax())) ||
	    unify(EC_arg(2), EC_word(v.getMin())));
  } else {
    Error(INSTANTIATION_FAULT);
  }
}


extern "C" 
int
c_ilog_get_size() // ilog_get_size(Var, Size)
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(1, intvar_method, (void**)&v)) { 
    return (unify(EC_arg(2), EC_word(v.getSize())));
  } else {
    Error(INSTANTIATION_FAULT);
  }
}


EC_word
make_interval(IlcInt last, IlcInt max, EC_word l, EC_functor dotdot)
{
  if (last == max) { // Single number
    return list(EC_word(max), l);
  } else if (last == max - 1) { // 2-value interval
    return list(EC_word(max-1), list(EC_word(max), l));
  } else { // Interval
    return list(term(dotdot, EC_word(last), EC_word(max)), l);
  }
	
}

extern "C"
c_ilog_get_domain() // ilog_get_domain(Var, List of integers and intervals)
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(1, intvar_method, (void**)&v)) { 
    if (v.isBound()) {ec_panic("Bound variable", "ilog_get_domain");}

    IlcInt max = v.getMax();
    EC_word l = nil();
      EC_functor dotdot("..", 2);

    if (max - v.getMin() > 100000) {
      // Too long to return the actual domain
      return unify(EC_arg(2), make_interval(v.getMin(), max, l, dotdot));
    } else {
      // Let's fetch all the values starting from the max
      IlcInt last = max;
      
      while (v.getNextLower(last) != last) {
	IlcInt lower = v.getNextLower(last);
	if (lower == last - 1) {
	  last = lower;
	} else {
	  l = make_interval(last, max, l, dotdot);
	  max = lower; last = lower;
	}
      }
      l = make_interval(last, max, l, dotdot);
      return (unify(EC_arg(2), l));
    }
  } else {
    Error(INSTANTIATION_FAULT);
  } 
}


int
ilog_call(IlcConstraint g, pword *handle)
{
  ReturnTryPushTrailCatchPop(m.add(g), handle);
}

extern "C" 
int
c_ilog_set_value() // ilog_set_value(H, Var, IntValue)
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(2, intvar_method, (void**)&v)) {
    if (v.isBound()) {
      Assert(v.getValue() == ec_arg(3).val.nint, "c_ilog_set_value");
      return EC_succeed;
    } else {
      return ilog_call(v == ec_arg(3).val.nint, ec_arg(1).val.ptr);
    }
  } else {
    Error(INSTANTIATION_FAULT);
  }
}


extern "C" 
int
c_ilog_eq_vars() // ilog_eq_vars(H, X, Y)
{
  CheckInit();
  IlcIntVar x, y; 

  if (get_handle_from_arg(2, intvar_method, (void**)&x)
      && get_handle_from_arg(3, intvar_method, (void**)&y)) {
    return ilog_call(x == y, ec_arg(1).val.ptr);
  } else {
    Error(INSTANTIATION_FAULT);
  }
}


extern "C" int
c_ilog_add() // ilog_add(SolverHandle, Constraint)
{
  CheckInit();
  try {
    return ilog_call(ec2il_cstr(EC_arg(2)), ec_arg(1).val.ptr);
  }
  catch (Ec2ilException) {
    Error(INSTANTIATION_FAULT);
  }
  catch(IlcFailException) { // ec2il_cstr may raise this exception (element/3)
    return EC_fail;
  }
}


extern "C"
int
c_ilog_setmin()
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(2, intvar_method, (void**)&v)) {
    ReturnTryPushTrailCatchPop(v.setValue(v.getMin()), ec_arg(1).val.ptr);
  } else {
    Error(INSTANTIATION_FAULT);
  }
}

extern "C"
int
c_ilog_removemin()
{
  CheckInit();
  IlcIntVar v;

  if (get_handle_from_arg(2, intvar_method, (void**)&v)) {
    ReturnTryPushTrailCatchPop(v.setMin(v.getMin()+1), ec_arg(1).val.ptr);
  } else {
    Error(INSTANTIATION_FAULT);
  }
}
