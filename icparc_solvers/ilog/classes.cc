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

// $Id: classes.cc,v 1.1 2006/09/23 01:54:03 snovello Exp $

// EC_IlcIntVar : ECLiPSe-ILOG variable class

#define CLASSES_CC
#include <iostream.h>
#include "classes.h"

char *
gensym(char *prefix)
{
  static int n = 0;
  static char buffer[10];
  sprintf(buffer, "%s%d", prefix, n);
  n++;
  return buffer;
}

// Object() of this IlcIntVar is an EC_ref to the attribute of the ECLIPSE
// variable
class EC_IlcIntVar : public IlcIntVar {
public:
  EC_IlcIntVar(IlcManager, IlcInt, IlcInt, EC_word);
  EC_IlcIntVar(IlcManager, IlcIntVar, EC_word);
  EC_IlcIntVar(IlcManager, IlcIntArray, EC_word);
  EC_ref *getEC_term() { return (EC_ref*)(getObject()); };
  void whenCondition(void (*)(EC_IlcIntVar), IlcWhenEvent event);
};

class WhenEventI : public IlcConstraintI {
private:
  EC_IlcIntVar _x;
  void (*_f)(EC_IlcIntVar);
  IlcWhenEvent _event;
  IlcBool _first_prop;
public:
  WhenEventI(IlcManager, EC_IlcIntVar, void (*)(EC_IlcIntVar), IlcWhenEvent);
  void post();
  void propagate();
};

WhenEventI::WhenEventI(IlcManager m, EC_IlcIntVar x, void (*f)(EC_IlcIntVar), IlcWhenEvent event): IlcConstraintI(m), _x(x), _event(event), _f(f){ _first_prop = IlcTrue; }

void WhenEventI::post() {
  switch (_event) {
  case IlcWhenValue: _x.whenValue(this);break;
  case IlcWhenDomain: _x.whenDomain(this);break;
  case IlcWhenRange: _x.whenRange(this);break;
  default: ec_panic("_event", "WhenEventI::post");
  }
}
void WhenEventI::propagate(){
  if (_first_prop == IlcTrue) {
    // I would prefer a better solution to avoid the call to propagate() when
    // the constraint is posted
    _first_prop = IlcFalse;
  } else {
    _f(_x);
  }
}

IlcConstraint WhenEvent(EC_IlcIntVar x,void (*f)(EC_IlcIntVar), IlcWhenEvent e)
{
  IlcManager m = x.getManager();
  return new (m.getHeap()) WhenEventI(m, x, f, e);
}


EC_IlcIntVar::EC_IlcIntVar(IlcManager m, IlcInt min, IlcInt max, EC_word w)
  : IlcIntVar(m, min, max)
{
  EC_ref *r = new EC_ref(nil()); // When is it deallocated ???
  *r = w;
  setObject((IlcAny)r);
  setName(gensym("R"));
}

EC_IlcIntVar::EC_IlcIntVar(IlcManager m, IlcIntVar intvar, EC_word w)
  : IlcIntVar(intvar)
{
  EC_ref *r = new EC_ref(nil()); // When is it deallocated ???
  *r = w;
  setObject((IlcAny)r);
  setName(gensym("C"));
}

EC_IlcIntVar::EC_IlcIntVar(IlcManager m, IlcIntArray values, EC_word w)
  : IlcIntVar(m, values)
{
  EC_ref *r = new EC_ref(nil()); // When is it deallocated ???
  *r = w;
  setObject((IlcAny)r);
  setName(gensym("V"));
}

void EC_IlcIntVar::whenCondition(void (*f)(EC_IlcIntVar), IlcWhenEvent event)
{
  IlcManager m = this->getManager();
  m.add(WhenEvent(*this, f, event));
}
