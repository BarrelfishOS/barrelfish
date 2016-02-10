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

// $Id: outof.cc,v 1.1 2006/09/23 01:54:04 snovello Exp $

#define OUTOF_CC
#include "stdecil.h"

class OutOfI: public IlcConstraintI {
  IlcIntVar _var;
  IlcIntVarArray _array;
  IlcBool first_time;
public:
  OutOfI(IlcManager m, IlcIntVar var, IlcIntVarArray array):
    IlcConstraintI(m), _var(var), _array(array) { first_time = IlcTrue;}
  void propagate();
  void post();
  IlcBool isViolated() const;
  void metaPost(IlcGoalI*);
  IlcConstraintI* makeOpposite() const;
};

IlcConstraint
OutOf(IlcIntVar var, IlcIntVarArray array)
{
  IlcManager m = var.getManager();
  return new (m.getHeap()) OutOfI(m, var, array);
}

class OneOfI: public IlcConstraintI {
  IlcIntVar _var;
  IlcIntVarArray _array;
  IlcBool first_time;
public:
  OneOfI(IlcManager m, IlcIntVar var, IlcIntVarArray array):
    IlcConstraintI(m), _var(var), _array(array) { first_time = IlcTrue;}
  void propagate();
  void post();
  IlcBool isViolated() const;
  void metaPost(IlcGoalI*);
  IlcConstraintI* makeOpposite() const;
};

IlcConstraint
OneOf(IlcIntVar var, IlcIntVarArray array)
{
  IlcManager m = var.getManager();
  return new (m.getHeap()) OneOfI(m, var, array);
}

IlcConstraintI*
OutOfI::makeOpposite() const
{
  return OneOf(_var, _array).getImpl();
}

void
OutOfI::post()
{
  _var.whenValue(this);
  _array.whenValue(this);
}

void
OutOfI::propagate()
{
  if (_var.isBound()) {
    int i;
    IlcInt val = _var.getValue();
    for(i= 0; i < _array.getSize(); i++) {
      _array[i].removeValue(val);
    }
  } else {
    if (first_time == IlcTrue) {
      first_time = IlcFalse;
      for(int i = 0; i < _array.getSize(); i++) {
	if (_array[i].isBound()) {
	  _var.removeValue(_array[i].getValue());
	}
      }
    } else {
      int i = _array.getIndexValue();
      _var.removeValue(_array[i].getValue());
    }
  }
}

IlcBool
OutOfI::isViolated() const // if _var == _array[i] for some i
{
  if (_var.isBound()) {
    IlcInt val = _var.getValue();
    int i;
    for(i= 0; i < _array.getSize(); i++) {
      if (_array[i].isBound() && _array[i].getValue() == val) {
	return IlcTrue;
      }
    }
  }
  return IlcFalse;
}

void
OutOfI::metaPost(IlcGoalI* c)
{
  _var.whenValue(c);
  _array.whenValue(c);
}



IlcConstraintI*
OneOfI::makeOpposite() const
{
  return OutOf(_var, _array).getImpl();
}

void
OneOfI::post()
{
  _var.whenValue(this);
  _array.whenValue(this);
}

void
OneOfI::propagate()
{
  if (_var.isBound()) {
    int i;
    IlcInt val = _var.getValue();
    for(i= 0; i < _array.getSize(); i++) {
      if ((! _array[i].isBound()) || val == val) {
	return;
      }
    }
    m.fail();
  }
}

IlcBool
OneOfI::isViolated() const // if _var != _array[i] for all i
{
  if (_var.isBound()) {
    IlcInt val = _var.getValue();
    int i;
    for(i= 0; i < _array.getSize(); i++) {
      if (_array[i].isBound() && _array[i].getValue() == val) {
	return IlcFalse;
      }
    }
    return IlcTrue;
  }
  return IlcFalse;
}

void
OneOfI::metaPost(IlcGoalI* c)
{
  _var.whenValue(c);
  _array.whenValue(c);
}

