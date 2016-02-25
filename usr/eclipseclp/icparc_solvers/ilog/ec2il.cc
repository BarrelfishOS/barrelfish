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
// $Id: ec2il.cc,v 1.1 2006/09/23 01:54:04 snovello Exp $

#define EC2IL_CC
#include <string.h>
#include <ilsolver/ilcpath.h>
#include "ec2il.h"
#include "stdecil.h"
#include "classes.h"
#include "outof.h"

long
EC_long(EC_word t)
{
  long e;
  int x = t.is_long(&e);
  Assert(x == EC_succeed, "EC_long");
  return e;
}

inline int
EC_functor_size(EC_word table)
{
  EC_functor f;
  int x = table.functor(&f);
  Assert(x == EC_succeed, "EC_functor_size");
  return f.arity();
}

IlcBool
binary_functor(EC_word e, EC_functor *f)
{
  return (e.functor(f) == EC_succeed  && f->arity() == 2);
}

IlcIntVar
var_or_int(EC_word w)
{
  IlcInt i;
  IlcIntVar v;

  if (w.is_long(&i) == EC_succeed) {
    return IlcIntVar(m, i, i);
  } else if (w.is_handle(intvar_method, (void**)&v) == EC_succeed) {
    return v;
  }
  // var or int expected in var_or_int()
  throw Ec2ilException();
}

IlcIntVarArray
array_of_list(EC_word list)
{
  // compute the length of the list
  int size=0;
  EC_word p = list;
  while (p.is_nil() != EC_succeed) {
    EC_word head, tail;
    if (p.is_list(head, tail) == EC_succeed) {
      size++;
    } else {
      // Bad argument in sum/1, must be a cons
      throw Ec2ilException();
    }
    p = tail;
  } // size computed

  IlcIntVarArray array(m, size);
  
  // Store the variables or integers from the list in the array
  p = list;
  int index;
  for(index = 0; index < size; index++) {
    EC_word head, tail;
    p.is_list(head, tail); // No checks (done in the previous loop)
    array[index] = var_or_int(head);
    p = tail;
  } // array initialized

  return array;
}

IlcIntArray
int_array_of_list(EC_word list)
{
  // compute the length of the list
  int size=0;
  EC_word p = list;
  while (p.is_nil() != EC_succeed) {
    EC_word head, tail;
    if (p.is_list(head, tail) == EC_succeed) {
      size++;
    } else {
      // Bad argument in sum/1, must be a cons
      throw Ec2ilException();
    }
    p = tail;
  } // size computed

  IlcIntArray array(m, size);
  
  // Store the integers from the list in the array
  p = list;
  int index;
  for(index = 0; index < size; index++) {
    EC_word head, tail;
    p.is_list(head, tail); // No checks (done in the previous loop)
    IlcInt n;
    if (head.is_long(&n) == EC_succeed) {
      array[index] = n;
    } else {
      throw IsLongException();
    }
    p = tail;
  } // array initialized

  return array;
}

IlcIntExp
sum(EC_word E) // The argument is a list of variables or integer
{
  return (IlcSum(array_of_list(EC_argument(E, 1))));
}


IlcIntExp
ec2il_expr(EC_word e)
{
  long i;
  IlcIntVar var;
  EC_functor f;

  if (e.functor(&f) == EC_succeed) {
    switch (f.arity()) {
    case 1: {
      if (strcmp(f.name(), "sum") == 0) { // sum(of a list)
	return sum(e);
      }
      // Unknown unary expression
      throw Ec2ilException();
    }
    case 2: {
      if (EC_argument(e, 1).is_long(&i) == EC_succeed) {
	IlcIntExp arg2 = ec2il_expr(EC_argument(e, 2));
	
	if (strcmp(f.name(), "+") == 0) { return (i + arg2); };
	if (strcmp(f.name(), "-") == 0) { return (i - arg2); };
	if (strcmp(f.name(), "*") == 0) { return (i * arg2); };
	if (strcmp(f.name(), "/") == 0) { return (i / arg2); };

	// Unknown binary expression
	throw Ec2ilException();
      } else if (EC_argument(e, 2).is_long(&i) == EC_succeed) {
	IlcIntExp arg1 = ec2il_expr(EC_argument(e, 1));

	if (strcmp(f.name(), "+") == 0) { return (arg1 + i); };
	if (strcmp(f.name(), "-") == 0) { return (arg1 - i); };
	if (strcmp(f.name(), "*") == 0) { return (arg1 * i); };
	if (strcmp(f.name(), "/") == 0) { return (arg1 / i); };

	// Unknown binary expression
	throw Ec2ilException();
      } else {
	IlcIntExp arg1 = ec2il_expr(EC_argument(e, 1));
      	IlcIntExp arg2 = ec2il_expr(EC_argument(e, 2));
	
	if (strcmp(f.name(), "+") == 0) { return (arg1 + arg2); };
	if (strcmp(f.name(), "-") == 0) { return (arg1 - arg2); };
	if (strcmp(f.name(), "*") == 0) { return (arg1 * arg2); };
	if (strcmp(f.name(), "/") == 0) { return (arg1 / arg2); };

	// Unknown binary expression
	throw Ec2ilException();
      }
    }
    default:
      // Unknown compound expression
      throw Ec2ilException();
    }
  } else /* non compound */ if (e.is_long(&i) == EC_succeed) { // Integer
    IlcIntExp e = (zero + i);
    return e;
  } else if (e.is_handle(intvar_method, (void**)&var) == EC_succeed) {// Variable
    return var;
  } else {
    // Unknown integer expression
    throw Ec2ilException();
  }
}

IlcConstraint
distribute(EC_word c)
{
  // distribute(Cards, Values, Vars, Level)
  // if Level = 0 then IlcBasic else IlcExtended
  
  EC_word Cards = EC_argument(c, 1);
  EC_word Values = EC_argument(c, 2);
  EC_word Vars = EC_argument(c, 3);
  int nb_values = EC_functor_size(Cards);
  int nb_vars = EC_functor_size(Vars);
  long level; EC_argument(c, 4).is_long(&level);
  if (nb_values != EC_functor_size(Values)) { throw Ec2ilException(); }
  
  IlcIntVarArray cards(m, nb_values);
  IlcIntArray values(m, nb_values);
  IlcIntVarArray vars(m, nb_vars);
  
  // Initialize cards and values from Cards and Values
  for(int i = 0; i < nb_values; i++) {
    cards[i] = var_or_int(EC_argument(Cards, i+1));
    
    IlcInt val;
    if (EC_argument(Values, i+1).is_long(&val) == EC_succeed) {
      values[i] = val;
    } else {
      // Integers expected in array Values
      throw Ec2ilException();
    }
  }
  // Initialize vars from Vars
  for(i = 0; i < nb_vars; i++) {
    vars[i] = var_or_int(EC_argument(Vars, i+1));
  }
  
  if (level == 0) {
    return IlcDistribute(cards, values, vars, IlcBasic);
  } else {
    return IlcDistribute(cards, values, vars, IlcExtended);
  }
}


IlcConstraint
element(EC_word C) // element(Index, List, Value)
{
  IlcIntExp arg1 = ec2il_expr(EC_argument(C,1));
  EC_word List = EC_argument(C, 2);
  IlcIntExp arg3 = ec2il_expr(EC_argument(C,3));

  // array[0] unused (because array[arg1-1] does not propagate correctly)
  List = list(EC_word(0L), List);

  try { // Try with an array of integers
    IlcIntArray array = int_array_of_list(List);
    return (arg1 > 0 && array[arg1] == arg3);
  }
  catch (IsLongException) { // Bad luck, the List contains variables
    IlcIntVarArray array = array_of_list(List);
    return (arg1 > 0 && array[arg1] == arg3);
  }
}


IlcConstraint
all_diff(EC_word C, EC_functor f) // alldistinct(Table) or alldifferent(Table)
{
  EC_word table = EC_argument(C, 1);
  int size = EC_functor_size(table);
  
  IlcIntVarArray array(m, size);
  for(int i = 0; i < size; i++) {
    array[i] = var_or_int(EC_argument(table, i+1));
  }
  
  if (strcmp(f.name(), "alldistinct") == 0) {
    return IlcAllDiff(array, IlcWhenValue);
  }
  if (strcmp(f.name(), "alldifferent") == 0) {
    return IlcAllDiff(array, IlcWhenDomain);
  }
}

class DistanceI : public IlcPathTransitI {
  IlcFloat *_distance;
  int _size;
public:
  DistanceI(EC_word, int);
  IlcFloat transit(IlcInt i,IlcInt j) { return _distance[i*_size+j]; }
};

DistanceI::DistanceI(EC_word ArrayDistances, int size): _size(size)
{
  _distance = new (m.getHeap()) (IlcFloat[size*size]);
  int i;

  for(i = 1; i <= size; i++) {
    EC_word From_i = EC_argument(ArrayDistances, i);
    int j;
    for(j = 1; j <= size; j++) {
      _distance[(i-1)*size+j-1] = IlcFloat(EC_long(EC_argument(From_i, j)));
    }
  }
}

IlcPathTransit Distance(IlcManager m, EC_word ArrayDistances, int size)
{
  return new (m.getHeap()) DistanceI(ArrayDistances, size);
}


IlcConstraint
path(EC_word Path) // path(NextCumulArray, ArrayDistances, MaxNbPaths, WhenEvent)
{
 IlcInitFloat();

  EC_word NextCumulArray = EC_argument(Path, 1); // Array of couples
  EC_word ArrayDistances = EC_argument(Path, 2); // Matrix
  IlcInt maxNbPaths = EC_long(EC_argument(Path, 3));
  // if WhenEvent = 0 then WhenValue else When WhenDomain
  IlcWhenEvent whenEvent =
    (EC_long(EC_argument(Path, 4)) == 0 ? IlcWhenValue: IlcWhenDomain);

  IlcInt nbNodes = EC_functor_size(NextCumulArray);

  // Construction of next and cumul
  IlcIntVarArray next(m, nbNodes);
  IlcFloatVarArray cumul(m, nbNodes);
  int i;
  for(i = 1; i <= nbNodes; i++) {
    EC_word NextCumul = EC_argument(NextCumulArray, i);
    EC_word Next = EC_argument(NextCumul, 1);
    EC_word Cumul = EC_argument(NextCumul, 2);

    next[i-1] = var_or_int(Next);
    cumul[i-1] = IlcFloatVar(var_or_int(Cumul));
  } // next and cumul initialized

  // Construction of the transit function
  IlcPathTransit transit = Distance(m, ArrayDistances, nbNodes);
  return(IlcPath(next, cumul, transit, maxNbPaths, whenEvent));
}

IlcConstraint
outof(EC_word Outof) // outof(Var, List), forall X in Array, Var ## X
{
  IlcIntVar var = var_or_int(EC_argument(Outof, 1));
  EC_word List = EC_argument(Outof, 2);

  return OutOf(var, array_of_list(List));
}



IlcConstraint
ec2il_cstr(EC_word c)
{
  EC_functor f;

  if (c.functor(&f) == EC_succeed) {
    switch (f.arity()) {
    case 1: {
      if (strcmp(f.name(), "alldistinct") == 0 || strcmp(f.name(), "alldifferent") == 0) {
	return all_diff(c, f);
      } else if (strcmp(f.name(), "#\\+") == 0) { // NEGATION
	return (! ec2il_cstr(EC_argument(c, 1)));
      }
	
      // Unknown unary constraint
      throw Ec2ilException();
    }
    case 2: {
      EC_word c1 = EC_argument(c,1);
      EC_word c2 = EC_argument(c,2);

      if (strcmp(f.name(), "#/\\") == 0 || strcmp(f.name(), "#\\/") == 0 ||
	  strcmp(f.name(), "#=>") == 0 || strcmp(f.name(), "#<=>") == 0) {
	IlcConstraint arg1 = ec2il_cstr(c1);
	IlcConstraint arg2 = ec2il_cstr(c2);

	if (strcmp(f.name(), "#/\\") == 0) { return (arg1 && arg2); };
	if (strcmp(f.name(), "#\\/") == 0) { return (arg1 || arg2); };
	if (strcmp(f.name(), "#=>") == 0) { return ((! arg1) || arg2); };
	if (strcmp(f.name(), "#<=>") == 0) { return (arg1 == arg2); };
      } else if (strcmp(f.name(), "isd") == 0) {
	IlcIntExp arg1 = ec2il_expr(c1);
	IlcConstraint arg2 = ec2il_cstr(c2);

	return (arg1 >= 0 && arg1 <= 1 && (arg1 == 1) == arg2);
      } else if (strcmp(f.name(), "outof") == 0) {
	return outof(c);
      } else { // Binary constraint on expressions
	IlcIntExp arg1 = ec2il_expr(c1);
	IlcIntExp arg2 = ec2il_expr(c2);
	
	if (strcmp(f.name(), "#=") == 0) { return (arg1 == arg2); };
	if (strcmp(f.name(), "##") == 0) { return (arg1 != arg2); };
	if (strcmp(f.name(), "#\\=") == 0) { return (arg1 != arg2); };
	if (strcmp(f.name(), "#>") == 0) { return (arg1 > arg2); };
	if (strcmp(f.name(), "#<") == 0) { return (arg1 < arg2); };
	if (strcmp(f.name(), "#>=") == 0) { return (arg1 >= arg2); };
	if (strcmp(f.name(), "#<=") == 0) { return (arg1 <= arg2); };
	if (strcmp(f.name(), "#>") == 0) { return (arg1 > arg2); };
	
	// Unknown binary constraint
	throw Ec2ilException();
      }
    }
    case 3: {
      if (strcmp(f.name(), "element") == 0) { // ELEMENT constraint
	return element(c);
      } 

      // Unknown ternary constraint
      throw Ec2ilException();
    }
    case 4: {
      if (strcmp(f.name(), "distribute") == 0) {
	return distribute(c);
      } else if (strcmp(f.name(), "path") == 0) {
	return path(c);
      }
       
      // Unknown 4-ary constraint
      throw Ec2ilException();
    }
    default: {
      // Unknown constraint
      throw Ec2ilException();
    }
    }
  } else { // Not a compound term
    throw Ec2ilException();
  }
}

