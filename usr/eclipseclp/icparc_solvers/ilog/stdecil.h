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
// $Id: stdecil.h,v 1.1 2006/09/23 01:54:05 snovello Exp $

#ifndef STDECIL_H
#define STDECIL_H

#include <iostream.h>
#include <assert.h>
#include <eclipseclass.h>
#include <ilsolver/ilcint.h>
#include <ilsolver/ilcfloat.h>

#include <error.h>
#define INSTANTIATION_FAULT -4
#define Error(x) return(x)

#ifdef DEBUG
#define Assert(x, msg) if (!(x)) { ec_panic(msg, __FILE__) ;}
#define DTrace(x) { cout << x << endl; }
#else
#define DTrace(x)
#define Assert(x, msg)
#endif


extern t_ext_type *intvar_method;
extern IlcManager m;
extern IlcIntExp zero;

inline EC_word
EC_argument(EC_word t, int i)
{
  EC_word e;
  int x = t.arg(i, e);
  assert(x == EC_succeed);
  return e;
}

extern "C" trail_undo(pword* p, void(*f)(pword*));


#define ReturnTryPushTrailCatchPop(to_do, handle) \
  try { \
    m.pushState(); \
    to_do;      \
    trail_undo(handle, ilog_fail); \
    return EC_succeed; \
  } \
  catch (IlcFailException) { \
    m.popState();   \
    return EC_fail; \
  }

extern void ilog_fail(pword *);

#endif

