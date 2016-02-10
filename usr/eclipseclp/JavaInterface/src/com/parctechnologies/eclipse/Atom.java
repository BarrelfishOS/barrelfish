// BEGIN LICENSE BLOCK
// Version: CMPL 1.1
//
// The contents of this file are subject to the Cisco-style Mozilla Public
// License Version 1.1 (the "License"); you may not use this file except
// in compliance with the License.  You may obtain a copy of the License
// at www.eclipse-clp.org/license.
// 
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
// the License for the specific language governing rights and limitations
// under the License. 
// 
// The Original Code is  The ECLiPSe Constraint Logic Programming System. 
// The Initial Developer of the Original Code is  Cisco Systems, Inc. 
// Portions created by the Initial Developer are
// Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Stefano Novello / Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: Atom.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Java representation of an ECLiPSe atom.
package com.parctechnologies.eclipse;

/**
 * An ECLiPSe atom. Although atoms are not strictly compound terms, this class
 * implements the CompoundTerm interface (it has 0 arguments), so it can be
 * passed to and returned by
 * rpc methods.
 * @see CompoundTerm
 */
public class Atom extends AbstractCompoundTerm implements CompoundTerm
{
  /**
   * Create an Atom given its functor as a String.
   */
  public Atom(String functor)
  {
    super(functor,0);
  }

  /**
   * This always throws an IndexOutOfBoundsException since the Atom has no arguments.
   * Note that a "throws" clause in the <i>CompoundTerm</i> interface is unnecessary
   * since this is RuntimeException.
   */
  public Object arg(int i)
  {
    throw new IndexOutOfBoundsException("Tried to access argument of ECLiPSe atom.");
  }

  public String toString()
  {
    String result = this.getClass().getName() + " with [";
    result += "functor="+functor()+"]";
    // System.err.println("WARNING: toString called on"+result);

    return(result);
  }


}

