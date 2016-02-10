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
// Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): 
// 
// END LICENSE BLOCK

package com.parctechnologies.eclipse.visualisation;

import com.parctechnologies.eclipse.*;
import java.io.Serializable;

public class ElementType implements Serializable
{
  private static String changeable = "changeable";
  private static String any = "any";
  private static String numericBounds = "numeric_bounds";
  private static String graphData = "graph_data";

  /**
   * Set to the module implementing the "changeable" variable
   * interface, or null if not changeable
   **/
  protected String changeableSolver;

  protected ElementType() {
    this.changeableSolver = null;
  }

  /**
   * Read the solver for this changeable element type, or null.
   **/
  public String getChangeableSolver() {
    return changeableSolver;
  }

  static ElementType parseFromCompoundTerm(CompoundTerm term)
    throws VisException
  {
    if(changeable.equals(term.functor()) && term.arity()==2)
    {
      try
      {
        CompoundTerm solverTerm = (CompoundTerm)(term.arg(1));
        CompoundTerm typeTerm = (CompoundTerm)(term.arg(2));
        ElementType type = parseFromCompoundTerm(typeTerm);
        type.changeableSolver = solverTerm.functor();
        return type;
      }
      catch(ClassCastException cce)
      {
        throw new VisException("Could not parse changeable element type from "+term);
      }
    }
    if(any.equals(term.functor()))
    {
      return(new AnyElementType());
    }

    if(numericBounds.equals(term.functor()))
    {
      return(new NumericBounds());
    }

    if(graphData.equals(term.functor()))
    {
      return(new GraphData());
    }
    throw new VisException("Could not parse element type from "+term);
  }

}
