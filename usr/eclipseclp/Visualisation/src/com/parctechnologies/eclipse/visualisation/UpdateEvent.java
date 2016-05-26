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
import java.util.*;

public abstract class UpdateEvent extends VisEvent
{
  private String interestSpecName;
  private List elementsUpdating;
  private static String forwardString = "forward";
  private static String backString = "back";

  static VisEvent parseFromCompoundTerm(CompoundTerm term)
    throws VisException
  {
    if(term.functor().equals("update") &&
       term.arity() == 4)
    {
      if(forwardString.equals(((Atom)term.arg(3)).functor()))
      {
        return(ForwardUpdateEvent.parseFromCompoundTerm(term));
      }
      if(backString.equals(((Atom)term.arg(3)).functor()))
      {
        return(BackUpdateEvent.parseFromCompoundTerm(term));
      }
    }
    throw(new VisException("Could not parse update event from "+term));
  }

  /**
   * Given a list of ECLiPSe 'element(Index)' terms return a list 
   * of 'Index' terms
   */
  protected static List getIndiciesUpdating(List elementsUpdating) {
    ArrayList result =
      new ArrayList(elementsUpdating.size());
    for(Iterator it = elementsUpdating.iterator(); it.hasNext(); ) {
      CompoundTerm element = (CompoundTerm)it.next();
      result.add( element.arg(1) );
    }
    return result;
  }

  public String getInterestSpecName()
  {
    return(interestSpecName);
  }

  public List getElementsUpdating()
  {
    return(elementsUpdating);
  }

  protected void setInterestSpecName(String interestSpecName)
  {
    this.interestSpecName = interestSpecName;
  }

  protected void setElementsUpdating(List elementsUpdating)
  {
    this.elementsUpdating = elementsUpdating;
  }

}
