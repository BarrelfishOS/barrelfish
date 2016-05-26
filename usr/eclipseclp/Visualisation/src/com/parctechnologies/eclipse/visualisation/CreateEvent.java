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

public class CreateEvent extends VisEvent
{
  private ViewableType type;
  private List size;

  static VisEvent parseFromCompoundTerm(CompoundTerm term)
    throws VisException
  {
    if(term.functor().equals("viewable_create") &&
       term.arity() == 2)
    {
      return(new CreateEvent(((Atom)term.arg(1)).functor(),
                             ViewableType.
                             parseFromCompoundTerm((CompoundTerm)term.arg(2))));
    }
    throw(new VisException("Could not parse CreateEvent from "+term));
  }


  private CreateEvent(String viewableName, ViewableType type)
  {
    setViewableName(viewableName);
    setViewableType(type);
  }

  void setViewableType(ViewableType type)
  {
    this.type = type;
  }

  public ViewableType getViewableType()
  {
    return(type);
  }

  void setViewableSize(List size)
  {
    this.size = size;
  }

  public List getViewableSize()
  {
    return(size);
  }

  public String getDescription()
  {
    return("Viewable "+getViewableName()+" created.");
  }

}
