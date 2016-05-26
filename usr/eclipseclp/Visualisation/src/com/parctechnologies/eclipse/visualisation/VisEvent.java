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
import java.util.Map;
import java.io.Serializable;

public abstract class VisEvent implements Serializable
{
  private String viewableName;

  static VisEvent eventFromCompoundTerm(CompoundTerm term)
    throws VisException
  {
    if(term.functor().equals("viewable_create"))
    {
      return(CreateEvent.parseFromCompoundTerm(term));
    }
    if(term.functor().equals("viewable_destroy"))
    {
      return(DestroyEvent.parseFromCompoundTerm(term));
    }
    if(term.functor().equals("viewable_expand"))
    {
      return(ExpandEvent.parseFromCompoundTerm(term));
    }
    if(term.functor().equals("viewable_contract"))
    {
      return(ContractEvent.parseFromCompoundTerm(term));
    }
    if(term.functor().equals("update"))
    {
      return(UpdateEvent.parseFromCompoundTerm(term));
    }


    throw(new VisException("Could not parse VisEvent from "+term));
  }

  public String getViewableName()
  {
    return(viewableName);
  }

  protected void setViewableName(String viewableName)
  {
    this.viewableName = viewableName;
  }

  public boolean unifies(VisEvent e2, Map map) {
    if (getClass() == e2.getClass()) {
      return true;
    }
    return false ;
  }

  public abstract String getDescription();
}
