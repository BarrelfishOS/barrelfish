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
import java.beans.*;

/**
 * This class represents the concept of "interest spec" on the Java side.
 * Any viewer monitoring a viewable has its own interest spec. An
 * interest spec is a specification of how the viewer preferres to be sent
 * updates. The two components at the moment are the change condition (i.e. the
 * waking condition for sending updates) and the view granularity (i.e. whether
 * to see propagation steps separately or not).
 */
public class InterestSpec implements CompoundTerm
{
  private String name;
  private CompoundTerm changeCondition;
  private Atom viewGranularity;
  private PropertyChangeSupport propertyChangeSupport;
  private Viewable viewable;

  static final CompoundTerm DEFAULT_CHANGE_CONDITION =
    new CompoundTermImpl("change_condition",
                         new Atom("suspend"),
                         new Atom("suspend"),
                         new Atom("constrained"));

  static final Atom DEFAULT_VIEW_GRANULARITY =
    new Atom("fine");

  public InterestSpec(String name, Viewable viewable)
  {
    this(name, DEFAULT_CHANGE_CONDITION, DEFAULT_VIEW_GRANULARITY, viewable);
  }

  public InterestSpec(String name, CompoundTerm changeCondition,
                      Atom viewGranularity, Viewable viewable)
  {
    this.viewable = viewable;
    this.name = name;
    setChangeCondition(changeCondition);
    this.viewGranularity = viewGranularity;
    this.propertyChangeSupport = new PropertyChangeSupport(this);
  }

  public Viewable getViewable()
  {
    return(viewable);
  }

  public PropertyChangeSupport getPropertyChangeSupport()
  {
    return(propertyChangeSupport);
  }

  public String getName()
  {
    return(name);
  }

  public CompoundTerm getChangeCondition()
  {
    return(changeCondition);
  }

  public void setChangeCondition(CompoundTerm changeCondition)
  {
    this.changeCondition = changeCondition;
  }

  public Atom getViewGranularity()
  {
    return(viewGranularity);
  }

  public void setViewGranularity(Atom newViewGranularity)
  {
    Object oldViewGranularity;
    if(!viewGranularity.equals(newViewGranularity))
    {
      oldViewGranularity = viewGranularity;
      viewGranularity = newViewGranularity;
      propertyChangeSupport.
        firePropertyChange("viewGranularity",
                           oldViewGranularity, newViewGranularity);
    }
  }

  public String functor()
  {
    return("interest_spec");
  }

  public int arity()
  {
    return(3);
  }

  public Object arg(int index)
  {
    switch(index)
    {
      case 1:
        return new Atom(name);
      case 2:
        return changeCondition;
      case 3:
        return viewGranularity;
      default:
        throw new IllegalArgumentException("Argument index must be between 1 and "+
                                           arity()+" inclusive.");
    }
  }

}
