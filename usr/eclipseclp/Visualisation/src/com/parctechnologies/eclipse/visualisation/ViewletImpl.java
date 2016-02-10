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
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import java.beans.PropertyChangeSupport;

/**
 * Common implementation for Viewlet. This is responsible for:
 * <ul>
 * <li> Managing a collection of viewlet actions
 * <li> Managing a propertyChangeSupport object which notifies of changes to:
 *    <ul>
 *    <li> Whether or not the viewlet is selected
 *    <li> Whether or not the viewlet holds on update events.
 *    <li> Whether or not, and in which direction the viewlet is updating
 *    </ul>
 * <li> Providing public accessors for the first two of these properties.
 * <li> Managing the updating (direction) property within the startUpdate and
 * stopUpdate methods required by the Viewlet interface.
 * <li> Providing a protected element reference and a setter for this (see
 * Viewlet documentation for more details of what this is for.
 * </ul>
 */
public class ViewletImpl implements Viewlet
{
    private static final Atom trueAtom = new Atom("true");
    protected PropertyChangeSupport propertyChangeSupport =
      new PropertyChangeSupport(this);
    private boolean holdsOnUpdates;
    private boolean selected;
    private String updating = "no";
    protected Component component;
    protected Collection actions = new LinkedList();
    protected Object elementReference;

    protected SymRef symRef ;

    public ViewletImpl()
    {
    }

    public void setElementReference(Object elementReference)
    {
      this.elementReference = elementReference;
    }

    public Collection getActions()
    {
      return(actions);
    }

    public CompoundTerm collectPreBuildGoal()
    {
      return(trueAtom);
    }

    public void startBuild(CompoundTerm goalResults){}


    public void stopBuild()
    {}

    public CompoundTerm collectPreUpdateGoal(UpdateEvent updateEvent)
    {
      return(trueAtom);
    }

    public void startUpdate(UpdateEvent updateEvent, CompoundTerm goalResults)
    {
      if(updateEvent instanceof ForwardUpdateEvent)
      {
        setUpdating("updatingForward");
      }
      else
      {
        setUpdating("updatingBack");
      }
    }

    public void stopUpdate()
    {
      // unsure why this is parameterised.
      component.repaint(50);
      setUpdating("no");
    }

    public String getDescription()
    {
      return("");
    }

    public Component getComponent()
    {
      return(component);
    }

    public PropertyChangeSupport getPropertyChangeSupport()
    {
      return(propertyChangeSupport);
    }

    public boolean getHoldsOnUpdates()
    {
      return(holdsOnUpdates);
    }

    public void setHoldsOnUpdates(boolean newValue)
    {
      (new ViewletHoldOnUpdatesCommand(this, newValue)).issue();
    }

    void setHoldsOnUpdatesPrivate(boolean newValue)
    {
      boolean oldValue = holdsOnUpdates;
      holdsOnUpdates = newValue;
      propertyChangeSupport.firePropertyChange("holdsOnUpdates", oldValue, newValue);
    }

    public boolean getSelected()
    {
      return(selected);
    }

    public void setSelected(boolean newValue)
    {
      boolean oldValue = selected;
      selected = newValue;
      propertyChangeSupport.firePropertyChange("selected", oldValue, newValue);
    }


    public String getUpdating()
    {
      return(updating);
    }

    protected void setUpdating(String newValue)
    {
      String oldValue = updating;
      updating = newValue;
      propertyChangeSupport.firePropertyChange("updating", oldValue, newValue);
    }

    public void setSymRef(SymRef symRef) {
	this.symRef = symRef;
    }

    public SymRef getSymRef() {
	return symRef;
    }

    public String getToolTipText() {
      return "";
    }
}


