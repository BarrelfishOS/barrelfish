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

import javax.swing.*;
import java.beans.*;
import java.awt.event.*;

/**
 * This is a simple "bridging" class between Action and JButton. It uses
 * the java beans classes PropertyChangeSupport and PropertyChangeListener
 * to synchronise the button's enabled property, its text and its icon with the
 * action's enabled property, name and icon. However, note that the
 * synchronisation is one-way: changing the button's text, icon or enabled
 * status does not alter that of the action.<p>
 *
 * Also, since the Action is
 * made an actionListener of the button, pressing the button will perform the
 * action.
 */
public class ActionButton extends JButton implements PropertyChangeListener
{
  private Action action;

  public ActionButton(Action action)
  {
    super();
    this.action = action;
    setTextFromActionName();
    setIconFromActionIcon();
    setEnabledFromActionEnabled();
    action.addPropertyChangeListener(this);
    this.addActionListener(action);
  }

  private void setTextFromActionName()
  {
    setText((String) action.getValue(Action.NAME));
  }

  private void setIconFromActionIcon()
  {
    setIcon((Icon) action.getValue(Action.SMALL_ICON));
  }

  private void setEnabledFromActionEnabled()
  {
    setEnabled(action.isEnabled());
  }

  public void propertyChange(PropertyChangeEvent event)
  {
    if(event.getPropertyName().equals(Action.NAME))
    {
      setTextFromActionName();
      return;
    }
    if(event.getPropertyName().equals(Action.SMALL_ICON))
    {
      setIconFromActionIcon();
      return;
    }
    if(event.getPropertyName().equals("enabled"))
    {
      setEnabledFromActionEnabled();
      return;
    }
  }
}
