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

import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.util.*;

/**
 * Superclass of actions performed on Viewlets.
 */
public class ViewletAction extends AbstractAction
{
  public void actionPerformed(ActionEvent event)
  {

  }

  public ViewletAction(String name)
  {
    super(name);
    putValue(Action.SMALL_ICON, new EmptyIcon(20,20));
  }

  /**
   * We override equals so that two ViewletActions are equal if they are
   * instances of the same class. This is important when determining whether
   * one action can be performed on two different Viewlets. If the second
   * Viewlet has an action which equals the first Viewlet's action then the
   * it makes sense to provide a compoundAction which acts on both viewlets.
   */
  public boolean equals(Object obj)
  {
    if(obj.getClass().equals(this.getClass()))
    {
      return(true);
    }
    else
    {
      return(super.equals(obj));
    }
  }

  /**
   * If you override equals you have to override hashCode to make sure that two
   * equals Actions also have the same hashCode. We do this by using the Class'
   * hashcode (two actions of the same class must therefore have the same
   * hashcode).
   */
  public int hashCode()
  {
    return(getClass().hashCode());
  }

  /**
   * For subclasses, this should return a ViewletAction which represents
   * this viewletAction applied to the multiple viewlets in the parameter
   * Collection. If such an action cannot be performed on multiple viewlets,
   * the returned action should be disabled.
   */
  public ViewletAction createCompoundAction(Collection viewlets)
  {
    return(null);
  }

  /**
   * EmptyIcon is the default icon for viewletActions. The reason to have this
   * is to make all the actions aligned in a menu, whether or not they have
   * icons.
   */
  private class EmptyIcon implements Icon
  {
    private int height;
    private int width;

    public EmptyIcon(int width, int height)
    {
      this.width = width;
      this.height = height;
    }

    public int getIconHeight()
    {
      return(height);
    }

    public int getIconWidth()
    {
      return(width);
    }

    public void paintIcon(Component c, Graphics g, int x, int y)
    {
    }
  }

}
