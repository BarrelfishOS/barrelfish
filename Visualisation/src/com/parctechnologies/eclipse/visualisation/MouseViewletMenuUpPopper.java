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
import java.util.*;
import javax.swing.JPopupMenu;

/**
 * MouseListener to create and display a viewletPopupMenu based on the current
 * selection when the right button is clicked on a Viewlet's component
 * (the invoker).
 */
public class MouseViewletMenuUpPopper extends MouseAdapter
{
  private ContainerViewer containerViewer;
  private Component invoker;
  private static final int RIGHT_BUTTON_MASK = InputEvent.BUTTON3_MASK;


  public MouseViewletMenuUpPopper(ContainerViewer containerViewer,
                                  Component invoker)
  {
    this.containerViewer = containerViewer;
    this.invoker = invoker;
    invoker.addMouseListener(this);
  }

  public void mouseReleased(MouseEvent e)
  {
    if((e.getModifiers() & RIGHT_BUTTON_MASK) == RIGHT_BUTTON_MASK) {
      JPopupMenu menu = containerViewer.getPopupMenu();
      if (menu.getComponentCount() > 0) {
        menu.show((Component) e.getSource(), e.getX(), e.getY());
      }
    }
  }

}
