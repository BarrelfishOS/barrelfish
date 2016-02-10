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
import java.util.*;

/**
 * Create a popup menu based on a given ViewletSelection.
 */
public class ViewletPopupMenu extends JPopupMenu
{
  public ViewletPopupMenu(ContainerViewer viewer,
                          ViewletRange range)
  {
    super("Viewlet actions");
    Collection actions = null;
    if(range.isEmpty())
    {
      throw new IllegalArgumentException("Cannot compose actions for empty viewlet set");
    }
    // if there is only one selected viewlet, then just use the actions of that
    // viewlet.
    //if(viewlets.size() == 1)
    //{
    //  Viewlet single = (Viewlet) viewlets.iterator().next();
    //  actions = single.getActions();
    //}
    // Otherwise, query the collection for a set of actions containing the
    // compound versions of the actions which are common to all viewlets in the
    // selection.
    //if(viewlets.size() > 1)
    //{
    //actions = viewer.getCommonActions();
    //}

    // add all actions to the menu
    Iterator actionsIterator = actions.iterator();
    Action currentAction;
    while(actionsIterator.hasNext())
    {
      currentAction = (Action) actionsIterator.next();
      add(currentAction);
    }
  }
}
