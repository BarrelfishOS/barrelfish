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

import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import java.beans.*;

/**
 * Provides a concrete implementation of the ViewletSelection interface.
 */
public class ViewletSelectionImpl extends HashSet implements ViewletSelection
{
  // Return a collection of common actions.
  // Each action in the collection is a compound
  // action. There is one compound action in the collection
  // for each action which is common to all viewlets in the selection.
  public Collection getCommonActions()
  {
    Collection commonActions = new LinkedList();
    Collection firstViewletsActions;
    ViewletAction currentAction;
    Iterator viewletsIterator;
    Iterator firstViewletsActionsIterator;
    Viewlet firstViewlet, currentViewlet;
    List currentViewletActions;
    boolean allContainAction;

    if(isEmpty())
    {
      return(Collections.EMPTY_SET);
    }
    firstViewlet = (Viewlet) (iterator().next());
    firstViewletsActions = firstViewlet.getActions();
    firstViewletsActionsIterator = firstViewletsActions.iterator();

    // for each currentAction in the first viewlet's actions
    while(firstViewletsActionsIterator.hasNext())
    {
      currentAction = (ViewletAction) firstViewletsActionsIterator.next();
      viewletsIterator = iterator();
      allContainAction = true;
      // test whether each viewlet has an action equals() to currentAction
      while(viewletsIterator.hasNext() && allContainAction)
      {
        currentViewlet = (Viewlet) viewletsIterator.next();
        if(!currentViewlet.getActions().contains(currentAction))
        {
          allContainAction = false;
        }
      }
      if(allContainAction)
      {
        commonActions.add(currentAction.createCompoundAction(this));
      }
    }
    return(commonActions);
  }
}

