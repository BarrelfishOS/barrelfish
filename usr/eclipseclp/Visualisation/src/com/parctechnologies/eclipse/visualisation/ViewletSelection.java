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
 * Set of selected viewlets. As well as implementing Collection, this is
 * responsible for:
 * <ul>
 * <li> Setting the selected property true for Viewlets that "enter" the
 * selection, and setting it to false when they leave.
 * <li> Providing access to a set of "common" actions. Each member of this set
 * is of the same class as an action in the actions of every viewlet in the
 * selection. Furthermore, each common action is <em>compound</em> i.e. it
 * applies to all viewlets in the selection.
 * </ul>
 */
public interface ViewletSelection extends Collection, Cloneable
{
  // Return a collection of common actions.
  // Each action in the collection is a compound
  // action. There is one compound action in the collection
  // for each action which is common to all viewlets in the selection.
  public Collection getCommonActions();

  // clone the selection
  public Object clone() throws CloneNotSupportedException;
}

