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

/**
 * Provides a concrete base class for command issued on Viewer's.
 *
 * Typically resize and zoom actions
 *
 */
public abstract class ViewerCommand extends SymRefCommand {

    public ViewerCommand(SymRefable viewer) {
	super(viewer);
    }

    /**
     * Get the value of viewer.
     * @return value of viewer.
     */
    public Viewer getViewer() {
	return (Viewer)lookupSymRef();
    }

    protected JInternalFrame getInternalFrame()
    {
      return((JInternalFrame)
          ((JComponent)getViewer().
            getComponent()).getRootPane().getParent());

    }

    protected DesktopManager getDesktopManager()
    {
      	DesktopManager dm = getInternalFrame().getDesktopPane().getDesktopManager();
        return(dm);
    }

    public abstract void postRecordIssue();
}
