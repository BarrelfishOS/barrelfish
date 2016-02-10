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

import javax.swing.table.TableCellRenderer;
import att.grappa.CustomRenderer;
import att.grappa.Element;
import att.grappa.Node;
import java.util.List;
import java.util.Collection;

/**
 * A ViewletType is a Java object which contains info on how to
 * render, store, extract data from ECLiPSe and apply actions to
 * representations of viewable elements.
 *
 * <p> Single instances of ViewletTypes willbe associated with any
 * given viewer.
 * */
public interface ViewletType extends SymRefable
{
    /**
     * Returns the renderer for this type of viewlet
     */
    public TableCellRenderer getTableCellRenderer();

    /**
     * Setup the graph element
     */
    public void customizeElement(ViewletDataStore store,
                                 java.util.List index,
                                 Element element);


    BatchGoal collectPreBuildGoal(Viewer viewer,
                                  ViewletDataStore store,
                                  ViewletRange range);

    void startBuild(Viewer viewer,
                    ViewletDataStore store,
		    ViewletRange range,
		    List goalResults);

    void stopBuild();

    BatchGoal collectPreUpdateGoal(Viewer viewer,
                                   ViewletDataStore store,
				   ViewletRange range,
				   UpdateEvent updateEvent);

    void startUpdate(Viewer viewer,
                     ViewletDataStore store,
                     ViewletRange range,
                     List results,
                     UpdateEvent updateEvent);

    void stopUpdate(Viewer viewer,
                    ViewletDataStore store,
		    ViewletRange range);

    Collection getActions(ViewletDataStore store,
                          ViewletRange range) ;

    /**
     * Description of the viewlet type produced, e.g. "Bounds Viewlet"
     */
    String getDescription();
}




