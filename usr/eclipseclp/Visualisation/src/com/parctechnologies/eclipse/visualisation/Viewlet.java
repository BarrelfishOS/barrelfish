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
import java.awt.Component;
import java.beans.PropertyChangeSupport;
import java.util.Collection;

/**
 * A Viewlet is a Java object which monitors one viewable element within a
 * viewable. The viewlet is contained within a ContainerViewer. Each viewlet
 * implements a number of methods which are invoked by the ContainerViewer so
 * that the viewlet is initialised properly and reacts properly to events
 * relating to the viewable element it is monitoring. These methods are:
 * <ul>
 * <li>setElementReference: the element reference is an object which the
 * viewlet uses when composing its pre-Build or pre-Update goal. When
 * implementing the collect...Goal methods, the elementReference should be
 * thought of as the ECLiPSe term which is the viewable element that the
 * viewlet is monitoring. In fact, the elementReference is a structure which is
 * <em>replaced</em> with this term during execution of
 * <code>viewable_element_execute/3</code> in the vc_support library.
 * <li>collectPreBuildGoal: used to produce the goal which the viewlet needs
 * executed in order to first initialise.
 * <li>startBuild: used to initialise the viewlet given the results of the
 * pre-build goal.
 * <li>stopBuild: used to complete the initialisation process.
 * <li>collectPreUpdateGoal: used to produce the goal which the viewlet needs
 * executed in order to react to an update in the element it is monitoring.
 * <li>startUpdate: used to indicate to the viewlet that an update to the
 * element it is monitoring has started
 * <li>stopUpdate: used to indicate that the update is complete.
 * </ul><p>
 * Then there are some other methods which are more related to the Viewlet's
 * graphical user interface:
 * <ul>
 * <li>get/setSelected: if the viewlet is selected then the next viewlet action
 * performed by the user will apply to it.
 * <li>getUpdating: true if the viewlet is in an UpdateEvent
 * <li>getDescription: gets a text description of the viewlet.
 * <li>getPropertyChangeSupport: the propertyChangeSupport object is used to
 * observe various properties of the viewlet.
 * <li>getHoldsOnUpdates: true if the viewlet asks for control to be retained
 * when the viewable element it is monitoring is updated.
 * <li>getActions: return a Collection of ViewletAction objects which represents
 * the set of actions which may be performed on this viewlet.
 * <li>getComponent: get the GUI component which will represent the viewlet.
 * </ul>
 */
public interface Viewlet extends SymRefable
{
    CompoundTerm collectPreBuildGoal();

    void startBuild(CompoundTerm goalResults);

    void stopBuild();

    CompoundTerm collectPreUpdateGoal(UpdateEvent updateEvent);

    void startUpdate(UpdateEvent updateEvent, CompoundTerm goalResults);

    void stopUpdate();

    boolean getHoldsOnUpdates();

    boolean getSelected();

    void setSelected(boolean newValue);

    void setElementReference(Object elementReference);

    String getUpdating();

    String getDescription();

    Component getComponent();

    Collection getActions();

    PropertyChangeSupport getPropertyChangeSupport();
}

