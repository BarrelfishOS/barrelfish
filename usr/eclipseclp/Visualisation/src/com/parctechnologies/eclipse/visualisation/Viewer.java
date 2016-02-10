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

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

/**
 * The concept of Viewer is key to the vis client architecture. A Viewer is an
 * object which is monitoring a viewable. The viewer can react to events
 * concerning the viewable it is monitoring. A viewer has two main
 * groups of methods:
 * <ol>
 * <li> Methods relating to its reaction to events. These correspond to the
 * progression through the different stages of an event. These methods are:
 * prepareForEvent, collectPreEventGoals, startEvent, shouldHold, stopEvent,
 * get/setInterestSpec. The methods are invoked by the ViewerManager at the
 * different stages of an event which the Viewer is participating in.
 * <li> Methods relating to the graphical display of the viewer. These are:
 * get/setDescription, getComponent, getJMenuBar, gain/loseFocus. These are
 * mainly invoked by the ViewerManagerFrame.
 */
public interface Viewer extends SymRefable
{
    /** signals that the argument event is about to start. The viewer must make
     * preparations so that it can return pre event goals.
     */
    void prepareForEvent(VisEvent event);

    /** queries the viewers for a batched set of goals that it would like
     * executed before the argument event starts.
     */
    BatchGoal collectPreEventGoals(VisEvent event);

    /**
     * Returns the results of the requested pre-event goals to the viewer, at
     * the same time signalling to it that the event is starting.
     */
    void startEvent(VisEvent event, java.util.List goalResults);

    /**
     * During an event, queries the viewer as to whether it would like the
     * vis client to "hold" i.e. retain control before allowing ECLiPSe to
     * continue execution.
     */
    boolean shouldHold();

    /** signals that the current event is finishing */
    void stopEvent();

    /**
     * Queries the viewer for the interest spec it requires in order to monitor
     * the viewable.
     */
    InterestSpec getInterestSpec();

    void setInterestSpec(InterestSpec interestSpec);

    String getDescription();

    void setDescription(String description);

    void setViewerManager(ViewerManager viewerManager);

    void close();

    /**
     * Query the Viewer for the GUI Component which is its graphical
     * representation.
     */
    Component getComponent();


    /**
     * Query the Viewer for the viewable which it is monitoring.
     */
    Viewable getViewable();


    /**
     * Query the Viewer for the JMenuBar to be used if the viewer is to appear
     * in a RootPane.
     */
    JMenuBar getJMenuBar();

    void gainFocus();

    void loseFocus();

    void setStateModel(VisClientStateModel stateModel);

    /** Returns the dimensions of the Viewer */
    Rectangle getBounds();

    /** Sets the dimensions of the Viewer */
    void setBounds(Rectangle rectangle);

    /** zoom to new zoom level */
    void zoomToLevel(float newZoomLevel);

    /** zoom to by given ratio */
    void zoomInByRatio(float zoomRatio);

}

