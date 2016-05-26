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
import java.io.*;
import java.awt.*;
import java.awt.geom.*;
import java.beans.*;
import java.awt.event.*;
import java.awt.print.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.filechooser.*;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;
import com.parctechnologies.eclipse.visualisation.viewers.*;
import att.grappa.*;

/**
 * A gantt chart viewer based on GraphViewer
 */
public class GanttViewer
    extends GraphViewer
{
  /** component representing the labelled time axis **/
  TickBar tickBar;

  /** xScale **/
  double xScale;

  /** yScale **/
  TickBar yTickBar;


  /** Inital value of xScale **/
  static final double INITIAL_XSCALE = 5.0;

  /** hold the store listener, so that we can trigger display updates
      without going via the data store */
  StoreListener storeListener;

  /**
   * Color to use for transparent task bars
   **/
  static final Color transparentColor = new Color(0.0f, 0.0f, 1.0f, 0.2f);

  /**
   * Color to use for solid task bars
   **/
  static final Color solidColor = new Color(0.0f, 0.0f, 1.0f);

  public GanttViewer(ViewletType viewletType,
                     VisClientStateModel stateModel,
                     Viewable viewable) {
    super(viewletType, stateModel, viewable, NETWORK_TYPE);
    this.moveable = false;
  }


  /**
   * To prepare for the creation event we initialise the viewlet array
   */
  protected void prepareForCreate(CreateEvent createEvent)
  {
    // for gantt charts, the entire viewable is queried in advance
    viewletDataStore =
      new ViewletArray(createEvent.getViewableSize(),
                       ((ViewableType.ArrayType)createEvent.getViewableType()).getFixityList(),
                       getViewable(),
                       (ViewletFactory)viewletType);
    viewletDataStore.setSymRef(new SymRef(viewletDataStore,
                                          this.getSymRef(),
                                          "store"));
    storeListener = new StoreListener();
    viewletDataStore.addViewletDataStoreListener(storeListener);
  }


  protected void initialiseMenu()
  {
    //super.initialiseMenu();
    addMenuAndPopupMenuItem("View",null);
    addMenuAndPopupMenuItem("View",new GrappaAntiAliasToggleAction());

    addMenuAndPopupMenuItem("Graph", new SetBackgroundAction(this, false));
    addMenuAndPopupMenuItem("Graph", new SetBackgroundAction(this, true));

    addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Hollow", null));
    addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Transparent", transparentColor));
    addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Solid", solidColor));
    //addMenuAndPopupMenuItem("Gantt",new SetXScaleAction());
  }

  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("Gantt".equals(menuTitle)) return KeyEvent.VK_T;
    return super.getMenuMnemonic(menuTitle);
  }

  /** Returns any actions or sub-menus that should be available via a
      right-click popup menu */
  public Collection getViewerPopupMenuCollection() {
    Collection result = super.getViewerPopupMenuCollection();
    result.add(getPopupMenu("Gantt"));
    return result;
  }


  /**
   * Construct graph structures
   */
  protected void initialiseGraph() {
    graph = new SteadyGraph(getViewable().getNameString(),
                            true,
                            false,
                            STEADY_GRAPH_SOFT_MARGIN);
    // interpret the viewable as a gantt structure
    initialiseGantt();
  }

  protected void initialiseComponent() {
    super.initialiseComponent();
    // add ticker bar to the top of the chart
    GrappaSize graphMargins = (GrappaSize)(graph.getAttributeValue(GrappaConstants.MARGIN_ATTR));
    double margin = 0;
    if (graphMargins != null) {
      margin = GrappaConstants.PointsPerInch * graphMargins.width;
    }
    tickBar = new TickBar(graphPanel,
                          false,
                          10,
                          2,
                          5.0,
                          margin);
    yTickBar = new TickBar(graphPanel,
                           true,
                           1,
                           1,
                           40.0,
                           margin);
    TickBarMouseListener tbml = new TickBarMouseListener();
    tickBar.addMouseListener(tbml);
    tickBar.addMouseMotionListener(tbml);
    scrollPane.setColumnHeaderView(tickBar);
    scrollPane.setRowHeaderView(yTickBar);
    setXScalePrivate(new Double(INITIAL_XSCALE));
  }

  class TickBarMouseListener extends MouseAdapter implements MouseMotionListener {
    int initialX;
    double initialXScale;

    public void mousePressed(MouseEvent e) {
      initialX=e.getX()-(int)tickBar.displayMargin;
      initialXScale=xScale;
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"initialX="+initialX);
      }
    }

    public void mouseDragged(MouseEvent e) {
      int currentX=e.getX()-(int)tickBar.displayMargin;
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"currentX="+currentX);
      }
      tickBar.setScalingRight(currentX > initialX);
      tickBar.setScale((initialXScale * currentX)/initialX);
    }

    public void mouseReleased(MouseEvent e) {
      int currentX=e.getX()-(int)tickBar.displayMargin;
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"currentX="+currentX);
      }
      setXScale((initialXScale * currentX)/initialX);
    }

    public void mouseMoved(MouseEvent e) {
      // do nothing
    }
  }


  protected Element getElement(java.util.List index) {
    ArrayList newIndex = new ArrayList(index);
    newIndex.set(0,new Integer(-1));
    Element element = graph.findNodeByName(newIndex.toString());
    if (element == null) {
      element = graph.findEdgeByName(newIndex.toString());
    }
    return element;
  }



  protected void initialiseGantt() {
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "initialiseGantt called");
    }
    Map nodeNumberToNode = new HashMap();
    int numColumns = ((Integer)(viewletDataStore.getSize().get(0))).intValue();
    int numTasks = ((Integer)(viewletDataStore.getSize().get(1))).intValue();
    ArrayList index = new ArrayList(2);
    index.add(new Integer(1));
    index.add(new Integer(1)); 
    for(int i = 1; i < numTasks+1; i++) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "i="+i);
      }
      ViewletData data;
      Integer integerI = new Integer(i);
      index.set(0,new Integer(-1));
      index.set(1,integerI);

      data = null;

      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"task="+data+" index="+index);
      }
      //Node node = new Node(graph, index.toString());
      Node node;
      
      node = insertNode(viewletDataStore, index, data, viewletType);
      node.getGrappaNexus().boundText = false;
      node.setAttribute("label",getLocationName(2,i));
    }
  }

  /**
   * configure all the viewlets prior to having the graph drawn for
   * the first time.
   */
  void customizeViewlets() {
    int numElements = ((Integer)(viewletDataStore.getSize().get(0))).intValue();
    int numColumns = ((Integer)(viewletDataStore.getSize().get(1))).intValue();
    ArrayList index = new ArrayList(2);
    index.add(new Integer(1));
    index.add(new Integer(1)); 
    for(int i = 1; i < numElements+1; i++) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "i="+i);
      }
      ViewletData data;
      Integer integerI = new Integer(i);
      index.set(0,integerI);

      index.set(1,new Integer(1));
      //data = viewletDataStore.getViewletDataAt(index);

      Element element = getElement(index);

      elementToViewletType(index).customizeElement(viewletDataStore, index, element);
    }
  }

  public void zoomToLevel(float zoomLevel)
  {
    // scale the graph
    super.zoomToLevel(zoomLevel);
    // scale the tickbar
    tickBar.zoomToLevel(zoomLevel);
    yTickBar.zoomToLevel(zoomLevel);
  }


  /**
   * Set the amount by which the display should be stretched in the X
   * direction.
   **/
  public void setXScale(double xScale) {
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,"old xScale="+this.xScale+" new xScale="+xScale);
    }
    new ViewerSetXScaleCommand(this, xScale).issue();
  }

  /**
   * Actualy set the amount by which the display should be stretched
   * in the X direction.  This method is called from the
   * ViewerSetXScaleCommand command.
   **/
  public void setXScalePrivate(Object xScale) {
    this.xScale=((Double)xScale).doubleValue();
    ((GanttTaskViewletType)viewletType).setXScale(this.xScale);
    tickBar.setScale(this.xScale);
    storeListener.rangeUpdated(viewletDataStore, viewletDataStore.getEntireViewletRange());
    graph.resetBoundingBox();
  }


  public String getToolTip(java.util.List index) {
    // return the task name
    int row = ((Integer)index.get(1)).intValue();
    String taskName = getLocationName(2,row);
    return "task "+taskName;
  }


  /**
   * Class to synchronize the graph elements when changes happen to
   * the ViewletData. */
  protected class StoreListener implements ViewletDataStoreListener {
    public StoreListener() {
    }

    public void rangeUpdated(ViewletDataStore store,
                             ViewletRange range) {
      for(Iterator it = range.iterator(); it.hasNext(); ) {
        java.util.List index = new ArrayList((java.util.List)(it.next()));
        index.set(0,new Integer(-1));
        Element element = getElement(index);
        // configure the renderer
        ViewletType type = elementToViewletType(index);
        type.customizeElement(store, index, element);
      }
      graph.repaint();
    }
  }

  class SetTaskFillAction extends AbstractAction {
    private GanttViewer viewer;
    private Color color;
    
    SetTaskFillAction(GanttViewer viewer, String name, Color color) {
      super("Set fill color to "+name);
      this.viewer = viewer;
      this.color = color;
    }

    public void actionPerformed(ActionEvent event) {
      (new GanttViewerSetTaskFillCommand(viewer, color)).issue();
    }
  }

  public void setTaskFillColorPrivate(Object color) {
    ((GanttTaskViewletType)viewletType).setFillColor((Color)color);
    storeListener.rangeUpdated(viewletDataStore, viewletDataStore.getEntireViewletRange());
  }

  public static class GanttViewerSetTaskFillCommand extends ViewerSetPropertyCommand {
    Color color;

    public GanttViewerSetTaskFillCommand(Viewer viewer, Color color) {
      super(viewer, "taskFillColor");
      this.color = color;
    }

    /**
     * Define the 'newValue' for this property to color
     */
    Object getNewValue() {
      return color;
    }
  }
}

