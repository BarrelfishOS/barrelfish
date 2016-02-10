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
 * A 2D chart viewer (bar chart, points, lines etc..)
 */
public class Chart2DViewer
    extends GraphViewer
{
  /** component representing the labelled time axis **/
  TickBar xTickBar;
  TickBar yTickBar;

  /** xScale **/
  double xScale;

  /** yScale **/
  double yScale;

  /** If true, the chart will auto scale to keep the entitre Y range
      visible **/
  boolean scaleToFitHeight;

  /** minimum X value **/
  double minimumX;

  /** minimum Y value **/
  double minimumY;

  /** maximum X value **/
  double maximumX;

  /** maximum Y value **/
  double maximumY;

  /** Hold the available viewlet types **/
  Collection viewletTypeCollection;

  /** Inital value of xScale **/
  static final double INITIAL_XSCALE = 10.0;

  /** Inital value of yScale **/
  static final double INITIAL_YSCALE = 5.0;

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

  public Chart2DViewer(Collection viewletTypeCollection,
                       VisClientStateModel stateModel,
                       Viewable viewable) {
    super((ViewletType)(viewletTypeCollection.iterator().next()),
          stateModel, viewable, NETWORK_TYPE);
    this.viewletTypeCollection = viewletTypeCollection;
    this.moveable = false;
    this.minimumX = 0.0;
    this.minimumY = 0.0;
    this.maximumX = 1.0;
    this.maximumY = 1.0;
    this.scaleToFitHeight = false;
  }


  /**
   * To prepare for the creation event we initialise the viewlet array
   */
  protected void prepareForCreate(CreateEvent createEvent)
  {
    // for 2D charts, the entire viewable is queried in advance
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

    JMenuItem scaleToFitHeightMenuItem =
        new JCheckBoxMenuItem("Scale to fit height");
    scaleToFitHeightMenuItem.
      setModel(new BooleanPropertyModel("scaleToFitHeight",
					this, getPropertyChangeSupport()));
    setScaleToFitHeightPrivate(true);
    addMenuAndPopupMenuItem("View", null);
    addMenuAndPopupMenuItem("View", scaleToFitHeightMenuItem);


//      addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Hollow", null));
//      addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Transparent", transparentColor));
//      addMenuAndPopupMenuItem("Gantt", new SetTaskFillAction(this, "Solid", solidColor));
    //addMenuAndPopupMenuItem("Gantt",new SetXScaleAction());
  }

  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("Chart".equals(menuTitle)) return KeyEvent.VK_C;
    return super.getMenuMnemonic(menuTitle);
  }

  /** Returns any actions or sub-menus that should be available via a
      right-click popup menu */
  public Collection getViewerPopupMenuCollection() {
    Collection result = super.getViewerPopupMenuCollection();
    result.add(getPopupMenu("Chart"));
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
    initialiseChart();
  }

  protected void initialiseComponent() {
    super.initialiseComponent();
    // add ticker bar to the top of the chart
    GrappaSize graphMargins = (GrappaSize)(graph.getAttributeValue(GrappaConstants.MARGIN_ATTR));
    double margin = 0;
    if (graphMargins != null) {
      margin = GrappaConstants.PointsPerInch * graphMargins.width;
    }
    xTickBar = new TickBar(graphPanel,
                           false,
                           10,
                           1,
                           INITIAL_XSCALE,
                           margin);
    yTickBar = new TickBar(graphPanel,
                           true,
                           10,
                           1,
                           INITIAL_YSCALE,
                           margin);
    TickBarMouseListener tbml = new TickBarMouseListener();
    xTickBar.addMouseListener(tbml);
    xTickBar.addMouseMotionListener(tbml);
    scrollPane.setColumnHeaderView(xTickBar);
    scrollPane.setRowHeaderView(yTickBar);
    setXScalePrivate(new Double(INITIAL_XSCALE));
    setYScalePrivate(new Double(INITIAL_YSCALE));
  }

  class TickBarMouseListener extends MouseAdapter implements MouseMotionListener {
    int initialX;
    double initialXScale;

    public void mousePressed(MouseEvent e) {
      initialX=e.getX()-(int)(xTickBar.displayMargin*xTickBar.zoomLevel);
      initialXScale=xScale;
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"initialX="+initialX);
      }
    }

    public void mouseDragged(MouseEvent e) {
      int currentX=e.getX()-(int)(xTickBar.displayMargin*xTickBar.zoomLevel);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"currentX="+currentX);
      }
      xTickBar.setScalingRight(currentX > initialX);
      xTickBar.setScale((initialXScale * currentX)/initialX);
    }

    public void mouseReleased(MouseEvent e) {
      int currentX=e.getX()-(int)(xTickBar.displayMargin*xTickBar.zoomLevel);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"currentX="+currentX);
      }
      setXScale((initialXScale * currentX)/initialX);
    }

    public void mouseMoved(MouseEvent e) {
      // do nothing
    }
  }


  public void startEvent(VisEvent event, java.util.List goalResults)
  {
    if(event instanceof ExpandEvent)
    {
      super.startEvent(event, goalResults);
      // add new nodes for these elements
      addNewNodes(getExpandingElementIndices());
      // trigger a refresh of the display to take account of the new nodes
      storeListener.rangeUpdated(viewletDataStore, viewletDataStore.getEntireViewletRange());
      graph.resetBoundingBox();
      graph.repaint();
      return;
    }
    super.startEvent(event, goalResults);
  }

  String makeLabel(java.util.List index) {
    StringBuffer sb =
      new StringBuffer(getLocationName(1,((Integer)index.get(0)).intValue()));
    for(int i = 1 ; i < index.size(); i++) {
      sb.append(',');
      sb.append(getLocationName(i+1,((Integer)index.get(i)).intValue()));
    }
    return sb.toString();
  }

  void addNewNodes(ViewletRange range) {
    for(Iterator it = range.iterator();
        it.hasNext(); ) {
      java.util.List index = (java.util.List)it.next();
      ViewletData data = null;

      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"data="+data+" index="+index);
      }
      //Node node = new Node(graph, index.toString());
      Node node;
      
      node = insertNode(viewletDataStore, index, data, viewletType);
      node.getGrappaNexus().boundText = false;
      //node.setAttribute("label",makeLabel(index));
    }
  }

  protected void initialiseChart() {
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "initialiseChart called");
    }
    addNewNodes(viewletDataStore.getEntireViewletRange());
  }


  public String getToolTip(java.util.List index) {
    return makeLabel(index)+":"+viewletDataStore.getViewletDataAt(index).toString();
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
    xTickBar.zoomToLevel(zoomLevel);
    yTickBar.zoomToLevel(zoomLevel);
  }

  public void zoomInByRatio(float zoomRatio)
  {
    // scale the graph
    super.zoomInByRatio(zoomRatio);
    // scale the tickbar
    xTickBar.zoomInByRatio(zoomRatio);
    yTickBar.zoomInByRatio(zoomRatio);
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
    ((ChartBarViewletType)viewletType).setXScale(this.xScale);
    xTickBar.setScale(this.xScale);
    storeListener.rangeUpdated(viewletDataStore, viewletDataStore.getEntireViewletRange());
    graph.resetBoundingBox();
  }

  /**
   * Actualy set the amount by which the display should be stretched
   * in the Y direction.  This method is called from the
   * ViewerSetXScaleCommand command.
   **/
  public void setYScalePrivate(Object yScale) {
    this.yScale=((Double)yScale).doubleValue();
    ((ChartBarViewletType)viewletType).setYScale(this.yScale);
    yTickBar.setScale(this.yScale);
    storeListener.rangeUpdated(viewletDataStore, viewletDataStore.getEntireViewletRange());
    graph.resetBoundingBox();
  }

  /**
   * The Y scale needs to be recalculated (probably because a data
   * point has increased)
   **/
  protected void reScaleY() {
    double paneHeight = scrollPane.getViewport().getExtentSize().getHeight() -
      STEADY_GRAPH_SOFT_MARGIN;
    //double graphHeight = graph.getBoundingBox().getBounds().getHeight();
    double graphHeight = (maximumY - minimumY);
    setYScalePrivate(new Double(paneHeight/graphHeight));
  }

  public boolean getScaleToFitHeight()
  {
    return(scaleToFitHeight);
  }


  public void setScaleToFitHeight(boolean newValue)
  {
    (new ViewerSetBooleanPropertyCommand(this, "scaleToFitHeight", newValue)).issue();
  }

  public void setScaleToFitHeightPrivate(boolean newValue)
  {
    boolean oldValue = scaleToFitHeight;
    scaleToFitHeight = newValue;
    this.getPropertyChangeSupport().
      firePropertyChange("scaleToFitHeight", oldValue, newValue);
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
        //index.set(0,new Integer(-1));
        Element element = getElement(index);
        // configure the renderer
        ViewletType type = elementToViewletType(index);
        if (type instanceof BoundsViewletType && scaleToFitHeight) {
          BoundsViewletType boundsType = 
            (BoundsViewletType)type;
          double min = boundsType.getMin(store, index);
          double max = boundsType.getMax(store, index);
          boolean change = false;
          if ( min < minimumY ) {
            minimumY = min;
            change = true;
          }
          if ( max > maximumY ) {
            maximumY = max;
            change = true;
          }
          if (change) {
            reScaleY();
          }
        }
        type.customizeElement(store, index, element);
      }
      graph.repaint();
    }
  }

}

