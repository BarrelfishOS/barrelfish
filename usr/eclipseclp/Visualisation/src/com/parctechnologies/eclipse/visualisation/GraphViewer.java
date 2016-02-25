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
import java.util.List;
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
 * A graph viewer based on GrappaPanel from the GraphViz project.
 */
public class GraphViewer
  extends ContainerViewer
  implements ZoomableViewer, Printable
{

  public static final int DESKTOP_TYPE = 0;
  public static final int NETWORK_TYPE = 1;
  public static final double STEADY_GRAPH_SOFT_MARGIN = 100.0;

  protected static final String DIRECTED_TB="directed (Top->Bottom)";
  protected static final String DIRECTED_LR="directed (Left->Right)";
  protected static final String UNDIRECTED ="undirected";
  protected static final String RADIAL     ="radial";
  protected static final String REROUTE_STRAIGHT ="re-route edges (straight)";
  protected static final String REROUTE_SPLINE ="re-route edges (splines)";

  protected int type;

  protected int imageCounter;

  protected GrappaPanel graphPanel;
  protected SteadyGraph graph;
  protected JScrollPane scrollPane;

  protected Map layoutActionMap;

    //private ScrollingViewletGrid scrollingViewletGrid;
  protected boolean trackExpansions = false;
    //  private ViewletTracker viewletTracker;

  protected Map elementToViewletType;


  /** The object to render a background image */
  protected GraphViewBacker backer;

  /** Set to true means the user can move nodes **/
  protected boolean moveable;

  public GraphViewer(ViewletType viewletType,
                     VisClientStateModel stateModel,
                     Viewable viewable,
                     int type) {
    super(stateModel, viewable, viewletType);
    this.type = type;
    this.elementToViewletType = new HashMap();
    this.imageCounter = 0;
    this.moveable = true;
    this.layoutActionMap = new HashMap();
    layoutActionMap.put(DIRECTED_TB,
                        new LayoutAction(this,DIRECTED_TB,"dot -Geclipse -Grankdir=TB"));
    layoutActionMap.put(DIRECTED_LR,
                        new LayoutAction(this,DIRECTED_LR,"dot -Geclipse -Grankdir=LR"));
    layoutActionMap.put(UNDIRECTED,
                        new LayoutAction(this,UNDIRECTED,"neato -Geclipse"));
    layoutActionMap.put(RADIAL,
                        new LayoutAction(this,RADIAL,"twopi -Geclipse"));
    layoutActionMap.put(REROUTE_STRAIGHT,
                        new LayoutAction(this,REROUTE_STRAIGHT,"neato -Geclipse -s -n"));
    layoutActionMap.put(REROUTE_SPLINE,
                        new LayoutAction(this,REROUTE_SPLINE,"neato -Geclipse -s -n -Gsplines"));
    initialiseMenu();
  }

  public ViewletData getViewletAt(java.util.List index)
  {
    return viewletDataStore.getViewletDataAt(index);
  }

  /**
   * This implements the abstract getViewletsAt method required by the
   * ContainerViewer class.
   */
  public Collection getViewletDataAt(java.util.List index)
  {
    Collection result = new LinkedList();
    result.add(getViewletAt(index));
    return(result);
  }

  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("Graph".equals(menuTitle)) return KeyEvent.VK_G;
    if ("Insert".equals(menuTitle)) return KeyEvent.VK_I;
    return super.getMenuMnemonic(menuTitle);
  }

  protected void initialiseMenu()
  {
    //JCheckBoxMenuItem trackExpansionsItem = new JCheckBoxMenuItem("Track expansions");
    //trackExpansionsItem.setModel(new BooleanPropertyModel("trackExpansions",
    //                          this, getPropertyChangeSupport()));
    //addMenuItem("Options", trackExpansionsItem);
    addMenuAndPopupMenuItem("View",null);
    addMenuAndPopupMenuItem("View",new GrappaAntiAliasToggleAction());

    addMenuAndPopupMenuItem("Graph", new SetBackgroundAction(this, false));
    addMenuAndPopupMenuItem("Graph", new SetBackgroundAction(this, true));
    addMenuAndPopupMenuItem("Graph", null);
    if (type == NETWORK_TYPE) {
      // only add "directed" and "undirected" layout actions for the
      // network viewer
      addMenuAndPopupMenuItem("Graph",
                              (LayoutAction)layoutActionMap.get(DIRECTED_TB));
      addMenuAndPopupMenuItem("Graph",
                              (LayoutAction)layoutActionMap.get(DIRECTED_LR));
      addMenuAndPopupMenuItem("Graph",
                              (LayoutAction)layoutActionMap.get(UNDIRECTED));
    }
    addMenuAndPopupMenuItem("Graph",
                            (LayoutAction)layoutActionMap.get(RADIAL));
    if (type == NETWORK_TYPE) {
      // add "edge re-layout actions"
      addMenuAndPopupMenuItem("Graph",
                              null);
      addMenuAndPopupMenuItem("Graph",
                              (LayoutAction)layoutActionMap.get(REROUTE_STRAIGHT));
      addMenuAndPopupMenuItem("Graph",
                              (LayoutAction)layoutActionMap.get(REROUTE_SPLINE));
    }
    
    addMenuAndPopupMenuItem("Insert", new AddImageAction(this));
    addMenuAndPopupMenuItem("Insert", null);
    if (type == DESKTOP_TYPE) {
      // only add "Insert XXX" actions for the desktop viewer
      for(Iterator it = ((MultiViewletType)viewletType).getViewletTypeCollection().iterator(); it.hasNext();) {
        ViewletType type = (ViewletType)it.next();
        addMenuAndPopupMenuItem("Insert", new AddViewletAction(this, type));
      }
    }
  }

  protected Action getZoomToFitWidthAction()
  {
    return(new ZoomToFitWidthAction(this));
  }

  protected Action getZoomToFitHeightAction()
  {
    return(new ZoomToFitHeightAction(this));
  }



  public void prepareForEvent(VisEvent event)
  {
    if(event instanceof CreateEvent)
    {
      prepareForCreate((CreateEvent) event);
    }
    if(event instanceof ExpandEvent)
    {
      prepareForExpand((ExpandEvent) event);
    }

    super.prepareForEvent(event);
  }

  /**
   * To prepare for the creation event we initialise the viewlet array
   */
  protected void prepareForCreate(CreateEvent createEvent)
  {
    switch(type) {
    case NETWORK_TYPE:
      {
        // for network, the entire viewable is queried in advance
        viewletDataStore =
          new ViewletArray(createEvent.getViewableSize(),
                           ((ViewableType.ArrayType)createEvent.getViewableType()).getFixityList(),
                           getViewable(),
                           (ViewletFactory)viewletType);
        viewletDataStore.setSymRef(new SymRef(viewletDataStore,
                                              this.getSymRef(),
                                              "store"));
        break;
      }
    default:
      {
        // for most graphs the viewlets will be added by need
        viewletDataStore =
          new SparseViewletStore(createEvent.getViewableSize(),
                                 ((ViewableType.ArrayType)createEvent.getViewableType()).getFixityList(),
                                 getViewable());
        viewletDataStore.setSymRef(new SymRef(viewletDataStore,
                                              this.getSymRef(),
                                              "store"));
      }
    }
    viewletDataStore.addViewletDataStoreListener(new StoreListener());
  }

  /**
   * To prepare for the expand event we forward the expansion to the viewlet
   * array, so that it can create the new viewlets from which we extract
   * preBuild goals.
   */
  protected void prepareForExpand(ExpandEvent expandEvent)
  {
    viewletDataStore.startExpandDimension(expandEvent.getExpandingDimension());
  }




  public void startEvent(VisEvent event, java.util.List goalResults)
  {
    if(event instanceof UpdateEvent)
    {
      super.startEvent(event, goalResults);
      return;
    }

    // for create event, initialise the component
    if(event instanceof CreateEvent)
    {
      super.startEvent(event, goalResults);

      initialiseGraph();
      initialiseComponent();
      return;
    }

    if(event instanceof ExpandEvent)
    {
      super.startEvent(event, goalResults);

      // if the "track expansions" option is on, scroll to the expanded bit of
      // the grid.
      //if(trackExpansions)
      //  scrollToTrackExpansion((ExpandEvent) event);
      return;
    }
    if(event instanceof ContractEvent)
    {
      super.startEvent(event, goalResults);
      // remove all nodes which are no-longer in the viewlet data store
      removeContractedElements(((ContractEvent)event).getViewableSize());
      return;
    }


    super.startEvent(event, goalResults);
  }
  
  void removeContractedElements(java.util.List newSize) {
    for(GraphIterator enumer = graph.elements();
        enumer.hasNext(); ) {
      // scan through all elements
      Element elem = enumer.nextGraphElement();
      java.util.List index;
      try {
        index = stringToIndexList(elem.getName());
      } catch(NumberFormatException nfe) {
        // not an indexed node
        continue;
      }
      // mark those which fall outside the new dimensions
      boolean contracted = false;
      for(int dim = 0; dim < newSize.size(); dim++) {
        int i = ((Integer)index.get(dim)).intValue();
        int newS = ((Integer)newSize.get(dim)).intValue();
        if (i > newS) {
          contracted = true;
        }
      }
      if (contracted) {
        removeElement(index);
      }
    }
  }

  /**
   * Map an element index to the viewlet type which represents it
   **/
  protected ViewletType elementToViewletType(java.util.List index) {
    return (ViewletType)(this.elementToViewletType.get(index.toString()));
  }


  /**
   * configure all the viewlets prior to having the graph drawn for
   * the first time.
   */
  void customizeViewlets() {
    for(Iterator it = viewletDataStore.getEntireViewletRange().iterator();
        it.hasNext(); ) {
      java.util.List index = (java.util.List)it.next();
      Element element = getElement(index);
      if (element != null) {
        ViewletData data = viewletDataStore.getViewletDataAt(index);;
        element.object = data;
        elementToViewletType(index).customizeElement(viewletDataStore, index, element);
      }
    }
  }

  /**
   * Removes an existing element
   */
  void removeElement(java.util.List index) {
    // does a node with this index already exist?
    Element element = getElement(index);
    if (element != null) {
      // remove the old element
      if (element.isNode()) {
        graph.removeNode(index.toString());
      } else if (element.isEdge()) {
        graph.removeEdge(index.toString());
      }
      elementToViewletType.remove(index.toString());
    }
  }

  /**
   * Create and insert a node into the graph
   */
  Node insertNode(ViewletDataStore store,
                  java.util.List index,
                  ViewletData data,
                  ViewletType type) {
    // remove any existing element
    removeElement(index);
    // create the node as a sub-node of this graph
    Node node = new Node(graph, index.toString());
    // store this data in the node
    node.object = data;
    // configure the visual representation of the node
    type.customizeElement(store, index, node);
    // store the type of this viewlet data
    elementToViewletType.put(index.toString(),type);
    return node;
  }

  /**
   * Create and insert an edge into the graph
   */
  Edge insertEdge(ViewletDataStore store,
                  java.util.List index,
                  Node fromNode, Node toNode,
                  ViewletData data, ViewletType type) {
    // remove any existing element
    removeElement(index);
    // create and edge
    Edge edge = new Edge(graph,
                         fromNode,
                         toNode,
                         index.toString());
    // store this data in the node
    edge.object = data;
    // configure the visual representation of the node
    type.customizeElement(store, index, edge);
    // store the type of this viewlet data
    elementToViewletType.put(index.toString(),type);
    return edge;
  }

  /**
   * Interprets the attributes string as follows...
   *   "key=value key=value key=value..."
   *
   */
  void setAttributes(Element element, String attributes) {
    // strip off the first and last char
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "element="+element+" adding attributes="+attributes);
    }
    StringTokenizer st =
      new StringTokenizer(attributes.substring(1,attributes.length()-1),"= ");
    while(st.hasMoreTokens()) {
      String key = st.nextToken();
      String value = st.nextToken();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "key="+key+" value="+value);
      }
      element.setAttribute(key,value);
    }
    
  }

  protected void initialiseNetwork() {
    Map nodeNumberToNode = new HashMap();
    int numElements = ((Integer)(viewletDataStore.getSize().get(0))).intValue();
    int numColumns = ((Integer)(viewletDataStore.getSize().get(1))).intValue();
    ArrayList index = new ArrayList(2);
    index.add(new Integer(1));
    index.add(new Integer(1)); 
    for(int i = 1; i < numElements+1; i++) {
      ViewletData data;
      Integer integerI = new Integer(i);
      index.set(0,integerI);

      index.set(1,new Integer(1));
      data = viewletDataStore.getViewletDataAt(index);
      String fromNode = data.toString();

      index.set(1,new Integer(2));
      data = viewletDataStore.getViewletDataAt(index);
      String toNode = data.toString();

      index.set(1,new Integer(3));
      data = viewletDataStore.getViewletDataAt(index);
      String info = data.toString();

      String attributes = null;
      if (numColumns > 3) {
        // read the position from the final column
        ArrayList index2 = (ArrayList)index.clone();
        index2.set(1, new Integer(4));
        attributes = viewletDataStore.getViewletDataAt(index2).toString();
      }
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"fromNode="+fromNode+" toNode="+toNode+" info="+info);
      }
      boolean isNode = true;
      try {
        isNode = (Integer.parseInt(fromNode) < 0);
      } catch(NumberFormatException nfe) {
        // do nothing
      }
      if (isNode) {
        // anything that is not a positive integer indicates this is a
        // node descriptor
        
        //Node node = new Node(graph, index.toString());
        Node node;
        // set the shape of node to use as specified in the "fromNode" string
        if ("-1".equals(fromNode)) {
          // default renderer
          node = insertNode(viewletDataStore, index, data, viewletType);
        } else {
          node = insertNode(viewletDataStore, index, data, viewletType);
          node.setAttribute("shape",fromNode);
        }
        if (attributes != null) {
          // interpret the attributes field as a list of key=value
          // pairs and use these to set element attributes
          setAttributes(node, attributes);
          //node.setAttribute("pos", pos);
        }
        // record node with its node number so that it can be looked
        // up when creating edges
        nodeNumberToNode.put(toNode, node);
      } else {
        // otherwise an edge descriptor
//          Edge edge = new Edge(graph,
//                               (Node)(nodeNumberToNode.get(fromNode)),
//                               (Node)(nodeNumberToNode.get(toNode)),
//                               index.toString());
        Edge edge = insertEdge(viewletDataStore,
                               index,
                               (Node)(nodeNumberToNode.get(fromNode)),
                               (Node)(nodeNumberToNode.get(toNode)),
                               data,
                               viewletType);
      }
    }
  }

  /**
   * Construct graph structures
   */
  protected void initialiseGraph() {
    graph = new SteadyGraph(getViewable().getNameString(),
                            true,
                            false,
                            STEADY_GRAPH_SOFT_MARGIN);
    if (type == NETWORK_TYPE) {
      // interpret the viewable as a network
      initialiseNetwork();
    }
  }

  /**
   * Layout the graph and set the new graph bounds
   */
  void externalLayoutGraph(String command) {
    final String EXTERNAL_SOLVERS_MESSAGE=
      "\nEnsure that the GraphViz tools are installed correctly and are"+
      "\navailable on your default path.  These tools can be downloaded"+
      "\nfrom http://www.research.att.com/sw/tools/graphviz/";
    Object connector = null;
    try {
      connector = Runtime.getRuntime().exec(command);
    } catch(Exception ex) {
      try {
        // try pre-prending the eclipse library path, since this is where the
        // tools that come with ECLiPSe may live
        String newCommand = getStateModel().getEclipseLibDir()+File.separator+command;
        connector = Runtime.getRuntime().exec(newCommand);
      } catch(Exception ex2) {
        JOptionPane.showMessageDialog(null,
                                      new String[] {"Problem calling external layout program: " + ex2.getMessage(), EXTERNAL_SOLVERS_MESSAGE});
        return;
      }
    }
    if(!GrappaSupport.filterGraph(graph,connector)) {
      JOptionPane.showMessageDialog(null,
                                    new String[] {"Problem during execution of external layout program.",EXTERNAL_SOLVERS_MESSAGE});
      return;
    }
    if(connector instanceof Process) {
      try {
        int code = ((Process)connector).waitFor();
        if(code != 0) {
          JOptionPane.showMessageDialog(null,
                                        new String[] {"External layout program exited with code "+code,EXTERNAL_SOLVERS_MESSAGE});
          //return;
        }
      } catch(InterruptedException ex) {
        JOptionPane.showMessageDialog(null,
                                      new String[] {"Problem whilst exiting external layout program.", EXTERNAL_SOLVERS_MESSAGE});
        ex.printStackTrace(System.err);
      }
    }
    connector = null;
    graph.oldBounds = null;
    customizeViewlets();
    graph.repaint();
  }

  /**
   * Layout the graph as a matrix based on the first two dimensions of
   * the viewlet indices */
  void layoutGraphAsMatrix() {
    //
  }


  protected void initialiseComponent()
  {
    // disable the "trackUpdates" feature
    trackUpdatesMenuItem.setEnabled(false);

    scrollPane = new JScrollPane();
    scrollPane.getViewport().setBackingStoreEnabled(true);
    
    backer = new GraphViewBacker();
    graphPanel = new GrappaPanel(graph, backer);
    // set no-antialiasing for grappa graphs by default
    Grappa.useAntiAliasing = false;
    // set node and edge label cut-off
    Grappa.nodeLabelsScaleCutoff = 0.1;
    Grappa.edgeLabelsScaleCutoff = 0.1;
    Grappa.subgLabelsScaleCutoff = 0.1;
    graphPanel.addGrappaListener(new GraphViewListener(this));
    graphPanel.setScaleToFit(false);
    
    java.awt.Rectangle bbox = graph.getBoundingBox().getBounds();



    //    addMenuAndPopupMenuItem("Insert", new AddViewletAction(this, viewletType));
    //    addMenuAndPopupMenuItem("Insert", new AddViewletAction(this, new TextViewletType()));

    //scrollingViewletGrid =
    //  new ScrollingViewletGrid(getViewletArray(), this);
    //viewletTracker = new ViewletTracker(scrollingViewletGrid);
    MouseViewletMenuUpPopper mouseViewletMenuUpPopper =
        new MouseViewletMenuUpPopper(this, graphPanel);
    // create the selection for this table
    //selection = new ViewletSelectionSpreadSheet((SpreadSheet)table, this);
    // set the row height to equal the column width
    scrollPane.setViewportView(graphPanel);
    scrollPane.setPreferredSize(new Dimension(430, 200));
  }


  public String getToolTip(java.util.List index) {
    return viewletDataStore.getViewletDataAt(index).toString();
  }

  public Component getComponent()
  {
    return scrollPane;
  }

  public void setTrackExpansions(boolean newValue)
  {
    //(new ViewerSetBooleanPropertyCommand(this, "trackExpansions", newValue)).issue();
  }

  public void setTrackExpansionsPrivate(boolean newValue)
  {
    //boolean oldValue = trackExpansions;
    //trackExpansions = newValue;
    //this.getPropertyChangeSupport().
    //  firePropertyChange("trackExpansions", oldValue, newValue);
  }

  public boolean getTrackExpansions()
  {
    return false;
    //return(trackExpansions);
  }


  public void zoomToLevel(float zoomLevel)
  {
    // scale the graph
    graphPanel.setScaleToFit(false);
    graphPanel.setScaleToSize(null);
    graphPanel.resetZoom();
    graphPanel.multiplyScaleFactor(zoomLevel);
    //table.setColumnWidth((int)(DEFAULT_COLUMN_WIDTH*zoomLevel));
    //table.setRowHeight((int)(DEFAULT_ROW_HEIGHT*zoomLevel));
    //table.revalidate();
  }

  public void zoomInByRatio(float zoomRatio)
  {
    graphPanel.setScaleToFit(false);
    graphPanel.setScaleToSize(null);
    graphPanel.multiplyScaleFactor(zoomRatio);
  }

  // move the scrollbars so that the new row/column comes into view.
  protected void scrollToTrackExpansion(ExpandEvent event)
  {
    //scrollingViewletGrid.scrollToTrackExpansion(event.getExpandingDimension());
  }

//    protected ViewletTracker getViewletTracker()
//    {
//      return(viewletTracker);
//    }


  protected class ZoomToFitWidthAction extends AbstractAction
  {
    private GraphViewer viewer;
    ZoomToFitWidthAction(GraphViewer viewer)
    {
      super("Zoom to fit width");
      this.viewer = viewer;
    }

    public void actionPerformed(ActionEvent event)
    {
      (new ZoomableViewerZoomToFitWidthCommand(viewer)).issue();
    }
  }

  public void zoomToFitWidth()
  {
    double graphWidth = graph.getBoundingBox().getWidth();
    GrappaSize margins = (GrappaSize)(graph.getAttributeValue(Grappa.MARGIN_ATTR));
    if(margins != null) {
      double x_margin = graphPanel.PointsPerInch * margins.width;
      graphWidth += 2.0 * x_margin;
    }
    double paneWidth = scrollPane.getViewport().getExtentSize().getWidth();
    zoomToLevel((float)(paneWidth / graphWidth));
  }

  protected class ZoomToFitHeightAction extends AbstractAction
  {
    private GraphViewer viewer;
    ZoomToFitHeightAction(GraphViewer viewer)
    {
      super("Zoom to fit height");
      this.viewer = viewer;
    }

    public void actionPerformed(ActionEvent event)
    {
      (new ZoomableViewerZoomToFitHeightCommand(viewer)).issue();
    }
  }

  public void zoomToFitHeight()
  {
    double graphHeight = graph.getBoundingBox().getHeight();
    GrappaSize margins = (GrappaSize)(graph.getAttributeValue(Grappa.MARGIN_ATTR));
    if(margins != null) {
      double y_margin = graphPanel.PointsPerInch * margins.height;
      graphHeight += 2.0 * y_margin;
    }
    double paneHeight = scrollPane.getViewport().getExtentSize().getHeight();
    zoomToLevel((float)(paneHeight / graphHeight));
  }


  protected class LayoutAction extends AbstractAction
  {
    private GraphViewer viewer;
    private String command;
    LayoutAction(GraphViewer viewer, String name, String command)
    {
      super("Layout graph:"+name);
      this.viewer = viewer;
      this.command = command;
    }

    public void actionPerformed(ActionEvent event)
    {
      String newCommand = (String)JOptionPane.
        showInputDialog(null,
                        "The graph layout will be preformed by the following command",
                        "Layout command",
                        JOptionPane.PLAIN_MESSAGE,
                        null,
                        null,
                        command);
      command = newCommand;
      new ExternalLayoutCommand(viewer, command).issue();
      //viewer.externalLayoutGraph(command);
    }
  }

  public static class ExternalLayoutCommand extends ViewerCommand {
    String command;
    public ExternalLayoutCommand(GraphViewer viewer, String command) {
      super(viewer);
      this.command = command;
    }

    public void postRecordIssue() {
      ((GraphViewer)getViewer()).externalLayoutGraph(command);
    }
  }

  public static class MoveNodeCommand extends ViewerCommand {
    String nodeName;
    double dx;
    double dy;

    public MoveNodeCommand(GraphViewer viewer, Node node,
                           double dx, double dy) {
      super(viewer);
      this.nodeName = node.getName();
      this.dx = dx;
      this.dy = dy;
    }

    public void postRecordIssue() {
      GraphViewer graphViewer = (GraphViewer)getViewer();
      graphViewer.moveNode((Node)graphViewer.graph.findNodeByName(nodeName),
                           dx, dy);
    }
  }

    //  public void setSelection(ViewletRange newSelection) {
      //(new ViewerSetSelectionCommand(this, newSelection)).issue();
    //  }


  protected void clearSelectionPrivate() {
    // clear the selection
    if(graph.currentSelection != null) {
      if(graph.currentSelection instanceof Element) {
        ((Element)(graph.currentSelection)).highlight &= ~GrappaConstants.HIGHLIGHT_MASK;
      } else {
        Vector vec = ((Vector)(graph.currentSelection));
        for(int i = 0; i < vec.size(); i++) {
          ((Element)(vec.elementAt(i))).highlight &= ~GrappaConstants.HIGHLIGHT_MASK;
        }
      }
      graph.currentSelection = null;
      graph.repaint();
    }
  }

  /**
   * Called by the ViewerSetSelectionCommand
   */
  protected void setSelectionPrivate(ViewletRange newSelection) {
    DebuggingSupport.
	logMessage(this,
		   "setSelectionPrivate called with newSelection=" +
		   newSelection);
    clearSelectionPrivate();
    if (!newSelection.isEmpty()) {
      graph.currentSelection = new Vector();
      for(Iterator it = newSelection.iterator();
          it.hasNext();) {
        java.util.List index = (java.util.List) it.next();
        Element element = getElement(index);
        if (element != null) {
          // highlight element
          element.highlight |= GrappaConstants.SELECTION_MASK;
          ((Vector)graph.currentSelection).add(element);
        }
      }
    }
  }


  protected Element getElement(java.util.List index) {
    Element element = graph.findNodeByName(index.toString());
    if (element == null) {
      element = graph.findEdgeByName(index.toString());
    }
    return element;
  }


  /**
   * Returns a copy of the current selection
   */
  public ViewletRange getSelection() {
    //return new SpreadSheetSelectionViewletRange(table);
    // scan the graph, looking for nodes which are selected
    ViewletRangeCollection range = new ViewletRangeCollection();
    for(Iterator it = viewletDataStore.getEntireViewletRange().iterator();
        it.hasNext();) {
      java.util.List index = (java.util.List) it.next();
      Element element = getElement(index);
      if (element != null) {
        if ((element.highlight & Grappa.SELECTION_MASK) == Grappa.SELECTION_MASK) {
          range.add(index);
        }
      }
    }
    return range;
  }
  

  /**
   * Return a collection of actions which can be applied to viewlets
   * in this table
   */
  public Collection getCompoundActions(ViewletRange selected) {
    return viewletType.getActions(viewletDataStore,selected);
    //    Collection ll = new LinkedList();
    //    ll.add((new ToggleHoldAction()).createCompoundAction(selected));
    //    return ll;
  }

//    private class ToggleHoldAction extends ViewletAction
//      {
//        ToggleHoldAction()
//        {
//          super("Hold on updates");
//          putValue(Action.NAME, "Hold on updates");
//          putValue(Action.LONG_DESCRIPTION,
//                   "Change whether control is held by the visualisation client during element updates");
//          putValue(Action.SHORT_DESCRIPTION,
//                   "Change whether control is held on updates");
//          putValue(Action.SMALL_ICON, new HoldIcon(20, 20));

//        }

//        public void actionPerformed(ActionEvent e)
//        {
//  	// do nothing
//        }

//        /**
//         * If all viewlets in the collection have hold set to true, then the
//         * compound version sets it to false. If any of them have it set to false
//         * then the compound version sets all to true.
//         */
//        public ViewletAction createCompoundAction(Collection viewlets)
//        {
//          boolean allHold = true;
//          ViewletImpl currentViewlet;
//          Iterator viewletsIterator = viewlets.iterator();
//          while(viewletsIterator.hasNext())
//          {
//            currentViewlet = (ViewletImpl) viewletsIterator.next();
//            if(!currentViewlet.getHoldsOnUpdates())
//            {
//              allHold = false;
//              break;
//            }
//          }
//          return(new CompoundToggleHoldAction(!allHold, viewlets));
//        }
//      }

//      private class CompoundToggleHoldAction extends ToggleHoldAction
//      {
//        private boolean newValue;
//        private Collection viewlets;
//        CompoundToggleHoldAction(boolean newValue, Collection viewlets)
//        {
//          super();
//          this.newValue = newValue;
//          this.viewlets = viewlets;
//        }

//        public void actionPerformed(ActionEvent e)
//        {
//          ViewletImpl currentViewlet;
//          Iterator viewletsIterator = viewlets.iterator();
//          while(viewletsIterator.hasNext())
//          {
//            currentViewlet = (ViewletImpl) viewletsIterator.next();
//            currentViewlet.setHoldsOnUpdates(newValue);
//          }
//  	// trigger the jtable to update as a whole bunch of viewlets have just
//  	// changed
//  	((AbstractTableModel)table.getModel()).fireTableDataChanged();
//        }

//      }

  /**
   * The default text cell render
   */
//    private class MyRenderer extends DefaultTableCellRenderer {
//      public MyRenderer() {
//        super();
//        setHorizontalAlignment(SwingConstants.CENTER);
//      }

//      public Component getTableCellRendererComponent(JTable table,
//  						   Object value,
//  						   boolean isSelected,
//  						   boolean hasFocus,
//  						   int row,
//  						   int column) {
//        JLabel result =
//  	(JLabel)(super.getTableCellRendererComponent(table,
//  						     value,
//  						     isSelected,
//  						     hasFocus,
//  						     row,
//  						     column));
//        ViewletImpl viewlet = (ViewletImpl)value;
//        String updating = viewlet.getUpdating();
//        Color col = Color.white;
//        if ("no".equals(updating)) {
//  	// do nothing
//        } else if ("updatingForward".equals(updating)) {
//  	col = Color.green;
//        } else /* updatingBack */ {
//  	col = Color.red;
//        }
//        if (viewlet.getHoldsOnUpdates()) {
//  	col = col.darker();
//        }
//        if (isSelected) {
//  	col = col.darker().darker();
//        }
//        result.setBackground(col);
//        return result;
//      }
    
    
//      public void setValue(Object value) {
//        super.setValue(value);
//        ViewletImpl viewlet = (ViewletImpl)value;
//        setToolTipText(viewlet.getToolTipText());
//      }
//    }


  /**
   * This implements the abstract getViewletsAt method required by the
   * ContainerViewer class.
   */
  protected ViewletRange getAllViewletData()
  {
    return(viewletDataStore.getEntireViewletRange());
  }

  /** Returns any actions or sub-menus that should be available via a
      right-click popup menu */
  public Collection getViewerPopupMenuCollection() {
    Collection result = super.getViewerPopupMenuCollection();
    result.add(getPopupMenu("Graph"));
    result.add(getPopupMenu("Insert"));
    result.add(new PrintAction(this));
    return result;
  }


  void insertNewViewletAt(ViewletType newViewletType,
			  java.util.List index,
			  int col,
			  int row
			  ) {
    ViewletData newViewletData = ((ViewletFactory)newViewletType).build();
//      Object elementReference = new CompoundTermImpl("element", index);
//      newViewlet.setElementReference(elementReference);
    MultiViewletType.Data multiData =
        (MultiViewletType.Data)(((ViewletFactory)viewletType).build());
    multiData.setViewletType(newViewletType);
    multiData.setViewletData(newViewletData);

    try {
      viewletDataStore.setViewletDataAt(index, multiData);
      ViewletRange range = new ViewletRangeCollection();
      range.add( index );
      BatchGoal preBuildGoal =
        viewletType.collectPreBuildGoal(this,
                                        viewletDataStore,
                                        range);
      java.util.List goalResults;
      try {
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,
                                      "Executing preBuild goal for new viewlet");
        }
        
        goalResults = getStateModel().
          executeMultitaskBatchGoal(preBuildGoal);
        
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,
                                      "Completed preBuild goal for new viewlet");
        }
      } catch(EclipseException ee) {
        throw(new RuntimeException("EclipseException "+ee+" thrown while running "+
                                   "viewlet's pre-build goal"));
      } catch(IOException ioe) {
        throw(new RuntimeException("IOException "+ioe+" thrown while running "+
                                   "viewlet's pre-build goal"));
      }
      this.viewletType.startBuild(this, viewletDataStore, range, goalResults);
//      List existingViewlets = (List) indexToViewlets.get(index);
//      if(existingViewlets == null)
//      {
//        existingViewlets = new LinkedList();
//      }
//      List identifier = new LinkedList(index);
//      // distinguish this viewlet from others at the same index
//      identifier.add(new Integer(existingViewlets.size()));
//      newViewlet.setSymRef(new SymRef(newViewlet, getSymRef(), identifier));
//      existingViewlets.add(newViewlet);
//      allViewlets.add(newViewlet);
//      indexToViewlets.put(index, existingViewlets);

//      if (DebuggingSupport.logMessages) {
//  	DebuggingSupport.logMessage(this,"set of viewlets for index "+index+
//  				    " is now "+existingViewlets);
//      }
    } catch(RuntimeException re) {
      // re-throw the exception after cleaning up the state by
      // removing the MultiViewletData from the viewletDataStore (it
      // was inserted so that the "collectPreBuildGoal" could be
      // called on it)
      viewletDataStore.setViewletDataAt(index, null);
      throw re;
    }
    Node node = insertNode(viewletDataStore, index, multiData, viewletType);
    //node.setAttribute("color",Color.white);
    //node.setAttribute("style","filled");
    //node.setAttribute("label",newViewletData.toString());
    moveNodeToVirtualTable(node, col, row);
    //    addNewViewletComponent(newViewlet, col, row);
  }

  /**
   * Sets the position attribute for thisnode to be at the given
   * coordinates in a virtual table whose top-left corner is in the
   * top-left of the viewer window.
   * @param Node The node to position
   * @param col Column in virtual table (counting from 0)
   * @param row Row in virtual table (counting from 0)
   * */
  protected void moveNodeToVirtualTable(Node node,
				      int col,
				      int row) {

    double x, y;

    Rectangle2D prefSize = node.getGrappaNexus().getBounds2D();
    x = prefSize.getWidth()*col;
    y = prefSize.getHeight()*row;

    //    clearSelection();
    //    addToSelection(viewlet);

    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,
                                  "Adding node at (row,col)=("+row+","+col+
                                  ") and (x,y)=("+x+","+y+")");
    }
    node.setAttribute("pos",x+","+y);

  }



  /**
   * Called from the InsertViewletCommand command class, this
   * function actualy adds the given viewlets.
   * @param viewletType The type of viewlet to use for creating the viewlets
   * @param indices A 2D array where the first dimension is the
   *   viewable element dimension and the second contains the values
   *   that need to be instatiated in that dimension. All combinations
   *   of the given dimensions must be inserted.
   */
  void insertNewViewlet(ViewletType viewletType, int[][] indices)
  {
    java.util.List index = new ArrayList(indices.length);
    int[] counters = new int[indices.length];
    // record the index of the first non-unit dimension
    int columnDimensionIndex = 0;
    // record the index of the second non-unit dimension
    int rowDimensionIndex = -1;
    int nonUnitDimensions = 0;
    for(int i = 0 ; i < indices.length; i++) {
	counters[i] = indices[i].length-1;
	if (indices[i].length > 1) {
	    nonUnitDimensions++;
	    switch(nonUnitDimensions) {
	    case 1: {
		columnDimensionIndex = i;
		break;
	    }
	    case 2: {
		rowDimensionIndex = i;
		break;
	    }
	    default:{
		// do nothing for higher dimensions at the moment
	    }
	    }
	}
    }
    int sp = indices.length ; // stack pointer
    int count = 0;
    int sum;
    // Collect any indices for which an error occured whilst trying to
    // insert them
    Vector failedIndices = new Vector();
    do {
	if (sp > indices.length-1) {
	    int row = 0; // hold the row for this index
	    int col = 0; // hold the column for this index
	    // construct the list of indices
	    index.clear();
	    for(int i = 0; i < indices.length; i++) {
		index.add(new Integer(indices[i][counters[i]]));
		if (i == columnDimensionIndex) {
		    col = counters[i];
		}
		if (i == rowDimensionIndex) {
		    row = counters[i];
		}
	    }
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.
		    logMessage(this,
			       "insert viewlet index=" + index +
			       " col=" + col +
			       " row=" + row);
	    }
            try {
              insertNewViewletAt(viewletType, new ArrayList(index), col, row);
            } catch(RuntimeException e) {
              // store a copy of the index (must be a copy because the
              // variable 'index' is destructively modified)
              failedIndices.add(new ArrayList(index));
            }
	    count++;
	    sp--;
	}

	sum = 0 ;
	for (int i = 0 ; i <indices.length; i++ ) {
	    sum += counters[i];
	}
	if (sum == 0) {
	    break;
	}

	counters[sp]--;

	if (counters[sp] < 0) {
	    counters[sp] = indices[sp].length;
	    sp--;
	} else {
	    sp++;
	}

    } while(sum > 0);
    if (DebuggingSupport.logMessages) {
	DebuggingSupport.
	    logMessage(this,"number of combinations= "+count +
                       " number of failed insertions="+failedIndices.size());
    }
    if (!failedIndices.isEmpty()) {
      // inform the user that some insertions failed
      JList jlist = new JList(failedIndices);
      JOptionPane.showMessageDialog(null,
                                    new Object[] {"Viewlets could not be created for the following indices: ", new JScrollPane(jlist)});
    }
  }



  protected class AddViewletAction extends AbstractAction implements PropertyChangeListener
  {
    ViewletType viewletType;
    GraphViewer viewer;

    /** record the fact that the viewer is being destroyed */
    boolean destroyed;

    public AddViewletAction(GraphViewer viewer, ViewletType viewletType)
    {
      super("Insert "+viewletType.getDescription().toLowerCase());
      this.destroyed = false ;
      this.viewletType = viewletType;
      this.viewer = viewer;
      setEnabled(!getStateModel().getCanPerformRPC());
      getStateModel().getPropertyChangeSupport().
          addPropertyChangeListener("canPerformRPC", this);
      getPropertyChangeSupport().
	  addPropertyChangeListener("destroyEventIssued", this);
    }

    public void propertyChange(PropertyChangeEvent event)
    {
	if (event.getPropertyName().equals("destroyEventIssued")) {
	    destroyed = true;
	}
        if((destroyed) ||
	   !((Boolean) event.getNewValue()).booleanValue())
        {
          setEnabled(false);
        }
        else
        {
          setEnabled(true);
        }
    }

    public void actionPerformed(ActionEvent event)
    {
      java.util.List locationNamesList = new LinkedList();
      for(int i = 1; i <= getSize().size(); i++)
      {
        locationNamesList.add(getLocationNames(i));
      }
      JDialog dialog =
        new IndexChoosingDialog(getViewable().getNameAtom(), locationNamesList,
                                viewletType, viewer);
      dialog.show();

    }
  }

  protected class IndexChoosingDialog extends JDialog
  {
    private ViewletType viewletType;
    private GraphViewer viewer;
    private InsertAction insertAction;

    private JList[] locationSelection;


    // second parameter is list of lists of location names
    public IndexChoosingDialog(Object viewableName,
                               java.util.List locationNamesList,
                               ViewletType viewletType,
                               GraphViewer viewer)
    {
      super();
      setModal(true);
      this.setTitle("New "+
                    viewletType.getDescription().toLowerCase());
      this.viewletType = viewletType;
      this.viewer = viewer;
      insertAction = new InsertAction();
      initialiseLocationSelectors(locationNamesList);
      getContentPane().setLayout(new GridBagLayout());
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.fill = GridBagConstraints.BOTH;
      gbc.weighty = 1;
      gbc.weightx = 1;
      gbc.gridy = 1;
      gbc.insets = new Insets(10,10,10,10);
      getContentPane().add(locationSelectorPanel(), gbc);
      gbc.fill = GridBagConstraints.NONE;
      gbc.gridy = 0;
      getContentPane().add(new JLabel("Select viewable element location"),
			   gbc);
      gbc.gridy = 2;
      JPanel buttonPanel = new JPanel();
      buttonPanel.add(new ActionButton(insertAction));
      buttonPanel.add(new ActionButton(new CancelAction()));
      getContentPane().add(buttonPanel, gbc);
      pack();
    }

    private void addViewlets()
    {
      // hold all the selected indices in an array
      int[][] indices = new int[locationSelection.length][];

      for(int i = 0 ; i < locationSelection.length; i++)
      {
	indices[i] = locationSelection[i].getSelectedIndices();
	for(int j = 0; j < indices[i].length; j++ ) {
	    // increment the selected value to get the viewable index
	    // which counts from 1 instead of 0
	    indices[i][j] ++;
	}
      }
      //index.add(new Integer(locationSelection[i].getMinSelectionIndex()+1));
      (new InsertViewletCommand(viewer,
                                indices, viewletType)).issue();
    }

    private void initialiseLocationSelectors(java.util.List locationNamesList)
    {
      Iterator i = locationNamesList.iterator();
      locationSelection = new JList[locationNamesList.size()];
      java.util.List locationNames;
      int j = 0;
      while(i.hasNext())
      {
        locationNames = (java.util.List) i.next();
        locationSelection[j] = new JList(new Vector(locationNames));
        locationSelection[j].
	    setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        locationSelection[j].setVisibleRowCount(6);
	locationSelection[j].addListSelectionListener(insertAction);
	if (locationNames.size() == 1) {
	    // if there's only one dimension then select it
	    locationSelection[j].setSelectedIndex(0);
	    locationSelection[j].setEnabled(false);
	}
        j++;
      }
    }

    private JPanel locationSelectorPanel()
    {
      GridBagLayout layout = new GridBagLayout();
      JPanel panel = new JPanel(layout);
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.weightx = 1;
      gbc.insets = new Insets(10, 10, 10, 10);
      gbc.fill = GridBagConstraints.HORIZONTAL;
      gbc.anchor = GridBagConstraints.NORTH;
      gbc.gridx = 0;
      for(int i = 0; i < locationSelection.length; i++)
      {
        gbc.weighty = 0;
        gbc.gridy = 0;
        panel.add(new JLabel("Dimension "+(i+1)), gbc);
        gbc.weighty = 1;
        gbc.gridy = 1;
        panel.add(new JScrollPane(locationSelection[i]), gbc);
        gbc.gridx++;
      }
      return(panel);

    }

    private class InsertAction
	extends AbstractAction
	implements ListSelectionListener
    {
      InsertAction()
      {
        super("Insert");
        this.setEnabled(false);
      }
      public void valueChanged(ListSelectionEvent event)
      {
        boolean allDimensionsHaveSelection = true;
        for(int i = 0; i < locationSelection.length; i++)
        {
          if((locationSelection[i] == null) ||
	     (locationSelection[i].getMinSelectionIndex() < 0))
          {
            allDimensionsHaveSelection = false;
	    break;
          }
        }
        if(allDimensionsHaveSelection)
        {
          this.setEnabled(true);
        }
      }
      public void actionPerformed(ActionEvent event)
      {
        addViewlets();
        dispose();
      }
    }

    private class CancelAction extends AbstractAction
    {
      CancelAction()
      {
        super("Cancel");
      }
      public void actionPerformed(ActionEvent event)
      {
        dispose();
      }
    }

  }


  /**
   * Command to insert a viewlet on the desktop.
   */
  public static class InsertViewletCommand extends ViewerCommand {

    private SymRef viewletType;
    private int[][] index;

    public InsertViewletCommand(GraphViewer viewer,
                                int[][] index,
                                ViewletType viewletType) {
      super(viewer);
      this.viewletType = viewletType.getSymRef();
      this.index = index;
    }

    private ViewletType getViewletType() {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "InsertViewletCommand getting ViewletType from SymRef="+viewletType);
      }
      return((ViewletType) SymRef.get(viewletType));
    }

    public void postRecordIssue(){
      ((GraphViewer) getViewer()).insertNewViewlet(getViewletType(), index);
    }
  }

  /**
   * Configure graphics options for viewer
    Grappa.antiAliasText = false;
    Grappa.useAntiAliasing = false;
    Grappa.useFractionalMetrics = false;
   */
  protected class GrappaAntiAliasToggleAction extends AbstractAction {
    public GrappaAntiAliasToggleAction() {
      super("Toggle Hi-Quality");
    }
    public void actionPerformed(ActionEvent e) {
      boolean newState = !Grappa.useAntiAliasing;
      Grappa.useAntiAliasing = newState;
      //Grappa.useFractionalMetrics = newState;
      //Grappa.antiAliasText = newState;
      graph.repaint();
    }
  }

  protected void addImage(String filename) {
    Node node = new Node(graph, "image-"+imageCounter++);
    node.setAttribute("shape","box");
    node.setAttribute("label","");
    node.setAttribute("image",filename);
    graph.resetBoundingBox();
    graph.repaint();
  }

  protected class AddImageCommand extends ViewerCommand {
    String filename;

    public AddImageCommand(GraphViewer viewer, String filename) {
      super(viewer);
      this.filename = filename;
    }

    public void postRecordIssue() {
      ((GraphViewer)getViewer()).addImage(filename);
    }
  }

  /**
   * Action to query the user for an image and then insert a node with
   * the "image" attribute set to this image
   **/
  protected class AddImageAction extends AbstractAction {
    protected GraphViewer viewer;

    protected File defaultImageDirectory;

    public AddImageAction(GraphViewer viewer) {
      super("Image");
      this.viewer = viewer;
    }

    public void actionPerformed(ActionEvent event) {
      JFileChooser fileChooser = new ImageFileChooser();
      if(defaultImageDirectory != null)
      {
        fileChooser.setCurrentDirectory(defaultImageDirectory);
      }
      int returnVal = fileChooser.showOpenDialog(getComponent());
      if(returnVal == JFileChooser.APPROVE_OPTION)
      {

	  if (DebuggingSupport.logMessages) {
	      DebuggingSupport.logMessage(this, "image: "+
					  fileChooser.getSelectedFile()+
					  " approved");
	  }

        if(fileChooser.getSelectedFile().exists())
        {
//            (new DesktopViewerImportBackgroundCommand(
//                     desktopViewer,
//                     fileChooser.getSelectedFile())).issue();
          new AddImageCommand(viewer,
                              fileChooser.getSelectedFile().toString()).issue();
        }
        else
        {
          JOptionPane.
          showMessageDialog(getComponent(),
          "File \n"+fileChooser.getSelectedFile().toString()+"\ndoes not exist",
          "Error in Visualisation Client", JOptionPane.ERROR_MESSAGE);
        }

      }
      defaultImageDirectory = fileChooser.getCurrentDirectory();

    }
  }


  protected class ImageFileFilter extends javax.swing.filechooser.FileFilter
  {
    public String getDescription()
    {
      return("image file");
    }
    public boolean accept(File f)
    {
      return(f.toString().endsWith(".gif") || f.toString().endsWith(".GIF")||
             f.toString().endsWith(".png") || f.toString().endsWith(".PNG")||
             f.toString().endsWith(".jpg") || f.toString().endsWith(".JPG")||
             f.toString().endsWith(".jpeg") || f.toString().endsWith(".JPEG")||
             f.isDirectory()); // so directories will be displayed.
    }
  }

  protected class ImageFileChooser extends JFileChooser implements PropertyChangeListener
  {
    ImageFileChooser()
    {
      super();
      removeChoosableFileFilter(getAcceptAllFileFilter());
      // This means that if a directory is opened it is navigated to.
      setFileSelectionMode(FILES_ONLY);
      setFileFilter(new ImageFileFilter());
      // This means that when the directory is opened its name is not retained
      // as the selected file.
      addPropertyChangeListener(DIRECTORY_CHANGED_PROPERTY, this);
    }
    public void propertyChange(PropertyChangeEvent event)
    {
      setSelectedFile(null);
    }

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
        java.util.List index = (java.util.List)(it.next());
        Element element = getElement(index);
        // configure the renderer
        ViewletType type = elementToViewletType(index);
//          ViewletData data =
//            (ViewletData)(store.getViewletDataAt(index));
        type.customizeElement(store, index, element);
//          node.setAttribute("label",data.toString());
//          java.awt.Component component =
//            viewletType.getTableCellRenderer().getTableCellRendererComponent(null, data, false, false, 1, 1);
//          node.setAttribute("color",component.getBackground());
      }
      graph.repaint();
    }
  }

  /**
   * A graph whose getBoundingBox() method will return the same bounds
   * even if the underlying graph has changed, so long as the change
   * does not exceed the specified amount.
   * */
  protected static class SteadyGraph extends Graph {
    double softMargin;

    java.awt.geom.Rectangle2D oldBounds;

    public SteadyGraph(String name,
                       boolean directed,
                       boolean strict,
                       double softMargin) {
      super(name, directed, strict);
      this.softMargin = softMargin;
    }

    public java.awt.geom.Rectangle2D resetBoundingBox() {
      oldBounds = null;
      return super.resetBoundingBox();
    }

    public void repaint() {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "SteadyGraph repaint call on thread="+Thread.currentThread());
      }
      super.repaint();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "SteadyGraph repaint finishing on thread="+Thread.currentThread());
      }
    }

    public java.awt.geom.Rectangle2D getBoundingBox() {
      java.awt.geom.Rectangle2D newBounds = super.getBoundingBox();
      if (oldBounds == null) {
        oldBounds = newBounds;
      } else {
        double dx = Math.abs(newBounds.getWidth()-oldBounds.getWidth());
        double dy = Math.abs(newBounds.getHeight()-oldBounds.getHeight());
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this, "SteadyGraph dx="+dx+" dy="+dy);
        }
        if ((dx > softMargin) || (dy > softMargin)) {
          oldBounds = newBounds;
        }
      }
      return oldBounds;
    }
  }

  /**
   * Internal method to setup the background
   */
  void setBackground(File file) {
    backer.setFile(file);
    graph.repaint();
  }

  /**
   * Action to load/clear a background image
   */
  protected class SetBackgroundAction extends AbstractAction {
    protected GraphViewer viewer;

    protected File defaultImageDirectory;
    /** If set to false, then clear the background, else load*/
    protected boolean load;

    /**
     * An action to load/clear the background of the viewer
     * @param viewer The GraphViewer to set the background of
     * @param load if "true" then query the user for a file to load,
     * if "false" clear the background.
     **/
    public SetBackgroundAction(GraphViewer viewer, boolean load) {
      super((load)?"Import background image":"Clear background");
      this.viewer = viewer;
      this.load = load;
    }

    public void actionPerformed(ActionEvent event) {
      if (!load) {
        (new SetBackgroundCommand(viewer,null)).issue();
        return;
      }
      JFileChooser fileChooser = new ImageFileChooser();
      if(defaultImageDirectory != null)
      {
        fileChooser.setCurrentDirectory(defaultImageDirectory);
      }
      int returnVal = fileChooser.showOpenDialog(getComponent());
      if(returnVal == JFileChooser.APPROVE_OPTION)
      {

	  if (DebuggingSupport.logMessages) {
	      DebuggingSupport.logMessage(this, "image: "+
					  fileChooser.getSelectedFile()+
					  " approved");
	  }

        if(fileChooser.getSelectedFile().exists())
        {
          (new SetBackgroundCommand(viewer,
                                    fileChooser.getSelectedFile())).issue();
        }
        else
        {
          JOptionPane.
          showMessageDialog(getComponent(),
          "File \n"+fileChooser.getSelectedFile().toString()+"\ndoes not exist",
          "Error in Visualisation Client", JOptionPane.ERROR_MESSAGE);
        }

      }
      defaultImageDirectory = fileChooser.getCurrentDirectory();

    }
  }

  /**
   * Command to set the background
   */
  public static class SetBackgroundCommand extends ViewerCommand {
    File file;
    public SetBackgroundCommand(GraphViewer viewer, File file) {
      super(viewer);
      this.file = file;;
    }

    public void postRecordIssue() {
      ((GraphViewer)getViewer()).setBackground(file);
    }
  }


  /**
   * Draws a background image behind the graph
   */
  protected class GraphViewBacker implements GrappaBacker, GrappaConstants {
    Image image;

    public GraphViewBacker() {
      image = null;
    }

    public void setFile(File file) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "loading image: "+file);
      }
      if ( file == null) {
        image = null;
        return;
      }

      Image source = Toolkit.getDefaultToolkit().getImage(file.toString());
      MediaTracker tracker = new MediaTracker(graphPanel);
      tracker.addImage(source, 0);
      try {
        tracker.waitForID(0);
      } catch(InterruptedException e) {
        JOptionPane.showMessageDialog(null,
                                      "Problem loading background image:"+e);
        return;
      }
//        BufferedImage bsource = null;
      
//        try
//          {
//            bsource =
//              new BufferedImage(source.getWidth(viewletDesktop),
//                                source.getHeight(viewletDesktop),
//                                BufferedImage.TYPE_INT_BGR);
//          }
//        catch(Exception e)
//          {
//            JOptionPane.
//              showMessageDialog(getComponent(),
//                                "Image could not be loaded from file \n"+imageFile.toString(),
//                                "Error in Visualisation Client", JOptionPane.ERROR_MESSAGE);
//            return;
//          }
//        bsource.createGraphics().drawImage(source, 0,0, viewletDesktop);
      if (tracker.isErrorID(0)) {
        JOptionPane.showMessageDialog(null,
                                      "Unknown error occurred loading background image.");
        return;
      }
      image = source;
    }
    
    public void drawBackground(java.awt.Graphics2D g2d,
                               Graph graph,
                               java.awt.geom.Rectangle2D bbox,
                               java.awt.Shape clip) {
      if (image != null) {
        Shape oldClip = g2d.getClip();
        g2d.setClip(clip);
        g2d.drawImage(image,
                      (int)bbox.getX(),
                      (int)bbox.getY(),
                      (int)bbox.getWidth(),
                      (int)bbox.getHeight(),
                      null);
        g2d.setClip(oldClip);
      }
    }    
  }


  /**
   * Implement the Printable interface
   */
  public int print(Graphics graphics,
                   PageFormat pageFormat,
                   int pageIndex) throws PrinterException {
    int result = graphPanel.print(graphics, pageFormat, pageIndex);
    JViewport headerViewport=scrollPane.getColumnHeader();
    if (headerViewport!=null) {
      Component header = headerViewport.getView();
      if (header!=null) {
        if (header instanceof Printable) {
          ((Printable)header).print(graphics, pageFormat, pageIndex);
        }
      }
    }
    return result;
  }

  /**
   * Print the graph
   */
  protected class PrintAction extends AbstractAction {
    private GraphViewer viewer;

    PrintAction(GraphViewer viewer)
    {
      super("Print");
      this.viewer = viewer;
    }

    public void actionPerformed(ActionEvent event)
    {
      PageFormat pf = new PageFormat();
      Rectangle2D bb = viewer.graph.getBoundingBox();
      if(bb.getWidth() > bb.getHeight())
        pf.setOrientation(PageFormat.LANDSCAPE);
      try {
        PrinterJob printJob = PrinterJob.getPrinterJob();
        printJob.setPrintable(viewer, pf);
        if (printJob.printDialog()) {
          printJob.print();
        }
      } catch (Exception ex) {
        Grappa.displayException(ex, "Problem with print request");
      }
    }
  }



  /**
   * Move the given node and all edges associated with it
   */
  void moveNode(Node node, double dx, double dy) {
    GrappaPoint oldPos = (GrappaPoint)node.getAttributeValue("pos");
    GrappaPoint newPos = new GrappaPoint(oldPos.x + dx, oldPos.y + dy);
    node.setAttribute("pos", newPos);
    // loop through all incoming edges
    for(Iterator<Edge> en = node.inEdgeElements(); en.hasNext(); ) {
      Edge edge = en.next();
      GrappaLine line = (GrappaLine)edge.getAttributeValue("pos");
      String lineStr = line.toAttributeString();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "opdPos="+oldPos+" newPos=" + newPos + " edge="+ edge +" line="+lineStr + " edge.goesForward()="+edge.goesForward());
      }
      // replace the e,X,Y substring with e,X+dx,Y+dy
      StringTokenizer st = new StringTokenizer(lineStr,", ");
      String points = lineStr.substring(lineStr.indexOf(' '));
      // skip the first token
      String firstToken = st.nextToken();
      String newLineStr = lineStr;
      if ("e".equals(firstToken)) {
          double newX = Double.parseDouble(st.nextToken()) + dx;
          double newY =(Double.parseDouble(st.nextToken()) - dy);
          newLineStr = "e"+","+ newX + "," + newY + points;
      } else if ("s".equals(firstToken)) {
          double newX = Double.parseDouble(st.nextToken()) + dx;
          double newY =(Double.parseDouble(st.nextToken()) - dy);
          newLineStr = "s"+","+ newX + "," + newY + points;
      }
      edge.setAttribute("pos", newLineStr);
    }
    // loop through all outgoing edges
    for(Iterator<Edge> en = node.outEdgeElements(); en.hasNext(); ) {
      Edge edge = en.next();
      GrappaLine line = (GrappaLine)edge.getAttributeValue("pos");
      String lineStr = line.toAttributeString();
      StringTokenizer st = new StringTokenizer(lineStr,", ");
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "opdPos="+oldPos+" newPos=" + newPos + " edge="+ edge +" line="+lineStr);
      }
      String newLineStr = lineStr;
      if (lineStr == null || lineStr.length()==0) {
        break;
      }
      if (lineStr.charAt(0) == 's') {
        // replace the final location on the line
        int tailPos = lineStr.lastIndexOf(' ');
        String firstPart = lineStr.substring(0,tailPos);
        int lastComma = lineStr.lastIndexOf(',');
        double newX = Double.parseDouble(lineStr.substring(tailPos,lastComma)) + dx;
        double newY =(Double.parseDouble(lineStr.substring(lastComma+1)) - dy);
        newLineStr = firstPart + ' ' + newX + ',' + newY;
      } else if (lineStr.charAt(0) == 'e') {
        String head = lineStr.substring(0, lineStr.indexOf(' '));
        int tailStart = lineStr.indexOf(' ', head.length()+1);
        String tail = "";
        if (tailStart >= 0) {
          tail = lineStr.substring(tailStart);
        }
        // skip the first three token
        st.nextToken();
        st.nextToken();
        st.nextToken();
        // replace the point after the arrow location
        double newX = Double.parseDouble(st.nextToken()) + dx;
        double newY =(Double.parseDouble(st.nextToken()) - dy);
        newLineStr = head + " " +newX + "," + newY + tail;
      }
      edge.setAttribute("pos", newLineStr);
    }
    graph.resetBoundingBox();
    graph.repaint();
  }

  /**
   * Converts an element name (ie the string form of the Index) into a
   * java.util.List object
   **/
  java.util.List stringToIndexList(String s) {
    java.util.List l = new LinkedList();
    StringTokenizer st = new StringTokenizer(s.substring(1,s.length()-1),", ");
    while(st.hasMoreTokens()) {
      String t = st.nextToken();
      l.add(Integer.valueOf(t));
    }
    return l;
  }


  /**
   * Listens to click and selection events on the graph pane
   */
  protected static class GraphViewListener implements GrappaListener, GrappaConstants {

    /** holds the viewer for which this listener is listening */
    protected GraphViewer viewer;

    /** holds the previous drag point.  Used to calculate incremental
        position updates to selected elements when moving them.*/
    protected GrappaPoint previousDragPoint;

    /**
     * Setup the listener
     */
    public GraphViewListener(GraphViewer viewer) {
      this.viewer = viewer;
      this.previousDragPoint = null;
    }

    /**
     * The method called when a single mouse click occurs on a
     * displayed subgraph.
     *
     * @param subg displayed subgraph where action occurred
     * @param elem subgraph element in which action occurred
     * @param pt the point where the action occurred (graph coordinates)
     * @param modifiers mouse modifiers in effect
     * @param panel specific panel where the action occurred
     **/
    public void grappaClicked(Subgraph subg,
                              Element elem,
                              GrappaPoint pt,
                              int modifiers,
                              int clickCount,
                              GrappaPanel panel) {
	if((modifiers&InputEvent.BUTTON1_MASK) == InputEvent.BUTTON1_MASK) {
	    if(clickCount == 1) {
		// looks like Java has a single click occur on the way to a
		// multiple click, so this code always executes (which is
		// not necessarily a bad thing)
		if(subg.getGraph().isSelectable()) {
		    if(modifiers == InputEvent.BUTTON1_MASK) {
			// select element
			if(elem == null) {
			    if(subg.currentSelection != null) {
				if(subg.currentSelection instanceof Element) {
				    ((Element)(subg.currentSelection)).highlight &= ~HIGHLIGHT_MASK;
				} else {
				    Vector vec = ((Vector)(subg.currentSelection));
				    for(int i = 0; i < vec.size(); i++) {
					((Element)(vec.elementAt(i))).highlight &= ~HIGHLIGHT_MASK;
				    }
				}
				subg.currentSelection = null;
				subg.getGraph().repaint();
			    }
			} else {
			    if(subg.currentSelection != null) {
				if(subg.currentSelection == elem) return;
				if(subg.currentSelection instanceof Element) {
				    ((Element)(subg.currentSelection)).highlight &= ~HIGHLIGHT_MASK;
				} else {
				    Vector vec = ((Vector)(subg.currentSelection));
				    for(int i = 0; i < vec.size(); i++) {
					((Element)(vec.elementAt(i))).highlight &= ~HIGHLIGHT_MASK;
				    }
				}
				subg.currentSelection = null;
			    }
                            if (elem != subg) {
                                // do not allow selection of the whole
                                // (sub)graph
                                elem.highlight |= SELECTION_MASK;
                                subg.currentSelection = elem;
                            }
			    subg.getGraph().repaint();
			}
		    } else if(modifiers == (InputEvent.BUTTON1_MASK|InputEvent.CTRL_MASK)) {
			// adjust selection
			if(elem != null) {
			    if((elem.highlight&SELECTION_MASK) == SELECTION_MASK) {
				// unselect element
				elem.highlight &= ~SELECTION_MASK;
				if(subg.currentSelection == null) {
				// something got messed up somewhere
				    throw new InternalError("currentSelection improperly maintained");
				} else if(subg.currentSelection instanceof Element) {
				    if(((Element)(subg.currentSelection)) != elem) {
					// something got messed up somewhere
					throw new InternalError("currentSelection improperly maintained");
				    }
				    subg.currentSelection = null;
				} else {
				    Vector vec = ((Vector)(subg.currentSelection));
				    boolean problem = true;
				    for(int i = 0; i < vec.size(); i++) {
					if(((Element)(vec.elementAt(i))) == elem) {
					    vec.removeElementAt(i);
					    problem = false;
					    break;
					}
				    }
				    if(problem) {
					// something got messed up somewhere
					throw new InternalError("currentSelection improperly maintained");
				    }
				}
			    } else {
				// select element
				elem.highlight |= SELECTION_MASK;
				if(subg.currentSelection == null) {
				    subg.currentSelection = elem;
				} else if(subg.currentSelection instanceof Element) {
				    Object obj = subg.currentSelection;
				    subg.currentSelection = new Vector();
				    ((Vector)(subg.currentSelection)).add(obj);
				    ((Vector)(subg.currentSelection)).add(elem);
				} else {
				    ((Vector)(subg.currentSelection)).add(elem);
				}
			    }
			    subg.getGraph().repaint();
			}
		    }
		}
	    } else {
		// multiple clicks
		// this code executes for each click beyond the first
		//System.err.println("clickCount="+clickCount);
	    }
	}
    }

    /**
     *
     * The method called when a mouse press occurs on a displayed subgraph.
     *
     * @param subg displayed subgraph where action occurred
     * @param elem subgraph element in which action occurred
     * @param pt the point where the action occurred (graph coordinates)
     * @param modifiers mouse modifiers in effect
     * @param panel specific panel where the action occurred
     **/
    public void grappaPressed(Subgraph subg,
                              Element elem,
                              GrappaPoint pt,
                              int modifiers,
                              GrappaPanel panel) {
      if(modifiers == InputEvent.BUTTON1_MASK && subg.getGraph().isSelectable()) {
        previousDragPoint = null;
      }
    }

    /**
     *
     * The method called when a mouse release occurs on a displayed subgraph.
     *
     * @param subg displayed subgraph where action occurred
     * @param elem subgraph element in which action occurred
     * @param pt the point where the action occurred (graph coordinates)
     * @param modifiers mouse modifiers in effect
     * @param pressedElem subgraph element in which the most recent mouse press occurred
     * @param pressedPt the point where the most recent mouse press occurred (graph coordinates)
     * @param pressedModifiers mouse modifiers in effect when the most recent mouse press occurred
     * @param outline enclosing box specification from the previous drag position (for XOR reset purposes)
     * @param panel specific panel where the action occurred
     **/
    public void grappaReleased(Subgraph subg,
                               Element elem,
                               GrappaPoint pt,
                               int modifiers,
                               Element pressedElem,
                               GrappaPoint pressedPt,
                               int pressedModifiers,
                               GrappaBox outline,
                               GrappaPanel panel) {
      if(modifiers == InputEvent.BUTTON1_MASK && subg.getGraph().isSelectable()) {
        if(outline != null) {
          boolean xorOutline = false;
          if(subg.currentSelection != null) {
            if(subg.currentSelection instanceof Element) {
              ((Element)(subg.currentSelection)).highlight = 0;
            } else {
              Vector vec = ((Vector)(subg.currentSelection));
              for(int i = 0; i < vec.size(); i++) {
                ((Element)(vec.elementAt(i))).highlight = 0;
              }
            }
            subg.currentSelection = null;
          }
          List elems = GrappaSupport.findContainedElements(subg, outline);
          if(elems != null) {
            drillDown(subg, elems, Grappa.SELECTION_MASK, Grappa.HIGHLIGHT_ON);
            xorOutline = false;
          }
          if(!xorOutline) {
            subg.getGraph().paintImmediately();
          }
          if(xorOutline) {
            Graphics2D g2d = (Graphics2D)(panel.getGraphics());
            AffineTransform orig = g2d.getTransform();
            g2d.setTransform(panel.getTransform());
            g2d.setXORMode(Color.darkGray);
            g2d.draw(outline);
            g2d.setPaintMode();
            g2d.setTransform(orig);
          }
        }
      } else if(modifiers == (InputEvent.BUTTON1_MASK|InputEvent.CTRL_MASK) && subg.getGraph().isSelectable()) {
        if(outline != null) {
          List elems = GrappaSupport.findContainedElements(subg, outline);
          if(elems != null) {
            drillDown(subg, elems, Grappa.SELECTION_MASK, Grappa.HIGHLIGHT_TOGGLE);
            subg.getGraph().repaint();
          } else {
            Graphics2D g2d = (Graphics2D)(panel.getGraphics());
            AffineTransform orig = g2d.getTransform();
            g2d.setTransform(panel.getTransform());
            g2d.setXORMode(Color.darkGray);
            g2d.draw(outline);
            g2d.setPaintMode();
            g2d.setTransform(orig);
          }
        }
      } else if(modifiers == (InputEvent.BUTTON1_MASK|InputEvent.SHIFT_MASK) && subg.getGraph().isEditable()) {
        if(elem != null && pressedElem != null) {
          if(pressedModifiers == modifiers) {
            if(outline == null) {
              if(pressedElem == elem && pt.distance(pressedPt) < 5) {
                // [should we only allow elem.isSubgraph()?]
                // create node
                Attribute[] attrs = null;
                Attribute attr = subg.getNodeAttribute(Grappa.LABEL_ATTR);
                if(attr == null || attr.getValue().equals("\\N")) {
                  attrs = new Attribute[] { new Attribute(Grappa.NODE, POS_ATTR, pt), new Attribute(Grappa.NODE, LABEL_ATTR, "Node" + subg.getGraph().getId(Grappa.NODE)) };
                } else {
                  attrs = new Attribute[] { new Attribute(Grappa.NODE, POS_ATTR, pt) };
                }
                Element el = subg.createElement(Grappa.NODE,null,attrs);
                if(el != null) {
                  el.buildShape();
                  subg.getGraph().repaint();
                }
                subg.getGraph().repaint();
              } else if(pressedElem != elem && pressedElem.isNode() && elem.isNode()) {
                // create edge
                Object[] info = new Object[] { elem, null, pressedElem };
                Attribute[] attrs = new Attribute[] { new Attribute(Grappa.EDGE, POS_ATTR, new GrappaLine(new GrappaPoint[] { ((Node)pressedElem).getCenterPoint(), ((Node)elem).getCenterPoint() }, subg.getGraph().isDirected()?GrappaLine.TAIL_ARROW_EDGE:GrappaLine.NONE_ARROW_EDGE) ) };
                Element el = subg.createElement(Grappa.EDGE,info,attrs);
                if(el != null) {
                  el.buildShape();
                  subg.getGraph().repaint();
                }
              }
            }
          }
        }
      }
    }

    /**
     *
     * The method called when a mouse drag occurs on a displayed subgraph.
     *
     * @param subg displayed subgraph where action occurred
     * @param currentPt the current drag point
     * @param currentModifiers the current drag mouse modifiers
     * @param pressedElem subgraph element in which the most recent mouse press occurred
     * @param pressedPt the point where the most recent mouse press occurred (graph coordinates)
     * @param pressedModifiers mouse modifiers in effect when the most recent mouse press occurred
     * @param outline enclosing box specification from the previous drag position (for XOR reset purposes)
     * @param panel specific panel where the action occurred
     **/
    public void grappaDragged(Subgraph subg,
                              GrappaPoint currentPt,
                              int currentModifiers,
                              Element pressedElem,
                              GrappaPoint pressedPt,
                              int pressedModifiers,
                              GrappaBox outline,
                              GrappaPanel panel) {
	if((currentModifiers&InputEvent.BUTTON1_MASK) == InputEvent.BUTTON1_MASK) {
          if ((pressedElem != null) && (pressedElem != subg) &&
              ((currentModifiers & InputEvent.CTRL_MASK) == 0) &&
              viewer.moveable) {
            // move the selected elements
            double dx = 0.0;
            double dy = 0.0;
            if ( previousDragPoint != null) {
              dx = currentPt.x - previousDragPoint.x;
              dy = currentPt.y - previousDragPoint.y;
            }
            previousDragPoint = currentPt;
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this, "Moving dx="+dx+" dy="+dy+" currentSelection="+subg.currentSelection);
            }
            if(subg.currentSelection != null) {
              if (subg.currentSelection instanceof Subgraph) {
                // move the whole sub-graph
              } else if(subg.currentSelection instanceof Element) {
                Element element = ((Element)(subg.currentSelection));
//                  GrappaPoint oldPos = (GrappaPoint)element.getAttributeValue("pos");
//                  GrappaPoint newPos = new GrappaPoint(oldPos.x + dx, oldPos.y + dy);
//                  element.setAttribute("pos", newPos);
                if (element instanceof Node) {
                  new MoveNodeCommand(viewer, (Node)element, dx, dy).issue();
                }
              } else {
                Vector vec = ((Vector)(subg.currentSelection));
                for(int i = 0; i < vec.size(); i++) {
                  Element element = ((Element)(vec.elementAt(i)));
                  if (element instanceof Node) {
                    new MoveNodeCommand(viewer, (Node)element, dx, dy).issue();
                  }
//                    GrappaPoint oldPos = (GrappaPoint)element.getAttributeValue("pos");
//                    GrappaPoint newPos = new GrappaPoint(oldPos.x + dx, oldPos.y + dy);
//                    element.setAttribute("pos", newPos);
                }
              }
              //subg.getGraph().resetBoundingBox();
              //subg.getGraph().repaint();
            }
          } else if(currentModifiers == InputEvent.BUTTON1_MASK || currentModifiers == (InputEvent.BUTTON1_MASK|InputEvent.CTRL_MASK)) {
            // draw the selection box
		Graphics2D g2d = (Graphics2D)(panel.getGraphics());
		AffineTransform orig = g2d.getTransform();
		g2d.setTransform(panel.getTransform());
		g2d.setXORMode(Color.darkGray);
		if(outline != null) {
		    g2d.draw(outline);
		}
		GrappaBox box = GrappaSupport.boxFromCorners(pressedPt.x, pressedPt.y, currentPt.x, currentPt.y);
		g2d.draw(box);
		g2d.setPaintMode();
		g2d.setTransform(orig);
	    }
	}
    }

    /**
     *
     * The method called when a element tooltip is needed.
     *
     * @param subg displayed subgraph where action occurred
     * @param elem subgraph element in which action occurred
     * @param pt the point where the action occurred (graph coordinates)
     * @param modifiers mouse modifiers in effect
     * @param panel specific panel where the action occurred
     * @return the tip to be displayed or null
     **/
    public String grappaTip(Subgraph subg,
                            Element elem,
                            GrappaPoint pt,
                            int modifiers,
                            GrappaPanel panel) {
      if (elem==null) {
        return "";
      }
      String name = elem.getName();
      if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,"grappaTip name="+name);
      }
      java.util.List index = null;
      try {
        index = viewer.stringToIndexList(name);
      } catch(NumberFormatException nfe) {
        return null;
      }
      if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,"grappaTip index="+index);
      }
      String data = viewer.getToolTip(index);
      //ViewletData data = viewer.viewletDataStore.getViewletDataAt(index);
      if (data == null) {
        return "";
      }
      return data.toString();
    }


    protected void drillDown(Subgraph subg, List elems, int mode, int setting) {
      for(Object obj: elems) {
        if(obj instanceof Vector) {
          drillDown(subg, (Vector)obj, mode, setting);
        } else {
          GrappaSupport.setHighlight(((Element)obj), mode, setting);
          switch(setting) {
          case HIGHLIGHT_TOGGLE:
            if((((Element)obj).highlight&mode) == mode) {
              if(subg.currentSelection == null) {
                subg.currentSelection = obj;
              } else if(subg.currentSelection instanceof Element) {
                Object crnt = subg.currentSelection;
                subg.currentSelection = new Vector();
                ((Vector)(subg.currentSelection)).add(crnt);
                ((Vector)(subg.currentSelection)).add(obj);
              } else {
                ((Vector)(subg.currentSelection)).add(obj);
              }
            } else {
              if(subg.currentSelection == obj) {
                subg.currentSelection = null;
              } else if(subg.currentSelection instanceof Vector) {
                ((Vector)(subg.currentSelection)).remove(obj);
              }
            }
            break;
          case HIGHLIGHT_ON:
            if(subg.currentSelection == null) {
              subg.currentSelection = obj;
            } else if(subg.currentSelection instanceof Element) {
              Object crnt = subg.currentSelection;
              subg.currentSelection = new Vector();
              ((Vector)(subg.currentSelection)).add(crnt);
              ((Vector)(subg.currentSelection)).add(obj);
            } else {
              ((Vector)(subg.currentSelection)).add(obj);
            }
            break;
          case HIGHLIGHT_OFF:
            if(subg.currentSelection != null) {
              if(subg.currentSelection == obj) {
                subg.currentSelection = null;
              } else if(subg.currentSelection instanceof Vector) {
                ((Vector)(subg.currentSelection)).remove(obj);
              }
            }
            break;
          }
        }
      }
    }
  }
}

