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
import java.awt.*;
import java.beans.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.table.*;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;


/**
 * A text table viewer based on JTable
 */
public class TableViewer
    extends ViewletArrayViewer
    implements ZoomableViewer
{
  protected static int DEFAULT_ROW_HEIGHT = 70;
  protected static int DEFAULT_COLUMN_WIDTH = 100;

  private SpreadSheet table;
  private JScrollPane scrollpane;
    //private ScrollingViewletGrid scrollingViewletGrid;
  private boolean trackExpansions = false;
    //  private ViewletTracker viewletTracker;
  

    public TableViewer(//ViewletFactory viewletFactory,
		ViewletType viewletType,
		VisClientStateModel stateModel,
		Viewable viewable)
  {
    super(viewletType, stateModel, viewable);
    initialiseMenu();
  }

  private void initialiseMenu()
  {
    //JCheckBoxMenuItem trackExpansionsItem = new JCheckBoxMenuItem("Track expansions");
    //trackExpansionsItem.setModel(new BooleanPropertyModel("trackExpansions",
    //                          this, getPropertyChangeSupport()));
    //addMenuItem("Options", trackExpansionsItem);
  }

  protected Action getZoomToFitWidthAction()
  {
    return(new ZoomToFitWidthAction(this));
  }

  protected Action getZoomToFitHeightAction()
  {
    return(new ZoomToFitHeightAction(this));
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

    super.startEvent(event, goalResults);
  }

  private void initialiseComponent()
  {
    // disable the "trackUpdates" feature
    trackUpdatesMenuItem.setEnabled(false);
    //table = new JTable(getViewletArray());
    table = new SpreadSheet(getViewletDataStore(), null, null);
    table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    table.setAutoCreateColumnsFromModel(false);
    table.setColumnSelectionAllowed(true);
    table.setRowSelectionAllowed(true);
    table.setCellSelectionEnabled(true);
    // set the default cell renderer
    table.setDefaultRenderer((new Object()).getClass(),
			     viewletType.getTableCellRenderer());
    scrollpane = new JScrollPane(table);
    scrollpane.setPreferredSize(new Dimension(430, 200));
    //scrollingViewletGrid =
    //  new ScrollingViewletGrid(getViewletArray(), this);
    //viewletTracker = new ViewletTracker(scrollingViewletGrid);
    MouseViewletMenuUpPopper mouseViewletMenuUpPopper =
        new MouseViewletMenuUpPopper(this, table);
    // create the selection for this table
    //selection = new ViewletSelectionSpreadSheet((SpreadSheet)table, this);
    // set the row height to equal the column width
    table.setColumnWidth(DEFAULT_COLUMN_WIDTH);
    table.setRowHeight(DEFAULT_ROW_HEIGHT);
  }


  public Component getComponent()
  {
    return scrollpane;
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
    // scale the column widths and row heights
    table.setColumnWidth((int)(DEFAULT_COLUMN_WIDTH*zoomLevel));
    table.setRowHeight((int)(DEFAULT_ROW_HEIGHT*zoomLevel));
    table.revalidate();
  }

  public void zoomInByRatio(float zoomRatio)
  {
    // scale the column widths and row heights
    final int col_count = table.getColumnCount();
    final TableColumnModel col_model = table.getColumnModel();
    for(int i = 0; i < col_count; i++) {
      int w = col_model.getColumn(i).getPreferredWidth();
      col_model.getColumn(i).setPreferredWidth((int)(w*zoomRatio));
    }
    table.setRowHeight((int)(table.getRowHeight()*zoomRatio));
    table.revalidate();
  }

  // move the scrollbars so that the new row/column comes into view.
  private void scrollToTrackExpansion(ExpandEvent event)
  {
    //scrollingViewletGrid.scrollToTrackExpansion(event.getExpandingDimension());
  }

//    protected ViewletTracker getViewletTracker()
//    {
//      return(viewletTracker);
//    }


  private class ZoomToFitWidthAction extends AbstractAction
  {
    private TableViewer viewer;
    ZoomToFitWidthAction(TableViewer viewer)
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
    table.fitWidth();
    table.revalidate();
  }

  private class ZoomToFitHeightAction extends AbstractAction
  {
    private TableViewer viewer;
    ZoomToFitHeightAction(TableViewer viewer)
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
    table.fitHeight();
    table.revalidate();
  }

    //  public void setSelection(ViewletRange newSelection) {
      //(new ViewerSetSelectionCommand(this, newSelection)).issue();
    //  }

  Rectangle rangeToBounds(ViewletRange range) {
    if (((ViewableType.ArrayType)getViewable().getType()).getNDimensions() == 1) {
      int min=Integer.MAX_VALUE;
      int max=-1;
      for(Iterator it = range.iterator(); it.hasNext(); ) {
        java.util.List index = (java.util.List)it.next();
        int i = ((Integer)index.get(0)).intValue();
        if (i < min) {
          min = i;
        }
        if (i > max) {
          max = i;
        }
      }
      return new Rectangle(0,min-1,1,(max-min)+1);
    }
    int minCol=Integer.MAX_VALUE;
    int maxCol=-1;
    int minRow=Integer.MAX_VALUE;
    int maxRow=-1;
    for(Iterator it = range.iterator(); it.hasNext(); ) {
      java.util.List index = (java.util.List)it.next();
      int row = ((Integer)index.get(0)).intValue();
      int col = ((Integer)index.get(1)).intValue();
      if (col < minCol) {
        minCol = col;
      }
      if (col > maxCol) {
        maxCol = col;
      }
      if (row < minRow) {
        minRow = row;
      }
      if (row > maxRow) {
        maxRow = row;
      }
    }
    return new Rectangle(minCol-1,minRow-1,(maxCol-minCol)+1,(maxRow-minRow)+1);
  }

  /**
   * Called by the ViewerSetSelectionCommand
   */
  protected void setSelectionPrivate(ViewletRange newSelection) {
    DebuggingSupport.
	logMessage(this,
		   "setSelectionPrivate called with newSelection=" +
		   newSelection);
    if (newSelection.isEmpty()) {
      table.clearSelection();
      return;
    }
    // Find the bounding box for these viewlets and select everything
    // within that box
    //    Rectangle box = table.findBoundingBox(newSelection);
    Rectangle box = rangeToBounds(newSelection);
    table.setColumnSelectionInterval(box.x, box.x + box.width - 1);
    table.setRowSelectionInterval(box.y, box.y + box.height - 1);
  }


  /**
   * Returns a copy of the current selection
   */
  public ViewletRange getSelection() {
    return new SpreadSheetSelectionViewletRange(table);
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


}
