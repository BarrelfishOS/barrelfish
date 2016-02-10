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
import javax.swing.event.*;
import javax.swing.border.*;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;

/**
 * Extends the JTable swing componennt to provide a more spread sheet
 * like functionality.
 * <P>
 * Includes RowHeader equivalents to the column header of JTable.
 **/
public class SpreadSheet extends JTable {

    /** Implements the table row hedaers */
    protected Component tableRowHeader ;

    public SpreadSheet(TableModel dm,
		       TableColumnModel cm,
		       ListSelectionModel sm) {
	super(dm, cm, sm);
    }

    /**
     * If this <code>JTable</code> is the <code>viewportView</code> of an enclosing <code>JScrollPane</code>
     * (the usual situation), configure this <code>ScrollPane</code> by, amongst other things,
     * installing the table's <code>tableHeader</code> as the <code>columnHeaderView</code> of the scroll pane.
     * When a <code>JTable</code> is added to a <code>JScrollPane</code> in the usual way,
     * using <code>new JScrollPane(myTable)</code>, <code>addNotify</code> is
     * called in the <code>JTable</code> (when the table is added to the viewport).
     * <code>JTable</code>'s <code>addNotify</code> method in turn calls this method,
     * which is protected so that this default installation procedure can
     * be overridden by a subclass.
     *
     * @see #addNotify
     */
    protected void configureEnclosingScrollPane() {
	super.configureEnclosingScrollPane();
        Container p = getParent();
        if (p instanceof JViewport) {
            Container gp = p.getParent();
            if (gp instanceof JScrollPane) {
                JScrollPane scrollPane = (JScrollPane)gp;
                // Make certain we are the viewPort's view and not, for
                // example, the rowHeaderView of the scrollPane -
                // an implementor of fixed columns might do this.
                JViewport viewport = scrollPane.getViewport();
                if (viewport == null || viewport.getView() != this) {
                    return;
                }
                //scrollPane.setRowHeaderView(getTableHeader());
		Component header = getTableRowHeader();
                scrollPane.setRowHeaderView(header);
		scrollPane.getRowHeader().setPreferredSize(header.getPreferredSize());
            }
        }
    }

    /**
     * Returns the table row header component
     *
     */
    protected Component getTableRowHeader() {
	if (tableRowHeader == null) {
	    // initialy just return a table set to display the row headers
	    RowHeaderModel model = new RowHeaderModel();
	    JTable table = new JTable(model);
	    // Add a listener to the main TableModel to listen for
	    // row-change events and to update the row header
	    // accordingly
	    getModel().addTableModelListener(model);	    

	    // Set theintercell spacing to zero
	    table.setIntercellSpacing(new Dimension(0,0));
	    table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	    table.setCellSelectionEnabled(false);
	    table.setOpaque(false);
	    TableColumn column = table.getColumn("row");

	    // Set the renderer for the row headings
	    TableCellRenderer headerRenderer =
		new RowHeaderCellRenderer();
	    // Works in JDK 1.3 but not JDK 1.2
	    //TableCellRenderer headerRenderer =
	    //  getTableHeader().getDefaultRenderer();

	    //headerRenderer.setBackground(getParent().getBackground());
	    column.setCellRenderer(headerRenderer);
	    tableRowHeader = table;
	}
	return tableRowHeader;
    }

  /**
   * Set the height of each row
   */
  public void setRowHeight(int height) {
    DebuggingSupport.logMessage(this, "setRowHeight for height="+height);
    // resize the row headers aswell
    ((JTable)getTableRowHeader()).setRowHeight(height);
    super.setRowHeight(height);
  }
  
  /**
   * Set the width of each column
   */
  public void setColumnWidth(int width) {
    DebuggingSupport.logMessage(this, "setColumnWidth for width="+width);
    final int col_count = getColumnCount();
    final TableColumnModel col_model = getColumnModel();
    for(int i = 0; i < col_count; i++) {
      col_model.getColumn(i).setPreferredWidth(width);
    }
  }
  
  /**
   * Set the height of each row
   */
  public void setRowHeight(int row, int height) {
    // resize the row headers aswell
    // only works in JDK 1.3
    //if (tableRowHeader != null) {
    //  ((JTable)tableRowHeader).setRowHeight(row, height);
    //}
    //super.setRowHeight(row, height);
  }

  /**
   * Get the height of each row
   */
  public int getRowHeight(int row) {
    int height ;
    // resize the row headers aswell
    //if (tableRowHeader == null) {
      height = super.getRowHeight();
      //} else {
      //height = ((JTable)tableRowHeader).getRowHeight(row);
      //}
    return height;
  }

  /**
   * Get the height of any table row
   */
  public int getRowHeight() {
    return getRowHeight(0);
  }


  /**
   * Resize columns to fit width
   */
  public void fitWidth() {
    final int total = getParent().getWidth();
    final int col_count = getColumnCount();
    final TableColumnModel col_model = getColumnModel();
    for(int i = 0; i < col_count; i++) {
      col_model.getColumn(i).
	setPreferredWidth((((i+1)*total)/col_count)-((i*total)/col_count));
    }
  }

  /**
   * Resize rows to fit height
   */
  public void fitHeight() {
    final int total = getParent().getHeight();
    final int row_count = getRowCount();
    // Poorly supported variable row width code, so cannot set each
    // row seperatly and must use a single common value
    //for(int i = 0; i < row_count; i++) {
    //  setRowHeight(i, (((i+1)*total)/row_count)-((i*total)/row_count));
    //}
    setRowHeight(Math.max(3,total/row_count));
  }


//    /**
//     * Find bounding box for these elements within the spreadsheet.
//     * @return Rectangle where x=min column, y = min row, w=number of
//     * columns and h=number of rows
//     */
//    public Rectangle findBoundingBox(Collection elements) {
//      SpreadSheetModel model = (SpreadSheetModel)(getModel());
//      int col1 = Integer.MAX_VALUE;
//      int row1 = Integer.MAX_VALUE;
//      int col2 = -1;
//      int row2 = -1;
//      boolean changedObjectMidWay = false;
//      Object o = null;
//      for(Iterator it = elements.iterator(); it.hasNext(); ) {
//        if (!changedObjectMidWay) {
//  	o = it.next();
//        } else {
//  	changedObjectMidWay = false;
//        }
//        int c=0;
//        int r=0;
//        for(r = 0; (r < model.getRowCount()) && (o != null); r++) {
//  	for(c = 0; c < model.getColumnCount(); c++) {
//  	  if (getValueAt(r,c) == o) {
//  	    // found one
//  	    if (c < col1) {
//  	      col1 = c;
//  	    }
//  	    if (c > col2) {
//  	      col2 = c;
//  	    }
//  	    if (r < row1) {
//  	      row1 = r;
//  	    }
//  	    if (r > row2) {
//  	      row2 = r;
//  	    }
//  	    if (it.hasNext()) {
//  	      o = it.next();
//  	      changedObjectMidWay = true;
//  	    } else {
//  	      o = null;
//  	      break;
//  	    }
//  	  }
//  	}
//        }
//      }
//      return new Rectangle(col1,row1,(col2-col1)+1,(row2-row1)+1);
//    }


  protected class RowHeaderModel
    extends AbstractTableModel
    implements TableModelListener {
    public int getRowCount() { return getModel().getRowCount(); }
    public int getColumnCount() { return 1; }
    public String getColumnName(int col) { return "row"; }
    public Object getValueAt(int row, int col) {
      return ((SpreadSheetModel)getModel()).getRowName(row);
    }
    
    public void tableChanged(TableModelEvent e) {
      if ((e.getType() == e.INSERT) ||
	  (e.getType() == e.DELETE)) {
	// fire row update events for the
	// row-header model to keep the header in
	// sync with the main datamodel
	fireTableChanged(new TableModelEvent(this,
					     e.getFirstRow(),
					     e.getLastRow(),
					     e.ALL_COLUMNS,
					     e.getType()));
      }
    }
  }

  protected class RowHeaderCellRenderer extends DefaultTableCellRenderer {
    protected Border border;

    public RowHeaderCellRenderer() {
      super();
      setHorizontalAlignment(SwingConstants.CENTER);
    }


    public Component getTableCellRendererComponent(JTable table,
						   Object value,
						   boolean isSelected,
						   boolean hasFocus,
						   int row,
						   int column) {
      Component result = super.getTableCellRendererComponent(table,
							     value,
							     isSelected,
							     hasFocus,
							     row,
							     column);
      if (border == null) {
	border = BorderFactory.createBevelBorder(BevelBorder.RAISED);
      }
      ((JLabel)result).setBorder(border);
      ((JLabel)result).setOpaque(false);
      return result;
    }
  }
    
}




