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

import javax.swing.table.*;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;

/**
 * Implements a SpreadSheetModel for an array of integers
 */
public class IntegerSpreadSheetModel
    extends AbstractTableModel
    implements SpreadSheetModel {

    int[][] array;
    int rowCount;
    int columnCount;

    /** We use this single object to return the contents of the int array */
    private MutableInteger value = new MutableInteger(0);

    public IntegerSpreadSheetModel(int rows, int cols) {
	rowCount = rows;
	columnCount = cols;
	array = new int[rows][cols];
    }

    public int getRowCount() {
	return rowCount;
    }

    public int getColumnCount() {
	return columnCount;
    }

    public Object getValueAt(int row, int col) {
	value.setValue(array[row][col]);
	return value;
    }

    public void setIntAt(int row, int col, int i) {
	array[row][col] = i;
	fireTableCellUpdated(row, col);
    }

    /**
     * Returns the name of the column at <code>rowIndex</code>.  This is used
     * to initialize the table's row header name.  Note: this name does
     * not need to be unique; two rows in a table can have the same name.
     *
     * @param	rowIndex	the index of the row
     * @return  the name of the row
     */
    public String getRowName(int rowIndex) {
	return "";
    }


    /**
     * Moveall values in the table toward zero
     */
    public void moveToZero() {
	for(int r = 0; r < rowCount; r++) {
	    for(int c = 0; c < columnCount; c++) {
		int v = array[r][c];
		if (v!=0) {
		    if (v<0) {
			array[r][c]++;
		    } else /* v>0 */{
			array[r][c]--;
		    }
		    fireTableCellUpdated(r,c);
		}
	    }
	}
    }

    public static class MutableInteger {
	int i = 0;
	public MutableInteger(int i) {
	    this.i = i;
	}

	public int intValue() {
	    return i;
	}

	public long longValue() {
	    return (long)i;
	}

	public void setValue(int i) {
	    this.i = i;
	}

	public String toString() {
	    return ""+i;
	}
    }

}
