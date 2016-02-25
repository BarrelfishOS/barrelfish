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
import java.beans.*;
import javax.swing.event.*;
import javax.swing.table.*;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;
/**
 * A WrappedMultiViewletDataStore wrapps a ViewletDataStore which
 * contains MultiViewletType.Data and intercedes whenever viewlet data
 * is get or set.
 * */
class WrappedMultiViewletDataStore implements ViewletDataStore {
    
    /** The underlying store */
    protected ViewletDataStore s;

    /** The type to allow access to */
    protected ViewletType type;

    /** hold the symref for this store */
    protected SymRef symRef;

    public WrappedMultiViewletDataStore(ViewletDataStore s, ViewletType type) {
        this.s = s;
        this.type = type;
    }

    /** Return a range which only refers to viewlets of the specific type*/
    protected ViewletRange filterRange(ViewletRange range, ViewletType type) {
        ViewletRange newRange = new ViewletRangeCollection();
        for(Iterator it = range.iterator(); it.hasNext(); ) {
            List index = (List) it.next();
            MultiViewletType.Data data =
                (MultiViewletType.Data) s.getViewletDataAt(index);
            if (data.getViewletType() == type) {
                newRange.add(index);
            }
        }
        return newRange;
    }

    /**
     * dimNumber starts from 1. List should be a list of Strings.
     */
    public void setLocationNames(int dimNumber, List locationNames) {
        s.setLocationNames(dimNumber, locationNames);
    }

    /**
     * dimNumber starts from 1.
     */
    public void finishExpandDimension(int dimNumber) {
        s.finishExpandDimension(dimNumber);
    }
    
    /**
     * dimNumber starts from 1.
     */
    public List getLocationNames(int dimNumber) {
        return s.getLocationNames(dimNumber);
    }

    /**
     * dimNumber & locNumber start from 1.
     */
    public String getLocationName(int dimNumber, int locNumber) {
        return s.getLocationName(dimNumber, locNumber);
    }
    
    /**
     * Set the Java handle to the ECLiPSe side viewable
     */
    public void setViewable(Viewable viewable) {
        s.setViewable(viewable);
    }
    
    /**
     * Get the Java handle to the ECLiPSe side viewable
     */
    public Viewable getViewable() {
        return s.getViewable();
    }
    
    /**
     * Traverses the entire structure recursively to collect together
     * all the indices.  */
    public ViewletRange getEntireViewletRange() {
        return filterRange(s.getEntireViewletRange(), type);
    }
    
    /**
     * Returns a data iterator for the given ViewletRange
     */
    public Iterator getViewletDataIterator(ViewletRange range) {
      return new AbstractViewletDataStore.ViewletDataIterator(this, filterRange(range, type));
      //        return s.getViewletDataIterator(filterRange(range, type));
    }

    /**
     * Return the number of dimensions in this store.
     */
    public int getNDimensions() {
        return s.getNDimensions();
    }
    
    /** Return the length of each dimension */
    public List getSize() {
        return s.getSize();
    }
    
    /**
     * Shrinks the ViewletArray to size newSize. Assumes the top-level
     * fixity is flexible.  */
    public void shrinkTo(List newSize) {
        s.shrinkTo(newSize);
    }
    
    /**
     * Expand dimension nested at level <code>dimension</code> by
     * one. If the parameter dimension is greater than 1 this means
     * that this.elements stays the same size and it is the
     * sub-ViewletArrays which expand. Otherwise, we have to expand
     * the top level by adding a new object to elements. This is
     * created using createSubArray.  */
    public void startExpandDimension(int dimension) {
        s.startExpandDimension(dimension);
    }
    
    /**
     * Get the ViewletData for the given index
     */
    public ViewletData getViewletDataAt(List index) {
        return ((MultiViewletType.Data)s.getViewletDataAt(index)).getViewletData();
    }
    
    /**
     * Sets the ViewletData for the given index
     */
    public void setViewletDataAt(List index, ViewletData data) {
        MultiViewletType.Data multiData =
            (MultiViewletType.Data)s.getViewletDataAt(index);
        multiData.setViewletData(data);
        s.setViewletDataAt(index, multiData);
    }
    
    /**
     * Fire data changed events upto any listeneing guis
     */
    public void fireViewletRangeUpdated(ViewletRange range) {
        s.fireViewletRangeUpdated(range);
    }
    
    /**
     * Adds a ViewletDataStoreListener
     */
    public void addViewletDataStoreListener(ViewletDataStoreListener listener) {
        s.addViewletDataStoreListener(listener);
    }
    
    /**
     * Removes a ViewletDataStoreListener
     */
    public void removeViewletDataStoreListener(ViewletDataStoreListener listener) {
        s.removeViewletDataStoreListener(listener);
    }
    
    /**
     * Factory method for creating ViewletRange object from any given
     * collection.
     *
     * <p>This method allows DataStores to create their own
     * specialised ViewletRange classes which can be optimised to the
     * type of data store being used.  */
    public ViewletRange createRange(Collection indices) {
        return filterRange(s.createRange(indices), type);
    }
    
    /**
     * Factory method for creating ViewletRange objects to cover all elements between the given start and end indices.
     *
     * <p>This method allows DataStores to create their own
     * specialised ViewletRange classes which can be optimised to the
     * type of data store being used.  */
    public ViewletRange createRange(List fromIndex, List toIndex) {
        return filterRange(s.createRange(fromIndex, toIndex), type);
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
        return s.getRowName(rowIndex);
    }
    
    /**
     * Return number of rows
     */
    public int getRowCount() {
        return s.getRowCount();
    }
    
    /**
     * Return number of columns
     */
    public int getColumnCount() {
        return s.getColumnCount();
    }
    
    /**
     * Return the viewlet at the given location.
     * Note that the TableModel counts from 0, where as the Viewlet 
     * locations start from 1.
     */
    public Object getValueAt(int row, int col) {
        return s.getValueAt(row,col);
    }
    
    /**
     * Returns the column name for the given column
     */
    public String getColumnName(int column) {
        return s.getColumnName(column);
    }
    
    //public Class getColumnClass(int c) {return getValueAt(0, c).getClass();}
    public boolean isCellEditable(int row, int col) {
        return s.isCellEditable(row, col);
    }
    
    
    public Class getColumnClass(int i) {
        return s.getColumnClass(i);
    }
    
    public void setValueAt(Object o, int r, int c) {
        // should never be called
    }
    
    public void addTableModelListener(TableModelListener l) {
        s.addTableModelListener(l);
    }
    public void removeTableModelListener(TableModelListener l) {
        s.removeTableModelListener(l);
    }

    /** implments the symrefable interface*/
    public void setSymRef(SymRef symRef) {
        this.symRef = symRef;
    }
    /** implments the symrefable interface*/
    public SymRef getSymRef() {
        return this.symRef;
    }
}
