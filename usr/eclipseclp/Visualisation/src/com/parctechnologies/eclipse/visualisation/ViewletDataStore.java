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
import javax.swing.table.*;
import com.parctechnologies.eclipse.*;

/**
 * A ViewletDataStore is an abstract mapping between indices and
 * ViewletData, intended to hold the ViewletType specific data for
 * viewable elements, size and fixity of the dimensions of a viewable
 * on the eclipse side.
 *
 * Instead of viewable elements, it contains ViewletData. Each viewlet
 * is produced from a ViewletFactory.
 *
 * All ViewletDataStores can be used as models for JTables and
 * SpreadSheets.
 *
 * <p> Note that ViewletDataStore is not responsible for maintaining
 * location names or the Viewable handle, only for keeping references
 * to them and providing access to them.  */
public interface ViewletDataStore extends SpreadSheetModel, SymRefable
{
    /**
     * dimNumber starts from 1. List should be a list of Strings.
     */
    public void setLocationNames(int dimNumber, List locationNames);

    /**
     * dimNumber starts from 1.
     */
    public void finishExpandDimension(int dimNumber);

    /**
     * dimNumber starts from 1.
     */
    public List getLocationNames(int dimNumber);

    /**
     * dimNumber & locNumber start from 1.
     */
    public String getLocationName(int dimNumber, int locNumber);

    /**
     * Set the Java handle to the ECLiPSe side viewable
     */
    public void setViewable(Viewable viewable);

    /**
     * Get the Java handle to the ECLiPSe side viewable
     */
    public Viewable getViewable();

    /**
     * Traverses the entire structure recursively to collect together
     * all the indices.  */
    public ViewletRange getEntireViewletRange();

    /**
     * Returns a data iterator for the given ViewletRange
     */
    public Iterator getViewletDataIterator(ViewletRange range);

    /**
     * Return the number of dimensions in this store.
     */
    public int getNDimensions();

    /** Return the length of each dimension */
    public List getSize();

    /**
     * Shrinks the ViewletArray to size newSize. Assumes the top-level
     * fixity is flexible.  */
    void shrinkTo(List newSize);

    /**
     * Expand dimension nested at level <code>dimension</code> by
     * one. If the parameter dimension is greater than 1 this means
     * that this.elements stays the same size and it is the
     * sub-ViewletArrays which expand. Otherwise, we have to expand
     * the top level by adding a new object to elements. This is
     * created using createSubArray.  */
    void startExpandDimension(int dimension);

    /**
     * Get the ViewletData for the given index
     */
    public ViewletData getViewletDataAt(List index);

    /**
     * Sets the ViewletData for the given index
     */
    public void setViewletDataAt(List index, ViewletData data);

    /**
     * Fire data changed events upto any listeneing guis
     */
    public void fireViewletRangeUpdated(ViewletRange range);

    /**
     * Adds a ViewletDataStoreListener
     */
    public void addViewletDataStoreListener(ViewletDataStoreListener listener);

    /**
     * Removes a ViewletDataStoreListener
     */
    public void removeViewletDataStoreListener(ViewletDataStoreListener listener);

    /**
     * Factory method for creating ViewletRange object from any given
     * collection.
     *
     * <p>This method allows DataStores to create their own
     * specialised ViewletRange classes which can be optimised to the
     * type of data store being used.  */
    public ViewletRange createRange(Collection indices);

    /**
     * Factory method for creating ViewletRange objects to cover all elements between the given start and end indices.
     *
     * <p>This method allows DataStores to create their own
     * specialised ViewletRange classes which can be optimised to the
     * type of data store being used.  */
    public ViewletRange createRange(List fromIndex, List toIndex);
}
