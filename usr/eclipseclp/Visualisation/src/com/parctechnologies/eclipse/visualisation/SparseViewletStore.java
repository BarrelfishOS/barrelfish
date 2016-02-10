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
 * A SparseViewletStore is a recursive data structure intended to
 * mirror the number, size and fixity of the dimensions of a certain
 * elements within a viewable on the eclipse side.
 *
 * <p> Instead of viewable elements, it contains viewlet data. Each
 * viewlet is produced from a ViewletFactory.
 *
 * <p> Note that ViewletArray is not responsible for maintaining
 * location names, only for keeping references to them and providing
 * access to them.
 * */
public class SparseViewletStore extends AbstractViewletDataStore implements MultiViewletDataStore
{
  /**
   *Hold the individual ViewletDataStores on a per type basis
   */
  private Map wrappedStoreMap;

  /**
   * lists the fixities of the viewable.
   * */
  private List fixity;

  /**
   * elements is a map, going from indices represented as lists to
   * ViewletData
   * */
  private Map elements;


  /**
   * Construct a SparseViewletStore given a list of integers (size), a
   * fixity list (fixity).
   * */
  public SparseViewletStore(List size,
                            List fixity,
                            Viewable viewable) {
    super(size, fixity, viewable, null);
    this.fixity = fixity;
    this.elements = new HashMap();
  }


  /**
   * Traverses the entire structure recursively to collect together all the
   * element indices.
   */
  public ViewletRange getEntireViewletRange()
  {
    //return super.getEntireViewletRange();
    return new ViewletRangeCollection(elements.keySet());
  }

  /**
   * Shrinks the ViewletArray to size newSize. Assumes the top-level fixity is
   * flexible.
   */
  public void shrinkTo(List newSize) {
    super.shrinkTo(newSize);
    // Indicate to the table that rows/columns have been removed
    fireTableStructureChanged();
  }

  /**
   * Expand dimension nested at level <code>dimension</code> by one.
   */
  public void startExpandDimension(int dimension) {
    super.startExpandDimension(dimension);
  }


  /**
   * Implements the abstract getViewletDataAt method
   */
  public ViewletData getViewletDataAt(List index) {
    if(index.size() != getSize().size()) {
      throw(new IllegalArgumentException());
    }
    return((ViewletData) getElement(index));
  }

  /**
   * Implements the abstract setViewletDataAt method
   */
  public void setViewletDataAt(List index, ViewletData data) {
    if(index.size() != getSize().size()) {
      throw(new IllegalArgumentException());
    }
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "SparseViewletStore setViewletDataAt index="+index+" data="+data);
    }
    if (data != null) {
      setElement(index, data);
    } else {
      removeElement(index);
    }
  }

  /**
   * Get element with location index within the ViewletArray. Result may be a
   * ViewletArray or a Viewlet, depending on the length of the index parameter.
   */
  private Object getElement(List index)
  {
    Object result = elements.get(index);
    return result;
  }

  /**
   * Sets the element with location index within the ViewletArray. */
  private void setElement(List index, Object data)
  {
    elements.put(index, data);
  }

  /**
   * Removes the element with location index within the ViewletArray. */
  private void removeElement(List index)
  {
    elements.remove(index);
  }

  /**
   * Factory method for creating ViewletRange object from any given
   * collection.
   *
   * <p>This defualt implementation will create a generic
   * ViewletRange.  Subclasses can override this if specialised
   * structures make more sense.  */
  public ViewletRange createRange(Collection indices) {
    ViewletRange range = new ViewletRangeCollection();
    // filter the range to only include those indices in the current
    // map
    for(Iterator it = indices.iterator(); it.hasNext(); ) {
      Object index = it.next();
      if (elements.get(index)!=null) {
        range.add(index);
      }
    }
    return range;
  }
  

  /**
   * Factory method for creating ViewletRange objects to cover all
   * elements between the given start and end indices.
   *
   * <p>This defualt implementation will create a generic
   * ViewletRange.  Subclasses can override this if specialised
   * structures make more sense.  */
  public ViewletRange createRange(List fromIndex, List toIndex) {
    ViewletRange indices = super.createRange(fromIndex, toIndex);
    ViewletRange range = new ViewletRangeCollection();
    // filter the range to only include those indices in the current
    // map
    for(Iterator it = indices.iterator(); it.hasNext(); ) {
      Object index = it.next();
      if (elements.get(index)!=null) {
        range.add(index);
      }
    }
    return range;
  }

    /**
     * Returns a wrapper store which intercedes whenever viewlet data
     * is get or set.
     *
     * Implements method from the MultiViewletDataStore interface
     * */
    public ViewletDataStore getViewletDataStore(ViewletType type) {
      // Note the outer test is not synchronized to speed up normal code flow
      // but the test is duplicated within
      if (wrappedStoreMap == null) {
        synchronized(this) {
          if (wrappedStoreMap == null) {
            wrappedStoreMap=new HashMap();
          }
        }
      }
      ViewletDataStore result = (ViewletDataStore)wrappedStoreMap.get(type);
      if (result == null) {
        synchronized(this) {
          if (result == null) {
            result = 
              new WrappedMultiViewletDataStore(this, type);
            result.setSymRef(new SymRef(result,this.getSymRef(),type.getClass().getName()));
            wrappedStoreMap.put(type, result);
          }
        }
      }
      return result;
    }

}
