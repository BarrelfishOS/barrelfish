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
import javax.swing.table.TableModel;
import com.parctechnologies.eclipse.visualisation.viewers.*;
/**
 * Provides a concrete implementation of the ViewletRange
 * interface for lightweight viewers which use a SpreadSheet component.
 *
 * <p> Works by delegating all ViewletSelection activites to the
 * selection functions of the SpreadSheet.  */
public class SpreadSheetSelectionViewletRange implements ViewletRange
{
  /** Holds the spreadSheet underlying this selection */
  SpreadSheet ss;

  /**
   * Construct a selection for the given SpreadSheet
   */
  public SpreadSheetSelectionViewletRange(SpreadSheet ss) {
    this.ss = ss;
  }


  private class SelectionIterator implements Iterator {
    SpreadSheetModel model;
    int dimensions;

    int[] rows;
    int[] columns;

    int r;
    int c;

    public SelectionIterator(SpreadSheet ss) {
      this.model = (SpreadSheetModel)(ss.getModel());
      this.rows = ss.getSelectedRows();
      this.columns = ss.getSelectedColumns();
      this.r = 0;
      this.c = 0;
      // Get the dimensions of the underlying ViewletDataStore (if
      // there is one)
      if (this.model instanceof ViewletDataStore) {
        dimensions = ((ViewletDataStore)model).getNDimensions();
      } else {
        dimensions = 2;
      }
    }

    public boolean hasNext() {
      return (r < rows.length);
    }

    public Object next() {
      if (r >= rows.length) {
	throw new NoSuchElementException();
      }
      //DebuggingSupport.logMessage(this, "iterator next r="+r+" c="+c);
      //DebuggingSupport.logMessage(this, "iterator next rows[r]="+rows[r]);
      //DebuggingSupport.logMessage(this, "iterator next cols[c]="+columns[c]);
      //Object o = model.getValueAt(rows[r],columns[c]);

      // construct the index
      List index;
      // This 1D v.s. 2D iterator could/should be coded more efficiently
      if (dimensions == 1) {
        index = new ArrayList(1);
      } else {
        index = new ArrayList(2);
      }
      index.add(new Integer(rows[r]+1)); // viewable indices count from 1
      if (dimensions > 1) {
        index.add(new Integer(columns[c]+1));
      }
      // move to next cell
      c++;
      if ( c >= columns.length ) {
	c = 0;
	r++;
      }
      //return o;
      return index;
    }

    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  /**
   * Clone the selection
   */
  public Object clone() throws CloneNotSupportedException {
    return super.clone();
  }

  /** The collection functionality */
  public boolean isEmpty() {
    return
      (ss.getSelectedColumnCount() == -1) &&
      (ss.getSelectedRowCount() == -1);
  }

  List realList() {
    List list = new ArrayList();
    for(Iterator it = iterator(); it.hasNext(); ) {
      list.add(it.next());
    }
    return list;
  }

  public Object[] toArray() {
    return realList().toArray();
  }

  public Object[] toArray(Object[] array) {
    return realList().toArray(array);
  }

  public Iterator iterator() {
    return new SelectionIterator(this.ss);
  }

  public boolean remove(Object o) {
    throw new UnsupportedOperationException("Collection is not externaly mutable.");
  }

  public boolean addAll(Collection c) {
    throw new UnsupportedOperationException("Collection is not externaly mutable.");
  }

  public boolean retainAll(Collection c) {
    throw new UnsupportedOperationException("Collection is not externaly mutable.");
  }

  public boolean contains(Object o) {
    throw new UnsupportedOperationException("Collection is not directly accessible.");
  }

  public boolean add(Object o) {
    throw new UnsupportedOperationException("Collection is not externaly mutable.");
  }

  public int size() {
    //DebuggingSupport.logMessage(this, "size() = " + (ss.getSelectedRowCount() * ss.getSelectedColumnCount()));

    return ss.getSelectedRowCount() * ss.getSelectedColumnCount();
  }

  public void clear() {
    throw new UnsupportedOperationException("Collection is not externaly mutable.");
  }

  public boolean containsAll(Collection c) {
    throw new UnsupportedOperationException("Collection is not directly accessible.");
  }

  public boolean removeAll(Collection c) {
    throw new UnsupportedOperationException("Collection is not directly accessible.");
  }

}

