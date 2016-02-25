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
import javax.swing.*;
import com.parctechnologies.eclipse.*;

/**
 * Basic implementation of the ViewletDataStore interface
 */
public abstract class AbstractViewletDataStore
    extends AbstractTableModel
    implements ViewletDataStore
{
  protected static final String fixedString = "fixed";
  protected static final String flexibleString = "flexible";

  // holds symref for this store
  SymRef symRef;


  // Handle to the viewable for which this is a store
  protected Viewable viewable;

  // used to store the size expanded *from* in between a startExpandDimension
  // and a finishExpandDimension.
  protected List oldSize;

  /**
   * Array of Lists of Strings. Each element in the array relates to a
   * dimension. Each List is a list of strings: location names for the
   * corresponding dimension. <p>
   * locationNames[0] is a List of location names for dimension 1 in the
   * viewable,
   * locationNames[1] is for dimension 2 etc.
   */
  protected List[] locationNames;

  /**
   * Atom which states whether the top level dimension is fixed or flexible.
   */
  protected String topLevelFixity;

  /**
   * This is a list of Integers which lists the size of Elements recursively:
   * i.e. if Elements contained 7 2-d ViewletArrays, each one with dimensions
   * 4 x 8 then wholeSize = [7,4,8].
   */
  protected List wholeSize;

  /**
   * viewletFactory is used to create the new Viewlets which will populate the
   * ViewletArray.
   */
  protected ViewletFactory viewletFactory;


  /**
   * Holds the list of all listeners
   */
  protected List listenerList;

  /**
   * Construct a ViewletArray given a list of integers (size), a fixity list
   * (fixity) and a ViewletFactory to be used to populate it with Viewlets.
   *
   */
  public AbstractViewletDataStore(List size,
				  List fixity,
                                  Viewable viewable,
				  ViewletFactory viewletFactory)
  {
    int topLevelSize = ((Integer) size.get(0)).intValue();
    this.wholeSize = new LinkedList(size);
    this.topLevelFixity = (String) fixity.get(0);
    this.viewable = viewable;
    this.viewletFactory = viewletFactory;
    this.listenerList = new LinkedList();
    locationNames = new List[getNDimensions()];
  }

  /**
   * dimNumber starts from 1. List should be a list of Strings.
   */
  public void setLocationNames(int dimNumber, List locationNames)
  {
    this.locationNames[dimNumber - 1] = locationNames;

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "Location names of dimension "+
				    dimNumber+" set to "+locationNames);
    }

  }

    /**
     * dimNumber starts from 1.  Fire propertyChange events to
     * indicate that the store has changed shape */
    public void finishExpandDimension(int dimNumber)
    {
	
	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "notifying property change listeners of expand");
	}
	
	//propertyChangeSupport.firePropertyChange("arraySize", oldSize, wholeSize);
	switch( dimNumber ) {
	case 1 : {
	    int oldLimit = ((Integer)oldSize.get(0)).intValue();
	    int newLimit = ((Integer)wholeSize.get(0)).intValue();
	    fireTableRowsInserted(oldLimit,newLimit-1);
	    break;
	}
	case 2: {
	    // A new column has been added
	    //int oldLimit = ((Integer)oldSize.get(1)).intValue();
	    //int newLimit = ((Integer)whole.get(1)).intValue();
	    fireTableStructureChanged();
	    //fireTableColumnsInserted(oldLimit,newLimit-1);
	    break;
	}
	default: {
	    //do nothing
	    //fireTableStructureChanged();
	}
	}
    }

  /**
   * dimNumber starts from 1.
   */
  public List getLocationNames(int dimNumber)
  {
    return(locationNames[dimNumber - 1]);
  }

  /**
   * dimNumber & locNumber start from 1.
   */
  public String getLocationName(int dimNumber, int locNumber)
  {
    return((String) getLocationNames(dimNumber).get(locNumber - 1));
  }

  /**
   * Set the Java handle to the ECLiPSe side viewable
   */
  public void setViewable(Viewable viewable) {
    this.viewable = viewable;
  }
  
  /**
   * Get the Java handle to the ECLiPSe side viewable
   */
  public Viewable getViewable() {
    return viewable;
  }

  /**
   * Traverses the entire structure recursively to collect together all the
   * viewlet indices.
   */
    public ViewletRange getEntireViewletRange() {
        // create an index representing the first element in the viewable
        List start = new ArrayList(wholeSize.size());
        Integer one = new Integer(1);
        for(int i = 0; i < wholeSize.size(); i++) {
            start.add(one);
        }
        //    createdElementIndices = allCombinations(size);
        return createRange(start,wholeSize);
    }


    /**
     * Returns a data iterator for the given ViewletRange.  While the
     * specified ViewletRange.iterator() will return indices, this
     * Iterator will return copies of the ViewletData in the store.  */
    public Iterator getViewletDataIterator(ViewletRange range) {
	return new ViewletDataIterator(this, range);
    }


    protected static class ViewletDataIterator implements Iterator {
	Iterator it; // store the underlying range iterator
	ViewletDataStore store ;
	public ViewletDataIterator(ViewletDataStore store,
				   ViewletRange range) {
	    this.it = range.iterator();
	    this.store = store;
	}
	public boolean hasNext() {
	    return it.hasNext();
	}
	public Object next() {
	    List index = (List)(it.next());
            if (DebuggingSupport.logMessages) {
                DebuggingSupport.logMessage(this, "DataIterator index="+index);
            }
	    return store.getViewletDataAt(index);
	}
	public void remove() {
	    it.remove();
	}
    };



  public int getNDimensions()
  {
    return(getSize().size());
  }

  public List getSize()
  {
    return(wholeSize);
  }

  /**
   * Shrinks the ViewletArray to size newSize. Assumes the top-level fixity is
   * flexible.
   */
  public void shrinkTo(List newSize)
  {

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "shrinking to "+newSize);
      }


      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "notifying property change listeners of shrink");
      }
  }

  /**
   * Expand dimension nested at level <code>dimension</code> by one.
   * records the pre-expansion size in the variable oldSize.
   */
    public void startExpandDimension(int dimension)
  {
    Integer oldDimSize = (Integer) wholeSize.get(dimension-1);
    Integer newDimSize =
      new Integer(oldDimSize.intValue() + 1);
    oldSize = new LinkedList(wholeSize);
    wholeSize.set(dimension-1, newDimSize);

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this,
				    "Viewlet array expanding to size:"+
				    wholeSize);
    }
  }

  /**
   * Handy method to get the Viewlet at co-ordinates (row, column) in the case
   * of a 2-d ViewletArray
   */
  public ViewletData getViewletDataAt(int row, int column)
  {
    if(getSize().size() != 2)
    {
      return(null);
    }
    LinkedList index = new LinkedList();
    index.add(new Integer(row));
    index.add(new Integer(column));
    return(getViewletDataAt(index));
  }

  /**
   * Handy method to get the Viewlet at co-ordinate row in the case
   * of a 1-d ViewletArray
   */
  public ViewletData getViewletDataAt(int row)
  {
    if(getSize().size() != 1)
    {
      return(null);
    }
    LinkedList index = new LinkedList();
    index.add(new Integer(row));
    return(getViewletDataAt(index));
  }

    /**
     * This method must be overridden in sub-classes
     */
    public abstract ViewletData getViewletDataAt(List index);

    /**
     * This method must be overridden in sub-classes
     */
    public abstract void setViewletDataAt(List index, ViewletData data);


  /**
   * Fire data changed events upto any listeneing GUIs.
   *
   * <p>Default implemenation only fires table change events for 1 or
   * two dimension viewables. */
  public void fireViewletRangeUpdated(ViewletRange range) {
    SwingUtilities.invokeLater(new RangeUpdater(this,range));
    //new RangeUpdater(this,range).run();
  }

  /**
   * Adds a ViewletDataStoreListener
   */
  public synchronized void addViewletDataStoreListener(ViewletDataStoreListener listener) {
    listenerList.add(listener);
  }
  
  /**
   * Removes a ViewletDataStoreListener
   */
  public synchronized void removeViewletDataStoreListener(ViewletDataStoreListener listener) {
    listenerList.remove(listener);
  }
  


  //-----------BEGIN: Implement the TableModel methods------------------

  /**
   * Return number of rows
   */
  public int getRowCount() {
    int row =  ((Integer)wholeSize.get(0)).intValue();
    return row;
  }
  /**
   * Return number of columns
   */
  public int getColumnCount() {
    int col = 1;
    if (wholeSize.size() > 1) {
      col =  ((Integer)wholeSize.get(1)).intValue();
    }
    return col;
  }

  /**
   * Return the viewlet at the given location.
   * Note that the TableModel counts from 0, where as the Viewlet 
   * locations start from 1.
   */
  public Object getValueAt(int row, int col) {
    if (wholeSize.size() == 1) {
      return getViewletDataAt(row+1);
    }
    return getViewletDataAt(row+1, col+1);
  }

  /**
   * Returns the column name for the given column
   */
  public String getColumnName(int column) {
    if (wholeSize.size() == 1) {
      return "1";
    }
    return (String)(locationNames[1].get(column));
  }

  /**
   * Returns the column name for the given column
   */
  public String getRowName(int row) {
    return (String)(locationNames[0].get(row));
  }

  //public Class getColumnClass(int c) {return getValueAt(0, c).getClass();}
  public boolean isCellEditable(int row, int col) {
    return false;
  }

  //-----------ENDOF: Implement the TableModel methods------------------

  /**
   * Factory method for creating ViewletRange object from any given
   * collection.
   *
   * <p>This defualt implementation will create a generic
   * ViewletRange.  Subclasses can override this if specialised
   * structures make more sense.  */
  public ViewletRange createRange(Collection indices) {
    return new ViewletRangeCollection(indices);
  }
  

  /**
   * Given a fromList and toList of integers returns a list of all
   * lists X where X[i] is between fromList[i] and toList[i],
   * inclusive.
   *  
   * (e.g. for fromList = [1,1,1] toList = [3,2,2]) the method would
   * return
   *
   * [1,1,1], [1,1,2], [1,2,1], [1,2,2], [2,1,1], ..., [3,2,1],
   * [3,2,2].  */
  protected List allCombinations(List fromIndex, List toIndex) {
    List result = new LinkedList();
    if(toIndex.size() > 0)
    {
      Integer fromHead = (Integer) fromIndex.get(0);
      List fromTail = fromIndex.subList(1, fromIndex.size());
      Integer toHead = (Integer) toIndex.get(0);
      List toTail = toIndex.subList(1, toIndex.size());
      List subCombs = allCombinations(fromTail, toTail);
      Iterator subCombsIterator;
      List currentComb;
      for(int i = fromHead.intValue(); i <= toHead.intValue(); i++)
      {
        subCombsIterator = subCombs.iterator();
        while(subCombsIterator.hasNext())
        {
          currentComb = new LinkedList();
          currentComb.add(new Integer(i));
          currentComb.addAll((List) subCombsIterator.next());
          result.add(currentComb);
        }
      }
    }
    else
    {
      result.add(new LinkedList());
    }
    return(result);
  }

  /**
   * Factory method for creating ViewletRange objects to cover all
   * elements between the given start and end indices.
   *
   * <p>This defualt implementation will create a generic
   * ViewletRange.  Subclasses can override this if specialised
   * structures make more sense.  */
  public ViewletRange createRange(List fromIndex, List toIndex) {
    return new ViewletRangeCollection(allCombinations(fromIndex, toIndex));
  }

  protected class RangeUpdater implements Runnable {
    ViewletRange range;
    AbstractViewletDataStore store;
    public RangeUpdater(AbstractViewletDataStore store, ViewletRange range) {
      this.range = range;
      this.store = store;
    }
    public void run() {
      for(Iterator listenerIt =listenerList.iterator(); listenerIt.hasNext();) {
        ((ViewletDataStoreListener)(listenerIt.next())).rangeUpdated(store,range);
      }
      if (range == null) {
        store.fireTableDataChanged();
        return;
      }
      for(Iterator it = range.iterator();
          it.hasNext();
          ) {
        List index = (List)it.next();
        if (index.size() == 1) {
          int col = 0;
          int row = ((Integer)index.get(0)).intValue()-1;
          store.fireTableCellUpdated(row,col);
        } else {
          int row = ((Integer)index.get(0)).intValue()-1;
          int col = ((Integer)index.get(1)).intValue()-1;
          store.fireTableCellUpdated(row,col);
        }
      }
    }
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
