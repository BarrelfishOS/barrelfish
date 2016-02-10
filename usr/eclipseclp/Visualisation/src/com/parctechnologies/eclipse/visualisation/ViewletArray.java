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
 * A ViewletArray is a recursive data structure intended to mirror the number,
 * size and fixity of the dimensions of a viewable on the eclipse side.
 *
 * Instead of viewable elements, it contains viewlets. Each viewlet is produced
 * from a ViewletFactory.
 * <p>
 * Note that ViewletArray is not responsible for maintaining location names,
 * only for keeping references to them and providing access to them.
 */
public class ViewletArray extends AbstractViewletDataStore implements MultiViewletDataStore
{
  /**
   * This is a list of Integers which denotes this ViewletArray's location
   * within the outermost ViewletArray. E.g. a top-level ViewletArray with
   * size [4,3] would have location [], and its elements would have location
   * [1], [2], [3], [4] and the elments of the first of these would have
   * locations [1,1], [1,2] and [1,3].
   */
  private List location;

  /**
   * subFixity lists the fixities of Elements recursively. So a ViewletArray
   * which mirrored a viewable with fixity [fixed, fixed, flexible] would have
   * subFixity [fixed, flexible].
   *
   * */
  private List subFixity;

  /**
   * elements is an instance of inner class Elements. If this ViewletArray is
   * one-dimensional, the contents of elements are the viewlets of the
   * ViewletArray. If this ViewletArray is multi-dimensional, then the contents
   * of elements are the sub-ViewletArrays.
   *
   */
  private Elements elements;

  /**
   *Hold the individual ViewletDataStores on a per type basis
   */
  private Map wrappedStoreMap;

  /**
   * Construct a ViewletArray given a list of integers (size), a fixity list
   * (fixity) and a ViewletFactory to be used to populate it with Viewlets.
   *
   */
  public ViewletArray(List size,
		      List fixity,
                      Viewable viewable,
		      ViewletFactory viewletFactory)
  {
    this(size, fixity, viewable, viewletFactory, Collections.EMPTY_LIST);
  }


  /**
   * Construct a ViewletArray given a list of integers (size), a fixity list
   * (fixity), a ViewletFactory to be used to populate it with Viewlets and a
   * list of integers to be the top level location (location).
   * The sub-ViewletArrays will then have locations location @ [1], location @
   * [2] etc. (where @ means append).
   * <p>
   * After setting up a few instance members based on the constructor's
   * parameters, we initialise the elements member by inspecting the fixity of
   * the top-level dimension. If it is 'fixed' we use FixedElements, and if it
   * is 'flexible' we use FlexibleElements. These are both subclasses of
   * Elements: FixedElements is optimized for fast access whereas
   * FlexibleElements allows expansion.
   * <p>
   * To populate elements, we use createSubArray which will either fill it with
   * Viewlets (in the one-dimensional case) or ViewletArrays in the
   * multi-dimensional case. So, the ViewletArray constructor is mutually
   * recursive with this static method.
   *
   */
  private ViewletArray(List size, List fixity, Viewable viewable,
                       ViewletFactory viewletFactory,
                       List location)
  {
    super(size, fixity, viewable, viewletFactory);
    int topLevelSize = ((Integer) size.get(0)).intValue();
    this.wholeSize = new LinkedList(size);
    this.topLevelFixity = (String) fixity.get(0);
    List subSize = size.subList(1, size.size());
    subFixity = fixity.subList(1, fixity.size());
    this.viewletFactory = viewletFactory;
    this.location = location;
    if(topLevelFixity.equals(fixedString))
    {
      elements = new FixedElements(topLevelSize);
    }
    else
    {
      elements = new FlexibleElements(topLevelSize);
    }
    List subLocation;
    for(int i = 0; i < topLevelSize; i++)
    {
      subLocation = new LinkedList(location);
      subLocation.add(new Integer(i+1));
      elements.set(i, createSubArray(subSize, subFixity, getViewable(), viewletFactory, subLocation));
    }
  }

  /**
   * dimNumber starts from 1. List should be a list of Strings.
   */
  public void setLocationNames(int dimNumber, List locationNames)
  {
      super.setLocationNames(dimNumber, locationNames);
  }

  /**
   * dimNumber starts from 1.
   */
  public void finishExpandDimension(int dimNumber)
  {
      super.finishExpandDimension(dimNumber);
  }

  /**
   * Traverses the entire structure recursively to collect together all the
   * element indices.
   */
  public ViewletRange getEntireViewletRange()
  {
      return super.getEntireViewletRange();
//      List elementsList = elements.toList();
//      if(wholeSize.size() == 1)
//      {
//  	ViewletRange res = new ViewletRangeCollection();
//  	res.addAll(elementsList);
//        return(res);
//      }
//      Iterator elementsIterator = elementsList.iterator();
//      ViewletArray viewletArray;
//      ViewletRange result = new ViewletRangeCollection();
//      while(elementsIterator.hasNext())
//      {
//        viewletArray = (ViewletArray) elementsIterator.next();
//        result.addAll(viewletArray.getAllViewletData());
//      }
//      return(result);
  }

  /**
   * Returns an Object to be inserted into the elements member of a
   * ViewletArray. This method is mutually recursive with the ViewletArray
   * constructor.<p>
   * If size isEmpty then the elements object being populated is one-dimensional
   * and so the method uses the ViewletFactory parameter to build a Viewlet and
   * returns the Viewlet. The newViewlet's elementReference is set based on
   * its location within the top level ViewletArray. <p>
   * Otherwise, the elements object being populated is multi-dimensional and so
   * the returned object is a ViewletArray, returned by the constructor.
   *
   */
  private static Object createSubArray(List size, List fixity,
                                       Viewable viewable,
                                       ViewletFactory viewletFactory,
                                       List location)
  {
    if(size.isEmpty())
    {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(null,
					"building viewlet with location: "+
					location);
	}

      ViewletData newViewlet = viewletFactory.build();
      return(newViewlet);
    }
    else
    {
      return(new ViewletArray(size, fixity, viewable, viewletFactory,
			      location));
    }
  }

  /**
   * Shrinks the ViewletArray to size newSize. Assumes the top-level fixity is
   * flexible.
   */
  public void shrinkTo(List newSize)
  {
      super.shrinkTo(newSize);
    if(wholeSize.equals(newSize)) // no change necessary
    {
      return;
    }
    // First shrink the top level dimension.
    int newTopLevelSize = ((Integer) newSize.get(0)).intValue();
    elements.shrinkTo(newTopLevelSize);
    if(wholeSize.size() > 1) // recursion necessary
    {
      // Cast each element to a Viewlet array and shrink recursively.
      List subNewSize = newSize.subList(1, newSize.size());
      for(int i = 0; i < elements.size(); i++)
      {
        ((ViewletArray) elements.get(i)).shrinkTo(subNewSize);
      }
    }
    wholeSize = new LinkedList(newSize);
    // Indicate to the table that rows/columns have been removed
    fireTableStructureChanged();
  }

  /**
   * Expand dimension nested at level <code>dimension</code> by one. If the
   * parameter dimension is greater than 1 this means that this.elements stays
   * the same size and it is the sub-ViewletArrays which expand. Otherwise, we
   * have to expand the top level by adding a new object to elements. This is
   * created using createSubArray.
   */
  public void startExpandDimension(int dimension)
  {
      super.startExpandDimension(dimension);
    Integer oldDimSize = (Integer) wholeSize.get(dimension-1);
    Integer newDimSize =
      new Integer(oldDimSize.intValue() + 1);
    if(dimension > 1)
    {
      for(int i = 0; i < elements.size(); i++)
      {
        ((ViewletArray) elements.get(i)).startExpandDimension(dimension - 1);
      }
    }
    else
    {
      List subSize = wholeSize.subList(1, wholeSize.size());
      List subLocation = new LinkedList(location);
      subLocation.add(newDimSize);
      elements.expand(createSubArray(subSize, subFixity,
                                     getViewable(),
                                     viewletFactory, subLocation));
    }
  }


    /**
     * Implements the abstract getViewletDataAt method
     */
  public ViewletData getViewletDataAt(List index)
  {
    if(index.size() != getSize().size())
    {
      throw(new IllegalArgumentException());
    }
    return((ViewletData) getElement(index));
  }

    /**
     * Implements the abstract setViewletDataAt method
     */
  public void setViewletDataAt(List index, ViewletData data)
  {
    if(index.size() != getSize().size())
    {
      throw(new IllegalArgumentException());
    }
    setElement(index, data);
  }

  /**
   * Get element with location index within the ViewletArray. Result may be a
   * ViewletArray or a Viewlet, depending on the length of the index parameter.
   */
  private Object getElement(List index)
  {
    Object sub =
      elements.get(((Integer) index.get(0)).intValue() - 1);
    if(index.size() == 1)
    {
      return(sub);
    }
    List subIndex = index.subList(1, index.size());
    return(((ViewletArray) sub).getElement(subIndex));
  }

  /**
   * Sets the element with location index within the ViewletArray. */
  private void setElement(List index, Object data)
  {
    if (index.size() == 1) {
      elements.set(((Integer) index.get(0)).intValue() - 1, data);
    } else {
      Object sub =
	elements.get(((Integer) index.get(0)).intValue() - 1);
      List subIndex = index.subList(1, index.size());
      ((ViewletArray) sub).setElement(subIndex, data);
    }
  }

  /**
   * Abstract superclass of the two classes which the elements member may
   * instantiate.
   */
  private abstract class Elements
  {
    abstract Object get(int index);
    abstract void set(int index, Object element);
    void expand(Object newElement){}
    void shrinkTo(int newSize){}
    abstract int size();
    abstract List toList();
  }

  /**
   * FixedElements is basically just a bridge between an Object array and
   * Elements
   */
  private class FixedElements extends Elements
  {
    private Object[] elements;
    FixedElements(int size)
    {
      elements = new Object[size];
    }
    Object get(int index)
    {
      return(elements[index]);
    }
    void set(int index, Object element)
    {
      elements[index] = element;
    }
    int size()
    {
      return(elements.length);
    }
    List toList()
    {
      LinkedList result = new LinkedList();
      for(int i = 0; i < elements.length; i++)
      {
        result.add(elements[i]);
      }
      return(result);
    }
  }

  /**
   * FlexibleElements is basically just a bridge between a Vector and
   * Elements. If there is a better class which supports expanding and
   * shrinking (maybe LinkedList?), this could be substituted for Vector.
   */
  private class FlexibleElements extends Elements
  {
    private Vector elements;
    FlexibleElements(int size)
    {
      elements = new Vector(size);
      elements.setSize(size);
    }
    Object get(int index)
    {
      return(elements.get(index));
    }
    void set(int index, Object element)
    {
      elements.set(index, element);
    }
    int size()
    {
      return(elements.size());
    }
    void expand(Object newElement)
    {
      elements.add(newElement);
    }
    void shrinkTo(int newSize)
    {
      while(size() > newSize)
      {
        elements.remove(newSize);
      }
    }
    List toList()
    {
      return(new LinkedList(elements));
    }
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






