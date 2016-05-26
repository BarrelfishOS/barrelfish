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

package com.parctechnologies.eclipse.visualisation.viewers;

import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.*;
import java.awt.Color;
import java.awt.Component;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Displays a fading square, color changes on forward/backward updates
 * and then fades slowly to white.  */
public class FadeViewletType extends AbstractViewletType {

    private TableCellRenderer tableCellRenderer;

    public static final int MAX_FADE = 10;

    /** Holds four colours per fade level*/
    FadeColorSupport fadeColorSupport;

    public FadeViewletType(String changeable) {
        super(changeable);
        fadeColorSupport = new FadeColorSupport(MAX_FADE, new Color(0,255,0), new Color(255,0,0));
    }
    

    /* ViewletFactory methods */    
    public boolean canBuildFrom(ElementType elementType)
    {
	return(true);
    }
    
    public ViewletData build()
    {
	return new Data();
    }
    
    public String getDescription()
    {
	return("Fade viewlet");
    }
    
    /* ViewletType methods */
    public synchronized TableCellRenderer getTableCellRenderer() {
	if (tableCellRenderer == null) {
	    tableCellRenderer = new CellRenderer();
	}
	return tableCellRenderer;
    }

    synchronized Class getCustomRendererClass() {
      return CustRenderer.class;
    }

    protected Color getColor(Data data, boolean isSelected) {
        int greyness = 0;
        if (data.getHoldsOnUpdates()) {
            greyness++;
        }
        if (isSelected) {
            greyness+=2;
        }
        Color col;
        int val = data.getFadeCount();
        if (val > 0) {
            col = fadeColorSupport.forwardColor[val][greyness];
        } else {
            col = fadeColorSupport.backwardColor[-val][greyness];
        }
        return col;
    }

    public void customizeElement(ViewletDataStore store,
                                 java.util.List index,
                                 Element element) {
        Data data = (Data)(store.getViewletDataAt(index));
        if (element instanceof Node) {
            // set the custom renderer
            element.setAttribute("shape","box");
            // set the custom renderer
            element.setAttribute("style","filled");
            // set the node label
            element.setAttribute("label","");
            // set background color
            element.setAttribute("color",getColor(data, false));
        } else {
            // instance of edge
        }
    }


    public BatchGoal collectPreBuildGoal(Viewer viewer,
                                         ViewletDataStore store,
                                         ViewletRange range)
    {
	BatchGoal result = new BatchGoal();
	return(result);
    }

    public void startBuild(Viewer viewer,
                           ViewletDataStore store,
                           ViewletRange range,
                           List results) {
    }

    public BatchGoal collectPreUpdateGoal(Viewer viewer,
                                          ViewletDataStore store,
					  ViewletRange range,
					  UpdateEvent updateEvent)
    {
	BatchGoal result = new BatchGoal();
	return(result);
    }

    protected void setUpdating(Viewer viewer,
                               ViewletDataStore store,
			       ViewletRange range,
			       int fadeCount) {
	Iterator indexListIterator = range.iterator();
	List currentIndex;
	while(indexListIterator.hasNext()) {
	    currentIndex = (List) indexListIterator.next();
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this, "currentIndex="+currentIndex);
	    }
	    Data viewletData =
		(Data)(store.getViewletDataAt(currentIndex));
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this, "viewletData="+viewletData);
	    }
	    // Set the updating flag and store the new data
	    if (viewletData == null) {
		viewletData = (Data)build();
	    }
	    viewletData.setFadeCount(fadeCount);
	    store.setViewletDataAt(currentIndex, viewletData);
	}	
    }

    public void startUpdate(Viewer viewer,
                            ViewletDataStore store,
			    ViewletRange range,
			    List results,
			    UpdateEvent updateEvent)
    {
        // set the fade counter
        setUpdating(viewer, store, range,((updateEvent instanceof ForwardUpdateEvent)?MAX_FADE:-MAX_FADE));

        ViewletRange faded = new ViewletRangeCollection();
        ViewletRange all = store.getEntireViewletRange() ;
        for(Iterator it = all.iterator(); it.hasNext(); ) {
            List index = (List)it.next();
            Data data = (Data)(store.getViewletDataAt(index));
            if (data.fade()) {
                faded.add(index);
            }
        }
        // Indicate that these cells were updated
        store.fireViewletRangeUpdated(faded);
    }

    /*
     * Data is a viewlet which can monitor elements of any type. It is
     * responsible for:
     * <ul>
     * <li> Maintaining a record of the text representation of the term.
     * </ul>
     */
    public static class Data extends ViewletDataImpl
    {
	int updating;
        protected int fadeCount;

	public Data()
	{
	    super();
            fadeCount = 0;
	}

	public String toString() {
	    return "";
	}


        public int getFadeCount() {
            return fadeCount;
        }
        
        public void setFadeCount(int count) {
            fadeCount = count;
        }
        

        /**
         * Move the fade counter toward zero
         * @return true iff the fadeCount changed
         */
        public boolean fade() {
            if (fadeCount == 0) {
                return false;
            }
            if (fadeCount > 0) {
                fadeCount--;
            } else {
                /* fadeCount < 0 */
                fadeCount++;
            }
            return true;
        }

    }


    /**
     * Return a collection of actions which can be applied to viewlets
     * in this table
     */
    public Collection getActions(ViewletDataStore store,
                                 ViewletRange range) {
        Collection ll = super.getActions(store, range);
        if ((range != null) & (!range.isEmpty())) {
            // Add type specific actions here
        }
        return ll;
    }

    /**
     * The default text cell render
     */
    private class CellRenderer extends DefaultTableCellRenderer {

	public CellRenderer() {
	    super();
	    setHorizontalAlignment(SwingConstants.CENTER);
	}

	public Component getTableCellRendererComponent(JTable table,
						       Object value,
						       boolean isSelected,
						       boolean hasFocus,
						       int row,
						       int column) {
          
            JLabel result ;
            if (table == null) {
                result = new JLabel();
            } else {
                result =
                    (JLabel)(super.getTableCellRendererComponent(table,
                                                                 value,
                                                                 isSelected,
                                                                 hasFocus,
                                                                 row,
                                                                 column));
            }
	    Data data = (Data)value;

            result.setBackground(getColor(data, isSelected));
	    return result;
	}
    }

    /**
     * Specialised ViewletDataStore for dealing with FadeViewlet's.
     *
     * <p>Stores the data in an expanding 1D array of ints.
     */
    public static class Store extends AbstractViewletDataStore {
        /** number of elements between rows.  Not the same as width,
            which is the number of elements in a row.  This difference
            allows the 2D array to expand a few times without having
            to copy large amounts of data */
        protected int pitch[];
        
        /** number of elements in a 'row' for a given dimension */
        protected int width[];
        
        /** Holds 'fixity' for each dimension */
        protected boolean fixed[];

        /** Holds the ints */
        protected int[] array;

        public Store(List size, List fixity, Viewable viewable) {
            super(size, fixity, viewable, null);
            width = new int[size.size()];
            fixed = new boolean[fixity.size()];
            // copy size to width
            for(int i = 0; i < width.length; i++) {
                width[i] = ((Integer)(size.get(i))).intValue();
                fixed[i] = "fixed".equals(fixity.get(i));
            }
            int arraySize = setPitch();
            // allocate initial array
            array = new int[arraySize];
        }

        protected synchronized int setPitch() {
            int arraySize = 1;
            pitch = new int[fixed.length];
            /* Set pitch based on fixity and width */
            for(int i = pitch.length-1; i >= 0; i++) {
                // on entry to the loop, arraySize holds the number of
                // elements taken up by sub-arrays of the previous
                // number of dimensions
                if (fixed[i]) {
                    pitch[i] = width[i] * arraySize;
                } else {
                    pitch[i] = (width[i] + 4) * arraySize;
                }
                // on exit from the loop, arraySize holds the number
                // of elements taken up by a sub-array of the current
                // number of elements
                arraySize *= arraySize * pitch[i];
            }
            return arraySize;
        }

        /**
         * Expand dimension nested at level <code>dimension</code> by one.
         * records the pre-expansion size in the variable oldSize.
         */
        public void startExpandDimension(int dimension)
        {
            super.startExpandDimension(dimension);
            if (width[dimension] < pitch[dimension]) {
                // no need to do much since we have enough space to
                // expand into
                width[dimension]++;
            } else {
                // need to allocate a new array and copy the data across
                int oldWidth[] = width;
                int oldPitch[] = pitch;
                int oldArray[] = array;
                // increase size
                width[dimension]++;
                // re-calculate pitch info
                int arraySize = setPitch();
                // allocate new array
                array = new int[arraySize];
                // copy data, this should be optimised to use
                // System.arrayCopy
                switch(pitch.length) {
                case 1: {
                    // optimise 1D array
                    System.arraycopy(oldArray, 0, array, 0, oldWidth[0]);
                    break;
                }
                case 2: {
                    // optimise 2D array
                    int srcI = 0;
                    int dstI = 0;
                    for(int i=0; i < width[0]; i++) {
                        // for each row
                        System.arraycopy(oldArray, srcI,
                                         array, dstI,
                                         oldWidth[1]);
                        srcI += oldPitch[1];
                        dstI += pitch[1];
                    }
                    break;
                }
                default: {
                    // generic
                }
                }
            }
        }

        protected int calcIndex(List index) {
            int i = 0;
            for(int dim=0; dim < index.size() ; dim++ ) {
                i += ((Integer)(index.get(dim))).intValue() * pitch[dim];
            }
            return i;
        }

        /**
         * This method must be overridden in sub-classes
         */
        public ViewletData getViewletDataAt(List index) {
            int value = array[calcIndex(index)];
            // construct Data object from value
            Data data = new Data();
            data.setFadeCount(value);
            return data;
        }
        
        /**
         * This method must be overridden in sub-classes
         */
        public void setViewletDataAt(List index, ViewletData data) {
            //  deconstruct Data object info value
            int value = ((Data)data).getFadeCount();
            // construct Data object from value
            array[calcIndex(index)] = value;
        }

    }

}

