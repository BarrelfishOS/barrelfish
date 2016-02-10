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
 * Displays a textual representation of the viewable element
 */
public class TextViewletType extends AbstractViewletType {
    private TableCellRenderer tableCellRenderer;
    private CustomRenderer customRenderer;
    public static final int MAX_FADE = 10;

    /** Holds four colours per fade level*/
    FadeColorSupport fadeColorSupport;

    public TextViewletType(String changeable) {
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
	return("Text viewlet");
    }
    

    /* ViewletType methods */
    public synchronized TableCellRenderer getTableCellRenderer() {
	if (tableCellRenderer == null) {
	    tableCellRenderer = new CellRenderer();
	}
	return tableCellRenderer;
    }

    synchronized Class getCustomRendererClass() {
//  	if (customRenderer == null) {
//  	    customRenderer = new CustRenderer();
//  	}
//  	return customRenderer;
      return Renderer.class;
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
            // set the element data
            element.object = data;

            // set the custom renderer
            element.setAttribute("shape",new Integer(Grappa.CUSTOM_SHAPE));
            element.setAttribute(Grappa.CUSTOM_ATTR,getCustomRendererClass().getName());
            // set the node label
            element.setAttribute("label",data.getText());

            // set filled
            element.setAttribute("style", "filled");

            // set background color
            element.setAttribute("color",getColor(data, false));

            // force shape update
            element.getGrappaNexus().updateShape();
        } else {
            // instance of edge
        }
    }


    public BatchGoal collectPreBuildGoal(Viewer viewer,
                                         ViewletDataStore store,
                                         ViewletRange range)
    {
	BatchGoal result = new BatchGoal();
	Iterator indexListIterator = range.iterator();
	java.util.List currentIndex;
	Collection viewlets;
	while(indexListIterator.hasNext())
	    {
		currentIndex = (java.util.List) indexListIterator.next();
		CompoundTerm goal =
		    new CompoundTermImpl(":", new Atom("vc_support"),
					 new CompoundTermImpl("viewable_element_to_string",
							      new CompoundTermImpl("element",currentIndex), null));
		result.add(composeElementGoal(currentIndex, store.getViewable().getNameAtom(), goal));
	    }
	return(result);
    }

    public void startBuild(Viewer viewer,
                           ViewletDataStore store,
                           ViewletRange range,
                           List results) {
	Iterator indexListIterator = range.iterator();
	Iterator resultsIterator = results.iterator();
	CompoundTermImpl elementResult;
	List currentIndex;
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,
                                        "startBuild called with range=" +
                                        range + " results=" + results);
        }
        
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
	    elementResult = (CompoundTermImpl) resultsIterator.next();
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this, "elementResult="+elementResult);
	    }
	    // store the goal result text into the viewletDataStore
	    Object text =
              ((CompoundTermImpl)decomposeElementGoal(elementResult)).argCT(2).arg(2);
	    if (viewletData == null) {
		viewletData = (Data)build();
	    }
	    viewletData.setText(text.toString());
	    store.setViewletDataAt(currentIndex, viewletData);
	}
    }


    public BatchGoal collectPreUpdateGoal(Viewer viewer,
                                          ViewletDataStore store,
					  ViewletRange range,
					  UpdateEvent updateEvent)
    {
	BatchGoal result = new BatchGoal();
	Iterator indexListIterator = range.iterator();
	java.util.List currentIndex;
	Collection viewlets;
	
	while(indexListIterator.hasNext())
	    {
		currentIndex = (java.util.List) indexListIterator.next();
		CompoundTerm goal =
		    new CompoundTermImpl(":", new Atom("vc_support"),
					 new CompoundTermImpl("viewable_element_to_string",
							      new CompoundTermImpl("element",currentIndex), null));
		result.add(composeElementGoal(currentIndex, store.getViewable().getNameAtom(), goal));
	    }
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
        // do the same as for building a viewlet
        startBuild(viewer, store, range, results);
        // set the fade counter
        setUpdating(viewer, store, range,((updateEvent instanceof ForwardUpdateEvent)?MAX_FADE:-MAX_FADE));

          ViewletRange all = store.getEntireViewletRange() ;
          ViewletRange faded = new ViewletRangeCollection();
          for(Iterator it = all.iterator(); it.hasNext(); ) {
              List index = (List)it.next();
              Data data = (Data)(store.getViewletDataAt(index));
              if (data.fade()) {
                  faded.add(index);
              }
          }
          // Indicate that these cells were updated
          store.fireViewletRangeUpdated(faded);
          //        store.fireViewletRangeUpdated(range);
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
	private String text;
	int updating;
        private int fadeCount;
        private boolean fading;

	public Data()
	{
	    super();
	    text = "";
            fadeCount = 0;
            fading = true;
	}

	public String getText()
	{
	    return text;
	}

	public void setText(String newValue)
	{
	    text = newValue;
	}

	public String toString() {
	    return text;
	}


        public int getFadeCount() {
            return fadeCount;
        }
        
        public void setFadeCount(int count) {
            fadeCount = count;
        }
        
        public void setFading(boolean fading) {
            this.fading = fading;
        }
        
        public boolean getFading() {
            return fading;
        }


        /**
         * Move the fade counter toward zero
         * @return true iff the fadeCount changed
         */
        public boolean fade() {
            if (fadeCount == 0) {
                return false;
            } if (fading) {
                if (fadeCount > 0) {
                    fadeCount--;
                } else {
                    /* fadeCount < 0 */
                    fadeCount++;
                }
            } else {
                // when not fading colors should revert after one cylce
                fadeCount = 0;
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
            ll.add((new ToggleFadeAction()).createCompoundAction(store, range));
        }
        return ll;
    }

    private class ToggleFadeAction extends ViewletAction
    {
        ToggleFadeAction()
        {
            super("Fade update history");
            putValue(Action.NAME, "Fade update history");
            putValue(Action.LONG_DESCRIPTION,
                     "Change whether previous updates show as fading colours or not");
            putValue(Action.SHORT_DESCRIPTION,
                     "Change whether colours fade");
            putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
            
        }
        
        public void actionPerformed(ActionEvent e)
        {
            // do nothing
        }
        
        /**
         * If all viewlets in the collection have hold set to true, then the
         * compound version sets it to false. If any of them have it set to false
         * then the compound version sets all to true.
         */
        public ViewletAction createCompoundAction(ViewletDataStore store,
                                                  ViewletRange range)
        {
            boolean allFade = true;
            Data currentViewlet;
            Iterator viewletsIterator = store.getViewletDataIterator(range);
            while(viewletsIterator.hasNext())
                {
                    currentViewlet = (Data) viewletsIterator.next();
                    if(!currentViewlet.getFading())
                        {
                            allFade = false;
                            break;
                        }
                }
            return(new CompoundToggleFadeAction(!allFade, store, range));
        }
    }
    
    private class CompoundToggleFadeAction extends ToggleFadeAction
    {
        private boolean newValue;
        private ViewletRange range;
        private ViewletDataStore store;
        CompoundToggleFadeAction(boolean newValue,
                                 ViewletDataStore store,
                                 ViewletRange range)
        {
            super();
            this.newValue = newValue;
            this.range = range;
            this.store = store;
        }
        
        public void actionPerformed(ActionEvent e)
        {
            Data currentViewlet;
            Iterator viewletsIterator = range.iterator();
            while(viewletsIterator.hasNext())
                {
                    List index = (List)viewletsIterator.next();
                    currentViewlet = (Data)(store.getViewletDataAt(index));
                    currentViewlet.setFading(newValue);
                    store.setViewletDataAt(index, currentViewlet);
                }
            // trigger the jtable to update as a whole bunch of viewlets have just
            // changed
            store.fireViewletRangeUpdated( range );
        }
    }




    /**
     * The default text cell render
     */
    private class CellRenderer extends DefaultTableCellRenderer {
        
        Icon fadeIcon;

	public CellRenderer() {
	    super();
	    setHorizontalAlignment(SwingConstants.CENTER);
            // Create the single instance fadeIcon to be used by the renderer
            fadeIcon = fadeColorSupport.getFadeIcon(10,10);
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
            if (data.getFading()) {
                result.setIcon(fadeIcon);
            } else {
                result.setIcon(null);
            }
	    return result;
	}

	public void setValue(Object value) {
	    super.setValue(value);
	    setToolTipText(value.toString());
	}

    }

    public static class Renderer extends CustRenderer {
        public Renderer(Element element,
                        double x, double y, double w, double h) {
            super(element, x, y, w, h);
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Plain Text Renderer constructed for element "+element);
            }
            // somehow work out a way of getting the actual
            // ViewletData for this element (must work when the
            // viewlettype is contained in a multiViewlet type)
            //Data data = (Data)element.object;
            Data data = (Data)getViewletData();
            if (data != null) {
              configure(new Rectangle2D.Double(x,y,w,h),
                        data);
            }
        }

        public void configure(Rectangle2D bounds,
                              Data data) {
            // draw a line underneath the text to show the selection status
            //     Text
            //     ----
            //
            float y = (float)(bounds.getMaxY());
            float dy = 1.0f;
            float vertices[][] = {{(float)bounds.getMinX(), y-dy},
                                  {(float)bounds.getMaxX(), y-dy},
                                  {(float)bounds.getMaxX(), y+dy},
                                  {(float)bounds.getMinX(), y+dy}};
          path.moveTo(vertices[0][0], vertices[0][1]);
          for(int i = 1; i < vertices.length; i++) {
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds point "+i+
                                          " x="+vertices[i][0]+
                                            " y="+vertices[i][1]);
            }
            path.lineTo(vertices[i][0], vertices[i][1]);
          }
          path.closePath();
        }

      public void draw(java.awt.Graphics2D g2d) {
        if ((element.highlight & SELECTION_MASK) != 0) {
          // do not draw outline unless the element is selected
          super.draw(g2d);
        }
      }
    }

}

