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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Displays a graphical representation of the numeric range, as
 * reported by get_bounds/3
 **/
public class BoundsViewletType extends AbstractViewletType {
    private TableCellRenderer tableCellRenderer;
    private CustomRenderer customRenderer;

    public BoundsViewletType(String changeable) {
        super(changeable);
    }
    

    /* ViewletFactory methods */    
    public boolean canBuildFrom(ElementType elementType)
    {
      //return(elementType instanceof NumericBounds);
      return true;
    }
    
    public ViewletData build()
    {
	return new Data();
    }
    
    public String getDescription()
    {
	return("Bounds viewlet");
    }
    
    /* ViewletType methods */
    public synchronized TableCellRenderer getTableCellRenderer() {
	if (tableCellRenderer == null) {
	    tableCellRenderer = new CellRenderer();
	}
	return tableCellRenderer;
    }

    public Class getCustomRendererClass() {
      return Renderer.class;
    }

    public void customizeElement(ViewletDataStore store,
                                 java.util.List index,
                                 Element element) {
        Data data = (Data)(store.getViewletDataAt(index));
        if (element instanceof Node) {
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 1");
            }
            // set the custom renderer
            element.setAttribute("shape",new Integer(Grappa.CUSTOM_SHAPE));
            element.setAttribute(Grappa.CUSTOM_ATTR,getCustomRendererClass().getName());
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 2");
            }
            // set the node label
            element.setAttribute("label","");
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 3");
            }
            // set background color
            if (data.getHoldsOnUpdates()) {
                element.setAttribute("color", Color.blue.darker());
            } else {
                element.setAttribute("color", Color.blue);
            }
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 4");
            }
            // set filled
            element.setAttribute("style", "filled");
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 5");
            }
            // force shape update
            element.object = data;
            element.getGrappaNexus().updateShape();
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds customizeElement for element "+element + " step 6");
            }
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
                    new CompoundTermImpl("get_var_bounds", new CompoundTermImpl("element",currentIndex), null, null);

//  		CompoundTerm goal =
//  		    new CompoundTermImpl(":", new Atom("vc_support"),

//  					 new CompoundTermImpl("viewable_element_to_string",
//  							      new CompoundTermImpl("element",currentIndex), null));
		result.add(composeElementGoal(currentIndex, store.getViewable().getNameAtom(), goal));
	    }
	return(result);
    }

    protected void processResults(Viewer viewer,
                                  ViewletDataStore store,
                                  ViewletRange range,
                                  List results,
                                  boolean isBuildResults) {
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
	    Double lowerBound = (Double)decomposeElementGoal(elementResult).arg(2);
	    Double upperBound = (Double)decomposeElementGoal(elementResult).arg(3);
	    if (viewletData == null) {
		viewletData = (Data)build();
	    }
            if (isBuildResults) {
                viewletData.setAbsoluteBounds(lowerBound.doubleValue(), upperBound.doubleValue());
                viewletData.setInitialBounds(lowerBound.doubleValue(), upperBound.doubleValue());
            }
	    viewletData.setCurrentBounds(lowerBound.doubleValue(), upperBound.doubleValue());
	    store.setViewletDataAt(currentIndex, viewletData);
	}
    }

    public void startBuild(Viewer viewer,
                           ViewletDataStore store,
                           ViewletRange range,
                           List results) {
        processResults(viewer, store, range, results, true);
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
                    new CompoundTermImpl("get_var_bounds", new CompoundTermImpl("element",currentIndex), null, null);

//  		CompoundTerm goal =
//  		    new CompoundTermImpl(":", new Atom("vc_support"),
//  					 new CompoundTermImpl("viewable_element_to_string",
//  							      new CompoundTermImpl("element",currentIndex), null));
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
	    //viewletData.setFadeCount(fadeCount);
	    store.setViewletDataAt(currentIndex, viewletData);
	}	
    }

    public void startUpdate(Viewer viewer,
                            ViewletDataStore store,
			    ViewletRange range,
			    List results,
			    UpdateEvent updateEvent)
    {
        processResults(viewer, store, range, results, false);
        // do the same as for building a viewlet
        //startBuild(viewer, store, range, results);
        // set the fade counter
        //setUpdating(viewer, store, range,((updateEvent instanceof ForwardUpdateEvent)?MAX_FADE:-MAX_FADE));

//            ViewletRange all = store.getEntireViewletRange() ;
//            ViewletRange faded = new ViewletRangeCollection();
//            for(Iterator it = all.iterator(); it.hasNext(); ) {
//                List index = (List)it.next();
//                Data data = (Data)(store.getViewletDataAt(index));
//                if (data.fade()) {
//                    faded.add(index);
//                }
//            }
          // Indicate that these cells were updated
          store.fireViewletRangeUpdated(range);
          //        store.fireViewletRangeUpdated(range);
    }

    /**
     * For the given index, return the smallest pertinent value
     **/
    public double getMin(ViewletDataStore store, List index) {
        return ((Data)(store.getViewletDataAt(index))).absoluteMin;
    }

    /**
     * For the given index, return the largest pertinent value
     **/
    public double getMax(ViewletDataStore store, List index) {
        return ((Data)(store.getViewletDataAt(index))).absoluteMax;
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
        double absoluteMin, absoluteMax;
        double initialMin, initialMax;
        double min, max;
        boolean vertical;

	public Data()
	{
	    super();
	}

	public String toString() {
          return initialMin+".."+min+".."+max+".."+initialMax;
	}

        public void setAbsoluteBounds(double min, double max) {
            absoluteMin = min;
            absoluteMax = max;
        }

        public void setInitialBounds(double min, double max) {
            initialMin = min;
            initialMax = max;
        }

        public void setCurrentBounds(double min, double max) {
            this.min = min;
            this.max = max;
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
            // Add new actions here
            ll.add(new AlignBoundsAction(store, range));
            ll.add(new ToggleHorizontalVerticalAction(store, range));
        }
        if ((range != null) & (range.size()==1)) {
            java.util.List index = (java.util.List)(range.iterator().next());
            // Add new actions here which apply only to single viewlets
            ll.add(new DisplayBoundsInDetailAction(store, index));
        }
        return ll;
    }

    protected void allignBounds(ViewletDataStore store,
                                ViewletRange range) {
            double newMin = Double.POSITIVE_INFINITY;
            double newMax = Double.NEGATIVE_INFINITY;

            for(Iterator iterator = range.iterator(); iterator.hasNext(); ) {
                List index = (List) iterator.next();
                Data viewlet = (Data)(store.getViewletDataAt(index));
                if(viewlet.initialMin < newMin) {
                    newMin = viewlet.initialMin;
                }
                if(viewlet.initialMax > newMax) {
                    newMax = viewlet.initialMax;
                }
            }
            
            for(Iterator iterator = range.iterator(); iterator.hasNext(); ) {
                List index = (List) iterator.next();
                Data viewlet = (Data)(store.getViewletDataAt(index));
                viewlet.setAbsoluteBounds(newMin, newMax);
                store.setViewletDataAt(index, viewlet);
            }
            store.fireViewletRangeUpdated(range);
    }

    /**
     * Recordable command to perform the bounds allignment
     */
    public static class AlignBoundsCommand extends ViewletTypeRangeCommand {
        public AlignBoundsCommand(ViewletType type,
                                  ViewletDataStore store,
                                  ViewletRange range) {
            super(type, store, range);
        }

        public void postRecordIssue() {
            if (DebuggingSupport.logMessages) {
                DebuggingSupport.logMessage(this, "AlignBoundCommand postRecordIssue invoked with type="+getViewletType()+" store="+getViewletDataStore()+" range="+getViewletRange());
            }
            ((BoundsViewletType)getViewletType()).
                allignBounds(getViewletDataStore(), getViewletRange());
        }
    }

    /**
     * Action class to set a range of viewables to have common
     * AbsoluteMin and AbsolutMax
     **/
    protected class AlignBoundsAction extends ViewletAction {
        ViewletRange range;
        ViewletDataStore store;
        
        AlignBoundsAction(ViewletDataStore store, ViewletRange range) {
            super("Align bounds");
            putValue(Action.NAME, "Align bounds");
            putValue(Action.LONG_DESCRIPTION,
                     "Re-align a selection of bounds viewlets so that their bounds are displayed on the same axes");
            putValue(Action.SHORT_DESCRIPTION,
               "Re-align so that bounds viewlets use the same axes");
            //putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
            this.store = store;
            this.range = range;
        }

      public void actionPerformed(ActionEvent e) {
          new AlignBoundsCommand(BoundsViewletType.this, store, range).issue();
      }
    }


    protected void toggleHorizontalVertical(ViewletDataStore store,
                                          ViewletRange range) {
        boolean allHorizontal = true;
        
        for(Iterator iterator = range.iterator(); iterator.hasNext(); ) {
            List index = (List) iterator.next();
            Data data = (Data)(store.getViewletDataAt(index));
            if(data.vertical) {
                allHorizontal = false;
            }
        }
        // if all the viewlets are horizontal then set them all
        // vertical, otherwise set them all horizontal
        for(Iterator iterator = range.iterator(); iterator.hasNext(); ) {
            List index = (List) iterator.next();
            Data data = (Data)(store.getViewletDataAt(index));
            data.vertical = allHorizontal;
            store.setViewletDataAt(index, data);
        }
        store.fireViewletRangeUpdated(range);
    }

    /**
     * Recordable command to perform the bounds allignment
     */
    public static class ToggleHorizontalVerticalCommand extends ViewletTypeRangeCommand {
        public ToggleHorizontalVerticalCommand(ViewletType type,
                                               ViewletDataStore store,
                                               ViewletRange range) {
            super(type, store, range);
        }

        public void postRecordIssue() {
            if (DebuggingSupport.logMessages) {
                DebuggingSupport.logMessage(this, "AlignBoundCommand postRecordIssue invoked with type="+getViewletType()+" store="+getViewletDataStore()+" range="+getViewletRange());
            }
            ((BoundsViewletType)getViewletType()).
                toggleHorizontalVertical(getViewletDataStore(), getViewletRange());
        }
    }


    /**
     * Action class to set a range of viewables to have common
     * AbsoluteMin and AbsolutMax
     **/
    protected class ToggleHorizontalVerticalAction extends ViewletAction {
        ViewletRange range;
        ViewletDataStore store;
        
        ToggleHorizontalVerticalAction(ViewletDataStore store,
                                       ViewletRange range) {
            super("Toggle horizontal/vertical range bar");
            putValue(Action.NAME, "Toggle horizontal/vertical range bar");
            putValue(Action.LONG_DESCRIPTION,
                     "Change whether the range bar is displayed horizontally or vertically");
            putValue(Action.SHORT_DESCRIPTION,
                     "Change whether the range bar is horizontal or vertical");
            this.store = store;
            this.range = range;
        }

        public void actionPerformed(ActionEvent e) {
            new ToggleHorizontalVerticalCommand(BoundsViewletType.this,
                                                store,
                                                range).issue();
        }
    }

    /**
     * Action class to display the bounds in detail in a popup window
     **/
    protected class DisplayBoundsInDetailAction extends ViewletAction {
        List index;
        ViewletDataStore store;
        
        DisplayBoundsInDetailAction(ViewletDataStore store,
                                    java.util.List index) {
            super("Display bounds");
            putValue(Action.NAME, "Display bounds");
            putValue(Action.LONG_DESCRIPTION,
                     "Popup window displaying bounds in detail");
            putValue(Action.SHORT_DESCRIPTION,
               "Popup window displaying bounds in detail");
            //putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
            this.store = store;
            this.index = index;
        }

      public void actionPerformed(ActionEvent e) {
        Data data = (Data)(store.getViewletDataAt(index));
        JOptionPane.
            showConfirmDialog(null,
                              "Initial upper bound: "+data.initialMax+"\n\n"+
                              "Current upper bound: "+data.max+"\n\n"+
                              "Current lower bound: "+data.min+"\n\n"+
                              "Initial lower bound: "+data.initialMin,
                              "Bounds data in detail",
                              JOptionPane.DEFAULT_OPTION);
      }
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
          Rectangle bounds ;
            if (table == null) {
              result = new JLabel();
              bounds = result.getBounds();
            } else {
  	    result =
  		(JLabel)(super.getTableCellRendererComponent(table,
  							     "",
  							     isSelected,
  							     hasFocus,
  							     row,
  							     column));
            bounds = table.getCellRect(row, column, false);
            }
	    Data data = (Data)value;

            
            Graphics2D g2d = (Graphics2D)result.getGraphics();
            Element element = new Graph("bar");
            element.object = null;
            Renderer renderer = new Renderer(element,
                                             0,0,
                                             1,1);
            // set background color
            if (data.getHoldsOnUpdates()) {
                if (isSelected) {
                    result.setBackground(Color.white.darker().darker().darker());
                } else {
                    result.setBackground(Color.white.darker());
                }
            } else {
                if (isSelected) {
                    result.setBackground(Color.white.darker().darker());
                } else {
                    result.setBackground(Color.white);
                }
            }
            result.setIcon(new RendererIcon(renderer,data,bounds));
            //result.setBackground(getColor(data, isSelected));
//              if (data.getFading()) {
//                  result.setIcon(fadeIcon);
//              } else {
//                  result.setIcon(null);
//              }
	    return result;
	}

	public void setValue(Object value) {
	    super.setValue(value);
	    setToolTipText(value.toString());
	}

    }

    public static class RendererIcon implements Icon {
        Renderer renderer;
        Data data;
        Rectangle bounds;
        
        public RendererIcon(Renderer renderer,
                            Data data,
                            Rectangle parentBounds) {
            this.renderer = renderer;
            this.data = data;
            int w = (parentBounds.width * 9) / 10;
            int h = (parentBounds.height * 9) / 10;
            int x = (parentBounds.width * 1) / 20;
            int y = (parentBounds.height * 1) / 20;
            this.bounds = new Rectangle(x,y,w,h);
        }

        public int getIconHeight() {
            // nominal height
            return bounds.height;
        }
        public int getIconWidth() {
            // nominal width
            return bounds.width;
        }
        public void paintIcon(Component c,
                              Graphics g,
                              int x,
                              int y) {
            //Data data = renderer.getViewletData();
          if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this, "Clip = "+g.getClip().getBounds2D()+" bounds="+bounds+" (x,y)=("+x+","+y+")");
          }
            //renderer.configure(new Rectangle2D.Double(x,y,c.getWidth(),c.getHeight()),
          //renderer.configure(g.getClip().getBounds2D(),
            renderer.configure(bounds,
                               data.absoluteMin,
                               data.absoluteMax,
                               data.initialMin,
                               data.initialMax,
                               data.min,
                               data.max,
                               data.vertical);
            g.setColor(Color.black);
            renderer.draw((Graphics2D)g);
            g.setColor(Color.blue);
            renderer.fill((Graphics2D)g);
        }
    }


    public static class Renderer extends CustRenderer {
        public Renderer(Element element,
                        double x, double y, double w, double h) {
            super(element, x, y, w, h);
            if (DebuggingSupport.logMessages) {
              DebuggingSupport.logMessage(this,"Bounds Renderer constructed for element "+element);
            }
            // somehow work out a way of getting the actual
            // ViewletData for this element (must work when the
            // viewlettype is contained in a multiViewlet type)
            //Data data = (Data)element.object;
            Data data = (Data)getViewletData();
            if (data != null) {
              configure(new Rectangle2D.Double(x,y,w,h),
                        data.absoluteMin,
                        data.absoluteMax,
                        data.initialMin,
                        data.initialMax,
                        data.min,
                        data.max,
                        data.vertical);
            }
        }

        public void configure(Rectangle2D bounds,
                              double absoluteMin, double absoluteMax,
                              double initialMin, double initialMax,
                              double min, double max, boolean vertical) {
            // draw a "box and stick" diagram within the specified bounds
            //
            // G  +--      +-------+         --+
            //    |        |       |           |
            // H      |    |       |     |      
            // I1     |----|       |-----|      
            // I      |                  |      
            // I2     |----|       |-----|      
            // J      |    |       |     |      
            //    |        |       |           |
            // K  +--      +-------+         --+
            //    A   B    C       D     E     F
            //
          //Rectangle2D bounds = getBounds2D();
          float A,B,C,D,E,F,G,H,I,I1,I2,J,K;
          if (vertical) {
            A = (float)bounds.getMinY();
            F = (float)bounds.getMaxY();
          } else {
            A = (float)bounds.getMinX();
            F = (float)bounds.getMaxX();
          }
          // remove potential for divide by zero
          if (absoluteMax == absoluteMin) {
            absoluteMax++;
          }
          // calculate scale for drawing
          float scaleX = (F-A) / (float)(absoluteMax - absoluteMin);
          B = A + (float)initialMin * scaleX;
          C = A + (float)min * scaleX;
          D = A + (float)max * scaleX;
          E = A + (float)initialMax * scaleX;
          
          if (vertical) {
            G = (float)bounds.getMinX();
            K = (float)bounds.getMaxX();
          } else {
            G = (float)bounds.getMinY();
            K = (float)bounds.getMaxY();
          }
          float sixteenth = (K-G)/16;
          I = G+8*sixteenth;

          H  = I - 4*sixteenth;
          I1 = I - 1*sixteenth;
          I2 = I + 1*sixteenth;
          J  = I + 4*sixteenth;

          // draw outline
          float vertices[][];

          if (vertical) {
            float vert[][] = {{H,B},{I1,B},{I1,C},{G,C},{G,D},{I1,D},{I1,E},
                              {H,E},{J,E},{I2,E},{I2,D},{K,D},{K,C},{I2,C},
                              {I2,B},{J,B}};
            vertices = vert;
          } else {
            float horiz[][] = {{B,H},{B,I1},{C,I1},{C,G},{D,G},{D,I1},{E,I1},
                               {E,H},{E,J},{E,I2},{D,I2},{D,K},{C,K},{C,I2},
                               {B,I2},{B,J}};
            vertices = horiz;
          }
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
    }

}

