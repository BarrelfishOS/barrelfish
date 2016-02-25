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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Display a task in a gantt viewer
 **/
public class ChartBarViewletType extends BoundsViewletType {

  /** The factor by which to stretch the displayy in the x direction **/
  double xScale = 1.0;

  /** The factor by which to stretch the displayy in the y direction **/
  double yScale = 1.0;

  /** defualt line color **/
  final Color DEFAULT_COLOR = Color.blue;

  /** The color to use when drawing tasks **/
  Color color;
  
  public ChartBarViewletType(String changeable) {
    super(changeable);
  }

  public ViewletData build()
  {
    return new Data();
  }

  public String getDescription()
  {
    return("ChartBar viewlet");
  }


  void fixBounds(ViewletDataStore store) {
    double max = 1.0;
    // find largest max
    for(Iterator it = store.getEntireViewletRange().iterator();
        it.hasNext(); ) {
      java.util.List index = (java.util.List)it.next();
      Data data = (Data)store.getViewletDataAt(index);
      if (data.absoluteMax > max) {
        max = data.absoluteMax;
      }
    }
    // set all absolute min to 0.0 and max to largest max
    for(Iterator it = store.getEntireViewletRange().iterator();
        it.hasNext(); ) {
      java.util.List index = (java.util.List)it.next();
      Data data = (Data)store.getViewletDataAt(index);
      data.absoluteMin = 0.0;
      data.absoluteMax = max;
      store.setViewletDataAt(index,data);
    }
  }

  public void startBuild(Viewer viewer,
                         ViewletDataStore store,
                         ViewletRange range,
                         List results) {
    super.startBuild(viewer, store, range, results);
    fixBounds(store);
  }

  public Class getCustomRendererClass() {
    return BarRenderer.class;
    //return Renderer.class;
    //return PointRenderer.class;
  }

  public void setXScale(double xScale) {
    this.xScale = xScale;
  }

  public void setYScale(double yScale) {
    this.yScale = yScale;
  }

  public void setFillColor(Color color) {
    this.color = color;
  }

  /** Calculate a unique index for each index ("index") with a store
   *  of size "size"
   **/
  protected int calcIndex(List size, List index) {
    int mult = 1;
    int x = 0;
    for(int i = 0; i < size.size(); i++) {
      Integer pitch = (Integer)(size.get(i));
      Integer offset = (Integer)(index.get(i));
      x += offset.intValue() * mult;
      mult *= pitch.intValue()+1;
    }
    return x;
  }

  public void customizeElement(ViewletDataStore store,
                               java.util.List index,
                               Element element) {
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,"ChartBar Viewlet customize, element="+element);
    }
    if (element==null) {
      return;
    }
    Data data = (Data)store.getViewletDataAt(index);
    data.vertical = true;
    List size = store.getSize();
    int xIndex = calcIndex(size,index);

    // set the custom renderer
    double width=(1)*xScale / GrappaConstants.PointsPerInch;
    double height=(data.absoluteMax-data.absoluteMin)*yScale / GrappaConstants.PointsPerInch;
    
    double x = ((double)xIndex + 0.5)*xScale;
    double y = -(((data.absoluteMax-data.absoluteMin)*yScale) / 2);
    element.setAttribute("pos",x+","+y);
    element.setAttribute("width",""+width);
    element.setAttribute("height",""+height);
    element.getGrappaNexus().updateShape();
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,"data="+data+" pos="+x+","+y+" width="+width+" height="+height);
      DebuggingSupport.logMessage(this,"element.getAttribute(width)="+element.getAttribute("width"));
      DebuggingSupport.logMessage(this,"element.getAttribute(height)="+element.getAttribute("height"));
    }
    element.setAttribute("shape",new Integer(Grappa.CUSTOM_SHAPE));
    element.setAttribute(Grappa.CUSTOM_ATTR,getCustomRendererClass().getName());
    
    // set background color
    Color backColor = (color==null?DEFAULT_COLOR:color);
    element.setAttribute("style", "filled");
    if (data.getHoldsOnUpdates()) {
      element.setAttribute("color", backColor.darker());
    } else {
      element.setAttribute("color", backColor);
    }
    // force shape update
    element.object = data;
    element.setAttribute("label","");
    element.getGrappaNexus().updateText();
    element.getGrappaNexus().updateShape();
  }

  /**
   * Return a collection of actions which can be applied to viewlets
   * in this table
   */
  public Collection getActions(ViewletDataStore store,
                               ViewletRange range) {
    Collection ll = new LinkedList();
    if ((range != null) & (!range.isEmpty())) {
      // Add new actions here
      ll.add((new ToggleHoldAction()).createCompoundAction(store, range));
    }
    if ((range != null) & (range.size()==1)) {
      java.util.List index = (java.util.List)(range.iterator().next());
      // Add new actions here which apply only to single viewlets
      ll.add(new DisplayBoundsInDetailAction(store, index));
    }
    return ll;
  }

  /**
   * For the given index, return the smallest pertinent value
   **/
  public double getMin(ViewletDataStore store, List index) {
    return ((Data)(store.getViewletDataAt(index))).min;
  }
  
  /**
   * For the given index, return the largest pertinent value
   **/
  public double getMax(ViewletDataStore store, List index) {
    return ((Data)(store.getViewletDataAt(index))).max;
  }



  public static class Data extends BoundsViewletType.Data {
    public Data() {
      super();
    }
    public String toString() {
      return Double.toString(max);
    }
  }

  public static class BarRenderer extends BoundsViewletType.Renderer {
    public BarRenderer(Element element,
                       double x, double y, double w, double h) {
      super(element, x, y, w, h);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"ChartBar renderer constructed for element "+element);
      }
    }
    
    public void configure(Rectangle2D bounds,
                          double absoluteMin, double absoluteMax,
                          double initialMin, double initialMax,
                          double min, double max, boolean vertical) {
      // draw a "box and stick" diagram within the specified bounds
      //
      // G  +--------+                 --+
      //    |        |                   |
      // H  |        +-------+            
      //    |                |            
      // I  |                |            
      //    |                |            
      // J  |        +-------+            
      //    |        |                   |
      // K  +--------+                 --+
      //    A        C       D           F
      //
      //Rectangle2D bounds = getBounds2D();
      float A,B,C,D,E,F,G,H,I,I1,I2,J,K;
      if (vertical) {
        F = (float)bounds.getMinY();
        A = (float)bounds.getMaxY();
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
        float vert[][] = {{G,A},{G,C},{H,C},{H,D},
                          {J,D},{J,C},{K,C},{K,A}};
        vertices = vert;
      } else {
        float horiz[][] = {{A,G},{C,G},{C,H},{D,H},
                           {D,J},{C,J},{C,K},{A,K}};
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

  public static class PointRenderer extends BoundsViewletType.Renderer {
    public PointRenderer(Element element,
                       double x, double y, double w, double h) {
      super(element, x, y, w, h);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"ChartBar point renderer constructed for element "+element);
      }
    }
    
    public void configure(Rectangle2D bounds,
                          double absoluteMin, double absoluteMax,
                          double initialMin, double initialMax,
                          double min, double max, boolean vertical) {
      // draw a small box from min to max
      //
      // G  +--                        --+
      //    |                            |
      // H           +-------+            
      //             |       |            
      // I           |       |            
      //             |       |            
      // J           +-------+            
      //    |                            |
      // K  +--                        --+
      //    A        C       D           F
      //
      float A,B,C,D,E,F,G,H,I,I1,I2,J,K;
      if (vertical) {
        F = (float)bounds.getMinY();
        A = (float)bounds.getMaxY();
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
      C = A + (float)min * scaleX;
      D = A + (float)max * scaleX;
      
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
      J  = I + 4*sixteenth;

      // draw outline
      float vertices[][];
      
      if (vertical) {
        float vert[][] = {{H,C},{H,D},{J,D},{J,C}};
        vertices = vert;
      } else {
        float horiz[][] = {{C,H},{D,H},{D,J},{C,J}};
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




