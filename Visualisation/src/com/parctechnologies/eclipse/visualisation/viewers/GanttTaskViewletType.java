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
public class GanttTaskViewletType extends BoundsViewletType {

  /** The factor by which to stretch the displayy in the x direction **/
  double xScale = 1.0;

  /** defualt line color **/
  final Color DEFAULT_COLOR = Color.blue;

  /** The color to use when drawing tasks **/
  Color color;
  
  public GanttTaskViewletType(String changeable) {
    super(changeable);
  }

  public ViewletData build()
  {
    return new Data();
  }

  public String getDescription()
  {
    return("GanttTask viewlet");
  }

  public Class getCustomRendererClass() {
    return Renderer.class;
  }

  public void setXScale(double xScale) {
    this.xScale = xScale;
  }

  public void setFillColor(Color color) {
    this.color = color;
  }

  public void customizeElement(ViewletDataStore store,
                               java.util.List index,
                               Element element) {
    if (((Integer)(index.get(0))).intValue() == -1) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"GanttTask Viewlet customize");
      }
      final double YSCALE=40;
      // when the first index is given as -1, this will force
      // the whole 'row' to be intepreted as a task in a gannt
      // chart.
      ArrayList index2 = new ArrayList(index);
      Data data;
      double taskData[] = new double[6];
      index2.set(0, new Integer(1));
      // start
      data = (Data)store.getViewletDataAt(index2);
      taskData[0] = data.min;
      taskData[1] = data.max;
      // end=start+duration
      index2.set(0, new Integer(2));
      data = (Data)store.getViewletDataAt(index2);
      taskData[2] = taskData[0] + data.min;
      taskData[3] = taskData[1] + data.max;
      // resource
      index2.set(0, new Integer(3));
      data = (Data)store.getViewletDataAt(index2);
      taskData[4] = data.min;
      taskData[5] = data.max+0.9;

      // set the custom renderer
      double width=(taskData[3]-taskData[0])*xScale / GrappaConstants.PointsPerInch;
      double height=(taskData[5]-taskData[4])*YSCALE / GrappaConstants.PointsPerInch;

      double x = ((taskData[3] + taskData[0])*xScale / 2);
      double y = ((taskData[5] + taskData[4])*YSCALE / 2);
      element.setAttribute("pos",x+",-"+y);
      element.setAttribute("width",""+width);
      element.setAttribute("height",""+height);
      element.getGrappaNexus().updateShape();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"taskData="+taskData+" pos="+x+",-"+y+" width="+width+" height="+height);
        DebuggingSupport.logMessage(this,"element.getAttribute(width)="+element.getAttribute("width"));
        DebuggingSupport.logMessage(this,"element.getAttribute(height)="+element.getAttribute("height"));
      }
      element.setAttribute("shape",new Integer(Grappa.CUSTOM_SHAPE));
      element.setAttribute(Grappa.CUSTOM_ATTR,getCustomRendererClass().getName());

      // set background color
      Color backColor = (color==null?DEFAULT_COLOR:color);
      // set filled, if a color has been specified
      if (color != null) {
        element.setAttribute("style", "filled");
        if (data.getHoldsOnUpdates()) {
          element.setAttribute("color", backColor.darker());
        } else {
          element.setAttribute("color", backColor);
        }
      } else {
        element.setAttribute("color", backColor);
        if (data.getHoldsOnUpdates()) {
          element.setAttribute("style", "dotted");
        } else {
          element.setAttribute("style", "solid");
        }
      }
      //Color color = new Color(0.0f, 0.0f, 1.0f, 0.2f);
      //Color color = new Color(0.0f, 0.0f, 1.0f);
      // force shape update
      element.object = taskData;
      element.getGrappaNexus().updateText();
      element.getGrappaNexus().updateShape();
    } else {
      super.customizeElement(store, index, element);
    }
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
    }
    if ((range != null) & (range.size()==3)) {
      java.util.List index = (java.util.List)(range.iterator().next());
      // Add new actions here which apply to a single task
      ll.add(new DisplayTaskInDetailAction(store,index));
    }
    return ll;
  }


  /**
   * Action class to display the bounds in detail in a popup window
   **/
  private class DisplayTaskInDetailAction extends ViewletAction {
    List index;
    ViewletDataStore store;
    
    DisplayTaskInDetailAction(ViewletDataStore store,
                              java.util.List index) {
      super("Display task bounds");
      putValue(Action.NAME, "Display task bounds");
      putValue(Action.LONG_DESCRIPTION,
               "Popup window displaying the task in detail");
      putValue(Action.SHORT_DESCRIPTION,
               "Popup window displaying the task in detail");
      //putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
      this.store = store;
      this.index = index;
    }

    public void actionPerformed(ActionEvent e) {
      ArrayList index2 = new ArrayList(index);
      Data data;
      
      Double taskData[][] = new Double[4][4]; // start, duration, end, resource

      // start
      index2.set(0, new Integer(1));
      data = (Data)store.getViewletDataAt(index2);
      taskData[0][0] = new Double(data.initialMin);
      taskData[0][1] = new Double(data.min);
      taskData[0][2] = new Double(data.max);
      taskData[0][3] = new Double(data.initialMax);

      // duration
      index2.set(0, new Integer(2));
      data = (Data)store.getViewletDataAt(index2);
      // end = duration
      taskData[1][0] = new Double(data.initialMin);
      taskData[1][1] = new Double(data.max);
      taskData[1][2] = new Double(data.min);
      taskData[1][3] = new Double(data.initialMax);
      // end=start+duration
      taskData[2][0] = new Double(taskData[0][0].doubleValue() + taskData[1][0].doubleValue());
      taskData[2][1] = new Double(taskData[0][1].doubleValue() + taskData[1][1].doubleValue());
      taskData[2][2] = new Double(taskData[0][2].doubleValue() + taskData[1][2].doubleValue());
      taskData[2][3] = new Double(taskData[0][3].doubleValue() + taskData[1][3].doubleValue());

      // resource
      index2.set(0, new Integer(3));
      data = (Data)store.getViewletDataAt(index2);
      taskData[3][0] = new Double(data.initialMin);
      taskData[3][1] = new Double(data.max);
      taskData[3][2] = new Double(data.min);
      taskData[3][3] = new Double(data.initialMax);

      String columnNames[] = {"Initial Min", "Min", "Max", "Initial Max"};
      String rowNames[][] = {{"Start"}, {"Duration"}, {"End"}, {"Resource"}};

      JTable table = new JTable(taskData,columnNames);
      table.setEnabled(false);
      table.setColumnSelectionAllowed(false);
      table.setRowSelectionAllowed(false);
      table.setCellSelectionEnabled(false);
      JScrollPane message = new JScrollPane(table);
      String rowName[] = {""};
      JTable rowNameTable = new JTable(rowNames,rowName);
      message.setRowHeaderView(rowNameTable);
      message.getRowHeader().setPreferredSize(rowNameTable.getPreferredSize());
      message.getViewport().setPreferredSize(table.getPreferredSize());

      JOptionPane.
        showConfirmDialog(null,
                          message,
                          "Task bounds in detail",
                          JOptionPane.DEFAULT_OPTION);
    }
  }



  public static class Renderer extends CustRenderer {
    public Renderer(Element element,
                    double x, double y, double w, double h) {
      super(element, x, y, w, h);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"GanttTask Renderer constructed for element "+element);
      }
      double[] data = (double[])(element.object);
      if (data != null) {
        configure(new Rectangle2D.Double(x,y,w,h),
                  data);
      }
    }

    public void configure(Rectangle2D bounds,
                          double[] data) {
      // data[0]=start min, data[1]=start max,
      // data[2]=end min, data[3]=end max,
      // data[4]=resource min, data[5]=resource max

      // draw a "box and stick" diagram within the specified bounds
      //
      // E  +------+    +----------------+
      //    |      |    |                |
      //    |      |    |                |
      //    |      |    |                |
      // F  +------+----+----------------+
      //    A      B    C                D
      //
      //Rectangle2D bounds = getBounds2D();
      float A,B,C,D,E,F;
      //A = (float)data[0];
      //D = (float)data[3];
      A = (float)bounds.getMinX();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"A="+A+" bounds.getMinX()"+bounds.getMinX());
      }
      D = (float)bounds.getMaxX();
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"D="+D);
      }
      //E = (float)(data[4]);
      //F = (float)((data[5]));
      E = (float)bounds.getMinY();
      F = (float)bounds.getMaxY();

      // remove potential for divide by zero
      if (data[0] == data[3]) {
        data[3]+=0.001;
      }
      // calculate scale for drawing
      double rangeX=data[3]-data[0];
      double rangeY=data[5]-data[4];

      B = A + (float)(((data[2]-data[0]) * bounds.getWidth()) / rangeX);
      C = A + (float)(((data[1]-data[0]) * bounds.getWidth()) / rangeX);

      // draw outline
      float vertices[][] = {{A,F},{A,E},{B,E},{B,F},
                            {C,F},{C,E},{D,E},{D,F}};
      path.moveTo(vertices[0][0], vertices[0][1]);
      for(int i = 1; i < vertices.length; i++) {
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,"GanttTask point "+i+
                                      " x="+vertices[i][0]+
                                      " y="+vertices[i][1]);
        }
        path.lineTo(vertices[i][0], vertices[i][1]);
      }
      path.closePath();
    }
  }

    

}

