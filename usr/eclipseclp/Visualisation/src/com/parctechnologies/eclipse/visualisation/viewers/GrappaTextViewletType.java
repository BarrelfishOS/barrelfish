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
import java.awt.event.ActionEvent;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.*;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Displays a textual representation of the viewable element as a
 * grappa Node, or as a label on a grapp Edge */
public class GrappaTextViewletType extends TextViewletType {

  FadeColorSupport edgeFadeColorSupport;

  /**
   * If true, the numeric value of the edge will be interpreted as the
   * width of the line. If greater than 1.0, the edge is draw in red.
   **/
  boolean capacityEdges;

  /**
   * If true, an edge value of "0" means the edge will not be draw,
   * "1" means it will and anything else will be draw gray with a
   * label.
   **/
  boolean zeroOneEdges;

  /**
   * Format used when printing numbers
   **/
  NumberFormat nf;

  public GrappaTextViewletType(String changeable,
                               boolean zeroOneEdges,
                               boolean capacityEdges) {
    super(changeable);
    // fade to black for edges
    edgeFadeColorSupport =
      new FadeColorSupport(MAX_FADE, new Color(0,255,0), new Color(255,0,0), new Color(0,0,0));
    this.capacityEdges = capacityEdges;
    this.zeroOneEdges = zeroOneEdges;
    this.nf = new DecimalFormat("0.000");
  }

  public void customizeElement(ViewletDataStore store,
                               java.util.List index,
                               Element element) {
    Data data = (Data)(store.getViewletDataAt(index));
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "update called with Data="+data);
    }
    if (element instanceof Node) {
      Node node = (Node)element;
      // sets the shape from the string in the "fromNode" field
      node.setAttribute("shape", data.getShape());
      // set background color
      element.setAttribute("color", getColor(data, false));
      //node.setAttribute("color",Color.white);
      node.setAttribute("style", "filled");
      // set the node label
      node.setAttribute("label", data.getText());
    } else if (element instanceof Edge) {
      // instance of edge
      Edge edge = (Edge)element;
      if (zeroOneEdges) {
        // set the line style to "dotted" if holdOnUpdate
        if (data.getHoldsOnUpdates()) {
          edge.setAttribute("style","dotted");
        } else {
          edge.setAttribute("style","solid");
        }
        // edge is labelled with "0" or "1"
        if ("1".equals(data.getText())) {
          // simple edge labelled with text
          // set background color
          element.setAttribute("color", getEdgeColor(data));
          //edge.setAttribute("label", "");
        } else if ("0".equals(data.getText())) {
          element.setAttribute("color", Color.white);
          //edge.setAttribute("label", "");
        } else {
          element.setAttribute("color", Color.gray);
          //edge.setAttribute("label", data.getText());
        }
      } else if (capacityEdges) {
        double val = 0.0;
        double width = 0.2;
        try {
          val = Double.parseDouble(data.getText());
        } catch(NumberFormatException nfe) {
          // not a number, considered to be zero
          val = 0.0;
        }
        edge.setAttribute("label", nf.format(val));
        if ((val > 1.0) || (val < 0.0)) {
          width = 25.0;
          element.setAttribute("color", Color.red);
        } else {
          width = (val < 0.01)?(0.2):(20.0 * val);
          element.setAttribute("color", Color.black);
        }
        // set the line style to "dotted" if holdOnUpdate
        if (data.getHoldsOnUpdates()) {
          edge.setAttribute("style","dotted,linewidth("+width+")");
        } else {
          edge.setAttribute("style","solid,linewidth("+width+")");
        }
      } else {
        // edge is labelled with simple text
        // set the line style to "dotted" if holdOnUpdate
        if (data.getHoldsOnUpdates()) {
          edge.setAttribute("style","dotted");
        } else {
          edge.setAttribute("style","solid");
        }
        // set edge label
        element.setAttribute("color", getEdgeColor(data));
        edge.setAttribute("label", data.getText());
      }
    }
  }

  protected Color getEdgeColor(Data data) {
    Color col;
    int val = data.getFadeCount();
    if (val > 0) {
      col = edgeFadeColorSupport.forwardColor[val][0];
    } else {
      col = edgeFadeColorSupport.backwardColor[-val][0];
    }
    return col;
  }


  public ViewletData build() {
    return new Data();
  }
  
  public String getDescription() {
    return("Shaped text viewlet");
  }


  /**
   * Return a collection of actions which can be applied to viewlets
   * in this table
   */
  public Collection getActions(ViewletDataStore store,
                               ViewletRange range) {
    Collection ll = super.getActions(store, range);
    if ((range != null) & (!range.isEmpty())) {
//        ll.add((new ChangeShapeAction(store, range, "circle")));
//        ll.add((new ChangeShapeAction(store, range, "ellipse")));
//        ll.add((new ChangeShapeAction(store, range, "triangle")));
//        ll.add((new ChangeShapeAction(store, range, "box")));
//        ll.add((new ChangeShapeAction(store, range, "pentagon")));
//        ll.add((new ChangeShapeAction(store, range, "house")));
//        ll.add((new ChangeShapeAction(store, range, "hexagon")));
//        ll.add((new ChangeShapeAction(store, range, "octagon")));
    }
    return ll;
  }

  private class ChangeShapeAction extends ViewletAction {
    String shape;
    ViewletRange range;
    ViewletDataStore store;

    ChangeShapeAction(ViewletDataStore store, ViewletRange range, String shape)
    {
      super("Change shape:"+shape);
      putValue(Action.NAME, "Change shape:"+shape);
      putValue(Action.LONG_DESCRIPTION,
               "Change the shape which surrounds the text to be:"+shape);
      putValue(Action.SHORT_DESCRIPTION,
               "Change shape to:"+shape);
      //putValue(Action.SMALL_ICON, new FadeIcon(20, 20));
      this.shape = shape;
      this.range = range;
      this.store = store;
    }

    public void actionPerformed(ActionEvent e)
    {

      Iterator viewletsIterator = range.iterator();
      while(viewletsIterator.hasNext()) {
        List index = (List) viewletsIterator.next();
        Data viewlet = (Data)(store.getViewletDataAt(index));
        viewlet.setShape(shape);
        store.setViewletDataAt(index, viewlet);
      }
      store.fireViewletRangeUpdated(range);
    }
  }
  
  /*
   * Data is a viewlet which can monitor elements of any type. It is
   * responsible for:
   * <ul>
   * <li> Maintaining a record of the text representation of the term.
   * <li> Storing the shape of grappa node to create
   * </ul>
   */
  public static class Data extends TextViewletType.Data {
    private String shape;
    
    public Data() {
      super();
      shape = "ellipse";
    }
    
    public String getShape() {
      return shape;
    }
    
    public void setShape(String newValue) {
      shape = newValue;
    }
  }
}
