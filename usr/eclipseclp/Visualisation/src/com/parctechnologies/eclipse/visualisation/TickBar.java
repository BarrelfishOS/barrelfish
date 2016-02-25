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

import java.awt.print.*;
import java.awt.FontMetrics;
import java.awt.Cursor;
import java.awt.Color;
import java.awt.Shape;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.Dimension;
import java.awt.event.ComponentListener;
import java.awt.event.ComponentEvent;
import javax.swing.*;


/**
 * A component which draws a "ruler" indicating the width/height of
 * another component.
 *
 * <p> The width displayed on the TickBar is the width in pixels of
 * the observed component divided by the value of "scale".
 *
 * <p> The TickBar resizes itself when the observed component resizes.
 **/
public class TickBar
  extends JComponent
  implements ComponentListener, Scrollable, Zoomable, Printable {

  final int HEIGHT=20;
  int WIDTH=20;
  final int PRINTER_GAP=10;

  /** Indicates that this ruler is vertical and so should measure height **/
  boolean vertical;

  /** Initial gap between large ticks with labels **/
  int initialMajorTickSpace;

  /** Initial gap between large ticks with labels **/
  int initialMinorTickSpace;

  /** Gap between large ticks with labels **/
  int majorTickSpace;

  /** Gap between large ticks with labels **/
  int minorTickSpace;

  /** Initial scaling factor in the X direction **/
  double initialScale;

  /** Scaling factor in the X direction **/
  double scale;

  /**
   * Space to leave on the left of bar before first tick.  Note this
   * margin is only applicable to screen display, and is NOT used when
   * printing.
   **/
  double displayMargin;

  /**
   * An overall zoom level
   **/
  float zoomLevel;

  /**
   * The object whose width this tick bar is meant to display (ie. if
   * this object gets re-sized, then the tick bar should change size
   * aswell).
   **/
  JComponent panel;

  /**
   * Cursor used to indicate that the tickbar is shrinking
   **/
  Cursor leftScalingCursor;

  /**
   * Cursor used to indicate that the tickbar is growing
   **/
  Cursor rightScalingCursor;

  public TickBar(JComponent panel,
                 boolean vertical,
                 int majorTickSpace,
                 int minorTickSpace,
                 double scale,
                 double margin) {
    this.vertical = vertical;
    this.initialMajorTickSpace = majorTickSpace;
    this.initialMinorTickSpace = minorTickSpace;
    this.majorTickSpace = majorTickSpace;
    this.minorTickSpace = minorTickSpace;
    this.initialScale = scale;
    this.scale = scale;
    this.displayMargin = margin;
    this.zoomLevel = 1.0f;
    this.panel = panel;
    leftScalingCursor = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR);
    rightScalingCursor = Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);
    setScalingRight(true);
    // register this object as a listener on the size of the "panel" component
    panel.addComponentListener(this);
  }

  public void paintComponent(Graphics g) {
    Rectangle clip = g.getClipRect();
    if (g instanceof PrinterGraphics) {
      // do not clear background when printing, and do not use a margin
      paintTicks(g,0);
    } else {
      // clear background when drawing on screen
      g.setColor(Color.white);
      g.fillRect(clip.x, clip.y, clip.width, clip.height);
      paintTicks(g,(int)displayMargin);
    }
  }

  void paintTicks(Graphics g,int margin) {
    int width;
    int height;
    width = (int)getBounds().width;
    height = (int)getBounds().height;
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,
                                  "paintTicks width="+width+
                                  " height="+height+
                                  " majorTickSpace="+majorTickSpace+
                                  " minorTickSpace="+minorTickSpace+
                                  " zoomLevel="+zoomLevel+
                                  " scale="+scale);
    }
    g.setColor(Color.black);
    FontMetrics fm = g.getFontMetrics();
    if (vertical) {
      boolean widthChanged = false;
      for(double i = 0; i <= height; i+= majorTickSpace) {
        String s = ""+i;
        int y = height - ((int)(margin*zoomLevel)+(int)(i*scale*zoomLevel));
        Rectangle2D rect = fm.getStringBounds(s,g);
        if (rect.getWidth() > WIDTH) {
          WIDTH = (int)(rect.getWidth());
          widthChanged = true;
        }
        g.drawString(s,0,y+(int)(rect.getHeight()/2));
        g.drawLine((width/2),y,width-1,y);
      }
      // minor ticks
      for(double i = 0; i <= height; i+= minorTickSpace) {
        int y = height - ((int)(margin*zoomLevel)+(int)(i*scale*zoomLevel));
        g.drawLine((width*3)/4,y,width-1,y);
      }
      if (widthChanged) {
        invalidate();
      }
    } else {
      // major ticks
      for(double i = 0; i <= width; i+= majorTickSpace) {
        String s = ""+i;
        int x = (int)(margin*zoomLevel)+(int)(i*scale*zoomLevel);
        Rectangle2D rect = fm.getStringBounds(s,g);
        g.drawString(s,x-(int)(rect.getWidth()/2),(int)(rect.getHeight()));
        g.drawLine(x,(height)/2,x,height-1);
      }
      // minor ticks
      for(double i = 0; i <= width; i+= minorTickSpace) {
        int x = (int)(margin*zoomLevel)+(int)(i*scale*zoomLevel);
        g.drawLine(x,(height*3)/4,x,height-1);
      }
    }
  }

  public void setScalingRight(boolean scaling) {
    if (scaling) {
      setCursor(rightScalingCursor);
    } else {
      setCursor(leftScalingCursor);
    }
  }

  public void setScale(double scale) {
    this.scale = scale;
    double ratio = (initialScale / scale);
    if ( ratio >= 2) {
      // the scale has shrunk more than twice, so modify the major and
      // minor ticks accordingly
      // round the ratio down to the nearest integer
      int ratioFloor = (int)(Math.round(Math.floor(ratio)));
      this.majorTickSpace = ratioFloor * initialMajorTickSpace;
      this.minorTickSpace = ratioFloor * initialMinorTickSpace;
    } else if ( ratio <= 1 ) {
      // the scale is larger than the original, so set the initial
      // major and minor ticks gaps
      this.majorTickSpace = initialMajorTickSpace;
      this.minorTickSpace = initialMinorTickSpace;
    }
    repaint();
  }

  public Dimension getPreferredSize() {
    if ( vertical ) {
      return new Dimension(WIDTH, panel.getPreferredSize().height);
    } else {
      return new Dimension(panel.getPreferredSize().width, HEIGHT);
    }
  }

  public Dimension getPreferredScrollableViewportSize() {
    return getPreferredSize();
  }

  public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
    return majorTickSpace;
  }

  public boolean getScrollableTracksViewportHeight() {
    return false;
  }

  public boolean getScrollableTracksViewportWidth() {
    return false;
  }

  public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
    return 1;
  }

  // Zoomable interface

  /**
   * Set the size which will be considered normal. i.e. the one which defines
   * the height/width ratio
   */
  public void setNormalSize(Dimension normalSize) {
    // do nothing for now, as I do not believe that this interface
    // method is ever used
  }
  
  public Dimension getNormalSize() {
    return getPreferredSize();
  }
  
  /** The parent graph has been zoomed, so we must zoom the tick bar
      accordingly **/
  public void zoomToLevel(float zoomLevel) {
    this.zoomLevel = zoomLevel;
    invalidate();
  }
  
  /** The parent graph has been zoomed, so we must zoom the tick bar
      accordingly **/
  public void zoomInByRatio(float zoomRatio) {
    this.zoomLevel = this.zoomLevel * zoomRatio;
    invalidate();
  }
  
  /**
   * Return most recently set zoomLevel;
   */
  public float getZoomLevel() {
    return zoomLevel;
  }


  public int print(Graphics g, PageFormat pf, int pi)
    throws PrinterException
  {
    if (pi >= 1) {
      return Printable.NO_SUCH_PAGE;
    }
    // draw the ticks "above" the chart and leave a gap of PRINTER_GAP
    //((Graphics2D)g).translate(0,-(HEIGHT+PRINTER_GAP));
    ((Graphics2D)g).translate(0,0);
    paintComponent(g);
    return Printable.PAGE_EXISTS;
  }

  // ComponentListener interface methods
  public void componentHidden(ComponentEvent e) {
    // do nothing
  }

  public void componentMoved(ComponentEvent e) {
    // do nothing
  }

  public void componentResized(ComponentEvent e) {
    setSize(e.getComponent().getSize());
  }

  public void componentShown(ComponentEvent e) {
    // do nothing
  }  
}
