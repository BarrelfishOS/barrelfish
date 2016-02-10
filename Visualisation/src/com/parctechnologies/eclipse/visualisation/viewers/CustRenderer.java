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

public class CustRenderer extends GrappaShape implements CustomRenderer {
  /** Stores copy of element for which this Custom Renderer was
      created */
  protected Element element;

  /**
   * This constructor is called by the GrappaNexus updateShape method */
  public CustRenderer(Element element,
                      double x, double y, double w, double h) {
    super(CUSTOM_SHAPE, x+(w/2), y+(h/2), w, h, 
          4,
          1,
          0.0,
          0.0,
          0.0,
          false,
          false,
          null);
    this.element = element;
  }

  /**
   * Retrieves the viewletData which this renderer must display.
   *
   * <p> Requires that element.object is a two element array
   * containing the ContainerViewer and the element index for this
   * viewlet.
   **/
  public ViewletData getViewletData() {
    return (ViewletData)element.object;
  }

  /**
   * The method called when the element needs to be drawn.
   * When used with an extention of <i>GrappaShape</i>,
   * the default behavior is obtained by:
   * <pre>
   * public void draw(java.awt.Graphics2D g2d) {
   *   g2d.draw(this);
   * }
   * </pre>
   *
   * @param g2d the Graphics2D context to be used for drawing
   */
  public void draw(java.awt.Graphics2D g2d) {
    g2d.draw(this);
  }
      
  /**
   * The method called when the element needs to be filled.
   * When used with an extention of <i>GrappaShape</i>,
   * the default behavior is obtained by:
   * <pre>
   * public void fill(java.awt.Graphics2D g2d) {
   *   g2d.fill(this);
   * }
   * </pre>
   *
   * @param g2d the Graphics2D context to be used for drawing
   */
  public void fill(java.awt.Graphics2D g2d) {
    g2d.fill(this);
  }
  
  /**
   * The method called when the element needs to draw its background
   * image.
   * When used with an extention of <i>GrappaShape</i> that provides
   * the underlying element as a global variable, the default behavior
   * is obtained by:
   * <pre>
   * public void drawImage(java.awt.Graphics2D g2d) {
   *   Rectangle sbox = this.getBounds();
   *   Shape clip = g2d.getClip();
   *   g2d.clip(this);
   *   g2d.drawImage(element.getGrappaNexus().getImage(), sbox.x, sbox.y, sbox.width, sbox.height, null);
   *   g2d.setClip(clip);
   * }
   * </pre>
   *
   * @param g2d the Graphics2D context to be used for drawing
   */
  public void drawImage(java.awt.Graphics2D g2d) {
    Rectangle sbox = this.getBounds();
    Shape clip = g2d.getClip();
    g2d.clip(this);
    g2d.drawImage(element.getGrappaNexus().getImage(), 
                  sbox.x, sbox.y, sbox.width, sbox.height, null);
    g2d.setClip(clip);
  }
} 
