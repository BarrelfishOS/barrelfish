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

import java.awt.*;
import javax.swing.*;

/**
 * Small Icon to be used for ToggleFade action and to display in some
 * corner of viewlet component.
 **/
public class FadeIcon implements Icon
{
  private int height;
  private int width;
  private Rectangle[] greenRect, redRect;
  private Color[] greenFadeColor = {new Color(255, 255, 255),
                                    new Color(200, 255, 200),
                                    new Color(150, 255, 150),
                                    new Color(100, 255, 100) };
  private Color[] redFadeColor = {new Color(255, 100, 100),
                                  new Color(255, 150, 150),
                                  new Color(255, 200, 200),
                                  new Color(255, 255, 255)};

  public FadeIcon(int width, int height)
  {
    this.width = width;
    this.height = height;
    initialiseShapes();
  }

  private void initialiseShapes()
  {
    greenRect = new Rectangle[greenFadeColor.length];
    double gh = 1.0 / (greenFadeColor.length + 1);
    for(int i = 0; i < greenFadeColor.length; i++) {
      greenRect[i] =
        new Rectangle((int)(width * 0.1), (int) (height * ((gh/2) + gh*i)),
                      (int)(width * 0.4), (int) (height * gh));
    }
    redRect = new Rectangle[redFadeColor.length];
    double rh = 1.0 / (redFadeColor.length + 1);
    for(int i = 0; i < redFadeColor.length; i++) {
      redRect[i] =
        new Rectangle((int)(width * 0.5), (int) (height * ((rh/2) + rh*i)),
                      (int)(width * 0.4), (int) (height * rh));
    }
  }

  public int getIconHeight()
  {
    return(height);
  }

  public int getIconWidth()
  {
    return(width);
  }

  public void paintIcon(Component c, Graphics g, int x, int y)
  {
    for(int i = 0; i < greenFadeColor.length; i++) {
      Rectangle rect = greenRect[i];
      g.setColor(greenFadeColor[i]);
      g.fillRect(x+rect.x, y+rect.y, rect.width, rect.height);
    }
    for(int i = 0; i < redFadeColor.length; i++) {
      Rectangle rect = redRect[i];
      g.setColor(redFadeColor[i]);
      g.fillRect(x+rect.x, y+rect.y, rect.width, rect.height);
    }
    g.setColor(c.getForeground());
  }
}
