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
 * Small Icon to be used for ToggleHold action and to display in corner of
 * viewlet component.
 *
 */
public class HoldIcon implements Icon
{
  private int height;
  private int width;
  private Rectangle rect1, rect2;
  private Color holdSymbolColor = new Color(180, 180, 180);

  public HoldIcon(int width, int height)
  {
    this.width = width;
    this.height = height;
    initialiseShapes();
  }

  private void initialiseShapes()
  {
    rect1 =
      new Rectangle((int) (width * 0.1), (int) (height * 0.2),
                    (int) (width * 0.35), (int) (height * 0.6));

    rect2 =
      new Rectangle((int) (width * 0.55), (int) (height * 0.2),
                    (int) (width * 0.35), (int) (height * 0.6));
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
    g.setColor(holdSymbolColor);
    g.fillRect(x+rect1.x, y+rect1.y, rect1.width, rect1.height);
    g.fillRect(x+rect2.x, y+rect2.y, rect2.width, rect2.height);
    g.setColor(c.getForeground());
    g.drawRect(x+rect1.x, y+rect1.y, rect1.width, rect1.height);
    g.drawRect(x+rect2.x, y+rect2.y, rect2.width, rect2.height);
  }
}
