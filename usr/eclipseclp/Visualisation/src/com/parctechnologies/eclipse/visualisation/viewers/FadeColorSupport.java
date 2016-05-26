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

import java.awt.Color;
import java.awt.Component;
import javax.swing.Icon;
import java.util.*;


/**
 * Store information about fading colors and number of fading steps
 */
public class FadeColorSupport {

    /** Holds four colours per fade level*/
    Color[][] forwardColor;
    Color[][] backwardColor;
    
    public Icon getFadeIcon(int w, int h) {
        return new FadeIcon(w,h);
    }

  // By default fade to white
    public FadeColorSupport(int maxFade, Color c1, Color c2) {
      this(maxFade, c1, c2, Color.white);
    }

    public FadeColorSupport(int maxFade, Color c1, Color c2, Color plain) {
        forwardColor = new Color[maxFade+1][4];
        backwardColor = new Color[maxFade+1][4];
//          for(int i = 0; i < maxFade+1; i++) {
//              int j = (255 * (maxFade-i)) / maxFade;
//              forwardColor[i][0] = new Color(j,255,j);
//              forwardColor[i][1] = forwardColor[i][0].darker();
//              forwardColor[i][2] = forwardColor[i][1].darker();
//              forwardColor[i][3] = forwardColor[i][2].darker();
            
//              backwardColor[i][0] = new Color(255,j,j);
//              backwardColor[i][1] = backwardColor[i][0].darker();
//              backwardColor[i][2] = backwardColor[i][1].darker();
//              backwardColor[i][3] = backwardColor[i][2].darker();
//          }
        int pr = plain.getRed();
        int pg = plain.getGreen();
        int pb = plain.getBlue();
        int dr1 = c1.getRed() - pr;
        int dg1 = c1.getGreen() - pg;
        int db1 = c1.getBlue() - pb;
        int dr2 = c2.getRed() - pr;
        int dg2 = c2.getGreen() - pg;
        int db2 = c2.getBlue() - pb;
        for(int i = 0; i < maxFade+1; i++) {
            int j = (255 * (maxFade-i)) / maxFade;
            int r1 = pr + (dr1 * i) / maxFade;
            int g1 = pr + (dg1 * i) / maxFade;
            int b1 = pr + (db1 * i) / maxFade;
            forwardColor[i][0] = new Color(r1, g1, b1);
            forwardColor[i][1] = forwardColor[i][0].darker();
            forwardColor[i][2] = forwardColor[i][1].darker();
            forwardColor[i][3] = forwardColor[i][2].darker();
            
            int r2 = pr + (dr2 * i) / maxFade;
            int g2 = pr + (dg2 * i) / maxFade;
            int b2 = pr + (db2 * i) / maxFade;
            backwardColor[i][0] = new Color(r2,g2,b2);
            backwardColor[i][1] = backwardColor[i][0].darker();
            backwardColor[i][2] = backwardColor[i][1].darker();
            backwardColor[i][3] = backwardColor[i][2].darker();
        }
    }
}
