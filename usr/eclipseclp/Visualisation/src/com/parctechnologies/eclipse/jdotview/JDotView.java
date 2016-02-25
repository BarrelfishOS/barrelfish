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

package com.parctechnologies.eclipse.jdotview;

import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.swing.*;
import java.awt.print.*;
import att.grappa.*;

public class JDotView
	implements GrappaConstants
{
    public static final int SLEEPTIME=500;

    public Frame  frame  = null;

    public static void main(String[] args) {
	InputStream input = System.in;
        String filename = "-";
	if(args.length > 2) {
	    System.err.println("USAGE: java JDotView [input_graph_file]");
	    System.exit(1);
	} else if(args.length >= 1) {
            filename=args[0];
	}
	JDotView viewer = new JDotView();
	viewer.view(filename);
    }

    JDotView() {
    }

    void view(String filename) {
	frame = new Frame("ECLiPSe graph viewer : "+filename);
        final GraphVizPanel panel = new GraphVizPanel(filename, SLEEPTIME);
        frame.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent wev) {
                    Window w = wev.getWindow();
                    w.setVisible(false);
                    w.dispose();
                    panel.cleanupAndExit();
                }
            });
	frame.add(panel);
        frame.setSize(600,400);
        frame.setLocation(100,100);
	frame.show();
    }


}
