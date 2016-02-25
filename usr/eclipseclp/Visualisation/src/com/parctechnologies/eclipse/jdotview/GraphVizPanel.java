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


public class GraphVizPanel extends JPanel implements ActionListener
{
    MyGrappaPanel gp;
    Graph graph = null;
    String filename;
    FileWatcher fw = null;
    
    JButton printer = null;
    JButton alias = null;
    JButton draw = null;
    JButton quit = null;
    JPanel panel = null;
    JScrollPane jsp = null;
    
    public GraphVizPanel(final String filename, long SLEEPTIME) {
        this.filename = filename;
        
        fw = new FileWatcher(filename, SLEEPTIME);
        fw.addActionListener(this);
        fw.start();
        
        
        jsp = new JScrollPane();
        jsp.getViewport().setBackingStoreEnabled(true);
        
        Grappa.nodeLabelsScaleCutoff = 0.1;
        Grappa.edgeLabelsScaleCutoff = 0.1;
        Grappa.subgLabelsScaleCutoff = 0.1;
        Grappa.useAntiAliasing = false;
        
        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        
        panel = new JPanel();
        panel.setLayout(gbl);
        
        draw = new JButton("Draw");
        gbl.setConstraints(draw,gbc);
        panel.add(draw);
        draw.addActionListener(this);
        
        alias = new JButton("Antialias");
        gbl.setConstraints(alias,gbc);
        panel.add(alias);
        alias.addActionListener(this);
        
        printer = new JButton("Print");
        gbl.setConstraints(printer,gbc);
        panel.add(printer);
        printer.addActionListener(this);
        
        quit = new JButton("Quit");
        gbl.setConstraints(quit,gbc);
        panel.add(quit);
        quit.addActionListener(this);
        
        this.setLayout(new BorderLayout());
        add("Center", jsp);
        add("West", panel);
        
        setVisible(true);
    }
    
    public void actionPerformed(ActionEvent evt) {
        if(evt.getSource() instanceof JButton) {
            JButton tgt = (JButton)evt.getSource();
            if((tgt == draw) && (graph != null)) {
                graph.repaint();
            } else if(tgt == quit) {
                cleanupAndExit();
            } else if((tgt == alias)  && (graph != null)) {
                Grappa.useAntiAliasing = !Grappa.useAntiAliasing;
                graph.repaint();
            } else if((tgt == printer)  && (graph != null)) {
                PageFormat pf = new PageFormat();
                Rectangle2D bb = graph.getBoundingBox();
                if(bb.getWidth() > bb.getHeight())
                    pf.setOrientation(PageFormat.LANDSCAPE);
                try {
                    PrinterJob printJob = PrinterJob.getPrinterJob();
                    printJob.setPrintable(gp, pf);
                    if (printJob.printDialog()) {
                        printJob.print();
                    }
                } catch (Exception ex) {
                    Grappa.displayException(ex, "Problem with print request");
                }
            }
        } else if (evt.getSource() instanceof FileWatcher) {
            // the file has changed so reload
            String fname = evt.paramString();
            try {
                setGraph(loadGraph(filename));
            } catch(FileNotFoundException fnf) {
                System.err.println(fnf.toString());
                System.exit(1);
            }
        }
    }
    
    public Graph loadGraph(String filename) throws FileNotFoundException {
        InputStream input = System.in;
        input = new FileInputStream(filename);
        Parser program = new Parser(input,System.err);
        try {
            program.parse();
        } catch(Exception ex) {
            System.err.println("Exception: " + ex.getMessage());
            ex.printStackTrace(System.err);
            System.exit(1);
        }
        return program.getGraph();
    }
    
    public void setGraph(Graph g) {
        boolean scale=true;
        Dimension2D dim = null;
        Point pt;
        double mult = 1.0;
        this.graph = g;
        if (gp != null) {
            // record previous scale and zoom state
            scale = gp.getScaleToFit();
            dim = gp.getScaleToSize();
            mult = gp.getScaleFactor();
        }
        // create the new GrappaPanel to display the graph
        gp = new MyGrappaPanel(graph);
        gp.addGrappaListener(new MyGrappaAdapter());
        // set the new scale and zoom state
        gp.setScaleToFit(scale);
        gp.setScaleToSize(dim);
        gp.multiplyScaleFactor(mult);
        
        java.awt.Rectangle bbox = graph.getBoundingBox().getBounds();
        pt = jsp.getViewport().getViewPosition();
        jsp.getViewport().setView(gp);
        // Hack to ensure windows contents correctly reflect the
        // old viewport position
        try {
            SwingUtilities.invokeAndWait(new Runnable() {public void run(){}});
        } catch(Exception e) {}
        jsp.getViewport().setViewPosition(pt);
    }
    
    public void cleanupAndExit() {
        if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(null,"Delete temporary file \""+ filename +"\"")) {
            new File(filename).delete();
        }
        System.exit(0);
    }
    
    class MyGrappaAdapter extends GrappaAdapter {
        public void actionPerformed(ActionEvent aev) {
            super.actionPerformed(aev);
            // force a repaint of the whole graph to account for
            // zoom related screen corruptions
            RepaintManager.currentManager(gp).markCompletelyDirty(gp);
        }
    }
    
    // We must use this intermediate GrappaPanel class to record
    // zoom and scale settings
    class MyGrappaPanel extends GrappaPanel {
        boolean myScaleToFit;
        Dimension2D myScaleToSize;
        double mult;
        public MyGrappaPanel(Graph g) {
            super(g);
        }
        public void setScaleToFit(boolean scale) {
            super.setScaleToFit(scale);
            myScaleToFit=scale;
        }
        public boolean getScaleToFit() {
            return myScaleToFit;
        }
        public void setScaleToSize(Dimension2D scaleSize) {
            super.setScaleToSize(scaleSize);
            myScaleToSize=scaleSize;
        }
        public Dimension2D getScaleToSize() {
            return myScaleToSize;
        }
        public double multiplyScaleFactor(double multiplier) {
            double res = super.multiplyScaleFactor(multiplier);
            mult = res * multiplier;
            if (mult==0) {
                mult=res;
            }
            return res;
        }
        public double getScaleFactor() {
            return mult;
        }
    }
}
