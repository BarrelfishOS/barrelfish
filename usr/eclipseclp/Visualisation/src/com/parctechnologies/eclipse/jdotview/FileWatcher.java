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

import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;


public class FileWatcher extends Thread {
    File file;
    String filename;
    long lastModified;
    ArrayList listenerList;
    long SLEEPTIME;
    
    public FileWatcher(String filename, long SLEEPTIME) {
        this.setDaemon(true);
        this.filename=filename;
        this.file = new File(filename);
        this.SLEEPTIME=SLEEPTIME;
        this.lastModified = 0;
        this.listenerList = new ArrayList();
    }
    
    public void addActionListener(ActionListener al) {
        listenerList.add(al);
    }
    
    public void run() {
        while(true) {
            try {
                long lm = file.lastModified();
                if (lm > lastModified ) {
                    lastModified = lm;
                    Object[] listeners = listenerList.toArray();
                    // Process the listeners last to first, notifying
                    // those that are interested in this event
                    for (int i = listeners.length-1; i>=0; i--) {
                        ((ActionListener)listeners[i]).
                            actionPerformed(new ActionEvent( this, 0, filename));
                    }
                }
                sleep(SLEEPTIME);
            } catch(InterruptedException ie) {
                // no need to do anything
            }
        }
    }
}
