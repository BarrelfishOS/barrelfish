/*

   Licensed to the Apache Software Foundation (ASF) under one or more
   contributor license agreements.  See the NOTICE file distributed with
   this work for additional information regarding copyright ownership.
   The ASF licenses this file to You under the Apache License, Version 2.0
   (the "License"); you may not use this file except in compliance with
   the License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

 */
package components;

import java.awt.Insets;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JButton;

/**
 * This class represents the buttons used in toolbars.
 *
 * @author <a href="mailto:stephane@hillion.org">Stephane Hillion</a>
 * @version $Id: JToolbarButton.java,v 1.1 2013/02/21 11:24:35 jschimpf Exp $
 */
public class JToolbarButton extends JButton {
    /**
     * Creates a new toolbar button.
     */
    public JToolbarButton() {
        initialize();
    }

    /**
     * Creates a new toolbar button.
     * @param txt The button text.
     */
    public JToolbarButton(String txt) {
        super(txt);
        initialize();
    }

    /**
     * Initializes the button.
     */
    protected void initialize() {
        if (!System.getProperty("java.version").startsWith("1.3")) {
            setOpaque(false);
            setBackground(new java.awt.Color(0, 0, 0, 0));
        }
        setBorderPainted(false);
        setMargin(new Insets(2, 2, 2, 2));
        addMouseListener(new MouseListener());
    }

    /**
     * To manage the mouse interactions.
     */
    protected class MouseListener extends MouseAdapter {
        public void mouseEntered(MouseEvent ev) {
            setBorderPainted(true);
        }
        public void mouseExited(MouseEvent ev) {
            setBorderPainted(false);
        }
    }
}
