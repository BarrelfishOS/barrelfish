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
// The Original Code is  CPViz Constraint Visualization System
// The Initial Developer of the Original Code is  Helmut Simonis
// Portions created by the Initial Developer are
// Copyright (C) 2009-2010 Helmut Simonis
// 
// Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
//			Paul Davern, 4C, Univerity College Cork, Cork
// 
// END LICENSE BLOCK
// ----------------------------------------------------------------------
package components;

import java.awt.Desktop;
import java.net.URI;
import java.net.URL;

import javax.help.JHelpContentViewer;
import javax.help.plaf.basic.BasicContentViewerUI;
import javax.swing.JComponent;
import javax.swing.event.HyperlinkEvent;

public class ExternalLinkContentViewerUI extends BasicContentViewerUI{
	public ExternalLinkContentViewerUI(JHelpContentViewer x){
		super(x);
	}

	public static javax.swing.plaf.ComponentUI createUI(JComponent x){
		return new ExternalLinkContentViewerUI((JHelpContentViewer)x);
	}
	public void hyperlinkUpdate(HyperlinkEvent he){
		if(he.getEventType()==HyperlinkEvent.EventType.ACTIVATED){
			try{
				URL u = he.getURL();
				if(u.getProtocol().equalsIgnoreCase("mailto")||u.getProtocol().equalsIgnoreCase("http")||u.getProtocol().equalsIgnoreCase("ftp")){
					URI ui = u.toURI();
					Desktop.getDesktop().browse(ui);
					return;
				}
			}
			catch(Throwable t){}
		}
		super.hyperlinkUpdate(he);
	}
}

