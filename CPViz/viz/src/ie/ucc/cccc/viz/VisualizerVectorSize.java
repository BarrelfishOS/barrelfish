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
//			
// 
// END LICENSE BLOCK
// ----------------------------------------------------------------------
package ie.ucc.cccc.viz;

import java.io.PrintWriter;

public class VisualizerVectorSize extends VisualizerGraph {

	public VisualizerVectorSize(VisualContext context) {
		super(context);
	}
	
	public void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] vars;
		int[] count = new int[100];
		int line = 0;
		int max = 1;
		do {
			vars = visualState.getEntries().asDomainArray();
			int total = 0;
			for(int i =1; i < vars.length; i++){
				total += vars[i].size();
			}
			max = Math.max(max,total);
			count[line] = total;
			visualState = visualState.getParent();
			line++;
		} while (visualState != null);
		setMax(max);
		standardGrid(out);
		pathStartSVG(out);
		pathMoveSVG(out,posX(line-1),posY(count[0]));
		for(int i=0;i<line;i++){
			pathLineSVG(out,posX(line-i-1),posY(count[i]));
		}
		pathEndSVG(out,Colors.ASSIGN_COLOR);
		for(int i=0;i<line;i++){
			textSVG(out,posX(line-i-1),posY(count[i]),count[i],Colors.LABEL_TEXT_COLOR);
		}
	}	


}
