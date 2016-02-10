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

/**
 * Visualizer for the alldifferent constraint. Currently uses no argument, holds the 
 * items directly in its entries list
 * @author hsimonis
 *
 */
public class VisualizerAllDifferent extends VisualizerVector {
	public VisualizerAllDifferent(VisualContext context) {
		super(context);
	}


	@Override public void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] vars = visualState.getEntries().asDomainArray();
		out.println("<!-- Vector " + context + " -->");
		standardGrid(out);
		for(int i =1; i < vars.length; i++){
			for(int value:vars[i]){
			
				unitSquareSVG(out,posX(i),
						posY(value),domainBasedColor(vars[i]));
			}
			if (vars[i].isFixed()) {
				int v = vars[i].getIntValue();
				lineSVG(out,leftX(),posY(v)+0.5,
						leftX()+width(),posY(v)+0.5,
						Colors.ASSIGN_COLOR);
			}
		}
		drawFocus(out,visualState.getFocus());
		drawFailed(out,visualState.getFailed());
	}	

}
