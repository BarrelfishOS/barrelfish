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
 * Visualizer for the gcc cosntraint with integer low high value parameters 
 * @author hsimonis
 *
 */
public class VisualizerGCC extends VisualizerDrawer {

	public VisualizerGCC(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		Tuple[] param = visualState.argumentTupleArray("1");
		FullDomain[] vars = visualState.argumentDomainArray("2");
		

		setWidth(vars.length-1);
		setHeight(param.length-1);
		standardGrid(out);
		// grid for counting
		gridSVG(out,left2X(),topY(),height(),width());
				
		for(int i=0;i<=width();i++){
			textSVG(out,left2X()+i,labelY()+0.8,0.5,
					i,Colors.LABEL_TEXT_COLOR);
		}
		// draw domains of vars
		for(int i = 1; i <=width(); i++){
			for(int value : vars[i]) {
				unitSquareSVG(out,posX(i),
						posY(value),domainBasedColor(vars[i]));
			}
		}
		// draw param limits
		for(int i=1;i<=height();i++) {
			int low = param[i].getField("low").getIntValue();
			int high = param[i].getField("high").getIntValue();
			int value = param[i].getField("value").getIntValue();
			// count possible assignments, not including fixed variables
			int possible = 0;
			// count fixed assignments
			int fixed = 0;
			for(int j = 1; j <= width(); j++){
				if (vars[j].isInDomain(value)) {
					if (vars[j].isFixed()) {
						fixed++;
					} else {
						possible++;
					}
				}
			}
			if (fixed == high){
				lineSVG(out,leftX(),posY(i)+0.5,
						leftX()+width(),posY(i)+0.5,Colors.ASSIGN_COLOR);
			}
			drawCount(out,left2X(),posY(value),width(),low,high,fixed,possible);
		}
	}
	

}
