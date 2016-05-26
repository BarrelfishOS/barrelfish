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

public class VisualizerVectorWaterfall extends VisualizerDrawer {

	public VisualizerVectorWaterfall(VisualContext context) {
		super(context);
	}
	
	public void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] vars = visualState.getEntries().asDomainArray();
		standardGrid(out);
		int line = 1;
		while (visualState.getParent()!= null){
			VisualState old = visualState.getParent();
			FullDomain[] oldVars = old.getEntries().asDomainArray();
			compare(out,line,vars,oldVars);
			vars = oldVars;
			visualState = old;
			line++;
		}
	}	

	public void compare(PrintWriter out, int line,FullDomain[] vars,FullDomain[] old){
		for(int i =1; i < vars.length; i++){
			Colors color;
			if (old[i].isFixed()){
				// previously fixed
				color = Colors.OLD_ASSIGN_COLOR;
			} else if (vars[i].isFixed()){
				// newly fixed
				color = Colors.ASSIGN_COLOR;
			} else if (vars[i].size() == old[i].size()){
				// unchanged
				color = Colors.UNCHANGED_COLOR;
			} else{
				int varsMin = vars[i].getMin();
				int varsMax = vars[i].getMax();
				int oldMin = old[i].getMin();
				int oldMax = old[i].getMax();
				assert varsMin >= oldMin;
				assert varsMax <= oldMax;
				if (varsMin > oldMin && varsMax < oldMax){
					// updated min max
					color = Colors.CHANGED_MINMAX_COLOR;					
				} else if (varsMin > oldMin){
					// updated min
					color = Colors.CHANGED_MIN_COLOR;
				} else if (varsMax < oldMax) {
					// updated max
					color = Colors.CHANGED_MAX_COLOR;
				} else {
					// updated size only
					color = Colors.CHANGED_SIZE_COLOR;
				}				
			}
			unitSquareSVG(out,posX(i),posY(line),color);
		}
	}

}
