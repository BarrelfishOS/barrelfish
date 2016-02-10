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
 * Visualizer for the inverse constraint.
 * 
 * @author hsimonis
 *
 */
public class VisualizerInverse extends VisualizerDrawer {

	public VisualizerInverse(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] vars1 = visualState.argumentDomainArray("1");
		FullDomain[] vars2 = visualState.argumentDomainArray("2");
		
		setWidth(vars1.length-1);
		assert width() == vars2.length-1;
		assert context.getMin() == 1;
		
		standardGrid(out);
		for(int i = 1; i <=width(); i++){
			for(int value : vars1[i]) {
				unitSquareSVG(out,posX(i),
						posY(value),Colors.COMPARE1_COLOR,0.5);
			}
			for(int value : vars2[i]) {
				unitSquareSVG(out,posX(value),
						posY(i),Colors.COMPARE2_COLOR,0.5);
			}
		}
	}

}
