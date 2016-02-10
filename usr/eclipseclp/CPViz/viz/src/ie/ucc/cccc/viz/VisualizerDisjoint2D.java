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
 * Simple Visualizer for a 2D non overallping constraint disjoint2
 * This does not exist in ECLiPSe, just checking the interface code
 * @author hsimonis
 *
 */
public class VisualizerDisjoint2D extends VisualizerLayout {

	public VisualizerDisjoint2D(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		Tuple[] rect = visualState.argumentTupleArray("1");
		FullDomain width = visualState.argumentDomain("2");
		FullDomain height = visualState.argumentDomain("3");
		
		int n = rect.length;
		setWidth(width.getMax());
		setHeight(height.getMax());
		setMin(0);
		
		standardGrid(out);
		// draw rectangles
		for(int i=1;i<n;i++) {
			int x1 = rect[i].getField("x").getMin();
			int y1 = rect[i].getField("y").getMin();
			int x2 = rect[i].getField("x").getMax();
			int y2 = rect[i].getField("y").getMax();
			int w1 = rect[i].getField("w").getMin();
			int h1 = rect[i].getField("h").getMin();
			if (x1 == x2 && y1 == y2) {
				rectSVG(out,posX(x1),posY(y1),
					w1,h1,Colors.ASSIGN_COLOR);
			} else {
				rectSVG(out,posX(x1),posY(y1),
						x2-x1+w1,(y2-y1+h1),Colors.UNASSIGNED_COLOR,0.3);
				
			}
		}
	}
	

}
