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
 * Abstract class for a Chart Item display. The chart is on the x axis, the y axis is 
 * index based.
 * @author hsimonis
 *
 */
public abstract class VisualizerItemChart extends VisualizerDrawer {

	public VisualizerItemChart(VisualContext context){
		super(context);
	}

	@Override
	void draw(PrintWriter out, VisualState visualState) {
	}
		
	@Override public void standardGrid(PrintWriter out){
		// grid for rect

		gridSVG(out,leftX(),topY(),
				width(),height());
		// horizontal value labels
		for(int i=0;i<=width();i++){
			textSVG(out,posX(i),labelY()+0.8,0.5,
					i,Colors.LABEL_TEXT_COLOR);
		}
		// vertical item labels
		for(int i=1;i<=height();i++){
			textSVG(out,labelX(),posY(i),i,Colors.LABEL_TEXT_COLOR);
		}
	}

	public void drawEnd(PrintWriter out,FullDomain end){
		if (end != null){
			rectSVG(out,posX(end.getMin()),topY(),
					posX(end.getMax())-posX(end.getMin()),height(),
					Colors.TOO_HIGH_COLOR,0.2);
			lineSVG(out,posX(end.getMin()),topY(),
					posX(end.getMin()),topY()+height(),Colors.TOO_HIGH_COLOR);
			lineSVG(out,posX(end.getMax()),topY(),
					posX(end.getMax()),topY()+height(),Colors.TOO_HIGH_COLOR);
		}
	}
	@Override public double posX(double x){
		return context.getX()+1+x;
	}
	@Override public double posY(double y){
		return context.getY()+y;
	}
	@Override public int labelY() {
		return context.getY();
	}
	
}
