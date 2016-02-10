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

public abstract class VisualizerGraph extends VisualizerDrawer {

	public VisualizerGraph(VisualContext context){
		super(context);
	}

	@Override
	void draw(PrintWriter out, VisualState visualState) {
	}
		
	@Override public void standardGrid(PrintWriter out){
		// grid for rect

		rectSVG(out,leftX(),labelY(),
				width(),height(),Colors.WHITE_COLOR);
		// horizontal value labels
		for(int i=0;i<=width();i++){
			textSVG(out,posX(i),labelY()+0.8,0.5,
					i,Colors.LABEL_TEXT_COLOR);
		}
		// vertical value labels
//		for(int i=0;i<=height();i++){
//			textSVG(out,labelX()+0.5,posY(i)+0.2,0.5,
//					i,Colors.LABEL_TEXT_COLOR);
//		}
	}


	@Override public double posX(double x){
		return context.getX()+1+x;
	}
	@Override public double posY(double y){
		return context.getY()+1+height()-height()*y/max();
	}
	@Override public int labelY() {
		return context.getY()+height()+1;
	}
	
	@Override public void rectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color) {
		super.rectSVG(out, x, y-height, width, height, color);
	}
	
	@Override public void rectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color,double opacity) {
		super.rectSVG(out, x, y-height, width, height, color,opacity);
	}	
	

}
