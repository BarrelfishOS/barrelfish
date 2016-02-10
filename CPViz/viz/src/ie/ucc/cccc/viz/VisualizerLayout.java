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
 * Abstract class for a layout display, both axis are drawn from 0, with the coordinates
 * going up and to the right
 * To work completely, it needs to redefine the drawing primitives to handle the 
 * changed system, currently only done partially
 * @author hsimonis
 *
 */
public abstract class VisualizerLayout extends VisualizerDrawer {

	public VisualizerLayout(VisualContext context){
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
		// vertical value labels
		for(int i=0;i<=height();i++){
			textSVG(out,labelX()+0.5,posY(i)+0.2,0.5,
					i,Colors.LABEL_TEXT_COLOR);
		}
	}

	public void drawLimit(PrintWriter out,FullDomain limit){
		openRectSVG(out,leftX(),posY(limit.getMin()),width(),
				posY(limit.getMax())-posY(limit.getMin()),Colors.TOO_HIGH_COLOR,0.2);
		lineSVG(out,leftX(),posY(limit.getMin()),
				leftX()+width(),posY(limit.getMin()),Colors.TOO_HIGH_COLOR);
		lineSVG(out,leftX(),posY(limit.getMax()),
				leftX()+width(),posY(limit.getMax()),Colors.TOO_HIGH_COLOR);
		
	}
	public void drawEnd(PrintWriter out, FullDomain end){
		if (end != null){
			openRectSVG(out,posX(end.getMin()),topY()+height(),
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
		return context.getY()+1+height()-y;
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
	
	@Override public void openRectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color,double opacity) {
		super.openRectSVG(out, x, y-height, width, height, color,opacity);
	}
	@Override public void openRectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color) {
		super.openRectSVG(out, x, y-height, width, height, color);
	}
	@Override public void roundedRectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color,double opacity) {
		super.roundedRectSVG(out, x, y-height, width, height, color,opacity);
	}
	@Override public void roundedRectSVG(PrintWriter out,double x, double y, 
			double width, double height, Colors color) {
		super.roundedRectSVG(out, x, y-height, width, height, color);
	}
}
