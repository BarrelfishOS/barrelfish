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
 * A specialized visualizer for a vector of 0/1 variables. This is not derived from 
 * the VisualizerVector, as it only uses one box per variable
 *  
 * @author hsimonis
 *
 */
public class VisualizerBinaryVector extends VisualizerDrawer {

	public VisualizerBinaryVector(VisualContext context) {
		super(context);
	}
	
	public void draw(PrintWriter out,VisualState visualState) {
		FullDomain[] vars = visualState.getEntries().asDomainArray();
		FullDomain[] removed = visualState.getRemovedValues(vars);
		int n = vars.length;
		out.println("<!-- Binary Vector "+context+" -->");
		standardGrid(out);
		for(int i=1;i < n; i++) {
			unitSquareSVG(out,posX(i),topY(),booleanColor(vars[i],removed[i]));		
		}
		drawFocus(out,visualState.getFocus());
		drawFailed(out,visualState.getFailed());
	}
	
	private void drawFocus(PrintWriter out,VizFocus focus) {
		if (isInFocus(focus)) {
			double x = posX(focus.getSingleIndex());
			double y = topY();
			out.println("<!-- "+ focus + " -->");
			hollowRectSVG(out,x,y,1,1,Colors.FOCUS_COLOR);
		}
	}
	
	private void drawFailed(PrintWriter out,VizFailed failed){
		if (isFailed(failed)) {
			double x = posX(failed.getSingleIndex());
			double y = topY();
			out.println("<!-- Failed "+failed + " -->");
			hollowRectSVG(out,x,y,1,1,Colors.FAILED_COLOR);
			unitSquareSVG(out,x,y,booleanColor(failed.getValue()));
		}
	}
	
	/**
	 * specialized version for this type of visualizer
	 */
	@Override public void standardGrid(PrintWriter out){
		gridSVG(out,leftX(),topY(),
				width(),1);
		// index labels
		for(int i = 1; i<= width(); i++){
			textSVG(out,posX(i),labelY(),i,Colors.LABEL_TEXT_COLOR);
		}
		// 0/1 label
		textSVG(out,labelX(),topY(),"0/1",Colors.LABEL_TEXT_COLOR);

	}
	
	/**
	 * specialized version of top2Y() for this layout
	 */
	@Override public int top2Y() {
		return topY()+2;
	}
}

