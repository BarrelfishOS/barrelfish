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
 * Visualizer for a vector of domain variables
 * @author hsimonis
 *
 */
public class VisualizerVector extends VisualizerDrawer {

	public VisualizerVector(VisualContext context) {
		super(context);
	}
	
	public void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] vars = visualState.getEntries().asDomainArray();
		FullDomain[] removed = visualState.getRemovedValues(vars);
		out.println("<!-- Vector " + context + " -->");
		standardGrid(out);
		for(int i =1; i < vars.length; i++){
			for(int value:vars[i]){
				unitSquareSVG(out,posX(i),
						posY(value),domainBasedColor(vars[i]));
			}
			for(int value:removed[i]){
				unitSquareSVG(out,posX(i),
						posY(value),Colors.REMOVED_VALUE_COLOR);
			}
		}
		drawFocus(out,visualState.getFocus());
		drawFailed(out,visualState.getFailed());
	}	

	protected void drawFocus(PrintWriter out,VizFocus focus) {
		if (isInFocus(focus)) {
			double x = posX(focus.getSingleIndex());
			double y = topY();
			hollowRectSVG(out,x,y,1,height(),Colors.FOCUS_COLOR);
		}
	}
	protected void drawFailed(PrintWriter out,VizFailed failed){
		if (isFailed(failed)){
			double x = posX(failed.getSingleIndex());
			double y = topY();
			out.println("<!-- " + failed +  " -->");
			hollowRectSVG(out,x,y,1,height(),Colors.FAILED_COLOR);
			unitSquareSVG(out,x,posY(failed.getValue()),Colors.FAILED_COLOR);
		}
	}


}

