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
 * Visualizer to show an element constraint
 * @author hsimonis
 *
 */
public class VisualizerElement extends VisualizerDrawer {

	public VisualizerElement(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		FullDomain var = visualState.argumentDomain("1");
		int[] vars2 = visualState.argumentIntArray("2");
		FullDomain cost = visualState.argumentDomain("3");
		int n = vars2.length;
		setMin(min(vars2));
		setMax(max(vars2));
		setWidth(n-1);
		setHeight(max()-min()+1);

		standardGrid(out);
		// grid for X
		gridSVG(out,leftX(),top2Y(),width(),1);
		// grid for C
		gridSVG(out,left2X(),topY(),1,height());
		// labels marking X and C variables
		textSVG(out,labelX(),top2Y(),
				"X",Colors.LABEL_TEXT_COLOR);			
		textSVG(out,left2X(),labelY(),
				"C",Colors.LABEL_TEXT_COLOR);			
		// draw cost list entries
		for(int i=1;i<n;i++){
			unitSquareSVG(out,posX(i),posY(vars2[i]),Colors.CONSTANT_COLOR);
		}
		// draw X domain
		for(int value : var){
			unitSquareSVG(out,posX(value),top2Y(),domainBasedColor(var));	
		}
		// draw cost domain
		for(int value : cost){
			unitSquareSVG(out,left2X(),posY(value),domainBasedColor(cost));	
		}
	}
	
	private int min(int[] x){
		int res=x[1];
		for(int i=2;i<x.length;i++) {
			res = Math.min(res,x[i]);
		}
		return res;
	}
	
	private int max(int[] x){
		int res=x[1];
		for(int i=2;i<x.length;i++) {
			res = Math.max(res,x[i]);
		}
		return res;
	}
}

