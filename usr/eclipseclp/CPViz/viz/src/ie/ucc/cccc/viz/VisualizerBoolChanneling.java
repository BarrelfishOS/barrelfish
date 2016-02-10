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
 * visualizer for the bool_channeling constraint. Three arguments, first a domain variable,
 * then a collection of boolean vars and third an integer
 * @author hsimonis
 *
 */
public class VisualizerBoolChanneling extends VisualizerDrawer {


	public VisualizerBoolChanneling(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		FullDomain var = visualState.argumentDomain("1");
		FullDomain[] bool = visualState.argumentDomainArray("2");
		int start = visualState.argumentInt("3");
		setWidth(bool.length-1);


		// grid for Var
		gridSVG(out,leftX(),topY(),width(),1);
		// grid for Bool
		gridSVG(out,leftX(),top2Y(),width(),1);
		// value labels for domain
		for(int i=1;i<=width();i++){
			textSVG(out,posX(i),labelY(),
					start+i-1,Colors.LABEL_TEXT_COLOR);			
		}
		// labels marking Var and Bool variables
		textSVG(out,labelX(),topY(),
				"X",Colors.LABEL_TEXT_COLOR);			
		textSVG(out,labelX(),top2Y(),
				"B",Colors.LABEL_TEXT_COLOR);			

		// draw Var domain
		for(int value : var){
			unitSquareSVG(out,posX(value),
					topY(),domainBasedColor(var));	
		}
		// draw Bool vector
		for(int i=1;i<=width();i++){
			unitSquareSVG(out,posX(i),
					top2Y(),booleanColor(bool[i]));	
		}
	}
	
	@Override public int top2Y() {
		return topY()+2;
	}
	
}

