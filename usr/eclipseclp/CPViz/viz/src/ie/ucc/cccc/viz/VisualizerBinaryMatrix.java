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
 * A visualizer for a matrix of 0/1 variables derived from VisualizerMatrix
 * @author hsimonis
 *
 */
public class VisualizerBinaryMatrix extends VisualizerMatrix {

	public VisualizerBinaryMatrix(VisualContext context) {
		super(context);
	}
		
	/**
	 * the drawing method displays 0/1 values by dedicated colors
	 */
	protected void drawMatrixEntry(PrintWriter out,Pair xy,
			FullDomain list,FullDomain removed){
		out.println("<!-- index "+xy.getX() + " "+ xy.getY() +" domain "+list+" -->");
		unitSquareSVG(out,posX(xy),posY(xy),booleanColor(list,removed));
		if (list.isFixed()){
			textSVG(out,posX(xy),posY(xy),
					list.getIntValue(),Colors.ASSIGNED_TEXT_COLOR);
		}
	}
	

}
