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
import java.util.Map;

/**
 * Visualizer for the gcc_matrix constraint. Does nothing more than VisualizerMatrix
 * @author hsimonis
 *
 */
public class VisualizerGCCMatrix extends VisualizerMatrix {

	public VisualizerGCCMatrix(VisualContext context){
		super(context);
	}
	
	@Override
	public void draw(PrintWriter out, VisualState visualState) {
		FullDomainMap matrix = visualState.argument("3").asDomainMap();
		// debug help
		out.println("<!-- Matrix " + context + " -->");
		// draw grid
		standardGrid(out);
		// draw entries
		for(Map.Entry<Pair, FullDomain> entry : matrix.entrySet()){
			drawMatrixEntry(out,entry.getKey(),entry.getValue(),new FullDomain());
		}
	}

}

