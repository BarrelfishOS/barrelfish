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

import java.io.*;

/**
 * This is a dispatcher for VisualizerDrawer instances. It create 
 * the context to store parameters of the visualizers, and then create the 
 * Drawer subclass instances
 * @author hsimonis
 *
 */
public class Visualizer {
	private VisualizerDrawer visualizerDrawer;
	
	/**
	 * This constructor must be extended when adding new visualizers. 
	 * This should be considered for replacement when sorting out extension of 
	 * the XML format.
	 * @param id
	 * @param type
	 * @param display
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @param group
	 * @param min
	 * @param max
	 */
	public Visualizer(int id,String type,String display,
			int x, int y, int width, int height, 
			String group,
			int min,int max,int indexStart) {
		VisualContext context = new VisualContext(id,type,display,
			x, y,width,height, 
			group,
			min,max,indexStart);
		if (type.equals("vector")) {
			visualizerDrawer = new VisualizerVector(context);
		} else if (type.equals("vector_waterfall")) {
			visualizerDrawer = new VisualizerVectorWaterfall(context);
		} else if (type.equals("vector_size")) {
			visualizerDrawer = new VisualizerVectorSize(context);
		} else if (type.equals("binary_vector")) {
			visualizerDrawer = new VisualizerBinaryVector(context);
		} else if (type.equals("domain_matrix")) {
			visualizerDrawer = new VisualizerMatrix(context);
		} else if (type.equals("binary_matrix")) {
			visualizerDrawer = new VisualizerBinaryMatrix(context);
		} else if (type.equals("alldifferent")) {
			visualizerDrawer = new VisualizerAllDifferent(context);
		} else if (type.equals("bin_packing")) {
			visualizerDrawer = new VisualizerBinPacking(context);
		} else if (type.equals("lex_le")) {
			visualizerDrawer = new VisualizerLexLe(context);
		} else if (type.equals("lex_lt")) {
			visualizerDrawer = new VisualizerLexLe(context);
		} else if (type.equals("inverse")) {
			visualizerDrawer = new VisualizerInverse(context);
		} else if (type.equals("element")) {
			visualizerDrawer = new VisualizerElement(context);
		} else if (type.equals("bool_channeling")) {
			visualizerDrawer = new VisualizerBoolChanneling(context);
		} else if (type.equals("gcc")) {
			visualizerDrawer = new VisualizerGCC(context);
		} else if (type.equals("disjoint2")) {
			visualizerDrawer = new VisualizerDisjoint2D(context);
		} else if (type.equals("alldifferent_matrix")) {
			visualizerDrawer = new VisualizerAllDifferentMatrix(context);
		} else if (type.equals("gcc_matrix")) {
			visualizerDrawer = new VisualizerGCCMatrix(context);
		} else if (type.equals("same")) {
			visualizerDrawer = new VisualizerSame(context);
			/**
			 *  We can handle display display variant by calling a different
			 *  constructor, as for cumulative here, or by handling it inside the 
			 *  visualizer code itself, like for VisualizerMatrix
			 */		
		} else if (type.equals("cumulative") && display.equals("gantt")) {
			visualizerDrawer = new VisualizerCumulativeGantt(context);
		} else if (type.equals("cumulative")) {
			visualizerDrawer = new VisualizerCumulative(context);
		} else if (type.equals("cumulative_cost")) {
			visualizerDrawer = new VisualizerCumulativeCost(context);
		} else if (type.equals("sequence_total")) {
			visualizerDrawer = new VisualizerSequenceTotal(context);
		} else if (type.equals("soft_prec")) {
			visualizerDrawer = new VisualizerSoftPrec(context);
		} else {
			System.out.println("Do not know this visualizer: "+type);
			visualizerDrawer = new VisualizerUnknown(context,type);
			
		}
		
	}
	
	/**
	 * Utility method to get the enclosing box when drawing a state
	 * @return Box
	 */
	public Box getBox() {
		return visualizerDrawer.getBox();
	}
	
	/**
	 * Draw the visualizer for a given VisualizerState.
	 * @param out file descriptor for SVG output
	 * @param visualState
	 */
	public void draw(PrintWriter out, VisualState visualState) {
		visualizerDrawer.draw(out,visualState);
	}
	public void drawBox(PrintWriter out, VisualState visualState,Colors color) {
		visualizerDrawer.drawBox(out,visualState,color);
	}
	public InvariantType invariant(PrintWriter out, VisualState visualState) {
		return visualizerDrawer.invariant(out,visualState);
	}
}
