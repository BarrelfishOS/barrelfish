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
import java.util.*;

/**
 * Class to describes a state in the visualization
 * @author hsimonis
 * 
 */
public class State {
	private State parent;
	private int id,treeNode;
	private List<VisualState> visualStates;
	
	/**
	 * create a state linked to a tree node
	 * @param id
	 * @param treeNode
	 */
	public State(int id, int treeNode,State parent) {
		this.id = id;
		this.treeNode = treeNode;
		this.parent = parent;
		this.visualStates = new ArrayList<VisualState>();
	}
	
	public int getId() {
		return id;
	}
	public int getTreeNode() {
		return treeNode;
	}
	public List<VisualState> getVisualStates() {
		return visualStates;
	}
	
	public State getParent(){
		return parent;
	}
	
	/**
	 * add a VisualizerState to the state for later display
	 * @param visualState
	 */
	public void addVisualizerState(VisualState visualState){
		visualStates.add(visualState);
	}

	/**
	 * Draw the State for a specific Tool; iterate over all VisualizerStates 
	 * to draw them in turn
	 * @param tool Tool, description of tool settings to decide how things are displayed
	 * @param box Box, the enclosing space needed
	 */
	public void draw(Tool tool,Box box) {
//		System.out.println("Draw state "+ id + " treeNode "+treeNode);
		for (VisualState visualState : visualStates) {
			box.expandBox(visualState.getVisualizer().getBox());
		}
		PrintWriter out= box.svgPrefix(tool,tool.getFileroot()+id+".svg");
		for (VisualState visualState : visualStates) {
			visualState.draw(out);
		}
		InvariantType overall = InvariantType.TRUE; 
		for (VisualState visualState : visualStates) {
			InvariantType res1 = visualState.invariant(out);
			overall=overall.update(res1);
			switch (res1) {
			case TRUE: 
				break;
			case MISSING_PROPAGATION: 
				visualState.drawBox(out,Colors.MISSING_PROPAGATION_COLOR);
				break;
			case INCONSISTENT:
				visualState.drawBox(out,Colors.INCONSISTENT_COLOR);
				break;
			case INTERESTING: 
				visualState.drawBox(out,Colors.INTERESTING_COLOR);
				break;
			case FALSE: 
				visualState.drawBox(out,Colors.FALSE_COLOR);
				break;
			default: 
				System.out.println("UNKNOWN invariant type");
			}
		}
		if (overall != InvariantType.TRUE){
			Viz.markTreeNode(overall,getTreeNode());
//			System.out.println("Invariant Trigger "+overall);
		}
		box.svgPostfix(out);
	}


}
