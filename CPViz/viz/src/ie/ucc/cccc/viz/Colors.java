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
/**
 * Utility class to hold the predefined color values as enum instances.
 * Uses a string attribute to hold the string itself. In the code the enum is used
 * throughout, when the enum is printed, it prints its string attribute value
 * @author hsimonis
 *
 */
public enum Colors {
	/**
	 * Tree color scheme
	 * color used for the text in unexplored nodes
	 */
	UNEXPLORED_NODE_TEXT_COLOR("#0000FF"),
	/**
	 * color used for completely explored nodes
	 */
	EXPLORED_NODE_TEXT_COLOR("#000000"),
	/**
	 * color used for small info text in sub tree abstraction
	 */
	NODE_INFO_TEXT_COLOR("#000000"),
	/**
	 * color used for the failure node circles
	 */
	FAILED_NODE_COLOR("#FF0000"),
	/**
	 * color used for the success node circles
	 */
	SUCC_NODE_COLOR("#00FF00"),
	/**
	 * color used for the links between nodes in the tree
	 */
	LAST_NODE_COLOR("#0000FF"),
	/**
	 * color used to mark unfinished part of tree
	 */
	LINK_COLOR("#000000"),
	/**
	 * color used to mark forced assignments in tree, if shown at all
	 */
	FIXED_LINK_COLOR("#FFFF00"),
	/**
	 * color used for the value text printed next to the links
	 */
	VALUE_TEXT_COLOR("#000000"),	
	/**
	 * overall color used for border of nodes
	 */
	NODE_BORDER_COLOR("#000000"),
	
	/**
	 * Visualizer color scheme
	 * color used for border of cells
	 */
	BORDER_COLOR("#000000"),
	/**
	 * color used for lines in grid
	 */
	GRID_COLOR("#000000"),
	/**
	 * color used for zero value in binary domains
	 */
	ZERO_COLOR("#FFFFFF"),
	OLD_ZERO_COLOR("#C8C8C8"),
	/**
	 * color used for value one in binary domains
	 */
	ONE_COLOR("#000000"),
	OLD_ONE_COLOR("#505050"),
	/**
	 * color used to show allowed range of values
	 */
	ALLOWED_COLOR("#00FF00"),
	/**
	 * color used to show not allowed range of values
	 */
	NOT_ALLOWED_COLOR("#FFFF00"),
	/**
	 * color used to show value is too low
	 */
	TOO_LOW_COLOR("#808000"),
	/**
	 * color used to show value is too high
	 */
	TOO_HIGH_COLOR("#800000"),
	/**
	 * color used to show capacity is used up
	 */
	FULL_COLOR("#000080"),
	/**
	 * color used to show removed values
	 */
	REMOVED_VALUE_COLOR("#008080"),
	/**
	 * Color used to mark removed values in domains
	 */
	REMOVED_VALUE_TEXT_COLOR("#FF0000"),
	/**
	 * color used to show value is too high
	 */
	FIXED_COLOR("#C0C0C0"),
	/**
	 * color used to show value is too high
	 */
	POSSIBLE_COLOR("#0000C0"),
	/**
	 * color used for text labels
	 */
	LABEL_TEXT_COLOR("#0000FF"),
	/**
	 * color used for cells marking fixed assignment
	 */
	FIXED_ASSIGNMENT_COLOR("#FFE0E0"),
	/**
	 * color used for failed assignment markers
	 */
	FAILED_COLOR("#FF0000"),
	/**
	 * color used for text in failed assignment cells
	 */
	FAILED_TEXT_COLOR("#0000FF"),
	/**
	 * color used to mark unknown visualizers
	 */
	UNKNOWN_COLOR("#808000"),
	/**
	 * color used to mark constant values
	 */
	CONSTANT_COLOR("#000080"),
	/**
	 * color used to draw items on top of each other (0 < opacity < 1)
	 */
	COMPARE1_COLOR("#00C000"),
	/**
	 * color used to draw items on top of each other (0 < opacity < 1)
	 */
	COMPARE2_COLOR("#0000C0"),
	/**
	 * color used to mark assigned values
	 */
	ASSIGN_COLOR("#FF0000"),
	OLD_ASSIGN_COLOR("#FF8080"),
	/**
	 * color used to mark text in assigned cells
	 */
	ASSIGNED_TEXT_COLOR("#0000FF"),
	/**
	 * color used to mark text in unassigned cells
	 */
	UNASSIGNED_TEXT_COLOR("#000000"),
	/**
	 * color used for cells which are not assigned
	 */
	UNASSIGNED_COLOR("#C0FFC0"),
	/**
	 * color used by invariant checker to mark missing propagation
	 */
	MISSING_PROPAGATION_COLOR("#0000FF"),
	/**
	 * color used by invariant checker to mark inconsistent partial assignment
	 */
	INCONSISTENT_COLOR("#FFFF00"),
	/**
	 * color used by invariant checker to mark inconsistent ground assignment
	 */
	FALSE_COLOR("#FF0000"),
	/**
	 * color used by invariant checker to mark an interesting state
	 */
	INTERESTING_COLOR("#C0C0C0"),
	/**
	 * colors used in the softprec constraint
	 */
	SOFTPREC_COLOR("#0000FF"),
	SOFTPREC_OLD_COLOR("#00C0C0"),
	HARDPREC_UNKNOWN_COLOR("#C0FFC0"),
	HARDPREC_ZERO_COLOR("#FFFFFF"),
	HARDPREC_OLD_ZERO_COLOR("#C8C8C8"),
	HARDPREC_ONE_COLOR("#000000"),
	HARDPREC_OLD_ONE_COLOR("#505050"),
	COMPONENT_COLOR("#808080"),
	COMPONENT_COLOR0("#FF0000"),
	COMPONENT_COLOR1("#00FF00"),
	COMPONENT_COLOR2("#0000FF"),
	COMPONENT_COLOR3("#FFFF00"),
	COMPONENT_COLOR4("#FF00FF"),
	COMPONENT_COLOR5("#00FFFF"),
	/**
	 * color used for focus marker
	 */
	UNCHANGED_COLOR("#C0C0C0"),
	CHANGED_MINMAX_COLOR("#00FFFF"),
	CHANGED_MIN_COLOR("#0000FF"),
	CHANGED_MAX_COLOR("#00FF00"),
	CHANGED_SIZE_COLOR("#008080"),
	FOCUS_COLOR("#FFFF00"),
	WHITE_COLOR("#FFFFFF"),
	/**
	 * color used for focus marker of blocks instead of single variables
	 */
	BLOCK_FOCUS_COLOR("#0000FF");
		
	private String string;

	Colors(String string){
		this.string = string;
	}

	/**
	 * used to print out enums as string values
	 */
	@Override public String toString() {
		return this.string;
	}

}
