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
 * This enum describes the different results of the invariant checks for the visualizers.
 * @author hsimonis
 *
 */
public enum InvariantType {
	/**
	 * the invariant checker succeeded
	 */
	TRUE,
	/**
	 * the invariant checker detects something interesting
	 */
	INTERESTING,
	/**
	 * the invariant checker detected some missing propagation
	 */
	MISSING_PROPAGATION,
	/**
	 * a partial assignment is inconsistent
	 */
	INCONSISTENT,
	/**
	 * the ground instance should not be satisfied 
	 */
	FALSE;
	
	/**
	 * Compute the maximum severity of the current and a new violation. This is simple 
	 * stratified by level,
	 * no clever attempts to maximize performance
	 * @param update InvariantType
	 * @return InvariantType the stronger of the current and the new type
	 */
	InvariantType update(InvariantType update){
		if (this == FALSE || update == FALSE) {
			return FALSE;
		} else if (this == INCONSISTENT || update == INCONSISTENT) {
			return INCONSISTENT; 
		} else if (this == MISSING_PROPAGATION || update == MISSING_PROPAGATION) {
			return MISSING_PROPAGATION;
		} else if (this == INTERESTING || update == INTERESTING) {
			return INTERESTING;
		} else {
			return TRUE;
		}
	}
}
