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
 * Holds focus information for a visualizer. 
 * This describes which element is currently being assigned
 * @author hsimonis
 *
 */
public class VizFocus extends VizEntry {
	private String group;
	private String type;

	@Override public String toString() {
		return "Focus Index "+index+" Group "+group+" Type "+type;
	}

	/**
	 * hold focus information for a visualizer
	 * @param index String, encodes integer or list of integers
	 * @param group String
	 * @param type String, may be null, describes the focus to use
	 */
	public VizFocus(String index,String group,String type){
		this.index = index;
		this.group = group;
		this.type = type;
	}
	
	public String getGroup() {
		return group;
	}
	public String getType() {
		return type;
	}
	
	public FullDomain getDomainAsList(){
		return new FullDomain();
	}
}
