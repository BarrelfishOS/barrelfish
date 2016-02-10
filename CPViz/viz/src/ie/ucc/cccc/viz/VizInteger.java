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
 * Holds information about integer values in visualizers
 * @author hsimonis
 *
 */
public class VizInteger extends VizEntry {
	private int value;
	
	@Override public String toString() {
		return "IntVar "+index+" value "+value;
	}
	
	/**
	 * Create a placeholder for an integer in a visualizer
	 * @param index String, encodes the position in the visualizer
	 * @param value int, the value of the integer variable
	 */
	public VizInteger(String index,int value){
		this.index = index;
		this.value = value;
	}
	
	/**
	 * create a list of one element as a domain
	 */
	public FullDomain getDomainAsList(){
		FullDomain list = new FullDomain();
		list.add(value);
		return list;
	}
}
