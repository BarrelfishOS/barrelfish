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
 * Class to hold failure description for visualizers.
 * @author hsimonis
 *
 */
public class VizFailed extends VizEntry {
	private String group;
	private int value;
	
	@Override public String toString() {
		return "Failed Index "+index+" Group "+group+" Value "+value;
	}
	
	/**
	 * hold failure location information for a visualizer
	 * @param index String, holds integer or list of integers
	 * @param group String
	 * @param value int, value which was assigned and led to failure
	 */
	public VizFailed(String index,String group,int value){
		this.index = index;
		this.group = group;
		this.value = value;
	}
	
	public String getGroup() {
		return group;
	}
	
	public int getValue(){
		return value;
	}

	public FullDomain getDomainAsList(){
		return new FullDomain();
	}
}
