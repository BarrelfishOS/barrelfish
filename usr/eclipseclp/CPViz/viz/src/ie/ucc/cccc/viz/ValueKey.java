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

public class ValueKey {
	String name;
	int value;

	public ValueKey(String name,int value){
		this.name = name;
		this.value = value;
	}

	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (! (o instanceof ValueKey)) return false;
		ValueKey mo = (ValueKey) o;
		return name.equals(mo.getName()) && value == mo.getValue();
	}
	
	@Override public int hashCode() {
		int res = 17;
		res = 31*res+name.hashCode();
		res = 31*res+value;
		return res;
	}

	public String getName(){
		return name;
	}
	
	public int getValue(){
		return value;
	}
}
