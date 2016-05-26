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

import java.util.HashMap;
import java.util.Map;

public class ValueMap {
	Map<ValueKey,Integer> m;
	public ValueMap() {
		m = new HashMap<ValueKey,Integer>();
	}
	
	public void increment(String name,int value){
		ValueKey key = new ValueKey(name,value);
		if (m.containsKey(key)){
			m.put(key,m.get(key)+1);
		} else {
			m.put(key,1);
		}
		
	}
	
	public Map<ValueKey,Integer> getMap(){
		return m;
	}

}
