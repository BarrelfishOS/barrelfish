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
 * Simple utility class for pairs of int. Overrides equals and hashCode so that pairs 
 * can be used as Map keys
 * @author hsimonis
 *
 */
public class Pair {
	private int x;
	private int y;
	
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (! (o instanceof Pair)) return false;
		Pair mo = (Pair) o;
		return x == mo.getX() && y == mo.getY();
	}
	
	@Override public int hashCode() {
		int res = 17;
		res = 31*res+x;
		res = 31*res+y;
		return res;
	}
	
	@Override public String toString() {
		return x+" "+y;
	}
	public Pair(int x,int y) {
		this.x = x;
		this.y = y;
	}
	
	int getX() {
		return x;
	}	
	int getY() {
		return y;
	}
	int getMin() {
		return x;
	}	
	int getMax() {
		return y;
	}
}
