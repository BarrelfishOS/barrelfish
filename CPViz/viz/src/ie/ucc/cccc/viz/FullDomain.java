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

import java.util.*;

/**
 * A class to represent a full domain as a collection of all its elements. 
 * @author hsimonis
 *
 */

public class FullDomain extends ArrayList<Integer> {

	/**
	 * This seems required to make eclipse shut up 
	 */
	private static final long serialVersionUID = 4229969495363870266L;

	/**
	 * get the minimal value in a domain
	 * @return int
	 */
	public int getMin(){
		Iterator<Integer> li = iterator();
		int res = li.next();
		while (li.hasNext()) {
			res = Math.min(res,li.next());
		}
		return res;	
	}
	/**
	 * get the maximal value in a domain
	 * @return int
	 */
	public int getMax(){
		Iterator<Integer> li = iterator();
		int res = li.next();
		while (li.hasNext()) {
			res = Math.max(res,li.next());
		}
		return res;	
	}
	/**
	 * get the unique value in the domain; assumes that the domain has 
	 * only one element, otherwise an assertion fails
	 * @return int
	 */
	public int getIntValue() {
		assert size() == 1;
		return get(0);
	}
	
	/**
	 * check if a value v is in the domain
	 * @param v int
	 * @return boolean true if the value is in the domain
	 */
	public boolean isInDomain(int v){
		return contains(v);
	}
	
	/**
	 * check if a domain is a singleton, i.e. contains only one value
	 * @return boolean, true if domain has only one value
	 */
	public boolean isFixed() {
		return size() == 1;
	}
	
	public FullDomain getRemovedValues(FullDomain current){
		FullDomain removed = new FullDomain();
		if (current != null) {
		for(int value: this){
			if (!current.isInDomain(value)){
				removed.add(value);
			}
		}
		}
//		System.out.println("Removed: "+removed);
		return removed;
	}
}
