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
 * Collection of VizParent objects. This is used to encode children of a VizParent
 * @author hsimonis
 *
 */

public class VizParentCollection extends ArrayList<VizParent> {

	/**
	 * required to avoid warning
	 */
	private static final long serialVersionUID = -970840735844030431L;

	/**
	 * used to extract a single domain variable; an assertion fails if the collection 
	 * does not contain a single element only
	 * @return FullDomain
	 */
	public FullDomain asDomain() {
		assert size() == 1;
		return get(0).getDomainAsList();
	}
	
	/**
	 * convert the collection into an array of domains
	 * We ignore the zeroth array element.	
	 * @return array of FullDomain
	 */
	public FullDomain[] asDomainArray() {
		FullDomain[] res = new FullDomain[size()+1];
		for(VizParent entry: this){
			res[entry.getSingleIndex()]= entry.getDomainAsList();
		}
		return res;
	}
	public FullDomainMap asDomainMap() {
		FullDomainMap res = new FullDomainMap();
		for(VizParent entry: this){
			res.put(entry.getXYIndex(), entry.getDomainAsList());
		}
		return res;
	}
	/**
	 * convert collection into an array of Tuples. 
	 * @return array of Tuple
	 */
	public Tuple[] asTupleArray() {
		Tuple[] res = new Tuple[size()+1];
		for(VizParent entry: this){
			res[entry.getSingleIndex()]= entry.asTuple();
		}
		return res;
	}
	/**
	 * convert the collection into an array of Pair holding min and max of domains
	 * @return array of Pair
	 */
	public Pair[] asMinMaxArray() {
		Pair[] res = new Pair[size()+1];
		for(VizParent entry: this){
			res[entry.getSingleIndex()] = new Pair(entry.getMinDomain(), entry.getMaxDomain());
		}
		return res;
	}
	/**
	 * convert collection into an array of integers
	 * @return array of int
	 */
	public int[] asIntArray() {
		int[] res = new int[size()+1];
		for(VizParent entry: this){
			res[entry.getSingleIndex()]= entry.getDomainAsList().get(0);
		}
		return res;
	}
	
}




