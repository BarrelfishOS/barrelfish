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
 * Data structure to keep the extent of a tree placement
 * each layer is a Pair node, the first element is the top of the tree
 * @author hsimonis
 *
 */
public class Extent extends LinkedList<DPair>{
	/**
	 * this seems to be required by eclipse, I don't know why
	 */
	private static final long serialVersionUID = -6864770301081628971L;
	/**
	 * all information is kept in a linked list
	 * we need the linked list as we add at the front and at the back of the list
	 */
	
	
	/**
	 * call the Constructor of the list to get an empty Extent
	 */
	public Extent() {
		super();
	}

	/**
	 * utility constructor to work with left and right values. creates a pair itself
	 * @param left
	 * @param right
	 */
	public Extent(double left, double right) {
		new Extent();
		add(new DPair(left,right));
	}
	
	public double getLeft(){
		return getFirst().getLeft();
	}
	
	public double getRight(){
		return getFirst().getRight();
	}
	
	/**
	 * more utility methods to add a node without exposing Pair
 	 * @param left
	 * @param right
	 */
	public void addFirst(double left, double right) {
		addFirst(new DPair(left,right));
	}
	
	/**
	 * utility to add implicit node at end of list
	 * @param left
	 * @param right
	 */
	protected void add(double left, double right) {
		add(new DPair(left,right));
	}
	
	/**
	 * add an extent to the given one. This moves the added extent to the right of the object
	 * and then combines the extents to cover the combined area
	 * This method modifies the extent that is being added, but returns a new, combined one
	 * @param added The extent to be added; this is modified by the code
	 * @return Extent a list of intervals (DPairs) describing the space needed by a tree
	 */
	public Extent addExtent(Extent added){
		double move = fit(added);
		added.shift(move);
		return merge(added); 
	}
	
	/**
	 * shift an extent to the right by move units
	 * modifies the object it self
	 * @param move double, amount of move to the right
	 */
	private void shift(double move) {
		Iterator<DPair> li =iterator();
		while (li.hasNext()){
			li.next().shift(move);
		}
	}
	
	/**
	 * Merge two extents
	 * at each level combine the left value of the left extend 
	 * and the right value of the right extent to build a new extent
	 * when either of the two extends reaches the end, copy the remaining
	 * entries of the other one
	 * @param added Extent, this is modified by shifting to the right
	 * @return Extent, new object created my merging
	 */
	private Extent merge(Extent added){
		Iterator<DPair> li1 = iterator();
		Iterator<DPair> li2 = added.iterator();
		Extent res = new Extent();
		while (li1.hasNext() && li2.hasNext()) {
			DPair p1=li1.next();
			DPair p2=li2.next();
			res.add(p1.getLeft(),p2.getRight());
		}
		// one of the iterators must be at the end of their list
		assert !li1.hasNext() || !li2.hasNext();
		// if li1 is not finished copy its elements
		while (li1.hasNext()) {
			res.add(li1.next());
		}
		// if li2 is not finished copy its arguments
		while (li2.hasNext()) {
			res.add(li2.next());
		}
		return res;
	}
	
	
	/**
	 * fit the added extent against the existing one. Find out how far the added
	 * extent must be moved to the right so that it does not overlap the existing one
	 * we assume separation of one unit. The code builds a collection of the distances 
	 * at each level and then takes the maximum. If either of the extents is empty, the 
	 * returned value is 0.0, i.e. no move required 
	 * @param added Extent, the new tree we try to fit against the existing forest
	 * @return double value, defines how far the added extend must be moved to the right
	 * to fit 
	 */
	private double fit(Extent added){
		Iterator<DPair> li1 = iterator();
		Iterator<DPair> li2 = added.iterator();
		Collection<Double> res = new ArrayList<Double>();
		while (li1.hasNext() && li2.hasNext()) {
			DPair p1=li1.next();
			DPair p2=li2.next();
			double dist1 = p1.getRight()-p2.getLeft()+1.0;
//			System.out.println(dist1);
			res.add(dist1);
		}
		if (res.isEmpty()) {
			return 0.0;
		} else {
			return Collections.max(res);
		}
	}
	

}
