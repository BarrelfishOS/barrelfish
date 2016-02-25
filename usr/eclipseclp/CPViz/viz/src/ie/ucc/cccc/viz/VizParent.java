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

import java.util.Scanner;


/**
 * Abstract class to define entries inside a visualizer. May contain a collection 
 * of other entries. This needs some cleanup, at the moment this whole hierarchy 
 * is not really useful, i.e. everything is done with the methods given here
 * @author hsimonis
 *
 */
public abstract class VizParent {
	private VizParentCollection entries;
	protected String index;
	
	public VizParent(){
		entries = new VizParentCollection();
	}
	
	/**
	 * all VizParent entries use an index
	 * @param index
	 */
	public VizParent(String index){
		entries = new VizParentCollection();
		this.index = index;
	}
	
	/**
	 * this should be used sparingly; instead use more detailed methods for specific 
	 * data structures
	 * @return String, the index as entered by the user; may be required for new visualizers
	 */
	public String getRawIndex(){
		return index;
	}
	/**
	 * get the index as a single integer
	 * @return int
	 */
	public int getSingleIndex() {
		return Integer.parseInt(index);
	}
	
	/**
	 * get the index as a pair of integers
	 * @return Pair x value is row, y value is column
	 */
	public Pair getXYIndex() {
		Scanner s = new Scanner(index);
		int x = s.nextInt();
		int y = s.nextInt();
		return new Pair(x,y);
	}
	
	/**
	 * extract a box from an index. This contains x,y,w,h integers and is used for special
	 * block focus types
	 * @return Box
	 */
	public Box getBoxIndex() {
		Scanner s = new Scanner(index);
		// read row first, i.e. y coor
		int y = s.nextInt();
		// then col, the x coor
		int x = s.nextInt();
		int w = s.nextInt();
		int h = s.nextInt();
		return new Box(x,y,w,h);
	}
	
	/**
	 * Each class must provide a method to get a domain as a list
	 * @return List of int
	 */
	abstract public FullDomain getDomainAsList();		
		
	/**
	 * utility to delegate to FullDomain method
	 * @return int
	 */
	public int getMinDomain() {
		return getDomainAsList().getMin();
	}
	/**
	 * utility to delegate to FullDomain method
	 * @return int
	 */
	public int getMaxDomain() {
		return getDomainAsList().getMax();
	}
	
	/**
	 * raw access to the collection, should be used sparingly
	 * @return collection of VizParent objects
	 */
	public VizParentCollection getEntries() {
		return entries;
	}
	
	/**
	 * get an argument by name
	 * @param name String
	 * @return collection of entries
	 */
	public VizParentCollection argument(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries();		
			}
		}
		return null;
	}
		
	/**
	 * access an argument as an array of integers
	 * @param name argument name
	 * @return array of int
	 */
	public int[] argumentIntArray(String name){
		return argument(name).asIntArray();
	}

	/**
	 * access an argument as an array of Pairs holding min and max domain elements
	 * @param name String
	 * @return array of Pair
	 */
	public Pair[] argumentMinMaxArray(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries().asMinMaxArray();		
			}
		}
		return null;
	}
	/**
	 * access an argument as an array of FullDomain objects
	 * @param name String
	 * @return array of domains
	 */
	public FullDomain[] argumentDomainArray(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries().asDomainArray();		
			}
		}
		return null;
	}
	
	/**
	 * access an argument as an array of tuples, each tuple holding some fields
	 * @param name String
	 * @return array of tuples
	 */
	public Tuple[] argumentTupleArray(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries().asTupleArray();		
			}
		}
		return null;
	}
	/**
	 * access an argument as a single domain
	 * @param name String
	 * @return single FullDomain object
	 */
	public FullDomain argumentDomain(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries().asDomain();		
			}
		}
		return null;
	}
	/**
	 * access an argument as a single integer
	 * @param name String
	 * @return int
	 */
	public int argumentInt(String name){
		for(VizParent argument: getEntries()){
			if (argument.getRawIndex().equals(name)) {
			 return argument.getEntries().asDomain().get(0);		
			}
		}
		// This should be unreachable ???
		return 0;
	}
		
	/**
	 * utility to add an entry to the child collection; used by parser
	 * @param entry VisParent object
	 */
	public void add(VizParent entry){
		entries.add(entry);
	}

	/**
	 * factory method to create a Tuple
	 * @return Tuple
	 */
	public Tuple asTuple() {
		Tuple res = new Tuple();
		for(VizParent entry: entries) {
//			System.out.println("Adding entry "+entry.getRawIndex()+
//					" "+entry.getDomainAsList().get(0));
			res.add(entry.getRawIndex(),entry.getDomainAsList());
		}
		return res;
	}
}
