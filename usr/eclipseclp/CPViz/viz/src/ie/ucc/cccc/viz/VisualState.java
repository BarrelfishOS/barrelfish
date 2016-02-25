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
import java.io.*;
import java.util.Map;




/**
 * Holds the information about the state of a visualizer at a particular 
 * point of execution
 * @author hsimonis
 *
 */
public class VisualState extends VizParent {
	private VisualState parent=null;
	private Visualizer visualizer;
	private State state;
	private VizFocus focus;
	private VizFailed failed;
	
	public VisualState(Visualizer visualizer,State state){
		assert visualizer != null;
		assert state != null;
		this.visualizer = visualizer;
		this.state = state;
		for(VisualState old : state.getParent().getVisualStates()){
			// this is slow, and should be changed
			if (old.getVisualizer() == visualizer){
				parent = old;
			}
		}
//		if (parent == null){
//			System.out.println("No VisualState parent");
//		}
	}
	
	public FullDomain getDomainAsList(){
		return new FullDomain();
	}
	
	public Visualizer getVisualizer() {
		return visualizer;
	}
	
	public State getState() {
		return state;
	}
	
	public VisualState getParent() {
		return parent;
	}
	
	/**
	 * find the corresponding entries in the parent for a FullDomain vector
	 * and extract which values have been removed
	 * @param vars the array of new values
	 * @return an array of removed values, which were in the parents entry, 
	 * but which are no longer there
	 */
	public FullDomain[] getRemovedValues(FullDomain[] vars){
		FullDomain[] res = new FullDomain[vars.length];
		if (getParent() == null){
			for(int i=0;i<vars.length;i++){
				res[i] = new FullDomain();
			}
		} else {
			FullDomain[] old = getParent().getEntries().asDomainArray();
			for(int i=0;i<vars.length;i++){
				// this is hacked to handle different indexStart values
				if (vars[i]==null){
					res[i] = new FullDomain();
				} else {
					res[i] = old[i].getRemovedValues(vars[i]);
				}
			}
		}
		return res;
	}
	
	/**
	 * find the corresponding entries in the parent for a FullDomain vector
	 * and extract which values have been removed
	 * @param argument String, the argument name
	 * @param vars the array of current values in the domain
	 * @return an array of domains which hold the removed values
	 */
	public FullDomain[] getRemovedValues(String argument,FullDomain[] vars){
		FullDomain[] res = new FullDomain[vars.length];
		if (getParent() == null){
			// no parent, this happens in the root node
			for(int i=0;i<vars.length;i++){
				res[i] = new FullDomain();
			}
		} else {
			FullDomain[] old = getParent().argumentDomainArray(argument);
			for(int i=0;i<vars.length;i++){
				// this is hacked to handle different indexStart values
				if (vars[i]== null){
					res[i] = new FullDomain();
				} else {
					res[i] = old[i].getRemovedValues(vars[i]);
				}
			}
		}
		return res;
	}
	
	public FullDomain getRemovedValues(String argument,FullDomain var){
		FullDomain res;
		if (getParent() == null){
			// no parent, this happens in the root node
				res = new FullDomain();
		} else {
			FullDomain old = getParent().argumentDomain(argument);
			res = old.getRemovedValues(var);
		}
		return res;
	}
	
	public FullDomainMap getRemovedValues(FullDomainMap vars){
		FullDomainMap res = new FullDomainMap();
		if (getParent() == null){
		} else {
			FullDomainMap old = getParent().getEntries().asDomainMap();
			for(Map.Entry<Pair, FullDomain> entry : old.entrySet()){
//				System.out.println("Entry "+entry+ " old "+ old);
				res.put(entry.getKey(),
						entry.getValue().getRemovedValues(vars.get(entry.getKey())));
			}
		}
		return res;
	}
	
	public FullDomainMap getRemovedValues(String attribute,FullDomainMap vars){
		FullDomainMap res = new FullDomainMap();
		if (getParent() == null){
//			System.out.println("Null parent");
		} else {
			FullDomainMap old = getParent().argument(attribute).asDomainMap();
			for(Map.Entry<Pair, FullDomain> entry : old.entrySet()){
//				System.out.println("Entry "+entry+ " old "+ old);
				res.put(entry.getKey(),
						entry.getValue().getRemovedValues(vars.get(entry.getKey())));
			}
		}
		return res;
	}
	
	
	public VizFocus getFocus() {
		return focus;
	}
	
	public VizFailed getFailed() {
		return failed;
	}
	
	public void focus(VizFocus entry){
		focus = entry;
	}
	public void failed(VizFailed entry){
		failed = entry;
	}
	
	
	public void draw(PrintWriter out) {
		visualizer.draw(out,this);
	}	
	public void drawBox(PrintWriter out,Colors color) {
		visualizer.drawBox(out,this,color);
	}	
	public InvariantType invariant(PrintWriter out) {
		return visualizer.invariant(out,this);
	}
}
