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
 * Generic node class for building the Tree structure
 * @author hsimonis
 *
 */
public class Node {
	private NodeType type;
	private int id;
	private int parent;
	private String name="";
	private int size;
	private int value=0;
	private String choice="";
	private List<Node> children = new ArrayList<Node>();
	private boolean allFailedBelow;
	private int totalDecendent;
	private int totalFailures;
	private int lastDecendent;
	private InvariantType invariantType=InvariantType.TRUE;
	
		@Override public String toString() {
			return "(Node id "+id+" p "+parent+" s "+size+" v " +value+" choice "+choice +")";
		}

		/**
		 * Create a node for a root node in the search tree
		 * @param type
		 * @param id
		 */
		public Node(NodeType type,int id) {
			this.type = type;
			this.id = id;
			this.parent= -1;
		}

		/**
		 * create a node for a fail node in the search tree
		 * 
		 * @param type
		 * @param id
		 * @param parent
		 */
		public Node(NodeType type,int id,int parent) {
			this.type = type;
			this.id = id;
			this.parent= parent;
		}

		/**
		 * Create a node for a try node in the search tree
		 * @param type
		 * @param id
		 * @param parent
		 * @param name
		 * @param size
		 * @param value
		 */
		public Node(NodeType type,int id,int parent,String name,int size,int value) {
			this.type = type;
			this.id = id;
			this.parent = parent;
			this.name = name;
			this.size=size;
			this.value =value;
		}
		/**
		 * Create a node for a tryc or a failc node in the sarch tree
		 * @param type
		 * @param id
		 * @param parent
		 * @param name
		 * @param size
		 * @param choice
		 */
		public Node(NodeType type,int id,int parent,String name,int size,String choice) {
			this.type = type;
			this.id = id;
			this.parent = parent;
			this.name = name;
			this.size=size;
			this.choice =choice;
		}

		public NodeType getType() {
			return type;
		}
		public int getId() {
			return id;
		}
		public int getParent() {
			return parent;
		}
		public String getName() {
			return name;
		}
		public int getSize() {
			return size;
		}
		public int getValue() {
			return value;
		}
		public String getChoice() {
			return choice;
		}
		public List<Node> getChildren() {
			return children;
		}
		public boolean isAllFailedBelow() {
			return allFailedBelow;
		}

		public int getTotalDecendent() {
			return totalDecendent;
		}

		public int getTotalFailures() {
			return totalFailures;
		}

		public int getLastDecendent() {
			return lastDecendent;
		}

		/**
		 * change the type of a node, used to mark success
		 * @param type
		 */
		public void setType(NodeType type) {
			this.type = type;
		}
		
		public InvariantType getInvariantType(){
			return invariantType;
		}
		public void setInvariantType(InvariantType invariantType){
			this.invariantType = invariantType;
		}
		
		/**
		 * add a child to a node
		 * @param child
		 */
		public void addChild(Node child){
			children.add(child);
		}
		
		public void count(){
			switch (type) {
			case FAIL: 
			case FAILC:
				allFailedBelow = true;
				totalDecendent = 0;
				totalFailures = 1;
				lastDecendent = id;
				break;
			case SUCC:
			case SUCCC:
				allFailedBelow = false;
				totalDecendent = 0;
				totalFailures = 0;
				lastDecendent = id;
				break;
			case TRY:
			case TRYC:
			case ROOT:
				Iterator<Node> li = children.iterator();
				allFailedBelow = true;
				totalDecendent = 1;
				totalFailures = 0;
				lastDecendent = id;
				while (li.hasNext()) {
					Node child = li.next();
					child.count();
					allFailedBelow = allFailedBelow && child.isAllFailedBelow();
					totalDecendent += child.getTotalDecendent();
					totalFailures += child.getTotalFailures();
					lastDecendent = Math.max(lastDecendent, child.getLastDecendent());
				};
				break;	
			default:
				System.out.println("Wrong node type");
				break;
			}
		}
		
}
