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
 * Data structure to hold the parsed tree of search tree nodes. 
 * Allow for mapping from integer ids to the nodes
 * @author hsimonis
 *
 */
public class Tree {
	private Map<Integer,Node> map;
	private int nrNodes;
	
	public Tree() {
		map = new HashMap<Integer,Node>();
		nrNodes = 0;
	}
	
	public Map<Integer,Node> getMap() {
		return map;
	}
	
	public int getNrNodes() {
		return nrNodes;
	}
	
	public Node getRootNode() {
		return getMap().get(0);
	}
	
	public void addNode(NodeType type,int id,int parent,String name,int size,int value) {
		Node node = new Node(type,id,parent,name,size,value);
//		System.out.println("addNode "+node);
		map.put(id,node);
		nrNodes ++;

	}
	
	public void addNode(NodeType type,int id,int parent,String name,int size,String choice) {
		Node node = new Node(type,id,parent,name,size,choice);
//		System.out.println("addNode "+node);
		map.put(id,node);
		nrNodes ++;

	}
	
	public void addNode(NodeType type,int id) {
		Node node = new Node(type,id);
//		System.out.println("addNode "+node);
		map.put(id,node);
		nrNodes++;

	}
	
	public void markSucc(int id) {
//		System.out.println("addNode "+type + " id " + id);
		Node node = map.get(id);
		if (node.getType()== NodeType.TRY) {
			node.setType(NodeType.SUCC); 
		} else if (node.getType()== NodeType.TRYC) {
			node.setType(NodeType.SUCCC); 
		} else {
			System.out.println("Wrong node type, TRY or TRY expected, not "+node.getType());
		}

	}
	
	/**
	 * when all nodes have been added, create the children list in all nodes
	 * process the nodes in the original order, so that the children list is 
	 * sorted the right way
	 */
	public void buildParentLists() {
		// note the strict < in test
		for(int i=0; i<nrNodes;i++) {
			addToParent(i);
		}
		// we can now count decendents
		map.get(0).count();
	}
	
	/**
	 * utility method for integer keys instead of nodes
	 * @param i
	 */
	public void addToParent(int i){
		addToParent(map.get(i));
		
	}
	
	/**
	 * add a node to the children list of the parent
	 * @param node Node
	 */
	public void addToParent(Node node) {
		if (node.getParent() >= 0) {
//			System.out.println("Node "+node.getId()+" Parent "+node.getParent());
			map.get(node.getParent()).addChild(node);
		}
	}

	/**
	 * find the node number of the parent of a node
	 * @param nr integer node number
	 * @return int node number of parent
	 */
	public int findParent(int nr){
		if (nr == -1) {
			return -1;
		} else {
			Node current = map.get(nr);	
			return current.getParent();
		}
	}
	
	/**
	 * check is tree with the given number is not a fail node (FAIL or FAILC)
	 * @param nr int
	 * @return boolean True if node is not a fail node
	 */
	public boolean isNotFailState(int nr) {
		return (map.get(nr).getType() != NodeType.FAIL) && (map.get(nr).getType() != NodeType.FAILC);
	}
}
	


