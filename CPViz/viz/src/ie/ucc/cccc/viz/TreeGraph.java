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

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class TreeGraph {
	static int[] tryCount;
	static int[] succCount;
	static int[] failCount;
	public TreeGraph(){		
	}
	
	public static TreeGraph graph(Node node) {
		TreeGraph treeGraph = new TreeGraph();
		int depth = treeGraph.depth(node,0);
		tryCount = new int[depth+1];
		succCount = new int[depth+1];
		failCount = new int[depth+1];
		treeGraph.recordNodes(node,0);
		return treeGraph;
	}
	
	public int depth(Node node,int level){
		int max = level;
		for(Node child : node.getChildren()){
			max = Math.max(max,depth(child,level+1));
		}
		return max;
	}
	public void recordNodes(Node node,int level) {
		recordNode(node,level);
		if(node.getChildren().size() > 0){
			for(Node child : node.getChildren()){
				recordNodes(child,level+1);
			}
		}
		
	}
	
	public void recordNode(Node node,int level){
		switch (node.getType()){
		case 	ROOT:
			assert(level == 0);
			break;
		case TRY:
		case TRYC:
			tryCount[level]++;
			break;
		case FAIL:
		case FAILC:
			failCount[level]++;
			break;
		case SUCC:
		case SUCCC:
			succCount[level-1]++;
			break;
		default:
				System.out.println("Unexpected node type");
		}
				
	}
	public void plot(String directory,String name){
		try{
			File f = new File(directory,name);
			PrintWriter out = new PrintWriter(new FileWriter(f));
			int n = tryCount.length;
			for(int i=0;i<n;i++){
				out.println(""+i+" "+tryCount[i]+" "+failCount[i]+" "+succCount[i]);
				System.out.println(""+i+" "+tryCount[i]+" "+failCount[i]+" "+succCount[i]);
			}
			out.close();
		} catch (IOException e) {
			System.out.println("no luck");
		}
	}
}
