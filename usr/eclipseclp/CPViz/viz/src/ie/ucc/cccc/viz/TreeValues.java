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
import java.util.Map;

public class TreeValues {
	static ValueMap tryMap;
	static ValueMap failMap;

	public TreeValues(){		
	}
	
	public static TreeValues graph(Node node) {
		TreeValues treeValues = new TreeValues();
		tryMap = new ValueMap();
		failMap = new ValueMap();

		treeValues.recordNodes(node);
		return treeValues;
	}
	
	public void recordNodes(Node node) {
		recordNode(node);
		if(node.getChildren().size() > 0){
			for(Node child : node.getChildren()){
				recordNodes(child);
			}
		}
		
	}
	
	public void recordNode(Node node){
		switch (node.getType()){
		case 	ROOT:
			break;
		case TRY:
		case TRYC:
			tryMap.increment(node.getName(),node.getValue());
			break;
		case FAIL:
		case FAILC:
			failMap.increment(node.getName(),node.getValue());
			break;
		case SUCC:
		case SUCCC:
			break;
		default:
				System.out.println("Unexpected node type");
		}
				
	}
	public void plot(String directory,String name){
		try{
			File f = new File(directory,name);
			PrintWriter out = new PrintWriter(new FileWriter(f));
			out.println("Fail");
			out.println("INTEGER");
			for(Map.Entry<ValueKey, Integer> pair : failMap.getMap().entrySet()){
				out.println(pair.getValue()+"  "+pair.getKey().getName()+" "+
						pair.getKey().getValue());
//				System.out.println(pair.getKey().getName()+" "+
//						pair.getKey().getValue()+" "+pair.getValue());
			}
			out.close();
		} catch (IOException e) {
			System.out.println("no luck");
		}
	}
}
