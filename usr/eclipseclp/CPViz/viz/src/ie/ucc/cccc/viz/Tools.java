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
 * Holds the collections of Tools for the tree and viz visualizers
 * @author hsimonis
 *
 */
public class Tools {
	private String directory;
	private String idx;
	private List<Tool> treeTools;
	private List<Tool> vizTools;
	
	public Tools() {
		treeTools = new ArrayList<Tool>();
		vizTools = new ArrayList<Tool>();
	}

	void setDirectory(String directory){
		this.directory = directory;
	}
	void setIdx(String idx){
		this.idx = idx;
	}
	public String getDirectory() {
		return directory;
	}
	public String getIdx(){
		return idx;
	}
	
	public List<Tool> getTreeTools() {
		return treeTools;
	}
	public boolean hasTreeTools() {
		return treeTools.size() > 0;
	}

	public List<Tool> getVizTools() {
		return vizTools;
	}
	public boolean hasVizTools() {
		return vizTools.size() > 0;
	}

	
	/**
	 * When adding a Tool, place it in the correct list
	 * @param tool
	 */
	public void addTool(Tool tool){
		if (tool.getShow().equals("tree")) {
			treeTools.add(tool);
		} else if (tool.getShow().equals("viz")) {
			vizTools.add(tool);
		} else{
			System.out.println("Tool has wrong show value "+tool);
		}
	}

}
