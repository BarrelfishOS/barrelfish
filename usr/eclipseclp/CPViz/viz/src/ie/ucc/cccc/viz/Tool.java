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


/**
 * Description of a tool which defines how visualizations should be drawn, where 
 * the results are stored, etc
 * @author hsimonis
 *
 */
public class Tool {
	private Tools tools;
	private String show;
	private String type;
	private String display;
	private String repeat; 
	private int width;
	private int height;
	private String fileroot;
	
	@Override public String toString(){
		return "Tool "+show+" type "+type+" display "+display+
		" repeat "+repeat+" width "+width+" height "+height+
		" fileroot "+fileroot;
	}
	public Tool(Tools tools,String show,String type, String display, String repeat, 
			int width,int height,String fileroot){
		this.tools = tools;
		this.show = show;
		this.type = type;
		this.display= display;
		this.repeat = repeat;
		this.width =width;
		this.height= height;
		this.fileroot=fileroot;
	}

	/**
	 * This code decides how trees are drawn
	 * @param tree Tree, tree structure to display
	 */
	public void handleTree(Tree tree){
		if (show.equals("tree") && type.equals("layout")) {
			boolean compact= display.equals("compact");
			if (repeat.equals("final")) {
				Layout layout = Layout.layoutNode(tree.getRootNode(),
						compact,tree.getNrNodes()+1,1);
				layout.draw(this,layout.getBox(),compact,tree.getNrNodes()+1,
						getFileroot()+".svg");
			} else if (repeat.equals("all")) {
				for(int i=0; i< tree.getNrNodes();i++) {
					Layout layout = Layout.layoutNode(tree.getRootNode(),compact,i,1);
					layout.draw(this,layout.getBox(),compact,i,getFileroot()+i+".svg");		
				}
			} else {
				int k =Integer.parseInt(repeat);
				if (k < 0) {
					// repeat = "-n"
					for(int i=0; i< Math.min(-k,tree.getNrNodes());i++) {
						Layout layout = Layout.layoutNode(tree.getRootNode(),compact,i,1);
						layout.draw(this,layout.getBox(),compact,i,getFileroot()+i+".svg");		
					}	
				} else {
					// repeat = "n"
					Layout layout = Layout.layoutNode(tree.getRootNode(),
							compact,k,1);
					layout.draw(this,layout.getBox(),compact,k,getFileroot()+".svg");
					
				}
			}
		} else if (show.equals("tree") && type.equals("graph")) {
			TreeGraph treeGraph = TreeGraph.graph(tree.getRootNode());
			treeGraph.plot(getDirectory(),getFileroot()+".dat");
			
		} else if (show.equals("tree") && type.equals("values")) {
			TreeValues treeValues = TreeValues.graph(tree.getRootNode());
			treeValues.plot(getDirectory(),getFileroot()+".tm3");
			
		} else {
			System.out.println("skip tool "+this);
		}
	}
	
	/**
	 * This decides how States are drawn, where the output goes, etc.
	 * @param state State, describes the state to be displayed
	 * @param box Box, the enclosing box containing all visual information
	 */
	public void handleState(State state,Box box){
		if (show.equals("viz") && type.equals("layout")) {
			state.draw(this,box);
		} else {
			System.out.println("Skipping tool "+ this);
		}

	}
	
	public String getShow() {
		return show;
	}
	public String getType() {
		return type;
	}
	public String getDisplay() {
		return display;
	}
	public String getRepeat() {
		return repeat;
	}
	public int getWidth() {
		return width;
	}
	public int getHeight() {
		return height;
	}
	public String getDirectory() {
		return tools.getDirectory();
	}
	public String getFileroot() {
		return fileroot;
	}
	
}
