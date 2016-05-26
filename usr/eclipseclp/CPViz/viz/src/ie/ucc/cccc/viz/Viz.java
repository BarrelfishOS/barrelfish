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

import javax.xml.parsers.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import java.util.*;

/**
 * The is the entry point for the Viz application. The program parses 
 * three XML files and produces output files accordingly.
 * @author hsimonis
 *
 */
public class Viz {
	static private Tools tools;
	static private PrintWriter idx;
	static private Tree tree;
	static private int countInvariant;
	/**
	 * This is the entry point for the command line interface. It works with three file names
	 * @param args String array with three entries config tree and viz files, all in XML format
	 */
	public static void main(String[] args)

	throws SAXException,IOException,ParserConfigurationException {
	
		if (args.length != 3) {
			System.out.println("Usage: viz configfile treefile vizfile");
			for(int i=0;i<args.length;i++) {
				System.out.println(args[i]);
			}
			return;
		}
		String configFilename = args[0];
		String treeFilename = args[1];
		String vizFilename = args[2];
		runViz(configFilename,treeFilename,vizFilename);
	}
	
	/**
	 * Method to run the viz program inside another program
	 * @param configFilename String filename for the configuration
	 * @param treeFilename String filename for the tree log
	 * @param vizFilename String filename for the viz log
	 * @throws SAXException
	 * @throws IOException
	 * @throws ParserConfigurationException
	 */
	static public void runViz(String configFilename,String treeFilename,String vizFilename)
	throws SAXException,IOException,ParserConfigurationException {
		countInvariant = 0;
		System.out.println("Running: viz " + configFilename + 
				" "+ treeFilename+ " "+ vizFilename);
		
		SAXParserFactory configFactory = SAXParserFactory.newInstance();
		SAXParser configParser = configFactory.newSAXParser();
		ConfigHandler configHandler = new ConfigHandler();
		configParser.parse(new File(configFilename),configHandler);
		System.out.println(configFilename +" contains " + configHandler.numNodes +
				" nodes ");
//		System.out.println("Tools" + configHandler.getTools());
		tools = configHandler.getTools();
		
		SAXParserFactory treeFactory = SAXParserFactory.newInstance();
		SAXParser treeParser = treeFactory.newSAXParser();
		TreeHandler treeHandler = new TreeHandler();
		try{
			treeParser.parse(new File(treeFilename),treeHandler);
		} catch (IllegalArgumentException e) {
			System.out.println("Caught "+e);
		}
		System.out.println(treeFilename +" contains " + treeHandler.numNodes +
				" nodes ");
		
		// the tree exists for marking purposes
		tree = treeHandler.tree;
		
		// create an idx file; this needs more work to respect TOOL
		try {
			File f = new File(tools.getDirectory(),tools.getIdx()+".idx");
			idx = new PrintWriter(new FileWriter(f));
		} catch (IOException e) {
			System.out.println("Can not create idx file");
		}
		if (tools.hasVizTools()){
			SAXParserFactory vizFactory = SAXParserFactory.newInstance();
			SAXParser vizParser = vizFactory.newSAXParser();
			VizHandler vizHandler = new VizHandler();
			vizParser.parse(new File(vizFilename),vizHandler);
			System.out.println(vizFilename +" contains " + vizHandler.numNodes +
			" nodes ");
		}
		// close the idx file; needs cleanup
		idx.close();
			
		// run all Tree Tools on the tree data structure
		for(Tool tool : tools.getTreeTools()){
			tool.handleTree(tree);
		}
		if (countInvariant > 0) {
			System.out.println("There were "+countInvariant+" Invariant violations.");
		}
	}

	
	static String getAttribute(String attributeName,Attributes attributes) {
		return getAttribute(attributeName,attributes,null);
	}
	static String getAttribute(String attributeName,
			Attributes attributes,String defaultValue) {
		String e = attributes.getValue(attributeName);
		if (e==null) {
			return defaultValue;
		} else {
			return e;
		}
	}
	
	static int getInteger(String attributeName,Attributes attributes) {
		return getInteger(attributeName,attributes,0);
	}
	static int getInteger(String attributeName,Attributes attributes,
			int defaultValue) {
		String e = attributes.getValue(attributeName);
		if (e==null) {
			return defaultValue;
		} else {
			return Integer.parseInt(e);
		}
	}
	
	static void markTreeNode(InvariantType type,int treeNode){
		countInvariant++;
		Node node = tree.getMap().get(treeNode);
		if (node != null){
//			System.out.println("Node "+node.getId()+" "+node.getType());
			node.setInvariantType(type);
		}
	}
	private static class ConfigHandler extends DefaultHandler {
		private Tools tools = new Tools();
		private int numNodes=0;
		
		private Tools getTools() {
			return tools;
		}
		private String getShow(Attributes attributes) {
			return getAttribute("show",attributes,"tree");
		}
		private String getType(Attributes attributes) {
			return getAttribute("type",attributes,"layout");
		}
		private String getDisplay(Attributes attributes) {
			return getAttribute("display",attributes,"compact");
		}
		private String getRepeat(Attributes attributes) {
			return getAttribute("repeat",attributes,"final");
		}
		private int getWidth(Attributes attributes) {
			return getInteger("width",attributes,500);
		}
		private int getHeight(Attributes attributes) {
			return getInteger("height",attributes,500);
		}
		private String getDirectory(Attributes attributes) {
			return getAttribute("directory",attributes,"TREE");
		}
		private String getIdx(Attributes attributes) {
			return getAttribute("idx",attributes,"aaa");
		}
		private String getFileRoot(Attributes attributes) {
			return getAttribute("fileroot",attributes,"tree");
		}
		
		private String getVersion(Attributes attributes) {
			return getAttribute("version",attributes);
		}
		
		public void startDocument() {
			System.out.println("Started Config Document");
		}
		
		public void endDocument() {
			System.out.println("End Document");
		}
		public void processingInstruction(String target,String data){
			System.out.println("Process target"+target+" data "+data);
		}
		public void startElement(String uri,
				String localname, String qname, 
				Attributes attributes) {
			if (qname.equals("configuration")) {	
				String version=getVersion(attributes);
				tools.setDirectory(getDirectory(attributes));
				tools.setIdx(getIdx(attributes));
				if (!version.equals("1.0")) {
					System.out.println("Wrong version "+version);
				};
			} else if (qname.equals("tool")){
				numNodes++;
				tools.addTool(new Tool(tools,
						getShow(attributes),
						getType(attributes),
						getDisplay(attributes),
						getRepeat(attributes),
						getWidth(attributes),
						getHeight(attributes),
						getFileRoot(attributes)
						));
			} else {
				System.out.println("Wrong Element Structure "+qname);
				numNodes++;
			}
		}
	}

	private static class TreeHandler extends DefaultHandler {
		private Tree tree;
		private int numNodes=0;
		
		private int getId(Attributes attributes) {
			return getInteger("id",attributes);
		}
		private int getParent(Attributes attributes) {
			return getInteger("parent",attributes);
		}
		private String getName(Attributes attributes) {
			return getAttribute("name",attributes);
		}
		private String getChoice(Attributes attributes) {
			return getAttribute("choice",attributes);
		}
		private int getSize(Attributes attributes) {
			return getInteger("size",attributes);
		}
		private int getValue(Attributes attributes) {
			return getInteger("value",attributes);
		}
		private String getVersion(Attributes attributes) {
			return getAttribute("version",attributes);
		}
		
		public void startDocument() {
			tree = new Tree();
			System.out.println("Started Tree Document");
		}
		
		public void endDocument() {
			tree.buildParentLists();
			System.out.println("End Tree Document");
		}
		public void processingInstruction(String target,String data){
			System.out.println("Process target"+target+" data "+data);
		}
		public void startElement(String uri,
				String localname, String qname, 
				Attributes attributes){
//			System.out.println(uri+ " " + localname+" "+ qname);
			if (numNodes % 10000 == 0 && numNodes > 0) {
				System.out.println(numNodes);
			}
			if (numNodes > 2000000){
				numNodes++;
				tree.buildParentLists();
				System.out.println("End Tree Document");
				throw new IllegalArgumentException("too long");
			} else {
			if (qname.equals("tree")) {	
				String version=getVersion(attributes);
				if (!version.equals("1.0")) {
					System.out.println("Wrong version "+version);
				};
			} else if (qname.equals("root")){
				int id=getId(attributes);
//				System.out.println("root"+id);
				tree.addNode(NodeType.ROOT,id);
				numNodes++;
			} else if (qname.equals("succ")){
				int id=getId(attributes);
				tree.markSucc(id);
			} else {
				int id=getId(attributes);
				int parent=getParent(attributes);
				String name=getName(attributes);
				int size=getSize(attributes);
				NodeType nodeType;
				if (qname.equals("try")){
					int value=getValue(attributes);
					nodeType = NodeType.TRY;
					tree.addNode(nodeType,
							id,parent,name,size,value);
				} else if (qname.equals("fail")){
					int value=getValue(attributes);
					nodeType = NodeType.FAIL;
					tree.addNode(nodeType,
							id,parent,name,size,value);
				} else if (qname.equals("tryc")){
					String choice = getChoice(attributes);
					nodeType = NodeType.TRYC;
					tree.addNode(nodeType,
							id,parent,name,size,choice);
				} else if (qname.equals("failc")){
					String choice = getChoice(attributes);
					nodeType = NodeType.FAILC;
					tree.addNode(nodeType,
							id,parent,name,size,choice);
				} else {
					System.out.println("Wrong node type >"+qname+"<");
					nodeType = NodeType.FAIL;
				}
				numNodes++;
			}
			}
		}
	}

	private static class VizHandler extends DefaultHandler {
		private int numNodes=0;
		private State state = null;
		private VisualState visualState = null;
		private Map<Integer,Visualizer> visualizerMap= new HashMap<Integer,Visualizer>();
		private Box box = new Box(0,0,1,1);
		private VizParentStack stack;
		private StateStack stateStack = new StateStack();;
		
		public void startDocument() {
			stack = new VizParentStack();
			System.out.println("Started Viz Document");
		}
		public void endDocument() {
			System.out.println("End Viz Document");
		}
		public void processingInstruction(String target,String data){
			System.out.println("Process target"+target+" data "+data);
		}
		public void startElement(String uri,
				String localName, String qName, 
				Attributes attributes) {
//			System.out.println(qName);
				numNodes++;
				if (qName.equals("visualization")) {
					// dummy bottom element of stack
					stateStack.push(new State(0,-1,null));
				} else if (qName.equals("state")) {
					int treeNode = getTreeNode(attributes);
					int parentTreeNode = tree.findParent(treeNode);
//					System.out.println("Parent "+treeNode+" "+ parentTreeNode);
					while (stateStack.peek().getTreeNode() != parentTreeNode){
						stateStack.pop();
					}
					state = new State(getId(attributes),
							treeNode,stateStack.peek());
//					if (tree.isNotFailState(treeNode)) {
						stateStack.push(state);
//					}
//					stateStack.dumpStack();

				} else if (qName.equals("visualizer_state")) {
					visualState = new VisualState(getVisualizer(attributes),state);
					stack.push(visualState);
					state.addVisualizerState(visualState);
				} else if (qName.equals("visualizer")) {
					int id=getId(attributes);
					//get attributes
					Visualizer visualizer = new Visualizer(id,getType(attributes),
							getDisplay(attributes),
							getX(attributes),
							getY(attributes),
							getWidth(attributes),
							getHeight(attributes),
							getGroup(attributes),
							getMin(attributes),
							getMax(attributes),
							getIndexStart(attributes));
					box.expandBox(visualizer.getBox());
					visualizerMap.put(id,visualizer);
				} else if (qName.equals("dvar")) {
					//get attributes
					stack.peek().add(new VizDVar(getIndex(attributes),
							getDomain(attributes)));
				} else if (qName.equals("integer")) {
					//get attributes
					stack.peek().add(new VizInteger(getIndex(attributes),
							getValue(attributes)));
				} else if (qName.equals("focus")) {
					//get attributes
					assert stack.peek() == visualState;
					visualState.focus(new VizFocus(getIndex(attributes),
							getGroup(attributes),
							getType(attributes)));
				} else if (qName.equals("failed")) {
					//get attributes
					assert stack.peek() == visualState;
					visualState.failed(new VizFailed(getIndex(attributes),
							getGroup(attributes),
							getValue(attributes)));
				} else if (qName.equals("tuple")) {
					VizParent e = new VizTuple(getIndex(attributes));
					//get attributes
					stack.peek().add(e);
					stack.push(e);
				} else if (qName.equals("collection")) {
					//get attributes
					VizParent e = new VizCollection(getIndex(attributes));
					stack.peek().add(e);
					stack.push(e);
				} else if (qName.equals("argument")) {
					//get attributes
					VizParent e = new VizArgument(getIndex(attributes));
					stack.peek().add(e);
					stack.push(e);
				} else {
					System.out.println("start:" + state.getId() + " " + 
						" uri" + uri + " local " + localName + " qname " + qName);
				
				}
		}
		
		public void endElement(String uri, String localName, String qName) {
			if (qName.equals("state")) {
				String treeRoot="tree";
				for(Tool tool : tools.getTreeTools()) {
					treeRoot = tool.getFileroot();
				}
				for(Tool tool : tools.getVizTools()) {
					tool.handleState(state,box);
					// this needs more work to respect information in Tool
//					if (state.getTreeNode() >= 0){
					idx.println(treeRoot+Math.max(0, state.getTreeNode())+".svg"+" "+
							tool.getFileroot()+state.getId()+".svg");
//					}

				}	
				state = null;
			} else if (qName.equals("tuple")) {
				stack.pop();
			} else if (qName.equals("collection")) {
				stack.pop();
			} else if (qName.equals("argument")) {
				stack.pop();
			} else if (qName.equals("visualizer_state")) {
				stack.pop();
				visualState = null;
			} else if (qName.equals("visualizer")) {
			} else {
			
			}
		}

		private Visualizer getVisualizer(Attributes attributes) {
			return visualizerMap.get(getId(attributes));
		}
		private int getId(Attributes attributes) {
			return getInteger("id",attributes);
		}
		private int getTreeNode(Attributes attributes) {
			return getInteger("tree_node",attributes);
		}
		private String getIndex(Attributes attributes) {
			return getAttribute("index",attributes);
		}
		private int getValue(Attributes attributes) {
			return getInteger("value",attributes);
		}
		private int getX(Attributes attributes) {
			return getInteger("x",attributes);
		}
		private int getY(Attributes attributes) {
			return getInteger("y",attributes);
		}
		private int getWidth(Attributes attributes) {
			return getInteger("width",attributes);
		}
		private int getHeight(Attributes attributes) {
			return getInteger("height",attributes);
		}
		private int getMin(Attributes attributes) {
			return getInteger("min",attributes);
		}
		private int getMax(Attributes attributes) {
			return getInteger("max",attributes);
		}
		private String getDomain(Attributes attributes) {
			return getAttribute("domain",attributes);
		}
		private String getGroup(Attributes attributes) {
			// if no group given, use dummy group 0, matches nothing
			return getAttribute("group",attributes,"0");
		}
		private String getType(Attributes attributes) {
			return getAttribute("type",attributes);
		}
		private String getDisplay(Attributes attributes) {
			return getAttribute("display",attributes);
		}
		private int getIndexStart(Attributes attributes) {
			// redefine lower index limit, default is 1
			return getInteger("indexstart",attributes,1);
		}
		
	}


}

