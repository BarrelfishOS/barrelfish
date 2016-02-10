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

import java.io.PrintWriter;
import java.util.*;


/**
 * This class is used to create the search tree layout.
 * @author hsimonis
 * 
 */
public class Layout {
	private Node node;
	private double x;
	private int y;
	private LinkedList<Layout> children;
	private Extent extent;
	public ArrayList<Pair> test;
	
	/**
	 * private constructor. use method layoutNode(...) to create a layout from Nodes
	 * @param node
	 * @param x
	 * @param y
	 * @param children
	 * @param extent
	 */
	private Layout(Node node,double x, int y, LinkedList<Layout> children,Extent extent){
		this.node = node;
		this.x = x;
		this.y = y;
		this.children = children;
		this.extent = extent;
	}
	/**
	 * static factory method to create a new layout. Use this method to 
	 * create a new layout from a tree given by Nodes.
	 * @param node top-level node of tree to generate
	 * @param level recursive integer level starting with one, defines the y coordinate of nodes
	 * @return a Layout object
	 * This is a not very clever layout routine, this should be rewritten to use an 
	 * adaptation of the Walker algorithm
	 */
	public static Layout layoutNode(Node node,boolean compact,int lastNode,int level) {
		if (node.getChildren().size() == 0 || node.getId() >= lastNode) {
			// leaf nodes are placed at 0
			double x = 0.0;
			int y = level;
			// the Layout contains no children, i.e. an empty list
			return new Layout(node,x,y,new LinkedList<Layout>(),new Extent(x,x));
		} else if (compact && 
				node.isAllFailedBelow() && 
				node.getLastDecendent() < lastNode){
			// this is a collapsed subtree
			// leaf nodes are placed at 0
			double x = 0.0;
			int y = level;
			// the Layout contains no children, i.e. an empty list
			// may want to add room for the failed subtree
			Extent subTreeExtent = new Extent(x,x);
			subTreeExtent.add(x,x);
			return new Layout(node,x,y,new LinkedList<Layout>(),subTreeExtent);
		} else {
			LinkedList<Layout> list = new LinkedList<Layout>();
			ListIterator<Node> li = node.getChildren().listIterator();
			Extent extent = new Extent();
			while (li.hasNext()) {
				Node child = li.next();
				if (child.getId() <= lastNode) {
					// layout the child recursively, ignoring other children
					Layout childLayout = Layout.layoutNode(child,compact,lastNode,level+1);
					// get the position of the top node
					double oldRight = childLayout.getExtent().getRight();
					// add the extent of this child to the previous ones
					extent = extent.addExtent(childLayout.extent);
					// move the child by the difference between old and new placement
					childLayout.moveTree(childLayout.getExtent().getRight()-oldRight);
					// remember the layout of this child
					list.add(childLayout);
				}
			}
			// place the parent node
			double x = (extent.getLeft()+extent.getRight())/2;
			int y = level;
			// add the position of the parent to the extent as first element
			extent.addFirst(x,x);
			return new Layout(node,x,y,list,extent);
		}
	}
	/**
	 * Utility method so that the code looks more consistent
	 * @return Extent
	 */
	private Extent getExtent(){
		return extent;
	}
	/**
	 * get the enclosing Box for the layout. This is used to set the coordinate 
	 * system  in the SVG files. 
	 * @return Box x,y,w,h describing an enclosing rectangle for all items in layout
	 */
	public Box getBox() {
		// make the box around a single node with one unit space around it
		Box box=new Box((int)x-1,y-1,2,2);
		ListIterator<Layout> li = children.listIterator();
		while (li.hasNext()) {
			box.expandBox(li.next().getBox());
		}
//		System.out.println("Box "+box);
		return box;
	}
	
	/**
	 * Utility method to shift a tree horizontally.
	 * @param shift double value, amount of shift
	 * This should disappear at some point, a proper layout method should not need this
	 */
	private void moveTree(double shift) {
		x += shift;
		ListIterator<Layout> li = children.listIterator();
		while (li.hasNext()) {
			Layout child = li.next();
			child.moveTree(shift);
		}
	}
	/**
	 * Draw a Layout to a file in the given directory, based on the enclosing Box. 
	 * 
	 * 	 
	 * @param tool Tool object describing configuration data 
	 * @param box enclosing Box defining the coordinate system for the SVG file
	 * @param lastNode the lastNode to be shown in tree
	 * @param name String filename to use for the SVG output
	 */
	public void draw(Tool tool,Box box,boolean compact,int lastNode,String name) {
		PrintWriter out= box.svgPrefix(tool,name);
		lineDraw(out);
		nodeDraw(out,lastNode);
		box.svgPostfix(out);
	}

	/**
	 * Draw the connecting lines for the tree, before nodes are drawn
	 * @param out file-descriptor to write to
	 */
	private void lineDraw(PrintWriter out) {
		ListIterator<Layout> li = children.listIterator();
		while (li.hasNext()) {
			Layout child = li.next();
			child.lineDraw(out);
			out.println("<!-- from "+node.getId()+" to "+child.node.getId()+" -->");
			Colors linkColor= Colors.LINK_COLOR;
			if (child.node.getSize() == 1) {
				linkColor = Colors.FIXED_LINK_COLOR;
			}
			NodeType type = child.node.getType();
			if (type == NodeType.TRYC || type == NodeType.FAILC || type == NodeType.SUCCC) {
				lineChoiceSVG(out,x,y,child.x,child.y,child.node.getChoice(),linkColor);
			} else {
				lineValueSVG(out,x,y,child.x,child.y,child.node.getValue(),linkColor);
			}
		}
		
	}
	
	/**
	 * Draw the nodes of the tree. Depending on the type of the node, use different formats. 
	 * Layout is traversed in a recursive decent
	 * @param out file descriptor to write to
	 */
	private void nodeDraw(PrintWriter out,int lastNode) {
		if (children.size() == 0) {
			double r=0.1;
			if (node.getId() == lastNode) {
				r = 0.15;
			}
			NodeType type=node.getType();
			if (type == NodeType.FAIL || type == NodeType.FAILC) {
				circleSVG(out,x,y,r,Colors.FAILED_NODE_COLOR);
			} else if (type == NodeType.SUCC || type == NodeType.SUCCC) {
				 circleSVG(out,x,y,r,Colors.SUCC_NODE_COLOR);
			} else if (node.getId() == lastNode) {
				circleSVG(out,x,y,r,Colors.LAST_NODE_COLOR);
			} else if (node.getTotalFailures() == 0){
				circleSVG(out,x,y,r,Colors.UNEXPLORED_NODE_TEXT_COLOR);				
			} else if (node.isAllFailedBelow() && node.getLastDecendent() < lastNode){
				triangleSVG(out,x,y,Colors.FAILED_NODE_COLOR,
						node.getTotalDecendent(),node.getTotalFailures());
			}
			invariantDraw(out,x,y,node);
		} else {
			ListIterator<Layout> li = children.listIterator();
			while (li.hasNext()) {
				Layout child = li.next();
				child.nodeDraw(out,lastNode);
				textSVG(out,x,y,child.node.getName(),
						Colors.UNEXPLORED_NODE_TEXT_COLOR);
				if(child.node.getSize() >= 0) {
					textSVG(out,x+0.4,y,0.2,child.node.getSize(),
						Colors.NODE_INFO_TEXT_COLOR );
				}
				invariantDraw(out,x,y,node);
			}
		}
	}
	

	public void invariantDraw(PrintWriter out, double x, double y,Node node){
		switch (node.getInvariantType()) {
		case TRUE: 
			break;
		case MISSING_PROPAGATION:
			drawCross(out,x,y,Colors.MISSING_PROPAGATION_COLOR);
			break;
		case INCONSISTENT:
			drawCross(out,x,y,Colors.INCONSISTENT_COLOR);
			break;
		case INTERESTING:
			drawCross(out,x,y,Colors.INTERESTING_COLOR);
			break;
		case FALSE:
			drawCross(out,x,y,Colors.FALSE_COLOR);
			break;
		default:
				System.out.println("Wrong invariant type");
		}
	}
	
	public void drawCross(PrintWriter out,double x, double y, Colors color){
//		System.out.println("Cross "+x+" "+y+" "+color);
		double size=0.2;
		wideLineSVG(out,x-size,y-size,x+size,y+size,color);
		wideLineSVG(out,x-size,y+size,x+size,y-size,color);
	}
	protected void wideLineSVG(PrintWriter out,double x1,double y1,
			double x2,double y2,Colors color) {
		out.println("<line x1=\""+x1*100+"\" y1=\""+y1*100+
				"\" x2=\""+x2*100+"\" y2=\""+y2*100+
				"\" style=\"stroke:"+color+";stroke-width:5;\"/>");
	}
	/**
	 * Drawing primitive for SVG output
	 * @param out file descriptor
	 * @param x1 double, as layout algorithm may place node on fractional x coordinate
	 * @param y1 int
	 * @param x2 double
	 * @param y2 int
	 */
	protected void lineSVG(PrintWriter out,double x1,int y1,double x2,int y2) {
		out.println("<line x1=\""+x1*100+"\" y1=\""+y1*100+
				"\" x2=\""+x2*100+"\" y2=\""+y2*100+
				"\" style=\"stroke:"+Colors.LINK_COLOR+";stroke-width:2;\"/>");
	}
	
	/**
	 * Drawing primitive for SVG output. Draw a line and the value it is associated with. 
	 * The value is drawn at the mid-point of the line, with a slight offset to the right
	 * @param out
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 * @param value integer value which was assigned
	 */
	protected void lineValueSVG(PrintWriter out,double x1,int y1,double x2,int y2,
			int value,Colors linkColor) {
		out.println("<line x1=\""+x1*100+"\" y1=\""+y1*100+
				"\" x2=\""+x2*100+"\" y2=\""+y2*100+
				"\" style=\"stroke:"+linkColor+";stroke-width:2;\"/>");
		double xm=(x1+x2)/2.0+0.05;
		double ym=(y1+y2)/2.0+0.15;
		out.println("<text x=\""+xm*100+"\" y=\""+ym*100+
				"\" style=\"stroke:none;fill:"+Colors.VALUE_TEXT_COLOR+
				";font-family:Arial,sans-serif;font-size:30px;text-anchor:start;\">");
		out.println(value);
		out.println("</text>");
	}
	
	protected void lineChoiceSVG(PrintWriter out,double x1,int y1,double x2,int y2,
			String choice,Colors linkColor) {
		out.println("<line x1=\""+x1*100+"\" y1=\""+y1*100+
				"\" x2=\""+x2*100+"\" y2=\""+y2*100+
				"\" style=\"stroke:"+linkColor+";stroke-width:2;\"/>");
		double xm=(x1+x2)/2.0+0.05;
		double ym=(y1+y2)/2.0+0.15;
		out.println("<text x=\""+xm*100+"\" y=\""+ym*100+
				"\" style=\"stroke:none;fill:"+Colors.VALUE_TEXT_COLOR+
				";font-family:Arial,sans-serif;font-size:20px;text-anchor:start;\">");
		out.println(choice);
		out.println("</text>");
	}
	
	/**
	 * Utility method to allow output of integer values instead of Strings
	 * @param out
	 * @param x
	 * @param y
	 * @param value
	 * @param color
	 */
	protected void textSVG(PrintWriter out,double x,int y,int value, Colors color){
		textSVG(out,x,y,Integer.toString(value), color);
	}

	/**
	 * Drawing primitive for SVG output. Draw text centered on the given coordinate
	 * @param out
	 * @param x
	 * @param y
	 * @param text
	 * @param color Colors enum to choose the color of the text
	 */
	protected void textSVG(PrintWriter out,double x,int y,String text, Colors color){
		out.println("<text x=\""+(100*x)+"\" y=\""+(100*y+20)+
				"\" style=\"stroke:none;fill:"+color+
				";font-family:Arial,sans-serif;font-size:40px;text-anchor:middle;\">");
		out.println(text);
		out.println("</text>");
	}

	protected void textSVG(PrintWriter out,double x,double y,double textSize,
			int text, Colors color){
		textSVG(out,x,y,textSize,Integer.toString(text),color);
	}
	protected void textSVG(PrintWriter out,double x,double y,double textSize,
					String text, Colors color){
		out.println("<text x=\""+(100*x)+"\" y=\""+(100*y)+
				"\" style=\"stroke:none;fill:"+color+
				";font-family:Arial,sans-serif;font-size:"+textSize*100+
				"px;text-anchor:middle;\">");
		out.println(text);
		out.println("</text>");
	}

	/**
	 * Drawing primitive for SVG output. Draw a small circle for the leaf nodes of the tree
	 * @param out File Descriptor
	 * @param x double 
	 * @param y double
	 * @param r double, radius
	 * @param color Colors enum to choose the color inside the circle
	 * the border color is defined by the NODE_BORDER_COLOR enum
	 */
	protected void circleSVG(PrintWriter out,double x,double y, double r,Colors color){
		out.println("<circle cx=\""+(100*x)+"\" cy=\""+(100*y)+
				"\" r=\""+r*100+"\" style=\"stroke:"+Colors.NODE_BORDER_COLOR+
				";stroke-width:1;fill:"+color+";\"/>");
	}
	
	private void triangleSVG(PrintWriter out, double x, int y,
			Colors Color, int totalDecendent, int totalFailures) {
		out.println("<polygon style=\"fill:"+Color+";stroke-width:1;stroke:"+Colors.NODE_BORDER_COLOR+";\" "+
				"points=\""+x*100+","+y*100+","+(x*100+50)+","+(y*100+50)+","+(x*100-50)+","+(y*100+50)+"\" />");
		textSVG(out,x,y+0.45,0.3,totalDecendent,Colors.NODE_INFO_TEXT_COLOR);
		textSVG(out,x,y+0.8,0.3,totalFailures,Colors.NODE_INFO_TEXT_COLOR);
	}
}
