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

	
/**
 * Abstract class forms basis of drawing variants for different visualizers. Provides many utility methods 
 * used by multiple visualizers	
 * @author hsimonis
 *
 */
public abstract class VisualizerDrawer {
	public VisualContext context;
	/**
	 * the SVG output is not drawn with the coordinates as given. They are multiplied 
	 * by this factor do get values which are closer to the window size values. 
	 * Some SVG tools operate better when this scaling is applied.
	 */
	private static int scaleSVG=100;
	
/**
 * Save the context for later reference
 * @param context a VisualizerContext holding parameters for the visualizer
 */

 	public VisualizerDrawer(VisualContext context) {
		this.context = context;
	}
	
	/**
	 * Drawing routine for a particular visualizer. Must be implemented for any new visualizer 
	 * @param out
	 * @param visualState
	 */
	abstract void draw(PrintWriter out, VisualState visualState);	
	
	/**
	 * Routine to check invariants for a constraint visualizer. Allows output to 
	 * draw in the current output
	 * @return InvariantType
	 */
	public InvariantType invariant(PrintWriter out, VisualState visualState) {
		return InvariantType.TRUE;
	}
	
	public void drawBox(PrintWriter out, VisualState visualState,Colors color){
		rawRectSVG(out,context.getX(),context.getY(),context.getBoxWidth(),context.getBoxHeight(),color,0.2);
	}
	/**
	 * Return the enclosing box of the visualizer. This might be overriden by 
	 * sub classes if they think they need more space.
	 * @return enclosing Box
	 */
	public Box getBox() {
		return new Box(context.getX(),context.getY(),
				context.getBoxWidth(),context.getBoxHeight());
	}
	
	/**
	 * get X coor where data drawing starts
	 * @return int
	 */
	public int leftX() {
		return context.getX()+1;
	}
	/**
	 * get X coor where labels are drawn
	 * @return int
	 */
	public int labelX() {
		return context.getX();
	}
	/**
	 * get Y coor where data drawing starts
	 * @return int
	 */
	public int topY() {
		return context.getY()+1;
	}
	/**
	 * get Y coor where labels are drawn
	 * @return int
	 */
	public int labelY() {
		return context.getY();
	}
	/**
	 * get width of data drawing area
	 * @return int
	 */
	public int width() {
		return context.getWidth();
	}
	/**
	 * get height of data drawing area
	 * @return int
	 */
	public int height(){
		return context.getHeight();
	}
	/**
	 * convert index to X coor
	 * @param i
	 * @return double
	 */
	public double posX(double i){
		return leftX()+i-context.getIndexStart();
	}	
	/** 
	 * convert value to Y coor
	 * @param v double 
	 * @return double
	 */
	public double posY(double v){
		return topY()+v-context.getMin();
	}
	/**
	 * get X coor for next field beyond main drawing area
	 * @return int
	 */
	public int left2X() {
		return context.getX()+width()+2;
	}
	/**
	 * get x coor for label to the right of main drawing area
	 * @return
	 */
	public int label2X() {
		return context.getX()+width()+1;
	}
	/**
	 * get Y coor for next field below main drawing area
	 * @return int
	 */
	public int top2Y(){
		return topY()+height()+1;
	}
	
	/**
	 * get Y coor for extra labels below main drawing area
	 * @return int
	 */
	public int label2Y(){
		return topY()+height();
	}
	
	/**
	 * get min value, not valid for all visualizers
	 * @return int
	 */
	public int min(){
		return context.getMin();
	}
	/**
	 * get max value, not valid for all visualizers
	 * @return int
	 */
	public int max(){
		return context.getMax();
	}

	/**
	 * set the width of the data drawing area
	 * @param width int
	 */
	public void setWidth(int width) {
		context.setWidth(width);
	}
	/**
	 *  set the height of the data drawing area
	 * @param height int
	 */
	public void setHeight(int height) {
		context.setHeight(height);
	}
	/**
	 * set the min field for the visualizer
	 * @param min int
	 */
	public void setMin(int min) {
		context.setMin(min);
	}
	/**
	 * set the max field for the visualizer
	 * @param max int
	 */
	public void setMax(int max) {
		context.setMax(max);
	}
	
	/**
	 * draw a standard grid with X and Y labels. This will be overriden by many visualizers
	 * @param out file descriptor
	 */
	public void standardGrid(PrintWriter out){
		gridSVG(out,leftX(),topY(),width(),height());
		// value labels
		for(int i=context.getMin();i<=context.getMax();i++){
			textSVG(out,labelX(),posY(i),i,Colors.LABEL_TEXT_COLOR);
		}
		// index labels
		for(int i = 1; i<= width(); i++){
			textSVG(out,posX(i),labelY(),i,Colors.LABEL_TEXT_COLOR);
		}
	}	
	
	/**
	 * Utility method to select the correct color for an entry
	 * @param list
	 * @return Colors enum
	 */
	public Colors domainBasedColor(FullDomain list) {
		if (list.size() > 1) {
			return Colors.UNASSIGNED_COLOR;
		} else {
			return Colors.ASSIGN_COLOR;
		}
	}
	
	/**
	 * Special utility method for Boolean domains
	 * @param list
	 * @return Colors enum
	 */
	public Colors booleanColor(FullDomain list){
		if (list.isFixed()) {
			return booleanColor(list.getIntValue());
		} else {
			return Colors.UNASSIGNED_COLOR;
		}
	}
	
	public Colors booleanColor(FullDomain list, FullDomain removed){
		if (!list.isFixed()) {
			return Colors.UNASSIGNED_COLOR;
		} else if (removed == null || removed.size()== 0) {
			if (list.getIntValue() == 0) {
				return Colors.OLD_ZERO_COLOR;
			} else {
				return Colors.OLD_ONE_COLOR;
			}
		} else {
			return booleanColor(list.getIntValue());
		}
	}
	
	/**
	 * Utility method to choose color based on 0/1 value
	 * @param value int
	 * @return Colors enum based on integer value
	 */
	public Colors booleanColor(int value){
		if (value == 0) {
			return Colors.ZERO_COLOR;
		} else {
			return Colors.ONE_COLOR;
		}
	}
	
	/**
	 * Check if the current visualizer is in focus and should draw focus information
	 * @param focus
	 * @return boolean
	 */
	public boolean isInFocus(VizFocus focus){
		return focus != null && focus.getGroup().equals(context.getGroup());
	}
	/**
	 * Check is the current visualizer should draw failure information
	 * @param failed
	 * @return boolean
	 */
	public boolean isFailed(VizFailed failed){
		return failed != null && failed.getGroup().equals(context.getGroup());
	}
	
	/**
	 * Drawing primitive to compare values against low and high bounds.
	 * @param out
	 * @param x
	 * @param y
	 * @param width
	 * @param low
	 * @param high
	 * @param fixed
	 * @param possible
	 */
	public void drawCount(PrintWriter out,double x,double y,double width,
			int low,int high ,int fixed,int possible) {
		// show total low and high limits and current counting state
		rectSVG(out,x,y,low,1,Colors.TOO_LOW_COLOR);
		rectSVG(out,x+low,y,high-low,1,
				Colors.ALLOWED_COLOR);
		rectSVG(out,x+high,y,width-high,1,
				Colors.TOO_HIGH_COLOR);
		rectSVG(out,x,y+0.5,fixed,0.5,Colors.FIXED_COLOR );
		rectSVG(out,x+fixed,y+0.5,possible,0.5,Colors.POSSIBLE_COLOR);
		if (fixed >= low && fixed+possible <= high) {
			hollowRectSVG(out,x,y,width,1,Colors.BORDER_COLOR);
		} else {
			hollowRectSVG(out,x,y,width,1,Colors.FOCUS_COLOR);		
		}
	}

	/**
	 * Drawing primitive for visualizers. Draws a one-unit box in given fill color.
	 * @param out
	 * @param x
	 * @param y
	 * @param color
	 */
	public void unitSquareSVG(PrintWriter out,double x,double y,Colors color){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+scaleSVG+"\" height=\""+scaleSVG+"\" style=\"stroke-width:5;stroke:"+
				Colors.BORDER_COLOR+";fill:"+color+";\"/>");
	}
	
	/**
	 * draw unit square with specified opacity value
	 * @param out
	 * @param x
	 * @param y
	 * @param color
	 * @param opacity double between 0 (transparent) and 1 (solid)
	 */
	public void unitSquareSVG(PrintWriter out,double x,double y,Colors color, double opacity){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+scaleSVG+"\" height=\""+scaleSVG+"\" style=\"stroke-width:5;stroke:"+
				Colors.BORDER_COLOR+";fill:"+color+";opacity:"+opacity+";\"/>");
	}
	
	/**
	 * Draw a hollow rectangle on SVG output; Used for focus and failure highlighting
	 * @param out
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @param color
	 */
	public void hollowRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color){
		hollowRectSVG(out,x,y,width,height,color,0.1);
	}
	
	public void hollowRectSVG(PrintWriter out,double x,double y,
					double width,double height,Colors color,double strokeWidth){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke-width:"+strokeWidth*scaleSVG+";stroke:"+color+";fill:none;\"/>");
		
	}

	public void openRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color,double opacity){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke:none;fill:"+color+";opacity:"+opacity+";\"/>");
		
	}
	public void openRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke:none;fill:"+color+";\"/>");
		
	}
	public void roundedRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color,double opacity){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" rx=\"50\" ry=\"50\" style=\"stroke:black;stroke-width:5;fill:"+color+";opacity:"+opacity+";\"/>");
		
	}
	public void roundedRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" rx=\"50\" ry=\"50\" style=\"stroke:black;stroke-width:5;fill:"+color+";\"/>");
		
	}

	/**
	 * Draw a rectangle on SVG output
	 * @param out
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @param color
	 */
	public void rectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke-width:10;stroke:"+
				Colors.BORDER_COLOR+";fill:"+color+";\"/>");
		
	}
	
	/**
	 * draw rectangle with opacity value
	 * @param out
	 * @param x
	 * @param y
	 * @param width
	 * @param height
	 * @param color
	 * @param opacity double between 0 and 1
	 */
	public void rectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color,double opacity){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke-width:10;stroke:"+
				Colors.BORDER_COLOR+";fill:"+color+";opacity:"+opacity+";\"/>");
		
	}

	public final void rawRectSVG(PrintWriter out,double x,double y,
			double width,double height,Colors color,double opacity){
		out.println("<rect x=\""+x*scaleSVG+"\" y=\""+y*scaleSVG+
				"\" width=\""+width*scaleSVG+"\" height=\""+height*scaleSVG+
				"\" style=\"stroke-width:10;stroke:"+
				Colors.BORDER_COLOR+";fill:"+color+";opacity:"+opacity+";\"/>");
		
	}

	/**
	 * draw line
	 * @param out
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 * @param color
	 */
	public void lineSVG(PrintWriter out,double x1,double y1,double x2,double y2,Colors color){
		lineSVG(out, x1, y1, x2, y2, color,0.1);
		
	}
	public void lineSVG(PrintWriter out,double x1,double y1,double x2,double y2,
			Colors color,double lineWidth){
		out.println("<line x1=\""+x1*scaleSVG+"\" y1=\""+y1*scaleSVG+
				"\" x2=\""+x2*scaleSVG+"\" y2=\""+y2*scaleSVG+
				"\" style=\"stroke-width:"+lineWidth*scaleSVG+";stroke:"+
				color+";fill:none;\"/>");
		
	}

	/**
	 * Utility method to allow text output of integers instead of Strings
	 * @param out
	 * @param x
	 * @param y
	 * @param value
	 * @param color
	 */
	public void textSVG(PrintWriter out,double x,double y,int value, Colors color){
		textSVG(out,x,y,Integer.toString(value), color);
	}
	
	/**
	 * Utility drawing primitive for SVG output. Text is drawn inside the 
	 * unit box at given coordinates. This tries to place the text in the middle of the box, actual 
	 * placement depends on content of String. It adds its own offset to the coordinates to
	 * achieve this
	 * @param out
	 * @param x
	 * @param y
	 * @param text
	 * @param color
	 */
	public void textSVG(PrintWriter out,double x,double y,String text, Colors color){
		textSVG(out,x+0.5,y+0.75,0.5,text,color);
	}
	/**
	 * draw text at given position with given text size
	 * @param out
	 * @param x
	 * @param y
	 * @param size
	 * @param text given as int, will be converted to String
	 * @param color
	 */
	public void textSVG(PrintWriter out,double x,double y,double size,int text, Colors color){
		textSVG(out,x,y,size,Integer.toString(text), color);
	}
	/**
	 * draw text at given position in given size; expects String text
	 * @param out
	 * @param x
	 * @param y
	 * @param size
	 * @param text
	 * @param color
	 */
	public void textSVG(PrintWriter out,double x,double y,double size,
			String text, Colors color){
		out.println("<text x=\""+(scaleSVG*x)+"\" y=\""+(scaleSVG*y)+
				"\" style=\"stroke:none;fill:"+color+
				";font-family:Arial,sans-serif;font-size:"+size*scaleSVG+"px;text-anchor:middle;\">");
		out.println(text);
		out.println("</text>");
	}
	
	public void textStartSVG(PrintWriter out,double x,double y,double size,
			String text, Colors color){
		out.println("<text x=\""+(scaleSVG*x)+"\" y=\""+(scaleSVG*y)+
				"\" style=\"stroke:none;fill:"+color+
				";font-family:Arial,sans-serif;font-size:"+size*scaleSVG+"px;text-anchor:start;\">");
		out.println(text);
		out.println("</text>");
	}
	
	/**
	 * Utility method to draw a grid in x and y direction. Typically called first to 
	 * define the background on which further elements are drawn. We force the grid to 
	 * be drawn on integer coordinates. This avoids trouble with floating point rounding
	 * affecting the end condition.
	 * @param out
	 * @param x int
	 * @param y int
	 * @param width int
	 * @param height int
	 */
	public void gridSVG(PrintWriter out,int x,int y,int width,int height){
		// start path element
		out.print("<path d=\"");

		// vertical lines
		for(int i=x;i <= x+width;i++){
			out.print("M"+i*scaleSVG+","+y*scaleSVG+" l0,"+height*scaleSVG+" ");
		}

		// horizontal lines
		for(int i=y;i <= y+height;i++){
			out.print("M"+x*scaleSVG+","+i*scaleSVG+" l"+width*scaleSVG+",0 ");
		}
		// rest of path element
		out.println("\" style=\"stroke-width:5;stroke:"+
				Colors.GRID_COLOR+";fill:none\"/>");
	}
	public void pathStartSVG(PrintWriter out){
		out.print("<path d=\"");
	}
	public void pathEndSVG(PrintWriter out,Colors color){
		out.println("\" style=\"stroke-width:5;stroke:"+
				color+";fill:none\"/>");
	}
	public void pathMoveSVG(PrintWriter out,double x, double y){
		out.print("M"+x*scaleSVG+","+y*scaleSVG+" ");		
	}
	public void pathLineSVG(PrintWriter out,double x, double y){
		out.print("L"+x*scaleSVG+","+y*scaleSVG+" ");		
	}
}


