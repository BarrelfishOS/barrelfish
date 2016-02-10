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


/**
 * Utility class to describe the space needed for the coordinate system of an SVG file.
 * Based on x,y,w,h integer values
 * @author hsimonis
 *
 */
public class Box {
	private static int scaleSVG=100;
	private int x,y,width,height;
	
	public String toString() {
		return "<Box X"+x+" Y "+y+" Width "+width+" Height "+height+">";
	}
	public Box(int x,int y, int width, int height){
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
	}
	
	public int getX() {
		return x;
	}
	public int getY() {
		return y;
	}
	public int getWidth() {
		return width;
	}
	public int getHeight() {
		return height;
	}
	
	/**
	 * Expand the current box to also include Box box. 
	 * @param box Box
	 */
	public void expandBox(Box box) {
		int x1 = Math.min(x,box.x);
		int y1 = Math.min(y,box.y);
		int x2 = Math.max(x+width,box.x+box.width);
		int y2 = Math.max(y+height,box.y+box.height);
		width = x2-x1;
		height = y2-y1;
		x = x1;
		y = y1;
	}

	/**
	 * code to generate the XML preamble for SVG files
	 * @param tool Tool, describes the settings for the output
	 * @param name String, the constructed filename for output
	 * @return the file descriptor to write the SVG content
	 */
	public PrintWriter svgPrefix(Tool tool,String name) {
		try{
			File f = new File(tool.getDirectory(),name);
			PrintWriter out = new PrintWriter(new FileWriter(f));
			out.println("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>");
			out.println("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"");
			out.println(" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">");
			out.println("<svg");
			out.println("   xmlns:svg=\"http://www.w3.org/2000/svg\"");
			out.println("   xmlns=\"http://www.w3.org/2000/svg\"");
			out.println("   version=\"1.1\"");
			out.println("   width=\""+tool.getWidth()+"px\"");
			out.println("   height=\""+tool.getHeight()+"px\"");
			int width1 = width;
			int height1 = height;
			out.println("   viewBox=\""+scaleSVG*x+" "+scaleSVG*y+
					" "+scaleSVG*width1+" "+scaleSVG*height1+"\">");
			return out;
		} catch (IOException e) {
			System.out.println("no luck");
			return null;
		}

	}
	
	/**
	 * Code to generate the final closing element of an SVG file and close 
	 * the file descriptor. This is placed here for symmetry with the svgPrefix method,
	 * there is no other connection to the Box class
	 * @param out file descriptor
	 */
	public void svgPostfix(PrintWriter out){
		out.println("</svg>");
		out.close();
	}
}
