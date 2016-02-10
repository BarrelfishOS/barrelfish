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
 * Utility class to store the parameter information about a visualizer
 * @author hsimonis
 *
 */
public class VisualContext {
	private int id;
	private String type;
	private String display;
	private int x;
	private int y; 
	private int width;
	private int height; 
	private String group;
	private int min;
	private int max;
	private int indexStart;
	/**
	 * the width of the total drawing area for this visualizer
	 */
	private int boxWidth=1;
	/**
	 * the height of the total drawing area of this visualizer
	 */
	private int boxHeight=1;

	@Override public String toString() {
		return "Context Id "+ id + " type "+ type + " display "+ display + 
		" x "+ x + " y "+ y + " width "+ width + " height "+ height + 
		" group "+ group + " min "+ min + " max "+ max;
	}
	public VisualContext(int id,String type,String display,
			int x, int y, int width, int height, 
			String group,
			int min,int max,int indexStart) {
		this.id = id;
		this.type = type;
		this.display = display;
		this.x = x;
		this.y = y;
		setWidth(width);
		setHeight(height);
		this.group = group;
		this.min = min;
		this.max = max;
		this.indexStart = indexStart;
//		System.out.println("Index start "+indexStart);
	}

	public int getId() {
		return id;
	}
	public String getType() {
		return type;
	}
	public String getDisplay() {
		return display;
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
	public int getBoxWidth() {
		return boxWidth;
	}
	public int getBoxHeight() {
		return boxHeight;
	}
	public int getMin() {
		return min;
	}
	public int getMax() {
		return max;
	}
	public int getIndexStart() {
		return indexStart;
	}
	public String getGroup() {
		return group;
	}
	
	public void setWidth(int width) {
		this.width = width;
		boxWidth = Math.max(boxWidth,width+2);
	}	
	public void setHeight(int height) {
		this.height = height;
		boxHeight = Math.max(boxHeight,height+2);
	}	
	public void setBoxWidth(int width) {
		this.boxWidth = width;
	}	
	public void setBoxHeight(int height) {
		this.boxHeight = height;
	}	
	public void setMin(int min) {
		this.min = min;
	}	
	public void setMax(int max) {
		this.max = max;
	}
	public Box getBox() {
		return new Box(getX(),getY(),getBoxWidth(),getBoxHeight());
	}
}
