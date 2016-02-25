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
import java.util.Map;




/**
 * Visualizer to draw a matrix of domain variables
 * @author hsimonis
 *
 */
public class VisualizerMatrix extends VisualizerDrawer {

	public VisualizerMatrix(VisualContext context) {
		super(context);
	}
		
	public void draw(PrintWriter out,VisualState visualState ) {
		FullDomainMap matrix = visualState.getEntries().asDomainMap();
		FullDomainMap removed = visualState.getRemovedValues(matrix);
		// debug help
		out.println("<!-- Matrix " + context + " -->");
		// draw grid
		standardGrid(out);
		// draw entries
		for(Map.Entry<Pair, FullDomain> entry : matrix.entrySet()){
			drawMatrixEntry(out,entry.getKey(),entry.getValue(),
					removed.get(entry.getKey()));
		}
		// draw focus if applicable
		drawFocus(out, visualState.getFocus());
		// draw failed if applicable
		drawFailed(out, visualState.getFailed());
	}	

	protected void drawMatrixEntry(PrintWriter out,Pair xy, 
			FullDomain list,FullDomain removed){
		if (list.isFixed()){
			Colors assignedColor;
			if (removed == null || removed.size() == 0) {
				assignedColor = Colors.OLD_ASSIGN_COLOR;
			} else {
				assignedColor = Colors.ASSIGN_COLOR;
			}
			unitSquareSVG(out,posX(xy),posY(xy),assignedColor);
			textSVG(out,posX(xy),posY(xy),
					list.getIntValue(),Colors.ASSIGNED_TEXT_COLOR);
		} else {
			unitSquareSVG(out,posX(xy),posY(xy),Colors.UNASSIGNED_COLOR);
			if (context.getDisplay().equals("expanded")) {
				int range = context.getMax()-context.getMin()+1;
				int cut = (int) Math.ceil(Math.sqrt(range));
				double size = 1.0/(cut+1);
				for( int v: list) {
					double x = xy.getY()+((v-context.getMin()) % cut + 0.5)/(double) cut;
					double y = xy.getX()+((v-context.getMin()) / cut + 0.8)/(double) cut;
					textSVG(out,posX(x),posY(y),size,
							Integer.toString(v),Colors.UNASSIGNED_TEXT_COLOR);
				}
				if (removed != null){
				for( int v: removed) {
					double x = xy.getY()+((v-context.getMin()) % cut + 0.5)/(double) cut;
					double y = xy.getX()+((v-context.getMin()) / cut + 0.8)/(double) cut;
					textSVG(out,posX(x),posY(y),size,
							Integer.toString(v),Colors.REMOVED_VALUE_TEXT_COLOR);
				}
				}
			}
		}
	}

	protected void drawFocus(PrintWriter out,VizFocus focus) {
		if (isInFocus(focus)) {
			// debug info
			out.println("<!-- "+ focus +" -->");
			if (focus.getType() == null) {
				// normal focus on variable assignment
				Pair xy = focus.getXYIndex();
				double x = posX(xy);
				double y = posY(xy);
				hollowRectSVG(out,x,y,1,1,Colors.FOCUS_COLOR);				
			} else if (focus.getType().equals("row")) {
				double x = leftX();
				double y = posY(focus.getSingleIndex());
				hollowRectSVG(out,x,y,width(),1,Colors.BLOCK_FOCUS_COLOR);				
				
			} else if (focus.getType().equals("col")) {
				double x = posX(focus.getSingleIndex());
				double y = topY();
				hollowRectSVG(out,x,y,1,height(),Colors.BLOCK_FOCUS_COLOR);				
				
			} else if (focus.getType().equals("block")) {
				Box box = focus.getBoxIndex();
				double x = posX(box.getX());
				double y = posY(box.getY());
				hollowRectSVG(out,x,y,box.getWidth(),box.getHeight(),Colors.BLOCK_FOCUS_COLOR);				
			} else {
				System.out.println("Unknown focus type: "+focus);
			}
		}
	}
	
	protected void drawFailed(PrintWriter out,VizFailed failed){
		if (isFailed(failed)) {
			Pair xy = failed.getXYIndex();
			double x = posX(xy);
			double y = posY(xy);
			out.println("<!-- "+ failed + " -->");
			unitSquareSVG(out,x,y,Colors.FAILED_COLOR);
			hollowRectSVG(out,x,y,1,1,Colors.FAILED_COLOR);
			textSVG(out,x,y,failed.getValue(),Colors.FAILED_TEXT_COLOR);
		}
	}
	
//	protected void drawEntry(PrintWriter out,VizParent entry){
//		Pair xy = entry.getXYIndex();
//		FullDomain list = entry.getDomainAsList();
//		out.println("<!-- index "+xy.getX() + " "+ xy.getY() +" domain "+list+" -->");
//		if (list.isFixed()){
//			unitSquareSVG(out,posX(xy),posY(xy),domainBasedColor(list));
//			textSVG(out,posX(xy),posY(xy),
//					list.getIntValue(),Colors.ASSIGNED_TEXT_COLOR);
//		} else {
//			unitSquareSVG(out,posX(xy),posY(xy),Colors.UNASSIGNED_COLOR);
//			if (context.getDisplay().equals("expanded")) {
//				int range = context.getMax()-context.getMin()+1;
//				int cut = (int) Math.ceil(Math.sqrt(range));
//				double size = 1.0/(cut+1);
//				for( int v: list) {
//					double x = xy.getY()+((v-context.getMin()) % cut + 0.5)/(double) cut;
//					double y = xy.getX()+((v-context.getMin()) / cut + 0.8)/(double) cut;
//					textSVG(out,posX(x),posY(y),size,
//							Integer.toString(v),Colors.UNASSIGNED_TEXT_COLOR);
//				}
//			}
//		}
//	}
	
	/**
	 * draw a standard grid with X and Y labels. This will be overriden by many visualizers
	 * @param out file descriptor
	 */
	@Override public void standardGrid(PrintWriter out){
		gridSVG(out,leftX(),topY(),width(),height());
		// y dimensional labels
		for(int i=context.getIndexStart();i<height()+context.getIndexStart();i++){
			textSVG(out,labelX(),posY(i),i,Colors.LABEL_TEXT_COLOR);
		}
		// X dimensional labels
		for(int i = context.getIndexStart(); i< width()+context.getIndexStart(); i++){
			textSVG(out,posX(i),labelY(),i,Colors.LABEL_TEXT_COLOR);
		}
	}	

	@Override public double posX(double x) {
		return context.getX()+1+x-context.getIndexStart();
	}
	@Override public double posY(double y) {
		return context.getY()+1+y-context.getIndexStart();
	}
	public double posX(Pair xy){
		return posX(xy.getY());
	}	
	public double posY(Pair xy){
		return posY(xy.getX());
	}
}

