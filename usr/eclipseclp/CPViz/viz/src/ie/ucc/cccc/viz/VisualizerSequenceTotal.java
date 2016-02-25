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

/**
 * Visualizer for the sequence constraint with boolean variables, local and global limits
 * @author hsimonis
 *
 */
public class VisualizerSequenceTotal extends VisualizerBinaryVector {

	public VisualizerSequenceTotal(VisualContext context) {
		super(context);
	}
	
	public void draw(PrintWriter out, VisualState visualState) {
		int totalLow = visualState.argumentInt("TotalLow");
		int totalHigh = visualState.argumentInt("TotalHigh");
		int low = visualState.argumentInt("Low");
		int high = visualState.argumentInt("High");
		int subSeqN = visualState.argumentInt("N");
		FullDomain[] vars = visualState.argumentDomainArray("Binary");
		int n = vars.length;
		setWidth(n-1);
		setHeight(subSeqN+5);
		standardGrid(out);
		// the 0/1 domain variables
		for(int i=1;i < n; i++) {
			unitSquareSVG(out,posX(i),topY(),booleanColor(vars[i]));		
		}
		// counting labels
		for(int i = 0; i<= width(); i++){
			textSVG(out,posX(i+1),topY()+1.8,0.5,i,Colors.LABEL_TEXT_COLOR);
		}
		// the overall count
		showCount(out,1,n,leftX(),top2Y(),width(),totalLow,totalHigh,vars);
		// the count for each sub-sequence
		for(int i = 1; i<= width()-subSeqN+1; i++){
			double y = top3Y()+((i-1) % subSeqN);
			showCount(out,i,i+subSeqN,posX(i),y,subSeqN,low,high,vars);
		}
		
	}	

	public void showCount(PrintWriter out,int from,int to,double x,double y,double width,
			int low,int high,FullDomain[] vars){
		int fixed=0;
		int possible = 0;
		for(int i=from;i < to; i++) {
			if (vars[i].isInDomain(1)){
				if (vars[i].isFixed()) {
					fixed++;
				} else {
					possible++;
				}				
			}
		}
		drawCount(out,x,y,width,low,high,fixed,possible);
	}
	
	
	public int top3Y() {
		return top2Y()+2;
	}


}
