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
 * Visualizer for the cumulative constraint as a Gantt chart. 
 * @author hsimonis
 *
 */
public class VisualizerCumulativeGantt extends VisualizerItemChart {

	public VisualizerCumulativeGantt(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		Tuple[] task = visualState.argumentTupleArray("tasks");
		FullDomain limit = visualState.argumentDomain("limit");
		FullDomain end = visualState.argumentDomain("end");
		
		int n = task.length;
		setHeight(n-1);	// should this be n or n-1 ???	
//		System.out.println("End"+end);
		if (end == null){
		setWidth(computeLastEnd(task));
		} else {
			setWidth(Math.max(computeLastEnd(task),end.getMax()));
		}
		standardGrid(out);
		for(int i=1;i<n;i++) {
			drawTask(out,task[i],i);
		}
		int[] profile = CumulativeProfile.computeProfile(task, width());
		for(int i=0;i<width();i++){
			if (profile[i] == limit.getMax()) {
				openRectSVG(out,posX(i),topY(),1,height(),Colors.FULL_COLOR,0.2);
			}
		}
		drawEnd(out,end);
		
	}

	public int computeLastEnd(Tuple[] task) {
		int w =0;
		for(int i=1;i<task.length;i++) {
			int startMax = task[i].getField("start").getMax();
			int durMax = task[i].getField("dur").getMax();
			w = Math.max(w,startMax+durMax);
		}
		return w;
	}
	public void drawTask(PrintWriter out, Tuple task,int i){
		int startMin = task.getField("start").getMin();
		int startMax = task.getField("start").getMax();
		int durMin = task.getField("dur").getMax();
		int durMax = task.getField("dur").getMax();
		rectSVG(out,posX(startMin),posY(i),
				startMax+durMax-startMin,1,Colors.UNASSIGNED_COLOR);
		hollowRectSVG(out,posX(startMin),posY(i),
				durMin,1,Colors.BORDER_COLOR,0.2);
		hollowRectSVG(out,posX(startMin),posY(i),
				durMax,1,Colors.BORDER_COLOR,0.1);
		hollowRectSVG(out,posX(startMax),posY(i),
				durMin,1,Colors.BORDER_COLOR,0.2);
		hollowRectSVG(out,posX(startMax),posY(i),
				durMax,1,Colors.BORDER_COLOR,0.1);
		if (startMin+durMin > startMax) {
				double obligatory = startMin+durMin-startMax;
				rectSVG(out,posX(startMax),posY(i),obligatory,1,Colors.ASSIGN_COLOR);
			
		}
		
	}


}
