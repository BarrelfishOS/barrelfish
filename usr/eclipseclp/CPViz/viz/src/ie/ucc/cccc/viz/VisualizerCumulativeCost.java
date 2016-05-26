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

public class VisualizerCumulativeCost extends VisualizerLayout {

	public VisualizerCumulativeCost(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		Tuple[] areas = visualState.argumentTupleArray("areas");
		Tuple[] task = visualState.argumentTupleArray("tasks");
		FullDomain limit = visualState.argumentDomain("limit");
		FullDomain end = visualState.argumentDomain("end");
//		FullDomain cost = visualState.argumentDomain("cost");
		
		int n = task.length;
		int m = areas.length;
		setHeight(limit.getMax());
		int w =end.getMax();
		for(int i=1;i<n;i++) {
			int startMax = task[i].getField("start").getMax();
			int durMax = task[i].getField("dur").getMax();
			w = Math.max(w,startMax+durMax);
		}
		setWidth(w);
		standardGrid(out);
		int costMax =0;
		for(int j=1;j<m;j++){
			int x = areas[j].getField("x").getMax();
			int y = areas[j].getField("y").getMax();
			int width = areas[j].getField("width").getMax();
			int height = areas[j].getField("height").getMax();
			int cj = areas[j].getField("cost").getMax();
			costMax = Math.max(cj, costMax);
//			System.out.println("X "+x+" Y "+y+" W "+width+" H "+height+" Cj "+ cj);
			roundedRectSVG(out,posX(x),posY(y),width,height,Colors.COMPARE2_COLOR);
			textSVG(out,posX(x),posY(y+height/2),cj,Colors.ASSIGNED_TEXT_COLOR);
		}
		
		int[] profile = CumulativeProfile.computeProfile(task,width());
		int[] outerProfile = CumulativeProfile.computeOuterProfile(task,width());
		int oldOuterProfile =0;
		for(int i=0;i <width();i++){
//			openRectSVG(out,posX(i),posY(0),1,profile[i],Colors.UNASSIGNED_COLOR);
			int current = Math.min(outerProfile[i], limit.getMax());
			lineSVG(out,posX(i),posY(current),posX(i)+1,posY(current),
					Colors.TOO_HIGH_COLOR);
			lineSVG(out,posX(i),posY(oldOuterProfile),posX(i),posY(current),
					Colors.TOO_HIGH_COLOR);
			openRectSVG(out,posX(i),posY(0),1,profile[i],Colors.UNASSIGNED_COLOR);
			oldOuterProfile = current;
		}
		int prev=0;
		for(int j=1;j<m;j++){
			int x = areas[j].getField("x").getMax();
			int width = areas[j].getField("width").getMax();
			int cj = areas[j].getField("cost").getMax();
			int costLine = cj*limit.getMax()/costMax;
//			openRectSVG(out,posX(x),posY(0),width,costLine,Colors.FOCUS_COLOR,0.3);
			lineSVG(out,posX(x),posY(prev),posX(x),posY(costLine),
					Colors.FOCUS_COLOR,0.4);
			lineSVG(out,posX(x),posY(costLine),posX(x+width),posY(costLine),
					Colors.FOCUS_COLOR,0.4);
			prev= costLine;
		}
		drawLimit(out,limit);
		drawEnd(out,end);		
	}
	
	@Override
	public InvariantType invariant(PrintWriter out, VisualState visualState) {
		Tuple[] task = visualState.argumentTupleArray("tasks");
		FullDomain limit = visualState.argumentDomain("limit");
		FullDomain end = visualState.argumentDomain("end");
		
		int n = task.length;
		int w =0;
		int volume = 0;
		int minStart = task[1].getField("start").getMin();
		for(int i=1;i<n;i++) {
			int startMax = task[i].getField("start").getMax();
			int startMin = task[i].getField("start").getMin();
			int durMax = task[i].getField("dur").getMax();
			int durMin = task[i].getField("dur").getMin();
			int resMin = task[i].getField("res").getMin();
			w = Math.max(w,startMax+durMax);
			volume += durMin*resMin;
			minStart = Math.min(minStart, startMin);
		}		
		int[] profile = CumulativeProfile.computeProfile(task,w);
		InvariantType overall = InvariantType.TRUE;
		for(int i=0;i <w;i++){
			if (profile[i] > limit.getMax()) {
				overall = overall.update(InvariantType.INCONSISTENT);
			}
			if (profile[i] > limit.getMin()) {
				overall= overall.update(InvariantType.MISSING_PROPAGATION);
			}
		}
		if (end != null){
			if (end.getMax() < minStart+Math.ceil(volume/limit.getMax())) {
				overall = overall.update(InvariantType.INCONSISTENT);				
			}
			if (end.getMin() < minStart+Math.ceil(volume/limit.getMax())) {
				overall = overall.update(InvariantType.MISSING_PROPAGATION);				
			}
		}
		return overall;			
	}


}
