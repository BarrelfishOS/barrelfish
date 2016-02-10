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
 * A visualizer for the bin_packing constraint. Has three arguments for the items (dvars),
 * sizes (integers) and bins (dvars)
 * @author hsimonis
 *
 */
public class VisualizerBinPacking extends VisualizerItemChart {

	public VisualizerBinPacking(VisualContext context){
		super(context);
	}
	
	@Override
	void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] items = visualState.argumentDomainArray("items");
		int[] sizes = visualState.argumentIntArray("sizes");
		Pair[] bins = visualState.argumentMinMaxArray("bins");
				
		int n = bins.length;
		int w= 0;
		for(int i=1;i<n;i++){
			w = Math.max(w,bins[i].getMax());

		}
		setWidth(w);
		setHeight(bins.length-1);
		standardGrid(out);
		
		for(int i=1;i<n;i++){
			int fixed = 0;
			int possible = 0;
			for(int j=1;j<items.length;j++) {
				if (items[j].isInDomain(i)) {
					if (items[j].isFixed()) {
						fixed += sizes[j];
					} else {
						possible += sizes[j];
					}
				}
			}
			// cut off excess possible length
			possible = Math.min(possible,width()-fixed);
			
			int low = bins[i].getMin();
			int high = bins[i].getMax();
			
			drawCount(out,leftX(),posY(i),width(),low,high,fixed,possible);
		}

	}
	@Override
	public InvariantType invariant(PrintWriter out, VisualState visualState) {
		FullDomain[] items = visualState.argumentDomainArray("items");
		int[] sizes = visualState.argumentIntArray("sizes");
		Pair[] bins = visualState.argumentMinMaxArray("bins");
				
		int n = bins.length;
		InvariantType overall=InvariantType.TRUE;
		int capacity=0;
		for(int i=1;i<n;i++){
			capacity += bins[i].getMax();

		}
		int required=0;
		for(int i=1;i<sizes.length;i++){
			required += sizes[i]; 
		}
		if (required > capacity) {
			overall = overall.update(InvariantType.INCONSISTENT);
		}
		int[] profile = new int[n];
			for(int j=1;j<items.length;j++) {
					if (items[j].isFixed()) {
						profile[items[j].getIntValue()] += sizes[j];
					} 
				
			}
		for(int i=1;i< n;i++){
			if (profile[i] > bins[i].getMax()){
				overall = overall.update(InvariantType.INCONSISTENT);
			}
			if (profile[i] > bins[i].getMin()){
				overall = overall.update(InvariantType.MISSING_PROPAGATION);
			}
		}
		return overall;
	}

}
