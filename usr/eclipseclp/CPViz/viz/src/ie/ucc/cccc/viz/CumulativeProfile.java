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
 * Utility class to compute profiles for cumulative. This is shared by two classes which
 * have a common ancestor only in VisualizerDrawer, so we factored this code out into
 * a little helper routine 
 * @author hsimonis
 *
 */
public class CumulativeProfile {

	public static int[] computeProfile(Tuple[] task,int width){
		int[] profile = new int[width];
		for(int i=1;i<task.length;i++) {
			int startMin = task[i].getField("start").getMin();
			int startMax = task[i].getField("start").getMax();
			int durMin = task[i].getField("dur").getMin();
//			int durMax = task[i].getField("dur").getMax();
			int resMin = task[i].getField("res").getMin();
//			int resMax = task[i].getField("red").getMax();
			if (startMin+durMin > startMax) {
				for(int j=startMax;j < startMin+durMin;j++){
					profile[j] += resMin;
				}
			}
		}
		return profile;
	}		
	public static int[] computeOuterProfile(Tuple[] task,int width){
		int[] profile = new int[width];
		for(int i=1;i<task.length;i++) {
			int startMin = task[i].getField("start").getMin();
			int startMax = task[i].getField("start").getMax();
			int durMin = task[i].getField("dur").getMax();
//			int durMax = task[i].getField("dur").getMax();
			int resMin = task[i].getField("res").getMin();
//			int resMax = task[i].getField("red").getMax();
			for(int j=startMin;j < startMax+durMin;j++){
				profile[j] += resMin;
			}
			
		}
		return profile;
	}		
}
