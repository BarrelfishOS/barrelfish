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
 * Utility class for the Extent class to hold left and right limits of extents as doubles
 * @author hsimonis
 *
 */
	public class DPair {
		private double x;
		private double y;
		
		/**
		 * override the toString methods so that Extents are printed nicely
		 */
		@Override public String toString() {
			return x+" "+y;
		}
		
		protected DPair(double x,double y) {
			this.x = x;
			this.y = y;
		}
		
		protected double getLeft() {
			return x;
		}	
		protected double getRight() {
			return y;
		}
		
		/**
		 * utility to shift left and right value together
		 * @param shift
		 */
		protected void shift(double shift) {
			x += shift;
			y += shift;
		}
	}
