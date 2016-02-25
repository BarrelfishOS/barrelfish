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


import java.util.Scanner;

/**
 * Store the information about a domain variable in a visualizer
 * @author hsimonis
 *
 */
public class VizDVar extends VizEntry {
	private String domain;
	
	@Override public String toString() {
		return "DVar "+index+" Domain "+domain;
	}
	
	/**
	 * the index is held as a String, it might be an integer or a list of integers,
	 * depending on the visualizer
	 * @param index String, holds integer or list of integers
	 * @param domain String, holds domain description increasing sequence of integers 
	 * or a .. b integer intervals, not spaces around ..
	 * 
	 */
	public VizDVar(String index,String domain){
		this.index = index;
		this.domain = domain;
	}
	
	/**
	 * get the domain as an integer list
	 */
	public FullDomain getDomainAsList(){
//		System.out.println(domain);
		FullDomain list = new FullDomain();
		Scanner s = new Scanner(domain);
		while (s.hasNext()) {
			int low = s.nextInt();
			if (s.hasNextInt()) {
				list.add(low);
			} else if (s.hasNext()) {
				String next = s.next();	
				if (next.equals("..")) {	
					int high = s.nextInt();
					for(int i=low; i <= high; i++){
						list.add(i);
					}
				} else {
					System.out.println("Syntax error in domain: "+ domain);
					// syntax error
					return list;
				}
			} else {
				list.add(low);
			}
		}
		return list;
	}
}
