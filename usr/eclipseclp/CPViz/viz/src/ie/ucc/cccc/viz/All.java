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

import java.io.IOException;

import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

/**
 * A class to run a collection of test cases as a single command. 
 * This calls the run.Viz method in Viz to perform the operations.
 * @author hsimonis
 *
 */
public class All {

	/**
	 * @param args No command line arguments at the moment
	 */
	public static void main(String[] args) {
		
//		System.out.println("Running Other Tests...");
//		try{
//			// ALMOST
//			Viz.runViz("examples/almost/configuration_11_22_26.xml",
//					"examples/almost/tree_11_22_26.tre",
//					"examples/almost/vis_11_22_26.viz");
//			Viz.runViz("examples/almost/configuration_15_34_40.xml",
//					"examples/almost/tree_15_34_40.tre",
//					"examples/almost/vis_15_34_40.viz");
//			Viz.runViz("examples/almost/configuration_16_32_51.xml",
//					"examples/almost/tree_16_32_51.tre",
//					"examples/almost/vis_16_32_51.viz");
//			Viz.runViz("examples/almost/configuration_17_34_57.xml",
//					"examples/almost/tree_17_34_57.tre",
//					"examples/almost/vis_17_34_57.viz");
//			Viz.runViz("examples/almost/configuration_18_30_76.xml",
//					"examples/almost/tree_18_30_76.tre",
//					"examples/almost/vis_18_30_76.viz");
//			Viz.runViz("examples/almost/configuration_19_35_76.xml",
//					"examples/almost/tree_19_35_76.tre",
//					"examples/almost/vis_19_35_76.viz");
//			Viz.runViz("examples/almost/configuration_20_35_88.xml",
//					"examples/almost/tree_20_35_88.tre",
//					"examples/almost/vis_20_35_88.viz");
//			Viz.runViz("examples/almost/configuration_21_39_91.xml",
//					"examples/almost/flat_21_39_91.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_21_46_77.xml",
//					"examples/almost/flat_21_46_77.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_22_44_92.xml",
//					"examples/almost/flat_22_44_92.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_23_40_115.xml",
//					"examples/almost/flat_23_40_115.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_24_40_130.xml",
//					"examples/almost/flat_24_40_130.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_25_39_150.xml",
//					"examples/almost/flat_25_39_150.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_25_45_130.xml",
//					"examples/almost/flat_25_45_130.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_26_39_168.xml",
//					"examples/almost/flat_26_39_168.tre",
//					"examples/almost/vis.viz");
//			Viz.runViz("examples/almost/configuration_26_42_156.xml",
//					"examples/almost/flat_26_42_156.tre",
//					"examples/almost/vis.viz");
//			// SOFTPREC
//			Viz.runViz("examples/softprec/configuration.xml",
//					"examples/softprec/cf50cp500uf10up5us1000tree.xml",
//					"examples/softprec/cf50cp500uf10up5us1000prop.xml");
//			Viz.runViz("examples/softprec/configuration.xml",
//					"examples/softprec/cf50cp250uf40up40us1000tree.xml",
//					"examples/softprec/cf50cp250uf40up40us1000prop.xml");
//			Viz.runViz("examples/softprec/configuration1.xml",
//					"examples/softprec/cf50cp500uf10up5us1000tree.xml",
//					"examples/softprec/cf50cp500uf10up5us1000prop.xml");
//			// TIDA
//			Viz.runViz("examples/tida/RESULT/configuration.xml",
//			"examples/tida/RESULT/tree.tre",
//			"examples/tida/RESULT/vis.viz");
//			Viz.runViz("examples/tida/RESULT/configuration.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/MULTI2.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-6m-50-60.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-6min-50-60.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-6m-50-70.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-6min-50-70.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-6m-50-80.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-6min-50-80.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-50-60.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-50-60.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-50-70.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-50-70.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-cumul-50-80.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Cumulative-Real-50-80.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-disj-50-60.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Disjunctive-Real-50-60.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-disj-50-70.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Disjunctive-Real-50-70.xml");
//			Viz.runViz("examples/tida/RESULT/configuration-disj-50-80.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/Disjunctive-Real-50-80.xml");
//			Viz.runViz("examples/tida/RESULT/configuration.xml",
//					"examples/tida/RESULT/tree.tre",
//					"examples/tida/RESULT/disjunctiveXMLFile.xml");
//
//			System.out.println("Tests Done");
//		} 
//		catch (SAXException e){
//			System.out.println("SAX Runtime error");
//		}
//		catch (IOException e){
//			System.out.println("IO error");
//		}
//		catch (ParserConfigurationException e){
//			System.out.println("Parser Configuration error");
//		}


		System.out.println("Running ECLiPSe Tests...");
		try{
	     //			Viz.runViz("configuration.xml","tree.tre","vis.viz");
			//BIBD
			Viz.runViz("examples/bibd/COLUMN/configuration.xml",
					"examples/bibd/COLUMN/tree.tre",
					"examples/bibd/COLUMN/vis.viz");
			Viz.runViz("examples/bibd/FIRST/configuration.xml",
					"examples/bibd/FIRST/tree.tre",
					"examples/bibd/FIRST/vis.viz");
			Viz.runViz("examples/bibd/MAX/configuration.xml",
					"examples/bibd/MAX/tree.tre",
					"examples/bibd/MAX/vis.viz");
     		Viz.runViz("examples/bibd/NAIVE/configuration.xml",
     				"examples/bibd/NAIVE/tree.tre",
     				"examples/bibd/NAIVE/vis.viz");
			Viz.runViz("examples/bibd/RESULT/configuration.xml",
					"examples/bibd/RESULT/tree.tre",
					"examples/bibd/RESULT/vis.viz");
			// BIN
			Viz.runViz("examples/bin/CREDIT/configuration.xml",
					"examples/bin/CREDIT/tree.tre",
					"examples/bin/CREDIT/vis.viz");
			// BRIDGE
			Viz.runViz("examples/bridge/RESULT/configuration.xml",
					"examples/bridge/RESULT/tree.tre",
					"examples/bridge/RESULT/vis.viz");
			// CAR
			Viz.runViz("examples/car/RESULT/configuration.xml",
					"examples/car/RESULT/tree.tre",
					"examples/car/RESULT/vis.viz");
			Viz.runViz("examples/car/NAIVE/configuration.xml",
					"examples/car/NAIVE/tree.tre",
					"examples/car/NAIVE/vis.viz");
			Viz.runViz("examples/car/NAIVE0/configuration.xml",
					"examples/car/NAIVE0/tree.tre",
					"examples/car/NAIVE0/vis.viz");
			// COSTAS
			Viz.runViz("examples/costas/MIDDLE/configuration.xml",
					"examples/costas/MIDDLE/tree.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_13.xml",
					"examples/costas/MIDDLE/tree_13.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_14.xml",
					"examples/costas/MIDDLE/tree_14.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_15.xml",
					"examples/costas/MIDDLE/tree_15.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_16.xml",
					"examples/costas/MIDDLE/tree_16.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_17.xml",
					"examples/costas/MIDDLE/tree_17.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MIDDLE/configuration_18.xml",
					"examples/costas/MIDDLE/tree_18.tre",
					"examples/costas/MIDDLE/vis.viz");
			Viz.runViz("examples/costas/MODEL/configuration.xml",
					"examples/costas/MODEL/tree.tre",
					"examples/costas/MODEL/vis.viz");
			Viz.runViz("examples/costas/MODEL/configuration_13.xml",
					"examples/costas/MODEL/tree_13.tre",
					"examples/costas/MODEL/vis.viz");
			Viz.runViz("examples/costas/MODEL/configuration_14.xml",
					"examples/costas/MODEL/tree_14.tre",
					"examples/costas/MODEL/vis.viz");
			Viz.runViz("examples/costas/MODEL/configuration_15.xml",
					"examples/costas/MODEL/tree_15.tre",
					"examples/costas/MODEL/vis.viz");
			Viz.runViz("examples/costas/MODEL/configuration_16.xml",
					"examples/costas/MODEL/tree_16.tre",
					"examples/costas/MODEL/vis.viz");
			Viz.runViz("examples/costas/NAIVE/configuration.xml",
					"examples/costas/NAIVE/tree.tre",
					"examples/costas/NAIVE/vis.viz");
			Viz.runViz("examples/costas/NAIVE/configuration_13.xml",
					"examples/costas/NAIVE/tree_13.tre",
					"examples/costas/NAIVE/vis.viz");
			Viz.runViz("examples/costas/NAIVE/configuration_14.xml",
					"examples/costas/NAIVE/tree_14.tre",
					"examples/costas/NAIVE/vis.viz");
			Viz.runViz("examples/costas/NAIVE/configuration_15.xml",
					"examples/costas/NAIVE/tree_15.tre",
					"examples/costas/NAIVE/vis.viz");
			Viz.runViz("examples/costas/NAIVE/configuration_16.xml",
					"examples/costas/NAIVE/tree_16.tre",
					"examples/costas/NAIVE/vis.viz");
			// MIX
			Viz.runViz("examples/mix/RESULT/configuration.xml",
					"examples/mix/RESULT/tree.tre",
					"examples/mix/RESULT/vis.viz");
			// NQUEEN
			Viz.runViz("examples/nqueen/QUEEN4/configuration.xml",
					"examples/nqueen/QUEEN4/tree.tre",
					"examples/nqueen/QUEEN4/vis.viz");
			Viz.runViz("examples/nqueen/COMPACT4/configuration.xml",
					"examples/nqueen/COMPACT4/tree.tre",
					"examples/nqueen/COMPACT4/vis.viz");
			Viz.runViz("examples/nqueen/COMPACT/configuration.xml",
					"examples/nqueen/COMPACT/tree.tre",
					"examples/nqueen/COMPACT/vis.viz");
			Viz.runViz("examples/nqueen/CREDIT/configuration.xml",
					"examples/nqueen/CREDIT/tree.tre",
					"examples/nqueen/CREDIT/vis.viz");
			Viz.runViz("examples/nqueen/FF/configuration.xml",
					"examples/nqueen/FF/tree.tre",
					"examples/nqueen/FF/vis.viz");
			Viz.runViz("examples/nqueen/FIRST_FAIL/configuration.xml",
					"examples/nqueen/FIRST_FAIL/tree.tre",
					"examples/nqueen/FIRST_FAIL/vis.viz");
			Viz.runViz("examples/nqueen/FULL/configuration.xml",
					"examples/nqueen/FULL/tree.tre",
					"examples/nqueen/FULL/vis.viz");
			Viz.runViz("examples/nqueen/MIDDLE/configuration.xml",
					"examples/nqueen/MIDDLE/tree.tre",
					"examples/nqueen/MIDDLE/vis.viz");
			Viz.runViz("examples/nqueen/NAIVE/configuration.xml",
					"examples/nqueen/NAIVE/tree.tre",
					"examples/nqueen/NAIVE/vis.viz");
			// PARTY
			Viz.runViz("examples/party/CREDIT/configuration.xml",
					"examples/party/CREDIT/tree.tre",
					"examples/party/CREDIT/vis.viz");
			Viz.runViz("examples/party/FF/configuration.xml",
					"examples/party/FF/tree.tre",
					"examples/party/FF/vis.viz");
			Viz.runViz("examples/party/LAYERED/configuration.xml",
					"examples/party/LAYERED/tree.tre",
					"examples/party/LAYERED/vis.viz");
			Viz.runViz("examples/party/NAIVE/configuration.xml",
					"examples/party/NAIVE/tree.tre",
					"examples/party/NAIVE/vis.viz");
			Viz.runViz("examples/party/RANDOM/configuration.xml",
					"examples/party/RANDOM/tree.tre",
					"examples/party/RANDOM/vis.viz");
			// PATH
			Viz.runViz("examples/path/CREDIT/configuration.xml",
					"examples/path/CREDIT/tree.tre",
					"examples/path/CREDIT/vis.viz");
			// ROOMS
			Viz.runViz("examples/rooms/CHANNEL/configuration.xml",
					"examples/rooms/CHANNEL/tree.tre",
					"examples/rooms/CHANNEL/vis.viz");
			Viz.runViz("examples/rooms/FF/configuration.xml",
					"examples/rooms/FF/tree.tre",
					"examples/rooms/FF/vis.viz");
			Viz.runViz("examples/rooms/IMPROVED/configuration.xml",
					"examples/rooms/IMPROVED/tree.tre",
					"examples/rooms/IMPROVED/vis.viz");
			Viz.runViz("examples/rooms/NAIVE/configuration.xml",
					"examples/rooms/NAIVE/tree.tre",
					"examples/rooms/NAIVE/vis.viz");
			// SBNO
			Viz.runViz("examples/sbno/TREE/configuration_6_10_5_3_2.xml",
					"examples/sbno/TREE/tree_6_10_5_3_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_6_20_10_3_4.xml",
					"examples/sbno/TREE/tree_6_20_10_3_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_6_30_15_3_6.xml",
					"examples/sbno/TREE/tree_6_30_15_3_6.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_6_40_20_3_8.xml",
					"examples/sbno/TREE/tree_6_40_20_3_8.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_6_50_25_3_10.xml",
					"examples/sbno/TREE/tree_6_50_25_3_10.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_6_60_30_3_12.xml",
					"examples/sbno/TREE/tree_6_60_30_3_12.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_7_3_3_1.xml",
					"examples/sbno/TREE/tree_7_7_3_3_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_14_6_3_2.xml",
					"examples/sbno/TREE/tree_7_14_6_3_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_21_6_2_1.xml",
					"examples/sbno/TREE/tree_7_21_6_2_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_21_9_3_3.xml",
					"examples/sbno/TREE/tree_7_21_9_3_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_28_12_3_4.xml",
					"examples/sbno/TREE/tree_7_28_12_3_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_35_15_3_5.xml",
					"examples/sbno/TREE/tree_7_35_15_3_5.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_42_18_3_6.xml",
					"examples/sbno/TREE/tree_7_42_18_3_6.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_49_21_3_7.xml",
					"examples/sbno/TREE/tree_7_49_21_3_7.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_56_24_3_8.xml",
					"examples/sbno/TREE/tree_7_56_24_3_8.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_7_63_27_3_9.xml",
					"examples/sbno/TREE/tree_7_63_27_3_9.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_8_14_7_4_3.xml",
					"examples/sbno/TREE/tree_8_14_7_4_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_8_28_14_4_6.xml",
					"examples/sbno/TREE/tree_8_28_14_4_6.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_12_4_3_1.xml",
					"examples/sbno/TREE/tree_9_12_4_3_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_18_8_4_3.xml",
					"examples/sbno/TREE/tree_9_18_8_4_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_24_8_3_2.xml",
					"examples/sbno/TREE/tree_9_24_8_3_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_36_12_3_3.xml",
					"examples/sbno/TREE/tree_9_36_12_3_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_108_36_3_9.xml",
					"examples/sbno/TREE/tree_9_108_36_3_9.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_9_120_40_3_10.xml",
					"examples/sbno/TREE/tree_9_120_40_3_10.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_10_15_6_4_2.xml",
					"examples/sbno/TREE/tree_10_15_6_4_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_10_18_9_5_4.xml",
					"examples/sbno/TREE/tree_10_18_9_5_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_10_30_9_3_2.xml",
					"examples/sbno/TREE/tree_10_30_9_3_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_10_90_27_3_6.xml",
					"examples/sbno/TREE/tree_10_90_27_3_6.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_10_120_36_3_8.xml",
					"examples/sbno/TREE/tree_10_120_36_3_8.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_11_11_5_5_2.xml",
					"examples/sbno/TREE/tree_11_11_5_5_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_11_22_10_5_4.xml",
					"examples/sbno/TREE/tree_11_22_10_5_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_12_22_11_6_5.xml",
					"examples/sbno/TREE/tree_12_22_11_6_5.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_12_88_22_3_4.xml",
					"examples/sbno/TREE/tree_12_88_22_3_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_13_13_4_4_1.xml",
					"examples/sbno/TREE/tree_13_13_4_4_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_13_26_6_3_1.xml",
					"examples/sbno/TREE/tree_13_26_6_3_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_13_26_8_4_2.xml",
					"examples/sbno/TREE/tree_13_26_8_4_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_13_104_24_3_4.xml",
					"examples/sbno/TREE/tree_13_104_24_3_4.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_15_15_7_7_3.xml",
					"examples/sbno/TREE/tree_15_15_7_7_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_15_21_7_5_2.xml",
					"examples/sbno/TREE/tree_15_21_7_5_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_15_35_7_3_1.xml",
					"examples/sbno/TREE/tree_15_35_7_3_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_15_70_14_3_2.xml",
					"examples/sbno/TREE/tree_15_70_14_3_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_16_16_6_6_2.xml",
					"examples/sbno/TREE/tree_16_16_6_6_2.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_16_20_5_4_1.xml",
					"examples/sbno/TREE/tree_16_20_5_4_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_16_24_9_6_3.xml",
					"examples/sbno/TREE/tree_16_24_9_6_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_19_19_9_9_4.xml",
					"examples/sbno/TREE/tree_21_21_5_5_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_21_21_5_5_1.xml",
					"examples/sbno/TREE/tree_21_21_5_5_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_25_25_9_9_3.xml",
					"examples/sbno/TREE/tree_25_25_9_9_3.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_25_30_6_5_1.xml",
					"examples/sbno/TREE/tree_25_30_6_5_1.tre",
					"examples/sbno/TREE/vis.viz");
			Viz.runViz("examples/sbno/TREE/configuration_31_31_6_6_1.xml",
					"examples/sbno/TREE/tree_31_31_6_6_1.tre",
					"examples/sbno/TREE/vis.viz");
			
			// SCHEDULE
			Viz.runViz("examples/schedule/NAIVE/configuration.xml",
					"examples/schedule/NAIVE/tree.tre",
					"examples/schedule/NAIVE/vis.viz");
			Viz.runViz("examples/schedule/RESULT/configuration.xml",
					"examples/schedule/RESULT/tree.tre",
					"examples/schedule/RESULT/vis.viz");
			// SENDMORE
			Viz.runViz("examples/sendmore/FULL/configuration.xml",
					"examples/sendmore/FULL/tree.tre",
					"examples/sendmore/FULL/vis.viz");
			Viz.runViz("examples/sendmore/SPLIT/configuration.xml",
					"examples/sendmore/SPLIT/tree.tre",
					"examples/sendmore/SPLIT/vis.viz");
			Viz.runViz("examples/sendmore/TREE/configuration.xml",
					"examples/sendmore/TREE/tree.tre",
					"examples/sendmore/TREE/vis.viz");
			Viz.runViz("examples/sendmore/WRONG/configuration.xml",
					"examples/sendmore/WRONG/tree.tre",
					"examples/sendmore/WRONG/vis.viz");
			// SONET
			Viz.runViz("examples/sonet/T21/configuration.xml",
					"examples/sonet/T21/tree.tre",
					"examples/sonet/T21/vis.viz");
			Viz.runViz("examples/sonet/T22/configuration.xml",
					"examples/sonet/T22/tree.tre",
					"examples/sonet/T22/vis.viz");
			Viz.runViz("examples/sonet/T23/configuration.xml",
					"examples/sonet/T23/tree.tre",
					"examples/sonet/T23/vis.viz");
			// SUDOKU
			Viz.runViz("examples/sudoku/BC/configuration.xml",
					"examples/sudoku/BC/tree.tre",
					"examples/sudoku/BC/vis.viz");
			Viz.runViz("examples/sudoku/DC/configuration.xml",
					"examples/sudoku/DC/tree.tre",
					"examples/sudoku/DC/vis.viz");
			Viz.runViz("examples/sudoku/FC/configuration.xml",
					"examples/sudoku/FC/tree.tre",
					"examples/sudoku/FC/vis.viz");
			// WAVE
			Viz.runViz("examples/wave/RESULT/configuration.xml",
					"examples/wave/RESULT/tree.tre",
					"examples/wave/RESULT/vis.viz");
			// Tests finished
			System.out.println("Tests Done");
		} 
		catch (SAXException e){
			System.out.println("SAX Runtime error");
		}
		catch (IOException e){
			System.out.println("IO error");
		}
		catch (ParserConfigurationException e){
			System.out.println("Parser Configuration error");
		}
	}

}
