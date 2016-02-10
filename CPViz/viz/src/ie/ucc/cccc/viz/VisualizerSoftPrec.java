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

public class VisualizerSoftPrec extends VisualizerBinaryMatrix {

	public VisualizerSoftPrec(VisualContext context) {
		super(context);
	}
	
	@Override
	public void draw(PrintWriter out, VisualState visualState) {
		FullDomain[] features = visualState.argumentDomainArray("features");
		FullDomain[] removedFeatures = visualState.getRemovedValues("features",features);
		FullDomainMap soft = visualState.argument("soft Precedences").asDomainMap();
		FullDomainMap removedSoft = visualState.getRemovedValues("soft Precedences",soft);
		FullDomainMap hard = visualState.argument("hard Precedences").asDomainMap();
		FullDomainMap removedHard = visualState.getRemovedValues("hard Precedences",hard);
		FullDomainMap components = visualState.argument("components").asDomainMap();
		FullDomainMap removedComponents = visualState.getRemovedValues("components",components);
		FullDomain cost = visualState.argumentDomain("cost");
		FullDomain removedCost = visualState.getRemovedValues("cost",cost);
		
		int n = features.length;
		setWidth(n-1+context.getIndexStart());
		setHeight(n-1+context.getIndexStart());
		
//		System.out.println("N "+n+" cost "+removedCost);
		// debug help
		out.println("<!-- SoftPrec " + context + " -->");
		// draw grid
		standardGrid(out);
		// X dimensional labels
		for(int i = context.getIndexStart(); i< width()+context.getIndexStart(); i++){
			textSVG(out,posX(i),label2Y(),i,Colors.LABEL_TEXT_COLOR);
			textSVG(out,label2X(),posY(i),i,Colors.LABEL_TEXT_COLOR);
		}
		for(int i= context.getIndexStart();i<height()+context.getIndexStart();i++){
			for(int j = context.getIndexStart();j<width()+context.getIndexStart();j++){
				Pair index= new Pair(i,j);
				
				drawSoftPrecEntry(out,i,j,soft.get(index),removedSoft.get(index),
						hard.get(index),removedHard.get(index));
			}
		}

		// components are drawn as hollow rectangles over the entry
		// the color depends on the component number
		// draw component entries
		for(Map.Entry<Pair, FullDomain> entry : components.entrySet()){
			drawComponentEntry(out,entry.getKey(),entry.getValue(),
					removedComponents.get(entry.getKey()));
		}
		// draw feature entries, both at the bottom and to the right
		for(int i=context.getIndexStart();i<n-1+context.getIndexStart();i++){
			unitSquareSVG(out,posX(i),top2Y(),
					booleanColor(features[i],removedFeatures[i]));
			unitSquareSVG(out,left2X(),posY(i),
					booleanColor(features[i],removedFeatures[i]));
			if (features[i].isFixed()){
				textSVG(out,posX(i),top2Y(),
						features[i].getIntValue(),Colors.ASSIGNED_TEXT_COLOR);
				textSVG(out,left2X(),posY(i),
						features[i].getIntValue(),Colors.ASSIGNED_TEXT_COLOR);
			}
		}
		// cross out the main diagonal
		lineSVG(out,posX(0),posY(0),posX(n-1),posY(n-1),Colors.GRID_COLOR,0.05);

		String costMsg = "Value: "+domainChangeMsg(cost,removedCost);
		textStartSVG(out,posX(0),top2Y()+1.75,0.5,costMsg,Colors.ASSIGNED_TEXT_COLOR);


	}

	// this should be moved somewhere else, to be reusable
	protected String domainChangeMsg(FullDomain current,FullDomain removed){
		String minMsg;
		String maxMsg;
		if (removed == null || removed.isEmpty()) {
			return current.getMin()+" .. "+current.getMax();
		} else {
			if (removed.getMin() < current.getMin()) {
				minMsg = "("+removed.getMin() + ") -- " + current.getMin();
			} else {
				minMsg = Integer.toString(current.getMin());
			}
			if (removed.getMax() > current.getMax()) {
				maxMsg = current.getMax() + " -- (" + removed.getMax()+")";
			} else {
				maxMsg = Integer.toString(current.getMax());
			}
			return minMsg+" .. "+maxMsg;
		}
		
	}
	protected void drawSoftPrecEntry(PrintWriter out,int i,int j,
			FullDomain softList,FullDomain softRemovedList,
			FullDomain hardList,FullDomain hardRemovedList){
		unitSquareSVG(out,posX(j),posY(i),
				softPrecFillColor(i,j,softList,softRemovedList,hardList,hardRemovedList));
		textSVG(out,posX(j),posY(i),
				softPrecText(i,j,softList,softRemovedList,hardList,hardRemovedList),
				softPrecTextColor(i,j,softList,softRemovedList,hardList,hardRemovedList));

	}
	protected Colors softPrecFillColor(int i,int j,
			FullDomain softList,FullDomain softRemovedList,
			FullDomain hardList,FullDomain hardRemovedList){
		if (hardList == null){
			return Colors.HARDPREC_UNKNOWN_COLOR;
		} else if (hardList.size() > 1){
			return Colors.HARDPREC_UNKNOWN_COLOR;
		} else if (hardList.getIntValue() == 0){
			if (hardRemovedList == null || hardRemovedList.isEmpty()){
				return Colors.HARDPREC_OLD_ZERO_COLOR;
			} else {
				return Colors.HARDPREC_ZERO_COLOR;				
			}
		} else if (hardList.getIntValue() == 1){
			if (hardRemovedList == null || hardRemovedList.isEmpty()){
				return Colors.HARDPREC_OLD_ONE_COLOR;
			} else {
				return Colors.HARDPREC_ONE_COLOR;				
			}
		} else {
			return Colors.INTERESTING_COLOR;
		}
	}
	protected Colors softPrecTextColor(int i,int j,
			FullDomain softList,FullDomain softRemovedList,
			FullDomain hardList,FullDomain hardRemovedList){
		if (softList == null){
			return Colors.SOFTPREC_COLOR;
		} else if (softList.size() > 1){
			return Colors.SOFTPREC_COLOR;
		} if (softRemovedList == null || softRemovedList.isEmpty()){
			return Colors.SOFTPREC_OLD_COLOR;
		} else {
			return Colors.SOFTPREC_COLOR;				
		}
	}
	protected String softPrecText(int i,int j,FullDomain softList,FullDomain softRemovedList,
			FullDomain hardList,FullDomain hardRemovedList){
		if (softList == null){
			return "";
		} else if (softList.size() > 1){
			return "01";
		} else if (softList.getIntValue() == 0){
			return "0";				
		} else if (softList.getIntValue() == 1){
			return "1";				
		} else {
			return "E";
		}
	}
	protected void drawComponentEntry(PrintWriter out,Pair xy,
			FullDomain list,FullDomain removed){
		out.println("<!-- index "+xy.getX() + " "+ xy.getY() +" domain "+list+" -->");
		Colors color;
		// this is a hack to recognize the first 6 components as special
		// and use their specific color
		switch(list.getIntValue()) {
		case 0: color = Colors.COMPONENT_COLOR0;
				break;
		case 1: color = Colors.COMPONENT_COLOR1;
				break;
		case 2: color = Colors.COMPONENT_COLOR2;
				break;
		case 3: color = Colors.COMPONENT_COLOR3;
				break;
		case 4: color = Colors.COMPONENT_COLOR4;
				break;
		case 5: color = Colors.COMPONENT_COLOR5;
				break;
		default: color = Colors.COMPONENT_COLOR;
				break;
		}
		double lineWidth;
		// draw new component entries with wider lines
		// the component number may have  changed, so don't compare the numbers
		if (removed == null || removed.size() == 0) {
			lineWidth = 0.2;
		} else {
			lineWidth = 0.1;
		}
		hollowRectSVG(out,posX(xy),posY(xy),1,1,color,lineWidth);
		lineSVG(out,posX(xy),posY(xy)+0.8,posX(xy)+1,posY(xy)+0.8,color);
	}
	
	@Override
	public InvariantType invariant(PrintWriter out, VisualState visualState) {
		FullDomainMap components = visualState.argument("components").asDomainMap();
		if (components.size() == 0) {
			// there are no components in this state
			// mark this state as interesting, to see if anything can be done here
			return InvariantType.INTERESTING;
		} else {
			return InvariantType.TRUE;
		}
	}

}
