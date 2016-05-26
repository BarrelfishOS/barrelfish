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
// The Original Code is  The ECLiPSe Constraint Logic Programming System. 
// The Initial Developer of the Original Code is  Cisco Systems, Inc. 
// Portions created by the Initial Developer are
// Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): 
// 
// END LICENSE BLOCK

package com.parctechnologies.eclipse.visualisation.viewers;

import java.util.*;
import com.parctechnologies.eclipse.visualisation.ElementType;
import com.parctechnologies.eclipse.visualisation.VisClientStateModel;
import com.parctechnologies.eclipse.visualisation.ViewletFactory;
import com.parctechnologies.eclipse.visualisation.ViewerFactory;
import com.parctechnologies.eclipse.visualisation.ViewableType;
import com.parctechnologies.eclipse.visualisation.Viewer;
import com.parctechnologies.eclipse.visualisation.Viewable;
import com.parctechnologies.eclipse.visualisation.Chart2DViewer;

/**
 * Factory for building 2D Chart Viewers
 */
public class Chart2DViewerFactory implements ViewerFactory
{
  private VisClientStateModel stateModel;
  private int type;
  private Class elementTypeClass;

  public Chart2DViewerFactory(VisClientStateModel stateModel,
                            int type,
                            Class elementTypeClass)
  {
    this.stateModel = stateModel;
    this.type = type;
    this.elementTypeClass = elementTypeClass;
  }

  /**
   * Note that both the dimensions and type affect the ability of a
   * GanttViewer to be built.
   **/
  public boolean canBuildFrom(ViewableType viewableType)
  {
    ViewableType.ArrayType arrayType =
      (ViewableType.ArrayType)viewableType;
    // Gantt viewers require a 2D array with specific dimensions and
    // element types
    if (elementTypeClass.isInstance(arrayType.getElementType())) {
      return(true);
    }
    return(false);
  }

  public Viewer build(Viewable viewable)
  {
    String changeable = null;
    ViewableType viewableType = viewable.getType();
    if (viewableType instanceof ViewableType.ArrayType) {
      ElementType elementType = 
        ((ViewableType.ArrayType)viewableType).getElementType();
      changeable = elementType.getChangeableSolver();
    }
    Collection viewletTypeCollection = new LinkedList();
    viewletTypeCollection.add(new ChartBarViewletType(changeable));
    Viewer newViewer = 
      new Chart2DViewer(viewletTypeCollection,
                        stateModel,
                        viewable);
    newViewer.setDescription("2D Chart viewer");
    return(newViewer);
  }

}
