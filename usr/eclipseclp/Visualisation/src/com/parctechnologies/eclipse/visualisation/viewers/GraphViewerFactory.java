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
import com.parctechnologies.eclipse.visualisation.VisClientStateModel;
import com.parctechnologies.eclipse.visualisation.ViewletFactory;
import com.parctechnologies.eclipse.visualisation.ViewerFactory;
import com.parctechnologies.eclipse.visualisation.ViewableType;
import com.parctechnologies.eclipse.visualisation.ElementType;
import com.parctechnologies.eclipse.visualisation.Viewer;
import com.parctechnologies.eclipse.visualisation.Viewable;
import com.parctechnologies.eclipse.visualisation.TableViewer;
import com.parctechnologies.eclipse.visualisation.GraphViewer;
import com.parctechnologies.eclipse.visualisation.MultiViewletType;
import com.parctechnologies.eclipse.visualisation.NumericBounds;

/**
 * Factory for building Lightweight TableViewers with TextViewlets in.
 */
public class GraphViewerFactory implements ViewerFactory
{
  private VisClientStateModel stateModel;
  private int type;
  private boolean zeroOne;
  private boolean capacity;
  private Class elementTypeClass;

  public GraphViewerFactory(VisClientStateModel stateModel,
                            int type,
                            boolean zeroOne, boolean capacity,
                            Class elementTypeClass)
  {
    this.stateModel = stateModel;
    this.type = type;
    this.zeroOne = zeroOne;
    this.capacity = capacity;
    this.elementTypeClass = elementTypeClass;
  }

  /**
   * Note that only the dimensionrestriction affects the ability of a
   * TextTableLight viewer to render a viewable
   */
  public boolean canBuildFrom(ViewableType viewableType)
  {
    if ( type == GraphViewer.DESKTOP_TYPE) {
      // desktop viewers work for anything
      return true;
    }
    ViewableType.ArrayType arrayType =
      (ViewableType.ArrayType)viewableType;
    // Network viewers require a 2D array with specific dimensions and
    // element types
    if(arrayType.getNDimensions() == 2 )
    {
      if (elementTypeClass.isInstance(arrayType.getElementType())) {
        return(true);
      }
    }
    return(false);
  }

  public Viewer build(Viewable viewable)
  {
      Collection typeCollection = new LinkedList();
      String changeable = null;
      ViewableType viewableType = viewable.getType();
      if (viewableType instanceof ViewableType.ArrayType) {
          ElementType elementType = 
              ((ViewableType.ArrayType)viewableType).getElementType();
          changeable = elementType.getChangeableSolver();
      }
      //typeCollection.add(new GrappaNodeViewletType());
      typeCollection.add(new GrappaTextViewletType(changeable,zeroOne,capacity));
      typeCollection.add(new TextViewletType(changeable));
      typeCollection.add(new FadeViewletType(changeable));

      if (viewableType instanceof ViewableType.ArrayType) {
        if (((ViewableType.ArrayType)viewableType).
            getElementType() instanceof NumericBounds) {
          // only add Bounds Viewlet Types if the viewable is an array,
          // and is known to contain numeric data
          typeCollection.add(new BoundsViewletType(changeable));
        }
      }
      MultiViewletType multiType = new MultiViewletType(typeCollection);
    Viewer newViewer = 
        new GraphViewer(multiType,
                        stateModel,
                        viewable,
                        type);
    newViewer.setDescription("Graph viewer");
    return(newViewer);
  }

}
