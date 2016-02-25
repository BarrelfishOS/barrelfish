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

import com.parctechnologies.eclipse.visualisation.VisClientStateModel;
import com.parctechnologies.eclipse.visualisation.ViewletFactory;
import com.parctechnologies.eclipse.visualisation.ViewletType;
import com.parctechnologies.eclipse.visualisation.ViewerFactory;
import com.parctechnologies.eclipse.visualisation.ViewableType;
import com.parctechnologies.eclipse.visualisation.ElementType;
import com.parctechnologies.eclipse.visualisation.Viewer;
import com.parctechnologies.eclipse.visualisation.Viewable;
import com.parctechnologies.eclipse.visualisation.TableViewer;
import com.parctechnologies.eclipse.visualisation.DebuggingSupport;

/**
 * Factory for building Lightweight TableViewers with specified viewlets
 */
public class TableViewerFactory implements ViewerFactory
{
  private VisClientStateModel stateModel;
  private Class viewletTypeClass;
  private Class elementTypeClass;

  public TableViewerFactory(VisClientStateModel stateModel,
                            Class viewletTypeClass,
                            Class elementTypeClass)
  {
    this.stateModel = stateModel;
    this.viewletTypeClass = viewletTypeClass;
    this.elementTypeClass = elementTypeClass;
  }

  /**
   * Note that only the dimensionrestriction affects the ability of a
   * TextTableLight viewer to render a viewable
   */
  public boolean canBuildFrom(ViewableType viewableType)
  {
    if (!(viewableType instanceof ViewableType.ArrayType)) {
      return false;
    }
    ViewableType.ArrayType arrayType = (ViewableType.ArrayType) viewableType;
    if(arrayType.getNDimensions() >= 1 &&
       arrayType.getNDimensions() < 3 )
    {
      if (elementTypeClass.isInstance(arrayType.getElementType())) {
        // can the viewletType render this elementType
        return(true);
      }
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
    Class argClasses[] = {String.class};
    Object argInstances[] = {changeable};
    ViewletType viewletType;
    try {
      viewletType = (ViewletType)(viewletTypeClass.getConstructor(argClasses).newInstance(argInstances));
    } catch(Exception e) {
      if (DebuggingSupport.logMessages) {
        e.printStackTrace(System.err);
      }
      throw new RuntimeException("Unable to construct viewlet type in TableViewerFactory:"+e);
    }
    Viewer newViewer = 
      new TableViewer(viewletType, stateModel, viewable);
    newViewer.setDescription(viewletType.getDescription()+" table viewer");
    return(newViewer);
  }

}
