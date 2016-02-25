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

/**
 * A viewer command which applies to a number of viewlets.
 */

package com.parctechnologies.eclipse.visualisation;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Iterator;

public abstract class ViewerViewletsCommand extends ViewerCommand{

  private Collection viewletSymRefs;

  public ViewerViewletsCommand(ContainerViewer viewer,
                               Collection viewlets) {
      super(viewer);
      viewletSymRefs = new LinkedList();
      Iterator i = viewlets.iterator();
      while(i.hasNext())
      {
        viewletSymRefs.add(((Viewlet) i.next()).getSymRef());
      }
  }

  protected Collection getViewlets()
  {
      Collection viewlets = new LinkedList();
      Iterator i = viewletSymRefs.iterator();
      while(i.hasNext())
      {
        viewlets.add(SymRef.get((SymRef) i.next()));
      }
      return(viewlets);
  }

  protected ContainerViewer getContainerViewer()
  {
    return((ContainerViewer) getViewer());
  }

}
