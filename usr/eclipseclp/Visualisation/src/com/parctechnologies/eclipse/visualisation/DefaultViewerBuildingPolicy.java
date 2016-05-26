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

package com.parctechnologies.eclipse.visualisation;
import com.parctechnologies.eclipse.visualisation.viewers.*;
import java.util.*;

/**
 * The default ViewerBuildingPolicy can build text-table viewers and bounds-
 * table viewers. Its policy is to build a bounds-table viewer if posible and
 * a text-table viewer otherwise.
 */
class DefaultViewerBuildingPolicy implements ViewerBuildingPolicy
{
  private Map factories ;
  private Map enabled ;
  private VisClientStateModel stateModel ;

  DefaultViewerBuildingPolicy(VisClientStateModel stateModel)
  {
    this.stateModel = stateModel ;
    factories = new HashMap();
    enabled = new HashMap();
    factories.put("TextTable", new TableViewerFactory(stateModel, TextViewletType.class, AnyElementType.class));
    enabled.put("TextTable", new Boolean(true));
    factories.put("FadeTable", new TableViewerFactory(stateModel, FadeViewletType.class, AnyElementType.class));
    enabled.put("FadeTable", new Boolean(true));
    factories.put("BoundsTable", new TableViewerFactory(stateModel, BoundsViewletType.class, NumericBounds.class));
    enabled.put("BoundsTable", new Boolean(true));
//      factories.put("TextTableLight", new TextTableLightViewerFactory(stateModel));
//      enabled.put("TextTableLight", new Boolean(true));
//      factories.put("FadeTable", new FadeTableViewerFactory(stateModel));
//      enabled.put("FadeTable", new Boolean(true));
    factories.put("Network", new GraphViewerFactory(stateModel,
                                                    GraphViewer.NETWORK_TYPE,
                                                    false,false,
                                                    GraphData.class));
    enabled.put("Network", new Boolean(true));
    factories.put("Network (0/1)", new GraphViewerFactory(stateModel,
                                                    GraphViewer.NETWORK_TYPE,
                                                    true,false,
                                                    GraphData.class));
    enabled.put("Network (0/1)", new Boolean(true));
    factories.put("Network (capacity)",
                  new GraphViewerFactory(stateModel,
                                         GraphViewer.NETWORK_TYPE,
                                         false,true,
                                         GraphData.class));
    enabled.put("Network (capacity)", new Boolean(true));
    factories.put("Desktop", new GraphViewerFactory(stateModel,
                                                    GraphViewer.DESKTOP_TYPE,
                                                    false,false,
                                                    AnyElementType.class));
    enabled.put("Desktop", new Boolean(true));
    factories.put("Gantt Chart", new GanttViewerFactory(stateModel,
                                                        GraphViewer.DESKTOP_TYPE,
                                                        NumericBounds.class));
    enabled.put("Gantt Chart", new Boolean(true));
    factories.put("2D Chart", new Chart2DViewerFactory(stateModel,
                                                       GraphViewer.DESKTOP_TYPE,
                                                       NumericBounds.class));
    enabled.put("2D Chart", new Boolean(true));
    //factories.put("FadeTableLight", new FadeTableViewerFactory(stateModel));
    //enabled.put("FadeTableLight", new Boolean(true));
  }

  public Set availableViewerFactories(ViewableType viewableType) {
    Set result = new TreeSet();
    for(Iterator it = factories.keySet().iterator(); it.hasNext(); ) {
      Object key = it.next();
      ViewerFactory vf = (ViewerFactory)factories.get(key);
      if (vf.canBuildFrom(viewableType)) {
        result.add(key);
      }
    }
    return result;
  }

  public void enableViewers(List keys) {
      for(Iterator it = factories.keySet().iterator(); it.hasNext(); ) {
	  Object key = it.next();
	  if (keys.contains(key)) {
	      enabled.put(key, new Boolean(true));
	  } else {
	      enabled.put(key, new Boolean(false));
	  }
      }
      stateModel.setViewerBuildingPolicySelected(true);
  }

  public List buildViewers(Viewable viewable)
  {
    List newViewers = new LinkedList();
    for(Iterator it = availableViewerFactories(viewable.getType()).iterator(); it.hasNext(); ) {
      Object key = it.next();
      ViewerFactory vf = (ViewerFactory)factories.get(key);
      if ( ((Boolean)enabled.get(key)).booleanValue() &&
	   vf.canBuildFrom(viewable.getType())) {
	newViewers.add(vf.build(viewable));
      }
    }
    return(newViewers);
  }
}
