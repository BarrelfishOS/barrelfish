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
 * Container viewer containing one viewlet per element. Stored in a
 * multidimensional, potentially flexible table
 */
package com.parctechnologies.eclipse.visualisation;
import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.viewers.*;
import java.util.*;

/**
 * A ViewletArrayViewer is a kind of ContainerViewer which makes certain
 * commitments about the viewlets it contains. In particular:
 * <ul>
 * <li>The ViewletArrayViewer has precisely one viewlet per viewable element.
 * <li>When the initial set of viewlets is created and when new viewlets are
 * created as a result of the viewable expanding, the ViewletArrayViewer uses
 * a ViewletFactory to produce the viewlets.
 * <li> The ViewletArrayViewer stores its viewlets in a ViewletArray, which is
 * a recursive data structure that reflects the structure of the viewable on
 * the ECLiPSe side.
 * </ul><p>
 * The ViewletArrayViewer is also responsible for collecting goals and
 * distributing results relating to the location names. These are stored in the
 * viewletArray.
 */
public abstract class ViewletArrayViewer extends ContainerViewer
{
  private ViewletFactory viewletFactory;

  protected ViewletRange previousUpdatingElementIndices;

  private final Atom vcSupportAtom = new Atom("vc_support");

//    protected ViewletArray getViewletArray()
//    {
//        //return(viewletArray);
//      return((ViewletArray)getViewletDataStore());
//    }

  protected ViewletFactory getViewletFactory()
  {
    return(viewletFactory);
  }

  public void setViewletFactory(ViewletFactory viewletFactory)
  {
    this.viewletFactory = viewletFactory;
  }


  public void setViewletType(ViewletType viewletType)
  {
      this.viewletType = viewletType;
      setViewletFactory((ViewletFactory)viewletType);
  }


    public ViewletArrayViewer(ViewletType viewletType,
			      VisClientStateModel stateModel,
			      Viewable viewable)
  {
    super(stateModel, viewable, viewletType);
    setViewletFactory((ViewletFactory)viewletType);
    previousUpdatingElementIndices = null;
  }

  public ViewletData getViewletAt(List index)
  {
    return viewletDataStore.getViewletDataAt(index);
  }

  /**
   * This implements the abstract getViewletsAt method required by the
   * ContainerViewer class.
   */
  public Collection getViewletDataAt(List index)
  {
    Collection result = new LinkedList();
    result.add(getViewletAt(index));
    return(result);
  }

  public void prepareForEvent(VisEvent event)
  {
    if(event instanceof CreateEvent)
    {
      prepareForCreate((CreateEvent) event);
    }
    if(event instanceof ExpandEvent)
    {
      prepareForExpand((ExpandEvent) event);
    }

    super.prepareForEvent(event);
  }


  public void startEvent(VisEvent event, java.util.List goalResults)
  {
    // for updateevent, fire the TableModel methods to indicate that
    // the data model has changed.  NOTE: It must be fired for those
    // cells which are being updated by the event AND for those cells
    // which WERE updated by the previous update event (so as to
    // record the fact that their 'updating' state has changed.
    if(event instanceof UpdateEvent)
    {
      viewletDataStore.fireViewletRangeUpdated(previousUpdatingElementIndices);
      super.startEvent(event, goalResults);
      viewletDataStore.fireViewletRangeUpdated(getUpdatingElementIndices());
      // remember these cells for next time
      previousUpdatingElementIndices = getUpdatingElementIndices();
      return;
    }


    // for create event, set the location names
    // obtained by the pre-event goal which will have been posted.
    if(event instanceof CreateEvent)
    {
      super.startEvent(event, goalResults);
      for(int i = 1 ; i <= viewletDataStore.getNDimensions(); i++)
      {
        viewletDataStore.setLocationNames(i, getLocationNames(i));
      }
      return;
    }

    // for expand event, add the location name of the new location
    // obtained by the pre-event goal which will have been posted.
    if(event instanceof ExpandEvent)
    {
      super.startEvent(event, goalResults);
      int expandingDimension = ((ExpandEvent) event).getExpandingDimension();
      viewletDataStore.finishExpandDimension(expandingDimension);
      return;
    }


    super.startEvent(event, goalResults);

  }


  public void stopEvent()
  {
    if(this.getCurrentEvent() instanceof ContractEvent)
    {
      stopContract((ContractEvent) this.getCurrentEvent());
    }    
    super.stopEvent();
  }

  /**
   * To prepare for the creation event we initialise the viewlet array
   */
  private void prepareForCreate(CreateEvent createEvent)
  {
      viewletDataStore =
          new ViewletArray(createEvent.getViewableSize(),
                           ((ViewableType.ArrayType)createEvent.getViewableType()).getFixityList(),
                           getViewable(),
                           viewletFactory);
      viewletDataStore.setSymRef(new SymRef(viewletDataStore,
                                            this.getSymRef(),
                                            "store"));
  }

  /**
   * To prepare for the expand event we forward the expansion to the viewlet
   * array, so that it can create the new viewlets from which we extract
   * preBuild goals.
   */
  private void prepareForExpand(ExpandEvent expandEvent)
  {
    viewletDataStore.startExpandDimension(expandEvent.getExpandingDimension());
  }

  /**
   * At the end of a contract event we contract the viewlet array to the new
   * size.
   */
  private void stopContract(ContractEvent contractEvent)
  {
    viewletDataStore.shrinkTo(contractEvent.getViewableSize());
  }

  /**
   * This implements the abstract getViewletsAt method required by the
   * ContainerViewer class.
   */
  protected ViewletRange getAllViewletData()
  {
    return(viewletDataStore.getEntireViewletRange());
  }
}
