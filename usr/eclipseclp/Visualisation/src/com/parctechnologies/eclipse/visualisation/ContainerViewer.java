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

import com.parctechnologies.eclipse.*;
import java.util.*;
import javax.swing.*;
import java.awt.event.*;
import java.awt.Component;

/**
 * A ContainerViewer is a Viewer which contains zero or more "Viewlets" per
 * viewable element. A Viewlet is an object, representing the data stored in a ViewletStore, which contains information about the viewable element. The ContainerViewer class does not commit itself to how many
 * viewlets per element there are, what kind they are or how to store/access the
 * viewlets. These decisions are left to subclasses. <p>
 *
 * The way this is done is that the protected abstract methods
 * getViewletsAtIndex(List index) and getAllViewlets() must be implemented by
 * the subclass, and should return the relevant Collection of Viewlets.<p>
 *
 * However, ContainerViewer <em>is</em> responsible for mediating between
 * the event ViewerManager and the viewlets it contains. These responsibilities
 * are as follows:<p>
 * <ul>
 * <li>For CreateEvents and ExpandEvents, the subclass of ContainerViewer may
 * have created new viewlets during the invocation of the prepareForEvent
 * method. In the collectPreEventGoals method, it is the responsibility of
 * ContainerViewer to marshall these new
 * viewlets and collect from each a goal using its
 * collectPreBuildGoal method. All such goals are collected together in a batch
 * goal and returned.
 * <li>Similarly, for UpdateEvents, during the collectPreEventGoals,
 * ContainerViewer must marshall all the
 * Viewlets whose viewable elements are updating and collect a goal from each
 * using its collectPreUpdateGoal method. These goals are batched together in a
 * BatchGoal which is then returned.
 * <li>Following on from the above, during startEvent, the ContainerViewer is
 * responsible for distributing the results of the collected goals to the
 * viewlets who generated the goals, using their startBuild methods in the case
 * of CreateEvents and ExpandEvents and their startUpdate methods in the case of
 * UpdateEvents.
 * <li>The ContainerViewlet's implementation of stopEvent should invoke
 * the stopBuild methods of all newly created viewlets at the end of a
 * CreateEvent or an ExpandEvent and it should invoke the stopUpdate methods at
 * the end of an UpdateEvent.
 * <li>Between the execution of startEvent and the execution of stopEvent,
 * ContainerViewer is also responsible for providing protected access to
 * Collections of the indices of created/expanded/updating viewable elements,
 * and to Collections of created/expanded/updating viewlets.
 * <li>Each ContainerViewer also has a subset of its viewlets which are
 * currently "selected" in the sense that the next user action will apply to
 * them. The ContainerViewer is responsible for managing this collection. At
 * present this means having a protected selection member, initialising
 * the selection and remembering the selection during periods when the viewer
 * loses focus. However, I intend to move actions such as "select all" and
 * "select updating" up into this class along with other selection-related
 * functionality where possible.
 * </ul>
 * ContainerViewer is also responsible for keeping track of the Location names.
 */

public abstract class ContainerViewer extends ViewerImpl
{
  private ViewletRange createdElementIndices;
  private ViewletRange updatingElementIndices;
  private ViewletRange expandingElementIndices;
  //private Collection createdViewlets;
  //private Collection updatingViewlets;
  //private Collection expandingViewlets;
  private List size;
  private boolean trackUpdates = false;

  protected ViewletType viewletType;

    protected ViewletDataStore viewletDataStore;

  /**
   * Action to select all updating viewlets
   */
  private SelectUpdatingAction selectUpdatingAction;

  /**
   * The menu entry specifying whether to track updates or not
   */
  protected JCheckBoxMenuItem trackUpdatesMenuItem;

  private static final Atom vcSupportAtom = new Atom("vc_support");
    //protected Collection selectionCopy = new LinkedList();
    //protected ViewletRange selection = new ViewletRangeCollection();

  /**
   * Array of Lists of Strings. Each element in the array relates to a
   * dimension. Each List is a list of strings: location names for the
   * corresponding dimension. <p>
   * locationNames[0] is a List of location names for dimension 1 in the
   * viewable,
   * locationNames[1] is for dimension 2 etc.
   */
  private List[] locationNames;


  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("Select".equals(menuTitle)) return KeyEvent.VK_S;
    if ("View".equals(menuTitle)) return KeyEvent.VK_V;
    return super.getMenuMnemonic(menuTitle);
  }

  public ContainerViewer(VisClientStateModel stateModel,
                         Viewable viewable,
			 ViewletType viewletType)
  {
    super(stateModel, viewable);
    this.viewletType = viewletType;
    viewletType.setSymRef(new SymRef(viewletType,
                                     this.getSymRef(),
                                     viewletType.getClass().getName()));
    trackUpdatesMenuItem = new JCheckBoxMenuItem("Track updates");
    trackUpdatesMenuItem.
      setModel(new BooleanPropertyModel("trackUpdates",
					this, getPropertyChangeSupport()));
    addMenuItem("Options", trackUpdatesMenuItem);

    addMenuAndPopupMenuItem("Select", new SelectAllAction());
    selectUpdatingAction = new SelectUpdatingAction();
    addMenuAndPopupMenuItem("Select", selectUpdatingAction);
    addMenuAndPopupMenuItem("Select", new ClearSelectionAction());
    addMenuAndPopupMenuItem("View", getZoomInAction(this, 0.5f));
    addMenuAndPopupMenuItem("View", getZoomInAction(this, 2.0f));
    addMenuAndPopupMenuItem("View", null);
    addMenuAndPopupMenuItem("View", getZoomAction(this, 4.0f));
    addMenuAndPopupMenuItem("View", getZoomAction(this, 2.0f));
    addMenuAndPopupMenuItem("View", getZoomAction(this, 1.5f));
    addMenuAndPopupMenuItem("View", getZoomNormalAction(this));
    addMenuAndPopupMenuItem("View", getZoomAction(this, 0.75f));
    addMenuAndPopupMenuItem("View", getZoomAction(this, 0.5f));
    addMenuAndPopupMenuItem("View", getZoomAction(this, 0.25f));
    addMenuAndPopupMenuItem("View", getZoomToFitWidthAction());
    addMenuAndPopupMenuItem("View", getZoomToFitHeightAction());
  }

    public ViewletDataStore getViewletDataStore() {
	return viewletDataStore;
    }

  protected abstract Action getZoomToFitWidthAction();

  protected abstract Action getZoomToFitHeightAction();

  protected List getSize()
  {
    return(size);
  }

  public boolean getTrackUpdates()
  {
    return(trackUpdates);
  }


  public void setTrackUpdates(boolean newValue)
  {
    (new ViewerSetBooleanPropertyCommand(this, "trackUpdates", newValue)).issue();
  }

  public void setTrackUpdatesPrivate(boolean newValue)
  {
    boolean oldValue = trackUpdates;
    trackUpdates = newValue;
    this.getPropertyChangeSupport().
      firePropertyChange("trackUpdates", oldValue, newValue);
  }


  public BatchGoal collectPreEventGoals(VisEvent event)
  {
    BatchGoal result = new BatchGoal();

    if(event instanceof UpdateEvent)
    {
      result.add(super.collectPreEventGoals(event));
      setUpdatingElementIndices((UpdateEvent) event);
      result.add(collectPreUpdateGoals(updatingElementIndices,
                                          ((UpdateEvent) event)));
      return(result);
    }

    if(event instanceof ExpandEvent)
    {
      result.add(super.collectPreEventGoals(event));
      ExpandEvent expandEvent = (ExpandEvent) event;
      int expandingDimension = expandEvent.getExpandingDimension();
      Integer newLocation =
          (Integer) expandEvent.getViewableSize().get(expandingDimension - 1);
      // add a goal to get the name of the new location in the expanded dimension
      result.add(getAddLocationNameGoal(expandingDimension,
                                        newLocation.intValue()));
      setExpandingElementIndices(expandEvent);
      result.add(collectPreBuildGoals(expandingElementIndices));
      return(result);
    }

    if(event instanceof CreateEvent)
    {
      result.add(super.collectPreEventGoals(event));
      CreateEvent createEvent = (CreateEvent) event;
      // add a location names goal for each dimension.
      for(int i = 1 ; i <= createEvent.getViewableSize().size() ; i++)
      {
        result.add(getLocationNamesGoal(i));
      }
      setCreatedElementIndices(createEvent);
      DebuggingSupport.logMessage(this,"collectPreEventGoal fooee");
      result.add(collectPreBuildGoals(createdElementIndices));
      return(result);
    }
    return(super.collectPreEventGoals(event));
  }

  public void clearSelection()
  {
    setSelection(new ViewletRangeCollection());
  }

  public void setSelection(ViewletRange newSelection)
  {
      (new ViewerSetSelectionCommand(this, newSelection)).issue();
  }

//    public void addAllToSelection(Collection newViewlets)
//    {
//  //      HashSet newSelection = new HashSet(selection);
//  //      newSelection.addAll(newViewlets);
//  //      setSelection(newViewlets);
//    }

//    public void removeAllFromSelection(Collection viewletsToRemove)
//    {
//  //      HashSet newSelection = new HashSet(selection);
//  //      newSelection.removeAll(viewletsToRemove);
//  //      setSelection(newSelection);
//    }

//  //    public void addToSelection(Viewlet viewlet)
//  //    {
//  //  //      LinkedList single = new LinkedList();
//  //  //      single.add(viewlet);
//  //  //      addAllToSelection(single);
//  //    }

//  //    public void removeFromSelection(Viewlet viewlet)
//  //    {
//  //  //      LinkedList single = new LinkedList();
//  //  //      single.add(viewlet);
//  //  //      removeAllFromSelection(single);
//  //    }

    protected void setSelectionPrivate(ViewletRange newSelection) {
        // default to doing nothing
        // must be overidden by subclasses

        //      Collection viewletsToAdd = new LinkedList();
        //      Collection viewletsToRemove = new LinkedList();
        //      Iterator i1 = selection.iterator();
        //      Viewlet current;
        //      while(i1.hasNext())
        //      {
        //        current = (Viewlet) i1.next();
        //        if(!newSelection.contains(current))
        //        {
        //          viewletsToRemove.add(current);
        //        }
        //      }
        //      Iterator i2 = newSelection.iterator();
        //      while(i2.hasNext())
        //      {
        //        current = (Viewlet) i2.next();
        //        if(!selection.contains(current))
        //        {
        //          viewletsToAdd.add(current);
        //        }
        //      }
        //      addAllToSelectionPrivate(viewletsToAdd);
        //      removeAllFromSelectionPrivate(viewletsToRemove);
    }

//    private void addAllToSelectionPrivate(Collection all)
//    {
//  //      Iterator i = all.iterator();
//  //      while(i.hasNext())
//  //      {
//  //        addToSelectionPrivate((Viewlet) i.next());
//  //      }
//    }

//    private void removeAllFromSelectionPrivate(Collection all)
//    {
//      Iterator i = all.iterator();
//      while(i.hasNext())
//      {
//        removeFromSelectionPrivate((Viewlet) i.next());
//      }
//    }

//    private void addToSelectionPrivate(Viewlet viewlet)
//    {
//  //      selection.add(viewlet);
//  //      viewlet.setSelected(true);
//    }

//    private void removeFromSelectionPrivate(Viewlet viewlet)
//    {
//  //      selection.remove(viewlet);
//  //      viewlet.setSelected(false);
//    }

    public ViewletRange getSelection()
    {
        //return selection;
      return null;
    }

  private Collection collectPreBuildGoals(ViewletRange indexList)
  {
      //List goals =  new LinkedList();
      //goals.add(viewletType.collectPreBuildGoal(this,new ViewletRangeCollection(indexList)));
      List goals = viewletType.collectPreBuildGoal(this,
                                                   viewletDataStore,
                                                   indexList);
      DebuggingSupport.logMessage(this,"PreBuildGoals="+goals);
      return goals;
  }


  private Collection collectPreUpdateGoals(ViewletRange indexList, UpdateEvent event)
  {
      //List goals =  new LinkedList();
      //goals.add(viewletType.collectPreUpdateGoal(this,new ViewletRangeCollection(indexList), event));
      List goals = viewletType.collectPreUpdateGoal(this,
                                                    viewletDataStore,
                                                    indexList,
                                                    event);
      DebuggingSupport.logMessage(this,"PreUpdateGoals="+goals);
      return goals;
//      BatchGoal result = new BatchGoal();
//      Iterator indexListIterator = indexList.iterator();
//      List currentIndex;
//      Collection viewlets;

//      while(indexListIterator.hasNext())
//      {
//        currentIndex = (List) indexListIterator.next();
//        viewlets = getViewletDataAt(currentIndex);
//        result.add(composeElementUpdateGoal(viewlets, currentIndex, event));
//      }
//      return(result);
  }

//    private BatchGoal composeElementBuildGoal(Collection viewlets, List index)
//    {
//      BatchGoal result = new BatchGoal();
//      Object elementReference = new CompoundTermImpl("element", index);
//      Iterator viewletsIterator = viewlets.iterator();
//      ViewletData currentViewlet;
//      CompoundTerm viewletGoal;
//      Object viewableName = getViewable().getNameAtom();

//      while(viewletsIterator.hasNext())
//      {
//        currentViewlet = (ViewletData) viewletsIterator.next();
//        //viewletGoal = currentViewlet.collectPreBuildGoal();
//        //result.add(composeElementGoal(elementReference,
//        //                              viewletGoal));
//      }
//      return(result);
//    }

//    private BatchGoal composeElementUpdateGoal(Collection viewlets, List index, UpdateEvent event)
//    {
//      BatchGoal result = new BatchGoal();
//      Object elementReference = new CompoundTermImpl("element", index);
//      Iterator viewletsIterator = viewlets.iterator();
//      ViewletData currentViewlet;
//      CompoundTerm viewletGoal;

//      while(viewletsIterator.hasNext())
//      {
//        currentViewlet = (ViewletData) viewletsIterator.next();
//        //      viewletGoal = currentViewlet.collectPreUpdateGoal(event);
//        //      result.add(composeElementGoal(elementReference,
//        //                                    viewletGoal));
//      }
//      return(result);
//    }

//    protected CompoundTerm composeElementGoal(Object elementReference,
//  					    CompoundTerm viewletGoal)
//    {
//      return(new CompoundTermImpl(":", vcSupportAtom,
//                    new CompoundTermImpl("viewable_element_execute",
//                      getViewable().getNameAtom(), elementReference, viewletGoal)));
//    }

  private void setCreatedElementIndices(CreateEvent event)
  {
    List size = event.getViewableSize();
    // create an index representing the first element in the viewable
    List start = new ArrayList(size.size());
    Integer one = new Integer(1);
    for(int i = 0; i < size.size(); i++) {
        start.add(one);
    }
    //    createdElementIndices = allCombinations(size);
    createdElementIndices = viewletDataStore.createRange(start,size);
    if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "setCreatedElementIndices event="+event+
                                    " start=" + start + " size=" +size +
                                    "createdElementIndices=" +
                                    createdElementIndices);
    }
  }

  private void setExpandingElementIndices(ExpandEvent event)
  {
    int expandingDimension = event.getExpandingDimension();
    List size = event.getViewableSize();
    Integer newDimensionSize
      = (Integer) size.get(expandingDimension-1);//list indices start at 0

    // create an index representing the first element in the newly
    // expanded dimension
    List start = new ArrayList(size.size());
    Integer one = new Integer(1);
    for(int i = 0; i < size.size(); i++) {
        start.add(one);
    }
    start.set(expandingDimension-1, newDimensionSize);
    expandingElementIndices = viewletDataStore.createRange(start, size);
  }

  private void setUpdatingElementIndices(UpdateEvent event)
  {
    updatingElementIndices =
        viewletDataStore.createRange(event.getElementsUpdating());
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "setUpdatingElementIndices "+
                                  "event="+event+
                                  "event.getElementsUpdating()="+event.getElementsUpdating()+
                                  "updatingElementIndices="+updatingElementIndices);
    }
  }


  protected ViewletRange getCreatedElementIndices()
  {
    return createdElementIndices;
  }

  protected ViewletRange getUpdatingElementIndices()
  {
    return updatingElementIndices;
  }

  protected ViewletRange getExpandingElementIndices()
  {
    return expandingElementIndices;
  }

  public void gainFocus()
  {
    super.gainFocus();
    //    this.setSelection(selectionCopy);
    getComponent().repaint();

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "focus gained");
    }

  }

  public void loseFocus()
  {
    super.loseFocus();
//      try {
//  	selectionCopy = (ViewletSelection)(getSelection().clone());
//      } catch(CloneNotSupportedException e) {
//        if (DebuggingSupport.logMessages) {
//  	DebuggingSupport.logMessage(this, "clone not supported");
//  	e.printStackTrace(System.err);
//        }
//        //selectionCopy = new LinkedList(getSelection());
//      }
    clearSelection();

    getComponent().repaint();

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "focus lost");
    }

  }

    //protected abstract ViewletTracker getViewletTracker();

  private void scrollToTrackUpdate(Collection updatingViewlets)
  {
//      ViewletTracker viewletTracker = getViewletTracker();
//      viewletTracker.setViewlets(getUpdatingViewlets());
//      try {
//        SwingUtilities.invokeAndWait(viewletTracker);
//      } catch(Exception e) { throw new RuntimeException("Exception "+e+" thrown "+
//            "while executing viewletTracker");}
  }

  public void startEvent(VisEvent event, java.util.List goalResults)
  {
    if(event instanceof UpdateEvent)
    {
      super.startEvent(event, (List) goalResults.get(0));
      distributeUpdateResults((List) goalResults.get(1), updatingElementIndices,
                                ((UpdateEvent) event));
      // When starting an update event, enable the selectUpdating action
      // and track the update if this is switched on.
      selectUpdatingAction.setEnabled(true);
//        if(trackUpdates)
//        {
//          scrollToTrackUpdate(getUpdatingViewlets());
//        }

      return;
    }
    else
    {
      // can't select updating viewlets unless update event is in progress
      // this line is possibly unnecessary (why would the action be enabled?).
      selectUpdatingAction.setEnabled(false);
    }


    if(event instanceof ExpandEvent)
    {
      size = ((ExpandEvent) event).getViewableSize();
      super.startEvent(event, (List) goalResults.get(0));
      String newLocationName =
        locationNameFromGoalResult((CompoundTermImpl) goalResults.get(1));
      int dimNumber = ((ExpandEvent) event).getExpandingDimension();
      locationNames[dimNumber - 1].add(newLocationName);

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "Added location name "+
                                      newLocationName+
                                      " to dimension "+dimNumber);
      }

      distributeBuildResults((List) goalResults.get(2), expandingElementIndices);
      return;
    }


    if(event instanceof ContractEvent)
    {
      size = ((ContractEvent) event).getViewableSize();
      shrinkLocationNamesTo(size);
    }

    if(event instanceof CreateEvent)
    {
      size = ((CreateEvent) event).getViewableSize();
      super.startEvent(event, (List) goalResults.get(0));
      // extract the results of the "getDimension Location names" goals
      int nDims = size.size();
      locationNames = new List[nDims];
      List locationNamesList;
      int i;
      for(i = 1 ; i <= nDims ; i++)
      {
        locationNamesList =
          locationNamesFromGoalResult((CompoundTermImpl) goalResults.get(i));
        locationNames[i-1] = locationNamesList;
      }
      // distribute the "preBuild" goal results
      distributeBuildResults((List) goalResults.get(i), createdElementIndices);
      return;
    }
    super.startEvent(event, goalResults);
  }

  public Collection getCreatedViewlets()
  {
//      if(createdViewlets == null)
//      {
//        createdViewlets = getViewletsAtIndexList(getCreatedElementIndices());
//      }
//      return(createdViewlets);
      return null;
  }

  public Collection getUpdatingViewlets()
  {
//      if(updatingViewlets == null)
//      {
//        updatingViewlets = getViewletsAtIndexList(getUpdatingElementIndices());
//      }
//      return(updatingViewlets);
      return null;
  }

  public Collection getExpandingViewlets()
  {
//      if(expandingViewlets == null)
//      {
//        expandingViewlets = getViewletsAtIndexList(getExpandingElementIndices());
//      }
//      return(expandingViewlets);
      return null;
  }

  private Collection getViewletDataAtIndexList(Collection indexList)
  {
    Iterator indexListIterator = indexList.iterator();
    List currentIndex;
    Collection viewlets;
    Collection result = new LinkedList();
    while(indexListIterator.hasNext())
    {
      currentIndex = (List) indexListIterator.next();
      viewlets = getViewletDataAt(currentIndex);
      result.addAll(viewlets);
    }
    return(result);
  }

  public boolean shouldHold()
  {
    if(super.shouldHold())
    {
      return(true);
    }
    if(getCurrentEvent() instanceof UpdateEvent)
    {
	//      Collection updatingViewlets = getUpdatingViewlets();
      Iterator updatingViewletsIterator =
	  viewletDataStore.getViewletDataIterator(new ViewletRangeCollection(updatingElementIndices));
      ViewletData currentViewlet;
      while(updatingViewletsIterator.hasNext())
      {
        currentViewlet = (ViewletData) updatingViewletsIterator.next();
        if(currentViewlet.getHoldsOnUpdates())
        {
          return(true);
        }
      }
    }
    return(false);
  }

  private void distributeBuildResults(List results, ViewletRange indexList)
  {
      viewletType.startBuild(this, viewletDataStore, indexList, results);
      /*
    Iterator indexListIterator = indexList.iterator();
    Iterator resultsIterator = results.iterator();
    List elementResult;
    Collection viewlets;
    List currentIndex;
    while(indexListIterator.hasNext())
    {
      currentIndex = (List) indexListIterator.next();
      viewlets = getViewletDataAt(currentIndex);
      elementResult = (List) resultsIterator.next();
      distributeBuildResult(elementResult, viewlets);
    }
      */
  }

//    private void distributeBuildResult(List elementResults, ViewletRange viewlets)
//    {
//      Iterator elementResultsIterator = elementResults.iterator();
//      Iterator viewletsIterator = viewlets.iterator();
//      ViewletData currentViewlet;
//      CompoundTermImpl currentResult;
//      Object next;
//      while(elementResultsIterator.hasNext())
//      {
//        currentViewlet = (ViewletData) viewletsIterator.next();
//        next = elementResultsIterator.next();
//        //
//        if (DebuggingSupport.logMessages) {
//  	  DebuggingSupport.logMessage(this, "currentViewlet:"+currentViewlet);
//  	  DebuggingSupport.logMessage(this, "next:"+next);
//        }

//        //currentResult = (CompoundTermImpl) next;
//        //currentViewlet.startBuild((CompoundTerm) (currentResult.argCT(2).arg(3)));
//      }
//    }

  private void distributeUpdateResults(List results,
                                       ViewletRange indexList,
                                       UpdateEvent event)
  {
      viewletType.startUpdate(this,
                              viewletDataStore,
                              indexList,
                              results,
                              event);
      /*
    Iterator indexListIterator = indexList.iterator();
    Iterator resultsIterator = results.iterator();
    List elementResult;
    Collection viewlets;
    List currentIndex;
    while(indexListIterator.hasNext())
    {
      currentIndex = (List) indexListIterator.next();
      viewlets = getViewletDataAt(currentIndex);
      elementResult = (List) resultsIterator.next();
      distributeUpdateResult(elementResult, viewlets, event);
    }
      */
  }

//    private void distributeUpdateResult(List elementResults, Collection viewlets,
//                                        UpdateEvent event)
//    {
//      Iterator elementResultsIterator = elementResults.iterator();
//      Iterator viewletsIterator = viewlets.iterator();
//      ViewletData currentViewlet;
//      CompoundTermImpl currentResult;
//      while(elementResultsIterator.hasNext())
//      {
//        currentViewlet = (ViewletData) viewletsIterator.next();
//        currentResult = (CompoundTermImpl) elementResultsIterator.next();
//        //currentViewlet.startUpdate(event, (CompoundTerm) currentResult.argCT(2).arg(3));
//      }
//    }

  public void stopEvent()
  {
    if(getCurrentEvent() instanceof CreateEvent)
    {
      stopBuild(createdElementIndices);
    }

    if(getCurrentEvent() instanceof ExpandEvent)
    {
      stopBuild(expandingElementIndices);
    }

    if(getCurrentEvent() instanceof UpdateEvent)
    {
      stopUpdate(updatingElementIndices);
      selectUpdatingAction.setEnabled(false);
    }

    createdElementIndices = null;
    updatingElementIndices = null;
    expandingElementIndices = null;
    //createdViewlets = null;
    //updatingViewlets = null;
    //expandingViewlets = null;
    super.stopEvent();
  }

  private void stopBuild(ViewletRange indexList)
  {
    Iterator indexListIterator = indexList.iterator();
    List currentIndex;
    Iterator viewletIterator;
    ViewletData currentViewlet;

    while(indexListIterator.hasNext())
    {
      currentIndex = (List) indexListIterator.next();
      viewletIterator = getViewletDataAt(currentIndex).iterator();
      while(viewletIterator.hasNext())
      {
        currentViewlet = (ViewletData) viewletIterator.next();
        //currentViewlet.stopBuild();
      }
    }
  }

  private void stopUpdate(ViewletRange indexList)
  {
      viewletType.stopUpdate(this, viewletDataStore, indexList);
//      Iterator indexListIterator = indexList.iterator();
//      List currentIndex;
//      Iterator viewletIterator;
//      ViewletData currentViewlet;

//      while(indexListIterator.hasNext())
//      {
//        currentIndex = (List) indexListIterator.next();
//        viewletIterator = getViewletDataAt(currentIndex).iterator();
//        while(viewletIterator.hasNext())
//        {
//          currentViewlet = (ViewletData) viewletIterator.next();
//          //currentViewlet.stopUpdate();
//        }
//      }
  }

  protected abstract Collection getViewletDataAt(List index);
  protected abstract ViewletRange getAllViewletData();


  /**
   * dimNumber starts from 1.
   */
  protected List getLocationNames(int dimNumber)
  {
    return(locationNames[dimNumber - 1]);
  }

  /**
   * dimNumber & locNumber start from 1.
   */
  protected String getLocationName(int dimNumber, int locNumber)
  {
    return((String) getLocationNames(dimNumber).get(locNumber - 1));
  }

  private void shrinkLocationNamesTo(List newSize)
  {
    for(int i = 0; i < locationNames.length; i++)
    {
      int newDimSize = ((Integer) newSize.get(i)).intValue();
      List newLocationNames = locationNames[i].subList(0, newDimSize);
      locationNames[i] = newLocationNames;

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "location names of dimension "+(i+1)+
				      " shrunk to "+newLocationNames);
      }

    }
  }


  private CompoundTerm getLocationNamesGoal(int dimNumber)
  {
    CompoundTerm locationNamesGoal =
        new CompoundTermImpl(":", vcSupportAtom,
          new CompoundTermImpl("viewable_get_location_names",
                                 getViewable().getNameAtom(),
                                 new Integer(dimNumber), null));
    return(locationNamesGoal);
  }

  private CompoundTerm getAddLocationNameGoal(int dimNumber, int newLocation)
  {
    CompoundTerm addLocationNameGoal =
        new CompoundTermImpl(":", vcSupportAtom,
          new CompoundTermImpl("viewable_get_location_name",
                                 getViewable().getNameAtom(),
                                 new Integer(dimNumber),
                                 new Integer(newLocation),
                                 null));
    return(addLocationNameGoal);
  }

  private List locationNamesFromGoalResult(CompoundTermImpl result)
  {
    return((List) result.argCT(2).arg(3));
  }

  private String locationNameFromGoalResult(CompoundTermImpl result)
  {
    return((String) result.argCT(2).arg(4));
  }

  private abstract class ViewletSelectionAction extends AbstractAction
  {
    protected abstract ViewletRange newSelection();

    ViewletSelectionAction(String text)
    {
      super(text);
    }
    public void actionPerformed(ActionEvent event)
    {
      clearSelection();
      //addAllToSelection(newSelection());
      setSelection(newSelection());
      getComponent().repaint();
    }
  }

  private class SelectAllAction extends ViewletSelectionAction
  {
    SelectAllAction()
    {
      super("Select all viewlets");
    }
    protected ViewletRange newSelection()
    {
      return(viewletDataStore.getEntireViewletRange());
    }
  }

  private class SelectUpdatingAction extends ViewletSelectionAction
  {
    SelectUpdatingAction()
    {
      super("Select updating viewlet(s)");
      setEnabled(false);
    }
    protected ViewletRange newSelection()
    {
      return getUpdatingElementIndices();
    }
  }

  private class ClearSelectionAction extends ViewletSelectionAction
  {
    ClearSelectionAction()
    {
      super("Clear selection");
    }
    protected ViewletRange newSelection()
    {
      return new ViewletRangeCollection();
    }
  }

  protected Action getZoomInAction(Viewer viewer, float ratio)
  {
    return(new ZoomInAction(viewer, ratio));
  }

  protected Action getZoomAction(Viewer viewer, float zoomRatio)
  {
    return(new ZoomAction(viewer, zoomRatio));
  }

  protected Action getZoomNormalAction(Viewer viewer)
  {
    return(new ZoomNormalAction(viewer));
  }

  private class ZoomAction extends AbstractAction
  {
    private float zoomRatio;
    private Viewer viewer;

    ZoomAction(Viewer viewer, float zoomRatio)
    {
      super("Zoom to "+((int)(zoomRatio*100))+"%");
      this.zoomRatio = zoomRatio;
      this.viewer = viewer;
    }
    public void actionPerformed(ActionEvent event)
    {
      (new ViewerZoomCommand(viewer, zoomRatio)).issue();
    }
  }

  private class ZoomInAction extends AbstractAction
  {
    private float zoomRatio;
    private Viewer viewer;

    ZoomInAction(Viewer viewer, float zoomRatio)
    {
      super("Zoom "+((zoomRatio<1)?"out":"in")+" by "+
            ((int)(((zoomRatio<1)?(1/zoomRatio):zoomRatio)*100))+"%");
      this.zoomRatio = zoomRatio;
      this.viewer = viewer;
    }
    public void actionPerformed(ActionEvent event)
    {
      (new ViewerZoomInCommand(viewer, zoomRatio)).issue();
    }
  }

  private class ZoomNormalAction extends ZoomAction
  {
    ZoomNormalAction(Viewer viewer)
    {
      super(viewer, 1.0f);
      this.putValue(Action.NAME, "Zoom to normal size");
    }
  }

  /** Returns any actions/menu items that should be available via a
      right-click popup menu */
  public Collection getViewerPopupMenuCollection() {
    Collection result = new LinkedList();
    result.add(getPopupMenu("Select"));
    result.add(getPopupMenu("View"));
    return result;
  }


  /** Access the context sensitive popup menu */
  public JPopupMenu getPopupMenu() {
    JPopupMenu menu = super.getPopupMenu();
    // Add action specific to the current selection
    ViewletRange range = getSelection();
    if (range != null) {
      for(Iterator it = viewletType.getActions(viewletDataStore, range).iterator();
          it.hasNext();) {
        menu.add((Action)(it.next()));
      }
    }
    // Add actions specific to this viewer
    menu.addSeparator();
    for(Iterator it = getViewerPopupMenuCollection().iterator();
        it.hasNext();) {
      Object item = it.next();
      if (item instanceof Action) {
        menu.add((Action)(item));
      } else if (item instanceof JMenuItem) {
        menu.add((JMenuItem)(item));
      } else if (item instanceof Component) {
        menu.add((Component)(item));
      } else if (item instanceof String) {
        menu.add((String)(item));
      } else if (item instanceof JMenu) {
        menu.add((JMenu)item);
      }
    }
    return menu;
  }
}
