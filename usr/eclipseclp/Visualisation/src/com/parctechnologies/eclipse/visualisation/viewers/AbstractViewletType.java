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

import com.parctechnologies.eclipse.*;
import com.parctechnologies.eclipse.visualisation.*;

import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.*;
import java.awt.Color;
import java.awt.Component;
import java.util.*;
import java.awt.event.ActionEvent;
import javax.swing.*;
import javax.swing.table.*;
import att.grappa.*;


/**
 * Contains default implementations of the ViewletType interface
 */
public abstract  class AbstractViewletType implements ViewletType, ViewletFactory {
  private SymRef symRef;

  protected final static String ELEMENT_EXECUTE_STRING = "viewable_element_execute";
  protected final static String CHANGEABLE_ELEMENT_EXECUTE_STRING = "viewable_changeable_execute";
  protected String changeableSolver;

  public AbstractViewletType(String changeable) {
    changeableSolver = changeable;
  }
    

  /* ViewletFactory methods */    
  public boolean canBuildFrom(ElementType elementType)
  {
    return(true);
  }
    
  abstract public ViewletData build();
    
  abstract public String getDescription() ;
    
  public void setSymRef(SymRef symRef)
  {
    this.symRef = symRef;
  }
  public SymRef getSymRef()
  {
    return(symRef);
  }

  /* ViewletType methods */
  abstract public TableCellRenderer getTableCellRenderer();

  abstract Class getCustomRendererClass();

  abstract public void customizeElement(ViewletDataStore store,
                                        java.util.List index,
                                        Element element);


  /**
   * Can be used to wrap a goal in either a "viewable_element_execute"
   * or "viewable_changeable_execute" goal, which will replace
   * occurences of the term "element(Index)" with the associated
   * viewable element or changeable value.
   **/
  protected CompoundTerm composeElementGoal(Object elementReference,
                                            CompoundTerm viewableName,
                                            CompoundTerm viewletGoal)
  {
    if (changeableSolver==null) {
      return(new CompoundTermImpl(":", new Atom("vc_support"),
                                  new CompoundTermImpl(ELEMENT_EXECUTE_STRING,
                                                       viewableName, new CompoundTermImpl("element",elementReference), viewletGoal)));
    } else {
      return(new CompoundTermImpl(":", new Atom("vc_support"),
                                  new CompoundTermImpl(CHANGEABLE_ELEMENT_EXECUTE_STRING,
                                                       viewableName, new CompoundTermImpl("element",elementReference), viewletGoal,new Atom(changeableSolver))));
    }
  }

  /**
   * Can be used to remove the wrapping applied by composeElementGoal
   **/
  protected CompoundTerm decomposeElementGoal(CompoundTerm viewletGoal)
  {
    return ((CompoundTermImpl)viewletGoal).argCT(2).argCT(3);
  }


  public BatchGoal collectPreBuildGoal(Viewer viewer,
                                       ViewletDataStore store,
                                       ViewletRange range) {
    return null;
  }

  public void startBuild(Viewer viewer,
                         ViewletDataStore store,
                         ViewletRange range,
                         List results) {
  }

  public void stopBuild()
  {}

  public BatchGoal collectPreUpdateGoal(Viewer viewer,
                                        ViewletDataStore store,
                                        ViewletRange range,
                                        UpdateEvent updateEvent)
  {
    return null;
  }
  public void startUpdate(Viewer viewer,
                          ViewletDataStore store,
                          ViewletRange range,
                          List results,
                          UpdateEvent updateEvent) {
  }

  public void stopUpdate(Viewer viewer,
                         ViewletDataStore store,
                         ViewletRange range) {
  }





  /**
   * Return a collection of actions which can be applied to viewlets
   * in this table
   */
  public Collection getActions(ViewletDataStore store,
                               ViewletRange range) {
    Collection ll = new LinkedList();
    if ((range != null) & (!range.isEmpty())) {
      ll.add((new ToggleHoldAction()).createCompoundAction(store, range));
    }
    return ll;
  }

  protected class ToggleHoldAction extends ViewletAction
  {
    ToggleHoldAction()
    {
      super("Hold on updates");
      putValue(Action.NAME, "Hold on updates");
      putValue(Action.LONG_DESCRIPTION,
               "Change whether control is held by the visualisation client during element updates");
      putValue(Action.SHORT_DESCRIPTION,
               "Change whether control is held on updates");
      putValue(Action.SMALL_ICON, new HoldIcon(20, 20));
            
    }
        
    public void actionPerformed(ActionEvent e)
    {
      // do nothing
    }
        
    /**
     * If all viewlets in the collection have hold set to true, then the
     * compound version sets it to false. If any of them have it set to false
     * then the compound version sets all to true.
     */
    public ViewletAction createCompoundAction(ViewletDataStore store,
                                              ViewletRange range)
    {
      boolean allHold = true;
      ViewletData currentViewlet;
      Iterator viewletsIterator = store.getViewletDataIterator(range);
      while(viewletsIterator.hasNext())
        {
          currentViewlet = (ViewletData) viewletsIterator.next();
          if(!currentViewlet.getHoldsOnUpdates())
            {
              allHold = false;
              break;
            }
        }
      return(new CompoundToggleHoldAction(!allHold, store, range));
    }
  }

  private void toggleHold(ViewletDataStore store,
                          ViewletRange range,
                          boolean newValue) {
    ViewletData currentViewlet;
    Iterator viewletsIterator = range.iterator();
    while(viewletsIterator.hasNext()) {
      List index = (List)viewletsIterator.next();
      currentViewlet = (ViewletData)(store.getViewletDataAt(index));
      currentViewlet.setHoldsOnUpdates(newValue);
      store.setViewletDataAt(index, currentViewlet);
    }
    // trigger the jtable to update as a whole bunch of
    // viewlets have just changed
    store.fireViewletRangeUpdated( range );
  }
    
  private class CompoundToggleHoldAction extends ToggleHoldAction
  {
    private boolean newValue;
    private ViewletRange range;
    private ViewletDataStore store;
    CompoundToggleHoldAction(boolean newValue,
                             ViewletDataStore store,
                             ViewletRange range)
    {
      super();
      this.newValue = newValue;
      this.range = range;
      this.store = store;
    }
        
    public void actionPerformed(ActionEvent e)
    {
      new ToggleHoldCommand(AbstractViewletType.this,store,range,newValue).issue();
    }        
  }

  public static class ToggleHoldCommand extends ViewletTypeRangeCommand {
    boolean newValue;

    public ToggleHoldCommand(ViewletType type,
                             ViewletDataStore store,
                             ViewletRange range,
                             boolean newValue) {
      super(type, store, range);
      this.newValue = newValue;
    }
    
    public void postRecordIssue() {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.
          logMessage(this,
                     "ToggleHoldCommand postRecordIssue invoked with type=" +
                     getViewletType() +
                     " store=" + getViewletDataStore() +
                     " range=" + getViewletRange() +
                     " newValue=" + newValue);
          }
      ((AbstractViewletType)getViewletType()).
        toggleHold(getViewletDataStore(), getViewletRange(), newValue);
    }
  }
}

