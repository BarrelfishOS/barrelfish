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

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.beans.*;
import javax.swing.*;

import com.parctechnologies.eclipse.*;

/**
 * Superclass of all classes in this package implementing Viewer.
 * Any implementation which is common to most viewers should be located in this
 * class. This includes actions which are common to all viewers, the
 * implementation of some default behaviour and the implementation of the
 * simplest get/set methods from the Viewer interface.
 *
 */
public abstract class ViewerImpl implements Viewer
{
    private InterestSpec interestSpec;
    private Viewable viewable;
    private VisEvent currentEvent;
    private VisClientStateModel stateModel;
    private ViewerManager viewerManager;
    private static final Atom fineAtom = new Atom("fine");
    private static final Atom timedAtom = new Atom("timed");
    private static final Atom coarseAtom = new Atom("coarse");
    private VPSRadioButton fineMenuButton;
    private VPSRadioButton timedMenuButton;
    private VPSRadioButton coarseMenuButton;
    private PropertyChangeSupport propertyChangeSupport =
      new PropertyChangeSupport(this);
    private JMenuBar jMenuBar = new JMenuBar();
    private Map menuTitleToMenu = new HashMap();
    private Map menuTitleToPopupMenu = new HashMap();
    private String description = "Viewer";

    private SymRef symRef ;

    // This boolean determines whether to hold when ECLiPSe backtracks over the
    // viewable_create goal, producing the DestroyEvent for the viewable.
    private boolean holdAtLastBacktrack = true;
    private boolean holdAtExpansions = true;
    private boolean holdAtContractions = true;

    public ViewerImpl(VisClientStateModel stateModel, Viewable viewable)
    {
      setViewable(viewable);
      setInterestSpec(viewable.createInterestSpec());
      setStateModel(stateModel);
      initialiseMenu();
      symRef = new SymRef(this, viewable.getSymRef(), getClass().getName());
    }

    public void setViewerManager(ViewerManager viewerManager)
    {
      this.viewerManager = viewerManager;
    }

    public void close()
    {
      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this,"ViewerImpl close called");
      }
      PropertyChangeSupport pcs = stateModel.getPropertyChangeSupport();
      pcs.removePropertyChangeListener("canPerformRPC", fineMenuButton);
      pcs.removePropertyChangeListener("canPerformRPC", timedMenuButton);
      pcs.removePropertyChangeListener("canPerformRPC", coarseMenuButton);
      viewerManager.closeViewer(this);
    }


    private void initialiseMenu()
    {
      // add some boolean options common to all viewers.
      JCheckBoxMenuItem halbCheckBox = new JCheckBoxMenuItem("Hold at destruction");
      halbCheckBox.setModel(new BooleanPropertyModel("holdAtLastBacktrack",
                            this, getPropertyChangeSupport()));
      JCheckBoxMenuItem haeCheckBox = new JCheckBoxMenuItem("Hold at expansions");
      haeCheckBox.setModel(new BooleanPropertyModel("holdAtExpansions",
                            this, getPropertyChangeSupport()));
      JCheckBoxMenuItem hacCheckBox = new JCheckBoxMenuItem("Hold at contractions");
      hacCheckBox.setModel(new BooleanPropertyModel("holdAtContractions",
                            this, getPropertyChangeSupport()));
      addMenuItem("Options", haeCheckBox);
      addMenuItem("Options", hacCheckBox);
      addMenuItem("Options", halbCheckBox);
      ButtonGroup bg = new ButtonGroup();
      fineMenuButton = new VPSRadioButton(this,fineAtom);
      bg.add(fineMenuButton);
      addMenuItem("Options", fineMenuButton);
      coarseMenuButton = new VPSRadioButton(this,coarseAtom);
      bg.add(coarseMenuButton);
      addMenuItem("Options", coarseMenuButton);
      timedMenuButton = new VPSRadioButton(this,timedAtom);
      bg.add(timedMenuButton);
      addMenuItem("Options", timedMenuButton);
    }

    public void setHoldAtLastBacktrack(boolean newValue)
    {
      (new ViewerSetBooleanPropertyCommand(this,
         "holdAtLastBacktrack", newValue)).issue();
    }

    public void setHoldAtLastBacktrackPrivate(boolean newValue)
    {
      boolean oldValue = holdAtLastBacktrack;
      holdAtLastBacktrack = newValue;
      this.getPropertyChangeSupport().
        firePropertyChange("holdAtLastBacktrack", oldValue, newValue);
    }

    public boolean getHoldAtLastBacktrack()
    {
      return(holdAtLastBacktrack);
    }

    public void setHoldAtExpansions(boolean newValue)
    {
      (new ViewerSetBooleanPropertyCommand(this,
          "holdAtExpansions", newValue)).issue();
    }

    public void setHoldAtExpansionsPrivate(boolean newValue)
    {
      boolean oldValue = holdAtExpansions;
      holdAtExpansions = newValue;
      this.getPropertyChangeSupport().
        firePropertyChange("holdAtExpansions", oldValue, newValue);

    }


    public boolean getHoldAtExpansions()
    {
      return(holdAtExpansions);
    }

    public void setHoldAtContractions(boolean newValue)
    {
      (new ViewerSetBooleanPropertyCommand(this,
          "holdAtContractions", newValue)).issue();
    }

    public void setHoldAtContractionsPrivate(boolean newValue)
    {
      boolean oldValue = holdAtContractions;
      holdAtContractions = newValue;
      this.getPropertyChangeSupport().
        firePropertyChange("holdAtContractions", oldValue, newValue);

    }

    public boolean getHoldAtContractions()
    {
      return(holdAtContractions);
    }

    public void setViewPropagationSteps(Object atom)
    {
      (new ViewerSetPropagationStepsCommand(this, (Atom)atom)).issue();
    }

    public void setViewPropagationStepsPrivate(Object newValue)
    {
      Object oldValue = getViewPropagationSteps();
      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.
	      logMessage(this,
			 "setViewPropagationStepsPrivate newValue = "+
			 newValue + " oldValue = " + oldValue);
      }
      if(!oldValue.equals(newValue))  {
          getInterestSpec().setViewGranularity((Atom)newValue);
	  propertyChangeSupport.
	      firePropertyChange("viewPropagationSteps",
				 oldValue, newValue);
      }
    }

    public Object getViewPropagationSteps()
    {
      return(getInterestSpec().getViewGranularity());
    }

    /**
     * each viewer uses a PropertyChangeSupport object to allow some of its
     * proprties to be observable by other objects.
     */
    public PropertyChangeSupport getPropertyChangeSupport()
    {
      return(propertyChangeSupport);
    }

    public String getDescription()
    {
      return(description);
    }

    public void setDescription(String description)
    {
      this.description = description;
    }

  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("Options".equals(menuTitle)) return KeyEvent.VK_O;
    return -1;
  }

  /**
   * Adds the given item to the named menu (creating the JMenu if
   * necessary), storing the menuTitle->JMenu map in the provided map
   * and adding the JMenu to the given JMenuBar provided it is not
   * null.  */
  private void addMenuItem(Map menuTitleMap,
                           JMenuBar menuBar,
                           String menuTitle,
                           Object item) {
    JMenu menu = (JMenu) menuTitleMap.get(menuTitle);
    if(menu == null) {
      menu = new JMenu(menuTitle);
      // Add optional menu shortcut
      int mnemonic = getMenuMnemonic(menuTitle);
      if (mnemonic != -1) {
        menu.setMnemonic(mnemonic);
      }
      if (menuBar != null) {
        menuBar.add(menu);
      }
      menuTitleMap.put(menuTitle, menu);
    } else {
      if(item == null) {
        // if item is null and the menu exists then add a seperator
        menu.addSeparator();
        return;
      }
    }
    if(item instanceof Action)
      {
        menu.add((Action) item);
        return;
      }
    if(item instanceof JMenuItem)
      {
        menu.add((JMenuItem) item);
        return;
      }
  }

  /** Convenience method to add menu items based on the menu title: if a menu
   * with that title exists, add to that menu. Otherwise, create menu with that
   * title and add to that.
   **/
    protected void addMenuItem(String menuTitle, Object item)
    {
      addMenuItem(menuTitleToMenu, jMenuBar, menuTitle, item);
    }

  /** Convenience method to add popup menu items based on the menu
   * title: if a popup menu with that title exists, add to that
   * menu. Otherwise, create menu with that title and add to that.
   **/
    protected void addPopupMenuItem(String menuTitle, Object item)
    {
      addMenuItem(menuTitleToPopupMenu, null, menuTitle, item);
    }

  /** Convenience method to add both normal menu and popup menu items
   * based on the menu title: if a popup menu with that title exists,
   * add to that menu. Otherwise, create menu with that title and add
   * to that.
   **/
    protected void addMenuAndPopupMenuItem(String menuTitle, Object item)
    {
      addMenuItem(menuTitleToMenu, jMenuBar, menuTitle, item);
      addMenuItem(menuTitleToPopupMenu, null, menuTitle, item);
    }

  public JMenu getPopupMenu(String menuTitle) {
    return (JMenu) menuTitleToPopupMenu.get(menuTitle);
  }

    public JMenuBar getJMenuBar()
    {
      return(jMenuBar);
    }

    public InterestSpec getInterestSpec()
    {
      return(interestSpec);
    }

    public void setInterestSpec(InterestSpec interestSpec)
    {
      this.interestSpec = interestSpec;
    }

    public void setStateModel(VisClientStateModel stateModel)
    {
      this.stateModel = stateModel;
    }

    protected VisClientStateModel getStateModel()
    {
      return(stateModel);
    }

    public Viewable getViewable()
    {
      return(viewable);
    }

    protected void setViewable(Viewable viewable)
    {
      this.viewable = viewable;
    }


    public abstract Component getComponent();

    /**
     * It is useful for some methods to have access to the current event while
     * it is happening.
     */
    protected VisEvent getCurrentEvent()
    {
      return(currentEvent);
    }

    /**
     * The default case here does nothing.
     */
    public void prepareForEvent(VisEvent event){
	if (event instanceof DestroyEvent) {
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,"destroyEventIssued fired");
	    }
	    getPropertyChangeSupport().
		firePropertyChange("destroyEventIssued",false,true);
	}
    }

    /**
     * The default case here returns an empty Batch goal.
     */
    public BatchGoal collectPreEventGoals(VisEvent event)
    {
      return(new BatchGoal());
    }

    /**
     * The default behaviour is to do nothing but set the current event. This
     * will obviously be overridden in most cases.
     */
    public void startEvent(VisEvent event, java.util.List goalResults)
    {
      currentEvent = event;
    }

    /**
     * The default behaviour for shouldHold is that we hold at any Create or
     * Expand events and at Destroy events if holdAtLastBacktrack is true.
     * This can be overridden.
     */
    public boolean shouldHold()
    {
      if(currentEvent instanceof CreateEvent)
      {
        return(true);
      }
      if(currentEvent instanceof DestroyEvent && holdAtLastBacktrack)
      {
        return(true);
      }
      if(currentEvent instanceof ContractEvent && holdAtContractions)
      {
        return(true);
      }
      if(currentEvent instanceof ExpandEvent && holdAtExpansions)
      {
        return(true);
      }
      return(false);
    }

    /**
     * The default behaviour here simply resets the currentEvent.
     */
    public void stopEvent()
    {
      currentEvent = null;
    }

    /**
     * This inner class provides the "view propagation steps X" radio
     * buttons. It must become disabled whenever ECLiPSe has control
     * because a user action which changes the value requires an
     * in-event goal, which can only be performed when the VC has
     * control.  */
    private class VPSRadioButton extends JRadioButtonMenuItem
      implements PropertyChangeListener
    {
      VPSRadioButton(Object propertyHolder,Atom atom)
      {
        super("View propagation steps ("+atom.functor()+")");
        setModel(new BooleanGroupPropertyModel("viewPropagationSteps",
                 propertyHolder, getPropertyChangeSupport(), atom));
	stateModel.getPropertyChangeSupport().
	    addPropertyChangeListener("canPerformRPC", this);
        getPropertyChangeSupport().
          addPropertyChangeListener("destroyEventIssued", this);
      }

      public void propertyChange(PropertyChangeEvent event)
      {
        if (event.getPropertyName() == "destroyEventIssued") {
          setEnabled(false);
        } else {
          // event.getPropertyName() == "canPerformRPC"
          if(((Boolean) event.getNewValue()).booleanValue()) {
            setEnabled(true);
          } else {
            setEnabled(false);
          }
        }
      }
    }

    /**
     * The default case here does nothing.
     */
    public void gainFocus()
    {}

    /**
     * The default case here does nothing.
     */
    public void loseFocus()
    {}

    /** Returns the dimensions of the Viewer */
    public Rectangle getBounds() {
	return getComponent().getBounds();
    }

    /**
     * The default case here does nothing
     */
    public void zoomInByRatio(float ratio)
    {}

    /**
     * The default case here does nothing.
     */
    public void zoomToLevel(float newLevel)
    {}

    /** Sets the dimensions of the Viewer */
    public void setBounds(Rectangle r) {
	// NOTE: should probably remove the ComponentListener for the
	// duration of this resizing
	getComponent().setBounds(r.x, r.y, r.width, r.height);
    }


    /** Access the context sensitive popup menu */
    public JPopupMenu getPopupMenu() {
        return new JPopupMenu();
    }


    public void setSymRef(SymRef symRef) {
	this.symRef = symRef;
    }

    public SymRef getSymRef() {
	return symRef;
    }
}


