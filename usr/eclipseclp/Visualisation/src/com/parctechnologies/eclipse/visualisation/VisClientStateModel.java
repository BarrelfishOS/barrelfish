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

import java.beans.*;
import javax.swing.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

/** Encapsulation of the state of the vis client. The main aspect of this is
 * what stage of event processing the vis client is in. Several objects may be
 * interested in this, so VisClientStateModel uses a PropertyChangeSupport to
 * register interested observers and inform them of changes to
 * currentVisClientState. I.e. the state integer is "observable". At a more
 * coarse level of state representation,
 * the state model also maintains an observable boolean representing whether
 * ECLiPSe or
 * the VisClient has control and also whether the VisClient can perform RPC
 * or not.
 * <p>The VisClientStateModel class also has some other responsibilities:
 * <ul>
 * <li> Maintain settable, observable boolean autoResume to
 * control whether autoResume is on.
 * <li> Provide access to the autoResume function's delay
 * model (i.e. how long it delays for), represented by a BoundedRangeModel.
 * <li> If AutoResume is on, ensure that resume takes place automatically
 * at the specified duration after HELD_ON_EVENT is entered.
 * <li> Maintain settable boolean interrupt to control whether the VC should
 * hold at the next event.
 * <li> Maintain settable boolean terminate to control whether the VC should
 * terminate as soon as possible.
 * <li> Provide access to the vis client's Resume action and Interrupt action.
 * <li> Provide access to the VisClient's eclipse-side name (an atom)
 * </ul>*/
public class VisClientStateModel
{
  public final static int NO_CURRENT_EVENT = 0;
  public final static int SETTING_VIEWER_POLICY = 1;
  public final static int COLLECTING_PRE_EVENT_GOALS = 2;
  public final static int EXECUTING_PRE_EVENT_GOALS = 3;
  public final static int DISTRIBUTING_PRE_EVENT_GOAL_RESULTS = 4;
  public final static int SETTING_BREAKPOINT = 5;
  public final static int HELD_ON_EVENT = 6;
  public final static int EVENT_IS_FINISHED = 7;


  private boolean eclipseHasControl ;
  private boolean canPerformRPC ;
  private boolean autoResume = false;
  private boolean interrupt = false;
  private boolean terminate = false;

  private boolean recordScenario = true;
  private boolean allScenarioCommandsExecuted;
  private boolean viewerBuildingPolicySelected = false;

  private Atom visClientName;
  private VisClient visClient;

  private BoundedRangeModel delayModel;

  private int currentState;
  /*private*/ EclipseMultitaskConnection eclipse;
  
  private String eclipseLibDir;
  
  private Action resumeAction;
  private Action interruptAction;
  private Action autoResumeAction;
  private AutoResumeTimer autoResumeTimer;

  private PropertyChangeSupport propertyChangeSupport;

  /** Runnable objects paced on this list will be run after pending
      RPCs in the multitaking phase */
  private List multitaskRunnableList;

  public VisClientStateModel(EclipseMultitaskConnection eclipse,
                             Atom visClientName,
                             VisClient visClient)
  {
    this.eclipse = eclipse;
    this.visClientName = visClientName;
    this.visClient = visClient;
    propertyChangeSupport = new PropertyChangeSupport(this);
    setCurrentState(NO_CURRENT_EVENT);
    setEclipseHasControl(true);
    setCanPerformRPC(false);
    resumeAction = new ResumeAction();
    autoResumeAction = new AutoResumeAction();
    interruptAction = new InterruptAction();
    delayModel = new DefaultBoundedRangeModel(1000,1,0,5000);
    multitaskRunnableList = new LinkedList();
  }

  public synchronized void setAllScenarioCommandsExecuted(boolean newValue)
  {
    allScenarioCommandsExecuted = newValue;
    notifyAll();
  }

  public Atom getVisClientName()
  {
    return(visClientName);
  }


  public VisClient getVisClient() {
    return visClient;
  }


  public BoundedRangeModel getDelayModel()
  {
    return(delayModel);
  }

  synchronized void setCurrentState(int newState)
  {
    int oldState;
    if(currentState != newState)
    {
      oldState = currentState;
      currentState = newState;
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "VC state change from "+oldState+
                                    " to " + newState);
      }
      propertyChangeSupport.
        firePropertyChange("currentVisClientState", new Integer(oldState),
                           new Integer(currentState));
      if(getAutoResume() && !getInterrupt() &&
         currentState == HELD_ON_EVENT)
      {
        startAutoResumeTimer();
      }
      if(getAutoResume() && currentState == NO_CURRENT_EVENT)
      {
        stopAutoResumeTimer();
      }
    }
    notifyAll();
  }

  private void startAutoResumeTimer()
  {
    autoResumeTimer =
          new AutoResumeTimer(delayModel.getValue(), autoResumeAction);
    autoResumeTimer.start();
  }

  private void stopAutoResumeTimer()
  {
    if(autoResumeTimer != null)
    {
      autoResumeTimer.stop();
      autoResumeTimer = null;
    }
  }

  synchronized void setEclipseHasControl(boolean newValue)
  {
    boolean oldValue;
    if(eclipseHasControl != newValue)
    {
      oldValue = eclipseHasControl;
      eclipseHasControl = newValue;
      propertyChangeSupport.
        firePropertyChange("eclipseHasControl",
                           new Boolean(oldValue),
                           new Boolean(newValue));
    }
  }

  synchronized void setCanPerformRPC(boolean newValue)
  {
    boolean oldValue;
    if(canPerformRPC != newValue)
    {
      oldValue = canPerformRPC;
      canPerformRPC = newValue;
      propertyChangeSupport.
        firePropertyChange("canPerformRPC",
                           new Boolean(oldValue),
                           new Boolean(newValue));
    }
  }

  synchronized public void setAutoResume(boolean newValue)
  {
    boolean oldValue;
    if(autoResume != newValue)
    {
      oldValue = autoResume;
      autoResume = newValue;
      propertyChangeSupport.
        firePropertyChange("autoResume",
                           new Boolean(oldValue),
                           new Boolean(newValue));
      if(!newValue && autoResumeTimer != null && autoResumeTimer.isRunning())
      {
        stopAutoResumeTimer();
      }
    }
  }


  synchronized public boolean getRecordScenario() {
    return(recordScenario);
  }

  synchronized public void setRecordScenario(boolean newValue) {
    boolean oldValue;
    if (recordScenario != newValue) {
      oldValue = recordScenario;
      recordScenario = newValue;
      propertyChangeSupport.firePropertyChange("recordScenario",
					       new Boolean(oldValue),
					       new Boolean(newValue));
    }
  }

  synchronized public boolean getViewerBuildingPolicySelected() {
    return viewerBuildingPolicySelected;
  }

  synchronized public void setViewerBuildingPolicySelected(boolean newValue) {
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this,
                                  "set policy newValue="+newValue);
    }
    viewerBuildingPolicySelected = newValue;
  }

  synchronized public int getCurrentState()
  {
    return(currentState);
  }

  synchronized public boolean getEclipseHasControl()
  {
    return(eclipseHasControl);
  }

  synchronized public boolean getCanPerformRPC()
  {
    return(canPerformRPC);
  }

  synchronized public boolean getAutoResume()
  {
    return(autoResume);
  }

  public void setInterrupt(boolean newValue)
  {
    interrupt = newValue;
  }

  public boolean getInterrupt()
  {
    return(interrupt);
  }

  public synchronized void setTerminate(boolean newValue)
  {
    terminate = newValue;
    if (newValue && getEclipseHasControl()) {
      visClient.exitNormal();
    }
    notifyAll();
  }

  public synchronized boolean getTerminate()
  {
    return(terminate);
  }

  void resume()
  {

    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "disabling resume action");
    }
    
    //disable the resume button
    getResumeAction().setEnabled(false);
    // clear the interrupt flag
    setInterrupt(false);

    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "resume action performed");
    }

    visClient.endMultitaskPhase();
  }

  public Action getResumeAction()
  {
    return(resumeAction);
  }

  public Action getInterruptAction()
  {
    return(interruptAction);
  }

  /**
   * interested objects can become observers of one or more properties by
   * invoking this object's addPropertyChangeListener method.
   */
  PropertyChangeSupport getPropertyChangeSupport()
  {
    return(propertyChangeSupport);
  }

  /**
   * Class implementing the VisClient's record action.
   *
   */
  private class RecordAction extends AbstractAction
  {
    RecordAction()
    {
      super("Record");
      setEnabled(true);
    }

    public void actionPerformed(ActionEvent e)
    {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "RecordAction actionPerformed method invoked");
	}

      setRecordScenario(!getRecordScenario());
    }
  }

  /** Class implementing the VisClient's resume action. The action is
   * disabled by default but becomes enabled when the state changes to
   * HELD_ON_EVENT or during a multitasking phase. The action
   * performed simply calls the resume method which ultimately
   * disables the resume action again. */
  private class ResumeAction extends AbstractAction
    implements PropertyChangeListener
  {
    static final String normalName = "Resume";
    static final String multitaskName = "Continue";

    ResumeAction()
    {
      super(normalName);
      setEnabled(false);
      propertyChangeSupport.
        addPropertyChangeListener("canPerformRPC", this);
    }

    public void propertyChange(PropertyChangeEvent event)
    {
      if (((Boolean) event.getNewValue()).booleanValue()) {
        setEnabled(true);
      } else {
        setEnabled(false);
      }
    }

    public void actionPerformed(ActionEvent e)
    {

      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "ResumeAction actionPerformed method invoked");
      }
      
      if (isEnabled()) {
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,
                                      "ResumeAction current state queried");
        }
        resume();
      }
    }
  }

  /** action executed by the auto resume timer. It is as the resume
   * action, but for the fact that execution has no effect if
   * interrupt is true or if the state is not HELD_ON_EVENT (ie. if a
   * multitask phase has started which is not a Visualisation
   * phase). This means that interrupt being true has the additional
   * effect of cancelling the next AutoResume if autoResume is on.
   * */
  private class AutoResumeAction extends ResumeAction
  {
    public void propertyChange(PropertyChangeEvent event)
    {
      super.propertyChange(event);
      if (currentState != HELD_ON_EVENT) {
        setEnabled(false);
      }
    }

    public void actionPerformed(ActionEvent e)
    {
      if(!getInterrupt() && (currentState == HELD_ON_EVENT))
      {
        super.actionPerformed(e);
      }
    }
  }


  /** Class implementing the vis client's interrupt action. This
   * action serves both to indicate that the vis client should hold at
   * the next event or, if AutoResume is on and the VC has control,
   * that the next autoResume should be cancelled. It is enabled by
   * default, and manages its own enabling by observing the
   * stateInteger. It becomes enabled when no event is current and
   * disable either when it is executed, or when an event is happening
   * (as long as autoResume is off). When executed, it stops the
   * autoResume if it is on, and sets interrupt to true.  */
  private class InterruptAction extends AbstractAction
    implements PropertyChangeListener
  {
    InterruptAction()
    {
      super("Interrupt");
      setEnabled(true);
      propertyChangeSupport.
        addPropertyChangeListener("currentVisClientState", this);
    }

    public void propertyChange(PropertyChangeEvent event)
    {
      int newState = ((Integer) event.getNewValue()).intValue();
      if(newState == NO_CURRENT_EVENT)
      {
        setEnabled(true);
        return;
      }
      DebuggingSupport.logMessage(this,"interrupt action newState="+newState+" getAutoResume()="+getAutoResume());
      if(!getAutoResume() && newState == HELD_ON_EVENT)
      {
        setEnabled(false);
        return;
      }
    }

    public void actionPerformed(ActionEvent e)
    {
        setEnabled(false);

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "interrupt action performed");
	}

        if(getAutoResume() && autoResumeTimer != null && autoResumeTimer.isRunning())
        {
          stopAutoResumeTimer();
        }
        setInterrupt(true);
    }
  }


  private class AutoResumeTimer extends javax.swing.Timer
  {
    AutoResumeTimer(int delay, Action action)
    {
      super(delay, action);

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "delay = "+delay);
      }

      setRepeats(false);
      setInitialDelay(delay);
    }
  }


  /** This method may be invoked by any thread to have the goal
   * performed during the multitask phase. */
  CompoundTerm executeMultitaskGoal(CompoundTerm goal)
    throws IOException, EclipseException
  {
    CompoundTerm result;
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "multitask goal posted");
    }
    
    // following statement blocks until goal is set.
    result = eclipse.rpc(goal);
    
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "multitask goal complete");
    }
    
    return(result);
  }

  /**
   * Executes the batch goal in the multitask thread, suspends until
   * the results is available.
   * @return The results of the batch goal
   */
  public List executeMultitaskBatchGoal(BatchGoal batch) throws IOException, EclipseException {
    List result;
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "multitask batch goal posted");
    }
    result = batch.execute(eclipse);
    if (DebuggingSupport.logMessages) {
      DebuggingSupport.logMessage(this, "multitask batch goal complete");
    }
    return result;
  }

  /**
   * Access the directory in which the ECLiPSe architecture specific
   * libraries are installed
   **/
  public void setEclipseLibDir(String eclipseLibDir) {
    this.eclipseLibDir=eclipseLibDir;
  }
  /**
   * Access the directory in which the ECLiPSe architecture specific
   * libraries are installed
   **/
  public String getEclipseLibDir() {
    return eclipseLibDir;
  }
}
