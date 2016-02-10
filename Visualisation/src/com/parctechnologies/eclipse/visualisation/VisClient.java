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
import java.io.*;
import java.net.SocketException;
import java.util.*;
import java.awt.event.*;
import javax.swing.*;
import java.beans.*;
import java.lang.reflect.*;
/**
 * This is the top-level class of the Visualisation architecture on the
 * Java side.<p>
 *
 * It is responsible for:<p>
 * 1 intialising the visualisation client's streams (in this case queues between
 *   Java and Eclipse).<p>
 * 2 Registering the VC with eclipse.<p>
 * 3 Fulfilling the VC side of the visualisation protocol<p>
 * 4 Maintaining a VisClientStateModel instance, which reflects the current state
 * that the VisClient is in.<p>
 * 5 Unregistering the visualisation client and cleaning up the Java side, when
 * termination occurs.<p>
 * 6 Handling any exceptions which arise from communicating with ECLiPSe.<p>
 * 7 Participating in any Multitaksing phases that ECLiPSe enters.
 *
 * (1) and (2) are done by the constructor, which assumes that the calling Java
 * thread has control (rather than Eclipse or some other Java thread) and can
 * therefore invoke methods such as rpc on the EclipseConnection.<p>
 *
 * (3) and (4) work as follows:<p>
 *
 * There are two QueueListeners on the two fromEclipse queues (viewables
 * and updates). These are responsible for dealing with the various messages
 * which appear according to the visualisation protocol on these queues.<p>
 *
 * The basic way this works is to <p>
 *
 * a) "parse" the message, turning it from an EXDR
 * structure into a VisEvent object.<p>
 *
 * b) "process" the event. This mainly involves calling the processEvent method,
 * but there are also various other things to do based on the event type, for
 * example getting the new size of the viewable in the case of
 * create/expand/contract events. <p>
 *
 * processEvent is really the top-level method which is invoked when a
 * visualisation event happens. For each event that happens, the VisClient
 * progresses through a common sequence of states. The processEvent method
 * "directs" the state sequence of the
 * visualisation client throughout the duration of the VisEvent. However, the
 * state itself is encapsulated in the VisClientStateModel, which is passed by
 * reference to various other objects which want to be aware of changes to the
 * state. <p>
 *
 * The state progression for the VC can be seen from the structure of
 * processEvent. During the event, most of the actual Viewer-related work
 * relating to the changes of state is delegated to a ViewerManager object,
 * which can see the state via the VisClientStateModel. Each stage of the state
 * progression has a corresponding
 * static int constant in VisClientStateModel. Transitions between states
 * correspond to invocations of methods in ViewerManager
 * <p>
 * <IMG src="doc-files/VisClient-1.png" ALT="state transition diagram"></IMG>
 * <p>
 * <ul>
 * <li> start off in state NO_CURRENT_EVENT
 * <li> message arrives on a queue, VC now has control, go to state
 * SETTING_VIEWER_POLICY
 * <li> If the event is a CREATE_EVENT
 *   <ul>
 *   <li> User is given option to selected a saved Scenario for playback
 *   <li> ViewerManager.configureViewerBuildingPolicy is called to initialise
 *   the Viewers for this viewable
 *   </ul>
 * </li>
 * <li> message arrives on a queue, VC now has control, go to state
 * COLLECTING_PRE_EVENT_GOALS.
 * <li> ViewerManager.prepareForEvent is called
 * <li> collect the preEvent goals from the ViewerManager
 * <li> Eclipse has control, move to state EXECUTING_PRE_EVENT_GOALS
 * <li> execute the goals
 * <li> VC has control again
 * <li> distribute the goal results to the viewerManager
 * <li> call ViewerManager.startEvent
 * <li> if we want to hold for this event, set a flag on the ECLiPSe side by means of an RPC (this flag will cause ECLiPSe to hit a breakpoint immediately control is returned to it),
 * otherwise go directly to state EVENT_IS_FINISHED.
 * <li> call ViewerManager.stopEvent
 * <li> return to state NO_CURRENT_EVENT
 * <li> ECLiPSe now has control again
 * <li> If we had set the breakpoint (indicating that we want to hold) then
 * the state is set to HELD_ON_EVENT and a Multitaksing phase will begin.
 * <li> As ECLiPSe polls registered peers during the multitask phase, the
 * VC will recieve multitask messages through the VisMultitaskListener.
 * When the multitask phase starts, the
 * state flag "multitaskPhase" is set to true (thus enabling the resume
 * button) and any RPCs which need to be performed can be done so (eg.
 * Changes to the update granularity, or pre-build goals for new Viewlets).
 * <li>Should another peer end the multitasking phase or should the Resume
 * action be performed (by pressing the resume button) the stopEvent()
 * method is called which sets the state back to NO_CURRENT_EVENT.
 * </ul>
 *
 *
 * (5): termination can happen due to a user event when Eclipse has control. This
 * is handled by the setTerminate method within VisClientStateModel. Termination
 * can also happen when the VC has control, either due to a user event or when a
 * specific "terminate" message has been received from ECLiPSe. In either of
 * these cases the terminate method is invoked, which does the necessary
 * cleaning up.<p>
 *
 * (6): since almost all communication with the EclipseConnection is via this class,
 * it handles exceptions arising from lost connections, terminated Eclipses
 * or violations of the visualisation protocol using some generic recovery methods .<p>
 *
 * The final stage of termination is performed by the exitNormal method in the
 * case where the VisClient meant to exit, and exitError if we are exiting as a
 * result of an exception being thrown.
 *
 * (7): Consists of 
 * <ol>
 *   <li>Enabling the 'resume' button whenever a multitaksing phase is started</li>
 *   <li>Setting visualisation breakpoints whenever the VC wishes to 'hold'.
 * </ol>
 *
 *
 *
 */
public class VisClient
{

  protected VisClientStateModel stateModel;
  private FromEclipseQueue viewables_stream;
  private String viewables_stream_name;
  private FromEclipseQueue updates_stream;
  private String updates_stream_name;
  private ToEclipseQueue interest_stream;
  private String interest_stream_name;
  private EXDROutputStream interest_stream_f;
  protected Atom clientName;

  protected static final String MULTITASK_PHASE_TYPE = "vis_event" ;

  private static final Atom terminateAtom = new Atom("terminate");
  private static final Atom visProtocolSupportedAtom =
    new Atom("vis_protocol_supported");
  private static final Atom vcSupportAtom = new Atom("vc_support");
  private static final Atom viewGranularityAtom =
      new Atom("view_granularity");

  private VisClient visClient;
  private ViewerManager viewerManager;

  private static final int SUPPORTED_PROTOCOL_VERSION = 1;

  private boolean protocol_version_supported;


  private EclipseMultitaskConnection eclipse;

  private VisEvent currentEvent;

  /**
   * Set up a java VC based on EclipseConnection eclipse. Assumes that the
   * thread calling this constructor has access to rpc etc on the
   * EclipseConnection (i.e. the eclipse does not have control and is not
   * "owned" by some other java thread).
   */
  public VisClient(EclipseMultitaskConnection eclipse)
  {
    this.eclipse = eclipse;
    //DebuggingSupport.logMessage(this,"VisClient called with "+eclipse);
    try
    {
      initialise();
    }
    catch(EclipseException ee){recover_ee(ee);}
    catch(IOException ioe){recover_ioe(ioe);}
    catch(VisException ve){recover_ve(ve);}
  }

  private void processEvent(VisEvent visEvent)
  {
    // store event for stopEvent method
    currentEvent = visEvent;

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, visEvent);
    }

    stateModel.setEclipseHasControl(false);

    String viewableName = visEvent.getViewableName();

    stateModel.setCurrentState(VisClientStateModel.SETTING_VIEWER_POLICY);
    if (visEvent instanceof CreateEvent) {
        ScenarioManager.getInstance().selectPlaybackScenario(viewableName);
        // Clear the policy flag so that the user will be prompted
        // unless the scenario that they selected (if any) sets it
        // as a result of the CREATE_EVENT
        stateModel.setViewerBuildingPolicySelected(false);
    }
    // Hand the event off to the scenario manager which will either record
    // the event or replay some commands or both
    ScenarioManager.getInstance().processEvent(visEvent);
    if (visEvent instanceof CreateEvent) {
        // Incase the scenario does not set the policy, or if there is no
        // scenario being played back, ask the user
	viewerManager.configureViewerBuildingPolicy(viewableName,
                                                    ((CreateEvent)visEvent).getViewableType());
    }


    stateModel.setCurrentState(VisClientStateModel.COLLECTING_PRE_EVENT_GOALS);


    // Hand the event off to the scenario manager which will either record
    // the event or replay some commands or both
    ScenarioManager.getInstance().processEvent(visEvent);

    viewerManager.prepareForEvent(visEvent);

    BatchGoal preEventGoals =
      viewerManager.collectPreEventGoals(visEvent);

    List goalResults = null;

    try
    {
      stateModel.setCurrentState(VisClientStateModel.EXECUTING_PRE_EVENT_GOALS);

      // Hand the event off to the scenario manager which will either record
      // the event or replay some commands or both
      ScenarioManager.getInstance().processEvent(visEvent);

      stateModel.setEclipseHasControl(true);
      goalResults = preEventGoals.execute(eclipse);
      stateModel.setEclipseHasControl(false);
    }
    catch(EclipseException ee){recover_ee(ee);}
    catch(IOException ioe){recover_ioe(ioe);}

    stateModel.
      setCurrentState(VisClientStateModel.DISTRIBUTING_PRE_EVENT_GOAL_RESULTS);
    viewerManager.startEvent(visEvent, goalResults);

    stateModel.setAllScenarioCommandsExecuted(false);

    // Hand the event off to the scenario manager which will either record
    // the event or replay some commands or both
    ScenarioManager.getInstance().processEvent(visEvent);

    if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,"invokeAndWait returns");
    }


    if((viewerManager.shouldHold() || stateModel.getInterrupt())
       &&
       !stateModel.getTerminate())
    {
      // Add to the swing queue a Runnable which signifies that all commands
      // have now been executed.
      try {
        SwingUtilities.invokeAndWait(new AllCommandsExecuted());
      } catch(InvocationTargetException ite) {
        ite.printStackTrace();
      } catch(InterruptedException ie) {
        ie.printStackTrace();
      }
      
      // trigger a breakpoint
      stateModel.setCurrentState(VisClientStateModel.SETTING_BREAKPOINT);
      try {
        if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,"Setting breakpoint");
        }
        eclipse.rpc(":",vcSupportAtom,new Atom("vis_client_breakpoint"));
      } catch(IOException ioe) {
        recover_ioe(ioe);
      } catch(EclipseException ee) {
        recover_ee(ee);
      } finally {
        // Set the information line to the name of this event
        viewerManager.holdingEvent(visEvent);
      }
      stateModel.setCurrentState(VisClientStateModel.HELD_ON_EVENT);
      // Hand the event off to the scenario manager which will either record
      // the event or replay some commands or both
      ScenarioManager.getInstance().processEvent(visEvent);
    } else {
      stopEvent();
    }
    synchronized(stateModel) {
      // Must be carefull here to ensure that any asyncrhronous
      // setTerminate(true) calls are handled properly
      if(stateModel.getTerminate()) {
        terminate();
      }
      stateModel.setEclipseHasControl(true);
    }
  }

  // To be called either by the processEvent method to set the state
  // back to NO_CURRENT_EVENT or by the MultitaskListener at the end of
  // a multitask phase
  private void stopEvent() {
    Collection interests = null;
    VisEvent visEvent = currentEvent;
    String viewableName = visEvent.getViewableName();
    stateModel.
      setCurrentState(VisClientStateModel.EVENT_IS_FINISHED);

    // Hand the event off to the scenario manager which will either record
    // the event or replay some commands or both
    ScenarioManager.getInstance().processEvent(visEvent);

    viewerManager.stopEvent();

    if(visEvent instanceof CreateEvent)
    {
      interests =
        viewerManager.getInterestSpecs(viewableName);
      try
      {
        expressInterestsToEclipse(viewableName, interests);
      }
      catch(IOException ioe){recover_ioe(ioe);}
      addInterestSynchronisers(interests);
    }

    stateModel.
      setCurrentState(VisClientStateModel.NO_CURRENT_EVENT);

    // Hand the event off to the scenario manager which will either record
    // the event or replay some commands or both
    ScenarioManager.getInstance().processEvent(visEvent);

    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "returning to ECLiPSe");
    }

    currentEvent = null;
  }

  private void addInterestSynchronisers(Collection interestsList)
  {
    Iterator interestsListIterator = interestsList.iterator();
    InterestSpec interestSpec;
    while(interestsListIterator.hasNext())
    {
      interestSpec = (InterestSpec) interestsListIterator.next();
      interestSpec.getPropertyChangeSupport().
        addPropertyChangeListener("viewGranularity",
                                  new InterestSpecSynchroniser(interestSpec));
    }
  }

  private void expressInterestsToEclipse(String viewableName,
                                         Collection interestsList)
    throws IOException
  {
    Atom yesNoAtom;
    if(interestsList.isEmpty())
    {
      yesNoAtom = new Atom("no");
    }
    else
    {
      yesNoAtom = new Atom("yes");
    }


    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "for viewable:"+viewableName);
    }



    if (DebuggingSupport.logMessages) {
	DebuggingSupport.logMessage(this, "writing interest:"+interestsList);
    }


    interest_stream_f.write(new CompoundTermImpl(
                                "interest",
                                new CompoundTermImpl("viewable_create",
                                                     new Atom(viewableName)),
                                yesNoAtom,
                                interestsList));

    interest_stream_f.flush();
  }

  private void initialise() throws IOException, EclipseException, VisException
  {
    initialiseStreams();
    initialiseQueueListeners();
    registerVC();
    initialiseStateModel();
    initialiseViewerManager();
    initialiseScenarioManager();
  }

  private void initialiseStateModel() throws EclipseException, IOException
  {
    stateModel = new VisClientStateModel(eclipse, clientName, this);
    
    stateModel.setEclipseLibDir(eclipse.rpc("get_flag(installation_directory, _)").arg(2)+
                                File.separator+"lib"+File.separator+
                                eclipse.rpc("get_flag(hostarch,_)").arg(2));
  }

  private void initialiseViewerManager()
  {
    viewerManager = new ViewerManager(stateModel);
  }

  private void initialiseScenarioManager()
  {
    ScenarioManager.initialise(stateModel);
  }

  private void initialiseQueueListeners() throws IOException
  {
     viewables_stream.setListener(new ViewablesQL());
     updates_stream.setListener(new UpdatesQL());
  }

  // note slight complication: we ensure a unique visClient name by suffixing
  // a number.
  private void registerVC() throws IOException, EclipseException, VisException
  {
    int n = 0;
    clientName = null;
    eclipse.rpc("ensure_loaded(library(vc_support))");
    protocol_version_supported = false;
    while(clientName == null)
    {
      interest_stream_f.write(new CompoundTermImpl("vis_protocol_version",
                                    new Integer(SUPPORTED_PROTOCOL_VERSION)));
      interest_stream_f.flush();
      try
      {
        eclipse.rpc(":", vcSupportAtom,
                         new CompoundTermImpl("vis_client_register",
                         new Atom(eclipse.getPeerName().functor() + "_jvc_"+n),
                         new Atom(viewables_stream_name),
                         new Atom(updates_stream_name),
                         new Atom(interest_stream_name))
                    );
      }
      catch(Fail f)
      // There are two fail cases: where the vis client name was not unique, and
      // where the protocol is unsupported (detected by a boolean flag which will
      // have been set by the viewables QL).
      {
        if(!protocol_version_supported)
        {
          throw new VisException("The visualisation protocol version is not supported.");
        }
        n++;
      }
      clientName = new Atom(eclipse.getPeerName().functor() + "_jvc_"+n);
    }
  }



  private void initialiseStreams() throws IOException, EclipseException
  {
    // NOTE: a slight complication: we use findUnusedStreamName to ensure that
    // the stream name is unique.
    String root_viewables_name, root_interest_name, root_updates_name;

    root_viewables_name = eclipse.getPeerName().functor() + "_jvc_viewables";
    viewables_stream_name = findUnusedStreamName(root_viewables_name);
    viewables_stream = eclipse.getFromEclipseQueue(viewables_stream_name);
    viewables_stream.setListener(new ViewablesQL());

    root_updates_name = eclipse.getPeerName().functor() + "_jvc_updates";
    updates_stream_name = findUnusedStreamName(root_updates_name);
    updates_stream = eclipse.getFromEclipseQueue(updates_stream_name);
    updates_stream.setListener(new UpdatesQL());

    root_interest_name = eclipse.getPeerName().functor() + "_jvc_interest";
    interest_stream_name = findUnusedStreamName(root_interest_name);
    interest_stream = eclipse.getToEclipseQueue(interest_stream_name);
    interest_stream_f = new EXDROutputStream(interest_stream);

    // Register this peer as being interested in Multitasking phases
    eclipse.registerMultitask(new VisMultitaskListener());
  }

  private String findUnusedStreamName(String name)
    throws IOException, EclipseException
  {
    String result = null;
    int n = 0;
    while(result == null)
    {
      try
      {
        eclipse.rpc("current_stream", new Atom(name+n));
      }
      catch(Fail f)
      {
        return(name+n);
      }
      n++;
    }
    return(name+n);
  }


  /**
   * Provides access to the ViewerManager
   */
  public ViewerManager getViewerManager() {
    return viewerManager;
  }

  /** Generic VisEvents queue listener: detaAvailable method sets up EXDR parsing
   * input stream if needed and reads the next eventTerm, catching any
   * IOException. */
  private abstract class VisEventsQL implements QueueListener
  {
    protected CompoundTerm eventTerm;
    EXDRInputStream eis = null;
    public void dataAvailable(Object source)
    {
      if(eis == null)
      {
        FromEclipseQueue feq = (FromEclipseQueue) source;
        eis = new EXDRInputStream(feq);
      }
      try
      {
        eventTerm = (CompoundTerm) eis.readTerm();
      }
      catch(IOException ioe){recover_ioe(ioe);}
    }


    public void dataRequest(Object source){}
  }

  /** MultitaskListener attached to the EclipseMultitaskConnection.
      Behaviour is to set the VisClientState to indicate being in
      multi-tasking mode */
  private class VisMultitaskListener implements MultitaskListener
  {
    public void starting(EclipseMultitaskConnection eclipseCon,
                         String type) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "VisClient multitask starting type="+type);
      }
      if(stateModel.getTerminate()) {
        terminate();
      }
      // enable the continue button
      stateModel.setCanPerformRPC(true);
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "VisClient multitask set RPC flag");
      }
      if (MULTITASK_PHASE_TYPE.equals(type)) {
        // A multitask phase has begun
        if (viewerManager.shouldHold() || stateModel.getInterrupt()) {
          // execute peer_set_multitask
          try {
            eclipse.multitaskConfirm();
          } catch(IOException ioe) {
            recover_ioe(ioe);
          } catch(EclipseException ee) {
            recover_ee(ee);
          }
        }
      }
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "VisClient multitask started type="+type);
      }
    }

    public void ending(EclipseMultitaskConnection eclipseCon,
                       String type) {
      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this,
                                    "VisClient multitask ending type="+type);
      }
      if(stateModel.getTerminate()) {
        terminate();
      }
      stateModel.setCanPerformRPC(false);
      if (MULTITASK_PHASE_TYPE.equals(type)) {
        stopEvent();
      }
    }
  }


  /** Queue listener attached to "updates" stream. Behaviour is simple: its
   * dataAvailable method simply gets the eventTerm and processes it */
  private class UpdatesQL extends VisEventsQL
  {
    public void dataAvailable(Object source)
    {
      super.dataAvailable(source);
      VisEvent event = null;
      try
      {
        event = UpdateEvent.parseFromCompoundTerm(eventTerm);
      }
      catch(VisException ve){recover_ve(ve);}
      try {
          processEvent(event);
      } catch(RuntimeException re) {
          recover_re(re);
      }
    }
  }


  private class ViewablesQL extends VisEventsQL
  {
  /** In the case of the viewables QL, dataAvailable is also able to handle the
   * termination and protocol version messages which may appear on this stream */
    public void dataAvailable(Object source)
    {
      super.dataAvailable(source);
      if(eventTerm.equals(terminateAtom))
      {

	  if (DebuggingSupport.logMessages) {
	      DebuggingSupport.logMessage(this, "read terminate atom");
	  }

        terminate();
        return;
      }
      if(eventTerm.equals(visProtocolSupportedAtom))
      {
        protocol_version_supported = true;
        return;
      }
      if(eventTerm.functor().equals("vis_protocol_version") &&
         eventTerm.arity() == 1)
      {
        protocol_version_supported = false;
        return;
      }
      parseAndProcess(eventTerm);
    }
   /** parseAndProcess is called on the eventTerm. As well as parsing the term,
    * it also queries the viewable's size if the event type indicates that this
    * information is required. */
    private void parseAndProcess(CompoundTerm eventTerm)
    {
      VisEvent visEvent = null;
      try
      {
        visEvent = VisEvent.eventFromCompoundTerm(eventTerm);
        if(visEvent instanceof CreateEvent)
        {
          CompoundTermImpl sizeResult =
            sizeGoal(visEvent);
          ((CreateEvent) visEvent).setViewableSize((List) sizeResult.argCT(2).arg(2));
        }
        if(visEvent instanceof SizeEvent)
        {
          CompoundTermImpl sizeResult =
            sizeGoal(visEvent);
          ((SizeEvent) visEvent).setViewableSize((List) sizeResult.argCT(2).arg(2));
        }
      }
      catch(VisException ve){recover_ve(ve);}
      try {
          processEvent(visEvent);
      } catch(RuntimeException re) {
          recover_re(re);
      }
    }

    private CompoundTermImpl sizeGoal(VisEvent visEvent)
    {
      try
      {
        return((CompoundTermImpl) eclipse.rpc(new CompoundTermImpl(
                          ":",
                          vcSupportAtom,
                          new CompoundTermImpl("viewable_size",
                            new Atom(visEvent.getViewableName()),
                            null))));
      }
      catch(EclipseException ee){recover_ee(ee);}
      catch(IOException ioe){recover_ioe(ioe);}
      return(null);
    }

  }

  /** synchronises the view granularity between the java rep of the
   * interest and the ECLiPSe rep. Added as a propertyChangeListener
   * to the interestSpec as soon as an Eclipse-side rep is
   * established. If the viewGranularity changes on the Java side, the
   * synchroniser uses a multitasking phase RPC to modify the ECLiPSe-side
   * interest spec. The converse direction is not synchronised at
   * present. Actions changing the java side rep can only be performed
   * when the stateModel is in the multitask phase and should only be
   * enabled during this state */
  private class InterestSpecSynchroniser implements PropertyChangeListener
  {
    private InterestSpec interestSpec;
    InterestSpecSynchroniser(InterestSpec interestSpec)
    {
      this.interestSpec = interestSpec;
    }
    public void propertyChange(PropertyChangeEvent event)
    {
      CompoundTerm goal;
      Atom visClientName = stateModel.getVisClientName();
      Atom viewableName = interestSpec.getViewable().getNameAtom();
      String interestSpecName = interestSpec.getName();
      Atom newViewGranularity = (Atom) event.getNewValue();
      goal = new CompoundTermImpl(":", vcSupportAtom,
              new CompoundTermImpl("vis_client_interest_modify",
                                   viewableName, visClientName,
                                   new Atom(interestSpecName),
                                   viewGranularityAtom,
                                   newViewGranularity));
      try {
        stateModel.executeMultitaskGoal(goal);
      } catch(IOException ioe){
        recover_ioe(ioe);
      } catch(EclipseException ee){
        recover_ee(ee);
      }
    }

  }

  /** Generic recovery method for IOExceptions, including EclipseTerminated. */
  protected void recover_ioe(IOException ioe)
  {
    if (DebuggingSupport.logMessages) {
      ioe.printStackTrace(System.err);
    }
    if(ioe instanceof EclipseTerminatedException)
    {
      viewerManager.errorDialog("The ECLiPSe process which the visualisation"+
                                "\nclient was connected to has terminated."+
                                "\n\nThe visualisation client will now exit.");
      exitError();
    } else if(stateModel.getTerminate() && ioe instanceof SocketException) {
      // do not print any message as this is to be expected during
      // the termination phase
      exitNormal();
    }
    else
    {
      viewerManager.errorDialog("The following IOException was raised: \n\n"+ioe+
                                "\n\nThe visualisation client will now exit.");
      exitError();
    }
  }

  /** Generic recovery method for EclipseException. */
  protected void recover_ee(EclipseException ee)
  {
    if (DebuggingSupport.logMessages) {
      ee.printStackTrace(System.err);
    }
    viewerManager.errorDialog("The following EclipseException was raised: \n\n"+ee+
                              "\n\nThe visualisation client will now exit.");
    exitError();
  }

  /** Generic recovery method for VisException. */
  protected void recover_ve(VisException ve)
  {
    if (DebuggingSupport.logMessages) {
      ve.printStackTrace(System.err);
    }
    viewerManager.errorDialog("The following VisException was raised: \n\n"+ve+
                              "\n\nThe visualisation client will now exit.");
    exitError();
  }

  /** Generic recovery method for RuntimeException. */
  protected void recover_re(RuntimeException re)
  {
    if (DebuggingSupport.logMessages) {
      re.printStackTrace(System.err);
    }
    viewerManager.errorDialog("The following RuntimeException was raised: \n\n"+re+
                              "\n\nThe visualisation client will now exit.");
    exitError();
  }

  /** Exit due to occurrence of error. The VisClient implementation does
   * nothing: subclasses override this behaviour according to the deployment
   * scenario */
  protected void exitError()
  {

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "exit error");
      }
      if (stateModel.getCanPerformRPC()) {
        endMultitaskPhase();
      }
  }

  /** Exit due to a terminate request. The VisClient implementation does
   * nothing: subclasses override this behaviour according to the deployment
   * scenario */
  protected void exitNormal()
  {

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this, "exit normal");
      }
      if (stateModel.getCanPerformRPC()) {
        endMultitaskPhase();
      }

  }

  /**
   * Termination intiated from the Java side (although may also follow a
   * terminate message from the eclipse side). Tries to inform Eclipse, using
   * a terminate message on the interest stream, then closes all streams and
   * unregisters the VC.
   */
  private void terminate()
  {
    try
    {
      interest_stream_f.write(terminateAtom);
      interest_stream_f.flush();
      viewables_stream.close();
      updates_stream.close();
      interest_stream.close();
      eclipse.rpc(new CompoundTermImpl(":", vcSupportAtom,
                  new CompoundTermImpl("vis_client_unregister", clientName)));
      exitNormal();
    }
    catch(EclipseException ee){recover_ee(ee);}
    catch(IOException ioe){recover_ioe(ioe);}
  }

  private class AllCommandsExecuted implements Runnable {
        public void run() {
            stateModel.setAllScenarioCommandsExecuted(true);
        }
  }


  /**
   * Ends the multitask phase
   */
  void endMultitaskPhase() {
    try {
      eclipse.multitaskTerminate();
    } catch(IOException ioe) {
      recover_ioe(ioe);
    } catch(EclipseException ee) {
      recover_ee(ee);
    }
  }
}
