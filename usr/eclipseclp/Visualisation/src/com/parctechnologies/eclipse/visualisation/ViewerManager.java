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
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import javax.swing.event.*;

/**
 * The ViewerManager is the delegatee of the VisClient.


 * The responsibilities of this class are all viewer-related and are grouped
 * around each of the methods which are invoked by VisClient's processEvent
 * method during the course of an event.<p>
 * The graphical
 * management of the viewers as well as the top level buttons and menus is
 * delegated to ViewerManagerFrame.<p>
 * The responsibilities are:
 * <ul>
 * <li> When setViewerBuildingPolicy is called for a create event, the
 * ViewerManager must ask the user which viewers should be created OR if
 * a suitable scenario already exists it should ask the user if they wish to
 * use the viewerBuildingPolicy stored in the scenario.
 * <li> When prepareForEvent is called for a create event, the ViewerManager must
 * build and configure any viewers which are going to monitor the new viewable,
 * according to its ViewerBuildingPolicy.
 * <li> When prepareForEvent is called for any event, the ViewerManager must
 * determine which viewers are participating in the event and then invoke the
 * prepareForEvent method of each of those viewers.
 * <li> For collectPreEventGoals, the viewerManager should collect the pre event
 * goals of each of the participating viewers into a batch goal and return it.
 * <li> For startEvent in the case of a createEvent, the viewerManager should
 * add the new viewers to the ViewerManagerFrame.
 * <li> For startEvent generally, the viewerManager should invoke the startEvent
 * method of all participating viewers, distributing the results of each one's
 * pre-event goals.
 * <li> For shouldHold, the viewer should return true if any one of the
 * participating viewers wants to hold.
 * <li> For stopEvent, in the case of a DestroyEvent, the viewerManager should
 * remove the viewers whose viewable has ceased to exist from the
 * viewerManagerFrame.
 * <li> For stopEvent generally, the viewerManager should invoke the stopEvent
 * method of all participating viewers.
 * <li> The ViewerManager must also provide access to the collection of current
 * interest specs for any existent viewable.
 * </ul>
 *
 * <p>
 */
class ViewerManager
{
  // in order to be able to efficiently determine which viewers are
  // participating in each event, the ViewerManager maintains several map
  // data structures.

  // this allows us to work out the viewable which the event refers to, from its
  // name.
  private Map viewableNameToViewable;

  // This is for update events, where we may need to determine which of several
  // viewers monitoring the viewable is participating.
  // we have exactly one interest per viewer
  private Map interestSpecNameToViewer;

  // maps viewable to all viewers monitoring it.
  private Map viewableToViewers;

  // required for implementation of getInterestSpecs()
  private Map viewableToInterestSpecs;

  private ViewerBuildingPolicy viewerBuildingPolicy;
  // List of viewers which are participating in the current event
  private List participatingViewers;
  private Atom vcSupportAtom = new Atom("vc_support");
  private ViewerManagerFrame viewerManagerFrame;
  private VisEvent currentEvent;
  private VisClientStateModel stateModel;


  ViewerManager(VisClientStateModel stateModel)
  {
    this.stateModel = stateModel;
    initialise();
  }

  private void initialise()
  {
    viewableNameToViewable = new HashMap();
    interestSpecNameToViewer = new HashMap();
    viewableToViewers = new HashMap();
    viewableToInterestSpecs = new HashMap();
    initialiseViewerBuildingPolicy();
    initialiseViewerManagerFrame();
  }

  private void initialiseViewerManagerFrame()
  {
    viewerManagerFrame = new ViewerManagerFrame(stateModel);
    if(DebuggingSupport.useGraphics)
    {
      viewerManagerFrame.setVisible(true);
    }
    viewerManagerFrame.pack();

  }

  public void initialiseViewerBuildingPolicy()
  {
    viewerBuildingPolicy = new DefaultViewerBuildingPolicy(stateModel);
  }

  public ViewerBuildingPolicy getViewerBuildingPolicy() {
    return viewerBuildingPolicy;
  }

  public void configureViewerBuildingPolicy(String viewableName,
                                            ViewableType viewableType)
  {
    if (!stateModel.getViewerBuildingPolicySelected()) {
	// The user wishes to select new viewers
	JDialog dialog = new SelectViewersDialog(viewableName,
                                                 viewableType);
	dialog.show();
    }
  }

  private Viewable initialiseViewable(String viewableName,
                                      ViewableType type)
  {
    Viewable viewable = new Viewable(viewableName, type);
    viewableNameToViewable.put(viewableName, viewable);
    return(viewable);
  }


  private java.util.List buildViewers(Viewable viewable)
  {
    java.util.List newViewers = viewerBuildingPolicy.buildViewers(viewable);
    Iterator newViewersIterator = newViewers.iterator();
    Viewer newViewer;

    while(newViewersIterator.hasNext())
    {
        newViewer = (Viewer) newViewersIterator.next();
        newViewer.setViewerManager(this);
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,
                                        "built viewer=" + newViewer +
                                        " interestSpec=" +
                                        newViewer.getInterestSpec().getName() );                                       
        }
        interestSpecNameToViewer.put(newViewer.getInterestSpec().getName(),
                                     newViewer);
    }
    return(newViewers);
  }

  void closeViewer(Viewer viewer)
  {
    viewerManagerFrame.removeViewer(viewer);
    Collection oldViewers =
      (Collection) viewableToViewers.remove(viewer.getViewable());
    Collection newViewers = new LinkedList(oldViewers);
    newViewers.remove(viewer);
    viewableToViewers.put(viewer.getViewable(), newViewers);
    interestSpecNameToViewer.remove(viewer.getInterestSpec().getName());
    DebuggingSupport.
      logMessage(this, "viewer "+viewer.getDescription()+" removed");
    // clean up memory used by the viewer
    System.gc();
  }

  void prepareForEvent(VisEvent visEvent)
  {
    String viewableName = visEvent.getViewableName();
    Viewable viewable;
    Viewer currentViewer;

    if(visEvent instanceof CreateEvent)
    {
      CreateEvent createEvent = (CreateEvent) visEvent;
      viewable =
        initialiseViewable(viewableName,
                           createEvent.getViewableType());
      java.util.List newViewers = buildViewers(viewable);
      viewableToViewers.put(viewable, newViewers);
      Collection interestSpecs = getAllInterestSpecs(newViewers);
      viewableToInterestSpecs.put(viewable, interestSpecs);
    }
    else
    {
      viewable =
      (Viewable) viewableNameToViewable.get(viewableName);
    }

    participatingViewers =
      (java.util.List) determineParticipatingViewers(visEvent, viewable);

    Iterator viewersIterator
      = participatingViewers.iterator();
    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      currentViewer.prepareForEvent(visEvent);
    }
  }

  private List determineParticipatingViewers(VisEvent event,
                                             Viewable viewable)
  {
    if(event instanceof UpdateEvent)
    {
      UpdateEvent updateEvent = (UpdateEvent) event;
      String interestSpecName = updateEvent.getInterestSpecName();
      LinkedList result = new LinkedList();
      Object viewer = interestSpecNameToViewer.get(interestSpecName);
      if (DebuggingSupport.logMessages) {
          DebuggingSupport.logMessage(this,
                                      "interestSpecName=" +
                                      interestSpecName + " viewer=" +
                                      viewer);
      }
      if(viewer != null)
      {
        result.add(viewer);
      }
      return(result);
    }
    else
    {
      return((List) viewableToViewers.get(viewable));
    }
  }

  BatchGoal collectPreEventGoals(VisEvent visEvent)
  {
    BatchGoal preEventGoals = new BatchGoal();
    Viewer currentViewer;

    Iterator viewersIterator
      = participatingViewers.iterator();
    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      preEventGoals.add(currentViewer.collectPreEventGoals(visEvent));
    }
    return(preEventGoals);
  }

  void holdingEvent(VisEvent visEvent)
  {
    viewerManagerFrame.setLastEventString(visEvent.getDescription());
  }

  void startEvent(VisEvent visEvent, java.util.List goalResults)
  {
    viewerManagerFrame.setLastEventString("");

    Viewer currentViewer;
    Viewable viewable =
      (Viewable) viewableNameToViewable.get(visEvent.getViewableName());
    java.util.List currentGoalResults;

    currentEvent = visEvent;

    Iterator viewersIterator
      = participatingViewers.iterator();
    Iterator goalResultsIterator
      = goalResults.iterator();

    String viewableNameString = viewable.getNameString();


    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      currentGoalResults = (java.util.List) goalResultsIterator.next();
      currentViewer.startEvent(visEvent, currentGoalResults);
      if(visEvent instanceof CreateEvent)
      {
        viewerManagerFrame.addViewer(currentViewer, viewableNameString);
      }
    }
  }



  boolean shouldHold()
  {
    Viewer currentViewer;
    Iterator viewersIterator
      = participatingViewers.iterator();
    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      if(currentViewer.shouldHold())
      {
        return(true);
      }
    }
    return(false);
  }

  void stopEvent()
  {
    Viewer currentViewer;
    Iterator viewersIterator
      = participatingViewers.iterator();
    String viewableName = currentEvent.getViewableName();
    Viewable viewable =
      (Viewable) viewableNameToViewable.get(viewableName);


    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      currentViewer.stopEvent();
      if(currentEvent instanceof DestroyEvent)
      {
        try
        {
          SwingUtilities.invokeAndWait(new ViewerCloser(currentViewer));
        }
        catch(Exception e)
        {
          throw new RuntimeException("Exception "+e+
             " thrown during ViewerCloser.run()");
        }
      }
    }
    if(currentEvent instanceof DestroyEvent)
    {
      viewableToViewers.remove(viewable);
      viewableToInterestSpecs.remove(viewable);
      viewableNameToViewable.remove(viewableName);
    }
    currentEvent = null;
    participatingViewers = null;
  }

  private class ViewerCloser implements Runnable {
      private Viewer viewer;
      ViewerCloser(Viewer viewer) {
          this.viewer = viewer;
      }
      public void run() {
          // closes the internal frame window and updates all book-keeping
          viewerManagerFrame.removeViewer(viewer);
      }
  }

  Collection getAllInterestSpecs(Collection viewers)
  {
    Collection interestSpecs = new LinkedList();
    Viewer currentViewer;
    Iterator viewersIterator
      = viewers.iterator();
    while(viewersIterator.hasNext())
    {
      currentViewer = (Viewer) viewersIterator.next();
      interestSpecs.add(currentViewer.getInterestSpec());
    }
    return(interestSpecs);
  }

  Collection getInterestSpecs(String viewableName)
  {
    Viewable viewable = (Viewable) viewableNameToViewable.get(viewableName);
    return((Collection) viewableToInterestSpecs.get(viewable));
  }


  void errorDialog(String message)
  {
    JOptionPane.showConfirmDialog(viewerManagerFrame, message,
                                  "Error in visualisation client",
                                  JOptionPane.DEFAULT_OPTION);
  }

  /**
   * Dialog from which the user will select the viewers they wish to be
   * created
   */
  private class SelectViewersDialog extends JDialog {
    String viewableName;
    ViewableType viewableType;

    List buttons;
    Set keys;

    public SelectViewersDialog(String viewableName,
                               ViewableType viewableType) {
      super();
      this.buttons = new LinkedList();
      this.viewableName = viewableName ;
      this.viewableType = viewableType ;
      setModal(true);
      this.setTitle("Select viewers for "+viewableName);
      getContentPane().setLayout(new BorderLayout());
      getContentPane().add(new JLabel("Which viewers would you like to create for viewable \""+viewableName+"\"?"),
			   BorderLayout.NORTH);

      getContentPane().add(viewerFactorySelectorPanel(),
			   BorderLayout.CENTER);
      JPanel buttonPanel = new JPanel();
      buttonPanel.add(new ActionButton(new OkayAction()));
      getContentPane().add(buttonPanel, BorderLayout.SOUTH);
      pack();
      // locate the window in the center for the screen
      Dimension screenSize =
	Toolkit.getDefaultToolkit().getScreenSize();
      Dimension windowSize = getPreferredSize();
      setLocation(screenSize.width/2 - (windowSize.width/2),
		  screenSize.height/2 - (windowSize.height/2));
    }

    private JPanel viewerFactorySelectorPanel() {
      JPanel panel = new JPanel();
      BoxLayout layout = new BoxLayout(panel, BoxLayout.Y_AXIS);
      panel.setLayout(layout);
      keys = ((DefaultViewerBuildingPolicy)viewerBuildingPolicy).availableViewerFactories(viewableType);
      Iterator it = keys.iterator();
      for(int i = 0; it.hasNext(); i++)
      {
	Object name = it.next();
	JCheckBox button = new JCheckBox(name.toString());
	buttons.add(button);
        panel.add(button);
      }
      return(panel);
    }

    private class OkayAction extends AbstractAction
    {
      OkayAction()
      {
        super("Okay");
      }
      public void actionPerformed(ActionEvent event)
      {
	List selected = new LinkedList();
	Iterator keyIt = keys.iterator();
	for(Iterator it = buttons.iterator(); it.hasNext(); ) {
	    Object key = keyIt.next();
	    if (((AbstractButton)it.next()).isSelected()) {
		selected.add(key);
	    }
	}
	(new PolicySelectedCommand(viewableName, selected)).issue();
        dispose();
      }
    }

  }
}
