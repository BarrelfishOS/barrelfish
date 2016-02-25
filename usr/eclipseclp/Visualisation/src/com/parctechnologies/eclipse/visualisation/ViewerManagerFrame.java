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
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.*;
import java.util.*;
import java.awt.*;
import java.awt.event.*;
import java.beans.*;

/**
 * The ViewerManagerFrame is a delagatee of the ViewerManager. Its
 * responsibilities are the graphical management of the top-level vis client
 * window, including the menu, control panel, and desktop area on which the
 * viewers are placed.
 *
 *
 */
class ViewerManagerFrame extends JFrame
{
  // The desktop pane is where the viewer windows are.
  private JDesktopPane jDesktopPane;

  // This map is used to find out which internal frame to remove when a viewable
  // is destroyed.
  private Map viewerToJInternalFrame;
  private VisClientStateModel stateModel;

  // the control panel is the part of the frame below the desktop area, where
  // the resume/interrupt control buttons are.
  private ControlPanel controlPanel;
  private final static int INITIAL_WIDTH = 800;
  private final static int INITIAL_HEIGHT = 600;
  private JMenuBar jMenuBar;

  // This map is used to allow the addition of menu items just based on menu
  // title (see method addMenuItem)
  private Map menuTitleToMenu;
  private ActionButton resumeButton;
  private ActionButton interruptButton;

  // label to record the last event;
  private JLabel lastEventLabel;

  public ViewerManagerFrame(VisClientStateModel stateModel)
  {
    super("ECLiPSe Visualisation Client ["+stateModel.getVisClientName().functor()+"]");
    this.stateModel = stateModel;
    initialise();
  }

  private void initialise()
  {
    // unfortunately to manage the keyboard focus exactly, we have to globally
    // alter the focus manager. In Java version 1.4 there are facilities for
    // doing this more locally, so this would be worth updating.
    javax.swing.FocusManager.
      setCurrentManager(new MyFocusManager());
    getContentPane().setLayout(new GridBagLayout());
    viewerToJInternalFrame = new HashMap();
    initialiseDesktopPane();
    initialiseControlPanel();
    initialiseLastEventLabel();
    initialiseMenu();

    // the WindowListener listens for windowClose events, and cleans up
    // appropriately.
    initialiseWindowListener();

    // locate the window in the center for the screen
    Dimension screenSize =
      Toolkit.getDefaultToolkit().getScreenSize();
    Dimension windowSize = getPreferredSize();
    setLocation(Math.max(screenSize.width/2 - (windowSize.width/2),2),
		Math.max(screenSize.height/2 - (windowSize.height/2),2));
  }

  void setLastEventString(String newString)
  {
    lastEventLabel.setText(" "+newString);
  }

  private void initialiseLastEventLabel()
  {
    GridBagConstraints lelc = new GridBagConstraints();
    lelc.fill = GridBagConstraints.HORIZONTAL;
    lelc.weightx = 1;
    lelc.weighty = 0;
    lelc.gridy = 2;

    lastEventLabel = new JLabel(" ", JLabel.LEFT);
    lastEventLabel.setBorder(new BevelBorder(BevelBorder.LOWERED));
    getContentPane().add(lastEventLabel, lelc);

  }

  // It is necessary to explicitly manage the keyboard focus of the Resume and
  // interrupt buttons. Otherwise when one has the focus and becomes disabled,
  // the Swing DefaultFocusManager looks for another component to receive the
  // focus, sometimes traversing the entire set of viewlet components in a large
  // table viewer, and causing long delays.
  private class MyFocusManager extends javax.swing.DefaultFocusManager
  {
     public Component getComponentBefore(Container container, Component component)
     {
       if(component == resumeButton)
       {
         return(interruptButton);
       }
       if(component == interruptButton)
       {
         return(resumeButton);
       }
       return(super.getComponentBefore(container, component));
     }
     public Component getComponentAfter(Container container, Component component)
     {
       if(component == resumeButton)
       {
         return(interruptButton);
       }
       if(component == interruptButton)
       {
         return(resumeButton);
       }
       return(super.getComponentAfter(container, component));
     }
  }

  /**
   * Returns the mnemonic to use for a given menuTitle
   */
  protected int getMenuMnemonic(String menuTitle) {
    if ("File".equals(menuTitle)) return KeyEvent.VK_F;
    if ("Options".equals(menuTitle)) return KeyEvent.VK_O;
    return -1;
  }

  /** Convenience method to add menu items based on the menu title: if a menu
   * with that title exists, add to that menu. Otherwise, create menu with that
   * title and add to that.*/
  private void addMenuItem(String menuTitle, Object item)
    {
      JMenu menu = (JMenu) menuTitleToMenu.get(menuTitle);
      if(menu == null)
      {
        menu = new JMenu(menuTitle);
        int mnemonic = getMenuMnemonic(menuTitle);
        if (mnemonic != -1) {
            menu.setMnemonic(mnemonic);
        }
        jMenuBar.add(menu);
        menuTitleToMenu.put(menuTitle, menu);
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

  private void initialiseMenu()
  {
    jMenuBar = new JMenuBar();
    setJMenuBar(jMenuBar);
    menuTitleToMenu = new HashMap();

    addMenuItem("File", new LoadAction());
    addMenuItem("File", new SaveAction());
    addMenuItem("File", new ExitAction());

    JCheckBoxMenuItem autoResumeOption =
      new JCheckBoxMenuItem("Auto Resume");

    // The following line means that the true/false state of the
    // autoResumeOption checkbox always matches that of the autoResume boolean
    // in the vis client stateModel.
    autoResumeOption.setModel(new BooleanPropertyModel("autoResume", stateModel,
                              stateModel.getPropertyChangeSupport()));
    addMenuItem("Options",
                autoResumeOption);

  }

  private void initialiseDesktopPane()
  {
    jDesktopPane = new JDesktopPane();
    jDesktopPane.setPreferredSize(new Dimension(INITIAL_WIDTH, INITIAL_HEIGHT));

    GridBagConstraints dpc = new GridBagConstraints();
    dpc.fill = GridBagConstraints.BOTH;
    dpc.weightx = 1;
    dpc.weighty = 1;

    getContentPane().add(jDesktopPane, dpc);

    JInternalFrame jInternalFrame = new JInternalFrame("dummy");
    jInternalFrame.setDefaultCloseOperation(JInternalFrame.DISPOSE_ON_CLOSE);

    jDesktopPane.add(jInternalFrame);
    jInternalFrame.setVisible(true);
    try {
        jInternalFrame.setClosed(true);
    } catch(PropertyVetoException pve) {
        pve.printStackTrace(System.err);
    }

  }

  private void initialiseControlPanel()
  {
    GridBagConstraints cpc = new GridBagConstraints();
    cpc.fill = GridBagConstraints.HORIZONTAL;
    cpc.weightx = 1;
    cpc.weighty = 0;
    cpc.gridy = 1;

    controlPanel = new ControlPanel(this);
    getContentPane().add(controlPanel, cpc);
  }

  private void initialiseWindowListener()
  {
    addWindowListener(new WindowClosingListener());
  }

  /**
   * This method is invoked by ViewerManager during the startEvent stage of
   * a CreateEvent. It adds a jInternalFrame with the new Viewer's component to
   * the desktop pane.
   */
  void addViewer(Viewer newViewer, String viewableName)
  {
    try
    {
      SwingUtilities.invokeAndWait(new ViewerAdder(newViewer, viewableName));
    }
    catch(Exception e)
    {
      e.printStackTrace(System.err);
      throw(new RuntimeException("Exception thrown by event thread executing"
                                 +" ViewerAdder: \n"+e));
    }
  }


  /**
   * This method is invoked by ViewerManager during the stopEvent stage of
   * a DestroyEvent. It removes the condemned Viewer's jInternalFrame from
   * the desktop pane.
   */
  void removeViewer(Viewer viewer)
  {
    (new ViewerRemover(viewer)).run();
  }

  // called by the Exit Action or by the window listener, when it hears a window
  // closing event.
  private void exit()
  {
    dispose();
    stateModel.setTerminate(true);
  }



  // The controlPanel is a component of the ViewerManagerFrame. It contains
  // buttons which give the user access to the stateModel's resume and interrupt
  // actions. It observes two properties of the vis client state model:
  // autoResume and the state integer. The control panel is responsible for
  // managing the keyboard focus of the interrupt/resume buttons, which is
  // done by observation of the state integer. It is also responsible for the
  // presence/absence of the autoResumePanel, which is done by observation of
  // the autoResume boolean property.
  private class ControlPanel extends JPanel implements PropertyChangeListener
  {
    private AutoResumePanel autoResumePanel;
    private ViewerManagerFrame enclosingComponent;

    ControlPanel(ViewerManagerFrame  enclosingComponenet)
    {
      super(new FlowLayout());
      this.enclosingComponent = enclosingComponent;
      initialiseButtons();
      initialiseAutoResumePanel();
    }

    public void propertyChange(PropertyChangeEvent event)
    {
      if(event.getPropertyName().equals("autoResume"))
      {
        if(event.getNewValue().equals(Boolean.TRUE))
        {
          showAutoResumePanel();
        }
        else
        {
          hideAutoResumePanel();
        }
        return;
      }
      if(event.getPropertyName().equals("currentVisClientState"))
      {
        if(((Integer) event.getNewValue()).intValue()
            == VisClientStateModel.HELD_ON_EVENT
            && !(interruptButton.hasFocus() && stateModel.getAutoResume()))
        {

	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this, "requesting focus for resume button");
	    }

          resumeButton.requestFocus();
          return;
        }
        if(((Integer) event.getNewValue()).intValue()
            == VisClientStateModel.NO_CURRENT_EVENT)
        {

	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this, "requesting focus for interrupt button");
	    }

          interruptButton.requestFocus();
          return;
        }
      }
    }

    private void initialiseButtons()
    {
      stateModel.getPropertyChangeSupport().
        addPropertyChangeListener("currentVisClientState", this);
      // Note: ActionButton makes a button from an action.
      resumeButton = new ActionButton(stateModel.getResumeAction());
      interruptButton = new ActionButton(stateModel.getInterruptAction());
      this.add(resumeButton);
      this.add(interruptButton);
    }


    private void initialiseAutoResumePanel()
    {
      autoResumePanel = new AutoResumePanel(stateModel);
      if(stateModel.getAutoResume())
      {
        showAutoResumePanel();
      }
      stateModel.getPropertyChangeSupport().
        addPropertyChangeListener("autoResume", this);
    }

    private void showAutoResumePanel()
    {
      this.add(autoResumePanel);
      revalidate();
      this.repaint();
    }

    private void hideAutoResumePanel()
    {
      this.remove(autoResumePanel);
      revalidate();
      this.repaint();
    }
  }

  /**
   * This component listener is responsible for recording any user
   * resize or movement actions as Commands and posting them to the
   * ScenarioManager
   *
   */
  private class ViewerInternalFrameComponentListener
      implements ComponentListener
  {
    private Viewer viewer;
    private JInternalFrame frame;
    ViewerInternalFrameComponentListener(Viewer viewer, JInternalFrame frame)
    {
      this.viewer = viewer;
      this.frame = frame;
    }
    public void componentMoved(ComponentEvent e) {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this,"InternalFrame moved e="+e);
	}

      Command command = new ViewerInternalFrameBoundsCommand(viewer,
							     frame.getBounds());
      ScenarioManager.getInstance().processCommand(command);
    }

    public void componentResized(ComponentEvent e) {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this,"InternalFrame resized e="+e);
	}

      Command command = new ViewerInternalFrameBoundsCommand(viewer,
							     frame.getBounds());
      ScenarioManager.getInstance().processCommand(command);
    }

    public void componentShown(ComponentEvent e) {
    }
    public void componentHidden(ComponentEvent e) {
    }
  }

  // Listens to whether the viewer's internal frame is maximized and
  // records the corresponding action
  private class ViewerMaximizeListener implements PropertyChangeListener
  {
    private Viewer viewer;
    ViewerMaximizeListener(Viewer viewer)
    {
      this.viewer = viewer;
    }
    public void propertyChange(PropertyChangeEvent event)
    {
      boolean maximize = ((Boolean) event.getNewValue()).booleanValue();
      ScenarioManager.getInstance().
        processCommand(new ViewerSetMaximizedCommand(viewer, maximize));
    }

  }

  // This InternalFrameListener makes sure that the gainFocus[loseFocus] method
  // of the viewer is invoked when its internal frame is activated [deactivated]
  // and also that [de]iconify actions are recorded.
  private class ViewerInternalFrameListener extends InternalFrameAdapter
  {
    private Viewer viewer;
    ViewerInternalFrameListener(Viewer viewer)
    {
      this.viewer = viewer;
    }
    public void internalFrameActivated(InternalFrameEvent e)
    {
      ScenarioManager.getInstance().processCommand(new ViewerActivationCommand(viewer, true));
      (new ViewerFocusCommand(viewer, true)).issue();
    }
    public void internalFrameDeactivated(InternalFrameEvent e)
    {
      ScenarioManager.getInstance().processCommand(new ViewerActivationCommand(viewer, false));
      (new ViewerFocusCommand(viewer, false)).issue();
    }
    public void internalFrameIconified(InternalFrameEvent e)
    {
      ScenarioManager.getInstance().processCommand(new ViewerIconifyCommand(viewer));
    }
    public void internalFrameDeiconified(InternalFrameEvent e)
    {
      ScenarioManager.getInstance().processCommand(new ViewerDeiconifyCommand(viewer));
    }
    public void internalFrameClosed(InternalFrameEvent e)
    {
      // this command will close the window if it is not already closed, and
      // also do all the other book-keeping related to it whether or not the
      // window is closed
      (new ViewerCloseCommand(viewer)).issue();

    }
  }



  private class WindowClosingListener extends WindowAdapter
  {
    public void windowClosing(WindowEvent e)
    {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "window closing...");
	}

      exit();
    }
  }

  private class ExitAction extends AbstractAction
  {
    ExitAction()
    {
      super("Exit");
    }

    public void actionPerformed(ActionEvent e)
    {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "exit action performed...");
	}

      exit();
    }
  }


  private class LoadAction extends AbstractAction
  {
    LoadAction()
    {
      super("Load");
    }

    public void actionPerformed(ActionEvent e)
    {

      if (DebuggingSupport.logMessages) {
        DebuggingSupport.logMessage(this, "load action performed...");
      }
      ScenarioManager.getInstance().loadScenarioForPlayback();
    }
  }

  private class SaveAction extends AbstractAction
  {
    SaveAction()
    {
      super("Save");
    }

    public void actionPerformed(ActionEvent e)
    {

	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "save action performed...");
	}
        ScenarioManager.getInstance().saveScenarioToDisk();
    }
  }


  /**
   * Adding and removing viewers contain invocations which should be made on the
   * Event thread, so we encapsulate this code as a Runnable object so that it
   * can be passed to SwingUtilities.invokeAndWait
   */
  private class ViewerAdder implements Runnable
  {
    private Viewer newViewer;
    private String viewableName;

    ViewerAdder(Viewer newViewer, String viewableName)
    {
      this.newViewer = newViewer;
      this.viewableName = viewableName;
    }

    public void run()
    {
      String title = viewableName+" - "+newViewer.getDescription();
      JInternalFrame jInternalFrame =
        new JInternalFrame(title, true, true, true, true);

      jInternalFrame.setDefaultCloseOperation(JInternalFrame.DISPOSE_ON_CLOSE);
      jInternalFrame.getRootPane().setJMenuBar(newViewer.getJMenuBar());
      jInternalFrame.getContentPane().add(newViewer.getComponent());
      jInternalFrame.pack();

      // Viewer internal frames must fit within the current size of the desktop
      // pane.
      int vWidth = Math.min(jInternalFrame.getWidth(), jDesktopPane.getWidth());
      int vHeight =
        Math.min(jInternalFrame.getHeight(), jDesktopPane.getHeight());
      jInternalFrame.setSize(vWidth, vHeight);
      jInternalFrame.
        addInternalFrameListener(new ViewerInternalFrameListener(newViewer));
      jInternalFrame.addComponentListener(new ViewerInternalFrameComponentListener(newViewer, jInternalFrame));
      jInternalFrame.
        addPropertyChangeListener(jInternalFrame.IS_MAXIMUM_PROPERTY,
                                  new ViewerMaximizeListener(newViewer));
      jDesktopPane.add(jInternalFrame);
      jInternalFrame.setVisible(true);
      viewerToJInternalFrame.put(newViewer, jInternalFrame);
    }
  }

  private class ViewerRemover implements Runnable
  {
    private Viewer viewer;

    ViewerRemover(Viewer viewer)
    {
      this.viewer = viewer;
    }

    public void run()
    {
      JInternalFrame jInternalFrame =
        (JInternalFrame) viewerToJInternalFrame.get(viewer);

      if(jInternalFrame != null && !jInternalFrame.isClosed())
      {

	  if (DebuggingSupport.logMessages) {
	      DebuggingSupport.logMessage(this,
					  "closing internal frame on thread "+
					  Thread.currentThread());
	  }

        try
        {
          jInternalFrame.setClosed(true);
        } catch(PropertyVetoException pve){
            pve.printStackTrace();
        }

        viewerToJInternalFrame.remove(viewer);
      }

    }
  }
}
