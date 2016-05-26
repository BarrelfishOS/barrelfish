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

import java.util.*;
import java.beans.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.filechooser.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

/**
 * A class designed to automate the recording and playback of
 * Scenarios for different Viewables
 *
 */


public class ScenarioManager {

    /**
     * Stores scenarios against their ViewableNames
     */
    private HashMap scenarioMap;

    /**
     * Stores currently active scenarioIterators against their ViewableNames
     */
    private HashMap iteratorMap;

    /**
     * The state of the vis client
     */
    private static VisClientStateModel stateModel ;


    /**
     * Hold the singleton instance
     */
    private static ScenarioManager instance ;

    /**
     * Holds the last opened directory for the file chooser
     */
    private File defaultScenarioDirectory;

    /**
     *  Returns the singleton instance
     */
    public static synchronized ScenarioManager getInstance() {
	if (instance == null) {
	    instance = new ScenarioManager();
	}
	return instance;
    }


    /**
     * Must be called to initailise the staic fields
     */
    public static void initialise(VisClientStateModel model) {
	stateModel = model ;
    }

    /**
     * Construct the scenario manager
     */
    private ScenarioManager() {
	scenarioMap = new HashMap();
	iteratorMap = new HashMap();
    }


    /**
     * Provides access to the state mode for replaying commands
     */
    public VisClientStateModel getVisClientStateModel() {
	return stateModel;
    }

    /**
     * Store the new scenario and associate it with its name
     */
    public void registerScenario(Scenario scenario) {
	scenarioMap.put(scenario.getViewableName(), scenario);
    }

    /**
     * Returns a collection of potential secnarios for the given
     * Viewable name.
     *
     * If no scenario can be found then a new (and empty) scenario will
     * be returned
     */
    public Collection getScenarioForName(String name) {
	Scenario scenario = null;

	if ( scenarioMap.containsKey(name)) {
	    scenario = (Scenario)scenarioMap.get(name);
	} else {
	    scenario = new Scenario(name);
	    registerScenario(scenario) ;
	}

	List list = new LinkedList();
	list.add(scenario);
	return list;
    }

    /**
     * Returns the single selected secnarios for the given
     * Viewable name.
     *
     * If no scenario can be found then null will be returned
     */
    public Scenario getExactScenarioForName(Object name) {
	if ( scenarioMap.containsKey(name)) {
	    return (Scenario)scenarioMap.get(name);
	}
	return null;
    }

    /**
     * Returns the scenario which should be used for playback on the given
     * Viewable
     *
     * If no scenario can be found then null is returned
     */
    public Scenario getMatchingScenarioForPlayback(Object viewableName) {
	// No iterator is active, try and find one
	if ( scenarioMap.containsKey(viewableName) ) {
	    // Found a mathcing scenario, so use an iterator from there
	    Scenario scenario = (Scenario)scenarioMap.get(viewableName);
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "Found a matching scenario, so use an iterator from there :"+scenario);
	    }

	    return scenario;
	}
	return null;
    }

    /**
     * Register an iterator for playback
     */
    public void registerIteratorForPlayback(Scenario scenario,
					    Object viewableName) {
	iteratorMap.put(viewableName, scenario.iterator());
    }

    /**
     * Clears any iterators for a given viewable name
     */
    public void clearIteratorForPlayback(Object viewableName) {
	iteratorMap.remove(viewableName);
    }


    /**
     * Returns the scenario iterator which is being used for playback
     * Viewable
     *
     * If no scenarioIterator can be found then null is returned
     */
    public ScenarioIterator getPlayingIteratorForName(Object viewableName) {
	if ( iteratorMap.containsKey(viewableName) ) {
	    // An iterator is already active for this viewable
	    ScenarioIterator iterator = (ScenarioIterator)iteratorMap.get(viewableName);
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "An iterator is already active for this viewable :"+iterator);
	    }

	    return iterator;
	}
	return null;
    }



    /**
     * Offers the user a list of loaded scenarios and saves the
     * selected ones
     */
    public void saveScenarioToDisk() {
	Collection scenarios = selectScenarios();
        if ( scenarios != null ) {
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "\nSaving :"+scenarios+"\n");
	    }
            saveScenarios(scenarios);
        }
    }


    /**
     * Offer the user a list of currently registered scenarios
     */
    Collection selectScenarios() {
        Collection scenarios = null;
        Scenario[] scenarioArray = new Scenario[ scenarioMap.size() ];
        Object[] descArray = new Object[ scenarioMap.size() ];
        JCheckBox[] buttonArray = new JCheckBox[ scenarioMap.size()];
        int i = 0;
        if (descArray.length == 0) {
            JOptionPane.showMessageDialog(null,
                                          "There are no currently no " +
                                          "scenarios in memory.");
            return null;
        }
        for(Iterator it = scenarioMap.keySet().iterator(); it.hasNext();) {
            Object key = it.next();
            Scenario scen = (Scenario)(scenarioMap.get(key));
            descArray[i] = key.toString();
            scenarioArray[i] = scen;
            buttonArray[i] = new JCheckBox(key.toString());
            buttonArray[i].setSelected(false);
            i++;
        }
        int selectedValue =
            JOptionPane.showConfirmDialog(null,
                                          buttonArray,
                                          "Select the scenarios to save",
                                          JOptionPane.OK_CANCEL_OPTION,
                                          JOptionPane.INFORMATION_MESSAGE);
        if (selectedValue == JOptionPane.CANCEL_OPTION) {
            return null;
        }
        // find the scenario which matched the selected
        for(i = 0; i < buttonArray.length; i++) {
            if ( buttonArray[i].isSelected() ) {
                if (scenarios == null) {
                    scenarios = new LinkedList();
                }
                scenarios.add(scenarioArray[i]);
            }
        }
        return scenarios;
    }


    /**
     * Saves the scenarios to disk
     *
     */
    void saveScenarios(Collection scenarios) {
        Object viewableName = "";
        if (scenarios.size()==1) {
            viewableName =
                ((Scenario)(scenarios.iterator().next())).getViewableName();
        }
	JFileChooser fileChooser = new ScenarioFileChooser(viewableName);
	if(defaultScenarioDirectory != null) {
	    fileChooser.setCurrentDirectory(defaultScenarioDirectory);
	}
	int returnVal = fileChooser.showSaveDialog(null);
	if(returnVal == JFileChooser.APPROVE_OPTION) {
	    File file = fileChooser.getSelectedFile() ;
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "scenario: " +
					    file +
					    " approved");
	    }
            if (!file.toString().endsWith(".scenario") &&
                !file.toString().endsWith(".SCENARIO")) {
                file = new File(file.toString()+".scenario");
            }
	    if(!file.exists() ||
               (JOptionPane.YES_OPTION == 
                JOptionPane.showConfirmDialog(null,
                                              "File already exists\n"+
                                              "Replace "+file+"?",
                                              "File exists",
                                              JOptionPane.YES_NO_OPTION)
                )
               ) {
		try {
		    // Load the scenario
		    saveScenariosToFile(file, scenarios);
		} catch(IOException ioe) {
		    JOptionPane.
			showMessageDialog(null,
					  "File \n" +
					  file +
					  "\n exception occured during save\n"+
					  ioe,
					  "Error in Visualisation Client",
					  JOptionPane.ERROR_MESSAGE);
                    if (DebuggingSupport.logMessages ) {
                        DebuggingSupport.logMessage(this,
                                                    ioe + " during save.");
                        ioe.printStackTrace();
                    }
		} catch(ClassNotFoundException ioe) {
		    JOptionPane.
			showMessageDialog(null,
					  "File \n" +
					  file +
					  "\n exception occured during save\n"+
					  ioe,
					  "Error in Visualisation Client",
					  JOptionPane.ERROR_MESSAGE);
                    if (DebuggingSupport.logMessages ) {
                        DebuggingSupport.logMessage(this,
                                                    ioe + " during save.");
                        ioe.printStackTrace();
                    }
		}
	    } else {
		JOptionPane.
		    showMessageDialog(null,
				      "File \n" +
				      file +
				      "\nwas not saved",
				      "Warning",
				      JOptionPane.ERROR_MESSAGE);
	    }
	}
	defaultScenarioDirectory = fileChooser.getCurrentDirectory();
    }


    /**
     * Saves the scenarios to the given file
     */
    private void saveScenariosToFile(File file, Collection scenarios)
	throws IOException,
	       ClassNotFoundException {
	FileOutputStream ostream = new FileOutputStream(file);
	ObjectOutputStream p = new ObjectOutputStream(ostream);
	
	p.writeObject(scenarios);
	ostream.close();
    }


    /**
     * Loads arbitrary scenarios
     */
    public void loadScenarioForPlayback() {
	Collection scenarios = loadScenarios("");
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,
                                        "\nLoaded :"+scenarios+"\n");
        }
        if ( (scenarios != null) && (scenarios.size() > 0) ) {
            Object[] message = new String[scenarios.size()+1];
            message[0] = "Successfully loaded scenarios for viewables : ";
            int i = 1;
            for(Iterator it = scenarios.iterator(); it.hasNext(); i++) {
                Scenario scenario = (Scenario)it.next();
                registerScenario(scenario);
                message[i] = scenario.getViewableName();
            }
            JOptionPane.showMessageDialog(null,
                                          message);
        }
    }

    /**
     * Loads scenarios from disk
     *
     */
    public Collection loadScenarios(Object viewableName) {
	JFileChooser fileChooser = new ScenarioFileChooser(viewableName);
	if(defaultScenarioDirectory != null) {
	    fileChooser.setCurrentDirectory(defaultScenarioDirectory);
	}
	int returnVal = fileChooser.showOpenDialog(null);
	Collection scenarios = null;
	if(returnVal == JFileChooser.APPROVE_OPTION) {
	    File file = fileChooser.getSelectedFile() ;
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "scenario: " +
					    file +
					    " approved");
	    }

	    if(file.exists()) {
		try {
		    // Load the scenario
		    scenarios = loadScenariosFromFile(file);
		} catch(IOException ioe) {
		    JOptionPane.
			showMessageDialog(null,
					  "File \n" +
					  file +
					  "\n exception occured during load\n"+
					  ioe,
					  "Error in Visualisation Client",
					  JOptionPane.ERROR_MESSAGE);
		} catch(ClassNotFoundException ioe) {
		    JOptionPane.
			showMessageDialog(null,
					  "File \n" +
					  file +
					  "\n exception occured during load\n"+
					  ioe,
					  "Error in Visualisation Client",
					  JOptionPane.ERROR_MESSAGE);
		}
	    } else {
		JOptionPane.
		    showMessageDialog(null,
				      "File \n" +
				      file +
				      "\ndoes not exist",
				      "Error in Visualisation Client",
				      JOptionPane.ERROR_MESSAGE);
	    }
	}
	defaultScenarioDirectory = fileChooser.getCurrentDirectory();
	return scenarios;
    }

    /**
     * Loads scenarios from the given file
     */
    private Collection loadScenariosFromFile(File file)
	throws IOException,
	       ClassNotFoundException {
	FileInputStream istream = new FileInputStream(file);
	ObjectInputStream p = new ObjectInputStream(istream);

	Collection scenarios = null;
	Object o = p.readObject();
        if (o instanceof Scenario) {
            // support old scenario format 
            scenarios = new ArrayList(1);
            scenarios.add(o);
        } else {
            scenarios = (Collection)o;
        }
	istream.close();
 	return scenarios;
    }


    /**
     * Asks the user to select a scenario for playback
     */
    public void selectPlaybackScenario(String viewableName) {
        Scenario scenario =
            getMatchingScenarioForPlayback(viewableName);
        // Pop up dialog asking the user if they wish to use the existing setup
        boolean useExistingScenario = false;
        // Either there was no scenario, or it is a freshly created one
        if (scenario != null) {
            int result =
                JOptionPane.showConfirmDialog(null,
                                              "Reinstate saved visualisation preferences for viewable "+
                                              viewableName+"?",
                                              "Visualisation preferences",
                                              JOptionPane.YES_NO_OPTION);
            useExistingScenario = (result == JOptionPane.YES_OPTION);
        }
        // Pop up a dialog from which the user may select their viewers
        if (useExistingScenario) {
            // The scenario manager will play the commands
            // that set the viewerBuildingPolicy, so nothing to do apart
            // from telling the scenario manager to play
            ScenarioManager.getInstance().
                registerIteratorForPlayback(scenario, viewableName);
        } else {
            ScenarioManager.getInstance().
                clearIteratorForPlayback(viewableName);
            
        }
    }

    /**
     * Called by the VisClient to indicate that an event has occured.
     */
    public void processEvent(VisEvent event) {
	if (stateModel.getRecordScenario() &&
	    (stateModel.getCurrentState() == VisClientStateModel.SETTING_VIEWER_POLICY)) {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "ScenarioManager recorded event " +
					    event);
	    }

	    if (event instanceof CreateEvent) {
		registerScenario(new Scenario(event.getViewableName()));
	    }
	    Scenario scenario =
		getExactScenarioForName(event.getViewableName()) ;
	    if (scenario != null) {
		scenario.add(event, stateModel.getCurrentState());
	    } else {
		
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.logMessage(this,
						"ScenarioManager no scenario found for viewable " + event.getViewableName());
		}

	    }
	} else {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "ScenarioManager not recorded event " +
					    event);
	    }

	}
        ScenarioIterator iterator =
	    getPlayingIteratorForName(event.getViewableName()) ;
        if (iterator != null) {
	    if (iterator.matches(event)) {
	
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.logMessage(this,
						"ScenarioManager playing commands for event " +
						event);
		}

	        iterator.play(stateModel.getCurrentState());
	    } else {
	
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.logMessage(this,
						"ScenarioManager event does not match scenario recieved (" + event + ") was expecting (" + iterator.peekEvent() + ")");
		}

	    }
        } else {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "ScenarioManager no iterator exists for "+ event.getViewableName());
	    }

	}
        // Clean up after a viewable has gone away
	if ((event instanceof DestroyEvent) &&
	    (stateModel.getCurrentState() == VisClientStateModel.NO_CURRENT_EVENT)
	    ) {
	    iteratorMap.remove(event.getViewableName());
        }
    }

    /**
     * Called by the many objects to indicate that a command has been issued
     */
    public void processCommand(Command command) {
	if (stateModel.getRecordScenario()) {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "ScenarioManager recorded command " +
					    command);
	    }

            String name = null;
	    if (command instanceof SymRefCommand) {
		name =
		    ((SymRefCommand)command).getSymRef().getRoot().toString();
		// Remove leading "/"
		name = name.substring(1);
            } else if (command instanceof PolicySelectedCommand) {
                name = ((PolicySelectedCommand)command).getViewableName();
            }
            if ( name != null ) {
                Scenario scenario =
                    getExactScenarioForName(name) ;
		if (scenario != null ) {
		    scenario.add(command, stateModel.getCurrentState());
		} else {
		
		    if (DebuggingSupport.logMessages) {
			DebuggingSupport.logMessage(this,
						    "ScenarioManager no scenario found for viewable " + name);
		    }

		}
	    } else {
		
		if (DebuggingSupport.logMessages) {
		    DebuggingSupport.logMessage(this,
						"ScenarioManager (wrong type) unable to record command " +
						command);
		}

	    }
	} else {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "ScenarioManager not recorded command " +
					    command);
	    }

	}
    }


    /**
     * Defines a file filter for Scenario files
     */
    private static class ScenarioFileFilter extends FileFilter
    {
	Object viewableName ;

	public ScenarioFileFilter(Object viewableName) {
	    super();
	    this.viewableName = viewableName;
	}

	public String getDescription()
	{
	    return("Visualisation Scenario");
	}
	public boolean accept(File f)
	{
	    return( f.isDirectory() ||
                    (f.toString().endsWith(".scenario") ||
                     f.toString().endsWith(".SCENARIO")
                     )
                    );
	}
    }

    /**
     * A dialog for selecting scenarios
     */
    private static class ScenarioFileChooser extends JFileChooser implements PropertyChangeListener
    {
	Object viewableName;

	ScenarioFileChooser(Object viewableName)
	{
	    super();
	    this.viewableName = viewableName;
	    removeChoosableFileFilter(getAcceptAllFileFilter());
	    // This means that if a directory is opened it is navigated to.
	    setFileSelectionMode(FILES_ONLY);
	    setFileFilter(new ScenarioFileFilter(viewableName));
	    // This means that when the directory is opened its name is not retained
	    // as the selected file.
	    addPropertyChangeListener(DIRECTORY_CHANGED_PROPERTY, this);
	}
	public void propertyChange(PropertyChangeEvent event)
	{
	    setSelectedFile(null);
	}
	
    }

}
