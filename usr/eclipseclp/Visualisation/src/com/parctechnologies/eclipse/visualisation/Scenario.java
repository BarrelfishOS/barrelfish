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

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;
import java.io.Serializable;
import java.io.*;

/**
 * This is class designed to hold a sequence of changes to the
 * visualisation client state, intermixed with eclipse events with a
 * view to replaying these as a response to sufficiently similar
 * sequence of eclipse events.
 *
 * Scenarios will be associated with specific Viewables and will
 * record user commands carried out on all Viewers associated with that
 * Viewable.
 */

public class Scenario implements Serializable {
    /**
     * Holds the name of the viewable for which this scenario applies
     */
    private Object viewableName;
    
    /**
     * Stores (in order) the list of events that have been received
     * from eclipse for this viewable.
     *
     * This list will contain instances of the inner class EventCommandList
     */
    private LinkedList eventList;

    /**
     * Constructs a Scenario for the given Viewable.
     */
    public Scenario(String viewableName) {
	setViewableName(viewableName);
	this.eventList = new LinkedList();
    }


    /**
     * Add an either an Event or an Command to the Scenario
     */
    public void add(Object obj, int state) {
	if (obj instanceof VisEvent) {
	    addEvent((VisEvent)obj, state);
	} else if (obj instanceof Command) {
	    addCommand((Command)obj, state);
	} else {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "Unable to add unknown object");
	    }

	    throw new IllegalArgumentException("Scenario was unable to add an object of type "+obj.getClass());
	}
	
    }
    /**
     * Records that a visualisation event has taken place
     */
    public void addEvent(VisEvent event, int state) {
	eventList.add(new EventCommandList(event));
	
	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "Adding event " + event);
	}

    }

    /**
     * Records that a user command has taken place
     */
    public void addCommand(Command command, int state) {
	((EventCommandList)(eventList.getLast())).add(command, state);
	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this,
					"Adding command " + command +
					" state=" + state);
	}

    }

    /**
     * Get the value of viewableName.
     * @return value of viewableName.
     */
    public Object getViewableName() {
	return viewableName;
    }
    
    /**
     * Set the value of viewableName.
     * @param v  Value to assign to viewableName.
     */
    public void setViewableName(String  v) {
	this.viewableName = v;
    }


    /**
     * Returns the list of EventCommandList objects
     *
     */
    List getEventList() {
	return eventList;
    }


    /**
     * Returns a ScenarioIterator for this scenario
     */
    public ScenarioIterator iterator() {
	return new ScenarioIterator(this);
    }


    /**
     * Save the Scenario to a file
     */
    public void save(String fname) {
	try {
	    FileOutputStream ostream = new FileOutputStream(fname);
	    ObjectOutputStream p = new ObjectOutputStream(ostream);
	    
	    p.writeObject(this);
	    
	    p.flush();
	    ostream.close();
	} catch(IOException e) {
	    e.printStackTrace();
	}
    }

    
    /**
     * Return contents of scenario as a string
     */
    public String toString() {
        StringBuffer sb =
            new StringBuffer("\n\nScenario for " + viewableName + "\n" );
        for(Iterator it = eventList.iterator(); it.hasNext(); ) {
            EventCommandList ecl = (EventCommandList)(it.next());
            sb.append(ecl.toString());
        }
        return sb.toString();
    }


    /**
     * A class used to record the user commands which have happened
     * since the last VisEvent on this Viewable
     */
    final class EventCommandList implements Serializable {
	/**
	 * Holds the event for which the commands exist
	 */
	private VisEvent visEvent;

	/**
	 * Holds the commands associated with this event
	 */
	private List commandList;

	/**
	 * Holds the state of the VisClient when the commands were issued
	 */
	private List stateList;
	
	/**
	 * Get the value of commandList.
	 * @return value of commandList.
	 */
	public final List getCommandList() {
	    return commandList;
	}
	
	/**
	 * Get the value of commandList, filtered by state.
	 * @return list of cammands which were recorded in the given state.
	 */
	public final List getCommandList(int state) {
	    List list = new LinkedList();
	    Iterator itCommand = commandList.iterator();
	    Iterator itState = stateList.iterator();
	    while (itCommand.hasNext()) {
		Command command = (Command)itCommand.next();
		Integer integer = (Integer)itState.next();
		if (integer.intValue() == state) {
		    list.add(command);
		}
	    }
	    return list;
	}
	
	/**
	 * Set the value of commandList.
	 * @param v  Value to assign to commandList.
	 */
	private final void setCommandList(List  v) {
	    this.commandList = v;
	}
	
	/**
	 * Get the value of visEvent.
	 * @return value of visEvent.
	 */
	public final VisEvent getVisEvent() {
	    return visEvent;
	}
	
	/**
	 * Set the value of visEvent.
	 * @param v  Value to assign to visEvent.
	 */
	private final void setVisEvent(VisEvent  v) {
	    this.visEvent = v;
	}
	
	
	/**
	 * Create a new CommandEventList to store the commandsassociated
	 * with the given VisEvent.
	 */
	public EventCommandList(VisEvent visEvent) {
	    setCommandList(new LinkedList());
	    this.stateList = new LinkedList();
	    setVisEvent(visEvent); 
	}

	/**
	 * Adds an command to the list
	 */
	public final void add(Command command, int state) {
	    commandList.add(command);
	    stateList.add(new Integer(state));
	}


        /**
         * Returns the command list as a string
         */
        public String toString() {
            StringBuffer sb = new StringBuffer("\nCommandList\n");
	    List list = new LinkedList();
	    Iterator itCommand = commandList.iterator();
	    Iterator itState = stateList.iterator();
	    while (itCommand.hasNext()) {
		Command command = (Command)itCommand.next();
		Integer integer = (Integer)itState.next();
                sb.append("state:"+integer+" command:"+command);
	    }
	    return sb.toString();
        }
    }
    
}
