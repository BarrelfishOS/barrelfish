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

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;
import javax.swing.SwingUtilities;
/**
 * This class represents the current state of a scenario playback
 *
 *
 */
public class ScenarioIterator {

    /** The model for this iterator */
    private Scenario scenario;

    /** The index of the next event to play in the scenario */
    private int index;

    /**
     * Unification map, holding the mappings between scenario SymRefs and
     * the SymRefs used in the current 'playing'
     */
    private Map map;

    /**
     * Create an iterator using the given Scenario as a model but with all
     * symbolic references re-written to use
     */
    public ScenarioIterator(Scenario scenario) {
	this.scenario = scenario ;
	this.index = 0;
	this.map = new HashMap();
    }

    /**
     * Returns the current EventCommandList record
     */
    protected Scenario.EventCommandList getEventCommandList() {
	return (Scenario.EventCommandList)scenario.getEventList().get(index);
    }

    /**
     * Returns the commands for the current EventCommandList record
     */
    protected Collection getCommandList(int state) {
	Scenario.EventCommandList eventCommandList = getEventCommandList();
	return eventCommandList.getCommandList(state);
    }

    /**
     * Does the given event match what happened next in the scenario
     *
     */
    public boolean matches(VisEvent event) {
	VisEvent peeked = peekEvent();
	if (peeked == null) {
	    return false;
	}
	if (event.unifies(peeked, map)) {
	    return true;
	}
	return false;
    }

    /**
     * Returns the next expected event.  This does NOT advance to the next one.
     *
     */
    public VisEvent peekEvent() {
	if (index >= scenario.getEventList().size()) {
	    return null;
	}
	Scenario.EventCommandList eventCommandList =(Scenario.EventCommandList)
	    scenario.getEventList().get(index);
	return eventCommandList.getVisEvent();
    }



    /**
     * Play the events associated with the current event, given the
     * current state.
     */
    public void play(int state) {
        if (DebuggingSupport.logMessages) {
            DebuggingSupport.logMessage(this,
                                        "\n\niterator state = " + state +
                                        "scenario =" + this);
        }
	for(Iterator it = getCommandList(state).iterator(); it.hasNext(); ) {
	    Command command = (Command)it.next();
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "iterator playing command="+
					    command);
	    }

            if (command instanceof ImmediateCommand) {
                // ImmediateCommands must not be exectuted on the
                // current thread, not the Swing Thread
                command.issue();
            } else {
                SwingUtilities.invokeLater(new IssueLater(command));
            }
	}
	
	if (DebuggingSupport.logMessages) {
	    DebuggingSupport.logMessage(this, "iterator play, state="+state);
	}

	if ( state == VisClientStateModel.NO_CURRENT_EVENT ) {
	
	    if (DebuggingSupport.logMessages) {
		DebuggingSupport.logMessage(this,
					    "iterator play, event_is_finished");
	    }

	    // We have issued all the commands for this event
	    index++;
	}
    }

    /**
     * Returns a string representing the contents of the scenario
     * iterator.
     */
    public String toString() {
        return
            "ScenarioIterator index=" + index +
            " scenario = " + scenario;
    }

    public static class IssueLater implements Runnable {
	private Command command ;
	public IssueLater(Command command) {
	    this.command = command ;
	}

	public void run() {
	    command.issue();
	}
    }

}

