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
/**
 * Provides a base class for commands which require access to a
 * viewletType, and a viewletRange (eg. those commands issued from
 * within ViewletType classes)
 **/
public abstract class ViewletTypeRangeCommand extends SymRefCommand {
    ViewletRange range;
    SymRef storeRef;

    public ViewletTypeRangeCommand(ViewletType type,
                                   ViewletDataStore store,
                                   ViewletRange range) {
	super(type);
        this.storeRef = store.getSymRef();
        this.range = store.createRange(range); // make a copy of the range
    }

    /**
     * Get the value of data store
     * @return value of data store
     */
    public ViewletDataStore getViewletDataStore() {
	return (ViewletDataStore)(SymRef.get(storeRef));
    }

    /**
     * Get the ViewletType for this command
     * @return value of ViewletType
     */
    public ViewletType getViewletType() {
	return (ViewletType)lookupSymRef();
    }

    /**
     * Get the value of the range
     * @return value of range
     */
    public ViewletRange getViewletRange() {
	return range;
    }

    public abstract void postRecordIssue() ;
}
