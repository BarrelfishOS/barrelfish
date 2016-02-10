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
 * Interface for objects which can produce new viewers when viewables are
 * created.
 */
public interface ViewerFactory
{
    /**
     * The ViewerFactory must be able to say, given a ViewableType, whether or
     * not it can build a viewer from it. The ability to build the viewer can
     * therefore depend only on the number and fixity of the dimensions and the
     * element type.
     */
    boolean canBuildFrom(ViewableType viewableType);

    /**
     * The contract for this method is to instantiate Viewer and initialise it
     * as much as is possible given the basic information in the Viewable.
     * Further initialisation of viewers comes later when the viewer has had a
     * chance to execute some preEventGoals.
     */
    Viewer build(Viewable viewable);
}

