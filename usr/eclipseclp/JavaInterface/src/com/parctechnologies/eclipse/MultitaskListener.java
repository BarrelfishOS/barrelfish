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
// Contributor(s): Andrew Sadler, IC-Parc
// 
// END LICENSE BLOCK

package com.parctechnologies.eclipse;
import java.io.*;
import java.util.*;

/**
 * Interface defining a listener for entering and leaving multitask
 * phases.
 *
 * @see EclipseMultitaskConnection
 */
public interface MultitaskListener {
  /**
   * Called when ECLiPSe enters a multitasking phase.
   *
   * <p>If a peer is interested in the type of multitasking phase, then
   * it should call multitaskConfirm() on the
   * EclipseMultitaskConnection object within this method.
   *
   * <p>This method is called before any pending multitasking RPCs */
  public void starting(EclipseMultitaskConnection eclipse, String type);

  /**
   * Called when ECLiPSe is leaving a multitasking phase.
   *
   * <p>The peer can still perform multitasking RPCs for the
   * duration of this method call.
   *
   * <p>This method is called before any pending RPCs */
  public void ending(EclipseMultitaskConnection eclipse, String type);
}
