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
// Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Stefano Novello / Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: EclipseMultitaskConnection.java,v 1.1 2006/09/23 01:54:09 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Java interface representing a connection to an ECLiPSe engine.
package com.parctechnologies.eclipse;
import java.io.*;

/**
 * Interface of objects which provide a multitask aware connection to
 * an ECLiPSe connection.
 * 
 * <p>Classes implementing <i>EclipseMultitaskConnection</i> provide
 * the same functionality as <i>EclipseConnection</i> except that this
 * peer is registered as being interested in multitasking phases, and
 * conceptualy the peer maintains control throughout a multitasking
 * phase.
 *
 * <p> Objects implementing this interface can are returned from calls
 * to <i>registerMultitask</i> in the EclipseConnection interface.
 *
 * <p><b>NOTE:</b>The blocking semantics of RPC calls changes slightly
 * for this object as compared to the standard EclipseConnection.  Any
 * thread may execute RPCs on this object (as with normal
 * EclipseConnections) however if the thread does not "own" the
 * EclipseConnection the RPC call will block until the thread can gain
 * "ownership" (as with normal EclipseConnections) <i>UNLESS</i>
 * ECLiPSe is in a multitasking phase. In such a case, the RPC will be
 * serviced (during the multitask phase) and the thread un-blocked.
 *
 * <p><b>NOTE:</b>As currently implemented it is ONLY RPC semantics
 * that change.  Writing to Queues are still subject to the same
 * synchronization restrictions as with a normal EclipseConnection
 * ie. only the "owner" thread can perform queue operations.  This may
 * be changed in a future release.
 *
 * <p>The intention is that GUI tools can perform simple RPCs whilst
 * another peer has instigated a multitasking phase and/or GUI peers
 * can use the multitasking phases to pause ECLiPSe and perform work
 * whilst allowing other peers to do simple queries.
 *
 * <p>The registered MultitaskListener methods will be called when
 * gaining/losing control because of entering/leaving multitasking
 * phases.
 *
 * <p>calling regiterMultitask on an object of this class will return
 * the same object, but will add the given MultitaskListener.
 *
 * @see MultitaskListener
 * @see EclipseConnection
 * @see EclipseConnection#registerMultitask
 **/
public interface EclipseMultitaskConnection extends EclipseConnection
{
  /**
   * Indicates that this peer would like the multitask phase to begin.
   *
   * @throws java.lang.IllegalStateException If ECLiPSe is not in a
   * multitask phase.
   */
  public void multitaskConfirm() throws EclipseException, IOException;

  /**
   * Indicates that this peer would like the multitask phase to terminate.
   *
   * @throws java.lang.IllegalStateException If ECLiPSe is not in a
   * multitask phase.
   */
  public void multitaskTerminate() throws EclipseException, IOException;
}
