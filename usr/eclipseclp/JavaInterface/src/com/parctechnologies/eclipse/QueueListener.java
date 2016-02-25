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
//Version:      $Id: QueueListener.java,v 1.1 2006/09/23 01:54:11 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Interface for queue handlers.
package com.parctechnologies.eclipse;

/**
 * Interface for handlers of incoming data or data requests on queues.
 * Any object which implements <i>QueueListener</i> can be
 * attached to a {@link FromEclipseQueue} or a {@link ToEclipseQueue} using the
 * <code>setListener()</code> methods of those classes.<p>
 * If attached to a <i>FromEclipseQueue</i>, when ECLiPSe
 * flushes the queue,
 * the <code>dataAvailable()</code> method is called with the flushed FromEclipseQueue as the
 * <code>source</code> parameter.<p>
 * If attached to a <i>ToEclipseQueue</i>, when ECLiPSe
 * tries to read data from that queue and it is empty,
 * the <code>dataRequest()</code> method is called with the ToEclipseQueue as the
 * <code>source</code> parameter.
 */
public interface QueueListener
{
    /**
     * Called with <i>FromEclipseQueue</i> as <code>source</code> when the
     * <i>QueueListener</i> is attached and ECLiPSe flushes the queue. If used
     * with an instance of <i>RemoteEclipse</i>, the <code>resume()</code>
     * method should not be invoked during <code>dataAvailable()</code>.
     */
    public void dataAvailable(Object source);

    /**
     * Called with <i>ToEclipseQueue</i> as <code>source</code> when the
     * <i>QueueListener</i> is attached and ECLiPSe tries to read data from
     * the queue when it is empty. If used
     * with an instance of <i>RemoteEclipse</i>, the <code>resume()</code>
     * method should not be invoked during <code>dataRequest()</code>.
     */
    public void dataRequest(Object source);
}
