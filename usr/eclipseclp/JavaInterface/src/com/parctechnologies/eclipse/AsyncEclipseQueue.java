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
//                 Joachim Schimpf / Andrew Cheadle, IC-Parc
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: AsyncEclipseQueue.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Stefano Novello / Josh Singer /  J Schimpf / A Cheadle
//Company:      Parc Technologies / IC-Parc
//Description:  Asynchronous Queue for communication with ECLiPSe
package com.parctechnologies.eclipse;
import java.io.*;
import java.util.*;
import java.net.Socket;

/**
 * An asynchronous, bidirectional queue for communicating between ECLiPSe
 * and Java. The following differences exist between a synchronous
 * FromEclipseQueue/ToEclipseQueue and an asynchronous AsyncEclipseQueue:
 * <ul>
 * <li>an asynchronous queue can be read/written from the Java side even
 * while the ECLiPSe side has control, e.g. during an RPC. This obviously
 * has to happen from a different thread than the one that executes the RPC.</li>
 * <li>I/O operations on asynchronous queues can block, they should therefore
 * be done from a dedicated thread.</li>
 * <li>the AsyncEclipseQueue class does not extend InputStream or OutputStream
 * and can therefore not be used for I/O directly.  Instead, a standard Java
 * InputStream can be obtained from it via the getInputStream() method, and
 * an OutputStream via the getOutputStream() method.</li>
 * <li>on the ECLiPSe side, an event can (and should) be raised when data
 * arrives from the Java side. If the ECLiPSe side has control at that time,
 * it will handle the event. Otherwise, the event handling may be deferred
 * until ECLiPSe has control back.</li>
 * </ul>
 *
 * There is no public constructor; to access an <i>AsyncEclipseQueue</i>,
 * use the <code>createAsyncEclipseQueue()</code> of an object implementing
 * the {@link EclipseConnection} interface.  But note that asynchronous
 * queues are only implemented by the {@link RemoteEclipse} and {@link OutOfProcessEclipse}
 * implementations, not the {@link EmbeddedEclipse} variant.
 *
 */
public class AsyncEclipseQueue {
    /**
     * This is the eclipse numeric id for the queue it is used to
     * uniquely identify the stream
     */
    private int id;
    /**
     * Name of the queue
     */
    private String name;

    /**
     * The eclipse which this is a queue from
     */
    private EclipseConnectionImpl eclipse;

    /**
     * Flag to indicate whether or not the queue has been closed.
     */
    private boolean isClosed = false;

    /**
     * Flag indicating whether this is a system or user queue. The eclipse side of
     * a System queue does not get closed when the EclipseConnectionImpl's
     * closeAllQueues method is called. Examples of system queues are ec_rpc_in
     * ec_rpc_out, and standard stream queues.
     */
    private boolean systemQueue = false;

    private InputStream cachedInputStream = null;

    private OutputStream cachedOutputStream = null;


    /**
     * make new queue
     */
    AsyncEclipseQueue(int id,String name, EclipseConnectionImpl eclipse)
    {
        this.eclipse = eclipse;
        this.id = id;
        this.name = name;
    }

    int getID()
    {
      return id;
    }

    /**
     * Returns true if this queue is a system queue, such as ec_rpc_in.
     */
    boolean isSystemQueue()
    {
      return(systemQueue);
    }

    /**
     * sets flag indicating wether this is a system queue
     */
    void setSystemQueue(boolean newValue)
    {
      systemQueue = newValue;
    }

    /**
     * Closes the queue (both eclipse and Java sides)
     */
    public void close() throws IOException
    {
      testClosed();
      close_cleanup();
      eclipse.closeAsyncEclipseStreamEclipseSide(id);
      eclipse.closeAsyncEclipseStreamJavaSide(id);
    }

    // method to clean up locally
    void close_cleanup()
    {
      isClosed = true;
    }

    /**
     * Test whether the queue has been closed; if so, throw an IOException
     * "Operation not possible: stream closed."
     */
    private void testClosed() throws IOException
    {
      if(isClosed)
      {
        throw new IOException("Operation not possible: stream closed.");
      }
    }

    /**
     * Gets the InputStream associated with the (bidirectional)
     * AsyncEclipseQueue. Throws an exception if the Queue is closed.
     */
    public InputStream getInputStream() throws IOException
    {
      testClosed();
      return cachedInputStream != null ? cachedInputStream
	  : eclipse.getAsyncInputStream(id);
    }

    /**
     * Gets the OutputStream associated with the (bidirectional)
     * AsyncEclipseQueue. Throws an exception if the Queue is closed.
     */
    public OutputStream getOutputStream() throws IOException
    {
      testClosed();
      return cachedOutputStream != null ? cachedOutputStream
	  : eclipse.getAsyncOutputStream(id);
    }
}
