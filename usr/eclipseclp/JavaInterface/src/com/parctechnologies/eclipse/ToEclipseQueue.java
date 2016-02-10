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
//Version:      $Id: ToEclipseQueue.java,v 1.1 2006/09/23 01:54:12 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Queue to send Java data to Eclipse
package com.parctechnologies.eclipse;
import java.util.*;
import java.io.*;

/**
 * A queue for
 * sending data from Java to ECLiPSe. This class performs no processing of the
 * data and cannot for example, convert to EXDR format (see {@link EXDROutputStream}
 * for a class which can do this). <p>
 *
 * In addition to the standard methods inherited from its superclass, this class
 * also provides the ability to attach a {@link QueueListener} object to the
 * ToEclipseQueue, as a handler for data requests from ECLiPSe.
 * <p>
 * There is no public constructor; to access an <i>ToEclipseQueue</i> use the
 * <code>createToEclipseQueue()</code> of an object implementing the {@link
 * EclipseConnection} interface or the <code>getEclipseStdin()</code> method of
 * an object implementing the {@link EclipseEngine} interface.
 *
 */
public class ToEclipseQueue extends OutputStream {

    /* Name of queue which is to be used by the user on the Java side.
       The corresponding stream on the Eclipse
       side may be referenced by the same string, but may also have other names. */
    private String name;
    /* Stream number uniquely identifying the stream on the Eclipse side, and also
       used by classes inside this package on the Java side */
    private int id;
    /* The Eclipse which this queue is a queue to. Declared as an
       EclipseConnectionImpl because this is the abstract class containing the methods
       such as writeToStream which are called from this class */
    private EclipseConnectionImpl eclipse;

    /**
     * Listener for handling outgoing data, none by default
     */
    private QueueListener listener = null;

    /**
     * Flag to indicate whether close() method has been called.
     */
    private boolean isClosed = false;

    /**
     * Flag indicating whether this is a system or user queue. The eclipse side of
     * a System queue does not get closed when the EclipseConnectionImpl's
     * closeAllQueues method is called. Examples of system queues are ec_rpc_in
     * ec_rpc_out, and standard stream queues.
     */
    private boolean systemQueue = false;


    /**
     * construct a toEclipseQueue
     */
    ToEclipseQueue(int id,String name, EclipseConnectionImpl eclipse)
    {
        this.eclipse = eclipse;
        this.id = id;
        this.name = name;
    }

    int getID()
    {
      return(id);
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


    // write a single byte: calls writeByteToStream in the
    // EclipseConnectionImpl class.

    public void write(int b) throws IOException
    {
      testClosed();
      eclipse.writeByteToStream(id, (byte) b);
    }

    // invokes a method in the underlying eclipse which writes to a stream

    public void write(byte b[], int off, int len) throws IOException
    {
      testClosed();
      eclipse.writeToStream(id,b,off,len);
    }

  /**
     * Attach a <i>QueueListener</i> to this <i>ToEclipseQueue</i> for handling
     * requests for data. When ECLiPSe
     * tries to read from an empty queue with a <i>QueueListener</i>
     * attached,
     * the <code>dataAvailable()</code> method of the
     * <i>QueueListener</i> is invoked.
     */
    public void setListener(QueueListener l) throws IOException
    {
      testClosed();
      listener = l;
    }

    /**
     * Detach any <i>QueueListener</i> from this <i>ToEclipseQueue</i>.
     */
    public void removeListener() throws IOException
    {
      testClosed();
      listener = null;
    }

    /**
     * Called to notify that data has been requested, this forwards the request
     * to any listeners.
     */
    void notifyRequest()
    {
      // if there is a listener, invoke its dataRequest method
      if(listener != null)
      {
        listener.dataRequest(this);
      }
    }

    /**
     * Calls the superclass' <code>flush</code> method
     */
    public void flush() throws IOException
    {
      testClosed();
      // Get the underlying eclipse to flush the relevant stream
      eclipse.flushStream(this.id);

      // somehow here allow eclipse to handle events
    }

    /**
     * Closes the queue (both eclipse and Java sides), removing any listener.
     */
    public void close() throws IOException
    {
      testClosed();
      close_cleanup();
      eclipse.closeToEclipseStreamEclipseSide(id);
      eclipse.closeToEclipseStreamJavaSide(id);
    }

    // method to clean up locally
    void close_cleanup()
    {
      try
      {
        this.removeListener();
      }
      catch(IOException ioe)
      {
        // only thrown if the queue has already been closed, which it hasn't
        System.err.println("Error: removal of listener threw an IOException.");
        System.err.flush();
      }
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

}
