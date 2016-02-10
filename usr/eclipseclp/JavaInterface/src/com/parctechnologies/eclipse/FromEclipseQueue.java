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
//Version:      $Id: FromEclipseQueue.java,v 1.1 2006/09/23 01:54:10 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Queue for sending ECLiPSe data to Java.
package com.parctechnologies.eclipse;
import java.io.*;
import java.util.*;

/**
 * A queue for
 * sending data from ECLiPSe to Java. This class performs no processing of the
 * data and cannot for example, deal with EXDR format (see {@link EXDRInputStream}
 * for a class which can handle this). <p>
 *
 * In addition to the standard methods inherited from its superclass, this class
 * also provides the ability to attach a {@link QueueListener} object to the
 * FromEclipseQueue, as a handler for incoming data.
 * <p>
 * There is no public constructor; to access an <i>FromEclipseQueue</i> use the
 * <code>createFromEclipseQueue()</code> of an object implementing the {@link
 * EclipseConnection} interface or the <code>getEclipseStderr()</code> or
 * <code>getEclipseStdout()</code> method of an object implementing the
 * {@link EclipseEngine} interface.
 *
 */
public class FromEclipseQueue extends InputStream {
    /**
     * This is the eclipse numeric id for the queue
     * it is used to uniquely identify the stream
     */
    private int id;
    /**
     * Handler for new data on queue
     */
    private QueueListener listener = null;
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


    /**
     * make new queue
     */
    FromEclipseQueue(int id,String name, EclipseConnectionImpl eclipse)
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



    // overrides superclass method. Return the number of bytes which
    // can be read from the FromEclipseQueue stream without blocking.
    public int available() throws IOException
    {
      testClosed();
      return(eclipse.availableOnStream(id));
    }

    /**
     * Read bytes from the FromEclipseQueue into a byte array.
     * <code>len</code> bytes are read from the queue and stored in byte
     * array <code>b</code> at offset <code>off</code>.
     *
     * @return the number of bytes read.
     *
     */
    public int read(byte[] b, int off, int len) throws IOException
    {
      testClosed();
      // read len bytes from the Eclipse stream and copy them into b at
      // offset off. Set read to the number of bytes read.
      int read = eclipse.readFromStream(id,off,len,b);
      // If nothing was read
/*      if(read == 0)
      {
        try
        {
          // pass control to eclipse to allow events to be
          // handled somehow here.

          // try the read operation again
          read = eclipse.readFromStream(id,off,len,b);
        }
        catch(EclipseException e)
        {
          throw(new IOException("Problem handling events on ECLiPSe side."));
        }
      }
*/
      // Return the number of bytes read
      return read;

    }

    public int read() throws IOException
    {
      testClosed();
      return(eclipse.readByteFromStream(id));
    }

    /**
     * Called to notify a listener, if there is one, that data is available
     */
    void notifyAvailable()
    {
        if (listener != null)
        {
            listener.dataAvailable(this);
        }
    }

    /**
     * Attach a <i>QueueListener</i> to this <i>FromEclipseQueue</i> for handling
     * incoming data. When ECLiPSe
     * flushes the
     * queue, the <code>dataAvailable()</code> method of the
     * <i>QueueListener</i> is invoked.
     */
    public void setListener(QueueListener l) throws IOException
    {
      testClosed();
      listener = l;
    }

    /**
     * Detach any <i>QueueListener</i> from this <i>FromEclipseQueue</i>.
     */
    public void removeListener() throws IOException
    {
      testClosed();
      listener = null;
    }

    /**
     * Closes the queue (both eclipse and Java sides), removing any listener.
     */
    public void close() throws IOException
    {
      testClosed();
      close_cleanup();
      eclipse.closeFromEclipseStreamEclipseSide(id);
      eclipse.closeFromEclipseStreamJavaSide(id);
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
