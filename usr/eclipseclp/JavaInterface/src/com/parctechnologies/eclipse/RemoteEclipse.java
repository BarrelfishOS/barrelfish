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
// Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
// 
// Contributor(s): Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK
//Title:        Java/ECLiPSe interface
//Version:      $Id: RemoteEclipse.java,v 1.1 2006/09/23 01:54:12 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Connection to a remote ECLiPSe.
package com.parctechnologies.eclipse;
import java.io.*;
import java.net.*;
import java.util.*;



/**
 * Remote connection to an existing ECLiPSe process. The connection may be made
 * over a TCP/IP network to an ECLiPSe which has been primed using the
 * <code>remote_connect/3</code> or the <code>remote_connect_setup/3</code>
 * builtin predicates. As well as the functionality provided by the
 * <i>EclipseConnection</i> interface, <i>RemoteEclipse</i> also allows for
 * execution control to be transferred explicity over to ECLiPSe with the
 * <code>resume()</code> method.
 *
 * <p>The connection is terminated from the Java side using either the
 * <code>disconnect()</code> method (when Java has execution control) or
 * <code>unilateralDisconnect()</code> (when ECLiPSe has execution control).
 * If the builtin predicate <code>remote_disconnect/1</code> is executed on
 * the ECLiPSe side, the effect on the Java side are similar to the
 * effects of <code>disconnect()</code>.
 *
 */
public class RemoteEclipse extends EclipseConnectionImpl implements EclipseConnection
{
  /**
   * Implementation overview
   *
   * I assume familiarity with the Eclips remote interface protocol.
   *
   * The main complication to the implementation of this class is the fact that
   * ECLiPSe sockets have a very small fixed buffer size. Consequences:
   * (1) When a client writes to an Eclipse socket and the socket's buffer
   * reaches capacity, the client's write/flush command blocks indefinitely
   * unless Eclipse is reading from the socket's stream. Because of this block,
   * any read call which the user may set up using rpc is never reached.
   *
   * (2) Conversely, when Eclipse flushes an amount of data through the socket
   * stream which is larger than capacity, its own write command blocks
   * indefinitely unless there is a corresponding read command waiting on the
   * Java side. However, the user cannot set up this read after Eclipse flushes,
   * because control does not return to Java until after the write command
   * completes.
   *
   * The way we handle (1) is to enclose the socket OutputStream in a special
   * NonBlockingOutputStream, whose write and flush methods are non-blocking
   * A thread is hidden within each NonBlockingOutputStream object. The thread
   * is activated whenever the NonBlockingOutputStream is flushed and handles
   * the write and flush calls to the underlying stream (the socket in this
   * case) concurrently. The hidden thread may block, but the original thread
   * is unblocked. It can therefore go ahead and set up a read call on the
   * Eclipse side, which will remove the blockage, and read the data written by
   * the hidden stream.
   *
   * (2) is handled using a buffer on the Java side. Before attempting to write
   * the data, Eclipse sends an ec_flushio signal, indicating the amount about
   * to be written. This amount of data is immediately read into a buffer.
   * From there it can be read by the user later. See the
   * FromEclipseQueueBuffer class for more details.
   *
   */



  // The socket for sending/receiving control signals
  private Socket control;
  // The socket for sending/receiving rpc goals
  private Socket rpc;
  // The name of the Eclipse remote connection which this object is connected
  // to.
  private String connectionName;
  // Stream for receiving control signals in EXDR format
  private EXDRInputStream controlEXDRInput;
  // Stream for receiving rpc goals in EXDR format
  private EXDRInputStream rpcEXDRInput;
  // Stream for sending control signals in EXDR format
  private EXDROutputStream controlEXDROutput;
  // Stream for sending rpc goals in EXDR format
  private EXDROutputStream rpcEXDROutput;
  // common atoms
  private static final Atom resumeAtom = new Atom("resume");
  private static final Atom rpcAtom = new Atom("rpc");
  private static final Atom yieldAtom = new Atom("yield");
  private static final Atom disconnectAtom = new Atom("disconnect");
  private static final Atom disconnectYieldAtom = new Atom("disconnect_yield");
  private static final Atom disconnectResumeAtom = new Atom("disconnect_resume");
  private static final Atom syncAtom = new Atom("sync");
  private static final Atom asyncAtom = new Atom("async");
  private static final Atom fromecAtom = new Atom("fromec");
  private static final Atom toecAtom = new Atom("toec");
  private static final Atom bidirectAtom = new Atom("bidirect");
  private static final Atom emptyAtom = new Atom("");
  private static final Atom failAtom = new Atom("fail");
  private static final Atom successAtom = new Atom("success");

  private Map queueInfo = new HashMap();

  // The address of the machine which Eclipse is running on, as read during the
  // protocol. This should be used in subsequent client socket connections.
  private InetAddress hostAddress;

  private static final int PROTOCOL_VERSION_NUMBER = 1;

  public static final int DEFAULT_TIMEOUT_MILLIS = 30000;

  /**
   * Make a connection to an existing ECLiPSe process. The ECLiPSe process must
   * be on a server machine which is reachable on a TCP/IP network. ECLiPSe must be
   * primed using <code>remote_connect_setup/3</code>. The builtin predicate
   * <code>remote_connect_accept/6</code> should then be used to complete the
   * connection. The connection details
   * (IP address, port number, password) are specified as parameters and must match those
   * specified/returned as arguments in the execution of
   * <code>remote_connect_setup/3</code> and <code>remote_connect_accept/6</code>.
   *
   * @throws IOException if the connection could not be made, or times out
   * within <code>DEFAULT_TIMEOUT_MILLIS</code> milliseconds.
   */
  public RemoteEclipse(InetAddress remoteEclipseHost, int remoteEclipsePort,
                       String passwd)
    throws IOException
  {
    this(remoteEclipseHost, remoteEclipsePort, passwd, DEFAULT_TIMEOUT_MILLIS);
  }

  /**
   * Make a connection to an existing ECLiPSe process. The ECLiPSe process must be
   *
   * on a server machine which is reachable on a TCP/IP network. ECLiPSe must be
   * primed using <code>remote_connect/3</code>. The connection details
   * (IP address, port number) are specified as parameters and must match those
   * specified/returned as arguments in the execution of
   * <code>remote_connect/3</code>. If <code>remote_connect_setup/3</code>. was
   * used to prime ECLiPSe for the remote connection, this constructor will fail
   * as it does not use a password.
   *
   * @throws IOException if the connection could not be made, or times out
   * within <code>DEFAULT_TIMEOUT_MILLIS</code> milliseconds.
   */
  public RemoteEclipse(InetAddress remoteEclipseHost, int remoteEclipsePort)
    throws IOException
  {
    this(remoteEclipseHost, remoteEclipsePort, "", DEFAULT_TIMEOUT_MILLIS);
  }

  /**
   * Make a connection to an existing ECLiPSe process. The ECLiPSe process must
   * be on a server machine which is reachable on a TCP/IP network. ECLiPSe must be
   * primed using <code>remote_connect/3</code>. The connection details
   * (IP address, port number) are specified as parameters and must match those
   * specified/returned as arguments in the execution of
   * <code>remote_connect/3</code>. If <code>remote_connect_setup/3</code>. was
   * used to prime ECLiPSe for the remote connection, this constructor will fail
   * as it does not use a password.
   *
   * @param timeoutMillis number of milliseconds to wait for the initial
   * connection to be established before throwing an exception. Set
   * <code>timeoutMillis</code> to 0 to wait indefinitely for the connection.
   *
   * @throws IOException if the connection could not be made, or times out
   * within <code>timeoutMillis</code> milliseconds.
   */
  public RemoteEclipse(InetAddress remoteEclipseHost, int remoteEclipsePort,
                       int timeoutMillis)
    throws IOException
  {
    this(remoteEclipseHost, remoteEclipsePort, "", timeoutMillis);
  }

  private void setUpControl(InetAddress remoteEclipseHost,
                            int remoteEclipsePort)
                            throws IOException
  {
    control = new Socket(remoteEclipseHost, remoteEclipsePort);
    // initialise the EXDR readers and writers on the control socket
    controlEXDRInput = new EXDRInputStream(control.getInputStream());
    controlEXDROutput = new EXDROutputStream(control.getOutputStream());
  }

  private void setUpRPC(InetAddress remoteEclipseHost, int remoteEclipsePort)
                        throws IOException
  {
    rpc = new Socket(remoteEclipseHost, remoteEclipsePort);
    // Initialise readers and writers on these sockets
    rpcEXDRInput = new EXDRInputStream(rpc.getInputStream());
    rpcEXDROutput = new EXDROutputStream(rpc.getOutputStream());
  }



  /**
   * Make a connection to an existing ECLiPSe process. The ECLiPSe process must
   * be on a server machine which is reachable on a TCP/IP network. ECLiPSe must be
   * primed using <code>remote_connect_setup/3</code>. The builtin predicate
   * <code>remote_connect_accept/6</code> should then be used to complete the
   * connection. The connection details
   * (IP address, port number, password) are specified as parameters and must match those
   * specified/returned as arguments in the execution of
   * <code>remote_connect_setup/3</code> and <code>remote_connect_accept/6</code>.
   *
   * @param timeoutMillis number of milliseconds to wait for the initial
   * connection to be established before throwing an exception. Set
   * <code>timeoutMillis</code> to 0 to wait indefinitely for the connection.
   *
   * @throws IOException if the connection could not be made, or times out
   * within <code>timeoutMillis</code> milliseconds.
   */
  public RemoteEclipse(InetAddress remoteEclipseHost, int remoteEclipsePort,
                       String passwd, int timeoutMillis)
    throws IOException
  {
    hostAddress = remoteEclipseHost;

    // System.out.println("starting remote protocol");

    setUpControl(remoteEclipseHost, remoteEclipsePort);

    CompoundTerm protocolTerm =
      new CompoundTermImpl("remote_protocol",
                           new Integer(PROTOCOL_VERSION_NUMBER));

    //System.out.println("protocol term is "+protocolTerm);

    writeControl(protocolTerm);

    Object presponse = readControl();
    //System.out.println("presponse is "+presponse);

    if(!presponse.equals("yes"))
    {
      //System.out.println("presponse not equal to \"yes\", closing control");
      control.close();
      throw(new IOException("Remote protocol error: protocol version unsupported."));
    }

    //System.out.println("presponse equal to \"yes\", continuing protocol");

    writeControl(passwd);

    // read the connectionName on the control socket
    connectionName = ((Atom) readControlTimeout(timeoutMillis)).functor();

    // write the language name on the control socket
    writeControl("java");
    // System.out.println("Read connection name: "+connectionName);
    setUpRPC(remoteEclipseHost, remoteEclipsePort);

    if(!connectionName.equals(((Atom) readRPCTimeout(timeoutMillis)).functor()))
    {
      throw(new IOException("Remote protocol error."));
    }
    // set up the peer name locally
    setPeerName(new Atom(connectionName));

    // System.out.println("remote protocol complete");
  }


  /**
   * Terminate the connection with ECLiPSe unilaterally. This method should be
   * invoked in unforseen circumstances when the connection should be terminated
   * while ECLiPSe has execution control. After
   * <code>unilateralDisconnect</code> has
   * been invoked, public methods invoked on this <i>RemoteEclipse</i> will
   * throw <i>EclipseTerminatedException</i>s.
   *
   * @throws EclipseTerminatedException if the connection has already been
   * terminated.
   */
  public void unilateralDisconnect() throws EclipseTerminatedException
  {
    shouldDisconnect = false; // Avoid recursive calls to unilateralDisconnect
    testTerminated();
    try
    {
      // don't use writeControl here as that might cause a recursive invocation
      // of unilateralDisconnect.
      controlEXDROutput.write(disconnectResumeAtom);
      controlEXDROutput.flush();
    }
    catch(Exception e) {}

    try
    {
      terminateJavaSide();
    }
    catch(IOException ioe) {}

  }

  /**
   * Terminate the remote connection to ECLiPSe. This should be invoked while
   * Java has control. If ECLiPSe has control then use
   * <code>unlateralDisconnect</code> instead. After <code>disconnect</code> has
   * been invoked, public methods invoked on this <i>RemoteEclipse</i> will
   * throw <i>EclipseTerminatedException</i>s.
   *
   * @throws EclipseTerminatedException if the connection has already been
   * terminated.
   * @throws IOException if there was a problem communicating with ECLiPSe
   * during disconnection.
   */
  public synchronized void disconnect() throws IOException
  {
    testTerminated();
    // write the disconnect atom on the control connection
    writeControl(disconnectAtom);

    // read "disconnect_yield" on the control connection
    Object result = null;
    try
    {
      result = readControl();
    }
    catch(IOException ioe)
    {
      // don't mind if the connection has already been lost
    }

    if(result != null && !result.equals(disconnectYieldAtom))
    {
      throw new IOException("Remote protocol error.");
    }
    terminateJavaSide();
  }


  private void terminateJavaSide() throws IOException
  {
    // set this object to terminated
    terminated = true;

    // close all user queues, but not the eclipse sides
    closeAllQueues(false);

    // try to close control and rpc sockets, but if this fails, don't worry.
    try
    {
      control.close();
    }
    catch(Exception e){}
    try
    {
      rpc.close();
    }
    catch(Exception e){}

  }

  /**
   * Explicitly transfer execution control to ECLiPSe. ECLiPSe will resume
   * execution immediately after the last goal which transferred control to
   * Java (normally <code>remote_connect/3</code> or
   * <code>remote_connect_setup/3</code>). This method should not be
   * invoked while control has been transferred to Java using a
   * <i>QueueListener</i>. An invocation of <code>resume()</code> should
   * normally be paired with an execution of the builtin predicate
   * <code>remote_yield/1</code>, which can return execution control to Java.
   *
   * @throws EclipseTerminatedException if the connection to ECLiPSe has been
   * terminated.
   * @throws IOException if there was a problem communicating with ECLiPSe.
   */
  public synchronized void resume() throws IOException
  {
    testTerminated();
    waitForEclipse(true);
  }

  // called to perform setup code additional to what is done in
  // EclipseConnectionImpl
  void setupFromEclipseQueue(String name) throws EclipseException, IOException
  {
    // port to be used on eclipse side for socket
    int port;
    // Atoms for name and connectionName (used in rpc calls)
    Atom nameAtom = new Atom(name);
    // compound term returned by ECLiPSe during interaction
    CompoundTerm result1 = null;
    // write queue_create(QueueName, sync, fromec, '') on the control connection
    writeControl(new CompoundTermImpl("queue_create", nameAtom, syncAtom,
                                      fromecAtom, emptyAtom));

    // read the next term from the control connection
    try
    {
      result1 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }


    if(result1.equals(yieldAtom))
    {
      throw new EclipseException("Could not create ECLiPSe side of queue.");
    }

    // System.out.println("result1 = "+result1);

    // check that the response obeys the protocol
    if(!result1.functor().equals("socket_client") ||
       result1.arity() != 4 ||
       !(result1.arg(1) instanceof Integer) ||
       !(result1.arg(2) instanceof Atom) ||
       !(result1.arg(3) instanceof Atom) ||
       !(result1.arg(4) instanceof Atom) ||
       !((Atom) result1.arg(2)).equals(nameAtom) ||
       !((Atom) result1.arg(3)).functor().equals("sync") ||
       !((Atom) result1.arg(4)).functor().equals("fromec"))
    {
      throw new IOException("Remote interface protocol error.");
    }

    // extract port number from the response
    port = ((Integer) result1.arg(1)).intValue();

    setupRemoteFromecQueue(nameAtom, port);

    // send the resume message + wait for yield.
    resume();
  }

  private void setupRemoteFromecQueue(Atom nameAtom, int port)
    throws IOException
  {
    // result term received during interaction
    CompoundTerm result2 = null;
    // Socket to be used for the new queue
    Socket newSocket;

    // try to connect the new socket to the address specified during the remote
    // protocol initialisation and the port specified above.
    try
    {
      newSocket = new Socket(hostAddress, port);
    }
    catch(IOException e) // thrown if (for example), something else
                               // has stolen the connection.
    {
      // inform ECLiPSe that socket creation failed
      writeControl(new CompoundTermImpl("socket_connect", nameAtom, failAtom));
      // throw the exception
      throw e;
    }
    // otherwise inform ECLiPSe that socket creation succeeded
    writeControl(new CompoundTermImpl("socket_connect", nameAtom, successAtom));

    // read the next term from the control connection
    try
    {
      result2 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }

    // check that this obeys the protocol
    if(!result2.functor().equals("socket_accept") ||
       result2.arity() != 2 ||
       !(result2.arg(1) instanceof Atom) ||
       !(result2.arg(2) instanceof Integer) ||
       !((Atom) result2.arg(1)).equals(nameAtom))
    {
      throw new IOException("Remote interface protocol error.");
    }
    // extract the stream number id of the named queue
    Integer Id = (Integer) (result2.arg(2));
    int id = Id.intValue();

    setupFromecInfo(id, newSocket);
  }



  // called to perform setup code additional to what is done in
  // EclipseConnectionImpl
  void setupToEclipseQueue(String name) throws EclipseException, IOException
  {
    // port to be used on eclipse side for socket
    int port;
    // Atoms for name and connectionName (used in rpc calls)
    Atom nameAtom = new Atom(name);
    // compound term returned by ECLiPSe during interaction
    CompoundTerm result1 = null;
    // write queue_create(QueueName, sync, fromec, '') on the control connection
    writeControl(new CompoundTermImpl("queue_create", nameAtom, syncAtom,
                                      toecAtom, emptyAtom));

    // read the next term from the control connection
    try
    {
      result1 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }


    if(result1.equals(yieldAtom))
    {
      throw new EclipseException("Could not create ECLiPSe side of queue.");
    }

    // System.out.println("result1 = "+result1);

    // check that the response obeys the protocol
    if(!result1.functor().equals("socket_client") ||
       result1.arity() != 4 ||
       !(result1.arg(1) instanceof Integer) ||
       !(result1.arg(2) instanceof Atom) ||
       !(result1.arg(3) instanceof Atom) ||
       !(result1.arg(4) instanceof Atom) ||
       !((Atom) result1.arg(2)).equals(nameAtom) ||
       !((Atom) result1.arg(3)).functor().equals("sync") ||
       !((Atom) result1.arg(4)).functor().equals("toec"))
    {
      throw new IOException("Remote interface protocol error.");
    }

    // extract port number from the response
    port = ((Integer) result1.arg(1)).intValue();

    setupRemoteToecQueue(nameAtom, port);

    // send the resume message + wait for yield
    resume();
  }


  private void setupRemoteToecQueue(Atom nameAtom, int port)
      throws IOException
  {
    // Socket to be used for the new queue
    Socket newSocket;
    // compound term returned by ECLiPSe during interaction
    CompoundTerm result2 = null;
    // try to connect the new socket to the address specified during the remote
    // protocol initialisation and the port specified above.
    try
    {
      newSocket = new Socket(hostAddress, port);
    }
    catch(IOException e) // thrown if (for example), something else
                               // has stolen the connection.
    {
      // inform ECLiPSe that socket creation failed
      writeControl(new CompoundTermImpl("socket_connect", nameAtom, failAtom));
      // throw the exception
      throw e;
    }
    // otherwise inform ECLiPSe that socket creation succeeded
    writeControl(new CompoundTermImpl("socket_connect", nameAtom, successAtom));

    // read the next term from the control connection
    try
    {
      result2 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }

    // check that this obeys the protocol
    if(!result2.functor().equals("socket_accept") ||
       result2.arity() != 2 ||
       !(result2.arg(1) instanceof Atom) ||
       !(result2.arg(2) instanceof Integer) ||
       !((Atom) result2.arg(1)).equals(nameAtom))
    {
      throw new IOException("Remote interface protocol error.");
    }
    // extract the stream number id of the named queue
    Integer Id = (Integer) (result2.arg(2));
    int id = Id.intValue();

    this.setupToecInfo(id, newSocket);
  }

//-------------------------------------------------------------------

  // called to perform setup code additional to what is done in
  // EclipseConnectionImpl
  void setupAsyncEclipseQueue(String name) throws EclipseException, IOException
  {
    // port to be used on eclipse side for socket
    int port;
    // Atoms for name and connectionName (used in rpc calls)
    Atom nameAtom = new Atom(name);
    // compound term returned by ECLiPSe during interaction
    CompoundTerm result1 = null;
    // write queue_create(QueueName, sync, fromec, '') on the control connection
    writeControl(new CompoundTermImpl("queue_create", nameAtom, asyncAtom,
                                      bidirectAtom, emptyAtom));

    // read the next term from the control connection
    try
    {
      result1 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }


    if(result1.equals(yieldAtom))
    {
      throw new EclipseException("Could not create ECLiPSe side of queue.");
    }

    // System.out.println("result1 = "+result1);

    // check that the response obeys the protocol
    if(!result1.functor().equals("socket_client") ||
       result1.arity() != 4 ||
       !(result1.arg(1) instanceof Integer) ||
       !(result1.arg(2) instanceof Atom) ||
       !(result1.arg(3) instanceof Atom) ||
       !(result1.arg(4) instanceof Atom) ||
       !((Atom) result1.arg(2)).equals(nameAtom) ||
       !((Atom) result1.arg(3)).functor().equals("async") ||
       !((Atom) result1.arg(4)).functor().equals("bidirect"))
    {
      throw new IOException("Remote interface protocol error.");
    }

    // extract port number from the response
    port = ((Integer) result1.arg(1)).intValue();

    setupRemoteAsyncecQueue(nameAtom, port);

    // send the resume message + wait for yield
    resume();
  }


  private void setupRemoteAsyncecQueue(Atom nameAtom, int port)
      throws IOException
  {
    // Socket to be used for the new queue
    Socket newSocket;
    // compound term returned by ECLiPSe during interaction
    CompoundTerm result2 = null;
    // try to connect the new socket to the address specified during the remote
    // protocol initialisation and the port specified above.
    try
    {
      newSocket = new Socket(hostAddress, port);
    }
    catch(IOException e) // thrown if (for example), something else
                               // has stolen the connection.
    {
      // inform ECLiPSe that socket creation failed
      writeControl(new CompoundTermImpl("socket_connect", nameAtom, failAtom));
      // throw the exception
      throw e;
    }
    // otherwise inform ECLiPSe that socket creation succeeded
    writeControl(new CompoundTermImpl("socket_connect", nameAtom, successAtom));

    // read the next term from the control connection
    try
    {
      result2 = (CompoundTerm) readControl();
    }
    catch(ClassCastException cce)
    {
      throw new IOException("Remote interface protocol error.");
    }

    // check that this obeys the protocol
    if(!result2.functor().equals("socket_accept") ||
       result2.arity() != 2 ||
       !(result2.arg(1) instanceof Atom) ||
       !(result2.arg(2) instanceof Integer) ||
       !((Atom) result2.arg(1)).equals(nameAtom))
    {
      throw new IOException("Remote interface protocol error.");
    }
    // extract the stream number id of the named queue
    Integer Id = (Integer) (result2.arg(2));
    int id = Id.intValue();

    setupAsyncecInfo(id, newSocket);
  }
//-------------------------------------------------------------------

  boolean shouldDisconnect = false;

  void testTerminated() throws EclipseTerminatedException
  {
    if(shouldDisconnect)
    {
      unilateralDisconnect();
    }
    super.testTerminated();
  }

  // Called if the flush() method of a ToEclipseQueue is called.
  synchronized void flushStream(int streamID) throws IOException
  {
    // write rem_flushio(Queuenumber, Bytes) on control connection & flush
    writeControl(new CompoundTermImpl("rem_flushio",
                                      new Integer(streamID),
                                      new Integer(getBytesBuffered(streamID))));
    // get the buffer to flush (in the background) to its destination
    getOutputStream(streamID).flush();
    // Now these bytes have been flushed, reset the byte counter
    setBytesBuffered(streamID, 0);
    // hand control to Eclipse and handle any resulting events until yield
    waitForEclipse(false);
  }


  synchronized void writeByteToStream(int streamid, byte b) throws IOException
  {
    getOutputStream(streamid).write(0xff & b);
    setBytesBuffered(streamid, getBytesBuffered(streamid)+1);
  }


  synchronized int writeToStream(int streamid, byte[] b, int off, int len) throws IOException
  {
    getOutputStream(streamid).write(b, off, len);

    // since the above statement cannot have written less than len bytes unless it
    // threw an exception, if execution reaches the current point, we know len
    // bytes have been written.

    setBytesBuffered(streamid, getBytesBuffered(streamid)+len);
    return(len);
  }

  // Called if the available() method of a FromEclipseQueue is called.
  synchronized int availableOnStream(int streamID)
  {
    return(this.availableInBuffer(streamID));
  }

  // called by the read method in FromEclipseQueue. Returns -1 if there are no
  // bytes in the buffer.
  synchronized int readByteFromStream(int streamID) throws IOException
  {
    return(readByteFromBuffer(streamID));
  }

  // called by the read methods of FromEclipseQueue. Returns the number of
  // bytes read (may be less than len).
  synchronized int readFromStream(int streamid, int off, int len, byte[] b) throws IOException
  {
    return(readBytesFromBuffer(streamid, b, off, len));
  }


  ControlSignal getNextControlSignal(boolean isFirstIteration,
                                     boolean transferControlWithResume)
    throws IOException
  {
    if(transferControlWithResume || !isFirstIteration)
    {
      writeControl(resumeAtom);
    }

    CompoundTerm nextControlTerm = getNextControlTerm();

    if(signalsYield(nextControlTerm))
    {
      return(new YieldSignal());
    }

    if(signalsMultilateralDisconnect(nextControlTerm))
    {
      return(new MultilateralDisconnectSignal());
    }

    if(signalsUnilateralDisconnect(nextControlTerm))
    {
      return(new UnilateralDisconnectSignal());
    }

    if(signalsFlushIO(nextControlTerm))
    {
      return(new FlushIOSignal((Integer) nextControlTerm.arg(1),
                               (Integer) nextControlTerm.arg(2)));
    }

    if(signalsWaitIO(nextControlTerm))
    {
      return(new WaitIOSignal((Integer) nextControlTerm.arg(1)));
    }

    if(signalsCloseQueue(nextControlTerm))
    {
      return(new CloseQueueSignal((Integer) nextControlTerm.arg(1)));
    }

    if(signalsOpenQueue(nextControlTerm))
    {
      return(new RemoteOpenQueueSignal((Integer) nextControlTerm.arg(1),
                                       (Atom) nextControlTerm.arg(2),
                                       (Atom) nextControlTerm.arg(3),
                                       (Atom) nextControlTerm.arg(4)));
    }

    // default, signifies unrecognised signal
    return(null);
  }


  /**
   * Send an RPC goal to ECLiPSe.
   */
  void sendGoal(Object goal) throws IOException
  {
    writeControl(rpcAtom);
    writeRPC(goal);
    // System.out.println("Sent goal on rpc connection: "+goal);
  }

  /**
   * Receive an RPC goal from ECLiPSe.
   */
  Object receiveGoal() throws IOException
  {
    return(rpcEXDRInput.readTerm());
  }

  void closeFromEclipseStreamJavaSide(int streamid) throws IOException
  {
    super.closeFromEclipseStreamJavaSide(streamid);
    closeFromecSocket(streamid);
    removeInfo(streamid);
  }

  void closeToEclipseStreamJavaSide(int streamid) throws IOException
  {
    super.closeToEclipseStreamJavaSide(streamid);
    getOutputStream(streamid).close();
    closeToecSocket(streamid);
    setBytesBuffered(streamid, 0);
  }

  void closeFromEclipseStreamEclipseSide(int streamid) throws IOException
  {
    super.closeFromEclipseStreamEclipseSide(streamid);
    // write queue_close(streamid) on the control connection
    writeControl(new CompoundTermImpl("queue_close",
                 new Integer(streamid)));
    // wait for a yield signal on the control connection
    Object result = readControl();
    if(!result.equals(yieldAtom))
    {
      throw new IOException("Remote protocol error.");
    }
  }

  void closeToEclipseStreamEclipseSide(int streamid) throws IOException
  {
    super.closeToEclipseStreamEclipseSide(streamid);
    // write queue_close(streamid) on the control connection
    writeControl(new CompoundTermImpl("queue_close",
                 new Integer(streamid)));
    // wait for a yield signal on the control connection
    Object result = readControl();
    if(!result.equals(yieldAtom))
    {
      throw new IOException("Remote protocol error.");
    }

  }

  void closeAsyncEclipseStreamEclipseSide(int streamid) throws IOException
  {
    super.closeAsyncEclipseStreamEclipseSide(streamid);
    // write queue_close(streamid) on the control connection
    writeControl(new CompoundTermImpl("queue_close",
                 new Integer(streamid)));
    // wait for a yield signal on the control connection
    Object result = readControl();
    if(!result.equals(yieldAtom))
    {
      throw new IOException("Remote protocol error.");
    }

  }

  /**
   * This buffer is used to store FromEclipseQueue data flushed by eclipse
   * through the socket after an ec_flushio signal. The buffer is initialised
   * with a DataInputStream (wrapped around the Socket's input stream). When
   * instructed, it can read a certain number of bytes from this stream into
   * the buffer, using the readBytesFromSocket method.
   *
   * These bytes can be read back from the socket using the readByte or
   * readBytes methods. Any amount can be read at a time as long as it does
   * not exceed the amount available.
   *
   */
  private class FromEclipseQueueBuffer
  {
    // number of available bytes held in the buffer
    private int available;
    // DataInputStream where the bytes come from. We use a DataInputStream
    // because it has the readFully method which blocks until either the
    // specified number of bytes have been read, the end-of-file character has
    // been reached, or there is an IOException
    private DataInputStream socketInputStream;
    // A queue storing chunks of bytes. Each chunk is the result of a call to
    // readBytesFromSocket.
    private Vector byteChunkVector;
    // The byte chunk in the buffer from where the first byte should be read
    private byte[] currentByteChunk;
    // The number of bytes which have already been read from the first byte
    // chunk
    private int readFromCurrentByteChunk;

    // constructor
    FromEclipseQueueBuffer(InputStream socketInputStream)
    {
      // initialise instance variables
      this.socketInputStream = new DataInputStream(socketInputStream);
      available = 0;
      byteChunkVector = new Vector();
      readFromCurrentByteChunk = 0;
      currentByteChunk = null;
    }
    // return the number of unread bytes stored in the buffer (some read bytes
    // will be stored in the currentByteChunk until it is exhausted).
    int available()
    {
      return(available);
    }
    // method to move currentByteChunk on to the next member of the queue.
    // If the queue is non-empty, the first member is popped and becomes the
    // current byte chunk. Otherwise the currentByteChunk is set to null.
    private void nextChunk()
    {
      if(byteChunkVector.size() > 0)
      {
        currentByteChunk = (byte []) byteChunkVector.remove(0);
      }
      else
      {
        currentByteChunk = null;
      }
      // we read from the beginning of the new currentByteChunk
      readFromCurrentByteChunk = 0;
    }

    // Method to read a byte from the buffer. Returns -1 if the buffer is empty
    int readByte()
    {
      byte signed_byte;

      if(available == 0)
      {
        return(-1);
      }
      // if we have reached the end of the current chunk, move on to the next.
      // Since available > 0, next chunk cannot be null
      if(readFromCurrentByteChunk == currentByteChunk.length)
      {
        nextChunk();
      }
      // One fewer bytes is available
      available--;
      // return the array element from the current byte chunk
      signed_byte = currentByteChunk[readFromCurrentByteChunk++];
      // in the byte chunk it is interpreted as between -128 and 127,
      // here we want a number between 0 and 255: if it is less than 0, add 256
      if(signed_byte < 0)
      {
        return(signed_byte+256);
      }
      else
      {
        return(signed_byte);
      }
    }

    // read <len> bytes from the buffer into array b, at offset <off>.
    // returns the number of bytes copied.
    int readBytes(byte[] b, int off, int len)
    {
      // the number of bytes copied into b, the number of unread bytes in the
      // current byte chunk and the number of bytes still remaining to be
      // copied into b
      int bytesCopied = 0, availableInCurrentChunk, bytesRequired;
      // while we have not copied enough bytes and there are still bytes to
      // copy...
      while(bytesCopied < len && available > 0)
      {
        // calculate no. of remaining bytes in current chunk
        availableInCurrentChunk =
          currentByteChunk.length - readFromCurrentByteChunk;
        // calculate no. of bytes still to be copied
        bytesRequired = len - bytesCopied;
        // if we can't get all remaining bytes from the current chunk
        if(availableInCurrentChunk < bytesRequired)
        {
          // copy all remaining bytes from the current chunk into b
          System.arraycopy(currentByteChunk, readFromCurrentByteChunk,
                           b, off + bytesCopied, availableInCurrentChunk);
          // update available and bytesCopied accordingly
          available -= availableInCurrentChunk;
          bytesCopied += availableInCurrentChunk;
          // move on to the next chunk
          nextChunk();
        }
        else // If we can get all remaining bytes from the current chunk into b
        {
          // copy the required bytes
          System.arraycopy(currentByteChunk, readFromCurrentByteChunk,
                           b, off + bytesCopied, bytesRequired);
          // update available, bytesCopied and readFromCurrentByteChunk
          // accordingly
          available -= bytesRequired;
          bytesCopied += bytesRequired;
          readFromCurrentByteChunk += bytesRequired;
        }
      }
      // return the number of bytes copied
      return(bytesCopied);
    }

    // read nBytes bytes from the input stream into the buffer. Block until this
    // is complete or it becomes impossible due to an exception.
    void readBytesFromSocket(int nBytes) throws IOException
    {
      // don't add an empty chunk if nBytes is 0
      if(nBytes == 0) return;
      // initialise a byte array for the new chunk
      byte[] newChunk = new byte[nBytes];
      // readFully the new chunk from the stream (blocks until complete or
      // throws exception)
      socketInputStream.readFully(newChunk);
      // add the new chunk on the end of the byteChunkVector queue.
      byteChunkVector.add(newChunk);
      // if the currentByteChunk was null because all the bytes were exhausted
      if(currentByteChunk == null)
      {
        // move it on to the first chunk
        nextChunk();
      }
      // update available
      available += nBytes;
    }
  }

  private void writeControl(Object message) throws IOException
  {
    try
    {
      controlEXDROutput.write(message);
      controlEXDROutput.flush();
    }
    catch(SocketException e)
    {
      unilateralDisconnect();
      throw(new EclipseTerminatedException());
    }
  }

  private void writeRPC(Object message) throws IOException
  {
    try
    {
      rpcEXDROutput.write(message);
      rpcEXDROutput.flush();
    }
    catch(SocketException e)
    {
      unilateralDisconnect();
      throw(new EclipseTerminatedException());
    }
  }

  private Object readControl() throws IOException
  {
    try
    {
      return(controlEXDRInput.readTerm());
    }
    catch(EOFException e)
    {
      unilateralDisconnect();
      throw(new EclipseTerminatedException());
    }
  }

  private Object readControlTimeout(int timeoutMillis) throws IOException
  {
    int old_timeout;
    old_timeout = control.getSoTimeout();
    try
    {
      control.setSoTimeout(timeoutMillis);
      return(readControl());
    }
    finally
    {
      control.setSoTimeout(old_timeout);
    }
  }


  private Object readRPCTimeout(int timeoutMillis) throws IOException
  {
    int old_timeout;
    old_timeout = rpc.getSoTimeout();
    try
    {
      rpc.setSoTimeout(timeoutMillis);
      return(readRPC());
    }
    finally
    {
      rpc.setSoTimeout(old_timeout);
    }
  }



  private Object readRPC() throws IOException
  {
    try
    {
      return(rpcEXDRInput.readTerm());
    }
    catch(EOFException e)
    {
      unilateralDisconnect();
      throw(new EclipseTerminatedException());
    }

  }


  /**
   * Finalizer method called when object is to be garbage collected
   */
  protected void finalize() throws IOException, EclipseException
  {
    this.unilateralDisconnect();
  }


  private CompoundTerm getNextControlTerm() throws IOException
  {
    Object nextControlObj = readControl();

    if(!(nextControlObj instanceof CompoundTerm))
    {
      throw(new IOException("Remote interface protocol error: control object not CompoundTerm"));
    }
    return((CompoundTerm) nextControlObj);

  }


  private boolean signalsYield(CompoundTerm controlTerm)
  {
    return(controlTerm.equals(yieldAtom));
  }
  private boolean signalsMultilateralDisconnect(CompoundTerm controlTerm)
  {
    return(controlTerm.equals(disconnectAtom));
  }
  private boolean signalsUnilateralDisconnect(CompoundTerm controlTerm)
  {
    return(controlTerm.equals(disconnectYieldAtom));
  }
  private boolean signalsFlushIO(CompoundTerm controlTerm)
  {
    return(controlTerm.functor().equals("ec_flushio") &&
           controlTerm.arity() == 2 &&
           controlTerm.arg(1) instanceof Integer &&
           controlTerm.arg(2) instanceof Integer);
  }
  private boolean signalsWaitIO(CompoundTerm controlTerm)
  {
    return(controlTerm.functor().equals("ec_waitio") &&
           controlTerm.arity() == 1 &&
           controlTerm.arg(1) instanceof Integer);
  }
  private boolean signalsCloseQueue(CompoundTerm controlTerm)
  {
    return(controlTerm.functor().equals("queue_close") &&
           controlTerm.arity() == 1 &&
           controlTerm.arg(1) instanceof Integer);
  }
  private boolean signalsOpenQueue(CompoundTerm controlTerm)
  {
    return(controlTerm.functor().equals("socket_client") &&
           controlTerm.arity() == 4 &&
           controlTerm.arg(1) instanceof Integer &&
           controlTerm.arg(2) instanceof Atom &&
           controlTerm.arg(3) instanceof Atom &&
           controlTerm.arg(4) instanceof Atom);
  }


  private void respondMultilateralDisconnect()
    throws IOException
  {
    //System.out.println("disconnection signal recieved from eclipse");
    // respond with the disconnect_resume acknowledgement
    writeControl(disconnectResumeAtom);
    //System.out.println("sent disconnect_resume message");
    // clean up java side
    terminateJavaSide();
    //System.out.println("completed java side of disconnection");
    throw(new EclipseTerminatedException());
  }
  private void respondUnilateralDisconnect()
    throws IOException
  {
    // clean up java side
    terminateJavaSide();
    //System.out.println("completed java side of disconnection");
    throw(new EclipseTerminatedException());
  }
  private void respondFlushIO(Integer streamID, Integer bytesFlushed)
    throws IOException
  {
    FromEclipseQueue feq;
    // look up the FromEclipseQueue based on the supplied stream number
    feq = lookupFromEclipseQueue(streamID.intValue());
    // if it is not there, print a message to stderr
    if (feq == null)
      System.err.println("ECLiPSe yielded after flushing stream "+streamID.intValue() +
                  " which is not registered as FromEclipseQueue.");
    else
    {
      bufferBytesFromSocket(streamID.intValue(), bytesFlushed.intValue());
      feq.notifyAvailable();
    }

    // pass control back to eclipse

  }

  void respondWaitIO(Integer streamID) throws IOException
  {
    super.respondWaitIO(streamID);
    // waitIO must always be answered with at least one rem_flushio/yield signal
    // pair. In this case the number of bytes flushed is 0, in case nothing
    // has been written to the queue. If something has been written but not
    // flushed, this is the API user's problem. The ECLiPSe-side read should
    // just block.
    writeControl(new CompoundTermImpl("rem_flushio",
                                      streamID,
                                      new Integer (0)));
    // hand control to Eclipse and handle any resulting events until yield
    waitForEclipse(false);


  }

  private void respondRemoteOpenQueue(Integer port, Atom nameAtom, Atom type,
                                      Atom direction)
    throws IOException
  {

    if(type.equals(syncAtom))
    {
      if(direction.equals(toecAtom))
      {
	setupRemoteToecQueue(nameAtom, port.intValue());
	createToEclipseQueue(nameAtom.functor());

	return;
      }

      if(direction.equals(fromecAtom))
      {
	setupRemoteFromecQueue(nameAtom, port.intValue());
	createFromEclipseQueue(nameAtom.functor());

	return;
      }

      throw new IOException("Remote interface protocol error: queue direction not recognised.");
    }
    else if(type.equals(asyncAtom))
    {
      if(direction.equals(bidirectAtom))
      {
	setupRemoteAsyncecQueue(nameAtom, port.intValue());
	createAsyncEclipseQueue(nameAtom.functor());

	return;
      }
      throw new IOException("Remote interface protocol error: queue direction not recognised.");
    }

    throw new IOException("Remote interface protocol error: queue type not recognised.");

  }

  class MultilateralDisconnectSignal extends ControlSignal
  {
    void respond() throws IOException
    {
      respondMultilateralDisconnect();
    }
  }

  class UnilateralDisconnectSignal extends ControlSignal
  {
    void respond() throws IOException
    {
      respondUnilateralDisconnect();
    }
  }

  class FlushIOSignal extends ControlSignal
  {
    private Integer streamID, bytesFlushed;

    FlushIOSignal(Integer streamID, Integer bytesFlushed)
    {
      this.streamID = streamID;
      this.bytesFlushed = bytesFlushed;
    }

    void respond() throws IOException
    {
      respondFlushIO(streamID, bytesFlushed);
    }
  }

  class CloseQueueSignal extends ControlSignal
  {
    private Integer streamID;

    CloseQueueSignal(Integer streamID)
    {
      this.streamID = streamID;
    }

    void respond() throws IOException
    {
      respondCloseQueue(streamID);
    }
  }

  class RemoteOpenQueueSignal extends ControlSignal
  {
    private Integer port;
    private Atom nameAtom;
    private Atom type;
    private Atom direction;

    RemoteOpenQueueSignal(Integer port, Atom nameAtom, Atom type, Atom direction)
    {
      this.port = port;
      this.nameAtom = nameAtom;
      this.type = type;
      this.direction = direction;
    }

    void respond() throws IOException
    {
      respondRemoteOpenQueue(port, nameAtom, type, direction);
    }
  }

  private FromEclipseQueueInfo getFromecInfo(int streamID)
  {
    return((FromEclipseQueueInfo) queueInfo.get(new Integer(streamID)));
  }

  private void bufferBytesFromSocket(int streamID, int nBytes)
    throws IOException
  {
    try
    {
      getFromecInfo(streamID).getBuffer().readBytesFromSocket(nBytes);
    }
    catch(EOFException e)
    {
      unilateralDisconnect();
      throw new EclipseTerminatedException();
    }
  }

  private void closeFromecSocket(int streamID) throws IOException
  {
    getFromecInfo(streamID).getSocket().close();
  }

  private void removeInfo(int streamID)
  {
    queueInfo.remove(new Integer(streamID));
  }

  private void setupFromecInfo(int streamID, Socket socket) throws IOException
  {

    queueInfo.put(new Integer(streamID), new FromEclipseQueueInfo(socket));
  }

  private int readByteFromBuffer(int streamID)
  {
    return(getFromecInfo(streamID).readByteFromBuffer());
  }

  private int readBytesFromBuffer(int streamID, byte[] b, int off, int len)
  {
    return(getFromecInfo(streamID).readBytesFromBuffer(b, off, len));
  }

  private int availableInBuffer(int streamID)
  {
    return(getFromecInfo(streamID).availableInBuffer());
  }

  private AsyncEclipseQueueInfo getAsyncecInfo(int streamID)
  {
    return((AsyncEclipseQueueInfo) queueInfo.get(new Integer(streamID)));
  }

  private void closeAsyncecSocket(int streamID) throws IOException
  {
    getAsyncecInfo(streamID).getSocket().close();
  }

  private void setupAsyncecInfo(int streamID, Socket socket) throws IOException
  {

    queueInfo.put(new Integer(streamID), new AsyncEclipseQueueInfo(socket));
  }

  InputStream getAsyncInputStream(int streamID) throws IOException
  {
    return getAsyncecInfo(streamID).getInputStream();
  }

  OutputStream getAsyncOutputStream(int streamID) throws IOException
  {
    return getAsyncecInfo(streamID).getOutputStream();
  }

  void closeAsyncEclipseStreamJavaSide(int streamid) throws IOException
  {
    super.closeAsyncEclipseStreamJavaSide(streamid);
    closeAsyncecSocket(streamid);
  }

  private class FromEclipseQueueInfo
  {
    private Socket socket;
    private FromEclipseQueueBuffer fromEclipseQueueBuffer;
    FromEclipseQueueInfo(Socket socket) throws IOException
    {
      this.socket = socket;
      this.fromEclipseQueueBuffer
        = new FromEclipseQueueBuffer(socket.getInputStream());
    }

    FromEclipseQueueBuffer getBuffer()
    {
      return(fromEclipseQueueBuffer);
    }

    Socket getSocket()
    {
      return(socket);
    }

    int readByteFromBuffer()
    {
      return(fromEclipseQueueBuffer.readByte());
    }
    int readBytesFromBuffer(byte[] b, int off, int len)
    {
      return(fromEclipseQueueBuffer.readBytes(b, off, len));
    }
    int availableInBuffer()
    {
      return(fromEclipseQueueBuffer.available());
    }

  }

  private class AsyncEclipseQueueInfo
  {
    private Socket socket;

    AsyncEclipseQueueInfo(Socket socket) throws IOException
    {
      this.socket = socket;
    }

    Socket getSocket()
    {
      return(socket);
    }

    InputStream getInputStream() throws IOException
    {
      return(socket.getInputStream());
    }

    OutputStream getOutputStream() throws IOException
    {
      return(socket.getOutputStream());
    }
  }



  private ToEclipseQueueInfo getToecInfo(int streamID)
  {
    return((ToEclipseQueueInfo) queueInfo.get(new Integer(streamID)));
  }

  private void closeToecSocket(int streamID) throws IOException
  {
    getToecInfo(streamID).getSocket().close();
  }

  private void setupToecInfo(int streamID, Socket socket) throws IOException
  {

    queueInfo.put(new Integer(streamID), new ToEclipseQueueInfo(socket));
  }

  private void setBytesBuffered(int streamID, int newVal)
  {
    getToecInfo(streamID).setBytesBuffered(newVal);
  }

  private int getBytesBuffered(int streamID)
  {
    return(getToecInfo(streamID).getBytesBuffered());
  }

  private OutputStream getOutputStream(int streamID)
  {
    return(getToecInfo(streamID).getOutputStream());
  }

  private class ToEclipseQueueInfo
  {
    private Socket socket;
    private int bytesBuffered;
    private OutputStream outputStream;

    void setBytesBuffered(int newValue)
    {
      bytesBuffered = newValue;
    }
    int getBytesBuffered()
    {
      return(bytesBuffered);
    }
    OutputStream getOutputStream()
    {
      return(outputStream);
    }

    ToEclipseQueueInfo(Socket socket) throws IOException
    {
      this.socket = socket;
      this.outputStream = new NonBlockingOutputStream(socket.getOutputStream());
      bytesBuffered = 0;
    }

    Socket getSocket()
    {
      return(socket);
    }

  }

  /**
   * An OutputStream whose write and flush methods do not block. This is useful
   * for when we would like to write large amounts of data to an output stream
   * but where that OutputStream's write/flush methods would normally block when
   * given such a large amount of input data.
   *
   * An instance is initialised by passing it a reference to the underlying
   * OutputStream.
   *
   * NonBlockingOutputStream's write methods store the bytes in a buffer and
   * therefore do not block.
   *
   * The flush method clears the buffer and puts its contents as a chunk of bytes
   * on a ByteChunkQueue. There is a "consumer" thread at the other end of the
   * queue, reading chunks of bytes and writing them to the underlying stream,
   * flushing it after each chunk.
   *
   * The write/flush methods will not raise IOExceptions by writing to the
   * buffer. However, if the copier thread was thrown an IOException when trying
   * to write to or flush the underlying stream, this IOException is stored,
   * and thrown at the next call of flush, write or close, with the text written
   * within an appropriate message. The close method may throw
   * IOExceptions either because an IOException from before was stored, or
   * because one was raised during its own operations.
   *
   *
   */
  private class NonBlockingOutputStream extends OutputStream
  {
    // The underlying stream where bytes are eventually written by the copier
    // thread
    private OutputStream underlying_stream;

    // The byte array where bytes are initially written
    private ByteArrayOutputStream bufferStream;

    // the copier thread (member class)
    private CopierThread copierThread;

    // if an IOException was thrown during writing to the underlying stream,
    // it is stored using this reference
    private IOException thrownException;

    // the queue on which chunks of bytes are queued, one chunk for each time the
    // flush method is called
    private ByteChunkQueue byteChunkQueue;

    // constructor
    NonBlockingOutputStream(OutputStream underlying_stream)
    {
      // Initialise instance variables
      thrownException = null;
      this.underlying_stream = underlying_stream;
      bufferStream = new ByteArrayOutputStream();
      byteChunkQueue = new ByteChunkQueue();
      copierThread = new CopierThread();
      // Start the copier thread
      copierThread.start();
    }

    // Method to throw the last IOException, wrapped in an appropriate message.
    // Also, resets the stored exception.
    private void throwLastIOException() throws IOException
    {
      if(thrownException != null)
      {
        throw(thrownException);
      }
    }

    // The public write methods work as follows:
    // 1. they throw any left-over IOExceptions
    // 2. they write the data to the buffer
    public void write(int b) throws IOException
    {
      throwLastIOException();
      bufferStream.write(b);
    }

    public void write(byte[] b) throws IOException
    {
      throwLastIOException();
      bufferStream.write(b);
    }

    public void write(byte[] b, int off, int len) throws IOException
    {
      throwLastIOException();
      bufferStream.write(b, off, len);
    }

    // returns the buffer contents as a byte array and clears the buffer.
    private byte[] getByteArrayAndReset()
    {
      byte[] bytes = bufferStream.toByteArray();
      bufferStream.reset();
      return(bytes);
    }

    // reads a chunk from the front of the queue, writes it to the underlying
    // stream and flushes it. Any IOExceptions from these operations are stored
    // in thrownException
    private void copyAndFlush()
    {
      // Get the next chunk of bytes from the buffer.
      byte[] copy_chunk = byteChunkQueue.retrieveChunk();

      try
      {
        // Try to write them to the buffer
        underlying_stream.write(copy_chunk);

        // If this works, try to flush underlying stream
        underlying_stream.flush();
      }
      catch(IOException ioe)
      {
        // Store any thrown exception and make the next invocation of
        // testTerminated disconnect unilaterally.
        thrownException = ioe;
        shouldDisconnect = true;
      }

    }

    // throws any leftover exception.
    // clears the buffer and stores its bytes as a chunk on the end of the queue
    // it wakes up any threads waiting on the queue.
    public void flush() throws IOException
    {
      // throw any Exceptions if there are any
      throwLastIOException();
      // if there are any bytes in the buffer
      if(bufferStream.size() > 0)
        {
          // get the buffer as a byte chunk and reset it
          byte[] byteChunk = getByteArrayAndReset();
          // add the chunk to the queue
          byteChunkQueue.addChunk(byteChunk);
          // wake up the copier thread
          copierThread.wake();
        }
    }

    // NOTE: does not wait until byte chunk queue is empty. Does wait until
    // copierThread is dead.
    public void close() throws IOException
    {
      // throw any stored exceptions
      throwLastIOException();
      // stop the copier thread
      copierThread.terminate();

      copierThread.interrupt(); // in case copierthread waiting and has not been
                                // notified

      // keep sleeping 1/4 of a second until copierThread is dead.
      while(copierThread.isAlive())
      {
        try
        {
          Thread.currentThread().sleep(250);
        }
        catch(InterruptedException ie)
        {
        }
      }
      // close the underlying stream
      underlying_stream.close();
    }

    // Object to represent the queue of byte chunks
    private class ByteChunkQueue
    {
      // represent the queue using Vector
      Vector queue;
      // constructor
      ByteChunkQueue()
      {
        queue = new Vector();
      }

      // synchronized methods for adding/retrieving and checking/waiting if empty.
      synchronized boolean isEmpty()
      {
        return(queue.isEmpty());
      }

      synchronized void waitUntilEmpty()
      {
        while(!isEmpty())
        {
          try
          {
            wait();
          }
          catch(InterruptedException ie) {}
        }
      }

      synchronized void addChunk(byte[] byteChunk)
      {
        queue.add(byteChunk);
      }

      synchronized byte[] retrieveChunk()
      {
        byte[] removedChunk = (byte[]) queue.remove(0);
        notifyAll(); // to wake anything waiting until empty.
        return(removedChunk);
      }
    }

    // class to implement thread which will copy and flush byte chunks
    private class CopierThread extends Thread
    {
      private boolean active = true;

      public CopierThread() {
        super();
        setDaemon(true);
      }

      // wait until either terminate or wake is called by another thread
      synchronized void waitWoken()
      {
        try
        {
          this.wait(1000); // timeout of one second
          // NOTE a timeout is needed because the notify may happen while the
          // copier thread is not in the wait state.
        }
        catch(InterruptedException ie)
        {}
      }

      synchronized void terminate()
      {
        active = false;
        notifyAll();
      }

      synchronized void wake()
      {
        notifyAll();
      }

      // Main method run when thread is started.
      public void run()
      {
        // Repeat this loop until terminated
        while(active)
        {
          // if the byteChunkQueue still has chunks, copy and flush them
         if(!byteChunkQueue.isEmpty())
         {
            copyAndFlush();
          }
          // if it is empty, wait until woken by terminate or wake
	 else
          {
            this.waitWoken();
          }
        }
      }
    }
  }



}
