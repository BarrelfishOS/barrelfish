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
//Version:      $Id: EclipseConnectionImpl.java,v 1.1 2006/09/23 01:54:09 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Abstract class providing common code for classes which implement the EclipseConnection interface.
package com.parctechnologies.eclipse;
import java.io.*;
import java.util.*;
import java.net.Socket;

/**
 * Abstract superclass of classes which implement the EclipseConnection interface.
 *
 * @see EmbeddedEclipse
 * @see RemoteEclipse
 */
public abstract class EclipseConnectionImpl implements EclipseConnection
{

  /**
   * Flag to indicate whether the ECLiPSe engine, or the connection to it,
   * has been terminated.
   */
  boolean terminated = false;

  /**
   * Maps ECLiPSe stream numbers (Integers) to the corresponding FromEclipseQueue.
   * Every FromEclipseQueue that is created for this EclipseConnectionImpl
   * is registered in this map with its
   * stream number. When it is closed, it is removed from the map.
   */
  private Map fromEclipseQueueRegister = new HashMap();

  /**
   * Maps ECLiPSe stream numbers (Integers) to the corresponding ToEclipseQueue.
   * Every ToEclipseQueue that is created is registered in this map with its
   * stream number. When it is closed, it is removed from the map.
   */
  private Map toEclipseQueueRegister = new HashMap();

  /**
   * Maps ECLiPSe stream numbers (Integers) to the corresponding AsyncEclipseQueue.
   * Every AsyncEclipseQueue that is created is registered in this map with its
   * stream number. When it is closed, it is removed from the map.
   */
  private Map asyncEclipseQueueRegister = new HashMap();

  /**
   * Stream used to send RPC goals to Eclipse. This must be initialised by the
   * concrete subclass -- no initialisation is performed here.
   */
  EXDROutputStream toEclipse;

  /**
   * Stream used to receive RPC goals from Eclipse. This must be initialised by the
   * concrete subclass -- no initialisation is performed here.
   */
  EXDRInputStream fromEclipse;

  /**
   * The singleton EclipseMultitaskConnection returned/created by the
   * registerMultitask method.
   */
  EclipseMultitaskConnection eclipseMultitaskConnection;

  /**
   * Peer name by which the Java side of the Eclipse connection is known in
   * ECLiPSe.
   */
  private Atom _peerName;

  // implements method required in EclipseConnection interface.
  public Atom getPeerName()
  {
    return(_peerName);
  }

  // set the peer name in this object. Should be called by subclass during
  // initialisation
  void setPeerName(Atom peerName)
  {
    _peerName = peerName;
  }

  /**
   * Test whether this EclipseConnectionImpl has been terminated, and if so,
   * throw an EclipseTerminatedException. This should be called at the beginning
   * of the implementations of the public methods of EclipseConnection.
   */
  void testTerminated() throws EclipseTerminatedException
  {
    if(terminated)
      throw new EclipseTerminatedException();
  }

  /**
   * Invoke the <code>close()</code> method on all registered user queues (i.e.
   * not system queues, such as ec_rpc_out in the embedded case).
   *
   * @param ec_side determines whether the eclipse side is closed as well as
   * the java side.
   */
  void closeAllQueues(boolean ec_side) throws IOException
  {
    // Invoke the close() method on all registered FromEclipseQueues
    closeAllFromEclipseQueues(ec_side);
    // Invoke the close() method on all registered ToEclipseQueues
    closeAllToEclipseQueues(ec_side);
    closeAllAsyncEclipseQueues(ec_side);
  }

  /**
   * Close all registered FromEclipseQueues (user queues only).
   * @param ec_side determines whether the eclipse side is closed as well as
   * the java side.
   */
  private void closeAllFromEclipseQueues(boolean ec_side) throws IOException
  {
    // We get the collection of queues by taking a copy of the value set
    // of the register. We take a copy because the close() method will
    // alter this set while we are iterating over it.
    Collection fromEclipseQueues = new LinkedList(fromEclipseQueueRegister.values());
    Iterator i = fromEclipseQueues.iterator();
    FromEclipseQueue feq;

    while(i.hasNext())
    {
      feq = (FromEclipseQueue) i.next();
      if(!feq.isSystemQueue())
      {
        feq.close_cleanup();
        this.closeFromEclipseStreamJavaSide(feq.getID());
        if(ec_side)
        {
          this.closeFromEclipseStreamEclipseSide(feq.getID());
        }
      }
    }
  }

  /**
   * Close all registered ToEclipseQueues (user queues only).
   * @param ec_side determines whether the eclipse side is closed as well as
   * the java side.
   */
  private void closeAllToEclipseQueues(boolean ec_side) throws IOException
  {
    // We get the collection of queues by taking a copy of the value set
    // of the register. We take a copy because the close() method will
    // alter this set while we are iterating over it.
    Collection toEclipseQueues = new LinkedList(toEclipseQueueRegister.values());
    Iterator i = toEclipseQueues.iterator();
    ToEclipseQueue teq;

    while(i.hasNext())
    {
      teq = (ToEclipseQueue) i.next();
      if(!teq.isSystemQueue())
      {
        this.closeToEclipseStreamJavaSide(teq.getID());
        if(ec_side)
        {
          this.closeToEclipseStreamEclipseSide(teq.getID());
        }
      }
    }
  }

  /**
   * Close all registered AsyncEclipseQueues (user queues only).
   * @param ec_side determines whether the eclipse side is closed as well as
   * the java side.
   */
  private void closeAllAsyncEclipseQueues(boolean ec_side) throws IOException
  {
    // We get the collection of queues by taking a copy of the value set
    // of the register. We take a copy because the close() method will
    // alter this set while we are iterating over it.
    Collection asyncEclipseQueues = new LinkedList(asyncEclipseQueueRegister.values());
    Iterator i = asyncEclipseQueues.iterator();
    AsyncEclipseQueue aeq;

    while(i.hasNext())
    {
      aeq = (AsyncEclipseQueue) i.next();
      if(!aeq.isSystemQueue())
      {
        this.closeAsyncEclipseStreamJavaSide(aeq.getID());
        if(ec_side)
        {
          this.closeAsyncEclipseStreamEclipseSide(aeq.getID());
        }
      }
    }
  }

  /**
   * Get an already registered FromEclipseQueue of this ECLiPSe given its
   * ECLiPSe stream number.
   */
  FromEclipseQueue lookupFromEclipseQueue(int id)
  {
    return (FromEclipseQueue) fromEclipseQueueRegister.get(new Integer(id));
  }

  /**
   * Get an already registered ToEclipseQueue of this ECLiPSe given its
   * ECLiPSe stream number.
   */
  ToEclipseQueue lookupToEclipseQueue(int id)
  {
    return (ToEclipseQueue) toEclipseQueueRegister.get(new Integer(id));
  }

  /**
   * Get an already registered AsyncEclipseQueue of this ECLiPSe given its
   * ECLiPSe stream number.
   */
  AsyncEclipseQueue lookupAsyncEclipseQueue(int id)
  {
    return (AsyncEclipseQueue) asyncEclipseQueueRegister.get(new Integer(id));
  }

  /**
   * Register a new FromEclipseQueue with its stream number.
   */
  void registerFromEclipseQueue(int id, FromEclipseQueue inputQueue) throws EclipseTerminatedException
  {
    fromEclipseQueueRegister.put(new Integer(id), inputQueue);
  }

  /**
   * Register a new ToEclipseQueue with its stream number.
   */
  void registerToEclipseQueue(int id, ToEclipseQueue outputQueue) throws EclipseTerminatedException
  {
    toEclipseQueueRegister.put(new Integer(id), outputQueue);
  }

  /**
   * Register a new AsyncEclipseQueue with its stream number.
   */
  void registerAsyncEclipseQueue(int id, AsyncEclipseQueue queue) throws EclipseTerminatedException
  {
    asyncEclipseQueueRegister.put(new Integer(id), queue);
  }

  /**
   * Unregister a FromEclipseQueue given its stream number.
   */
  void unregisterFromEclipseQueue(int id)
  {
    fromEclipseQueueRegister.remove(new Integer(id));
  }

  /**
   * Unregister an ToEclipseQueue given its stream number.
   */
  void unregisterToEclipseQueue(int id)
  {
    toEclipseQueueRegister.remove(new Integer(id));
  }

  /**
   * Unregister an AsyncEclipseQueue given its stream number.
   */
  void unregisterAsyncEclipseQueue(int id)
  {
    asyncEclipseQueueRegister.remove(new Integer(id));
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized void compile(File f) throws EclipseException, IOException
  {
    rpc(new CompoundTermImpl("compile" , getPath(f)));
  }

  // Implementation of public method from EclipseConnection interface
  public String getPath(File f) throws EclipseException, IOException
  {
    CompoundTerm call = new CompoundTermImpl("os_file_name" , null , f.getAbsolutePath() );
    return (String) rpc(call).arg(1);
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized CompoundTerm rpc(String goal) throws EclipseException,
    IOException
  {
    testTerminated();
    return executeRpc(goal);
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object arg1) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, arg1)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, arg1, arg2)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, arg1, arg2, arg3)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, arg1, arg2, arg3, arg4)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4,
                          Object arg5) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, arg1, arg2, arg3, arg4, arg5)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(String functor, Object[] args) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(functor, args)));
  }

  // Implementation of public method from EclipseConnection interface
  public CompoundTerm rpc(Object[] goalTerm) throws EclipseException, IOException
  {
    return(rpc(new CompoundTermImpl(goalTerm)));
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized CompoundTerm rpc(CompoundTerm goal) throws EclipseException, IOException
  {
    testTerminated();
    return executeRpc(goal);
  }

  /**
   * Common implementation for rpc, for both Strings and CompoundTerms. Relies
   * on three abstract methods to be implemented by subclasses: sendGoal(Object),
   * waitForEclipse() and receiveGoal().
   */
  private CompoundTerm executeRpc(Object goal) throws EclipseException, IOException
  {
    // send the goal object to ECLiPSe
    sendGoal(goal);
    // pass control to ECLiPSe and handle any events it generates when it
    // returns control. Keep doing this until it reports that it has finished
    // executing the rpc goal.
    waitForEclipse(false);
    // receive the goal term from ECLiPSe
    CompoundTerm answer = (CompoundTerm) receiveGoal();

    // if the returned term is the atom fail
    if(answer.functor().equals("fail") &&
       answer.arity() == 0)
    {
      // throw the appropriate fail exception
      throw new Fail(goal);
    }
    // similarly for throw
    if(answer.functor().equals("throw") &&
       answer.arity() == 0)
    {
      throw new Throw(goal);
    }
    // otherwise return the returned goal term.
    return answer;
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized FromEclipseQueue getFromEclipseQueue(String name)
    throws EclipseException, IOException
  {
    // throw exception if terminated
    testTerminated();

    // try to get the numeric id of the stream in eclipse (returns
    // negative if the stream name is not valid)
    int id = getStreamNumber(name);

    // if id is non-negative then see if it is registered
    if(id >= 0)
    {
      FromEclipseQueue feq = lookupFromEclipseQueue(id);

      // If so, return it.
      if (feq != null)
      {
        return(feq);
      }
    }

    try
    {
      // see if there is already a stream with the above name on the eclipse side
      rpc("current_stream", new Atom(name));
      // if the above goal succeeds, throw an exception
      throw(new EclipseException("Cannot create FromEclipseQueue: stream name in use."));
    }
    catch(Fail e) // if the above goal fails
    {
      // additional setup routine
      // this is implemented by an abstract method as it will vary according to
      // subclass
      setupFromEclipseQueue(name);
      // create, register and return the queue based on this
      return(createFromEclipseQueue(name));

    }
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized ToEclipseQueue getToEclipseQueue(String name)
    throws EclipseException, IOException
  {
    // throw exception if terminated
    testTerminated();

    // try to get the numeric id of the stream in eclipse (returns
    // negative if the stream name is not valid)
    int id = getStreamNumber(name);

    // if id is non-negative then see if it is registered
    if(id >= 0)
    {
      ToEclipseQueue teq = lookupToEclipseQueue(id);

      // If it is, return it
      if (teq != null)
      {
        return teq;
      }
    }

    try
    {
      // see if there is already a stream with the above name
      rpc("current_stream", new Atom(name));
      // if the above goal succeeds, throw an exception
      throw(new EclipseException("Cannot create ToEclipseQueue: stream name in use."));
    }
    catch(Fail e) // if the above goal fails
    {
      // additional setup routine
      // this is implemented by an abstract method as it will vary according to
      // subclass
      setupToEclipseQueue(name);
      // create, register and return the queue based on this
      return(createToEclipseQueue(name));
    }
  }


  // Implementation of public method from EclipseConnection interface
  public synchronized AsyncEclipseQueue getAsyncEclipseQueue(String name)
    throws EclipseException, IOException
  {
    // throw exception if terminated
    testTerminated();

    // try to get the numeric id of the stream in eclipse (returns
    // negative if the stream name is not valid)
    int id = getStreamNumber(name);

    // if id is non-negative then see if it is registered
    if(id >= 0)
    {
      AsyncEclipseQueue aeq = lookupAsyncEclipseQueue(id);

      // If it is, return it
      if (aeq != null)
      {
        return aeq;
      }
    }

    try
    {
      // see if there is already a stream with the above name
      rpc("current_stream", new Atom(name));
      // if the above goal succeeds, throw an exception
      throw(new EclipseException("Cannot create AsyncEclipseQueue: stream name in use."));
    }
    catch(Fail e) // if the above goal fails
    {
      // additional setup routine
      // this is implemented by an abstract method as it will vary according to
      // subclass
      setupAsyncEclipseQueue(name);
      // create, register and return the queue based on this
      return(createAsyncEclipseQueue(name));
    }
  }

  /**
   * Creates, registers and returns a new FromEclipseQueue object. Assumes that
   * there is no FromEclipseQueue registered in this Eclipse with the same name.
   * Also assumes that the Eclipse side of the queue has been set up.
   */
  FromEclipseQueue createFromEclipseQueue(String name) throws IOException
  {
    int id = getStreamNumber(name);
    FromEclipseQueue inQ = new FromEclipseQueue(id,name,this);
    registerFromEclipseQueue(id,inQ);
    return inQ;
  }

  /**
   * Creates, registers and returns a new ToEclipseQueue object. Assumes that
   * there is no ToEclipseQueue registered in this Eclipse with the same name.
   * Also assumes that the Eclipse side of the queue has been set up.
   */
  ToEclipseQueue createToEclipseQueue(String name) throws IOException
  {
    int id = getStreamNumber(name);
    ToEclipseQueue outQ = new ToEclipseQueue(id,name,this);
    registerToEclipseQueue(id,outQ);
    return outQ;
  }

  /**
   * Creates, registers and returns a new AsyncEclipseQueue object. Assumes that
   * there is no AsyncEclipseQueue registered in this Eclipse with the same name.
   * Also assumes that the Eclipse side of the queue has been set up.
   */
  AsyncEclipseQueue createAsyncEclipseQueue(String name) throws IOException
  {
    int id = getStreamNumber(name);
    AsyncEclipseQueue q = new AsyncEclipseQueue(id,name,this);
    registerAsyncEclipseQueue(id,q);
    return q;
  }

  InputStream getAsyncInputStream(int id) throws IOException
  {
    throw new IOException("Asynchronous queues not implemented for this connection type");
  }

  OutputStream getAsyncOutputStream(int id) throws IOException
  {
    throw new IOException("Asynchronous queues not implemented for this connection type");
  }

  /**
   * Send an RPC goal to ECLiPSe.
   */
  abstract void sendGoal(Object goal) throws IOException;

  /**
   * Receive an RPC goal from ECLiPSe.
   */
  abstract Object receiveGoal() throws IOException;


  /**
   * Look up the stream number of an existing stream within this ECLiPSe,
   * given its name. Returns negative if there is no stream with that name.
   */
  int getStreamNumber(String streamName) throws IOException
  {
    try
    {
      rpc("current_stream", new Atom(streamName));
      CompoundTerm result =
        rpc("get_stream_info", new Atom(streamName), new Atom("physical_stream"), null);
      Integer stream_number = (Integer) result.arg(3);
      return(stream_number.intValue());
    }
    catch(EclipseException f)
    {
      // if there is no stream with the supplied name, return -1
      return(-1);
    }

  }


  /**
   * Abstract methods, must be implemented by subclasses. These are used to
   * supply the subclass-specific implementations of certain operations used by
   * methods in both this class and other classes in the package.
   */

  /**
   * Perform any additional setup required to initialise a
   * FromEclipseQueue
   */
  abstract void setupFromEclipseQueue(String name)
    throws EclipseException, IOException;

  /**
   * Perform any additional setup required to initialise a
   * ToEclipseQueue
   */
  abstract void setupToEclipseQueue(String name)
    throws EclipseException, IOException;

  /**
   * Perform any additional setup required to initialise a
   * AsyncEclipseQueue
   */
  abstract void setupAsyncEclipseQueue(String name)
    throws EclipseException, IOException;

  /**
   * Keep resuming ECLiPSe and handling any control signals it generates
   * until a yield signal occurs.
   */
  void waitForEclipse(boolean transferControlWithResume) throws IOException
  {
    boolean isFirstIteration = true;
    ControlSignal nextControlSignal;
    do
    {
      nextControlSignal =
        getNextControlSignal(isFirstIteration, transferControlWithResume);
      if(nextControlSignal == null)
        throw new IOException("Unrecognised ECLiPSe control signal.");
      isFirstIteration = false;
      nextControlSignal.respond();
    }
    while(!(nextControlSignal instanceof YieldSignal));
  }

  abstract ControlSignal getNextControlSignal(boolean isFirstIteration,
                                              boolean transferControlWithResume)
    throws IOException;


  abstract class ControlSignal
  {
    abstract void respond() throws IOException;
  }

  class YieldSignal extends ControlSignal
  {
    void respond() throws IOException
    {
      respondYield();
    }
  }

  class WaitIOSignal extends ControlSignal
  {
    private Integer streamID;
    WaitIOSignal(Integer streamID)
    {
      this.streamID = streamID;
    }
    void respond() throws IOException
    {
      respondWaitIO(streamID);
    }
  }

  class OpenQueueSignal extends ControlSignal
  {
    private Atom nameAtom;
    private Integer streamID;
    private Atom direction;
    OpenQueueSignal(Atom nameAtom, Integer streamID, Atom direction)
    {
      this.nameAtom = nameAtom;
      this.streamID = streamID;
      this.direction = direction;
    }
    void respond() throws IOException
    {
      respondOpenQueue(nameAtom, streamID, direction);
    }
  }

  void respondYield() throws IOException
  {
  }

  void respondWaitIO(Integer streamID) throws IOException
  {
    // look up the queue in the toEclipseQueue register
    ToEclipseQueue teq = lookupToEclipseQueue(streamID.intValue());
    // if it is not there, print a message to stderr
    if (teq == null) {
      System.err.println("ECLiPSe yielded after reading empty stream "+
      streamID.intValue() +
        " which is not registered as a ToEclipseQueue.");
    } else {
      // otherwise notify the queue's listener of a request for data
      // (causes its dataRequest method to be invoked)
      teq.notifyRequest();
    }
  }

  void respondCloseQueue(Integer streamID) throws IOException
  {
    FromEclipseQueue feq = null;
    ToEclipseQueue teq = null;
    AsyncEclipseQueue aeq = null;
    teq = lookupToEclipseQueue(streamID.intValue());
    if(teq != null)
    {
      teq.close_cleanup();
      closeToEclipseStreamJavaSide(streamID.intValue());
      return;
    }
    feq = lookupFromEclipseQueue(streamID.intValue());
    if(feq != null)
    {
      feq.close_cleanup();
      closeFromEclipseStreamJavaSide(streamID.intValue());
      return;
    }
    aeq = lookupAsyncEclipseQueue(streamID.intValue());
    if(aeq != null)
    {
      aeq.close_cleanup();
      closeAsyncEclipseStreamJavaSide(streamID.intValue());
      return;
    }
    System.err.println("Cannot close "+streamID+": not the "+
      "stream number of a registered ECLiPSe queue.");
  }


  void respondOpenQueue(Atom nameAtom, Integer streamID, Atom direction)
    throws IOException
  {
    if(direction.functor().equals("fromec"))
    {
      createFromEclipseQueue(nameAtom.functor());
    }
    else if(direction.functor().equals("toec"))
    {
      createToEclipseQueue(nameAtom.functor());
    }
    else if(direction.functor().equals("bidirect"))
    {
      createAsyncEclipseQueue(nameAtom.functor());
    }
  }






  /**
   * Read <code>len</code> bytes from this ECLiPSe's stream number
   * <code>streamid</code> and store them in
   * byte array <code>b</code> at offset <code>off</code>.
   *
   * @returns the number of bytes read.
   */
  abstract int readFromStream(int streamid, int off, int len, byte[] b)
    throws IOException;

  /**
   * Read a single byte from this ECLiPSe's stream number
   * <code>streamid</code>
   *
   * @returns byte read, an int between 0 and 255 or -1 if 0 bytes were read.
   */
  abstract int readByteFromStream(int streamid) throws IOException;

  /**
   * Returns the number of bytes available on stream streamid which may be
   * read or skipped over without blocking.
   */
  abstract int availableOnStream(int streamid) throws IOException;


  /**
   * Write <code>len</code> bytes to this ECLiPSe's stream number
   * <code>streamid</code> at offset <code>off</code> from
   * byte array <code>b</code>.
   *
   * @returns the number of bytes written.
   */
  abstract int writeToStream(int streamid, byte[] b, int off, int len)
    throws IOException;

  /**
   * Write a single byte to this ECLiPSe's stream number
   * <code>streamid</code>.
   *
   */
  abstract void writeByteToStream(int streamid, byte b)
    throws IOException;

  /**
   * Flush this ECLiPSe's stream number <code>streamid</code>.
   *
   */
  abstract void flushStream(int streamid) throws IOException;

  // when a ToEclipseStream is closed, unregister it. Subclasses may
  // over-ride this method and perform additional actions, but they should call
  // it first using super.
  void closeToEclipseStreamJavaSide(int streamid) throws IOException
  {
    ToEclipseQueue teq = lookupToEclipseQueue(streamid);
    unregisterToEclipseQueue(streamid);
  }

  // when a AsyncEclipseStream is closed, unregister it. Subclasses may
  // over-ride this method and perform additional actions, but they should call
  // it first using super.
  void closeAsyncEclipseStreamJavaSide(int streamid) throws IOException
  {
    unregisterAsyncEclipseQueue(streamid);
  }

  // when a FromEclipseStream is closed, unregister it. Subclasses may
  // over-ride this method and perform additional actions, but they should call
  // it first using super.
  void closeFromEclipseStreamJavaSide(int streamid) throws IOException
  {
    FromEclipseQueue feq = lookupFromEclipseQueue(streamid);
    unregisterFromEclipseQueue(streamid);
  }

  // Common code to close the eclipse side of a From/ToEclipse queue. Subclasses
  // will override these methods to perform additional actions, but they should
  // invoke them first using super.
  void closeFromEclipseStreamEclipseSide(int streamid) throws IOException
  {
  }

  void closeToEclipseStreamEclipseSide(int streamid) throws IOException
  {
  }

  void closeAsyncEclipseStreamEclipseSide(int streamid) throws IOException
  {
  }

  synchronized EclipseMultitaskConnection getEclipseMultitaskConnection() throws EclipseException, IOException {
    if ( eclipseMultitaskConnection == null ) {
      CompoundTermImpl resultGoal =
        (CompoundTermImpl)
        rpc("peer_register_multitask",
            getPeerName(),
            null);
      String fromStream = ((Atom)resultGoal.arg(2)).functor();
      FromEclipseQueue queue = getFromEclipseQueue(fromStream);
      eclipseMultitaskConnection =
        new EclipseMultitaskConnectionImpl(this, queue);
    }
    return eclipseMultitaskConnection;
  }

  // implements method from EclipseConnection
  public EclipseMultitaskConnection registerMultitask(MultitaskListener multitaskListener) throws EclipseException,IOException {
    testTerminated();
    EclipseMultitaskConnection emc = getEclipseMultitaskConnection();
    return emc.registerMultitask(multitaskListener);
  }
}
