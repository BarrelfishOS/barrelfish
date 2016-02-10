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
 * Wrapper class which implements the EclipseMultitaskConnection
 * interface so as to make it seem as though the peer has control for
 * the entire duration of ECLiPSe multitasking phases.
 *
 * <p> Objects of this class can only be constructed by calling the
 * registerMultitask method of EclipseConnections. This class works by
 * delegating most of the work to the EclipseConnection that created it.
 *
 *
 * @see EclipseMultitaskConnection
 * */
class EclipseMultitaskConnectionImpl implements EclipseMultitaskConnection {
  static final String STATE_EXCEPTION_MESSAGE = "Attempt to confirm/terminate multitasking phase whilst not in multitasking phase.";

  protected static final Atom peerConfirmMultitaskGoal = new Atom("peer_multitask_confirm");
  protected static final Atom peerTerminateMultitaskGoal = new Atom("peer_multitask_terminate");

  static final String START_MULTITASK_MESSAGE = "start_multitask";
  static final String END_MULTITASK_MESSAGE = "end_multitask";
  static final String INTERACT_MULTITASK_MESSAGE = "interact";

  static final int MULTITASK_RPC_TIMEOUT = 10000;

  /** underlying eclipse connection */
  EclipseConnectionImpl eclipse;
  
  /** queue on which multitask messages are recieved */
  FromEclipseQueue multitaskQueue;
  
  /** Are we in a multitask phase, and if so which type. null=not in
      multitask phase. */
  String multitaskPhase;

  /** Holds the thread on which the multitask phase messages are begin
      delivered.  Any RPCs performed during a multitask phase fromthis
      thread must be begin called from inside queue callbacks and so
      they should NOT be delayed, but rather should be performed
      immediately (so as to avoid deadlock). */
  Thread multitaskThread;

  /** Holds the delayed RPCs */
  MultitaskGoalQueue multitaskGoalQueue;

  /** Holds the MultitaskListeners */
  List multitaskListenerList ;
  
  EclipseMultitaskConnectionImpl(EclipseConnectionImpl eclipse,
                                 FromEclipseQueue multitaskQueue) throws IOException {
    this.eclipse = eclipse;
    this.multitaskQueue = multitaskQueue;
    this.multitaskGoalQueue = new MultitaskGoalQueue();
    this.multitaskPhase = null;
    this.multitaskListenerList = new LinkedList();
    this.multitaskThread = null;
    // register the queue listener on the multitask queue
    multitaskQueue.setListener(new MultitaskQL());      
  }

  public CompoundTerm rpc(String goal) throws EclipseException, IOException {
    if ((multitaskPhase == null) ||
        (Thread.currentThread() == multitaskThread)) {
      return eclipse.rpc(goal);
    }
    eclipse.testTerminated();
    return multitaskGoalQueue.execute(goal);
  }

  public CompoundTerm rpc(CompoundTerm goal) throws EclipseException, IOException {
    if ((multitaskPhase == null) ||
        (Thread.currentThread() == multitaskThread)) {
      return eclipse.rpc(goal);
    }
    eclipse.testTerminated();
    return multitaskGoalQueue.execute(goal);
  }
  
  public FromEclipseQueue getFromEclipseQueue(String name) throws EclipseException, IOException {
    return eclipse.getFromEclipseQueue(name);
  }

  public ToEclipseQueue getToEclipseQueue(String name) throws EclipseException, IOException {
    return eclipse.getToEclipseQueue(name);
  }

  public AsyncEclipseQueue getAsyncEclipseQueue(String name) throws EclipseException, IOException {
    return eclipse.getAsyncEclipseQueue(name);
  }

  public void compile(File f) throws EclipseException, IOException {
    rpc(new CompoundTermImpl("compile" , getPath(f)));
  }
  
  public String getPath(File f) throws EclipseException, IOException {
    CompoundTerm call = new CompoundTermImpl("os_file_name" , null , f.getAbsolutePath() );
    return (String) rpc(call).arg(1);
  }

  public CompoundTerm rpc(String functor, Object arg1) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor,arg1));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor, arg1, arg2));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor, arg1, arg2, arg3));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor, arg1, arg2, arg3, arg4));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4,
                          Object arg5) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor, arg1, arg2, arg3, arg4, arg5));
  }

  public CompoundTerm rpc(String functor, Object[] args) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(functor, args));
  }

  public CompoundTerm rpc(Object[] goalTerm) throws EclipseException, IOException {
    return rpc(new CompoundTermImpl(goalTerm));
  }

  public Atom getPeerName() {
    return eclipse.getPeerName();
  }

  /**
   * Add the given listener to this EclipseMultitaskConnection.
   * @return <code>this</code>.  Since this object is already
   * registered for multitask phases. */
  public EclipseMultitaskConnection registerMultitask(MultitaskListener multitaskListener) throws EclipseException,IOException {
    eclipse.testTerminated();
    if (multitaskListener != null) {
      multitaskListenerList.add(multitaskListener);
    }
    return this;
  }

 /**
   * Set the property that a multitasking phase is in progress
   */
  void setMultitaskPhase(String type) {
    multitaskPhase = type ;
  }

  /**
   * Perform any pending RPC's
   */
  void processMultitaskTimeSlice() {
    // execute any delayed RPC's using the underlying EclipseConnection
    multitaskGoalQueue.process(eclipse);
  }
  

  private static class MultitaskGoalQueue {
    /** Stores the MultitaskGoals yet to be processed by the
        multitask timeslice handler */
    private List pendingGoals = new LinkedList();

    /** Stores any exception which occurs as a result of errors in the
        multitask protocol */
    Exception multitaskProtocolException;

    synchronized void setProtocolException(Exception ioe) {
      if (multitaskProtocolException != null) {
        multitaskProtocolException = ioe;
      }
    }

    synchronized void testProtocolException() throws EclipseException,IOException {
      if (multitaskProtocolException != null) {
        if (multitaskProtocolException instanceof EclipseException) {
          throw (EclipseException)multitaskProtocolException;
        }
        if (multitaskProtocolException instanceof IOException) {
          throw (IOException)multitaskProtocolException;
        }
        throw new IOException("Multitask protocol error:"+
                              multitaskProtocolException.getMessage());
      }
    }

    /** Puts the Object (String or CompoundTerm) on the list of goals
        to be executed and then wait until it has been. */
    public CompoundTerm execute(Object goal) throws IOException, EclipseException {
      MultitaskGoal mg = new MultitaskGoal(goal);
      synchronized(this) {
        pendingGoals.add(mg);
      }
      synchronized(mg) {
        while((mg.result == null) && (mg.exception == null)) {
          try {
            mg.wait(MULTITASK_RPC_TIMEOUT);
          }
          catch(InterruptedException ie){}
          testProtocolException();
        }
      }
      // if an exception occured during the RPC, then re-throw it in
      // the caller thread
      if (mg.exception != null) {
        if (mg.exception instanceof IOException) {
          throw (IOException)(mg.exception).fillInStackTrace();
        } else if (mg.exception instanceof EclipseException) {
          throw (EclipseException)(mg.exception).fillInStackTrace();
        } else {
          throw new EclipseException(mg.exception.getMessage());
        }
      }
      return mg.result;
    }
    
    /** Process the pending goals.  This should only be called from
        within the handler for the multitask message queue. */
    public void process(EclipseConnection eclipse) {
      Iterator it;
      synchronized(this) {
        if (pendingGoals.isEmpty()) {
          return;
        }
        // get the iterator for the current pending goals
        it = pendingGoals.iterator();
        // clear the pending goals before performing any RPC to avoid
        // infinite recusion
        pendingGoals = new LinkedList();
      }
      while(it.hasNext()) {
        MultitaskGoal mg = (MultitaskGoal)(it.next());
        try {
          if (mg.goal instanceof CompoundTerm) {
            mg.result = eclipse.rpc((CompoundTerm)mg.goal);
          } else if (mg.goal instanceof String) {
            mg.result = eclipse.rpc((String)mg.goal);
          } else {
            throw new EclipseException("Unknown object type ("+mg.goal+") for rpc.");
          }
        } catch(IOException ioe) {
          mg.exception = ioe;
        } catch(EclipseException ee) {
          mg.exception = ee;
        }
        synchronized(mg) {
          /* wake up the threads which were waiting for the results */
          mg.notifyAll();
        }
      }
    }
  }

  /** Class to hold the goal to be executed by the Multitask.timeslice
   * thread */
  private static class MultitaskGoal
  { 
    private Object goal;
    private CompoundTerm result;
    private Exception exception;
    
    private MultitaskGoal(Object goal)
    {
      this.goal = goal;
    }
  }



  /** Queue listener attached to the "Multitask" stream.  Behaviour is
      to set the state to indicate being in multi-tasking
      mode */
  private class MultitaskQL implements QueueListener
  {
    protected CompoundTerm eventTerm;
    EXDRInputStream eis = null;
    

    public void dataAvailable(Object source)
    {
      if(eis == null) {
        FromEclipseQueue feq = (FromEclipseQueue) source;
        eis = new EXDRInputStream(feq);
      }
      try {
        eventTerm = (CompoundTerm) eis.readTerm();
        processEvent();
      } catch(EclipseException ee) {
        // record the exception so it can be thrown by the next thread
        // to attempt multitasking RPC
        multitaskGoalQueue.setProtocolException(ee);
      } catch(IOException ioe) {
        // record the exception so it can be thrown by the next thread
        // to attempt multitasking RPC
        multitaskGoalQueue.setProtocolException(ioe);
      }
    }
    
    void processEvent() throws EclipseException,IOException {
      String functor = eventTerm.functor();
      if (START_MULTITASK_MESSAGE.equals(functor)) {
        Object typeTerm = eventTerm.arg(1);
        if (typeTerm instanceof String) {
          setMultitaskPhase((String)typeTerm);
        } else if (typeTerm instanceof CompoundTerm) {
          setMultitaskPhase(((CompoundTerm)typeTerm).functor());
        } else {
          throw new IOException("Multitask protocol error.  Unexpected multitasking phase type:"+typeTerm);
        }
        /* Record the thread on which this messages was delivered */
        multitaskThread = Thread.currentThread();
        /* notify all listeners */
        for(Iterator it = multitaskListenerList.iterator(); it.hasNext(); ) {
          MultitaskListener ml = (MultitaskListener)it.next();
          ml.starting(EclipseMultitaskConnectionImpl.this,getMultitaskPhase());
        }
        /* perform any pending rpc*/
        processMultitaskTimeSlice();
      } else if (END_MULTITASK_MESSAGE.equals(functor)) {
        /* notify all listeners */
        for(Iterator it = multitaskListenerList.iterator(); it.hasNext(); ) {
          MultitaskListener ml = (MultitaskListener)it.next();
          ml.ending(EclipseMultitaskConnectionImpl.this,getMultitaskPhase());
        }
        /* perform any pending rpc*/
        processMultitaskTimeSlice();
        /* Clear the thread on which this messages was delivered */
        multitaskThread = null;
        /* leave the multitask phase */
        setMultitaskPhase(null);
      } else if (INTERACT_MULTITASK_MESSAGE.equals(functor)) {
        /* perform any pending rpc*/
        processMultitaskTimeSlice();
      }
    }
    
    public void dataRequest(Object source){}
  }

  /**
   * Gets the non-"multitasking aware" EclipseConnection from which
   * this "multitasking aware" connection was created..
   *
   * <p>This method is provided incase the caller needs access to the
   * type of the underlying connection, or if the caller requires the
   * blocking RPC semantics of the underlying connection. */
  public EclipseConnection getEclipseConnection() {
    return eclipse;
  }

  protected String getMultitaskPhase() throws EclipseException, IOException {
    eclipse.testTerminated();
    multitaskGoalQueue.testProtocolException();
    return multitaskPhase;
  }

  public void multitaskConfirm() throws EclipseException, IOException, IllegalStateException {
    synchronized(this) {
      if (multitaskPhase == null) {
        throw new IllegalStateException(STATE_EXCEPTION_MESSAGE);
      }
    }
    rpc(peerConfirmMultitaskGoal);
  }

  public void multitaskTerminate() throws EclipseException, IOException, IllegalStateException {
    synchronized(this) {
      if (multitaskPhase == null) {
        throw new IllegalStateException(STATE_EXCEPTION_MESSAGE);
      }
    }
    rpc(peerTerminateMultitaskGoal);
  }
}
