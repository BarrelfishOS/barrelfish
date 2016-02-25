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
//Version:      $Id: EmbeddedEclipse.java,v 1.2 2012/01/09 21:30:55 jschimpf Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Embedded ECLiPSe engine.
package com.parctechnologies.eclipse;
import java.io.*;
import java.util.*;

/**
 * An ECLiPSe engine embedded in the JVM process. Since there can only be one
 * embedded ECLiPSe engine at any one time, there is no public constructor and you
 * should use the {@link EmbeddedEclipse#getInstance(EclipseEngineOptions)}
 * method to create an embedded ECLiPSe engine if none exists.
 * The {@link EclipseEngineOptions}
 * object should be created, configured,
 * and then passed to this method to start the ECLiPSe with user-defined options.<p>
 * Once started, a reference to the <i>EmbeddedEclipse</i> instance can be
 * obtained using the {@link EmbeddedEclipse#getInstance()} method.
 * Note that
 * once the EmbeddedEclipse has been shut down by calling its <code>destroy()</code> method,
 * no more embedded ECLiPSe engines
 * can be started using {@link EmbeddedEclipse#getInstance(EclipseEngineOptions)}.
 *
 * Here is some example code for working with an EmbeddedEclipse:
 * <p><hr><blockquote><pre>
 * // create a new EclipseEngineOptions object
 * EclipseEngineOptions options = new EclipseEngineOptions(new File("C:\Eclipse\"));
 * // alter the useQueues option
 * options.setUseQueues(true);
 * // start up the embedded Eclipse. testEclipse is a reference to the
 * // Eclipse engine.
 * EclipseEngine testEclipse = EmbeddedEclipse.getInstance(options);
 * // Direct the Eclipse to load a source file.
 * testEclipse.compile(new File(".\myEclipseSourceFile.pl"));
 * // Execute a top-level goal in ECLiPSe.
 * testEclipse.rpc("go");
 * // Destroy the Eclipse
 * ((EmbeddedEclipse) testEclipse).destroy();
 * </pre></blockquote><hr>
 *
 *
 * @see EclipseConnection
 * @see EclipseEngine
 * @see EclipseEngineOptions
 *
 */
public class EmbeddedEclipse extends EclipseConnectionImpl
  implements EclipseConnection, EclipseEngine
{
  // Class Overview
  //
  // Roughly speaking, this class uses the Adapter pattern to adapt the
  // NativeEclipse class (the Java class which provides access to the C Eclipse
  // embedding code) to the EclipseEngine interface. The abstract methods inherited
  // from EclipseEngineImpl and EclipseConnectionImpl are implemented using calls
  // to NativeEclipse. It also uses the Singleton pattern: there
  // can only be one EmbeddedEclipse per JVM.

  /**
   * The singleton instance of EmbeddedEclipse.
   */
  private static EmbeddedEclipse single;

  // queue used to send requests to create/close queues.
  private FromEclipseQueue embed_info;
  // used to read exdr from the above;
  private EXDRInputStream embed_infoEXDRInput;

  /**
   * If the embedded ECLiPSe has not yet been started in this JVM, start the
   * embedded ECLiPSe using the options specified in the parameter
   * <i>EclipseEngineOptions</i>.
   *
   * @param options user-definable options used to configure the embedded
   * ECLiPSe engine.
   *
   * @throws EclipseException if the embedded ECLiPSe has already been started,
   * or if there was some ECLiPSe-related problem whilst
   * attempting to start ECLiPSe, for example if the ECLiPSe installation
   * directory was not set in the supplied EclipseEngineOptions object.
   * @throws EclipseTerminatedException if the embedded ECLiPSe has already been terminated
   *
   * @throws IOException if there is some other I/O-related problem starting the ECLiPSe engine.
   *
   */
  public synchronized static EmbeddedEclipse getInstance(EclipseEngineOptions options)
      throws EclipseException, IOException
  {
    // if an instance has already been created
    if(single != null)
    {
      // if it has been destroyed throw an exception
      single.testTerminated();
      // otherwise throw an EclipseException
      throw new EclipseException("The embedded ECLiPSe has already been started"
      + " in this JVM.");
    }
    else // if no instance has been created
    {
      // create a new one using the param options
      single = new EmbeddedEclipse(options);
      // return it
      return single;
    }
  }

  /**
   * Returns the unique <i>EmbeddedEclipse</i> object if the embedded ECLiPSe
   * has been started, but not yet terminated in this JVM.
   * @throws EclipseException if ECLiPSe has not been started.
   * @throws EclipseTerminatedException if the embedded ECLiPSe has been
   * terminated.
   */
  public synchronized static EmbeddedEclipse getInstance()
      throws EclipseException, IOException
  {
    // if an instance has already been created
    if(single != null)
    {
      // if it has been destroyed throw an exception
      single.testTerminated();
      // otherwise return it
      return single;
    }
    else // if no instance has been created
    {
      throw new EclipseException("The embedded ECLiPSe has not yet been started"+
      " in this JVM.");
    }
  }


  /**
   * Constructs an EmbeddedEclipse using some EclipseEngineOptions
   */
  private EmbeddedEclipse(EclipseEngineOptions options) throws EclipseException,
    IOException
  {
    // Process the set of options. This examines the options and
    // sets up the NativeEclipse accordingly.
    processOptions(options);

    // Initialise the NativeEclipse. Result 0 indicates successful
    // initialisation
    int res = NativeEclipse.init();

    if (res != 0)
            throw new EclipseException("Eclipse init = "+res);

    // Initialise toEclipse based on the pre-existing stream called
    // "ec_rpc_in"
    ToEclipseQueue toEclipseTEQ = createToEclipseQueue("ec_rpc_in");
    // underlying queue is a system queue (should not be closed when
    // Eclipse's closeAllQueues method is called).
    toEclipseTEQ.setSystemQueue(true);

    toEclipse = new EXDROutputStream(toEclipseTEQ);

    // Initialise fromEclipse based on the pre-existing stream called
    // "ec_rpc_out"
    FromEclipseQueue fromEclipseFEQ = createFromEclipseQueue("ec_rpc_out");
    // underlying queue is a system queue (should not be closed when
    // Eclipse's closeAllQueues method is called).
    fromEclipseFEQ.setSystemQueue(true);

    fromEclipse = new EXDRInputStream(fromEclipseFEQ);

    useQueues = options.isUsingQueues();

    // If options object dictates that we should use queues to represent
    // the Eclipse's standard streams
    if (options.isUsingQueues())
    {
      // Initialise them to do so (defined in EclipseEngineImpl).
      initialiseStandardStreamQueues();
    }

    // do an rpc to set up the peer name from the options.
    rpc(new CompoundTermImpl(":",new Atom("sepia_kernel"),
                                 new CompoundTermImpl("set_embed_peer",
                                     new Atom(options.getPeerName()),
                                     new Atom("java"))));


    // setup embed_info to be a fromEclipse queue and a systemQueue
    embed_info = this.getFromEclipseQueue("embed_info");
    embed_info.setSystemQueue(true);
    embed_infoEXDRInput = new EXDRInputStream(embed_info);
  }

  /**
   * Process the set of options. This examines the options and
   * sets up the NativeEclipse accordingly.
   */
  private void processOptions(EclipseEngineOptions options) throws EclipseException
  {
    int res;    // result of Native operation

    // set the peer name in this object from the peer name in the options object
    this.setPeerName(new Atom(options.getPeerName()));

    // If the user has not set the Eclipse directory, throw an appropriate
    // exception
    if(options.getEclipseDir() == null)
    {
      throw new EclipseException("The location of the ECLiPSe installation was not set in the EclipseEngineOptions object supplied.");
    }

    // load the Eclipse shared libraries, based on the location of the Eclipse
    // installation.
    loadEclipse(options.getEclipseDir());

    // set up the installation directory in NativeEclipse according to options
    res = NativeEclipse.setOption(StringOption.ECLIPSEDIR , options.getEclipseDir());
    if (res != 0)
      throw new EclipseException(
        "Setting eclipse installation directory to '" +
        options.getEclipseDir() +
        "' result = " + res);

    // if user has set it in options, set up the command line options in
    // NativeEclipse according to options
    if(options.getCommandLineOptions() != null)
    {
      res = NativeEclipse.setOption(IntOption.OPTION_ARGC, options.getCommandLineOptions().length);
      if (res != 0)
      {
        throw new EclipseException(
                    "Setting eclipse no. of command line arguments to '"+
                    options.getCommandLineOptions().length +
                    "' result = "+res);
      }
      res = NativeEclipse.setOption(StringArrayOption.OPTION_ARGV, options.getCommandLineOptions());
      if (res != 0)
      {
        throw new EclipseException(
                    "Setting eclipse command line arguments to '"+
                    options.getCommandLineOptions()+
                    "' result = "+res);
      }


    }

    // if user has set it in options, set up the default module in
    // NativeEclipse according to options
    if ( options.getDefaultModule() != null )
    {
      res = NativeEclipse.setOption(StringOption.DEFAULT_MODULE , options.getDefaultModule());
      if (res != 0)
        throw new EclipseException(
                    "Setting eclipse default module to '"+ options.getDefaultModule() +
                    "' result = "+res);
    }

    // if user has set it in options, set up the local stack size in
    // NativeEclipse according to options
    if ( options.getLocalSize() != 0 )
    {
      res = NativeEclipse.setOption(IntOption.LOCALSIZE , options.getLocalSize() * options.MEGABYTE);
      if (res != 0)
        throw new EclipseException(
        "Setting eclipse local stack size to "+ options.getLocalSize() +
                    "M result = "+res);
    }

    // if user has set it in options, set up the global stack size in
    // NativeEclipse according to options
    if ( options.getGlobalSize() != 0 )
    {
      res = NativeEclipse.setOption(IntOption.GLOBALSIZE , options.getGlobalSize() * options.MEGABYTE);
      if (res != 0)
        throw new EclipseException(
                    "Setting eclipse global stack size to "+ options.getGlobalSize() + "M result = "+res);
    }

    // tell NativeEclipse whether or not we are using queues for standard streams
    NativeEclipse.setOption(IntOption.IO , options.isUsingQueues()? 2 : 0);

    // make NativeEclipse use its own cwd rather than the process's cwd
    // in order to be more consistent with the out-of-process situation
    // [not yet enabled for backward compatibility]
    // NativeEclipse.setOption(IntOption.CWD_SEPARATE , 1);
  }


  /**
   * load the Eclipse shared libraries, based on the location of the Eclipse
   * installation. Throws an exception if the platform is not supported.
   */
  private static void loadEclipse(String eclipseDir)
  {
    // What to do here varies according to platform
    if(!Platform.getInstance().supportsEmbeddedEclipse())
    {
      throw
        new UnsupportedOperationException("The creation of an EmbeddedEclipse"+
        " is not supported on platform "+ System.getProperty("os.name")+
        "/"+ System.getProperty("os.arch") + ".");
    }
    else
    {
      Platform.getInstance().loadEclipseSharedLibrary(new File(eclipseDir));
    }
  }

  /**
   * Codes to indicate what happened while eclipse ran.
   */
  private interface resume_result
  {
    static final int PSUCCEED =	0;	// success
    static final int PFAIL =		1;	// failure
    static final int PTHROW =	2;	// exit_block, ball in A1
    static final int PYIELD =	4;	// Eclipse engine suspended
    static final int PRUNNING =	5;	// Eclipse engine running
    static final int PWAITIO =	6;	// waiting for queue input
    static final int PFLUSHIO =	7;	// request queue output
  }

  /**
   * The String typed options
   */
  private interface StringOption
  {
    static final int DEFAULT_MODULE =  10;
    static final int ECLIPSEDIR =      11;
  }

  /**
   * The StringArray typed options
   */
  private interface StringArrayOption
  {
      static final int OPTION_ARGV = 3;
  }

  /**
   * The int typed options
   */
  private interface IntOption
  {
    static final int LOCALSIZE =   4;
    static final int GLOBALSIZE =  5;
    static final int PRIVATESIZE = 6;
    static final int SHAREDSIZE =  7;
    static final int IO =          12;
    static final int OPTION_ARGC = 2;
    static final int CWD_SEPARATE = 15;
  }

 /**
   * Read <code>len</code> bytes from this ECLiPSe's input queue stream number
   * <code>streamid</code> at offset <code>off</code> and store them in
   * byte array <code>b</code>.
   *
   * @returns the number of bytes read.
   */
  synchronized int readFromStream(int streamid, int off, int len, byte[] b)
    throws IOException
  {
    int res = NativeEclipse.QueueRead(streamid, off, len, b);
    // if res is negative, this is an error code, so throw an exception.
    if(res < 0)
    {
      throw new IOException("NativeEclipse.QueueRead exited with error code "+res);
    }
    return(res);
  }

  /**
   * Read a single byte from this ECLiPSe's stream number
   * <code>streamid</code>
   *
   * @returns byte read, an int between 0 and 255 or -1 if zero bytes were read.
   */
  synchronized int readByteFromStream(int streamid) throws IOException
  {
    int res = NativeEclipse.QueueReadByte(streamid);
    if(res == -1)
    {
      return(-1);
    }
    if(res < -1)
    {
      throw new IOException("NativeEclipse.QueueReadByte exited with error code "+res);
    }
    return(res);
  }


  /**
   * Returns the number of bytes available on stream streamid which may be
   * read or skipped over without blocking.
   */
  synchronized int availableOnStream(int streamid) throws IOException
  {
    int res = NativeEclipse.QueueAvailable(streamid);
    // if res is negative, this is an error code, so throw an exception.
    if(res < 0)
    {
      throw new IOException("NativeEclipse.QueueAvailable exited with error code "+res);
    }
    return(res);
  }

  /**
   * Write <code>len</code> bytes to this ECLiPSe's output queue stream number
   * <code>streamid</code> at offset <code>off</code> from
   * byte array <code>b</code>.
   *
   * @returns the number of bytes written.
   */
  synchronized int writeToStream(int streamid, byte[] b, int off, int len)
    throws IOException
  {
    int res = NativeEclipse.QueueWrite(streamid, b, off, len);
    // if res is negative, this is an error code, so throw an exception.
    if(res < 0)
    {
      throw new IOException("NativeEclipse.QueueWrite exited with error code "+res);
    }
    return(res);
  }

  /**
   * Write a single byte to this ECLiPSe's stream number
   * <code>streamid</code>.
   *
   */
  synchronized void writeByteToStream(int streamid, byte b)
    throws IOException
  {
    int res = NativeEclipse.QueueWriteByte(streamid, b);
    // if res is negative, this is an error code, so throw an exception.
    if(res < 0)
    {
      throw new IOException("NativeEclipse.QueueWriteByte exited with error code "+res);
    }
    return;
  }


  ControlSignal getNextControlSignal(boolean isFirstIteration,
                                     boolean transferControlWithResume)
    throws IOException
  {
    int res;
    // in the cases where the resulting event code is a queue operation,
    // during HandleEvents, stream gets set by Eclipse to the number of the
    // relavent stream
    Integer stream = new Integer(0);

    if(isFirstIteration)
    {
      // resume eclipse to handle events only (and not execute posted goals)
      // and set res to the resulting event code
      res = NativeEclipse.HandleEvents(stream);
    }
    else
    {
      // resume eclipse again, this time executing posted goals, as well as
      // handling events.
      res = NativeEclipse.resumeLong(stream);
    }

    if(res == resume_result.PWAITIO)
    {
      return(new WaitIOSignal(stream));
    }
    if(res == resume_result.PFLUSHIO &&
       lookupFromEclipseQueue(stream.intValue()) == embed_info)
    {
      CompoundTerm nextTerm =
      (CompoundTerm) embed_infoEXDRInput.readTerm();
      if(nextTerm.functor().equals("queue_connect") &&
         nextTerm.arity() == 3 &&
         nextTerm.arg(1) instanceof Atom &&
         nextTerm.arg(2) instanceof Integer &&
         nextTerm.arg(3) instanceof Atom)
      {
        Atom nameAtom = (Atom) nextTerm.arg(1);
        stream = (Integer) nextTerm.arg(2);
        Atom directionAtom = (Atom) nextTerm.arg(3);
        return(new OpenQueueSignal(nameAtom, stream, directionAtom));
      }
      if(nextTerm.functor().equals("queue_close") &&
         nextTerm.arity() == 1 &&
         nextTerm.arg(1) instanceof Integer)
      {
        stream = (Integer) nextTerm.arg(1);
        return(new CloseQueueSignal(stream));
      }
    }
    if(res == resume_result.PFLUSHIO &&
       lookupFromEclipseQueue(stream.intValue()) != embed_info)
      {
        return(new FlushIOSignal(stream));
      }
    if(res == resume_result.PSUCCEED)
      {
        return(new YieldSignal());
      }
    if(res == resume_result.PRUNNING)
      {
        return(new RunningSignal());
      }
    return(null); // Default: null signifies no signal was recognised
  }


  /**
   * This method is invoked as part of the routine to establish a FromEclipseQueue
   * between Eclipse and Java. It sets up the Eclipse side of the queue.
   */
  void setupFromEclipseQueue(String name)
    throws IOException, EclipseException
  {
    rpc(":",
        new Atom("sepia_kernel"),
        new CompoundTermImpl("ecl_create_embed_queue", new Atom(name),
                             new Atom("fromec"), new Atom(""))
        );
  }


  /**
   * This method is invoked as part of the routine to establish a ToEclipseQueue
   * between Eclipse and Java. It sets up the Eclipse side of the queue.
   */
  void setupToEclipseQueue(String name)
    throws IOException, EclipseException
  {
    rpc(":",
        new Atom("sepia_kernel"),
        new CompoundTermImpl("ecl_create_embed_queue", new Atom(name),
                             new Atom("toec"), new Atom(""))
        );
  }

  // Implementation of public method from EclipseConnection interface
  public synchronized AsyncEclipseQueue getAsyncEclipseQueue(String name)
    throws EclipseException, IOException
  {
    throw new IOException("Asynchronous queues not implemented for this connection type");
  }

  void setupAsyncEclipseQueue(String name)
    throws EclipseException, IOException
  {
    throw new IOException("Asynchronous queues not implemented for this connection type");
  }

  // overrides common implementation in EclipseConnectionImpl: using class
  // NativeEclipse is more efficient, and also necessary since we need to
  // get a stream number in order to set up toEclipse (the rpc queue).
  // We can't use rpc until this is done. The common implementation uses rpc
  // so it wouldn't work.

  // returns negative if there is no open stream with name streamName
  int getStreamNumber(String streamName)
  {
    return(NativeEclipse.StreamNumber(streamName));
  }


  // implements flushStream(id) by doing nothing (memory queues do not need to
  // be flushed.
  synchronized void flushStream(int id)
  {

  }

  /**
   * Send an RPC goal to ECLiPSe.
   */
  void sendGoal(Object goal) throws IOException
  {
    toEclipse.write(goal);
    toEclipse.flush();
  }

  /**
   * Receive an RPC goal from ECLiPSe.
   */
  Object receiveGoal() throws IOException
  {
    return(fromEclipse.readTerm());
  }

  /**
   * Finalizer method called when object is to be garbage collected
   */
  protected void finalize() throws IOException
  {
    this.destroy();
  }

  void closeFromEclipseStreamEclipseSide(int streamid) throws IOException
  {
    super.closeFromEclipseStreamEclipseSide(streamid);
    try
    {
      rpc(":", new Atom("sepia_kernel"),
               new CompoundTermImpl("close_embed_queue_eclipseside",
                                    this.getPeerName(),
                                    new Integer(streamid))
          );
    }
    catch(EclipseException e)
    {
      throw new IOException("Problem closing ECLiPSe stream "+streamid+
                            ": "+e.toString());
    }
  }

  void closeToEclipseStreamEclipseSide(int streamid) throws IOException
  {
    super.closeToEclipseStreamEclipseSide(streamid);
    try
    {
      rpc(":", new Atom("sepia_kernel"),
               new CompoundTermImpl("close_embed_queue_eclipseside",
                                    this.getPeerName(),
                                    new Integer(streamid))
          );
    }
    catch(EclipseException e)
    {
      throw new IOException("Problem closing ECLiPSe stream "+streamid+
                            ": "+e.toString());
    }
  }


  void respondRunning() throws IOException
  {
    throw new IOException("NativeEclipse.HandleEvents reports that"+
            " an asynchronous ECLiPSe thread is already running: cannot handle"+
            " events in this case.");
  }


  void respondWaitIO(Integer streamID) throws IOException
  {
    // look up the queue in the toEclipseQueue register
    ToEclipseQueue teq = lookupToEclipseQueue(streamID.intValue());
    // if it is not there, print a message to stderr
    if (teq == null) {
      System.err.println("ECLiPSe yielded after flushing stream "+streamID.intValue() +
        " which is not registered as a ToEclipseQueue.");
    } else {
      // otherwise notify the queue's listener of a request for data
      // (causes its dataRequest method to be invoked)
      teq.notifyRequest();
    }
  }

  void respondFlushIO(Integer streamID) throws IOException
  {
    // look up the queue in the fromEclipseQueue register
    FromEclipseQueue feq = lookupFromEclipseQueue(streamID.intValue());
    // if it is not there, print a message to stderr
    if (feq == null) {
      System.err.println("ECLiPSe yielded after reading empty stream "+streamID.intValue() +
                         " which is not registered as a FromEclipseQueue.");
    } else {
      // otherwise notify the queue's listener of the flushed data
      // (causes its dataAvailable method to be invoked)
      feq.notifyAvailable();
    }
  }


  class RunningSignal extends ControlSignal
  {
    void respond() throws IOException
    {
      respondRunning();
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

  class FlushIOSignal extends ControlSignal
  {
    private Integer streamID;
    FlushIOSignal(Integer streamID)
    {
      this.streamID = streamID;
    }
    void respond() throws IOException
    {
      respondFlushIO(streamID);
    }
  }

    /**
   * Queue representing the EclipseEngine's stdin stream.
   */
  ToEclipseQueue stdin;

  /**
   * Queue representing the EclipseEngine's stdout stream.
   */
  FromEclipseQueue stdout;

  /**
   * Queue representing the EclipseEngine's stderr stream.
   */
  FromEclipseQueue stderr;

  /**
   * Terminate access to the <i>EmbeddedEclipse</i>. This closes the embedded
   * ECLiPSe engine and frees all system resources associated with it. After
   * <code>destroy()</code> has been invoked, use of this EmbeddedEclipse's
   * methods will throw <i>EclipseTerminatedException</i>s. Also, once <code>
   * destroy()</code> has been invoked, no more <i>EmbeddedEclipse</i> instances
   * can be created during the lifetime of this Java virtual machine.
   *
   * @throws EclipseTerminatedException if the ECLiPSe engine has already been
   * terminated.
   * @throws IOException if there was a problem communicating with ECLiPSe
   * during termination.
   */
  public synchronized void destroy() throws IOException
  {
    testTerminated();
    // close all user queues, including the eclipse sides
    this.closeAllQueues(true);
    this.terminated = true;

    NativeEclipse.Cleanup();

  }

  // Implementation of public method from EclipseEngine
  public synchronized ToEclipseQueue getEclipseStdin() throws EclipseTerminatedException
  {
    testTerminated();
    return(stdin);
  }

  // Implementation of public method from EclipseEngine
  public synchronized FromEclipseQueue getEclipseStdout() throws EclipseTerminatedException
  {
    testTerminated();
    return(stdout);
  }

  // Implementation of public method from EclipseEngine
  public synchronized FromEclipseQueue getEclipseStderr() throws EclipseTerminatedException
  {
    testTerminated();
    return(stderr);
  }

  // Initialise the standard streams. Assumes that the Eclipse side of these
  // queues has already been set up. Should be called by concrete subclasses
  // at the appropriate point during object initialisation. All these queues
  // are system queues (they should not be closed on the Eclipse side when
  // the Eclipse's closeAllQueues method is called).
  void initialiseStandardStreamQueues() throws EclipseException, IOException
  {
    stdin = createToEclipseQueue("input");
    stdin.setSystemQueue(true);
    stdout = createFromEclipseQueue("output");
    stdout.setSystemQueue(true);
    stderr = createFromEclipseQueue("error");
    stderr.setSystemQueue(true);
  }

  private boolean useQueues;

  public synchronized boolean isUsingQueues()
  {
    return useQueues;
  }

}
