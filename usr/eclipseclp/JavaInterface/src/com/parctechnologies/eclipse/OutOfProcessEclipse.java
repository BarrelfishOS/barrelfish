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
//Version:      $Id: OutOfProcessEclipse.java,v 1.5 2013/02/10 18:54:45 jschimpf Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  ECLiPSe engine run on local machine, separate process
package com.parctechnologies.eclipse;
import java.io.*;
import java.net.*;

/**
 * An ECLiPSe engine which runs in a child process of the Java virtual machine.
 * An <i>OutOfProcessEclipse</i> is created using the public constructor, which
 * takes an <i>EclipseEngineOptions</i> object. A JVM may have any number of
 * instances of this class. Invocation of the <code>destroy()</code> method does
 * not affect the ability to create new <i>OutOfProcessEclipse</i> instances.
 */
public class OutOfProcessEclipse implements EclipseConnection, EclipseEngine
{
  private Process eclipseProcess;

  private RemoteEclipse eclipseConnection;

  private BufferedReader eclipseProcessStdout;

  private BufferedReader eclipseProcessStderr;

  private BufferedWriter eclipseProcessStdin;

  private boolean useQueues;

  private ToEclipseQueue stdin;
  private FromEclipseQueue stdout, stderr;

  public synchronized boolean isUsingQueues()
  {
    return useQueues;
  }

  /**
   * Create a new OutOfProcessEclipse using the supplied options.
   *
   * @throws IOException if the connection to the child process failed
   * @throws EclipseException if there was a problem setting up the ECLiPSe side
   * of the connection.
   *
   * @param options settings for the new ECLiPSe engine.
   */
  public OutOfProcessEclipse(EclipseEngineOptions options)
    throws IOException, EclipseException
  {
    startLocalEclipse(options);

    // Read the port number from the eclipseProcess' stdout
    String portString = eclipseProcessStdout.readLine();

    // Read the password from the eclipseProcess' stdout
    String passwd = eclipseProcessStdout.readLine();

    // make port string into an int
    int port = Integer.parseInt(portString);

    // connect
    remoteConnect(InetAddress.getByName("localhost"), port, passwd);

    // set the useQueues flag according to options
    useQueues = options.isUsingQueues();

    // System.out.println("initial attachment complete");

    // connect the local Eclipse's standard streams to queues,
    // and redirect these to the System standard streams if
    // necessary.
    connectStdStreams();

    // if not using standard stream queues, need to add listeners to them to
    // link up with JVM's standard streams
    if(!options.isUsingQueues())
    {
      stdStreamsAddListeners();
    }


  }

  private void connectStdStreams() throws EclipseException, IOException
  {
    // For each standard stream, set up a queue
    stdout = eclipseConnection.getFromEclipseQueue("joop_stdout");
    stderr = eclipseConnection.getFromEclipseQueue("joop_stderr");
    stdin = eclipseConnection.getToEclipseQueue("joop_stdin");

    // make sure these get flushed eagerly, so users see error messages
    eclipseConnection.rpc("set_stream_property(joop_stdout,flush,end_of_line)");
    eclipseConnection.rpc("set_stream_property(joop_stderr,flush,end_of_line)");

    // now redirect Eclipse streams to write to / read from these queues
    // (NOTE: setting user_XXX before stdXXX is the only way to change stdXXX!)
    eclipseConnection.rpc("set_stream(user_input, joop_stdin)");
    eclipseConnection.rpc("set_stream(stdin, joop_stdin)");
    eclipseConnection.rpc("set_stream(user_output, joop_stdout)");
    eclipseConnection.rpc("set_stream(stdout, joop_stdout)");
    eclipseConnection.rpc("set_stream(user_error, joop_stderr)");
    eclipseConnection.rpc("set_stream(stderr, joop_stderr)");
    // ... and all the aliases of these basic streams as well
    eclipseConnection.rpc("set_stream(output, joop_stdout)");
    eclipseConnection.rpc("set_stream(warning_output, joop_stdout)");
    eclipseConnection.rpc("set_stream(log_output, joop_stdout)");
    eclipseConnection.rpc("set_stream(error, joop_stderr)");
    eclipseConnection.rpc("set_stream(input, joop_stdin)");
  }

  private void stdStreamsAddListeners()
  {
    try
      {stdin.setListener(new StdInListener());}
    catch(IOException ioe) // should never be thrown as queue is not closed
      {System.err.println("Error: setting of listener threw an IOException.");}
    try
      {stdout.setListener(new StdOutListener("output"));}
    catch(IOException ioe) // should never be thrown as queue is not closed
      {System.err.println("Error: setting of listener threw an IOException.");}
    try
      {stderr.setListener(new StdOutListener("error"));} // no special listener for stderr
    catch(IOException ioe) // should never be thrown as queue is not closed
      {System.err.println("Error: setting of listener threw an IOException.");}
  }

  private void remoteConnect(InetAddress hostAddr, int port,
                             String passwd) throws IOException, EclipseException
  {

    //System.out.println("Trying to make connection on host "+hostString+", port "
    //                   +port+" password "+passwd);



    // try to set up the connection on the specified port
    try
    {
      eclipseConnection =
        new RemoteEclipse(hostAddr, port, passwd);


      //System.out.println("Connection successful.");

      eclipseProcessStdin.write("accept. \n");
      eclipseProcessStdin.flush();

      //System.out.println("Accept signal written and flushed.");

      // signal to the eclipse to resume execution + wait until it relinquishes
      // control
      eclipseConnection.resume();
    }
    catch(IOException ioe)
    {
      // if unsuccessful, signal to the eclipse to shut down before re-throwing
      // exception
      //System.out.println("Connection unsuccessful."+ioe);

      try
      {
        eclipseProcessStdin.write("reject. \n");
        eclipseProcessStdin.flush();
      }
      catch(Exception e){}
      //System.out.println("Reject signal written and flushed.");

      throw ioe;
    }


  }


  private void startLocalEclipse(EclipseEngineOptions options)
    throws EclipseException, IOException
  {
    // What to do here varies according to platform
    if(!Platform.getInstance().supportsOutOfProcessEclipse())
    {
      throw
        new UnsupportedOperationException("The creation of an OutOfProcessEclipse"+
        " is not supported on platform "+ System.getProperty("os.name")+
        "/"+ System.getProperty("os.arch") + ".");
    }

    // The command to start eclipse : varies according to platform + options
    String[] cmdarray = new String[100];

    // index of next empty string in command array
    int cmd = 0;

    if(options.getEclipseDir() == null)
    {
      throw new EclipseException("The location of the ECLiPSe installation was not set in the EclipseEngineOptions object supplied.");
    }
    File eclipseDir = new File(options.getEclipseDir());
    File executableSubdir =
      Platform.getInstance().getExecutableSubdirectory(eclipseDir);
    File librarySubdir =
      Platform.getInstance().getLibrarySubdirectory(eclipseDir);

    if (!executableSubdir.equals(librarySubdir) && executableSubdir.exists())
    {
	// Eclipse has been fully installed, and an 'eclipse' script exists:
	// run the script because it sets some potentially useful environment
	// variables (e.g. LD_LIBRARY_PATH, JRE_HOME, TCL_LIBRARY)
	cmdarray[cmd++] = (new File(executableSubdir, "eclipse")).toString();
    }
    else
    {
	// No bin-directory, i.e. we have a non-installed Unix-Eclipse, or we
	// are on Windows (possibly without registry entries).  We try to run
	// eclipse.exe, supplying the installation directory via the command
	// line.  This should work for many applications, provided things like
	// LD_LIBRARY_PATH are taken care of by the host application.
	cmdarray[cmd++] = (new File(librarySubdir, "eclipse.exe")).toString();

	cmdarray[cmd++] = "-D";
	cmdarray[cmd++] = eclipseDir.toString();
    }

    // if user has set it in options, add a command line option to set up the
    // local stack size
    if(options.getLocalSize() != 0)
    {
      cmdarray[cmd++] = "-l";
      cmdarray[cmd++] = (new Integer(options.getLocalSize()).toString())+"M";
    }

    // if user has set it in options, add a command line option to set up the
    // global stack size
    if(options.getGlobalSize() != 0)
    {
      cmdarray[cmd++] = "-g";
      cmdarray[cmd++] = (new Integer(options.getGlobalSize()).toString())+"M";
    }

    cmdarray[cmd++] = "-e";
    // if user has set default module in options, add goal to do
    // this, and then start the joop_boot
    if (options.getDefaultModule() != null )
    {
      cmdarray[cmd++] = "set_flag(toplevel_module, "+
        options.getDefaultModule()+"), joop_boot:jb_go";
    }
    else // Otherwise just add a goal to start the joop_boot
    {
      cmdarray[cmd++] = "joop_boot:jb_go";
    }

    // Add bootfile. This is the ECLiPSe code which will set up the remote
    // connection etc. It is initialised using the "jb_go" predicate.
    cmdarray[cmd++] = "-b";

    cmdarray[cmd++] = (new File(new File(eclipseDir, "lib"), "joop_boot.eco")).toString();

    // copy to new array which is just big enough
    String[] cmdarray2 = new String[cmd];
    System.arraycopy(cmdarray, 0, cmdarray2, 0, cmd);

    eclipseProcess = Runtime.getRuntime().exec(cmdarray2);

    eclipseProcessStdout =
      new BufferedReader(new InputStreamReader(eclipseProcess.getInputStream()));

    eclipseProcessStderr =
      new BufferedReader(new InputStreamReader(eclipseProcess.getErrorStream()));

    eclipseProcessStdin =
      new BufferedWriter(new OutputStreamWriter(eclipseProcess.getOutputStream()));

    // write the peer name specified in options on stdin
    eclipseProcessStdin.write(options.getPeerName()+". \n");
    eclipseProcessStdin.flush();
  }


  /**
   * Finalizer method called when object is to be garbage collected
   */
  protected void finalize() throws IOException, EclipseException
  {
    this.destroy();
  }


  public synchronized ToEclipseQueue getEclipseStdin() throws EclipseTerminatedException
  {
    eclipseConnection.testTerminated();
    if(useQueues)
      return(stdin);
    else
      return(null);
  }

  public synchronized FromEclipseQueue getEclipseStdout() throws EclipseTerminatedException
  {
    eclipseConnection.testTerminated();
    if(useQueues)
      return(stdout);
    else
      return(null);
  }

  public synchronized FromEclipseQueue getEclipseStderr() throws EclipseTerminatedException
  {
    eclipseConnection.testTerminated();
    if(useQueues)
      return(stderr);
    else
      return(null);
  }

  /**
   * Terminate the <i>OutOfProcessEclipse</i> process and the connection to it.
   * After <code>destroy()</code> has been invoked, future invocations of public
   * methods will throw <i>EclipseTerminatedException</i>s
   *
   * @throws EclipseTerminatedException if the <code>destroy()</code> method had
   * already been called.
   */
  public void destroy() throws IOException
  {
    eclipseConnection.testTerminated();
    try // multilateral disconnect
    {
      eclipseConnection.disconnect();
    }
    catch(Exception e)
    { // if that doesnt work, try unilateral
      try
      {
        eclipseConnection.unilateralDisconnect();
      }
      catch(EclipseTerminatedException ete)
      {}
    }
  }

  public CompoundTerm rpc(String goal) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(goal));
  }

  public CompoundTerm rpc(CompoundTerm goal) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(goal));
  }

  public FromEclipseQueue getFromEclipseQueue(String name) throws EclipseException, IOException
  {
    return(eclipseConnection.getFromEclipseQueue(name));
  }

  public ToEclipseQueue getToEclipseQueue(String name) throws EclipseException, IOException
  {
    return(eclipseConnection.getToEclipseQueue(name));
  }

  public AsyncEclipseQueue getAsyncEclipseQueue(String name) throws EclipseException, IOException
  {
    return(eclipseConnection.getAsyncEclipseQueue(name));
  }

  public void compile(File f) throws EclipseException, IOException
  {
    eclipseConnection.compile(f);
  }

  public String getPath(File f) throws EclipseException, IOException
  {
    return(eclipseConnection.getPath(f));
  }

  public CompoundTerm rpc(String functor, Object arg1) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, arg1));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, arg1, arg2));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, arg1, arg2, arg3));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4)
                          throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, arg1, arg2, arg3, arg4));
  }

  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4,
                          Object arg5) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, arg1, arg2, arg3, arg4, arg5));
  }

  public CompoundTerm rpc(String functor, Object[] args) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(functor, args));
  }

  public CompoundTerm rpc(Object[] goalTerm) throws EclipseException, IOException
  {
    return(eclipseConnection.rpc(goalTerm));
  }

  public Atom getPeerName()
  {
    return(eclipseConnection.getPeerName());
  }

  private class StdInListener implements QueueListener
  {
    // size of byte chunk we attempt to read from stdin when there is a request
    private static final int READ_CHUNK_SIZE = 512;

    public void dataAvailable(Object source)
    {
    }

    public void dataRequest(Object source)
    {
      ToEclipseQueue teq = (ToEclipseQueue) source;

      byte[] chunk = new byte[READ_CHUNK_SIZE];
      int readResult ;

      try
      {
        readResult = System.in.read(chunk);
        if(readResult == -1) // EOF
        {
           // do nothing?
        }
        else // write the part of chunk to queue
        {
          teq.write(chunk, 0, readResult);
          teq.flush();
        }
      }
      catch(IOException ioe)
      {
        // write these out to stderr
        System.err.println("Attempt to read from JVM's stdin produced exception:\n"+ioe);
        System.err.flush();
      }
    }
  }

  private class StdOutListener implements QueueListener
  {
    private String destination;

    public StdOutListener(String destination)
    {
      this.destination = destination;
    }
    public void dataAvailable(Object source)
    {
      FromEclipseQueue feq = (FromEclipseQueue) source;
      byte[] chunk;

      try
      {
        int availableBytes = feq.available();
        chunk = new byte[availableBytes];
        feq.read(chunk);
      }
      catch(IOException ioe)
      {
        // write these out to stderr
        System.err.println("Attempt to use ECLiPSe's output queue produced exception:\n"
                            +ioe);
        System.err.flush();
        return;
      }
      if(destination.equals("output"))
      {
        System.out.print(new String(chunk));
        return;
      }
      if(destination.equals("error"))
      {
        System.err.print(new String(chunk));
        return;
      }

    }

    public void dataRequest(Object source)
    {
    }
  }

  // implements method from EclipseConnection
  public EclipseMultitaskConnection registerMultitask(MultitaskListener multitaskListener) throws EclipseException,IOException {
    return eclipseConnection.registerMultitask(multitaskListener);
  }
}
