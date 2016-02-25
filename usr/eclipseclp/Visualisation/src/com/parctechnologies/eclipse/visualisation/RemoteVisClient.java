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
// Contributor(s): 
// 
// END LICENSE BLOCK

//Title:        Eclipse Visualisation library
//Version:      $Id: RemoteVisClient.java,v 1.1 2006/09/23 01:56:34 snovello Exp $
//Copyright:    Copyright (c) 2001
//Author:       Josh Singer
//Company:      Parc Technologies Ltd.
//Description:  Visualisation class which attaches to a primed remote eclipse
package com.parctechnologies.eclipse.visualisation;

import com.parctechnologies.eclipse.*;
import java.net.*;
import java.io.*;

/**
 * Class with command line main method. This allows connection to an Eclipse
 * process remotely, and then sets up a VisClient based on this connection. The
 * Eclipse has to have been primed using e.g. remote_connect/3. Also has
 * specialised exitError and exitNormal methods for exiting: they have to also
 * disconnect the EclipseConnection.
 */
public class RemoteVisClient extends VisClient
{
  private static final int MIN_CL_ARGS = 2;
  private RemoteEclipse eclipse;

  public RemoteVisClient(RemoteEclipse eclipse) throws IOException, EclipseException
  {
    super(eclipse.registerMultitask(null));
    this.eclipse = eclipse;
    // Write client name out to stdout so that it can be read and returned by
    // whatever called the command line.
    EXDROutputStream exdr_out = new EXDROutputStream(System.out);
    try
    {
      exdr_out.write(clientName);
      exdr_out.flush();
    }    
    catch(IOException ioe){super.recover_ioe(ioe);}
  }

  // main method takes address and port as args, connects to a remote
  // eclipse at that address/port and constructs/attaches an instance
  // to it.
  public static void main(String[] args)
  {
    checkargs(args);
    RemoteEclipse eclipse = null;
    try
    {
      eclipse = new RemoteEclipse(InetAddress.getByName(args[0]),
                                  Integer.parseInt(args[1]));
    }
    catch(IOException ioe)
    {
      System.err.println("Unable to connect to ECLiPSe: "+ioe+" raised.");
      System.exit(1);
    }
    try {
      RemoteVisClient remoteVisClient1 = new RemoteVisClient(eclipse);
      try {
        eclipse.resume();
      } catch(IOException ioe) {
        remoteVisClient1.recover_ioe(ioe);
      }
    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private static void usage_message()
  {
    System.err.println("Usage:");
    System.err.println("        java RemoteVisClient <host> <port>");
  }

  private static void checkargs(String[] args)
  {
    InetAddress host = null;
    int port = 0;

    if(args.length < MIN_CL_ARGS)
    {
      usage_message();
      System.exit(1);
    }

    try
    {
      host = InetAddress.getByName(args[0]);
    }
    catch(UnknownHostException uhe)
    {
      System.err.println("Can't reach host "+args[0]+".");
      System.exit(1);
    }

    try
    {
      port = Integer.parseInt(args[1]);
      if(port <= 0)
      {
        System.err.println("Invalid port number "+args[1]+".");
        System.exit(1);
      }
    }
    catch(NumberFormatException nfe)
    {
      System.err.println("Invalid port number "+args[1]+".");
      System.exit(1);
    }

  }

  /**
   * Tries to disconnect if possible, then exits JVM with error exit code.
   */
  protected void exitError()
  {
    super.exitError();
    try
    {
      disconnect();
    }
    catch(Exception e){}
    System.exit(1);
  }

  /**
   * Tries to disconnect if possible, and reports an error if this does not work
   * then exits the JVM normal exit code.
   */
  protected void exitNormal()
  {
    super.exitNormal();
    try
    {
      disconnect();
    }
    catch(IOException ioe){recover_ioe(ioe);}
    System.exit(0);
  }

  /**
   * A unilateralDisconnect must be made if Eclipse has control because it will
   * not be listening for a disconnect message. Otherwise, we can do a cleaner
   * disconnect.
   */
  void disconnect() throws IOException
  {
    if(stateModel.getEclipseHasControl())
    {
      eclipse.unilateralDisconnect();
    }
    else
    {
      eclipse.disconnect();
    }
  }
}
