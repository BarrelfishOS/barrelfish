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

//Title:        JEclipse.java
//Version:      $Id: JEclipse.java,v 1.1 2006/09/23 01:54:10 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java-embedded top-level Eclipse
package com.parctechnologies.eclipse;
import java.io.*;

/**
 * Class with main method which, when called appropriately from the command
 * line, behaves as similarly as possible to a top-level eclipse. The underlying
 * ECLiPSe engine is embedded in the Java VM.
 *
 * Given the $ECLIPSEDIR environment variable is set up to be the path of
 * the ECLiPSe installation, the program should be run as follows:
 *
 * % java -Declipse.directory=$ECLIPSEDIR -classpath \
 *  $ECLIPSEDIR/JavaInterface/classes/:$ECLIPSEDIR/JavaInterface/jeclipse/ \
 *  JEclipse [options]
 *
 * [options] can be any of the usual ECLiPSe command-line options. -g, and -l
 * are interpreted by the Java program. Kilobyte settings are rounded up to
 * the nearest megabyte. -h is ignored. The other documented options, -e and
 * -b, and options following -- are processed as usual by the underlying
 * ECLiPSe engine.
 *
 * Note that the library path must also be set up correctly. Refer to the
 * chapter of the Embedding and Interfacing Manual entitled "Using the Java-
 * ECLiPSe Interface" for details.
 *
 */
public class JEclipse
{

  public static void main(String[] args) throws Exception
  {
    JEclipse jeclipse = new JEclipse();

    jeclipse.start(args);
  }

  protected void start(String[] args) throws Exception
  {
    // underlying eclipse engine
    EmbeddedEclipse eclipse;

    // options object
    EclipseEngineOptions options = null;

    // array of arguments that will actually be passed to eclipse
    String[] clargs;

    // try to create the options object. If an exception is thrown because
    // no eclipse.directory system property was set, print out an error
    // message and exit with failure.
    try
    {
      options = createOptions();
    }
    catch(NoEclipseDirectoryException e)
    {
      System.err.println("Path of ECLiPSe installation must be specified using");
      System.err.println("system property \"eclipse.directory\", e.g.");
      System.err.println("UNIX: % java -Declipse.directory=/usr/local/eclipse JEclipse");
      System.err.println("Windows: C:\\> java -Declipse.directory=C:\\Eclipse JEclipse");
      System.exit(1);

    }

    // scan the command line arguments for the options -g, -l and -s.
    try
    {
      setMemoryOptionsFromCommandLine(options, args);
    }
    // if any of these has an illegal setting, print out an error message and
    // exit with failure.
    catch(BadMemorySettingException e)
    {
      System.err.println("Memory settings must be of the form:");
      System.err.println("          -g <memory size> (global stack)");
      System.err.println("          -l <memory size> (local stack)");
      System.err.println("  where <memory size> is <integer> (kilobytes) or <integer>M (megabytes). ");
      System.err.println("  Kilobyte settings will be rounded up to the nearest megabyte.");
      System.exit(1);
    }

    // set up clargs to be a string array with one more element than args. This
    // is because eclipse expects the first arg to be the program name.
    clargs = new String[args.length + 1];
    clargs[0] = "Embedded ECLiPSE"; // Hardwired to be the same as what is
                                    // set by C.
    System.arraycopy(args, 0, clargs, 1, args.length);

    options.setCommandLineOptions(clargs);

    // Initialise eclipse to be an EmbeddedEclipse
    eclipse = EmbeddedEclipse.getInstance(options);

    try
    {
	run_toplevel(eclipse);
    }
    catch(Fail f)
    {
	System.exit(1);
    }
    catch(Throw t)
    {
	System.exit(2);
    }
    finally
    {
    // shutdown
    eclipse.destroy();
    System.exit(0);
    }
  }


  protected void run_toplevel(EclipseConnection eclipse) throws Exception
  {
    // Start eclipse toplevel
    eclipse.rpc("sepia_kernel:standalone_toplevel");
  }


  // creates options object
  private EclipseEngineOptions createOptions()
    throws NoEclipseDirectoryException
  {
    File eclipseDir;

    // try to get the eclipse directory from the system property,
    // throw exception if null.
    try
    {
      eclipseDir = new File(System.getProperty("eclipse.directory"));
    }
    catch(NullPointerException e)
    {
      throw(new NoEclipseDirectoryException());
    }

    // create options
    EclipseEngineOptions options = new EclipseEngineOptions(eclipseDir);

    // shared io
    options.setUseQueues(false);

    return(options);
  }

  class BadMemorySettingException extends Exception {}

  class NoEclipseDirectoryException extends Exception {}

  // scan the application command line arguments for -g, -l, or -s. If these
  // occur before -- then set the various memory options according to the
  // argument settings. If any of them is set illegally a
  // BadMemorySettingException is thrown.
  private void setMemoryOptionsFromCommandLine(EclipseEngineOptions options,
                                                      String[] args)
    throws BadMemorySettingException
  {
    Integer globalSize = findMemorySetting("-g", args);
    Integer localSize = findMemorySetting("-l", args);
    if(globalSize != null)
    {
      options.setGlobalSize(globalSize.intValue());
    }
    if(localSize != null)
    {
      options.setLocalSize(localSize.intValue());
    }
  }

  // Takes an option string (e.g. "-g") and an array of command line arguments
  // and returns the memory size specified by the user. Returns null if
  // no matching argument was found. Throws an exception if the user set
  // one of the options illegally.
  private Integer findMemorySetting(String option, String[] args)
    throws BadMemorySettingException
  {
    Integer result = null;
    int i, intResult;
    String setting;

    for(i = 0; i < args.length; i++)
    {
      // if argument is --, ignore all further options
      if(args[i].equals("--"))
      {
        return(result);
      }

      // if option matches
      if(args[i].equals(option))
      {
        // if there is no following argument, throw exception
        if(i == args.length - 1)
        {
          throw(new BadMemorySettingException());
        }
        // otherwise, get following argument
        setting = args[i+1];
        try
        {
          // try to parse an integer from it
          intResult = Integer.parseInt(setting);
          // round up to the nearest megabyte and return it
          result = new Integer((int) Math.ceil(intResult / 1024.0));
          return(result);
        }
        // if an integer cannot be parsed
        catch(NumberFormatException e)
        {
          // and the setting doesn't end in M, throw an exception
          if(!setting.endsWith("M"))
          {
            throw(new BadMemorySettingException());
          }
          else
          {
            // otherwise try to parse an integer from all characters except
            // the last one, and return it.
            try
            {
              return(new Integer(Integer.parseInt(setting.substring(0,
                                                  setting.length() - 1))));
            }
            // throw exception if this doesn't work
            catch(NumberFormatException e2)
            {
              throw(new BadMemorySettingException());
            }
          }
        }
      }
    }
    return(result);
  }

}
