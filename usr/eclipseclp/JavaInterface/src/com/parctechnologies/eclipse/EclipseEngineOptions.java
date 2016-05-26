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
//Version:      $Id: EclipseEngineOptions.java,v 1.1 2006/09/23 01:54:09 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Set of options used to initialise a new ECLiPSe engine.
package com.parctechnologies.eclipse;
import java.io.*;
import java.util.Properties;


/** Encapsulates a set of options which can be used to initialise a new ECLiPSe
 * engine. When constructing an instance of <i>EclipseEngineOptions</i> the
 * options settings can be read from an instance of <i>java.util.Properties</i>
 * using a standard key string for each option:
 *
 * <ul>
 * <li> <b>eclipse.default-module</b> (<i>String</i>) for the module to use
 * initially for executing goals. Defaults to "eclipse".
 * <li> <b>eclipse.directory</b> (<i>String</i>) for the path of the ECLiPSe installation. This
 * option has no default and must be explicitly set either using a parameter
 * <i>File</i> or a property.
 * <li> <b>eclipse.global-size</b> (<code>int</code>) for the size of the global
 * stack in megabytes.
 * <li> <b>eclipse.local-size</b> (<code>int</code>) for the size of the local
 * stack in megabytes.
 * <li> <b>eclipse.peer-name</b> (<i>String</i>) for the name of the peer which represents this
 * <i>EclipseEngine</i> in ECLiPSe. Defaults to "host".
 * <li> <b>eclipse.use-queues</b> (<code>boolean</code>) for the "use queues" flag. Defaults to false.
 * </ul>
 * There are also set methods to explicitly set each option after the
 * <i>EclipseEngineOptions</i> object has been created.
 *
 *
 *
 */
public class EclipseEngineOptions
{
  // The eclipse module in which goals will be executed by default
  private String defaultModule;

  // The location of the Eclipse installation
  private String eclipseDir;

  // size of local stack in megabytes
  private int localSize;

  // size of global stack in megabytes
  private int globalSize;

  // Flag indicating whether queues are used to represent standard streams
  private boolean useQueues = false;

  // set of command-line options to be passed to the eclipse
  private String[] commandLineOptions = null;

  // the string to be used as the peer name
  private String _peerName = "host";

  // Size of a megabyte
  static final int MEGABYTE = 1024 * 1024;

  /**
   * Construct a set of EclipseEngineOptions using a specified ECLiPSe
   * installation and looking up all other settings in the system properties. If
   * a setting is not found in this properties list, the defaults are as listed
   * above.
   *
   * @throws IllegalArgumentException if any of the system properties does not
   * parse to the correct type.
   *
   */
  public EclipseEngineOptions(File eclipseDirectory)
  {
    setEclipseDir(eclipseDirectory);
    getOptionsFromProperties(System.getProperties());
  }

  /**
   * Construct a set of EclipseEngineOptions looking up all settings in
   * the system properties. If
   * a setting is not found in this properties list, the defaults are as listed
   * above.
   *
   * @throws IllegalArgumentException if there is not an
   * <b>eclipse.directory</b> property in the system properties, or if any of
   * the system properties does not parse to the correct type.
   *
   */
  public EclipseEngineOptions()
  {
    this(System.getProperties());
  }

  /**
   * Construct a set of EclipseEngineOptions looking up all settings in
   * the parameter set of properties. If
   * a setting is not found in this properties list, the defaults are as listed
   * above.
   *
   * @throws IllegalArgumentException if there is not an
   * <b>eclipse.directory</b> property in the parameter <i>Properties</i>, or if any of
   * the properties does not parse to the correct type.
   *
   */
  public EclipseEngineOptions(Properties properties)
  {
    getOptionsFromProperties(properties);
  }

  private void getOptionsFromProperties(Properties properties)
  {
    if(eclipseDir == null)
    {
      getEclipseDirFromProperties(properties);
    }
    getDefaultModuleFromProperties(properties);
    getPeerNameFromProperties(properties);
    getGlobalSizeFromProperties(properties);
    getLocalSizeFromProperties(properties);
    getUseQueuesFromProperties(properties);
  }

  private void getEclipseDirFromProperties(Properties properties)
  {
    if(!properties.containsKey("eclipse.directory"))
    {
      throw new IllegalArgumentException("Missing eclipse.directory property");
    }
    else
    {
      setEclipseDir(new File(properties.getProperty("eclipse.directory")));
    }
  }

  private void getDefaultModuleFromProperties(Properties properties)
  {
    if(properties.containsKey("eclipse.default-module"))
    {
      setDefaultModule(properties.getProperty("eclipse.default-module"));
    }
  }

  private void getPeerNameFromProperties(Properties properties)
  {
    if(properties.containsKey("eclipse.peer-name"))
    {
      setPeerName(properties.getProperty("eclipse.peer-name"));
    }
  }

  private void getGlobalSizeFromProperties(Properties properties)
  {
    if(properties.containsKey("eclipse.global-size"))
    {
      try
      {
        setGlobalSize(Integer.parseInt(properties.getProperty("eclipse.global-size")));
      }
      catch(NumberFormatException e)
      {
        throw new IllegalArgumentException("Property eclipse.global-size not an integer");
      }
    }
  }
  private void getLocalSizeFromProperties(Properties properties)
  {
    if(properties.containsKey("eclipse.local-size"))
    {
      try
      {
        setLocalSize(Integer.parseInt(properties.getProperty("eclipse.local-size")));
      }
      catch(NumberFormatException e)
      {
        throw new IllegalArgumentException("Property eclipse.local-size not an integer");
      }
    }
  }
  private void getUseQueuesFromProperties(Properties properties)
  {
    if(properties.containsKey("eclipse.use-queues"))
    {
      if(properties.getProperty("eclipse.use-queues").equalsIgnoreCase("true"))
      {
        setUseQueues(true);
        return;
      }
      if(properties.getProperty("eclipse.use-queues").equalsIgnoreCase("false"))
      {
        setUseQueues(false);
        return;
      }
      throw new IllegalArgumentException("Property eclipse.use-queues not a boolean");
    }
  }


  /**
   * Return default module
   */
  String getDefaultModule()
  {
    return(defaultModule);
  }

  /**
   * Return the array of command line options
   */
  String[] getCommandLineOptions()
  {
    return(commandLineOptions);
  }


  /**
   * Return the ECLiPSe installation directory.
   */
  String getEclipseDir()
  {
    return(eclipseDir);
  }

  /**
   * Return size of local stack
   */
  int getLocalSize()
  {
    return(localSize);
  }

  /**
   * Return size of global stack
   */
  int getGlobalSize()
  {
    return(globalSize);
  }

  /**
   * Set the default ECLiPSe module where goals are called. If none is set,
   * goals are called in the module 'eclipse'.
   */
  public void setDefaultModule(String defaultModule)
  {
    this.defaultModule = defaultModule;
  }

  /**
   * Set the peer name by which the Java side will be referenced in ECLiPSe.
   * The default setting is "host".
   */
  public void setPeerName(String peerName)
  {
    _peerName = peerName;
  }

  /**
   * get the peer name
   */
  String getPeerName()
  {
    return(_peerName);
  }


  /**
   * Set the directory where ECLiPSe is installed. This may correspond to
   * the ECLIPSEDIR environment variable/registry entry when using the
   * ECLiPSe development environment.
   */
  public void setEclipseDir(File eclipseDir)
  {
    this.eclipseDir = eclipseDir.getAbsolutePath();
  }

  /**
   * Set size of the ECLiPSe local stack in megabytes.
   */
  public void setLocalSize(int localSize)
  {
    this.localSize = localSize;
  }

  /**
   * Set size of the ECLiPSe global stack in megabytes.
   */
  public void setGlobalSize(int globalSize)
  {
    this.globalSize = globalSize;
  }

  /**
   * Set the "use queues" flag. If true, links ECLiPSe's standard streams
   * (stdin, stdout and stderr)
   * to FromEclipseQueue/ToEclipseQueue objects: if false, links them to
   * the operating system standard streams. In the false (default) case, the methods
   * {@link EclipseEngine#getEclipseStdin()}, {@link EclipseEngine#getEclipseStdout()}
   * and {@link EclipseEngine#getEclipseStderr()} will all return <code>null</code>.
   */
  public void setUseQueues(boolean useQueues)
  {
    this.useQueues = useQueues;
  }

  /**
   * Set an array of strings which will be passed to the ECLiPSe engine as command-
   * line arguments. These will then be accessible in Eclipse using the built-in
   * predicates argc/1 and argv/2. Note that by default the options will not be interpreted
   * unless the goal sepia_kernel:standalone_toplevel is executed.
   * Also note that conventionally, element 0 is the command-line command (e.g.
   * "eclipse") and the other options begin at element 1.
   *
   * Note: the access modifier of this should not be public. This method is for
   * internal package use only.
   */
  void setCommandLineOptions(String[] args)
  {
    this.commandLineOptions = args;
  }


  boolean isUsingQueues()
  {
    return useQueues;
  }


}
