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
//Version:      $Id: Platform.java,v 1.8 2010/11/26 04:15:13 kish_shen Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Encapsulated Singleton for platform-dependent code.
package com.parctechnologies.eclipse;

import java.io.*;
import java.lang.RuntimeException;

abstract class Platform
{
  private static Platform single = null;

  public abstract boolean supportsEmbeddedEclipse();
  public abstract boolean supportsOutOfProcessEclipse();

  public static Platform getInstance()
  {
    if(single == null)
    {
      single = findPlatform();
    }
    return(single);
  }

  abstract String getEclipsePlatformName();

  abstract File getExecutableSubdirectory(File eclipseDirectory);

  abstract String getSharedLibraryExtension();

  abstract void loadEclipseSharedLibrary(File eclipseDirectory)
    throws UnsatisfiedLinkError;

  File getLibrarySubdirectory(File eclipseDirectory)
  {
    return(new File(new File(eclipseDirectory, "lib"),
                    getEclipsePlatformName()));
  }

  private static Platform findPlatform()
  {
    // Get the operating system and the architecture from the System class
    String OSName = System.getProperty("os.name");
    String arch = System.getProperty("os.arch");

    // Windows variants
    if(OSName.startsWith("Windows")) // intended to catch future Windows OSs
    {
       if(arch.equals("x86"))
       {
	  return(new Platform_i386_nt());
       }
      else if(arch.equals("x86_64") || arch.equals("amd64"))
      {
	return(new Platform_x86_64_nt());
      }
    }

    // Linux on intels
    else if(OSName.equals("Linux"))
    {
      if(arch.equals("i386") || arch.equals("x86"))
      {
	return(new Platform_i386_linux());
      }
      else if(arch.equals("x86_64") || arch.equals("amd64"))
      {
	return(new Platform_x86_64_linux());
      }
    }

    // SunOS/Solaris on sparc/intel
    else if(OSName.equals("SunOS") || OSName.equals("Solaris"))
    {
      if(arch.equals("sparc"))
      {
	return(new Platform_sparc_sunos5());
      }
      else if(arch.equals("i386") || arch.equals("x86"))
      {
	return(new Platform_i386_sunos5());
      }
      else if(arch.equals("x86_64") || arch.equals("amd64"))
      {
	return(new Platform_x86_64_sunos5());
      }
    }

    // MacOSX on PPC/Intel
    else if(OSName.equals("Mac OS X") || OSName.equals("Darwin"))
    {
      if(arch.equals("ppc"))
      {
	  return(new Platform_ppc_macosx());
      }
      else if(arch.equals("i386") || arch.equals("x86"))
      {
	  return(new Platform_i386_macosx());
      }
      else if(arch.equals("x86_64") || arch.equals("amd64"))
	  return(new Platform_x86_64_macosx());
    }

    throw new RuntimeException("Unsupported platform: " + OSName + "/" + arch);

  }

}
