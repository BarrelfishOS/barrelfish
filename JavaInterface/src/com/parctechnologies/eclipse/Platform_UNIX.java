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
// Contributor(s):  Josh Singer, Parc Technologies
// 
// END LICENSE BLOCK

//Title:        Java/ECLiPSe interface
//Version:      $Id: Platform_UNIX.java,v 1.1 2006/09/23 01:54:11 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Encapsulated Singleton for platform-dependent code (UNIX).
package com.parctechnologies.eclipse;
import java.io.*;
abstract class Platform_UNIX extends Platform
{
  File getExecutableSubdirectory(File eclipseDirectory)
  {
    return(new File(new File(eclipseDirectory, "bin"),
                    getEclipsePlatformName()));
  }

  abstract String getSharedLibraryExtension();

  void loadEclipseSharedLibrary(File eclipseDirectory)
    throws UnsatisfiedLinkError
  {
    Runtime runtime = Runtime.getRuntime();
    String libPath =
      (new File(getLibrarySubdirectory(eclipseDirectory),
		"libec_java_load."+getSharedLibraryExtension())).toString();
    try
      {
        runtime.load(libPath);
      }
      catch(UnsatisfiedLinkError e)
      {
        throw new UnsatisfiedLinkError("The shared library "+libPath+
	    " could not be found.");
      }

    libPath = (new File(getLibrarySubdirectory(eclipseDirectory),
			"libec_java."+getSharedLibraryExtension())).toString();
    NativeEclipse.chdir(getLibrarySubdirectory(eclipseDirectory).toString());
    try
      {
        runtime.load(libPath);
      }
      catch(UnsatisfiedLinkError e)
      {
        throw new UnsatisfiedLinkError("Either the shared library "+libPath+
        "could not be found, or a library it depends on "+
        "could not be found in LD_LIBRARY_PATH (LD_LIBRARY_PATH is set to "+
        System.getProperty("java.library.path")+ ").");
      }
      finally
      {
	NativeEclipse.resetdir();
      }
  }
}
