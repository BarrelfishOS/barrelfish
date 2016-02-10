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
//Version:      $Id: QueueExample1.java,v 1.1 2006/09/23 01:54:13 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;
import java.util.*;

public class QueueExample1
{
  public static void main(String[] args) throws Exception
  {
    // Create some default Eclipse options
    EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();

    // Object representing the Eclipse process
    EclipseEngine eclipse;

    // Path of the Eclipse program
    File eclipseProgram;

    // Data going out from java
    ToEclipseQueue java_to_eclipse;

    // Data coming in from eclipse
    FromEclipseQueue eclipse_to_java;

    // EXDR translation of data going out from Java
    EXDROutputStream java_to_eclipse_formatted;

    // EXDR translation of data coming in from eclipse
    EXDRInputStream eclipse_to_java_formatted;

    // Connect the Eclipse's standard streams to the JVM's
    eclipseEngineOptions.setUseQueues(false);

    // Initialise Eclipse
    eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);

    String sep = System.getProperty("file.separator");

    // Set up the path of the example Eclipse program to be used.
    eclipseProgram = new File(System.getProperty("eclipse.directory") +
				   sep + "doc" + sep + "examples" + sep +
                                   "JavaInterface" + sep +
				   "queue_example_1.pl");

    // Compile the eclipse program.
    eclipse.compile(eclipseProgram);

    // Create the two queues
    java_to_eclipse = eclipse.getToEclipseQueue("java_to_eclipse");
    eclipse_to_java = eclipse.getFromEclipseQueue("eclipse_to_java");

    // Set up the two formatting streams
    java_to_eclipse_formatted = new EXDROutputStream(java_to_eclipse);
    eclipse_to_java_formatted = new EXDRInputStream(eclipse_to_java);

    java_to_eclipse_formatted.write(new Atom("a"));
    java_to_eclipse_formatted.write(new Atom("b"));
    java_to_eclipse_formatted.flush();

    eclipse.rpc("read_2_write_1");

    System.out.println(eclipse_to_java_formatted.readTerm());

    // Destroy the Eclipse process
    ((EmbeddedEclipse) eclipse).destroy();

  }

   /**
   * QueueListener which just writes everything to standard out.
   */
  static class OutputQL implements QueueListener
  {
    String name;
    EXDRInputStream eis;

    public OutputQL(String name, EXDRInputStream eis)
    {
      this.name = name;
      this.eis = eis;
    }

    public void dataAvailable(Object source)
    {
      try
      {
        while(((InputStream) source).available() > 0)
        {
          System.out.println(name + ": "+eis.readTerm());
        }
      }
      catch(IOException e)
      {
        System.err.println("IOException: "+e);
      }
    }

    public void dataRequest(Object source){};
  }

}
