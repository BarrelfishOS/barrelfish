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
//Version:      $Id: QueueExample2.java,v 1.1 2006/09/23 01:54:13 snovello Exp $
//Copyright:    Copyright (c) 2001
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;
import java.util.*;

public class QueueExample2
{
  // Create some default Eclipse options
  static EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();

  // Object representing the Eclipse process
  static EclipseEngine eclipse;

  // Path of the Eclipse program
  static File eclipseProgram;

  // Data going out from java
  static ToEclipseQueue java_to_eclipse;

  // Data coming in from eclipse
  static FromEclipseQueue eclipse_to_java;

  public static void main(String[] args) throws Exception
  {
    // Connect the Eclipse's standard streams to the JVM's
    eclipseEngineOptions.setUseQueues(false);

    // Initialise Eclipse
    eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);

    String sep = System.getProperty("file.separator");

    // Set up the path of the example Eclipse program to be used.
    eclipseProgram = new File(System.getProperty("eclipse.directory") +
				   sep + "doc" + sep + "examples" + sep +
                                   "JavaInterface" + sep +
				   "queue_example_2.pl");

    // Compile the eclipse program. This sets up the two queue streams
    eclipse.compile(eclipseProgram);

    // Set up the java representation of the two queue streams
    java_to_eclipse = eclipse.getToEclipseQueue("java_to_eclipse");
    eclipse_to_java = eclipse.getFromEclipseQueue("eclipse_to_java");

    // add a TermProducer as a listener to the java_to_eclipse ToEclipseQueue
    java_to_eclipse.setListener(new TermProducer());

    // add a TermConsumer as a listener to the eclipse_to_java FromEclipseQueue
    eclipse_to_java.setListener(new TermConsumer());

    eclipse.rpc("read_5_write_5");

    // Destroy the Eclipse process
    ((EmbeddedEclipse) eclipse).destroy();

  }

  /**
   * QueueListener whose dataRequest method sends 5 different atoms
   * in exdr format along the queue it is attached to.
   */
  static class TermProducer implements QueueListener
  {
    ToEclipseQueue output_queue_stream = null;
    EXDROutputStream output_queue_stream_formatted = null;
    String[] atoms = {"mercury", "venus", "earth", "mars", "jupiter"};
    int atom_number = 0;

    // Required to implement QueueListener
    public void dataAvailable(Object source)
    {
    }

    // Called when Eclipse tries to read from source when it is empty.
    public void dataRequest(Object source)
    {
      if(output_queue_stream == null)
      {
	output_queue_stream = (ToEclipseQueue) source;
	output_queue_stream_formatted =
	  new EXDROutputStream(output_queue_stream);
      }

      try
      {
	output_queue_stream_formatted.write(atoms[atom_number]);
        output_queue_stream_formatted.flush();
      } catch(IOException ioe){}

      atom_number++;
      if(atom_number == 5)
	  atom_number = 0;
    }
  }

  /**
   * QueueListener whose dataAvailable method reads a term
   * in exdr format from the queue it is attached to, converts it to the
   * Java representation of the term, and prints this out to stdout.
   */
  static class TermConsumer implements QueueListener
  {
    FromEclipseQueue input_queue_stream = null;
    EXDRInputStream input_queue_stream_formatted = null;

    // Called when Eclipse flushes source
    public void dataAvailable(Object source)
    {
      if(input_queue_stream == null)
      {
	input_queue_stream = (FromEclipseQueue) source;
	input_queue_stream_formatted =
	  new EXDRInputStream(input_queue_stream);
      }

      try
      {
        System.out.println(input_queue_stream_formatted.readTerm());
      } catch(IOException ioe){}

    }

    // Required to implement QueueListener
    public void dataRequest(Object source)
    {
    }
  }

}
