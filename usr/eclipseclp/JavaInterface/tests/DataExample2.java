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
//Version:      $Id: DataExample2.java,v 1.1 2006/09/23 01:54:12 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;
import java.util.*;

public class DataExample2
{
  public static void main(String[] args) throws Exception
  {
    // Create some default Eclipse options
    EclipseEngineOptions eclipseEngineOptions = new EclipseEngineOptions();
    // Object representing the Eclipse process
    EclipseEngine eclipse;

    // Connect the Eclipse's standard streams to the JVM's
    eclipseEngineOptions.setUseQueues(false);

    // Initialise Eclipse
    eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);

    // Construct a collection
    Collection a_collection = construct_collection();

    // Write out the collection
    System.out.println(a_collection);

    // Get Eclipse to write the collection (a list) to stdout and flush
    eclipse.rpc(
		new CompoundTermImpl(",",
			      new CompoundTermImpl("write",
					    new Atom("output"),a_collection),
			      new CompoundTermImpl("flush", new Atom("output"))
			      )
		);

    // Destroy the Eclipse process
    ((EmbeddedEclipse) eclipse).destroy();

  }

  // Construct a collection in Java to represent the Eclipse
  // list [1, foo(3.5), bar].
  private static Collection construct_collection()
  {
      Collection theCollection = new LinkedList();

      theCollection.add(new Integer(1));
      theCollection.add(new CompoundTermImpl("foo", new Double(3.5)));
      theCollection.add(new Atom("bar"));

      return(theCollection);
  }

}

