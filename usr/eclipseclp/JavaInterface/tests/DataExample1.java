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
//Version:      $Id: DataExample1.java,v 1.1 2006/09/23 01:54:12 snovello Exp $
//Author:       Josh Singer
//Company:      Parc Technologies
//Description:  Java/ECLiPSe Interface example Java program
import com.parctechnologies.eclipse.*;
import java.io.*;

public class DataExample1
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

    // Construct a term
    CompoundTerm a_term = construct_term();

    // Get Eclipse to write the term to stdout and flush
    eclipse.rpc(
		new CompoundTermImpl(",",
			      new CompoundTermImpl("write",
					    new Atom("output"), a_term),
			      new CompoundTermImpl("flush", new Atom("output"))
			      )
		);

    // Destroy the Eclipse
    ((EmbeddedEclipse) eclipse).destroy();
  }

  // Construct a term in Java to represent the Eclipse term foo(a, b, 3).
  private static CompoundTerm construct_term()
  {
    Atom a = new Atom("a");
    Atom b = new Atom("b");
    Integer numberThree = new Integer(3);
    CompoundTerm theTerm = new CompoundTermImpl("foo", a, b, numberThree);

    return(theTerm);
  }

}

