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

package com.parctechnologies.eclipse.visualisation;

import com.parctechnologies.eclipse.*;
import java.util.*;
import java.io.*;

/**
 * The BatchGoal class is used in conjunction with a simple ECLiPSe library
 * to execute a number of goals independently at once (i.e. within a single
 * invocation of rpc). The advantage of this is that it minimises the amount of
 * communication with ECLiPSe, especially useful when the connection is with a
 * RemoteEclipse and each rpc call requires several TCP/IP packets. <p>
 * See the <code>batch_goals</code> library documentation to see how the
 * ECLiPSe side of this design works.<p>
 * On the Java side, the implementation is very simple. A BatchGoal is a kind of
 * LinkedList. BatchGoals may in fact be nested. The ECLiPSe library detects
 * this and executes nested batched goals appropriately. The key method is
 * execute, which takes an EclipseConnection. In this method, the BatchGoal
 * simply ensures that the <code>batch_goals</code> library is loaded, then
 * passes itself in to an <code>execute_batch</code> goal as the first
 * argument. It executes the goal using the rpc method of the Eclipse
 * Connection. Finally, it extracts the results of the goal from the second
 * argument and returns these, cast to a List. The results can be deconstructed
 * by the client.
 *
 *
 */
public class BatchGoal extends LinkedList
{
  private static final Atom batchGoalsAtom = new Atom("batch_goals");

  private static final CompoundTerm libGoal =
    new CompoundTermImpl("ensure_loaded",
                         new CompoundTermImpl("library",
                                              batchGoalsAtom));

  // execute the batched goal in eclipse, return results as a collection.
  // NB results of nested batched goals will be returned as inner-nested
  // collections. The atomic elements are EclipseExceptions or CompoundTerms
  List execute(EclipseConnection eclipse)
    throws EclipseException, IOException
  {
    if(this.isEmpty())
    {
      return(Collections.EMPTY_LIST);
    }
    CompoundTerm executeGoal =
      new CompoundTermImpl(":", batchGoalsAtom,
                           new CompoundTermImpl("execute_batch", this, null));
    CompoundTerm fullGoal =
      new CompoundTermImpl(",", libGoal, executeGoal);

    CompoundTermImpl result = (CompoundTermImpl) eclipse.rpc(fullGoal);
    return((List) (result.argCT(2).argCT(2).arg(2)));
  }

}
