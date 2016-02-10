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
//Version:      $Id: EclipseEngine.java,v 1.1 2006/09/23 01:54:09 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Interface for objects providing full access to an ECLiPSe engine.
package com.parctechnologies.eclipse;
import java.io.*;

/**
 * Interface of objects providing "ownership" of an ECLiPSe engine.
 * As well as the functionality
 * provided by the {@link EclipseConnection} interface,
 * classes which implement
 * <i>EclipseEngine</i> also allow access to the ECLiPSe engine's standard
 * streams: <code>stdin</code>, <code>stdout</code> and <code>sterr</code>.</li>
 * @see EclipseConnection
 * @see EmbeddedEclipse
 * @see OutOfProcessEclipse
 */
public interface EclipseEngine extends EclipseConnection
{
  /**
   * Return a <i>ToEclipseQueue</i> which allows access to the ECLiPSe engine's
   * <code>stdin</code> stream. Returns null if the ECLiPSe was not set
   * up to use queue objects for its standard streams (see {@link EclipseEngineOptions}).
   *
   * @throws EclipseTerminatedException if the ECLiPSe engine has been destroyed
   * or disconnected.
   */
  public ToEclipseQueue getEclipseStdin() throws EclipseTerminatedException;

  /**
   * Return a <i>FromEclipseQueue</i> which allows access to the ECLiPSe engine's
   * <code>stdout</code> stream. Returns null if the ECLiPSe was not set
   * up to use queue objects for its standard streams (see {@link EclipseEngineOptions}).
   *
   * @throws EclipseTerminatedException if the ECLiPSe engine has been
   * destroyed.
   */
  public FromEclipseQueue getEclipseStdout() throws EclipseTerminatedException;

  /**
   * Return a <i>FromEclipseQueue</i> which allows access to the ECLiPSe engine's
   * <code>stderr</code> stream. Returns null if the ECLiPSe was not set
   * up to use queue objects for its standard streams (see {@link EclipseEngineOptions}).
   *
   * @throws EclipseTerminatedException if the ECLiPSe engine has been
   * destroyed.
   */
  public FromEclipseQueue getEclipseStderr() throws EclipseTerminatedException;

  /**
   * Query the ECLiPSe engine's "use queues" flag. If true, ECLiPSe's standard streams
   * (stdin, stdout and stderr)
   * have been linked to FromEclipseQueue/ToEclipseQueue objects: if false,
   * they have been linked to
   * the operating system standard streams.
   */
  public boolean isUsingQueues();


}
