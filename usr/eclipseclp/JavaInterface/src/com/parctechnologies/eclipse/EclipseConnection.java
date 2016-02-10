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
//Version:      $Id: EclipseConnection.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Josh Singer / Stefano Novello
//Company:      Parc Technologies
//Description:  Java interface representing a connection to an ECLiPSe engine.
package com.parctechnologies.eclipse;
import java.io.*;

/**
 * Interface of objects which provide a connection to an ECLiPSe engine.
 * Classes implementing <i>EclipseConnection</i> provide the following areas of
 * functionality:
 * <ul>
 * <li> "RPC" calls: deterministic ECLiPSe goals which are executed in ECLiPSe
 * and whose resulting instantiation is returned to Java. </li>
 * <li> The ability to create/access "queues" between Java and ECLiPSe. </li>
 * <li> The ability to register a handler to be called when ECLiPSe
 * enters/leaves multitasking phases.</li>
 * </ul>
 * @see EmbeddedEclipse
 * @see RemoteEclipse
 * @see OutOfProcessEclipse
 * @see EclipseEngine
 */
public interface EclipseConnection
{
  /**
   * Make an "RPC" (remote predicate call) to the ECLiPSe engine.
   * @param goal the goal as it would be typed in on the ECLiPSe command line,
   * (the full stop is unnecessary).
   * @throws EclipseException if execution of the goal fails or throws an
   * ECLiPSe exception.
   * @throws IOException if there was an I/O problem communicating with the
   * ECLiPSe engine.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   * @return a <i>CompoundTerm</i> representing the goal, with any variables
   * possibly further instantiated with the results of the computation.
   */
  public CompoundTerm rpc(String goal) throws EclipseException, IOException;

  /**
   * Make an "RPC" (remote predicate call) to the ECLiPSe engine.
   * @param goal the goal represented as a <i>CompoundTerm</i>.
   * @throws EclipseException if execution of the goal fails or throws an
   * ECLiPSe exception.
   * @throws IOException if there was an I/O problem communicating with the
   * ECLiPSe engine.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   * @return a <i>CompoundTerm</i> representing the goal, with any variables
   * possibly further instantiated with the results of the computation.
   */
  public CompoundTerm rpc(CompoundTerm goal) throws EclipseException, IOException;

  /**
   * Create or access a queue to transfer  data from ECLiPSe to Java. If a
   * <i>FromEclipseQueue</i>
   * with this name has already been created for this EclipseConnection, it
   * is returned. The supplied name should not be in use by any
   * ECLiPSe stream which is not a <i>FromEclipseQueue</i> between ECLiPSe and
   * this EclipseConnection:
   * if it is, an <i>EclipseException</i> is thrown. Otherwise, a new
   * <i>FromEclipseQueue</i> with the specified name is returned.
   *
   * @param name the name to be used for the stream representing the queue on
   * the ECLiPSe side.
   * @return a <i>FromEclipseQueue</i> object which can be used by Java to read data
   * on the queue which was written there by ECLiPSe.
   * @throws EclipseException if the name for the ECLiPSe stream is already in use,
   *  or ECLiPSe could not create its side of the queue for some reason.
   * @throws IOException if there was an I/O problem while accessing ECLiPSe.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public FromEclipseQueue getFromEclipseQueue(String name) throws EclipseException, IOException;
  /**
   * Create or access a queue to transfer data from Java to ECLiPSe. If a
   * <i>ToEclipseQueue</i>
   * with this name has already been created for this EclipseConnection, it
   * is returned. The supplied name should not be in use by any
   * ECLiPSe stream which is not a <i>ToEclipseQueue</i> between ECLiPSe and
   * this EclipseConnection:
   * if it is, an <i>EclipseException</i> is thrown. Otherwise, a new
   * <i>ToEclipseQueue</i> with the specified name is returned.
   *
   *
   * @param name the name to be used for the stream representing the queue on
   * the ECLiPSe side.
   * @return a <i>ToEclipseQueue</i> object which can be used by Java to write data
   * on the queue which can be read by ECLiPSe.
   * @throws EclipseException if the name for the ECLiPSe stream is already in use,
   *  or ECLiPSe could not create its side of the queue for some reason.
   * @throws IOException if there was an I/O problem while accessing ECLiPSe.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public ToEclipseQueue getToEclipseQueue(String name) throws EclipseException, IOException;
  /**
   * Create or access an asynchronous queue to transfer data between Java
   * and ECLiPSe. If an <i>AsyncEclipseQueue</i>
   * with this name has already been created for this EclipseConnection, it
   * is returned. The supplied name should not be in use by any
   * ECLiPSe stream which is not a <i>AsyncEclipseQueue</i> between ECLiPSe and
   * this EclipseConnection:
   * if it is, an <i>EclipseException</i> is thrown. Otherwise, a new
   * <i>AsyncEclipseQueue</i> with the specified name is returned.
   *
   *
   * @param name the name to be used for the stream representing the queue on
   * the ECLiPSe side.
   * @return a <i>AsyncEclipseQueue</i> object which can be used by Java
   * to obtain an InputStream object (which can be used to read data from
   * the queue which was written there by ECLiPSe) and/or an OutputStream
   * object (which can be used to write data on the queue which can be read
   * by ECLiPSe).
   * @throws EclipseException if the name for the ECLiPSe stream is already in use,
   *  or ECLiPSe could not create its side of the queue for some reason.
   * @throws IOException if there was an I/O problem while accessing ECLiPSe.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public AsyncEclipseQueue getAsyncEclipseQueue(String name) throws EclipseException, IOException;

  /**
   * Direct ECLiPSe to compile a named object file.
   *
   * @param file the path of the ECLiPSe object file which is to be compiled.
   *
   * @throws EclipseException if ECLiPSe failed or threw an exception whilst trying
   * to compile the file.
   * @throws IOException if there was an I/O problem while communicating with
   * ECLiPSe.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public void compile(File f) throws EclipseException, IOException;

  /**
   * Convert a file path from the Java representation to the ECLiPSe
   * representation.
   *
   * @param f the file path to be converted.
   *
   * @throws EclipseException if ECLiPSe failed or threw an exception whilst trying
   * to convert the file path.
   * @throws IOException if there was an I/O problem while communicating with
   * ECLiPSe.
   * @return a String: the file path in the ECLiPSe representation
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public String getPath(File f) throws EclipseException, IOException;


  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * 1 argument.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object arg1) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * 2 arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * 3 arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * 4 arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * 5 arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object arg1,
                          Object arg2, Object arg3, Object arg4,
                          Object arg5) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies the functor string and
   * an array of arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(String functor, Object[] args) throws EclipseException, IOException;

  /**
   * Convenience <code>rpc</code> method. The user supplies an array. Element 0
   * is the functor of the goal term and the remaining elements are the arguments.
   * @see EclipseConnection#rpc(CompoundTerm)
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   */
  public CompoundTerm rpc(Object[] goalTerm) throws EclipseException, IOException;

  /**
   * Return the name by which the peer representing the Java side of the
   * connection is indexed in ECLiPSe.
   *
   * @return an Atom, the peer name.
   */
  public Atom getPeerName();


  /**
   * Register this peer as desiring participation in any multitasking
   * phases that ECLiPSe enters.
   *
   * @param multitaskListener A listener whose methods are called when
   * ECLiPSe enters/leaves multitasking phases.
   * @throws EclipseException if registration fails or throws an
   * ECLiPSe exception.
   * @throws IOException if there was an I/O problem communicating with the
   * ECLiPSe engine.
   * @throws EclipseTerminatedException if this <i>EclipseConnection</i> has
   * been terminated.
   * @return An object which can be used to perform RPCs during
   * ECLiPSe multitasking phases. */
  public EclipseMultitaskConnection registerMultitask(MultitaskListener multitaskListener) throws EclipseException,IOException;
}
