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
//Version:      $Id: CompoundTerm.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Java interface to represent an ECLiPSe compound term
package com.parctechnologies.eclipse;

/**
 * Interface to describe an ECLiPSe compound term.
 * An object that implements it must be able to supply a functor, an arity,
 * and arguments. The arguments may be of any Java class which represents an
 * ECLiPSe data.
 * Compound terms converted from EXDR format using
 * {@link EXDRInputStream} implement this
 * interface.
 * <p>
 * A compound term like p(q(2),a,"b") has functor "p" and arity 3, arg(1) is the term q(2)
 * arg(2) is the term 'a' and arg(3) is the Java String "b".
 */
public interface CompoundTerm
{
    /**
     * Return the functor of the compound term.
     */
    public String functor();

    /**
     * Return the number of arguments.
     */
    public int arity();

    /**
     * Return the argument at position <code>i</code>. The returned object will
     * instantiate the Java class/interface representing the corresponding ECLiPSe
     * data type.
     * @param i may vary between 1 and <code>arity()</code>
     */
    public Object arg(int i);
}
