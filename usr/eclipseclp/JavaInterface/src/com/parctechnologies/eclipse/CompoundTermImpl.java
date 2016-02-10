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
//Version:      $Id: CompoundTermImpl.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Class for constructing Java objects representing ECLiPSe compound terms.
package com.parctechnologies.eclipse;

/**
 * An ECLiPSe compound term. This concrete class can be used to construct objects
 * which implement the CompoundTerm interface.
 * @see CompoundTerm
 */
public class CompoundTermImpl extends AbstractCompoundTerm implements CompoundTerm
{
  /**
   * Element 0 of term is the functor String, the remaining elements are the
   * compound term's arguments. Arity is therefore term.length - 1
   *
   */
  private Object term[];

  /**
   * Construct a compound term from an Object array. In this constructor the
   * functor (which should be a String)
   * is passed in as element 0 of the array parameter, the other arguments,
   * which can
   * be instances of any Java classes/interfaces representing ECLiPSe types,
   * are the remaining elements of the array. The arity is
   * therefore the size of the array - 1.
   */
  public CompoundTermImpl(Object[] term)
  {
    // call AbstractCompoundTerm constructor setting up functor and arity
    super((String) term[0], term.length - 1);
    this.term = term;
  }

  /**
   * Construct a compound term from a String and an Object array. User
   * supplies functor and an array of argument objects.
   * Arity of the resulting compound term is <code>args.length</code>.
   */
  public CompoundTermImpl(String functor, Object[] args)
  {
    // call AbstractCompoundTerm constructor setting up functor and arity
    super(functor, args.length);
    int i;
    term = new Object[args.length + 1];
    term[0] = functor;
    for(i = 0; i < args.length; i++)
    {
      term[i+1] = args[i];
    }
  }

  /**
   * Convenience constructor for compound terms with arity 1.
   */
  public CompoundTermImpl(String functor,Object a1)
  {
    this(new Object[] {functor,a1});
  }
  /**
   *  Convenience constructor for terms with arity 2.
   */
  public CompoundTermImpl(String functor,Object a1,Object a2)
  {
    this(new Object[] {functor,a1,a2});
  }

  /**
   *  Convenience constructor for compound terms with arity 3.
   */
  public CompoundTermImpl(String functor,Object a1,Object a2,Object a3)
  {
    this(new Object[] {functor,a1,a2,a3});
  }

  /**
   *  Convenience constructor for compound terms with arity 4.
   */
  public CompoundTermImpl(String functor,Object a1,Object a2,Object a3,Object a4)
  {
    this(new Object[] {functor,a1,a2,a3,a4});
  }
  /**
   *  Convenience constructor for compound terms with arity 5.
   */
  public CompoundTermImpl(String functor,Object a1,Object a2,Object a3,Object a4,Object a5)
  {
    this(new Object[] {functor,a1,a2,a3,a4,a5});
  }

  /**
   * Return one of the term's arguments. These may be instances of any Java
   * class/interface representing an ECLiPSe type.
   * @param i the argument index. This may vary between 1 and <code>arity()</code> inclusive.
   *
   */
  public Object arg(int i)
  {
    if(i == 0 || i > arity())
    {
      throw new IllegalArgumentException("Argument index must be between 1 and "+
                                         arity()+" inclusive.");

    }
    return term[i];
  }

  /**
   * Return the argument at position <code>i</code>, as a CompoundTermImpl.
   * This operation is the same as arg, except that it tries to cast the result
   * to a CompoundTermImpl. Useful for extracting nested objects: eg.
   * <code>a = result.argCT(1).argCT(3).argCT(3);</code>
   * @param i may vary between 1 and <code>arity()</code>
   */
  public CompoundTermImpl argCT(int i)
  {
    return((CompoundTermImpl) arg(i));
  }

  public String toString()
  {
    String result = this.getClass().getName() + " with [";
    result += "functor="+functor();
    result += " arity="+arity();
    for(int i = 1; i <= arity(); i++)
    {
      result += " arg("+i+")="+arg(i);
    }
    result +="]";
    // System.err.println("WARNING: toString called on"+result);
    return(result);
  }


}

