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
//Version:      $Id: AbstractCompoundTerm.java,v 1.1 2006/09/23 01:54:08 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technologies
//Description:  Superclass for user-defined objects which implement CompoundTerm.
package com.parctechnologies.eclipse;

/**
 * A useful superclass for objects implementing the
 * {@link CompoundTerm} interface. <p>
 * Subclass <i>AbstractCompoundTerm</i> if you are creating a class which implements
 * {@link CompoundTerm}. This abstract class provides some of the methods required.
 * @see CompoundTerm
 * @see CompoundTermImpl
 */
public abstract class AbstractCompoundTerm implements CompoundTerm
{
  /**
   * The functor.
   */
  private String functor;

  /**
   * The arity.
   */
  private int arity;

  /**
   * Construct an <i>AbstractCompoundTerm</i> with a given functor and arity.
   */
  public AbstractCompoundTerm(String functor, int arity)
  {
    this.functor = functor;
    this.arity = arity;
  }

  /**
   * Returns the functor.
   */
  public String functor()
  {
    return functor;
  }

  /**
   * Return the arity.
   */
  public int arity()
  {
    return arity;
  }


  /**
   * Overrides <code>equals()</code> in <i>java.lang.Object</i>. Returns true
   * iff the parameter Object implements CompoundTerm and its functor and arity
   * are equal to this object's and pairwise invocations of <code>equals()</code>
   * return true between each of this object's arguments and the corresponding
   * argument of the parameter object.
   */
  public boolean equals(Object obj)
  {
    if(!(obj instanceof CompoundTerm))
    {
      return(false);
    }
    CompoundTerm ct = (CompoundTerm) obj;
    if(!ct.functor().equals(this.functor()))
    {
      return(false);
    }
    if(ct.arity() != this.arity())
    {
      return(false);
    }
    for(int i = 1; i <= ct.arity(); i++)
    {
      if(!this.arg(i).equals(ct.arg(i)))
      {
        return(false);
      }
    }
    return(true);
  }

  // overrides Object: two of these which are equals will have equals functors
  // and args and therefore the same hashcode.
  public int hashCode()
  {
    int hashcode = functor().hashCode();
    for(int i = 1; i <= arity(); i++)
    {
      if(arg(i) != null)
      {
        hashcode += arg(i).hashCode();
      }
    }
    return(hashcode);
  }

}
