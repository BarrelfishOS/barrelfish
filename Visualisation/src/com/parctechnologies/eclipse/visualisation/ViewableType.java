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
import java.io.Serializable;

/**
 * Class to represent the type of a viewable.
 */
public class ViewableType implements Serializable
{
  static List atomListToStringList(Collection atomList) {
    // copy the Atoms into Strings
    List stringList = new ArrayList(atomList.size());
    for(Iterator it = atomList.iterator(); it.hasNext(); ) {
      Atom fix = (Atom)it.next();
      stringList.add(fix.functor());
    }
    return stringList;
  }

  /**
   * Given a CompoundTerm, try to create a ViewableType object from it, and
   * throw a VisException otherwise.
   */
  static ViewableType parseFromCompoundTerm(CompoundTerm term)
    throws VisException
  {
    if(term.functor().equals("array") &&
       term.arity() == 2)
    {
      // copy the Atoms into Strings
//        Collection fixityListAtoms = (Collection) term.arg(1);
//        Collection fixityListStrings = new ArrayList(fixityListAtoms.size());
//        for(Iterator it = fixityListAtoms.iterator(); it.hasNext(); ) {
//            Atom fix = (Atom)it.next();
//            fixityListStrings.add(fix.functor());
//        }

      Collection fixityListStrings = 
        atomListToStringList((Collection) term.arg(1));
     CompoundTerm elementType = (CompoundTerm) term.arg(2);

      return(
        new ArrayType(
          new LinkedList(fixityListStrings),
          ElementType.parseFromCompoundTerm(elementType)));
    }
    else if ("multi_array".equals(term.functor()) &&
             term.arity() == 3) {
      Collection fixityListList = (Collection) term.arg(1);
      Collection elementTypeList = (Collection) term.arg(2);
      Collection subArrayNameList = (Collection) term.arg(3);
      
      Iterator fixIt = fixityListList.iterator();
      Iterator elemIt = elementTypeList.iterator();

      List subViewableTypes = new ArrayList(fixityListList.size());

      while(fixIt.hasNext()) {
        Collection fixityListStrings =
          atomListToStringList((Collection) fixIt.next());
        CompoundTerm elementType = (CompoundTerm) elemIt.next();
        ViewableType subType = 
          new ArrayType(new LinkedList(fixityListStrings),
                                ElementType.parseFromCompoundTerm(elementType));
        subViewableTypes.add(subType);
      }
      return new MultiArrayType(subViewableTypes,
                                atomListToStringList(subArrayNameList));
    }
    else
    {
      throw(
        new VisException("Could not parse viewable type from term "+
                         term));
    }

  }




  public static class ArrayType extends ViewableType {
    private ElementType elementType;
    private List fixityList;

    ArrayType(List fixityList, ElementType elementType)
    {
      setElementType(elementType);
      setFixityList(fixityList);
    }
    
    protected void setElementType(ElementType elementType)
    {
      this.elementType = elementType;
    }
    
    protected void setFixityList(List fixityList)
    {
      this.fixityList = fixityList;
    }
    
    public int getNDimensions()
    {
      return(fixityList.size());
    }
    
    public List getFixityList()
    {
      return(fixityList);
    }
    
    public ElementType getElementType()
    {
      return(elementType);
    }    
  }

  public static class MultiArrayType extends ViewableType {
    List subTypes;
    List subNames;

    MultiArrayType(List subTypes, List subNames) {
      this.subTypes = subTypes;
      this.subNames = subNames;
    }

    public ArrayType getSubType(int i) {
      return (ArrayType) (subTypes.get(i));
    }

    public String getSubTypeName(int i) {
      return (String) (subNames.get(i));
    }
  }
}

