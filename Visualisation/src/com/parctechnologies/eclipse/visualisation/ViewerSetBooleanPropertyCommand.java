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

/**
 * Command to set a named viewer property to a boolean value. The property, if
 * called xxx must have a single method named setXxxPrivate(boolean).
 */
package com.parctechnologies.eclipse.visualisation;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class ViewerSetBooleanPropertyCommand extends ViewerCommand {
  private String propertyName;
  private Boolean newValue;

  public ViewerSetBooleanPropertyCommand(Viewer viewer, String propertyName,
                                         boolean newValue) {
      super(viewer);
      this.propertyName = propertyName;
      this.newValue = new Boolean(newValue);
  }

  public void postRecordIssue()
  {
      String methodName = "set";
      char [] propChars = propertyName.toCharArray();
      propChars[0] = Character.toUpperCase(propChars[0]);
      methodName += new String(propChars);
      methodName += "Private";

      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this,"method name is : "+methodName);
      }

      Class[] sig = new Class[1];
      sig[0] = Boolean.TYPE;

      Method writeMethod = null;
      try
      {
        writeMethod = getViewer().getClass().getMethod(methodName, sig);
      }
      catch(NoSuchMethodException nsme){}
      catch(SecurityException se){}


      if (DebuggingSupport.logMessages) {
	  DebuggingSupport.logMessage(this,"method is : "+writeMethod);
      }

      Object[] args = new Object[1];
      args[0] = newValue;
      try
      {
          writeMethod.invoke(getViewer(), args);
      }
      catch(IllegalAccessException iae){}
      catch(InvocationTargetException ite){}
  }
}
