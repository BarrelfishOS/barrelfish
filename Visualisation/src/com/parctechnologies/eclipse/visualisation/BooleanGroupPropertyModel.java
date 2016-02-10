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

import javax.swing.*;
import java.beans.*;
import java.lang.reflect.*;

/**
 * This is a "bridging" class between some java beans classes
 * (PropertyChangeSupport, PropertyDescriptor etc) and the swing JToggleButton
 * class. The idea is to have a clean way of providing toggle buttons that
 * accurately control and reflect the state of a named boolean property of some
 * object.<p>
 * The way it works is that the "propertyHolder" has a boolean property such
 * that:
 * <ul>
 * <li> The property has a name propertyName (such as "autoResume")
 * <li> The propertyHolder has public get/set methods which obey the conventional
 * get/set naming pattern e.g. (public boolean getAutoResume(); public void
 * setAutoResume(boolean)).
 * <li> There is a PropertyChangeSupport object which "observes" the boolean
 * property, in that a PropertyChangeEvent is sent to all its listeners when
 * the boolean property changes value.
 * </ul>
 * We can then construct a BooleanPropertyModel given the property's name, and
 * references to the propertyHolder and the observing PropertyChangeSupport.
 * This BooleanPropertyModel is a subclass of JToggleButton.ToggleButtonModel
 * and so can be used as the underlying model of any swing JToggleButton.
 */
public class BooleanGroupPropertyModel extends JToggleButton.ToggleButtonModel
  implements PropertyChangeListener
{
  // PropertyDescriptor is a beans class which handles the necessary
  // reflection to work with the get/set methods of the property.
  private PropertyDescriptor propertyDescriptor;
  private Object propertyHolder;

  // Hold the value of the property which this model should consider true
  private Object match;

  public BooleanGroupPropertyModel(String propertyName, Object propertyHolder,
				   PropertyChangeSupport propertyChangeSupport,
				   Object match)
  {
    super();
    this.match = match;
    PropertyDescriptor pd;
    try
    {
      pd = new PropertyDescriptor(propertyName,
       propertyHolder.getClass());
    }
    catch(IntrospectionException ie)
    {
      throw(new RuntimeException("Exception thrown: "+ie));
    }
    this.propertyHolder = propertyHolder;
    setSelected(match.equals(getValue(pd)));
    this.propertyDescriptor = pd;
    propertyChangeSupport.addPropertyChangeListener(propertyName, this);
  }

  /**
   * This propertyChange method means that the ButtonModel's setSelected method
   * will be invoked if anything changes the property setting.
   */
  public void propertyChange(PropertyChangeEvent event)
  {
    Object newValue =  event.getNewValue();
    boolean isMatch = (newValue.equals(match));
    if(isSelected() != isMatch)
    {
      setSelected(isMatch);
    }
  }

  /**
   * setSelected extends the behaviour of the parent class by invoking the
   * propertyHolder's writeMethod.
   */
  public void setSelected(boolean newValue)
  {
    super.setSelected(newValue);
    // check that it has been initialised
    if((propertyDescriptor != null) && newValue)
    {
      Method writeMethod = propertyDescriptor.getWriteMethod();
      Object[] args = new Object[1];

      args[0] = match;
      try
      {
	  if (DebuggingSupport.logMessages) {
	      DebuggingSupport.logMessage(this, "BooleanGroupPropertyModel invoking " + writeMethod);
	  }
          writeMethod.invoke(propertyHolder, args);
      }
      catch(IllegalAccessException iae)
      {throw (new RuntimeException("Exception thrown: "+iae));}
      catch(InvocationTargetException ite)
      {
	  ite.printStackTrace();
	  throw (new RuntimeException("Exception thrown: "+ite.getTargetException()));}
    }
  }

  /**
   * The getValue method is used when the BooleanPropertyModel is first
   * initialised. It is invoked in order to discover the initial value of the
   * property, which is done by invoking the getMethod.
   */
  private Object getValue(PropertyDescriptor pd)
  {
    Method readMethod = pd.getReadMethod();
    Object[] args = new Object[0];

    Object result;

    try
    {
      result = readMethod.invoke(propertyHolder, args);
    }
    catch(IllegalAccessException iae)
    {throw (new RuntimeException("Exception thrown: "+iae));}
    catch(InvocationTargetException ite)
    {throw (new RuntimeException("Exception thrown: "+ite));}
    return(result);
  }

}
