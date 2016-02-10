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
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

public class AutoResumePanel extends JPanel
{
  public AutoResumePanel(VisClientStateModel stateModel)
  {
    super();
    setLayout(new GridBagLayout());
    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridy = GridBagConstraints.RELATIVE;
    gbc.gridx = 0;
    add(new JLabel("Auto Resume"),gbc);
    add(new JSlider(stateModel.getDelayModel()),gbc);
    add(new MillisecondsLabel(stateModel.getDelayModel()),gbc);
  }

  private class MillisecondsLabel extends JLabel implements ChangeListener
  {
    MillisecondsLabel(BoundedRangeModel model)
    {
      super();
      setText(getLabelText(model));
      model.addChangeListener(this);
    }
    private String getLabelText(BoundedRangeModel model)
    {
      return(model.getValue()+"ms");
    }
    public void stateChanged(ChangeEvent event)
    {
      setText(getLabelText((BoundedRangeModel) event.getSource()));
    }
  }

}
