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

import com.parctechnologies.eclipse.JEclipse;
import com.parctechnologies.eclipse.EclipseConnection;

/**
 * Variant of JEclipse which has a Visualisation client attached to the ECLiPSe
 * by default. The Visualisation is connected over an embedded connection rather
 * than a remote one, so communication of updates is much faster.
 */
public class JEclipse_VC extends JEclipse
{
  private VisClient vis_client;

  public static void main(String[] args) throws Exception
  {
    JEclipse_VC jeclipse_vc = new JEclipse_VC();

    jeclipse_vc.start(args);
  }

  // overrides run_toplevel in class JEclipse
  protected void run_toplevel(EclipseConnection eclipse) throws Exception
  {
        vis_client = new VisClient(eclipse.registerMultitask(null));
        super.run_toplevel(eclipse);
  }
}
