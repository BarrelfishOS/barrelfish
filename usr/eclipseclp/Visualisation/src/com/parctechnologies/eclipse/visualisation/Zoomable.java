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

import java.awt.*;

/**
 * Swing components which can be zoomed. <p>
 * The contract for those methods which alter the zoom level (setZoomLevel,
 * zoomToWidth, zoomToHeight), is to alter the
 * preferredSize according to the client specification and invalidate the
 * component. Revalidation is the responsibility of the method's client rather
 * than the Zoomable.
 *
 *  */
public interface Zoomable
{

    /**
     * Set the size which will be considered normal. i.e. the one which defines
     * the height/width ratio
     */
    void setNormalSize(Dimension normalSize);

    Dimension getNormalSize();

    void zoomToLevel(float zoomLevel);

    /**
     * Return most recently set zoomLevel;
     */
    float getZoomLevel();
}
