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

import java.util.*;
import java.beans.*;
import javax.swing.table.*;
import com.parctechnologies.eclipse.*;

/**
 * A data store which holds only MultiViewletType.data, but provides
 * methods to access <i>views</i> of this store as though it only
 * contained single types.
 * */
public interface MultiViewletDataStore extends ViewletDataStore {
    /**
     * Returns a data store based on this one, but containing only
     * viewlets of a specific type.
     *
     * <p> Implementing classes may actualy contain seperate
     * ViewletDataStores for each type of viewlet which they return
     * here, or they may construct wrapper objects which map the
     * <code>getViewletDataAt</code> methods to the underlying
     * ViewletData.
     * */
    public ViewletDataStore getViewletDataStore(ViewletType type);
}
