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

//Title:        Java/ECLiPSe Interface
//Version:      $Id: NativeEclipse.java,v 1.1 2006/09/23 01:54:10 snovello Exp $
//Author:       Stefano Novello / Josh Singer
//Company:      Parc Technogies Ltd.
//Description:  Java interface to C ECLiPSe embedding code.
package com.parctechnologies.eclipse;

class NativeEclipse {

  static native int chdir(String dir) ;

  static native int resetdir() ;

  static native int init() ;

  static native int resume() ;

  static native int resumeLong(Integer stream);

  static native int setOption(int opt, int val);

  static native int setOption(int opt, String val);

  static native int setOption(int opt, String[] vals);

  static native int QueueWrite(int streamid,byte b[], int off, int len);

  static native int QueueRead(int streamid,int off,int len,byte[] b);

  static native int QueueWriteByte(int streamid, byte b);

  static native int QueueReadByte(int streamid);

  static native int QueueAvailable(int streamid);

  static native int StreamNumber(String Name);

  // returns a status value, if it's PFLUSHIO or PWAITTIO, Stream
  // is set to the id of the stream
  static native int HandleEvents(Integer Stream);

  static native void Cleanup();

}
