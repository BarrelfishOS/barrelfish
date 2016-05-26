/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 *
 * Contributor(s): 
 *
 * END LICENSE BLOCK */

/*
 * Eclipse Java interface
 *
 *	An auxiliary library which should not depend on anything else.
 *	Containing just chdir() functions (Java has no chdir), which are
 *	used to change the current directory while loading libec_java.so
 *	to prevent the need to set LD_LIBRARY_PATH.
 *	This is hacky, but it has been requested by Parc Technologies.
 *
 * $Id: ec_java_load.c,v 1.1 2006/09/23 01:54:08 snovello Exp $
 *
 */

#include "com_parctechnologies_eclipse_NativeEclipse.h"

#ifdef _WIN32
#include <direct.h>
#define getcwd(buf,len) _getcwd(buf,len)
#define chdir(dir) _chdir(dir)
#else
#include <unistd.h>
#endif

#define MAX_PATH_LEN	1024
static char old_cwd[MAX_PATH_LEN];


/*
 * Class:     eclipse_NativeEclipse
 * Method:    ec_chdir
 * Signature: 
 * Returns 0 if ok, -1 if error
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_chdir
  (JNIEnv * e, jclass jc, jstring jsDir)
{
    const char *Dir;
    jint res;

    if (!getcwd(old_cwd, MAX_PATH_LEN))
    	return -1;
    Dir = (*e)->GetStringUTFChars(e,jsDir,NULL);
    res = (jint) chdir(Dir);
    (*e)->ReleaseStringUTFChars(e,jsDir,Dir);
    return res;
}

/*
 * Class:     eclipse_NativeEclipse
 * Method:    ec_resetdir
 * Signature: 
 * Returns 0 if ok, -1 if error
 */
JNIEXPORT jint JNICALL Java_com_parctechnologies_eclipse_NativeEclipse_resetdir
  (JNIEnv * e, jclass jc)
{
    return (jint) chdir(old_cwd);
}

