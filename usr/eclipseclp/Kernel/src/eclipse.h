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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe INCLUDE FILE
 *
 * $Id: eclipse.h,v 1.2 2012/02/25 13:36:44 jschimpf Exp $
 *
 * DESCRIPTION
 *		Included by C programs that use embedding interface.
 */

#define EC_EXTERNAL
#define EC_EMBED

#ifdef _WIN32

#include <windows.h>
#define Winapi WINAPI
#define DLLEXP __declspec(dllimport)

#else	/* UNIX */

#define Winapi
#define DLLEXP

#endif


#include "config.h"
#include "ec_public.h"
#include "types.h"
#include "embed.h"
