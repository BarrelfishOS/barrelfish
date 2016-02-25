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
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Kees Schuerman, ECRC
 * 
 * END LICENSE BLOCK */
/**********************************************************************
**      System: Parallel Distributed System
**        File: pds.mem.h
**      Author: Kees Schuerman
**      SccsId: "@(#)pds.mem.h	1.4 28 Nov 1995"
** Description: The files pds.mem.h and pds.mem.c provide the interface
**		to the memory and interrupt system. There are two 
**		compilation flags, i.e. THREAD_SAFE and INTERRUPT_SAFE. 
**		When processes are single threaded and no pds functions 
**		are invoked from interrupt handlers, there is no real 
**		need for enabling the thread safeness or interrupt safeness 
**		and neither of the flags should be activated. In general, 
**		one selects however THREAD_SAFE for applications with 
**		multithreaded processes and INTERRUPT_SAFE for applications 
**		with just single threaded processes.
**		Thread synchronization is based on mutexes which may
**		differ depending on their purpose, i.e. intra-process
**		synchronization (local mutex: l_mutex_t) or inter-process 
**		synchronization (global mutex: g_mutex_t). In general the 
**		former are more efficient than the latter.
***********************************************************************/

#ifndef _PDS_MEM_H_
#define _PDS_MEM_H_

#include <memman.h>		/* ECLiPSe Memory and Interrupt System */


/*
** Interrupt System
*/

#if defined(__STDC__)
extern void pds_int_init(void (* handler) (void));
#else /* __STDC__ */
extern void pds_int_init();
#endif /* __STDC__ */

#define pds_int_disabled  	InterruptsDisabled
#define pds_int_pending  	InterruptsPending
#define pds_disable_int()	Disable_Int()
#define pds_enable_int()	Enable_Int()
#define pds_set_int_pending()  	Set_Interrupts_Pending()
#define pds_clr_int_pending()  	Clr_Interrupts_Pending()


/*
** Global Mutexes
*/

typedef a_mutex_t g_mutex_t;

#define g_mutex_init(m)		a_mutex_init(m)
#define g_mutex_lock(m)		a_mutex_lock(m)
#define g_mutex_unlock(m)	a_mutex_unlock(m)


/*
** Local Mutexes
*/

#if defined(THREAD_SAFE)

typedef a_mutex_t l_mutex_t;

#define l_mutex_init(m)		a_mutex_init(m)
#define l_mutex_lock(m)		a_mutex_lock(m)
#define l_mutex_unlock(m)	a_mutex_unlock(m)

#else /* THREAD_SAFE */

typedef int l_mutex_t;

#if defined(INTERRUPT_SAFE)

#define l_mutex_init(m)
#define l_mutex_lock(m)		pds_disable_int()
#define l_mutex_unlock(m)	pds_enable_int()

#else /* INTERRUPT_SAFE */

#define l_mutex_init(m)
#define l_mutex_lock(m)
#define l_mutex_unlock(m)

#endif /* INTERRUPT_SAFE */

#endif /* THREAD_SAFE */


/*
** Memory System
*/

#define PDS_LINE_SIZE	128	/* PDS Virtual Cache Line Size         */

typedef struct heap_descriptor pds_heap_descriptor_t;

#if defined(__STDC__)
extern char * pds_mem_init(char * file,
			   char * address,
			   pds_size_t size,
			   pds_heap_descriptor_t * descriptor,
			   unsigned option);
#else /* __STDC__ */
extern char * pds_mem_init();
#endif /* __STDC__ */

#define pds_mem_base()			   shared_mem_base()
#define pds_mem_release(dscr)		   shared_mem_release(dscr)
#define pds_mem_alloc(dscr,size)	   h_alloc(dscr,size)
#define pds_mem_free(dscr,addr)		   h_free(dscr,addr)
#define pds_mem_alloc_size(dscr,size)	   alloc_size(dscr,size)
#define pds_mem_free_size(dscr,addr,size)  free_size(dscr,addr,size)


#endif /* _PDS_MEM_H_ */
