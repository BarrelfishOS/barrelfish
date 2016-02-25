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
**      System: BMSG
**        File: bmsg.msg.c
**      Author: Kees Schuerman
**      SccsId: "@(#)bmsg.msg.c	1.11 14 Nov 1995"
** Description: Basic Message Passing Libary
***********************************************************************
** A parallel distributed system is viewed as consisting of a set of
** processes and a collection of shared memory domains. Every process
** is identified by a unique identifier, the bport identifier, and
** belongs to one of the shared memory domains. In the implementation
** of the communication mechanisms the following distinctions are
** made:
**		intra-domain   vs  inter-domain
**		intra-family   vs  inter-family
**		intra-machine  vs  inter-machine
**
**  Intra-domain: Message passing is based on shared memory.
**  Intra-family: Peers are familiar to one another, they
**		  use the same data representation such that
**		  message passing does not require any data 
**		  representation conversions .
** Inter-machine: Message passing is based on TCP/IP.
***********************************************************************/

#include "machine.h"    /* architecture specific constant definitions */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/socket.h>
/*
#include <sys/uio.h>
*/

#if defined(SIGIO_FIOASYNC)
#include <sys/ioctl.h>
#endif

#if defined(SIGIO_SETSIG)
#include <stropts.h>
#endif

#include <signal.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <memory.h>
#include <string.h>
#include <errno.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.error.h"
#include "pds.mem.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"


/**********************************************************************
** BMSG Version
***********************************************************************/

#define BMSG_VERSION_MAJOR	0
#define BMSG_VERSION_MINOR	1



/**********************************************************************
** Some Global Variables
***********************************************************************/

/*
** Global Mutex
*/

static l_mutex_t bmsg_mutex;

#define Bmsg_Lock()	l_mutex_lock(&bmsg_mutex)
#define Bmsg_Unlock()	l_mutex_unlock(&bmsg_mutex)


/*
** Initialising, Exiting
*/

int bmsg_initialised = 0;
static int bmsg_initialising = 0;
int bmsg_exited = 0;
int bmsg_exiting = 0;



/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(NDEBUG)
#define bmsg_assert(ex)
#else
#define bmsg_assert(ex) {					\
    if (!(ex)) {						\
	(void) fprintf(stderr,					\
	       "PDS MPS-B Assertion Failed:");			\
	(void) fprintf(stderr, " file \"%s\"", __FILE__);	\
	(void) fprintf(stderr, " line %d\n", __LINE__);		\
	exit(1);						\
    }								\
}
#endif /* NDEBUG */

#define bmsg_assert_always() {					\
	(void) fprintf(stderr,					\
	       "PDS MPS-B Assertion Failed:");			\
	(void) fprintf(stderr, " file \"%s\"", __FILE__);	\
	(void) fprintf(stderr, " line %d\n", __LINE__);		\
	exit(1);						\
}

#define bmsg_perror_and_assert_always(s) {			\
	(void) perror(s);					\
	bmsg_assert_always();					\
}



/**********************************************************************
** Runtime Logging
***********************************************************************/

static bmsg_bool_t bmsg_alog_on = 0;
static bmsg_bool_t bmsg_alog_open = 0;
static bmsg_bool_t bmsg_alog_close = 0;
static bmsg_bool_t bmsg_alog_master = 0;


#if !defined(BMSGLOG)

#define bmsg_initlog(option)
#define bmsg_openlog()
#define bmsg_closelog()

#define BMSGLOG_SEND_MESSAGE(receiver)
#define BMSGLOG_SEND_PORT_OPEN(receiver)
#define BMSGLOG_SEND_PORT_CLOSE(receiver)
#define BMSGLOG_SEND_PORT_BLOCK(receiver)
#define BMSGLOG_SEND_PORT_UNBLOCK(receiver)
#define BMSGLOG_SEND_PORT_DIE(receiver)
#define BMSGLOG_SEND_MEM_PUT(receiver)
#define BMSGLOG_SEND_MEM_GET(receiver)
#define BMSGLOG_SEND_PORT_OPEN_ACK(receiver)
#define BMSGLOG_SEND_PORT_CLOSE_ACK(receiver)
#define BMSGLOG_SEND_PORT_BLOCK_ACK(receiver)
#define BMSGLOG_SEND_PORT_UNBLOCK_ACK(receiver)
#define BMSGLOG_SEND_MEM_PUT_ACK(receiver)
#define BMSGLOG_SEND_MEM_GET_ACK(receiver)
#define BMSGLOG_RCV_MESSAGE(sender)
#define BMSGLOG_RCV_PORT_OPEN(sender)
#define BMSGLOG_RCV_PORT_CLOSE(sender)
#define BMSGLOG_RCV_PORT_BLOCK(sender)
#define BMSGLOG_RCV_PORT_UNBLOCK(sender)
#define BMSGLOG_RCV_PORT_DIE(sender)
#define BMSGLOG_RCV_MEM_PUT(sender)
#define BMSGLOG_RCV_MEM_GET(sender)
#define BMSGLOG_RCV_PORT_OPEN_ACK(sender)
#define BMSGLOG_RCV_PORT_CLOSE_ACK(sender)
#define BMSGLOG_RCV_PORT_BLOCK_ACK(sender)
#define BMSGLOG_RCV_PORT_UNBLOCK_ACK(sender)
#define BMSGLOG_RCV_MEM_PUT_ACK(sender)
#define BMSGLOG_RCV_MEM_GET_ACK(sender)

#endif /* BMSGLOG */


#if (defined(BMSGLOG) && !defined(ALOG_TRACE))

static void
bmsg_initlog(option)
    bmsg_option_t option;
{
    return;
}

static void 
bmsg_openlog()
{
    setbuf(LOG_INFO,(char *) 0);
}

static void 
bmsg_closelog()
{
    return;
}

#undef  LOG_INFO
#define LOG_INFO        stdout

#define BMSGLOG_SEND_MESSAGE(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND MESSAGE --> %d \n",		\
		bport_self(),receiver); 				\
}
#define BMSGLOG_SEND_PORT_OPEN(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_OPEN --> %d \n",		\
		bport_self(),receiver);					\
}
#define BMSGLOG_SEND_PORT_CLOSE(receiver) {				\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_CLOSE --> %d \n",		\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_BLOCK(receiver) {				\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_BLOCK --> %d \n",		\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_UNBLOCK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_UNBLOCK --> %d \n",	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_DIE(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_DIE --> %d \n",	   	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_MEM_PUT(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND MEM_PUT --> %d \n",	   	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_MEM_GET(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND MEM_GET --> %d \n",	   	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_OPEN_ACK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_OPEN_ACK --> %d \n",   	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_CLOSE_ACK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_CLOSE_ACK --> %d \n",   	\
		bport_self(),receiver);		 		        \
}
#define BMSGLOG_SEND_PORT_BLOCK_ACK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_BLOCK_ACK --> %d \n",   	\
		bport_self(),receiver);		 		       	\
}
#define BMSGLOG_SEND_PORT_UNBLOCK_ACK(receiver) {		   	\
	fprintf(LOG_INFO,"%d: BMSG SEND PORT_UNBLOCK_ACK --> %d \n",  	\
		bport_self(),receiver);		 		       	\
}
#define BMSGLOG_SEND_MEM_PUT_ACK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND MEM_PUT_ACK --> %d \n",   	\
		bport_self(),receiver);		 		       	\
}
#define BMSGLOG_SEND_MEM_GET_ACK(receiver) {			   	\
	fprintf(LOG_INFO,"%d: BMSG SEND MEM_GET_ACK --> %d \n",   	\
		bport_self(),receiver);		 		       	\
}
#define BMSGLOG_RCV_MESSAGE(sender) {				   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV MESSAGE <-- %d \n",		\
		bport_self(),sender);	   				\
}
#define BMSGLOG_RCV_PORT_OPEN(sender) {				   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_OPEN <-- %d \n",		\
		bport_self(),sender);   				\
}
#define BMSGLOG_RCV_PORT_CLOSE(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_CLOSE <-- %d \n",		\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_BLOCK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_BLOCK <-- %d \n",		\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_UNBLOCK(sender) {		   		\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_UNBLOCK <-- %d \n",	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_DIE(sender) {		   		   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_DIE <-- %d \n",	   	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_MEM_PUT(sender) {				   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV MEM_PUT <-- %d \n",		\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_MEM_GET(sender) {				   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV MEM_GET <-- %d \n",		\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_OPEN_ACK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_OPEN_ACK <-- %d \n",	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_CLOSE_ACK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_CLOSE_ACK <-- %d \n",	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_BLOCK_ACK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_BLOCK_ACK <-- %d \n",	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_PORT_UNBLOCK_ACK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV PORT_UNBLOCK_ACK <-- %d \n",	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_MEM_PUT_ACK(sender) {			       	\
	fprintf(LOG_INFO,"%d: BMSG  RCV MEM_PUT_ACK <-- %d \n",	   	\
		bport_self(),sender);  					\
}
#define BMSGLOG_RCV_MEM_GET_ACK(sender) {			   	\
	fprintf(LOG_INFO,"%d: BMSG  RCV MEM_GET_ACK <-- %d \n",	   	\
		bport_self(),sender);  					\
}

#endif /* BMSGLOG && !ALOG_TRACE */


#if (defined(BMSGLOG) && defined(ALOG_TRACE))

#include "alog.h"

#define BMSG_ALOG_EVENT_BASE			1
#define ALOG_EVENT_SEND_BMSG			(BMSG_ALOG_EVENT_BASE+0)
#define ALOG_EVENT_RCV_BMSG 			(BMSG_ALOG_EVENT_BASE+1)
#define ALOG_EVENT_SEND_PORT_OPEN		(BMSG_ALOG_EVENT_BASE+2)
#define ALOG_EVENT_SEND_PORT_CLOSE		(BMSG_ALOG_EVENT_BASE+3)
#define ALOG_EVENT_SEND_PORT_BLOCK		(BMSG_ALOG_EVENT_BASE+4)
#define ALOG_EVENT_SEND_PORT_UNBLOCK		(BMSG_ALOG_EVENT_BASE+5)
#define ALOG_EVENT_SEND_PORT_DIE    		(BMSG_ALOG_EVENT_BASE+6)
#define ALOG_EVENT_SEND_PORT_OPEN_ACK		(BMSG_ALOG_EVENT_BASE+7)
#define ALOG_EVENT_SEND_PORT_CLOSE_ACK		(BMSG_ALOG_EVENT_BASE+8)
#define ALOG_EVENT_SEND_PORT_BLOCK_ACK		(BMSG_ALOG_EVENT_BASE+9)
#define ALOG_EVENT_SEND_PORT_UNBLOCK_ACK	(BMSG_ALOG_EVENT_BASE+10)
#define ALOG_EVENT_RCV_PORT_OPEN		(BMSG_ALOG_EVENT_BASE+11)
#define ALOG_EVENT_RCV_PORT_CLOSE		(BMSG_ALOG_EVENT_BASE+12)
#define ALOG_EVENT_RCV_PORT_BLOCK		(BMSG_ALOG_EVENT_BASE+13)
#define ALOG_EVENT_RCV_PORT_UNBLOCK		(BMSG_ALOG_EVENT_BASE+14)
#define ALOG_EVENT_RCV_PORT_DIE			(BMSG_ALOG_EVENT_BASE+15)
#define ALOG_EVENT_RCV_PORT_OPEN_ACK		(BMSG_ALOG_EVENT_BASE+16)
#define ALOG_EVENT_RCV_PORT_CLOSE_ACK		(BMSG_ALOG_EVENT_BASE+17)
#define ALOG_EVENT_RCV_PORT_BLOCK_ACK		(BMSG_ALOG_EVENT_BASE+18)
#define ALOG_EVENT_RCV_PORT_UNBLOCK_ACK		(BMSG_ALOG_EVENT_BASE+19)
#define ALOG_EVENT_SEND_MEM_PUT			(BMSG_ALOG_EVENT_BASE+20)
#define ALOG_EVENT_SEND_MEM_GET			(BMSG_ALOG_EVENT_BASE+21)
#define ALOG_EVENT_SEND_MEM_PUT_ACK		(BMSG_ALOG_EVENT_BASE+22)
#define ALOG_EVENT_SEND_MEM_GET_ACK		(BMSG_ALOG_EVENT_BASE+23)
#define ALOG_EVENT_RCV_MEM_PUT			(BMSG_ALOG_EVENT_BASE+24)
#define ALOG_EVENT_RCV_MEM_GET			(BMSG_ALOG_EVENT_BASE+25)
#define ALOG_EVENT_RCV_MEM_PUT_ACK		(BMSG_ALOG_EVENT_BASE+26)
#define ALOG_EVENT_RCV_MEM_GET_ACK		(BMSG_ALOG_EVENT_BASE+27)

static void
bmsg_initlog(option)
    bmsg_option_t option;
{
    if (option & BMSG_ALOG_ON) bmsg_alog_on = 1;
    if (option & BMSG_ALOG_OPEN) bmsg_alog_open = 1;
    if (option & BMSG_ALOG_CLOSE) bmsg_alog_close = 1;
    if (option & BMSG_ALOG_MASTER) bmsg_alog_master = 1;
}

static void
bmsg_openlog()
{
    if (bmsg_alog_on) {
	if (bmsg_alog_open) {
	    if (bmsg_alog_master) { 
		ALOG_MASTER(bport_self(),ALOG_TRUNCATE);
	    }
	    else {
	        ALOG_SETUP(bport_self(),ALOG_TRUNCATE);
	    }
	}
	if (bmsg_alog_master) {
	    ALOG_DEFINE(ALOG_EVENT_SEND_BMSG,
			"-> MSG","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_BMSG,
			"<- MSG","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_OPEN,
			"-> OPEN ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_CLOSE,
			"-> CLOSE ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_BLOCK,
			"-> BLOCK ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_UNBLOCK,
			"-> UNBLOCK ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_DIE,
			"-> DIE ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_OPEN_ACK,
			"-> OPEN !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_CLOSE_ACK,
			"-> CLOSE !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_BLOCK_ACK,
			"-> BLOCK !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_PORT_UNBLOCK_ACK,
			"-> UNBLOCK !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_OPEN,
			"<- OPEN ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_CLOSE,
			"<- CLOSE ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_BLOCK,
			"<- BLOCK ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_UNBLOCK,
			"<- UNBLOCK ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_DIE,
			"<- DIE ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_OPEN_ACK,
			"<- OPEN !","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_CLOSE_ACK,
			"<- CLOSE !","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_BLOCK_ACK,
			"<- BLOCK !","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_PORT_UNBLOCK_ACK,
			"<- UNBLOCK !","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_MEM_PUT,
			"-> PUT ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_MEM_GET,
			"-> GET ?","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_MEM_PUT_ACK,
			"-> PUT !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_SEND_MEM_GET_ACK,
			"-> GET !","-> PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_MEM_PUT,
			"<- PUT ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_MEM_GET,
			"<- GET ?","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_MEM_PUT_ACK,
			"<- PUT !","<- PROC %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_MEM_GET_ACK,
			"<- GET !","<- PROC %d");
	}
    }
}

static void
bmsg_closelog() 
{
    if (bmsg_alog_on && bmsg_alog_close) { 
	ALOG_OUTPUT;
    }
}

#define BMSGLOG_SEND_MESSAGE(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_BMSG,			\
   		     receiver,"MSG");					\
	}								\
}
#define BMSGLOG_SEND_PORT_OPEN(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_OPEN,		\
   		     receiver,"OPEN ?");				\
	}								\
}
#define BMSGLOG_SEND_PORT_CLOSE(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_CLOSE,		\
   		     receiver,"CLOSE ?");				\
	}								\
}
#define BMSGLOG_SEND_PORT_BLOCK(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_BLOCK,		\
   		     receiver,"BLOCK ?");				\
	}								\
}
#define BMSGLOG_SEND_PORT_UNBLOCK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_UNBLOCK,		\
   		     receiver,"UNBLOCK ?");				\
	}								\
}
#define BMSGLOG_SEND_PORT_DIE(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_DIE,		\
   		     receiver,"DIE ?");					\
	}								\
}
#define BMSGLOG_SEND_MEM_PUT(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_MEM_PUT,		\
   		     receiver,"PUT ?");					\
	}								\
}
#define BMSGLOG_SEND_MEM_GET(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_MEM_GET,		\
   		     receiver,"GET ?");					\
	}								\
}
#define BMSGLOG_SEND_PORT_OPEN_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_OPEN_ACK,	\
   		     receiver,"OPEN !");				\
	}								\
}
#define BMSGLOG_SEND_PORT_CLOSE_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_CLOSE_ACK,	\
   		     receiver,"CLOSE !");				\
	}								\
}
#define BMSGLOG_SEND_PORT_BLOCK_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_BLOCK_ACK,	\
   		     receiver,"BLOCK !");				\
	}								\
}
#define BMSGLOG_SEND_PORT_UNBLOCK_ACK(receiver) {			\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_PORT_UNBLOCK_ACK,	\
   		     receiver,"UNBLOCK !");				\
	}								\
}
#define BMSGLOG_SEND_MEM_PUT_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_MEM_PUT_ACK,		\
   		     receiver,"PUT !");					\
	}								\
}
#define BMSGLOG_SEND_MEM_GET_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_SEND_MEM_GET_ACK,		\
   		     receiver,"GET !");					\
	}								\
}
#define BMSGLOG_RCV_MESSAGE(sender) {					\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_BMSG,			\
	    sender,"MSG");				   		\
	}							   	\
}
#define BMSGLOG_RCV_PORT_OPEN(receiver) {			        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_OPEN,		\
   		     receiver,"OPEN ?");				\
	}								\
}
#define BMSGLOG_RCV_PORT_CLOSE(receiver) {		        	\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_CLOSE,		\
   		     receiver,"CLOSE ?");				\
	}								\
}
#define BMSGLOG_RCV_PORT_BLOCK(receiver) {		        	\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_BLOCK,		\
   		     receiver,"BLOCK ?");				\
	}								\
}
#define BMSGLOG_RCV_PORT_UNBLOCK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_UNBLOCK,		\
   		     receiver,"UNBLOCK ?");				\
	}								\
}
#define BMSGLOG_RCV_PORT_DIE(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_DIE,		\
   		     receiver,"DIE ?");					\
	}								\
}
#define BMSGLOG_RCV_MEM_PUT(receiver) {				        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_MEM_PUT,		\
   		     receiver,"PUT ?");					\
	}								\
}
#define BMSGLOG_RCV_MEM_GET(receiver) {				        \
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_MEM_GET,		\
   		     receiver,"GET ?");					\
	}								\
}
#define BMSGLOG_RCV_PORT_OPEN_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_OPEN_ACK,		\
   		     receiver,"OPEN !");				\
	}								\
}
#define BMSGLOG_RCV_PORT_CLOSE_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_CLOSE_ACK,	\
   		     receiver,"CLOSE !");				\
	}								\
}
#define BMSGLOG_RCV_PORT_BLOCK_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_BLOCK_ACK,	\
   		     receiver,"BLOCK !");				\
	}								\
}
#define BMSGLOG_RCV_PORT_UNBLOCK_ACK(receiver) {			\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_PORT_UNBLOCK_ACK,	\
   		     receiver,"UNBLOCK !");				\
	}								\
}
#define BMSGLOG_RCV_MEM_PUT_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_MEM_PUT_ACK,		\
   		     receiver,"PUT !");					\
	}								\
}
#define BMSGLOG_RCV_MEM_GET_ACK(receiver) {				\
	if (bmsg_alog_on) {						\
	    ALOG_LOG(bport_self(),ALOG_EVENT_RCV_MEM_GET_ACK,		\
   		     receiver,"GET !");					\
	}								\
}

#endif /* BMSGLOG && ALOG_TRACE */



/**********************************************************************
** Memory Management
***********************************************************************/

/*
** Configuration
** -------------
** To avoid false sharing and its implied performance degradation 
** one should set BMSG_LINE_SIZE to a multiple of the cache line 
** size.
*/

#define BMSG_LINE_SIZE          PDS_LINE_SIZE


/*
** Shared Memory Heap
*/

#define BMSG_HEAP_SIZE_MIN	0x100000

static pds_heap_descriptor_t bmsg_shd;


/*
** Private Memory Heap
*/

#define bmsg_phd bmsg_shd



/**********************************************************************
** Messages: <bmsg_header><bmsg_data>
***********************************************************************
** Messages consist of a fixed sized header and the message data. The
** size of the header is a multiple of 8 bytes. Messages are stored in
** message buffers.
***********************************************************************/

typedef pds_word_t bmsg_itype_t;

#define xdr_bmsg_itype           xdr_pds_word


/*
** Message Header
*/

typedef struct {
    bmsg_itype_t bmsg_type;	/* message type		         */
    bport_id_t  bport_id;	/* sender 		         */
    bmsg_size_t bmsg_data_size; /* message size	(without header) */
    pds_word_t  pad;            /* ensures 8-byte alignment      */
} bmsg_header_t;


/*
** Message Type Masks
*/

#define BMSG_PORT                       0x1000
#define BMSG_MEM                        0x2000

#define BMSG_OPEN                       0x0001
#define BMSG_OPEN_ACK                   0x0002
#define BMSG_CLOSE                      0x0004
#define BMSG_CLOSE_ACK                  0x0008
#define BMSG_BLOCK                      0x0010
#define BMSG_BLOCK_ACK                  0x0020
#define BMSG_UNBLOCK                    0x0040
#define BMSG_UNBLOCK_ACK                0x0080
#define BMSG_DIE                        0x0100

#define BMSG_PUT                        0x0001
#define BMSG_PUT_ACK                    0x0002
#define BMSG_GET                        0x0004
#define BMSG_GET_ACK                    0x0008


/*
** Message Types
*/

#define BMSG_MSG                        0x4000

#define BMSG_PORT_OPEN                  (BMSG_PORT | BMSG_OPEN)
#define BMSG_PORT_OPEN_ACK              (BMSG_PORT | BMSG_OPEN_ACK)
#define BMSG_PORT_CLOSE                 (BMSG_PORT | BMSG_CLOSE)
#define BMSG_PORT_CLOSE_ACK             (BMSG_PORT | BMSG_CLOSE_ACK)
#define BMSG_PORT_BLOCK                 (BMSG_PORT | BMSG_BLOCK)
#define BMSG_PORT_BLOCK_ACK             (BMSG_PORT | BMSG_BLOCK_ACK)
#define BMSG_PORT_UNBLOCK               (BMSG_PORT | BMSG_UNBLOCK)
#define BMSG_PORT_UNBLOCK_ACK           (BMSG_PORT | BMSG_UNBLOCK_ACK)

#define BMSG_MEM_PUT                    (BMSG_MEM | BMSG_PUT)
#define BMSG_MEM_PUT_ACK                (BMSG_MEM | BMSG_PUT_ACK)
#define BMSG_MEM_GET                    (BMSG_MEM | BMSG_GET)
#define BMSG_MEM_GET_ACK                (BMSG_MEM | BMSG_GET_ACK)

#define BMSG_PORT_DIE                   (BMSG_PORT | BMSG_DIE)


/*
** System messages are used for operations on ports and memory. These
** messages may contain addresses which have to align with a machine
** word. Since the header is ensured to align on 8-byte words address
** alignment is guaranteed by positioning them at the beginning of the
** the message data, i.e. just following the header. Note that this
** assumes that the machine word size is either 4 or 8 bytes.
*/


/* 
** Port Operation Messages: <bmsg_header><bport_request>
**			    <bmsg_header><bport_reply>
*/

typedef struct {
    bmsg_address_t msg_queue_address;
    bdomain_id_t bdomain_id;
    bpid_t bpid;
    bnet_address_t bnet_address;
    bport_number_t bport_number;
} bport_request_t;

typedef struct {
    bmsg_ret_t bmsg_ret;
} bport_reply_t;


/* 
** Memory Operation Messages: <bmsg_header><bmem_request>[bmem_data]
**			      <bmsg_header><bmem_request>[bmem_data]
*/

typedef struct {
    bmsg_address_t bmem_src_address;
    bmsg_address_t bmem_dst_address;
    bmsg_size_t bmem_size;
    bmem_id_t bmem_id;
} bmem_request_t;

typedef struct {
    bmsg_address_t bmem_src_address;
    bmsg_address_t bmem_dst_address;
    bmem_id_t bmem_id;
    bmsg_ret_t bmsg_ret;
} bmem_reply_t;


/*
** Addresses
** ---------
** The exchange of addresses is based on the representing them in a 
** 64-bit bmsg_address_t. Addresses are invalid if they refer to 
** locations outside the address space. This can for example happen if
** a 64-bit machine sends an address to a 32-bit machine (see memory 
** operations below). The validity of addresses can however be checked 
** by the macro BmsgAddressOk();
*/

/*
** int BmsgAddressOk(bmsg_address_t * msg_address)
*/

#define BmsgAddressOk(msg_address)					\
	((sizeof(bmem_address_t) == sizeof(pds_word_t)) ?		\
	  ((msg_address)->high == (pds_word_t) 0) : 1)



/**********************************************************************
** Message Buffers
***********************************************************************
** Message buffers have a size that is a multiple of the bmsg line size
** and they are aligned on an 8-byte boundary (ensured by the memory
** allocator). Both the message header and the message data are therefore
** aligned on an 8-byte boundary which eases their manipulation.
** It is expected that short messages are being transferred far more
** frequently than large messages. We have therefore decided to maintain
** a special pool of buffers for short messages, i.e. messages with a size
** less or equal than BUF_DATA_BYTES.
** Buffers for long messages are allocated on demand and they are freed
** (i.e. returned to the shared memory heap) as soon as they are not
** needed any longer.
** Message buffer reusage is supported by the buffer data structure
** carrying its size, i.e. the number of data bytes it can store.
** Note that in addition to space for the actual message, the buffers
** contain also a so called scratch area.
***********************************************************************/

/*
** Configuration
** -------------
** BUF_SIZE_LINES: Size (in bmsg lines) of small message buffer. 
**    BUF_SCRATCH: Size of message buffer scratch area.
**      BUF_BYTES: Size of small message buffer.
** BUF_DATA_BYTES: Size of small message buffer area for message data.
**                 BUF_BYTES >= (BUF_SCRATCH + sizeof(pds_word_t) +
**                              sizeof(bmsg_header_t))
**                           == multiple of sizeof(pds_word_t)
*/

#define BUF_SIZE_LINES	1

#define BUF_SCRATCH     BMSG_BUF_SCRATCH_BYTES

#define BUF_PAD_BYTES   ((BUF_SCRATCH + sizeof(pds_word_t) +    \
                          sizeof(bmsg_header_t)) % 8)

#define BUF_PAD_BITS (8 * BUF_PAD_BYTES)

#define BUF_BYTES       (BUF_SIZE_LINES * BMSG_LINE_SIZE)

#define BUF_DATA_BYTES                                          \
        (pds_word_round(BUF_BYTES) -                            \
         sizeof(struct {                                        \
                    pds_byte_t scratch[BUF_SCRATCH];            \
                    pds_word_t buf_data_bytes;                  \
                    bmsg_header_t header;                       \
                    unsigned    : BUF_PAD_BITS;                 \
                    pds_byte_t data[sizeof(pds_word_t)];        \
                 }) + sizeof(pds_word_t))


typedef struct bmsg_buffer {
    struct bmsg_buffer * next;          /* next message buffer  */
    bmsg_bool_t familiar;	        /* familiarity 		*/
    pds_byte_t scratch[BUF_SCRATCH -
                       sizeof(void_ptr) - sizeof(bmsg_bool_t)];
    pds_word_t buf_data_bytes;
    bmsg_header_t header;
    unsigned    : BUF_PAD_BITS;         /* padding              */
    pds_byte_t data[BUF_DATA_BYTES];    /* message data         */
} bmsg_buffer_t;



/**********************************************************************
** Message Buffer Management
***********************************************************************
** The allocation and freeing of message buffers is optimized by
** maintaining a pool of small buffers which is expected to
** be able to serve the majority of the bmsg_alloc() and bmsg_free()
** invocations.
** The number of buffers in the buffer pool can be adjusted by
** calling bmsg_buffer_pool_adjust(). Its operation is configured
** by the following constants:
**
**                      BUF_POOL_SIZE_MIN
**                      BUF_POOL_SIZE_MID
**                      BUF_POOL_SIZE_MAX
**
** The number of buffers in buffer pool will be adjusted to
** BUF_POOL_SIZE_MID when the number of buffers becomes larger
** than BUF_POOL_SIZE_MAX or smaller than BUF_POOL_SIZE_MIN.
***********************************************************************/

/*
** Configuration
*/

#define BUF_POOL_SIZE_MIN       8
#define BUF_POOL_SIZE_MID       16
#define BUF_POOL_SIZE_MAX       32


typedef struct {
    l_mutex_t mutex;
    unsigned size;
    bmsg_buffer_t *buffer;      /* first buffer    */
} bmsg_buffer_pool_t;

static bmsg_buffer_pool_t bmsg_buffer_pool;


#if !defined(NDEBUG)

static bmsg_ret_t
bmsg_buffer_pool_check()
{
    bmsg_buffer_t *buffer;
    unsigned size = 0;

    buffer = bmsg_buffer_pool.buffer;
    while (buffer) {
        size++;
        buffer = buffer->next;
    }

    if (size == bmsg_buffer_pool.size)
        return(BMSG_OK);
    else
        return(BMSG_ERROR);
}

#endif /* NDEBUG */


#define MBP_Lock()      l_mutex_lock(&bmsg_buffer_pool.mutex)
#define MBP_Unlock()    l_mutex_unlock(&bmsg_buffer_pool.mutex)

#define MBP_Size        (bmsg_buffer_pool.size)


/* void MBP_Get(bmsg_buffer_t * * b)                            */

#define MBP_Get(b) {                                             \
        bmsg_assert(b);                                          \
        bmsg_assert(bmsg_buffer_pool.size > 0);                  \
        bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);        \
        *(b) = bmsg_buffer_pool.buffer;                          \
        bmsg_buffer_pool.buffer = bmsg_buffer_pool.buffer->next; \
        bmsg_buffer_pool.size--;                                 \
        bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);        \
}


/* void MBP_Put(bmsg_buffer_t * b)                              */

#define MBP_Put(b) {                                             \
        bmsg_assert(b);                                          \
        bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);        \
        (b)->next = bmsg_buffer_pool.buffer;                     \
        bmsg_buffer_pool.buffer = (b);                           \
        bmsg_buffer_pool.size++;                                 \
        bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);        \
}


static bmsg_ret_t
bmsg_buffer_pool_adjust(size)
    unsigned size;
{
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);

    if ((size >= BUF_POOL_SIZE_MIN) &&
        (size <= BUF_POOL_SIZE_MAX) &&
        (MBP_Size >= BUF_POOL_SIZE_MIN) &&
        (MBP_Size <= BUF_POOL_SIZE_MAX))
        return(BMSG_OK);

    while (MBP_Size < size) {
           buffer = (bmsg_buffer_t *)
                    pds_mem_alloc_size(&bmsg_shd,sizeof(bmsg_buffer_t));
           if (!buffer)
                return(BMSG_NOMEMORY);
           else {
                buffer->buf_data_bytes = BUF_DATA_BYTES;
                MBP_Put(buffer);
           }
    }

    while (MBP_Size > size) {
        MBP_Get(&buffer);
        pds_mem_free_size(&bmsg_shd,(void_ptr) buffer,sizeof(bmsg_buffer_t));
    }

    bmsg_assert(bmsg_buffer_pool_check() == BMSG_OK);

    return(BMSG_OK);
}


static bmsg_ret_t
bmsg_buffer_pool_init()
{
    l_mutex_init(&bmsg_buffer_pool.mutex);
    bmsg_buffer_pool.size = 0;
    bmsg_buffer_pool.buffer = (bmsg_buffer_t *) 0;
    return(bmsg_buffer_pool_adjust(BUF_POOL_SIZE_MID));
}


static bmsg_ret_t
bmsg_sbuffer_alloc(buffer)
    bmsg_buffer_t * * buffer;
{
    bmsg_buffer_t * buf;

    MBP_Lock();
    if (MBP_Size) {
        MBP_Get(buffer);
        MBP_Unlock();
        return(BMSG_OK);
    }
    else {
        MBP_Unlock();
        buf = (bmsg_buffer_t *)
              pds_mem_alloc_size(&bmsg_shd,sizeof(bmsg_buffer_t));
        if (!buf)
            return(BMSG_NOMEMORY);
        else {
            buf->buf_data_bytes = BUF_DATA_BYTES;
            *buffer = buf;
            return(BMSG_OK);
        }
    }
}


static bmsg_ret_t
bmsg_buffer_alloc(size, buffer)
    unsigned size;
    bmsg_buffer_t * * buffer;
{
    bmsg_size_t buf_bytes;
    bmsg_size_t remainder;

    if (size > BUF_DATA_BYTES) {
        buf_bytes = sizeof(bmsg_buffer_t) - BUF_DATA_BYTES + size;
        remainder = buf_bytes % BMSG_LINE_SIZE;
        buf_bytes = remainder ?
                    buf_bytes + BMSG_LINE_SIZE - remainder :
                    buf_bytes;

        /* check for extremely large message size */
        if (buf_bytes < size)
            return(BMSG_NOMEMORY);

        *buffer = (bmsg_buffer_t *)
                  pds_mem_alloc_size(&bmsg_shd,buf_bytes);
        if (!*buffer)
            return(BMSG_NOMEMORY);
        else {
            (*buffer)->buf_data_bytes = buf_bytes - sizeof(bmsg_buffer_t) +
                                        BUF_DATA_BYTES;
            return(BMSG_OK);
        }
    }
    else
        return(bmsg_sbuffer_alloc(buffer));
}


static void
bmsg_sbuffer_free(buffer)
    bmsg_buffer_t * buffer;
{
    MBP_Lock();
    MBP_Put(buffer);
    MBP_Unlock();
}


static void
bmsg_buffer_free(buffer)
    bmsg_buffer_t * buffer;
{
    if (buffer->buf_data_bytes != BUF_DATA_BYTES) {
        pds_mem_free_size(&bmsg_shd,(void_ptr) buffer,
                          sizeof(bmsg_buffer_t) - BUF_DATA_BYTES +
                          buffer->buf_data_bytes);
    }
    else
        bmsg_sbuffer_free(buffer);
}



/**********************************************************************
** Message Queues
***********************************************************************
** A message queue is a singly linked list of message buffers. Each
** process has a global receive queue and an associated bport with 
** a local receive queue. The global receive queue is in shared memory
** and is thus visible by other processes of the shared memory domain
** as a send queue. When a process detects the arrival of one or more
** messages in the global receive queue, it copies all these messages 
** from the global receive queue grcv_queue (in shared memory) into the 
** local receive queue lrcv_queue (in private memory).
**
**					     ::
**       ___________	    ___________      ::     ___________
**      | | | |		   | | | |           ::    | | | |
**      | | | |  * * *	   | | | |  * * *    ::    | | | |  * * *
**      |_|_|_|_____       |_|_|_|_____      ::    |_|_|_|____      
**					     ::
**      app_queue          lrcv_queue        ::    grcv_queue 
**					     ::
**					     ::
**                         private memory <- :: -> shared memory
**
** There are two classes of messages: system messages and application
** messages. System messages are handled when they are taken from the 
** receive queue lrcv_queue. Application messages are handled by the
** application when convenient. To avoid blocking of system message
** handling, the application messages are queued in a dedicated message 
** queue, the so called app_queue.
** Sending a message to a bport in the same shared memory domain 
** involves putting a message in the bport's send queue. Application 
** messages are received by taking them from the app_queue or, when the 
** app_queue is empty, directly from the lrcv_queue. System messages are 
** always received by taking them from the lrcv_queue.
***********************************************************************/

static bmsg_bool_t bmsg_intra_domain_triggering;

typedef struct {
    l_mutex_t mutex;
    bmsg_buffer_t * first;	 /* first message buffer      */
    bmsg_buffer_t * last;	 /* last message buffer       */
} bmsg_queue_t;

typedef struct {
    g_mutex_t mutex;
    bmsg_buffer_t * first;	 /* first message buffer      */
    bmsg_buffer_t * last;	 /* last message buffer       */
    bmsg_option_t option;
} gmsg_queue_t;

static gmsg_queue_t * grcv_queue; /* global receive queue      */
static bmsg_queue_t lrcv_queue;   /* local receive queue       */
static bmsg_queue_t app_queue;    /* application message queue */


/* 
** Global Message Queue Options
*/

#define BMSG_TRIGGERING		0x01


#define LMQ_Lock(q)	l_mutex_lock(&(q)->mutex)
#define LMQ_Unlock(q)	l_mutex_unlock(&(q)->mutex)

#define GMQ_Lock(q)	g_mutex_lock(&(q)->mutex)
#define GMQ_Unlock(q)	g_mutex_unlock(&(q)->mutex)

#define MQ_Empty(q)	(!((q)->last))


/* void MQ_First(bmsg_queue_t * q, bmsg_buffer_t * * b) */
/* void MQ_First(gmsg_queue_t * q, bmsg_buffer_t * * b) */

#define MQ_First(q,b)	(*(b) = (q)->first)


/* void MQ_Put(bmsg_queue_t * q, bmsg_buffer_t * b) 	*/
/* void MQ_Put(gmsg_queue_t * q, bmsg_buffer_t * b) 	*/

#if defined(NDEBUG)
#define MQ_Put(q,b) {					\
	if (MQ_Empty(q))  				\
	    (q)->first = (b);				\
	else  						\
	    (q)->last->next = (b);			\
	(q)->last = (b);				\
}
#else
#define MQ_Put(q,b) {					\
	(b)->next = (bmsg_buffer_t *) 0;		\
	if (MQ_Empty(q))  				\
	    (q)->first = (b);				\
	else  						\
	    (q)->last->next = (b);			\
	(q)->last = (b);				\
}
#endif


/* void MQ_Get(bmsg_queue_t * q, bmsg_buffer_t * * b) 	*/
/* void MQ_Get(gmsg_queue_t * q, bmsg_buffer_t * * b) 	*/

#if defined(NDEBUG)
#define MQ_Get(q,b) {					\
	bmsg_assert(!MQ_Empty(q));			\
	*(b) = (q)->first; 				\
	if ((q)->first == (q)->last)			\
	    (q)->last = (bmsg_buffer_t *) 0;		\
	else						\
	    (q)->first = (q)->first->next;          	\
}
#else
#define MQ_Get(q,b) {					\
	bmsg_assert(!MQ_Empty(q));			\
	*(b) = (q)->first; 				\
	if ((q)->first == (q)->last)			\
	    (q)->last = (bmsg_buffer_t *) 0;		\
	else						\
	    (q)->first = (q)->first->next;          	\
        (*(b))->next = (bmsg_buffer_t *) 0;		\
}
#endif


/* void MQ_Move(bmsg_queue_t * d, bmsg_queue_t * s) 	*/
/* void MQ_Move(bmsg_queue_t * d, gmsg_queue_t * s) 	*/
/* void MQ_Move(gmsg_queue_t * d, gmsg_queue_t * s) 	*/
/* void MQ_Move(gmsg_queue_t * d, bmsg_queue_t * s) 	*/

#if defined(NDEBUG)
#define MQ_Move(d,s) {					\
	bmsg_assert(!MQ_Empty(s));			\
        bmsg_assert((d)->last != (s)->first);		\
        bmsg_assert((d)->last != (s)->last);		\
	if (MQ_Empty(d))				\
	    (d)->first = (s)->first;			\
	else 						\
	    (d)->last->next = (s)->first;		\
	(d)->last = (s)->first;				\
        if ((s)->first == (s)->last)			\
	    (s)->last = (bmsg_buffer_t *) 0;		\
	else						\
	    (s)->first = (s)->first->next;		\
}
#else
#define MQ_Move(d,s) {					\
	bmsg_assert(!MQ_Empty(s));			\
        bmsg_assert((d)->last != (s)->first);		\
        bmsg_assert((d)->last != (s)->last);		\
	if (MQ_Empty(d))				\
	    (d)->first = (s)->first;			\
	else 						\
	    (d)->last->next = (s)->first;		\
	(d)->last = (s)->first;				\
        if ((s)->first == (s)->last) {			\
	    (s)->last = (bmsg_buffer_t *) 0;		\
	    (s)->first = (bmsg_buffer_t *) 0;		\
	}						\
	else						\
	    (s)->first = (s)->first->next;		\
	(d)->last->next = (bmsg_buffer_t *) 0;		\
}
#endif


/* void MQ_MoveAll(bmsg_queue_t * d, bmsg_queue_t * s)  */
/* void MQ_MoveAll(bmsg_queue_t * d, gmsg_queue_t * s)  */
/* void MQ_MoveAll(gmsg_queue_t * d, bmsg_queue_t * s)  */
/* void MQ_MoveAll(gmsg_queue_t * d, gmsg_queue_t * s)  */

#if defined(NDEBUG)
#define MQ_MoveAll(d,s) {				\
	bmsg_assert(!MQ_Empty(s));			\
        bmsg_assert((d)->last != (s)->first);		\
        bmsg_assert((d)->last != (s)->last);		\
	if (MQ_Empty(d))				\
	    (d)->first = (s)->first;			\
	else						\
	    (d)->last->next = (s)->first;		\
	(d)->last = (s)->last;				\
	(s)->last = (bmsg_buffer_t *) 0;		\
	bmsg_assert(MQ_Empty(s));			\
}
#else
#define MQ_MoveAll(d,s) {				\
	bmsg_assert(!MQ_Empty(s));			\
        bmsg_assert((d)->last != (s)->first);		\
        bmsg_assert((d)->last != (s)->last);		\
	if (MQ_Empty(d))				\
	    (d)->first = (s)->first;			\
	else						\
	    (d)->last->next = (s)->first;		\
	(d)->last = (s)->last;				\
	(d)->last->next = (bmsg_buffer_t *) 0;		\
	(s)->first = (bmsg_buffer_t *) 0;		\
	(s)->last = (bmsg_buffer_t *) 0;		\
	bmsg_assert(MQ_Empty(s));			\
}
#endif


/* void MQ_Clear(bmsg_queue_t * q)  			*/
/* void MQ_Clear(gmsg_queue_t * q)  			*/

#define MQ_Clear(q) {					\
    bmsg_buffer_t * buffer;				\
    bmsg_size_t data_size;				\
							\
    while (!MQ_Empty(q)) {				\
	MQ_Get(q,&buffer);				\
        data_size = (bmsg_size_t) 			\
		    buffer->header.bmsg_data_size;	\
        if (data_size > BUF_DATA_BYTES)			\
	    pds_mem_free_size(&bmsg_shd,		\
			      (void_ptr) buffer,	\
		              sizeof(bmsg_buffer_t) - 	\
			      BUF_DATA_BYTES + 		\
			      data_size);		\
	else						\
	    pds_mem_free_size(&bmsg_shd,		\
			      (void_ptr) buffer,	\
			      sizeof(bmsg_buffer_t));	\
    }							\
}


static void 
local_message_queue_init(queue)
    bmsg_queue_t * queue;
{
    l_mutex_init(&queue->mutex);
    queue->first = (bmsg_buffer_t *) 0;
    queue->last = (bmsg_buffer_t *) 0;
}


static void 
global_message_queue_init(queue)
    gmsg_queue_t * queue;
{
    g_mutex_init(&queue->mutex);
    queue->option = (bmsg_option_t) 0;
    queue->first = (bmsg_buffer_t *) 0;
    queue->last = (bmsg_buffer_t *) 0;
}


static void 
global_message_queue_clear(queue)
    gmsg_queue_t * queue;
{
    MQ_Clear(queue);
}


static void 
local_message_queue_clear(queue)
    bmsg_queue_t * queue;
{
    MQ_Clear(queue);
}



/**********************************************************************
** Sockets 
***********************************************************************
** Inter-machine message passing is based on TCP/IP, i.e. internet 
** stream connections. Every process has therefore an associated 
** 'control' socket for accepting connections. Accepting a connection 
** involves the creation of a 'message' socket which is used for 
** sending/receiving messages to/from the remote peer. 
***********************************************************************/

/*
** Configuration
*/

#define SOCK_BACKLOG	32


static int ctrl_sock;		/* control socket		*/
static fd_set sockset;		/* sockset			*/
static long sockset_size;	/* size of sockset		*/
static l_mutex_t sockset_mutex;	/* protects sockset 		*/

#define SS_Lock()	l_mutex_lock(&sockset_mutex)
#define SS_Unlock()	l_mutex_unlock(&sockset_mutex)


static void
sockset_init()
{
    l_mutex_init(&sockset_mutex);
    FD_ZERO(&sockset);
    sockset_size = sysconf(_SC_OPEN_MAX);
}


static void
set_sock_ni(sock)
    int sock;
{
    bmsg_assert(sock > 0);

    /* don't inherit socket on exec() */
    if (fcntl(sock,F_SETFD,1) == -1)
        bmsg_perror_and_assert_always("set_sock_ni() : fcntl() ");
}


static void
set_sock_nb(sock)
    int sock;
{
    int stat;

    bmsg_assert(sock > 0);

    /* get socket descriptor status flag */
    stat = fcntl(sock,F_GETFL);
    if (stat == -1)
        bmsg_perror_and_assert_always("set_sock_nb() : fcntl() ");
    /* set socket to non-blocking */
    if (fcntl(sock,F_SETFL,stat|O_NONBLOCK) == -1) /* POSIX */
        bmsg_perror_and_assert_always("set_sock_nb() : fcntl() ");
}


static void
set_sock_as(sock)
    int sock;
{
    bmsg_assert(sock > 0);

#ifdef SIGIO_FASYNC
    {
        int stat;

        /* set the process receiving SIGIO/SIGURG signals to us */
        if (fcntl(sock,F_SETOWN,getpid()) == -1)
            bmsg_perror_and_assert_always("set_sock_as() : fcntl() ");
        /* get socket descriptor status flag */
        stat = fcntl(sock,F_GETFL);
        if (stat == -1)
            bmsg_perror_and_assert_always("set_sock_as() : fcntl() ");
        /* allow receipt of asynchronous I/O signals */
        if (fcntl(sock,F_SETFL,stat|FASYNC) == -1)
            bmsg_perror_and_assert_always("set_sock_as() : fcntl() ");
    }
#else
#ifdef SIGIO_FIOASYNC
    {
	int on = 1;
	int pid;

	/* set the process receiving SIGIO/SIGURG signals to us */
	pid = (int) getpid();
	if (ioctl(sock,SIOCSPGRP,&pid) == -1)
	    bmsg_perror_and_assert_always("set_sock_as() : ioctl() ");
        /* allow receipt of asynchronous I/O signals */
        if (ioctl(sock,FIOASYNC,&on) == -1)
	    bmsg_perror_and_assert_always("set_sock_as() : ioctl() ");
    }
#else
#ifdef SIGIO_SETSIG
    /* allow receipt of asynchronous I/O signals */
    if (ioctl(sock,I_SETSIG,S_RDNORM|S_RDBAND|S_HIPRI) == -1)
        bmsg_perror_and_assert_always("set_sock_as() : ioctl() ");
#else
    bmsg_assert_always();
#endif
#endif
#endif
}


static void 
set_sock_nd(sock)
    int sock;
{
    int on = 1;

    bmsg_assert(sock > 0);

    /* disable the default delay of TCP data transmission */ 
    if (setsockopt(sock,IPPROTO_TCP,TCP_NODELAY,
		   (char *) &on,sizeof(on)) == -1)
        bmsg_perror_and_assert_always("set_sock_nd() : setsockopt() ");
}


static void 
set_sock_ka(sock)
    int sock;
{
    int on;
    int len = sizeof(on);

    bmsg_assert(sock > 0);

    /* get connections alive status */ 
    if (getsockopt(sock,SOL_SOCKET,SO_KEEPALIVE,
		   (char *) &on,&len) == -1)
        bmsg_perror_and_assert_always("get_sock_ka() : getsockopt() ");
    if (on)
	return;
    /* toggle keep connections alive */
    on = 1;
    if (setsockopt(sock,SOL_SOCKET,SO_KEEPALIVE, 
	           (char *) &on,sizeof(on)) == -1)
        bmsg_perror_and_assert_always("set_sock_ka() : setsockopt() ");
}


static void 
set_sock_li(sock)
    int sock;
{
    struct linger li;

    bmsg_assert(sock > 0);

    li.l_onoff = 1;    /* switch lingering on     */
    li.l_linger = 10;  /* linger up to 10 seconds */

    /* linger to ensure data delivery before closing socket */ 
    if (setsockopt(sock,SOL_SOCKET,SO_LINGER,
		   (char *) &li,sizeof(li)) == -1)
        bmsg_perror_and_assert_always("set_sock_li() : setsockopt() ");
}


static int
write_sock(sock,buffer,size)
    int sock;
    char * buffer;
    unsigned size;
{
    unsigned tbytes;
    int nbytes;

    bmsg_assert(sock > 0);

    if (!size)
	return(0);

    tbytes = 0;
    while (tbytes < size) {
        nbytes = write(sock,
                       buffer + tbytes,
                       size - tbytes);
        if (nbytes >= 0)
            tbytes += nbytes;
        else if ((errno != EAGAIN) && (errno != EINTR)) { /* failure */
	    errno = EIO;
            return(-1);
	}
        /* else retry */
    }
    return(0);
}


static int
read_sock(sock,buffer,size)
    int sock;
    char * buffer;
    unsigned size;
{
    unsigned tbytes;
    int nbytes;

    bmsg_assert(sock > 0);

    if (!size)
	return(0);

    tbytes = 0;
    while (tbytes < size) {
        nbytes = read(sock,
                      buffer + tbytes,
                      size - tbytes);
        if (nbytes > 0) /* successful read */
            tbytes += nbytes;
        else if (nbytes == 0) { /* End Of Stream */
	    errno = EIO;
	    return(-1);
	}
	else if ((errno == EAGAIN) && (tbytes == 0))
	    return(-1);
	else if ((errno != EAGAIN) && (errno != EINTR)) { /* failure */
	    errno = EIO;
            return(-1);
	}
        /* else retry */
    }
    return(0);
}


/* 
**    Function: close_sock()
** Description: Closes socket and removes it from the sockset.
**        Note: The sockset lock should be hold on entering and is 
**		also hold on returning. 
*/

static void
close_sock(sock)
    int sock;
{
    int ret;

    bmsg_assert(sock >= 0);

    if (!sock || !(FD_ISSET(sock,&sockset)))
	return;

    FD_CLR(sock,&sockset);

    /*
    ** Although one would expect that sock is a valid file
    ** descriptor associated with a socket, it turns out that
    ** on some operating systems, i.e. at least on SunOS 4.1.3,
    ** an asynchronous I/O socket is closed when its asynchronous
    ** I/O socket peer is closed. When this turns out to be the
    ** case we just return.
    ** Furthermore, on SunOS 4.1.3, the file descriptors associated
    ** with asynchronous I/O sockets are not removed from the
    ** (global) file table on closing. We therefore disable the
    ** delivery of SIGIO signals to avoid filling up the file
    ** table.
    */

#ifdef SIGIO_FASYNC
    {
        int stat;

        /* get socket descriptor status flag */
        stat = fcntl(sock,F_GETFL);
        if (stat == -1)
	    return;
        /* disable receipt of asynchronous I/O signals */
        if (fcntl(sock,F_SETFL,stat&~FASYNC) == -1)
	    return;
    }
#else
#ifdef SIGIO_FIOASYNC
    {
        int off = 0;

        /* disable receipt of asynchronous I/O signals */
        if (ioctl(sock,FIOASYNC,&off) == -1)
	    return;
    }
#else
#ifdef SIGIO_SETSIG
    /* disable receipt of asynchronous I/O signals */
    if (ioctl(sock,I_SETSIG,0) == -1)
	return;
#else
    bmsg_assert_always();
#endif
#endif
#endif

    do {
	ret = close(sock);
    } while ((ret == -1) && (errno == EINTR));
}



/**********************************************************************
** Embryonic Sockets 
***********************************************************************
** The establishment of an inter-machine communication channel consists
** of (1) accepting a TCP/IP socket connection and 2) the creation of a
** a bport. The bport is created on receiving a bport_open message. 
** Until this event, the newly created message socket is in the embryonic
** state and stored in a so called embryonic socket list.
***********************************************************************/

typedef struct emb_sock {	/* embryonic socket 		*/
    struct emb_sock * next;
    struct emb_sock * prev;
    int sock;
    bnet_address_t bnet_address;
    bport_number_t bport_number;
    bport_id_t bport_id;
} emb_sock_t;

typedef struct {		/* list of embryonic sockets 	*/
    l_mutex_t mutex;
    emb_sock_t * first;		
    emb_sock_t * last;		
} emb_sock_list_t;

static emb_sock_list_t emb_sock_list;

#define EMB_Lock()	l_mutex_lock(&emb_sock_list.mutex)
#define EMB_Unlock()	l_mutex_unlock(&emb_sock_list.mutex)
#define EMB_Empty()	(emb_sock_list.last == (emb_sock_t *) 0)


static void 
emb_sock_list_init()
{
    l_mutex_init(&emb_sock_list.mutex);
    emb_sock_list.first = (emb_sock_t *) 0;
    emb_sock_list.last = (emb_sock_t *) 0;
}


static void
emb_sock_select(port_id,emb_sock)
    bport_id_t port_id;
    emb_sock_t * * emb_sock;
{
    emb_sock_t * sd;

    sd = emb_sock_list.first;
    while (sd && (sd->bport_id != port_id)) 
	sd = sd->next;
    *emb_sock = sd;
}


static void
emb_sock_destroy(emb_sock)
    emb_sock_t * * emb_sock;
{
    emb_sock_t * sd;

    sd = *emb_sock;
    *emb_sock = sd->next;
    if ((sd)->prev)
        (sd)->prev->next = (sd)->next;
    else
        emb_sock_list.first = (sd)->next;
    if ((sd)->next)
        (sd)->next->prev = (sd)->prev;
    else
        emb_sock_list.last = (sd)->prev;
    pds_mem_free(&bmsg_phd,(void_ptr) (sd));
}


static void
emb_sock_put(emb_sock)
    emb_sock_t * emb_sock;
{
    emb_sock->next = (emb_sock_t *) 0;
    if (EMB_Empty()) {
        emb_sock_list.first = emb_sock;
        emb_sock->prev = (emb_sock_t *) 0;
    }
    else {
        emb_sock_list.last->next = emb_sock;
        emb_sock->prev = emb_sock_list.last;
    }
    emb_sock_list.last = emb_sock;
}



/**********************************************************************
** Identifier Generator
***********************************************************************
** Identifiers are required by the asynchronous remote procedure calls 
** (RPC) bmem_put and bmem_get. Since a process may have multiple of 
** such RPCs outstanding, a mechanism is required to match replies with
** requests. This is achieved by associating an identifier with every
** outstanding RPC.
**
** Identifiers are globally unique but may be reused. Identifier reuse 
** is however delayed as much as possible. 
**
** ID_MAX       Maximum number of identifiers in use.
** ID_WARN      Give a warning when ID_WARN identifiers are in use.
***********************************************************************/

/*
** Configuration
*/

#define ID_MAX		PDS_HALF_WORD_MAX
#define ID_WARN		(ID_MAX - 1024)


typedef struct id {
    struct id * prev;
    struct id * next;
    pds_half_word_t offset;
    pds_word_t identifier;
} identifier_t;

static struct {
    l_mutex_t mutex;
    identifier_t * first;
    identifier_t * last;
    identifier_t * old;
    pds_word_t count;		/* # identifiers 	*/
    pds_word_t base;		/* identifier base	*/
} identifiers;

#define IDG_Lock()	l_mutex_lock(&identifiers.mutex)
#define IDG_Unlock()	l_mutex_unlock(&identifiers.mutex)


#if !defined(NDEBUG)

static bmsg_ret_t 
identifier_generator_check()
{
    identifier_t * identifier;
    unsigned count;

    count = 0;
    identifier = identifiers.first;

    while (identifier) {
	identifier = identifier->next;
	count++;
    }

    if (count == identifiers.count)
	return(BMSG_OK);
    else
	return(BMSG_ERROR);
}

#endif /* NDEBUG */


static void
identifier_generator_init()
{
    l_mutex_init(&identifiers.mutex);

    identifiers.first = (identifier_t *) 0;
    identifiers.last = (identifier_t *) 0;
    identifiers.old = (identifier_t *) 0;
    identifiers.count = 0;
    identifiers.base = (pds_word_t)
	               bport_self() << 8 * sizeof(pds_half_word_t);
}


static bmsg_ret_t 
acquire_identifier(identifier)
    pds_word_t * identifier;
{
    static pds_half_word_t offset = 0;
    identifier_t * idn;
    identifier_t * ide;

    IDG_Lock();

    bmsg_assert(identifier_generator_check() == BMSG_OK);

    ide = identifiers.old;

    if (identifiers.count == ID_MAX) {
        IDG_Unlock();
	*identifier = (bmem_id_t) 0;
	return(BMSG_NOID);
    }
    else identifiers.count++;
	
    /* derive unique identifier offset */
    if (++offset == 0)		
	ide = identifiers.first;				
    while (ide && offset >= ide->offset)
	ide = ide->next;

    /* get identifier holder */
    idn = (identifier_t *) 
	  pds_mem_alloc(&bmsg_phd,sizeof(identifier_t));
    if (!idn) {
        IDG_Unlock();
	*identifier = (bmem_id_t) 0;
	return(BMSG_NOMEMORY);
    }
    idn->offset = offset;
    idn->identifier = identifiers.base + offset;
    *identifier = idn->identifier;

    /* put new identifier in identifier list */
    if (!ide) { /* append */
        if (!identifiers.last) {
	    idn->prev = (identifier_t *) 0;
	    idn->next = (identifier_t *) 0;
	    identifiers.first = idn;
	    identifiers.last = idn;
        }
        else {
	    idn->prev = identifiers.last;
	    idn->next = (identifier_t *) 0;
	    identifiers.last->next = idn;
	    identifiers.last = idn;
        }
        ide = idn;
    }
    else { /* insert */
        if (!ide->prev) {
            idn->prev = (identifier_t *) 0;
            idn->next = ide;
            ide->prev = idn;
            identifiers.first = idn;
        }
        else {
            idn->prev = ide->prev;
            idn->next = ide;
            ide->prev->next = idn;
            ide->prev = idn;
        }
    }

    identifiers.old = ide;

    bmsg_assert(identifier_generator_check() == BMSG_OK);

    if (identifiers.count == ID_WARN) {
	IDG_Unlock();
	return(BMSG_WARN);
    }
    else {
	IDG_Unlock();
        return(BMSG_OK);
    }
}


static void 
release_identifier(identifier)
    pds_word_t identifier;
{
    identifier_t * id;

    IDG_Lock();

    bmsg_assert(identifier_generator_check() == BMSG_OK);

    if (!identifier)
	return;

    id = identifiers.first;
    while (id && (id->identifier != identifier))
	id = id->next;

    if (!id) { /* spurious identifier release */
	IDG_Unlock();
        bmsg_assert_always();
	return;
    }

    if (id == identifiers.old) {
	if (id->prev)
	    identifiers.old = id->prev;
	else 
	    identifiers.old = id->next;
    }

    /* remove identifier from identifier list */
    if (id->prev)
        id->prev->next = id->next;
    else
        identifiers.first = id->next;
    if (id->next)		
        id->next->prev = id->prev;
    else				
        identifiers.last = id->prev;		
    pds_mem_free(&bmsg_phd,(void_ptr) id);		
    identifiers.count--;

    bmsg_assert(identifier_generator_check() == BMSG_OK);

    IDG_Unlock();
}



/**********************************************************************
** Ports
***********************************************************************
** Every process has a single base port (bport) to/from which messages
** can be sent/received. Before two processes can communicate they have
** to establish a bi-directional communication channel. A process can
** achieve this by opening the port of the communication partner.
** After a communication channel has been set up, the ports of that
** communication channel are represented by a port data structure in
** the processes at both ends.
** Communication channels can be created, destroyed, blocked and 
** unblocked. This can all be done dynamically by any of the two
** communication partners of the channel. When both processes request
** for a state change of their communication channel at more or less
** the same time, only one of them will succeed. The winner is the one 
** which was first or, if the requests arrive at exactly the same time,
** the one with the highest bport identifier.
***********************************************************************/

/* 
** Port State Masks 
*/

#define BPORT_EMBRYONIC			0x1000
#define BPORT_OPENED			0x2000
#define BPORT_BLOCKED   		0x4000

#define BPORT_TRANSITS			0x0100
#define BPORT_DYING            		0x0200
#define BPORT_DEAD      		0x0400

#define BPORT_LOCAL_OPENS  		0x0010
#define BPORT_LOCAL_CLOSES		0x0020
#define BPORT_LOCAL_BLOCKS		0x0040
#define BPORT_LOCAL_UNBLOCKS		0x0080

#define BPORT_REMOTE_OPENS 		0x0001
#define BPORT_REMOTE_CLOSES		0x0002
#define BPORT_REMOTE_BLOCKS		0x0004
#define BPORT_REMOTE_UNBLOCKS		0x0008


/*
** Port state: many bit-or combinations of the port masks above. 
*/

#define	BPORT_UNAVAILABLE (BPORT_EMBRYONIC | BPORT_DEAD | \
			   BPORT_TRANSITS | BPORT_DYING)


typedef  struct port {
    struct port * next;         /* next bport 		   	        */
    struct port * prev;         /* previous bport 	   	        */
#if WORDS_BIGENDIAN
    unsigned : PDS_PAD_BITS;	 /* pad					*/
    gmsg_queue_t * send_queue;   /* send queue                     	*/
#else /* LITTLE ENDIAN */
    gmsg_queue_t * send_queue;   /* send queue                     	*/
    unsigned : PDS_PAD_BITS;	 /* pad					*/
#endif /* WORDS_BIGENDIAN */
    int sock;		         /* message socket			*/
    bmsg_bool_t familiar;	 /* familiarity 			*/
    bnet_address_t bnet_address; /* network (internet) address      	*/
    bport_number_t bport_number; /* network port number                 */
    bpid_t bpid;                 /* (local) process identifier      	*/
    bport_id_t bport_id;         /* port identifier	   	        */
    bdomain_id_t bdomain_id;     /* domain identifier	   	        */
    l_mutex_t mutex;	         /* protects following port fields	*/
    bmem_id_t mem_id;		 /* preacquired memory identifier	*/
    unsigned lusers;	         /* # concurrent local users   		*/
    unsigned rusers;	         /* # concurrent remote users  		*/
    unsigned state;         	 /* port state 				*/
    bport_info_t info;	         /* port statistics    			*/
} port_t;


#define Port_Lock(p)		l_mutex_lock(&(p)->mutex)
#define Port_Unlock(p)		l_mutex_unlock(&(p)->mutex)

#define PortLusers_Increment(p)	((p)->lusers++)
#define PortLusers_Decrement(p)	((p)->lusers--)
#define PortNoLusers(p)		((p)->lusers == 0)
#define PortOneLuser(p)		((p)->lusers == 1)

#define PortRusers_Increment(p)	((p)->rusers++)
#define PortRusers_Decrement(p)	((p)->rusers--)
#define PortNoRusers(p)		((p)->rusers == 0)

#define Port_Opened(p)		((p)->state & BPORT_OPENED)
#define Port_Closed(p)		(!Port_Opened(p))
#define Port_Blocked(p)	 	((p)->state & BPORT_BLOCKED)
#define Port_Unblocked(p)	(!Port_Blocked(p))

#define Port_Intransit(p)	((p)->state & BPORT_TRANSITS)
#define Port_Dying(p)		((p)->state & BPORT_DYING)
#define Port_Dead(p)		((p)->state & BPORT_DEAD)
#define Port_Embryonic(p)	((p)->state & BPORT_EMBRYONIC)

#define Port_Available(p)	(!((p)->state & BPORT_UNAVAILABLE))


static port_t * port_local;
bport_id_t bport_id_local;
bdomain_id_t bdomain_id_local;

    
static bmsg_ret_t
bport_allocate(port,bport)
    port_t * * port;
    bport_t * bport;
{
    port_t * p;

    p = (port_t *) pds_mem_alloc(&bmsg_phd,sizeof(port_t));
    if (!p) {
	*port = (port_t *) 0;
	return(BMSG_NOMEMORY);
    }

    p->next = (port_t *) 0;
    p->prev = (port_t *) 0;

    * (pds_address_t *) ((char *) &p->send_queue + BMSG_ADDR_OFFSET) =
    * (pds_address_t *) ((char *) &bport->bmsg_queue_address + BMSG_ADDR_OFFSET);

    p->sock = 0;
    /* assume familiarity with own domain only */
    if (bport->bdomain_id == bdomain_self())
        p->familiar = 1;
    else
        p->familiar = 0;	
    strncpy((char *) p->bnet_address,
            (char *) bport->bnet_address,
            BNET_ADDRESSLEN_MAX);
    p->bport_number = bport->bport_number;
    p->bpid = bport->bpid;
    p->bport_id = bport->bport_id;
    p->bdomain_id = bport->bdomain_id;
    l_mutex_init(&p->mutex);
    (void) acquire_identifier(&p->mem_id);
    p->lusers = 0;
    p->rusers = 0;
    p->state = BPORT_EMBRYONIC;
    p->info.lputs = 0;
    p->info.lgets = 0;
    p->info.rputs = 0;
    p->info.rgets = 0;
    p->info.sends = 0;
    p->info.receives = 0;
    Port_Lock(p);

    *port = p;
    return(BMSG_OK);
}


static void
bport_deallocate(port)
    port_t * port;
{
    SS_Lock();	
    close_sock(port->sock);
    SS_Unlock();
    release_identifier(port->mem_id);
    Port_Unlock(port);
    pds_mem_free(&bmsg_phd,(void_ptr) port);
}



/**********************************************************************
** Port Set 
***********************************************************************
** Ports are stored in a port set which consists of a number of
** port lists. 
***********************************************************************/

/*
** Configuration
*/

#define PORTSET_SIZE	32


typedef struct {
    l_mutex_t mutex;
    port_t * first;		/* first bport 	*/
    port_t * last;		/* last bport	*/
} bport_list_t;

#define PL_Lock(l)	l_mutex_lock(&(l)->mutex)
#define PL_Unlock(l)	l_mutex_unlock(&(l)->mutex)
#define PL_Empty(l)	(!((l)->last))


static bport_list_t bport_set[PORTSET_SIZE];


/* bport_list_t * PS_List(bport_id_t port_id) 				*/

#define PS_List(port_id)	((bport_list_t *) 			\
				&bport_set[(port_id) % PORTSET_SIZE])


static void
bport_set_init()
{
    int i;
    for (i = 0; i < PORTSET_SIZE; i++) {
        bport_set[i].first = (port_t *) 0;
        bport_set[i].last = (port_t *) 0;
    }
}


static bmsg_ret_t
add_port(port,port_out)
    port_t * port;
    port_t * * port_out;
{
    bmsg_ret_t ret;
    bport_id_t port_id = port->bport_id;
    bport_list_t * list = PS_List(port_id);
    port_t * ptmp;

    PL_Lock(list);

    ptmp = list->last;
    while (ptmp && (ptmp->bport_id != (port_id)))
	ptmp = ptmp->prev;
    if (!ptmp) { /* port does not exist yet */
	port->next = (port_t *) 0;
	if (PL_Empty(list)) {	
	    port->prev = (port_t *) 0;
	    list->first = port;	
	}
	else {
	    port->prev = list->last;
	    list->last->next = port;
	}
	list->last = port;
	port->lusers = 1;
	*port_out = port;
	ret = BMSG_OK;
    }
    else { /* port exists already */
	Port_Lock(ptmp);
	if ((ptmp->send_queue == port->send_queue) &&
	    (ptmp->bdomain_id == port->bdomain_id)) {
	    PortLusers_Increment(ptmp);
	    if (!Port_Available(ptmp))
	        ret = BMSG_PNOTAVAILABLE;
	    else if (Port_Blocked(ptmp))
	        ret = BMSG_PBLOCKED;
	    else {
	        bmsg_assert(Port_Opened(ptmp));
	        ret = BMSG_POPENED;
	    }
	}
	else {
	    Port_Unlock(ptmp);
	    ret = BMSG_INVAL;
	}
	*port_out = ptmp;
    }

    bmsg_assert((list->last == (port_t *) 0) || 
		(list->last->next == (port_t *) 0));
    PL_Unlock(list);

    return(ret);
}


static bmsg_ret_t remove_port(port)
    port_t * port;
{
    bmsg_ret_t ret;
    bport_list_t * list;
    bport_id_t port_id;
    port_t * ptmp;

    port_id = port->bport_id;
    list = PS_List(port_id);
    PL_Lock(list);

    ptmp = list->last;				
    while (ptmp && (ptmp->bport_id != port_id))
	ptmp = ptmp->prev;
    if ((!ptmp) || (port != ptmp))
	ret = BMSG_NOPORT;
    else {
	if (PortNoLusers(port) && PortNoRusers(port)) {
	    if (port->prev)		
	        port->prev->next = port->next;
	    else			
                list->first = port->next;
	    if (port->next)		
    	        port->next->prev = port->prev;	
	    else				
    	        list->last = port->prev;			
	    ret = BMSG_OK;
	}
	else
	    ret = BMSG_PNOTAVAILABLE;
    }						

    bmsg_assert((list->last == (port_t *) 0) || 
		(list->last->next == (port_t *) 0));
    PL_Unlock(list);

    return(ret);
}


static bmsg_ret_t
acquire_port(port_id,port)
    bport_id_t port_id;
    port_t * * port;
{
    bmsg_ret_t ret;
    port_t * p;
    bport_list_t * list = PS_List(port_id);

    PL_Lock(list);

    p = list->last;
    while (p && (p->bport_id != port_id))
        p = p->prev;
    if (p) {
	Port_Lock(p);
	if (Port_Dead(p)) {
	    Port_Unlock(p);
	    *port = (port_t *) 0;
	    ret = BMSG_NOPORT;
	}
	else {
	    PortLusers_Increment(p);
            *port = p;
	    bmsg_assert(p->sock >= 0);
	    ret = BMSG_OK;
	}
    }
    else {
	*port = (port_t *) 0;
	ret = BMSG_NOPORT;
    }

    PL_Unlock(list);

    return(ret);
}


#if defined(__STDC__)
static void release_port(port_t * port);
#else /* __STDC__ */
static void release_port();
#endif /* __STDC__ */



/**********************************************************************
** Port Requests and Replies
***********************************************************************/

#define Portrequest(bmsg_data)	((bport_request_t *) (bmsg_data))
#define Portreply(bmsg_data)	((bport_reply_t *) (bmsg_data))


/* 
** void Portrequest_Put(bport_request_t * req,
**			bpid_t p_id,
**		        bdomain_id_t d_id,
**		        bmem_address_t q_addr,
**			bnet_address_t n_addr,
**			bport_number_t p_no)
*/

#define Portrequest_Put(req,p_id,d_id,q_addr,n_addr,p_no) {	\
	bmsg_address(q_addr,&(req)->msg_queue_address);		\
	(req)->bdomain_id = d_id;				\
	(req)->bpid = p_id;					\
	strncpy((char *) (req)->bnet_address,			\
	        (char *) n_addr,				\
		BNET_ADDRESSLEN_MAX);				\
	(req)->bport_number = p_no;				\
}


/* 
** void Portrequest_Get(bport_request_t * req,
**			bpid_t * p_id,
**		        bdomain_id_t * d_id,
**		        bmem_address_t * q_addr,
**			bnet_address_t n_addr,
**			bport_number_t * p_no)
*/

#define Portrequest_Get(req,p_id,d_id,q_addr,n_addr,p_no) {    	\
	(void) bmem_address((req)->msg_queue_address,q_addr);	\
	*(d_id) = (req)->bdomain_id;				\
	*(p_id) = (req)->bpid;					\
	strncpy((char *) n_addr,				\
	        (char *) (req)->bnet_address,			\
	        BNET_ADDRESSLEN_MAX);				\
	*(p_no) = (req)->bport_number;				\
}


/* 
** void Portreply_Put(bport_reply_t * req,bmsg_ret_t ret)
*/

#define Portreply_Put(req,ret) {		    	    	\
	(req)->bmsg_ret = ret;				    	\
}


/* 
** void Portreply_Get(bport_reply_t * req,bmsg_ret_t * ret)
*/

#define Portreply_Get(req,ret) {				\
	*(ret) = (req)->bmsg_ret;				\
}



/**********************************************************************
** Memory Requests and Replies
***********************************************************************/

#define Memrequest(bmsg_data)	((bmem_request_t *) (bmsg_data))
#define Memreply(bmsg_data)	((bmem_reply_t *) (bmsg_data))


/* 
** void MemPutRequest_Put(bmem_request_t * r,
**		          bmem_id id,
**		          bmem_address_t dst,
**		          bmem_size_t sz)
*/

#define MemPutRequest_Put(r,id,dst,sz) {			\
	bmsg_address(dst,&(r)->bmem_dst_address);		\
	(r)->bmem_id = id;					\
	(r)->bmem_size = sz;					\
}


/* 
** void MemPutRequest_Get(bmem_request_t * r,
**		          bmem_id * id,
**		          bmem_address_t * dst,
**		          bmem_size_t * sz)
*/

#define MemPutRequest_Get(r,id,dst,sz) {			\
	(void) bmem_address((r)->bmem_dst_address,dst);		\
	*(id) = (r)->bmem_id;					\
	*(sz) = (r)->bmem_size;					\
}


/* 
** void MemGetRequest_Put(bmem_request_t * r,
**		          bmem_id id,
**		          bmem_address_t src,
**		          bmem_address_t dst,
**		          bmem_size_t sz)
*/

#define MemGetRequest_Put(r,id,src,dst,sz) {			\
	bmsg_address(src,&(r)->bmem_src_address);		\
	bmsg_address(dst,&(r)->bmem_dst_address);		\
	(r)->bmem_id = id;					\
	(r)->bmem_size = sz;					\
}


/* 
** void MemGetRequest_Get(bmem_request_t * r,
**		          bmem_id * id,
**		          bmem_address_t * src,
**		          bmem_address_t * dst,
**		          bmem_size_t * sz)
*/


#define MemGetRequest_Get(r,id,src,dst,sz) {			\
	(void) bmem_address((r)->bmem_src_address,src);		\
	(void) bmem_address((r)->bmem_dst_address,dst);		\
	*(id) = (r)->bmem_id;					\
	*(sz) = (r)->bmem_size;					\
}


/* 
** void MemPutReply_Put(bmem_reply_t * r,
**		        bmem_id id,
**		        bmsg_ret_t ret)
*/

#define MemPutReply_Put(r,id,ret) {				\
	(r)->bmem_id = id;					\
	(r)->bmsg_ret = ret;					\
}


/* 
** void MemPutReply_Get(bmem_reply_t * r,
**		        bmem_id * id,
**		        bmsg_ret_t * ret)
*/

#define MemPutReply_Get(r,id,ret) {			    	\
	*(id) = (r)->bmem_id;					\
	*(ret) = (r)->bmsg_ret;					\
}


/* 
** void MemGetReply_Put(bmem_reply_t * r,
**		        bmem_id id,
**		        bmem_address_t dst,
**		        bmsg_ret_t ret)
*/

#define MemGetReply_Put(r,id,dst,ret) {				\
	bmsg_address(dst,&(r)->bmem_dst_address);		\
	(r)->bmem_id = id;					\
	(r)->bmsg_ret = ret;					\
}


/* 
** void MemGetReply_Get(bmem_reply_t * r,
**		        bmem_id * id,
**		        bmem_address_t * dst,
**		        bmsg_ret_t * ret)
*/


#define MemGetReply_Get(r,id,dst,ret) {			    	\
	(void) bmem_address((r)->bmem_dst_address,dst);		\
	*(id) = (r)->bmem_id;					\
	*(ret) = (r)->bmsg_ret;					\
}



/**********************************************************************
** XDR
***********************************************************************/

#define BMSG_XDRBUF_SIZE        64

typedef struct {
    unsigned bmsg_header;	/* size of header in XDR format        */
    unsigned bmem_request;	/* size of bmem_request in XDR format  */
    unsigned bmem_reply;	/* size of bmem_reply in XDR format    */
    unsigned bport_request;	/* size of bport_request in XDR format */
    unsigned bport_reply;	/* size of bport_reply in XDR format   */
} bmsg_xdr_size_t;

static bmsg_xdr_size_t bmsg_xdr_size;


static bool_t 
xdr_bmsg_header(xdrs,bmsg_header)
    XDR * xdrs;
    bmsg_header_t * bmsg_header;
{
    return(xdr_bmsg_itype(xdrs,&bmsg_header->bmsg_type) &&
           xdr_bport_id(xdrs,&bmsg_header->bport_id) &&
           xdr_bmsg_size(xdrs,&bmsg_header->bmsg_data_size));
}


static bool_t
xdr_bmem_request(xdrs,bmem_request)
    XDR * xdrs;
    bmem_request_t * bmem_request;
{
    return(xdr_bmsg_address(xdrs,&bmem_request->bmem_src_address) &&
           xdr_bmsg_address(xdrs,&bmem_request->bmem_dst_address) &&
           xdr_bmem_size(xdrs,&bmem_request->bmem_size) &&
           xdr_bmem_id(xdrs,&bmem_request->bmem_id));
}


static bool_t 
xdr_bmem_reply(xdrs,bmem_reply)
    XDR * xdrs;
    bmem_reply_t * bmem_reply;
{
    return(xdr_bmsg_address(xdrs,&bmem_reply->bmem_src_address) &&
           xdr_bmsg_address(xdrs,&bmem_reply->bmem_dst_address) &&
           xdr_bmem_id(xdrs,&bmem_reply->bmem_id) &&
           xdr_bmsg_ret(xdrs,&bmem_reply->bmsg_ret));
}


static bool_t
xdr_bport_request(xdrs,bport_request)
    XDR * xdrs;
    bport_request_t * bport_request;
{
    char * bnet_address = bport_request->bnet_address;

    return(xdr_bmsg_address(xdrs,&bport_request->msg_queue_address) &&
           xdr_bdomain_id(xdrs,&bport_request->bdomain_id) &&
           xdr_bpid(xdrs,&bport_request->bpid) &&
	   xdr_bnet_address(xdrs,&bnet_address) &&
	   xdr_bport_number(xdrs,&bport_request->bport_number));
}


static bool_t
xdr_bport_reply(xdrs,bport_reply)
    XDR * xdrs;
    bport_reply_t * bport_reply;
{
    return(xdr_bmsg_ret(xdrs,&bport_reply->bmsg_ret));
}


static void
bmsg_xdr_init()
{
    bmsg_header_t bmsg_header;
    bmem_request_t bmem_request;
    bmem_reply_t bmem_reply;
    bport_request_t bport_request;
    bport_reply_t bport_reply;
    pds_int32 xdrbuf[BMSG_XDRBUF_SIZE];
    XDR xdrs;
    int i;

    bmsg_header.bmsg_type = 0;
    bmsg_header.bport_id = 0;
    bmsg_header.bmsg_data_size = 0;

    bmem_request.bmem_src_address.low = 0;
    bmem_request.bmem_src_address.high = 0;
    bmem_request.bmem_dst_address.low = 0;
    bmem_request.bmem_dst_address.high = 0;
    bmem_request.bmem_size = 0;
    bmem_request.bmem_id = 0;

    bmem_reply.bmem_src_address.low = 0;
    bmem_reply.bmem_src_address.high = 0;
    bmem_reply.bmem_dst_address.low = 0;
    bmem_reply.bmem_dst_address.high = 0;
    bmem_reply.bmem_id = 0;
    bmem_reply.bmsg_ret = 0;

    bport_request.msg_queue_address.low = 0;
    bport_request.msg_queue_address.high = 0;
    bport_request.bdomain_id = 0;
    bport_request.bpid = 0;
    for (i=0;i<=INET_ADDRESSLEN_MAX;i++)
        bport_request.bnet_address[i] = '\0';
    bport_request.bport_number = 0;

    bport_reply.bmsg_ret = 0;

    xdrmem_create(&xdrs,(const caddr_t) xdrbuf,
                  (const u_int) BMSG_XDRBUF_SIZE * sizeof(pds_int32),
                  XDR_ENCODE);

    if (!xdr_bmsg_header(&xdrs,&bmsg_header))
        bmsg_assert_always();
    bmsg_xdr_size.bmsg_header = xdr_getpos(&xdrs);

    if (!xdr_setpos(&xdrs,(const u_int) 0))
        bmsg_assert_always();
    if (!xdr_bmem_request(&xdrs,&bmem_request))
        bmsg_assert_always();
    bmsg_xdr_size.bmem_request = xdr_getpos(&xdrs);

    if (!xdr_setpos(&xdrs,(const u_int) 0))
        bmsg_assert_always();
    if (!xdr_bmem_reply(&xdrs,&bmem_reply))
        bmsg_assert_always();
    bmsg_xdr_size.bmem_reply = xdr_getpos(&xdrs);

    if (!xdr_setpos(&xdrs,(const u_int) 0))
        bmsg_assert_always();
    if (!xdr_bport_request(&xdrs,&bport_request))
        bmsg_assert_always();
    bmsg_xdr_size.bport_request = xdr_getpos(&xdrs);

    if (!xdr_setpos(&xdrs,(const u_int) 0))
        bmsg_assert_always();
    if (!xdr_bport_reply(&xdrs,&bport_reply))
        bmsg_assert_always();
    bmsg_xdr_size.bport_reply = xdr_getpos(&xdrs);

    bmsg_assert(BMSG_XDRBUF_SIZE * sizeof(pds_int32) >= 
		bmsg_xdr_size.bmsg_header + bmsg_xdr_size.bmem_request);
    bmsg_assert(BMSG_XDRBUF_SIZE * sizeof(pds_int32) >= 
		bmsg_xdr_size.bmsg_header + bmsg_xdr_size.bmem_reply);
    bmsg_assert(BMSG_XDRBUF_SIZE * sizeof(pds_int32) >= 
		bmsg_xdr_size.bmsg_header + bmsg_xdr_size.bport_request);
    bmsg_assert(BMSG_XDRBUF_SIZE * sizeof(pds_int32) >= 
		bmsg_xdr_size.bmsg_header + bmsg_xdr_size.bport_reply);

    xdr_destroy(&xdrs);
}



/**********************************************************************
** Statistics
***********************************************************************/

static l_mutex_t msg_info_mutex;
static bmsg_info_t msg_info;

static l_mutex_t mem_info_mutex;
static bmem_info_t mem_info;


static void 
bmsg_info_init()
{
    l_mutex_init(&msg_info_mutex);
    msg_info.sent_short = 0;
    msg_info.sent_medium = 0;
    msg_info.sent_long = 0;
    msg_info.rcvd_short = 0;
    msg_info.rcvd_medium = 0;
    msg_info.rcvd_long = 0;
}


static void 
bmem_info_init()
{
    l_mutex_init(&mem_info_mutex);
    mem_info.lputs = 0;
    mem_info.lgets = 0;
    mem_info.rputs = 0;
    mem_info.rgets = 0;
}


#define MsgInfo_Lock()		l_mutex_lock(&msg_info_mutex)
#define MsgInfo_Unlock()	l_mutex_unlock(&msg_info_mutex)

#define MemInfo_Lock()		l_mutex_lock(&mem_info_mutex)
#define MemInfo_Unlock()	l_mutex_unlock(&mem_info_mutex)


#if !defined(BMSG_STAT_OFF)
#define MsgInfo_Sent_Increment(s) {		\
	MsgInfo_Lock();				\
	if ((s) <= BMSG_SHORT)			\
	    msg_info.sent_short++;    		\
	else if ((s) <= BMSG_MEDIUM)		\
	    msg_info.sent_medium++;    		\
	else					\
	    msg_info.sent_long++;		\
	MsgInfo_Unlock();			\
}
#define MsgInfo_Rcvd_Increment(s) {		\
	MsgInfo_Lock();				\
	if ((s) <= BMSG_SHORT)			\
	    msg_info.rcvd_short++;    		\
	else if ((s) <= BMSG_MEDIUM)		\
	    msg_info.rcvd_medium++;    		\
	else					\
	    msg_info.rcvd_long++;		\
	MsgInfo_Unlock();			\
}
#else
#define MsgInfo_Sent_Increment(s)
#define MsgInfo_Rcvd_Increment(s)
#endif /* BMSG_STAT_OFF */


#if !defined(BMEM_STAT_OFF)
#define MemInfo_Lput_Increment() {		\
	MemInfo_Lock();				\
	mem_info.lputs++;			\
	MemInfo_Unlock();			\
}
#define MemInfo_Lget_Increment() {		\
	MemInfo_Lock();				\
	mem_info.lgets++;			\
	MemInfo_Unlock();			\
}
#define MemInfo_Rput_Increment() {		\
	MemInfo_Lock();				\
	mem_info.rputs++;			\
	MemInfo_Unlock();			\
}
#define MemInfo_Rget_Increment() {		\
	MemInfo_Lock();				\
	mem_info.rgets++;			\
	MemInfo_Unlock();			\
}
#else
#define MemInfo_Lput_Increment()
#define MemInfo_Lget_Increment()
#define MemInfo_Rput_Increment()
#define MemInfo_Rget_Increment()
#endif /* BMEM_STAT_OFF */


#if !defined(BPORT_STAT_OFF) && !defined(BMEM_STAT_OFF)
#define PortInfo_Lput_Increment(p) ((p)->info.lputs++);
#define PortInfo_Lget_Increment(p) ((p)->info.lgets++);
#define PortInfo_Rput_Increment(p) ((p)->info.rputs++);
#define PortInfo_Rget_Increment(p) ((p)->info.rgets++);	
#else
#define PortInfo_Lput_Increment(p)
#define PortInfo_Lget_Increment(p)
#define PortInfo_Rput_Increment(p)
#define PortInfo_Rget_Increment(p)
#endif /* !BPORT_STAT_OFF && !BMEM_STAT_OFF */

#if !defined(BPORT_STAT_OFF) && !defined(BMSG_STAT_OFF)
#define PortInfo_Sent_Increment(p) ((p)->info.sends++);
#define PortInfo_Rcvd_Increment(port_id) {			\
	port_t * p;						\
	{							\
	   if (acquire_port(port_id,&p) == BMSG_OK) {		\
	       p->info.receives++;				\
	       release_port(p);					\
	   }							\
	}							\
}
#else
#define PortInfo_Sent_Increment(p)
#define PortInfo_Rcvd_Increment(port_id)
#endif /* !BPORT_STAT_OFF && !BMSG_STAT_OFF */



/**********************************************************************
** Notifications
***********************************************************************/

static int bport_notification = 0;

#define Bport_notify(id,pr) {		\
    if (bport_notification) {		\
	bport_notify(id,pr);		\
    }					\
}

#define Bmsg_notify() bmsg_notify()

static int bmem_notification = 0;

#define Bmem_notify(id,pr,ad,sz) {	\
    if (bmem_notification) {		\
	bmem_notify(id,pr,ad,sz);	\
    }					\
}



/**********************************************************************
** Statistics Updating and Actual Logging
***********************************************************************/

static void
bmsg_send_loginfo(port,msg_header)
    port_t * port;
    bmsg_header_t * msg_header;
{
    switch (msg_header->bmsg_type) {
	case BMSG_MSG :
	    BMSGLOG_SEND_MESSAGE(port->bport_id);
	    MsgInfo_Sent_Increment(msg_header->bmsg_data_size);
	    PortInfo_Sent_Increment(port);
	    break;
	case BMSG_PORT_OPEN :
	    BMSGLOG_SEND_PORT_OPEN(port->bport_id);
	    break;
	case BMSG_PORT_CLOSE :
	    BMSGLOG_SEND_PORT_CLOSE(port->bport_id);
	    break;
	case BMSG_PORT_BLOCK :
	    BMSGLOG_SEND_PORT_BLOCK(port->bport_id);
	    break;
	case BMSG_PORT_UNBLOCK :
	    BMSGLOG_SEND_PORT_UNBLOCK(port->bport_id);
	    break;
	case BMSG_PORT_DIE :
	    BMSGLOG_SEND_PORT_DIE(port->bport_id);
	    break;
	case BMSG_PORT_OPEN_ACK :
	    BMSGLOG_SEND_PORT_OPEN_ACK(port->bport_id);
	    break;
	case BMSG_PORT_CLOSE_ACK :
	    BMSGLOG_SEND_PORT_CLOSE_ACK(port->bport_id);
	    break;
	case BMSG_PORT_BLOCK_ACK :
	    BMSGLOG_SEND_PORT_BLOCK_ACK(port->bport_id);
	    break;
	case BMSG_PORT_UNBLOCK_ACK :
	    BMSGLOG_SEND_PORT_UNBLOCK_ACK(port->bport_id);
	    break;
	case BMSG_MEM_PUT :
	    BMSGLOG_SEND_MEM_PUT(port->bport_id);
	    break;
	case BMSG_MEM_GET :
	    BMSGLOG_SEND_MEM_GET(port->bport_id);
	    break;
	case BMSG_MEM_PUT_ACK :
	    BMSGLOG_SEND_MEM_PUT_ACK(port->bport_id);
	    break;
	case BMSG_MEM_GET_ACK :
	    BMSGLOG_SEND_MEM_GET_ACK(port->bport_id);
	    break;
	default :
	    /* unknown message type */
            bmsg_assert_always();
	    break;
    }
}


static void
bmsg_receive_loginfo(msg_header)
    bmsg_header_t * msg_header;
{
    switch (msg_header->bmsg_type) {
        case BMSG_MSG :
            BMSGLOG_RCV_MESSAGE(msg_header->bport_id);
            MsgInfo_Rcvd_Increment(msg_header->bmsg_data_size);
            PortInfo_Rcvd_Increment(msg_header->bport_id);
            break;
        case BMSG_PORT_OPEN :
            BMSGLOG_RCV_PORT_OPEN(msg_header->bport_id);
            break;
        case BMSG_PORT_CLOSE :
            BMSGLOG_RCV_PORT_CLOSE(msg_header->bport_id);
            break;
        case BMSG_PORT_BLOCK :
            BMSGLOG_RCV_PORT_BLOCK(msg_header->bport_id);
            break;
        case BMSG_PORT_UNBLOCK :
            BMSGLOG_RCV_PORT_UNBLOCK(msg_header->bport_id);
            break;
        case BMSG_PORT_DIE :
            BMSGLOG_RCV_PORT_DIE(msg_header->bport_id);
            break;
        case BMSG_PORT_OPEN_ACK :
            BMSGLOG_RCV_PORT_OPEN_ACK(msg_header->bport_id);
            break;
        case BMSG_PORT_CLOSE_ACK :
            BMSGLOG_RCV_PORT_CLOSE_ACK(msg_header->bport_id);
            break;
        case BMSG_PORT_BLOCK_ACK :
            BMSGLOG_RCV_PORT_BLOCK_ACK(msg_header->bport_id);
            break;
        case BMSG_PORT_UNBLOCK_ACK :
            BMSGLOG_RCV_PORT_UNBLOCK_ACK(msg_header->bport_id);
            break;
        case BMSG_MEM_PUT :
            BMSGLOG_RCV_MEM_PUT(msg_header->bport_id);
            break;
        case BMSG_MEM_GET :
            BMSGLOG_RCV_MEM_GET(msg_header->bport_id);
            break;
        case BMSG_MEM_PUT_ACK :
            BMSGLOG_RCV_MEM_PUT_ACK(msg_header->bport_id);
            break;
        case BMSG_MEM_GET_ACK :
            BMSGLOG_RCV_MEM_GET_ACK(msg_header->bport_id);
            break;
        default:
	    /* unknown message type */
            bmsg_assert_always();
            break;
    }
}



/**********************************************************************
** Message Transfer
***********************************************************************/

static bmsg_ret_t
bmsg_intra_domain_send(port,buffer)
    port_t * port;
    bmsg_buffer_t * buffer;
{
    bmsg_option_t triggering;
    gmsg_queue_t * send_queue;
    bport_t bport;

    bmsg_assert(port->bdomain_id == bdomain_self());

    bmsg_send_loginfo(port,&buffer->header);

    send_queue = port->send_queue;
    GMQ_Lock(send_queue);
    triggering = send_queue->option & BMSG_TRIGGERING;
    if (MQ_Empty(send_queue)) {
        MQ_Put(send_queue,buffer); 
	GMQ_Unlock(send_queue);
	if (triggering) {
	    if (port->bport_id == bport_self()) {
	        Port_Unlock(port);
		(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
		Port_Lock(port);
	    }
	    else {
	        bport.bpid = port->bpid;
	        bport.bport_id = port->bport_id;
	        bport.bdomain_id = port->bdomain_id;
	        bport.bmsg_queue_address = (bmem_address_t) send_queue;
	        strcpy((char *) bport.bnet_address,
	               (char *) port->bnet_address);
	        bport.bport_number = port->bport_number;
	        Port_Unlock(port);
	        bproc_trigger(&bport);
	        Port_Lock(port);
	    }
	}
    }
    else {
	MQ_Put(send_queue,buffer);
	GMQ_Unlock(send_queue);
    }

    return(BMSG_OK);
}


static bmsg_ret_t
bmsg_inet_send(port,buffer)
    port_t * port;
    bmsg_buffer_t * buffer;
{
    pds_int32 xdrbuf[BMSG_XDRBUF_SIZE];
    unsigned data_size;
    char * data_ptr;
    unsigned size;
    XDR xdrs;

    bmsg_assert(port->sock);

    xdrmem_create(&xdrs,(const caddr_t) xdrbuf,
                  (const u_int) BMSG_XDRBUF_SIZE * sizeof(pds_int32),
                  XDR_ENCODE);
    if (!xdr_bmsg_header(&xdrs,&buffer->header))
        bmsg_assert_always();

    data_size = 0;
    switch (buffer->header.bmsg_type) {
	case BMSG_MSG :
	    data_size = buffer->header.bmsg_data_size;
	    data_ptr = (char *) buffer->data;
	    break;
	case BMSG_PORT_DIE :
	    break;
	case BMSG_PORT_OPEN :
	case BMSG_PORT_CLOSE :
	case BMSG_PORT_BLOCK :
	case BMSG_PORT_UNBLOCK :
	    if (!xdr_bport_request(&xdrs,buffer->data))
        	bmsg_assert_always();
	    break;
	case BMSG_PORT_OPEN_ACK :
	case BMSG_PORT_CLOSE_ACK :
	case BMSG_PORT_BLOCK_ACK :
	case BMSG_PORT_UNBLOCK_ACK :
	    if (!xdr_bport_reply(&xdrs,buffer->data))
        	bmsg_assert_always();
	    break;
	case BMSG_MEM_PUT :
	    data_size = buffer->header.bmsg_data_size - 
			sizeof(bmem_request_t);
	    data_ptr = (char *) buffer->data + sizeof(bmem_request_t);
	case BMSG_MEM_GET :
	    if (!xdr_bmem_request(&xdrs,buffer->data))
        	bmsg_assert_always();
	    break;
	case BMSG_MEM_GET_ACK :
	    data_size = buffer->header.bmsg_data_size - 
			sizeof(bmem_reply_t);
	    data_ptr = (char *) buffer->data + sizeof(bmem_reply_t);
	case BMSG_MEM_PUT_ACK :
	    if (!xdr_bmem_reply(&xdrs,buffer->data))
        	bmsg_assert_always();
	    break;
	default :
            /* unknown message type */
            bmsg_assert_always();
            break;
    }
    size = xdr_getpos(&xdrs);

    if (write_sock(port->sock, (char *) xdrbuf, size) ||
        write_sock(port->sock, data_ptr, data_size)) {
    	/* destroy socket */
        SS_Lock();	
    	close_sock(port->sock);
        SS_Unlock();	
    	/* remove port lazily */
    	/* reuse buffer for BMSG_PORT_DIE message */
    	bmsg_assert(sizeof(bport_request_t) < BUF_DATA_BYTES);
    	buffer->header.bmsg_type = BMSG_PORT_DIE;
    	buffer->header.bport_id = port->bport_id;
    	port->state |= BPORT_DYING; 
    	PortRusers_Increment(port);
    	LMQ_Lock(&lrcv_queue);
    	MQ_Put(&lrcv_queue,buffer);
    	LMQ_Unlock(&lrcv_queue);
        xdr_destroy(&xdrs);
    	return(BMSG_PDYING);
    }
    else { 
        bmsg_send_loginfo(port,&buffer->header);
        bmsg_buffer_free(buffer);
        xdr_destroy(&xdrs);
    }

    return(BMSG_OK);
}


static bmsg_ret_t
bmsg_sys_send(port,buffer)
    port_t * port;
    bmsg_buffer_t * buffer;
{
    if (Port_Dying(port)) {
	bmsg_buffer_free(buffer);
	return(BMSG_PDYING);
    }

    if (port->bdomain_id == bdomain_self()) {
        /* assume familiarity with own domain only */
        buffer->familiar = 1;
	return(bmsg_intra_domain_send(port,buffer));
    }
    else if (strcmp(port->bnet_address,port_local->bnet_address))
	/* inter machine communication */
	return(bmsg_inet_send(port,buffer));
    else {
	/* inter-domain intra-machine communication */
	return(bmsg_inet_send(port,buffer));
    }
}


static bmsg_ret_t
bmsg_intra_domain_accept()
{
    bmsg_ret_t bret;

    /* move contents global receive queue to local receive queue */
    if (!MQ_Empty(grcv_queue)) { /* avoid unnecessary locking */
        LMQ_Lock(&lrcv_queue);
        GMQ_Lock(grcv_queue);
        if (!MQ_Empty(grcv_queue)) { /* reassure non-emptyness */
            MQ_MoveAll(&lrcv_queue,grcv_queue);
            bret = BMSG_OK;
        }
        else
            bret = BMSG_NOMESSAGE;
        GMQ_Unlock(grcv_queue);
        LMQ_Unlock(&lrcv_queue);
        return(bret);
    }
    else
        return(BMSG_NOMESSAGE);
}


static bmsg_ret_t
bmsg_sock_read(sock,buffer,bport_id)
    int sock;
    bmsg_buffer_t * * buffer;
    bport_id_t * bport_id;
{
    pds_int32 xdrbuf[BMSG_XDRBUF_SIZE];
    bmsg_buffer_t * sbuf;
    bmsg_buffer_t * buf;
    unsigned data_size;
    bmsg_ret_t bret;
    char * data_ptr;
    unsigned size;
    XDR xdrs;
    int ret;

    xdrmem_create(&xdrs,(const caddr_t) xdrbuf,
                  (const u_int) BMSG_XDRBUF_SIZE * sizeof(pds_int32),
                  XDR_DECODE);

    if ((bret = bmsg_sbuffer_alloc(&sbuf)) != BMSG_OK) {
        xdr_destroy(&xdrs);
	return(bret);
    }

    /* read header */
    if (read_sock(sock,
		  (char *) xdrbuf,
		  bmsg_xdr_size.bmsg_header)) {
	if (errno == EAGAIN)
	    bret = BMSG_NOMESSAGE;
	else
	    bret = BMSG_PDYING;
	bmsg_sbuffer_free(sbuf);
        xdr_destroy(&xdrs);
	return(bret);
    }
    if (!xdr_bmsg_header(&xdrs,&sbuf->header))
        bmsg_assert_always();

    /* get relevant info from message header */
    if (bport_id)
        *bport_id = sbuf->header.bport_id;
    size = sbuf->header.bmsg_data_size;

    /* allocate larger buffer if necessary */
    if (size > BUF_DATA_BYTES) { /* long message */
        if ((bret = bmsg_buffer_alloc(size,&buf)) != BMSG_OK) {
	    bmsg_sbuffer_free(sbuf);
            xdr_destroy(&xdrs);
	    return(bret);
	}
	buf->header = sbuf->header;
	bmsg_sbuffer_free(sbuf);
    }
    else
	buf = sbuf;

    /* read {bport|bmem}{request|reply} */
    if (!xdr_setpos(&xdrs,(const u_int) 0))
        bmsg_assert_always();
    data_size = 0;
    do {
        switch (buf->header.bmsg_type) {
            case BMSG_MSG :
		/* inet unfamiliar communication */
    		buf->familiar = 0;
		data_ptr = (char *) buf->data;
		data_size = buf->header.bmsg_data_size;
		ret = 0;
		break;
            case BMSG_PORT_DIE :
    		ret = 0;
                break;
            case BMSG_PORT_OPEN :
            case BMSG_PORT_CLOSE :
            case BMSG_PORT_BLOCK :
            case BMSG_PORT_UNBLOCK :
        	if ((ret = read_sock(sock,
				     (char *) xdrbuf,
				     bmsg_xdr_size.bport_request)))
		    break;
                if (!xdr_bport_request(&xdrs,buf->data))
                    bmsg_assert_always();
                break;
            case BMSG_PORT_OPEN_ACK :
            case BMSG_PORT_CLOSE_ACK :
            case BMSG_PORT_BLOCK_ACK :
            case BMSG_PORT_UNBLOCK_ACK :
        	if ((ret = read_sock(sock,
				     (char *) xdrbuf,
				     bmsg_xdr_size.bport_reply)))
		    break;
                if (!xdr_bport_reply(&xdrs,buf->data))
                    bmsg_assert_always();
                break;
            case BMSG_MEM_PUT :
		data_size = buf->header.bmsg_data_size -
			    sizeof(bmem_request_t);
		data_ptr = (char *) buf->data + sizeof(bmem_request_t);
            case BMSG_MEM_GET :
        	if ((ret = read_sock(sock,
				     (char *) xdrbuf,
				     bmsg_xdr_size.bmem_request)))
		    break;
                if (!xdr_bmem_request(&xdrs,buf->data))
                    bmsg_assert_always();
                break;
            case BMSG_MEM_GET_ACK :
		data_size = buf->header.bmsg_data_size -
                            sizeof(bmem_reply_t);
                data_ptr = (char *) buf->data + sizeof(bmem_reply_t);
            case BMSG_MEM_PUT_ACK :
        	if ((ret = read_sock(sock,
				     (char *) xdrbuf,
				     bmsg_xdr_size.bmem_reply)))
		    break;
                if (!xdr_bmem_reply(&xdrs,buf->data))
                    bmsg_assert_always();
                break;
            default :
                /* unknown message type */
                bmsg_assert_always();
                break;
        }
    } while (ret && (errno == EAGAIN));
    xdr_destroy(&xdrs);
    if (ret) {
	bmsg_buffer_free(buf);
	return(BMSG_PDYING);
    }

    /* read message data */
    do {
        ret = read_sock(sock,data_ptr,data_size);
    } while (ret && (errno == EAGAIN));
    if (ret) {
	bmsg_buffer_free(buf);
	return(BMSG_PDYING);
    }
    *buffer = buf;

    return(BMSG_OK);
}


/*
**    Function: bmsg_embsock_accept()
** Description: Accept pending messages from up to count ready embryonic
**              sockets that are member of the socket set.
**              Messages are read from the ready embryonic sockets, 
**              stored in message buffers, and these buffers are 
**              appended to the local receive queue.
**        Note: The anonymity of the ready embryonic sockets is resolved
**		by storing the identifier of their associated bport
**		(returned by bmsg_sock_read()) in the embryonic socket
**		structure.
**              The sockset lock should be hold on entering and is 
**		also hold on returning. 
*/

static bmsg_ret_t
bmsg_embsock_accept(set,count)
    fd_set * set;
    unsigned * count;
{
    bmsg_buffer_t * buffer;
    bmsg_ret_t bret, bret1;
    emb_sock_t * emb_sock;
    int msg_sock;

    bret = BMSG_NOMESSAGE;
    EMB_Lock();
    emb_sock = emb_sock_list.first;
    while ((*count > 0) && (emb_sock)) {
	msg_sock = emb_sock->sock;
        if (FD_ISSET(msg_sock,set)) { 
	    /* read message from embryonic socket */
	    (*count)--;
	    while ((bret1 = bmsg_sock_read(msg_sock,
                                           &buffer,
		               		   &emb_sock->bport_id)) == BMSG_OK) {
		/* append buffer to local receive queue */
		LMQ_Lock(&lrcv_queue);
		MQ_Put(&lrcv_queue,buffer);
		LMQ_Unlock(&lrcv_queue);
		bret = BMSG_OK;
	    }
	    if (bret1 != BMSG_NOMESSAGE) {
		/* destroy embryonic socket */
		close_sock(msg_sock);
		emb_sock_destroy(&emb_sock);
	    }
	    else
	        emb_sock = emb_sock->next;
	}
	else 
	    emb_sock = emb_sock->next;
    }
    EMB_Unlock();
    return(bret);
}


/*
**    Function: bmsg_msgsock_accept()
** Description: Accept pending messages from up to count ready message
**		sockets that are member of the socket set.
**              Messages are read from the ready message sockets, 
**		stored in message buffers, and these buffers are 
**		appended to the local receive queue.
**        Note: The sockset lock should be hold on entering and is 
**		also hold on returning. 
*/

static bmsg_ret_t
bmsg_msgsock_accept(set,count)
    fd_set * set;
    unsigned * count;
{
    bmsg_ret_t bret, bret1;
    bmsg_buffer_t * buffer;
    port_t * port;
    int msg_sock;
    int list;

    bret = BMSG_NOMESSAGE;

    list = 0;
    while ((*count > 0) && (list < PORTSET_SIZE)) {
	PL_Lock(&bport_set[list]);
	port = bport_set[list].first;
	while ((*count > 0) && (port)) {
	    msg_sock = port->sock;
	    if ((msg_sock) && (FD_ISSET(msg_sock,set))) {
		/* read message socket */
	        (*count)--;
	        while ((bret1 = bmsg_sock_read(msg_sock,
                                               &buffer,
				               (bport_id_t *) 0)) == BMSG_OK) {
		    /* append buffer to local receive queue */
    		    LMQ_Lock(&lrcv_queue);
		    MQ_Put(&lrcv_queue,buffer);
    		    LMQ_Unlock(&lrcv_queue);
		    bret = BMSG_OK;
	        }
	        if (bret1 != BMSG_NOMESSAGE) {
		    /* destroy socket */
		    close_sock(msg_sock);
		    /* remove port lazily */
		    Port_Lock(port);
		    if (!Port_Dying(port)) {
		        /* allocate message buffer for BMSG_PORT_DIE message */
			bmsg_assert(sizeof(bport_request_t) < BUF_DATA_BYTES);
			bret1 = bmsg_sbuffer_alloc(&buffer);
            		if (bret1 != BMSG_OK) {
			    Port_Unlock(port);    
			    PL_Unlock(&bport_set[list]);
			    SS_Unlock();
			    bmsg_panic(BMSG_WEP_MEMORY,bport_self());
			    exit(-1);
			}
            		buffer->header.bmsg_type = BMSG_PORT_DIE;
            		buffer->header.bport_id = port->bport_id;
		        port->state |= BPORT_DYING;
            		PortRusers_Increment(port);
    		        LMQ_Lock(&lrcv_queue);
            		MQ_Put(&lrcv_queue,buffer);
    		        LMQ_Unlock(&lrcv_queue);
		        bret = BMSG_OK;
		    }
		    Port_Unlock(port);
	        }
	    }
	    port = port->next;
	}
	PL_Unlock(&bport_set[list]);
	list++;
    }
    return(bret);
}


static bmsg_ret_t
bmsg_inet_accept()
{
    struct sockaddr_in msg_sock_name;
    struct timeval timeout;
    emb_sock_t * emb_sock;
    int nof_ready_socks;
    fd_set readsockset;
    int accept_errno;
    int msg_sock;
    int namelen;

    timeout.tv_sec = 0;
    timeout.tv_usec = 0;

    SS_Lock();

    do {
	do {

	    /* 
	    ** Determine number of ready sockets.
	    */

            do {
                /* copy sockset into readsockset */
                (void) memcpy((char *) &readsockset,
		              (char *) &sockset,
		              sizeof(fd_set));
	        nof_ready_socks = select(sockset_size,&readsockset,(fd_set *) 0,
				         (fd_set *) 0,&timeout);
            }
            while ((nof_ready_socks < 0) && 
		   ((errno == EINTR) || (errno == EAGAIN)));

	    bmsg_assert(nof_ready_socks >= 0);

	    if (nof_ready_socks <= 0 || !FD_ISSET(ctrl_sock,&readsockset)) {
		msg_sock = 0;
		break;
	    }
	
	    /*
	    ** Accept possible new socket connection.
	    */

	    nof_ready_socks--;
	    emb_sock = (emb_sock_t *) 
		       pds_mem_alloc(&bmsg_phd,sizeof(emb_sock_t));
	    if (!emb_sock) { /* pds_mem_alloc() failed */
                SS_Unlock();
                bmsg_panic(BMSG_WEP_MEMORY,bport_self());
		exit(-1);
	    }

	    /* On some platforms accept() is not interrupt-safe ! */
	    {
    		sigset_t sigset_old;
    		sigset_t sigset;

		/* disable signal delivery */
		sigemptyset(&sigset);
#if defined(SIGIO)
            	sigaddset(&sigset,SIGIO);
#if (defined(SIGPOLL) && (SIGIO != SIGPOLL))
            	sigaddset(&sigset,SIGPOLL);
#endif
#else
#if defined(SIGPOLL)
            	sigaddset(&sigset,SIGPOLL);
#endif
#endif
#if defined(SIGURG)
            	sigaddset(&sigset,SIGURG);
#endif
#if defined(SIGPIPE)
            	sigaddset(&sigset,SIGPIPE);
#endif
#if defined(SIGUSR1)
            	sigaddset(&sigset,SIGUSR1);
#endif
#if defined(SIGUSR2)
            	sigaddset(&sigset,SIGUSR2);
#endif
		sigprocmask(SIG_BLOCK,&sigset,&sigset_old);
	    	pds_disable_int();
		/* signal free accept() */
	        namelen = sizeof(struct sockaddr_in);
	        msg_sock = accept(ctrl_sock, 
			          (struct sockaddr *) &msg_sock_name, 
			          &namelen);
		accept_errno = errno;
		/* reenable signal delivery */
	    	pds_enable_int();
		sigprocmask(SIG_SETMASK,&sigset_old,(sigset_t *) 0);
	    }

	    if (msg_sock == -1) { /* accept() failed */
		pds_mem_free(&bmsg_phd,(void_ptr) emb_sock);
		switch (accept_errno) {
		    case ENOMEM :
                    	bmsg_panic(BMSG_WEP_MEMORY,bport_self());
                    	exit(-1);
		    case ENOSR :
		    case ENOBUFS :
                    case EMFILE :
                    case ENFILE :
                        bmsg_panic(BMSG_WEP_RESOURCES,bport_self());
                        exit(-1);
 		    case EINTR :
		        break;
		    default :
                        bmsg_assert_always();
		    break;
		}
	    }
	    else { /* accepted a new message socket */
		bmsg_assert(msg_sock > 0); 
	        set_sock_nb(msg_sock);
	        set_sock_as(msg_sock);
		set_sock_nd(msg_sock);
		set_sock_ka(msg_sock);
		set_sock_li(msg_sock);
		set_sock_ni(msg_sock);
		/* add message socket to sockset */
		FD_SET(msg_sock,&sockset);
		/* add message socket to list of embryonic sockets */
		emb_sock->next = (emb_sock_t *) 0;
		emb_sock->sock = msg_sock;
		emb_sock->bport_id = 0;	/* anonymous embryonic socket */
		strncpy((char *) emb_sock->bnet_address,
                        inet_ntoa(* (struct in_addr *) 
				    &msg_sock_name.sin_addr),
                        BNET_ADDRESSLEN_MAX);
                emb_sock->bnet_address[BNET_ADDRESSLEN_MAX] = '\0';
		emb_sock->bport_number = ntohs(msg_sock_name.sin_port);
		EMB_Lock();
		emb_sock_put(emb_sock);
    		EMB_Unlock();
	    }
	} 
	while ((msg_sock == -1) && (accept_errno == EINTR));
	bmsg_assert(msg_sock >= 0); 
    }
    while (msg_sock); /* accept another possible socket connection */


    if ((bmsg_embsock_accept(&readsockset,
			     &nof_ready_socks) == BMSG_NOMESSAGE) &&
    	(bmsg_msgsock_accept(&readsockset,
			     &nof_ready_socks) == BMSG_NOMESSAGE)) {
        SS_Unlock();
	return(BMSG_NOMESSAGE);
    }
    
    SS_Unlock();

    return(BMSG_OK);
}


/* 
**    Function: bmsg_sys_receive()
** Description: Gets first buffer of lrcv_queue and invokes the message 
**		handler. The buffer is supposed to hold a system message, 
**		i.e. not a message of type BMSG_MESSAGE.
**        Note: The lrcv_queue lock should be hold on entering and is 
**		also hold on returning. It protects both the lrcv_queue
**		and the app_queue.
*/

static void
bmsg_sys_receive()
{
    bmsg_ret_t ret,ret1;
    bmsg_buffer_t * buffer;
    port_t * port;
    port_t * port_out;
    bport_id_t port_id;
    bmsg_t msg;
    bmsg_header_t * header;
    bmsg_itype_t msg_type;
    bmsg_size_t msg_data_size;
    bmsg_data_t * msg_data;

    /* port primitive related variables */
    bport_t bport;
    bpid_t bpid;
    bdomain_id_t domain_id;
    bmem_address_t msg_queue_address;
    bnet_address_t bnet_address;
    bport_number_t bport_number;
    emb_sock_t * emb_sock;

    /* memory primitive related variables */
    bmem_id_t mem_id;
    bmem_size_t mem_data_size;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;

    MQ_Get(&lrcv_queue,&buffer);

    header = &buffer->header;
    port_id = header->bport_id;
    msg_type = header->bmsg_type;
    msg_data_size = (bmsg_size_t) header->bmsg_data_size;
    msg_data = (bmsg_data_t *) buffer->data;

    bmsg_receive_loginfo(header);

    switch (msg_type) {

        case BMSG_PORT_OPEN :
        case BMSG_PORT_CLOSE :
        case BMSG_PORT_BLOCK :
        case BMSG_PORT_UNBLOCK :

            Portrequest_Get(Portrequest(msg_data),
                            &bpid,
                            &domain_id,
                            &msg_queue_address,
			    bnet_address,
			    &bport_number);

            /* validate parameters */
            bmsg_assert(port_id != (bport_id_t) 0);
            bmsg_assert(port_id != bport_self());
            bmsg_assert(domain_id != (bdomain_id_t) 0);
            bmsg_assert(msg_queue_address != (bmem_address_t) 0);

            /* buffer can be reused for replies */
            bmsg_assert(buffer->buf_data_bytes >= sizeof(bport_reply_t));
            header->bport_id = bport_self();
            header->bmsg_data_size = sizeof(bport_reply_t);

            break;

        case BMSG_PORT_DIE :
        case BMSG_PORT_OPEN_ACK :
        case BMSG_PORT_CLOSE_ACK :
        case BMSG_PORT_BLOCK_ACK :
        case BMSG_PORT_UNBLOCK_ACK :
	    break;

        case BMSG_MEM_PUT :

            MemPutRequest_Get(Memrequest(msg_data),
                              &mem_id,
                              &mem_dst_address,
                              &mem_data_size);

            /* buffer can be reused for replies */
            bmsg_assert(buffer->buf_data_bytes >= sizeof(bmem_reply_t));

            header->bport_id = bport_self();
            header->bmsg_data_size = sizeof(bmem_reply_t);

	    break;

        case BMSG_MEM_GET :

            MemGetRequest_Get(Memrequest(msg_data),
                              &mem_id,
                              &mem_src_address,
                              &mem_dst_address,
                              &mem_data_size);

	    break;

        case BMSG_MEM_PUT_ACK :
        case BMSG_MEM_GET_ACK :
	    break;

        default:
            /* unknown message type */
            bmsg_assert_always();
	    break;
    }
	
    if (msg_type == BMSG_PORT_OPEN) {

	if (!BmsgAddressOk(& Portrequest(msg_data)->msg_queue_address)) {
            /* cannot send a reply ! */
    	    LMQ_Unlock(&lrcv_queue);
            bmsg_panic(BMSG_WEP_INCONSISTENCY,port_id);
            exit(-1);
        }

        /* initialise bport */
        bport.bpid = bpid;
        bport.bport_id = port_id;
        bport.bdomain_id = domain_id;
        bport.bmsg_queue_address = msg_queue_address;
	strncpy((char *) bport.bnet_address,
		(char *) bnet_address,
		BNET_ADDRESSLEN_MAX);
        bport.bport_number = bport_number;

        /* allocate port */
        if ((ret = bport_allocate(&port,&bport)) != BMSG_OK) {
            /* cannot send a reply ! */
            (void) bmsg_free((bmsg_t) &buffer->next);
            bmsg_panic(BMSG_WEP_MEMORY,bport_self());
            exit(-1);
        }

        /* add port to port set */
        ret = add_port(port,&port_out);
    	LMQ_Unlock(&lrcv_queue);

        if (ret != BMSG_OK) {  /* port exists already */

            bport_deallocate(port);
            port = port_out;

            if (ret == BMSG_INVAL) { /* incompatible port */
                bmsg_panic(BMSG_WEP_INCONSISTENCY,port_id);
                exit(-1);
            }
            else if (bport_self() > port_id) {
                /* send negative reply */
                header->bmsg_type = BMSG_PORT_OPEN_ACK;
                Portreply_Put(Portreply(msg_data),BMSG_PNOTAVAILABLE);
                if (bmsg_sys_send(port,buffer) != BMSG_OK)
                    release_port(port);
                else { /* send positive reply (done in release_port()) */
                    release_port(port);
                    (void) bmsg_free((bmsg_t) &buffer->next);
                }
	    }
	}
        else {
	    port->state &= ~BPORT_EMBRYONIC;
            port->state = BPORT_TRANSITS | BPORT_REMOTE_OPENS;
            if (domain_id != bdomain_self()) {
                EMB_Lock();
                emb_sock_select(port_id,&emb_sock);
                if (!emb_sock) { /* port dying */
                    /* reuse buffer for BMSG_PORT_DIE message  */
                    bmsg_assert(buffer->buf_data_bytes >= sizeof(bport_request_t));
                    buffer->header.bmsg_type = BMSG_PORT_DIE;
                    buffer->header.bport_id = port->bport_id;
                    buffer->header.bmsg_data_size = sizeof(bport_request_t);
                    port->state |= BPORT_DYING;
                    PortRusers_Increment(port);
                    LMQ_Lock(&lrcv_queue);
                    MQ_Put(&lrcv_queue,buffer);
                    LMQ_Unlock(&lrcv_queue);
                }
                else {
                    strncpy((char *) port->bnet_address,
                            (char *) emb_sock->bnet_address,
                            BNET_ADDRESSLEN_MAX);
                    port->bport_number = emb_sock->bport_number;
                    port->sock = emb_sock->sock;
                    emb_sock_destroy(&emb_sock);
                }
                EMB_Unlock();
	    }
	    else {
		strncpy((char *) port->bnet_address,
                        (char *) port_local->bnet_address,
                        BNET_ADDRESSLEN_MAX);
                port->bport_number = 0;
                port->sock = 0;
	    }
            release_port(port);
        }
    }

    else {

        ret = acquire_port(port_id,&port);
        bmsg_assert(ret == BMSG_OK);
    	LMQ_Unlock(&lrcv_queue);

	switch (msg_type) {

            case BMSG_PORT_CLOSE :

                if (Port_Intransit(port) &&
                    (bport_self() > port_id)) { /* send negative reply */
                    header->bmsg_type = BMSG_PORT_CLOSE_ACK;
                    Portreply_Put(Portreply(msg_data),BMSG_PNOTAVAILABLE);
                    (void) bmsg_sys_send(port,buffer);
                }
                else { /* submit request */
                    port->state = BPORT_TRANSITS | BPORT_REMOTE_CLOSES;
                    (void) bmsg_free((bmsg_t) &buffer->next);
                }

                release_port(port);
                break;

            case BMSG_PORT_BLOCK :

                if (Port_Intransit(port) &&
                    (bport_self() > port_id)) { /* send negative reply */
                    header->bmsg_type = BMSG_PORT_BLOCK_ACK;
                    Portreply_Put(Portreply(msg_data),BMSG_PNOTAVAILABLE);
                    (void) bmsg_sys_send(port,buffer);
                }
                else {
                    port->state = BPORT_TRANSITS | BPORT_REMOTE_BLOCKS;
                    (void) bmsg_free((bmsg_t) &buffer->next);
                }

                release_port(port);
                break;

            case BMSG_PORT_UNBLOCK :

                if (Port_Intransit(port) &&
                    (bport_self() > port_id)) { /* send negative reply */
                    header->bmsg_type = BMSG_PORT_UNBLOCK_ACK;
                    Portreply_Put(Portreply(msg_data),BMSG_PNOTAVAILABLE);
                    (void) bmsg_sys_send(port,buffer);
                }
                else {
                    port->state = BPORT_TRANSITS | BPORT_REMOTE_UNBLOCKS;
                    (void) bmsg_free((bmsg_t) &buffer->next);
                }

                release_port(port);
                break;

            case BMSG_PORT_OPEN_ACK :

                PortRusers_Decrement(port);
                Portreply_Get(Portreply(msg_data),&ret);
                if (ret == BMSG_OK) {
                    port->state |= BPORT_OPENED;
                    port->state &= ~BPORT_TRANSITS;
                }

                release_port(port);
                (void) bmsg_free((bmsg_t) &buffer->next);
                bport_ack(port_id,BPORT_OPEN,ret);

                break;

            case BMSG_PORT_CLOSE_ACK :

                PortRusers_Decrement(port);
                Portreply_Get(Portreply(msg_data),&ret);
                if (ret == BMSG_OK) {
                    port->state &= ~BPORT_OPENED;
                    port->state &= ~BPORT_TRANSITS;
                    PortLusers_Decrement(port);
                    ret1 = remove_port(port);
	    	    if (ret1 != BMSG_PNOTAVAILABLE) {
                        bport_deallocate(port);
                        bmsg_assert(ret1 == BMSG_OK);
		    }
		    else {
                        PortLusers_Increment(port);
                        release_port(port);
		    }
                }
                else
                    release_port(port);

                (void) bmsg_free((bmsg_t) &buffer->next);
                bport_ack(port_id,BPORT_CLOSE,ret);

                break;

            case BMSG_PORT_BLOCK_ACK :

                PortRusers_Decrement(port);
                Portreply_Get(Portreply(msg_data),&ret);
                if (ret == BMSG_OK) {
                    port->state |= BPORT_BLOCKED;
                    port->state &= ~BPORT_TRANSITS;
                }

                release_port(port);
                (void) bmsg_free((bmsg_t) &buffer->next);
                bport_ack(port_id,BPORT_BLOCK,ret);

                break;

            case BMSG_PORT_UNBLOCK_ACK :

                PortRusers_Decrement(port);
                Portreply_Get(Portreply(msg_data),&ret);
                if (ret == BMSG_OK) {
                    port->state &= ~BPORT_BLOCKED
                                & ~BPORT_TRANSITS;
                }

                release_port(port);
                (void) bmsg_free((bmsg_t) &buffer->next);
                bport_ack(port_id,BPORT_UNBLOCK,ret);

                break;

            case BMSG_PORT_DIE :

                port->state &= ~BPORT_DYING;
                port->state |= BPORT_DEAD;
		port->rusers = 0;
                release_port(port);

                break;

            case BMSG_MEM_PUT :

                header->bmsg_type = BMSG_MEM_PUT_ACK;

                if (!BmsgAddressOk(& Memrequest(msg_data)->bmem_dst_address)) {
                    MemPutReply_Put(Memreply(msg_data),
                                    mem_id,
                                    BMSG_INVAL);
                    (void) bmsg_sys_send(port,buffer);
                    release_port(port);
                    break;
                }

                /* copy data from buffer to destination */
                bmem_cpy(mem_dst_address,
                         (bmem_address_t)
			 ((char *) msg_data + sizeof(bmem_request_t)),
                         mem_data_size);

                MemPutReply_Put(Memreply(msg_data),
                                mem_id,
                                BMSG_OK);

                (void) bmsg_sys_send(port,buffer);
		if (mem_id) {
	    	    MemInfo_Rput_Increment();
	    	    PortInfo_Rput_Increment(port);
                    release_port(port);
                    Bmem_notify(port_id,BMEM_PUT,mem_dst_address,mem_data_size);
		}
		else {
		    release_port(port);
		}
                break;

            case BMSG_MEM_PUT_ACK :

                PortRusers_Decrement(port);
                release_port(port);
                MemPutReply_Get(Memreply(msg_data),
                                &mem_id,
                                &ret);
                (void) bmsg_free((bmsg_t) &buffer->next);
		if (mem_id) {
                    bmem_ack(mem_id,BMEM_PUT,ret);
                    release_identifier(mem_id);
		}
                break;

            case BMSG_MEM_GET :

                /* buffer can be reused for simple replies */
                bmsg_assert(buffer->buf_data_bytes >= sizeof(bmem_reply_t));

                if (!BmsgAddressOk(&Memrequest(msg_data)->bmem_src_address)) {
                    header->bmsg_type = BMSG_MEM_GET_ACK;
            	    header->bport_id = bport_self();
            	    header->bmsg_data_size = sizeof(bmem_reply_t);

                    MemGetReply_Put(Memreply(msg_data),
                                    mem_id,
                                    (bmem_address_t) 0,
                                    BMSG_INVAL);
                    (void) bmsg_sys_send(port,buffer);
                    release_port(port);
                    break;
                }

                msg_data_size = sizeof(bmem_reply_t) + mem_data_size;
                if ((ret = bmsg_alloc(msg_data_size,&msg_data,&msg)) != BMSG_OK) {
                    header->bmsg_type = BMSG_MEM_GET_ACK;
            	    header->bport_id = bport_self();
            	    header->bmsg_data_size = sizeof(bmem_reply_t);
                    MemGetReply_Put(Memreply(msg_data),
                                    mem_id,
                                    (bmem_address_t) 0,
                                    ret);
                    (void) bmsg_sys_send(port,buffer);
                    release_port(port);
                    break;
                }
                /* free small buffer which carried request */
                (void) bmsg_free((bmsg_t) &buffer->next);

                buffer = (bmsg_buffer_t *) msg;
                header = &buffer->header;
                header->bmsg_type = BMSG_MEM_GET_ACK;
                header->bport_id = bport_self();
                header->bmsg_data_size = msg_data_size;

                MemGetReply_Put(Memreply(msg_data),
                                mem_id,
                                mem_dst_address,
                                BMSG_OK);

                /* copy source data in message buffer */
                bmem_cpy((bmem_address_t)
			 ((char *) msg_data + sizeof(bmem_request_t)),
                         mem_src_address,
                         mem_data_size);

                buffer = (bmsg_buffer_t *) msg;

                (void) bmsg_sys_send(port,buffer);
		if (mem_id) {
	    	    MemInfo_Rget_Increment();
	    	    PortInfo_Rget_Increment(port);
                    release_port(port);
                    Bmem_notify(port_id,BMEM_GET,mem_src_address,mem_data_size);
		}
		else {
                    release_port(port);
		}
                break;

            case BMSG_MEM_GET_ACK :

                bmsg_assert(ret == BMSG_OK);
                PortRusers_Decrement(port);
                MemGetReply_Get(Memreply(msg_data),
                                &mem_id,
                                &mem_dst_address,
                                &ret);
                if (ret == BMSG_OK) {
                    /* copy data from buffer to destination */
                    bmem_cpy(mem_dst_address,
			     (bmem_address_t)
                             ((char *) msg_data + sizeof(bmem_reply_t)),
                             msg_data_size - sizeof(bmem_reply_t));
                }      
                release_port(port);
                (void) bmsg_free((bmsg_t) &buffer->next);
		if (mem_id) {
                    bmem_ack(mem_id,BMEM_GET,ret);
                    release_identifier(mem_id);
		}
                break;

            default:
                /* unknown message type */
                bmsg_assert_always();
                break;
        }
    }
    LMQ_Lock(&lrcv_queue);
}


static void
release_port(port)
    port_t * port;
{
    bport_id_t port_id = port->bport_id;
    bmsg_t msg;
    bmsg_buffer_t * buffer;
    bmsg_header_t * header;
    bmsg_data_t * msg_data;
    bmsg_size_t msg_data_size;
    bmsg_ret_t bret;

    bmsg_assert(port->sock >= 0);

    if (Port_Dead(port) && PortOneLuser(port)) {
        PortLusers_Decrement(port);
	bret = remove_port(port);
        bmsg_assert(bret == BMSG_OK);
	if (!Port_Closed(port)) {
            bport_deallocate(port);
	    bmsg_error(BMSG_WEP_PDIED,port_id);
	}
	else
            bport_deallocate(port);
	return;
    }
    else if (!(Port_Intransit(port) && PortOneLuser(port) && 
	       PortNoRusers(port))) {
        PortLusers_Decrement(port);
        Port_Unlock(port);
        return;
    }

    /* allocate message buffer */
    bmsg_assert(sizeof(bport_request_t) > sizeof(bport_reply_t));
    msg_data_size = sizeof(bport_request_t);
    if (bmsg_alloc(msg_data_size,&msg_data,&msg) != BMSG_OK) {
        PortLusers_Decrement(port);
        Port_Unlock(port);
	bmsg_panic(BMSG_WEP_MEMORY,bport_self());
	exit(-1);
    }

    buffer = (bmsg_buffer_t *) msg;
    header = &buffer->header;
    header->bmsg_data_size = msg_data_size;
    header->bport_id = bport_self();

    if (port->state & BPORT_LOCAL_UNBLOCKS) { /* send request */
	port->state &= ~BPORT_LOCAL_UNBLOCKS;
        header->bmsg_type = BMSG_PORT_UNBLOCK;
	Portrequest_Put(Portrequest(msg_data),
			port_local->bpid,
			bdomain_self(),
			(bmem_address_t) grcv_queue,
			port_local->bnet_address,
			port_local->bport_number);
	if (bmsg_sys_send(port,buffer) == BMSG_OK)
	    PortRusers_Increment(port);
        PortLusers_Decrement(port);
	Port_Unlock(port);
    }

    else if (port->state & BPORT_REMOTE_UNBLOCKS) { /* send positive reply */
	port->state &= ~BPORT_REMOTE_UNBLOCKS
	            & ~BPORT_BLOCKED;
        header->bmsg_type = BMSG_PORT_UNBLOCK_ACK;
	Portreply_Put(Portreply(msg_data),BMSG_OK);
	(void) bmsg_sys_send(port,buffer);
	port->state &= ~BPORT_TRANSITS;
        PortLusers_Decrement(port);
	Port_Unlock(port);
	Bport_notify(port_id,BPORT_UNBLOCK);
    }

    else if (port->state & BPORT_LOCAL_BLOCKS) { /* send request */
	port->state &= ~BPORT_LOCAL_BLOCKS;
        header->bmsg_type = BMSG_PORT_BLOCK;
	Portrequest_Put(Portrequest(msg_data),
			port_local->bpid,
			bdomain_self(),
			(bmem_address_t) grcv_queue,
			port_local->bnet_address,
			port_local->bport_number);
	if (bmsg_sys_send(port,buffer) == BMSG_OK)
	    PortRusers_Increment(port);
        PortLusers_Decrement(port);
	Port_Unlock(port);
    }

    else if (port->state & BPORT_REMOTE_BLOCKS) { /* send positive reply */
	port->state &= ~BPORT_REMOTE_BLOCKS;
	port->state |= BPORT_BLOCKED;
        header->bmsg_type = BMSG_PORT_BLOCK_ACK;
	Portreply_Put(Portreply(msg_data),BMSG_OK);
	(void) bmsg_sys_send(port,buffer);
	port->state &= ~BPORT_TRANSITS;
        PortLusers_Decrement(port);
	Port_Unlock(port);
	Bport_notify(port_id,BPORT_BLOCK);
    }

    else if (port->state & BPORT_LOCAL_CLOSES) { /* send request */
	port->state &= ~BPORT_LOCAL_CLOSES;
        header->bmsg_type = BMSG_PORT_CLOSE;
	Portrequest_Put(Portrequest(msg_data),
			port_local->bpid,
			bdomain_self(),
			(bmem_address_t) grcv_queue,
			port_local->bnet_address,
			port_local->bport_number);
	if (bmsg_sys_send(port,buffer) == BMSG_OK)
	    PortRusers_Increment(port);
        PortLusers_Decrement(port);
	Port_Unlock(port);
    }

    else if (port->state & BPORT_REMOTE_CLOSES) { /* send positive reply */
	port->state &= ~BPORT_REMOTE_CLOSES 
		    & ~BPORT_OPENED;
        header->bmsg_type = BMSG_PORT_CLOSE_ACK;
	Portreply_Put(Portreply(msg_data),BMSG_OK);
	/* 
	** Isolating the port before sending the reply ensures
	** that it is invisible when it is being reopened (from
	** remote) more or less immediately. A close and a 
	** (re)open can therefore not clash.
	*/
        PortLusers_Decrement(port);
	bret = remove_port(port);
	if (bret != BMSG_OK)
	    bmsg_buffer_free(buffer);
	bmsg_assert(bret == BMSG_OK);
	(void) bmsg_sys_send(port,buffer);
	port->state &= ~BPORT_TRANSITS;
	bport_deallocate(port);
	Bport_notify(port_id,BPORT_CLOSE);
    }

    else if (port->state & BPORT_LOCAL_OPENS) { /* send request */
	port->state &= ~BPORT_LOCAL_OPENS;
        header->bmsg_type = BMSG_PORT_OPEN;
	Portrequest_Put(Portrequest(msg_data),
			port_local->bpid,
			bdomain_self(),
			(bmem_address_t) grcv_queue,
			port_local->bnet_address,
			port_local->bport_number);
	if (bmsg_sys_send(port,buffer) == BMSG_OK)
	    PortRusers_Increment(port);
        PortLusers_Decrement(port);
	Port_Unlock(port);
    }

    else { /* send positive reply */
	bmsg_assert(port->state & BPORT_REMOTE_OPENS);
	port->state &= ~BPORT_REMOTE_OPENS;
	port->state |= BPORT_OPENED;
        header->bmsg_type = BMSG_PORT_OPEN_ACK;
	Portreply_Put(Portreply(msg_data),BMSG_OK);
	(void) bmsg_sys_send(port,buffer);
	port->state &= ~BPORT_TRANSITS;
        PortLusers_Decrement(port);
	Port_Unlock(port);
	Bport_notify(port_id,BPORT_OPEN);
    }

    return;
}



/**********************************************************************
** Memory Primitives
***********************************************************************/

static bmsg_ret_t
bmem_pput(port,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    port_t * port;
    bmem_id_t mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    bmsg_ret_t bret;
    bmsg_t msg;
    bmsg_buffer_t * buffer;
    bmsg_data_t * msg_data;
    bmsg_size_t msg_data_size;

    msg_data_size = sizeof(bmem_request_t) + mem_data_size;
    if ((bret = bmsg_alloc(msg_data_size,&msg_data,&msg)) == BMSG_OK) {
	buffer = (bmsg_buffer_t *) msg;
	buffer->header.bmsg_type = BMSG_MEM_PUT;
	buffer->header.bport_id = bport_self();
	buffer->header.bmsg_data_size = msg_data_size;

	MemPutRequest_Put(Memrequest(msg_data),
			  mem_id,
			  mem_dst_address,
			  mem_data_size);

	/* copy source data in message buffer */
	bmem_cpy((bmem_address_t) ((char *) msg_data + sizeof(bmem_request_t)),
		 mem_src_address,
		 mem_data_size);

	if ((bret = bmsg_sys_send(port,buffer)) == BMSG_OK) {
	    PortRusers_Increment(port);
	}
    }
    return(bret);
}


static bmsg_ret_t
bmem_pget(port,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    port_t * port;
    bmem_id_t mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    bmsg_ret_t bret;
    bmsg_t msg;
    bmsg_buffer_t * buffer;
    bmsg_data_t * msg_data;
    bmsg_size_t msg_data_size;

    msg_data_size = sizeof(bmem_request_t);
    if ((bret = bmsg_alloc(msg_data_size,&msg_data,&msg)) == BMSG_OK) {
	buffer = (bmsg_buffer_t *) msg;
	buffer->header.bmsg_type = BMSG_MEM_GET;
	buffer->header.bport_id = bport_self();
	buffer->header.bmsg_data_size = msg_data_size;

        MemGetRequest_Put(Memrequest(msg_data),
                          mem_id,
                          mem_src_address,
                          mem_dst_address,
                          mem_data_size);

        if ((bret = bmsg_sys_send(port,buffer)) == BMSG_OK) {
	    PortRusers_Increment(port);
	}
    }
    return(bret);
}



/**********************************************************************
** Type Primitives
***********************************************************************/

msg_type_t MDT_INETADDRESS;
msg_type_t MDT_BDOMAIN;
msg_type_t MDT_BPORT;
msg_type_t MDT_BMSGINFO;
msg_type_t MDT_BMEMINFO;
msg_type_t MDT_BMSGVERSION;


static bmsg_ret_t
bmsg_types_init()
{
    pds_ret_t pret;

    msg_typedef_t mdt_bmsginfo[10];
    msg_typedef_t mdt_bmeminfo[8];
    msg_typedef_t mdt_inetaddress[5];
    msg_typedef_t mdt_bdomain[10];
    msg_typedef_t mdt_bport[10];
    msg_typedef_t mdt_version[6];

    mdt_bmsginfo[0] = MDT_BEGIN;
    mdt_bmsginfo[1] = MDT_STRUCT_OPEN;
    mdt_bmsginfo[2] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[3] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[4] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[5] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[6] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[7] = MDT_BMSGCOUNTER;
    mdt_bmsginfo[8] = MDT_STRUCT_CLOSE;
    mdt_bmsginfo[9] = MDT_END;

    pret = pds_type_define(BMSG_INTFC,1,mdt_bmsginfo,&MDT_BMSGINFO);

    if (pret == PDS_OK) {
        mdt_bmeminfo[0] = MDT_BEGIN;
        mdt_bmeminfo[1] = MDT_STRUCT_OPEN;
        mdt_bmeminfo[2] = MDT_BMSGCOUNTER;
        mdt_bmeminfo[3] = MDT_BMSGCOUNTER;
        mdt_bmeminfo[4] = MDT_BMSGCOUNTER;
        mdt_bmeminfo[5] = MDT_BMSGCOUNTER;
        mdt_bmeminfo[6] = MDT_STRUCT_CLOSE;
        mdt_bmeminfo[7] = MDT_END;
        pret = pds_type_define(BMSG_INTFC,2,mdt_bmeminfo,&MDT_BMEMINFO);
    }

    if (pret == PDS_OK) {
        mdt_inetaddress[0] = MDT_BEGIN;
        mdt_inetaddress[1] = MDT_ARRAY_OF;
        mdt_inetaddress[2] = INET_ADDRESSLEN_MAX+1;
        mdt_inetaddress[3] = MDT_CHAR;
        mdt_inetaddress[4] = MDT_END;
        pret = pds_type_define(BMSG_INTFC,3,mdt_inetaddress,&MDT_INETADDRESS);
    }

    if (pret == PDS_OK) {
        mdt_bdomain[0] = MDT_BEGIN;
        mdt_bdomain[1] = MDT_STRUCT_OPEN;
        mdt_bdomain[2] = MDT_BDOMAINID;
        mdt_bdomain[3] = MDT_ARRAY_OF;
        mdt_bdomain[4] = BMSG_FILENAMELEN_MAX+1;
        mdt_bdomain[5] = MDT_CHAR;
        mdt_bdomain[6] = MDT_BMSGADDRESS;
        mdt_bdomain[7] = MDT_BMEMSIZE;
        mdt_bdomain[8] = MDT_STRUCT_CLOSE;
        mdt_bdomain[9] = MDT_END;
        pret = pds_type_define(BMSG_INTFC,4,mdt_bdomain,&MDT_BDOMAIN);
    }

    if (pret == PDS_OK) {
        mdt_bport[0] = MDT_BEGIN;
        mdt_bport[1] = MDT_STRUCT_OPEN;
        mdt_bport[2] = MDT_BPID;
        mdt_bport[3] = MDT_BPORTID;
        mdt_bport[4] = MDT_BDOMAINID;
        mdt_bport[5] = MDT_BMSGADDRESS;
        mdt_bport[6] = MDT_BNETADDRESS;
        mdt_bport[7] = MDT_BPORTNUMBER;
        mdt_bport[8] = MDT_STRUCT_CLOSE;
        mdt_bport[9] = MDT_END;
        pret = pds_type_define(BMSG_INTFC,5,mdt_bport,&MDT_BPORT);
    }

    if (pret == PDS_OK) {
        mdt_version[0] = MDT_BEGIN;
        mdt_version[1] = MDT_STRUCT_OPEN;
        mdt_version[2] = MDT_INT32;
        mdt_version[3] = MDT_INT32;
        mdt_version[4] = MDT_STRUCT_CLOSE;
        mdt_version[5] = MDT_END;
        pret = pds_type_define(BMSG_INTFC,6,mdt_version,&MDT_BMSGVERSION);
    }

    if (pret == PDS_INVAL) {
	bmsg_assert_always();
    }
    else
	return((bmsg_ret_t) pret);
}



/**********************************************************************
** Debugging Support
***********************************************************************/

#if !defined(NDEBUG)

#define GRCV_QUEUE	1
#define LRCV_QUEUE	2
#define APP_QUEUE	3

void
bmsg_print_rcv_queue(queue)
    int queue;
{
    bmsg_buffer_t * buf;

    if (!bmsg_ready())
        return;

    switch (queue) {
	case GRCV_QUEUE :
    	    if (MQ_Empty(grcv_queue)) {
    	    	printf("\n");
                printf("Global Receive Queue Empty\n");
	        fflush(stdout);
	        return;
    	    }
    	    buf = grcv_queue->first;
	    break;
	case LRCV_QUEUE :
    	    if (MQ_Empty(&lrcv_queue)) {
    	    	printf("\n");
    	        printf("Local Receive Queue Empty\n");
	        fflush(stdout);
	        return;
    	    }
    	    buf = lrcv_queue.first;
	    break;
	case APP_QUEUE :
    	    if (MQ_Empty(&app_queue)) {
    	        printf("\n");
                printf("Application Receive Queue Empty\n");
	        fflush(stdout);
	        return;
    	    }
    	    buf = app_queue.first;
	    break;
	default :
	    return;
    }

    printf("\n");
    printf("------------------------------------------------\n");
    switch (queue) {
	case GRCV_QUEUE :
    	    printf("         Global Receive Queue         \n");
	    break;
	case LRCV_QUEUE :
    	    printf("         Local Receive Queue          \n");
	    break;
	case APP_QUEUE :
    	    printf("      Application Receive Queue       \n");
	    break;
	default :
	    return;
    }
    printf("------------------------------------------------\n");
    printf(" Sender |     Message Type | Message            \n");
    printf("--------+------------------+--------------------\n");

    do {
	printf(" %6d | ",buf->header.bport_id);
	switch (buf->header.bmsg_type) {
	    case BMSG_MSG :
		printf("%16s | ","APPLICATION");
		break;
	    case BMSG_PORT_OPEN :
		printf("%16s | ","PORT_OPEN");
		break;
	    case BMSG_PORT_OPEN_ACK :
		printf("%16s | ","PORT_OPEN_ACK");
		break;
	    case BMSG_PORT_CLOSE :
		printf("%16s | ","PORT_CLOSE");
		break;
	    case BMSG_PORT_CLOSE_ACK :
		printf("%16s | ","PORT_CLOSE_ACK");
		break;
	    case BMSG_PORT_BLOCK :
		printf("%16s | ","PORT_BLOCK");
		break;
	    case BMSG_PORT_BLOCK_ACK :
		printf("%16s | ","PORT_BLOCK_ACK");
		break;
	    case BMSG_PORT_UNBLOCK :
		printf("%16s | ","PORT_UNBLOCK");
		break;
	    case BMSG_PORT_UNBLOCK_ACK :
		printf("%16s | ","PORT_UNBLOCK_ACK");
		break;
	    case BMSG_PORT_DIE :
		printf("%16s | ","PORT_DIE");
		break;
	    case BMSG_MEM_PUT :
		printf("%16s | ","MEM_PUT");
		break;
	    case BMSG_MEM_PUT_ACK :
		printf("%16s | ","MEM_PUT_ACK");
		break;
	    case BMSG_MEM_GET :
		printf("%16s | ","MEM_GET");
		break;
	    case BMSG_MEM_GET_ACK :
		printf("%16s | ","MEM_GET_ACK");
		break;
	    default :
		printf("%16s | ","UNKNOWN");
	}
	printf("0x%lx ", (char *) buf->data - (char *) 0);
	printf("\n");
	buf = buf->next;
    } while (buf);
    printf("------------------------------------------------\n");
    printf("\n");
    fflush(stdout);
}


void
bmsg_print_grcv_queue()
{
    bmsg_print_rcv_queue(GRCV_QUEUE);
}


void
bmsg_print_lrcv_queue()
{
    bmsg_print_rcv_queue(LRCV_QUEUE);
}


void
bmsg_print_app_queue()
{
    bmsg_print_rcv_queue(APP_QUEUE);
}


void
bmsg_print_rcv_queues()
{
    bmsg_print_grcv_queue();
    bmsg_print_lrcv_queue();
    bmsg_print_app_queue();
}

#endif /* NDEBUG */




/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

/**********************************************************************
** Port Primitives
***********************************************************************/

bmsg_ret_t
bport_familiar(port_id,familiar)
    bport_id_t port_id;
    bmsg_bool_t * familiar;
{
    port_t * port;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !familiar) return(BMSG_INVAL);
#endif 

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);
    *familiar = (port->familiar);

    release_port(port);

    return(BMSG_OK);
}


bmsg_ret_t 
bport_port(port_id,bport)
    bport_id_t port_id;
    bport_t * bport;
{
    port_t * port;
    
    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !bport) return(BMSG_INVAL);
#endif 

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    bport->bpid = port->bpid;
    bport->bport_id = port->bport_id;
    bport->bdomain_id = port->bdomain_id;

    * (pds_address_t *) 
      ((char *) &bport->bmsg_queue_address + BMSG_ADDR_OFFSET) =
    * (pds_address_t *)
      ((char *) &port->send_queue + BMSG_ADDR_OFFSET);

    strncpy((char *) bport->bnet_address,
	    (char *) port->bnet_address,
            BNET_ADDRESSLEN_MAX);
    bport->bport_number = port->bport_number;

    release_port(port);

    return(BMSG_OK);
}


#define BMSG_CONNECT_TRIES_MAX 64

bmsg_ret_t 
bport_open(bport)
    bport_t * bport;
{
    bmsg_ret_t bret; 
    int ret;
    int msg_sock;
    port_t * port;
    port_t * port_out;
    struct sockaddr_in rem_ctrl_sock_name;
    unsigned long address;
    int tries = 0;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!bport ||   
	!bport->bport_id ||
	!bport->bdomain_id) return(BMSG_INVAL);
#endif

    if ((bret = bport_allocate(&port,bport)) != BMSG_OK)
	return(bret);

    /* add port to port set */
    bret = add_port(port,&port_out);

    if (bret != BMSG_OK) { /* port exists already */
        if (bret != BMSG_INVAL)
            release_port(port_out);
	bport_deallocate(port);
	return(bret);
    }

    /* establish socket connection for inter domain communication */
    if (bport->bdomain_id != bdomain_self()) {
        /* construct name of remote control socket */
	address = inet_addr(bport->bnet_address);
	if (address == -1) {
	    bmsg_perror_and_assert_always("bport_open() : inet_addr() ");
	}
	else {
            rem_ctrl_sock_name.sin_family = AF_INET;
            rem_ctrl_sock_name.sin_addr = * (struct in_addr *) &address;
            rem_ctrl_sock_name.sin_port = htons(bport->bport_number);

	    do {
		bret = BMSG_OK;

		/* create message socket */
	        do {
                    msg_sock = socket(PF_INET,SOCK_STREAM,0);
                    if (msg_sock < 0) {
	                switch (errno) {
		            case EMFILE :
		            case ENFILE :
		            case ENOBUFS :
		                bret = BMSG_NORESOURCES;
				break;
	        	    case EINTR :
		                break;
		            default :
	    	                bmsg_perror_and_assert_always("bport_open() : socket() ");
		                break;
	                }
	            }
		}
		while ((msg_sock < 0) && (errno == EINTR));

		if (bret != BMSG_OK)
		    break;

	        /* establish connection */
	        ret = connect(msg_sock,
		              (struct sockaddr *) &rem_ctrl_sock_name,
		              sizeof(struct sockaddr_in));
		if (ret == -1) {
	            switch (errno) {
	                case ENOSR :
		            bret = BMSG_NORESOURCES;
		            break;
		        case EADDRNOTAVAIL :
		            bret = BMSG_NOPORT;
		            break;
	                case ENETUNREACH :
		            bret = BMSG_PUNREACH;
		            break;
		        case ECONNREFUSED :
			case ETIMEDOUT :
	        	case EINTR :
			    bret = BMSG_PNOTAVAILABLE;
			    break;
			case EISCONN :    /* spurious error */
                	case EADDRINUSE : /* spurious error */
                	case EALREADY :   /* spurious error */
			    tries--;
			    bret = BMSG_PNOTAVAILABLE;
			    break;
		        default :
	    	            bmsg_perror_and_assert_always("bport_open() : connect() ");
		            break;
		    }
        	    SS_Lock();	
		    close_sock(msg_sock);
        	    SS_Unlock();	
		}
	    } while ((bret == BMSG_PNOTAVAILABLE) && 
		     (tries++ < BMSG_CONNECT_TRIES_MAX));

            /* set message socket to non-blocking 	       */
            /* set message socket to asynchronous 	       */
            /* set message socket to non-delayed transmission  */ 
            /* set message socket to keep connections alive    */ 
            /* set message socket to linger 		       */ 
            /* set message socket to not inherit on exec()     */
	    if (bret == BMSG_OK) {
                set_sock_nb(msg_sock);
                set_sock_as(msg_sock);
		set_sock_nd(msg_sock);
		set_sock_ka(msg_sock);
		set_sock_li(msg_sock);
		set_sock_ni(msg_sock);
                /* add message socket to sockset */
	        SS_Lock();
                FD_SET(msg_sock,&sockset);
	        SS_Unlock();
    		port->sock = msg_sock;
            }
	}
    }

    if (bret != BMSG_OK) {
	bmsg_ret_t bret1;

	PortLusers_Decrement(port);
	bret1 = remove_port(port);
	bmsg_assert(bret1 == BMSG_OK);
	bport_deallocate(port);
	return(bret);
    }

    port->state &= ~BPORT_EMBRYONIC;
    port->state = BPORT_TRANSITS | BPORT_LOCAL_OPENS;

    release_port(port);

    return(BMSG_POPENING);
}

    

bmsg_ret_t 
bport_close(port_id)
    bport_id_t port_id;
{
    bmsg_ret_t ret = BMSG_PCLOSING;
    port_t * port;
    
    bmsg_assert(bmsg_ready());

    if (!port_id || port_id == bport_self()) 
	return(BMSG_INVAL);

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (Port_Dying(port))
	ret = BMSG_PDYING;
    else if (Port_Intransit(port) || Port_Embryonic(port))
	ret = BMSG_PNOTAVAILABLE;
    else 
	port->state |= BPORT_TRANSITS | BPORT_LOCAL_CLOSES;

    release_port(port);

    return(ret);
}


bmsg_ret_t 
bport_flush(port_id)
    bport_id_t port_id;
{
    bmsg_ret_t ret;
    port_t * port;
    
    bmsg_assert(bmsg_ready());

    if (!port_id || port_id == bport_self()) 
	return(BMSG_INVAL);

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (Port_Dying(port))
        ret = BMSG_PDYING;
    else if (Port_Intransit(port) || Port_Embryonic(port) || Port_Blocked(port))
        ret = BMSG_PNOTAVAILABLE;
    else if (port->bdomain_id == bdomain_self())
        /* nothing to flush */
	ret = BMSG_OK;
    else { /* inter domain flushing currently not supported */
	bmsg_assert(port->sock != 0);
        ret = BMSG_NYI;
    }

    release_port(port);

    return(ret);
}


bmsg_ret_t 
bport_block(port_id)
    bport_id_t port_id;
{
    bmsg_ret_t ret = BMSG_PBLOCKING;
    port_t * port;
    
    bmsg_assert(bmsg_ready());

    if (!port_id || port_id == bport_self()) 
	return(BMSG_INVAL);

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (Port_Dying(port))
	ret = BMSG_PDYING;
    else if (Port_Intransit(port) || Port_Embryonic(port))
	ret = BMSG_PNOTAVAILABLE;
    else if (Port_Blocked(port))
	ret = BMSG_PBLOCKED;
    else
	port->state |= BPORT_TRANSITS | BPORT_LOCAL_BLOCKS;

    release_port(port);

    return(ret);
}


bmsg_ret_t 
bport_unblock(port_id)
    bport_id_t port_id;
{
    bmsg_ret_t ret = BMSG_PUNBLOCKING;
    port_t * port;
    
    bmsg_assert(bmsg_ready());

    if (!port_id || port_id == bport_self()) 
	return(BMSG_INVAL);

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (Port_Dying(port))
	ret = BMSG_PDYING;
    else if (Port_Intransit(port) || Port_Embryonic(port))
	ret = BMSG_PNOTAVAILABLE;
    else if (Port_Unblocked(port))
	    ret = BMSG_PUNBLOCKED;
    else 
	port->state |= BPORT_TRANSITS | BPORT_LOCAL_UNBLOCKS;

    release_port(port);

    return(ret);
}



/**********************************************************************
** Message Primitives
***********************************************************************/

bmsg_ret_t 
bmsg_alloc(size,data,msg)
    bmsg_size_t size;
    bmsg_data_t * * data;
    bmsg_t * msg;
{
    bmsg_ret_t ret;
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!msg) return(BMSG_INVAL);
#endif

    if ((ret = bmsg_buffer_alloc(size,&buffer)) != BMSG_OK)
	return(ret);

    *msg = (bmsg_t) buffer;
    if (data)
        *data = (bmsg_data_t *) buffer->data;

#if !defined(NDEBUG)
    buffer->next = (bmsg_buffer_t *) 0;
#endif
   
    return(BMSG_OK);
}


bmsg_ret_t 
bmsg_free(msg)
    bmsg_t msg;
{
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!msg) return(BMSG_INVAL);
#endif

    buffer = (bmsg_buffer_t *) msg;

    if (buffer->buf_data_bytes != BUF_DATA_BYTES)
	pds_mem_free_size(&bmsg_shd,(void_ptr) buffer,
		          sizeof(bmsg_buffer_t) - BUF_DATA_BYTES + 
		          buffer->buf_data_bytes);
    else {
	MBP_Lock();
	MBP_Put(buffer);
	if ((bmsg_buffer_pool_adjust(BUF_POOL_SIZE_MID)) != BMSG_OK) {
	    MBP_Unlock();
	    bmsg_warn(BMSG_WEP_MEMORY,bport_self());
	}
	else
	    MBP_Unlock();
    }

    return(BMSG_OK);
}


bmsg_size_t
bmsg_size(msg)
    bmsg_t msg;
{
    return((bmsg_size_t) 
	   ((bmsg_buffer_t *) msg)->buf_data_bytes);
}


bmsg_data_t *
bmsg_data(msg)
    bmsg_t msg;
{
    return((bmsg_data_t *) 
	   ((bmsg_buffer_t *) msg)->data);
}


bmsg_ret_t 
bmsg_send(port_id,msg,size)
    bport_id_t port_id;
    bmsg_t msg;
    bmsg_size_t size;
{
    bmsg_ret_t ret;
    port_t * port;
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !msg) return(BMSG_INVAL);
#endif
    
    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);
    
    if (port->state & (BPORT_DYING | 
		       BPORT_TRANSITS |
		       BPORT_EMBRYONIC |
		       BPORT_BLOCKED)) {
        if (Port_Dying(port))
	    ret = BMSG_PDYING;
        else if (Port_Intransit(port) || Port_Embryonic(port))
	    ret = BMSG_PNOTAVAILABLE;
        else if (Port_Blocked(port))
            ret = BMSG_PBLOCKED;
    }
    else {
        /* initialise header */
	buffer = (bmsg_buffer_t *) msg;
    	buffer->header.bmsg_type = BMSG_MSG;
    	buffer->header.bport_id = bport_self();
        buffer->header.bmsg_data_size = size;
	ret = bmsg_sys_send(port,buffer);
    }

    release_port(port);

    return(ret);
}


bmsg_ret_t 
bmsg_receive(msg,data,size,port_id,familiar)
    bmsg_t * msg;
    bmsg_data_t * * data;
    bmsg_size_t * size;
    bport_id_t * port_id;
    bmsg_bool_t * familiar;
{
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_ready());

    LMQ_Lock(&lrcv_queue); /* locks both app_queue and rcv_queue */

    if (!MQ_Empty(&app_queue)) { 
	MQ_Get(&app_queue,&buffer);
	LMQ_Unlock(&lrcv_queue);
	BMSGLOG_RCV_MESSAGE(buffer->header.bport_id);
	MsgInfo_Rcvd_Increment(buffer->header.bmsg_data_size);
	PortInfo_Rcvd_Increment(buffer->header.bport_id);
	if (familiar) 
	    *familiar = buffer->familiar;
	if (port_id) 
	    *port_id = buffer->header.bport_id;
	if (msg)
	    *msg = (bmsg_t) buffer;
	else {
	    (void) bmsg_free((bmsg_t) buffer);
	    return(BMSG_OK);
	}
	if (data) {
	    *data = (bmsg_data_t *) buffer->data;
    	    if (size) 
	        *size = buffer->header.bmsg_data_size;
	}
	return(BMSG_OK);
    }

    while (!MQ_Empty(&lrcv_queue)) {
        MQ_First(&lrcv_queue,&buffer);
	if (buffer->header.bmsg_type == BMSG_MSG) {
	    MQ_Get(&lrcv_queue,&buffer);
            LMQ_Unlock(&lrcv_queue);
	    BMSGLOG_RCV_MESSAGE(buffer->header.bport_id);
	    MsgInfo_Rcvd_Increment(buffer->header.bmsg_data_size);
	    PortInfo_Rcvd_Increment(buffer->header.bport_id);
	    if (familiar) 
	        *familiar = buffer->familiar;
	    if (port_id) 
		*port_id = buffer->header.bport_id;
	    if (msg)
	        *msg = (bmsg_t) buffer;
	    else {
	        (void) bmsg_free((bmsg_t) buffer);
	        return(BMSG_OK);
	    }
	    if (data) {
	        *data = (bmsg_data_t *) buffer->data;
    	        if (size) 
	            *size = buffer->header.bmsg_data_size;
	    }
	    return(BMSG_OK);
	}
        bmsg_sys_receive(buffer);
    }

    LMQ_Unlock(&lrcv_queue);

    return(BMSG_NOMESSAGE);
}


bmsg_ret_t 
bmsg_peek(msg,data,size,port_id,familiar)
    bmsg_t * msg;
    bmsg_data_t * * data;
    bmsg_size_t * size;
    bport_id_t * port_id;
    bmsg_bool_t * familiar;
{
    bmsg_buffer_t * buffer;

    bmsg_assert(bmsg_ready());

    LMQ_Lock(&lrcv_queue); /* locks both app_queue and rcv_queue */

    if (!MQ_Empty(&app_queue)) { 
	MQ_First(&app_queue,&buffer);
	if (familiar) 
	    *familiar = buffer->familiar;
	if (port_id) 
	    *port_id = buffer->header.bport_id;
	if (msg)
	    *msg = (bmsg_t) buffer;
	if (data)
	    *data = (bmsg_data_t *) buffer->data;
    	if (size) 
	    *size = buffer->header.bmsg_data_size;
	LMQ_Unlock(&lrcv_queue);
	return(BMSG_OK);
    }

    while (!MQ_Empty(&lrcv_queue)) {
        MQ_First(&lrcv_queue,&buffer);
	if (buffer->header.bmsg_type == BMSG_MSG) {
	    if (familiar) 
	        *familiar = buffer->familiar;
	    if (port_id) 
		*port_id = buffer->header.bport_id;
	    if (msg)
	        *msg = (bmsg_t) buffer;
	    if (data)
	        *data = (bmsg_data_t *) buffer->data;
    	    if (size) 
	        *size = buffer->header.bmsg_data_size;
            LMQ_Unlock(&lrcv_queue);
	    return(BMSG_OK);
	}
        bmsg_sys_receive(buffer);
    }

    LMQ_Unlock(&lrcv_queue);

    return(BMSG_NOMESSAGE);
}



/**********************************************************************
** Memory Primitives
***********************************************************************/

/*
** Configuration
** -------------
** When there are more than BEM_TRF_PIPELINE bytes to transfer on an 
** inter process memory transfer the memory transfer will be pipelined 
** by means of a sequence of smaller memory transfers. Each small memory 
** transfer will transfer not more than BMEM_TRF_BYTES_MAX bytes.
*/

#define BMEM_TRF_PIPELINE       2048
#define BMEM_TRF_BYTES_MAX      4096


#if defined(NDEBUG)
#define bmem_cpy_address_check(a_src,a_dst,a_size,m_src,m_dst,m_size)
#else
#define bmem_cpy_address_check(a_src,a_dst,a_size,m_src,m_dst,m_size) {	\
        bmsg_assert((char *) (a_dst) >= (char *) (m_dst));		\
	bmsg_assert((char *) (a_dst) + (a_size) <=			\
		    (char *) (m_dst) + (m_size));			\
        bmsg_assert((char *) (a_src) >= (char *) (m_src));		\
	bmsg_assert((char *) (a_src) + (a_size) <= 			\
		    (char *) (m_src) + (m_size));			\
}
#endif

void
bmem_cpy(mem_dst_address,mem_src_address,mem_data_size)
    bmem_address_t mem_dst_address;
    bmem_address_t mem_src_address;
    bmem_size_t mem_data_size;
{
    char * src;
    char * dst;
    char * src_end;
    char * dst_end;
    char tmp0;
    char tmp1;
    char tmp2;
    char tmp3;
    double * dst_double;
    double * src_double;
    double * src_double_end;
    double tmp_double0;
    double tmp_double1;
    double tmp_double2;
    double tmp_double3;
    int * dst_int;
    int * src_int;
    int * src_int_end;
    int tmp_int0;
    int tmp_int1;
    int tmp_int2;
    int tmp_int3;
    short * dst_short;
    short * src_short;
    short * src_short_end;
    short tmp_short0;
    short tmp_short1;
    short tmp_short2;
    short tmp_short3;
    long diff;
    long count;

    src = (char *) mem_src_address;
    dst = (char *) mem_dst_address;
    src_end = src + mem_data_size;
    dst_end = dst + mem_data_size;

    /* align src on a double */
    count = (src - (char *) 0) % sizeof(double);
    if (count > 0)
	count = sizeof(double) - count;
    else
	count = - count;
    while ((src < src_end) && (count-- > 0)) {
	bmem_cpy_address_check(src,dst,1,mem_src_address,
			       mem_dst_address,mem_data_size);
	*dst++ = *src++;
    }

    /* align src_end on double */
    count = (src_end - (char *) 0) % sizeof(double);
    if (count < 0)
        count = sizeof(double) + count;
    while ((src < src_end) && (count-- > 0)) {
	bmem_cpy_address_check(src_end-1,dst_end-1,1,mem_src_address,
			       mem_dst_address,mem_data_size);
	*--dst_end = *--src_end;
    }

    diff = dst - src;

    if (!(diff % sizeof(double))) {
	src_double = (double *) src;
	src_double_end = (double *) src_end;
	dst_double = (double *) dst;

	if ((src_double_end - src_double) < 4) {
	    while (src_double < src_double_end) {
	        bmem_cpy_address_check(src_double,dst_double,sizeof(double),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        *dst_double++ = *src_double++;
	    }
	    return;
	}

        /* align src_double on 4 doubles */
        switch ((src_double - (double *) 0) % (4 * sizeof(double))) {
	    case -1 :
	    case  3 :
	        bmem_cpy_address_check(src_double,dst_double,sizeof(double),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_double0 = src_double[0];
	        dst_double[0] = tmp_double0;
	        src_double++;
	        dst_double++;
		break;
	    case -2 :
	    case  2 :
	        bmem_cpy_address_check(src_double,dst_double,2*sizeof(double),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_double1 = src_double[1]; /* potential prefetch */
	        tmp_double0 = src_double[0];
	        dst_double[1] = tmp_double1; /* potential prefetch */
	        dst_double[0] = tmp_double0;
	        src_double += 2;
	        dst_double += 2;
		break;
	    case -3 :
	    case  1 :
	        bmem_cpy_address_check(src_double,dst_double,3*sizeof(double),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_double2 = src_double[2]; /* potential prefetch */
	        tmp_double0 = src_double[0];
	        tmp_double1 = src_double[1];
	        dst_double[2] = tmp_double2; /* potential prefetch */
	        dst_double[0] = tmp_double0;
	        dst_double[1] = tmp_double1;
	        src_double += 3;
	        dst_double += 3;
		break;
	    default :
		break;
	}

	count = (src_double_end - src_double) / 16;

        while (count-- > 0) {

	    bmem_cpy_address_check(src_double,dst_double,16*sizeof(double),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_double3 = src_double[3]; /* potential prefetch */
	    tmp_double0 = src_double[0];
	    tmp_double1 = src_double[1];
	    tmp_double2 = src_double[2];
	    dst_double[3] = tmp_double3; /* potential prefetch */
	    dst_double[0] = tmp_double0;
	    dst_double[1] = tmp_double1;
	    dst_double[2] = tmp_double2;

	    tmp_double3 = src_double[7]; /* potential prefetch */
	    tmp_double0 = src_double[4];
	    tmp_double1 = src_double[5];
	    tmp_double2 = src_double[6];
	    dst_double[7] = tmp_double3; /* potential prefetch */
	    dst_double[4] = tmp_double0;
	    dst_double[5] = tmp_double1;
	    dst_double[6] = tmp_double2;

	    tmp_double3 = src_double[11]; /* potential prefetch */
	    tmp_double0 = src_double[8];
	    tmp_double1 = src_double[9];
	    tmp_double2 = src_double[10];
	    dst_double[11] = tmp_double3; /* potential prefetch */
	    dst_double[8] = tmp_double0;
	    dst_double[9] = tmp_double1;
	    dst_double[10] = tmp_double2;

	    tmp_double3 = src_double[15]; /* potential prefetch */
	    tmp_double0 = src_double[12];
	    tmp_double1 = src_double[13];
	    tmp_double2 = src_double[14];
	    dst_double[15] = tmp_double3; /* potential prefetch */
	    dst_double[12] = tmp_double0;
	    dst_double[13] = tmp_double1;
	    dst_double[14] = tmp_double2;

	    src_double += 16;
	    dst_double += 16;
	}

	count = (src_double_end - src_double) / 4;
        while (count-- > 0) {
	    bmem_cpy_address_check(src_double,dst_double,4*sizeof(double),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_double3 = src_double[3]; /* potential prefetch */
	    tmp_double0 = src_double[0];
	    tmp_double1 = src_double[1];
	    tmp_double2 = src_double[2];
	    dst_double[3] = tmp_double3; /* potential prefetch */
	    dst_double[0] = tmp_double0;
	    dst_double[1] = tmp_double1;
	    dst_double[2] = tmp_double2;

	    src_double += 4;
	    dst_double += 4;
	}

	while (src_double < src_double_end) {
	    bmem_cpy_address_check(src_double,dst_double,sizeof(double),
				   mem_src_address,mem_dst_address,
				   mem_data_size);
	    *dst_double++ = *src_double++;
	}
    }

    else if (!(diff % sizeof(int))) {
	src_int = (int *) src;
	src_int_end = (int *) src_end;
	dst_int = (int *) dst;

	if ((src_int_end - src_int) < 4) {
	    while (src_int < src_int_end) {
	        bmem_cpy_address_check(src_int,dst_int,sizeof(int),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        *dst_int++ = *src_int++;
	    }
	    return;
	}

        /* align src_int on 4 integers */
        switch ((src_int - (int *) 0) % (4 * sizeof(int))) {
	    case -1 :
	    case  3 :
	        bmem_cpy_address_check(src_int,dst_int,sizeof(int),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_int0 = src_int[0];
	        dst_int[0] = tmp_int0;
	        src_int++;
	        dst_int++;
		break;
	    case -2 :
	    case  2 :
	        bmem_cpy_address_check(src_int,dst_int,2*sizeof(int),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_int1 = src_int[1]; /* potential prefetch */
	        tmp_int0 = src_int[0];
	        dst_int[1] = tmp_int1; /* potential prefetch */
	        dst_int[0] = tmp_int0;
	        src_int += 2;
	        dst_int += 2;
		break;
	    case -3 :
	    case  1 :
	        bmem_cpy_address_check(src_int,dst_int,3*sizeof(int),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_int2 = src_int[2]; /* potential prefetch */
	        tmp_int0 = src_int[0];
	        tmp_int1 = src_int[1];
	        dst_int[2] = tmp_int2; /* potential prefetch */
	        dst_int[0] = tmp_int0;
	        dst_int[1] = tmp_int1;
	        src_int += 3;
	        dst_int += 3;
		break;
	    default :
		break;
	}

	count = (src_int_end - src_int) / 16;

        while (count-- > 0) {

	    bmem_cpy_address_check(src_int,dst_int,16*sizeof(int),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_int3 = src_int[3]; /* potential prefetch */
	    tmp_int0 = src_int[0];
	    tmp_int1 = src_int[1];
	    tmp_int2 = src_int[2];
	    dst_int[3] = tmp_int3; /* potential prefetch */
	    dst_int[0] = tmp_int0;
	    dst_int[1] = tmp_int1;
	    dst_int[2] = tmp_int2;

	    tmp_int3 = src_int[7]; /* potential prefetch */
	    tmp_int0 = src_int[4];
	    tmp_int1 = src_int[5];
	    tmp_int2 = src_int[6];
	    dst_int[7] = tmp_int3; /* potential prefetch */
	    dst_int[4] = tmp_int0;
	    dst_int[5] = tmp_int1;
	    dst_int[6] = tmp_int2;

	    tmp_int3 = src_int[11]; /* potential prefetch */
	    tmp_int0 = src_int[8];
	    tmp_int1 = src_int[9];
	    tmp_int2 = src_int[10];
	    dst_int[11] = tmp_int3; /* potential prefetch */
	    dst_int[8] = tmp_int0;
	    dst_int[9] = tmp_int1;
	    dst_int[10] = tmp_int2;

	    tmp_int3 = src_int[15]; /* potential prefetch */
	    tmp_int0 = src_int[12];
	    tmp_int1 = src_int[13];
	    tmp_int2 = src_int[14];
	    dst_int[15] = tmp_int3; /* potential prefetch */
	    dst_int[12] = tmp_int0;
	    dst_int[13] = tmp_int1;
	    dst_int[14] = tmp_int2;

	    src_int += 16;
	    dst_int += 16;
	}

	count = (src_int_end - src_int) / 4;
        while (count-- > 0) {
	    bmem_cpy_address_check(src_int,dst_int,4*sizeof(int),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_int3 = src_int[3]; /* potential prefetch */
	    tmp_int0 = src_int[0];
	    tmp_int1 = src_int[1];
	    tmp_int2 = src_int[2];
	    dst_int[3] = tmp_int3; /* potential prefetch */
	    dst_int[0] = tmp_int0;
	    dst_int[1] = tmp_int1;
	    dst_int[2] = tmp_int2;

	    src_int += 4;
	    dst_int += 4;
	}

	while (src_int < src_int_end) {
	    bmem_cpy_address_check(src_int,dst_int,sizeof(int),
				   mem_src_address,mem_dst_address,
				   mem_data_size);
	    *dst_int++ = *src_int++;
	}
    }

    else if (!(diff % sizeof(short))) {
	src_short = (short *) src;
	src_short_end = (short *) src_end;
	dst_short = (short *) dst;

	if ((src_short_end - src_short) < 4) {
	    while (src_short < src_short_end) {
	        bmem_cpy_address_check(src_short,dst_short,sizeof(short),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        *dst_short++ = *src_short++;
	    }
	    return;
	}

        /* align src_short on 4 shorts */
        switch ((src_short - (short *) 0) % (4 * sizeof(short))) {
	    case -1 :
	    case  3 :
	        bmem_cpy_address_check(src_short,dst_short,sizeof(short),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_short0 = src_short[0];
	        dst_short[0] = tmp_short0;
	        src_short++;
	        dst_short++;
		break;
	    case -2 :
	    case  2 :
	        bmem_cpy_address_check(src_short,dst_short,2*sizeof(short),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_short1 = src_short[1]; /* potential prefetch */
	        tmp_short0 = src_short[0];
	        dst_short[1] = tmp_short1; /* potential prefetch */
	        dst_short[0] = tmp_short0;
	        src_short += 2;
	        dst_short += 2;
		break;
	    case -3 :
	    case  1 :
	        bmem_cpy_address_check(src_short,dst_short,3*sizeof(short),
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp_short2 = src_short[2]; /* potential prefetch */
	        tmp_short0 = src_short[0];
	        tmp_short1 = src_short[1];
	        dst_short[2] = tmp_short2; /* potential prefetch */
	        dst_short[0] = tmp_short0;
	        dst_short[1] = tmp_short1;
	        src_short += 3;
	        dst_short += 3;
		break;
	    default :
		break;
	}

	count = (src_short_end - src_short) / 16;

        while (count-- > 0) {

	    bmem_cpy_address_check(src_short,dst_short,16*sizeof(short),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_short3 = src_short[3]; /* potential prefetch */
	    tmp_short0 = src_short[0];
	    tmp_short1 = src_short[1];
	    tmp_short2 = src_short[2];
	    dst_short[3] = tmp_short3; /* potential prefetch */
	    dst_short[0] = tmp_short0;
	    dst_short[1] = tmp_short1;
	    dst_short[2] = tmp_short2;

	    tmp_short3 = src_short[7]; /* potential prefetch */
	    tmp_short0 = src_short[4];
	    tmp_short1 = src_short[5];
	    tmp_short2 = src_short[6];
	    dst_short[7] = tmp_short3; /* potential prefetch */
	    dst_short[4] = tmp_short0;
	    dst_short[5] = tmp_short1;
	    dst_short[6] = tmp_short2;

	    tmp_short3 = src_short[11]; /* potential prefetch */
	    tmp_short0 = src_short[8];
	    tmp_short1 = src_short[9];
	    tmp_short2 = src_short[10];
	    dst_short[11] = tmp_short3; /* potential prefetch */
	    dst_short[8] = tmp_short0;
	    dst_short[9] = tmp_short1;
	    dst_short[10] = tmp_short2;

	    tmp_short3 = src_short[15]; /* potential prefetch */
	    tmp_short0 = src_short[12];
	    tmp_short1 = src_short[13];
	    tmp_short2 = src_short[14];
	    dst_short[15] = tmp_short3; /* potential prefetch */
	    dst_short[12] = tmp_short0;
	    dst_short[13] = tmp_short1;
	    dst_short[14] = tmp_short2;

	    src_short += 16;
	    dst_short += 16;
	}

	count = (src_short_end - src_short) / 4;
        while (count-- > 0) {
	    bmem_cpy_address_check(src_short,dst_short,4*sizeof(short),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp_short3 = src_short[3]; /* potential prefetch */
	    tmp_short0 = src_short[0];
	    tmp_short1 = src_short[1];
	    tmp_short2 = src_short[2];
	    dst_short[3] = tmp_short3; /* potential prefetch */
	    dst_short[0] = tmp_short0;
	    dst_short[1] = tmp_short1;
	    dst_short[2] = tmp_short2;

	    src_short += 4;
	    dst_short += 4;
	}

	while (src_short < src_short_end) {
	    bmem_cpy_address_check(src_short,dst_short,sizeof(short),
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    *dst_short++ = *src_short++;
	}
    }

    else {

	if ((src_end - src) < 4) {
	    while (src < src_end) {
	        bmem_cpy_address_check(src,dst,1,
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        *dst++ = *src++;
	    }
	    return;
	}

        /* align src on 4 chars */
        switch ((src - (char *) 0) % (4 * sizeof(char))) {
	    case -1 :
	    case  3 :
	        bmem_cpy_address_check(src,dst,1,
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp0 = src[0];
	        dst[0] = tmp0;
	        src++;
	        dst++;
		break;
	    case -2 :
	    case  2 :
	        bmem_cpy_address_check(src,dst,2,
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp1 = src[1]; /* potential prefetch */
	        tmp0 = src[0];
	        dst[1] = tmp1; /* potential prefetch */
	        dst[0] = tmp0;
	        src += 2;
	        dst += 2;
		break;
	    case -3 :
	    case  1 :
	        bmem_cpy_address_check(src,dst,3,
				       mem_src_address,mem_dst_address,
				       mem_data_size);
	        tmp2 = src[2]; /* potential prefetch */
	        tmp0 = src[0];
	        tmp1 = src[1];
	        dst[2] = tmp2; /* potential prefetch */
	        dst[0] = tmp0;
	        dst[1] = tmp1;
	        src += 3;
	        dst += 3;
		break;
	    default :
		break;
	}

	count = (src_end - src) / 16;

        while (count-- > 0) {

	    bmem_cpy_address_check(src,dst,16,
				   mem_src_address,mem_dst_address,
				   mem_data_size);

	    tmp3= src[3]; /* potential prefetch */
	    tmp0 = src[0];
	    tmp1 = src[1];
	    tmp2 = src[2];
	    dst[3] = tmp3; /* potential prefetch */
	    dst[0] = tmp0;
	    dst[1] = tmp1;
	    dst[2] = tmp2;

	    tmp3 = src[7]; /* potential prefetch */
	    tmp0 = src[4];
	    tmp1 = src[5];
	    tmp2 = src[6];
	    dst[7] = tmp3; /* potential prefetch */
	    dst[4] = tmp0;
	    dst[5] = tmp1;
	    dst[6] = tmp2;

	    tmp3 = src[11]; /* potential prefetch */
	    tmp0 = src[8];
	    tmp1 = src[9];
	    tmp2 = src[10];
	    dst[11] = tmp3; /* potential prefetch */
	    dst[8] = tmp0;
	    dst[9] = tmp1;
	    dst[10] = tmp2;

	    tmp3 = src[15]; /* potential prefetch */
	    tmp0 = src[12];
	    tmp1 = src[13];
	    tmp2 = src[14];
	    dst[15] = tmp3; /* potential prefetch */
	    dst[12] = tmp0;
	    dst[13] = tmp1;
	    dst[14] = tmp2;

	    src += 16;
	    dst += 16;
	}

	count = (src_end - src) / 4;
        while (count-- > 0) {
	    bmem_cpy_address_check(src,dst,4,
				   mem_src_address,mem_dst_address,
				   mem_data_size);
	    tmp3 = src[3]; /* potential prefetch */
	    tmp0 = src[0];
	    tmp1 = src[1];
	    tmp2 = src[2];
	    dst[3] = tmp3; /* potential prefetch */
	    dst[0] = tmp0;
	    dst[1] = tmp1;
	    dst[2] = tmp2;

	    src += 4;
	    dst += 4;
	}

	while (src < src_end) {
	    bmem_cpy_address_check(src,dst,1,
				   mem_src_address,mem_dst_address,
				   mem_data_size);
	    *dst++ = *src++;
	}
    }
}


bmsg_ret_t 
bmem_put(port_id,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    bport_id_t port_id;
    bmem_id_t * mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    bmsg_ret_t bret;
    port_t * port;
    bmsg_warn_t bmsg_warning = BMSG_WEP_NONE;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !mem_id) return(BMSG_INVAL);
#endif

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (port->state & (BPORT_DYING | 
		       BPORT_TRANSITS |
		       BPORT_EMBRYONIC |
		       BPORT_BLOCKED)) {
        if (Port_Dying(port))
	    bret = BMSG_PDYING;
        else if (Port_Intransit(port) || Port_Embryonic(port))
	    bret = BMSG_PNOTAVAILABLE;
        else if (Port_Blocked(port))
            bret = BMSG_PBLOCKED;
        release_port(port);
        return(bret);
    }

    else {

	if (mem_data_size > BMEM_TRF_PIPELINE) { /* pipelining */

	    bmem_size_t size;
    	    unsigned pputs;

	    /* determine number of partial puts */
	    if (mem_data_size < 2 * BMEM_TRF_BYTES_MAX) {
	        pputs = 2;
	        size = mem_data_size / 2;
	    }
	    else {
	        pputs = mem_data_size / BMEM_TRF_BYTES_MAX + 1;
	        size = BMEM_TRF_BYTES_MAX;
	    }

	    while (pputs-- > 1) {
	        bret = bmem_pput(port,0,mem_src_address,
			        mem_dst_address,size);
	        if (bret != BMSG_OK) {
                    release_port(port);
                    return(bret);
                }
	        mem_data_size -= size;
	        mem_src_address = (bmem_address_t) 
				  ((char *) mem_src_address + size);
	        mem_dst_address = (bmem_address_t) 
				  ((char *) mem_dst_address + size);
	        if (pputs == 2)
	            size = mem_data_size / 2;
		/* allow for replies to arrive */
	        Port_Unlock(port);
		(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
	        Port_Lock(port);
	    }
	}

	/* acquire identifier */
	if (!(*mem_id = port->mem_id)) {
	    if ((bret = acquire_identifier(mem_id)) == BMSG_WARN)
		bmsg_warning = BMSG_WEP_ID;
	    else if (bret != BMSG_OK) {
                release_port(port);
                return(bret);
	    }
	}
	else
	    port->mem_id = (bmem_id_t) 0;

	/* final put */
	if ((bret = bmem_pput(port,*mem_id,mem_src_address,
			     mem_dst_address,mem_data_size)) == BMSG_OK) {
	    MemInfo_Lput_Increment();
	    PortInfo_Lput_Increment(port);
	    /* preacquire identifier */
	    (void) acquire_identifier(&port->mem_id);
	}
	else /* restore identifier */
	    port->mem_id = *mem_id;

        release_port(port);
        if (bmsg_warning != BMSG_WEP_NONE)
	    bmsg_warn(bmsg_warning,bport_self());
        return(bret);
    }
}


bmsg_ret_t 
bmem_get(port_id,mem_id,mem_src_address,mem_dst_address,mem_data_size)
    bport_id_t port_id;
    bmem_id_t * mem_id;
    bmem_address_t mem_src_address;
    bmem_address_t mem_dst_address;
    bmem_size_t mem_data_size;
{
    bmsg_ret_t bret;
    port_t * port;
    bmsg_warn_t bmsg_warning = BMSG_WEP_NONE;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !mem_id) return(BMSG_INVAL);
#endif

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    if (port->state & (BPORT_DYING | 
		       BPORT_TRANSITS |
		       BPORT_EMBRYONIC |
		       BPORT_BLOCKED)) {
        if (Port_Dying(port))
	    bret = BMSG_PDYING;
        else if (Port_Intransit(port) || Port_Embryonic(port))
	    bret = BMSG_PNOTAVAILABLE;
        else if (Port_Blocked(port))
            bret = BMSG_PBLOCKED;
        release_port(port);
        return(bret);
    }

    else {

	if (mem_data_size > BMEM_TRF_PIPELINE) { /* pipelining */

	    bmem_size_t size;
    	    unsigned pgets;

	    /* determine number of partial gets */
	    if (mem_data_size < 2 * BMEM_TRF_BYTES_MAX) {
	        pgets = 2;
	        size = mem_data_size / 2;
	    }
	    else {
	        pgets = mem_data_size / BMEM_TRF_BYTES_MAX + 1;
	        size = BMEM_TRF_BYTES_MAX;
	    }

	    while (pgets-- > 1) {
	        bret = bmem_pget(port,0,mem_src_address,
			        mem_dst_address,size);
	        if (bret != BMSG_OK) {
                    release_port(port);
                    return(bret);
                }
	        mem_data_size -= size;
	        mem_src_address = (bmem_address_t) 
				  ((char *) mem_src_address + size);
	        mem_dst_address = (bmem_address_t) 
				  ((char *) mem_dst_address + size);
	        if (pgets == 2)
	            size = mem_data_size / 2;
		/* allow for replies to arrive */
	        Port_Unlock(port);
		(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
	        Port_Lock(port);
	    }
	}

	/* acquire identifier */
	if (!(*mem_id = port->mem_id)) {
	    if ((bret = acquire_identifier(mem_id)) == BMSG_WARN)
		bmsg_warning = BMSG_WEP_ID;
	    else if (bret != BMSG_OK) {
                release_port(port);
                return(bret);
	    }
	}
	else
	    port->mem_id = (bmem_id_t) 0;

	/* final get */
	if ((bret = bmem_pget(port,*mem_id,mem_src_address,
			     mem_dst_address,mem_data_size)) == BMSG_OK) {
	    MemInfo_Lget_Increment();
	    PortInfo_Lget_Increment(port);
	    /* preacquire identifier */
	    (void) acquire_identifier(&port->mem_id);
	}
	else /* restore identifier */
	    port->mem_id = *mem_id;

        release_port(port);
        if (bmsg_warning != BMSG_WEP_NONE)
	    bmsg_warn(bmsg_warning,bport_self());
        return(bret);
    }
}



/**********************************************************************
** Miscellaneous Primitives
***********************************************************************/

bmsg_ret_t 
bmsg_version(version)
    bmsg_version_t * version;
{
#if !defined(TRUSTED)
    if (!version) return(BMSG_INVAL);
#endif 

    version->v_major = BMSG_VERSION_MAJOR;
    version->v_minor = BMSG_VERSION_MINOR;

    return(BMSG_OK);
}


bmsg_ret_t
bmsg_init(port_id,domain,option)
    bport_id_t port_id;
    bdomain_t * domain;
    bmsg_option_t option;
{
    bmsg_ret_t bret;
    port_t * port_out;
    bport_t bport;
    char * bdomain_start;
    struct sockaddr_in ctrl_sock_name;
    int length;
    struct hostent *hostentry;
    char hostname[MAXHOSTNAMELEN+1];

    if (bmsg_initialised || bmsg_initialising || bmsg_exited || bmsg_exiting) 
	return(BMSG_ERROR);

    bmsg_initialising = 1;

#if !defined(NDEBUG)
    /* check compatibility of pointer range */
    if (sizeof(bmem_address_t) > sizeof(bmsg_address_t)) 
	return(BMSG_IMPLIM);
#endif

#if !defined(TRUSTED)
    /* check constraints on port_id */
    if (!port_id) return(BMSG_INVAL);

    /* check constraints on domain */
    if (!domain) return(BMSG_INVAL);
    if (!domain->bdomain_id) return(BMSG_INVAL);
#endif

    if (domain->bdomain_size < MDT_HEAP_SIZE + BMSG_HEAP_SIZE_MIN)
	return(BMSG_INVAL);

    /* initialise local identifiers */
    bport_id_local = port_id;
    bdomain_id_local = domain->bdomain_id;

    /* initialise global mutex */
    l_mutex_init(&bmsg_mutex);

    /* copy identifiers into bport structure */
    bport.bport_id = port_id;
    bport.bdomain_id = domain->bdomain_id;

    /* store local process identifier in bport structure */
    bport.bpid = (bpid_t) getpid();

    if (option & BPORT_NOTIFICATION) bport_notification = 1;
    if (option & BMEM_NOTIFICATION) bmem_notification = 1;

    /* initialise logging system */
    bmsg_initlog(option);

    /* initialise XDR system */
    bmsg_xdr_init();

    /* initialise bdomain_start */
    bdomain_start = (char *) domain->bdomain_start;

    /* initialise type system */
    if (pds_types_init(bdomain_start) != PDS_OK)
	return(BMSG_NORESOURCES);
    if ((bret = bmsg_types_init()) != BMSG_OK)
	return(bret);

    /* create or attach to shared heap */
    if (option & BDOMAIN_CREATE) {
	if (bdomain_start)
	    bdomain_start += MDT_HEAP_SIZE;
        if (pds_mem_init(domain->bdomain_file,
			 bdomain_start,
		         domain->bdomain_size - MDT_HEAP_SIZE,
			 &bmsg_shd,1) == (char *) -1)
	    return(BMSG_NOMEMORY);
    }
    else {
        if (pds_mem_init(domain->bdomain_file,(char *) 0,
			 0,&bmsg_shd,0) == (char *) -1)
	    return(BMSG_NOMEMORY);
    }
    
    /* ensure a valid full size domain start address */ 
    bmsg_address((bmem_address_t) domain->bdomain_start,
                 (bmsg_address_t *)
                 ((char *) &domain->bdomain_start + BMSG_ADDR_OFFSET));

    /* initialise sockset */
    sockset_init();

    /* initialise port set */
    bport_set_init();

    /* initialise buffer pool */
    if ((bret = bmsg_buffer_pool_init()) != BMSG_OK)
	return(bret);

    /* initialise list of embryonic sockets */
    emb_sock_list_init();

    /* initialise identifier generator */
    identifier_generator_init();

    /* initialise statistics */
    bmsg_info_init();
    bmem_info_init();

    /* 
    ** Create receive queue in shared memory such that its size is
    ** equal to the size of a bmsg cache line. Furthermore, it is
    ** aligned with a bmsg cache line since that is ensured by the
    ** memory allocator.
    */
    grcv_queue = (gmsg_queue_t *) 
		 pds_mem_alloc_size(&bmsg_shd,
			            (sizeof(gmsg_queue_t) % BMSG_LINE_SIZE) ? 
			            (sizeof(gmsg_queue_t) + BMSG_LINE_SIZE - 
			            sizeof(gmsg_queue_t) % BMSG_LINE_SIZE) :
			    	    (sizeof(gmsg_queue_t)));
    if (!grcv_queue) 
	return(BMSG_NOMEMORY);

    /* copy receive queue address into bport structure */
    /* ensure a valid full size receive queue address  */
    bmsg_address((bmem_address_t) grcv_queue,
	         (bmsg_address_t *) 
	         ((char *) &bport.bmsg_queue_address + BMSG_ADDR_OFFSET));

    global_message_queue_init(grcv_queue);
    local_message_queue_init(&lrcv_queue);
    local_message_queue_init(&app_queue);

    /* enable intra domain triggering */
    grcv_queue->option |= BMSG_TRIGGERING;
    bmsg_intra_domain_triggering = BMSG_ON;

    /* create control socket */
    do {
	ctrl_sock = socket(PF_INET,SOCK_STREAM,0);
        if (ctrl_sock < 0) {
	    switch (errno) {
	        case EMFILE :
	        case ENFILE :
	        case ENOBUFS :
		    return(BMSG_NORESOURCES);
	        case EINTR :
		    break;
	        default :
		    bmsg_perror_and_assert_always("bmsg_init() : socket() ");
	    }
        }
    }
    while ((ctrl_sock < 0) && (errno == EINTR));

    /* name control socket */
    ctrl_sock_name.sin_family = AF_INET;
    ctrl_sock_name.sin_addr.s_addr = htonl(INADDR_ANY);
    ctrl_sock_name.sin_port = 0;
    if (bind(ctrl_sock,
	     (struct sockaddr *) &ctrl_sock_name,
	     sizeof(struct sockaddr_in)) < 0) {
	bmsg_perror_and_assert_always("bmsg_init() : bind() ");
    }

    /* set control socket to asynchronous */
    set_sock_as(ctrl_sock);

    /* disable control socket inheritance on exec() */
    set_sock_ni(ctrl_sock);

    /* add control socket to sockset */
    SS_Lock();
    FD_SET(ctrl_sock,&sockset);
    SS_Unlock();

    /* get portno and copy it into bport structure */
    length = sizeof(struct sockaddr_in);
    if (getsockname(ctrl_sock,(struct sockaddr *) 
			      &ctrl_sock_name,&length) < 0) {
	if (errno == ENOBUFS)
	    return(BMSG_NORESOURCES);
	else {
	    bmsg_perror_and_assert_always("bmsg_init() : getsockname() ");
	}
    }
    bport.bport_number = ntohs(ctrl_sock_name.sin_port);

    /* get internet address and copy it into bport structure */
    if (gethostname(hostname,MAXHOSTNAMELEN) != 0) {
	bmsg_perror_and_assert_always("bmsg_init() : gethostname() ");
    }
    hostentry = gethostbyname(hostname);
    if ((!hostentry) || (hostentry->h_addrtype != AF_INET)) {
	bmsg_perror_and_assert_always("bmsg_init() : gethostbyname() ");
    }
    if (hostentry->h_length != 4) { /* wrong internet address length */
	bmsg_assert_always();
    }
    strncpy((char *) bport.bnet_address,
	    inet_ntoa(* (struct in_addr *) *hostentry->h_addr_list),
	    BNET_ADDRESSLEN_MAX);
    bport.bnet_address[BNET_ADDRESSLEN_MAX] = '\0';
    
    /* allocate port */
    if ((bret = bport_allocate(&port_local,&bport)) != BMSG_OK)
	return(bret);

    port_local->state &= ~BPORT_EMBRYONIC;
    port_local->state |= BPORT_OPENED;

    /* add port to port set */
    bret = add_port(port_local,&port_out);
    bmsg_assert(bret == BMSG_OK);

    release_port(port_local);

    bmsg_openlog();

    /* listen for connections on control socket */
    if (listen(ctrl_sock,SOCK_BACKLOG) == -1) {
	bmsg_perror_and_assert_always("bmsg_init() : listen() ");
    }

    /* initialisation was successful */
    bmsg_initialised = 1;
    bmsg_initialising = 0;

    /* get messages that may have arrived during initialisation */
    (void) bmsg_trigger(BMSG_INTER_DOMAIN | BMSG_INTRA_DOMAIN);

    return(BMSG_OK);
}


void
bmsg_exit()
{
    port_t * port;
    bport_id_t port_id;
    bport_list_t * list;
    bmsg_ret_t bret;
    int i;

    if (!bmsg_initialised || bmsg_exiting || bmsg_exited)
	return;

    /* warn for existence of connections to other ports */
    i = 0;
    while (i < PORTSET_SIZE) {
	list = &bport_set[i];
        PL_Lock(list);
	if (PL_Empty(list))
	    i++;
	else {
	    port = list->first;
	    while (port && (port == port_local))
		port = port->next;
	    if (!port) 
	        i++;
	    else { /* give warning */
		port_id = port->bport_id;
                PL_Unlock(list);
	        bmsg_warn(BMSG_WEP_PORT,port_id);
		PL_Lock(list);
		port = list->first;
		while (port && (port->bport_id != port_id))
		    port = port->next;
		if (port) { /* don't insist by giving further warnings */
		    bmsg_assert(port->bport_id == port_id)
		    PL_Unlock(list);
		    break;
		}
	    }
	}
        PL_Unlock(list);
    }

    bmsg_exiting = 1;

    /* remove local port from port set */
    bret = acquire_port(bport_self(),&port);
    bmsg_assert(bret == BMSG_OK);
    bmsg_assert(port == port_local);
    PortLusers_Decrement(port);
    bret = remove_port(port);
    bmsg_assert(bret == BMSG_OK);

    /* deallocate local port */
    bport_deallocate(port);

    /* clear receive queues */
    LMQ_Lock(&lrcv_queue);  	/* locks both app_queue and rcv_queue */
    local_message_queue_clear(&lrcv_queue);
    local_message_queue_clear(&app_queue);
    LMQ_Unlock(&lrcv_queue);
    GMQ_Lock(grcv_queue);
    global_message_queue_clear(grcv_queue);
    GMQ_Unlock(grcv_queue);

    /* deallocate global receive queue */
    pds_mem_free_size(&bmsg_shd,(void_ptr) grcv_queue,
	              (sizeof(bmsg_queue_t) % BMSG_LINE_SIZE) ? 
	              (sizeof(bmsg_queue_t) + BMSG_LINE_SIZE - 
	              sizeof(bmsg_queue_t) % BMSG_LINE_SIZE) :
	              (sizeof(bmsg_queue_t)));

    /* clear message buffer pool */
    MBP_Lock();
    (void) bmsg_buffer_pool_adjust(0);
    MBP_Unlock();

    pds_mem_release(&bmsg_shd);

    bmsg_closelog();

    bmsg_exited = 1;
    bmsg_exiting = 0;
}


bmsg_ret_t 
bmsg_set_option(optname,optval)
    bmsg_optname_t optname;
    bmsg_optval_t optval;
{
    bmsg_assert(bmsg_ready());

    if (optname == BMSG_INTRA_DOMAIN_TRIGGERING) {
	switch (optval) {
	    case BMSG_OFF :
		Bmsg_Lock();
		if (bmsg_intra_domain_triggering == BMSG_ON) {
		    bmsg_intra_domain_triggering = BMSG_OFF;
    		    GMQ_Lock(grcv_queue);
		    grcv_queue->option &= ~BMSG_TRIGGERING;
    		    GMQ_Unlock(grcv_queue);
		}
		bmsg_assert(bmsg_intra_domain_triggering == BMSG_OFF);
		Bmsg_Unlock();
		return(BMSG_OK);
	    case BMSG_ON :
		Bmsg_Lock();
		if (bmsg_intra_domain_triggering == BMSG_OFF) {
		    bmsg_intra_domain_triggering = BMSG_ON;
		    Bmsg_Unlock();
    		    GMQ_Lock(grcv_queue);
		    grcv_queue->option |= BMSG_TRIGGERING;
    		    if (!(MQ_Empty(grcv_queue))) {
    		        GMQ_Unlock(grcv_queue);
			(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
		    }
		    else
    		        GMQ_Unlock(grcv_queue);
		}
		else
                    Bmsg_Unlock();
                return(BMSG_OK);
	    default :
        	return(BMSG_INVAL);
	}
    }
    else
        return(BMSG_INVAL);
}


bmsg_ret_t
bmsg_get_option(optname,optval)
    bmsg_optname_t optname;
    bmsg_optval_t * optval;
{
    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!optval) return(BMSG_INVAL);
#endif

    if (optname == BMSG_INTRA_DOMAIN_TRIGGERING) {
	Bmsg_Lock();
	*optval = (bmsg_optval_t) bmsg_intra_domain_triggering;
	Bmsg_Unlock();
	return(BMSG_OK);
    }
    else
        return(BMSG_INVAL);
}


bmsg_ret_t 
bmsg_trigger(option)
    bmsg_option_t option;
{
    bmsg_ret_t bret;
    bmsg_buffer_t * buffer;

    if (!bmsg_ready())
	return(BMSG_NOMESSAGE);

    /* accept messages in local receive queue lrcv_queue */
    if ( (option & BMSG_INTRA_DOMAIN) && 
        !(option & BMSG_INTER_DOMAIN)) {
	bret = bmsg_intra_domain_accept();
	if (bret == BMSG_NOMESSAGE)
	    return(bret);
    }
    else if (!(option & BMSG_INTRA_DOMAIN) && 
	      (option & BMSG_INTER_DOMAIN)) {
	bret = bmsg_inet_accept();
	if (bret == BMSG_NOMESSAGE)
	    return(bret);
    }
    else if ((option & BMSG_INTRA_DOMAIN) && 
             (option & BMSG_INTER_DOMAIN)) {
	bret = bmsg_inet_accept();
	if (bret != BMSG_NOMESSAGE) 
	    (void) bmsg_intra_domain_accept();
	else
	    bret = bmsg_intra_domain_accept();
	if (bret == BMSG_NOMESSAGE)
	    return(bret);
    }
    else {
	return(BMSG_NOMESSAGE);
    }


    /* 
    ** Put messages to be handled by application into app_queue.
    ** Handle system messages, i.e. the memory and port messages. 
    */

    LMQ_Lock(&lrcv_queue); /* locks both app_queue and rcv_queue */

    while (!MQ_Empty(&lrcv_queue)) {

	MQ_First(&lrcv_queue,&buffer);

	switch (buffer->header.bmsg_type) {
            case BMSG_MSG :
		if (MQ_Empty(&app_queue)) {
		    MQ_Move(&app_queue,&lrcv_queue);
	            LMQ_Unlock(&lrcv_queue);
		    Bmsg_notify();
		    LMQ_Lock(&lrcv_queue);
		}
		else {
		    MQ_Move(&app_queue,&lrcv_queue);
		}
		break;
	    case BMSG_PORT_OPEN :
            case BMSG_PORT_CLOSE :
            case BMSG_PORT_BLOCK :
            case BMSG_PORT_UNBLOCK :
            case BMSG_PORT_DIE :
            case BMSG_PORT_OPEN_ACK :
            case BMSG_PORT_CLOSE_ACK :
            case BMSG_PORT_BLOCK_ACK :
            case BMSG_PORT_UNBLOCK_ACK :
            case BMSG_MEM_PUT :
            case BMSG_MEM_GET :
            case BMSG_MEM_PUT_ACK :
            case BMSG_MEM_GET_ACK :
	        bmsg_sys_receive(buffer);
		break;
	    default :
		/* unknown message type */
                bmsg_assert_always();
		break;
	}
    }

    LMQ_Unlock(&lrcv_queue);

    return(BMSG_OK);
}


bmsg_ret_t 
bmsg_info(bmsg_info)
    bmsg_info_t * bmsg_info;
{
    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!bmsg_info) return(BMSG_INVAL);
#endif

    MsgInfo_Lock();

    bmsg_info->sent_short = msg_info.sent_short;
    bmsg_info->sent_medium = msg_info.sent_medium;
    bmsg_info->sent_long = msg_info.sent_long;
    bmsg_info->rcvd_short = msg_info.rcvd_short;
    bmsg_info->rcvd_medium = msg_info.rcvd_medium;
    bmsg_info->rcvd_long = msg_info.rcvd_long;

    MsgInfo_Unlock();

    return(BMSG_OK);
}


bmsg_ret_t 
bmem_info(bmem_info)
    bmem_info_t * bmem_info;
{
    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!bmem_info) return(BMSG_INVAL);
#endif

    MemInfo_Lock();

    bmem_info->lputs = mem_info.lputs;
    bmem_info->lgets = mem_info.lgets;
    bmem_info->rputs = mem_info.rputs;
    bmem_info->rgets = mem_info.rgets;

    MemInfo_Unlock();

    return(BMSG_OK);
}


bmsg_ret_t 
bport_info(port_id,port_info)
    bport_id_t port_id;
    bport_info_t * port_info;
{
    port_t * port;

    bmsg_assert(bmsg_ready());

#if !defined(TRUSTED)
    if (!port_id || !port_info) return(BMSG_INVAL);
#endif

    if (acquire_port(port_id,&port) != BMSG_OK)
	return(BMSG_NOPORT);

    port_info->lputs = port->info.lputs;
    port_info->lgets = port->info.lgets;
    port_info->rputs = port->info.rputs;
    port_info->rgets = port->info.rgets;
    port_info->sends = port->info.sends;
    port_info->receives = port->info.receives;

    release_port(port);

    return(BMSG_OK);
}


bmsg_ret_t
bmem_address(msg_address,mem_address)
    bmsg_address_t msg_address;
    bmem_address_t * mem_address;
{
#if defined(PTR_64_BITS)
    bmsg_assert(sizeof(bmem_address_t) == sizeof(bmsg_address_t));
    *mem_address = * (bmem_address_t *) &msg_address;
    return(BMSG_OK);
#else /* PTR_64_BITS */
    bmsg_assert(sizeof(bmem_address_t) == sizeof(pds_word_t));
    *mem_address = (bmem_address_t) msg_address.low;
    if (msg_address.high == 0)
	return(BMSG_OK);
    else
	return(BMSG_INVAL);
#endif /* PTR_64_BITS */
}


void
bmsg_address(mem_address,msg_address)
    bmem_address_t mem_address;
    bmsg_address_t * msg_address;
{
#if defined(PTR_64_BITS)
    bmsg_assert(sizeof(bmem_address_t) == sizeof(bmsg_address_t));
    *msg_address = * (bmsg_address_t *) &mem_address;
#else /* PTR_64_BITS */
    bmsg_assert(sizeof(bmem_address_t) == sizeof(pds_word_t));
    msg_address->low = (pds_word_t) mem_address; 
    msg_address->high = (pds_word_t) 0; 
#endif /* PTR_64_BITS */
}


static char * bmsg_errlist[BMSG_PUNREACH-PDS_RET_MAX] = {
	"No such port !",
	"No message !",
	"No identifiers !",
	"Port opened !",
	"Port blocked !",
	"Port unblocked !",
	"Port opening !",
	"Port closing !",
	"Port blocking !",
	"Port unblocking !",
	"Port dying !",
	"Port not available !",
	"Port unreachable !"};


char *
bmsg_error_string(bret)
    bmsg_ret_t bret;
{
    return bmsg_errlist[bret-PDS_RET_MAX];
}


void
bmsg_perror(bret, s)
    bmsg_ret_t bret;
    char * s;
{
    if (bret > PDS_RET_MAX) {
        if (s && s[0] != '\0')
            fprintf(stderr,"%s: ",s);
	fprintf(stderr, "%s\n", bmsg_error_string(bret));
    }
    else
	pds_perror((pds_ret_t) bret, s);
}
