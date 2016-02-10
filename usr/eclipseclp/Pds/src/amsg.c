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
**        File: amsg.c
**      Author: Kees Schuerman
**      SccsId: "@(#)amsg.c	1.30 8/10/95"
** Description: Message Passing System: Application Layer
***********************************************************************/

#include "machine.h"	/* architecture specific constant definitions */

#include <stdio.h>
#include <malloc.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.error.h"
#include "pds.mem.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"
#include "amsg.h"
#include "amsg.xdr.h"


/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(NDEBUG)
#define amsg_assert(ex)
#else
#define amsg_assert(ex) {                                       \
    if (!(ex)) {                                                \
        (void) fprintf(stderr,                                  \
               "PDS MPS-A Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
    }                                                           \
}
#endif /* NDEBUG */
#define amsg_assert_always() {                                  \
        (void) fprintf(stderr,                                  \
               "PDS MPS-A Assertion Failed:");                  \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(1);                                                \
}



/**********************************************************************
** Runtime Logging
***********************************************************************/

static int amsg_alog_on = 0;
static int amsg_alog_open = 0;
static int amsg_alog_close = 0;
static int amsg_alog_master = 0;

#if defined(AMSGLOG)

#if defined(ALOG_TRACE)

#include "alog.h"

#define AMSG_ALOG_EVENT_BASE	32
#define ALOG_EVENT_SEND_AMSG	(AMSG_ALOG_EVENT_BASE+0)
#define ALOG_EVENT_RCV_AMSG	(AMSG_ALOG_EVENT_BASE+1)

static void
amsg_openlog()
{
    if (amsg_alog_on) {
	if (amsg_alog_open) {
	    if (amsg_alog_master) {
                ALOG_MASTER(bport_self(),ALOG_TRUNCATE);
	    }
            else {
	        ALOG_SETUP(bport_self(),ALOG_TRUNCATE);
	    }
	}
        if (amsg_alog_master) {
	    ALOG_DEFINE(ALOG_EVENT_SEND_AMSG,
			"SEND AMSG","PORT %d");
	    ALOG_DEFINE(ALOG_EVENT_RCV_AMSG,
			" RCV AMSG","PORT %d");
	}
    }
}

static void
amsg_closelog() 
{
    if (amsg_alog_on && amsg_alog_close) {
	ALOG_OUTPUT;
    }
}

#define AMSGLOG_SEND_MESSAGE(receiver) {                                \
        if ((amsg_alog_on) && (UserPort(receiver))) {			\
            ALOG_LOG(bport_self(),ALOG_EVENT_SEND_AMSG,                 \
                     receiver,"SEND AMSG");                             \
        }                                                               \
}

#define AMSGLOG_RCV_MESSAGE(sender,receiver) {                          \
        if ((amsg_alog_on) && (UserPort(receiver))) {			\
            ALOG_LOG(bport_self(),ALOG_EVENT_RCV_AMSG,                  \
                     receiver,"RCV AMSG");                              \
        }                                                               \
}


#else /* ALOG_TRACE */

#undef  LOG_INFO
#define LOG_INFO        stdout

static void
amsg_openlog()
{
    setbuf(LOG_INFO, (char *) 0);
}
   
static void
amsg_closelog()
{
    return;
}

#define AMSGLOG_SEND_MESSAGE(receiver) {                                \
        fprintf(LOG_INFO,						\
		"%d: AMSG SEND MESSAGE --> %d, %d \n",			\
		bport_self(),Bport_Id(receiver),receiver);		\
}
#define AMSGLOG_RCV_MESSAGE(sender,receiver) {                          \
        fprintf(LOG_INFO,						\
		"%d: AMSG  RCV MESSAGE %d <-- %d \n",			\
		bport_self(),receiver,sender);  			\
}

#endif /* ALOG_TRACE */

#else /* AMSGLOG */

#define amsg_openlog()
#define amsg_closelog()

#define AMSGLOG_SEND_MESSAGE(receiver)
#define AMSGLOG_RCV_MESSAGE(sender,receiver)

#endif /* AMSGLOG */



/**********************************************************************
** Initialising, Exiting
***********************************************************************/

int amsg_initialised = 0;
static int amsg_initialising = 0;
int amsg_exited = 0;
int amsg_exiting = 0;



/**********************************************************************
** Statistics
***********************************************************************/

static l_mutex_t msg_info_mutex;
static amsg_info_t msg_info;

static void
amsg_info_init()
{
    l_mutex_init(&msg_info_mutex);
    msg_info.sent_short = 0;
    msg_info.sent_medium = 0;
    msg_info.sent_long = 0;
    msg_info.rcvd_short = 0;
    msg_info.rcvd_medium = 0;
    msg_info.rcvd_long = 0;
}


#define MsgInfo_Lock()          l_mutex_lock(&msg_info_mutex)
#define MsgInfo_Unlock()        l_mutex_unlock(&msg_info_mutex)

#if !defined(AMSG_STAT_OFF)
#define MsgInfo_Sent_Increment(s) {             \
        MsgInfo_Lock();                         \
	if ((s) <= AMSG_SHORT)			\
	    msg_info.sent_short++;		\
	else if ((s) <= AMSG_MEDIUM)		\
	    msg_info.sent_medium++;		\
	else					\
	    msg_info.sent_long++;		\
        MsgInfo_Unlock();                       \
}
#define MsgInfo_Rcvd_Increment(s) {             \
        MsgInfo_Lock();                         \
        if ((s) <= AMSG_SHORT)             	\
            msg_info.rcvd_short++;              \
        else if ((s) <= AMSG_MEDIUM)            \
            msg_info.rcvd_medium++;             \
        else                                    \
            msg_info.rcvd_long++;               \
        MsgInfo_Unlock();                       \
}
#else
#define MsgInfo_Sent_Increment(s)
#define MsgInfo_Rcvd_Increment(s)
#endif /* AMSG_STAT_OFF */



/**********************************************************************
** Messages: <amsg_header><amsg_data>
***********************************************************************
** Messages consist of a fixed sized header and the message data and 
** form together the <bmsg_data> of a bmsg. The size of the header is 
** a multiple of 8 bytes. This ensures that <amsg_data> is aligned on 
** 8 bytes, because of the fact that <bmsg_data> is ensured to be 
** aligned on 8 bytes.
***********************************************************************/

/*
** Message Type Masks
**
** XDR: eXternal Data Representation
** IDR: Internal Data Representation
*/

#define AMSG_XDR                        0x1000
#define AMSG_IDR                        0x2000

#define AMSG_MSG			0x0100
#define AMSG_RPC_REQ			0x0200
#define AMSG_RPC_REP			0x0400


/*
** Message Types
*/

#define AMSG_MSG_XDR                    (AMSG_MSG     | AMSG_XDR)
#define AMSG_MSG_IDR                    (AMSG_MSG     | AMSG_IDR)
#define AMSG_RPC_REQ_XDR		(AMSG_RPC_REQ | AMSG_XDR)
#define AMSG_RPC_REQ_IDR		(AMSG_RPC_REQ | AMSG_IDR)
#define AMSG_RPC_REP_XDR		(AMSG_RPC_REP | AMSG_XDR)
#define AMSG_RPC_REQ_IDR		(AMSG_RPC_REQ | AMSG_IDR)


/*
** Message Header
*/

typedef struct {
    amsg_type_t amsg_type;	/* message type			 */
    amsg_type_t amsg_data_type;	/* message data element type	 */
    amsg_count_t amsg_count;    /* # message data elements	 */
    aport_id_t aport_id;	/* destination port 		 */
} amsg_header_t;



/**********************************************************************
** Message Buffers
***********************************************************************
** Messages are stored in message buffers managed by the basic message
** passing layer (i.e. the b-layer). In addition to space for the actual 
** message, the buffers contain also a so called scratch area. The 
** scratch area can be used by the application for storing some message 
** associated information. The application message passing layer (i.e.
** the a-layer) uses the scratch area for storing a message buffer pointer 
** (used for linking the message buffers in a list), the bport_id of the 
** sender (for logging purposes), and a message pointer pointing to the
** start of the message (i.e. the message header) that is stored somewhere
** in the message buffer.
** Note that the contents of the scratch area is not part of the actual
** message and is not guaranteed to be transferred on an amsg_send() or
** amsg_receive().
***********************************************************************/

typedef struct amsg_buffer_scratch {
    struct amsg_buffer_scratch * next;
    bport_id_t sender;
    amsg_t * amsg;
} amsg_buffer_scratch_t;


#define AmsgBuffer(msg) ((amsg_buffer_scratch_t *) msg)

#define AmsgHeader(msg) ((amsg_header_t *) AmsgBuffer(msg)->amsg)

#define AmsgData(msg) ((amsg_data_t *)				    \
		       AmsgHeader(msg) + sizeof(amsg_header_t))



/**********************************************************************
** Ports
***********************************************************************
** In general, when a message arrives on an empty port its associated 
** notification procedure is called. By default there will be at most 
** one such notification in progress per port. This limits the number 
** of required notifications and eases the management of in order 
** message handling, i.e. when the messages are received and handled 
** in the notification procedure itself.
** With the aport_set_option() primitive the application can disable
** notifications or disable notification deference.
***********************************************************************/

#if defined(__STDC__)
typedef struct aport {
    struct aport * prev;		/* previous port	       */
    struct aport * next;		/* next port 		       */
    aport_id_t aport_id;                /* port identifier             */
    void (* notify_procedure) 		/* port notification procedure */
	 (aport_id_t port_id);		/*                             */
    l_mutex_t mutex;	                /* protects following fields   */
    int notify_count;			/* # active notifications      */
    int notify_level;			/* notify level	               */
    int notify_defer;			/* notify deference            */
    int notify_pending;			/* notifications pending (0/1) */
    void_ptr data;			/* data pointer		       */
    aport_info_t info;			/* port statistics	       */
    amsg_buffer_scratch_t * first;	/* first message buffer        */
    amsg_buffer_scratch_t * last;       /* last message buffer         */
} port_t;
#else /* __STDC__ */
typedef struct aport {
    struct aport * prev;		/* previous port	       */
    struct aport * next;		/* next port 		       */
    aport_id_t aport_id;            	/* port identifier             */
    void (* notify_procedure) ();	/* port notification procedure */
    l_mutex_t mutex;	            	/* protects following fields   */
    int notify_count;			/* # active notifications      */
    int notify_level;			/* notify level	               */
    int notify_defer;			/* notify deference            */
    int notify_pending;			/* notifications pending (0/1) */
    void_ptr data;			/* data pointer		       */
    aport_info_t info;			/* port statistics	       */
    amsg_buffer_scratch_t * first;  	/* first message buffer        */
    amsg_buffer_scratch_t * last;   	/* last message buffer         */
} port_t;
#endif /* __STDC__ */

#define P_Lock(p)      l_mutex_lock(&(p)->mutex)
#define P_Unlock(p)    l_mutex_unlock(&(p)->mutex)
#define P_Empty(p)     (!(p)->last)


static void
port_init(port)
    port_t * port;
{
    port->prev = (port_t *) 0;
    port->next = (port_t *) 0;
    port->aport_id = 0;
    l_mutex_init(&port->mutex);
    port->notify_procedure = (void (*) ()) 0;
    port->notify_count = 0;
    port->notify_level = 0;
    port->notify_defer = (int) APORT_NOTIFY_DEFER_ALL;
    port->notify_pending = 0;
    port->data = (void_ptr) 0;
    port->info.sends = 0;
    port->info.receives = 0;
    port->first = (amsg_buffer_scratch_t *) 0;
    port->last = (amsg_buffer_scratch_t *) 0;
}



/**********************************************************************
** Port Identifiers
***********************************************************************
** <aport_id> :  <bport_number><aport_number>
***********************************************************************/

typedef pds_half_word_t aport_no_t;
typedef pds_half_word_t bport_no_t;

#if WORDS_BIGENDIAN
typedef struct {
    bport_no_t Bp_number;
    aport_no_t Ap_number;
} aport_id_struct_t;
#else /* LITTLE ENDIAN */
typedef struct {
    aport_no_t Ap_number;
    bport_no_t Bp_number;
} aport_id_struct_t;
#endif /* WORDS_BIGENDIAN */


/* 
** aport_no_t Aport_Number(aport_id_t Ap_id) 
*/

#define Aport_Number(Ap_id) 					\
	(((aport_id_struct_t *) &(Ap_id))->Ap_number)


/* 
** bport_no_t Bport_Number(aport_id_t Ap_id) 
*/

#define Bport_Number(Ap_id) 					\
	(((aport_id_struct_t *) &(Ap_id))->Bp_number)


/* 
** bport_id_t Bport_Id(aport_id_t Ap_id) 
*/

#define Bport_Id(Ap_id) 					\
	((bport_id_t) Bport_Number(Ap_id))


/*
** void Aport_Id(bport_no_t bp_no,
**		 aport_no_t ap_no,
**		 aport_id_t * ap_id)
*/

#define Aport_Id(bp_no,ap_no,ap_id) {				\
	((aport_id_struct_t *) (ap_id))->Bp_number = (bp_no);	\
	((aport_id_struct_t *) (ap_id))->Ap_number = (ap_no);   \
}
	
	

/**********************************************************************
** Port Set
***********************************************************************
** The ports are stored in an array ports[]. This array is divided into
** three subarrays, i.e. 
**
**			   system_ports[NOF_SYSTEM_PORTS] 
**			   dynamic_ports[MAX_NOF_DYNAMIC_PORTS]
**			   static_ports[]
**
** There are three system ports, i.e. the warn_port, the error_port, and
** the panic_port. These ports are basically used for notifying remote 
** processes about warnings, errors, and panics, respectively. 
** The application ports are either static or dynamic. The static ports 
** are allocated by amsg_init(). Additionally, up to MAX_NOF_DYNAMIC_PORTS 
** ports can be allocated (and deallocated) dynamically.
***********************************************************************/

#if defined(__STDC__)
static void amsg_wep_notifier(aport_id_t port_id);
#else /* __STDC__ */
static void amsg_wep_notifier();
#endif /* __STDC__ */

#define NOF_SYSTEM_PORTS	3
#define MAX_NOF_DYNAMIC_PORTS	64

#define USER_NOTIFY_LEVEL_MAX		APORT_NOTIFY_LEVEL_MAX
#define SYSTEM_NOTIFY_LEVEL_MAX		APORT_NOTIFY_LEVEL_MAX+4
#define NOTIFY_LEVEL_MAX		SYSTEM_NOTIFY_LEVEL_MAX

static port_t * ports;
static aport_no_t port_number_first;
static aport_no_t port_number_last;

static port_t * system_ports;
static port_t * warn_port;
static port_t * error_port;
static port_t * panic_port;
static aport_no_t system_port_number_first;
static aport_no_t system_port_number_last;

static port_t * static_ports;
static aport_no_t static_port_number_first;
static aport_no_t static_port_number_last;

static port_t * dynamic_ports;
static aport_no_t dynamic_port_number_first;
static aport_no_t dynamic_port_number_last;

static unsigned port_set_hint;
static l_mutex_t port_sel_mutex;

#define PS_Lock()      l_mutex_lock(&port_sel_mutex)
#define PS_Unlock()    l_mutex_unlock(&port_sel_mutex)


/* unsigned int AportIndex(aport_id_t id)				*/

#define AportIndex(id) (Aport_Number(id)-port_number_first)


/* void GetPort(aport_id_t id, port_t * * p)                            */

#define GetPort(id,p) {							\
	aport_no_t no;							\
	no = Aport_Number(id);						\
	if (!(id) ||							\
	    (no < port_number_first) ||					\
	    (no > port_number_last))					\
	    *(p) = (port_t *) 0;					\
	else {								\
	    *(p) = &ports[AportIndex(id)];				\
	    if ((id) != (*(p))->aport_id)				\
		*(p) = (port_t *) 0;					\
	}								\
}


static amsg_ret_t 
aport_set_init(size,notify_procedure,port_id)
    unsigned size;                   /*  in: # ports in default port set */
    void (* notify_procedure []) (); /*  in: port notifier procedures    */
    aport_id_t port_id [];           /* out: port identifiers            */
{
    unsigned nof_static_ports;
    unsigned max_nof_ports;
    int i;

    /* allocate space for ports */
    nof_static_ports = size;
    max_nof_ports = nof_static_ports + MAX_NOF_DYNAMIC_PORTS + NOF_SYSTEM_PORTS;
    ports = (port_t *) malloc(max_nof_ports * sizeof(port_t));
    if (!ports) 
	return(AMSG_NOMEMORY);

    system_ports = ports;
    static_ports = system_ports + NOF_SYSTEM_PORTS;
    dynamic_ports = static_ports + nof_static_ports;

    /* associate a number with each port */
    port_number_first = 1;
    port_number_last = port_number_first + max_nof_ports - 1;
    system_port_number_first = port_number_first;
    system_port_number_last = system_port_number_first + NOF_SYSTEM_PORTS - 1;
    static_port_number_first = system_port_number_last + 1;
    static_port_number_last = static_port_number_first + nof_static_ports - 1;
    dynamic_port_number_first = static_port_number_last + 1;
    dynamic_port_number_last = dynamic_port_number_first + MAX_NOF_DYNAMIC_PORTS - 1;

    /* initialise system ports */
    for (i=0; i<NOF_SYSTEM_PORTS; i++) {
	port_init(&system_ports[i]);
        Aport_Id((bport_no_t) bport_self(),
                 i+system_port_number_first,
                 &system_ports[i].aport_id);
    }
    warn_port = &system_ports[0];
    error_port = &system_ports[1];
    panic_port = &system_ports[2];
    warn_port->notify_level = 0;
    error_port->notify_level = SYSTEM_NOTIFY_LEVEL_MAX-1;
    amsg_assert(error_port->notify_level > USER_NOTIFY_LEVEL_MAX);
    panic_port->notify_level = SYSTEM_NOTIFY_LEVEL_MAX;
    amsg_assert(panic_port->notify_level > USER_NOTIFY_LEVEL_MAX);
    warn_port->notify_procedure = amsg_wep_notifier;
    error_port->notify_procedure = amsg_wep_notifier;
    panic_port->notify_procedure = amsg_wep_notifier;

    /* initialise dynamic port array */
    for (i=0; i<MAX_NOF_DYNAMIC_PORTS; i++)
	port_init(&dynamic_ports[i]);

    /* initialise static port array */
    for (i=0; i<nof_static_ports; i++) {
        port_init(&static_ports[i]);
        static_ports[i].notify_procedure = notify_procedure[i];
        Aport_Id((bport_no_t) bport_self(),
                 i+static_port_number_first,
                 &static_ports[i].aport_id);
        port_id[i] = static_ports[i].aport_id;
    }

    l_mutex_init(&port_sel_mutex);
    port_set_hint = 0; 

    return(AMSG_OK);
}



/**********************************************************************
** Port State
***********************************************************************/

/* int StaticPort(aport_id_t port_id)	*/

#define StaticPort(port_id)			\
	((Aport_Number(port_id) >= 		\
	  static_port_number_first) &&		\
	 (Aport_Number(port_id) <= 		\
	  static_port_number_last))


/* int DynamicPort(aport_id_t port_id)	*/

#define DynamicPort(port_id)			\
	((Aport_Number(port_id) >= 		\
	  dynamic_port_number_first) &&		\
	 (Aport_Number(port_id) <= 		\
	  dynamic_port_number_last))


/* int SystemPort(aport_id_t port_id)	*/

#define SystemPort(port_id)			\
	((Aport_Number(port_id) >= 		\
	  system_port_number_first) &&		\
	 (Aport_Number(port_id) <= 		\
	  system_port_number_last))


/* int UserPort(aport_id_t port_id)	*/

#define UserPort(port_id)			\
	(StaticPort(port_id) || 		\
	 DynamicPort(port_id))


/* int PortOwner(aport_id_t port_id)		*/

#define PortOwner(port_id)			\
	(Bport_Id(port_id) == bport_self())


/* int SendRights(aport_id_t port_id) 		*/

#define SendRights(port_id) (1)


/* int ReceiveRights(aport_id_t port_id) 	*/

#define ReceiveRights(port_id)	PortOwner(port_id)



/**********************************************************************
** Message Buffers
***********************************************************************/

/* 
** void P_Put(port_t * p, 
**	      amsg_buffer_scratch_t * m)	
*/

#if defined(NDEBUG)
#define P_Put(p,m) {					\
        if (P_Empty(p))                         	\
            (p)->first = (m);                   	\
        else                                    	\
            (p)->last->next = (m);              	\
        (p)->last = (m);                        	\
}
#else
#define P_Put(p,m) {					\
	(m)->next = (amsg_buffer_scratch_t *) 0;	\
        if (P_Empty(p))                         	\
            (p)->first = (m);                   	\
        else                                    	\
            (p)->last->next = (m);              	\
        (p)->last = (m);                        	\
}
#endif


/* 
** void P_Get(port_t * p, 
**            amsg_buffer_scratch_t * * m)   	
*/

#if defined(NDEBUG)
#define P_Get(p,m) {                                    \
	amsg_assert(!P_Empty(p));			\
        *(m) = (p)->first;                              \
	if ((p)->first == (p)->last)			\
            (p)->last = (amsg_buffer_scratch_t *) 0;	\
	else						\
            (p)->first = (p)->first->next;             	\
}
#else
#define P_Get(p,m) {                                    \
	amsg_assert(!P_Empty(p));			\
        *(m) = (p)->first;                              \
	if ((p)->first == (p)->last)			\
            (p)->last = (amsg_buffer_scratch_t *) 0;	\
	else						\
            (p)->first = (p)->first->next;             	\
        (*(m))->next = (amsg_buffer_scratch_t *) 0;	\
}
#endif


/* 
** void P_Peek(port_t * p, 
**             amsg_buffer_scratch_t * * m)   	
*/

#define P_Peek(p,m) {                                   \
        *(m) = (p)->first;                              \
}



/**********************************************************************
** Notifications
***********************************************************************/

typedef struct {
    int count;		/* # active notifications	*/
    port_t * first;
    port_t * last;
} a_notification_t;

static a_notification_t notifications[NOTIFY_LEVEL_MAX+1]; 
static l_mutex_t notifications_mutex;
static int notifications_level;

#define NFS_Lock()		l_mutex_lock(&notifications_mutex);
#define NFS_Unlock()		l_mutex_unlock(&notifications_mutex);

#define NF_Empty(lvl)		(notifications[lvl].last == (port_t *) 0)


/* 
** void NF_First(int lvl, 
**               port_t * * p)   	
*/

#define NF_First(lvl,p) {				\
    *(p) = notifications[lvl].first;			\
}


/* 
** void NF_Put(port_t * p)   	
*/

#define NF_Put(p) {					\
	a_notification_t * nf;				\
	{						\
	    nf = &notifications[p->notify_level];	\
	    if ((nf)->last) {				\
	        (p)->prev = (nf)->last;			\
	        (nf)->last->next = (p);			\
	        (nf)->last = (p);			\
	    }						\
	    else {					\
	        (nf)->first = (p);			\
	        (nf)->last = (p);			\
	        (p)->prev = (port_t *) 0;		\
	    }						\
	    (p)->next = (port_t *) 0;			\
	}						\
}


/* 
** void NF_Isolate(port_t * p)   	
*/

#define NF_Isolate(p) {					\
	a_notification_t * nf;				\
	{						\
	    nf = &notifications[p->notify_level];	\
	    if ((nf)->last == (p))			\
	        (nf)->last = (p)->prev;			\
	    if ((nf)->first == (p))			\
	        (nf)->first = (p)->next;		\
            if ((p)->prev)                              \
                (p)->prev->next = (p)->next;            \
            if ((p)->next)                              \
                (p)->next->prev = (p)->prev;            \
	}						\
}


static void
notifications_init()
{
    int i;

    for (i=0;i<=NOTIFY_LEVEL_MAX;i++) {
        notifications[i].first = (port_t *) 0;
        notifications[i].last = (port_t *) 0;
        notifications[i].count = 0;
    }
    l_mutex_init(&notifications_mutex);
    notifications_level = -1;
}


static void
amsg_notify(level)
    int level;
{
    port_t * aport;
    port_t * next;

    amsg_assert(level <= NOTIFY_LEVEL_MAX);
    amsg_assert(level >= 0);

    NFS_Lock();

    if (level < notifications_level) {
	NFS_Unlock();
	return;
    }
    notifications_level = level;

    while (level >= 0) {
        NF_First(level,&aport);
        while (aport) {
	    next = aport->next;
	    P_Lock(aport);
 	    amsg_assert(aport->notify_pending);
	    if ((notifications[level].count) /* notifications in progress */
	        && ((aport->notify_defer == (int) APORT_NOTIFY_DEFER_ALL) ||
		    ((aport->notify_count > 0) &&
		     (aport->notify_defer == (int) APORT_NOTIFY_DEFER_ONE)))) {
	        P_Unlock(aport);
	        aport = next;
	    }
	    else {
                NF_Isolate(aport);
		notifications[level].count++;
	        aport->notify_pending = 0;
	        aport->notify_count++;
	        P_Unlock(aport);
		NFS_Unlock();
	        /* notify message arrival */
                aport->notify_procedure(aport->aport_id);  
	        P_Lock(aport);
	        aport->notify_count--;
	        P_Unlock(aport);
		NFS_Lock();
		notifications[level].count--;
		level = notifications_level;
        	NF_First(level,&aport);
            }
        }

	amsg_assert(notifications_level == level);
	if ((NF_Empty(level)) && 
	    (notifications[level].count == 0)) { /* go to next level */
	    notifications_level = --level;
	}
	else
	    break;
    }

    NFS_Unlock();
}



/**********************************************************************
** WEP (Warning,Error,Panic) System
***********************************************************************/

typedef struct {
    aport_id_t culprit;
    amsg_wep_t wep;
} amsg_wep_msg_t;

static msg_type_t MDT_AMSGWEPMSG;


static void
amsg_wep_notifier(port_id)
    aport_id_t port_id;
{
    amsg_wep_msg_t * wep_msg;
    amsg_count_t msg_count;
    amsg_type_t msg_type;
    amsg_ret_t aret;
    amsg_t msg;

    do {
	aret = amsg_receive(port_id,
			    &msg,
			    (amsg_data_t * *) &wep_msg,
			    &msg_type,
			    &msg_count,
			    (amsg_option_t) 0);
	switch (aret) {
	    case AMSG_OK :
		amsg_assert(msg_type == MDT_AMSGWEPMSG);
		amsg_assert(msg_count == 1);
		if (port_id == warn_port->aport_id)
        	    amsg_warn((amsg_warn_t) wep_msg->wep,wep_msg->culprit);
		else if (port_id == error_port->aport_id)
        	    amsg_error((amsg_error_t) wep_msg->wep,wep_msg->culprit);
		else if (port_id == panic_port->aport_id)
        	    amsg_panic((amsg_panic_t) wep_msg->wep,wep_msg->culprit);
		else
		    amsg_assert_always();
		(void) amsg_free(msg);
		break;
	    case AMSG_NOMESSAGE :
		break;
	    default :
		amsg_assert_always();
	}
    } while (aret == AMSG_OK);
}



/**********************************************************************
** Message Ordering Mutex
***********************************************************************/

static l_mutex_t mo_mutex;

#define MO_Lock()	l_mutex_lock(&mo_mutex);
#define MO_Unlock()	l_mutex_unlock(&mo_mutex);

static void 
mo_mutex_init()
{
    l_mutex_init(&mo_mutex);
}



/**********************************************************************
** XDR
***********************************************************************/

#define AMSG_XDRBUF_SIZE	64

typedef struct {
    unsigned amsg_header;       /* size of header in XDR format        */
} amsg_xdr_size_t;

static amsg_xdr_size_t amsg_xdr_size;


static bool_t
xdr_amsg_header(xdrs,amsg_header)
    XDR * xdrs;
    amsg_header_t * amsg_header;
{
    return(xdr_amsg_type(xdrs,&amsg_header->amsg_type) &&
           xdr_amsg_type(xdrs,&amsg_header->amsg_data_type) &&
           xdr_amsg_count(xdrs,&amsg_header->amsg_count) &&
           xdr_aport_id(xdrs,&amsg_header->aport_id));
}


static void
amsg_xdr_init()
{
    amsg_header_t amsg_header;
    pds_int32 xdrbuf[AMSG_XDRBUF_SIZE];
    XDR xdrs;

    amsg_header.amsg_type = 0;
    amsg_header.amsg_data_type = 0;
    amsg_header.amsg_count = 0;
    amsg_header.aport_id = 0;

    xdrmem_create(&xdrs,(const caddr_t) xdrbuf,
                  (const u_int) AMSG_XDRBUF_SIZE * sizeof(pds_int32),
                  XDR_ENCODE);
    if (!xdr_amsg_header(&xdrs,&amsg_header))
        amsg_assert_always();
    amsg_xdr_size.amsg_header = xdr_getpos(&xdrs);
    xdr_destroy(&xdrs);
}



/**********************************************************************
** Type System
***********************************************************************/

msg_type_t MDT_APORT;
msg_type_t MDT_AMSGINFO;
msg_type_t MDT_APORTINFO;


static amsg_ret_t
amsg_types_init()
{
    pds_ret_t pret;

    msg_typedef_t mdt_amsginfo[10];
    msg_typedef_t mdt_aportinfo[6];
    msg_typedef_t mdt_aport[7];
    msg_typedef_t mdt_wepmsg[6];

    mdt_amsginfo[0] = MDT_BEGIN;
    mdt_amsginfo[1] = MDT_STRUCT_OPEN;
    mdt_amsginfo[2] = MDT_AMSGCOUNTER;
    mdt_amsginfo[3] = MDT_AMSGCOUNTER;
    mdt_amsginfo[4] = MDT_AMSGCOUNTER;
    mdt_amsginfo[5] = MDT_AMSGCOUNTER;
    mdt_amsginfo[6] = MDT_AMSGCOUNTER;
    mdt_amsginfo[7] = MDT_AMSGCOUNTER;
    mdt_amsginfo[8] = MDT_STRUCT_CLOSE;
    mdt_amsginfo[9] = MDT_END;

    if ((pret = pds_type_define(AMSG_INTFC,1,
				mdt_amsginfo,&MDT_AMSGINFO)) != PDS_OK)
	return((amsg_ret_t) pret);

    mdt_aportinfo[0] = MDT_BEGIN;
    mdt_aportinfo[1] = MDT_STRUCT_OPEN;
    mdt_aportinfo[2] = MDT_AMSGCOUNTER;
    mdt_aportinfo[3] = MDT_AMSGCOUNTER;
    mdt_aportinfo[4] = MDT_STRUCT_CLOSE;
    mdt_aportinfo[5] = MDT_END;

    if ((pret = pds_type_define(AMSG_INTFC,2,
				mdt_aportinfo,&MDT_APORTINFO)) != PDS_OK)
	return((amsg_ret_t) pret);

    mdt_aport[0] = MDT_BEGIN;
    mdt_aport[1] = MDT_STRUCT_OPEN;
    mdt_aport[2] = MDT_APORTID;
    mdt_aport[3] = MDT_BPORTID;
    mdt_aport[4] = MDT_BDOMAINID;
    mdt_aport[5] = MDT_STRUCT_CLOSE;
    mdt_aport[6] = MDT_END;

    if ((pret = pds_type_define(AMSG_INTFC,3,
				mdt_aport,&MDT_APORT)) != PDS_OK)
	return((amsg_ret_t) pret);

    mdt_wepmsg[0] = MDT_BEGIN;
    mdt_wepmsg[1] = MDT_STRUCT_OPEN;
    mdt_wepmsg[2] = MDT_APORTID;
    mdt_wepmsg[3] = MDT_AMSGWEP;
    mdt_wepmsg[4] = MDT_STRUCT_CLOSE;
    mdt_wepmsg[5] = MDT_END;

    if ((pret = pds_type_define(AMSG_INTFC,4,
				mdt_wepmsg,&MDT_AMSGWEPMSG)) != PDS_OK)
	return((amsg_ret_t) pret);

    return(AMSG_OK);
}



/**********************************************************************
** Miscellaneous Primitives
***********************************************************************/

static amsg_ret_t
amsg_abret(bret)
    bmsg_ret_t bret;
{
    if (bret <= PDS_RET_MAX)
	return((amsg_ret_t) bret);

    switch (bret) {
	case BMSG_NOPORT :
	    return(AMSG_NOPORT);
	case BMSG_NOMESSAGE :
	    return(AMSG_NOMESSAGE);
	case BMSG_NOID :
	case BMSG_POPENED :
	case BMSG_PBLOCKED :
	    return(AMSG_PBLOCKED);
	case BMSG_PUNBLOCKED :
	case BMSG_POPENING :
	case BMSG_PCLOSING :
	case BMSG_PBLOCKING :
	case BMSG_PUNBLOCKING :
	    return(AMSG_ERROR);
	case BMSG_PDYING :  
	    return(AMSG_PDYING);
	case BMSG_PNOTAVAILABLE :  
	    return(AMSG_PNOTAVAILABLE);
	case BMSG_PUNREACH :  
	    return(AMSG_PUNREACH);
	default :
	    /* unknown return value */
	    amsg_assert_always();
    }
}



/**********************************************************************
** Debugging Support
***********************************************************************/

#if !defined(NDEBUG)

void
amsg_print_port_queue(port_id)
aport_id_t port_id;
{
    amsg_buffer_scratch_t * buf;
    port_t * port;
    unsigned no;

    if (!amsg_ready())
	return;

    GetPort(port_id,&port);

    if (!port) {
	printf("\n");
	printf("No such port !\n");
	printf("\n");
        fflush(stdout);
	return;
    }

    if (P_Empty(port)) {
	printf("\n");
        printf("Port 0x%x Empty\n",(char *) port);
	printf("\n");
        fflush(stdout);
	return;
    }

    printf("\n");
    printf("-----------------------\n");
    printf("    Port 0x%x\n", (char *) port);
    printf("-----------------------\n");
    printf(" Sender | Message      \n");
    printf("--------+--------------\n");

    buf = port->first;
    do {
	printf(" %6d | ",buf->sender);
	printf("0x%x",(char *) AmsgData(buf));
	printf("\n");
        buf = buf->next;
    } while (buf);

    printf("-----------------------\n");
    printf("\n");
    fflush(stdout);
}


void
amsg_print_port_queues()
{
    aport_id_t port_id;
    aport_no_t port_no;

    if (!amsg_ready())
	return;

    if (system_port_number_last - system_port_number_first + 1) {
	printf("\n");
        printf("Amsg System Ports\n");
        printf("=================\n");
    }
    for (port_no = system_port_number_first;
	 port_no <= system_port_number_last;
	 port_no++) {
        port_id = ports[port_no-port_number_first].aport_id;
	if (port_id)
	    amsg_print_port_queue(port_id);
    }

    if (static_port_number_last - static_port_number_first + 1) {
	printf("\n");
        printf("Static Ports\n");
        printf("============\n");
    }
    for (port_no = static_port_number_first;
	 port_no <= static_port_number_last;
	 port_no++) {
        port_id = ports[port_no-port_number_first].aport_id;
	if (port_id)
	    amsg_print_port_queue(port_id);
    }

    if (dynamic_port_number_last - dynamic_port_number_first + 1) {
	int dp_header = 0;
        for (port_no = dynamic_port_number_first;
	     port_no <= dynamic_port_number_last;
	     port_no++) {
            port_id = ports[port_no-port_number_first].aport_id;
	    if (port_id) {
		if (!dp_header) {
		    printf("\n");
        	    printf("Dynamic Ports\n");
        	    printf("=============\n");
		    dp_header = 1;
		}
	        amsg_print_port_queue(port_id);
	    }
        }
    }
}


void
amsg_print_pending_notifications()
{
    port_t * port;
    int level;

    if (!amsg_ready())
	return;

    if (notifications_level < 0) {
        printf("No Pending Notifications\n");
	return;
    }

    printf("-----------------------\n");
    printf(" Pending Notifications \n");
    printf("-----------------------\n");
    printf(" Level | Port          \n");

    for (level=0;level<=NOTIFY_LEVEL_MAX;level++) {
        if (!NF_Empty(level)) {
    	    printf("-------+---------------\n");
	    port = notifications[level].first;
            printf("   %2d  |", level);
            printf(" 0x%x \n", (char *) port);
	    port = port->next;
	    while (port) {
                printf("       |");
                printf(" 0x%x \n", (char *) port);
	        port = port->next;
	    }
        }
    }

    printf("-----------------------\n");
}

#endif /* NDEBUG */



/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

/**********************************************************************
** Port Primitives
***********************************************************************/

amsg_ret_t
aport_allocate(port_id,notify_procedure)
    aport_id_t * port_id;
    void (* notify_procedure) ();
{
    port_t * aport;
    unsigned index;
    amsg_ret_t aret;

    amsg_assert(amsg_ready());

#if !defined(TRUSTED)
    if (!port_id) return(AMSG_INVAL);
#endif

    PS_Lock();

    index = port_set_hint;
    while ((index < MAX_NOF_DYNAMIC_PORTS) && 
	   (dynamic_ports[index].aport_id != 0))
	index++;
    if (index < MAX_NOF_DYNAMIC_PORTS) {
	Aport_Id((bport_no_t) bport_self(),
                 index+dynamic_port_number_first,
                 port_id);
	aport = &dynamic_ports[index];
	P_Lock(aport);
	aport->aport_id = *port_id;
	aport->notify_procedure = notify_procedure;
	P_Unlock(aport);
	port_set_hint = (index + 1) % MAX_NOF_DYNAMIC_PORTS;
	aret = AMSG_OK;
    }
    else {
        index = 0;
        while ((index < port_set_hint) && 
	       (dynamic_ports[index].aport_id != 0))
	    index++;
        if (index < port_set_hint) {
	    Aport_Id((bport_no_t) bport_self(),
                     index+dynamic_port_number_first,
                     port_id);
	    aport = &dynamic_ports[index];
	    P_Lock(aport);
	    aport->aport_id = *port_id;
	    aport->notify_procedure = notify_procedure;
	    P_Unlock(aport);
	    port_set_hint = (index + 1) % MAX_NOF_DYNAMIC_PORTS;
	    aret = AMSG_OK;
        }
	else
	    aret = AMSG_NORESOURCES;
    }

    PS_Unlock();

    return(aret);
}


amsg_ret_t
aport_deallocate(port_id)
    aport_id_t port_id;
{
    port_t * port;

    amsg_assert(amsg_ready());

    if (!PortOwner(port_id)) return(AMSG_NOOWNER);
    if (!DynamicPort(port_id)) return(AMSG_NORIGHTS);

    GetPort(port_id,&port);
    if (!port) return(AMSG_NOPORT);

    P_Lock(port);

    if ((!P_Empty(port)) || (port->prev) || (port->next)) {
	P_Unlock(port);
	PS_Unlock();
	return(AMSG_ERROR);
    }
	
    port->aport_id = 0;
    port->notify_procedure = (void (*) ()) 0;
    port->notify_level = 0;
    port->notify_defer = (int) APORT_NOTIFY_DEFER_ALL;
    port->notify_pending = 0;
    port->data = (void_ptr) 0;
    port->info.sends = 0;
    port->info.receives = 0;

    P_Unlock(port);

    return(AMSG_OK);
}


amsg_ret_t 
aport_port(port_id,port)
    aport_id_t port_id;
    aport_t * port;
{
    bport_t bport;
    bmsg_ret_t bret;

    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!port) return(AMSG_INVAL);
#endif

    port->aport_id = port_id;
    port->bport_id = Bport_Id(port_id);
    bret = bport_port(port->bport_id,&bport);
    if (bret == BMSG_OK) {
	port->bdomain_id = bport.bdomain_id;
	return(AMSG_OK);
    }
    else {
	amsg_assert(bret == BMSG_NOPORT);
	return(AMSG_NOPORT);
    }
}


aport_id_t
aport_id(bport_id,index)
    bport_id_t bport_id;
    unsigned index;
{
    aport_id_t port_id;

    amsg_assert(amsg_ready());

    Aport_Id((bport_no_t) bport_id,static_port_number_first+index,&port_id);
    return(port_id);
}


bport_id_t
aport_bport_id(port_id)
    aport_id_t port_id;
{
    amsg_assert(amsg_ready());

    return(Bport_Id(port_id));
}


amsg_ret_t
aport_flush(port_id)
    aport_id_t port_id;
{
    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!SendRights(port_id)) return(AMSG_NOSENDRIGHTS);
#endif

    if (bport_flush(Bport_Id(port_id)) != BMSG_OK)
	return(AMSG_ERROR);

    return(AMSG_OK);
}


amsg_ret_t
aport_set_option(port_id,optname,optval)
    aport_id_t port_id;
    aport_optname_t optname;
    aport_optval_t optval;
{
    port_t * port;

    amsg_assert(amsg_ready());

    if (!PortOwner(port_id)) return(AMSG_NOOWNER);
    if (SystemPort(port_id)) return(AMSG_NORIGHTS);

    GetPort(port_id,&port);
    if (!port) return(AMSG_NOPORT);

    P_Lock(port);

    switch (optname) {
	case APORT_NOTIFY_LEVEL :
	    if (((int) optval > USER_NOTIFY_LEVEL_MAX) ||
		((int) optval < 0)) {
		P_Unlock(port);
	        return(AMSG_INVAL);
	    }
	    else
	        port->notify_level = (int) optval;
	    break;
	case APORT_NOTIFY_DEFER :
	    if ((optval != APORT_NOTIFY_DEFER_ONE) &&
		(optval != APORT_NOTIFY_DEFER_NONE) &&
		(optval != APORT_NOTIFY_DEFER_ALL)) {
		P_Unlock(port);
	        return(AMSG_INVAL);
	    }
	    else
		port->notify_defer = (int) optval;
	    break;
	case APORT_DATA_PTR :
	    port->data = (void_ptr) optval;
	    break;
	default :
	    P_Unlock(port);
	    return(AMSG_INVAL);
    }

    P_Unlock(port);

    return(AMSG_OK);
}


amsg_ret_t
aport_get_option(port_id,optname,optval)
    aport_id_t port_id;
    aport_optname_t optname;
    aport_optval_t * optval;
{
    port_t * port;

    amsg_assert(amsg_ready());

    if (!PortOwner(port_id)) return(AMSG_NOOWNER);
    if (SystemPort(port_id)) return(AMSG_NORIGHTS);

    GetPort(port_id,&port);
    if (!port) return(AMSG_NOPORT);

    P_Lock(port);

    switch (optname) {
	case APORT_NOTIFY_LEVEL :
	    *optval = (aport_optval_t) port->notify_level;
	    break;
	case APORT_NOTIFY_DEFER :
	    *optval = (aport_optval_t) port->notify_defer;
	    break;
	case APORT_DATA_PTR :
	    *optval = (aport_optval_t) port->data;
	    break;
	default :
	    P_Unlock(port);
	    return(AMSG_INVAL);
	    break;
    }

    P_Unlock(port);

    return(AMSG_OK);
}



/**********************************************************************
** Message Primitives
***********************************************************************/

amsg_ret_t
amsg_alloc(size,msg_data,msg)
    amsg_size_t size;
    amsg_data_t * * msg_data;
    amsg_t * msg;
{
    bmsg_ret_t bret;
    bmsg_data_t * bdata;
    bmsg_t bmsg;

    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!msg) return(AMSG_INVAL);
#endif

    bret = bmsg_alloc(sizeof(amsg_header_t) + size,
		      &bdata,
		      &bmsg);
    if (bret != BMSG_OK)
	return(amsg_abret(bret));

    *msg = (amsg_t) bmsg;
    if (msg_data)
        *msg_data = (amsg_data_t *)
	        ((char *) bdata + sizeof(amsg_header_t));


    /* initialise buffer scratch area */
    AmsgBuffer(bmsg)->amsg = (void_ptr) bdata;
#if !defined(NDEBUG)
    AmsgBuffer(bmsg)->next = (amsg_buffer_scratch_t *) 0;
    AmsgBuffer(bmsg)->sender = (bport_id_t) 0;
#endif

    /* initialise message header */
    AmsgHeader(bmsg)->amsg_type = AMSG_MSG_IDR;
#if !defined(NDEBUG)
    AmsgHeader(bmsg)->aport_id = (bport_id_t) 0;
#endif

    return(AMSG_OK);
}


amsg_ret_t
amsg_free(msg)
    amsg_t msg;
{
    bmsg_ret_t bret;

    bret = bmsg_free((bmsg_t) msg);
    if (bret != BMSG_OK)
        return(amsg_abret(bret));
    return(AMSG_OK);
}


amsg_size_t
amsg_size(msg)
    amsg_t msg;
{
    return((amsg_size_t) 
	   (bmsg_size((bmsg_t) msg) - sizeof(amsg_header_t)));
}


amsg_data_t *
amsg_data(msg)
    amsg_t msg;
{
    return((amsg_data_t *) 
	   ((char *) bmsg_data((bmsg_t) msg) + sizeof(amsg_header_t)));
}


amsg_ret_t
amsg_send(port_id,msg,msg_type,msg_count,option)
    aport_id_t port_id;
    amsg_t msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_option_t option;
{
    bmsg_data_t * bdata;
    bmsg_bool_t familiar;
    pds_size_t data_size;
    pds_size_t idr_data_size;
    bport_id_t destination;
    bmsg_ret_t bret;
    bmsg_t bmsg;
    pds_ret_t pret;
    port_t * aport;
    int level;
    XDR xdrs;

    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!msg) return(AMSG_INVAL);
    if (!SendRights(port_id)) return(AMSG_NOSENDRIGHTS);
#endif

    /* initialise message header */
    amsg_assert(AmsgHeader(msg)->amsg_type == AMSG_MSG_IDR);
    AmsgHeader(msg)->amsg_data_type = msg_type;
    AmsgHeader(msg)->amsg_count = msg_count;
    AmsgHeader(msg)->aport_id = port_id;

#if !defined(AMSG_STAT_OFF)
    if ((pret = pds_type_size(msg_type,
                              &idr_data_size,
                              MDT_IDR)) != PDS_OK)
	return((amsg_ret_t) pret);
    else
        idr_data_size *= msg_count;
#endif

    destination = Bport_Id(port_id);

    if (destination != bport_self()) { /* send to remote aport */
	if ((bret = bport_familiar(destination,&familiar)) != BMSG_OK)
	    return(amsg_abret(bret));
	if (!familiar) {
	    /* determine message data size */
            if ((pret = pds_type_size(msg_type,&data_size,MDT_XDR)) != PDS_OK)
                return((amsg_ret_t) pret);
            data_size *= AmsgHeader(msg)->amsg_count;
	    bret = bmsg_alloc(amsg_xdr_size.amsg_header+data_size,&bdata,&bmsg);
            if (bret != BMSG_OK)
                return(amsg_abret(bret));
    
	    /* modify type field of  header */
            AmsgHeader(msg)->amsg_type = AMSG_MSG_XDR;

	    xdrmem_create(&xdrs,(const caddr_t) bdata,
		          (const u_int) amsg_xdr_size.amsg_header+data_size,
		          XDR_ENCODE);

	    if (!xdr_amsg_header(&xdrs,AmsgHeader(msg)))
		amsg_assert_always();
	    if (!pds_msg_xdr(&xdrs,msg_type,msg_count,amsg_data(msg)))
		return(AMSG_INVAL);

	    xdr_destroy(&xdrs);

            bret = bmsg_send(destination,bmsg,amsg_xdr_size.amsg_header+data_size);
	    if (bret != BMSG_OK) {
	        (void) bmsg_free(bmsg);
                return(amsg_abret(bret));
	    }

	    (void) amsg_free(msg);
        }
	else { /* send to familiar remote aport */
	    /* determine message data size */
#if defined(AMSG_STAT_OFF)
    	    if ((pret = pds_type_size(msg_type,
                                      &data_size,
                              	      MDT_IDR)) != PDS_OK)
		return((amsg_ret_t) pret);
            data_size *= AmsgHeader(msg)->amsg_count; 
#else
            data_size = idr_data_size; 
#endif /* AMSG_STAT_OFF */
	    bret = bmsg_send(destination,(bmsg_t) msg,
			     sizeof(amsg_header_t) + data_size);
            if (bret != BMSG_OK)
                return(amsg_abret(bret));
	}
    }
    else { /* send to local aport */
#if defined(AMSGLOG) || !defined(NDEBUG)
    	/* store identifier of sender in message buffer */
        AmsgBuffer(msg)->sender = bport_self();
#endif
	GetPort(port_id,&aport);
        if (!aport) return(AMSG_NOPORT);
	P_Lock(aport);
	if (P_Empty(aport) && aport->notify_procedure) {
            /* append message to queue of destination aport */
	    P_Put(aport,AmsgBuffer(msg));
	    aport->notify_pending = 1;
	    level = aport->notify_level;
	    NFS_Lock();
	    NF_Put(aport);
    	    NFS_Unlock();
	    P_Unlock(aport);
	    /* notify message arrival */
            amsg_notify(level);
	}
	else {
            /* append message to queue of destination aport */
            P_Put(aport,AmsgBuffer(msg));
	    P_Unlock(aport);
	}
    }

    MsgInfo_Sent_Increment(idr_data_size);
    AMSGLOG_SEND_MESSAGE(port_id);
    return(AMSG_OK);
}


amsg_ret_t
amsg_receive(port_id,msg,msg_data,msg_type,msg_count,option)
    aport_id_t port_id;
    amsg_t * msg;
    amsg_data_t * * msg_data;
    amsg_type_t * msg_type;
    amsg_count_t * msg_count;
    amsg_option_t option;
{
    port_t * port;
    amsg_t amsg;

    amsg_assert(amsg_ready());

    if (!ReceiveRights(port_id)) return(AMSG_NORCVRIGHTS);
    if (SystemPort(port_id)) return(AMSG_NORIGHTS);

    GetPort(port_id,&port);
    if (!port) return(AMSG_NOPORT);

    P_Lock(port);

    if (P_Empty(port)) {
	P_Unlock(port);
	return(AMSG_NOMESSAGE);
    }
    else {
        P_Get(port,(amsg_buffer_scratch_t * *) &amsg);
	MsgInfo_Rcvd_Increment(amsg_size(amsg)); 
	AMSGLOG_RCV_MESSAGE(AmsgBuffer(amsg)->sender,port_id);
	if (port->notify_pending) {
            NFS_Lock();
	    NF_Isolate(port);
            NFS_Unlock();
	    port->notify_pending = 0;
	}
	P_Unlock(port);
	if (msg_type) 
	    *msg_type = AmsgHeader(amsg)->amsg_data_type;
	if (msg_count) 
	    *msg_count = AmsgHeader(amsg)->amsg_count;
	if (msg) {
	    *msg = amsg;
	    if (msg_data)
	        *msg_data = AmsgData(amsg);
	    return(AMSG_OK);
	}
	else 
	    return(amsg_free(amsg));
    }
}


amsg_ret_t
amsg_peek(port_id,msg,msg_data,msg_type,msg_count)
    aport_id_t port_id;
    amsg_t * msg;
    amsg_data_t * * msg_data;
    amsg_type_t * msg_type;
    amsg_count_t * msg_count;
{
    port_t * port;
    amsg_t amsg;

    amsg_assert(amsg_ready());

    if (!ReceiveRights(port_id)) return(AMSG_NORCVRIGHTS);
    if (SystemPort(port_id)) return(AMSG_NORIGHTS);

    GetPort(port_id,&port);
    if (!port) return(AMSG_NOPORT);

    P_Lock(port);

    if (P_Empty(port)) {
	P_Unlock(port);
	return(AMSG_NOMESSAGE);
    }
    else {
        P_Peek(port,(amsg_buffer_scratch_t * *) &amsg);
	if (msg)
	    *msg = amsg;
        if (msg_data)
	    *msg_data = AmsgData(amsg);
	if (msg_type)
            *msg_type = AmsgHeader(amsg)->amsg_data_type;
        if (msg_count)
            *msg_count = AmsgHeader(amsg)->amsg_count;
	P_Unlock(port);
	return(AMSG_OK);
    }
}


amsg_ret_t
amsg_forward(port_id_src,port_id_dst)
    aport_id_t port_id_src;
    aport_id_t port_id_dst;
{
    amsg_assert(amsg_ready());

    return(AMSG_NYI);
}



/**********************************************************************
** Miscellaneous Primitives
***********************************************************************/

amsg_ret_t
amsg_init(size,notify_procedure,port_id,option)
    unsigned size;		     /*  in: # ports in default port set */
    void (* notify_procedure []) (); /*  in: port notify procedures      */
    aport_id_t port_id [];  	     /* out: port identifiers	         */
    amsg_option_t option;
{
    amsg_ret_t aret;

#if !defined(TRUSTED) 
    if (size && (!notify_procedure || !port_id)) return(AMSG_INVAL);
#endif

    if (amsg_initialised || amsg_initialising) 
	return(AMSG_ERROR);

    amsg_initialising = 1;

    /* check implementation assumptions */
    if (sizeof(amsg_buffer_scratch_t) > BMSG_BUF_SCRATCH_BYTES)
	return(AMSG_IMPLIM);
    if (sizeof(aport_id_t) != sizeof(aport_id_struct_t))
	return(AMSG_IMPLIM);

    if (option & AMSG_ALOG_ON) amsg_alog_on = 1;
    if (option & AMSG_ALOG_OPEN) amsg_alog_open = 1;
    if (option & AMSG_ALOG_CLOSE) amsg_alog_close = 1;
    if (option & AMSG_ALOG_MASTER) amsg_alog_master = 1;

    /* initialise port set */
    aret = aport_set_init(size,notify_procedure,port_id);
    if (aret != AMSG_OK)
	return(aret);

    /* initialise notification data structures */
    notifications_init();

    /* initialise message ordering mutex */
    mo_mutex_init();

    /* initialise statistics */
    amsg_info_init();

    /* initialise logging system */
    amsg_openlog();

    /* initialise XDR system */
    amsg_xdr_init();

    /* initialise type system */
    if ((aret = amsg_types_init()) != AMSG_OK)
        return(aret);

    /* initialisation was successful */
    amsg_initialised = 1;
    amsg_initialising = 0;

    return(AMSG_OK);
}


void
amsg_exit()
{
    if (!amsg_initialised || amsg_exiting || amsg_exited)
	return;

    amsg_exiting = 1;

    free(ports);
    amsg_closelog();

    amsg_exited = 1;
    amsg_exiting = 0;
}


amsg_ret_t
amsg_info(info)
    amsg_info_t * info;
{
    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!info) return(AMSG_INVAL);
#endif

    MsgInfo_Lock();

    info->sent_short = msg_info.sent_short;
    info->sent_medium = msg_info.sent_medium;
    info->sent_long = msg_info.sent_long;
    info->rcvd_short = msg_info.rcvd_short;
    info->rcvd_medium = msg_info.rcvd_medium;
    info->rcvd_long = msg_info.rcvd_long;

    MsgInfo_Unlock();
    
    return(AMSG_OK);
}


amsg_ret_t
aport_info(port_id,info)
    aport_id_t port_id;
    aport_info_t * info;
{
    amsg_assert(amsg_ready());

#if !defined(TRUSTED) 
    if (!info) return(AMSG_INVAL);
#endif

    return(AMSG_NYI);
}


void 
bmsg_notify()
{
    amsg_size_t adata_size;
    bmsg_size_t bdata_size;
    bmsg_bool_t familiar;
    amsg_header_t header;
    amsg_data_t * adata;
    bmsg_data_t * bdata;
    bport_id_t source;
    amsg_ret_t aret;
    bmsg_ret_t bret;
    pds_ret_t pret;
    port_t * aport;
    amsg_t amsg;
    bmsg_t bmsg;
    int level = 0;
    XDR xdrs;

    /* receive messages */

    MO_Lock(); /* ensure message ordering on any aport */

    while ((bret = bmsg_receive(&bmsg,
                                &bdata,
                                &bdata_size,
	  		        &source,
				&familiar)) == BMSG_OK) {

	if ((source != bport_self()) && !familiar) {

	    xdrmem_create(&xdrs,(const caddr_t) bdata,
			  (const u_int) bdata_size,
			  XDR_DECODE);

	    if (!xdr_amsg_header(&xdrs,&header))
		amsg_assert_always();

	    xdr_destroy(&xdrs);

	    /* determine message data size */
            pret = pds_type_size(header.amsg_data_type,&adata_size,MDT_IDR);
	    if (pret != PDS_OK) {
		MO_Unlock();
		amsg_error((amsg_error_t) pret,(aport_id_t) source);
	    }
	    adata_size *= header.amsg_count;

	    aret = amsg_alloc(adata_size,&adata,&amsg);
	    if (aret != AMSG_OK) {
		if (aret > PDS_RET_MAX)
		    aret = AMSG_ERROR;
		MO_Unlock();
		amsg_panic((amsg_panic_t) aret,(aport_id_t) bport_self());
	    }

	    header.amsg_type = AMSG_MSG_IDR;
	    *AmsgHeader(amsg) = header;

	    xdrmem_create(&xdrs,(const caddr_t) bdata + amsg_xdr_size.amsg_header,
			  (const u_int) bdata_size - amsg_xdr_size.amsg_header,
			  XDR_DECODE);

	    if (!pds_msg_xdr(&xdrs,header.amsg_data_type,header.amsg_count,adata))
		amsg_assert_always();

	    xdr_destroy(&xdrs);

	    (void) bmsg_free(bmsg);

	    bmsg = (bmsg_t) amsg;
	    bdata = bmsg_data(bmsg);
	}

	/* initialise 'scratch' area of message buffer */
	AmsgBuffer(bmsg)->sender = source;
	AmsgBuffer(bmsg)->amsg = (amsg_t *) bdata;

        GetPort(AmsgHeader(bmsg)->aport_id,&aport);
        if (!aport) {
	    MO_Unlock();
	    amsg_error(AMSG_NOPORT,(aport_id_t) source);
	}

	P_Lock(aport); /* ensure message ordering on specific aport */ 
	MO_Unlock();
	if (P_Empty(aport) && aport->notify_procedure) {
	    P_Put(aport,AmsgBuffer(bmsg));
	    NFS_Lock();
	    NF_Put(aport);
	    aport->notify_pending = 1;
	    if (aport->notify_level > level)
	        level = aport->notify_level;
    	    NFS_Unlock();
	}
	else {
	    P_Put(aport,AmsgBuffer(bmsg));
	}
	P_Unlock(aport);
        MO_Lock(); /* ensure message ordering on any aport */
    }

    MO_Unlock();

    /* notify message arrival */
    amsg_notify(level);
}


