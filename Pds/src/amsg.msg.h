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
**        File: amsg.msg.h
**      Author: Kees Schuerman
**      SccsId: "@(#)amsg.msg.h	1.1 8/31/95"
** Description: Message Passing System: Application Layer Interface
***********************************************************************/

#ifndef _AMSG_MSG_H_
#define _AMSG_MSG_H_


/* 
** Return Codes
*/

#define AMSG_OK              PDS_OK            /* success               */
#define AMSG_NYI	     PDS_NYI	       /* not yet implemented   */
#define AMSG_WARN            PDS_WARN          /* general warning       */
#define AMSG_ERROR           PDS_ERROR         /* general error         */
#define AMSG_IMPLIM          PDS_IMPLIM        /* implementation limit  */
#define AMSG_INVAL           PDS_INVAL         /* invalid argument      */
#define AMSG_NORESOURCES     PDS_NORESOURCES   /* no resources	        */
#define AMSG_NOMEMORY        PDS_NOMEMORY      /* no memory             */
#define AMSG_NOT_READY       PDS_NOT_READY     /* not ready             */

#define AMSG_NOPORT          (PDS_RET_MAX + 1)  /* no such port         */
#define AMSG_NOMESSAGE       (PDS_RET_MAX + 2)  /* no message           */
#define AMSG_NOOWNER 	     (PDS_RET_MAX + 3)  /* no owner		*/
#define AMSG_NOSENDRIGHTS    (PDS_RET_MAX + 4)  /* no send rights	*/
#define AMSG_NORCVRIGHTS     (PDS_RET_MAX + 5)  /* no receive rights	*/
#define AMSG_NORIGHTS 	     (PDS_RET_MAX + 6)  /* no rights		*/
#define AMSG_PBLOCKED        (PDS_RET_MAX + 7)  /* port blocked         */
#define AMSG_PDYING          (PDS_RET_MAX + 8)  /* port dying           */
#define AMSG_PNOTAVAILABLE   (PDS_RET_MAX + 9)  /* port not available   */
#define AMSG_PUNREACH  	     (PDS_RET_MAX + 10) /* port unreachable 	*/


/*
** Warnings, Errors, Panics
*/

#define AMSG_WEP_OK	     AMSG_OK           /* success               */
#define AMSG_WEP_NYI	     AMSG_NYI	       /* not yet implemented   */
#define AMSG_WEP_WARN        AMSG_WARN         /* general warning       */
#define AMSG_WEP_ERROR       AMSG_ERROR        /* general error         */
#define AMSG_WEP_IMPLIM      AMSG_IMPLIM       /* implementation limit  */
#define AMSG_WEP_INVAL       AMSG_INVAL        /* invalid argument      */
#define AMSG_WEP_NORESOURCES AMSG_NORESOURCES  /* no resources	        */
#define AMSG_WEP_NOMEMORY    AMSG_NOMEMORY     /* no memory             */

#define AMSG_WEP_NOPORT	     AMSG_NOPORT       /* no such port		*/


/*
** Version
*/

typedef struct {
    pds_uint32 v_minor;
    pds_uint32 v_major;
} amsg_version_t;

extern msg_type_t MDT_AMSGVERSION;


/*
** Message Passing System Options
*/

#define AMSG_ALOG_ON            0x10
#define AMSG_ALOG_OPEN          0x20
#define AMSG_ALOG_CLOSE         0x40
#define AMSG_ALOG_MASTER        0x80


/*
** Generic Option Values
*/

#define AMSG_OFF        	0x00
#define AMSG_ON         	0x01


/*
** Port Option Names
*/

#define APORT_NOTIFY      	0x01	/* on,off      		  */
#define APORT_NOTIFY_LEVEL	0x02	/* notify level		  */
#define APORT_NOTIFY_DEFER     	0x03	/* off,local,global  	  */
#define APORT_DATA_PTR         	0x04	


/*
** Port Option Values
*/

#define APORT_NOTIFY_LEVEL_MAX		15
#define APORT_NOTIFY_DEFER_OFF		((aport_optval_t) 0x00)
#define APORT_NOTIFY_DEFER_LOCAL	((aport_optval_t) 0x01)
#define APORT_NOTIFY_DEFER_GLOBAL 	((aport_optval_t) 0xFF)



/*
** Message Classes
*/

#define AMSG_SHORT      64      /* message data size:     <=   64 bytes */
#define AMSG_MEDIUM     512     /*                  :  65 ..  512 bytes */
#define AMSG_LONG               /*                  :      >  512 bytes */


/* 
** Type Definitions and Associated Message Data Types
*/

#define AMSG_INTFC	2

typedef pds_byte_t amsg_descriptor_t;
#define msg_descriptor_t amsg_descriptor_t

typedef pds_word_t aport_id_t;
typedef pds_size_t amsg_size_t;
typedef void_ptr amsg_t;
typedef msg_data_t amsg_data_t;
typedef void_ptr aport_optval_t;
typedef msg_option_t amsg_option_t;
typedef pds_word_t aport_optname_t;
typedef msg_type_t amsg_type_t;
typedef msg_typedef_t amsg_typedef_t;
typedef msg_type_no_t amsg_type_no_t;
typedef msg_intfc_no_t amsg_intfc_no_t;
typedef msg_count_t amsg_count_t;
typedef pds_ret_t amsg_ret_t;

#define MDT_APORTID		MDT_WORD
#define MDT_AMSGSIZE		MDT_SIZE
#define MDT_AMSGOPTION		MDT_MSGOPTION
#define MDT_APORTOPTNAME	MDT_WORD
#define MDT_AMSGTYPE		MDT_MSGTYPE
#define MDT_AMSGTYPEDEF		MDT_MSGTYPEDEF
#define MDT_AMSGTYPENO		MDT_MSGTYPENO
#define MDT_AMSGINTFCNO		MDT_MSGINTFCNO
#define MDT_AMSGCOUNT		MDT_MSGCOUNT
#define MDT_AMSGRET		MDT_RET


typedef struct {
    aport_id_t aport_id;	/* aport identifier   */
    bport_id_t bport_id;	/* bport identifier   */
    bdomain_id_t bdomain_id;	/* bdomain identifier */
} aport_t;

extern msg_type_t MDT_APORT;

typedef pds_dp_float amsg_counter_t;

#define MDT_AMSGCOUNTER		MDT_DP_FLOAT

typedef struct {
    amsg_counter_t sent_short;	/* # short sized messages sent	    */
    amsg_counter_t sent_medium;	/* # medium sized messages sent     */
    amsg_counter_t sent_long;	/* # long sized messages sent	    */
    amsg_counter_t rcvd_short;	/* # short sized messages received  */
    amsg_counter_t rcvd_medium; /* # medium sized messages received */
    amsg_counter_t rcvd_long;	/* # long sized messages received   */
} amsg_info_t;

extern msg_type_t MDT_AMSGINFO;

typedef struct {
    amsg_counter_t sends;	/* # messages sent      */
    amsg_counter_t receives;	/* # messages received  */
} aport_info_t;

extern msg_type_t MDT_APORTINFO;

typedef pds_word_t amsg_wep_t;
typedef amsg_wep_t amsg_warn_t;
typedef amsg_wep_t amsg_error_t;
typedef amsg_wep_t amsg_panic_t;

#define MDT_AMSGWEP	MDT_WORD
#define MDT_AMSGWARN	MDT_AMSGWEP
#define MDT_AMSGERROR	MDT_AMSGWEP
#define MDT_AMSGPANIC	MDT_AMSGWEP



/*
** Type System
*/

#define amsg_type_define(IntfcNo,TypeNo,TypeDef,MsgType)        \
        pds_type_define(IntfcNo,TypeNo,TypeDef,MsgType)

#define amsg_type_size(MsgType,Size,Option)                     \
        pds_type_size(MsgType,Size,Option)

#define amsg_type_xdr(Xdrs,MsgType,MsgData)                     \
        pds_type_xdr(Xdrs,MsgType,MsgData)



/* 
** Port Primitives
*/

#if defined(__STDC__)
extern amsg_ret_t aport_allocate(aport_id_t * port_id,
				 void (* notify_procedure)
				      (aport_id_t aport_id));
extern amsg_ret_t aport_deallocate(aport_id_t port_id);
extern amsg_ret_t aport_port(aport_id_t port_id,
                             aport_t * port);
extern aport_id_t aport_id(bport_id_t bport_id,
			   unsigned index);
extern bport_id_t aport_bport_id(aport_id_t port_id);
extern amsg_ret_t aport_flush(aport_id_t port_id);
extern amsg_ret_t aport_set_option(aport_id_t port_id,
				   aport_optname_t optname,
				   aport_optval_t optval);
extern amsg_ret_t aport_get_option(aport_id_t port_id,
				   aport_optname_t optname,
				   aport_optval_t * optval);
#else /* __STDC__ */
extern amsg_ret_t aport_allocate();
extern amsg_ret_t aport_deallocate();
extern amsg_ret_t aport_port();
extern aport_id_t aport_id();
extern bport_id_t aport_bport_id();
extern amsg_ret_t aport_flush();
extern amsg_ret_t aport_set_option();
extern amsg_ret_t aport_get_option();
#endif /* __STDC__ */


/* 
** Message Primitives
*/

#if defined(__STDC__)
extern amsg_ret_t amsg_alloc(amsg_size_t size,
			     amsg_data_t * * msg_data,
			     amsg_t * msg);
extern amsg_ret_t amsg_free(amsg_t msg);
extern amsg_size_t amsg_size(amsg_t msg);
extern amsg_data_t * amsg_data(amsg_t msg);
extern amsg_ret_t amsg_send(aport_id_t port_id,
			    amsg_t msg,
			    amsg_type_t msg_type,
			    amsg_count_t msg_count,
			    amsg_option_t option);
extern amsg_ret_t amsg_receive(aport_id_t port_id,
			       amsg_t * msg,
			       amsg_data_t * * msg_data,
                               amsg_type_t * msg_type,
			       amsg_count_t * msg_count,
			       amsg_option_t option);
extern amsg_ret_t amsg_peek(aport_id_t port_id,
			    amsg_t * msg,
                            amsg_data_t * * msg_data,
                            amsg_type_t * msg_type,
                            amsg_count_t * msg_count);
#else /* __STDC__ */
extern amsg_ret_t amsg_alloc();
extern amsg_ret_t amsg_free();
extern amsg_size_t amsg_size();
extern amsg_data_t * amsg_data();
extern amsg_ret_t amsg_send();
extern amsg_ret_t amsg_receive();
extern amsg_ret_t amsg_peek();
#endif /* __STDC__ */


/* 
** Miscellaneous Primitives
*/

extern int amsg_initialised;
extern int amsg_exited;
extern int amsg_exiting;

#define amsg_ready()	(amsg_initialised && !amsg_exited)


#if defined(__STDC__)
extern bmsg_ret_t amsg_version(amsg_version_t * version);
extern amsg_ret_t amsg_init(unsigned size,
			    void (* notify_procedure []) 
				 (aport_id_t port_id),
			    aport_id_t port_id [],
			    amsg_option_t option);
extern void amsg_exit(void);
extern void amsg_warn(amsg_warn_t msg_warn,			/* upcall */
		      aport_id_t culprit);
extern void amsg_error(amsg_error_t msg_error,			/* upcall */
		       aport_id_t culprit);
extern void amsg_panic(amsg_panic_t msg_panic,			/* upcall */
		       aport_id_t culprit);
extern char * amsg_error_string(amsg_ret_t aret);
extern void amsg_perror(amsg_ret_t aret,
                        char * s);
extern amsg_ret_t amsg_info(amsg_info_t * msg_info);
extern amsg_ret_t aport_info(aport_id_t port_id,
			     aport_info_t * port_info);
#else /* __STDC__ */
extern amsg_ret_t amsg_version();
extern amsg_ret_t amsg_init();
extern void amsg_exit();
extern void amsg_warn();					/* upcall */
extern void amsg_error();					/* upcall */
extern void amsg_panic();					/* upcall */
extern char * amsg_error_string();
extern void amsg_perror();
extern amsg_ret_t amsg_info();
extern amsg_ret_t aport_info();
#endif /* __STDC__ */

#endif /* _AMSG_MSG_H_ */

