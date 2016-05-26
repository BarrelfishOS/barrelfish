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
**        File: bmsg.msg.h
**      Author: Kees Schuerman
**      SccsId: "@(#)bmsg.msg.h	1.1 6/28/95"
** Description: Basic Message Passing Library API 
***********************************************************************/

#ifndef _BMSG_MSG_H_
#define _BMSG_MSG_H_


/* 
** Return Codes
*/

#define BMSG_OK              PDS_OK             /* success               */
#define BMSG_NYI             PDS_NYI            /* not yet implemented   */
#define BMSG_WARN            PDS_WARN           /* general warning       */
#define BMSG_ERROR           PDS_ERROR          /* general error         */
#define BMSG_IMPLIM          PDS_IMPLIM         /* implementation limit  */
#define BMSG_INVAL           PDS_INVAL          /* invalid argument      */
#define BMSG_NORESOURCES     PDS_NORESOURCES    /* no resources          */
#define BMSG_NOMEMORY        PDS_NOMEMORY       /* no memory             */

#define BMSG_NOPORT	     (PDS_RET_MAX + 1)  /* no such port          */
#define BMSG_NOMESSAGE	     (PDS_RET_MAX + 2)  /* no message            */
#define BMSG_NOID	     (PDS_RET_MAX + 3)  /* no identifiers 	 */
#define BMSG_POPENED	     (PDS_RET_MAX + 4)  /* port opened	   	 */
#define BMSG_PBLOCKED        (PDS_RET_MAX + 5)  /* port blocked     	 */
#define BMSG_PUNBLOCKED      (PDS_RET_MAX + 6)  /* port unblocked        */
#define BMSG_POPENING	     (PDS_RET_MAX + 7)  /* port opening        	 */
#define BMSG_PCLOSING	     (PDS_RET_MAX + 8)  /* port closing      	 */
#define BMSG_PBLOCKING	     (PDS_RET_MAX + 9)  /* port blocking   	 */
#define BMSG_PUNBLOCKING     (PDS_RET_MAX + 10) /* port unblocking   	 */
#define BMSG_PDYING          (PDS_RET_MAX + 11) /* port dying        	 */
#define BMSG_PNOTAVAILABLE   (PDS_RET_MAX + 12) /* port not available	 */
#define BMSG_PUNREACH        (PDS_RET_MAX + 13) /* port unreachable      */


/* 
** Warnings, Errors, Panics 
*/

#define BMSG_WEP_NONE  		0
#define BMSG_WEP_INCONSISTENCY	1
#define BMSG_WEP_RESOURCES	2
#define BMSG_WEP_MEMORY  	3
#define BMSG_WEP_ID  		4
#define BMSG_WEP_PORT  		5
#define BMSG_WEP_PDIED 		6


/*
** Message Passing System Options
*/

#define BDOMAIN_CREATE		0x01
#define BPORT_NOTIFICATION	0x02
#define BMEM_NOTIFICATION	0x04

#define BMSG_ALOG_ON		0x10
#define BMSG_ALOG_OPEN		0x20
#define BMSG_ALOG_CLOSE		0x40
#define BMSG_ALOG_MASTER	0x80


/*
** Message Passing Trigger Options
*/

#define BMSG_INTER_DOMAIN	0x01
#define BMSG_INTRA_DOMAIN	0x02


/*
** Option Names
*/

#define BMSG_INTRA_DOMAIN_TRIGGERING	0x01


/*
** Option Values
*/

#define BMSG_OFF	0x00
#define BMSG_ON		0x01


/*
** Message Passing System Events
*/

#define BEVENT_PORT_OPEN_ACK		0x0001
#define BEVENT_PORT_OPEN_NOTIFY		0x0002
#define BEVENT_PORT_CLOSE_ACK		0x0004
#define BEVENT_PORT_CLOSE_NOTIFY	0x0008
#define BEVENT_PORT_BLOCK_ACK		0x0010
#define BEVENT_PORT_BLOCK_NOTIFY	0x0020
#define BEVENT_PORT_UNBLOCK_ACK		0x0040
#define BEVENT_PORT_UNBLOCK_NOTIFY	0x0080
#define BEVENT_MEM_PUT_ACK		0x0100
#define BEVENT_MEM_PUT_NOTIFY		0x0200
#define BEVENT_MEM_GET_ACK		0x0400
#define BEVENT_MEM_GET_NOTIFY		0x0800
#define BEVENT_MSG			0x1000


/*
** Port Primitives
*/

#define BPORT_OPEN 		0x01
#define BPORT_CLOSE		0x02
#define BPORT_BLOCK     	0x03
#define BPORT_UNBLOCK     	0x04


/*
** Memory Primitives
*/

#define BMEM_PUT 		0x05
#define BMEM_GET 		0x06


/*
** Message Classes                (Message Data Size)
*/

#define BMSG_SHORT	64	/*     <=   64 bytes */
#define BMSG_MEDIUM	512 	/*  65 ..  512 bytes */
#define BMSG_LONG	    	/*      >  512 bytes */


/*
** Message Buffer Scratch Area
*/

#define BMSG_BUF_SCRATCH_BYTES	(3 * sizeof(void_ptr))



/*
** Generic Type Definitions and Associated Message Data Types
*/

#define BMSG_INTFC	1

typedef pds_size_t bmsg_size_t;
typedef pds_ret_t bmsg_ret_t;

#define MDT_BMSGSIZE		MDT_SIZE
#define MDT_BMSGRET		MDT_RET

typedef pds_word_t bport_id_t;
typedef pds_word_t bdomain_id_t;
typedef pds_uint16 bport_number_t;
typedef pds_word_t bpid_t;

#define BPORT_ID_MAX		PDS_HALF_WORD_MAX
#define BDOMAIN_ID_MAX		PDS_WORD_MAX

#define MDT_BPORTID		MDT_WORD
#define MDT_BDOMAINID		MDT_WORD
#define MDT_BPORTNUMBER		MDT_UINT16
#define MDT_BPID		MDT_WORD

typedef pds_word_t bmem_id_t;
typedef pds_size_t bmem_size_t;
typedef pds_double_word_t bmsg_address_t;

#define BMSG_ADDR_OFFSET	PDS_ADDR_OFFSET

#define MDT_BMEMID		MDT_WORD
#define MDT_BMEMSIZE		MDT_SIZE
#define MDT_BMSGADDRESS		MDT_ADDRESS

typedef void_ptr bmem_address_t;

#define INET_ADDRESSLEN_MAX 32
#define BNET_ADDRESSLEN_MAX INET_ADDRESSLEN_MAX

typedef char inet_address_t[INET_ADDRESSLEN_MAX+1];
typedef inet_address_t bnet_address_t;

extern msg_type_t MDT_INETADDRESS;
#define MDT_BNETADDRESS	MDT_INETADDRESS

typedef pds_int32 bmsg_bool_t;
typedef void_ptr bmsg_t;

#define MDT_BMSGBOOL		MDT_INT32

typedef msg_type_t bmsg_type_t;
typedef msg_type_no_t bmsg_type_no_t;
typedef msg_intfc_no_t bmsg_intfc_no_t;
typedef msg_typedef_t bmsg_typedef_t;
typedef msg_count_t bmsg_count_t;
typedef msg_data_t bmsg_data_t;
typedef pds_word_t bmsg_optval_t;
typedef msg_option_t bmsg_option_t;
typedef pds_word_t bmsg_optname_t;


#define MDT_BMSGTYPE            MDT_MSGTYPE
#define MDT_BMSGTYPENO          MDT_MSGTYPENO
#define MDT_BMSGINTFCNO         MDT_MSGINTFCNO
#define MDT_BMSGTYPEDEF         MDT_MSGTYPEDEF
#define MDT_BMSGCOUNT           MDT_MSGCOUNT
#define MDT_BMSGDATA 		MDT_MSGDATA
#define MDT_BMSGOPTION		MDT_MSGOPTION
#define MDT_BMSGOPTVAL		MDT_WORD
#define MDT_BMSGOPTNAME		MDT_WORD


#define BMSG_FILENAMELEN_MAX 255

typedef struct {
    bpid_t bpid;		       /* (local) process identifier	  */
    bport_id_t bport_id;	       /* port identifier	          */
    bdomain_id_t bdomain_id;	       /* domain identifier  	          */
#if WORDS_BIGENDIAN
    unsigned : PDS_PAD_BITS;           /* pad				  */
    bmem_address_t bmsg_queue_address; /* address of port's message queue */
#else /* LITTLE ENDIAN */
    bmem_address_t bmsg_queue_address; /* address of port's message queue */
    unsigned : PDS_PAD_BITS;           /* pad				  */
#endif /* WORDS_BIGENDIAN */
    bnet_address_t bnet_address;       /* network address	  	  */
    bport_number_t bport_number;       /* port number             	  */
} bport_t;

extern msg_type_t MDT_BPORT;

typedef struct {
    bdomain_id_t bdomain_id;	  /* domain identifier		      */
    char bdomain_file[BMSG_FILENAMELEN_MAX+1];  
				  /* shared memory backing store file */
#if WORDS_BIGENDIAN
    unsigned : PDS_PAD_BITS;      /* pad			      */
    bmem_address_t bdomain_start; /* shared memory start address      */
#else /* LITTLE ENDIAN */
    bmem_address_t bdomain_start; /* shared memory start address      */
    unsigned : PDS_PAD_BITS;      /* pad			      */
#endif /* WORDS_BIGENDIAN */
    bmem_size_t bdomain_size;     /* shared memory size 	      */
} bdomain_t;

extern msg_type_t MDT_BDOMAIN;

typedef pds_dp_float bmsg_counter_t;

#define MDT_BMSGCOUNTER MDT_DP_FLOAT

typedef struct {
    bmsg_counter_t sent_short;	 /* # short sized messages sent      */
    bmsg_counter_t sent_medium;	 /* # medium sized messages sent     */
    bmsg_counter_t sent_long;	 /* # long sized messages sent       */
    bmsg_counter_t rcvd_short;	 /* # short sized messages received  */
    bmsg_counter_t rcvd_medium;	 /* # medium sized messages received */
    bmsg_counter_t rcvd_long;	 /* # long sized messages received   */
} bmsg_info_t;

typedef struct {
    bmsg_counter_t lputs;  	 /* # puts to local memory    	     */
    bmsg_counter_t lgets;  	 /* # gets from local memory  	     */
    bmsg_counter_t rputs;	 /* # puts to remote memory   	     */
    bmsg_counter_t rgets;	 /* # gets from remote memory 	     */
} bmem_info_t;

typedef struct {
    bmsg_counter_t lputs;	 /* # puts to local memory	     */
    bmsg_counter_t lgets;	 /* # gets from local memory	     */
    bmsg_counter_t rputs;	 /* # puts to remote memory   	     */
    bmsg_counter_t rgets;	 /* # gets from remote memory 	     */
    bmsg_counter_t sends;	 /* # messages sent to port 	     */
    bmsg_counter_t receives;	 /* # messages received from port    */
} bport_info_t;

extern msg_type_t MDT_BMSGINFO;
extern msg_type_t MDT_BMEMINFO;
#define MDT_BPORTINFO	MDT_BMSGINFO

typedef pds_word_t bmem_primitive_t;
typedef pds_word_t bport_primitive_t;

#define MDT_BMEMPRIMITIVE	MDT_WORD
#define MDT_BPORTPRIMITIVE	MDT_WORD

typedef pds_word_t bmsg_wep_t;
typedef bmsg_wep_t bmsg_warn_t;
typedef bmsg_wep_t bmsg_error_t;
typedef bmsg_wep_t bmsg_panic_t;

#define MDT_BMSGWEP     MDT_WORD
#define MDT_BMSGWARN    MDT_BMSGWEP
#define MDT_BMSGERROR   MDT_BMSGWEP
#define MDT_BMSGPANIC   MDT_BMSGWEP

typedef struct {
    pds_uint32 v_minor;
    pds_uint32 v_major;
} bmsg_version_t;

extern msg_type_t MDT_BMSGVERSION;



/*
** Type System
*/

#define bmsg_type_define(IntfcNo,TypeNo,TypeDef,MsgType)	\
	pds_type_define(IntfcNo,TypeNo,TypeDef,MsgType)

#define bmsg_type_size(MsgType,Size,Option)			\
	pds_type_size(MsgType,Size,Option)

#define bmsg_type_xdr(Xdrs,MsgType,MsgData)			\
	pds_type_xdr(Xdrs,MsgType,MsgData)



/* 
** Port Primitives
*/

extern bport_id_t bport_id_local;

#define bport_self()	bport_id_local


#if defined(__STDC__)
extern bmsg_ret_t bport_familiar(bport_id_t port_id,
				 bmsg_bool_t * familiar);
extern bmsg_ret_t bport_port(bport_id_t port_id,
			     bport_t * port);
extern bmsg_ret_t bport_open(bport_t * port);
extern bmsg_ret_t bport_close(bport_id_t port_id);
extern bmsg_ret_t bport_flush(bport_id_t port_id);
extern bmsg_ret_t bport_block(bport_id_t port_id);
extern bmsg_ret_t bport_unblock(bport_id_t port_id);
extern void bport_ack(bport_id_t port_id,          		/* upcall */
		      bport_primitive_t port_primitive,
		      bmsg_ret_t ret);
extern void bport_notify(bport_id_t port_id,			/* upcall */
		         bport_primitive_t port_primitive);
#else /* __STDC__ */
extern bmsg_ret_t bport_familiar();
extern bmsg_ret_t bport_port();
extern bmsg_ret_t bport_open();
extern bmsg_ret_t bport_close();
extern bmsg_ret_t bport_flush();
extern bmsg_ret_t bport_block();
extern bmsg_ret_t bport_unblock();
extern void bport_ack();					/* upcall */
extern void bport_notify();					/* upcall */
#endif /* __STDC__ */



/* 
** Message Primitives
*/

#if defined(__STDC__)
extern bmsg_ret_t bmsg_alloc(bmsg_size_t size,
			     bmsg_data_t * * data,
			     bmsg_t * msg);
extern bmsg_ret_t bmsg_free(bmsg_t msg);
extern bmsg_size_t bmsg_size(bmsg_t msg);
extern bmsg_data_t * bmsg_data(bmsg_t msg);
extern bmsg_ret_t bmsg_send(bport_id_t port_id,
			    bmsg_t msg,
			    bmsg_size_t size);
extern bmsg_ret_t bmsg_receive(bmsg_t * msg,
			       bmsg_data_t * * data,
                               bmsg_size_t * size,
			       bport_id_t * port_id,
			       bmsg_bool_t * familiar);
extern bmsg_ret_t bmsg_peek(bmsg_t * msg,
			    bmsg_data_t * * data,
                            bmsg_size_t * size,
			    bport_id_t * port_id,
			    bmsg_bool_t * familiar);
extern void bmsg_notify(void);					/* upcall */
#else /* __STDC__ */
extern bmsg_ret_t bmsg_alloc();
extern bmsg_ret_t bmsg_free();
extern bmsg_size_t bmsg_size();
extern bmsg_data_t * bmsg_data();
extern bmsg_ret_t bmsg_send();
extern bmsg_ret_t bmsg_receive();
extern bmsg_ret_t bmsg_peek();
extern void bmsg_notify();					/* upcall */
#endif /* __STDC__ */



/* 
** Memory Primitives
*/

#if defined(__STDC__)
extern void bmem_cpy(bmem_address_t mem_dst_address,
		     bmem_address_t mem_src_address,
		     bmem_size_t mem_data_size);
extern bmsg_ret_t bmem_put(bport_id_t port_id,
			   bmem_id_t * mem_id,
		           bmem_address_t mem_src_address,
			   bmem_address_t mem_dst_address,
			   bmem_size_t mem_data_size);
extern bmsg_ret_t bmem_get(bport_id_t port_id,
			   bmem_id_t * mem_id,
		           bmem_address_t mem_src_address,
			   bmem_address_t mem_dst_address,
			   bmem_size_t mem_data_size);
extern void bmem_ack(bmem_id_t mem_id,       			/* upcall */
		     bmem_primitive_t mem_primitive,
		     bmsg_ret_t ret);
extern void bmem_notify(bport_id_t port_id,                     /* upcall */
                        bmem_primitive_t mem_primitive,
                        bmem_address_t mem_address,
                        bmem_size_t mem_data_size);
#else /* __STDC__ */
extern void bmem_cpy();
extern bmsg_ret_t bmem_put();
extern bmsg_ret_t bmem_get();
extern void bmem_ack();				          	/* upcall */
extern void bmem_notify();                                      /* upcall */
#endif /* __STDC__ */



/* 
** Miscellaneous Primitives
*/

extern int bmsg_initialised;
extern int bmsg_exited;
extern int bmsg_exiting;
extern bdomain_id_t bdomain_id_local;

#define bmsg_ready()	(bmsg_initialised && !bmsg_exited && !bmsg_exiting)
#define bdomain_self()	bdomain_id_local

#if defined(__STDC__)
extern bmsg_ret_t bmsg_version(bmsg_version_t * version);
extern bmsg_ret_t bmsg_init(bport_id_t port_id,
			    bdomain_t * domain,
			    bmsg_option_t option);
extern void bmsg_exit(void);
extern bmsg_ret_t bmsg_set_option(bmsg_optname_t optname,
                                  bmsg_optval_t optval);
extern bmsg_ret_t bmsg_get_option(bmsg_optname_t optname,
                                  bmsg_optval_t * optval);
extern bmsg_ret_t bmsg_trigger(bmsg_option_t option);
extern void bproc_trigger(bport_t * port);		  	/* upcall */
extern void bmsg_warn(bmsg_warn_t msg_warn,		  	/* upcall */
		      bport_id_t culprit);
extern void bmsg_error(bmsg_error_t msg_error,		  	/* upcall */
		       bport_id_t culprit);
extern void bmsg_panic(bmsg_panic_t msg_panic,		  	/* upcall */
		       bport_id_t culprit);
extern char * bmsg_error_string(bmsg_ret_t bret);
extern void bmsg_perror(bmsg_ret_t bret, 
			char * s);
extern bmsg_ret_t bmsg_info(bmsg_info_t * msg_info);
extern bmsg_ret_t bmem_info(bmem_info_t * mem_info);
extern bmsg_ret_t bport_info(bport_id_t port_id,
			     bport_info_t * port_info);
extern bmsg_ret_t bmem_address(bmsg_address_t msg_address,
			       bmem_address_t * mem_address);
extern void bmsg_address(bmem_address_t mem_address,
			 bmsg_address_t * msg_address);
#else /* __STDC__ */
extern bmsg_ret_t bmsg_version();
extern bmsg_ret_t bmsg_init();
extern void bmsg_exit();
extern bmsg_ret_t bmsg_trigger();
extern void bproc_trigger();		  		  	/* upcall */
extern void bmsg_warn();				  	/* upcall */
extern void bmsg_error();		  		  	/* upcall */
extern void bmsg_panic();				  	/* upcall */
extern char * bmsg_error_string();
extern void bmsg_perror();
extern bmsg_ret_t bmsg_info();
extern bmsg_ret_t bmem_info();
extern bmsg_ret_t bport_info();
extern bmsg_ret_t bmem_address();
extern void bmsg_address();
#endif /* __STDC__ */


#endif /* _BMSG_MSG_H_ */
