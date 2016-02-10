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
**        File: nsrv.h
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv.h	1.14 07 Nov 1995"
** Description: Name Server Interface
***********************************************************************/

#ifndef _NSRV_H_
#define _NSRV_H_

/* 
** Return Codes
*/

#define NSRV_OK              PDS_OK             /* success               */
#define NSRV_NYI             PDS_NYI            /* not yet implemented   */
#define NSRV_WARN            PDS_WARN           /* general warning       */
#define NSRV_ERROR           PDS_ERROR          /* general error         */
#define NSRV_IMPLIM          PDS_IMPLIM         /* implementation limit  */
#define NSRV_INVAL           PDS_INVAL          /* invalid argument      */
#define NSRV_NORESOURCES     PDS_NORESOURCES    /* no resources          */
#define NSRV_NOMEMORY        PDS_NOMEMORY       /* no memory             */

#define NSRV_NOPORT    	     (PDS_RET_MAX + 1)  /* no such port        	 */
#define NSRV_NOT_READY	     (PDS_RET_MAX + 2)  /* not ready for request */
#define NSRV_NOT_YOURS       (PDS_RET_MAX + 3)  /* signature mismatch    */
#define NSRV_NOT_REGISTERED  (PDS_RET_MAX + 4)  /* not registered      	 */
#define NSRV_NODOMAIN        (PDS_RET_MAX + 5)  /* no such domain 	 */
#define NSRV_NOSERVER        (PDS_RET_MAX + 6)  /* no name server 	 */
#define NSRV_NOHOST    	     (PDS_RET_MAX + 7)  /* no such host   	 */
#define NSRV_EVERSION 	     (PDS_RET_MAX + 8)  /* version mismatch 	 */



/* 
** Configurable Parameters
*/

#define NSRV_PORT_DEFAULT       4916       /* number of 1st default port  */
#define NSRV_DEFAULT_PORTS      5          /* number of default ports     */
#define NSRV_DATA_AREA_SIZE     0x00800000 /* size of nsrv data mem area  */
#define NSRV_MSG_AREA_SIZE      0x00800000 /* size of nsrv msg mem area   */
#define NSRV_NAMELEN		64
#define NSRV_FILENAMELEN_MAX	BMSG_FILENAMELEN_MAX



/*
** Environment Variables
*/

#define NSRV_PATH		"NSRV_PATH"
#define NSRV_PORT		"NSRV_PORT"
#define NSRV_HOST		"NSRV_HOST"



/*
** Identifiers
*/

#define NSRV_BDOMAIN_ID 1
#define NSRV_BPORT_ID   1



/*
** File Names 
*/

#define NSRV_DATAFILE	"nsrv.data"
#define NSRV_MSGFILE	"nsrv.msg"



/*
** Key, Domain Name, and Port Name
*/

#define NSRV_KEY                "nsrv_key"
#define NSRV_DOMAIN_NAME        "nsrv_domain_name"
#define NSRV_PORT_NAME          "nsrv_port_name"



/* 
** Type Definitions 
*/

#define	NSRV_INTFC	16

typedef char nsrv_name_t [NSRV_NAMELEN+1];
typedef pds_ret_t nsrv_ret_t;
typedef struct {
    pds_uint32 v_minor;
    pds_uint32 v_major;
} nsrv_version_t;

#define MDT_NSRVRET	MDT_RET

extern msg_type_t MDT_NSRVNAME;
extern msg_type_t MDT_NSRVVERSION;



/*
** Global Variables
*/

extern int nsrv_initialised;
extern int nsrv_exited;
extern int nsrv_exiting;

#define nsrv_ready()    (nsrv_initialised && !nsrv_exited && !nsrv_exiting)



/* 
** Name Server Primitives
*/

#if defined(__STDC__)
extern nsrv_ret_t nsrv_init(char * hostname,
			    unsigned * port_number);
extern nsrv_ret_t nsrv_types_init(void);
extern void nsrv_exit(void);
extern char * nsrv_data_base(void);
extern char * nsrv_msg_base(void);
extern nsrv_ret_t nsrv_aport_register(nsrv_name_t key,
                                      nsrv_name_t port_name,
                                      nsrv_name_t signature,
				      aport_t * port);
extern nsrv_ret_t nsrv_aport_deregister(nsrv_name_t key,
                                        nsrv_name_t port_name,
                                        nsrv_name_t signature);
extern nsrv_ret_t nsrv_aport_look_up(nsrv_name_t key,
                                     nsrv_name_t port_name,
                                     aport_t * port);
extern nsrv_ret_t nsrv_bport_register(nsrv_name_t key,
                                      nsrv_name_t port_name,
                                      nsrv_name_t signature,
				      bport_t * port);
extern nsrv_ret_t nsrv_bport_deregister(nsrv_name_t key,
                                        nsrv_name_t port_name,
                                        nsrv_name_t signature);
extern nsrv_ret_t nsrv_bport_look_up(nsrv_name_t key,
                                     nsrv_name_t port_name,
                                     bport_t * port);
extern nsrv_ret_t nsrv_bdomain_register(nsrv_name_t key,
                                        nsrv_name_t domain_name,
                                        nsrv_name_t signature,
				        bdomain_t * domain);
extern nsrv_ret_t nsrv_bdomain_deregister(nsrv_name_t key,
                                          nsrv_name_t domain_name,
                                          nsrv_name_t signature);
extern nsrv_ret_t nsrv_bdomain_look_up(nsrv_name_t key,
                                       nsrv_name_t port_name,
                                       bdomain_t * domain);
extern nsrv_ret_t nsrv_new_bport_id(nsrv_name_t signature,
				    bport_id_t * port_id);
extern nsrv_ret_t nsrv_free_bport_id(nsrv_name_t signature,
				     bport_id_t port_id);
extern nsrv_ret_t nsrv_new_bdomain_id(nsrv_name_t signature,
				      bdomain_id_t * domain_id);
extern nsrv_ret_t nsrv_free_bdomain_id(nsrv_name_t signature,
				       bdomain_id_t domain_id);
extern nsrv_ret_t nsrv_ping(char * hostname,
			    unsigned * port_number);
extern nsrv_ret_t nsrv_version(nsrv_version_t * version);
extern char * nsrv_error_string(nsrv_ret_t nret);
extern void nsrv_perror(nsrv_ret_t nret,
                        char * s);
#else /* __STDC__ */
extern nsrv_ret_t nsrv_init();
extern nsrv_ret_t nsrv_types_init();
extern void nsrv_exit();
extern char * nsrv_data_base();
extern char * nsrv_msg_base();
extern nsrv_ret_t nsrv_aport_register();
extern nsrv_ret_t nsrv_aport_deregister();
extern nsrv_ret_t nsrv_aport_look_up();
extern nsrv_ret_t nsrv_bport_register();
extern nsrv_ret_t nsrv_bport_deregister();
extern nsrv_ret_t nsrv_bport_look_up();
extern nsrv_ret_t nsrv_bdomain_register();
extern nsrv_ret_t nsrv_bdomain_deregister();
extern nsrv_ret_t nsrv_bdomain_look_up();
extern nsrv_ret_t nsrv_new_bport_id();
extern nsrv_ret_t nsrv_free_bport_id();
extern nsrv_ret_t nsrv_new_bdomain_id();
extern nsrv_ret_t nsrv_free_bdomain_id();
extern nsrv_ret_t nsrv_ping();
extern nsrv_ret_t nsrv_version();
extern char * nsrv_error_string();
extern void nsrv_perror();
#endif /* __STDC__ */


#endif /* _NSRV_H_ */

