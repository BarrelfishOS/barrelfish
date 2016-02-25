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
**        File: nsrv_server.c
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv_server.c	1.37 14 Nov 1995"
** Description: Name Server 
***********************************************************************/

#include "machine.h"     /* architecture specific constant definitions */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <signal.h>
#include <malloc.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <rpc/rpc.h>

#include "pds.types.h"
#include "pds.error.h"
#include "pds.mem.h"
#include "pds.mdt.h"
#include "pds.xdr.h"
#include "bmsg.msg.h"
#include "bmsg.xdr.h"
#include "amsg.msg.h"
#include "amsg.xdr.h"
#include "nsrv.h"
#include "nsrv.xdr.h"
#include "nsrv_int.h"


/**********************************************************************
** Some Constants
***********************************************************************/

#define NSRV_SIGNATURE		"h^lX*7$;@cLfgEg#}+G@!"
#define NSRV_SOCK_BACKLOG	32



/**********************************************************************
** Global Variables
***********************************************************************/

static char * nsrv_path = (char *) 0; /* path                            */
static int nsrv_shared = 1;           /* allow shared memory interaction */
static int nsrv_pds = 1;              /* allow PDS based interaction     */
static int nsrv_verbose = 0;	      /* verbose option			 */

static char * datafile = (char *) 0;
static char * msgfile = (char *) 0;

static unsigned nsrv_port_number = 0;
static int nsrv_ctrl_sock = 0;        /* control socket                  */
static bport_t nsrv_port;



/**********************************************************************
** A-Layer Primitives
***********************************************************************/

void
amsg_warn(msg_warn,culprit)
    amsg_warn_t msg_warn;
    aport_id_t culprit;
{
}


void
amsg_error(msg_error,culprit)
    amsg_error_t msg_error;
    aport_id_t culprit;
{
}


void
amsg_panic(msg_panic,culprit)
    amsg_panic_t msg_panic;
    aport_id_t culprit;
{
}


/**********************************************************************
** B-Layer Primitives
***********************************************************************/

void
bport_ack(port_id, port_primitive, ret)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
    bmsg_ret_t ret;
{
}


void
bport_notify(port_id,port_primitive)
    bport_id_t port_id;
    bport_primitive_t port_primitive;
{
}


void
bmem_ack(mem_id,mem_primitive,ret)
    bmem_id_t mem_id;
    bmem_primitive_t mem_primitive;
    bmsg_ret_t ret;
{
}


void
bmem_notify(port_id,mem_primitive,mem_address,mem_data_size)
    bport_id_t port_id;
    bmem_primitive_t mem_primitive;
    bmem_address_t mem_address;
    bmem_size_t mem_data_size;
{
}


void
bmsg_warn(msg_warn,culprit)
    bmsg_warn_t msg_warn;
    bport_id_t culprit;
{
}


void
bmsg_error(msg_error,culprit)
    bmsg_error_t msg_error;
    bport_id_t culprit;
{
}


void
bmsg_panic(msg_panic,culprit)
    bmsg_panic_t msg_panic;
    bport_id_t culprit;
{
}


void
bproc_trigger(port)
    bport_t * port;
{
#if defined(SIGIO)
    kill((int) port->bpid,SIGIO);
#else
    nsrv_assert_always();
#endif
}


/**********************************************************************
** Primitives
***********************************************************************/

static void
nsrv_warn(culprit)
    char * culprit;
{
    fprintf(stderr,"nsrv: warning : %s !!! \n", culprit);
}


static void
nsrv_pds_exit()
{
    if (amsg_ready())
	amsg_exit();
    if (bmsg_ready())
	bmsg_exit();
}


static void
nsrv_panic(culprit)
    char * culprit;
{
    fprintf(stderr,"nsrv: panic : %s !!! \n", culprit);
    nsrv_pds_exit();
    nsrv_exit_i();
    exit(-1);
}


static void
nsrv_send_reply(reply_port_id,msg,msg_type)
    aport_id_t reply_port_id;
    amsg_t msg;
    amsg_type_t msg_type;
{
    amsg_ret_t aret;

    do {
        aret = amsg_send(reply_port_id,msg,msg_type,
			 (amsg_count_t) 1,(amsg_option_t) 0);
    } while (aret == AMSG_PNOTAVAILABLE);
    switch (aret) {
	case AMSG_OK :
	    break;
	case AMSG_NOPORT :
	case AMSG_PDYING :
	case AMSG_NOSENDRIGHTS :
	    aret = amsg_free(msg);
	    nsrv_assert(aret == AMSG_OK);
	    break;
	case AMSG_PBLOCKED :
	    /*
	    ** Currently we throw the reply messag away. Should be changed
	    ** such that they are remembered so that they can be sent at
	    ** a later time, i.e. when the port is unblocked. 
	    */
    	    fprintf(stderr,"nsrv: Losing reply !!! \n");
	    aret = amsg_free(msg);
	    nsrv_assert(aret == AMSG_OK);
	    break;
	case AMSG_NORESOURCES :
	case AMSG_NOMEMORY :
	    nsrv_panic("Not enough memory");
	    break;
	default :
	    nsrv_panic("AMSG system error");
	    break;
    }
}


static void
nsrv_send_simple_reply(reply_port_id,ret)
    aport_id_t reply_port_id;
    nsrv_ret_t ret;
{
    amsg_ret_t aret;
    amsg_t msg;
    simple_reply_t * reply;

    aret = amsg_alloc((amsg_size_t) sizeof(simple_reply_t),
		      (amsg_data_t * *) &reply,
		      &msg);
    if (aret != AMSG_OK)
	nsrv_panic("Not enough memory");
    reply->ret = ret;
    nsrv_send_reply(reply_port_id,msg,MDT_SIMPLE_REPLY);
}


static void
nsrv_aport_register_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    aport_register_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_APORT_REGISTER_REQUEST);
	nsrv_assert(msg_count == 1);
	nret = nsrv_aport_register_i(request->key,
				     request->name,
				     request->signature,
				     &request->port);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_deregister_s(port_id,deregister_i)
    aport_id_t port_id;
    nsrv_ret_t (* deregister_i) ();
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    deregister_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_DEREGISTER_REQUEST);
	nsrv_assert(msg_count == 1);
	nret = deregister_i(request->key,
			    request->name,
			    request->signature);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_aport_deregister_s(port_id)
    aport_id_t port_id;
{
    nsrv_deregister_s(port_id,nsrv_aport_deregister_i);
}


static void
nsrv_aport_look_up_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_t reply_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    look_up_request_t * request;
    aport_reply_t * reply;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_LOOK_UP_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(aport_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	nret = nsrv_aport_look_up_i(request->key,
				    request->name,
				    &reply->port);
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_APORT_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_bport_register_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    bport_register_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BPORT_REGISTER_REQUEST);
	nsrv_assert(msg_count == 1);

	nret = nsrv_bport_register_i(request->key,
				     request->name,
				     request->signature,
				     &request->port);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_bport_deregister_s(port_id)
    aport_id_t port_id;
{
    nsrv_deregister_s(port_id,nsrv_bport_deregister_i);
}


static void
nsrv_bport_look_up_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_t reply_msg;
    look_up_request_t * request;
    bport_reply_t * reply;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_LOOK_UP_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(bport_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	nret = nsrv_bport_look_up_i(request->key,
				    request->name,
				    &reply->port);
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_BPORT_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_bdomain_register_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    bdomain_register_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BDOMAIN_REGISTER_REQUEST);
	nsrv_assert(msg_count == 1);
	nret = nsrv_bdomain_register_i(request->key,
				       request->name,
				       request->signature,
				       &request->domain);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_bdomain_deregister_s(port_id)
    aport_id_t port_id;
{
    nsrv_deregister_s(port_id,nsrv_bdomain_deregister_i);
}


static void
nsrv_bdomain_look_up_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_t reply_msg;
    look_up_request_t * request;
    bdomain_reply_t * reply;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_LOOK_UP_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(bdomain_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	nret = nsrv_bdomain_look_up_i(request->key,
				      request->name,
				      &reply->domain);
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_BDOMAIN_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_new_bport_id_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_t reply_msg;
    bport_id_request_t * request;
    bport_id_reply_t * reply;
    bport_id_t bport_id;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BPORTID_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(bport_id_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	bport_id = reply->port_id;
	nret = nsrv_new_bport_id_i(request->signature,&bport_id);
	reply->port_id = bport_id;
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_BPORTID_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_free_bport_id_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    bport_id_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BPORTID_REQUEST);
	nsrv_assert(msg_count == 1);
	nret = nsrv_free_bport_id_i(request->signature,
				    request->port_id);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_new_bdomain_id_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_t reply_msg;
    bdomain_id_request_t * request;
    bdomain_id_reply_t * reply;
    bdomain_id_t domain_id;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BDOMAINID_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(bdomain_id_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	domain_id = reply->domain_id;
	nret = nsrv_new_bdomain_id_i(request->signature,&domain_id);
	reply->domain_id = domain_id;
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_BDOMAINID_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_free_bdomain_id_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    bdomain_id_request_t * request;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_BDOMAINID_REQUEST);
	nsrv_assert(msg_count == 1);
	nret = nsrv_free_bdomain_id_i(request->signature,
				      request->domain_id);
	nsrv_send_simple_reply(request->reply_port_id,nret);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_version_s(port_id)
    aport_id_t port_id;
{
    amsg_ret_t aret;
    nsrv_ret_t nret;
    amsg_t request_msg;
    amsg_type_t msg_type;
    amsg_count_t msg_count;
    amsg_t reply_msg;
    version_request_t * request;
    version_reply_t * reply;

    while ((aret = amsg_receive(port_id,
                                &request_msg,
                                (amsg_data_t * *) &request,
                                &msg_type,
                                &msg_count,
                                (amsg_option_t) 0)) == AMSG_OK) {
	nsrv_assert(msg_type == MDT_VERSION_REQUEST);
	nsrv_assert(msg_count == 1);
	aret = amsg_alloc((amsg_size_t) sizeof(version_reply_t),
                          (amsg_data_t * *) &reply,
			  &reply_msg);
	if (aret != AMSG_OK)
	    nsrv_panic("Not enough memory");
	nret = nsrv_version_i(&reply->version);
	reply->ret = nret;
	nsrv_send_reply(request->reply_port_id,reply_msg,MDT_VERSION_REPLY);
	aret = amsg_free(request_msg);
	nsrv_assert(aret == AMSG_OK);
    }
    nsrv_assert(aret == AMSG_NOMESSAGE);
}


static void
nsrv_exit_s()
{
    if (datafile && unlink(datafile)) {
	fprintf(stderr,"nsrv: Lost track of file %s.\n",datafile);
	fprintf(stderr,"If still existing, please remove it !\n");
    }

    fprintf(stderr, "nsrv: Name Server Died !\n");
    nsrv_pds_exit();
    exit(-1);
}


static void
handle_signal(sig) 
    int sig;
{
    sigset_t sigset;

    switch (sig) {
#if defined(SIGUSR1)
        case SIGUSR1 :
#endif
#if defined(SIGUSR2)
        case SIGUSR2 :
#endif
#if defined(SIGIO)
        case SIGIO :
#if (defined(SIGPOLL) && (SIGIO != SIGPOLL))
        case SIGPOLL :
#endif
#else
#if defined(SIGPOLL)
        case SIGPOLL :
#endif
#endif
#if defined(SIGPIPE)
        case SIGPIPE :
#endif
#if defined(SIGURG)
        case SIGURG :
#endif
            /* unblock signal */
            sigemptyset(&sigset);
            sigaddset(&sigset,sig);
            sigprocmask(SIG_UNBLOCK,&sigset,(sigset_t *) 0);
	    if ( 0 ||
#if defined(SIGUSR1)
	        (sig == SIGUSR1) || 
#endif
#if defined(SIGUSR2)
	 	(sig == SIGUSR2) ||
#endif
		 0
				)
               (void) bmsg_trigger(BMSG_INTRA_DOMAIN);
	    else
               (void) bmsg_trigger(BMSG_INTRA_DOMAIN | BMSG_INTER_DOMAIN);
            break;
#ifdef SIGINT
	case SIGINT :
	    nsrv_exit_s();
            break;
#endif /* SIGINT */
#ifdef SIGTERM
	case SIGTERM :
	    nsrv_exit_s();
            break;
#endif /* SIGTERM */
#ifdef SIGILL
	case SIGILL :
    	    fprintf(stderr, "nsrv: Illegal instruction !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGILL */
#ifdef SIGFPE
	case SIGFPE :
    	    fprintf(stderr, "nsrv: Arithmetic exception !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGFPE */
#ifdef SIGSEGV
	case SIGSEGV :
    	    fprintf(stderr, "nsrv: Segmentation violation !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGSEGV */
#ifdef SIGBUS
	case SIGBUS :
    	    fprintf(stderr, "nsrv: Bus error !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGBUS */
#ifdef SIGXCPU
	case SIGXCPU :
    	    fprintf(stderr, "nsrv: CPU time limit exceeded !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGXCPU */
#ifdef SIGXFSZ
	case SIGXFSZ :
    	    fprintf(stderr, "nsrv: File size limit exceeded !\n");
	    nsrv_exit_s();
            break;
#endif /* SIGXFSZ */
#ifdef SIGWINCH
	case SIGWINCH :
            break;
#endif /* SIGWINCH */
        default :
    	    fprintf(stderr, "nsrv: Catched unexpected signal %d !\n", sig);
	    nsrv_exit_s();
            break;
    }
}


static void
signal_handler(sig)
    int sig;
{
    if ((InterruptsDisabled) && (0 ||
#if defined(SIGIO)
				 (sig == SIGIO) || 
#if (defined(SIGPOLL) && (SIGIO != SIGPOLL))
				 (sig == SIGPOLL) || 
#endif
#else
#if defined(SIGPOLL)
				 (sig == SIGPOLL) ||
#endif
#endif
#if defined(SIGPIPE)
				 (sig == SIGPIPE) || 
#endif
#if defined(SIGURG)
				 (sig == SIGURG) || 
#endif
#if defined(SIGUSR1)
				 (sig == SIGUSR1) || 
#endif
#if defined(SIGUSR2)
				 (sig == SIGUSR2) ||
#endif
				 0
						 )) {
        Set_Interrupts_Pending();
    }
    else {
        handle_signal(sig);
    }
}


static void
delayed_signal_handler()
{
    int errno_main;

    /* save errno */
    errno_main = errno;

    Clr_Interrupts_Pending();
#if defined(SIGIO)
    handle_signal(SIGIO);
#else
    nsrv_assert_always();
#endif

    /* restore errno */
    errno = errno_main;
}


static void
install_sighandler(sig)
    int sig;
{
    struct sigaction act;

    act.sa_handler = signal_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;

    (void) sigaction(sig,
                     &act,
                     (struct sigaction *) 0);
}


static void
install_sighandlers() 
{
    int sig;

    for (sig=0; sig<NSIG; sig++) {
	if (1 
#if defined(SIGTSTP)
	    && (sig != SIGTSTP) 
#endif
#if defined(SIGCONT)
	    && (sig != SIGCONT)
#endif
#if defined(SIGCHLD)
	    && (sig != SIGCHLD)
#endif
			       )
	    install_sighandler(sig);
    }
}


static void 
usage(program_name)
    char *program_name;
{
    fprintf(stderr,
            "\nUsage: %s [-v] [-s port_number] [-p path] [-n[shm]] [-npds] [-h]\n",
            program_name);

    fprintf(stderr, "       -v: verbose \n");
    fprintf(stderr, "       -s port_number: port number \n");
    fprintf(stderr, "       -p path: path name \n");
    fprintf(stderr, "       -nshm: disable shared memory interaction \n");
    fprintf(stderr, "       -npds: disable PDS interaction \n");
    fprintf(stderr, "       -h: help \n");
    fprintf(stderr, "\n");
    exit(0);
}


static void 
args(argc, argv)
    int argc;
    char *argv[];
{
    int i;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-')
            switch (argv[i][1]) {
		case 'h': /* help */
                    usage(argv[0]); /* exit with usage message */
                    break;
		case 'v': /* verbose */
		    nsrv_verbose = 1;
		    break;
		case 'n': 
		    if ((strcmp(argv[i],"-n") == 0) ||
		        (strcmp(argv[i],"-nshm") == 0)) {
			/* disable shared memory interaction */
			nsrv_shared = 0;
		    }
		    else if (strcmp(argv[i],"-npds") == 0) {
			/* disable PDS interaction */
			nsrv_pds = 0;
		    }
		    else
                        usage(argv[0]); /* exit with usage message */
                    break;
		case 'p': /* path */
		    if (++i >= argc) {
                        fprintf(stderr,
                                "\npath parameter expected !\n");
                        usage(argv[0]);
			break;
                    }
		    else 
		        nsrv_path = argv[i];
		    break;
		case 's': /* port number */
		    if (++i >= argc) {
                        fprintf(stderr,
                                "\nport number parameter expected !\n");
                        usage(argv[0]);
			break;
                    }
		    else {
			int port_number;
		        port_number = atoi(argv[i]);
			if ((port_number < 1024) || 
			    (port_number > 0xffff)) {
                            fprintf(stderr,
                                    "\n Invalid port number !\n");
                            usage(argv[0]);
			    break;
			}
			else 
			    nsrv_port_number = port_number;
		    }
		    break;
		default :
		    usage(argv[0]); /* exit with usage message */
		    break;
	}
    }
}


static int
nsrv_path_ok(hostname)
    char * hostname;
{
    char filename[NSRV_FILENAMELEN_MAX+1];
    int fd;

    if ((int)
	(strlen(nsrv_path) + 1 + 
	 strlen(NSRV_DATAFILE) + 1 +
	 strlen(hostname) + 1 + 6) > NSRV_FILENAMELEN_MAX) { 
	fprintf(stderr, 
		"nsrv: Implementation limit reached at \"%s:%d\" !\n",
		__FILE__, __LINE__ );
        fprintf(stderr, 
		"      Consult an expert !\n");
	exit(-1);
    }

    (void) sprintf(filename, "%s/%s.%s.%d",
		   nsrv_path,NSRV_DATAFILE,hostname,getpid() % 1000000 );

    fd = open(filename,O_RDWR|O_CREAT,0700);
    if (fd == -1)
	return(0);

    (void) unlink(filename);
    return(~0);
}


static void
nsrv_bmsg_init(nsrv_port)
    bport_t * nsrv_port;
{
    bdomain_t nsrv_domain;
    bmsg_ret_t bret;
    nsrv_ret_t nret;
    int ret;

    /* initialise b-layer of message passing system */
    nsrv_domain.bdomain_id = NSRV_BDOMAIN_ID;
    strcpy(nsrv_domain.bdomain_file,msgfile);
    if (!pds_mem_base())
    	nsrv_domain.bdomain_start = (bmem_address_t) 0;
    else
    	nsrv_domain.bdomain_start = (bmem_address_t) 
				    (nsrv_data_start + NSRV_DATA_AREA_SIZE);
    nsrv_domain.bdomain_size = NSRV_MSG_AREA_SIZE;
    bret = bmsg_init(NSRV_BPORT_ID,&nsrv_domain, BDOMAIN_CREATE);
    if (bret == BMSG_OK) {
        if (bport_port(NSRV_BPORT_ID,nsrv_port) != BMSG_OK) {
    	    fprintf(stderr,"nsrv: panic : bport_port() !!! \n");
            bmsg_exit();
            nsrv_exit_i();
            exit(-1);
	}
        /* adjust access restrictions of msgfile */
        ret = chmod(msgfile,
                    S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
        if (ret) {
    	    fprintf(stderr,"nsrv: panic : chmod() !!! \n");
            bmsg_exit();
            nsrv_exit_i();
            exit(-1);
        }
    }
    else {
    	fprintf(stderr,"nsrv: panic : bmsg_init() !!! \n");
        bmsg_exit();
        nsrv_exit_i();
        exit(-1);
    }
    nsrv_msg_start = (char *) nsrv_domain.bdomain_start;

    nret = nsrv_bdomain_register_i(NSRV_KEY,NSRV_DOMAIN_NAME,NSRV_SIGNATURE,
				   &nsrv_domain);
    if (nret == NSRV_OK)
	nret = nsrv_bport_register_i(NSRV_KEY,NSRV_PORT_NAME,NSRV_SIGNATURE,
				     nsrv_port);
    if (nret != NSRV_OK) {
    	fprintf(stderr,"nsrv: panic : nsrv_bport_register_i() !!! \n");
	bmsg_exit();
	nsrv_exit_i();
	exit(-1);
    }
}


static void
nsrv_amsg_init()
{
    amsg_ret_t aret;
    void (* nsrv_notify [NSRV_NOF_APORTS]) ();
    aport_id_t nsrv_aport_id[NSRV_NOF_APORTS];

    /* install amsg notification procedures */
    nsrv_notify[NSRV_APORT_REGISTER] = nsrv_aport_register_s;
    nsrv_notify[NSRV_APORT_DEREGISTER] = nsrv_aport_deregister_s;
    nsrv_notify[NSRV_APORT_LOOK_UP] = nsrv_aport_look_up_s;
    nsrv_notify[NSRV_BPORT_REGISTER] = nsrv_bport_register_s;
    nsrv_notify[NSRV_BPORT_DEREGISTER] = nsrv_bport_deregister_s;
    nsrv_notify[NSRV_BPORT_LOOK_UP] = nsrv_bport_look_up_s;
    nsrv_notify[NSRV_BDOMAIN_REGISTER] = nsrv_bdomain_register_s;
    nsrv_notify[NSRV_BDOMAIN_DEREGISTER] = nsrv_bdomain_deregister_s;
    nsrv_notify[NSRV_BDOMAIN_LOOK_UP] = nsrv_bdomain_look_up_s;
    nsrv_notify[NSRV_NEW_BPORT_ID] = nsrv_new_bport_id_s;
    nsrv_notify[NSRV_FREE_BPORT_ID] = nsrv_free_bport_id_s;
    nsrv_notify[NSRV_NEW_BDOMAIN_ID] = nsrv_new_bdomain_id_s;
    nsrv_notify[NSRV_FREE_BDOMAIN_ID] = nsrv_free_bdomain_id_s;
    nsrv_notify[NSRV_VERSION] = nsrv_version_s;

    /* initialise a-layer of message passing system */
    aret = amsg_init(NSRV_NOF_APORTS,nsrv_notify,nsrv_aport_id,0);

    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_APORT_REGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_APORT_DEREGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_BPORT_REGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_BPORT_DEREGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_BDOMAIN_REGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_BDOMAIN_DEREGISTER], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_NEW_BPORT_ID], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_FREE_BPORT_ID], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_NEW_BDOMAIN_ID], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_FREE_BDOMAIN_ID], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);
    if (aret == AMSG_OK) 
        aret = aport_set_option(nsrv_aport_id[NSRV_VERSION], 
			        APORT_NOTIFY_LEVEL,(aport_optval_t) 1);

    if (aret != AMSG_OK) {
        bmsg_exit();
        nsrv_exit_i();
        exit(-1);
    }
}


static void
nsrv_server_loop()
{
    bport_id_t bport_id;
    bdomain_id_t bdomain_id;
    bdomain_t bdomain;
    bport_t bport;
    aport_t aport;
    nsrv_name_t signature;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_version_t version;
    nsrv_mode_t mode;
    nsrv_ret_t nret;
    char * nsrv_signature;
    char * nsrv_key;
    char * nsrv_name;

    sigset_t sigset_old;
    sigset_t sigset;

    int nsrv_msg_sock; 
    fd_set fdset;
    long fdset_size;
    XDR xdrs;
    int ret;

    pds_uint32 reply_size;
    pds_uint32 request_size;
    pds_uint32 nsrv_request[NSRV_BUF_SIZE];
    pds_uint32 nsrv_reply[NSRV_BUF_SIZE];
    nsrv_number_t nsrv_request_number;
    int errno_accept;

    fdset_size = sysconf(_SC_OPEN_MAX);

    nsrv_signature = signature;
    nsrv_key = key;
    nsrv_name = name;

    /* disable delivery of PDS related signals */
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
    Disable_Int();

    do {

	do {

	    /*
	    ** We do not allow PDS based nsrv request to
	    ** interrupt the handling of a socket based
	    ** request. This is done mainly because on 
	    ** some platforms accept() and possibly some
	    ** other system calls and socket related 
	    ** primitives are not fully interrupt safe.
	    */

            /* reenable PDS related signal delivery */
	    Enable_Int();
            sigprocmask(SIG_SETMASK,&sigset_old,(sigset_t *) 0);

            do {
                FD_ZERO(&fdset);
                FD_SET(nsrv_ctrl_sock,&fdset);

                ret = select(fdset_size,
                             &fdset,
                             (fd_set *) 0,
                             (fd_set *) 0,
                             (struct timeval *) 0);
            } while ((!ret) || ((ret == -1) && 
			        ((errno == EINTR) || (errno == EAGAIN))));
	    if (ret == -1)
                nsrv_perror_and_assert_always("select()");
            nsrv_assert(ret == 1);

            /* disable PDS related signal delivery */
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
	    Disable_Int();

	    nsrv_msg_sock = accept(nsrv_ctrl_sock,
                                   (struct sockaddr *) 0,
                                   (int *) 0);
	    errno_accept = errno;

	    if (nsrv_msg_sock == -1) { /* accept() failed */
	        switch (errno_accept) {
                    case ENOMEM :
            	        nsrv_panic("Not enough memory");
		        break;
                    case ENOSR :
                    case ENOBUFS :
                    case EMFILE :
                    case ENFILE :
	    	        nsrv_panic("Not enough resources");
		        break;
                    case EINTR :
                        break;
                    default :
        		nsrv_perror_and_assert_always("accept()");
                        break;
	        }
	    }
	}
	while ((nsrv_msg_sock == -1) && (errno_accept == EINTR));

	nsrv_sock_linger(nsrv_msg_sock);
	nsrv_sock_nodelay(nsrv_msg_sock);

	nret = nsrv_receive_sock(nsrv_msg_sock,(char *) nsrv_request,&request_size);
	if (nret != NSRV_OK) {
	    nsrv_close_sock(nsrv_msg_sock);
	    if (nret != NSRV_WARN)
	        nsrv_warn("Lost request"); 
	    continue;
	}

	xdrmem_create(&xdrs,(const caddr_t) nsrv_request,
                      (const u_int) request_size,
                      XDR_DECODE); 

	/* unpack request number */
	if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number))) {
	    xdr_destroy(&xdrs);
	    nsrv_close_sock(nsrv_msg_sock);
	    nsrv_warn("Interface violation"); 
	    continue;
	}

	switch (nsrv_request_number) {

	    case NSRV_APORT_REGISTER :
	    case NSRV_BPORT_REGISTER :
	    case NSRV_BDOMAIN_REGISTER :
	    case NSRV_APORT_DEREGISTER :
	    case NSRV_BPORT_DEREGISTER :
	    case NSRV_BDOMAIN_DEREGISTER :

		/* unpack key, name and signature */
		if (!(xdr_nsrv_name(&xdrs,&nsrv_key) &&
                     xdr_nsrv_name(&xdrs,&nsrv_name) &&
                     xdr_nsrv_name(&xdrs,&nsrv_signature))) {
		    nret = NSRV_XDR_DECODE;
		    break;
		}

		/* unpack data to be registered */
		switch (nsrv_request_number) {
	    	    case NSRV_APORT_REGISTER :
		        if (!(xdr_aport(&xdrs,&aport)))
			    nret = NSRV_XDR_DECODE;
			break;
	    	    case NSRV_BPORT_REGISTER :
		        if (!(xdr_bport(&xdrs,&bport)))
			    nret = NSRV_XDR_DECODE;
			break;
	    	    case NSRV_BDOMAIN_REGISTER :
		        if (!(xdr_bdomain(&xdrs,&bdomain)))
			    nret = NSRV_XDR_DECODE;
			break;
		    default :
			break;
		}
		if (nret == NSRV_XDR_DECODE)
		    break;

		/* process request */
		switch (nsrv_request_number) {
	    	    case NSRV_APORT_REGISTER :
		        nret = nsrv_aport_register_i(key,name,signature,&aport);
			break;
	    	    case NSRV_BPORT_REGISTER :
		    	nret = nsrv_bport_register_i(key,name,signature,&bport);
			break;
	    	    case NSRV_BDOMAIN_REGISTER :
			nret = nsrv_bdomain_register_i(key,name,signature,&bdomain);
			break;
	    	    case NSRV_APORT_DEREGISTER :
			nret = nsrv_aport_deregister_i(key,name,signature);
			break;
	    	    case NSRV_BPORT_DEREGISTER :
			nret = nsrv_bport_deregister_i(key,name,signature);
			break;
	    	    case NSRV_BDOMAIN_DEREGISTER :
			nret = nsrv_bdomain_deregister_i(key,name,signature);
			break;
		    default :
			break;
		}

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret)))
		    nret = NSRV_XDR_ENCODE;

		break;

	    case NSRV_APORT_LOOK_UP :
	    case NSRV_BPORT_LOOK_UP :
    	    case NSRV_BDOMAIN_LOOK_UP :

		/* unpack key and name */
		if (!(xdr_nsrv_name(&xdrs,&nsrv_key) &&
                     xdr_nsrv_name(&xdrs,&nsrv_name))) {
		    nret = NSRV_XDR_DECODE;
		    break;
		}

		/* process request */
		switch (nsrv_request_number) {
	    	    case NSRV_APORT_LOOK_UP :
			nret = nsrv_aport_look_up_i(key,name,&aport);
			break;
	    	    case NSRV_BPORT_LOOK_UP :
			nret = nsrv_bport_look_up_i(key,name,&bport);
			break;
    	    	    case NSRV_BDOMAIN_LOOK_UP :
			nret = nsrv_bdomain_look_up_i(key,name,&bdomain);
			break;
		    default :
			break;
		}

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret))) {
		    nret = NSRV_XDR_ENCODE;
		    break;
		}

		/* pack data that has been looked up */
		if (nret != NSRV_OK)
		    break;
		switch (nsrv_request_number) {
	    	    case NSRV_APORT_LOOK_UP :
			if (!(xdr_aport(&xdrs,&aport)))
			    nret = NSRV_XDR_ENCODE;
			break;
	    	    case NSRV_BPORT_LOOK_UP :
			if (!(xdr_bport(&xdrs,&bport)))
			    nret = NSRV_XDR_ENCODE;
			break;
    	    	    case NSRV_BDOMAIN_LOOK_UP :
			if (!(xdr_bdomain(&xdrs,&bdomain)))
			    nret = NSRV_XDR_ENCODE;
			break;
		    default :
			break;
		}

		break;

	    case NSRV_NEW_BPORT_ID :
	    case NSRV_FREE_BPORT_ID :
	    case NSRV_NEW_BDOMAIN_ID :
	    case NSRV_FREE_BDOMAIN_ID :

		/* unpack signature */
		if (!(xdr_nsrv_name(&xdrs,&nsrv_signature))) {
		    nret = NSRV_XDR_DECODE;
		    break;
		}

		/* unpack data */
		switch (nsrv_request_number) {
	    	    case NSRV_NEW_BPORT_ID :
		    case NSRV_FREE_BPORT_ID :
		        if (!(xdr_bport_id(&xdrs,&bport_id)))
			    nret = NSRV_XDR_DECODE;
			break;
	    	    case NSRV_NEW_BDOMAIN_ID :
	    	    case NSRV_FREE_BDOMAIN_ID :
		        if (!(xdr_bdomain_id(&xdrs,&bdomain_id)))
			    nret = NSRV_XDR_DECODE;
			break;
		    default :
			break;
		}
		if (nret == NSRV_XDR_DECODE)
		    break;

		/* process request */
		switch (nsrv_request_number) {
	    	    case NSRV_NEW_BPORT_ID :
			nret = nsrv_new_bport_id_i(signature,&bport_id);
			break;
	    	    case NSRV_FREE_BPORT_ID :
			nret = nsrv_free_bport_id_i(signature,bport_id);
			break;
	    	    case NSRV_NEW_BDOMAIN_ID :
			nret = nsrv_new_bdomain_id_i(signature,&bdomain_id);
			break;
	    	    case NSRV_FREE_BDOMAIN_ID :
			nret = nsrv_free_bdomain_id_i(signature,bdomain_id);
			break;
		    default :
			break;
		}

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret))) {
		    nret = NSRV_XDR_ENCODE;
		    break;
		}

		/* pack identifier */
		if (nret != NSRV_OK)
		    break;
		switch (nsrv_request_number) {
	    	    case NSRV_NEW_BPORT_ID :
			if (!(xdr_bport_id(&xdrs,&bport_id)))
			    nret = NSRV_XDR_ENCODE;
			break;
	    	    case NSRV_NEW_BDOMAIN_ID :
			if (!(xdr_bdomain_id(&xdrs,&bdomain_id)))
			    nret = NSRV_XDR_ENCODE;
			break;
		    default :
			break;
		}

		break;

	    case NSRV_VERSION :
		
		nret = nsrv_version_i(&version);

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret))) {
		    nret = NSRV_XDR_ENCODE;
		    break;
		}

		/* pack version */
		if (nret != NSRV_OK)
		    break;
		if (!(xdr_nsrv_version(&xdrs,&version)))
		    nret = NSRV_XDR_ENCODE;

		break;

	    case NSRV_PING :

		nret = NSRV_OK;

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret)))
		    nret = NSRV_XDR_ENCODE;

		break;

	    case NSRV_GET_MODE :

		nret = NSRV_OK;

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret)))
		    nret = NSRV_XDR_ENCODE;

		/* pack mode */
		if (nret != NSRV_OK)
		    break;
		mode = 0;
		if (nsrv_shared)
		    mode = NSRV_SHM;
		if (nsrv_pds)
		    mode |= NSRV_PDS;
		if (!(xdr_nsrv_mode(&xdrs,&mode)))
		    nret = NSRV_XDR_ENCODE;

		break;

	    case NSRV_GET_PATH :

		nret = NSRV_OK;

		/* pack nret */
		xdrmem_create(&xdrs,(const caddr_t) nsrv_reply,
                      	      (const u_int) 4*NSRV_BUF_SIZE,
                      	      XDR_ENCODE); 
		if (!(xdr_nsrv_ret(&xdrs,&nret)))
		    nret = NSRV_XDR_ENCODE;

		/* pack path */
		if (nret != NSRV_OK)
		    break;
		if (!(xdr_string(&xdrs,&nsrv_path,NSRV_FILENAMELEN_MAX)))
		    nret = NSRV_XDR_ENCODE;

		break;

	    default :
		nret = NSRV_XDR_DECODE;
		break;
	}

       	/* send reply */
	if ((nret != NSRV_XDR_DECODE) && (nret != NSRV_XDR_ENCODE)) {
	    reply_size = (pds_uint32) xdr_getpos(&xdrs);
	    nret = nsrv_send_sock(nsrv_msg_sock,(char *) nsrv_reply,
				  reply_size);
	    if (nret != NSRV_OK)
	        nsrv_warn("Lost reply"); 
	}

	xdr_destroy(&xdrs);
        nsrv_close_sock(nsrv_msg_sock);

	if (nret == NSRV_XDR_DECODE)
	    nsrv_warn("Interface violation"); 
	else if (nret == NSRV_XDR_ENCODE)
	    nsrv_panic("XDR encoding error"); 

    } while (1);
}


int
main(argc, argv)
    int argc;
    char *argv[];
{
    struct sockaddr_in ctrl_sock_name;
    char hostname[MAXHOSTNAMELEN+1];
    unsigned property;

    int on = 1;
    int ret;

    /* disable interrupts */
    pds_disable_int();

    /* install signal handlers */
    install_sighandlers();
    irq_lock_init(delayed_signal_handler);

    /* get arguments */
    args(argc, argv);

    /* get hostname */
    if (gethostname(hostname,MAXHOSTNAMELEN) != 0)
	nsrv_perror_and_assert_always("gethostname()");

    /* ping existing name server */
    if (nsrv_ping(hostname,&nsrv_port_number) == NSRV_OK) {
        fprintf(stderr, "nsrv: Name server already up and running !\n");
	exit(0);
    }

    /* initialise nsrv_port_number */
    nsrv_init_port_number(&nsrv_port_number,&property);

    /* initialise nsrv_path */
    if (!nsrv_path) /* use environment variable NSRV_PATH */
        nsrv_path = getenv(NSRV_PATH);

    if (!nsrv_path) { /* try current working directory */
	nsrv_path = getcwd((char *) 0, NSRV_FILENAMELEN_MAX+2);
	if (!nsrv_path || !nsrv_path_ok(hostname)) { /* try /tmp */
	    nsrv_path = (char *) malloc(strlen("/tmp")+1);
	    if (!nsrv_path) {
                fprintf(stderr, "nsrv: Not enough memory !\n");
	        exit(-1);
	    }
	    strcpy(nsrv_path,"/tmp");
	}
    }

    if (!nsrv_path_ok(hostname)) {
	fprintf(stderr, "nsrv: Problems accessing %s ! \n",
	        nsrv_path);
	fprintf(stderr, 
		"      Select another path by, for example, (re)defining\n");
	fprintf(stderr, 
		"      the environment variable %s.\n", NSRV_PATH);
	exit(-1);
    }

    /* create control socket */
    do {
        nsrv_ctrl_sock = socket(PF_INET,SOCK_STREAM,0);
    }
    while ((nsrv_ctrl_sock == -1) && (errno == EINTR));
    if (nsrv_ctrl_sock == -1) {
        switch (errno) {
            case EMFILE :
            case ENFILE :
            case ENOBUFS :
        	fprintf(stderr, "nsrv: Not enough resources !\n");
		exit(-1);
            default :
		nsrv_perror_and_assert_always("socket()");
        }
    }
    /* disable the default delay of TCP data transmission */
    if (setsockopt(nsrv_ctrl_sock,IPPROTO_TCP,TCP_NODELAY,
		   (char *) &on,sizeof(on)) == -1)
	nsrv_perror_and_assert_always("setsockopt()");

    /* name control socket */
    ctrl_sock_name.sin_family = AF_INET;
    ctrl_sock_name.sin_addr.s_addr = htonl(INADDR_ANY);
    do {
        ctrl_sock_name.sin_port = htons((u_short) nsrv_port_number);
        if (bind(nsrv_ctrl_sock,
                 (struct sockaddr *) &ctrl_sock_name,
                 sizeof(ctrl_sock_name)) < 0) {
	    switch (errno) {
	        case EACCES :
	        case EADDRINUSE :
	        case EADDRNOTAVAIL :
		    if ((property == NSRV_DEFAULT) &&
			(nsrv_port_number >= NSRV_PORT_DEFAULT) &&
			(nsrv_port_number < (NSRV_PORT_DEFAULT + NSRV_DEFAULT_PORTS - 1))) {
			nsrv_port_number++;
			break;
		    }
		    fprintf(stderr,
                            "nsrv: Failed to start name server !\n");
		    if (property == NSRV_DEFAULT) {
        	        fprintf(stderr, 
			        "      Default ports not available.\n");
		        fprintf(stderr,
                                "      Select another port by, for example, (re)defining\n");
		        fprintf(stderr,
                                "             the environment variable %s.\n", NSRV_PORT);
		    }
		    else {
        	        fprintf(stderr, 
			        "      Port %d not available.\n", 
			        nsrv_port_number);
		        fprintf(stderr,
                                "      Select another port by, for example, (re)defining\n");
		        fprintf(stderr,
                                "             the environment variable %s.\n", NSRV_PORT);
		    }
		    exit(-1);
	        default :
	            nsrv_perror_and_assert_always("bind()");
	    }
        }
	else
	    break;
    } while (1);

    datafile = malloc(strlen(nsrv_path) + 1 +
		      strlen(NSRV_DATAFILE) + 1 +
		      strlen(hostname) + 1 + 10);
    if (!datafile) {
        fprintf(stderr, "nsrv: Not enough memory !\n");
	exit(-1);
    }
    sprintf(datafile,"%s/%s.%s.%d",
	    nsrv_path,NSRV_DATAFILE,hostname,nsrv_port_number % 1000000);

    msgfile = malloc(strlen(nsrv_path) + 1 +
		     strlen(NSRV_MSGFILE) + 1 +
		     strlen(hostname) + 1 + 10);
    if (!msgfile) {
        fprintf(stderr, "nsrv: Not enough memory !\n");
	exit(-1);
    }
    sprintf(msgfile, "%s/%s.%s.%d",
	    nsrv_path,NSRV_MSGFILE,hostname,nsrv_port_number % 1000000);

    /* check existence of nsrv files */
    if (!access(msgfile,F_OK) || !access(datafile,F_OK)) {
	fprintf(stderr,
                "nsrv: Failed to start name server !\n");
	fprintf(stderr,
                "      Before retrying, remove the following file(s):\n");
        if (!access(msgfile,F_OK))
	    fprintf(stderr, "          %s\n",msgfile);
        if (!access(datafile,F_OK))
	    fprintf(stderr, "          %s\n",datafile);
        exit(-1);
    }

    /* initialise name server for internal use (create datafile) */
    nsrv_init_server_i(datafile,nsrv_verbose);

    /* adjust access restrictions of datafile */
    if (nsrv_shared)
        ret = chmod(datafile,
                    S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
    else
        ret = chmod(datafile,S_IRUSR|S_IWUSR);
    if (ret) {
        fprintf(stderr,"nsrv: panic : chmod() !!! \n");
	nsrv_exit_i();
        exit(-1);
    }

    /* initialise message passing system */
    nsrv_bmsg_init(&nsrv_port);
    nsrv_amsg_init();
    if (nsrv_types_init_i() != NSRV_OK)
	nsrv_panic("Not enough resources");

    /* listen for connections on control socket */
    if (listen(nsrv_ctrl_sock,NSRV_SOCK_BACKLOG) == -1)
	nsrv_perror_and_assert_always("listen()");

    /* enable interrupts */
    pds_enable_int();

    nsrv_server_loop();

    return(0);
}

