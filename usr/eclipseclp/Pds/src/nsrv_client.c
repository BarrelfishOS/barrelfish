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
**        File: nsrv_client.c
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv_client.c	1.31 24 Nov 1995"
** Description: Name Service Client Stubs 
***********************************************************************/

#include "machine.h"     /* architecture specific constant definitions */

#include <sys/types.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <rpc/rpc.h>
#include <fcntl.h>

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
** Global Variables
***********************************************************************/

static int shm_enabled = 0;
static int pds_enabled = 0;
static int stubs_ready = 0;
static int types_ready = 0;

int nsrv_initialised = 0;
static int nsrv_initialising = 0;
int nsrv_exited = 0;
int nsrv_exiting = 0;

static bport_t nsrv_port;
static unsigned nsrv_port_number;
static inet_address_t nsrv_inet_address;



/**********************************************************************
*************************   Local Primitives  *************************
***********************************************************************/

#define AmsgCheck(aret) {						\
	switch (aret) {							\
	    case AMSG_NYI :						\
		return(NSRV_NYI);					\
	    case AMSG_NORESOURCES :					\
		return(NSRV_NORESOURCES);				\
	    case AMSG_NOMEMORY :					\
		return(NSRV_NOMEMORY);					\
	    case AMSG_OK :						\
		break;							\
	    default :							\
		return(NSRV_ERROR);					\
		break;							\
	}								\
}


#define NsrvVariables							\
        aport_id_t reply_port_id;					\
	amsg_type_t msg_type;						\
        amsg_t request_msg;						\
        amsg_t reply_msg;						\
        amsg_ret_t aret


#define NsrvPrepareRequest(type) {					\
	msg_type = type;						\
        AmsgCheck(aport_allocate(&reply_port_id,			\
				 (void (*)()) 0));			\
        AmsgCheck(amsg_alloc((amsg_size_t) sizeof(*request),		\
                             (amsg_data_t * *) &request,		\
                             &request_msg));				\
}


#define NsrvRPC(number) {						\
        /* send request */						\
        AmsgCheck(amsg_send(aport_id(NSRV_BPORT_ID,number),		\
			    request_msg,				\
			    msg_type,					\
			    (amsg_count_t) 1,				\
		            (amsg_option_t) 0));			\
        /* wait for reply */						\
        do {								\
	    aret = amsg_receive(reply_port_id,				\
				&reply_msg,				\
				(amsg_data_t * *) &reply,		\
				&msg_type,				\
				(amsg_count_t *) 0,			\
				(amsg_option_t) 0);			\
        } while (aret == AMSG_NOMESSAGE);				\
        AmsgCheck(aret);						\
}


#define NsrvFinishRequest() {						\
        aret = amsg_free(reply_msg);					\
        nsrv_assert(aret == AMSG_OK);					\
        AmsgCheck(aport_deallocate(reply_port_id));			\
}


#define NsrvSockVariables						\
        nsrv_number_t nsrv_request_number;				\
        pds_uint32 buf[NSRV_BUF_SIZE];					\
        char * buffer;							\
        pds_uint32 bufsize;						\
        int nsrv_msg_sock;						\
        XDR xdrs


#define NsrvSockPrepareRequest(InetAddress,PortNumber) {		\
	buffer = (char *) buf;						\
									\
	nret = nsrv_connect(&nsrv_msg_sock,InetAddress,PortNumber);     \
        if (nret == NSRV_OK)						\
            xdrmem_create(&xdrs,(const caddr_t) buffer,			\
                          (const u_int) 4*NSRV_BUF_SIZE,		\
                          XDR_ENCODE);					\
}


#define NsrvSockUnpackSimpleReply() {					\
	if (!xdr_nsrv_ret(&xdrs,&nret))					\
	    nret = NSRV_ERROR;						\
	xdr_destroy(&xdrs);						\
}



#define NsrvSockRPC() {							    \
	/* derive size of request */					    \
	bufsize = (pds_uint32) xdr_getpos(&xdrs);			    \
									    \
	/* send request */						    \
	if (nret == NSRV_OK)						    \
            nret = nsrv_send_sock(nsrv_msg_sock,buffer,bufsize);    	    \
	xdr_destroy(&xdrs);						    \
									    \
	/* receive reply */						    \
	if (nret == NSRV_OK)						    \
	    nret = nsrv_receive_sock(nsrv_msg_sock,buffer,&bufsize);  	    \
	    								    \
        nsrv_close_sock(nsrv_msg_sock);					    \
									    \
	if (nret == NSRV_OK)						    \
	    xdrmem_create(&xdrs,(const caddr_t) buffer,			    \
		          (const u_int) 4*NSRV_BUF_SIZE,		    \
		          XDR_DECODE);					    \
	else								    \
	    nret = NSRV_ERROR;						    \
}


static nsrv_ret_t
nsrv_init_stubs()
{
    bmsg_ret_t bret;
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!amsg_ready()) {
	stubs_ready = 0;
        return(NSRV_OK);
    }

    if (!types_ready) {
        if ((nret = nsrv_types_init_i()) != NSRV_OK) {
    	    nsrv_assert(nret != AMSG_INVAL);
	    return(nret);
	}
	else
	    types_ready = 1;
    }

    if ((!pds_enabled) || (stubs_ready))
        return(NSRV_OK);

    do {
        bret = bport_open(&nsrv_port);
        switch (bret) {
	    case BMSG_POPENING :
		break;
	    case BMSG_NOPORT :
	    case BMSG_PUNREACH :
		return(NSRV_NOSERVER);
	    case BMSG_POPENED :
		break;
	    case BMSG_PBLOCKED :
		nsrv_assert_always();
		break;
	    case BMSG_PNOTAVAILABLE :
		break;
	    case BMSG_NORESOURCES :
	        return(NSRV_NORESOURCES);
	    case BMSG_NOMEMORY :
	        return(NSRV_NOMEMORY);
	    default :
		nsrv_assert_always();
		break;
        }
    } while (bret != BMSG_POPENED);

    stubs_ready = 1;

    return(NSRV_OK);
}


#define NSRV_RETRIES_MAX 64

static nsrv_ret_t
nsrv_connect(sock,inet_address,port_number)
    int * sock;
    char * inet_address;
    unsigned port_number;
{
    int ret;
    int nsrv_msg_sock;
    unsigned long address;
    struct sockaddr_in nsrv_ctrl_sock_name;
    int on = 1;
    int retries = 0;

    do {
        /* create message socket */
        do {
	    nsrv_msg_sock = socket(PF_INET,SOCK_STREAM,0);
	} while ((nsrv_msg_sock == -1) && (errno == EINTR));
        if (nsrv_msg_sock == -1) {
	    switch (errno) {
                case EMFILE :
                case ENFILE :
                case ENOBUFS :
	            return(NSRV_NORESOURCES);
	        default :
		    nsrv_perror_and_assert_always("socket()");
		    break;
	    }
        }
        /* construct name of control socket */
        address = inet_addr(inet_address);
        if (address == -1) {
	    nsrv_close_sock(nsrv_msg_sock);
	    return(NSRV_NOSERVER);
	}
        nsrv_ctrl_sock_name.sin_family = AF_INET;
        nsrv_ctrl_sock_name.sin_addr = * (struct in_addr *) &address;
        nsrv_ctrl_sock_name.sin_port = htons((u_short) port_number);
        /* establish connection */
        do {
	    ret = connect(nsrv_msg_sock,
                          (struct sockaddr *) &nsrv_ctrl_sock_name,
                          sizeof(nsrv_ctrl_sock_name));
        } while ((ret == -1) && (errno == EINTR));
        if (ret == -1) {
            switch (errno) {
                case ENOSR :
	    	    nsrv_close_sock(nsrv_msg_sock);
                    return(NSRV_NORESOURCES);
                case ETIMEDOUT :
                case EADDRNOTAVAIL :
                case ENETUNREACH :
	    	    nsrv_close_sock(nsrv_msg_sock);
		    return(NSRV_NOSERVER);
                case ECONNREFUSED :
		case EISCONN :    /* spurious error */
		case EADDRINUSE : /* spurious error */
		case EALREADY :   /* spurious error */
	    	    nsrv_close_sock(nsrv_msg_sock);
		    if (retries++ < NSRV_RETRIES_MAX)
		        break; /* retry */
		    return(NSRV_NOT_READY);
                default :
		    nsrv_perror_and_assert_always("connect()");
		    nsrv_close_sock(nsrv_msg_sock);
		    break;
            }
        }
        else {
	    nsrv_sock_noinherit(nsrv_msg_sock);
	    nsrv_sock_linger(nsrv_msg_sock);
	    nsrv_sock_nodelay(nsrv_msg_sock);
	    *sock = nsrv_msg_sock;
	    return(NSRV_OK);
	}
    } while (1);
}


static nsrv_ret_t
nsrv_get_inet_address(hostname,inet_address)
    char * hostname;
    inet_address_t inet_address;
{
    struct hostent * hostentry;
    unsigned long address;

    /* get internet address */
    hostentry = gethostbyname(hostname);
    if (!hostentry) {
        address = inet_addr(hostname);
        if (address == -1)
            return(NSRV_NOHOST);
	hostentry = gethostbyaddr((char *) &address,4,AF_INET);
	if (!hostentry)
	    return(NSRV_NOHOST);
    }
    if (hostentry->h_addrtype != AF_INET)
	return(NSRV_ERROR);
    if (hostentry->h_length != 4)
	return(NSRV_ERROR);
    strncpy((char *) inet_address,
            inet_ntoa(* (struct in_addr *) *hostentry->h_addr_list),
            INET_ADDRESSLEN_MAX);
    inet_address[INET_ADDRESSLEN_MAX] = '\0';
    return(NSRV_OK);
}


static nsrv_ret_t
nsrv_get_mode(inet_address,port_number,mode)
    inet_address_t inet_address;
    unsigned port_number;
    nsrv_mode_t * mode;
{
    NsrvSockVariables;
    nsrv_ret_t nret;

    NsrvSockPrepareRequest(inet_address,port_number);

    if (nret != NSRV_OK) 
	return(nret);

    /* pack request */
    nsrv_request_number = NSRV_GET_MODE;
    if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number)))
	nret = NSRV_ERROR;

    NsrvSockRPC();

    if (nret != NSRV_OK) 
	return(nret);

    /* unpack reply */

    if (!xdr_nsrv_ret(&xdrs,&nret))
        nret = NSRV_ERROR;

    if ((nret == NSRV_OK) && (!(xdr_nsrv_mode(&xdrs,mode))))
        nret = NSRV_ERROR;

    xdr_destroy(&xdrs);

    return(nret);
}


static nsrv_ret_t
nsrv_get_path(inet_address,port_number,path)
    inet_address_t inet_address;
    unsigned port_number;
    char * path;
{
    NsrvSockVariables;
    nsrv_ret_t nret;

    NsrvSockPrepareRequest(inet_address,port_number);

    if (nret != NSRV_OK) 
	return(nret);

    /* pack request */
    nsrv_request_number = NSRV_GET_PATH;
    if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number)))
	nret = NSRV_ERROR;

    NsrvSockRPC();

    if (nret != NSRV_OK) 
	return(nret);

    /* unpack reply */

    if (!xdr_nsrv_ret(&xdrs,&nret))
        nret = NSRV_ERROR;

    if ((nret == NSRV_OK) && (!(xdr_string(&xdrs,&path,NSRV_FILENAMELEN_MAX))))
        nret = NSRV_ERROR;

    xdr_destroy(&xdrs);

    return(nret);
}




/**********************************************************************
************************  Exported Primitives  ************************
***********************************************************************/

nsrv_ret_t
nsrv_ping(hostname,port_number)
    char * hostname;
    unsigned * port_number;
{
    NsrvSockVariables;
    inet_address_t inet_address;
    unsigned property;
    nsrv_ret_t nret;
    unsigned number;

    if (!hostname || !port_number)
	return(NSRV_INVAL);

    /* get internet address */
    if ((nret = nsrv_get_inet_address(hostname,inet_address)) != NSRV_OK)
	return(nret);

    /* get port number */
    number = *port_number;

    /* initialise port_number */
    nsrv_init_port_number(&number,&property);
    
    do {
	nret = NSRV_OK;

        NsrvSockPrepareRequest(inet_address,(u_short) number);

        /* pack request */
        nsrv_request_number = NSRV_PING;
        if ((nret == NSRV_OK) &&
	    (!xdr_nsrv_number(&xdrs,&nsrv_request_number)))
	    nret = NSRV_ERROR;

	if (nret == NSRV_OK)
	    NsrvSockRPC();

	if (nret == NSRV_OK)
	    NsrvSockUnpackSimpleReply();

	if ((nret != NSRV_OK) &&
	    (property == NSRV_DEFAULT) &&
	    (number >= NSRV_PORT_DEFAULT) &&
	    (number < (NSRV_PORT_DEFAULT + NSRV_DEFAULT_PORTS - 1)))
	    number++;
	else
	    break;

    } while (1);

    if (nret == NSRV_OK)
	*port_number = number;  /* update port number */

    return(nret);
}


nsrv_ret_t 
nsrv_init(hostname,port_number)
    char * hostname;
    unsigned * port_number;
{
    char nsrv_path[NSRV_FILENAMELEN_MAX+1];
    char datafile[NSRV_FILENAMELEN_MAX+1];
    char localhostname[MAXHOSTNAMELEN+1];
    nsrv_version_t version;
    nsrv_ret_t nret;
    nsrv_mode_t nsrv_mode;
    bdomain_t nsrv_domain;

    if (nsrv_initialised || nsrv_initialising)
	return(NSRV_ERROR);

    nsrv_initialising = 1;

    /* initialise hostname */
    if (!hostname) { /* use environment variable NSRV_HOST */
	hostname = getenv(NSRV_HOST);
        if (!hostname) { /* use default host_name, i.e. the local host */
	    if (gethostname(localhostname,MAXHOSTNAMELEN) != 0) {
    		nsrv_initialising = 0;
		return(NSRV_ERROR);
	    }
	    else
		hostname = localhostname;
	}
    }

    /* derive nsrv_port_number */
    if (!port_number)
	nsrv_port_number = 0;
    else
	nsrv_port_number = *port_number;
    nret = nsrv_ping(hostname,&nsrv_port_number);
    if (nret != NSRV_OK) {
    	nsrv_initialising = 0;
	return(NSRV_NOSERVER);
    }

    /* derive internet address */
    nret = nsrv_get_inet_address(hostname,nsrv_inet_address);
    if (nret != NSRV_OK) {
    	nsrv_initialising = 0;
	return(nret);
    }

    /* get nsrv_mode */
    if (nsrv_get_mode(nsrv_inet_address,nsrv_port_number,&nsrv_mode) != NSRV_OK) {
    	nsrv_initialising = 0;
	return(NSRV_NOSERVER);
    }

    /* get nsrv_path */
    if (nsrv_get_path(nsrv_inet_address,nsrv_port_number,nsrv_path) != NSRV_OK) {
    	nsrv_initialising = 0;
	return(NSRV_NOSERVER);
    }
    
    nsrv_initialised = 1;

    /* check version */
    nret = nsrv_version(&version);
    nsrv_assert(nret != NSRV_NOT_READY);
    nsrv_assert(nret != NSRV_INVAL);
    if (nret != NSRV_OK) {
	(void) nsrv_exit();
	nsrv_exited = 0;
	return(nret);
    }
    if (version.v_major != NSRV_VERSION_MAJOR) {
	(void) nsrv_exit();
	nsrv_exited = 0;
        return(NSRV_EVERSION);
    }

    /* derive nsrv_msg_start */
    if (nsrv_bdomain_look_up(NSRV_KEY,NSRV_DOMAIN_NAME,&nsrv_domain) != NSRV_OK) {
        (void) nsrv_exit();
	nsrv_exited = 0;
	return(NSRV_NOSERVER);
    }
    nsrv_msg_start = (char *) nsrv_domain.bdomain_start;

    /* get name server bport */
    if (nsrv_mode & NSRV_PDS) {
        if (nsrv_bport_look_up(NSRV_KEY,NSRV_PORT_NAME,&nsrv_port) != NSRV_OK) {
	    (void) nsrv_exit();
	    nsrv_exited = 0;
	    return(NSRV_NOSERVER);
	}
	pds_enabled = 1;
    }

    /* if possible, enable shared memory interaction */
    if ((nsrv_mode & NSRV_SHM) &&
	!strcmp(localhostname,hostname) &&
	(pathconf(nsrv_path,_PC_LINK_MAX) == -1) && (errno)) {
	sprintf(datafile,"%s/%s.%s.%d",
		nsrv_path,NSRV_DATAFILE,hostname,nsrv_port_number);
	if (nsrv_init_client_i(datafile) == NSRV_OK)
	    shm_enabled = 1;
    }

    /* update port number */
    if (port_number)
        *port_number = nsrv_port_number;

    nsrv_initialising = 0;

    return(NSRV_OK);
}


nsrv_ret_t
nsrv_types_init()
{
    nsrv_ret_t nret;

    if (!amsg_ready())
	return(NSRV_NOT_READY);

    if (!types_ready) {
        if ((nret = nsrv_types_init_i()) != NSRV_OK)
	    return(nret);
	else
	    types_ready = 1;
    }
    return(NSRV_OK);
}


void 
nsrv_exit()
{
    bmsg_ret_t bret;
    bport_t port;

    if (nsrv_init_stubs() != NSRV_OK)
	return;

    if (!nsrv_initialised || nsrv_exiting || nsrv_exited)
	return;

    nsrv_exiting = 1;

    if (stubs_ready) {
	stubs_ready = 0;
	/* close name server port */
	do {
	    bret = bport_close(nsrv_port.bport_id);
	} while (bret == BMSG_PNOTAVAILABLE);
	/* wait till name server port has actually been closed */
	do {
	    bret = bport_port(nsrv_port.bport_id,&port);
	} while (bret != BMSG_NOPORT);
    }
    shm_enabled = 0;
    pds_enabled = 0;
    nsrv_exited = 1;
    nsrv_exiting = 0;
    nsrv_initialising = 0;
}


char * 
nsrv_data_base()
{
    return(nsrv_data_start);
}


char * 
nsrv_msg_base()
{
    return(nsrv_msg_start);
}


nsrv_ret_t 
nsrv_aport_register(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    aport_t * port;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature || !port)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_aport_register_i(key,name,signature,port));

    else if (!stubs_ready) {
	NsrvSockVariables;

	NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

	/* pack request */
	nsrv_request_number = NSRV_APORT_REGISTER;
	if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
	      xdr_nsrv_name(&xdrs,&key) &&
	      xdr_nsrv_name(&xdrs,&name) &&
	      xdr_nsrv_name(&xdrs,&signature) &&
	      xdr_aport(&xdrs,port)))
	    nret = NSRV_ERROR;

	NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

	return(nret);
    }

    else {
        NsrvVariables;
        aport_register_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_APORT_REGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);
	request->port = *port;

        NsrvRPC(NSRV_APORT_REGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_aport_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_aport_deregister_i(key,name,signature));

    else if (!stubs_ready) {
	NsrvSockVariables;

	NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

	/* pack request */
	nsrv_request_number = NSRV_APORT_DEREGISTER;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name) &&
              xdr_nsrv_name(&xdrs,&signature)))
            nret = NSRV_ERROR;

	NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

	return(nret);
    }

    else {
        NsrvVariables;
	deregister_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_DEREGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);

        NsrvRPC(NSRV_APORT_DEREGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_aport_look_up(key,name,port)
    nsrv_name_t key;
    nsrv_name_t name;
    aport_t * port;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !port)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_aport_look_up_i(key,name,port));

    else if (!stubs_ready) {
	NsrvSockVariables;

	NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
	nsrv_request_number = NSRV_APORT_LOOK_UP;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name)))
            nret = NSRV_ERROR;

	NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	/* unpack reply */

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_aport(&xdrs,port))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs); 

	return(nret);
    }

    else {
        NsrvVariables;
	look_up_request_t * request;
	aport_reply_t * reply;

        NsrvPrepareRequest(MDT_LOOK_UP_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);

        NsrvRPC(NSRV_APORT_LOOK_UP);

	nsrv_assert(msg_type == MDT_APORT_REPLY);

        /* unpack reply message */
        nret = reply->ret;
	*port = reply->port;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t 
nsrv_bport_register(key,name,signature,port)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bport_t * port;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature || !port)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bport_register_i(key,name,signature,port));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
	nsrv_request_number = NSRV_BPORT_REGISTER;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bport(&xdrs,port)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

        NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
        NsrvVariables;
	bport_register_request_t * request;
	simple_reply_t * reply;

        NsrvPrepareRequest(MDT_BPORT_REGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);
	request->port = *port;

        NsrvRPC(NSRV_BPORT_REGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t 
nsrv_bport_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bport_deregister_i(key,name,signature));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_BPORT_DEREGISTER;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name) &&
              xdr_nsrv_name(&xdrs,&signature)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

        NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
        NsrvVariables;
        deregister_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_DEREGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);

        NsrvRPC(NSRV_BPORT_DEREGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t 
nsrv_bport_look_up(key,name,port)
    nsrv_name_t key;
    nsrv_name_t name;
    bport_t * port;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !port)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bport_look_up_i(key,name,port));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_BPORT_LOOK_UP;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_bport(&xdrs,port))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs);

        return(nret);
    }

    else {
	NsrvVariables;
        look_up_request_t * request;
        bport_reply_t * reply;

        NsrvPrepareRequest(MDT_LOOK_UP_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);

        NsrvRPC(NSRV_BPORT_LOOK_UP);

	nsrv_assert(msg_type == MDT_BPORT_REPLY);

        /* unpack reply message */
        nret = reply->ret;
	*port = reply->port;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_bdomain_register(key,name,signature,domain)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bdomain_t * domain;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature || !domain)
	return(NSRV_INVAL);

    /* ensure validity of filename string */
    domain->bdomain_file[BMSG_FILENAMELEN_MAX] = '\0';

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bdomain_register_i(key,name,signature,domain));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_BDOMAIN_REGISTER;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bdomain(&xdrs,domain)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
	NsrvVariables;
        bdomain_register_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_BDOMAIN_REGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);
	request->domain = *domain;

        NsrvRPC(NSRV_BDOMAIN_REGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_bdomain_deregister(key,name,signature)
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !signature)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bdomain_deregister_i(key,name,signature));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_BDOMAIN_DEREGISTER;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name) &&
              xdr_nsrv_name(&xdrs,&signature)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
	NsrvVariables;
        bdomain_register_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_DEREGISTER_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);
        strcpy(request->signature,signature);

        NsrvRPC(NSRV_BDOMAIN_DEREGISTER);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_bdomain_look_up(key,name,domain)
    nsrv_name_t key;
    nsrv_name_t name;
    bdomain_t * domain;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!key || !name || !domain || !domain->bdomain_file)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_bdomain_look_up_i(key,name,domain));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_BDOMAIN_LOOK_UP;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&key) &&
              xdr_nsrv_name(&xdrs,&name)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

        if (nret != NSRV_OK)
	    return(nret);

        /* unpack reply */

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_bdomain(&xdrs,domain))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs);

        return(nret);
    }

    else {
	NsrvVariables;
        look_up_request_t * request;
        bdomain_reply_t * reply;

        NsrvPrepareRequest(MDT_LOOK_UP_REQUEST);

        /* pack request message */
        request->reply_port_id = reply_port_id;
        strcpy(request->key,key);
        strcpy(request->name,name);

        NsrvRPC(NSRV_BDOMAIN_LOOK_UP);

	nsrv_assert(msg_type == MDT_BDOMAIN_REPLY);

        /* unpack reply message */
        nret = reply->ret;
	*domain = reply->domain;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_new_bport_id(signature,port_id)
    nsrv_name_t signature;
    bport_id_t * port_id;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!signature || !port_id)
        return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_new_bport_id_i(signature,port_id));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_NEW_BPORT_ID;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bport_id(&xdrs,port_id)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

	if (nret != NSRV_OK)
	    return(nret);

        /* unpack reply */

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_bport_id(&xdrs,port_id))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs);

        return(nret);
    }

    else {
	NsrvVariables;
        bport_id_request_t * request;
        bport_id_reply_t * reply;

        NsrvPrepareRequest(MDT_BPORTID_REQUEST);

        /* pack request message */
	request->request_number = NSRV_NEW_BPORT_ID;
        request->reply_port_id = reply_port_id;
        strcpy(request->signature,signature);
        request->port_id = *port_id;

        NsrvRPC(NSRV_NEW_BPORT_ID);

	nsrv_assert(msg_type == MDT_BPORTID_REPLY);

        /* unpack reply message */
        nret = reply->ret;
        *port_id = reply->port_id;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_free_bport_id(signature,port_id)
    nsrv_name_t signature;
    bport_id_t port_id;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!signature)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_free_bport_id_i(signature,port_id));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_FREE_BPORT_ID;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bport_id(&xdrs,&port_id)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

	if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
	NsrvVariables;
        bport_id_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_BPORTID_REQUEST);

        /* pack request message */
	request->request_number = NSRV_FREE_BPORT_ID;
        request->reply_port_id = reply_port_id;
        strcpy(request->signature,signature);
        request->port_id = port_id;

        NsrvRPC(NSRV_FREE_BPORT_ID);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_new_bdomain_id(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t * domain_id;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!signature || !domain_id)
        return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_new_bdomain_id_i(signature,domain_id));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_NEW_BDOMAIN_ID;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bdomain_id(&xdrs,domain_id)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

	if (nret != NSRV_OK)
	    return(nret);

        /* unpack reply */

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_bdomain_id(&xdrs,domain_id))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs);

        return(nret);
    }

    else {
	NsrvVariables;
        bdomain_id_request_t * request;
        bdomain_id_reply_t * reply;

        NsrvPrepareRequest(MDT_BDOMAINID_REQUEST);

        /* pack request message */
	request->request_number = NSRV_NEW_BDOMAIN_ID;
        request->reply_port_id = reply_port_id;
        strcpy(request->signature,signature);
        request->domain_id = *domain_id;

        NsrvRPC(NSRV_NEW_BDOMAIN_ID);

	nsrv_assert(msg_type == MDT_BDOMAINID_REPLY);

        /* unpack reply message */
        nret = reply->ret;
        *domain_id = reply->domain_id;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_free_bdomain_id(signature,domain_id)
    nsrv_name_t signature;
    bdomain_id_t domain_id;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!signature)
	return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled)
	return(nsrv_free_bdomain_id_i(signature,domain_id));

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_FREE_BDOMAIN_ID;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_name(&xdrs,&signature) &&
              xdr_bdomain_id(&xdrs,&domain_id)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

	if (nret != NSRV_OK)
	    return(nret);

	NsrvSockUnpackSimpleReply();

        return(nret);
    }

    else {
	NsrvVariables;
        bdomain_id_request_t * request;
        simple_reply_t * reply;

        NsrvPrepareRequest(MDT_BDOMAINID_REQUEST);

        /* pack request message */
	request->request_number = NSRV_FREE_BDOMAIN_ID;
        request->reply_port_id = reply_port_id;
        strcpy(request->signature,signature);
        request->domain_id = domain_id;

        NsrvRPC(NSRV_FREE_BDOMAIN_ID);

	nsrv_assert(msg_type == MDT_SIMPLE_REPLY);

        /* unpack reply message */
        nret = reply->ret;

        NsrvFinishRequest();

        return(nret);
    }
}


nsrv_ret_t
nsrv_version(version)
    nsrv_version_t * version;
{
    nsrv_ret_t nret;

    if (!nsrv_ready())
	return(NSRV_NOT_READY);

    if (!version)
        return(NSRV_INVAL);

    if ((nret = nsrv_init_stubs()) != NSRV_OK)
	return(nret);

    if (shm_enabled) {
	version->v_major = NSRV_VERSION_MAJOR;
	version->v_minor = NSRV_VERSION_MINOR;
	return(nsrv_version_i(version));
    }

    else if (!stubs_ready) {
	NsrvSockVariables;

        NsrvSockPrepareRequest(nsrv_inet_address,nsrv_port_number);

        if (nret != NSRV_OK) 
	    return(nret);

        /* pack request */
        nsrv_request_number = NSRV_VERSION;
        if (!(xdr_nsrv_number(&xdrs,&nsrv_request_number) &&
              xdr_nsrv_version(&xdrs,version)))
            nret = NSRV_ERROR;

        NsrvSockRPC();

	if (nret != NSRV_OK)
	    return(nret);

        /* unpack reply */

	if (!xdr_nsrv_ret(&xdrs,&nret))
	    nret = NSRV_ERROR;

	if ((nret == NSRV_OK) && (!(xdr_nsrv_version(&xdrs,version))))
            nret = NSRV_ERROR;

        xdr_destroy(&xdrs);

        return(nret);
    }

    else {
	NsrvVariables;
        version_request_t * request;
        version_reply_t * reply;

        NsrvPrepareRequest(MDT_VERSION_REQUEST);

        /* pack request message */
	request->request_number = NSRV_VERSION;
        request->reply_port_id = reply_port_id;

        NsrvRPC(NSRV_VERSION);

	nsrv_assert(msg_type == MDT_VERSION_REPLY);

        /* unpack reply message */
        nret = reply->ret;
        version->v_major = reply->version.v_major;
        version->v_minor = reply->version.v_minor;

        NsrvFinishRequest();

        return(nret);
    }
}


static char * nsrv_errlist[NSRV_EVERSION-PDS_RET_MAX] = {
	"No such port !",
	"Name server not ready !",
	"Not yours !",
	"Not registered !",
	"No such domain !",
	"No name server !",
	"No such host !",
	"Incompatible version !"};


char *
nsrv_error_string(nret)
    nsrv_ret_t nret;
{
    return nsrv_errlist[nret-PDS_RET_MAX];
}


void
nsrv_perror(nret, s)
    nsrv_ret_t nret;
    char * s;
{
    if (nret > PDS_RET_MAX) {
        if (s && s[0] != '\0')
            fprintf(stderr,"%s: ",s);
	fprintf(stderr, "%s\n", nsrv_error_string(nret));
    }
    else
        pds_perror((pds_ret_t) nret, s);
}


