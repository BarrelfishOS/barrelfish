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
**        File: nsrv_int.h
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv_int.h	1.18 07 Nov 1995"
** Description: Name Service Internals
***********************************************************************/

#ifndef _NSRV_INT_H_
#define _NSRV_INT_H_

/*
** Return Codes
*/

#define NSRV_XDR_ENCODE        -3          /* xdr encoding error             */
#define NSRV_XDR_DECODE        -2          /* xdr decoding error             */



/*
** Property
*/

#define NSRV_ENVIRONMENT	1
#define NSRV_DEFAULT		2
#define NSRV_PARAMETER  	3



/*
** Version
*/

#define NSRV_VERSION_MAJOR      0
#define NSRV_VERSION_MINOR      1


/*
** Mode
*/

typedef pds_word_t nsrv_mode_t;

#define xdr_nsrv_mode	xdr_pds_word

#define NSRV_PDS		0x0001
#define NSRV_SHM		0x0010



/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(NDEBUG)
#define nsrv_assert(ex)
#else
#if defined(__STDC__)
    extern int fprintf(FILE * stream, const char * format, ...);
#else /* __STDC__ */
    extern int fprintf();
#endif /* __STDC__ */
#define nsrv_assert(ex) {                                       \
    if (!(ex)) {                                                \
        (void) fprintf(stderr,                                  \
               "PDS NSRV Assertion Failed:");                   \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(-1);                                               \
    }                                                           \
}
#endif /* NDEBUG */

#define nsrv_assert_always() {					\
        (void) fprintf(stderr,                                  \
               "PDS NSRV Assertion Failed:");                   \
        (void) fprintf(stderr, " file \"%s\"", __FILE__);       \
        (void) fprintf(stderr, " line %d\n", __LINE__);         \
        exit(-1);                                               \
}

#define nsrv_perror_and_assert_always(s) {			\
	(void) perror(s);					\
	nsrv_assert_always();					\
}


/*
** NSRV Memory Areas
*/

extern char * nsrv_data_start;	/* start of (shared) data area	*/
extern char * nsrv_msg_start;	/* start of (shared) msg area	*/


/*
** Request/Reply Buffer 
**
** Note: XDR requires that buffers are aligned on a 4-byte boundary.
**       The size of the buffers is measured in 4-byte XDR entities.
*/

#define NSRV_BUF_SIZE (NSRV_FILENAMELEN_MAX + 128)	


/*
** Request Numbers
*/

typedef pds_int32  nsrv_number_t;

#define MDT_NSRVNUMBER	MDT_INT32

#define xdr_nsrv_number	xdr_pds_int32

#define NSRV_APORT_REGISTER	0
#define NSRV_APORT_DEREGISTER	1
#define NSRV_APORT_LOOK_UP	2
#define NSRV_BPORT_REGISTER	3
#define NSRV_BPORT_DEREGISTER	4
#define NSRV_BPORT_LOOK_UP	5
#define NSRV_BDOMAIN_REGISTER	6
#define NSRV_BDOMAIN_DEREGISTER	7
#define NSRV_BDOMAIN_LOOK_UP	8
#define NSRV_NEW_BPORT_ID	9
#define NSRV_FREE_BPORT_ID	10
#define NSRV_NEW_BDOMAIN_ID	11
#define NSRV_FREE_BDOMAIN_ID	12
#define NSRV_VERSION		13
#define NSRV_NOF_APORTS		14

#define NSRV_PING		20

#define NSRV_GET_MODE		30
#define NSRV_GET_PATH		31


/*
** Requests
*/

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    aport_t port;
} aport_register_request_t;

extern msg_type_t MDT_APORT_REGISTER_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bport_t port;
} bport_register_request_t;

extern msg_type_t MDT_BPORT_REGISTER_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
    bdomain_t domain;
} bdomain_register_request_t;

extern msg_type_t MDT_BDOMAIN_REGISTER_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t key;
    nsrv_name_t name;
    nsrv_name_t signature;
} deregister_request_t;

extern msg_type_t MDT_DEREGISTER_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t key;
    nsrv_name_t name;
} look_up_request_t;

extern msg_type_t MDT_LOOK_UP_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t signature;
    bport_id_t port_id;
} bport_id_request_t;

extern msg_type_t MDT_BPORTID_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
    nsrv_name_t signature;
    bdomain_id_t domain_id;
} bdomain_id_request_t;

extern msg_type_t MDT_BDOMAINID_REQUEST;

typedef struct {
    nsrv_number_t request_number;
    aport_id_t reply_port_id;
} version_request_t;

extern msg_type_t MDT_VERSION_REQUEST;


/*
** Replies
*/

typedef struct {
    nsrv_ret_t ret;
} simple_reply_t;

extern msg_type_t MDT_SIMPLE_REPLY;

typedef struct { 
    nsrv_ret_t ret;
    aport_t port;
} aport_reply_t;

extern msg_type_t MDT_APORT_REPLY;

typedef struct {
    nsrv_ret_t ret;
    bport_t port;
} bport_reply_t;

extern msg_type_t MDT_BPORT_REPLY;

typedef struct {
    nsrv_ret_t ret;
    bdomain_t domain;
} bdomain_reply_t;

extern msg_type_t MDT_BDOMAIN_REPLY;

typedef struct {
    nsrv_ret_t ret;
    bport_id_t port_id;
} bport_id_reply_t;

extern msg_type_t MDT_BPORTID_REPLY;

typedef struct {
    nsrv_ret_t ret;
    bdomain_id_t domain_id;
} bdomain_id_reply_t;

extern msg_type_t MDT_BDOMAINID_REPLY;

typedef struct {
    nsrv_ret_t ret;
    nsrv_version_t version;
} version_reply_t;

extern msg_type_t MDT_VERSION_REPLY;

#if defined(__STDC__)
extern nsrv_ret_t nsrv_types_init_i(void);
extern void nsrv_init_port_number(unsigned * port_number,
				  unsigned * property);
extern void nsrv_sock_linger(int sock);
extern void nsrv_sock_nodelay(int sock);
extern nsrv_ret_t nsrv_send_sock(int sock,
				 char * message,
				 pds_uint32 size);
extern nsrv_ret_t nsrv_receive_sock(int sock,
				    char * message,
				    pds_uint32 * size);
extern void nsrv_close_sock(int sock);
extern void nsrv_init_server_i(char * datafile, int verbose);
extern void nsrv_exit_i(void);
extern nsrv_ret_t nsrv_init_client_i(char * datafile);
extern nsrv_ret_t nsrv_aport_register_i(nsrv_name_t key,
                                        nsrv_name_t name,
                                        nsrv_name_t signature,
                                        aport_t * port);
extern nsrv_ret_t nsrv_aport_deregister_i(nsrv_name_t key,
                                          nsrv_name_t name,
                                          nsrv_name_t signature);
extern nsrv_ret_t nsrv_aport_look_up_i(nsrv_name_t key,
                                       nsrv_name_t name,
                                       aport_t * port);
extern nsrv_ret_t nsrv_bport_register_i(nsrv_name_t key,
                                        nsrv_name_t name,
                                        nsrv_name_t signature,
                                        bport_t * port);
extern nsrv_ret_t nsrv_bport_deregister_i(nsrv_name_t key,
                                          nsrv_name_t name,
                                          nsrv_name_t signature);
extern nsrv_ret_t nsrv_bport_look_up_i(nsrv_name_t key,
                                       nsrv_name_t name,
                                       bport_t * port);
extern nsrv_ret_t nsrv_bdomain_register_i(nsrv_name_t key,
                                          nsrv_name_t name,
                                          nsrv_name_t signature,
                                          bdomain_t * domain);
extern nsrv_ret_t nsrv_bdomain_deregister_i(nsrv_name_t key,
                                            nsrv_name_t name,
                                            nsrv_name_t signature);
extern nsrv_ret_t nsrv_bdomain_look_up_i(nsrv_name_t key,
                                         nsrv_name_t name,
                                         bdomain_t * domain);
extern nsrv_ret_t nsrv_new_bport_id_i(nsrv_name_t signature,
                                      bport_id_t * port_id);
extern nsrv_ret_t nsrv_free_bport_id_i(nsrv_name_t signature,
                                       bport_id_t port_id);
extern nsrv_ret_t nsrv_new_bdomain_id_i(nsrv_name_t signature,
                                        bdomain_id_t * domain_id);
extern nsrv_ret_t nsrv_free_bdomain_id_i(nsrv_name_t signature,
                                         bdomain_id_t domain_id);
extern nsrv_ret_t nsrv_version_i(nsrv_version_t * version);
#else /* __STDC__ */
extern nsrv_ret_t nsrv_types_init_i();
extern void nsrv_init_port_number();
extern void nsrv_sock_linger();
extern void nsrv_sock_nodelay();
extern nsrv_ret_t nsrv_send_sock();
extern nsrv_ret_t nsrv_receive_sock();
extern void nsrv_close_sock();
extern void nsrv_init_server_i();
extern void nsrv_exit_i();
extern nsrv_ret_t nsrv_init_client_i();
extern nsrv_ret_t nsrv_aport_register_i();
extern nsrv_ret_t nsrv_aport_deregister_i();
extern nsrv_ret_t nsrv_aport_look_up_i();
extern nsrv_ret_t nsrv_bport_register_i();
extern nsrv_ret_t nsrv_bport_deregister_i();
extern nsrv_ret_t nsrv_bport_look_up_i();
extern nsrv_ret_t nsrv_bdomain_register_i();
extern nsrv_ret_t nsrv_bdomain_deregister_i();
extern nsrv_ret_t nsrv_bdomain_look_up_i();
extern nsrv_ret_t nsrv_new_bport_id_i();
extern nsrv_ret_t nsrv_free_bport_id_i();
extern nsrv_ret_t nsrv_new_bdomain_id_i();
extern nsrv_ret_t nsrv_free_bdomain_id_i();
extern nsrv_ret_t nsrv_version_i();
#endif /* __STDC__ */


#endif /* _NSRV_INT_H_ */

