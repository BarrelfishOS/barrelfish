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
**        File: nsrv_ping.c
**      Author: Kees Schuerman
**      SccsId: "@(#)nsrv_ping.c	1.3 5/15/95"
** Description: Name Server Ping
***********************************************************************/

#include "machine.h"	/* architecture specific constant definitions */

#include <sys/types.h>
#include <sys/param.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <netdb.h>
#include <errno.h>

#include "pds.h"
#include "nsrv.h"



/**********************************************************************
** Global Variables
***********************************************************************/

static char * hostname = (char *) 0;
static unsigned port_number = 0;



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
    return;
}



/**********************************************************************
** Local Primitives
***********************************************************************/

static void
usage(program_name)
    char *program_name;
{
    fprintf(stderr,
            "\nUsage: %s [-p port_number] [-m machine_name] [-h]\n",
            program_name);

    fprintf(stderr, "       -p port_number: port number \n");
    fprintf(stderr, "       -m machine_name: machine name \n");
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
                case 'm': /* machine name */
		    if (++i >= argc) {
                        fprintf(stderr,
                                "\nmachine name parameter expected !\n");
                        usage(argv[0]);
                        break;
                    }
                    else
                        hostname = argv[i];
		    break;
                case 'p': /* port number */
                    if (++i >= argc) {
                        fprintf(stderr,
                                "\nport number parameter expected !\n");
                        usage(argv[0]);
                        break;
                    }
                    else {
                        int number;
                        number = atoi(argv[i]);
                        if ((number < 1024) ||
                            (number > 0xffff)) {
                            fprintf(stderr,
                                    "\n Invalid port number !\n");
                            usage(argv[0]);
                            break;
                        }
                        else
                            port_number = number;
                    }
                    break;
                default :
                    usage(argv[0]); /* exit with usage message */
                    break;
        }
    }
    if (!hostname) {
	hostname = (char *) malloc(MAXHOSTNAMELEN + 1);
	if (!hostname) {
	    fprintf(stderr, "Not enough memory !\n");
	    exit(-1);
	}
	if (gethostname(hostname,MAXHOSTNAMELEN)) {
	    perror("Implementation error: gethostname() ");
	    exit(-1);
	}
    }
}



/**********************************************************************
** Main
***********************************************************************/

int
main(argc, argv)
    int argc;
    char *argv[];
{
    nsrv_ret_t nret;

    /* get arguments */
    args(argc, argv);

    nret = nsrv_ping(hostname,&port_number);
    switch (nret) {
	case NSRV_OK :
	    printf("Name server nsrv on host %s is listening on port %d !\n",
	           hostname,
	           port_number);
	    break;
	case NSRV_NOHOST :
	    printf("No host %s !\n", hostname);
	    break;
	default :
	    if (!port_number)
	        printf("No name server nsrv on host %s listening on default ports !\n",
	               hostname);
	    else
	        printf("No name server nsrv on host %s listening on port %d !\n",
	               hostname,
	               port_number);
	    break;
    }

    return(0);
}

