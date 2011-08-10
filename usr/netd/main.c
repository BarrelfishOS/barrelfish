/**
 * \file
 * \brief Echo server main
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/* giving smaller sizes to use by lwip network stack */

#define MEM_SIZE            (16*1024)

/// Number of PBUF structs available
#define MEMP_NUM_PBUF           128

/// Number of PBUF buffers available
#define PBUF_POOL_SIZE          128

/// the size of the pool
#define PBUF_POOL_BUFSIZE       (2 * 1027)

#define MEM_CONF_LOC     "custom"

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/net_constants.h>
#include <barrelfish/deferred.h>
#include <barrelfish/dispatch.h>
#include <bfdmuxtools/tools.h>
#include <bfdmuxtools/codegen.h>
#include "netd.h"
#include "netd_debug.h"
#include "portalloc.h"
#include "idc_barrelfish.h"


uint64_t minbase = -1, maxbase = -1;

void network_polling_loop(void);


/*
static void timer_wrapper(void *arg)
{
//    NETD_DEBUG("timer_wrapper: called\n");

    void (*lwip_callback)(void) = arg;
    lwip_callback();

//    NETD_DEBUG("timer_wrapper: terminated\n");
}

static void run_timer(uint64_t duration_ms, void (*callback)(void))
{
//    NETD_DEBUG("run_timer: called\n");
    struct periodic_event *e = malloc(sizeof(*e));
    assert(e != NULL);

    errval_t err = periodic_event_create(e, get_default_waitset(),
                                         duration_ms * 1000,
                                         MKCONT(timer_wrapper, callback));
    assert(err_is_ok(err));

//    NETD_DEBUG("run_timer: terminated\n");
}
*/


/****************************************************************************
 * netd main function
 ***************************************************************************/

int main(int argc, char**argv)
{
    char *card_name = NULL;
    char filter_controller_name[100];
    char net_ctrl_service_name[100];

    NETD_DEBUG("running on core %d\n", disp_get_core_id());
    NETD_DEBUG("###################################################\n");


    /* Initilization */
    local_ip.addr = 0;
    card_conn[0] = NULL;
    card_conn[1] = NULL;
//    init_free_ports();
    
    /* Read commandline args */
    for (int i = 0; i < argc; i++) {
        if(strncmp(argv[i],"affinitymin=",strlen("affinitymin="))==0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            NETD_DEBUG("minbase = %" PRIu64 "\n", minbase);
        }
        if(strncmp(argv[i],"affinitymax=",strlen("affinitymax=")-1)==0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            NETD_DEBUG("maxbase = %"PRIu64"\n", maxbase);
        }
        if(strncmp(argv[i],"cardname=",strlen("cardname=")-1)==0) {
            card_name = argv[i] + strlen("cardname=");
            NETD_DEBUG("card name = %s\n", card_name);
        }
    }

    if (card_name == NULL) {
        fprintf(stderr, "Error: netd: card name not specified, but it is required\n");
        fprintf(stderr, "Hint: try \"netd cardname=e1000\"\n");
        return 1;
    }

    snprintf(filter_controller_name, sizeof(filter_controller_name), "%s%s",
            card_name, FILTER_SERVICE_SUFFIX);

    snprintf(net_ctrl_service_name, sizeof(net_ctrl_service_name), "%s%s",
            card_name, CTL_SERVICE_SUFFIX);


    /* Set memory affinity if requested */
    if ((minbase != -1) && (maxbase != -1)) {
	   ram_set_affinity(minbase, maxbase);
    }

/*    for (int i = 0; i < 0xfffff; i++) {
	   thread_yield();
    }
*/
    // Initialize Timer and LWIP
/*
    NETD_DEBUG("setting up timers for lwip\n");

    run_timer(DHCP_FINE_TIMER_MSECS, dhcp_fine_tmr);
    run_timer(DHCP_COARSE_TIMER_MSECS, dhcp_coarse_tmr);
*/

    /* Connect to e1000 driver */
    NETD_DEBUG("trying to connect to the %s driver...\n", card_name);
    startlwip(card_name);

    init_free_ports();

    /* connect with NIC_ctrl interface of driver */
    init_controller_service(filter_controller_name, net_ctrl_service_name);



    NETD_DEBUG("starting network loop...\n");
    network_polling_loop();
    return 0;
}

