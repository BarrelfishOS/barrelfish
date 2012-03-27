/**
 * \file
 * \brief Echo server main
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/net_constants.h>

// For event loops
#include <barrelfish/dispatch.h>

// standard include files
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "netd.h"
#include "netd_debug.h"


static void netd_event_polling_loop(void)
{
    errval_t err;
    printf("Starting event polling loop\n");
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}


/****************************************************************************
 * netd main function
 ***************************************************************************/

int main(int argc, char **argv)
{
    char *card_name = NULL;
    uint64_t allocated_queue = 0;

    uint64_t minbase = -1, maxbase = -1;

    NETD_DEBUG("running on core %d\n", disp_get_core_id());
    NETD_DEBUG("###################################################\n");


    /* Read commandline args */
    for (int i = 0; i < argc; i++) {
        if (strncmp(argv[i], "affinitymin=", strlen("affinitymin=")) == 0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            NETD_DEBUG("minbase = %" PRIu64 "\n", minbase);
        }
        if (strncmp(argv[i], "affinitymax=", strlen("affinitymax=") - 1) == 0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            NETD_DEBUG("maxbase = %" PRIu64 "\n", maxbase);
        }
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            card_name = argv[i] + strlen("cardname=");
            NETD_DEBUG("card name = %s\n", card_name);
        }
    }

    if (card_name == NULL) {
        fprintf(stderr,
                "Error: netd: card name not specified, but it is required\n");
        fprintf(stderr, "Hint: try \"netd cardname=e1000\"\n");
        return 1;
    }

#if 0
    NETD_DEBUG("setting up timers for lwip\n");

    // Set memory affinity if requested
    if ((minbase != -1) && (maxbase != -1)) {
        ram_set_affinity(minbase, maxbase);
    }
    // Initialize Timer and LWIP
/*
    NETD_DEBUG("setting up timers for lwip\n");

    run_timer(DHCP_FINE_TIMER_MSECS, dhcp_fine_tmr);
    run_timer(DHCP_COARSE_TIMER_MSECS, dhcp_coarse_tmr);
*/

#endif // 0

    // FIXME: This has to be done for every card
    // Connect to e1000 driver
    NETD_DEBUG("trying to connect to the %s:%"PRIu64" driver...\n",
            card_name, allocated_queue);
    startlwip(card_name, allocated_queue);
    init_ARP_lookup_service(card_name);

    netd_event_polling_loop();
    return 0;
}
