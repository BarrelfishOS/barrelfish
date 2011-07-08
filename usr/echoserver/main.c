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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include "echoserver.h"

uint64_t minbase = -1, maxbase = -1;

void network_polling_loop(void);

int main(int argc, char**argv)
{
    printf("%s running on core %u\n", argv[0], disp_get_core_id());

    /* Read commandline args */
    char *card_name = NULL;
    for (int i = 0; i < argc; i++) {
        if(strncmp(argv[i],"affinitymin=",strlen("affinitymin="))==0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            printf("minbase = %lu\n", minbase);
        }
        if(strncmp(argv[i],"affinitymax=",strlen("affinitymax=")-1)==0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            printf("maxbase = %lu\n", maxbase);
        }
        if(strncmp(argv[i],"cardname=",strlen("cardname=")-1)==0) {
            card_name = argv[i] + strlen("cardname=");
            printf("card name = %s\n", card_name);
        }
    }

    /* Set memory affinity if requested */
    if ((minbase != -1) && (maxbase != -1)) {
        ram_set_affinity(minbase, maxbase);
    }

    /* Connect to e1000 driver */
    printf("%s: trying to connect to the NIC driver...\n", argv[0]);
    startlwip(card_name);

    printf("echoserver: init finished.\n");

    network_polling_loop();

    return 0;
}

