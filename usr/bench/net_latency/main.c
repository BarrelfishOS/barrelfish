/*
 * Copyright (c) 2007-2012, ETH Zurich.
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
#include <barrelfish/spawn_client.h>

#include "sleep.h"


static void start_run(uint8_t core, int payload, int nocache, int read_incoming,
                      int head_idx_wb)
{
    errval_t r;
    domainid_t new_domain;
    //uint8_t code;
    char plsz[strlen("payload_size=0000") + 1];
    char noca[strlen("elb_nocache=0") + 1];
    char rdic[strlen("read_incoming=0") + 1];
    char hiwb[strlen("head_idx_wb=0") + 1];
    char prefix[128];

    char* const argv[] = { "e10k_queue_elb", "queue=0", "runs=100",
                           "dump_each=1", plsz, noca, rdic, hiwb, prefix, NULL};

    sprintf(plsz, "payload_size=%d", payload);
    sprintf(plsz, "elb_nocache=%d", nocache);
    sprintf(rdic, "read_incoming=%d", read_incoming);
    sprintf(hiwb, "head_idx_wb=%d", head_idx_wb);
    sprintf(prefix, "elp_outprefix=%d,%d,%d,%d,%d,", core, payload, nocache,
            read_incoming, head_idx_wb);

    r = spawn_program(core, argv[0], argv, NULL, SPAWN_NEW_DOMAIN, &new_domain);
    assert(err_is_ok(r));

    /*r = spawn_wait(new_domain, &code, false);
    assert(err_is_ok(r));*/
    milli_sleep(3*1000);
    //spawn_wait(new_domain, &code, true);
}


int main(int argc, char **argv)
{
    uint8_t core;
    int payloadsz = 64;
    int nocache = 0;
    int read_incoming = 0;
    int head_idx_wb = 0;
    sleep_init();

    printf("Net latency benchmark start\n");
    printf("%%  \"core\",\"payload\",\"nocache\",\"touch\",\"hiwb\","
           "\"rtt\",\"time\"\n");
    for (core = 0; core < 16; core++) {
        for (payloadsz = 64; payloadsz < 1500; payloadsz *= 4) {
            for (nocache = 0; nocache <= 1; nocache++) {
                for (read_incoming = 0; read_incoming <= 1; read_incoming++) {
                    for (head_idx_wb = 0; head_idx_wb <= 1; head_idx_wb++) {
                        start_run(core, payloadsz, nocache, read_incoming,
                                  head_idx_wb);
                    }
                }
            }
        }
    }
    printf("Net latency benchmark done\n");
    return 0;
}

