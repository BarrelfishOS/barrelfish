/**
 * \file
 * \brief Intel e1000 driver: Helper functions
 *
 * This file is a driver for the PCI Express e1000 card
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "e1000n.h"



uint16_t read_eeprom(struct e1000_t *dev, uint64_t offset)
{
    //make sure that there is no direct access request
    e1000_eec_t c = e1000_eec_rd(dev);
    c.ee_req = 0;
    e1000_eec_wr(dev, c);

    // set startbit and address of the EEPROM word to be read
    e1000_eerd_wr(dev, (e1000_eerd_t) { .start=1, .addr=offset } );

    //wait until read done and return then the value
    while ( e1000_eerd_rd(dev).done == 0 );
    return ( e1000_eerd_rd(dev).data );
}


/* allocate a single frame, mapping it into our vspace with given attributes */
void *alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap)
{
    struct capref frame;
    errval_t r;

    r = frame_alloc(&frame, size, NULL);
    assert(err_is_ok(r));
    void *va;
    r = vspace_map_one_frame_attr(&va, size, frame, attr,
                                  NULL, NULL);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "vspace_map_one_frame failed");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}


//these functions are most probably useless...
//only one client can connect to the e1000 driver. This client should be
//the demultiplexer...
#if 0
struct cpoll {
    struct chips_connection *conn;
    struct cpoll *prev, *next;
};
struct cpoll *polling_pointer = 0;
void add_channel_to_poll(struct chips_connection *conn)
{
    struct cpoll *tmp = (struct cpoll*)malloc(sizeof(struct cpoll));
    tmp->conn = conn;
    if (polling_pointer == 0) {
        tmp->prev = tmp;
        tmp->next = tmp;
        polling_pointer = tmp;
    } else {
        tmp->next = polling_pointer->next;
        tmp->prev = polling_pointer;
        polling_pointer->next->prev = tmp;
        polling_pointer->next = tmp;
    }
}
void remove_channel_to_poll(struct chips_connection *conn)
{
    struct cpoll *tmp = polling_pointer;
    if (tmp == 0) {
        return;
    }
    if (tmp->next == tmp) {
        if (tmp->conn == conn) {
            polling_pointer = 0;
            free(tmp);
        }
        return;
    }
    while (tmp->next != polling_pointer) {
        if (tmp->conn == conn) {
            tmp->prev->next = tmp->next;
            tmp->next->prev = tmp->prev;
            free(tmp);
            return;
        }
        tmp = tmp->next;
    }
    if (tmp->conn == conn) {
        polling_pointer->next = polling_pointer;
        polling_pointer->prev = polling_pointer;
        free(tmp);
    }
}
#endif
