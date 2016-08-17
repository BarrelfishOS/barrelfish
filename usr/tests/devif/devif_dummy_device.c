/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>

static struct endpoint_state* state;
static struct capref last_region;
static regionid_t last_rid;


// Prototypes

errval_t notify_loopback(struct devq *q, uint8_t num_slots);
errval_t setup (uint64_t* features,
                bool* reconnect, char* name) ;
errval_t create(struct devq *q, uint64_t flags);
errval_t reg(struct devq *q, struct capref cap, regionid_t rid) ;
errval_t dereg(struct devq *q, regionid_t rid); 
errval_t notify(struct devq* q, uint8_t num_slots);

errval_t setup (uint64_t* features,
                bool* reconnect, char* name) 
{
    *features = 0;
    *reconnect = false;
    name = "loopback";
    return SYS_ERR_OK;
}

errval_t create (struct devq *q, uint64_t flags) 
{
    void* device = devq_get_state(q);
    device = malloc(sizeof(uint64_t));
    return SYS_ERR_OK;  
}

errval_t reg(struct devq *q, struct capref cap, regionid_t rid) 
{
    last_region = cap;
    last_rid = rid;
    return SYS_ERR_OK;
}


errval_t dereg(struct devq *q, regionid_t rid) 
{
    return SYS_ERR_OK;
}

errval_t notify_loopback(struct devq *q, uint8_t num_slots) 
{
    errval_t err;
    struct devq_buf* bufs = malloc(sizeof(struct devq_buf)*num_slots);
    for (int i = 0; i < num_slots; i++) {
        err = devq_dequeue(q, &(bufs[i].rid), &(bufs[i].addr),
                           &(bufs[i].len), &(bufs[i].bid), &(bufs[i].flags));

        if (err_is_fail(err)) {
            return err;
        }
    }

    for (int i = 0; i < num_slots; i++) {
        err = devq_enqueue(q, bufs[i].rid, bufs[i].addr,
                           bufs[i].len, bufs[i].flags, &bufs[i].bid);

        if (err_is_fail(err)) {
            return err;
        }
    }
    
    
    err = devq_notify(q);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}


int main(int argc, char *argv[])
{
    errval_t err;
    struct devq_func_pointer f = {
        .setup = setup,
        .create = create,
        .reg = reg,
        .notify = notify_loopback,
        .dereg = dereg,
    };

    struct endpoint_state my_state = {
        .endpoint_type = ENDPOINT_TYPE_NET,
        .device_name = "loopback",
        .q = NULL,
        .features = 0,
        .f = f,
    };
    
    state = &my_state;
    
    err = devq_driver_export(&my_state);
    if (err_is_fail(err)){  
        abort();
    }

    while (true) {
        event_dispatch(get_default_waitset());
    }
}

