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

static struct devq_func_pointer f;

errval_t create_forward(struct devq* q, uint64_t flags);
errval_t register_forward(struct devq* q, struct capref cap,
                         regionid_t rid);
errval_t deregister_forward(struct devq* q, regionid_t rid);
errval_t control_forward(struct devq* q, uint64_t cmd, uint64_t value);
errval_t destroy_forward(struct devq* q);
errval_t notify_forward(struct devq* q, uint8_t num_slots);


errval_t create_forward(struct devq* q, uint64_t flags)
{
    return SYS_ERR_OK;
}

errval_t register_forward(struct devq* q, struct capref cap,
                         regionid_t rid) 
{
    return SYS_ERR_OK;
}


errval_t deregister_forward(struct devq* q, regionid_t rid) 
{
    return SYS_ERR_OK;
}


errval_t control_forward(struct devq* q, uint64_t cmd, uint64_t value)
{
    return SYS_ERR_OK;
}


errval_t notify_forward(struct devq* q, uint8_t num_slots)
{
    return SYS_ERR_OK;
}

errval_t destroy_forward(struct devq* q)
{
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{

    errval_t err;
    struct devq* q;   
    f.create = create_forward;
    f.reg = register_forward;
    f.dereg = deregister_forward;
    f.notify = notify_forward;
    f.destroy = destroy_forward;
    f.ctrl = control_forward;   

    struct endpoint_state my_state = {
        .endpoint_type = ENDPOINT_TYPE_FORWARD,
        .device_name = "", // name will be assigned 
        .q = NULL,
        .features = 0,
        .f = f,
    };

    printf("Forward queue created\n");
    err = devq_create(&q, &my_state, "loopback", 1);
    if (err_is_fail(err)){
        USER_PANIC("Allocating devq failed \n");
    }    

    while(true) {
        event_dispatch(get_default_waitset());
    }
}

