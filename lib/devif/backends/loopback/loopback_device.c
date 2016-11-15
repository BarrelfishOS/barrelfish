/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/loopback_device.h>


// Loopback device functions
errval_t devq_loopback_setup(uint32_t coreid, uint64_t flags,   
                             uint64_t *features, uint32_t* default_qsize, 
                             uint32_t* default_bufsize, bool* reconnect, 
                             char* name)
{
    *features = 0;
    *default_qsize = 0;
    *default_bufsize = 0;
    *reconnect = false;
    name = "loopback";
    return SYS_ERR_OK;
}

errval_t devq_loopback_destroy(struct devq *q)
{
    return SYS_ERR_OK;
}

errval_t devq_loopback_create(struct devq *q, uint64_t flags)
{
    return SYS_ERR_OK;
}

errval_t devq_loopback_notify(struct devq *q, uint8_t num_slots)
{
    errval_t err;
    struct devq_buf buf;
    for (int i = 0; i < num_slots; i++) {
        err = devq_dequeue(q, &(buf.rid), &(buf.addr), 
                           &(buf.len), &(buf.bid), &(buf.flags));
        if (err_is_fail(err))  {
            return err;
        }   

        err = devq_enqueue(q, buf.rid, buf.addr, buf.len,
                           buf.flags, &buf.bid);
        if (err_is_fail(err))  {
            return err;
        }   
    }
   
    err = devq_notify(q);
    return err;
}

errval_t devq_loopback_register(struct devq *q, struct capref cap,
                                regionid_t region_id)
{
    return SYS_ERR_OK;
}

errval_t devq_loopback_deregister(struct devq *q, regionid_t region_id)
{
    return SYS_ERR_OK;
}

errval_t devq_loopback_control(struct devq *q, uint64_t request,
                               uint64_t value)
{
    // TODO Might have some options for loopback device?
    return SYS_ERR_OK;
}
