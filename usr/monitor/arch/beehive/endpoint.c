/**
 * \file
 * \brief Beehive messaging
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

/**
 * \brief Set the handler endpoint for a given BMP association
 *
 * \param associd BMP association number
 * \param ep Endpoint to receive interrupts
 */
static errval_t monitor_bmptable_set(unsigned int associd, struct capref ep)
{
    return invoke_monitor_bmptable_set(cap_bmptable, associd, ep);
}
#if 0 // unused
/**
 * \brief Remove the handler endpoint for a given BMP association
 *
 * \param associd BMP association number
 */
static errval_t monitor_bmptable_delete(unsigned int associd)
{
    /* XXX: delete endpoint! */
    return invoke_monitor_bmptable_delete(cap_bmptable, associd);
}
#endif


extern errval_t beehive_create_cap(coreid_t coreid, int chanid,
                                   struct capref *retcap);

extern errval_t beehive_chan_allocate(struct capref ep, int *chanid);


static int glbl_chanid = 0;


errval_t beehive_chan_allocate(struct capref ep, int *chanid)
{
    *chanid = glbl_chanid;
    glbl_chanid++;

    if(get_cap_addr(ep) != CPTR_NULL) {
        return monitor_bmptable_set(*chanid, ep);
    } else {
        return SYS_ERR_OK;
    }
}

errval_t beehive_create_cap(coreid_t coreid, int chanid, 
                            struct capref *retcap)
{
    errval_t err;

    printf("%d: creating beehive cap with chanid %d, coreid %d, caller %p\n",
           my_core_id, chanid, coreid, __builtin_return_address(0));

    /* Construct the notification capability */
    struct capability beehive_cap = {
        .type = ObjType_BMPEndPoint,
        .rights = CAPRIGHTS_READ_WRITE, // XXX
        .u.bmpendpoint = {
            .coreid = coreid,
            .chanid = chanid
        }
    };

    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to allocate slot from channel_alloc");
        printf("Failed to allocate slot from channel_alloc\n");
        abort(); //XXX
    }
    err = monitor_cap_create(*retcap, &beehive_cap, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_create failed");
        printf("monitor_cap_create failed\n");
        abort(); //XXX
    }

    return SYS_ERR_OK;
}
