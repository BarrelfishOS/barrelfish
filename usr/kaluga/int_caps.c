#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>

#include <stdio.h>
#include <stdlib.h>
#include <if/monitor_blocking_defs.h>

#include "kaluga.h"

static struct capref all_irq_cap;

#define ARGNODE_INT_SLOT 0

errval_t store_int_cap(int start, int end, struct driver_argument *arg){
    errval_t err;
    assert(!cnoderef_is_null(arg->argnode_ref));
    assert(!capref_is_null(all_irq_cap));

    struct capref cap;
    cap.cnode = arg->argnode_ref;
    cap.slot = ARGNODE_INT_SLOT;
    err = cap_retype(cap, all_irq_cap, start, ObjType_IRQSrc, 
            end, 1);
    if(err_is_fail(err)){
        DEBUG_ERR(err, "Could not create int_src cap");
        return err;
    }
    return SYS_ERR_OK;
} 

/**
 * Initialize the cap manager. It will track retypes of all_irq_cap
 */
errval_t init_int_caps_manager(struct capref irq)
{
    assert(!capref_is_null(irq));
    all_irq_cap = irq;
    return SYS_ERR_OK;
}
