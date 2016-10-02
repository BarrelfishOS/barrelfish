/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>


#include "region_pool.h"
#include "queue_interface_internal.h"

 /*
 * ===========================================================================
 * Device queue creation and destruction (General devif initalisation)
 * ===========================================================================
 */
 /**
  * @brief creates a queue 
  *
  * @param q             Return pointer to the devq (handle)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_init(struct devq *q)
{
    
    errval_t err;

    err = region_pool_init(&(q->pool));
    if (err_is_fail(err)) {
        return err;
    }
    
    return SYS_ERR_OK;
}


 /**
  * @brief destroys the device queue
  *
  * @param q           The queue state to free (and the device queue to be 
                       shut down in the driver)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
errval_t devq_destroy(struct devq *q)
{
    errval_t err;

    err = region_pool_destroy(q->pool);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

