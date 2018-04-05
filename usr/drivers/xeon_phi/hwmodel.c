/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>

#include <flounder/flounder_txqueue.h>

#include <if/interphi_defs.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_domain.h>

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "smpt.h"
#include "domain.h"
#include "service.h"
#include "xphi_service.h"
#include "sysmem_caps.h"


#include <driverkit/iommu.h>
#include "xeon_phi_internal.h"


/*
 * the translate address is called when a region is registered
 */
errval_t xeon_phi_hw_model_query_and_config(void *arg,
                                            struct capref mem,
                                            genpaddr_t *retaddr)
{
    /* query the model */
    #ifdef __k1om__
    return LIB_ERR_NOT_IMPLEMENTED;
    #endif

    //struct xeon_phi *phi = arg;

    // set the

    /* xxx */

    return LIB_ERR_NOT_IMPLEMENTED;
}