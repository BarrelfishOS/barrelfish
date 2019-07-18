/*
 * Copyright (c) 2019 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <limits.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/monitor_client.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish_kpi/domain_params.h>
#include <arch/registers.h>
#include <barrelfish/dispatch.h>
#include <if/interdisp_defs.h>
#include "arch/threads.h"
#include "init.h"
#include <if/monitor_defs.h>
#include "threads_priv.h"
#include "waitset_chan_priv.h"
#include "domain_priv.h"

errval_t domain_new_dispatcher_arch(dispatcher_handle_t handle)
{
    
    // XXX: share LDT state between all dispatchers
    // this needs to happen before the remote core starts, otherwise the segment
    // selectors in the new thread state are invalid
    struct dispatcher_shared_x86_64 *disp_x64
        = get_dispatcher_shared_x86_64(handle);
    struct dispatcher_shared_x86_64 *mydisp_x64
        = get_dispatcher_shared_x86_64(curdispatcher());

    disp_x64->ldt_base = mydisp_x64->ldt_base;
    disp_x64->ldt_npages = mydisp_x64->ldt_npages;    

    return SYS_ERR_OK;
}
