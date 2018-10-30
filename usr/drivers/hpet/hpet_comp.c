/*
 * Copyright (c) 2018, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <hpet.h>
#include <hpet_debug.h>


static errval_t init(struct bfdriver_instance *bfi, const char *name,
                     uint64_t flags, struct capref *caps, size_t caps_len,
                     char **args, size_t args_len, iref_t *dev) {

    debug_printf("hpet_comp: init enter! argv[0]=%s \n", args[0]);
    return SYS_ERR_OK;
}

static errval_t attach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t detach(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

static errval_t set_sleep_level(struct bfdriver_instance *bfi, uint32_t level) {
    return SYS_ERR_OK;
}

static errval_t destroy(struct bfdriver_instance *bfi) { return SYS_ERR_OK; }

DEFINE_MODULE(hpet_comp_module, init, attach, detach, set_sleep_level, destroy);
