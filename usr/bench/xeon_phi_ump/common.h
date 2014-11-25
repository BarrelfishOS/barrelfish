/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XPHI_BENCH_COMMON
#define XPHI_BENCH_COMMON 1

extern struct xeon_phi_callbacks callbacks;

extern struct ump_chan xphi_uc;
extern struct ump_chan xphi_uc_rev;

extern struct capref local_frame;
extern struct capref remote_frame;

extern xphi_dom_id_t domainid;

void alloc_local(void);

void wait_for_connection(void);

#define EXPECT_SUCCESS(_err, msg) \
                if(err_is_fail(_err)) {USER_PANIC_ERR(_err, msg);}

#endif
