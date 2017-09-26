/**
 * \file
 * \brief Internal proc_mgmt functions.
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PROC_MGMT_INTERNAL_H_
#define PROC_MGMT_INTERNAL_H_

#define SERVICE_BASENAME    "proc_mgmt"

extern coreid_t my_core_id;

errval_t start_service(void);

#endif //PROC_MGMT_INTERNAL_H_
