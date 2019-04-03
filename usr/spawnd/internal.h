/**
 * \file
 * \brief internal functions
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERNAL_H_
#define INTERNAL_H_

#define SERVICE_BASENAME    "spawn" // the core ID is appended to this
#define ALL_SPAWNDS_UP 	    "all_spawnds_up"

extern coreid_t my_core_id;
extern const char *gbootmodules;

errval_t start_service(void);

#endif //INTERNAL_H_
