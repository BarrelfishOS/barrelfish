/**
 * \file
 * \brief Header file for lock related functions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_LOCK_H_
#define DIST2_LOCK_H_

errval_t dist_lock(char*, char**);
errval_t dist_unlock(char*);

#endif /* DIST2_LOCK_H_ */
