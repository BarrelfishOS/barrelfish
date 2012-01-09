/**
 * \file
 * \brief Header file for the dist2 get/set API.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_GETSET_H_
#define DIST2_GETSET_H_

#include <barrelfish/barrelfish.h>

typedef uint64_t dist_mode_t;

#define SET_DEFAULT		(0x0)       /*!< Normal set mode for records. */
#define SET_SEQUENTIAL	(0x1)       /*!< Append a monotonically increasing number
                                         (based on the record name) to the name
                                         of the record. */
#define SET_TRANSIENT	(0x1 << 1) /*!< Record gets deleted as soon as the
                                        domain has ended it's execution. */
// TODO SET_TRANSIENT NYI Due to limitations of barrelfish:
// (can't figure out if a domain is done/crashed...).

#define DIST_ON_SET (0x1)           /*!< Trigger checked for set events. */
#define DIST_ON_DEL (0x1 << 1)      /*!< Trigger checked for del events. */

errval_t dist_get_names(char*** names, size_t*, const char*, ...);
void dist_free_names(char**, size_t);

errval_t dist_get(char**, const char*, ...);
errval_t dist_set(const char*, ...);
errval_t dist_set_get(dist_mode_t, char**, const char*, ...);
errval_t dist_del(const char*, ...);
errval_t dist_exists(const char*, ...);

errval_t dist_read(const char*, const char*, ...);

#endif /* DIST2_GETSET_H_ */
