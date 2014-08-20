/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_SM_H
#define BULK_SM_H

struct bulk_sm_endpoint_descriptor {
    /* generic part */
    struct bulk_endpoint_descriptor ep_generic; ///< generic endpoint part
    /* start of implementation specific part */
    /* TODO: references to iref / flounder data structure */
};


#endif /* BULK_SM_H */
