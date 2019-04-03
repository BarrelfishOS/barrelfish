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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_LOCAL_H
#define BULK_LOCAL_H

#include <bulk_transfer/bulk_transfer.h>



struct bulk_local_endpoint {
    struct bulk_endpoint_descriptor   generic;
    struct bulk_channel              *other_channel;
};

/**
 * @param endpoint      Pointer to the endpoint that is to be initialized
 * @param other_channel NULL for a create channel endpoint, the unbound channel
 *                      of the other endpoint.
 */
void bulk_local_init_endpoint(struct bulk_local_endpoint *endpoint,
                              struct bulk_channel        *other_channel);


#endif /* BULK_LOCAL_H */

