/**
 * @brief
 *  queue_service.h
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef QUEUE_SERIVCE_INCLUDE_H_
#define QUEUE_SERIVCE_INCLUDE_H_

struct queue_service_state;
struct driver_instance;
/*
 * ==============================================================================
 * Library Initialization
 * ==============================================================================
 */

/**
 * @brief initializes the queue service with a default name
 *
 * @param st            returned queue service state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_init_default(struct queue_service_state** st);


/**
 * @brief initializes the queue service with a provided name
 *
 * @param st            returned queue service state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_init(struct queue_service_state** st, char* name);

/*
 * ==============================================================================
 * Adding queue endpoint factory
 * ==============================================================================
 */

/**
 * @brief initializes the queue service with a provided name
 *
 * @param st            service state handle
 * @param name          factory name
 * @param core          the core on which the ep factory resides
 * @param factory       driver_instance struct of a endpoint factory 
 *                      (normally only available in kaluga)
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_add_ep_factory(struct queue_service_state* st, 
                                      char* factory_name,
                                      coreid_t core,
                                      struct driver_instance* factory);


/**
 * @brief Create an endpoint to the queue service itself
 *
 * @param st            service state handle
 * @param core          core on which the program requesting the ep runs
 * @param ep            returend endpoint of this queue service
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_create_self_ep(struct queue_service_state* st, 
                                      coreid_t core,
                                      struct capref* ep);

#endif /* QUEUE_SERIVCE_INCLUDE_H_ */
