/**
 * @brief
 *  client.h
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef QUEUE_CLIENT_INCLUDE_H_
#define QUEUE_CLIENT_INCLUDE_H_


struct queue_service_client;


/*
 * ==============================================================================
 * Library Initialization
 * ==============================================================================
 */
/**
 * @brief initializes the queue service client using an endpoint
 *
 * @param q            returned queue service client state handle
 * @param ep           the queue service endpoint to connect to
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */

errval_t queue_service_client_init_with_ep(struct queue_service_client** cl,
                                           struct capref* ep);

/**
 * @brief initializes the queue service client using the nameservice
 *
 * @param q            returned queue service client state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_client_init(struct queue_service_client** cl);

/**
 * @brief requests a endpoint to a NIC by name. Using the endpoint the queue
 *        itself can be initalized.
 *
 * @param cl            queue service client
 * @param name          name of the service we want an endpont from.
 * @param ep            returned endpoint, slot has to be allocated beforehand
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_client_request_ep_by_name(struct queue_service_client* cl,
                                                 char* name, struct capref* ep);


#endif /* QUEUE_CLIENT_INCLUDE_H_ */
