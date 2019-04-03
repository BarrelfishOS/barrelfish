/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef E10K_DEVIF_H_
#define E10K_DEVIF_H_ 1

struct e10k_queue;
typedef void (*e10k_event_cb_t)(void* q);


/**
* @brief create a handle to a queue of an e10k card. Assumes the driver is
*        running
*
* @param q             Return pointer to the device queue
* @param cb            Callback function when an interrupt is raised
* @param ep            Endpoint to physical function, possibly NULL
* @param bus           PCI bus number
* @param function      PCI function number
* @param devid         PCI device id
* @param dev           PCI device number
* @param use_vf        Start virtual functions, otherwise the physical function
*                      will be used
* @param interrupts    Use interrupts, otherwise polling
* @param default_q     Request a handle to the default queue where
*                      all the not filtered packes go to. Mostly use when
*                      a queue is request from the driver itself.
*
* @returns error on NIC_ERR_ALLOC or SYS_ERR_OK on success
*
*/
errval_t e10k_queue_create(struct e10k_queue** q, e10k_event_cb_t cb, struct capref* ep,
                           uint32_t bus, uint32_t function, uint32_t devid, uint32_t dev,
                           bool use_vf, bool interrupts, bool default_q);

/**
* @brief get the queues endpoint to the netfilter interface
*
* @param q             handl to the device queue
* @param ep            Endpoint cap to netfilter interface of this card
*
* @returns the queues id
*
*/
void e10k_queue_get_netfilter_ep(struct e10k_queue* q, struct capref* ep);


/**
* @brief get the queue id of this queue handle. The returned id corresponds
*        to the real id used on the card
*
* @param q             handl to the device queue
*
* @returns the queues id
*
*/
uint64_t e10k_queue_get_id(struct e10k_queue* q);

struct bench_ctl* e10k_get_benchmark_data(struct devq* q, uint8_t type);

#endif
