/** \file
 *  \brief Virtual Function management for e10k
 */

/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __E10K_DEVIF_VF_H__
#define __E10K_DEVIF_VF_H__

struct vf_state;
struct e10k_queue;


 /*
  * The library is not fully stateless. During the initalization
  * call of a function there is a lot of state that can only
  * stored in global variables
  */ 

 /**
  * @brief Initializes a new virtual function of the 10k.
  *
  * @param pci_function     From which physical function this virtual
  *                         function should be initalized
  * @param interrupts       Enable interrupts
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
errval_t e10k_init_vf_driver(uint8_t pci_function, uint8_t seg, uint32_t bus,
                             uint32_t dev, uint32_t device_id, bool interrupts);

 /**
  * @brief Checks if the state of the library initalized
  *
  * @returns true if it is otherwise false
  */
bool e10k_vf_started(void);

 /**
  * @brief Checks if current active VF can still initalize another queue.
  *        The general assumption is that each VF can allocate 2 queues.
  *        Other configurations have to be specified in the PF driver.
  *
  * @returns true if it is otherwise false
  */
bool e10k_vf_can_create_queue(void);


 /**
  * @brief initalized queue on VF  in hardware
  *
  * @param queue     e10k queue struct with information about the mapped
  *                  memory that is used for DMA
  *
  * @returns SYS_ERR_OK on success otherwise failter
  */
errval_t e10k_vf_init_queue_hw(struct e10k_queue* q);

#endif
