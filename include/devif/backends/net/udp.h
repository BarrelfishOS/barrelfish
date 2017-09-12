/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVIF_UDP_H_
#define DEVIF_UDP_H_ 1


#include <barrelfish/barrelfish.h>
#include <devif/backends/net/ip.h>

struct udp_q;

/**
 *  @param q        udp queue to destroy
 */
errval_t udp_destroy(struct udp_q* q);

/**
 * @brief initalized a queue that can send UPD packets with a certain requirements.
 *        all other packets received on this queue will be dropped.
 *
 * @param q            udp queue return value
 * @param card_name    the card name from which a hardware queue will be used
 *                      to send UPD packets. Internally a queue to the device with
 *                      the card_name will be initalized
 * @param src_port     UDP source port
 * @param dst_port     UPD destination port
 * @param dst_ip       Destination IP
 * @param interrupt    Interrupt handler
 * @param poll         If the queue is polled or should use interrupts             
 *
 */
errval_t udp_create(struct udp_q** q, const char* card_name, 
                    uint16_t src_port, uint16_t dst_port,
                    uint32_t dst_ip, void(*interrupt)(void*), 
                    bool poll);
/*
 * @brief  Writes into a buffer so that we still have space to add the headers
 *
 * @param q           udp queue to which the region was registered to
 * @param rid         The region ID in which the buffer to write to is contained
 * @param offset      The offset into the region at which the buffer start
 * @param data        The data to write into the buffer
 * @param len         The length of the data
 * 
 */
errval_t udp_write_buffer(struct udp_q* q, regionid_t rid, genoffset_t offset,
                          void* data, uint16_t len);
#endif /* DEVIF_UDP_H_ */
