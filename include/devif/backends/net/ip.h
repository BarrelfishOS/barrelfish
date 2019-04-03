/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVIF_IP_H_
#define DEVIF_IP_H_ 1


#include <barrelfish/barrelfish.h>

#define UDP_PROT 0
#define TCP_PROT 1

//#define BENCH 1
#ifdef BENCH
#define BENCH_SIZE 100000
#endif


struct bench_ctl; 
struct ip_q;
    
struct __attribute__((packed)) eth_addr {
  uint8_t addr[6];
};

/**
 *  @param q        ip queue to destroy
 */
errval_t ip_destroy(struct ip_q* q);

/**
 * @brief initalized a queue that can send IP packets with a certain requirements.
 *        all other packets received on this queue will be dropped.
 *
 * @param q            ip queue return value
 * @param card_name    the card name from which a hardware queue will be used
 *                      to send IP packets. Internally a queue to the device with
 *                      the card_name will be initalized
 * @param qid          the id of the hardware queue (used for filters)
 * @param prot         The protocol that is running on top of IP
 * @param dst_ip       Destination IP
 * @param interrupt    Interrupt handler
 * @param poll         If the queue is polled or should use interrupts             
 *
 */
errval_t ip_create(struct ip_q** q, const char* card_name, uint64_t* qid,
                   uint8_t prot, uint32_t dst_ip, void(*interrupt)(void*), 
                   bool poll);

void ip_get_netfilter_ep(struct ip_q* q, struct capref* ep);

struct bench_ctl* ip_get_benchmark_data(struct ip_q* q, uint8_t type);
#endif /* DEVIF_IP_H_ */
