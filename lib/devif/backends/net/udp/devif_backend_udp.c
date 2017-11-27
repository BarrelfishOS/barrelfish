/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätstrasse 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <devif/queue_interface.h>
#include <devif/queue_interface_backend.h>
#include <devif/backends/net/udp.h>
#include <lwip/inet_chksum.h>
#include <lwip/lwip/inet.h>
#include <net_interfaces/flags.h>
#include <net/net.h>
#include <net/net_filter.h>
#include <net/dhcp.h>
#include "../headers.h"

#define MAX_NUM_REGIONS 64

//#define DEBUG_ENABLED

#if defined(DEBUG_ENABLED) 
#define DEBUG(x...) do { printf("UDP_QUEUE: %s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DEBUG(x...) ((void)0)
#endif 

struct region_vaddr {
    void* va;
    regionid_t rid;
};

struct udp_q {
    struct devq my_q;
    struct devq* q;
    struct udp_hdr header; // can fill in this header and reuse it by copying
    struct region_vaddr regions[MAX_NUM_REGIONS];
    struct net_filter_state* filter;
};


#ifdef DEBUG_ENABLED
static void print_buffer(struct udp_q* q, void* start, uint64_t len)
{
    uint8_t* buf = (uint8_t*) start;
    printf("Packet in region at address %p len %zu \n",
           buf, len);
    for (int i = 0; i < len; i+=2) {
        if (((i % 16) == 0) && i > 0) {
            printf("\n");
        }
        printf("%2X", buf[i]);
        printf("%2X ", buf[i+1]);
    }
    printf("\n");
}
#endif

static errval_t udp_register(struct devq* q, struct capref cap,
                            regionid_t rid) 
{
       
    errval_t err;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };

    struct udp_q* que = (struct udp_q*) q;

    // Map device registers
    invoke_frame_identify(cap, &frameid);

    err = vspace_map_one_frame_attr(&que->regions[rid % MAX_NUM_REGIONS].va, 
                                    frameid.bytes, cap, VREGION_FLAGS_READ_WRITE, 
                                    NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }
    que->regions[rid % MAX_NUM_REGIONS].rid = rid;
    DEBUG("id-%d va-%p \n", que->regions[rid % MAX_NUM_REGIONS].rid, 
          que->regions[rid % MAX_NUM_REGIONS].va);

    return que->q->f.reg(que->q, cap, rid);
}

static errval_t udp_deregister(struct devq* q, regionid_t rid) 
{
    
    struct udp_q* que = (struct udp_q*) q;
    que->regions[rid % MAX_NUM_REGIONS].va = NULL;
    que->regions[rid % MAX_NUM_REGIONS].rid = 0;
    return que->q->f.dereg(que->q, rid);
}


static errval_t udp_control(struct devq* q, uint64_t cmd, uint64_t value,
                           uint64_t* result)
{
    struct udp_q* que = (struct udp_q*) q;
    return que->q->f.ctrl(que->q, cmd, value, result);
}


static errval_t udp_notify(struct devq* q)
{
    struct udp_q* que = (struct udp_q*) q;
    return que->q->f.notify(que->q);
}

static errval_t udp_enqueue(struct devq* q, regionid_t rid, 
                           genoffset_t offset, genoffset_t length,
                           genoffset_t valid_data, genoffset_t valid_length,
                           uint64_t flags)
{

    // for now limit length
    //  TODO fragmentation

    struct udp_q* que = (struct udp_q*) q;
    if (flags & NETIF_TXFLAG) {
        
        DEBUG("TX rid: %d offset %ld length %ld valid_length %ld \n", rid, offset, 
              length, valid_length);
        assert(valid_length <= 1500);    
        que->header.len = htons(valid_length + UDP_HLEN);

        assert(que->regions[rid % MAX_NUM_REGIONS].va != NULL);

        uint8_t* start = (uint8_t*) que->regions[rid % MAX_NUM_REGIONS].va + 
                         offset + valid_data + ETH_HLEN + IP_HLEN;   

        memcpy(start, &que->header, sizeof(que->header));   

        return que->q->f.enq(que->q, rid, offset, length, valid_data, 
                             valid_length + UDP_HLEN, flags);
    } 

    if (flags & NETIF_RXFLAG) {
        assert(valid_length <= 2048);    
        DEBUG("RX rid: %d offset %ld length %ld valid_length %ld \n", rid, offset, 
              length, valid_length);
        return que->q->f.enq(que->q, rid, offset, length, valid_data, 
                             valid_length, flags);
    } 

    return NET_QUEUE_ERR_UNKNOWN_BUF_TYPE;
}

static errval_t udp_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                           genoffset_t* length, genoffset_t* valid_data,
                           genoffset_t* valid_length, uint64_t* flags)
{
    errval_t err;
    struct udp_q* que = (struct udp_q*) q;

    err = que->q->f.deq(que->q, rid, offset, length, valid_data, valid_length, flags);
    if (err_is_fail(err)) {    
        return err;
    }

    if (*flags & NETIF_RXFLAG) {
        DEBUG("RX rid: %d offset %ld valid_data %ld length %ld va %p \n", *rid, 
              *offset, *valid_data, 
              *valid_length, que->regions[*rid % MAX_NUM_REGIONS].va + *offset + *valid_data);

        struct udp_hdr* header = (struct udp_hdr*) 
                                 (que->regions[*rid % MAX_NUM_REGIONS].va +
                                 *offset + *valid_data);
 
        // Correct port for this queue?
        if (header->dest != que->header.dest) {
            printf("UDP queue: dropping packet, wrong port %d %d \n",
                   header->dest, que->header.dest);
            err = que->q->f.enq(que->q, *rid, *offset, *length, 0, 0, NETIF_RXFLAG);
            return err_push(err, NET_QUEUE_ERR_WRONG_PORT);
        }
        
#ifdef DEBUG_ENABLED
        print_buffer(que, que->regions[*rid % MAX_NUM_REGIONS].va + *offset, *valid_length);
#endif

        *valid_length = ntohs(header->len) - UDP_HLEN;
        *valid_data += UDP_HLEN;
        //print_buffer(que, que->regions[*rid % MAX_NUM_REGIONS].va + *offset+ *valid_data, *valid_length);
        return SYS_ERR_OK;
    }

#ifdef DEBUG_ENABLED
    DEBUG("TX rid: %d offset %ld length %ld \n", *rid, *offset, 
          *valid_length);
#endif

    return SYS_ERR_OK;
}

/*
 * Public functions
 *
 */
errval_t udp_create(struct udp_q** q, const char* card_name, 
                    uint16_t src_port, uint16_t dst_port,
                    uint32_t dst_ip, void(*interrupt)(void*), bool poll)
{
    errval_t err;
    struct udp_q* que;
    que = calloc(1, sizeof(struct udp_q));
    assert(que);

    uint32_t src_ip;
    err = net_config_current_ip_query(NET_FLAGS_BLOCKING_INIT, &src_ip);
    if (err_is_fail(err)) {
        return err;
    }

    // init other queue
    uint64_t qid;
    err = ip_create((struct ip_q**) &que->q, card_name, &qid, UDP_PROT, dst_ip, 
                    interrupt, poll);
    if (err_is_fail(err)) {
        return err;
    }

    err = net_filter_init(&que->filter, card_name);
    if (err_is_fail(err)) {
        return err;
    }  
    
    src_ip = htonl(src_ip);
    struct net_filter_ip ip = {
        .qid = qid,
        .ip_src = dst_ip,
        .ip_dst = src_ip,
        .port_dst = dst_port,
        .type = NET_FILTER_UDP,    
    };

    err = net_filter_ip_install(que->filter, &ip);
    if (err_is_fail(err)) {
        return err;
    }

    err = devq_init(&que->my_q, false);
    if (err_is_fail(err)) {
        errval_t err2;
        err2 = net_filter_ip_remove(que->filter, &ip);
        if (err_is_fail(err)) {
            return err_push(err2, err);
        }
        return err;
    }   

    // UDP fields
    que->header.src = htons(src_port);
    que->header.dest = htons(dst_port);
    que->header.chksum = 0x0;

    que->my_q.f.reg = udp_register;
    que->my_q.f.dereg = udp_deregister;
    que->my_q.f.ctrl = udp_control;
    que->my_q.f.notify = udp_notify;
    que->my_q.f.enq = udp_enqueue;
    que->my_q.f.deq = udp_dequeue;
    *q = que;

    return SYS_ERR_OK;
}

errval_t udp_destroy(struct udp_q* q)
{
    // TODO destroy q->q;
    free(q);    

    return SYS_ERR_OK;
}

errval_t udp_write_buffer(struct udp_q* q, regionid_t rid, genoffset_t offset,
                          void* data, uint16_t len) 
{
    assert(len <= 1500);
    if (q->regions[rid % MAX_NUM_REGIONS].va != NULL) {
        uint8_t* start = q->regions[rid % MAX_NUM_REGIONS].va + offset 
                         + sizeof (struct udp_hdr) 
                         + sizeof (struct ip_hdr)
                         + sizeof (struct eth_hdr);
        memcpy(start, data, len);
        return SYS_ERR_OK;
    } else {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }
}



