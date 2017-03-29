/*
 *Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SFN5122F_CHANNEL_H_
#define SFN5122F_CHANNEL_H_



#include <string.h>
#include <stdlib.h>

#include <dev/sfn5122f_q_dev.h>
#include <dev/sfn5122f_dev.h>
#include "helper.h"
#include "sfn5122f_debug.h"

#define MTU_MAX 2048

struct sfn5122f_queue;

struct sfn5122f_queue_ops {
    errval_t (*update_txtail)(struct sfn5122f_queue*, void*, size_t);
    errval_t (*update_rxtail)(struct sfn5122f_queue*, void*, size_t);
};

struct region_entry {
    uint32_t rid;
    lpaddr_t phys;
    size_t size;
    uint64_t buftbl_idx;
    struct capref cap;
    struct region_entry* next;
};

struct sfn5122f_queue {
    union {
        sfn5122f_q_tx_user_desc_array_t* user;
        sfn5122f_q_tx_ker_desc_array_t* ker;
    } tx_ring;
    void**                          tx_opaque;
    struct devq_buf*                tx_bufs;
    uint16_t                        tx_head;
    uint16_t                        tx_tail;
    size_t                          tx_size;

    union {
        sfn5122f_q_rx_user_desc_array_t* user;
        sfn5122f_q_rx_ker_desc_array_t* ker;
    } rx_ring;
    void**                          rx_opaque;
    struct devq_buf*                rx_bufs;
    uint16_t                        rx_head;
    uint16_t                        rx_tail;
    uint16_t                        rx_size;

    sfn5122f_q_event_entry_array_t* ev_ring;
    void**                          ev_opaque;
    uint32_t                        ev_head;
    uint32_t                        ev_tail;
    size_t                          ev_size;

    struct sfn5122f_queue_ops       ops;
    void*                           opaque;
    bool                            userspace;

    // state for devif interface
    struct sfn5122f_devif_binding* b;
    struct sfn5122f_devif_rpc_client* rpc;
    bool bound;

    // Direct interface fields
    uint16_t id;
    sfn5122f_t *device;
    struct region_entry* regions;

    // TX envents might merge multiple TX descirptors
    void* bufs[32];
    uint8_t last;
    uint8_t num_left;
};

typedef struct sfn5122f_queue sfn5122f_queue_t;


static inline bool is_batched(size_t size, uint16_t tx_head, uint16_t q_tx_head)
{
    if (tx_head >= q_tx_head) {
        return (tx_head - q_tx_head > 0);
    } else {
        return (((tx_head + size) - q_tx_head) > 0);
    }
}

static inline sfn5122f_queue_t* sfn5122f_queue_init(void* tx, 
                                                    size_t tx_size,
                                                    void* rx, 
                                                    size_t rx_size, 
                                                    void* ev, 
                                                    size_t ev_size,
                                                    struct sfn5122f_queue_ops* ops, 
                                                    void* opaque, bool userspace)
{

    sfn5122f_queue_t* q = malloc(sizeof(*q));

    if (userspace) {
        q->tx_ring.user = tx;
    } else {
        q->tx_ring.ker = tx;
    }
    q->tx_opaque = malloc(sizeof(void*) * tx_size);
    q->tx_bufs = malloc(sizeof(struct devq_buf) * tx_size);
    q->tx_head = 0;
    q->tx_tail = 0;
    q->tx_size = tx_size;

    if (userspace) {
        q->rx_ring.user = rx;
    } else {
        q->rx_ring.ker = rx;
    }
    q->rx_opaque = malloc(sizeof(void*) * rx_size);
    q->rx_bufs = malloc(sizeof(struct devq_buf) * rx_size);
    q->rx_head = 0;
    q->rx_tail = 0;
    q->rx_size = rx_size;
  
    q->ev_ring = ev;
    q->ev_head = 0;
    q->ev_tail = 0;
    q->ev_size = ev_size;
    q->userspace = userspace; 

    q -> ops = *ops;
    if(!userspace) {
        q->opaque = opaque;
    }

    // Initialize ring memory with 0xff
    if(!userspace){
       memset(tx, 0xff, tx_size * sfn5122f_q_tx_ker_desc_size);
       memset(rx, 0xff, rx_size * sfn5122f_q_rx_ker_desc_size);
    }else{
       memset(tx, 0xff, tx_size * sfn5122f_q_tx_user_desc_size);
       memset(rx, 0xff, rx_size * sfn5122f_q_rx_user_desc_size);
    }
    /* all 0 is potential valid event */
    memset(ev, 0xff, ev_size * sfn5122f_q_event_entry_size);
    return q;
}

static inline uint8_t sfn5122f_get_event_code(sfn5122f_queue_t* queue)
{             
       sfn5122f_q_event_entry_t ev;
       ev = queue->ev_ring[queue->ev_head];
       return sfn5122f_q_event_entry_ev_code_extract(ev);
}


static inline errval_t sfn5122f_queue_bump_txtail(sfn5122f_queue_t* q)
{
    return q->ops.update_txtail(q, q->opaque, q->tx_tail);
}


static inline errval_t sfn5122f_queue_bump_rxtail(sfn5122f_queue_t* q)
{
    return q->ops.update_rxtail(q, q->opaque, q->rx_tail);
}


static inline errval_t sfn5122f_handle_drv_ev(sfn5122f_queue_t* q, uint16_t n)
{   
    size_t ev_head = q->ev_head;

    sfn5122f_q_event_entry_t code;
    code = q->ev_ring[ev_head]; 

    if (sfn5122f_q_driver_ev_driver_ev_subcode_extract(code) == 2) {
        printf("Event queue init done %d \n", n);
    }

    if (sfn5122f_q_driver_ev_driver_ev_subcode_extract(code) == 9) {
        printf("Packet neither TCP nor UPD %d \n", n);
    }
    
    if (sfn5122f_q_driver_ev_driver_ev_subcode_extract(code) == 14) {
        printf("RX error %d \n", n);
        return NIC_ERR_RX_PKT;
    }

    if (sfn5122f_q_driver_ev_driver_ev_subcode_extract(code) == 15) {
        printf("TX error %d \n", n);
        return NIC_ERR_TX_PKT;
    }

    memset(code, 0xff, sfn5122f_q_event_entry_size);
    return SYS_ERR_OK;

}



static inline errval_t sfn5122f_queue_handle_mcdi_event(sfn5122f_queue_t* q)
{
    // TODO handle different events    
    size_t ev_head = q->ev_head;
    sfn5122f_q_event_entry_t ev;
    uint64_t reg;
    ev = q->ev_ring[ev_head]; 
    reg = sfn5122f_q_event_entry_ev_data_extract(ev);
    memset(ev, 0xff, sfn5122f_q_event_entry_size);

    return SYS_ERR_OK;

}

/*    RX      */
static inline int sfn5122f_queue_add_rxbuf(sfn5122f_queue_t* q, 
                                           uint64_t phys,
                                           void* opaque)
{
    sfn5122f_q_rx_ker_desc_t d;
    size_t tail = q->rx_tail;

    q->rx_opaque[tail] = opaque;
    d = q->rx_ring.ker[tail];

    sfn5122f_q_rx_ker_desc_rx_ker_buf_addr_insert(d, phys);
    sfn5122f_q_rx_ker_desc_rx_ker_buf_region_insert(d, 0);
    // TODO: Check size
    sfn5122f_q_rx_ker_desc_rx_ker_buf_size_insert(d, MTU_MAX);
    q->rx_tail = (tail + 1) % q->rx_size;
    return 0;
}

static inline int sfn5122f_queue_add_user_rxbuf(sfn5122f_queue_t* q, 
                                                uint32_t buf_id,
                                                uint16_t offset)
{
    sfn5122f_q_rx_user_desc_t d;
    size_t tail = q->rx_tail;

    d = q->rx_ring.user[tail];

    sfn5122f_q_rx_user_desc_rx_user_buf_id_insert(d, buf_id);
    sfn5122f_q_rx_user_desc_rx_user_2byte_offset_insert(d, offset);
    q->rx_tail = (tail + 1) % q->rx_size;
    return 0;
}

static inline errval_t sfn5122f_queue_handle_rx_ev(sfn5122f_queue_t* q, 
                                                   void** opaque, 
                                                   size_t* len)
{   
    /*  Only one event is generated even if there is more than one
        descriptor per packet  */
    size_t ev_head = q->ev_head;
    size_t rx_head;
    sfn5122f_q_rx_ev_t ev;
    sfn5122f_q_rx_ker_desc_t d = 0;
    sfn5122f_q_rx_user_desc_t d_user = 0;

    ev = q->ev_ring[ev_head];
    rx_head = sfn5122f_q_rx_ev_rx_ev_desc_ptr_extract(ev);

    if(!sfn5122f_q_rx_ev_rx_ev_pkt_ok_extract(ev)) {   
         // TODO error handling
         q->rx_head = (rx_head + 1) % q->rx_size;
         if (sfn5122f_q_rx_ev_rx_ev_tobe_disc_extract(ev)) {
            // packet discared by softare -> ok
            return NIC_ERR_RX_DISCARD;
         }

         if (sfn5122f_q_rx_ev_rx_ev_frm_trunc_extract(ev)) {
            //printf("Packet truncated \n");
         }

         if (sfn5122f_q_rx_ev_rx_ev_pkt_not_parsed_extract(ev)) {
            DEBUG_QUEUE("Packet not parsed\n");
         }

         if (sfn5122f_q_rx_ev_rx_ev_ip_frag_err_extract(ev)) {
            DEBUG_QUEUE("Packet IP header err\n");
         }

         if (sfn5122f_q_rx_ev_rx_ev_ip_hdr_chksum_err_extract(ev)) {
            DEBUG_QUEUE("Packet IP header checksum err\n");
         }

         if (sfn5122f_q_rx_ev_rx_ev_tcp_udp_chksum_err_extract(ev)) {
            DEBUG_QUEUE("Packet TCP/UPD checksum err\n");
         }

         if (sfn5122f_q_rx_ev_rx_ev_eth_crc_err_extract(ev)) {
            DEBUG_QUEUE("Packet Ethernet CRC err\n");
         }

         DEBUG_QUEUE("Packet not ok \n");
         return NIC_ERR_RX_PKT;
    }

    *len = sfn5122f_q_rx_ev_rx_ev_byte_ctn_extract(ev);
    /* Length of 0 is treated as 16384 bytes */
    if (*len == 0) {
        *len = 16384;
    }

    if (q->userspace) {
        d_user = q->rx_ring.user[rx_head];  
    } else {
        d = q->rx_ring.ker[rx_head];
    }

    *opaque = q->rx_opaque[rx_head]; 

    memset(ev, 0xff, sfn5122f_q_event_entry_size);
    if (q->userspace) {
        memset(d_user, 0 , sfn5122f_q_rx_user_desc_size);
    } else {
        memset(d, 0 , sfn5122f_q_rx_ker_desc_size);
    }

    q->rx_head = (rx_head + 1) % q->rx_size;
    return SYS_ERR_OK;
}

static inline void sfn5122f_queue_bump_evhead(sfn5122f_queue_t* q)
{     
     q->ev_head = (q->ev_head +1) % q->ev_size;
}

static inline size_t sfn5122f_queue_free_rxslots(sfn5122f_queue_t* q)
{
    size_t head = q->rx_head;
    size_t tail = q->rx_tail;
    size_t size = q->rx_size;

    if (tail >= head) {
        return size - (tail - head) -1; 
    } else {
        return size - (tail + size - head) -1; 
    }
}


/*   TX       */
static inline size_t sfn5122f_queue_free_txslots(sfn5122f_queue_t* q)
{
    size_t head = q->tx_head;
    size_t tail = q->tx_tail;
    size_t size = q->tx_size;

    if (tail >= head) {
        return size - (tail - head) - 1; 
    } else {
        return size - (tail + size - head) - 1; 
    }

}


static inline errval_t sfn5122f_queue_handle_tx_ev(sfn5122f_queue_t* q)
{   
    /*  Only one event is generated even if there is more than one
        descriptor per packet  */
    size_t ev_head = q->ev_head;
    size_t tx_head;
    sfn5122f_q_tx_ev_t ev;
    sfn5122f_q_tx_ker_desc_t d = 0;
    

    ev = q->ev_ring[ev_head];
    tx_head = sfn5122f_q_tx_ev_tx_ev_desc_ptr_extract(ev);

    if(sfn5122f_q_tx_ev_tx_ev_pkt_err_extract(ev)){     
           //TODO error handling
           return NIC_ERR_TX_PKT;
    }


    if (sfn5122f_q_tx_ev_tx_ev_comp_extract(ev) == 1){  
        // TX Event is a batch
        if (is_batched(q->tx_size, tx_head, q->tx_head)) {
            uint8_t index = 0;
            q->num_left = 0;
            d = q->tx_ring.ker[q->tx_head];  
            while (q->tx_head != (tx_head + 1) % q->tx_size ) {
                q->bufs[index] = q->tx_opaque[q->tx_head];
                index++;
                q->tx_head = (q->tx_head + 1) % q->tx_size;
                q->num_left++;
            }          
            memset(d, 0 , sfn5122f_q_tx_ker_desc_size*q->num_left);
        } else { // Singe descriptor
            d = q->tx_ring.ker[tx_head];  
            q->num_left = 1;
            q->bufs[0] = q->tx_opaque[tx_head];
            memset(d, 0 , sfn5122f_q_tx_ker_desc_size);
        }

        // reset entry event in queue
        memset(ev, 0xff, sfn5122f_q_event_entry_size);
        q->tx_head = (tx_head +1) % q->tx_size;
    }

    return SYS_ERR_OK;
}

static inline int sfn5122f_queue_add_txbuf(sfn5122f_queue_t* q, 
                                           uint64_t phys,
                                           size_t len, 
                                           void* opaque, 
                                           int last)
{
    sfn5122f_q_tx_ker_desc_t d;
    size_t tail = q->tx_tail;

    q->tx_opaque[tail] = opaque;
    d = q->tx_ring.ker[tail];
   
    sfn5122f_q_tx_ker_desc_tx_ker_buf_addr_insert(d, phys);
    sfn5122f_q_tx_ker_desc_tx_ker_byte_count_insert(d, len);
    sfn5122f_q_tx_ker_desc_tx_ker_cont_insert(d, !(last == 1));
    sfn5122f_q_tx_ker_desc_tx_ker_buf_region_insert(d, 0);

    __sync_synchronize();
 
    q->tx_tail = (tail + 1) % q->tx_size;
    return 0;
}

#endif 

