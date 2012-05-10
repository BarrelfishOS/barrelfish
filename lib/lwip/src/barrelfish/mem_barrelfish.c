/**
 * \file
 * \brief Buffer memory for LWIP using caps
 *
 * This file provides caps for the buffers in LWIP. Needed because the network
 * stack should pass a cap to a buffer to the network device driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/waitset.h>
#include <contmng/netbench.h>

#include <assert.h>
#include <stdlib.h>
#include "lwip/init.h"
#include "lwip/def.h"
#include "lwip/pbuf.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"
#include <procon/procon.h>

#include "lwip_barrelfish_debug.h"



//XXX: the rx descriptor in the e1000 card always assumes a buffer of size 2048
//     bytes. So this works only if we assume that no packet longer than 1518
//     bytes will be received, otherwise a buffer overflow occurs. On the other
//     hand, we don't want smaller buffers and having the card split larger
//     packets to multiple buffers, because otherwise we'd had to create a
//     pbuf chain.
#define RECEIVE_PBUF_SIZE 1514

//LWIP only needs two heaps. It allocates memory and pbufs from these two heaps.
#define MAX_NR_BUFFERS 2

//remember the frame caps which point to the memory regions used
//to allocate memory and pbufs in LWIP


static struct buffer_desc *buffer_list = 0;
static struct buffer_desc *short_buf_list[2] = {NULL, NULL};
struct pbuf_desc {
    struct pbuf *p;
    uint64_t pbuf_id;
};

static struct pbuf_desc pbufs[RECEIVE_BUFFERS];


static void rx_populate_sp_pbuf(struct buffer_desc *buf)
{
    struct pbuf *p;
    ptrdiff_t offset = 0;
    uint64_t i = 0;
    struct buffer_desc *buff_ptr;

    uint64_t ts = rdtsc();
    sp_reload_regs(buf->spp_prv);
    printf("Allocating %"PRIu64" pbufs\n", buf->spp_prv->c_size);

//    sys_debug_set_breakpoint(&buffer_list, 1, 1);

    for (i = 0; i < buf->spp_prv->c_size; i++) {
        /* We allocate a pbuf chain of pbufs from the pool. */
        p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);
        if (p == 0) {
            printf("Error in allocating %"PRIu64"'th pbuf, no more pbufs\n",
                    i);
        }
        assert(p != 0);
        if (p->next != 0) {
            printf("Error in allocating %"PRIu64"'th pbuf\n", i);
        }
        assert(p->next == 0);   //make sure there is no chain for now...
        assert(p->tot_len == RECEIVE_PBUF_SIZE);
        assert(p->len == RECEIVE_PBUF_SIZE);
        //ignore the len returned here, because this is the len of the whole
        //static buffer (the frame cap size in bytes), not the pbuf
        buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
        assert(buf == buff_ptr);

        //, &buffer_id, &paddr, &len, &offset, &vaddr);
        offset = (uintptr_t) p->payload - (uintptr_t) (buf->va);

        pbufs[i].p = p;
        pbufs[i].pbuf_id = i;

        copy_data_into_slot(buf->spp_prv, buf->buffer_id, i, offset, p->len, 1,
                (uint64_t)p, ts);
    } // end for:
//    printf("pbuf is from buff %"PRIu64" -----\n", buf->buffer_id);
//    printf("Added %"PRIu64" no of pbufs for receiving in SP ---\n", i);

} // end function: put_pbufs_in_shared_pool


//create a frame cap with at least size 'size', map it to vspace
//and remember the corresponding cap. This function is used
//to initialize (get memory) for LWIP's heaps.
uint8_t *mem_barrelfish_alloc(uint8_t binding_index, uint32_t size)
{
    errval_t err;

    LWIPBF_DEBUG("@@@@@@ mem alloc %" PRIx32 " for index %d\n", size,
                 binding_index);

    struct buffer_desc *buf = (struct buffer_desc *)
      malloc(sizeof(struct buffer_desc));

    assert(buf != NULL);
    buf->role = binding_index;

    LWIPBF_DEBUG("allocating %" PRIx32 " bytes of memory for index %u.\n",
                 size, binding_index);
    LWIPBF_DEBUG("memp pbuf %x, pool size %x\n", MEMP_NUM_PBUF, PBUF_POOL_SIZE);
    LWIPBF_DEBUG("allocating %" PRIx32 " bytes of memory.\n", size);

    struct bulk_transfer bt_packet;
#ifdef __scc__
    err = bulk_create(size, PBUF_PKT_SIZE, &(buf->cap), &bt_packet, true);
#else
    err = bulk_create(size, PBUF_PKT_SIZE, &(buf->cap), &bt_packet, false);
#endif
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bulk_create failed.");
        return NULL;
    }
    LWIPBF_DEBUG("bulk_create success!!!\n");

    buf->va = bt_packet.mem;
    LWIPBF_DEBUG("mem_barrelfish_alloc: VA addr [%p]\n",
            buf->va);

    struct frame_identity f;

    err = invoke_frame_identify(buf->cap, &f);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_identify failed");
        return NULL;
    }
    buf->pa = f.base;
    buf->size = (1 << f.bits);
    buf->buffer_id = -1;        /* shows that buffer id is not yet known */

    /* Put this new buffer on the top of buffers list */
    buf->next = buffer_list;
    buffer_list = buf;

    // Creating shared_pool for communication
    buf->spp_prv = sp_create_shared_pool(RECEIVE_BUFFERS, buf->role);
    assert(buf->spp_prv != NULL);
    short_buf_list[binding_index] = buf;
    return ((uint8_t *) buf->va);
}

uint8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size)
{
    struct buffer_desc *buf = short_buf_list[binding_index];
    errval_t err;
    assert(buf != NULL);

    if (binding_index == RX_BUFFER_ID) {
        rx_populate_sp_pbuf(buf);
    }

    /* FIXME: should buffer be also put in the client_closure_NC? */
    idc_register_buffer(buf, buf->spp_prv, binding_index);

//    struct waitset *ws = get_default_waitset();
    /* Wait for actually getting the ID back from driver */
    while (buf->buffer_id == -1) {
        extern struct waitset *lwip_waitset;    // XXX: idc_barrelfish.c

        err = event_dispatch(lwip_waitset);
//        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch on LWIP waitset");
        }
    }
    return ((uint8_t *) buf->va);
}

/* for given pointer p, find out the details of the buffer in which it lies. */
struct buffer_desc *mem_barrelfish_get_buffer_desc(void *p)
{
    struct buffer_desc *tmp = buffer_list;

    while ((tmp != 0) && (((uintptr_t) p < (uintptr_t) tmp->va)
                          || ((uintptr_t) p >
                              ((uintptr_t) tmp->va + tmp->size)))) {
        tmp = tmp->next;
    }
    return tmp;
}


//XXX: asq: Big parts of this code are duplicated (see above).
//struct pbuf *mem_barrelfish_replace_pbuf(struct buffer_desc *buf, uint64_t idx)
struct pbuf *mem_barrelfish_replace_pbuf(uint64_t idx)
{
//    ptrdiff_t offset = 0;
    uint64_t offset = 0;
    struct buffer_desc *buff_ptr;
    struct slot_data new_slot;
    uint64_t ts = rdtsc();

    LWIPBF_DEBUG("mem_barrelfish_replace_pbuf %"PRIu64" ++++++++\n", idx);
    struct pbuf *p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);
    LWIPBF_DEBUG("Sending pbuf %p for reuse at id %"PRIu64" ++++++++\n",
            p, idx);

    assert(p != 0);
    assert(p->next == 0);       //make sure there is no chain for now...
    assert(p->tot_len == RECEIVE_PBUF_SIZE);
    assert(p->len == RECEIVE_PBUF_SIZE);

    //ignore the len returned here, because this is the len of the whole
    //static buffer (the frame cap size in bytes), not the pbuf
    buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
    assert(buff_ptr != NULL);
//    assert(buf == buff_ptr);
    offset = (uintptr_t) p->payload - (uintptr_t) (buff_ptr->va);

    new_slot.buffer_id = buff_ptr->buffer_id;
    new_slot.pbuf_id = idx;
    new_slot.offset = offset;
    new_slot.len = p->len;
    new_slot.no_pbufs = 1;
    new_slot.client_data = (uint64_t)p;
    new_slot.ts = rdtsc();
    //XXX: the msg handler should free the pbuf in case of an error.
    //uint64_t r = idc_register_pbuf(idx, paddr + offset, p->len);
    bool ret = sp_replace_slot(buff_ptr->spp_prv, &new_slot);
    if (!ret) {
        printf("Error: receive spp is empty and can't add more free slots\n");
        sp_print_metadata(buff_ptr->spp_prv);
        pbuf_free(p);
        return NULL;
    }
    buff_ptr->spp_prv->ghost_read_id = buff_ptr->spp_prv->c_read_id;
    netbench_record_event_simple(nb, RE_PBUF_REPLACE_1, ts);

    pbufs[idx].p = p;
    pbufs[idx].pbuf_id = idx;

    return ((struct pbuf *)new_slot.client_data);
}

struct pbuf *mem_barrelfish_get_pbuf(uint64_t pbuf_id)
{
    return (pbufs[pbuf_id].p);
}
