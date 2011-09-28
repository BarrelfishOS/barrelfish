/**
 * \file
 * \brief Buffer memory for LWIP using caps
 *
 * This file provides caps for the buffers in LWIP. Needed because the network
 * stack should pass a cap to a buffer to the network device driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/waitset.h>

#include <assert.h>
#include <stdlib.h>
#include "lwip/pbuf.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"

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

struct pbuf_desc {
    struct pbuf *p;
    uint64_t pbuf_id;
};

struct pbuf_desc pbufs[NR_PREALLOCATED_PBUFS];




//create a frame cap with at least size 'size', map it to vspace
//and remember the corresponding cap. This function is used
//to initialize (get memory) for LWIP's heaps.

uint8_t *mem_barrelfish_alloc_and_register(uint8_t binding_index, uint32_t size)
{
    errval_t err;
    struct bulk_transfer bt_packet;

    LWIPBF_DEBUG("@@@@@@ mem alloc %"PRIx32" for index %d\n", size, binding_index);


    struct buffer_desc *tmp = (struct buffer_desc*)
                                malloc(sizeof(struct buffer_desc));
    assert(tmp != 0);
    LWIPBF_DEBUG("allocating %"PRIx32" bytes of memory for index %u.\n",
            size, binding_index);

    LWIPBF_DEBUG("memp pbuf %x, pool size %x\n", MEMP_NUM_PBUF, PBUF_POOL_SIZE);
    LWIPBF_DEBUG("allocating %"PRIx32" bytes of memory.\n", size);
#if defined(__scc__) && !defined(RCK_EMU)
    err = bulk_create(size, PBUF_PKT_SIZE, &(tmp->cap), &bt_packet, true);
#else
    err = bulk_create(size, PBUF_PKT_SIZE, &(tmp->cap), &bt_packet, false);
#endif // defined(__scc__) && !defined(RCK_EMU)
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "bulk_create failed.");
        return NULL;
    }
    LWIPBF_DEBUG("bulk_create success!!!\n");


    tmp->va = bt_packet.mem;

    struct frame_identity f;
    printf("invoke frame identity\n");
    err = invoke_frame_identify(tmp->cap, &f);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame_identify failed");
        return NULL;
    }
    printf("after invoke frame identity\n");
    tmp->pa = f.base;
    tmp->size = (1 << f.bits);
    tmp->buffer_id = -1;	/* shows that buffer id is not yet known */

    /* Put this new buffer on the top of buffers list */
    tmp->next = buffer_list;
    buffer_list = tmp;
    /* FIXME: should buffer be also put in the client_closure_NC? */
    printf("idc_register_buffer with index %d\n", binding_index);
    idc_register_buffer(tmp, binding_index);
    printf("after idc_register_buffer\n");

//    struct waitset *ws = get_default_waitset();
    /* Wait for actually getting the ID back from driver */
    while (tmp->buffer_id == -1) {
        extern struct waitset *lwip_waitset; // XXX: idc_barrelfish.c
        printf("###event_dispatch tmp->buffer_id %d\n", tmp->buffer_id);
        err = event_dispatch(lwip_waitset);
//        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch on LWIP waitset");
        }
        printf("event_dispatch looping again!\n", tmp->buffer_id);
    }
    printf("mem_barrelfish_alloc_and_register: about to return\n");
    return((uint8_t *)tmp->va);
}

/* for given pointer p, find out the details of the buffer in which it lies. */
struct buffer_desc *mem_barrelfish_get_buffer_desc(void *p)
{
    struct buffer_desc *tmp = buffer_list;

    while ((tmp != 0) && (((uintptr_t)p < (uintptr_t)tmp->va)
                          || ((uintptr_t)p > ((uintptr_t)tmp->va + tmp->size)))) {
                tmp = tmp->next;
    }
    return tmp;

}

void mem_barrelfish_pbuf_init(void)
{
    struct pbuf *p;
    ptrdiff_t offset = 0;
    int i = 0;
    struct buffer_desc *buff_ptr;
    printf("Allocating %d pbufs\n", NR_PREALLOCATED_PBUFS);

    for (i = 0; i < NR_PREALLOCATED_PBUFS; i++) {
        /* We allocate a pbuf chain of pbufs from the pool. */
        p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);
        assert(p != 0);
        if(p->next != 0) {
            printf("Error in allocating %d'th pbuf\n", i);
        }
        assert(p->next == 0); //make sure there is no chain for now...
        assert(p->tot_len == RECEIVE_PBUF_SIZE);
        assert(p->len == RECEIVE_PBUF_SIZE);

        //ignore the len returned here, because this is the len of the whole
        //static buffer (the frame cap size in bytes), not the pbuf
        buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
			//, &buffer_id, &paddr, &len, &offset, &vaddr);
        offset = (uintptr_t)p->payload - (uintptr_t)(buff_ptr->va);

        pbufs[i].p = p;
        pbufs[i].pbuf_id = i;

        /* NOTE: comment it as it will occur with large frequencey */
/*        LWIPBF_DEBUG("pbuf %lu is from buff %lu -------\n",
        		pbufs[i].pbuf_id, buff_ptr->buffer_id);
*/

        //XXX: the msg handler should free the pbuf in case of an error
        //uint64_t r = idc_register_pbuf(i, paddr + offset, p->len);
//        printf("############## register pbuf %d\n", i);
        idc_register_pbuf(i, buff_ptr->pa + offset, p->len);
    }
    LWIPBF_DEBUG("pbuf is from buff %"PRIx64" -------\n", buff_ptr->buffer_id);
    LWIPBF_DEBUG("Registered %d no. of pbufs for receiving -------\n", i);
    printf("pbuf is from buff %"PRIx64" -------\n", buff_ptr->buffer_id);
    printf("Registered %d no. of pbufs for receiving -------\n", i);

}


//XXX: asq: Big parts of this code are duplicated (see above).
void mem_barrelfish_replace_pbuf(uint64_t idx)
{
    ptrdiff_t offset = 0;
    struct buffer_desc *buff_ptr;

//    LWIPBF_DEBUG("Sending pbuf back for reuse ++++++++\n");
        struct pbuf *p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);
        assert(p != 0);
        assert(p->next == 0); //make sure there is no chain for now...
        assert(p->tot_len == RECEIVE_PBUF_SIZE);
        assert(p->len == RECEIVE_PBUF_SIZE);

        //ignore the len returned here, because this is the len of the whole
        //static buffer (the frame cap size in bytes), not the pbuf
        buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
        offset = (uintptr_t)p->payload - (uintptr_t)(buff_ptr->va);

        pbufs[idx].p = p;
        pbufs[idx].pbuf_id = idx;
        //XXX: the msg handler should free the pbuf in case of an error.
        //uint64_t r = idc_register_pbuf(idx, paddr + offset, p->len);
        idc_register_pbuf(idx, buff_ptr->pa + offset, p->len);
/*
        if (r != 0) {
            //registering the pbuf in the network driver failed. So free it again.
            pbuf_free(p);
            pbufs[idx].p = 0;
        }
*/
}

struct pbuf *mem_barrelfish_get_pbuf(uint64_t pbuf_id)
{
    return (pbufs[pbuf_id].p);
}
