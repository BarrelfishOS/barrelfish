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
#include <barrelfish/sys_debug.h>
#include <barrelfish/waitset.h>

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


static void copy_data_into_slot_local(struct shared_pool_private *spp,
        uint64_t buf_id, uint64_t id, uint64_t offset, uint64_t len,
        uint64_t no_pbufs, uint64_t client_data, uint64_t ts)
{
    assert(id < spp->c_size);
    spp->sp->slot_list[id].d.buffer_id = buf_id;
    spp->sp->slot_list[id].d.no_pbufs = no_pbufs;
    spp->sp->slot_list[id].d.pbuf_id = id;
    spp->sp->slot_list[id].d.offset = offset;
    spp->sp->slot_list[id].d.len = len;
    spp->sp->slot_list[id].d.client_data = client_data;
    spp->sp->slot_list[id].d.ts = ts;
    // copy the s into shared_pool
#if !defined(__scc__) && !defined(__i386__)
    cache_flush_range(&spp->sp->slot_list[id], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
}



static void rx_populate_sp_pbuf(struct buffer_desc *buf)
{
    struct pbuf *p;
    ptrdiff_t offset = 0;
    uint64_t i = 0;
    struct buffer_desc *buff_ptr;
    volatile struct buffer_desc *buffer_list_bak = buffer_list;
    volatile void **pp;

    pp = (void *)&buffer_list;
    uint64_t ts = rdtsc();
    sp_reload_regs(buf->spp);
    printf("Allocating %"PRIu64" pbufs\n", buf->spp->c_size);

//    sys_debug_set_breakpoint(&buffer_list, 1, 1);

    printf("###### BP set!!! %u buffer_list [%p] sizeof(void *)[%lu]\n",
            __LINE__, buffer_list, sizeof(void *));
    printf("####  buf_list_addr [%p, %p], buf_list[%p, %p] spt[%p]\n",
                &buffer_list, pp, buffer_list, *pp, &ts);

    for (i = 0; i < buf->spp->c_size; i++) {
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
        assert(buffer_list_bak == buffer_list);
        //ignore the len returned here, because this is the len of the whole
        //static buffer (the frame cap size in bytes), not the pbuf
        buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
        assert(buf == buff_ptr);

        //, &buffer_id, &paddr, &len, &offset, &vaddr);
        offset = (uintptr_t) p->payload - (uintptr_t) (buf->va);

        pbufs[i].p = p;
        pbufs[i].pbuf_id = i;

        /* NOTE: comment it as it will occur with large frequencey */
/*        LWIPBF_DEBUG("pbuf %lu is from buff %lu -------\n",
        		pbufs[i].pbuf_id, buff_ptr->buffer_id);
*/
//        wait_for_receive_channel();
        //XXX: the msg handler should free the pbuf in case of an error
//        idc_register_pbuf(i, buff_ptr->pa + offset, p->len);
        assert(buffer_list_bak == buffer_list);
/*
        copy_data_into_slot(buf->spp, buf->buffer_id, i, offset, p->len, 1,
                (uint64_t)p, ts);
*/

        assert(buffer_list_bak == *pp);

        if (i == 2043) {
            int k = 44;
            assert(buffer_list_bak == *pp);
/*            printf("### i = %"PRIu64", sp %p, and heap %p\n",i,
                    &k, &buffer_list);
*/
            printf("hello world\n");
//            assert(buffer_list_bak == *pp);
        }

        if ( buffer_list_bak != *pp) {
            printf("assert: i = %"PRIu64", orig %p, new %p, addr %p\n",
                    i, buffer_list_bak, *pp, pp);

        }
        assert(buffer_list_bak == *pp);
        copy_data_into_slot_dbg(buf->spp, buf->buffer_id, i, offset, p->len,
                1, (uint64_t)p, ts, (void *)&buffer_list,
                (void *)buffer_list_bak);
        /*
    struct shared_pool_private *spp = buf->spp;
    uint64_t id = i;
    spp->sp->slot_list[id].d.buffer_id = buf->buffer_id;
    spp->sp->slot_list[id].d.no_pbufs = 1;
    spp->sp->slot_list[id].d.pbuf_id = id;
    spp->sp->slot_list[id].d.offset = offset;
    spp->sp->slot_list[id].d.len = p->len;
    spp->sp->slot_list[id].d.client_data = (uint64_t)p;
    spp->sp->slot_list[id].d.ts = ts;
    // copy the s into shared_pool
#if !defined(__scc__) && !defined(__i386__)
    cache_flush_range(&spp->sp->slot_list[id], SLOT_SIZE);
#endif // !defined(__scc__) && !defined(__i386__)
*/
        if (buffer_list_bak != buffer_list) {
            printf("#### buffer_list overwritten %"PRIu64"\n", i);
            printf("spp has %"PRIu64", %"PRIu64" slots\n", buf->spp->c_size,
                    buf->spp->sp->size_reg.value);
            printf("slot range [%p %p] \n",
                    &buf->spp->sp->slot_list[i],
                   &buf->spp->sp->slot_list[i+1]);
            printf("overwritten addr [%p] to %p from %p \n",
                   &buffer_list, buffer_list,  buffer_list_bak);
            printf("##### sizeof spp[%lu], sizeof sp[%lu]\n",
                    sizeof(struct shared_pool_private),
                    sizeof(struct shared_pool) );

            printf("##### buffer_list addr [%p]\n", &buffer_list);
        }
        assert(buffer_list_bak == buffer_list);
    } // end for:
    printf("pbuf is from buff %"PRIx64" -----\n", buf->buffer_id);
    printf("Added %"PRIx64" no of pbufs for receiving in SP ---\n", i);
    printf("### success ### %u buffer_list  [%p], [%p]\n", __LINE__, buffer_list,
            buffer_list_bak);
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
#if defined(__scc__) && !defined(RCK_EMU)
    err = bulk_create(size, PBUF_PKT_SIZE, &(buf->cap), &bt_packet, true);
#else
    err = bulk_create(size, PBUF_PKT_SIZE, &(buf->cap), &bt_packet, false);
#endif                          // defined(__scc__) && !defined(RCK_EMU)
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bulk_create failed.");
        return NULL;
    }
    LWIPBF_DEBUG("bulk_create success!!!\n");

    buf->va = bt_packet.mem;
    printf("mem_barrelfish_alloc: VA addr [%p]\n",
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

    printf("###### Adding buf[%"PRIu64", %p, %p] to buffer list [%"PRIu64", %p]\n",
            buf->va, buf->va, buf, buffer_list, buffer_list);
    /* Put this new buffer on the top of buffers list */
    buf->next = buffer_list;
    buffer_list = buf;

    printf("###### Added buf[%"PRIu64", %p, %p] to buffer list [%"PRIu64", %p],"
           "next [%p]\n",
            buf->va, buf->va, buf, buffer_list, buffer_list, buffer_list->next);

    // Creating shared_pool for communication
    buf->spp = sp_create_shared_pool(RECEIVE_BUFFERS, buf->role);
    assert(buf->spp != NULL);
    printf("mem_barrelfish_alloc: shared pool with [%d] slots\n",
            RECEIVE_BUFFERS);
    short_buf_list[binding_index] = buf;
    printf("mem_barrelfish_alloc: returning VA addr [%p]\n", buf->va);
    printf("###### buffer list [%p]\n", buffer_list);
    printf("###### buffer address [%p]\n", buf);

    return ((uint8_t *) buf->va);
}

uint8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size)
{
    struct buffer_desc *buf = short_buf_list[binding_index];
    errval_t err;
    assert(buf != NULL);
    printf("###### buffer address [%p]\n", buf);

    printf("###### %u buffer_list [%p]\n",
            __LINE__, buffer_list);
    if (binding_index == RX_BUFFER_ID) {
        rx_populate_sp_pbuf(buf);
    }
    printf("###### %u reg_buf populate done buffer list [%p]\n",
            __LINE__, buffer_list);

    /* FIXME: should buffer be also put in the client_closure_NC? */
    idc_register_buffer(buf, binding_index);

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
    printf("mem_barrelfish_alloc: returning2 VA addr [%p]\n", buf->va);
    printf("###### reg_buf_done buffer list [%p]\n", buffer_list);
    return ((uint8_t *) buf->va);
}

/*
uint8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size)
{
    struct buffer_desc *buf = short_buf_list[binding_index];
    errval_t err;

    printf("Dummy function called for index %"PRIu8"\n", binding_index);
    assert(!"dummy function called");
    return NULL;
}
*/

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

static struct buffer_desc *mem_barrelfish_get_buffer_desc_dbg(void *p)
{
    struct buffer_desc *tmp = buffer_list;
    assert(buffer_list != NULL );

    while(tmp != 0) {
        if (((uintptr_t) p < (uintptr_t) tmp->va)
                          || ((uintptr_t) p >
                              ((uintptr_t) tmp->va + tmp->size))) {
            printf("va[%p, %"PRIu64"] p[%p] va+size[%p, %"PRIu64"]\n",
                    (uintptr_t)tmp->va, tmp->va, p,
                    ((uintptr_t) tmp->va + tmp->size),
                    ((uintptr_t) tmp->va + tmp->size));
            tmp = tmp->next;
        } else {
            printf("buf Found\n");
            break;
        }
    }
    return tmp;

}


#if 0
static void wait_for_receive_channel(void)
{

    errval_t r;
    struct waitset *ws = get_default_waitset();
    // Make sure that you are not registering pbufs too fast
    // for continuation management!
    while (idc_check_capacity(RECEIVE_CONNECTION) < 10 ) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    } // end while
} // end function : wait_for_receive_channel


void mem_barrelfish_pbuf_init(void)
{
    struct pbuf *p;
    ptrdiff_t offset = 0;
    int i = 0;
    struct buffer_desc *buff_ptr;

    printf("Allocating %d pbufs\n",RECEIVE_BUFFERS);

    for (i = 0; i < RECEIVE_BUFFERS; i++) {
        /* We allocate a pbuf chain of pbufs from the pool. */
        p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);
        assert(p != 0);
        if (p->next != 0) {
            printf("Error in allocating %d'th pbuf\n", i);
        }
        assert(p->next == 0);   //make sure there is no chain for now...
        assert(p->tot_len == RECEIVE_PBUF_SIZE);
        assert(p->len == RECEIVE_PBUF_SIZE);

        //ignore the len returned here, because this is the len of the whole
        //static buffer (the frame cap size in bytes), not the pbuf
        buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
        //, &buffer_id, &paddr, &len, &offset, &vaddr);
        offset = (uintptr_t) p->payload - (uintptr_t) (buff_ptr->va);

        pbufs[i].p = p;
        pbufs[i].pbuf_id = i;

        /* NOTE: comment it as it will occur with large frequencey */
/*        LWIPBF_DEBUG("pbuf %lu is from buff %lu -------\n",
        		pbufs[i].pbuf_id, buff_ptr->buffer_id);
*/
        wait_for_receive_channel();
        //XXX: the msg handler should free the pbuf in case of an error
        //uint64_t r = idc_register_pbuf(i, paddr + offset, p->len);
//        printf("############## register pbuf %d\n", i);
        idc_register_pbuf(i, buff_ptr->pa + offset, p->len);
    }
    LWIPBF_DEBUG("pbuf is from buff %" PRIx64 " -------\n",
                 buff_ptr->buffer_id);
    LWIPBF_DEBUG("Registered %d no. of pbufs for receiving -------\n", i);
    printf("pbuf is from buff %" PRIx64 " -------\n", buff_ptr->buffer_id);
    printf("Registered %d no. of pbufs for receiving -------\n", i);

}
#endif // 0

//XXX: asq: Big parts of this code are duplicated (see above).
//struct pbuf *mem_barrelfish_replace_pbuf(struct buffer_desc *buf, uint64_t idx)
struct pbuf *mem_barrelfish_replace_pbuf(uint64_t idx)
{
//    ptrdiff_t offset = 0;
    uint64_t offset = 0;
    struct buffer_desc *buff_ptr;
    struct slot_data new_slot;
    uint64_t ts = rdtsc();

//    LWIPBF_DEBUG("Sending pbuf back for reuse ++++++++\n");
    struct pbuf *p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);

    assert(p != 0);
    assert(p->next == 0);       //make sure there is no chain for now...
    assert(p->tot_len == RECEIVE_PBUF_SIZE);
    assert(p->len == RECEIVE_PBUF_SIZE);

    //ignore the len returned here, because this is the len of the whole
    //static buffer (the frame cap size in bytes), not the pbuf
    buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
    if (buff_ptr == NULL) {
        // Debug this problem
        printf("ERROR: pbuf replace ID[%"PRIu64"], p[%p], payload[%p]\n",
                idx, p, p->payload);
        buff_ptr = mem_barrelfish_get_buffer_desc_dbg(p->payload);
    }
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
    bool ret = sp_replace_slot(buff_ptr->spp, &new_slot);
    if (!ret) {
        pbuf_free(p);
        return NULL;
    }
    lwip_record_event_simple(RE_PBUF_REPLACE_1, ts);

    pbufs[idx].p = p;
    pbufs[idx].pbuf_id = idx;

    return ((struct pbuf *)new_slot.client_data);
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
