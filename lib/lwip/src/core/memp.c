/**
 * @file
 * Dynamic pool memory manager
 *
 * lwIP has dedicated pools for many structures (netconn, protocol control blocks,
 * packet buffers, ...). All these pools are managed here.
 */

/*
 * Copyright (c) 2001-2004 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */

#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/udp.h"
#include "lwip/raw.h"
#include "lwip/tcp.h"
#include "lwip/igmp.h"
#include "lwip/api.h"
#include "lwip/api_msg.h"
#include "lwip/tcpip.h"
#include "lwip/sys.h"
#include "lwip/stats.h"
#include "netif/etharp.h"
#include "lwip/ip_frag.h"

#include <string.h>

#if !MEMP_MEM_MALLOC            /* don't build if not configured for use in lwipopts.h */

struct memp {
    struct memp *next;
#if MEMP_OVERFLOW_CHECK
    const char *file;
    int line;
#endif                          /* MEMP_OVERFLOW_CHECK */
};

#if MEMP_OVERFLOW_CHECK
/* if MEMP_OVERFLOW_CHECK is turned on, we reserve some bytes at the beginning
 * and at the end of each element, initialize them as 0xcd and check
 * them later. */
/* If MEMP_OVERFLOW_CHECK is >= 2, on every call to memp_malloc or memp_free,
 * every single element in each pool is checked!
 * This is VERY SLOW but also very helpful. */
/* MEMP_SANITY_REGION_BEFORE and MEMP_SANITY_REGION_AFTER can be overridden in
 * lwipopts.h to change the amount reserved for checking. */
#ifndef MEMP_SANITY_REGION_BEFORE
#define MEMP_SANITY_REGION_BEFORE  16
#endif                          /* MEMP_SANITY_REGION_BEFORE */
#if MEMP_SANITY_REGION_BEFORE > 0
#define MEMP_SANITY_REGION_BEFORE_ALIGNED    LWIP_MEM_ALIGN_SIZE(MEMP_SANITY_REGION_BEFORE)
#else
#define MEMP_SANITY_REGION_BEFORE_ALIGNED    0
#endif                          /* MEMP_SANITY_REGION_BEFORE */
#ifndef MEMP_SANITY_REGION_AFTER
#define MEMP_SANITY_REGION_AFTER   16
#endif                          /* MEMP_SANITY_REGION_AFTER */
#if MEMP_SANITY_REGION_AFTER > 0
#define MEMP_SANITY_REGION_AFTER_ALIGNED     LWIP_MEM_ALIGN_SIZE(MEMP_SANITY_REGION_AFTER)
#else
#define MEMP_SANITY_REGION_AFTER_ALIGNED     0
#endif                          /* MEMP_SANITY_REGION_AFTER */

/* MEMP_SIZE: save space for struct memp and for sanity check */
#define MEMP_SIZE          (LWIP_MEM_ALIGN_SIZE(sizeof(struct memp)) + MEMP_SANITY_REGION_BEFORE_ALIGNED)
#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x) + MEMP_SANITY_REGION_AFTER_ALIGNED)

#else                           /* MEMP_OVERFLOW_CHECK */

/* No sanity checks
 * We don't need to preserve the struct memp while not allocated, so we
 * can save a little space and set MEMP_SIZE to 0.
 */
#define MEMP_SIZE           0
#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x))

#endif                          /* MEMP_OVERFLOW_CHECK */

/** This array holds the first free element of each pool.
 *  Elements form a linked list. */
static struct memp *memp_tab[MEMP_MAX];

#else                           /* MEMP_MEM_MALLOC */

#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x))

#endif                          /* MEMP_MEM_MALLOC */

/** This array holds the element sizes of each pool. */
#if !MEM_USE_POOLS && !MEMP_MEM_MALLOC
static
#endif
const u16_t memp_sizes[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  LWIP_MEM_ALIGN_SIZE(size),
#include "lwip/memp_std.h"
};

#if !MEMP_MEM_MALLOC            /* don't build if not configured for use in lwipopts.h */

/** This array holds the number of elements in each pool. */
static const u16_t memp_num[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  (num),
#include "lwip/memp_std.h"
};


/** This array holds a textual description of each pool. */
#ifdef LWIP_DEBUG
static const char *memp_desc[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  (desc),
#include "lwip/memp_std.h"
};
#endif                          /* LWIP_DEBUG */

#if 0
static u8_t memp_memory_orig[MEM_ALIGNMENT - 1
#define LWIP_MEMPOOL(name,num,size,desc) + ( (num) * (MEMP_SIZE + MEMP_ALIGN_SIZE(size) ) )
#include "lwip/memp_std.h"
  ];
#endif

/* This is the size of memory required by all the pools. */
static const size_t memp_memory_size = (MEM_ALIGNMENT - 1
#define LWIP_MEMPOOL(name,num,size,desc) + ( (num) * (MEMP_SIZE + MEMP_ALIGN_SIZE(size) ) )
#include "lwip/memp_std.h"
  );

static u8_t *memp_memory = 0;
u8_t *mem_barrelfish_alloc(uint8_t buf_index, uint32_t size);
u8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size);

#if MEMP_SANITY_CHECK
/**
 * Check that memp-lists don't form a circle
 */
static int memp_sanity(void)
{
    s16_t i, c;
    struct memp *m, *n;

    for (i = 0; i < MEMP_MAX; i++) {
        for (m = memp_tab[i]; m != NULL; m = m->next) {
            c = 1;
            for (n = memp_tab[i]; n != NULL; n = n->next) {
                if (n == m && --c < 0) {
                    return 0;
                }
            }
        }
    }
    return 1;
}
#endif                          /* MEMP_SANITY_CHECK */
#if MEMP_OVERFLOW_CHECK
/**
 * Check if a memp element was victim of an overflow
 * (e.g. the restricted area after it has been altered)
 *
 * @param p the memp element to check
 * @param memp_size the element size of the pool p comes from
 */
static void memp_overflow_check_element(struct memp *p, u16_t memp_size)
{
    u16_t k;
    u8_t *m;

#if MEMP_SANITY_REGION_BEFORE_ALIGNED > 0
    m = (u8_t *) p + MEMP_SIZE - MEMP_SANITY_REGION_BEFORE_ALIGNED;
    for (k = 0; k < MEMP_SANITY_REGION_BEFORE_ALIGNED; k++) {
        if (m[k] != 0xcd) {
            LWIP_ASSERT("detected memp underflow!", 0);
        }
    }
#endif
#if MEMP_SANITY_REGION_AFTER_ALIGNED > 0
    m = (u8_t *) p + MEMP_SIZE + memp_size;
    for (k = 0; k < MEMP_SANITY_REGION_AFTER_ALIGNED; k++) {
        if (m[k] != 0xcd) {
            LWIP_ASSERT("detected memp overflow!", 0);
        }
    }
#endif
}

/**
 * Do an overflow check for all elements in every pool.
 *
 * @see memp_overflow_check_element for a description of the check
 */
static void memp_overflow_check_all(void)
{
    u16_t i, j;
    struct memp *p;

    p = LWIP_MEM_ALIGN(memp_memory);
    for (i = 0; i < MEMP_MAX; ++i) {
        p = p;
        for (j = 0; j < memp_num[i]; ++j) {
            memp_overflow_check_element(p, memp_sizes[i]);
            p =
              (struct memp *) ((u8_t *) p + MEMP_SIZE + memp_sizes[i] +
                               MEMP_SANITY_REGION_AFTER_ALIGNED);
        }
    }
}

/**
 * Initialize the restricted areas of all memp elements in every pool.
 */
static void memp_overflow_init(void)
{
    u16_t i, j;
    struct memp *p;
    u8_t *m;

    p = LWIP_MEM_ALIGN(memp_memory);
    for (i = 0; i < MEMP_MAX; ++i) {
        p = p;
        for (j = 0; j < memp_num[i]; ++j) {
#if MEMP_SANITY_REGION_BEFORE_ALIGNED > 0
            m = (u8_t *) p + MEMP_SIZE - MEMP_SANITY_REGION_BEFORE_ALIGNED;
            memset(m, 0xcd, MEMP_SANITY_REGION_BEFORE_ALIGNED);
#endif
#if MEMP_SANITY_REGION_AFTER_ALIGNED > 0
            m = (u8_t *) p + MEMP_SIZE + memp_sizes[i];
            memset(m, 0xcd, MEMP_SANITY_REGION_AFTER_ALIGNED);
#endif
            p =
              (struct memp *) ((u8_t *) p + MEMP_SIZE + memp_sizes[i] +
                               MEMP_SANITY_REGION_AFTER_ALIGNED);
        }
    }
}
#endif                          /* MEMP_OVERFLOW_CHECK */

static u16_t pbuf_pool_counter = 0;

/**
 * Initialize this module.
 *
 * Carves out memp_memory into linked lists for each pool-type.
 */
void memp_init(void)
{

//    printf("memp_init: allocating %zx memory for index %d\n", memp_memory_size,
//           RX_BUFFER_ID);

    memp_memory = NULL;
    memp_memory =
      mem_barrelfish_alloc(RX_BUFFER_ID, memp_memory_size);
    if (memp_memory == 0) {
        fprintf(stderr, "could not allocate memory");
        abort();
    }
//    printf("memp_init: allocated memory is at VA [%p]\n", memp_memory);

    memp_initialize_pbuf_list();
    mem_barrelfish_register_buf(RX_BUFFER_ID, memp_memory_size);
}


void memp_initialize_pbuf_list(void)
{

    assert(memp_memory != NULL);
    struct memp *memp;
    u16_t i, j;
    for (i = 0; i < MEMP_MAX; ++i) {
        MEMP_STATS_AVAIL(used, i, 0);
        MEMP_STATS_AVAIL(max, i, 0);
        MEMP_STATS_AVAIL(err, i, 0);
        MEMP_STATS_AVAIL(avail, i, memp_num[i]);
    }
    memp = LWIP_MEM_ALIGN(memp_memory);
/*    printf("memp_init: total types of pools %d\n", MEMP_MAX );
    printf("memp_init: total types of pools %d, memp_mem %p\n",
            MEMP_MAX, memp_memory);
    printf("memp_init: total types of pools %d, memp %p\n", MEMP_MAX, memp);
*/
    memp->next = NULL;
    /* for every pool: */
    for (i = 0; i < MEMP_MAX; ++i) {
        memp_tab[i] = NULL;
/*      printf("memp_init: %" PRIu16 "(%s) size %" PRIu16 " num %" PRIu16 "\n",
               i, memp_desc[i], memp_sizes[i], memp_num[i]);
*/
        /* create a linked list of memp elements */
        for (j = 0; j < memp_num[i]; ++j) {
            memp->next = NULL;
            memp->next = memp_tab[i];
            memp_tab[i] = memp;
            memp = (struct memp *) ((u8_t *) memp + MEMP_SIZE + memp_sizes[i]
#if MEMP_OVERFLOW_CHECK
                                    + MEMP_SANITY_REGION_AFTER_ALIGNED
#endif
              );
        }
    }
    // Set how many free pbuf_pools are there
//    printf("memp_num[PBUF_POOL] %" PRIu16 "\n", memp_num[MEMP_MAX - 1]);
    pbuf_pool_counter = 0;
#if MEMP_OVERFLOW_CHECK
    memp_overflow_init();
    /* check everything a first time to see if it worked */
    memp_overflow_check_all();
#endif                          /* MEMP_OVERFLOW_CHECK */

//    mem_barrelfish_register_buf(RX_BUFFER_ID, memp_memory_size);
}

// Returns the count of free pbufs available
u16_t memp_pbuf_peek(void)
{
    return (memp_num[MEMP_MAX - 1] - pbuf_pool_counter);
}

/**
 * Get an element from a specific pool.
 *
 * @param type the pool to get an element from
 *
 * the debug version has two more parameters:
 * @param file file name calling this function
 * @param line number of line where this function is called
 *
 * @return a pointer to the allocated memory or a NULL pointer on error
 */
void *
#if !MEMP_OVERFLOW_CHECK
memp_malloc(memp_t type)
#else
memp_malloc_fn(memp_t type, const char *file, const int line)
#endif
{
    struct memp *memp;

    SYS_ARCH_DECL_PROTECT(old_level);

    LWIP_ERROR("memp_malloc: type < MEMP_MAX", (type < MEMP_MAX), return NULL;
      );

    SYS_ARCH_PROTECT(old_level);
#if MEMP_OVERFLOW_CHECK >= 2
    memp_overflow_check_all();
#endif                          /* MEMP_OVERFLOW_CHECK >= 2 */

    memp = memp_tab[type];

    if (memp != NULL) {
        memp_tab[type] = memp->next;
        ++pbuf_pool_counter;
//    printf("memp_malloc: %s %"PRIu16" %"PRIu16" \n",
//            disp_name(), type, pbuf_pool_counter);
#if MEMP_OVERFLOW_CHECK
        memp->next = NULL;
        memp->file = file;
        memp->line = line;
#endif                          /* MEMP_OVERFLOW_CHECK */
        MEMP_STATS_INC_USED(used, type);
        LWIP_ASSERT("memp_malloc: memp properly aligned",
                    ((mem_ptr_t) memp % MEM_ALIGNMENT) == 0);
        memp = (struct memp *) ((u8_t *) memp + MEMP_SIZE);
    } else {
        LWIP_DEBUGF(MEMP_DEBUG | 2,
                    ("memp_malloc: out of memory in pool %s\n",
                     memp_desc[type]));
        MEMP_STATS_INC(err, type);
    }

    SYS_ARCH_UNPROTECT(old_level);
/*
    if (memp == NULL) {
#if !MEMP_OVERFLOW_CHECK
        printf("memp_malloc: %" PRIu16 "\n", type);
#else
        printf("memp_malloc_fn:\n");
#endif
    }
*/
    return memp;
}

/**
 * Put an element back into its pool.
 *
 * @param type the pool where to put mem
 * @param mem the memp element to free
 */
static uint64_t free_counter = 0;
void memp_free(memp_t type, void *mem)
{
    ++free_counter;
/*    printf("memp_free %s called for type %"PRIu16" with counter "
            "%"PRIu16", free counter %"PRIu64"\n",
           disp_name(), type, pbuf_pool_counter, free_counter);
*/
    struct memp *memp;

    SYS_ARCH_DECL_PROTECT(old_level);

    if (mem == NULL) {
        printf("memp_free: mem is NULL\n");
        return;
    }
    LWIP_ASSERT("memp_free: mem properly aligned",
                ((mem_ptr_t) mem % MEM_ALIGNMENT) == 0);

    memp = (struct memp *) ((u8_t *) mem - MEMP_SIZE);

    SYS_ARCH_PROTECT(old_level);
#if MEMP_OVERFLOW_CHECK
#if MEMP_OVERFLOW_CHECK >= 2
    memp_overflow_check_all();
#else
    memp_overflow_check_element(memp, memp_sizes[type]);
#endif                          /* MEMP_OVERFLOW_CHECK >= 2 */
#endif                          /* MEMP_OVERFLOW_CHECK */

    MEMP_STATS_DEC(used, type);

    memp->next = memp_tab[type];
    memp_tab[type] = memp;
    assert(pbuf_pool_counter > 0);
    --pbuf_pool_counter;

#if MEMP_SANITY_CHECK
    LWIP_ASSERT("memp sanity", memp_sanity());
#endif                          /* MEMP_SANITY_CHECK */

    SYS_ARCH_UNPROTECT(old_level);
}

#endif                          /* MEMP_MEM_MALLOC */
