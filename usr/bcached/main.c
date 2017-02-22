/**
 * \file
 * \brief Buffer cache daemon.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#define WITH_BUFFER_CACHE
#include <vfs/vfs.h>

#include "bcached.h"
#include <hashtable/hashtable.h>

struct waitlist {
    struct waitlist *next;
    void *ptr;
};

struct lru_queue {
    struct lru_queue *prev, *next;
    uintptr_t index, block_length;
    char *key;
    size_t key_len;
    /* notify when transit is finished */
    struct {
        struct waitlist *start, *end;
    } waiters;
    bool in_transit;
    bool in_use;
};

struct capref cache_memory;
size_t cache_size, block_size = BUFFER_CACHE_BLOCK_SIZE;
void *cache_pool;
static struct hashtable *cache_hash = NULL;
static struct lru_queue *lru_start, *lru_end, *lru;
static size_t partial_hits = 0, hits = 0, misses = 0, allocations = 0, evictions = 0;

void print_stats(void)
{
    printf("cache statistics [%d]\n"
           "----------------\n"
           "cache size               = %u blocks * %u KB = %u MB\n"
           "hits                     = %zu\n"
           "part. hits (in transit)  = %zu\n"
           "misses                   = %zu\n"
           "allocations              = %zu / %u blocks (%zu%% utilization)\n"
           "evictions (replacements) = %zu blocks\n",
           disp_get_core_id(),
           NUM_BLOCKS, BUFFER_CACHE_BLOCK_SIZE / 1024, CACHE_SIZE / 1024 / 1024,
           hits, partial_hits, misses, allocations, NUM_BLOCKS,
           (allocations * 100) / NUM_BLOCKS, evictions);
}

static struct lru_queue *lru_get_untouched(uintptr_t idx)
{
    return lru + idx;
}

static struct lru_queue *lru_use(uintptr_t idx)
{
    assert(idx < NUM_BLOCKS);
    struct lru_queue *e = &lru[idx];

    // Make it new head of the queue
    if(e->prev != NULL) {
        struct lru_queue *oldnext = e->next;
        e->prev->next = e->next;
        if(oldnext != NULL) {
            oldnext->prev = e->prev;
        } else {
            // This was the tail of the queue, we need to designate a new tail
            assert(e == lru_end);
            lru_end = e->prev;
        }
        e->prev = NULL;
        e->next = lru_start;
        lru_start->prev = e;
        lru_start = e;
    } else {
        assert(e == lru_start);
    }

    return lru_start;
}

static struct lru_queue *lru_get(void)
{
    assert(lru_end != NULL);
    return lru_use(lru_end->index);
}

static void lru_init(void)
{
    // Initialize LRU queue
    lru_start = lru = calloc(NUM_BLOCKS, sizeof(struct lru_queue));
    assert(lru_start != NULL);
    lru_end = lru_start + NUM_BLOCKS - 1;
    for(struct lru_queue *p = lru_start; p <= lru_end; p++) {
        p->prev = p - 1;
        p->next = p + 1;
        p->index = p - lru_start;
    }
    lru_start->prev = lru_end->next = NULL;
}

uint64_t cache_get_block_length(uintptr_t idx)
{
    idx /= BUFFER_CACHE_BLOCK_SIZE;
    struct lru_queue *l = lru_get_untouched(idx);
    return l->block_length;
}

key_state_t cache_lookup(const char *key, size_t key_len,
                         uintptr_t *idx, uintptr_t *length)
{
    ENTRY_TYPE et;
    void *val;
    key_state_t ret = KEY_MISSING;

    et = cache_hash->d.get(&cache_hash->d, key, key_len, &val);

    if (et == 0)  {
        misses++;
        ret = KEY_MISSING;
    } else if (et == TYPE_WORD) {
        *idx = (uintptr_t)val;
        struct lru_queue *l = lru_use(*idx);
        *length = l->block_length;

        if (l->in_transit) {
            partial_hits++;
            ret = KEY_INTRANSIT;
        } else {
            hits++;
            ret = KEY_EXISTS;
        }
    } else {
        assert(et == TYPE_WORD || et == 0);
    }

    // Convert to byte offset from start of cache (XXX does this make sense when et == 0?)
    *idx *= BUFFER_CACHE_BLOCK_SIZE;

    /* printf("cache_lookup(\"%s\", %" PRIuPTR ") = %s\n", key, *idx, */
    /*        et == TYPE_WORD ? "true" : "false"); */
    return ret;
}

void
cache_register_wait(uintptr_t idx, void *ptr)
{
    struct waitlist *wl;
    struct lru_queue *e;

    assert(ptr != NULL);

    wl = malloc(sizeof(struct waitlist));
    assert(wl);
    wl->ptr = ptr;
    wl->next = NULL;

    idx /= BUFFER_CACHE_BLOCK_SIZE;
    e = lru_get_untouched(idx);
    if (e->waiters.start == NULL) {
        e->waiters.start = e->waiters.end = wl;
    } else {
        assert(e->waiters.end->next == NULL);
        e->waiters.end->next = wl;
        e->waiters.end = wl;
    }
}

void *
cache_get_next_waiter(uintptr_t idx)
{
    struct waitlist *wl;
    void *ret;
    idx /= BUFFER_CACHE_BLOCK_SIZE;
    struct lru_queue *e = lru_get_untouched(idx);
    if (e->waiters.start == NULL) {
        return NULL;
    }

    wl = e->waiters.start;
    e->waiters.start = wl->next;

    ret = wl->ptr;
    free(wl);

    return ret;
}

uintptr_t cache_allocate(const char *key, size_t key_len)
{
    struct lru_queue *e = lru_get();

    assert(!e->in_transit);

    if(e->in_use) {
        // Cache is write-through, so we just have to delete the old entry
        int r = cache_hash->d.remove(&cache_hash->d, e->key, e->key_len);
        assert(r == 0);
        free(e->key);

#ifdef WITH_WRITE_BACK_CACHE
        assert(!"NYI");
#endif

        evictions++;
    } else {
        allocations++;
    }

    e->in_use = true;
    e->in_transit = true;
    e->key = strdup(key);
    assert(e->key);
    e->key_len = key_len;
    e->block_length = 0;
    e->waiters.start = e->waiters.end = NULL;

    int r = cache_hash->d.put_word(&cache_hash->d, key, key_len, e->index);
    assert(r == 0);

    // Convert to byte offset from start of cache
    return e->index * BUFFER_CACHE_BLOCK_SIZE;
}

void cache_update(uintptr_t idx, uintptr_t length)
{
    assert(idx % BUFFER_CACHE_BLOCK_SIZE == 0);
    idx /= BUFFER_CACHE_BLOCK_SIZE;
    struct lru_queue *l = lru_use(idx);
    l->block_length = length;
    l->in_transit = false;
}

static errval_t create_cache_mem(size_t size)
{
    // Create a Frame Capability
    errval_t r = frame_alloc(&cache_memory, size, &cache_size);
    if (err_is_fail(r)) {
        return err_push(r, LIB_ERR_FRAME_ALLOC);
    }
    assert(cache_size >= size);

    // Map the frame in local memory
    r = vspace_map_one_frame(&cache_pool, cache_size, cache_memory, NULL, NULL);
    if (err_is_fail(r)) {
        cap_destroy(cache_memory);
        return err_push(r, LIB_ERR_VSPACE_MAP);
    }
    assert(cache_pool != NULL);

    return SYS_ERR_OK;
}


/* alt_malloc provided only by OLDC -AKK */

#ifdef CONFIG_OLDC
#include <dmalloc/dmalloc.h>
typedef void *(*alt_malloc_t)(size_t bytes);
extern alt_malloc_t alt_malloc;

typedef void (*alt_free_t)(void *p);
extern alt_free_t alt_free;

typedef void *(*alt_realloc_t)(void *p, size_t bytes);
extern alt_realloc_t alt_realloc;

static void init_dmalloc(void)
{
    alt_malloc = &dlmalloc;
    alt_free = &dlfree;
    alt_realloc = &dlrealloc;
}
#endif

int main(int argc, char *argv[])
{
    errval_t err;

    #ifdef CONFIG_OLDC
    init_dmalloc();
    #endif

    err = create_cache_mem(CACHE_SIZE);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "create_cache_mem");
    }

    cache_hash = create_hashtable2(2 * NUM_BLOCKS, 75);
    assert(cache_hash != NULL);

    lru_init();

    err = start_service();
    assert(err_is_ok(err));

    for(;;) {
        err = event_dispatch(get_default_waitset());
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "event_dispatch");
        }
    }

    return 0;
}
