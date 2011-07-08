/**
 * \file
 * \brief Memory server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <mm/mm.h>
#include <trace/trace.h>
#include <barrelfish/morecore.h>
#include <barrelfish/monitor_client.h>

#include <if/mem_defs.h>
#include <if/monitor_defs.h>

size_t mem_total = 0, mem_avail = 0;

#ifdef __BEEHIVE__
// Flag this as an initialisation domain for systems without ELF entry
bool __barrelfish_initialisation_domain = 1;
#endif

/* parameters for size of supported RAM and thus required storage */

// XXX: Even though we could manage an arbitrary amount of RAM on any
// architecture, we use paddr_t as the type to represent region
// limits, which limits us its size.
#if defined(__x86_64__)
#       define MAXSIZEBITS     38              ///< Max size of memory in allocator
#elif defined(__i386__)
#       define MAXSIZEBITS     32
#elif defined(__BEEHIVE__)
/* XXX This is better if < 32! - but there were no compile time warnings! */
#       define MAXSIZEBITS     31
#elif defined(__arm__)
/* XXX This is better if < 32! - but there were no compile time warnings! */
#       define MAXSIZEBITS     31
#else
#       error Unknown architecture
#endif

#define MINSIZEBITS     OBJBITS_DISPATCHER ///< Min size of each allocation
#define MAXCHILDBITS    4               ///< Max branching of BTree nodes

/// Maximum depth of the BTree, assuming only branching by two at each level
#define MAXDEPTH        (MAXSIZEBITS - MINSIZEBITS + 1)
/// Maximum number of BTree nodes
#define NNODES          ((1UL << MAXDEPTH) - 1)

/* Parameters for per-core memserv */
#define PERCORE_BITS 24
#define PERCORE_MEM (1UL<<PERCORE_BITS)           ///< How much memory per-core


static struct multi_slot_allocator msa;
static struct bootinfo *bi;

/**
 * \brief Size of CNodes to be created by slot allocator.
 *
 * Must satisfy both:
 *    #CNODE_BITS >= MAXCHILDBITS           (cnode enough for max branching factor)
 *    (1UL << #CNODE_BITS) ** 2 >= #NNODES  (total number of slots is enough)
 */
#define CNODE_BITS      12
#define NCNODES         (1UL << CNODE_BITS)     ///< Maximum number of CNodes

/// Watermark at which we must refill the slab allocator used for nodes
#define MINSPARENODES   (MAXDEPTH * 8) // XXX: FIXME: experimentally determined!

/// MM allocator instance data
static struct mm mm_ram;

/// Slot allocator for MM
static struct slot_prealloc ram_slot_alloc;

static errval_t mymm_alloc(struct capref *ret, uint8_t bits, genpaddr_t minbase,
                           genpaddr_t maxlimit)
{
    errval_t err;

    assert(bits >= MINSIZEBITS);

    if(maxlimit == 0) {
        err = mm_alloc(&mm_ram, bits, ret, NULL);
    } else {
        err = mm_alloc_range(&mm_ram, bits, minbase, maxlimit, ret, NULL);
    }

    return err;
}

static errval_t cap_identify(struct capref ramcap, struct capability *info)
{

    // TODO: bind to monitor_blocking and call cap_identify

    return LIB_ERR_NOT_IMPLEMENTED;

}

static errval_t mymm_free(struct capref ramcap)
{
    errval_t ret;

    struct capability info;
    genpaddr_t mem_to_add;

    // debug_printf("mem_free\n");

    ret = cap_identify(ramcap, &info);
    if (err_is_fail(ret)) {
        // XXX: Until cap_identify() is implemented, we return OK here
        // and leak all memory returned to us.
        if(err_no(ret) == LIB_ERR_NOT_IMPLEMENTED) {
            return SYS_ERR_OK;
        }

        return ret;
    }

    if (info.type != ObjType_RAM) {
        return SYS_ERR_INVALID_SOURCE_TYPE;
    }

    /*
    debug_printf("Cap is type %d Frame base 0x%"PRIxGENPADDR
                 " (%"PRIuGENPADDR") Bits %d\n",
                 info.type, info.u.frame.base, info.u.frame.base, 
                 info.u.frame.bits);
    */

    mem_to_add = (genpaddr_t)1 << info.u.frame.bits;
        
    ret = mm_free(&mm_ram, info.u.frame.base, info.u.frame.bits);
    if (err_is_fail(ret)) {
        if (err_no(ret) == MM_ERR_NOT_FOUND) {
            // memory wasn't there initially, add it
            ret = mm_add(&mm_ram, ramcap, info.u.frame.bits, 
                         info.u.frame.base);
            if (err_is_fail(ret)) {
                DEBUG_ERR(ret, "failed to add RAM to allocator");
                return ret;
            }
            mem_total += mem_to_add;
        } else {
            DEBUG_ERR(ret, "failed to free RAM in allocator");
            return ret;
        }
    }

    mem_avail += mem_to_add;

    return SYS_ERR_OK;
}


/// state for a pending reply
// because we have only one message that we send to a client, and there can only
// be one outstanding per binding (because this is an RPC interface) this is
// quite simple
struct pending_reply {
    struct mem_binding *b;
    errval_t err;
    struct capref cap;
};


static void retry_free_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.free_response(b, NOP_CONT, r->err);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), 
                               MKCONT(retry_free_reply,r));
    }

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to free request");
        free(r);
    }
}


static void retry_reply(void *arg)
{
    struct pending_reply *r = arg;
    assert(r != NULL);
    struct mem_binding *b = r->b;
    errval_t err;

    err = b->tx_vtbl.allocate_response(b, NOP_CONT, r->err, r->cap);
    if (err_is_ok(err)) {
        b->st = NULL;
        free(r);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply,r));
        assert(err_is_ok(err));
    } else {
        DEBUG_ERR(err, "failed to reply to memory request");
    }
}



static void mem_free_handler(struct mem_binding *b,
                                 struct capref ramcap)
{
    errval_t ret;
    errval_t err;

    // debug_printf("mem_free_handler\n");

    ret = mymm_free(ramcap);

    err = b->tx_vtbl.free_response(b, NOP_CONT, ret);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            err = b->register_send(b, get_default_waitset(), 
                                   MKCONT(retry_free_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to free request");
        }
    }
}


static void mem_available_handler(struct mem_binding *b) 
{
    errval_t err;
    /* Reply */
    err = b->tx_vtbl.available_response(b, NOP_CONT, mem_avail);
    if (err_is_fail(err)) {
        // FIXME: handle FLOUNDER_ERR_TX_BUSY
        DEBUG_ERR(err, "failed to reply to memory request");
    }

}

// FIXME: error handling (not asserts) needed in this function
static void mem_allocate_handler(struct mem_binding *b, uint8_t bits,
                                 genpaddr_t minbase, genpaddr_t maxlimit)
{
    struct capref cap;
    errval_t err, ret;

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_ALLOC, bits);

    /* refill slot allocator if needed */
    err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
    assert(err_is_ok(err));

    /* refill slab allocator if needed */
    while (slab_freecount(&mm_ram.slabs) <= MINSPARENODES) {
        struct capref frame;
        err = msa.a.alloc(&msa.a, &frame);
        assert(err_is_ok(err));
        err = frame_create(frame, BASE_PAGE_SIZE * 8, NULL);
        assert(err_is_ok(err));
        void *buf;
        err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE * 8, frame, NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_one_frame failed");
            assert(buf);
        }
        slab_grow(&mm_ram.slabs, buf, BASE_PAGE_SIZE * 8);
    }

    ret = mymm_alloc(&cap, bits, minbase, maxlimit);
    if (err_is_ok(ret)) {
        mem_avail -= 1UL << bits;
    } else {
        // DEBUG_ERR(ret, "allocation of %d bits in % " PRIxGENPADDR "-%" PRIxGENPADDR " failed",
        //          bits, minbase, maxlimit);
        cap = NULL_CAP;
    }

    /* Reply */
    err = b->tx_vtbl.allocate_response(b, NOP_CONT, ret, cap);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply *r = malloc(sizeof(struct pending_reply));
            assert(r != NULL);
            r->b = b;
            r->err = ret;
            r->cap = cap;
            err = b->register_send(b, get_default_waitset(), MKCONT(retry_reply,r));
            assert(err_is_ok(err));
        } else {
            DEBUG_ERR(err, "failed to reply to memory request");
        }
    }
}

static void dump_ram_region(int index, struct mem_region* m)
{
#if 0
    uintptr_t start, limit;

    start = (uintptr_t)m->mr_base;
    limit = start + (1UL << m->mr_bits);

    char prefix = ' ';
    size_t quantity = 1UL << m->mr_bits;

    if (m->mr_bits >= 30) {
        prefix = 'G';
        quantity >>= 30;
    }
    else if (m->mr_bits >= 20) {
        prefix = 'M';
        quantity >>= 20;
    }
    else if (m->mr_bits >= 10) {
        prefix = 'K';
        quantity >>= 10;
    }

    printf("RAM region %d: 0x%" PRIxPTR
           " - 0x%" PRIxPTR " (%zu %cB, %u bits)\n",
           index, start, limit, quantity, prefix, m->mr_bits);
#endif // 0
}



// FIXME: error handling (not asserts) needed in this function
static errval_t initialize_ram_alloc(void)
{
    errval_t err;

    /* Initialize slot allocator by passing a cnode cap for it to start with */
    struct capref cnode_cap;
    err = slot_alloc(&cnode_cap);
    assert(err_is_ok(err));
    struct capref cnode_start_cap = { .slot  = 0 };

    struct capref ram;
    err = ram_alloc_fixed(&ram, BASE_PAGE_BITS, 0, 0);
    assert(err_is_ok(err));
    err = cnode_create_from_mem(cnode_cap, ram, &cnode_start_cap.cnode,
                              DEFAULT_CNODE_BITS);
    assert(err_is_ok(err));

    /* location where slot allocator will place its top-level cnode */
    struct capref top_slot_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_MODULECN, // XXX: we don't have the module CNode
    };

    /* init slot allocator */
    err = slot_prealloc_init(&ram_slot_alloc, top_slot_cap, MAXCHILDBITS,
                           CNODE_BITS, cnode_start_cap,
                           1UL << DEFAULT_CNODE_BITS, &mm_ram);
    assert(err_is_ok(err));

    err = mm_init(&mm_ram, ObjType_RAM, 0, MAXSIZEBITS, MAXCHILDBITS, NULL,
                slot_alloc_prealloc, &ram_slot_alloc);
    assert(err_is_ok(err));

    /* give MM allocator static storage to get it started */
    static char nodebuf[SLAB_STATIC_SIZE(MINSPARENODES, MM_NODE_SIZE(MAXCHILDBITS))];
    slab_grow(&mm_ram.slabs, nodebuf, sizeof(nodebuf));

    /* walk bootinfo and add all unused RAM caps to allocator */
    struct capref mem_cap = {
        .cnode = cnode_super,
        .slot = 0,
    };

    for (int i = 0; i < bi->regions_length; i++) {
        if (bi->regions[i].mr_type == RegionType_Empty) {
            dump_ram_region(i, bi->regions + i);

            mem_total += ((size_t)1) << bi->regions[i].mr_bits;

            if (bi->regions[i].mr_consumed) {
                // region consumed by init, skipped
                mem_cap.slot++;
                continue;
            }

            err = mm_add(&mm_ram, mem_cap, bi->regions[i].mr_bits,
                         bi->regions[i].mr_base);
            if (err_is_ok(err)) {
                mem_avail += ((size_t)1) << bi->regions[i].mr_bits;
            } else {
                DEBUG_ERR(err, "Warning: adding RAM region %d (%p/%d) FAILED",
                          i, bi->regions[i].mr_base, bi->regions[i].mr_bits);
            }

            /* try to refill slot allocator (may fail if the mem allocator is empty) */
            err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
            if (err_is_fail(err) && err_no(err) != MM_ERR_SLOT_MM_ALLOC) {
                DEBUG_ERR(err, "in slot_prealloc_refill() while initialising"
                               " memory allocator");
                abort();
            }

            /* refill slab allocator if needed and possible */
            if (slab_freecount(&mm_ram.slabs) <= MINSPARENODES
                && mem_avail > (1UL << (CNODE_BITS + OBJBITS_CTE)) * 2
                                + 10 * BASE_PAGE_SIZE) {
                slab_default_refill(&mm_ram.slabs); // may fail
            }
            mem_cap.slot++;
        }
    }

    err = slot_prealloc_refill(mm_ram.slot_alloc_inst);
    if (err_is_fail(err)) {
        printf("Fatal internal error in RAM allocator: failed to initialise "
               "slot allocator\n");
        DEBUG_ERR(err, "failed to init slot allocator");
        abort();
    }

    printf("RAM allocator initialised, %zd MB (of %zd MB) available\n",
           mem_avail / 1024 / 1024, mem_total / 1024 / 1024);

    return SYS_ERR_OK;
}

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl. set_mem_iref_request(mb, NOP_CONT, iref);
    assert(err_is_ok(err));
}

static struct mem_rx_vtbl rx_vtbl = {
    .allocate_call = mem_allocate_handler,
    .available_call = mem_available_handler,
    .free_call = mem_free_handler,
};

static errval_t connect_callback(void *st, struct mem_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    // TODO: set error handler
    return SYS_ERR_OK;
}


#if 0 // percore mem_serv hack

/// MM per-core allocator instance data
static struct mm mm_percore;

/// Slot allocator for MM
static struct slot_prealloc percore_slot_alloc;

static iref_t percore_mem_serv_iref = 0xBAD;

static errval_t percore_alloc(struct capref *ret, uint8_t bits,
                              uint64_t minbase, uint64_t maxlimit,
                              struct cspace_allocator **alloc)
{
    errval_t err;

    assert(bits >= MINSIZEBITS);

    if(maxlimit == 0) {
        err = mm_alloc(&mm_percore, bits, ret, NULL);
    } else {
        err = mm_alloc_range(&mm_percore, bits, minbase, maxlimit, ret, NULL);
    }

    if (alloc != NULL) {
        *alloc = NULL;
    }

    return err;
}

// FIXME: error handling (not asserts) needed in this function
static void percore_allocate_handler(struct mem_binding *st,
                                     uintptr_t bits,
                                     uintptr_t minbase, uintptr_t maxlimit)
{
    struct capref cap;
    errval_t err, ret;

    trace_event(TRACE_SUBSYS_MEMSERV, TRACE_EVENT_ALLOC, bits);

    /* refill slot allocator if needed */
    err = slot_prealloc_refill(mm_percore.slot_alloc_inst);
    assert(err_is_ok(err));


    if (slab_freecount(&mm_percore.slabs) <= MINSPARENODES) {
        //  printf("slabs=%ld\n", slab_freecount(&mm_percore.slabs));
    }

    /* refill slab allocator if needed */
    if (0) {
        /* XXX There is something wrong with this logic */
        while (slab_freecount(&mm_percore.slabs) <= MINSPARENODES) {
            sys_print("GROW\n", 5);
            struct capref frame;
            err = msa.a.alloc(&msa.a, &frame);
            assert(err_is_ok(err));
            err = frame_create(frame, BASE_PAGE_SIZE * 8, NULL);
            assert(err_is_ok(err));
            void *buf;
            err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE * 8, frame,
                                       NULL, NULL);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "vspace_map_one_frame failed");
                assert(buf);
            }
            slab_grow(&mm_percore.slabs, buf, BASE_PAGE_SIZE * 8);
        }
    }

    ret = percore_alloc(&cap, bits, minbase, maxlimit, NULL);
    if (err_is_fail(ret)){
        sys_print("\nFAIL\n", 6);
        cap = NULL_CAP;
    }

    /* Reply */
    err = st->f->allocate(st, ret, cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to reply to memory request");
    }
}



static errval_t initialize_percore_mem_serv(void)
{
    errval_t err;

    /* Initialize slot allocator by passing a cnode cap for it to start with */
    struct capref cnode_cap;

    err = slot_alloc(&cnode_cap);
    assert(err_is_ok(err));

    struct capref cnode_start_cap = { .slot  = 0 };
    struct capref ram;

    err = ram_alloc(&ram, BASE_PAGE_BITS);
    assert(err_is_ok(err));
    err = cnode_create_from_mem(cnode_cap, ram, &cnode_start_cap.cnode,
                                DEFAULT_CNODE_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to create cnode from mem");
        abort();
    }
    assert(err_is_ok(err));

    /* location where slot allocator will place its top-level cnode */
    struct capref top_slot_cap = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_MODULECN, // XXX: we don't have the module CNode
    };

    /* init slot allocator */
    err = slot_prealloc_init(&percore_slot_alloc, top_slot_cap,
                             MAXCHILDBITS,
                             CNODE_BITS, cnode_start_cap,
                             1UL << DEFAULT_CNODE_BITS, &mm_percore);
    assert(err_is_ok(err));

    /* XXX Base shouldn't need to be 0 ? */
    err = mm_init(&mm_percore, ObjType_RAM,
                  0, MAXSIZEBITS, MAXCHILDBITS, NULL,
                  slot_alloc_prealloc, &percore_slot_alloc);
    assert(err_is_ok(err));

    /* give MM allocator static storage to get it started */
    static char nodebuf[SLAB_STATIC_SIZE(MINSPARENODES,
                                         MM_NODE_SIZE(MAXCHILDBITS))];
    slab_grow(&mm_percore.slabs, nodebuf, sizeof(nodebuf));

    size_t mem_total = 0, mem_avail = 0;
    struct capref ramcap;
    struct capability info;

    // Need to bootstrap with a small cap first!
    err = ram_alloc(&ramcap, 20);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to get RAM from mem_serv.0");
        abort();
    }

    err = debug_cap_identify(ramcap, &info);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to identify cap");
        abort();
    }

    if (0) printf("Cap is type %d Frame base %"PRIxGENPADDR" Bits %d\n",
                  info.type, info.u.frame.base, info.u.frame.bits);

    mem_total += 1<<20;

    err = mm_add(&mm_percore, ramcap, 20, info.u.frame.base);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to add RAM to allocator");
        abort();
    }
    mem_avail += 1<<20;

    /* try to refill slot allocator (may fail or do nothing) */
    slot_prealloc_refill(mm_percore.slot_alloc_inst);

    // Now get a serious chunk of RAM

    // XXX we should try to do something sensible with affinity here
    // uint64_t base = 0x100000000;
    // ram_set_affinity(base, base + PERCORE_MEM);
    err = ram_alloc(&ramcap, PERCORE_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to get RAM from mem_serv.0");
        abort();
    }
    //ram_set_affinity(0,0);

    err = debug_cap_identify(ramcap, &info);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to identify cap");
        abort();
    }
    if (0) printf("Cap is type %d Frame base %"PRIxGENPADDR" Bits %d\n",
                  info.type, info.u.frame.base, info.u.frame.bits);

    mem_total += PERCORE_MEM;

    err = mm_add(&mm_percore, ramcap, PERCORE_BITS, info.u.frame.base);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to add RAM to allocator");
        abort();
    }
    mem_avail += PERCORE_MEM;

    /* try to refill slot allocator (may fail or do nothing) */
    slot_prealloc_refill(mm_percore.slot_alloc_inst);

    /* refill slab allocator if needed and possible */
    if (slab_freecount(&mm_percore.slabs) <= MINSPARENODES
        && mem_avail > (1UL << (CNODE_BITS + OBJBITS_CTE)) * 2
        + 10 * BASE_PAGE_SIZE) {
        slab_default_refill(&mm_percore.slabs); // may fail
    }

    err = slot_prealloc_refill(mm_percore.slot_alloc_inst);
    if (err_is_fail(err)) {
        printf("Fatal internal error in RAM allocator: failed to initialise "
               "slot allocator\n");
        DEBUG_ERR(err, "failed to init slot allocator");
        abort();
    }

    printf("Percore RAM allocator initialised, %zd MB (of %zd MB) available\n",
           mem_avail / 1024 / 1024, mem_total / 1024 / 1024);

    return SYS_ERR_OK;
}


static void percore_listen_callback(struct mem_service *closure, iref_t iref)
{
    errval_t err;

    assert(iref != 0);
    percore_mem_serv_iref = iref;

    // Notify the monitor of our iref
    struct monitor_client_response *st = get_monitor_closure();
    err = st->call_vtbl->
        set_percore_iref_request(st, percore_mem_serv_iref);
    assert(err_is_ok(err));
}

struct mem_server_call_vtbl percore_mem_server_call_vtbl = {
    .allocate = percore_allocate_handler,
    ._disconnect = NULL,
    ._listening = percore_listen_callback,
    ._connected = NULL,
};

struct mem_service percore_mem_service = {
    .f = &percore_mem_server_call_vtbl,
    .st = NULL,
    .flags = 0 //SERV_NEW_CONN | SERV_MEMSERV_HACK,
};

#endif // percore
// FIXME: error handling (not asserts) needed in this function
int main(int argc, char ** argv)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[1], NULL, 10);

    if (argc == 2) {
        /* construct special-case LMP connection to monitor */
        static struct monitor_lmp_binding mcb;
        set_monitor_binding(&mcb.b);

        err = monitor_client_lmp_accept(&mcb, ws, DEFAULT_LMP_BUF_WORDS);
        assert(err_is_ok(err));

        idc_init();

        /* Send the cap for this endpoint to init, who will pass it to 
           the monitor */
        err = lmp_ep_send0(cap_initep, 0, mcb.chan.local_cap);
        assert(err_is_ok(err));

        // XXX: handle messages (ie. block) until the monitor binding is ready
        while (capref_is_null(mcb.chan.remote_cap)) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch while waiting for monitor");
                return EXIT_FAILURE;
            }
        }

        /* Initialize our own memory allocator */
        err = ram_alloc_set(mymm_alloc, mymm_free);
        assert(err_is_ok(err));

        err = initialize_ram_alloc();
        assert(err_is_ok(err));

        /* Initialize self slot_allocator */
        err = multi_slot_alloc_init(&msa, DEFAULT_CNODE_SLOTS, NULL);
        assert(err_is_ok(err));

        err = mem_export(NULL, export_callback, connect_callback, ws,
                         IDC_EXPORT_FLAGS_DEFAULT);
        assert(err_is_ok(err));

    } else {
        coreid_t core = disp_get_core_id();
        printf("mem_serv on core %d\n", core);

        assert(!"percore mem_serv support disabled");

#if 0
        /* Complete libbarrelfish initialization like a normal domain */
        err = monitor_client_connect();
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_MONITOR_CLIENT_CONNECT);
        }

        /* Setup the channel with mem_serv.0 */
        err = ram_alloc_set(NULL, NULL);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_RAM_ALLOC_SET);
        }

        /* Connect to name service */
        err = chips_get_context()->init();
        if (err_is_fail(err)) { // Chips fails with following error
            if (err_no(err) != CHIPS_ERR_CONNECT_FAILED) {
                return err_push(err, CHIPS_ERR_CHIPS_INIT);
            }
        }

        /* Init the memory allocator */
        err = initialize_percore_mem_serv();
        assert(err_is_ok(err));

        /* Initialize self slot_allocator */
        err = multi_slot_alloc_init(&msa, DEFAULT_CNODE_SLOTS, NULL);
        assert(err_is_ok(err));

        mem_listen(&percore_mem_service);
        assert(err_is_ok(err));
#endif
    }

    /* initialise tracing */
#if defined(TRACING_EXISTS) && defined(CONFIG_TRACE)
    err = trace_my_setup();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "initialising tracing");
        // return EXIT_FAILURE;
    }        
    trace_init_disp();
#endif

    // handle messages on this thread
    while (true) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in main event_dispatch loop");
            return EXIT_FAILURE;
        }
    }
}
