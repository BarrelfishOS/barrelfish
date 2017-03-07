#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <bench/bench.h>
#include <assert.h>
#include "debug.h"
#include "vspace_cow.h"
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];
static struct vregion *cow_vregion;
static void *cow_vbuf = NULL;
static size_t cow_bufsize = 0;
static struct cnoderef cow_frames;
static size_t cow_frame_count = 0;

static void handler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    DEBUG_COW("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    uintptr_t base = (uintptr_t) cow_vbuf;
    if (addr < base || addr >= base + cow_bufsize) {
        DEBUG_COW("unexpected write pagefault on %p\n", vaddr);
        exit(1);
    }
    assert(cow_frame_count);
    DEBUG_COW("got expected write pagefault on %p, creating copy of page\n", vaddr);
    // get and map copy of page
    size_t frame_id = (addr - base) / BASE_PAGE_SIZE;
    DEBUG_COW("remapping frame %zu\n", frame_id);
    struct memobj *m = vregion_get_memobj(cow_vregion);
    assert(m);
    errval_t err;
    struct capref retframe;
    struct capref f = (struct capref) { .cnode = cow_frames, .slot = frame_id };
    size_t retoff;
    struct vregion *vr;
    void *buf;
    // copy data from faulting page to new page
    err = vspace_map_one_frame(&buf, BASE_PAGE_SIZE, f, NULL, &vr);
    assert(err_is_ok(err));
    memcpy(buf, (void *)faddr, BASE_PAGE_SIZE);
    vregion_destroy(vr);
    err = m->f.unfill(m, frame_id * BASE_PAGE_SIZE, &retframe, &retoff);
    assert(err_is_ok(err));
    err = m->f.fill(m, frame_id * BASE_PAGE_SIZE, f, BASE_PAGE_SIZE);
    assert(err_is_ok(err));
    err = m->f.pagefault(m, cow_vregion, frame_id * BASE_PAGE_SIZE, 0);
    assert(err_is_ok(err));
    err = m->f.protect(m, cow_vregion, frame_id * BASE_PAGE_SIZE,
            BASE_PAGE_SIZE, VREGION_FLAGS_READ_WRITE);
    assert(err_is_ok(err));
}

errval_t cow_init(size_t bufsize, size_t granularity,
        void *vaddr, struct vregion *vregion)
{
    errval_t err;
    struct capref frame, cncap;
    struct cnoderef cnode;

    // get RAM cap bufsize = (bufsize / granularity + 1) * granularity;
    err = slot_alloc(&frame);
    assert(err_is_ok(err));
    size_t rambits = log2floor(bufsize);
    DEBUG_COW("bits = %zu\n", rambits);
    err = ram_alloc(&frame, rambits);
    assert(err_is_ok(err));
    DEBUG_COW("ram alloc done\n");
    // calculate #slots
    granularity = 1 << log2floor(granularity);
    cslot_t cap_count = bufsize / granularity;
    debug_printf("cap_count = %"PRIuCSLOT"\n", cap_count); assert(cap_count < 256);
    // get CNode
    err = cnode_create_l2(&cncap, &cnode);
    assert(err_is_ok(err));
    DEBUG_COW("cnode create done\n");

    // retype RAM into Frames
    struct capref first_frame = (struct capref) { .cnode = cnode, .slot = 0 };
    err = cap_retype(first_frame, frame, 0, ObjType_Frame, granularity, cap_count);
    assert(err_is_ok(err));
    DEBUG_COW("retype done\n");

    //err = cap_destroy(frame);
    //assert(err_is_ok(err));
    //DEBUG_COW("destroy done\n");

    err = thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));

    cow_frame_count = cap_count;
    cow_frames = cnode;
    cow_vbuf = vaddr;
    cow_bufsize = bufsize;
    cow_vregion = vregion;
    return SYS_ERR_OK;
}


// create cow-enabled vregion & backing
// Can copy-on-write in granularity-sized chunks
errval_t vspace_map_one_frame_cow(void **buf, size_t size,
        struct capref frame, vregion_flags_t flags,
        struct memobj **memobj, struct vregion **vregion,
        size_t granularity)
{
    errval_t err;
    if (!memobj) {
        memobj = malloc(sizeof(*memobj));
    }
    assert(memobj);
    if (!vregion) {
        vregion = malloc(sizeof(*vregion));
    }
    assert(vregion);
    err = vspace_map_anon_attr(buf, memobj, vregion, size, &size, flags);
    assert(err_is_ok(err));

    size_t chunks = size / granularity;
    cslot_t slots;
    struct capref cncap;
    struct cnoderef cnode;
    err = cnode_create(&cncap, &cnode, chunks, &slots);
    assert(err_is_ok(err));
    assert(slots >= chunks);

    cycles_t fstart = 0;
    cycles_t pstart = 0;

    struct capref fc = (struct capref) { .cnode = cnode, .slot = 0 };
    for (int i = 0; i < chunks; i++) {
        err = cap_copy(fc, frame);
        assert(err_is_ok(err));

        cycles_t start = bench_tsc();
        err = (*memobj)->f.fill_foff(*memobj, i * granularity, fc, granularity, i*granularity);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "call failed.");
        }
        cycles_t end = bench_tsc();
        fstart += end-start;

        start = bench_tsc();
        err = (*memobj)->f.pagefault(*memobj, *vregion, i * granularity, 0);
        assert(err_is_ok(err));
        fc.slot++;
        end = bench_tsc();
        pstart += end-start;
    }

    printf("fill_foff: %"PRIu64"\n", bench_tsc_to_ms(fstart));
    printf("pagefault: %"PRIu64"\n", bench_tsc_to_ms(pstart));

    return SYS_ERR_OK;
}

