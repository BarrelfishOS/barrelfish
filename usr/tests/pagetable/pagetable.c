/**
 * Program that maps its own page-table into
 * it's virtual address space.
 **/
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/invocations_arch.h>
#include <barrelfish/except.h>

#define ALL_PRIVILEGES (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_EXECUTE)
#define NO_PRIVILEGES 0x0

static void print_vnodes(struct vnode* current, int depth);
static void print_vnode(struct vnode *current, int depth)
{
    char pad[depth+1];
    for (int i=0; i<depth; i++) {
        pad[i] = ' ';
    }
    pad[depth] = '\0';

    char capbuffer[1024];

    printf("%s", pad);
    printf("vnode:%p { entry: %d is_vnode: %d\n", current, current->v.entry, current->v.is_vnode);
    if (current->v.is_vnode) {
        debug_print_cap_at_capref(capbuffer, 1024, current->v.cap);
        printf("%s", pad);
        printf("vnode: cap=%s\n", capbuffer);
#if defined(PMAP_LL)
        print_vnodes(current->v.u.vnode.children, depth+1);
#elif defined(PMAP_ARRAY)
        print_vnodes(current->v.u.vnode.children[0], depth+1);
#else
#error Invalid pmap datastructure
#endif
    }
    else {
        debug_print_cap_at_capref(capbuffer, 1024, current->v.cap);
        printf("%s", pad);
        printf("frame: cap=%s offset:%"PRIxGENVADDR" flags:%d",
                capbuffer, current->v.u.frame.offset, current->v.u.frame.flags);
    }
    printf("\n");
    printf("%s", pad);
    printf("}\n");
}

static void print_vnodes(struct vnode* current, int depth) {
#if defined(PMAP_LL)
    while(current != NULL) {
#elif defined(PMAP_ARRAY)
    if (depth == 0) {
        print_vnode(current, depth);
        return;
    }
    struct vnode **currentp = &current;
    for (int i = 0; i < PTABLE_SIZE; i++) {
        current = currentp[i];
#else
#error Invalid pmap datastructure
#endif
        print_vnode(current, depth);
    }
}

#if defined(PMAP_LL)
static void find_pagetables(struct vnode* current) {
    char capbuffer[1024];

    while(current != NULL) {
        if (current->v.is_vnode) {
            struct capability ret;
            errval_t err = debug_cap_identify(current->v.cap, &ret);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "debug_cap_identify failed.");
            }

            if (ret.type == ObjType_VNode_x86_64_ptable) {
                printf("%s:%s:%d: we have a pagetable\n", __FILE__, __FUNCTION__, __LINE__);

                struct frame_identity id = { .base = 0, .bits = 0 };
                err = frame_identify(current->v.cap, &id);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Invoke vnode identify failed.");
                }

                struct capref ptable_copy;
                err = slot_alloc(&ptable_copy);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Failed to allocate slot");
                }

                err = cap_copy(ptable_copy, current->v.cap);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "cap_copy failed");
                }
                
                genpaddr_t *ptable = NULL;
                err = vspace_map_one_frame((void**)&ptable, (size_t)1<<id.bits, 
                                           ptable_copy, NULL, NULL);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Can not map the frame.");
                }

                debug_print_cap_at_capref(capbuffer, 1024, current->v.cap);
                printf("vnode: cap=%s\n", capbuffer);
 
                for(size_t i=0; i < X86_64_PTABLE_SIZE; i++) {
                    bool is_dirty = (ptable[i] & X86_64_PTABLE_DIRTY) > 0;
                    if (is_dirty) {
                        printf("retaddr[%zu] = 0x%"PRIxGENPADDR" is_dirty = %d\n", i, ptable[i], is_dirty);
                    }
                }

                size_t how_many = 0;
                err = invoke_clean_dirty_bits(current->v.cap, &how_many);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "clean dirty bits failed.");
                }
            }

            find_pagetables(current->v.u.vnode.children);
        }
        else {
            // Ignore frames
        }
        current = current->next;
    }
}
#elif defined(PMAP_ARRAY)
static void find_pagetables(struct vnode* current) {
    USER_PANIC("find_pagetables NYI for array-backed pmap.");
#if 0
    char capbuffer[1024];

    while(current != NULL) {
        if (current->v.is_vnode) {
            struct capability ret;
            errval_t err = debug_cap_identify(current->v.cap, &ret);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "debug_cap_identify failed.");
            }

            if (ret.type == ObjType_VNode_x86_64_ptable) {
                printf("%s:%s:%d: we have a pagetable\n", __FILE__, __FUNCTION__, __LINE__);

                struct frame_identity id = { .base = 0, .bits = 0 };
                err = frame_identify(current->v.cap, &id);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Invoke vnode identify failed.");
                }

                struct capref ptable_copy;
                err = slot_alloc(&ptable_copy);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Failed to allocate slot");
                }

                err = cap_copy(ptable_copy, current->v.cap);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "cap_copy failed");
                }

                genpaddr_t *ptable = NULL;
                err = vspace_map_one_frame((void**)&ptable, (size_t)1<<id.bits,
                        ptable_copy, NULL, NULL);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Can not map the frame.");
                }

                debug_print_cap_at_capref(capbuffer, 1024, current->v.cap);
                printf("vnode: cap=%s\n", capbuffer);

                for(size_t i=0; i < X86_64_PTABLE_SIZE; i++) {
                    bool is_dirty = (ptable[i] & X86_64_PTABLE_DIRTY) > 0;
                    if (is_dirty) {
                        printf("retaddr[%zu] = 0x%"PRIxGENPADDR" is_dirty = %d\n", i, ptable[i], is_dirty);
                    }
                }

                size_t how_many = 0;
                err = invoke_clean_dirty_bits(current->v.cap, &how_many);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "clean dirty bits failed.");
                }
            }

            find_pagetables(current->v.u.vnode.children);
        }
        else {
            // Ignore frames
        }
        current = current->next;
    }
#endif
}
#else
#error Invalid pmap datastructure
#endif

int main(int argc, char *argv[])
{
    printf("%s:%s:%d: Hi, lets map our own ptable!\n",
           __FILE__, __FUNCTION__, __LINE__);

    struct pmap* pmap = get_current_pmap();
    assert(pmap != NULL);
    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    printf("%s:%s:%d: About to print the vnode tree\n",
           __FILE__, __FUNCTION__, __LINE__);
    struct vnode* current = &x86->root;
    print_vnodes(current, 0);
    find_pagetables(current);

    void *sbrk(intptr_t increment);
    void* base = sbrk(0);
    void* limit = sbrk(1<<20); // 1mb
    assert(base != NULL);
    printf("%s:%s:%d: base = %p limit = %p\n", __FILE__, __FUNCTION__, __LINE__, base, limit);
    assert(limit > base);

    struct memobj_anon* sbrk_get_memobj(void);
    struct vregion* sbrk_get_vregion(void);

    struct memobj_anon* m = sbrk_get_memobj();
    struct vregion* vr = sbrk_get_vregion();
    assert(m != NULL);
    assert(vr != NULL);



    return EXIT_SUCCESS;
}
