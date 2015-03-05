/**
 * Program that maps its own page-table into
 * it's virtual address space.
 **/
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/invocations_arch.h>

static void print_vnodes(struct vnode* current, int depth) {
    char pad[depth+1];
    for (int i=0; i<depth; i++) {
        pad[i] = ' ';
    }
    pad[depth] = '\0';

    char capbuffer[1024];

    while(current != NULL) {
        printf("%s", pad);
        printf("vnode:%p { entry: %d is_vnode: %d\n", current, current->entry, current->is_vnode);
        if (current->is_vnode) {
            debug_print_cap_at_capref(capbuffer, 1024, current->u.vnode.cap);
            printf("%s", pad);
            printf("vnode: cap=%s\n", capbuffer);
            print_vnodes(current->u.vnode.children, depth+1);
        }
        else {
            debug_print_cap_at_capref(capbuffer, 1024, current->u.frame.cap);
            printf("%s", pad);
            printf("frame: cap=%s offset:%"PRIxGENVADDR" flags:%d", capbuffer, current->u.frame.offset, current->u.frame.flags);
        }
        printf("\n");
        printf("%s", pad);
        printf("}\n");
        current = current->next;
    }
}

static void find_pagetables(struct vnode* current) {
    char capbuffer[1024];

    while(current != NULL) {
        if (current->is_vnode) {
            struct capability ret;
            errval_t err = debug_cap_identify(current->u.vnode.cap, &ret);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "debug_cap_identify failed.");
            }

            if (ret.type == ObjType_VNode_x86_64_ptable) {
                printf("%s:%s:%d: we have a pagetable\n", __FILE__, __FUNCTION__, __LINE__);

                struct frame_identity id = { .base = 0, .bits = 0 };
                err = invoke_frame_identify(current->u.vnode.cap, &id);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Invoke vnode identify failed.");
                }

                struct capref ptable_copy;
                err = slot_alloc(&ptable_copy);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Failed to allocate slot");
                }

                err = cap_copy(ptable_copy, current->u.vnode.cap);
                if (err_is_fail(err)) {
                    DEBUG_ERR(err, "cap_copy failed");
                }

                genpaddr_t *ptable = NULL;
                err = vspace_map_one_frame((void**)&ptable, (size_t)1<<id.bits,
                        ptable_copy, NULL, NULL);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "Can not map the frame.");
                }

                debug_print_cap_at_capref(capbuffer, 1024, current->u.vnode.cap);
                printf("vnode: cap=%s\n", capbuffer);

                for(size_t i=0; i < X86_64_PTABLE_SIZE; i++) {
                    bool is_dirty = (ptable[i] & X86_64_PTABLE_DIRTY) > 0;
                    if (is_dirty) {
                        printf("retaddr[%zu] = 0x%"PRIxGENPADDR" is_dirty = %d\n", i, ptable[i], is_dirty);
                    }
                }

                size_t how_many = 0;
                err = invoke_clean_dirty_bits(current->u.vnode.cap, &how_many);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "clean dirty bits failed.");
                }
            }

            find_pagetables(current->u.vnode.children);
        }
        else {
            // Ignore frames
        }
        current = current->next;
    }
}

int main(int argc, char *argv[])
{
    errval_t err;

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

    struct memobj_anon* get_sbrk_memobj(void);
    struct vregion* get_sbrk_vregion(void);

    struct memobj_anon* m = get_sbrk_memobj();
    struct vregion* vr = get_sbrk_vregion();
    assert(m != NULL);
    assert(vr != NULL);

    return EXIT_SUCCESS;
}
