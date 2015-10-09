#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish_kpi/paging_target.h>
#include <assert.h>

#include "pmap_cow.h"
#include "debug.h"

static struct vnode *cow_root_pte = NULL;
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];

static errval_t alloc_vnode_noalloc(struct pmap_x86 *pmap, struct vnode *root,
                     struct capref vnodecap, uint32_t entry,
                     struct vnode **retvnode)
{
    errval_t err;

    struct vnode *newvnode = slab_alloc(&pmap->slab);
    if (newvnode == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }
    newvnode->u.vnode.cap = vnodecap;

    // Map it
    err = vnode_map(root->u.vnode.cap, newvnode->u.vnode.cap, entry,
                    PTABLE_ACCESS_DEFAULT, 0, 1);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    // The VNode meta data
    newvnode->is_vnode  = true;
    newvnode->is_cloned = false;
    newvnode->entry     = entry;
#ifdef PMAP_LL
    newvnode->next      = root->u.vnode.children;
    root->u.vnode.children = newvnode;
    newvnode->u.vnode.children = NULL;
#elif defined(PMAP_ARRAY)
    memset(newvnode->u.vnode.children, 0, sizeof(struct vode *)*PTABLE_SIZE);
    root->u.vnode.children[entry] = newvnode;
#else
#error Invalid pmap datastructure
#endif

    *retvnode = newvnode;
    return SYS_ERR_OK;
}

static errval_t alloc_vnode(struct pmap_x86 *pmap, struct vnode *root,
                     enum objtype type, uint32_t entry,
                     struct vnode **retvnode)
{
    errval_t err;

    struct capref vnodecap;
    // The VNode capability
    err = pmap->p.slot_alloc->alloc(pmap->p.slot_alloc, &vnodecap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = vnode_create(vnodecap, type);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_CREATE);
    }

    return alloc_vnode_noalloc(pmap, root, vnodecap, entry, retvnode);
}

#if defined(PMAP_LL)
static struct vnode *find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->is_vnode);
    struct vnode *n;

    for(n = root->u.vnode.children; n != NULL; n = n->next) {
        if (!n->is_vnode) {
            // check whether entry is inside a large region
            uint16_t end = n->entry + n->u.frame.pte_count;
            if (n->entry <= entry && entry < end) {
                //if (n->entry < entry) {
                //    debug_printf("%d \\in [%d, %d]\n", entry, n->entry, end);
                //}
                return n;
            }
        }
        else if(n->entry == entry) {
            // return n if n is a vnode and the indices match
            return n;
        }
    }
    return NULL;
}
#elif defined(PMAP_ARRAY)
static struct vnode *find_vnode(struct vnode *root, uint16_t entry)
{
    assert(root != NULL);
    assert(root->is_vnode);
    assert(entry < PTABLE_SIZE);

    if (root->u.vnode.children) {
        return root->u.vnode.children[entry];
    } else {
        return NULL;
    }
}

static errval_t vnode_clone(struct pmap_x86 *x86,
        struct vnode *parent, size_t entry,
        struct vnode **dest, struct vnode *src)
{
    errval_t err;
    // TODO: better change to r/o on pml4e or pdpt?
    err = vnode_modify_flags(src->u.vnode.cap, 0,
            PTABLE_SIZE, PTABLE_ACCESS_READONLY);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vnode_modify_flags");
    }
    assert(err_is_ok(err));
    // create copy of pdpt cap
    struct capref copy;
    err = slot_alloc(&copy);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc");
    }
    assert(err_is_ok(err));
    err = cap_copy(copy, src->u.vnode.cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_copy");
    }
    assert(err_is_ok(err));

    err = alloc_vnode_noalloc(x86, parent, copy,
                      entry, dest);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "alloc_vnode_noalloc");
    }
    assert(*dest);
    // copy children metadata
    // XXX: should copy caps to keep revoke safety
    memcpy((*dest)->u.vnode.children, src->u.vnode.children,
            PTABLE_SIZE * sizeof(struct vnode *));

    return SYS_ERR_OK;
}
#else
#error Invalid pmap datastructure
#endif

static errval_t cow_get_pdir(struct pmap_x86 *pmap,
        genvaddr_t base, struct vnode **ptable)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}
/**
 * \brief Returns the vnode (potentially cloned) for `base'
 */
static errval_t cow_get_ptable(struct pmap_x86 *pmap,
        genvaddr_t base, struct vnode **ptable)
{
    errval_t err;
    struct vnode *pdir;
    err = cow_get_pdir(pmap, base, &pdir);
    if (err_is_fail(err)) {
        return err;
    }
    assert(pdir != NULL);
    assert(pdir->is_cloned);

    // PDIR mapping
    *ptable = find_vnode(pdir, X86_64_PDIR_BASE(base));
    if(*ptable == NULL) {
        err = alloc_vnode(pmap, pdir, ObjType_VNode_x86_64_ptable,
                            X86_64_PDIR_BASE(base), ptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    } else if (!(*ptable)->is_cloned) {
        // need to clone ptable to ensure copy on write
        struct vnode *newptable;
        err = alloc_vnode(pmap, pdir, ObjType_VNode_x86_64_ptable,
                            X86_64_PDIR_BASE(base), &newptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
        err = vnode_inherit(newptable->u.vnode.cap,
                (*ptable)->u.vnode.cap, 0, PTABLE_SIZE);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_CLONE_VNODE);
        }
        err = vnode_modify_flags(newptable->u.vnode.cap, 0, PTABLE_SIZE,
                PTABLE_ACCESS_READONLY);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_MODIFY_FLAGS);
        }
    }

    return SYS_ERR_OK;
}

static void handler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    errval_t err;
    DEBUG_COW("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    faddr = faddr;
    DEBUG_COW("got expected write pagefault on %p, creating copy of page\n", vaddr);
    struct vnode *ptable = NULL;
    struct capref newframe;
    void *temp;
    struct vregion *tempvr;
    size_t size = 0;
    struct pmap_x86 *pmap = (struct pmap_x86 *)get_current_pmap();
    err = cow_get_ptable(pmap, faddr, &ptable);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cow_get_ptable");
    }
    err = frame_alloc(&newframe, BASE_PAGE_SIZE, &size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_alloc");
    }
    // TODO: clone_frame(newframe, newoffset, frame, offset, size)
    vspace_map_one_frame(&temp, BASE_PAGE_SIZE, newframe, NULL, &tempvr);
    memcpy(temp, (void *)faddr, BASE_PAGE_SIZE);
    assert(size == BASE_PAGE_SIZE);
    vregion_destroy(tempvr);
    // TODO: allow overwrite mapping + TLB flush
    err = vnode_map(ptable->u.vnode.cap, newframe, X86_64_PTABLE_BASE(faddr),
            PTABLE_READ_WRITE, 0, 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_alloc");
    }
}

errval_t pmap_cow_init(void)
{
    errval_t err;
    err = thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}


errval_t pmap_setup_cow(struct vregion *vregion, void **retbuf)
{
    errval_t err;
    struct pmap *pmap = get_current_pmap();
    genvaddr_t vregion_base = vregion_get_base_addr(vregion);
    size_t vregion_size = vregion_get_size(vregion);

    size_t pml4e = X86_64_PML4_BASE(vregion_base);
    // no support for regions that are not in a single pml4e
    if (pml4e != X86_64_PML4_BASE(vregion_base + vregion_size - 1)) {
        debug_printf("vregion spanning pml4 entries\n");
        return LIB_ERR_NOT_IMPLEMENTED; //XXX
    }

    genvaddr_t new_vaddr;
    // XXX: right now this allocates pml4 entries
    err = pmap->f.determine_addr_raw(pmap, vregion_size, 0, &new_vaddr);
    assert(err_is_ok(err));
    size_t new_pml4e = X86_64_PML4_BASE(new_vaddr);
    if ((new_pml4e << 39) != new_vaddr) {
        USER_PANIC("new_vaddr not pml4e aligned: %"PRIxGENVADDR"\n",
                new_vaddr);
    }
    assert((new_pml4e << 39) == new_vaddr);
    DEBUG_COW("using pml4e %zu to alias pml4e %zu\n",
            new_pml4e, pml4e);

    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;

    // get pml4 vnode for region that we wanna cow
    cow_root_pte = find_vnode(&x86->root, pml4e);
    if (!cow_root_pte) {
        USER_PANIC("cow_root_pte NULL");
    }
    DEBUG_COW("cow_root_pte:%p\n", cow_root_pte);
    assert(cow_root_pte);

    // create vnode for new aliased mapping
    struct vnode *root_pte_copy = NULL;
    err = vnode_clone(x86, &x86->root, new_pml4e,
            &root_pte_copy, cow_root_pte);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vnode_clone");
    }
    assert(err_is_ok(err));

    //XXX: fix this if we have better determine_addr()
    *retbuf = (void *)new_vaddr;

    return err;
}
