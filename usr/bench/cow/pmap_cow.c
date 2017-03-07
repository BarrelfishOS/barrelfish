#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish_kpi/paging_target.h>
#include <assert.h>

#include "pmap_cow.h"
#include "debug.h"

static struct vnode *cow_root_pte = NULL;
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];

// default alloc 1MB
static size_t default_frame_bytes = 1ULL << 20;
static struct capref current_ram, current_frame;
static cslot_t current_slot_count = 0;
size_t get_ram_caps_count = 0;
static errval_t get_ram_caps(void)
{
    get_ram_caps_count ++;
    struct capref ram;
    size_t alloc_bytes = default_frame_bytes;
    errval_t err;
ram_alloc_retry:
    err = ram_alloc(&ram, log2ceil(alloc_bytes));
    if (err_no(err) == LIB_ERR_RAM_ALLOC_WRONG_SIZE) {
        DEBUG_COW("early ram_alloc, retry with BASE_PAGE_BITS\n");
        // this is probably before we have a connection to init and are using
        // ram_alloc_fixed() which can only do 4kB pages, so we don't yet
        // touch the default allocation size
        alloc_bytes = BASE_PAGE_SIZE;
        err = ram_alloc(&ram, BASE_PAGE_BITS);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "early ram_alloc failed\n");
            return err;
        }
    } else if (err_no(err) == MM_ERR_NOT_FOUND && alloc_bytes > BASE_PAGE_SIZE) {
        DEBUG_COW("err: %s\n", err_getstring(err));
        default_frame_bytes >>= 1; // halve default allocation size
        DEBUG_COW("smaller allocation size: %zd\n", default_frame_bytes);
        alloc_bytes = default_frame_bytes;
        goto ram_alloc_retry;
    } else if (err_is_fail(err)) {
        debug_printf("error in ram_alloc: %s\n", err_getstring(err));
        return err;
    }
    // make sure we have a RAM cap that is a multiple of base pages
    assert(alloc_bytes >= BASE_PAGE_SIZE);
    assert(alloc_bytes % BASE_PAGE_SIZE == 0);

    // retype into 4k caps in new cnode
    cslot_t slots_needed = alloc_bytes / BASE_PAGE_SIZE;
    current_slot_count = slots_needed;
    debug_printf("slots_needed = %"PRIuCSLOT"\n", slots_needed);
    if (slots_needed == 1) {
        USER_PANIC("OOM");
    }
    if (slots_needed < L2_CNODE_SLOTS) {
        debug_printf("slowly running out of RAM: only got %d pages\n", slots_needed);
    }
    assert(slots_needed > 1);
    assert(slots_needed <= L2_CNODE_SLOTS);
    struct capref nextcncap;
    struct cnoderef nextcn;
    DEBUG_COW("%s: need CNode with %d slots\n", __FUNCTION__, slots_needed);
    err = cnode_create_l2(&nextcncap, &nextcn);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cnode_create");
        return err;
    }
    current_ram = (struct capref) {
        .cnode = nextcn,
        .slot = 0,
    };
    // Create empty cnode for retypes to frames/ptables in cow_get_page
    err = cnode_create_l2(&nextcncap, &nextcn);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "cnode_create");
        return err;
    }
    current_frame = (struct capref) {
        .cnode = nextcn,
        .slot = 0,
    };

    // Retype into 4kB RAM caps
    err = cap_retype(current_ram, ram, 0, ObjType_RAM, BASE_PAGE_SIZE, slots_needed);
    if (err_is_fail(err)) {
        debug_printf("error in cap_retype: %s\n", err_getstring(err));
        return err;
    }

    return SYS_ERR_OK;
}

size_t cow_get_page_count = 0;
static errval_t cow_get_page(struct capref *f, enum objtype type)
{
    cow_get_page_count++;
    errval_t err;
    assert(f);
    if (current_slot_count == 0 || current_ram.slot == current_slot_count) {
        err = get_ram_caps();
        if (err_is_fail(err)) {
            return err;
        }
    }
    err = cap_retype(current_frame, current_ram, 0, type, BASE_PAGE_SIZE, 1);
    if (err_is_fail(err)) {
        return err;
    }
    *f = current_frame;
    current_frame.slot++;
    current_ram.slot++;
    return SYS_ERR_OK;
}

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

    err = slot_alloc(&newvnode->mapping);
    assert(err_is_ok(err));

    // Map it
    err = vnode_map(root->u.vnode.cap, newvnode->u.vnode.cap, entry,
                    PTABLE_ACCESS_DEFAULT, 0, 1, newvnode->mapping);
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
    // Get the VNode capability
    err = cow_get_page(&vnodecap, type);
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

size_t cow_pt_alloc_count = 0, cow_pd_alloc_count = 0, cow_pdpt_alloc_count = 0;
static errval_t find_or_clone_vnode(struct pmap_x86 *pmap,
        struct vnode *parent, enum objtype type,
        size_t entry, struct vnode **ptable)
{
    errval_t err;
    *ptable = find_vnode(parent, entry);
    if (*ptable == NULL || !(*ptable)->is_cloned) {
        switch(type) {
            case ObjType_VNode_x86_64_ptable:
                cow_pt_alloc_count++;
                break;
            case ObjType_VNode_x86_64_pdir:
                cow_pd_alloc_count++;
                break;
            case ObjType_VNode_x86_64_pdpt:
                cow_pdpt_alloc_count++;
                break;
            default:
                break;
        }
    }
    if(*ptable == NULL) {
        err = alloc_vnode(pmap, parent, type, entry, ptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
    } else if (!(*ptable)->is_cloned) {
        // need to clone ptable to ensure copy on write
        struct vnode *newptable;
        err = alloc_vnode(pmap, parent, type, entry, &newptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
        err = vnode_inherit_attr(newptable->u.vnode.cap,
                (*ptable)->u.vnode.cap, 0, PTABLE_SIZE, PTABLE_ACCESS_READONLY);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_CLONE_VNODE);
        }
        memcpy(newptable->u.vnode.children, (*ptable)->u.vnode.children,
                PTABLE_SIZE * sizeof(struct vnode *));
        newptable->is_cloned = true;
        *ptable = newptable;
    }
    assert(*ptable);

    return SYS_ERR_OK;
}

// assume that we created a struct vnode but didn't clone the actual pte page
// for pml4 entries
static errval_t cow_get_pdpt(struct pmap_x86 *pmap,
        genvaddr_t base, struct vnode **pdpt)
{
    DEBUG_COW("%s: %"PRIxGENVADDR"\n", __FUNCTION__, base);
    errval_t err;
    struct vnode *root = &pmap->root;
    size_t entry = X86_64_PML4_BASE(base);
    *pdpt = find_vnode(root, entry);
    assert(*pdpt);
    DEBUG_COW("%s: is_cloned=%d\n", __FUNCTION__, (*pdpt)->is_cloned);
    if (!(*pdpt)->is_cloned) {
        // need to clone ptable to ensure copy on write
        struct vnode *newptable;
        err = alloc_vnode(pmap, root, ObjType_VNode_x86_64_pdpt, entry,
                &newptable);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_VNODE);
        }
        err = vnode_inherit_attr(newptable->u.vnode.cap,
                (*pdpt)->u.vnode.cap, 0, PTABLE_SIZE, PTABLE_ACCESS_READONLY);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_CLONE_VNODE);
        }
        memcpy(newptable->u.vnode.children, (*pdpt)->u.vnode.children,
                PTABLE_SIZE * sizeof(struct vnode *));
        newptable->is_cloned = true;
        *pdpt = newptable;
    }
    return SYS_ERR_OK;
}

static errval_t cow_get_pdir(struct pmap_x86 *pmap,
        genvaddr_t base, struct vnode **pdir)
{
    DEBUG_COW("%s: %"PRIxGENVADDR"\n", __FUNCTION__, base);
    errval_t err;
    struct vnode *pdpt = NULL;
    err = cow_get_pdpt(pmap, base, &pdpt);
    if (err_is_fail(err)) {
        return err;
    }
    assert(pdpt != NULL);
    assert(pdpt->is_cloned);

    return find_or_clone_vnode(pmap, pdpt,
            ObjType_VNode_x86_64_pdir,
            X86_64_PDPT_BASE(base), pdir);
}
/**
 * \brief Returns the vnode (potentially cloned) for `base'
 */
static errval_t cow_get_ptable(struct pmap_x86 *pmap,
        genvaddr_t base, struct vnode **ptable)
{
    DEBUG_COW("%s: %"PRIxGENVADDR"\n", __FUNCTION__, base);
    errval_t err;
    struct vnode *pdir;
    err = cow_get_pdir(pmap, base, &pdir);
    if (err_is_fail(err)) {
        return err;
    }
    assert(pdir != NULL);
    assert(pdir->is_cloned);

    return find_or_clone_vnode(pmap, pdir,
            ObjType_VNode_x86_64_ptable,
            X86_64_PDIR_BASE(base), ptable);
}

static paging_x86_64_flags_t vregion_to_pmap_flag(vregion_flags_t vregion_flags)
{
    paging_x86_64_flags_t pmap_flags =
        PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE;

    if (!(vregion_flags & VREGION_FLAGS_GUARD)) {
        if (vregion_flags & VREGION_FLAGS_WRITE) {
            pmap_flags |= PTABLE_READ_WRITE;
        }
        if (vregion_flags & VREGION_FLAGS_EXECUTE) {
            pmap_flags &= ~PTABLE_EXECUTE_DISABLE;
        }
        if (vregion_flags & VREGION_FLAGS_NOCACHE) {
            pmap_flags |= PTABLE_CACHE_DISABLED;
        }
        else if (vregion_flags & VREGION_FLAGS_WRITE_COMBINING) {
            // PA4 is configured as write-combining
            pmap_flags |= PTABLE_ATTR_INDEX;
        }
    }

    return pmap_flags;
}

static exception_handler_fn next_handler = NULL;
static void cow_handler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    errval_t err;
    DEBUG_COW("got exception %d(%d) on %p\n", type, subtype, vaddr);
    if (next_handler && type != EXCEPT_PAGEFAULT) {
        next_handler(type, subtype, vaddr, regs, fpuregs);
    }
    assert(type == EXCEPT_PAGEFAULT);
    if (next_handler && subtype != PAGEFLT_WRITE) {
        next_handler(type, subtype, vaddr, regs, fpuregs);
    }
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    // TODO: check whether fault inside a registered COW region
    DEBUG_COW("got write pagefault on %p, creating copy of page\n", vaddr);
    struct vnode *ptable = NULL;
    struct capref newframe;
    struct pmap_x86 *pmap = (struct pmap_x86 *)get_current_pmap();
    err = cow_get_ptable(pmap, faddr, &ptable);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cow_get_ptable");
    }
    err = cow_get_page(&newframe, ObjType_Frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cow_get_page");
    }
    struct capref mapping;
    err = slot_alloc(&mapping);
    assert(err_is_ok(err));
    err = vnode_copy_remap(ptable->u.vnode.cap, newframe, X86_64_PTABLE_BASE(faddr),
            vregion_to_pmap_flag(VREGION_FLAGS_READ_WRITE), 0, 1, mapping);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame_alloc");
    }
}

errval_t pmap_cow_init(void)
{
    errval_t err;
    err = thread_set_exception_handler(cow_handler, &next_handler, ex_stack,
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

    default_frame_bytes = L2_CNODE_SLOTS * BASE_PAGE_SIZE;
    DEBUG_COW("setting up frame pool (%uMB) for remapping pages\n",
            default_frame_bytes / 1024 / 1024);
    err = get_ram_caps();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_frames");
    }

    //XXX: fix this if we have better determine_addr()
    *retbuf = (void *)new_vaddr;

    return err;
}
