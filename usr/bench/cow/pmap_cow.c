#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish_kpi/paging_target.h>
#include <assert.h>

#include "pmap_cow.h"
#include "debug.h"

static struct vnode *cow_root_pte = NULL;
#define EX_STACK_SIZE 16384
static char ex_stack[EX_STACK_SIZE];

static void handler(enum exception_type type, int subtype, void *vaddr,
        arch_registers_state_t *regs, arch_registers_fpu_state_t *fpuregs)
{
    DEBUG_COW("got exception %d(%d) on %p\n", type, subtype, vaddr);
    assert(type == EXCEPT_PAGEFAULT);
    assert(subtype == PAGEFLT_WRITE);
    uintptr_t addr = (uintptr_t) vaddr;
    uintptr_t faddr = addr & ~BASE_PAGE_MASK;
    faddr = faddr;
    DEBUG_COW("got expected write pagefault on %p, creating copy of page\n", vaddr);
    // TODO;
}

errval_t pmap_cow_init(void)
{
    errval_t err;
    err = thread_set_exception_handler(handler, NULL, ex_stack,
            ex_stack+EX_STACK_SIZE, NULL, NULL);
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}

errval_t pmap_setup_cow(struct vregion *vregion)
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
    err = pmap->f.determine_addr_raw(pmap, vregion_size, 0, &new_vaddr);
    assert(err_is_ok(err));
    size_t new_pml4e = X86_64_PML4_BASE(new_vaddr);
    debug_printf("using pml4e %zu to alias pml4e %zu\n",
            pml4e, new_pml4e);

    struct pmap_x86 *x86 = (struct pmap_x86*)pmap;
    // XXX
    x86 = x86;
    cow_root_pte = cow_root_pte;

    return LIB_ERR_NOT_IMPLEMENTED;
}
