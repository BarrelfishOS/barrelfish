#include <kernel.h>
#include <capabilities.h>
#include <paging_kernel_arch.h>

#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"

void paging_context_switch(lpaddr_t ttbr)
{
    panic("NYI");
}

errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    panic("NYI");
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    panic("NYI");
}
