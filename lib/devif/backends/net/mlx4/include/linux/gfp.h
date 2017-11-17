/*-
 * Copyright (c) 2010 Isilon Systems, Inc.
 * Copyright (c) 2010 iX Systems, Inc.
 * Copyright (c) 2010 Panasas, Inc.
 * Copyright (c) 2013 Mellanox Technologies, Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice unmodified, this list of conditions, and the following
 *    disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef	_LINUX_GFP_H_
#define	_LINUX_GFP_H_

#include <barrelfish/barrelfish.h>
#include <linux/page.h>
/*
 #include <sys/cdefs.h>
 #include <sys/types.h>
 #include <sys/systm.h>
 #include <sys/malloc.h>

 #include <linux/page.h>

 #include <vm/vm_param.h>
 #include <vm/vm_object.h>
 #include <vm/vm_extern.h>
 #include <vm/vm_kern.h>
 */
#define	__GFP_NOWARN	0
#define	__GFP_HIGHMEM	0
/*
 #define	__GFP_ZERO	M_ZERO
 */
#define	M_NOWAIT	0x0001		/* do not block */
#define	M_USE_RESERVE	0x0400		/* can alloc out of reserve memory */
#define	GFP_NOWAIT	M_NOWAIT
#define	GFP_ATOMIC	(M_NOWAIT | M_USE_RESERVE)

#define	GFP_KERNEL	0/*M_WAITOK*/
#define	GFP_USER	0/*M_WAITOK*/

#define	GFP_HIGHUSER	0x0002		/* ok to block */ /*was M_WAITOK*/
/*
 #define	GFP_HIGHUSER_MOVABLE	M_WAITOK
 #define	GFP_IOFS	M_NOWAIT

 static inline void *
 page_address(struct page *page)
 {

 if (page->object != kmem_object && page->object != kernel_object)
 return (NULL);
 return ((void *)(uintptr_t)(VM_MIN_KERNEL_ADDRESS +
 IDX_TO_OFF(page->pindex)));
 }

 static inline unsigned long
 _get_page(gfp_t mask)
 {

 return kmem_malloc(kmem_arena, PAGE_SIZE, mask);
 }

 #define	get_zeroed_page(mask)	_get_page((mask) | M_ZERO)
 #define	alloc_page(mask)	virt_to_page(_get_page((mask)))
 #define	__get_free_page(mask)	_get_page((mask))

 static inline void
 free_page(unsigned long page)
 {

 if (page == 0)
 return;
 kmem_free(kmem_arena, page, PAGE_SIZE);
 }

 static inline void
 __free_page(struct page *m)
 {

 if (m->object != kmem_object)
 panic("__free_page:  Freed page %p not allocated via wrappers.",
 m);
 kmem_free(kmem_arena, (vm_offset_t)page_address(m), PAGE_SIZE);
 }

 static inline void
 __free_pages(struct page *m, unsigned int order)
 {
 size_t size;

 if (m == NULL)
 return;
 size = PAGE_SIZE << order;
 kmem_free(kmem_arena, (vm_offset_t)page_address(m), size);
 }

 static inline void free_pages(uintptr_t addr, unsigned int order)
 {
 if (addr == 0)
 return;
 __free_pages(virt_to_page((void *)addr), order);
 }
 */

/* Alloc pages allocates directly from the buddy allocator on linux so
 * order specifies a power of two bucket of pages and the results
 * are expected to be aligned on the size as well.*/

static inline void *
dma_alloc(unsigned int size, genpaddr_t *phys_addr) {
	errval_t err;
	struct frame_identity fid;
	struct capref slot;
	void * virt_addr;

	err = frame_alloc(&slot, size, NULL);
	if (err_is_fail(err)) {
		return NULL;
	}

	err = invoke_frame_identify(slot, &fid);
	if (err_is_fail(err)) {
		return NULL;
	}

	*phys_addr = fid.base; //this is the physical address

	err = vspace_map_one_frame_attr(&virt_addr, size, slot,
	VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
	if (err_is_fail(err)) {
		return NULL;
	}

	/*printf("_paddr1: %"PRIx64"\n", fid.base);*/
	/*memset(virt_addr, 0, size);*/

	return virt_addr;
}

/*void dma_free(void *vaddr) {
 errval_t cleanup_err;
 struct capref *slot;
 struct pmap *pmap;

 pmap = get_current_pmap();
 assert(!!pmap);

 cleanup_err = lookup(pmap, (genvaddr_t) vaddr, NULL, NULL, slot, NULL,
 NULL);
 assert(err_is_ok(cleanup_err));

 TODO: vspace_lookup_vregion

 cleanup_err = vspace_unmap(vaddr);
 assert(err_is_ok(cleanup_err));

 cleanup_err = cap_destroy(*slot);
 assert(err_is_ok(cleanup_err));
 }*/

static inline struct page *
alloc_pages(unsigned int order) {
	struct page *page;
	struct frame_identity fid;
	size_t size;
	errval_t err;
	/*uint64_t minbase, maxlimit;*/

	size = BASE_PAGE_SIZE << order;

	page = malloc(sizeof *page);
	if (page == 0)
		return (NULL);

	/*ram_get_affinity(&minbase, &maxlimit);
	 ram_set_affinity(0xFFFFFFFF, 0xF00000000);*/

	err = frame_alloc(&page->slot, size, NULL);
	if (err_is_fail(err)) {
		return NULL;
	}

	err = invoke_frame_identify(page->slot, &fid);
	if (err_is_fail(err)) {
		return NULL;
	}

	page->phys_addr = fid.base; //this is the physical address
	//page->bits = fid.bits;

	err = vspace_map_one_frame_attr(&page->virt_addr, size, page->slot,
	VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
	if (err_is_fail(err)) {
		return NULL;
	}

	/*printf("_paddr2: %"PRIx64"\n", fid.base);*/
	/*memset(page->virt_addr, 0, size);*/

	return page;
}
/*
 static inline uintptr_t __get_free_pages(gfp_t gfp_mask, unsigned int order)
 {
 struct page *page;

 page = alloc_pages(gfp_mask, order);
 if (page == NULL)
 return (0);
 return ((uintptr_t)page_address(page));
 }

 #define alloc_pages_node(node, mask, order)     alloc_pages(mask, order)

 #define kmalloc_node(chunk, mask, node)         kmalloc(chunk, mask)
 */

#endif	/* _LINUX_GFP_H_ */
