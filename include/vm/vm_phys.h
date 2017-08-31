/*-
 * Copyright (c) 2002-2006 Rice University
 * Copyright (c) 2007 Alan L. Cox <alc@cs.rice.edu>
 * All rights reserved.
 *
 * This software was developed for the FreeBSD Project by Alan L. Cox,
 * Olivier Crameri, Peter Druschel, Sitaram Iyer, and Juan Navarro.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
 * HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
 * WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * $FreeBSD$
 */

/*
 *	Physical memory system definitions
 */

#ifndef	_VM_PHYS_H_
#define	_VM_PHYS_H_

#ifdef _KERNEL

/* Domains must be dense (non-sparse) and zero-based. */
struct mem_affinity {
	vm_paddr_t start;
	vm_paddr_t end;
	int domain;
};

struct vm_freelist {
	struct pglist pl;
	int lcnt;
};

struct vm_phys_seg {
	vm_paddr_t	start;
	vm_paddr_t	end;
	vm_page_t	first_page;
	int		domain;
	struct vm_freelist (*free_queues)[VM_NFREEPOOL][VM_NFREEORDER];
};

extern struct mem_affinity *mem_affinity;
extern int *mem_locality;
extern int vm_ndomains;
extern struct vm_phys_seg vm_phys_segs[];
extern int vm_phys_nsegs;

/*
 * The following functions are only to be used by the virtual memory system.
 */
void vm_phys_add_page(vm_paddr_t pa);
void vm_phys_add_seg(vm_paddr_t start, vm_paddr_t end);
vm_page_t vm_phys_alloc_contig(u_long npages, vm_paddr_t low, vm_paddr_t high,
    u_long alignment, vm_paddr_t boundary);
vm_page_t vm_phys_alloc_freelist_pages(int freelist, int pool, int order);
vm_page_t vm_phys_alloc_pages(int pool, int order);
boolean_t vm_phys_domain_intersects(long mask, vm_paddr_t low, vm_paddr_t high);
int vm_phys_fictitious_reg_range(vm_paddr_t start, vm_paddr_t end,
    vm_memattr_t memattr);
void vm_phys_fictitious_unreg_range(vm_paddr_t start, vm_paddr_t end);
vm_page_t vm_phys_fictitious_to_vm_page(vm_paddr_t pa);
void vm_phys_free_contig(vm_page_t m, u_long npages);
void vm_phys_free_pages(vm_page_t m, int order);
void vm_phys_init(void);
vm_page_t vm_phys_paddr_to_vm_page(vm_paddr_t pa);
vm_page_t vm_phys_scan_contig(u_long npages, vm_paddr_t low, vm_paddr_t high,
    u_long alignment, vm_paddr_t boundary, int options);
void vm_phys_set_pool(int pool, vm_page_t m, int order);
boolean_t vm_phys_unfree_page(vm_page_t m);
int vm_phys_mem_affinity(int f, int t);

/*
 *	vm_phys_domain:
 *
 * 	Return the memory domain the page belongs to.
 */
static inline struct vm_domain *
vm_phys_domain(vm_page_t m)
{
#ifdef VM_NUMA_ALLOC
	int domn, segind;

	/* XXXKIB try to assert that the page is managed */
	segind = m->segind;
	KASSERT(segind < vm_phys_nsegs, ("segind %d m %p", segind, m));
	domn = vm_phys_segs[segind].domain;
	KASSERT(domn < vm_ndomains, ("domain %d m %p", domn, m));
	return (&vm_dom[domn]);
#else
	return (&vm_dom[0]);
#endif
}

static inline void
vm_phys_freecnt_adj(vm_page_t m, int adj)
{

	mtx_assert(&vm_page_queue_free_mtx, MA_OWNED);
	vm_cnt.v_free_count += adj;
	vm_phys_domain(m)->vmd_free_count += adj;
}

#endif	/* _KERNEL */
#endif	/* !_VM_PHYS_H_ */
