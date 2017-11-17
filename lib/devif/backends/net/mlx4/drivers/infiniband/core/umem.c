/*

 * Copyright (c) 2005 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005 Cisco Systems.  All rights reserved.
 * Copyright (c) 2005 Mellanox Technologies. All rights reserved.
 *
 * This software is available to you under a choice of one of two
 * licenses.  You may choose to be licensed under the terms of the GNU
 * General Public License (GPL) Version 2, available from the file
 * COPYING in the main directory of this source tree, or the
 * OpenIB.org BSD license below:
 *
 *     Redistribution and use in source and binary forms, with or
 *     without modification, are permitted provided that the following
 *     conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.


 #include <linux/mm.h>
 #include <linux/dma-mapping.h>
 #include <linux/sched.h>
 #ifdef __linux__
 #include <linux/hugetlb.h>
 #endif
 #include <linux/dma-attrs.h>

 #include <sys/priv.h>
 #include <sys/resource.h>
 #include <sys/resourcevar.h>

 #include <vm/vm.h>
 #include <vm/vm_map.h>
 #include <vm/vm_object.h>
 #include <vm/vm_pageout.h>

 #include "uverbs.h"

 static int allow_weak_ordering;
 module_param(allow_weak_ordering, bool, 0444);
 MODULE_PARM_DESC(allow_weak_ordering,  "Allow weak ordering for data registered memory");

 #define IB_UMEM_MAX_PAGE_CHUNK						\
	((PAGE_SIZE - offsetof(struct ib_umem_chunk, page_list)) /	\
	 ((void *) &((struct ib_umem_chunk *) 0)->page_list[1] -	\
	  (void *) &((struct ib_umem_chunk *) 0)->page_list[0]))

 #ifdef __ia64__
 extern int dma_map_sg_hp_wa;

 static int dma_map_sg_ia64(struct ib_device *ibdev,
 struct scatterlist *sg,
 int nents,
 enum dma_data_direction dir)
 {
 int i, rc, j, lents = 0;
 struct device *dev;

 if (!dma_map_sg_hp_wa)
 return ib_dma_map_sg(ibdev, sg, nents, dir);

 dev = ibdev->dma_device;
 for (i = 0; i < nents; ++i) {
 rc = dma_map_sg(dev, sg + i, 1, dir);
 if (rc <= 0) {
 for (j = 0; j < i; ++j)
 dma_unmap_sg(dev, sg + j, 1, dir);

 return 0;
 }
 lents += rc;
 }

 return lents;
 }

 static void dma_unmap_sg_ia64(struct ib_device *ibdev,
 struct scatterlist *sg,
 int nents,
 enum dma_data_direction dir)
 {
 int i;
 struct device *dev;

 if (!dma_map_sg_hp_wa)
 return ib_dma_unmap_sg(ibdev, sg, nents, dir);

 dev = ibdev->dma_device;
 for (i = 0; i < nents; ++i)
 dma_unmap_sg(dev, sg + i, 1, dir);
 }

 #define ib_dma_map_sg(dev, sg, nents, dir) dma_map_sg_ia64(dev, sg, nents, dir)
 #define ib_dma_unmap_sg(dev, sg, nents, dir) dma_unmap_sg_ia64(dev, sg, nents, dir)

 #endif

 static void __ib_umem_release(struct ib_device *dev, struct ib_umem *umem, int dirty)
 {
 #ifdef __linux__
 struct ib_umem_chunk *chunk, *tmp;
 int i;

 list_for_each_entry_safe(chunk, tmp, &umem->chunk_list, list) {
 ib_dma_unmap_sg_attrs(dev, chunk->page_list,
 chunk->nents, DMA_BIDIRECTIONAL, &chunk->attrs);
 for (i = 0; i < chunk->nents; ++i) {
 struct page *page = sg_page(&chunk->page_list[i]);
 if (umem->writable && dirty)
 set_page_dirty_lock(page);
 put_page(page);
 }
 kfree(chunk);
 }
 #else
 struct ib_umem_chunk *chunk, *tmp;
 vm_object_t object;
 int i;

 object = NULL;
 list_for_each_entry_safe(chunk, tmp, &umem->chunk_list, list) {
 ib_dma_unmap_sg_attrs(dev, chunk->page_list,
 chunk->nents, DMA_BIDIRECTIONAL, &chunk->attrs);
 for (i = 0; i < chunk->nents; ++i) {
 struct page *page = sg_page(&chunk->page_list[i]);
 if (umem->writable && dirty) {
 if (object && object != page->object)
 VM_OBJECT_WUNLOCK(object);
 if (object != page->object) {
 object = page->object;
 VM_OBJECT_WLOCK(object);
 }
 vm_page_dirty(page);
 }
 }
 kfree(chunk);
 }
 if (object)
 VM_OBJECT_WUNLOCK(object);

 #endif
 }

 *
 * ib_umem_get - Pin and DMA map userspace memory.
 * @context: userspace context to pin memory for
 * @addr: userspace virtual address to start at
 * @size: length of region to pin
 * @access: IB_ACCESS_xxx flags for memory being pinned
 * @dmasync: flush in-flight DMA when the memory region is written
 */
struct ib_umem *ib_umem_get(struct ib_ucontext *context, unsigned long addr,
		size_t size, int access, int dmasync) {

	/*
	 #ifdef bla
	 struct ib_umem *umem;
	 struct page **page_list;
	 struct vm_area_struct **vma_list;
	 struct ib_umem_chunk *chunk;
	 unsigned long locked;
	 unsigned long lock_limit;
	 unsigned long cur_base;
	 unsigned long npages;
	 int ret;
	 int off;
	 int i;
	 DEFINE_DMA_ATTRS(attrs);

	 if (dmasync)
	 dma_set_attr(DMA_ATTR_WRITE_BARRIER, &attrs);
	 else if (allow_weak_ordering)
	 dma_set_attr(DMA_ATTR_WEAK_ORDERING, &attrs);

	 if (!can_do_mlock())
	 return ERR_PTR(-EPERM);

	 umem = kmalloc(sizeof *umem, GFP_KERNEL);
	 if (!umem)
	 return ERR_PTR(-ENOMEM);

	 umem->context   = context;
	 umem->length    = size;
	 umem->offset    = addr & ~PAGE_MASK;
	 umem->page_size = PAGE_SIZE;

	 * We ask for writable memory if any access flags other than
	 * "remote read" are set.  "Local write" and "remote write"
	 * obviously require write access.  "Remote atomic" can do
	 * things like fetch and add, which will modify memory, and
	 * "MW bind" can change permissions by binding a window.

	 umem->writable  = !!(access & ~IB_ACCESS_REMOTE_READ);

	 We assume the memory is from hugetlb until proved otherwise
	 umem->hugetlb   = 1;

	 INIT_LIST_HEAD(&umem->chunk_list);

	 page_list = (struct page **) __get_free_page(GFP_KERNEL);
	 if (!page_list) {
	 kfree(umem);
	 return ERR_PTR(-ENOMEM);
	 }


	 * if we can't alloc the vma_list, it's not so bad;
	 * just assume the memory is not hugetlb memory

	 vma_list = (struct vm_area_struct **) __get_free_page(GFP_KERNEL);
	 if (!vma_list)
	 umem->hugetlb = 0;

	 npages = PAGE_ALIGN(size + umem->offset) >> PAGE_SHIFT;

	 down_write(&current->mm->mmap_sem);

	 locked     = npages + current->mm->locked_vm;
	 lock_limit = current->signal->rlim[RLIMIT_MEMLOCK].rlim_cur >> PAGE_SHIFT;

	 if ((locked > lock_limit) && !capable(CAP_IPC_LOCK)) {
	 ret = -ENOMEM;
	 goto out;
	 }

	 cur_base = addr & PAGE_MASK;

	 ret = 0;

	 while (npages) {
	 ret = get_user_pages(current, current->mm, cur_base,
	 min_t(unsigned long, npages,
	 PAGE_SIZE / sizeof (struct page *)),
	 1, !umem->writable, page_list, vma_list);

	 if (ret < 0)
	 goto out;

	 cur_base += ret * PAGE_SIZE;
	 npages   -= ret;

	 off = 0;

	 while (ret) {
	 chunk = kmalloc(sizeof *chunk + sizeof (struct scatterlist) *
	 min_t(int, ret, IB_UMEM_MAX_PAGE_CHUNK),
	 GFP_KERNEL);
	 if (!chunk) {
	 ret = -ENOMEM;
	 goto out;
	 }

	 chunk->attrs = attrs;
	 chunk->nents = min_t(int, ret, IB_UMEM_MAX_PAGE_CHUNK);
	 sg_init_table(chunk->page_list, chunk->nents);
	 for (i = 0; i < chunk->nents; ++i) {
	 if (vma_list &&
	 !is_vm_hugetlb_page(vma_list[i + off]))
	 umem->hugetlb = 0;
	 sg_set_page(&chunk->page_list[i], page_list[i + off], PAGE_SIZE, 0);
	 }

	 chunk->nmap = ib_dma_map_sg_attrs(context->device,
	 &chunk->page_list[0],
	 chunk->nents,
	 DMA_BIDIRECTIONAL,
	 &attrs);
	 if (chunk->nmap <= 0) {
	 for (i = 0; i < chunk->nents; ++i)
	 put_page(sg_page(&chunk->page_list[i]));
	 kfree(chunk);

	 ret = -ENOMEM;
	 goto out;
	 }

	 ret -= chunk->nents;
	 off += chunk->nents;
	 list_add_tail(&chunk->list, &umem->chunk_list);
	 }

	 ret = 0;
	 }

	 out:
	 if (ret < 0) {
	 __ib_umem_release(context->device, umem, 0);
	 kfree(umem);
	 } else
	 current->mm->locked_vm = locked;

	 up_write(&current->mm->mmap_sem);
	 if (vma_list)
	 free_page((unsigned long) vma_list);
	 free_page((unsigned long) page_list);

	 return ret < 0 ? ERR_PTR(ret) : umem;
	 #else
	 */

	struct ib_umem *umem;
	struct ib_umem_chunk *chunk;
	struct proc *proc;
	pmap_t pmap;
	vm_offset_t end, last, start;
	vm_size_t npages;
	int error;
	int ents;
	int ret;
	int i;
	DEFINE_DMA_ATTRS(attrs);

	error = priv_check(curthread, PRIV_VM_MLOCK);
	if (error)
		return ERR_PTR(-error);

	last = addr + size;
	start = addr & PAGE_MASK; /*Use the linux PAGE_MASK definition. */
	end = roundup2(last, PAGE_SIZE); /* Use PAGE_MASK safe operation. */
	if (last < addr || end < addr)
		return ERR_PTR(-EINVAL);
	npages = atop(end - start);
	if (npages > vm_page_max_wired)
		return ERR_PTR(-ENOMEM);
	umem = kzalloc(sizeof *umem, GFP_KERNEL);
	if (!umem)
		return ERR_PTR(-ENOMEM);
	proc = curthread->td_proc;
	PROC_LOCK(proc);
	if (ptoa(npages + pmap_wired_count(vm_map_pmap(&proc->p_vmspace->vm_map)))
			> lim_cur(proc, RLIMIT_MEMLOCK)) {
		PROC_UNLOCK(proc);
		kfree(umem);
		return ERR_PTR(-ENOMEM);
	}
	PROC_UNLOCK(proc);
	if (npages + cnt.v_wire_count > vm_page_max_wired) {
		kfree(umem);
		return ERR_PTR(-EAGAIN);
	}
	error = vm_map_wire(&proc->p_vmspace->vm_map, start, end,
			VM_MAP_WIRE_USER | VM_MAP_WIRE_NOHOLES
					| (umem->writable ? VM_MAP_WIRE_WRITE : 0));
	if (error != KERN_SUCCESS) {
		kfree(umem);
		return ERR_PTR(-ENOMEM);
	}

	umem->context = context;
	umem->length = size;
	umem->offset = addr & ~PAGE_MASK;
	umem->page_size = PAGE_SIZE;
	umem->start = addr;

	/** We ask for writable memory if any access flags other than
	 * "remote read" are set.  "Local write" and "remote write"
	 * obviously require write access.  "Remote atomic" can do
	 * things like fetch and add, which will modify memory, and
	 * "MW bind" can change permissions by binding a window.*/

	umem->writable = !!(access & ~IB_ACCESS_REMOTE_READ);
	umem->hugetlb = 0;
	INIT_LIST_HEAD(&umem->chunk_list);

	pmap = vm_map_pmap(&proc->p_vmspace->vm_map);
	ret = 0;
	while (npages) {
		ents = min_t(int, npages, IB_UMEM_MAX_PAGE_CHUNK);
		chunk = kmalloc(sizeof(*chunk) + (sizeof(struct scatterlist) * ents),
				GFP_KERNEL);
		if (!chunk) {
			ret = -ENOMEM;
			goto out;
		}

		chunk->attrs = attrs;
		chunk->nents = ents;
		sg_init_table(&chunk->page_list[0], ents);
		for (i = 0; i < chunk->nents; ++i) {
			vm_paddr_t pa;

			pa = pmap_extract(pmap, start);
			if (pa == 0) {
				ret = -ENOMEM;
				kfree(chunk);
				goto out;
			}
			sg_set_page(&chunk->page_list[i], PHYS_TO_VM_PAGE(pa), PAGE_SIZE,
					0);
			npages--;
			start += PAGE_SIZE;
		}

		chunk->nmap = ib_dma_map_sg_attrs(context->device, &chunk->page_list[0],
				chunk->nents, DMA_BIDIRECTIONAL, &attrs);
		if (chunk->nmap != chunk->nents) {
			kfree(chunk);
			ret = -ENOMEM;
			goto out;
		}

		list_add_tail(&chunk->list, &umem->chunk_list);
	}

	out: if (ret < 0) {
		__ib_umem_release(context->device, umem, 0);
		kfree(umem);
	}

	return ret < 0 ? ERR_PTR(ret) : umem;
	/*#endif*/
}
/*
 EXPORT_SYMBOL(ib_umem_get);

 #ifdef __linux__
 static void ib_umem_account(struct work_struct *work)
 {
 struct ib_umem *umem = container_of(work, struct ib_umem, work);

 down_write(&umem->mm->mmap_sem);
 umem->mm->locked_vm -= umem->diff;
 up_write(&umem->mm->mmap_sem);
 mmput(umem->mm);
 kfree(umem);
 }
 #endif

 *
 * ib_umem_release - release memory pinned with ib_umem_get
 * @umem: umem struct to release

 void ib_umem_release(struct ib_umem *umem)
 {
 #ifdef __linux__
 struct ib_ucontext *context = umem->context;
 struct mm_struct *mm;
 unsigned long diff;

 __ib_umem_release(umem->context->device, umem, 1);

 mm = get_task_mm(current);
 if (!mm) {
 kfree(umem);
 return;
 }

 diff = PAGE_ALIGN(umem->length + umem->offset) >> PAGE_SHIFT;


 * We may be called with the mm's mmap_sem already held.  This
 * can happen when a userspace munmap() is the call that drops
 * the last reference to our file and calls our release
 * method.  If there are memory regions to destroy, we'll end
 * up here and not be able to take the mmap_sem.  In that case
 * we defer the vm_locked accounting to the system workqueue.

 if (context->closing) {
 if (!down_write_trylock(&mm->mmap_sem)) {
 INIT_WORK(&umem->work, ib_umem_account);
 umem->mm   = mm;
 umem->diff = diff;

 schedule_work(&umem->work);
 return;
 }
 } else
 down_write(&mm->mmap_sem);

 current->mm->locked_vm -= diff;
 up_write(&mm->mmap_sem);
 mmput(mm);
 #else
 vm_offset_t addr, end, last, start;
 vm_size_t size;
 int error;

 __ib_umem_release(umem->context->device, umem, 1);
 if (umem->context->closing) {
 kfree(umem);
 return;
 }
 error = priv_check(curthread, PRIV_VM_MUNLOCK);
 if (error)
 return;
 addr = umem->start;
 size = umem->length;
 last = addr + size;
 start = addr & PAGE_MASK;  Use the linux PAGE_MASK definition.
 end = roundup2(last, PAGE_SIZE);  Use PAGE_MASK safe operation.
 vm_map_unwire(&curthread->td_proc->p_vmspace->vm_map, start, end,
 VM_MAP_WIRE_USER | VM_MAP_WIRE_NOHOLES);

 #endif
 kfree(umem);
 }
 EXPORT_SYMBOL(ib_umem_release);

 int ib_umem_page_count(struct ib_umem *umem)
 {
 struct ib_umem_chunk *chunk;
 int shift;
 int i;
 int n;

 shift = ilog2(umem->page_size);

 n = 0;
 list_for_each_entry(chunk, &umem->chunk_list, list)
 for (i = 0; i < chunk->nmap; ++i)
 n += sg_dma_len(&chunk->page_list[i]) >> shift;

 return n;
 }
 EXPORT_SYMBOL(ib_umem_page_count);

 ********************************************

 * Stub functions for contiguous pages - 
 * We currently do not support this feature

 ********************************************

 *
 * ib_cmem_release_contiguous_pages - release memory allocated by
 *                                              ib_cmem_alloc_contiguous_pages.
 * @cmem: cmem struct to release

 void ib_cmem_release_contiguous_pages(struct ib_cmem *cmem)
 {
 }
 EXPORT_SYMBOL(ib_cmem_release_contiguous_pages);

 *
 *  * ib_cmem_alloc_contiguous_pages - allocate contiguous pages
 *  *  @context: userspace context to allocate memory for
 *   * @total_size: total required size for that allocation.
 *    * @page_size_order: order of one contiguous page.
 *
 struct ib_cmem *ib_cmem_alloc_contiguous_pages(struct ib_ucontext *context,
 unsigned long total_size,
 unsigned long page_size_order)
 {
 return NULL;
 }
 EXPORT_SYMBOL(ib_cmem_alloc_contiguous_pages);

 *
 *  * ib_cmem_map_contiguous_pages_to_vma - map contiguous pages into VMA
 *   * @ib_cmem: cmem structure returned by ib_cmem_alloc_contiguous_pages
 *    * @vma: VMA to inject pages into.
 *
 int ib_cmem_map_contiguous_pages_to_vma(struct ib_cmem *ib_cmem,
 struct vm_area_struct *vma)
 {
 return 0;
 }
 EXPORT_SYMBOL(ib_cmem_map_contiguous_pages_to_vma);
 */
