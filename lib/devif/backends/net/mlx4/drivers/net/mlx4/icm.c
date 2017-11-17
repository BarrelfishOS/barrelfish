/*

 * Copyright (c) 2005, 2006, 2007, 2008, 2014 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2006, 2007 Cisco Systems, Inc.  All rights reserved.
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


 #include <linux/errno.h>
 #include <linux/scatterlist.h>
 #include <linux/slab.h>
 */
#include <linux/math64.h>
#include <linux/types.h>
#include <linux/gfp.h>
#include <linux/mm.h>
#include <linux/mlx4/cmd.h>

#include <debug.h>
#include "fw.h"
/*

 * We allocate in as big chunks as we can, up to a maximum of 256 KB
 * per chunk.
 */
enum {
	MLX4_ICM_ALLOC_SIZE = 1 << 18, MLX4_TABLE_CHUNK_SIZE = 1 << 18
};
/*
 static void mlx4_free_icm_pages(struct mlx4_dev *dev, struct mlx4_icm_chunk *chunk)
 {
 int i;

 if (chunk->nsg > 0)
 pci_unmap_sg(dev->pdev, chunk->mem, chunk->npages,
 PCI_DMA_BIDIRECTIONAL);

 for (i = 0; i < chunk->npages; ++i)
 __free_pages(sg_page(&chunk->mem[i]),
 get_order(chunk->mem[i].length));
 }

 static void mlx4_free_icm_coherent(struct mlx4_dev *dev, struct mlx4_icm_chunk *chunk)
 {
 int i;

 for (i = 0; i < chunk->npages; ++i)
 dma_free_coherent(&dev->pdev->dev, chunk->mem[i].length,
 lowmem_page_address(sg_page(&chunk->mem[i])),
 sg_dma_address(&chunk->mem[i]));
 }

 void mlx4_free_icm(struct mlx4_dev *dev, struct mlx4_icm *icm, int coherent)
 {
 struct mlx4_icm_chunk *chunk, *tmp;

 if (!icm)
 return;

 list_for_each_entry_safe(chunk, tmp, &icm->chunk_list, list) {
 if (coherent)
 mlx4_free_icm_coherent(dev, chunk);
 else
 mlx4_free_icm_pages(dev, chunk);

 kfree(chunk);
 }

 kfree(icm);
 }
 */
static int mlx4_alloc_icm_pages(struct scatterlist *mem, int order) {
	struct page *page;

	page = alloc_pages(order);
	if (!page)
		return -ENOMEM;

	sg_set_page(mem, page, BASE_PAGE_SIZE << order, 0);

	/*a basic replacing solution for pci_map_sg()*/
	mem->address = page->phys_addr;

	/*printf("page->virt_addr: %p; page->phys_addr: %lx\n",page->virt_addr, page->phys_addr);*/

	return 0;
}
/*
 static int mlx4_alloc_icm_coherent(struct device *dev, struct scatterlist *mem,
 int order, gfp_t gfp_mask)
 {
 void *buf = dma_alloc_coherent(dev, PAGE_SIZE << order,
 &sg_dma_address(mem), gfp_mask);
 if (!buf)
 return -ENOMEM;

 sg_set_buf(mem, buf, PAGE_SIZE << order);
 BUG_ON(mem->offset);
 sg_dma_len(mem) = PAGE_SIZE << order;
 return 0;
 }
 */
struct mlx4_icm *mlx4_alloc_icm(struct mlx4_priv *priv, int npages,
		int coherent) {
	struct mlx4_icm *icm;
	/*
	 * A chunk is formed of 7 (MLX4_ICM_CHUNK_LEN) scaterrlists
	 * Each scatterlist is associated with a memory segment of 2^18 (PAGE_SIZE << cur_order) bytes*/
	struct mlx4_icm_chunk *chunk = NULL;
	int cur_order;
	int ret;

	/*We use sg_set_buf for coherent allocs, which assumes low memory*/
	/*assert(coherent && (gfp_mask & __GFP_HIGHMEM));*/

	icm = malloc(sizeof *icm);
	if (!icm)
		return NULL;

	icm->refcount = 0;
	INIT_LIST_HEAD(&icm->chunk_list);

	/*the order is 6*/
	cur_order = get_order(MLX4_ICM_ALLOC_SIZE);
	while (npages > 0) {
		if (!chunk) {
			chunk = malloc(sizeof *chunk);
			if (!chunk)
				goto fail;
			sg_init_table(chunk->mem, MLX4_ICM_CHUNK_LEN);
			chunk->npages = 0;
			chunk->nsg = 0;
			list_add_tail(&chunk->list, &icm->chunk_list);
		}

		while (1 << cur_order > npages)
			--cur_order;

		/*allocates a memory segment of 2^order and fills the scatterlist structure*/
		ret = mlx4_alloc_icm_pages(&chunk->mem[chunk->npages], cur_order);

		/*printf("cur_order: %d\n", cur_order);*/

		if (ret) {
			if (--cur_order < 0)
				goto fail;
			else
				continue;
		}

		/*normally nsg was assigned by pci_map_sg()*/
		++chunk->nsg;
		++chunk->npages;

		/*MLX4_ICM_CHUNK_LEN=7*/
		if (chunk->npages == MLX4_ICM_CHUNK_LEN)
			chunk = NULL;

		npages -= 1 << cur_order;
	}

	if (!coherent && chunk) {
		/*chunk->nsg = pci_map_sg(dev->pdev, chunk->mem, chunk->npages,
		 PCI_DMA_BIDIRECTIONAL);*/

		if (chunk->nsg <= 0)
			goto fail;
	}

	return icm;

	fail:
	/*TODO mlx4_free_icm(dev, icm, coherent);*/
	return NULL;
}

static int mlx4_MAP_ICM(struct mlx4_priv *priv, struct mlx4_icm *icm, u64 virt) {
	return mlx4_map_cmd(priv, MLX4_CMD_MAP_ICM, icm, virt);
}
/*
 static int mlx4_UNMAP_ICM(struct mlx4_dev *dev, u64 virt, u32 page_count)
 {
 return mlx4_cmd(dev, virt, page_count, 0, MLX4_CMD_UNMAP_ICM,
 MLX4_CMD_TIME_CLASS_B, MLX4_CMD_NATIVE);
 }
 */
int mlx4_MAP_ICM_AUX(struct mlx4_priv *priv, struct mlx4_icm *icm) {
	return mlx4_map_cmd(priv, MLX4_CMD_MAP_ICM_AUX, icm, -1);
}
/*
 int mlx4_UNMAP_ICM_AUX(struct mlx4_dev *dev)
 {
 return mlx4_cmd(dev, 0, 0, 0, MLX4_CMD_UNMAP_ICM_AUX,
 MLX4_CMD_TIME_CLASS_B, MLX4_CMD_NATIVE);
 }
 */
int mlx4_table_get(struct mlx4_priv *priv, struct mlx4_icm_table *table,
		u32 obj) {
	u32 i = (obj & (table->num_obj - 1))
			/ (MLX4_TABLE_CHUNK_SIZE / table->obj_size);
	int ret = 0;

	/*mutex_lock(&table->mutex);*/

	if (table->icm[i]) {
		++table->icm[i]->refcount;
		goto out;
	}

	table->icm[i] = mlx4_alloc_icm(priv, MLX4_TABLE_CHUNK_SIZE >> PAGE_SHIFT,
			table->coherent);
	if (!table->icm[i]) {
		ret = -ENOMEM;
		goto out;
	}

	if (mlx4_MAP_ICM(priv, table->icm[i],
			table->virt + (u64) i * MLX4_TABLE_CHUNK_SIZE)) {
		/*TODO*/
		/*mlx4_free_icm(dev, table->icm[i], table->coherent);*/
		table->icm[i] = NULL;
		ret = -ENOMEM;
		goto out;
	}

	++table->icm[i]->refcount;

	out: /*mutex_unlock(&table->mutex);*/
	return ret;
}

/*void mlx4_table_put(struct mlx4_dev *dev, struct mlx4_icm_table *table, u32 obj) {
 u32 i;
 u64 offset;

 i = (obj & (table->num_obj - 1))
 / (MLX4_TABLE_CHUNK_SIZE / table->obj_size);

 mutex_lock(&table->mutex);

 if (--table->icm[i]->refcount == 0) {
 offset = (u64) i * MLX4_TABLE_CHUNK_SIZE;

 if (!mlx4_UNMAP_ICM(dev, table->virt + offset,
 MLX4_TABLE_CHUNK_SIZE / MLX4_ICM_PAGE_SIZE)) {
 mlx4_free_icm(dev, table->icm[i], table->coherent);
 table->icm[i] = NULL;
 } else {
 pr_warn("mlx4_core: mlx4_UNMAP_ICM failed.\n");
 }
 }

 mutex_unlock(&table->mutex);
 }
 */
void *mlx4_table_find(struct mlx4_icm_table *table, u32 obj,
		genpaddr_t *dma_handle) {
	int offset, dma_offset, i;
	u64 idx;
	struct mlx4_icm_chunk *chunk;
	struct mlx4_icm *icm;
	struct page *page = NULL;

	/*if (!table->lowmem)
	 return NULL;*/

	/*mutex_lock(&table->mutex);*/

	idx = (u64) (obj & (table->num_obj - 1)) * table->obj_size;
	icm = table->icm[idx / MLX4_TABLE_CHUNK_SIZE];
	dma_offset = offset = idx % MLX4_TABLE_CHUNK_SIZE;

	if (!icm)
		goto out;

	list_for_each_entry(chunk, &icm->chunk_list, list)
	{
		for (i = 0; i < chunk->npages; ++i) {
			if (dma_handle && dma_offset >= 0) {
				if (sg_dma_len(&chunk->mem[i]) > dma_offset)
					*dma_handle = sg_dma_address(&chunk->mem[i]) + dma_offset;
				dma_offset -= sg_dma_len(&chunk->mem[i]);
			}

			/*DMA mapping can merge pages but not split them,
			 *so if we found the page, dma_handle has already
			 *been assigned to.*/

			if (chunk->mem[i].length > offset) {
				page = sg_page(&chunk->mem[i]);
				goto out;
			}

			offset -= chunk->mem[i].length;
		}
	}

	out: /*mutex_unlock(&table->mutex);*/
	return page ? lowmem_page_address(page) + offset : NULL;
}

int mlx4_table_get_range(struct mlx4_priv *priv, struct mlx4_icm_table *table,
		u32 start, u32 end) {
	int inc = MLX4_TABLE_CHUNK_SIZE / table->obj_size;
	int err;
	u32 i;

	for (i = start; i <= end; i += inc) {
		err = mlx4_table_get(priv, table, i);
		if (err)
			goto fail;
	}

	return 0;

	fail: while (i > start) {
		i -= inc;
		/*TODO*/
		/*mlx4_table_put(dev, table, i);*/
	}

	return err;
}
/*
 void mlx4_table_put_range(struct mlx4_dev *dev, struct mlx4_icm_table *table,
 u32 start, u32 end)
 {
 u32 i;

 for (i = start; i <= end; i += MLX4_TABLE_CHUNK_SIZE / table->obj_size)
 mlx4_table_put(dev, table, i);
 }
 */
int mlx4_init_icm_table(struct mlx4_priv *priv, struct mlx4_icm_table *table,
		u64 virt, int obj_size, u64 nobj, int reserved, int use_lowmem,
		int use_coherent) {
	int obj_per_chunk;
	int num_icm;
	unsigned chunk_size;
	int i;
	u64 size;

	obj_per_chunk = MLX4_TABLE_CHUNK_SIZE / obj_size;
	num_icm = div_u64((nobj + obj_per_chunk - 1), obj_per_chunk);

	table->icm = calloc(num_icm, sizeof *table->icm);
	if (!table->icm)
		return -ENOMEM;
	table->virt = virt;
	table->num_icm = num_icm;
	table->num_obj = nobj;
	table->obj_size = obj_size;
	table->lowmem = use_lowmem;
	table->coherent = use_coherent;
	/*mutex_init(&table->mutex);*/

	size = (u64) nobj * obj_size;
	for (i = 0; i * MLX4_TABLE_CHUNK_SIZE < reserved * obj_size; ++i) {
		chunk_size = MLX4_TABLE_CHUNK_SIZE;
		if ((i + 1) * MLX4_TABLE_CHUNK_SIZE > size)
			chunk_size = PAGE_ALIGN(size - i * MLX4_TABLE_CHUNK_SIZE);

		table->icm[i] = mlx4_alloc_icm(priv, chunk_size >> PAGE_SHIFT,
				use_coherent);
		if (!table->icm[i])
			goto err;
		if (mlx4_MAP_ICM(priv, table->icm[i],
				virt + i * MLX4_TABLE_CHUNK_SIZE)) {
			/*TODO*/
			/*mlx4_free_icm(priv, table->icm[i], use_coherent);*/
			table->icm[i] = NULL;
			goto err;
		}

		/*
		 * Add a reference to this ICM chunk so that it never
		 * gets freed (since it contains reserved firmware objects).
		 */
		++table->icm[i]->refcount;
	}

	return 0;

	/*TODO*/
	err: /*for (i = 0; i < num_icm; ++i)
	 if (table->icm[i]) {
	 if (!mlx4_UNMAP_ICM(priv, virt + i * MLX4_TABLE_CHUNK_SIZE,
	 MLX4_TABLE_CHUNK_SIZE / MLX4_ICM_PAGE_SIZE)) {
	 mlx4_free_icm(priv, table->icm[i], use_coherent);
	 } else {
	 pr_warn("mlx4_core: mlx4_UNMAP_ICM failed.\n");
	 return -ENOMEM;
	 }
	 }*/
	free(table->icm);

	return -ENOMEM;
}
/*
 void mlx4_cleanup_icm_table(struct mlx4_dev *dev, struct mlx4_icm_table *table)
 {
 int i, err = 0;

 for (i = 0; i < table->num_icm; ++i)
 if (table->icm[i]) {
 err = mlx4_UNMAP_ICM(dev,
 table->virt + i * MLX4_TABLE_CHUNK_SIZE,
 MLX4_TABLE_CHUNK_SIZE / MLX4_ICM_PAGE_SIZE);
 if (!err) {
 mlx4_free_icm(dev, table->icm[i],
 table->coherent);
 } else {
 pr_warn("mlx4_core: mlx4_UNMAP_ICM failed.\n");
 break;
 }
 }

 if (!err)
 kfree(table->icm);
 }*/
