/*

 * Copyright (c) 2007, 2014 Mellanox Technologies. All rights reserved.
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
 *

 #include "opt_inet.h"
 #include <linux/mlx4/cq.h>
 #include <linux/slab.h>
 #include <linux/mlx4/qp.h>
 #include <linux/if_ether.h>
 #include <linux/if_vlan.h>
 #include <linux/vmalloc.h>
 #include <linux/mlx4/driver.h>
 #ifdef CONFIG_NET_RX_BUSY_POLL
 #include <net/busy_poll.h>
 #endif

 #include "mlx4_en.h"
 */
#include <asm/byteorder.h>

#include <linux/mlx4/cq.h>
#include <linux/log2.h>
#include <linux/gfp.h>

#include <debug.h>

#include "mlx4_en.h"

static void mlx4_en_init_rx_desc(struct mlx4_en_priv *priv,
		struct mlx4_en_rx_ring *ring, int index) {
	struct mlx4_en_rx_desc *rx_desc = (struct mlx4_en_rx_desc *) (ring->buf
			+ (ring->stride * index));
	int possible_frags;
	int i;

	/*Set size and memtype fields*/
	rx_desc->data[0].byte_count = cpu_to_be32(1500);
	rx_desc->data[0].lkey = cpu_to_be32(priv->mdev->mr.key);

	/** If the number of used fragments does not fill up the ring
	 * stride, remaining (unused) fragments must be padded with
	 * null address/size and a special memory key:*/

	possible_frags = (ring->stride - sizeof(struct mlx4_en_rx_desc)) / DS_SIZE;
	/*printf("possible_frags: %d\n", possible_frags);*/
	for (i = 1; i < possible_frags; i++) {
		rx_desc->data[i].byte_count = 0;
		rx_desc->data[i].lkey = cpu_to_be32(MLX4_EN_MEMTYPE_PAD);
		rx_desc->data[i].addr = 0;
	}
}

static int mlx4_en_alloc_buf(struct mlx4_en_rx_ring *ring, __be64 *pdma,
		struct mlx4_en_rx_mbuf *mb_list) {

	/*bus_dma_segment_t segs[1];
	 bus_dmamap_t map;*/
	void *mb;
	genpaddr_t t;
	/*int nsegs;
	 int err;*/

	/*try to allocate a new spare mbuf*/
	/*if (unlikely(ring->spare.mbuf == NULL)) {
	 mb = m_getjcl(M_NOWAIT, MT_DATA, M_PKTHDR, ring->rx_mb_size);
	 if (unlikely(mb == NULL))
	 return (-ENOMEM);
	 setup correct length
	 mb->m_pkthdr.len = mb->m_len = ring->rx_mb_size;

	 make sure IP header gets aligned
	 m_adj(mb, MLX4_NET_IP_ALIGN);

	 load spare mbuf into BUSDMA
	 err = -bus_dmamap_load_mbuf_sg(ring->dma_tag, ring->spare.dma_map, mb,
	 segs, &nsegs, BUS_DMA_NOWAIT);
	 if (unlikely(err != 0)) {
	 m_freem(mb);
	 return (err);
	 }

	 store spare info
	 ring->spare.mbuf = mb;
	 ring->spare.paddr_be = cpu_to_be64(segs[0].ds_addr);

	 bus_dmamap_sync(ring->dma_tag, ring->spare.dma_map,
	 BUS_DMASYNC_PREREAD);
	 }*/

	/*synchronize and unload the current mbuf, if any*/
	/*if (likely(mb_list->mbuf != NULL)) {
	 bus_dmamap_sync(ring->dma_tag, mb_list->dma_map, BUS_DMASYNC_POSTREAD);
	 bus_dmamap_unload(ring->dma_tag, mb_list->dma_map);
	 }
	 */
	mb = dma_alloc(16384, &t);/*m_getjcl(M_NOWAIT, MT_DATA, M_PKTHDR, ring->rx_mb_size);*/
	if (/*unlikely(*/mb == NULL/*)*/)
		goto use_spare;

	/*setup correct length*/
	// mb->m_pkthdr.len = mb->m_len = MAX_ETH_FRAME_SIZE;

	/*make sure IP header gets aligned*/
	/*m_adj(mb, MLX4_NET_IP_ALIGN);*/

	/*err = -bus_dmamap_load_mbuf_sg(ring->dma_tag, mb_list->dma_map, mb, segs,
	 &nsegs, BUS_DMA_NOWAIT);
	 if (unlikely(err != 0)) {
	 m_freem(mb);
	 goto use_spare;
	 }*/

	*pdma = cpu_to_be64(t);
	mb_list->buffer = mb;

	/*bus_dmamap_sync(ring->dma_tag, mb_list->dma_map, BUS_DMASYNC_PREREAD);*/
	return (0);

	use_spare:
	/*swap DMA maps
	 map = mb_list->dma_map;
	 mb_list->dma_map = ring->spare.dma_map;
	 ring->spare.dma_map = map;*/

	/*swap MBUFs*/
	/*mb_list->mbuf = ring->spare.mbuf;
	 ring->spare.mbuf = NULL;*/

	/*store physical address*/
	/**pdma = ring->spare.paddr_be;*/
	return (0);
}
/*
 static void
 mlx4_en_free_buf(struct mlx4_en_rx_ring *ring, struct mlx4_en_rx_mbuf *mb_list)
 {
 bus_dmamap_t map = mb_list->dma_map;
 bus_dmamap_sync(ring->dma_tag, map, BUS_DMASYNC_POSTREAD);
 bus_dmamap_unload(ring->dma_tag, map);
 m_freem(mb_list->mbuf);
 mb_list->mbuf = NULL;	 safety clearing
 }
 */
static int mlx4_en_prepare_rx_desc(struct mlx4_en_priv *priv,
		struct mlx4_en_rx_ring *ring, int index) {
	struct mlx4_en_rx_desc *rx_desc = (struct mlx4_en_rx_desc *) (ring->buf
			+ (index * ring->stride));
	struct mlx4_en_rx_mbuf *mb_list = ring->mbuf + index;

	mb_list->buffer = NULL;

	if (mlx4_en_alloc_buf(ring, &rx_desc->data[0].addr, mb_list)) {
		priv->port_stats.rx_alloc_failed++;
		return (-ENOMEM);
	}

	/*rx_desc->data[0].addr = 0;*/

	return (0);
}

static inline void mlx4_en_update_rx_prod_db(struct mlx4_en_rx_ring *ring) {
	*ring->wqres.db.db = cpu_to_be32(ring->prod & 0xffff);
}

static int mlx4_en_fill_rx_buffers(struct mlx4_en_priv *priv) {
	struct mlx4_en_rx_ring *ring;
	int ring_ind;
	int buf_ind;
	int new_size;
	int err;

	for (buf_ind = 0; buf_ind < priv->prof->rx_ring_size; buf_ind++) {
		for (ring_ind = 0; ring_ind < priv->rx_ring_num; ring_ind++) {
			ring = priv->rx_ring[ring_ind];

			err = mlx4_en_prepare_rx_desc(priv, ring, ring->actual_size);
			if (err) {
				if (ring->actual_size == 0) {
					MLX4_ERR("Failed to allocate "
							"enough rx buffers\n");
					return -ENOMEM;
				} else {
					new_size = rounddown_pow_of_two(ring->actual_size);
					MLX4_WARN("Only %d buffers allocated "
							"reducing ring size to %d\n", ring->actual_size,
							new_size);
					goto reduce_rings;
				}
			}
			ring->actual_size++;
			ring->prod++;
		}
	}
	return 0;

	reduce_rings: for (ring_ind = 0; ring_ind < priv->rx_ring_num; ring_ind++) {
		ring = priv->rx_ring[ring_ind];
		while (ring->actual_size > new_size) {
			ring->actual_size--;
			ring->prod--;
			/*mlx4_en_free_buf(ring, ring->mbuf + ring->actual_size);*/
		}
	}

	return 0;
}
/*
 static void mlx4_en_free_rx_buf(struct mlx4_en_priv *priv,
 struct mlx4_en_rx_ring *ring)
 {
 int index;

 MLX4_DEBUG( "Freeing Rx buf - cons:%d prod:%d\n",
 ring->cons, ring->prod);

 Unmap and free Rx buffers
 BUG_ON((u32) (ring->prod - ring->cons) > ring->actual_size);
 while (ring->cons != ring->prod) {
 index = ring->cons & ring->size_mask;
 MLX4_DEBUG( "Processing descriptor:%d\n", index);
 mlx4_en_free_buf(ring, ring->mbuf + index);
 ++ring->cons;
 }
 }
 */
void mlx4_en_calc_rx_buf(struct mlx4_en_priv *priv) {
	/*struct mlx4_en_priv *priv = netdev_priv(dev);*/
	int eff_mtu = priv->if_mtu + ETH_HLEN + VLAN_HLEN + ETH_FCS_LEN +
	MLX4_NET_IP_ALIGN;

	if (eff_mtu > MJUM16BYTES) {
		MLX4_ERR("MTU(%d) is too big\n", (int )priv->if_mtu);
		eff_mtu = MJUM16BYTES;
	} else if (eff_mtu > MJUM9BYTES) {
		eff_mtu = MJUM16BYTES;
	} else if (eff_mtu > MJUMPAGESIZE) {
		eff_mtu = MJUM9BYTES;
	} else if (eff_mtu > MCLBYTES) {
		eff_mtu = MJUMPAGESIZE;
	} else {
		eff_mtu = MCLBYTES;
	}

	priv->rx_mb_size = eff_mtu;

	MLX4_DEBUG("Effective RX MTU: %d bytes\n", eff_mtu);
}

int mlx4_en_create_rx_ring(struct mlx4_en_priv *priv,
		struct mlx4_en_rx_ring **pring, u32 size, int node) {
	struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_en_rx_ring *ring;
	int err;
	int tmp;
	/*uint32_t x;*/

	ring = calloc(1, sizeof(struct mlx4_en_rx_ring));
	if (!ring) {
		MLX4_ERR("Failed to allocate RX ring structure\n");
		return -ENOMEM;
	}

	/*Create DMA descriptor TAG*/
	/*if ((err = -bus_dma_tag_create(bus_get_dma_tag(mdev->pdev->dev.bsddev), 1,  any alignment
	 0,  no boundary
	 BUS_SPACE_MAXADDR,  lowaddr
	 BUS_SPACE_MAXADDR,  highaddr
	 NULL, NULL,  filter, filterarg
	 MJUM16BYTES,  maxsize
	 1,  nsegments
	 MJUM16BYTES,  maxsegsize
	 0,  flags
	 NULL, NULL,  lockfunc, lockfuncarg
	 &ring->dma_tag))) {
	 MLX4_ERR( "Failed to create DMA tag\n");
	 goto err_ring;
	 }*/

	ring->prod = 0;
	ring->cons = 0;
	ring->size = size;
	ring->size_mask = size - 1;
	ring->stride = roundup_pow_of_two(sizeof(struct mlx4_en_rx_desc) + DS_SIZE);
	ring->log_stride = ffs(ring->stride) - 1;
	ring->buf_size = ring->size * ring->stride + TXBB_SIZE;

	tmp = size * sizeof(struct mlx4_en_rx_mbuf);

	ring->mbuf = calloc(1, tmp);
	if (ring->mbuf == NULL) {
		err = -ENOMEM;
		goto err_dma_tag;
	}

	/*err = -bus_dmamap_create(ring->dma_tag, 0, &ring->spare.dma_map);
	 if (err != 0)
	 goto err_info;

	 for (x = 0; x != size; x++) {
	 err = -bus_dmamap_create(ring->dma_tag, 0, &ring->mbuf[x].dma_map);
	 if (err != 0) {
	 while (x--)
	 bus_dmamap_destroy(ring->dma_tag, ring->mbuf[x].dma_map);
	 goto err_info;
	 }
	 }*/
	MLX4_DEBUG("Allocated MBUF ring at addr:%p size:%d\n", ring->mbuf, tmp);

	err = mlx4_alloc_hwq_res(mdev->dev, &ring->wqres, ring->buf_size,
			2 * BASE_PAGE_SIZE);
	if (err)
		goto err_dma_map;

	err = mlx4_en_map_buffer(&ring->wqres.buf);
	if (err) {
		MLX4_ERR("Failed to map RX buffer\n");
		goto err_hwq;
	}
	ring->buf = ring->wqres.buf.direct.buf;
	*pring = ring;
	return 0;

	err_hwq: /*mlx4_free_hwq_res(mdev->dev, &ring->wqres, ring->buf_size);*/
	err_dma_map: /*for (x = 0; x != size; x++) {
	 bus_dmamap_destroy(ring->dma_tag, ring->mbuf[x].dma_map);
	 }
	 bus_dmamap_destroy(ring->dma_tag, ring->spare.dma_map);*/
	/*err_info:*/free(ring->mbuf);
	err_dma_tag: /*bus_dma_tag_destroy(ring->dma_tag);*/
	/*err_ring:*/free(ring);
	return (err);
}

int mlx4_en_activate_rx_rings(struct mlx4_en_priv *priv) {
	struct mlx4_en_rx_ring *ring;
	int i;
	int ring_ind;
	int err;
	int stride = roundup_pow_of_two(sizeof(struct mlx4_en_rx_desc) + DS_SIZE);

	for (ring_ind = 0; ring_ind < priv->rx_ring_num; ring_ind++) {
		ring = priv->rx_ring[ring_ind];

		ring->prod = 0;
		ring->cons = 0;
		ring->actual_size = 0;
		ring->cqn = priv->rx_cq[ring_ind]->mcq.cqn;
		ring->rx_mb_size = priv->rx_mb_size;

		ring->stride = stride;
		if (ring->stride <= TXBB_SIZE)
			ring->buf += TXBB_SIZE;

		ring->log_stride = ffs(ring->stride) - 1;
		ring->buf_size = ring->size * ring->stride;

		memset(ring->buf, 0, ring->buf_size);
		mlx4_en_update_rx_prod_db(ring);

		/*Initialize all descriptors*/
		for (i = 0; i < ring->size; i++)
			mlx4_en_init_rx_desc(priv, ring, i);

#ifdef INET
		Configure lro mngr
		if (priv->dev->if_capenable & IFCAP_LRO) {
			if (tcp_lro_init(&ring->lro))
			priv->dev->if_capenable &= ~IFCAP_LRO;
			else
			ring->lro.ifp = priv->dev;
		}
#endif
	}

	err = mlx4_en_fill_rx_buffers(priv);
	if (err)
		goto err_buffers;

	for (ring_ind = 0; ring_ind < priv->rx_ring_num; ring_ind++) {
		ring = priv->rx_ring[ring_ind];

		ring->size_mask = ring->actual_size - 1;
		mlx4_en_update_rx_prod_db(ring);
	}

	return 0;

	err_buffers: /*for (ring_ind = 0; ring_ind < priv->rx_ring_num; ring_ind++)
	 mlx4_en_free_rx_buf(priv, priv->rx_ring[ring_ind]);*/

	ring_ind = priv->rx_ring_num - 1;

	while (ring_ind >= 0) {
		ring = priv->rx_ring[ring_ind];
		if (ring->stride <= TXBB_SIZE)
			ring->buf -= TXBB_SIZE;
		ring_ind--;
	}

	return err;
}
/*

 void mlx4_en_destroy_rx_ring(struct mlx4_en_priv *priv,
 struct mlx4_en_rx_ring **pring,
 u32 size, u16 stride)
 {
 struct mlx4_en_dev *mdev = priv->mdev;
 struct mlx4_en_rx_ring *ring = *pring;
 uint32_t x;

 mlx4_en_unmap_buffer(&ring->wqres.buf);
 mlx4_free_hwq_res(mdev->dev, &ring->wqres, size * stride + TXBB_SIZE);
 for (x = 0; x != size; x++)
 bus_dmamap_destroy(ring->dma_tag, ring->mbuf[x].dma_map);
 free spare mbuf, if any
 if (ring->spare.mbuf != NULL) {
 bus_dmamap_sync(ring->dma_tag, ring->spare.dma_map,
 BUS_DMASYNC_POSTREAD);
 bus_dmamap_unload(ring->dma_tag, ring->spare.dma_map);
 m_freem(ring->spare.mbuf);
 }
 bus_dmamap_destroy(ring->dma_tag, ring->spare.dma_map);
 vfree(ring->mbuf);
 bus_dma_tag_destroy(ring->dma_tag);
 kfree(ring);
 *pring = NULL;
 #ifdef CONFIG_RFS_ACCEL
 mlx4_en_cleanup_filters(priv, ring);
 #endif
 }

 void mlx4_en_deactivate_rx_ring(struct mlx4_en_priv *priv,
 struct mlx4_en_rx_ring *ring)
 {
 #ifdef INET
 tcp_lro_free(&ring->lro);
 #endif
 mlx4_en_free_rx_buf(priv, ring);
 if (ring->stride <= TXBB_SIZE)
 ring->buf -= TXBB_SIZE;
 }


 static void validate_loopback(struct mlx4_en_priv *priv, struct mbuf *mb)
 {
 int i;
 int offset = ETHER_HDR_LEN;

 for (i = 0; i < MLX4_LOOPBACK_TEST_PAYLOAD; i++, offset++) {
 if (*(mb->m_data + offset) != (unsigned char) (i & 0xff))
 goto out_loopback;
 }
 Loopback found
 priv->loopback_ok = 1;

 out_loopback:
 m_freem(mb);
 }

 */
static inline int invalid_cqe(struct mlx4_en_priv *priv, struct mlx4_cqe *cqe) {
	/*Drop packet on bad receive or bad checksum*/
	if (/*unlikely(*/
	(cqe->owner_sr_opcode & MLX4_CQE_OPCODE_MASK)
			== MLX4_CQE_OPCODE_ERROR/*)*/) {
		MLX4_ERR("CQE completed in error - vendor syndrom:%d syndrom:%d\n",
				((struct mlx4_err_cqe * )cqe)->vendor_err_syndrome,
				((struct mlx4_err_cqe * )cqe)->syndrome);
		return 1;
	}
	if (/*unlikely(*/cqe->badfcs_enc & MLX4_CQE_BAD_FCS/*)*/) {
		MLX4_DEBUG("Accepted frame with bad FCS\n");
		return 1;
	}

	return 0;
}

static void *
mlx4_en_rx_mb(struct mlx4_en_priv *priv, struct mlx4_en_rx_ring *ring,
		struct mlx4_en_rx_desc *rx_desc, struct mlx4_en_rx_mbuf *mb_list,
		int length) {
	void *mb;

	/*get mbuf*/
	mb = mb_list->buffer;

	/*collect used fragment while atomically replacing it*/
	// if (mlx4_en_alloc_buf(ring, &rx_desc->data[0].addr, mb_list))
	// 	return (NULL);
    //
	// /*range check hardware computed value*/
	// if (/*unlikely(*/length > mb->m_len/*)*/)
	// 	length = mb->m_len;
    //
	// /*update total packet length in packet header*/
	// mb->m_len = mb->m_pkthdr.len = length;
	return (mb);
}
/*
 For cpu arch with cache line of 64B the performance is better when cqe size==64B
 * To enlarge cqe size from 32B to 64B --> 32B of garbage (i.e. 0xccccccc)
 * was added in the beginning of each cqe (the real data is in the corresponding 32B).
 * The following calc ensures that when factor==1, it means we are alligned to 64B
 * and we get the real cqe data
 */
#define CQE_FACTOR_INDEX(index, factor) ((index << factor) + factor)
int mlx4_en_process_rx_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq *cq,
		int budget) {
	/*struct mlx4_en_priv *priv = netdev_priv(dev);*/
	struct mlx4_cqe *cqe;
	struct mlx4_en_rx_ring *ring = priv->rx_ring[cq->ring];
	struct mlx4_en_rx_mbuf *mb_list;
	struct mlx4_en_rx_desc *rx_desc;
	void *mb;
	struct mlx4_cq *mcq = &cq->mcq;
	struct mlx4_cqe *buf = cq->buf;
#ifdef INET
	struct lro_entry *queued;
#endif
	int index;
	unsigned int length;
	int polled = 0;
	u32 cons_index = mcq->cons_index;
	u32 size_mask = ring->size_mask;
	int size = cq->size;
	int factor = priv->cqe_factor;

	uint16_t *data;
	// int i;
	/*data = (uint32_t *) ring->buf;
	 for (i = 0; i < ring->buf_size / 4; ++i) {
	 printf("0x%x\n", data[i]);
	 }
	 printf("\n");*/

	if (!priv->port_up)
		return 0;

	/*We assume a 1:1 mapping between CQEs and Rx descriptors, so Rx
	 * descriptor offset can be deducted from the CQE index instead of
	 * reading 'cqe->index'*/
	index = cons_index & size_mask;
	cqe = &buf[CQE_FACTOR_INDEX(index, factor)];

	printf("cqe->owner_sr_opcode %d; cons_index %d; size %d; cq->buf %p; index %d\n",
			cqe->owner_sr_opcode, cons_index, size, cq->buf, index);

	/*Process all completed CQEs*/
	while (XNOR(cqe->owner_sr_opcode & MLX4_CQE_OWNER_MASK, cons_index & size)) {
		mb_list = ring->mbuf + index;
		rx_desc = (struct mlx4_en_rx_desc *) (ring->buf
				+ (index << ring->log_stride));

		/** make sure we read the CQE after we read the ownership bit*/

		rmb();

		if (invalid_cqe(priv, cqe)) {
			goto next;
		}
        uint8_t *cqe_data = (void *)cqe;
        char line[256];
        int i, s = 0;
        for (i = 0; i < 32; i++ ) {
            s += sprintf(line + s, "%02x", cqe_data[i]);
            if ((i & 3) == 3)
                line[s++] = ' ';
        }
        line[s] = 0;
        debug_printf("CQE: %s\n", line);
		/** Packet is OK - process it.*/

		length = be32_to_cpu(cqe->byte_cnt);
		length -= ring->fcs_del;

		mb = mlx4_en_rx_mb(priv, ring, rx_desc, mb_list, length);
		if (/*unlikely(*/!mb/*)*/) {
			ring->errors++;
			goto next;
		}

		ring->bytes += length;
		ring->packets++;

		data = (uint16_t *) mb;
        debug_printf("Got packet: %d: %04x:%04x %04x:%04x %04x:%04x %04x:%04x, %04x:%04x %04x:%04x %04x:%04x %04x:%04x, %04x:%04x %04x:%04x %04x:%04x %04x:%04x, %04x:%04x %04x:%04x %04x:%04x %04x:%04x\n", length,
            data[0x0], data[0x1], data[0x2], data[0x3], data[0x4], data[0x5], data[0x6], data[0x7],
            data[0x8], data[0x9], data[0xa], data[0xb], data[0xc], data[0xd], data[0xe], data[0xf],
            data[0x10], data[0x11], data[0x12], data[0x13], data[0x14], data[0x15], data[0x16], data[0x17],
            data[0x18], data[0x19], data[0x1a], data[0x1b], data[0x1c], data[0x1d], data[0x1e], data[0x1f]);
		/*if (unlikely(priv->validate_loopback)) {
		 validate_loopback(priv, mb);
		 goto next;
		 }*/

		/*forward Toeplitz compatible hash value*/
		// mb->m_pkthdr.flowid = be32_to_cpu(cqe->immed_rss_invalid);
		// M_HASHTYPE_SET(mb, M_HASHTYPE_OPAQUE);
		/*mb->m_pkthdr.rcvif = dev;*/
        // if (be32_to_cpu(cqe->vlan_my_qpn) & MLX4_CQE_VLAN_PRESENT_MASK) {
		// 	mb->m_pkthdr.ether_vtag= be16_to_cpu(cqe->sl_vid);
		// 	mb->m_flags |= M_VLANTAG;
		// }
		if (/*likely(dev->if_capenable
		 & (IFCAP_RXCSUM | IFCAP_RXCSUM_IPV6))
		 &&*/(cqe->status & cpu_to_be16(MLX4_CQE_STATUS_IPOK))
				&& (cqe->checksum == cpu_to_be16(0xffff))) {
			priv->port_stats.rx_chksum_good++;
			// mb->m_pkthdr.csum_flags = CSUM_IP_CHECKED | CSUM_IP_VALID
			// 		| CSUM_DATA_VALID | CSUM_PSEUDO_HDR;
			// mb->m_pkthdr.csum_data= bswap16(0xffff);
			/*This packet is eligible for LRO if it is:
			 * - DIX Ethernet (type interpretation)
			 * - TCP/IP (v4)
			 * - without IP options
			 * - not an IP fragment*/

// #ifdef INET
// 			if (mlx4_en_can_lro(cqe->status) &&
// 					(dev->if_capenable & IFCAP_LRO)) {
// 				if (ring->lro.lro_cnt != 0 &&
// 						tcp_lro_rx(&ring->lro, mb, 0) == 0)
// 				goto next;
// 			}
//
// #endif
			/*LRO not possible, complete processing here*/
			INC_PERF_COUNTER(priv->pstats.lro_misses);
		} else {
			// mb->m_pkthdr.csum_flags = 0;
			priv->port_stats.rx_chksum_none++;
		}

		/*Push it up the stack
		 dev->if_input(dev, mb);*/

		next: ++cons_index;
		index = cons_index & size_mask;
		cqe = &buf[CQE_FACTOR_INDEX(index, factor)];
		if (++polled == budget)
			goto out;
	}
	/*Flush all pending IP reassembly sessions*/
	out:
#ifdef INET
	while ((queued = SLIST_FIRST(&ring->lro.lro_active)) != NULL) {
		SLIST_REMOVE_HEAD(&ring->lro.lro_active, next);
		tcp_lro_flush(&ring->lro, queued);
	}
#endif
	AVG_PERF_COUNTER(priv->pstats.rx_coal_avg, polled);
	mcq->cons_index = cons_index;
	mlx4_cq_set_ci(mcq);
	wmb();
	/*ensure HW sees CQ consumer before we post new buffers*/
	ring->cons = mcq->cons_index;
	ring->prod += polled; /*Polled descriptors were realocated in place*/
	mlx4_en_update_rx_prod_db(ring);
	return polled;

}


void mlx4_en_rx_irq(struct mlx4_cq *mcq) {
    struct mlx4_en_cq *cq = container_of(mcq, struct mlx4_en_cq, mcq);
    struct mlx4_en_priv *priv = cq->dev;
    int done;

    // Shoot one within the irq context
    // Because there is no NAPI in freeBSD
    done = mlx4_en_process_rx_cq(priv, cq, MLX4_EN_RX_BUDGET);
    cq->tot_rx += done;
    // if (priv->port_up && (done == MLX4_EN_RX_BUDGET)) {
    //         cq->curr_poll_rx_cpu_id = curcpu;
    //         taskqueue_enqueue(cq->tq, &cq->cq_task);
    //     } else {
            mlx4_en_arm_cq(priv, cq);
    // }
}
/*
 void mlx4_en_rx_que(void *context, int pending) {
 struct mlx4_en_cq *cq;
 struct thread *td;

 cq = context;
 td = curthread;

 thread_lock(td);
 sched_bind(td, cq->curr_poll_rx_cpu_id);
 thread_unlock(td);

 while (mlx4_en_poll_rx_cq(cq, MLX4_EN_RX_BUDGET) == MLX4_EN_RX_BUDGET)
 ;
 mlx4_en_arm_cq(cq->dev->if_softc, cq);
 }


 RSS related functions
 */
static inline int mlx4_en_config_rss_qp(struct mlx4_en_priv *priv, int qpn,
		struct mlx4_en_rx_ring *ring, enum mlx4_qp_state *state,
		struct mlx4_qp *qp) {
	struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_qp_context *context;
	int err = 0;

	context = malloc(sizeof *context);
	if (!context) {
		MLX4_ERR("Failed to allocate qp context\n");
		return -ENOMEM;
	}

	err = mlx4_qp_alloc(mdev->dev, qpn, qp);
	if (err) {
		MLX4_ERR("Failed to allocate qp #%x\n", qpn);
		goto out;
	}
	qp->event = mlx4_en_sqp_event;

	memset(context, 0, sizeof *context);
	mlx4_en_fill_qp_context(priv, ring->actual_size, ring->stride, 0, 0, qpn,
			ring->cqn, -1, context);
	context->db_rec_addr = cpu_to_be64(ring->wqres.db.dma);

	/*Cancel FCS removal if FW allows*/
	if (mdev->dev->caps.flags & MLX4_DEV_CAP_FLAG_FCS_KEEP) {
		context->param3 |= cpu_to_be32(1 << 29);
		ring->fcs_del = ETH_FCS_LEN;
	} else
		ring->fcs_del = 0;

	/*ring->wqres.mtt.offset = 0xf0f0f;*/

	err = mlx4_qp_to_ready(mdev->dev, &ring->wqres.mtt, context, qp, state);
	if (err) {
		/*mlx4_qp_remove(mdev->dev, qp);
		 mlx4_qp_free(mdev->dev, qp);*/
	}

	mlx4_en_update_rx_prod_db(ring);

	out: free(context);
	return err;
}

int mlx4_en_create_drop_qp(struct mlx4_en_priv *priv) {
	int err;
	int qpn;

	err = mlx4_qp_reserve_range(priv->mdev->dev, 1, 1, &qpn, 0);
	if (err) {
		MLX4_ERR("Failed reserving drop qpn\n");
		return err;
	}
	err = mlx4_qp_alloc(priv->mdev->dev, qpn, &priv->drop_qp);
	if (err) {
		MLX4_ERR("Failed allocating drop qp\n");
		/*mlx4_qp_release_range(priv->mdev->dev, qpn, 1);*/
		return err;
	}

	return 0;
}
/*
 void mlx4_en_destroy_drop_qp(struct mlx4_en_priv *priv)
 {
 u32 qpn;

 qpn = priv->drop_qp.qpn;
 mlx4_qp_remove(priv->mdev->dev, &priv->drop_qp);
 mlx4_qp_free(priv->mdev->dev, &priv->drop_qp);
 mlx4_qp_release_range(priv->mdev->dev, qpn, 1);
 }

 Allocate rx qp's and configure them according to rss map
 */
int mlx4_en_config_rss_steer(struct mlx4_en_priv *priv) {
	struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_en_rss_map *rss_map = &priv->rss_map;
	struct mlx4_qp_context context;
	/*uint32_t *data = (uint32_t *) &context;*/
	struct mlx4_rss_context *rss_context;
	int rss_rings;
	void *ptr;
	u8 rss_mask = (MLX4_RSS_IPV4 | MLX4_RSS_TCP_IPV4 | MLX4_RSS_IPV6
			| MLX4_RSS_TCP_IPV6);
	int i;
	int err = 0;
	int good_qps = 0;
	static const u32 rsskey[10] = { 0xD181C62C, 0xF7F4DB5B, 0x1983A2FC,
			0x943E1ADB, 0xD9389E6B, 0xD1039C2C, 0xA74499AD, 0x593D56D9,
			0xF3253C06, 0x2ADC1FFC };

	MLX4_DEBUG("Configuring rss steering\n");
	err = mlx4_qp_reserve_range(mdev->dev, priv->rx_ring_num, priv->rx_ring_num,
			&rss_map->base_qpn, 0);
	if (err) {
		MLX4_ERR("Failed reserving %d qps\n", priv->rx_ring_num);
		return err;
	}

	for (i = 0; i < priv->rx_ring_num; i++) {
		priv->rx_ring[i]->qpn = rss_map->base_qpn + i;
		err = mlx4_en_config_rss_qp(priv, priv->rx_ring[i]->qpn,
				priv->rx_ring[i], &rss_map->state[i], &rss_map->qps[i]);
		if (err)
			goto rss_err;

		++good_qps;
	}

	/*Configure RSS indirection qp*/
	err = mlx4_qp_alloc(mdev->dev, priv->base_qpn, &rss_map->indir_qp);
	if (err) {
		MLX4_ERR("Failed to allocate RSS indirection QP\n");
		goto rss_err;
	}
	rss_map->indir_qp.event = mlx4_en_sqp_event;
	mlx4_en_fill_qp_context(priv, 0, 0, 0, 1, priv->base_qpn,
			priv->rx_ring[0]->cqn, -1, &context);

	if (!priv->prof->rss_rings || priv->prof->rss_rings > priv->rx_ring_num)
		rss_rings = priv->rx_ring_num;
	else
		rss_rings = priv->prof->rss_rings;

	ptr = ((u8 *) &context) + offsetof(struct mlx4_qp_context, pri_path)
			+ MLX4_RSS_OFFSET_IN_QPC_PRI_PATH;
	rss_context = ptr;
	rss_context->base_qpn = cpu_to_be32(
			ilog2(rss_rings) << 24 | (rss_map->base_qpn));
	rss_context->default_qpn = cpu_to_be32(rss_map->base_qpn);
	if (priv->mdev->profile.udp_rss) {
		rss_mask |= MLX4_RSS_UDP_IPV4 | MLX4_RSS_UDP_IPV6;
		rss_context->base_qpn_udp = rss_context->default_qpn;
	}
	rss_context->flags = rss_mask;
	rss_context->hash_fn = MLX4_RSS_HASH_TOP;
	for (i = 0; i < 10; i++)
		rss_context->rss_key[i] = cpu_to_be32(rsskey[i]);

	/*for (i = 0; i < sizeof context / 4; ++i) {
	 printf("context: 0x%x\n", data[i]);
	 }*/

	err = mlx4_qp_to_ready(mdev->dev, &priv->res.mtt, &context,
			&rss_map->indir_qp, &rss_map->indir_state);
	if (err)
		goto indir_err;

	return 0;

	indir_err: mlx4_qp_modify(mdev->dev, NULL, rss_map->indir_state,
			MLX4_QP_STATE_RST, NULL, 0, 0, &rss_map->indir_qp);
	/*mlx4_qp_remove(mdev->dev, &rss_map->indir_qp);
	 mlx4_qp_free(mdev->dev, &rss_map->indir_qp);*/
	rss_err: for (i = 0; i < good_qps; i++) {
		mlx4_qp_modify(mdev->dev, NULL, rss_map->state[i], MLX4_QP_STATE_RST,
		NULL, 0, 0, &rss_map->qps[i]);
		/*mlx4_qp_remove(mdev->dev, &rss_map->qps[i]);
		 mlx4_qp_free(mdev->dev, &rss_map->qps[i]);*/
	}
	/*mlx4_qp_release_range(mdev->dev, rss_map->base_qpn, priv->rx_ring_num);*/
	return err;
}
/*
 void mlx4_en_release_rss_steer(struct mlx4_en_priv *priv)
 {
 struct mlx4_en_dev *mdev = priv->mdev;
 struct mlx4_en_rss_map *rss_map = &priv->rss_map;
 int i;

 mlx4_qp_modify(mdev->dev, NULL, rss_map->indir_state,
 MLX4_QP_STATE_RST, NULL, 0, 0, &rss_map->indir_qp);
 mlx4_qp_remove(mdev->dev, &rss_map->indir_qp);
 mlx4_qp_free(mdev->dev, &rss_map->indir_qp);

 for (i = 0; i < priv->rx_ring_num; i++) {
 mlx4_qp_modify(mdev->dev, NULL, rss_map->state[i],
 MLX4_QP_STATE_RST, NULL, 0, 0, &rss_map->qps[i]);
 mlx4_qp_remove(mdev->dev, &rss_map->qps[i]);
 mlx4_qp_free(mdev->dev, &rss_map->qps[i]);
 }
 mlx4_qp_release_range(mdev->dev, rss_map->base_qpn, priv->rx_ring_num);
 }

 */
