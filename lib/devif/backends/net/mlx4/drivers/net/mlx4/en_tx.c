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
 */
#include <linux/mlx4/cq.h>
#include <linux/mlx4/qp.h>

#include <linux/net/ethernet.h>

#include <linux/page.h>
#include <linux/if_vlan.h>
#include <linux/vmalloc.h>
#include <linux/moduleparam.h>
#include <linux/mm.h>
#include <linux/io.h>

#include <net/if_vlan_var.h>

#include <netinet/ip.h>
#include <netinet/ip6.h>
#include <netinet/tcp.h>


#include <netinet/in_systm.h>
/*#include <netinet/in.h>
 #include <netinet/if_ether.h>
 #include <netinet/ip.h>
 #include <netinet/ip6.h>
 #include <netinet/tcp.h>
 #include <netinet/tcp_lro.h>
 #include <netinet/udp.h>*/

#include <debug.h>

#include "mlx4_en.h"
#include "mlx4_devif_queue.h"
#include <net_interfaces/flags.h>
/*#include "utils.h"*/

enum {
	MAX_INLINE = 104, /* 128 - 16 - 4 - 4 */
	MAX_BF = 256, MIN_PKT_LEN = 17,
};

static int inline_thold /*__read_mostly*/= MAX_INLINE;

/*module_param_named(inline_thold, inline_thold, uint, 0444);
 MODULE_PARM_DESC(inline_thold, "threshold for using inline data");*/

int mlx4_en_create_tx_ring(struct mlx4_en_priv *priv,
		struct mlx4_en_tx_ring **pring, u32 size, u16 stride, int node,
		int queue_idx) {
	struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_en_tx_ring *ring;
	/*uint32_t x;*/
	int tmp;
	int err;

	/*ring = kzalloc_node(sizeof(struct mlx4_en_tx_ring), GFP_KERNEL, node);
	 if (!ring) {*/
	ring = calloc(1, sizeof(struct mlx4_en_tx_ring));
	if (!ring) {
		MLX4_ERR("Failed allocating TX ring\n");
		return -ENOMEM;
	}
	/*}*/

	/* Create DMA descriptor TAG */
	/*if ((err = -bus_dma_tag_create(
	 bus_get_dma_tag(mdev->pdev->dev.bsddev),
	 1,  any alignment
	 0,  no boundary
	 BUS_SPACE_MAXADDR,  lowaddr
	 BUS_SPACE_MAXADDR,  highaddr
	 NULL, NULL,  filter, filterarg
	 MLX4_EN_TX_MAX_PAYLOAD_SIZE,  maxsize
	 MLX4_EN_TX_MAX_MBUF_FRAGS,  nsegments
	 MLX4_EN_TX_MAX_MBUF_SIZE,  maxsegsize
	 0,  flags
	 NULL, NULL,  lockfunc, lockfuncarg
	 &ring->dma_tag)))
	 goto done;*/

	ring->size = size;
	ring->size_mask = size - 1;
	ring->stride = stride;
	ring->inline_thold = MAX(MIN_PKT_LEN, MIN(inline_thold, MAX_INLINE));
	/*mtx_init(&ring->tx_lock.m, "mlx4 tx", NULL, MTX_DEF);
	 mtx_init(&ring->comp_lock.m, "mlx4 comp", NULL, MTX_DEF);*/

	/* Allocate the buf ring */
	/*ring->br = buf_ring_alloc(MLX4_EN_DEF_TX_QUEUE_SIZE, M_DEVBUF, M_WAITOK,
	 &ring->tx_lock.m);
	 if (ring->br == NULL) {
	 MLX4_ERR("Failed allocating tx_info ring\n");
	 err = -ENOMEM;
	 goto err_free_dma_tag;
	 }*/

	tmp = size * sizeof(struct mlx4_en_tx_info);
	/*ring->tx_info = kzalloc_node(tmp, GFP_KERNEL, node);
	 if (!ring->tx_info) {*/
	ring->tx_info = calloc(1, tmp);
	if (!ring->tx_info) {
		err = -ENOMEM;
		goto err_ring;
	}
	/*}*/

	/* Create DMA descriptor MAPs */
	/*for (x = 0; x != size; x++) {
	 err = -bus_dmamap_create(ring->dma_tag, 0, &ring->tx_info[x].dma_map);
	 if (err != 0) {
	 while (x--) {
	 bus_dmamap_destroy(ring->dma_tag, ring->tx_info[x].dma_map);
	 }
	 goto err_info;
	 }
	 }*/

	MLX4_DEBUG("Allocated tx_info ring at addr:%p size:%d\n", ring->tx_info,
			tmp);

	ring->buf_size = ALIGN(size * ring->stride, MLX4_EN_PAGE_SIZE);

	/* Allocate HW buffers on provided NUMA node */
	err = mlx4_alloc_hwq_res(mdev->dev, &ring->wqres, ring->buf_size,
			2 * BASE_PAGE_SIZE);
	if (err) {
		MLX4_ERR("Failed allocating hwq resources\n");
		goto err_dma_map;
	}

	err = mlx4_en_map_buffer(&ring->wqres.buf);
	if (err) {
		MLX4_ERR("Failed to map TX buffer\n");
		goto err_hwq_res;
	}

	ring->buf = ring->wqres.buf.direct.buf;

	MLX4_DEBUG("Allocated TX ring (addr:%p) - buf:%p size:%d "
			"buf_size:%d dma:%llx\n", ring, ring->buf, ring->size,
			ring->buf_size, (unsigned long long ) ring->wqres.buf.direct.map);

	err = mlx4_qp_reserve_range(mdev->dev, 1, 1, &ring->qpn,
			MLX4_RESERVE_BF_QP);
	if (err) {
		MLX4_ERR("failed reserving qp for TX ring\n");
		goto err_map;
	}

	err = mlx4_qp_alloc(mdev->dev, ring->qpn, &ring->qp);
	if (err) {
		MLX4_ERR("Failed allocating qp %d\n", ring->qpn);
		goto err_reserve;
	}
	ring->qp.event = mlx4_en_sqp_event;

	err = mlx4_bf_alloc(mdev->dev, &ring->bf, node);
	if (err) {
		MLX4_DEBUG("working without blueflame (%d)", err);
		ring->bf.uar = &mdev->priv_uar;
		ring->bf.uar->map = mdev->uar_map;
		ring->bf_enabled = false;
	} else
		ring->bf_enabled = true;
	ring->queue_index = queue_idx;
	/*if (queue_idx < priv->num_tx_rings_p_up)
	 CPU_SET(queue_idx, &ring->affinity_mask);*/

	*pring = ring;
	return 0;

	err_reserve: /*mlx4_qp_release_range(mdev->dev, ring->qpn, 1);*/
	err_map: /*mlx4_en_unmap_buffer(&ring->wqres.buf);*/
	err_hwq_res: /*mlx4_free_hwq_res(mdev->dev, &ring->wqres, ring->buf_size);*/
	err_dma_map: /*for (x = 0; x != size; x++)
	 bus_dmamap_destroy(ring->dma_tag, ring->tx_info[x].dma_map);
	 err_info: vfree(ring->tx_info);*/
	err_ring: /*buf_ring_free(ring->br, M_DEVBUF);
	 err_free_dma_tag: bus_dma_tag_destroy(ring->dma_tag);
	 done: kfree(ring);*/
	return err;
}

/*
 void mlx4_en_destroy_tx_ring(struct mlx4_en_priv *priv,
 struct mlx4_en_tx_ring **pring) {
 struct mlx4_en_dev *mdev = priv->mdev;
 struct mlx4_en_tx_ring *ring = *pring;
 uint32_t x;
 MLX4_DEBUG( "Destroying tx ring, qpn: %d\n", ring->qpn);

 buf_ring_free(ring->br, M_DEVBUF);
 if (ring->bf_enabled)
 mlx4_bf_free(mdev->dev, &ring->bf);
 mlx4_qp_remove(mdev->dev, &ring->qp);
 mlx4_qp_free(mdev->dev, &ring->qp);
 mlx4_qp_release_range(priv->mdev->dev, ring->qpn, 1);
 mlx4_en_unmap_buffer(&ring->wqres.buf);
 mlx4_free_hwq_res(mdev->dev, &ring->wqres, ring->buf_size);
 for (x = 0; x != ring->size; x++)
 bus_dmamap_destroy(ring->dma_tag, ring->tx_info[x].dma_map);
 vfree(ring->tx_info);
 mtx_destroy(&ring->tx_lock.m);
 mtx_destroy(&ring->comp_lock.m);
 bus_dma_tag_destroy(ring->dma_tag);
 kfree(ring);
 *pring = NULL;
 }
 */
int mlx4_en_activate_tx_ring(struct mlx4_en_priv *priv,
		struct mlx4_en_tx_ring *ring, int cq, int user_prio) {
	struct mlx4_en_dev *mdev = priv->mdev;
	int err;

	ring->cqn = cq;
	ring->prod = 0;
	ring->cons = 0xffffffff;
	ring->last_nr_txbb = 1;
	ring->poll_cnt = 0;
	ring->blocked = 0;
	memset(ring->buf, 0, ring->buf_size);

	ring->qp_state = MLX4_QP_STATE_RST;
	ring->doorbell_qpn = ring->qp.qpn << 8;

	mlx4_en_fill_qp_context(priv, ring->size, ring->stride, 1, 0, ring->qpn,
			ring->cqn, user_prio, &ring->context);
	if (ring->bf_enabled)
		ring->context.usr_page = cpu_to_be32(ring->bf.uar->index);

	err = mlx4_qp_to_ready(mdev->dev, &ring->wqres.mtt, &ring->context,
			&ring->qp, &ring->qp_state);
	return err;
}
/*
 void mlx4_en_deactivate_tx_ring(struct mlx4_en_priv *priv,
 struct mlx4_en_tx_ring *ring) {
 struct mlx4_en_dev *mdev = priv->mdev;

 mlx4_qp_modify(mdev->dev, NULL, ring->qp_state, MLX4_QP_STATE_RST, NULL, 0,
 0, &ring->qp);
 }
 */

/*
 * Copy data from an mbuf chain starting "off" bytes from the beginning,
 * continuing for "len" bytes, into the indicated buffer.
 */
// #define mtod(m,t)	((t)(m)->m_data)
// static void m_copydata(const struct mbuf *m, int off, int len, caddr_t cp) {
// 	u_int count;
//
// 	assert(off >= 0);
// 	assert(len >= 0);
// 	while (off > 0) {
// 		assert(m != NULL);
// 		if (off < m->m_len)
// 			break;
// 		off -= m->m_len;
// 		m = m->m_next;
// 	}
// 	while (len > 0) {
// 		assert(m != NULL);
// 		count = min(m->m_len - off, len);
// 		bcopy(mtod(m, caddr_t) + off, cp, count);
// 		len -= count;
// 		cp += count;
// 		off = 0;
// 		m = m->m_next;
// 	}
// }
//
// static volatile struct mlx4_wqe_data_seg *
// mlx4_en_store_inline_lso_data(volatile struct mlx4_wqe_data_seg *dseg,
// 		struct mbuf *mb, int len, __be32 owner_bit) {
// 	uint8_t *inl = __DEVOLATILE(uint8_t *, dseg);
//
// 	/*copy data into place*/
// 	m_copydata(mb, 0, len, (caddr_t) inl + 4);
// 	dseg += DIV_ROUND_UP(4 + len, DS_SIZE_ALIGNMENT);
// 	return (dseg);
// }

// static void mlx4_en_store_inline_lso_header(
// 		volatile struct mlx4_wqe_data_seg *dseg, int len, __be32 owner_bit) {
// }

static void mlx4_en_stamp_wqe(struct mlx4_en_priv *priv,
		struct mlx4_en_tx_ring *ring, u32 index, u8 owner) {
	struct mlx4_en_tx_info *tx_info = &ring->tx_info[index];
	struct mlx4_en_tx_desc *tx_desc = (struct mlx4_en_tx_desc *) (ring->buf
			+ (index * TXBB_SIZE));
	volatile __be32 *ptr = (__be32 *) tx_desc;
	const __be32 stamp = cpu_to_be32(STAMP_VAL | ((u32) owner << STAMP_SHIFT));
	u32 i;

	/*Stamp the freed descriptor*/
	for (i = 0; i < tx_info->nr_txbb * TXBB_SIZE; i += STAMP_STRIDE) {
		*ptr = stamp;
		ptr += STAMP_DWORDS;
	}
}

static u32 mlx4_en_free_tx_desc(struct mlx4_en_priv *priv,
		struct mlx4_en_tx_ring *ring, u32 index) {
	struct mlx4_en_tx_info *tx_info;
	// struct mbuf *mb;

	tx_info = &ring->tx_info[index];
	// mb = tx_info->mb;

	// if (mb == NULL)
		// goto done;

	/*bus_dmamap_sync(ring->dma_tag, tx_info->dma_map, BUS_DMASYNC_POSTWRITE);
	 bus_dmamap_unload(ring->dma_tag, tx_info->dma_map);*/

	/*TODO proper cleanup*/
	/*m_freem(mb);*/
	// done:
    return (tx_info->nr_txbb);
}
/*
 int mlx4_en_free_tx_buf(struct net_device *dev, struct mlx4_en_tx_ring *ring) {
 struct mlx4_en_priv *priv = netdev_priv(dev);
 int cnt = 0;

 Skip last polled descriptor
 ring->cons += ring->last_nr_txbb;
 MLX4_DEBUG( "Freeing Tx buf - cons:0x%x prod:0x%x\n", ring->cons,
 ring->prod);

 if ((u32)(ring->prod - ring->cons) > ring->size) {
 en_warn(priv, "Tx consumer passed producer!\n");
 return 0;
 }

 while (ring->cons != ring->prod) {
 ring->last_nr_txbb = mlx4_en_free_tx_desc(priv, ring,
 ring->cons & ring->size_mask);
 ring->cons += ring->last_nr_txbb;
 cnt++;
 }

 if (cnt)
 MLX4_DEBUG( "Freed %d uncompleted tx descriptors\n", cnt);

 return cnt;
 }
 */
static bool mlx4_en_tx_ring_is_full(struct mlx4_en_tx_ring *ring) {
	int wqs;
	wqs = ring->size - (ring->prod - ring->cons);
	return (wqs < (HEADROOM + (2 * MLX4_EN_TX_WQE_MAX_WQEBBS)));
}

errval_t mlx4_en_dequeue_tx(mlx4_queue_t *queue, regionid_t* rid, genoffset_t* offset,
                            genoffset_t* length, genoffset_t* valid_data,
                            genoffset_t* valid_length, uint64_t* flags)
{
	struct mlx4_en_priv *priv = queue->priv;
    struct mlx4_en_cq *cq = priv->tx_cq[0];
	struct mlx4_cq *mcq = &cq->mcq;
	struct mlx4_en_tx_ring *ring = priv->tx_ring[cq->ring];
	struct mlx4_cqe *cqe;
	u16 index;
	u16 new_index, ring_index, stamp_index;
	u32 txbbs_skipped = 0;
	u32 txbbs_stamp = 0;
	u32 cons_index = mcq->cons_index;
	int size = cq->size;
	u32 size_mask = ring->size_mask;
	struct mlx4_cqe *buf = cq->buf;
	int factor = priv->cqe_factor;

	if (!priv->port_up)
		return 0;

	index = cons_index & size_mask;
	cqe = &buf[(index << factor) + factor];
	ring_index = ring->cons & size_mask;
	stamp_index = ring_index;

	/*Process all completed CQEs*/
	if (XNOR(cqe->owner_sr_opcode & MLX4_CQE_OWNER_MASK, cons_index & size)) {

		/* make sure we read the CQE after we read the
		 * ownership bit*/


		rmb();

		if (/*unlikely(*/
		(cqe->owner_sr_opcode & MLX4_CQE_OPCODE_MASK)
				== MLX4_CQE_OPCODE_ERROR/*)*/) {
			MLX4_ERR(
					"CQE completed in error - vendor syndrom: 0x%x syndrom: 0x%x\n",
					((struct mlx4_err_cqe * ) cqe)->vendor_err_syndrome,
					((struct mlx4_err_cqe * ) cqe)->syndrome);
		}

		/*Skip over last polled CQE*/
		new_index = be16_to_cpu(cqe->wqe_index) & size_mask;

		if (ring_index != new_index) {
			txbbs_skipped += ring->last_nr_txbb;
			ring_index = (ring_index + ring->last_nr_txbb) & size_mask;
			/*free next descriptor*/

	        struct mlx4_en_tx_info *tx_info;
	        tx_info = &ring->tx_info[ring_index];
			ring->last_nr_txbb = tx_info->nr_txbb;
            
			mlx4_en_stamp_wqe(priv, ring, stamp_index,
					!!((ring->cons + txbbs_stamp) & ring->size));
			stamp_index = ring_index;
			txbbs_stamp = txbbs_skipped;
            
            *rid = queue->region_id;
            *offset = tx_info->offset;
            *length = tx_info->length;
            *valid_data = 0;
            *valid_length = tx_info->nr_bytes;
            *flags = NETIF_TXFLAG | NETIF_TXFLAG_LAST;
		}

		++cons_index;
		index = cons_index & size_mask;
		cqe = &buf[(index << factor) + factor];
	} else {
        // debug_printf("%s: NONE\n", __func__);
        return DEVQ_ERR_QUEUE_EMPTY;
    }

	/* To prevent CQ overflow we first update CQ consumer and only then
	 * the ring consumer.*/

	mcq->cons_index = cons_index;
	mlx4_cq_set_ci(mcq);
	wmb();
	ring->cons += txbbs_skipped;

	/*Wakeup Tx queue if it was stopped and ring is not full*/
	if (/*unlikely(*/ring->blocked/*)*/&& !mlx4_en_tx_ring_is_full(ring)) {
		ring->blocked = 0;
		/*if (atomic_fetchadd_int(&priv->blocked, -1) == 1)
		 atomic_clear_int(&dev->if_drv_flags, IFF_DRV_OACTIVE);*/
		ring->wake_queue++;
		priv->port_stats.wake_queue++;
	}
    // debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", queue->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);
    return SYS_ERR_OK;
}

static int mlx4_en_process_tx_cq(struct mlx4_en_priv *priv,
		struct mlx4_en_cq *cq) {
	/*struct mlx4_en_priv *priv = netdev_priv(dev);*/
	struct mlx4_cq *mcq = &cq->mcq;
	struct mlx4_en_tx_ring *ring = priv->tx_ring[cq->ring];
	struct mlx4_cqe *cqe;
	u16 index;
	u16 new_index, ring_index, stamp_index;
	u32 txbbs_skipped = 0;
	u32 txbbs_stamp = 0;
	u32 cons_index = mcq->cons_index;
	int size = cq->size;
	u32 size_mask = ring->size_mask;
	struct mlx4_cqe *buf = cq->buf;
	int factor = priv->cqe_factor;

	if (!priv->port_up)
		return 0;

	index = cons_index & size_mask;
	cqe = &buf[(index << factor) + factor];
	ring_index = ring->cons & size_mask;
	stamp_index = ring_index;

	/*Process all completed CQEs*/
	while (XNOR(cqe->owner_sr_opcode & MLX4_CQE_OWNER_MASK, cons_index & size)) {

		/* make sure we read the CQE after we read the
		 * ownership bit*/


		rmb();

		if (/*unlikely(*/
		(cqe->owner_sr_opcode & MLX4_CQE_OPCODE_MASK)
				== MLX4_CQE_OPCODE_ERROR/*)*/) {
			MLX4_ERR(
					"CQE completed in error - vendor syndrom: 0x%x syndrom: 0x%x\n",
					((struct mlx4_err_cqe * ) cqe)->vendor_err_syndrome,
					((struct mlx4_err_cqe * ) cqe)->syndrome);
		}

		/*Skip over last polled CQE*/
		new_index = be16_to_cpu(cqe->wqe_index) & size_mask;

		do {
			txbbs_skipped += ring->last_nr_txbb;
			ring_index = (ring_index + ring->last_nr_txbb) & size_mask;
			/*free next descriptor*/
			ring->last_nr_txbb = mlx4_en_free_tx_desc(priv, ring, ring_index);
			mlx4_en_stamp_wqe(priv, ring, stamp_index,
					!!((ring->cons + txbbs_stamp) & ring->size));
			stamp_index = ring_index;
			txbbs_stamp = txbbs_skipped;
		} while (ring_index != new_index);

		++cons_index;
		index = cons_index & size_mask;
		cqe = &buf[(index << factor) + factor];
	}

	/* To prevent CQ overflow we first update CQ consumer and only then
	 * the ring consumer.*/

	mcq->cons_index = cons_index;
	mlx4_cq_set_ci(mcq);
	wmb();
	ring->cons += txbbs_skipped;

	printf("process TX CQE %d %d\n", ring->cons, index);

	/*Wakeup Tx queue if it was stopped and ring is not full*/
	if (/*unlikely(*/ring->blocked/*)*/&& !mlx4_en_tx_ring_is_full(ring)) {
		ring->blocked = 0;
		/*if (atomic_fetchadd_int(&priv->blocked, -1) == 1)
		 atomic_clear_int(&dev->if_drv_flags, IFF_DRV_OACTIVE);*/
		ring->wake_queue++;
		priv->port_stats.wake_queue++;
	}
	return (0);
}

void mlx4_en_tx_irq(struct mlx4_cq *mcq) {
    // struct mlx4_en_cq *cq = container_of(mcq, struct mlx4_en_cq, mcq);
    // struct mlx4_en_priv *priv = cq->dev;
    // struct mlx4_en_tx_ring *ring = priv->tx_ring[cq->ring];

    // debug_printf("%s.%d:\n", __func__, __LINE__);
    // if (priv->port_up == 0/* || !spin_trylock(&ring->comp_lock)*/)
    //     return;
    // mlx4_en_process_tx_cq(priv, cq);
    // mod_timer(&cq->timer, jiffies + 1);
    // spin_unlock(&ring->comp_lock);
}

// void mlx4_en_poll_tx_cq(unsigned long data) {
//     struct mlx4_en_cq *cq = (struct mlx4_en_cq *) data;
//     struct mlx4_en_priv *priv = netdev_priv(cq->dev);
//     struct mlx4_en_tx_ring *ring = priv->tx_ring[cq->ring];
//     u32 inflight;
//
//     INC_PERF_COUNTER(priv->pstats.tx_poll);
//
//     if (priv->port_up == 0)
//         return;
//     if (!spin_trylock(&ring->comp_lock)) {
//         mod_timer(&cq->timer, jiffies + MLX4_EN_TX_POLL_TIMEOUT);
//         return;
//     }
//     mlx4_en_process_tx_cq(cq->dev, cq);
//     inflight = (u32)(ring->prod - ring->cons - ring->last_nr_txbb);
//
//     /* If there are still packets in flight and the timer has not already
//      * been scheduled by the Tx routine then schedule it here to guarantee
//      * completion processing of these packets
//      */
//     if (inflight && priv->port_up)
//         mod_timer(&cq->timer, jiffies + MLX4_EN_TX_POLL_TIMEOUT);
//
//     spin_unlock(&ring->comp_lock);
// }
 
inline void mlx4_en_xmit_poll(struct mlx4_en_priv *priv, int tx_ind) {
	struct mlx4_en_cq *cq = priv->tx_cq[tx_ind];
	struct mlx4_en_tx_ring *ring = priv->tx_ring[tx_ind];

    
	if (priv->port_up == 0)
		return;

	/*If we don't have a pending timer, set one up to catch our recent
	 post in case the interface becomes idle*/
	/*if (!timer_pending(&cq->timer))
	 mod_timer(&cq->timer, jiffies + MLX4_EN_TX_POLL_TIMEOUT);*/

	/*Poll the CQ every mlx4_en_TX_MODER_POLL packets*/
	if ((++ring->poll_cnt & (MLX4_EN_TX_POLL_MODER - 1)) == 0)
		/*if (spin_trylock(&ring->comp_lock)) {*/
		mlx4_en_process_tx_cq(priv, cq);
	/*spin_unlock(&ring->comp_lock);*/
	/*}*/
}

// static u16 mlx4_en_get_inline_hdr_size(struct mlx4_en_tx_ring *ring,
// 		struct mbuf *mb) {
// 	u16 retval;
//
// 	/*only copy from first fragment, if possible*/
// 	retval = MIN(ring->inline_thold, mb->m_len);
//
// 	/*check for too little data*/
// 	if (/*unlikely(*/retval < MIN_PKT_LEN/*)*/)
// 		retval = MIN(ring->inline_thold, mb->m_pkthdr.len);
//
// 	return (retval);
// }

// static int mlx4_en_get_header_size(struct mbuf *mb) {
// 	struct ether_vlan_header *eh;
// 	struct tcphdr *th;
// 	struct ip *ip;
// 	int ip_hlen, tcp_hlen;
// 	struct ip6_hdr *ip6;
// 	uint16_t eth_type;
// 	int eth_hdr_len;
//
// 	eh = mtod(mb, struct ether_vlan_header *);
// 	if (mb->m_len < ETHER_HDR_LEN) {
// 		return (0);
// 	}
// 	if (eh->evl_encap_proto == htons(ETHERTYPE_VLAN)) {
// 		eth_type = ntohs(eh->evl_proto);
// 		eth_hdr_len = ETHER_HDR_LEN + ETHER_VLAN_ENCAP_LEN;
// 	} else {
// 		eth_type = ntohs(eh->evl_encap_proto);
// 		eth_hdr_len = ETHER_HDR_LEN;
// 	}
// 	if (mb->m_len < eth_hdr_len) {
// 		return (0);
// 	}
// 	switch (eth_type) {
// 	case ETHERTYPE_IP:
// 		ip = (struct ip *) (mb->m_data + eth_hdr_len);
// 		if (mb->m_len < eth_hdr_len + sizeof(*ip))
// 			return (0);
// 		if (ip->ip_p != IPPROTO_TCP)
// 			return (0);
// 		ip_hlen = ip->ip_hl << 2;
// 		eth_hdr_len += ip_hlen;
// 		break;
// 	case ETHERTYPE_IPV6:
// 		ip6 = (struct ip6_hdr *) (mb->m_data + eth_hdr_len);
// 		if (mb->m_len < eth_hdr_len + sizeof(*ip6))
// 			return (0);
// 		if (ip6->ip6_nxt != IPPROTO_TCP)
// 			return (0);
// 		eth_hdr_len += sizeof(*ip6);
// 		break;
// 	default:
// 		return (0);
// 	}
// 	if (mb->m_len < eth_hdr_len + sizeof(*th))
// 		return (0);
// 	th = (struct tcphdr *) (mb->m_data + eth_hdr_len);
// 	tcp_hlen = th->th_off << 2;
// 	eth_hdr_len += tcp_hlen;
// 	if (mb->m_len < eth_hdr_len)
// 		return (0);
// 	return (eth_hdr_len);
// }

// static volatile struct mlx4_wqe_data_seg *
// mlx4_en_store_inline_data(volatile struct mlx4_wqe_data_seg *dseg,
// 		struct mbuf *mb, int len, __be32 owner_bit) {
// 	uint8_t *inl = __DEVOLATILE(uint8_t *, dseg);
// 	const int spc = MLX4_INLINE_ALIGN - CTRL_SIZE - 4;
//
// 	if (/*unlikely(*/len < MIN_PKT_LEN/*)*/) {
// 		m_copydata(mb, 0, len, (caddr_t) inl + 4);
// 		memset(inl + 4 + len, 0, MIN_PKT_LEN - len);
// 		dseg += DIV_ROUND_UP(4 + MIN_PKT_LEN, DS_SIZE_ALIGNMENT);
// 	} else if (len <= spc) {
// 		m_copydata(mb, 0, len, (caddr_t) inl + 4);
// 		dseg += DIV_ROUND_UP(4 + len, DS_SIZE_ALIGNMENT);
// 	} else {
// 		m_copydata(mb, 0, spc, (caddr_t) inl + 4);
// 		m_copydata(mb, spc, len - spc, (caddr_t) inl + 8 + spc);
// 		dseg += DIV_ROUND_UP(8 + len, DS_SIZE_ALIGNMENT);
// 	}
// 	return (dseg);
// }

// static void mlx4_en_store_inline_header(volatile struct mlx4_wqe_data_seg *dseg,
// 		int len, __be32 owner_bit) {
// 	uint8_t *inl = __DEVOLATILE(uint8_t *, dseg);
// 	const int spc = MLX4_INLINE_ALIGN - CTRL_SIZE - 4;
//
// 	if (/*unlikely(*/len < MIN_PKT_LEN/*)*/) {
// 		*(volatile uint32_t *) inl = SET_BYTE_COUNT((1 << 31) | MIN_PKT_LEN);
// 	} else if (len <= spc) {
// 		*(volatile uint32_t *) inl = SET_BYTE_COUNT((1 << 31) | len);
// 	} else {
// 		*(volatile uint32_t *) (inl + 4 + spc) = SET_BYTE_COUNT(
// 				(1 << 31) | (len - spc));
// 		wmb();
// 		*(volatile uint32_t *) inl = SET_BYTE_COUNT((1 << 31) | spc);
// 	}
// }
/*
 static unsigned long hashrandom;
 static void hashrandom_init(void *arg) {
 hashrandom = random();
 }
 SYSINIT(hashrandom_init, SI_SUB_KLD, SI_ORDER_SECOND, &hashrandom_init, NULL);

 u16 mlx4_en_select_queue(struct net_device *dev, struct mbuf *mb) {
 struct mlx4_en_priv *priv = netdev_priv(dev);
 u32 rings_p_up = priv->num_tx_rings_p_up;
 u32 up = 0;
 u32 queue_index;

 #if (MLX4_EN_NUM_UP > 1)
 Obtain VLAN information if present
 if (mb->m_flags & M_VLANTAG) {
 u32 vlan_tag = mb->m_pkthdr.ether_vtag;
 up = (vlan_tag >> 13) % MLX4_EN_NUM_UP;
 }
 #endif
 queue_index = mlx4_en_hashmbuf(MLX4_F_HASHL3 | MLX4_F_HASHL4, mb, hashrandom);

 return ((queue_index % rings_p_up) + (up * rings_p_up));
 }
 */
// static void mlx4_bf_copy(void /*__iomem*/*dst, volatile unsigned long *src,
// 		unsigned bytecnt) {
// __iowrite64_copy(dst, __DEVOLATILE(void *, src), bytecnt / 8);
// }

// static u64 mlx4_en_mac_to_u64(u8 *addr) {
// u64 mac = 0;
// int i;
//
// for (i = 0; i < ETHER_ADDR_LEN; i++) {
// 	mac <<= 8;
// 	mac |= addr[i];
// }
// return mac;
// }

// static void m_adj(struct mbuf *mp, int req_len) {
// int len = req_len;
// struct mbuf *m;
// int count;
//
// if ((m = mp) == NULL)
// 	return;
// if (len >= 0) {
// 	/*
// 	 * Trim from head.
// 	 */
// 	while (m != NULL && len > 0) {
// 		if (m->m_len <= len) {
// 			len -= m->m_len;
// 			m->m_len = 0;
// 			m = m->m_next;
// 		} else {
// 			m->m_len -= len;
// 			m->m_data += len;
// 			len = 0;
// 		}
// 	}
// 	if (mp->m_flags & M_PKTHDR)
// 		mp->m_pkthdr.len -= (req_len - len);
// } else {
// 	/*
// 	 * Trim from tail.  Scan the mbuf chain,
// 	 * calculating its length and finding the last mbuf.
// 	 * If the adjustment only affects this mbuf, then just
// 	 * adjust and return.  Otherwise, rescan and truncate
// 	 * after the remaining size.
// 	 */
// 	len = -len;
// 	count = 0;
// 	for (;;) {
// 		count += m->m_len;
// 		if (m->m_next == (struct mbuf *) 0)
// 			break;
// 		m = m->m_next;
// 	}
// 	if (m->m_len >= len) {
// 		m->m_len -= len;
// 		if (mp->m_flags & M_PKTHDR)
// 			mp->m_pkthdr.len -= len;
// 		return;
// 	}
// 	count -= len;
// 	if (count < 0)
// 		count = 0;
// 	/*
// 	 * Correct length for chain is "count".
// 	 * Find the mbuf with last data, adjust its length,
// 	 * and toss data from remaining mbufs on chain.
// 	 */
// 	m = mp;
// 	if (m->m_flags & M_PKTHDR)
// 		m->m_pkthdr.len = count;
// 	for (; m; m = m->m_next) {
// 		if (m->m_len >= count) {
// 			m->m_len = count;
// 			if (m->m_next != NULL) {
//
// 				/*TODO: cleanup*/
// 				/*m_freem(m->m_next);*/
// 				m->m_next = NULL;
// 			}
// 			break;
// 		}
// 		count -= m->m_len;
// 	}
// }
// }

errval_t mlx4_en_enqueue_tx(mlx4_queue_t *queue, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
    // debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", queue->name, __func__, offset, length, valid_data, valid_length, flags);
    // uint8_t *packet = queue->region_mapped + offset + valid_data;
    // int i;
    // packet[6] = 0;
    // packet[7] = 0;
    // packet[8] = 0;
    // packet[9] = 0;
    // packet[10] = 0;
    // packet[11] = 0;
    // packet[0x28] = 0xe4;
    // packet[0x29] = 0x17;
    // packet[0x2e] = 0x42;
    // packet[0x2f] = 0x6a;
    // packet[0x30] = 0x61;
    // packet[0x31] = 0x6b;
    // packet[0x46] = 0;
    // packet[0x47] = 0;
    // packet[0x48] = 0;
    // packet[0x49] = 0;
    // packet[0x4a] = 0;
    // packet[0x4b] = 0;

    // for (i = 0; i < valid_length; i += 16) {
    //     debug_printf("%s: %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx  %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n", __func__, packet[0], packet[1], packet[2], packet[3], packet[4], packet[5], packet[6], packet[7],
    //         packet[8], packet[9], packet[10], packet[11], packet[12], packet[13], packet[14], packet[15]);
    //     packet += 16;
    // }
    struct mlx4_en_priv *priv = queue->priv;
    int tx_ind = 0;
    genpaddr_t buffer_data = queue->region_base + offset + valid_data;
    
    /*bus_dma_segment_t segs[MLX4_EN_TX_MAX_MBUF_FRAGS];*/
    // volatile struct mlx4_wqe_data_seg *dseg;
    // volatile struct mlx4_wqe_data_seg *dseg_inline;
    // volatile struct mlx4_en_tx_desc *tx_desc;
    // struct mlx4_en_tx_ring *ring = priv->tx_ring[tx_ind];
    // /*struct ifnet *ifp = priv->dev;*/
    // struct mlx4_en_tx_info *tx_info;
    // /*struct mbuf *m;*/
    // volatile __be32 *aux;
    // __be32 owner_bit;
    // int nr_segs;
    // int pad;
    // int err;
    // u32 bf_size;
    // u32 bf_prod;
    // u32 opcode;
    // u16 index;
    // u16 ds_cnt;
    // u16 ihs;
    // ihs = 0;
    
    
	// struct mlx4_en_priv *priv = netdev_priv(dev);
	// struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_en_tx_ring *ring;
	struct mlx4_en_cq *cq;
	struct mlx4_en_tx_desc *tx_desc;
	struct mlx4_wqe_data_seg *data;
	struct mlx4_en_tx_info *tx_info;
	int nr_txbb;
	int desc_size;
	int real_size;
	u32 index, bf_index, ring_size;
	__be32 op_own;
	bool inl = false;
	__be32 owner_bit;

    if (!priv->port_up) {
    	goto tx_drop;
    }

	ring = priv->tx_ring[tx_ind];
	ring_size = ring->size;

	owner_bit = (ring->prod & ring->size) ?
		cpu_to_be32(MLX4_EN_BIT_DESC_OWN) : 0;

    // debug_printf("MAC: %lx\n", priv->mac);
    // if (ring->inline_thold)
	//    inl = length <= ring->inline_thold;
    // debug_printf("%s.%d: inline=%d\n", __func__, __LINE__, inl);

	real_size = CTRL_SIZE + DS_SIZE; // for now only one data segment

	/* Align descriptor to TXBB size */
	desc_size = ALIGN(real_size, TXBB_SIZE);
	nr_txbb = desc_size / TXBB_SIZE;
    // debug_printf("%s.%d: nr_txbb=%d\n", __func__, __LINE__, nr_txbb);
    
	/* Packet is good - grab an index and transmit it */
	index = ring->prod & ring->size_mask;
	bf_index = ring->prod;
    // debug_printf("%s.%d: index=%d bf_index=%d\n", __func__, __LINE__, index, bf_index);

	/* See if we have enough space for whole descriptor TXBB for setting
	 * SW ownership on next descriptor; if not, use a bounce buffer. */
	tx_desc = ring->buf + index * TXBB_SIZE;

	/* Save mb in tx_info ring */
	tx_info = &ring->tx_info[index];
	// tx_info->mb = mb;
	tx_info->nr_txbb = nr_txbb;
	// tx_info->nr_segs = nr_segs;
    tx_info->offset = offset;
    tx_info->length = length;
    
    data = &tx_desc->data;
    
	/* valid only for none inline segments */
	// tx_info->data_offset = (void *)data - (void *)tx_desc;

    // tx_info->inl = inl;
	if (!inl) {
		data->addr = cpu_to_be64(buffer_data);
		data->lkey = cpu_to_be32(priv->mdev->mr.key);
		data->byte_count = SET_BYTE_COUNT(valid_length);
        tx_info->nr_bytes = valid_length;
    }
    
	tx_desc->ctrl.vlan_tag = cpu_to_be16(0);
	tx_desc->ctrl.ins_vlan = 0;
	tx_desc->ctrl.fence_size = (real_size / 16) & 0x3f;
	tx_desc->ctrl.srcrb_flags = cpu_to_be32(MLX4_WQE_CTRL_CQ_UPDATE | MLX4_WQE_CTRL_SOLICITED);

	op_own = cpu_to_be32(MLX4_OPCODE_SEND);
	// tx_info->nr_bytes = max(mb->m_pkthdr.len,
    //             (unsigned int)ETHER_MIN_LEN - ETHER_CRC_LEN);
	ring->packets++;

	// if (tx_info->inl) {
	// 	build_inline_wqe(tx_desc, mb, real_size, &vlan_tag, tx_ind,
	// 			 owner_bit);
	// 	tx_info->inl = 1;
	// }
	op_own |= owner_bit;
	ring->prod += nr_txbb;
    
    // debug_printf("%s.%d: index=%d bf_index=%d prod=%d\n", __func__, __LINE__, index, bf_index, ring->prod);

    cq = priv->tx_cq[tx_ind];
    mlx4_en_arm_cq(priv, cq);
    
    {
        /* Ensure new descirptor hits memory
        * before setting ownership of this descriptor to HW */
        wmb();
        tx_desc->ctrl.owner_opcode = op_own;

        // uint8_t *desc_data = (void *)tx_desc;
        // char line[256];
        // int i, s = 0;
        // for (i = 0; i < 64; i++ ) {
        //     s += sprintf(line + s, "%02x", desc_data[i]);
        //     if ((i & 3) == 3)
        //         line[s++] = ' ';
        // }
        // line[s] = 0;
        // debug_printf("DESC: %s\n", line);

        wmb();
        writel(cpu_to_be32(ring->doorbell_qpn), ring->bf.uar->map + MLX4_SEND_DOORBELL);
    }

	return 0;
tx_drop:
	// *mbp = NULL;
	// m_freem(mb);
	return EINVAL;
}

/*
 static int mlx4_en_transmit_locked(struct ifnet *dev, int tx_ind,
 struct mbuf *m) {
 struct mlx4_en_priv *priv = netdev_priv(dev);
 struct mlx4_en_tx_ring *ring;
 struct mbuf *next;
 int enqueued, err = 0;

 ring = priv->tx_ring[tx_ind];
 if ((dev->if_drv_flags & (IFF_DRV_RUNNING | IFF_DRV_OACTIVE)) != IFF_DRV_RUNNING
 || priv->port_up == 0) {
 if (m != NULL)
 err = drbr_enqueue(dev, ring->br, m);
 return (err);
 }

 enqueued = 0;
 if (m != NULL)

 * If we can't insert mbuf into drbr, try to xmit anyway.
 * We keep the error we got so we could return that after xmit.

 err = drbr_enqueue(dev, ring->br, m);

 Process the queue
 while ((next = drbr_peek(dev, ring->br)) != NULL) {
 if (mlx4_en_xmit(priv, tx_ind, &next) != 0) {
 if (next == NULL) {
 drbr_advance(dev, ring->br);
 } else {
 drbr_putback(dev, ring->br, next);
 }
 break;
 }
 drbr_advance(dev, ring->br);
 enqueued++;
 if ((dev->if_drv_flags & IFF_DRV_RUNNING) == 0)
 break;
 }

 if (enqueued > 0)
 ring->watchdog_time = ticks;

 return (err);
 }

 void mlx4_en_tx_que(void *context, int pending) {
 struct mlx4_en_tx_ring *ring;
 struct mlx4_en_priv *priv;
 struct net_device *dev;
 struct mlx4_en_cq *cq;
 int tx_ind;
 cq = context;
 dev = cq->dev;
 priv = dev->if_softc;
 tx_ind = cq->ring;
 ring = priv->tx_ring[tx_ind];

 if (priv->port_up != 0 && (dev->if_drv_flags & IFF_DRV_RUNNING) != 0) {
 mlx4_en_xmit_poll(priv, tx_ind);
 spin_lock(&ring->tx_lock);
 if (!drbr_empty(dev, ring->br))
 mlx4_en_transmit_locked(dev, tx_ind, NULL);
 spin_unlock(&ring->tx_lock);
 }
 }

 int mlx4_en_transmit(struct ifnet *dev, struct mbuf *m) {
 struct mlx4_en_priv *priv = netdev_priv(dev);
 struct mlx4_en_tx_ring *ring;
 struct mlx4_en_cq *cq;
 int i, err = 0;

 if (priv->port_up == 0) {
 m_freem(m);
 return (ENETDOWN);
 }

 Compute which queue to use
 if (M_HASHTYPE_GET(m) != M_HASHTYPE_NONE) {
 i = (m->m_pkthdr.flowid % 128) % priv->tx_ring_num;
 } else {
 i = mlx4_en_select_queue(dev, m);
 }

 ring = priv->tx_ring[i];
 if (spin_trylock(&ring->tx_lock)) {
 err = mlx4_en_transmit_locked(dev, i, m);
 spin_unlock(&ring->tx_lock);
 Poll CQ here
 mlx4_en_xmit_poll(priv, i);
 } else {
 err = drbr_enqueue(dev, ring->br, m);
 cq = priv->tx_cq[i];
 taskqueue_enqueue(cq->tq, &cq->cq_task);
 }

 return (err);
 }


 * Flush ring buffers.

 void mlx4_en_qflush(struct ifnet *dev) {
 struct mlx4_en_priv *priv = netdev_priv(dev);
 struct mlx4_en_tx_ring *ring;
 struct mbuf *m;

 if (priv->port_up == 0)
 return;

 for (int i = 0; i < priv->tx_ring_num; i++) {
 ring = priv->tx_ring[i];
 spin_lock(&ring->tx_lock);
 while ((m = buf_ring_dequeue_sc(ring->br)) != NULL)
 m_freem(m);
 spin_unlock(&ring->tx_lock);
 }
 if_qflush(dev);
 }
 */
