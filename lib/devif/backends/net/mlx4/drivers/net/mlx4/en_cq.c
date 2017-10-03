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
#include <linux/mlx4/cmd.h>


#include <debug.h>

#include "mlx4_en.h"

static void mlx4_en_cq_event(struct mlx4_cq *cq, enum mlx4_event event) {
	return;
}

int mlx4_en_create_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq **pcq,
		int entries, int ring, enum cq_type mode, int node) {
	struct mlx4_en_dev *mdev = priv->mdev;
	struct mlx4_en_cq *cq;
	int err;

	/*cq = kzalloc_node(sizeof(struct mlx4_en_cq), GFP_KERNEL, node);
	 if (!cq) {*/
	cq = calloc(1, sizeof(struct mlx4_en_cq));
	if (!cq) {
		MLX4_ERR("Failed to allocate CW struture\n");
		return -ENOMEM;
	}
	/*}*/

	cq->size = entries;
	cq->buf_size = cq->size * mdev->dev->caps.cqe_size;

	/*printf("cq->buf_size: %d; mdev->dev->caps.cqe_size: %d\n", cq->buf_size, mdev->dev->caps.cqe_size);*/

	/*TODO: launch mlx4_en_rx_que or mlx4_en_tx_que*/
	/*cq->tq = taskqueue_create_fast("mlx4_en_que", M_NOWAIT,
	 taskqueue_thread_enqueue, &cq->tq);
	 if (mode == RX) {
	 TASK_INIT(&cq->cq_task, 0, mlx4_en_rx_que, cq);
	 taskqueue_start_threads(&cq->tq, 1, PI_NET, "%s rx cq",
	 if_name(priv->dev));

	 } else {
	 TASK_INIT(&cq->cq_task, 0, mlx4_en_tx_que, cq);
	 taskqueue_start_threads(&cq->tq, 1, PI_NET, "%s tx cq",
	 if_name(priv->dev));
	 }*/

	cq->ring = ring;
	cq->is_tx = mode;
	/*spin_lock_init(&cq->lock);*/

	err = mlx4_alloc_hwq_res(mdev->dev, &cq->wqres, cq->buf_size,
			2 * BASE_PAGE_SIZE);
	if (err)
		goto err_cq;

	err = mlx4_en_map_buffer(&cq->wqres.buf);
	if (err)
		goto err_res;

	cq->buf = (struct mlx4_cqe *) cq->wqres.buf.direct.buf;
	*pcq = cq;

	return 0;

	err_res: /*mlx4_free_hwq_res(mdev->dev, &cq->wqres, cq->buf_size);*/
	err_cq: free(cq);
	return err;
}

int mlx4_en_activate_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq *cq,
		int cq_idx) {
	struct mlx4_en_dev *mdev = priv->mdev;
	int err = 0;
	/*char name[25];*/
	int timestamp_en = 0;

	cq->dev = priv;//mdev->pndev[priv->port];
    debug_printf("%s.%d: %p\n", __func__, __LINE__, cq->dev);
	cq->mcq.set_ci_db = cq->wqres.db.db;
	cq->mcq.arm_db = cq->wqres.db.db + 1;
	*cq->mcq.set_ci_db = 0;
	*cq->mcq.arm_db = 0;
	memset(cq->buf, 0, cq->buf_size);

	if (cq->is_tx == RX) {
		/*if (mdev->dev->caps.comp_pool) {
		 if (!cq->vector) {
		 sprintf(name, "%s-%d", "mlxen", cq->ring);
		 Set IRQ for specific name (per ring)
		 if (mlx4_assign_eq(mdev->dev, name, &cq->vector)) {
		 cq->vector = (cq->ring + 1 + priv->port)
		 % mdev->dev->caps.num_comp_vectors;
		 mlx4_warn(mdev, "Failed Assigning an EQ to "
		 "%s ,Falling back to legacy EQ's\n", name);
		 }
		 }
		 } else {*/
		cq->vector = (cq->ring + 1 + priv->port)
				% mdev->dev->caps.num_comp_vectors;
		/*}*/
	} else {
		struct mlx4_en_cq *rx_cq;

		/** For TX we use the same irq per
		 * ring we assigned for the RX*/

		cq_idx = cq_idx % priv->rx_ring_num;
		rx_cq = priv->rx_cq[cq_idx];
		cq->vector = rx_cq->vector;
	}

	if (!cq->is_tx)
		cq->size = priv->rx_ring[cq->ring]->actual_size;
	err = mlx4_cq_alloc(mdev->dev, cq->size, &cq->wqres.mtt, &mdev->priv_uar,
			cq->wqres.db.dma, &cq->mcq, cq->vector, 0, timestamp_en);
	if (err)
		return err;

	/*TODO: implement it*/
	cq->mcq.comp = cq->is_tx ? mlx4_en_tx_irq :mlx4_en_rx_irq;
	cq->mcq.event = mlx4_en_cq_event;

	/*if (cq->is_tx) {
	 init_timer(&cq->timer);
	 cq->timer.function = mlx4_en_poll_tx_cq;
	 cq->timer.data = (unsigned long) cq;
	 }*/

	return 0;
}
/*
 void mlx4_en_destroy_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq **pcq)
 {
 struct mlx4_en_dev *mdev = priv->mdev;
 struct mlx4_en_cq *cq = *pcq;

 taskqueue_drain(cq->tq, &cq->cq_task);
 taskqueue_free(cq->tq);
 mlx4_en_unmap_buffer(&cq->wqres.buf);
 mlx4_free_hwq_res(mdev->dev, &cq->wqres, cq->buf_size);
 if (priv->mdev->dev->caps.comp_pool && cq->vector)
 mlx4_release_eq(priv->mdev->dev, cq->vector);
 kfree(cq);
 *pcq = NULL;
 }

 void mlx4_en_deactivate_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq *cq)
 {
 struct mlx4_en_dev *mdev = priv->mdev;

 taskqueue_drain(cq->tq, &cq->cq_task);
 if (cq->is_tx)
 del_timer(&cq->timer);

 mlx4_cq_free(mdev->dev, &cq->mcq);
 }


 Set rx cq moderation parameters
 int mlx4_en_set_cq_moder(struct mlx4_en_priv *priv, struct mlx4_en_cq *cq)
 {
 return mlx4_cq_modify(priv->mdev->dev, &cq->mcq,
 cq->moder_cnt, cq->moder_time);
 }
 */
int mlx4_en_arm_cq(struct mlx4_en_priv *priv, struct mlx4_en_cq *cq) {
	mlx4_cq_arm(&cq->mcq, MLX4_CQ_DB_REQ_NOT, priv->mdev->uar_map/*,
	 &priv->mdev->uar_lock*/);

	return 0;
}
