/*

 * Copyright (c) 2005, 2006, 2007, 2008, 2014 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2005, 2006, 2007 Cisco Systems, Inc. All rights reserved.
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
 *	- Redistributions of source code must retain the above
 *	  copyright notice, this list of conditions and the following
 *	  disclaimer.
 *
 *	- Redistributions in binary form must reproduce the above
 *	  copyright notice, this list of conditions and the following
 *	  disclaimer in the documentation and/or other materials
 *	  provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.


 #include <linux/interrupt.h>
 #include <linux/slab.h>
 #include <linux/module.h>
 #include <linux/mm.h>
 #include <linux/dma-mapping.h>

 #include <linux/mlx4/cmd.h>
 */
#include <linux/mm.h>
#include <linux/log2.h>
#include <linux/kernel.h>
#include <linux/err.h>
#include <linux/gfp.h>
#include <linux/io.h>

#include <linux/mlx4/cmd.h>

#include <asm/byteorder.h>

#include "icm.h"
#include <debug.h>
/*
 #include "fw.h"
 */
enum {
	MLX4_IRQNAME_SIZE = 32
};

enum {
	MLX4_NUM_ASYNC_EQE = 0x100,
	MLX4_NUM_SPARE_EQE = 0x80,
	MLX4_EQ_ENTRY_SIZE = 0x20
};

#define MLX4_EQ_STATUS_OK	   ( 0 << 28)
#define MLX4_EQ_STATUS_WRITE_FAIL  (10 << 28)
#define MLX4_EQ_OWNER_SW	   ( 0 << 24)
#define MLX4_EQ_OWNER_HW	   ( 1 << 24)
#define MLX4_EQ_FLAG_EC		   ( 1 << 18)
#define MLX4_EQ_FLAG_OI		   ( 1 << 17)
#define MLX4_EQ_STATE_ARMED	   ( 9 <<  8)
#define MLX4_EQ_STATE_FIRED	   (10 <<  8)
#define MLX4_EQ_STATE_ALWAYS_ARMED (11 <<  8)

#define MLX4_ASYNC_EVENT_MASK ((1ull << MLX4_EVENT_TYPE_PATH_MIG)	    | \
			       (1ull << MLX4_EVENT_TYPE_COMM_EST)	    | \
			       (1ull << MLX4_EVENT_TYPE_SQ_DRAINED)	    | \
			       (1ull << MLX4_EVENT_TYPE_CQ_ERROR)	    | \
			       (1ull << MLX4_EVENT_TYPE_WQ_CATAS_ERROR)	    | \
			       (1ull << MLX4_EVENT_TYPE_EEC_CATAS_ERROR)    | \
			       (1ull << MLX4_EVENT_TYPE_PATH_MIG_FAILED)    | \
			       (1ull << MLX4_EVENT_TYPE_WQ_INVAL_REQ_ERROR) | \
			       (1ull << MLX4_EVENT_TYPE_WQ_ACCESS_ERROR)    | \
			       (1ull << MLX4_EVENT_TYPE_PORT_CHANGE)	    | \
			       (1ull << MLX4_EVENT_TYPE_ECC_DETECT)	    | \
			       (1ull << MLX4_EVENT_TYPE_SRQ_CATAS_ERROR)    | \
			       (1ull << MLX4_EVENT_TYPE_SRQ_QP_LAST_WQE)    | \
			       (1ull << MLX4_EVENT_TYPE_SRQ_LIMIT)	    | \
			       (1ull << MLX4_EVENT_TYPE_CMD)		    | \
			       (1ull << MLX4_EVENT_TYPE_OP_REQUIRED)	    | \
			       (1ull << MLX4_EVENT_TYPE_COMM_CHANNEL)       | \
			       (1ull << MLX4_EVENT_TYPE_FLR_EVENT)	    | \
			       (1ull << MLX4_EVENT_TYPE_FATAL_WARNING))

static u64 get_async_ev_mask(struct mlx4_priv *priv) {
	u64 async_ev_mask = MLX4_ASYNC_EVENT_MASK;
	if (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_PORT_MNG_CHG_EV)
		async_ev_mask |= (1ull << MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT);
	if (priv->dev.caps.flags2 & MLX4_DEV_CAP_FLAG2_RECOVERABLE_ERROR_EVENT)
		async_ev_mask |= (1ull << MLX4_EVENT_TYPE_RECOVERABLE_ERROR_EVENT);

	return async_ev_mask;
}
static void eq_set_ci(struct mlx4_eq *eq, int req_not) {
	__raw_writel((__force u32) cpu_to_be32((eq->cons_index & 0xffffff) |
					req_not << 31), eq->doorbell);
	/*We still
	 want ordering, just
	 not swabbing, so
	 add a
	 barrier*/
	mb();
}
/*
 static struct mlx4_eqe *get_eqe(struct mlx4_eq *eq, u32 entry, u8 eqe_factor)
 {
 (entry & (eq->nent - 1)) gives us a cyclic array
 unsigned long offset = (entry & (eq->nent - 1)) * (MLX4_EQ_ENTRY_SIZE << eqe_factor);
 CX3 is capable of extending the EQE from 32 to 64 bytes.
 * When this feature is enabled, the first (in the lower addresses)
 * 32 bytes in the 64 byte EQE are reserved and the next 32 bytes
 * contain the legacy EQE information.

 return eq->page_list[offset / PAGE_SIZE].buf + (offset + (eqe_factor ? MLX4_EQ_ENTRY_SIZE : 0)) % PAGE_SIZE;
 }

 static struct mlx4_eqe *next_eqe_sw(struct mlx4_eq *eq, u8 eqe_factor)
 {
 struct mlx4_eqe *eqe = get_eqe(eq, eq->cons_index, eqe_factor);
 return !!(eqe->owner & 0x80) ^ !!(eq->cons_index & eq->nent) ? NULL : eqe;
 }

 static struct mlx4_eqe *next_slave_event_eqe(struct mlx4_slave_event_eq *slave_eq)
 {
 struct mlx4_eqe *eqe =
 &slave_eq->event_eqe[slave_eq->cons & (SLAVE_EVENT_EQ_SIZE - 1)];
 return (!!(eqe->owner & 0x80) ^
 !!(slave_eq->cons & SLAVE_EVENT_EQ_SIZE)) ?
 eqe : NULL;
 }

 void mlx4_gen_slave_eqe(struct work_struct *work)
 {
 struct mlx4_mfunc_master_ctx *master =
 container_of(work, struct mlx4_mfunc_master_ctx,
 slave_event_work);
 struct mlx4_mfunc *mfunc =
 container_of(master, struct mlx4_mfunc, master);
 struct mlx4_priv *priv = container_of(mfunc, struct mlx4_priv, mfunc);
 struct mlx4_dev *dev = &priv->dev;
 struct mlx4_slave_event_eq *slave_eq = &mfunc->master.slave_eq;
 struct mlx4_eqe *eqe;
 u8 slave;
 int i;

 for (eqe = next_slave_event_eqe(slave_eq); eqe;
 eqe = next_slave_event_eqe(slave_eq)) {
 slave = eqe->slave_id;

 All active slaves need to receive the event
 if (slave == ALL_SLAVES) {
 for (i = 0; i < priv->dev.num_slaves; i++) {
 if (mlx4_GEN_EQE(&priv->dev, i, eqe))
 MLX4_DEBUG( "Failed to generate "
 "event for slave %d\n", i);
 }
 } else {
 if (mlx4_GEN_EQE(&priv->dev, slave, eqe))
 MLX4_DEBUG( "Failed to generate event "
 "for slave %d\n", slave);
 }
 ++slave_eq->cons;
 }
 }


 static void slave_event(struct mlx4_priv *priv, u8 slave, struct mlx4_eqe *eqe)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_event_eq *slave_eq = &priv->mfunc.master.slave_eq;
 struct mlx4_eqe *s_eqe;
 unsigned long flags;

 spin_lock_irqsave(&slave_eq->event_lock, flags);
 s_eqe = &slave_eq->event_eqe[slave_eq->prod & (SLAVE_EVENT_EQ_SIZE - 1)];
 if ((!!(s_eqe->owner & 0x80)) ^
 (!!(slave_eq->prod & SLAVE_EVENT_EQ_SIZE))) {
 MLX4_DEBUG( "Master failed to generate an EQE for slave: %d. "
 "No free EQE on slave events queue\n", slave);
 spin_unlock_irqrestore(&slave_eq->event_lock, flags);
 return;
 }

 memcpy(s_eqe, eqe, priv->dev.caps.eqe_size - 1);
 s_eqe->slave_id = slave;
 ensure all information is written before setting the ownersip bit
 wmb();
 s_eqe->owner = !!(slave_eq->prod & SLAVE_EVENT_EQ_SIZE) ? 0x0 : 0x80;
 ++slave_eq->prod;

 queue_work(priv->mfunc.master.comm_wq,
 &priv->mfunc.master.slave_event_work);
 spin_unlock_irqrestore(&slave_eq->event_lock, flags);
 }

 static void mlx4_slave_event(struct mlx4_priv *priv, int slave,
 struct mlx4_eqe *eqe)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);

 if (slave < 0 || slave >= priv->dev.num_slaves ||
 slave == priv->dev.caps.function)
 return;

 if (!priv->mfunc.master.slave_state[slave].active)
 return;

 slave_event(&priv->dev, slave, eqe);
 }

 int mlx4_gen_pkey_eqe(struct mlx4_priv *priv, int slave, u8 port)
 {
 struct mlx4_eqe eqe;

 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_state *s_slave = &priv->mfunc.master.slave_state[slave];

 if (!s_slave->active)
 return 0;

 memset(&eqe, 0, sizeof eqe);

 eqe.type = MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT;
 eqe.subtype = MLX4_DEV_PMC_SUBTYPE_PKEY_TABLE;
 eqe.event.port_mgmt_change.port = port;

 return mlx4_GEN_EQE(&priv->dev, slave, &eqe);
 }
 EXPORT_SYMBOL(mlx4_gen_pkey_eqe);

 int mlx4_gen_guid_change_eqe(struct mlx4_priv *priv, int slave, u8 port)
 {
 struct mlx4_eqe eqe;

 don't send if we don't have the that slave
 if (priv->dev.num_vfs < slave)
 return 0;
 memset(&eqe, 0, sizeof eqe);

 eqe.type = MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT;
 eqe.subtype = MLX4_DEV_PMC_SUBTYPE_GUID_INFO;
 eqe.event.port_mgmt_change.port = port;

 return mlx4_GEN_EQE(&priv->dev, slave, &eqe);
 }
 EXPORT_SYMBOL(mlx4_gen_guid_change_eqe);

 int mlx4_gen_port_state_change_eqe(struct mlx4_priv *priv, int slave, u8 port,
 u8 port_subtype_change)
 {
 struct mlx4_eqe eqe;

 don't send if we don't have the that slave
 if (priv->dev.num_vfs < slave)
 return 0;
 memset(&eqe, 0, sizeof eqe);

 eqe.type = MLX4_EVENT_TYPE_PORT_CHANGE;
 eqe.subtype = port_subtype_change;
 eqe.event.port_change.port = cpu_to_be32(port << 28);

 MLX4_DEBUG( "%s: sending: %d to slave: %d on port: %d\n", __func__,
 port_subtype_change, slave, port);
 return mlx4_GEN_EQE(&priv->dev, slave, &eqe);
 }
 EXPORT_SYMBOL(mlx4_gen_port_state_change_eqe);

 enum slave_port_state mlx4_get_slave_port_state(struct mlx4_priv *priv, int slave, u8 port)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_state *s_state = priv->mfunc.master.slave_state;
 if (slave >= priv->dev.num_slaves || port > MLX4_MAX_PORTS) {
 pr_err("%s: Error: asking for slave:%d, port:%d\n",
 __func__, slave, port);
 return SLAVE_PORT_DOWN;
 }
 return s_state[slave].port_state[port];
 }
 EXPORT_SYMBOL(mlx4_get_slave_port_state);

 static int mlx4_set_slave_port_state(struct mlx4_priv *priv, int slave, u8 port,
 enum slave_port_state state)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_state *s_state = priv->mfunc.master.slave_state;

 if (slave >= priv->dev.num_slaves || port > MLX4_MAX_PORTS || port == 0) {
 pr_err("%s: Error: asking for slave:%d, port:%d\n",
 __func__, slave, port);
 return -1;
 }
 s_state[slave].port_state[port] = state;

 return 0;
 }

 static void set_all_slave_state(struct mlx4_priv *priv, u8 port, int event)
 {
 int i;
 enum slave_port_gen_event gen_event;

 for (i = 0; i < priv->dev.num_slaves; i++)
 set_and_calc_slave_port_state(&priv->dev, i, port, event, &gen_event);
 }
 *************************************************************************
 The function get as input the new event to that port,
 and according to the prev state change the slave's port state.
 The events are:
 MLX4_PORT_STATE_DEV_EVENT_PORT_DOWN,
 MLX4_PORT_STATE_DEV_EVENT_PORT_UP
 MLX4_PORT_STATE_IB_EVENT_GID_VALID
 MLX4_PORT_STATE_IB_EVENT_GID_INVALID
 **************************************************************************
 int set_and_calc_slave_port_state(struct mlx4_priv *priv, int slave,
 u8 port, int event,
 enum slave_port_gen_event *gen_event)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_state *ctx = NULL;
 unsigned long flags;
 int ret = -1;
 enum slave_port_state cur_state =
 mlx4_get_slave_port_state(&priv->dev, slave, port);

 *gen_event = SLAVE_PORT_GEN_EVENT_NONE;

 if (slave >= priv->dev.num_slaves || port > MLX4_MAX_PORTS || port == 0) {
 pr_err("%s: Error: asking for slave:%d, port:%d\n",
 __func__, slave, port);
 return ret;
 }

 ctx = &priv->mfunc.master.slave_state[slave];
 spin_lock_irqsave(&ctx->lock, flags);

 switch (cur_state) {
 case SLAVE_PORT_DOWN:
 if (MLX4_PORT_STATE_DEV_EVENT_PORT_UP == event)
 mlx4_set_slave_port_state(&priv->dev, slave, port,
 SLAVE_PENDING_UP);
 break;
 case SLAVE_PENDING_UP:
 if (MLX4_PORT_STATE_DEV_EVENT_PORT_DOWN == event)
 mlx4_set_slave_port_state(&priv->dev, slave, port,
 SLAVE_PORT_DOWN);
 else if (MLX4_PORT_STATE_IB_PORT_STATE_EVENT_GID_VALID == event) {
 mlx4_set_slave_port_state(&priv->dev, slave, port,
 SLAVE_PORT_UP);
 *gen_event = SLAVE_PORT_GEN_EVENT_UP;
 }
 break;
 case SLAVE_PORT_UP:
 if (MLX4_PORT_STATE_DEV_EVENT_PORT_DOWN == event) {
 mlx4_set_slave_port_state(&priv->dev, slave, port,
 SLAVE_PORT_DOWN);
 *gen_event = SLAVE_PORT_GEN_EVENT_DOWN;
 } else if (MLX4_PORT_STATE_IB_EVENT_GID_INVALID ==
 event) {
 mlx4_set_slave_port_state(&priv->dev, slave, port,
 SLAVE_PENDING_UP);
 *gen_event = SLAVE_PORT_GEN_EVENT_DOWN;
 }
 break;
 default:
 pr_err("%s: BUG!!! UNKNOWN state: "
 "slave:%d, port:%d\n", __func__, slave, port);
 goto out;
 }
 ret = mlx4_get_slave_port_state(&priv->dev, slave, port);

 out:
 spin_unlock_irqrestore(&ctx->lock, flags);
 return ret;
 }

 EXPORT_SYMBOL(set_and_calc_slave_port_state);

 int mlx4_gen_slaves_port_mgt_ev(struct mlx4_priv *priv, u8 port, int attr, u16 sm_lid, u8 sm_sl)
 {
 struct mlx4_eqe eqe;

 memset(&eqe, 0, sizeof eqe);

 eqe.type = MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT;
 eqe.subtype = MLX4_DEV_PMC_SUBTYPE_PORT_INFO;
 eqe.event.port_mgmt_change.port = port;
 eqe.event.port_mgmt_change.params.port_info.changed_attr =
 cpu_to_be32((u32) attr);
 if (attr & MSTR_SM_CHANGE_MASK) {
 eqe.event.port_mgmt_change.params.port_info.mstr_sm_lid =
 cpu_to_be16(sm_lid);
 eqe.event.port_mgmt_change.params.port_info.mstr_sm_sl =
 sm_sl;
 }

 slave_event(&priv->dev, ALL_SLAVES, &eqe);
 return 0;
 }
 EXPORT_SYMBOL(mlx4_gen_slaves_port_mgt_ev);

 void mlx4_master_handle_slave_flr(struct work_struct *work)
 {
 struct mlx4_mfunc_master_ctx *master =
 container_of(work, struct mlx4_mfunc_master_ctx,
 slave_flr_event_work);
 struct mlx4_mfunc *mfunc =
 container_of(master, struct mlx4_mfunc, master);
 struct mlx4_priv *priv =
 container_of(mfunc, struct mlx4_priv, mfunc);
 struct mlx4_dev *dev = &priv->dev;
 struct mlx4_slave_state *slave_state = priv->mfunc.master.slave_state;
 int i;
 int err;
 unsigned long flags;

 MLX4_DEBUG( "mlx4_handle_slave_flr\n");

 for (i = 0 ; i < priv->dev.num_slaves; i++) {

 if (MLX4_COMM_CMD_FLR == slave_state[i].last_cmd) {
 MLX4_DEBUG( "mlx4_handle_slave_flr: "
 "clean slave: %d\n", i);

 mlx4_delete_all_resources_for_slave(&priv->dev, i);
 return the slave to running mode
 spin_lock_irqsave(&priv->mfunc.master.slave_state_lock, flags);
 slave_state[i].last_cmd = MLX4_COMM_CMD_RESET;
 slave_state[i].is_slave_going_down = 0;
 spin_unlock_irqrestore(&priv->mfunc.master.slave_state_lock, flags);
 notify the FW:
 err = mlx4_cmd(&priv->dev, 0, i, 0, MLX4_CMD_INFORM_FLR_DONE,
 MLX4_CMD_TIME_CLASS_A, MLX4_CMD_WRAPPED);
 if (err)
 MLX4_DEBUG( "Failed to notify FW on "
 "FLR done (slave:%d)\n", i);
 }
 }
 }

 static int mlx4_eq_int(struct mlx4_priv *priv, struct mlx4_eq *eq)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_eqe *eqe;
 int cqn;
 int eqes_found = 0;
 int set_ci = 0;
 int port;
 int slave = 0;
 int ret;
 u32 flr_slave;
 u8 update_slave_state;
 int i;
 enum slave_port_gen_event gen_event;
 unsigned long flags;
 struct mlx4_vport_state *s_info;

 while ((eqe = next_eqe_sw(eq, priv->dev.caps.eqe_factor))) {

 * Make sure we read EQ entry contents after we've
 * checked the ownership bit.

 rmb();

 switch (eqe->type) {
 case MLX4_EVENT_TYPE_COMP:
 cqn = be32_to_cpu(eqe->event.comp.cqn) & 0xffffff;
 mlx4_cq_completion(&priv->dev, cqn);
 break;

 case MLX4_EVENT_TYPE_PATH_MIG:
 case MLX4_EVENT_TYPE_COMM_EST:
 case MLX4_EVENT_TYPE_SQ_DRAINED:
 case MLX4_EVENT_TYPE_SRQ_QP_LAST_WQE:
 case MLX4_EVENT_TYPE_WQ_CATAS_ERROR:
 case MLX4_EVENT_TYPE_PATH_MIG_FAILED:
 case MLX4_EVENT_TYPE_WQ_INVAL_REQ_ERROR:
 case MLX4_EVENT_TYPE_WQ_ACCESS_ERROR:
 MLX4_DEBUG( "event %d arrived\n", eqe->type);
 if (mlx4_is_master(&priv->dev)) {
 forward only to slave owning the QP
 ret = mlx4_get_slave_from_resource_id(&priv->dev,
 RES_QP,
 be32_to_cpu(eqe->event.qp.qpn)
 & 0xffffff, &slave);
 if (ret && ret != -ENOENT) {
 MLX4_DEBUG( "QP event %02x(%02x) on "
 "EQ %d at index %u: could "
 "not get slave id (%d)\n",
 eqe->type, eqe->subtype,
 eq->eqn, eq->cons_index, ret);
 break;
 }

 if (!ret && slave != priv->dev.caps.function) {
 mlx4_slave_event(&priv->dev, slave, eqe);
 break;
 }

 }
 mlx4_qp_event(&priv->dev, be32_to_cpu(eqe->event.qp.qpn) &
 0xffffff, eqe->type);
 break;

 case MLX4_EVENT_TYPE_SRQ_LIMIT:
 MLX4_DEBUG( "%s: MLX4_EVENT_TYPE_SRQ_LIMIT\n",
 __func__);
 fall through
 case MLX4_EVENT_TYPE_SRQ_CATAS_ERROR:
 if (mlx4_is_master(&priv->dev)) {
 forward only to slave owning the SRQ
 ret = mlx4_get_slave_from_resource_id(&priv->dev,
 RES_SRQ,
 be32_to_cpu(eqe->event.srq.srqn)
 & 0xffffff,
 &slave);
 if (ret && ret != -ENOENT) {
 MLX4_DEBUG( "SRQ event %02x(%02x) "
 "on EQ %d at index %u: could"
 " not get slave id (%d)\n",
 eqe->type, eqe->subtype,
 eq->eqn, eq->cons_index, ret);
 break;
 }
 MLX4_DEBUG( "%s: slave:%d, srq_no:0x%x, event: %02x(%02x)\n",
 __func__, slave,
 be32_to_cpu(eqe->event.srq.srqn),
 eqe->type, eqe->subtype);

 if (!ret && slave != priv->dev.caps.function) {
 MLX4_DEBUG( "%s: sending event %02x(%02x) to slave:%d\n",
 __func__, eqe->type,
 eqe->subtype, slave);
 mlx4_slave_event(&priv->dev, slave, eqe);
 break;
 }
 }
 mlx4_srq_event(&priv->dev, be32_to_cpu(eqe->event.srq.srqn) &
 0xffffff, eqe->type);
 break;

 case MLX4_EVENT_TYPE_CMD:
 mlx4_cmd_event(&priv->dev,
 be16_to_cpu(eqe->event.cmd.token),
 eqe->event.cmd.status,
 be64_to_cpu(eqe->event.cmd.out_param));
 break;

 case MLX4_EVENT_TYPE_PORT_CHANGE:
 port = be32_to_cpu(eqe->event.port_change.port) >> 28;
 if (eqe->subtype == MLX4_PORT_CHANGE_SUBTYPE_DOWN) {
 mlx4_dispatch_event(&priv->dev, MLX4_DEV_EVENT_PORT_DOWN,
 port);
 mlx4_priv(&priv->dev)->sense.do_sense_port[port] = 1;
 if (!mlx4_is_master(&priv->dev))
 break;
 for (i = 0; i < priv->dev.num_slaves; i++) {
 if (priv->dev.caps.port_type[port] == MLX4_PORT_TYPE_ETH) {
 if (i == mlx4_master_func_num(&priv->dev))
 continue;
 MLX4_DEBUG( "%s: Sending MLX4_PORT_CHANGE_SUBTYPE_DOWN"
 " to slave: %d, port:%d\n",
 __func__, i, port);
 s_info = &priv->mfunc.master.vf_oper[slave].vport[port].state;
 if (IFLA_VF_LINK_STATE_AUTO == s_info->link_state)
 mlx4_slave_event(&priv->dev, i, eqe);
 } else {   IB port
 set_and_calc_slave_port_state(&priv->dev, i, port,
 MLX4_PORT_STATE_DEV_EVENT_PORT_DOWN,
 &gen_event);
 we can be in pending state, then do not send port_down event
 if (SLAVE_PORT_GEN_EVENT_DOWN ==  gen_event) {
 if (i == mlx4_master_func_num(&priv->dev))
 continue;
 mlx4_slave_event(&priv->dev, i, eqe);
 }
 }
 }
 } else {
 mlx4_dispatch_event(&priv->dev, MLX4_DEV_EVENT_PORT_UP, port);

 mlx4_priv(&priv->dev)->sense.do_sense_port[port] = 0;

 if (!mlx4_is_master(&priv->dev))
 break;
 if (priv->dev.caps.port_type[port] == MLX4_PORT_TYPE_ETH)
 for (i = 0; i < priv->dev.num_slaves; i++) {
 if (i == mlx4_master_func_num(&priv->dev))
 continue;
 s_info = &priv->mfunc.master.vf_oper[slave].vport[port].state;
 if (IFLA_VF_LINK_STATE_AUTO == s_info->link_state)
 mlx4_slave_event(&priv->dev, i, eqe);
 }
 else  IB port
 port-up event will be sent to a slave when the
 * slave's alias-guid is set. This is done in alias_GUID.c

 set_all_slave_state(&priv->dev, port, MLX4_DEV_EVENT_PORT_UP);
 }
 break;

 case MLX4_EVENT_TYPE_CQ_ERROR:
 MLX4_DEBUG( "CQ %s on CQN %06x\n",
 eqe->event.cq_err.syndrome == 1 ?
 "overrun" : "access violation",
 be32_to_cpu(eqe->event.cq_err.cqn) & 0xffffff);
 if (mlx4_is_master(&priv->dev)) {
 ret = mlx4_get_slave_from_resource_id(&priv->dev,
 RES_CQ,
 be32_to_cpu(eqe->event.cq_err.cqn)
 & 0xffffff, &slave);
 if (ret && ret != -ENOENT) {
 MLX4_DEBUG( "CQ event %02x(%02x) on "
 "EQ %d at index %u: could "
 "not get slave id (%d)\n",
 eqe->type, eqe->subtype,
 eq->eqn, eq->cons_index, ret);
 break;
 }

 if (!ret && slave != priv->dev.caps.function) {
 mlx4_slave_event(&priv->dev, slave, eqe);
 break;
 }
 }
 mlx4_cq_event(&priv->dev,
 be32_to_cpu(eqe->event.cq_err.cqn)
 & 0xffffff,
 eqe->type);
 break;

 case MLX4_EVENT_TYPE_EQ_OVERFLOW:
 MLX4_DEBUG( "EQ overrun on EQN %d\n", eq->eqn);
 break;

 case MLX4_EVENT_TYPE_OP_REQUIRED:
 atomic_inc(&priv->opreq_count);
 FW commands can't be executed from interrupt context
 working in deferred task
 queue_work(mlx4_wq, &priv->opreq_task);
 break;

 case MLX4_EVENT_TYPE_COMM_CHANNEL:
 if (!mlx4_is_master(&priv->dev)) {
 MLX4_DEBUG( "Received comm channel event "
 "for non master device\n");
 break;
 }

 memcpy(&priv->mfunc.master.comm_arm_bit_vector,
 eqe->event.comm_channel_arm.bit_vec,
 sizeof eqe->event.comm_channel_arm.bit_vec);

 if (!queue_work(priv->mfunc.master.comm_wq,
 &priv->mfunc.master.comm_work))
 MLX4_DEBUG( "Failed to queue comm channel work\n");

 if (!queue_work(priv->mfunc.master.comm_wq,
 &priv->mfunc.master.arm_comm_work))
 MLX4_DEBUG( "Failed to queue arm comm channel work\n");
 break;

 case MLX4_EVENT_TYPE_FLR_EVENT:
 flr_slave = be32_to_cpu(eqe->event.flr_event.slave_id);
 if (!mlx4_is_master(&priv->dev)) {
 MLX4_DEBUG( "Non-master function received"
 "FLR event\n");
 break;
 }

 MLX4_DEBUG( "FLR event for slave: %d\n", flr_slave);

 if (flr_slave >= priv->dev.num_slaves) {
 MLX4_DEBUG(
 "Got FLR for unknown function: %d\n",
 flr_slave);
 update_slave_state = 0;
 } else
 update_slave_state = 1;

 spin_lock_irqsave(&priv->mfunc.master.slave_state_lock, flags);
 if (update_slave_state) {
 priv->mfunc.master.slave_state[flr_slave].active = false;
 priv->mfunc.master.slave_state[flr_slave].last_cmd = MLX4_COMM_CMD_FLR;
 priv->mfunc.master.slave_state[flr_slave].is_slave_going_down = 1;
 }
 spin_unlock_irqrestore(&priv->mfunc.master.slave_state_lock, flags);
 queue_work(priv->mfunc.master.comm_wq,
 &priv->mfunc.master.slave_flr_event_work);
 break;

 case MLX4_EVENT_TYPE_FATAL_WARNING:
 if (eqe->subtype == MLX4_FATAL_WARNING_SUBTYPE_WARMING) {
 if (mlx4_is_master(&priv->dev))
 for (i = 0; i < priv->dev.num_slaves; i++) {
 MLX4_DEBUG( "%s: Sending "
 "MLX4_FATAL_WARNING_SUBTYPE_WARMING"
 " to slave: %d\n", __func__, i);
 if (i == priv->dev.caps.function)
 continue;
 mlx4_slave_event(&priv->dev, i, eqe);
 }
 MLX4_DEBUG( "Temperature Threshold was reached! "
 "Threshold: %d celsius degrees; "
 "Current Temperature: %d\n",
 be16_to_cpu(eqe->event.warming.warning_threshold),
 be16_to_cpu(eqe->event.warming.current_temperature));
 } else
 MLX4_DEBUG( "Unhandled event FATAL WARNING (%02x), "
 "subtype %02x on EQ %d at index %u. owner=%x, "
 "nent=0x%x, slave=%x, ownership=%s\n",
 eqe->type, eqe->subtype, eq->eqn,
 eq->cons_index, eqe->owner, eq->nent,
 eqe->slave_id,
 !!(eqe->owner & 0x80) ^
 !!(eq->cons_index & eq->nent) ? "HW" : "SW");

 break;

 case MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT:
 mlx4_dispatch_event(&priv->dev, MLX4_DEV_EVENT_PORT_MGMT_CHANGE,
 (unsigned long) eqe);
 break;

 case MLX4_EVENT_TYPE_RECOVERABLE_ERROR_EVENT:
 switch (eqe->subtype) {
 case MLX4_RECOVERABLE_ERROR_EVENT_SUBTYPE_BAD_CABLE:
 MLX4_DEBUG( "Bad cable detected on port %u\n",
 eqe->event.bad_cable.port);
 break;
 case MLX4_RECOVERABLE_ERROR_EVENT_SUBTYPE_UNSUPPORTED_CABLE:
 MLX4_DEBUG( "Unsupported cable detected\n");
 break;
 default:
 MLX4_DEBUG( "Unhandled recoverable error event "
 "detected: %02x(%02x) on EQ %d at index %u. "
 "owner=%x, nent=0x%x, ownership=%s\n",
 eqe->type, eqe->subtype, eq->eqn,
 eq->cons_index, eqe->owner, eq->nent,
 !!(eqe->owner & 0x80) ^
 !!(eq->cons_index & eq->nent) ? "HW" : "SW");
 break;
 }
 break;

 case MLX4_EVENT_TYPE_EEC_CATAS_ERROR:
 case MLX4_EVENT_TYPE_ECC_DETECT:
 default:
 MLX4_DEBUG( "Unhandled event %02x(%02x) on EQ %d at "
 "index %u. owner=%x, nent=0x%x, slave=%x, "
 "ownership=%s\n",
 eqe->type, eqe->subtype, eq->eqn,
 eq->cons_index, eqe->owner, eq->nent,
 eqe->slave_id,
 !!(eqe->owner & 0x80) ^
 !!(eq->cons_index & eq->nent) ? "HW" : "SW");
 break;
 };

 ++eq->cons_index;
 eqes_found = 1;
 ++set_ci;


 * The HCA will think the queue has overflowed if we
 * don't tell it we've been processing events.  We
 * create our EQs with MLX4_NUM_SPARE_EQE extra
 * entries, so we must update our consumer index at
 * least that often.

 if (unlikely(set_ci >= MLX4_NUM_SPARE_EQE)) {
 eq_set_ci(eq, 0);
 set_ci = 0;
 }
 }

 eq_set_ci(eq, 1);

 return eqes_found;
 }

 static irqreturn_t mlx4_interrupt(int irq, void *dev_ptr)
 {
 struct mlx4_dev *dev = dev_ptr;
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int work = 0;
 int i;

 writel(priv->eq_table.clr_mask, priv->eq_table.clr_int);

 for (i = 0; i < priv->dev.caps.num_comp_vectors + 1; ++i)
 work |= mlx4_eq_int(&priv->dev, &priv->eq_table.eq[i]);

 return IRQ_RETVAL(work);
 }

 static irqreturn_t mlx4_msi_x_interrupt(int irq, void *eq_ptr)
 {
 struct mlx4_eq  *eq  = eq_ptr;
 struct mlx4_dev *dev = eq->dev;

 mlx4_eq_int(&priv->dev, eq);

 MSI-X vectors always belong to us
 return IRQ_HANDLED;
 }

 int mlx4_MAP_EQ_wrapper(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_slave_event_eq_info *event_eq =
 priv->mfunc.master.slave_state[slave].event_eq;
 u32 in_modifier = vhcr->in_modifier;
 u32 eqn = in_modifier & 0x3FF;
 u64 in_param =  vhcr->in_param;
 int err = 0;
 int i;

 if (slave == priv->dev.caps.function)
 err = mlx4_cmd(&priv->dev, in_param, (in_modifier & 0x80000000) | eqn,
 0, MLX4_CMD_MAP_EQ, MLX4_CMD_TIME_CLASS_B,
 MLX4_CMD_NATIVE);
 if (!err)
 for (i = 0; i < MLX4_EVENT_TYPES_NUM; ++i)
 if (in_param & (1LL << i))
 event_eq[i].eqn = in_modifier >> 31 ? -1 : eqn;

 return err;
 }
 */
static int mlx4_MAP_EQ(struct mlx4_priv *priv, u64 event_mask, int unmap,
		int eq_num) {
	return mlx4_cmd(&priv->dev, event_mask, (unmap << 31) | eq_num, 0,
			MLX4_CMD_MAP_EQ, MLX4_CMD_TIME_CLASS_B, MLX4_CMD_WRAPPED);
}

static int mlx4_SW2HW_EQ(struct mlx4_priv *priv,
		struct mlx4_cmd_mailbox *mailbox, int eq_num) {
	return mlx4_cmd(&priv->dev, mailbox->dma, eq_num, 0, MLX4_CMD_SW2HW_EQ,
			MLX4_CMD_TIME_CLASS_A, MLX4_CMD_WRAPPED);
}
/*
 static int mlx4_HW2SW_EQ(struct mlx4_priv *priv, struct mlx4_cmd_mailbox *mailbox,
 int eq_num)
 {
 return mlx4_cmd_box(&priv->dev, 0, mailbox->dma, eq_num,
 0, MLX4_CMD_HW2SW_EQ, MLX4_CMD_TIME_CLASS_A,
 MLX4_CMD_WRAPPED);
 }
 */
static int mlx4_num_eq_uar(struct mlx4_dev *dev) {

	/*
	 * Each UAR holds 4 EQ doorbells.  To figure out how many UARs
	 * we need to map, take the difference of highest index and
	 * the lowest index we'll use and add 1.
	 */
	return (dev->caps.num_comp_vectors + 1 + dev->caps.reserved_eqs
			+ dev->caps.comp_pool) / 4 - dev->caps.reserved_eqs / 4 + 1;
}

static void *mlx4_get_eq_uar(struct mlx4_priv *priv, struct mlx4_eq *eq) {
	int index;

	index = eq->eqn / 4 - priv->dev.caps.reserved_eqs / 4;

	if (!priv->eq_table.uar_map[index]) {
		priv->eq_table.uar_map[index] = priv->dev.bar_info[1].vaddr
				+ ((eq->eqn / 4) << PAGE_SHIFT);
	}

	return priv->eq_table.uar_map[index] + 0x800 + 8 * (eq->eqn % 4);
}
/*
 static void mlx4_unmap_uar(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int i;

 for (i = 0; i < mlx4_num_eq_uar(&priv->dev); ++i)
 if (priv->eq_table.uar_map[i]) {
 iounmap(priv->eq_table.uar_map[i]);
 priv->eq_table.uar_map[i] = NULL;
 }
 }
 */
static int mlx4_create_eq(struct mlx4_priv *priv, int nent, u8 intr,
		struct mlx4_eq *eq) {
	struct mlx4_cmd_mailbox *mailbox;
	struct mlx4_eq_context *eq_context;
	int npages;
	u64 *dma_list = NULL;
	genpaddr_t t = 0;
	u64 mtt_addr;
	int err = -ENOMEM;
	int i;

	eq->priv = priv;
	eq->nent = roundup_pow_of_two(max(nent, 2));
	/* CX3 is capable of extending the CQE\EQE from 32 to 64 bytes*/
	npages = PAGE_ALIGN(
			eq->nent * (MLX4_EQ_ENTRY_SIZE << priv->dev.caps.eqe_factor))
			/ BASE_PAGE_SIZE;

	eq->page_list = malloc(npages * sizeof *eq->page_list);
	if (!eq->page_list)
		goto err_out;

	for (i = 0; i < npages; ++i)
		eq->page_list[i].buf = NULL;

	dma_list = malloc(npages * sizeof *dma_list);
	if (!dma_list)
		goto err_out_free;

	mailbox = mlx4_alloc_cmd_mailbox();
	if (IS_ERR(mailbox))
		goto err_out_free;
	eq_context = mailbox->buf;

	for (i = 0; i < npages; ++i) {
		eq->page_list[i].buf = dma_alloc(BASE_PAGE_SIZE, &t);
		if (!eq->page_list[i].buf)
			goto err_out_free_pages;

		dma_list[i] = t;
		eq->page_list[i].map = t;

		memset(eq->page_list[i].buf, 0, BASE_PAGE_SIZE);
	}

	eq->eqn = mlx4_bitmap_alloc(&priv->eq_table.bitmap);
	if (eq->eqn == -1)
		goto err_out_free_pages;

	eq->doorbell = mlx4_get_eq_uar(priv, eq);
	if (!eq->doorbell) {
		err = -ENOMEM;
		goto err_out_free_eq;
	}

	err = mlx4_mtt_init(&priv->dev, npages, PAGE_SHIFT, &eq->mtt);
	if (err)
		goto err_out_free_eq;

	err = mlx4_write_mtt(&priv->dev, &eq->mtt, 0, npages, dma_list);
	if (err)
		goto err_out_free_mtt;

	memset(eq_context, 0, sizeof *eq_context);
	eq_context->flags = cpu_to_be32(MLX4_EQ_STATUS_OK |
	MLX4_EQ_STATE_ARMED);
	eq_context->log_eq_size = ilog2(eq->nent);
	eq_context->intr = intr;
	eq_context->log_page_size = PAGE_SHIFT - MLX4_ICM_PAGE_SHIFT;

	/*printf("mtt_addr: %lx\n", mlx4_mtt_addr(&priv->dev, &eq->mtt));
	 printf("off: %d\n", eq->mtt.offset);
	 printf("size: %d\n", priv->dev.caps.mtt_entry_sz);*/

	mtt_addr = mlx4_mtt_addr(&priv->dev, &eq->mtt);
	eq_context->mtt_base_addr_h = mtt_addr >> 32;
	eq_context->mtt_base_addr_l = cpu_to_be32(mtt_addr & 0xffffffff);

	err = mlx4_SW2HW_EQ(priv, mailbox, eq->eqn);
	if (err) {
		MLX4_DEBUG("SW2HW_EQ failed (%d)\n", err);
		goto err_out_free_mtt;
	}

	free(dma_list);
	mlx4_free_cmd_mailbox(mailbox);

	eq->cons_index = 0;

	return err;

	/*TODO*/
	err_out_free_mtt: /*mlx4_mtt_cleanup(&priv->dev, &eq->mtt);*/

	err_out_free_eq: /*mlx4_bitmap_free(&priv->eq_table.bitmap, eq->eqn,
	 MLX4_USE_RR);*/

	err_out_free_pages: /*for (i = 0; i < npages; ++i)
	 if (eq->page_list[i].buf)
	 dma_free(&priv->dev.pdev->dev, PAGE_SIZE,
	 eq->page_list[i].buf, eq->page_list[i].map);*/

	mlx4_free_cmd_mailbox(mailbox);

	err_out_free: free(eq->page_list);
	free(dma_list);

	err_out: return err;
}
/*
 static void mlx4_free_eq(struct mlx4_priv *priv,
 struct mlx4_eq *eq)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 struct mlx4_cmd_mailbox *mailbox;
 int err;
 int i;
 CX3 is capable of extending the CQE\EQE from 32 to 64 bytes
 int npages = PAGE_ALIGN((MLX4_EQ_ENTRY_SIZE << priv->dev.caps.eqe_factor) * eq->nent) / PAGE_SIZE;

 mailbox = mlx4_alloc_cmd_mailbox(&priv->dev);
 if (IS_ERR(mailbox))
 return;

 err = mlx4_HW2SW_EQ(&priv->dev, mailbox, eq->eqn);
 if (err)
 MLX4_DEBUG( "HW2SW_EQ failed (%d)\n", err);

 if (0) {
 MLX4_DEBUG( "Dumping EQ context %02x:\n", eq->eqn);
 for (i = 0; i < sizeof (struct mlx4_eq_context) / 4; ++i) {
 if (i % 4 == 0)
 pr_cont("[%02x] ", i * 4);
 pr_cont(" %08x", be32_to_cpup(mailbox->buf + i * 4));
 if ((i + 1) % 4 == 0)
 pr_cont("\n");
 }
 }

 mlx4_mtt_cleanup(&priv->dev, &eq->mtt);
 for (i = 0; i < npages; ++i)
 dma_free_coherent(&priv->dev.pdev->dev, PAGE_SIZE,
 eq->page_list[i].buf,
 eq->page_list[i].map);

 kfree(eq->page_list);
 mlx4_bitmap_free(&priv->eq_table.bitmap, eq->eqn, MLX4_USE_RR);
 mlx4_free_cmd_mailbox(&priv->dev, mailbox);
 }

 static void mlx4_free_irqs(struct mlx4_priv *priv)
 {
 struct mlx4_eq_table *eq_table = &mlx4_priv(&priv->dev)->eq_table;
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int	i, vec;

 if (eq_table->have_irq)
 free_irq(priv->dev.pdev->irq, dev);

 for (i = 0; i < priv->dev.caps.num_comp_vectors + 1; ++i)
 if (eq_table->eq[i].have_irq) {
 free_irq(eq_table->eq[i].irq, eq_table->eq + i);
 eq_table->eq[i].have_irq = 0;
 }

 for (i = 0; i < priv->dev.caps.comp_pool; i++) {

 * Freeing the assigned irq's
 * all bits should be 0, but we need to validate

 if (priv->msix_ctl.pool_bm & 1ULL << i) {
 NO need protecting
 vec = priv->dev.caps.num_comp_vectors + 1 + i;
 free_irq(priv->eq_table.eq[vec].irq,
 &priv->eq_table.eq[vec]);
 }
 }


 kfree(eq_table->irq_names);
 }
 */
static int mlx4_map_clr_int(struct mlx4_priv *priv) {

	/*equivalent of: pci_resource_start(priv->dev.pdev, priv->fw.clr_int_bar) + priv->fw.clr_int_base*/
	priv->clr_base = priv->dev.bar_info[priv->fw.clr_int_bar].vaddr
			+ priv->fw.clr_int_base;

	return 0;
}
/*
 static void mlx4_unmap_clr_int(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);

 iounmap(priv->clr_base);
 }
 */
int mlx4_alloc_eq_table(struct mlx4_priv *priv) {

	priv->eq_table.eq = calloc(
			priv->dev.caps.num_eqs - priv->dev.caps.reserved_eqs,
			sizeof *priv->eq_table.eq);
	if (!priv->eq_table.eq)
		return -ENOMEM;

	return 0;
}
/*
 void mlx4_free_eq_table(struct mlx4_priv *priv)
 {
 kfree(mlx4_priv(&priv->dev)->eq_table.eq);
 }
 */
int mlx4_init_eq_table(struct mlx4_priv *priv) {
	int err;
	int i;

	priv->eq_table.uar_map = calloc(mlx4_num_eq_uar(&priv->dev),
			sizeof *priv->eq_table.uar_map);
	if (!priv->eq_table.uar_map) {
		err = -ENOMEM;
		goto err_out_free;
	}

	err = mlx4_bitmap_init(&priv->eq_table.bitmap, priv->dev.caps.num_eqs,
			priv->dev.caps.num_eqs - 1, priv->dev.caps.reserved_eqs, 0);
	if (err)
		goto err_out_free;

	for (i = 0; i < mlx4_num_eq_uar(&priv->dev); ++i)
		priv->eq_table.uar_map[i] = NULL;

	if (!mlx4_is_slave(&priv->dev)) {
		err = mlx4_map_clr_int(priv);
		if (err)
			goto err_out_bitmap;

		priv->eq_table.clr_mask = swab32(1 << (priv->eq_table.inta_pin & 31));
		priv->eq_table.clr_int = priv->clr_base
				+ (priv->eq_table.inta_pin < 32 ? 4 : 0);
	}

	priv->eq_table.irq_names = malloc(
			MLX4_IRQNAME_SIZE
					* (priv->dev.caps.num_comp_vectors + 1
							+ priv->dev.caps.comp_pool));
	if (!priv->eq_table.irq_names) {
		err = -ENOMEM;
		goto err_out_clr_int;
	}

	for (i = 0; i < priv->dev.caps.num_comp_vectors; ++i) {
		err = mlx4_create_eq(priv,
				priv->dev.caps.num_cqs - priv->dev.caps.reserved_cqs
						+ MLX4_NUM_SPARE_EQE,
				(priv->dev.flags & MLX4_FLAG_MSI_X) ? i : 0,
				&priv->eq_table.eq[i]);
		if (err) {
			--i;
			goto err_out_unmap;
		}
	}

	err = mlx4_create_eq(priv, MLX4_NUM_ASYNC_EQE + MLX4_NUM_SPARE_EQE,
			(priv->dev.flags & MLX4_FLAG_MSI_X) ?
					priv->dev.caps.num_comp_vectors : 0,
			&priv->eq_table.eq[priv->dev.caps.num_comp_vectors]);
	if (err)
		goto err_out_comp;

	/*if additional completion vectors poolsize is 0 this loop will not run*/
	for (i = priv->dev.caps.num_comp_vectors + 1;
			i < priv->dev.caps.num_comp_vectors + priv->dev.caps.comp_pool + 1;
			++i) {

		err = mlx4_create_eq(priv,
				priv->dev.caps.num_cqs - priv->dev.caps.reserved_cqs
						+ MLX4_NUM_SPARE_EQE,
				(priv->dev.flags & MLX4_FLAG_MSI_X) ? i : 0,
				&priv->eq_table.eq[i]);
		if (err) {
			--i;
			goto err_out_unmap;
		}
	}

	if (priv->dev.flags & MLX4_FLAG_MSI_X) {
		assert(!"not implemented!");
		/*const char *eq_name;

		 for (i = 0; i < priv->dev.caps.num_comp_vectors + 1; ++i) {
		 if (i < priv->dev.caps.num_comp_vectors) {
		 snprintf(priv->eq_table.irq_names + i * MLX4_IRQNAME_SIZE,
		 MLX4_IRQNAME_SIZE, "mlx4-comp-%d@pci:", i
		 pci_name(priv->dev.pdev));
		 } else {
		 snprintf(priv->eq_table.irq_names + i * MLX4_IRQNAME_SIZE,
		 MLX4_IRQNAME_SIZE, "mlx4-async@pci:"
		 pci_name(priv->dev.pdev));
		 }

		 eq_name = priv->eq_table.irq_names + i * MLX4_IRQNAME_SIZE;
		 err = request_irq(priv->eq_table.eq[i].irq, mlx4_msi_x_interrupt, 0,
		 eq_name, priv->eq_table.eq + i);
		 if (err)
		 goto err_out_async;

		 priv->eq_table.eq[i].have_irq = 1;
		 }*/
	} else {
		snprintf(priv->eq_table.irq_names, MLX4_IRQNAME_SIZE,
		DRV_NAME "@pci:"/*, pci_name(priv->dev.pdev)*/);
		/*err = request_irq(priv->dev.pdev->irq, mlx4_interrupt, IRQF_SHARED,
		 priv->eq_table.irq_names, dev);
		 if (err)
		 goto err_out_async;*/

		priv->eq_table.have_irq = 1;
	}

	err = mlx4_MAP_EQ(priv, get_async_ev_mask(priv), 0,
			priv->eq_table.eq[priv->dev.caps.num_comp_vectors].eqn);
	if (err)
		MLX4_DEBUG("MAP_EQ for async EQ %d failed (%d)\n",
				priv->eq_table.eq[priv->dev.caps.num_comp_vectors].eqn, err);

	for (i = 0; i < priv->dev.caps.num_comp_vectors + 1; ++i)
		eq_set_ci(&priv->eq_table.eq[i], 1);

	return 0;

	/*TODO*/
	/*err_out_async:*//*mlx4_free_eq(&priv->dev,
	 &priv->eq_table.eq[priv->dev.caps.num_comp_vectors]);*/

	err_out_comp: i = priv->dev.caps.num_comp_vectors - 1;

	err_out_unmap: /*while (i >= 0) {
	 mlx4_free_eq(&priv->dev, &priv->eq_table.eq[i]);
	 --i;
	 }
	 mlx4_free_irqs(&priv->dev);*/

	err_out_clr_int: /*if (!mlx4_is_slave(&priv->dev))
	 mlx4_unmap_clr_int(&priv->dev);*/

	err_out_bitmap: /*mlx4_unmap_uar(&priv->dev);*/
	mlx4_bitmap_cleanup(&priv->eq_table.bitmap);

	err_out_free: free(priv->eq_table.uar_map);

	return err;
}
/*		void mlx4_cleanup_eq_table(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int i;

 mlx4_MAP_EQ(&priv->dev, get_async_ev_mask(&priv->dev), 1,
 priv->eq_table.eq[priv->dev.caps.num_comp_vectors].eqn);

 mlx4_free_irqs(&priv->dev);

 for (i = 0; i < priv->dev.caps.num_comp_vectors + priv->dev.caps.comp_pool + 1; ++i)
 mlx4_free_eq(&priv->dev, &priv->eq_table.eq[i]);

 if (!mlx4_is_slave(&priv->dev))
 mlx4_unmap_clr_int(&priv->dev);

 mlx4_unmap_uar(&priv->dev);
 mlx4_bitmap_cleanup(&priv->eq_table.bitmap);

 kfree(priv->eq_table.uar_map);
 }

 A test that verifies that we can accept interrupts on all
 * the irq vectors of the device.
 * Interrupts are checked using the NOP command.

 int mlx4_test_interrupts(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int i;
 int err;

 err = mlx4_NOP(&priv->dev);
 When not in MSI_X, there is only one irq to check
 if (!(priv->dev.flags & MLX4_FLAG_MSI_X) || mlx4_is_slave(&priv->dev))
 return err;

 A loop over all completion vectors, for each vector we will check
 * whether it works by mapping command completions to that vector
 * and performing a NOP command

 for(i = 0; !err && (i < priv->dev.caps.num_comp_vectors); ++i) {
 Temporary use polling for command completions
 mlx4_cmd_use_polling(&priv->dev);

 Map the new eq to handle all asyncronous events
 err = mlx4_MAP_EQ(&priv->dev, get_async_ev_mask(&priv->dev), 0,
 priv->eq_table.eq[i].eqn);
 if (err) {
 MLX4_DEBUG( "Failed mapping eq for interrupt test\n");
 mlx4_cmd_use_events(&priv->dev);
 break;
 }

 Go back to using events
 mlx4_cmd_use_events(&priv->dev);
 err = mlx4_NOP(&priv->dev);
 }

 Return to default
 mlx4_MAP_EQ(&priv->dev, get_async_ev_mask(&priv->dev), 0,
 priv->eq_table.eq[priv->dev.caps.num_comp_vectors].eqn);
 return err;
 }
 EXPORT_SYMBOL( mlx4_test_interrupts);

 int mlx4_assign_eq(struct mlx4_priv *priv, char* name, int * vector) {

 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 int vec = 0, err = 0, i;

 mutex_lock(&priv->msix_ctl.pool_lock);
 for (i = 0; !vec && i < priv->dev.caps.comp_pool; i++) {
 if (~priv->msix_ctl.pool_bm & 1ULL << i) {
 priv->msix_ctl.pool_bm |= 1ULL << i;
 vec = priv->dev.caps.num_comp_vectors + 1 + i;
 snprintf(priv->eq_table.irq_names + vec * MLX4_IRQNAME_SIZE,
 MLX4_IRQNAME_SIZE, "%s", name);
 err = request_irq(priv->eq_table.eq[vec].irq,
 mlx4_msi_x_interrupt, 0,
 &priv->eq_table.irq_names[vec << 5],
 priv->eq_table.eq + vec);
 if (err) {
 zero out
 bit by
 fliping it
 priv->msix_ctl.pool_bm ^= 1 << i;
 vec = 0;
 continue;
 we dont
 want to
 break here
 }
 eq_set_ci(&priv->eq_table.eq[vec], 1);
 }
 }
 mutex_unlock(&priv->msix_ctl.pool_lock);

 if (vec) {
 *vector = vec;
 } else {
 *vector = 0;
 err = (i == priv->dev.caps.comp_pool) ? -ENOSPC : err;
 }
 return err;
 }
 EXPORT_SYMBOL( mlx4_assign_eq);

 void mlx4_release_eq(struct mlx4_priv *priv, int vec) {
 struct mlx4_priv *priv = mlx4_priv(&priv->dev);
 bm index
 int i = vec - priv->dev.caps.num_comp_vectors - 1;

 if (likely(i >= 0)) {
 sanity check, making
 sure were
 not trying
 to free
 irq
 's
 Belonging to
 a legacy
 EQ
 mutex_lock(&priv->msix_ctl.pool_lock);
 if (priv->msix_ctl.pool_bm & 1ULL << i) {
 free_irq(priv->eq_table.eq[vec].irq, &priv->eq_table.eq[vec]);
 priv->msix_ctl.pool_bm &= ~(1ULL << i);
 }
 mutex_unlock(&priv->msix_ctl.pool_lock);
 }

 }
 EXPORT_SYMBOL( mlx4_release_eq);
 */
