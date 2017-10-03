/*

 * Copyright (c) 2004, 2005 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005, 2006, 2007, 2008, 2014 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2005, 2006, 2007 Cisco Systems, Inc.  All rights reserved.
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


 #include <linux/sched.h>
 #include <linux/slab.h>
 #include <linux/module.h>
 #include <linux/pci.h>
 #include <linux/errno.h>
 #include <linux/mlx4/device.h>
 #include <linux/semaphore.h>
 #include <rdma/ib_smi.h>

 #include <asm/io.h>
 #include <linux/ktime.h>
 */
#include <debug.h>
#include "fw.h"

#include <linux/io.h>
#include <linux/pci.h>
#include <linux/mlx4/cmd.h>
#include <linux/completion.h>

#include <asm/byteorder.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/sys_debug.h>

#define CMD_POLL_TOKEN 0xffff
/*#define INBOX_MASK	0xffffffffffffff00ULL

 #define CMD_CHAN_VER 1
 #define CMD_CHAN_IF_REV 1
 */
enum {
	/* command completed successfully: */
	CMD_STAT_OK = 0x00,
	/* Internal error (such as a bus error) occurred while processing command: */
	CMD_STAT_INTERNAL_ERR = 0x01,
	/* Operation/command not supported or opcode modifier not supported: */
	CMD_STAT_BAD_OP = 0x02,
	/* Parameter not supported or parameter out of range: */
	CMD_STAT_BAD_PARAM = 0x03,
	/* System not enabled or bad system state: */
	CMD_STAT_BAD_SYS_STATE = 0x04,
	/* Attempt to access reserved or unallocaterd resource: */
	CMD_STAT_BAD_RESOURCE = 0x05,
	/* Requested resource is currently executing a command, or is otherwise busy: */
	CMD_STAT_RESOURCE_BUSY = 0x06,
	/* Required capability exceeds device limits: */
	CMD_STAT_EXCEED_LIM = 0x08,
	/* Resource is not in the appropriate state or ownership: */
	CMD_STAT_BAD_RES_STATE = 0x09,
	/* Index out of range: */
	CMD_STAT_BAD_INDEX = 0x0a,
	/* FW image corrupted: */
	CMD_STAT_BAD_NVMEM = 0x0b,
	/* Error in ICM mapping (e.g. not enough auxiliary ICM pages to execute command): */
	CMD_STAT_ICM_ERROR = 0x0c,
	/* Attempt to modify a QP/EE which is not in the presumed state: */
	CMD_STAT_BAD_QP_STATE = 0x10,
	/* Bad segment parameters (Address/Size): */
	CMD_STAT_BAD_SEG_PARAM = 0x20,
	/* Memory Region has Memory Windows bound to: */
	CMD_STAT_REG_BOUND = 0x21,
	/* HCA local attached memory not present: */
	CMD_STAT_LAM_NOT_PRE = 0x22,
	/* Bad management packet (silently discarded): */
	CMD_STAT_BAD_PKT = 0x30,
	/* More outstanding CQEs in CQ than new CQ size: */
	CMD_STAT_BAD_SIZE = 0x40,
	/* Multi Function device support required: */
	CMD_STAT_MULTI_FUNC_REQ = 0x50,
};

enum {
	HCR_IN_PARAM_OFFSET = 0x00,
	HCR_IN_MODIFIER_OFFSET = 0x08,
	HCR_OUT_PARAM_OFFSET = 0x0c,
	HCR_TOKEN_OFFSET = 0x14,
	HCR_STATUS_OFFSET = 0x18,

	HCR_OPMOD_SHIFT = 12,
	HCR_T_BIT = 21,
	HCR_E_BIT = 22,
	HCR_GO_BIT = 23
};
/*
 enum {
 GO_BIT_TIMEOUT_MSECS	= 10000
 };

 enum mlx4_vlan_transition {
 MLX4_VLAN_TRANSITION_VST_VST = 0,
 MLX4_VLAN_TRANSITION_VST_VGT = 1,
 MLX4_VLAN_TRANSITION_VGT_VST = 2,
 MLX4_VLAN_TRANSITION_VGT_VGT = 3,
 };

 */
struct mlx4_cmd_context {
	struct completion done;
	int result;
	int next;
	u64 out_param;
	u16 token;
	u8 fw_status;
};
/*
 static int mlx4_master_process_vhcr(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr_cmd *in_vhcr);
 */
static int mlx4_status_to_errno(u8 status) {
	static const int trans_table[] =
			{ [CMD_STAT_INTERNAL_ERR] = -EIO, [CMD_STAT_BAD_OP] = -EPERM,
					[CMD_STAT_BAD_PARAM] = -EINVAL, [CMD_STAT_BAD_SYS_STATE
							] = -ENXIO, [CMD_STAT_BAD_RESOURCE] = -EBADF,
					[CMD_STAT_RESOURCE_BUSY] = -EBUSY, [CMD_STAT_EXCEED_LIM
							] = -ENOMEM, [CMD_STAT_BAD_RES_STATE] = -EBADF,
					[CMD_STAT_BAD_INDEX] = -EBADF, [CMD_STAT_BAD_NVMEM
							] = -EFAULT, [CMD_STAT_ICM_ERROR] = -ENFILE,
					[CMD_STAT_BAD_QP_STATE] = -EINVAL, [CMD_STAT_BAD_SEG_PARAM
							] = -EFAULT, [CMD_STAT_REG_BOUND] = -EBUSY,
					[CMD_STAT_LAM_NOT_PRE] = -EAGAIN, [CMD_STAT_BAD_PKT
							] = -EINVAL, [CMD_STAT_BAD_SIZE] = -ENOMEM,
					[CMD_STAT_MULTI_FUNC_REQ] = -EACCES, };

	if (status >= ARRAY_SIZE(trans_table)
			|| (status != CMD_STAT_OK && trans_table[status] == 0))
		return -EIO;

	return trans_table[status];
}

static const char *cmd_to_str(u16 cmd) {
	switch (cmd) {
	case MLX4_CMD_SYS_EN:
		return "SYS_EN";
	case MLX4_CMD_SYS_DIS:
		return "SYS_DIS";
	case MLX4_CMD_MAP_FA:
		return "MAP_FA";
	case MLX4_CMD_UNMAP_FA:
		return "UNMAP_FA";
	case MLX4_CMD_RUN_FW:
		return "RUN_FW";
	case MLX4_CMD_MOD_STAT_CFG:
		return "MOD_STAT_CFG";
	case MLX4_CMD_QUERY_DEV_CAP:
		return "QUERY_DEV_CAP";
	case MLX4_CMD_QUERY_FW:
		return "QUERY_FW";
	case MLX4_CMD_ENABLE_LAM:
		return "ENABLE_LAM";
	case MLX4_CMD_DISABLE_LAM:
		return "DISABLE_LAM";
	case MLX4_CMD_QUERY_DDR:
		return "QUERY_DDR";
	case MLX4_CMD_QUERY_ADAPTER:
		return "QUERY_ADAPTER";
	case MLX4_CMD_INIT_HCA:
		return "INIT_HCA";
	case MLX4_CMD_CLOSE_HCA:
		return "CLOSE_HCA";
	case MLX4_CMD_INIT_PORT:
		return "INIT_PORT";
	case MLX4_CMD_CLOSE_PORT:
		return "CLOSE_PORT";
	case MLX4_CMD_QUERY_HCA:
		return "QUERY_HCA";
	case MLX4_CMD_QUERY_PORT:
		return "QUERY_PORT";
	case MLX4_CMD_SENSE_PORT:
		return "SENSE_PORT";
	case MLX4_CMD_HW_HEALTH_CHECK:
		return "HW_HEALTH_CHECK";
	case MLX4_CMD_SET_PORT:
		return "SET_PORT";
	case MLX4_CMD_SET_NODE:
		return "SET_NODE";
	case MLX4_CMD_QUERY_FUNC:
		return "QUERY_FUNC";
	case MLX4_CMD_MAP_ICM:
		return "MAP_ICM";
	case MLX4_CMD_UNMAP_ICM:
		return "UNMAP_ICM";
	case MLX4_CMD_MAP_ICM_AUX:
		return "MAP_ICM_AUX";
	case MLX4_CMD_UNMAP_ICM_AUX:
		return "UNMAP_ICM_AUX";
	case MLX4_CMD_SET_ICM_SIZE:
		return "SET_ICM_SIZE";
		/*master notify fw on finish for slave's flr*/
	case MLX4_CMD_INFORM_FLR_DONE:
		return "INFORM_FLR_DONE";
	case MLX4_CMD_GET_OP_REQ:
		return "GET_OP_REQ";

		/* TPT commands */
	case MLX4_CMD_SW2HW_MPT:
		return "SW2HW_MPT";
	case MLX4_CMD_QUERY_MPT:
		return "QUERY_MPT";
	case MLX4_CMD_HW2SW_MPT:
		return "HW2SW_MPT";
	case MLX4_CMD_READ_MTT:
		return "READ_MTT";
	case MLX4_CMD_WRITE_MTT:
		return "WRITE_MTT";
	case MLX4_CMD_SYNC_TPT:
		return "SYNC_TPT";

		/* EQ commands */
	case MLX4_CMD_MAP_EQ:
		return "MAP_EQ";
	case MLX4_CMD_SW2HW_EQ:
		return "SW2HW_EQ";
	case MLX4_CMD_HW2SW_EQ:
		return "HW2SW_EQ";
	case MLX4_CMD_QUERY_EQ:
		return "QUERY_EQ";

		/* CQ commands */
	case MLX4_CMD_SW2HW_CQ:
		return "SW2HW_CQ";
	case MLX4_CMD_HW2SW_CQ:
		return "HW2SW_CQ";
	case MLX4_CMD_QUERY_CQ:
		return "QUERY_CQ:";
	case MLX4_CMD_MODIFY_CQ:
		return "MODIFY_CQ:";

		/* SRQ commands */
	case MLX4_CMD_SW2HW_SRQ:
		return "SW2HW_SRQ";
	case MLX4_CMD_HW2SW_SRQ:
		return "HW2SW_SRQ";
	case MLX4_CMD_QUERY_SRQ:
		return "QUERY_SRQ";
	case MLX4_CMD_ARM_SRQ:
		return "ARM_SRQ";

		/* QP/EE commands */
	case MLX4_CMD_RST2INIT_QP:
		return "RST2INIT_QP";
	case MLX4_CMD_INIT2RTR_QP:
		return "INIT2RTR_QP";
	case MLX4_CMD_RTR2RTS_QP:
		return "RTR2RTS_QP";
	case MLX4_CMD_RTS2RTS_QP:
		return "RTS2RTS_QP";
	case MLX4_CMD_SQERR2RTS_QP:
		return "SQERR2RTS_QP";
	case MLX4_CMD_2ERR_QP:
		return "2ERR_QP";
	case MLX4_CMD_RTS2SQD_QP:
		return "RTS2SQD_QP";
	case MLX4_CMD_SQD2SQD_QP:
		return "SQD2SQD_QP";
	case MLX4_CMD_SQD2RTS_QP:
		return "SQD2RTS_QP";
	case MLX4_CMD_2RST_QP:
		return "2RST_QP";
	case MLX4_CMD_QUERY_QP:
		return "QUERY_QP";
	case MLX4_CMD_INIT2INIT_QP:
		return "INIT2INIT_QP";
	case MLX4_CMD_SUSPEND_QP:
		return "SUSPEND_QP";
	case MLX4_CMD_UNSUSPEND_QP:
		return "UNSUSPEND_QP";
		/* special QP and management commands */
	case MLX4_CMD_CONF_SPECIAL_QP:
		return "CONF_SPECIAL_QP";
	case MLX4_CMD_MAD_IFC:
		return "MAD_IFC";

		/* multicast commands */
	case MLX4_CMD_READ_MCG:
		return "READ_MCG";
	case MLX4_CMD_WRITE_MCG:
		return "WRITE_MCG";
	case MLX4_CMD_MGID_HASH:
		return "MGID_HASH";

		/* miscellaneous commands */
	case MLX4_CMD_DIAG_RPRT:
		return "DIAG_RPRT";
	case MLX4_CMD_NOP:
		return "NOP";
	case MLX4_CMD_ACCESS_MEM:
		return "ACCESS_MEM";
	case MLX4_CMD_SET_VEP:
		return "SET_VEP";

		/* Ethernet specific commands */
	case MLX4_CMD_SET_VLAN_FLTR:
		return "SET_VLAN_FLTR";
	case MLX4_CMD_SET_MCAST_FLTR:
		return "SET_MCAST_FLTR";
	case MLX4_CMD_DUMP_ETH_STATS:
		return "DUMP_ETH_STATS";

		/* Communication channel commands */
	case MLX4_CMD_ARM_COMM_CHANNEL:
		return "ARM_COMM_CHANNEL";
	case MLX4_CMD_GEN_EQE:
		return "GEN_EQE";

		/* virtual commands */
	case MLX4_CMD_ALLOC_RES:
		return "ALLOC_RES";
	case MLX4_CMD_FREE_RES:
		return "FREE_RES";
	case MLX4_CMD_MCAST_ATTACH:
		return "MCAST_ATTACH";
	case MLX4_CMD_UCAST_ATTACH:
		return "UCAST_ATTACH";
	case MLX4_CMD_PROMISC:
		return "PROMISC";
	case MLX4_CMD_QUERY_FUNC_CAP:
		return "QUERY_FUNC_CAP";
	case MLX4_CMD_QP_ATTACH:
		return "QP_ATTACH";

		/* debug commands */
	case MLX4_CMD_QUERY_DEBUG_MSG:
		return "QUERY_DEBUG_MSG";
	case MLX4_CMD_SET_DEBUG_MSG:
		return "SET_DEBUG_MSG";

		/* statistics commands */
	case MLX4_CMD_QUERY_IF_STAT:
		return "QUERY_IF_STAT";
	case MLX4_CMD_SET_IF_STAT:
		return "SET_IF_STAT";

		/* register/delete flow steering network rules */
	case MLX4_QP_FLOW_STEERING_ATTACH:
		return "QP_FLOW_STEERING_ATTACH";
	case MLX4_QP_FLOW_STEERING_DETACH:
		return "QP_FLOW_STEERING_DETACH";
	case MLX4_FLOW_STEERING_IB_UC_QP_RANGE:
		return "FLOW_STEERING_IB_UC_QP_RANGE";
	default:
		return "OTHER";
	}
}
/*
 static u8 mlx4_errno_to_status(int errno)
 {
 switch (errno) {
 case -EPERM:
 return CMD_STAT_BAD_OP;
 case -EINVAL:
 return CMD_STAT_BAD_PARAM;
 case -ENXIO:
 return CMD_STAT_BAD_SYS_STATE;
 case -EBUSY:
 return CMD_STAT_RESOURCE_BUSY;
 case -ENOMEM:
 return CMD_STAT_EXCEED_LIM;
 case -ENFILE:
 return CMD_STAT_ICM_ERROR;
 default:
 return CMD_STAT_INTERNAL_ERR;
 }
 }

 static int comm_pending(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 u32 status = readl(&priv->mfunc.comm->slave_read);

 return (swab32(status) >> 31) != priv->cmd.comm_toggle;
 }

 static void mlx4_comm_cmd_post(struct mlx4_priv *priv, u8 cmd, u16 param)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 u32 val;

 priv->cmd.comm_toggle ^= 1;
 val = param | (cmd << 16) | (priv->cmd.comm_toggle << 31);
 __raw_writel((__force u32) cpu_to_be32(val),
 &priv->mfunc.comm->slave_write);
 mmiowb();
 }

 static int mlx4_comm_cmd_poll(struct mlx4_priv *priv, u8 cmd, u16 param,
 unsigned long timeout)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 unsigned long end;
 int err = 0;
 int ret_from_pending = 0;

 First, verify that the master reports correct status
 if (comm_pending(dev)) {
 MLX4_DEBUG( "Communication channel is not idle."
 "my toggle is %d (cmd:0x%x)\n",
 priv->cmd.comm_toggle, cmd);
 return -EAGAIN;
 }

 Write command
 down(&priv->cmd.poll_sem);
 mlx4_comm_cmd_post(dev, cmd, param);

 end = msecs_to_jiffies(timeout) + jiffies;
 while (comm_pending(dev) && time_before(jiffies, end))
 cond_resched();
 ret_from_pending = comm_pending(dev);
 if (ret_from_pending) {
 check if the slave is trying to boot in the middle of
 * FLR process. The only non-zero result in the RESET command
 * is MLX4_DELAY_RESET_SLAVE
 if ((MLX4_COMM_CMD_RESET == cmd)) {
 MLX4_DEBUG( "Got slave FLRed from Communication"
 " channel (ret:0x%x)\n", ret_from_pending);
 err = MLX4_DELAY_RESET_SLAVE;
 } else {
 MLX4_DEBUG( "Communication channel timed out\n");
 err = -ETIMEDOUT;
 }
 }

 up(&priv->cmd.poll_sem);
 return err;
 }

 static int mlx4_comm_cmd_wait(struct mlx4_priv *priv, u8 op,
 u16 param, unsigned long timeout)
 {
 struct mlx4_cmd *cmd = &mlx4_priv(dev)->cmd;
 struct mlx4_cmd_context *context;
 unsigned long end;
 int err = 0;

 down(&cmd->event_sem);

 end = msecs_to_jiffies(timeout) + jiffies;
 while (comm_pending(dev) && time_before(jiffies, end))
 cond_resched();
 if (comm_pending(dev)) {
 MLX4_DEBUG( "mlx4_comm_cmd_wait: Comm channel "
 "is not idle. My toggle is %d (op: 0x%x)\n",
 mlx4_priv(dev)->cmd.comm_toggle, op);
 up(&cmd->event_sem);
 return -EAGAIN;
 }

 spin_lock(&cmd->context_lock);
 BUG_ON(cmd->free_head < 0);
 context = &cmd->context[cmd->free_head];
 context->token += cmd->token_mask + 1;
 cmd->free_head = context->next;
 spin_unlock(&cmd->context_lock);

 init_completion(&context->done);

 mlx4_comm_cmd_post(dev, op, param);

 In slave, wait unconditionally for completion
 wait_for_completion(&context->done);

 err = context->result;
 if (err && context->fw_status != CMD_STAT_MULTI_FUNC_REQ) {
 MLX4_DEBUG( "command 0x%x failed: fw status = 0x%x\n",
 op, context->fw_status);
 goto out;
 }

 out:
 wait for comm channel ready
 * this is necessary for prevention the race
 * when switching between event to polling mode

 end = msecs_to_jiffies(timeout) + jiffies;
 while (comm_pending(dev) && time_before(jiffies, end))
 cond_resched();

 spin_lock(&cmd->context_lock);
 context->next = cmd->free_head;
 cmd->free_head = context - cmd->context;
 spin_unlock(&cmd->context_lock);

 up(&cmd->event_sem);
 return err;
 }

 int mlx4_comm_cmd(struct mlx4_priv *priv, u8 cmd, u16 param,
 unsigned long timeout)
 {
 if (mlx4_priv(dev)->cmd.use_events)
 return mlx4_comm_cmd_wait(dev, cmd, param, timeout);
 return mlx4_comm_cmd_poll(dev, cmd, param, timeout);
 }
 */
static int cmd_pending(struct mlx4_priv *priv) {
	u32 status;

	if (pci_channel_offline())
		return -EIO;

	status = __raw_readl(priv->cmd.hcr + HCR_STATUS_OFFSET);

	/*MLX4_DEBUG("status: %x\n",status);*/

	return (status & swab32(1 << HCR_GO_BIT))
			|| (priv->cmd.toggle == !!(status & swab32(1 << HCR_T_BIT)));
}

static int get_status(struct mlx4_priv *priv, u32 *status, int *go_bit,
		int *t_bit) {
	if (pci_channel_offline())
		return -EIO;

	*status = __raw_readl(priv->cmd.hcr + HCR_STATUS_OFFSET);
	*t_bit = !!(*status & swab32(1 << HCR_T_BIT));
	*go_bit = !!(*status & swab32(1 << HCR_GO_BIT));

	return 0;
}

static int mlx4_cmd_post(struct mlx4_priv *pdev, struct timespec *ts1,
		u64 in_param, u64 out_param, u32 in_modifier, u8 op_modifier, u16 op,
		u16 token, int event) {
	struct mlx4_cmd *cmd = &pdev->cmd;
	u32 *hcr = cmd->hcr;
	int ret = -EAGAIN;
	unsigned long end = 0;
	/*errval_t err;*/
	/*int err, go_bit = 0, t_bit = 0;
	 u32 status = 0;*/

	/*mutex_lock(&cmd->hcr_mutex);*/

	if (pci_channel_offline()) {
		/*
		 * Device is going through error recovery
		 * and cannot accept commands.
		 */
		ret = -EIO;
		goto out;
	}
	/*
	 end = jiffies;
	 if (event)
	 end += msecs_to_jiffies(GO_BIT_TIMEOUT_MSECS);
	 */

	/*TODO: replace "end" with a barrelfish timestamp*/
	while (cmd_pending(pdev)) {
		if (pci_channel_offline()) {

			ret = -EIO;
			goto out;
		}

		if (end > 10000) {
			MLX4_DEBUG("%s:cmd_pending failed\n", __func__);
			goto out;
		}
		end++;
	}

	/*err = sys_debug_flush_cache();
	 assert(err_is_ok(err));*/

	/*	MLX4_DEBUG("hcr + 0: %d\n", (u32 ) cpu_to_be32(in_param >> 32));
	 MLX4_DEBUG("hcr + 1: %d\n", (u32 ) cpu_to_be32(in_param & 0xfffffffful));
	 MLX4_DEBUG("hcr + 2: %d\n", (u32 ) cpu_to_be32(in_modifier));
	 MLX4_DEBUG("hcr + 3: %d\n", (u32 ) cpu_to_be32(out_param >> 32));
	 MLX4_DEBUG("hcr + 4: %d\n", (u32 ) cpu_to_be32(out_param & 0xfffffffful));
	 MLX4_DEBUG("hcr + 5: %d\n", (u32 ) cpu_to_be32(token << 16));
	 MLX4_DEBUG("hcr + 6: %d\n",
	 (u32 ) cpu_to_be32(
	 (1 << HCR_GO_BIT) | (cmd->toggle << HCR_T_BIT)
	 | (event ? (1 << HCR_E_BIT) : 0)
	 | (op_modifier << HCR_OPMOD_SHIFT) | op));*/

	/*
	 * We use writel (instead of something like memcpy_toio)
	 * because writes of less than 32 bits to the HCR don't work
	 * (and some architectures such as ia64 implement memcpy_toio
	 * in terms of writeb).
	 */
	__raw_writel((u32) cpu_to_be32(in_param >> 32), hcr + 0);
	__raw_writel((u32) cpu_to_be32(in_param & 0xfffffffful), hcr + 1);
	__raw_writel((u32) cpu_to_be32(in_modifier), hcr + 2);
	__raw_writel((u32) cpu_to_be32(out_param >> 32), hcr + 3);
	__raw_writel((u32) cpu_to_be32(out_param & 0xfffffffful), hcr + 4);
	__raw_writel((u32) cpu_to_be32(token << 16), hcr + 5);

	/*	if (ts1)
	 ktime_get_ts(ts1);*/

	/* __raw_writel may not order writes. */
	__asm volatile("" ::: "memory");

	__raw_writel(
			(u32) cpu_to_be32(
					(1 << HCR_GO_BIT) | (cmd->toggle << HCR_T_BIT)
							| (event ? (1 << HCR_E_BIT) : 0)
							| (op_modifier << HCR_OPMOD_SHIFT) | op), hcr + 6);

	/*
	 * Make sure that our HCR writes don't get mixed in with
	 * writes from another CPU starting a FW command.
	 */
	/*	mmiowb();

	 */
	cmd->toggle = cmd->toggle ^ 1;
	ret = 0;

	/*printf("Posted command %s (0x%x): ret=%d, "
	 "in_param=0x%llx, in_mod=0x%x, op_mod=0x%x, "
	 "get_status err=, status_reg=, go_bit=, "
	 "t_bit=, toggle=0x%x\n", cmd_to_str(op), op, ret,
	 (unsigned long long) in_param, in_modifier, op_modifier, err,
	 status, go_bit, t_bit,cmd->toggle);*/

	out:
	/*	if (ret) {
	 err = get_status(dev, &status, &go_bit, &t_bit);
	 MLX4_DEBUG( "Could not post command %s (0x%x): ret=%d, "
	 "in_param=0x%llx, in_mod=0x%x, op_mod=0x%x, "
	 "get_status err=%d, status_reg=0x%x, go_bit=%d, "
	 "t_bit=%d, toggle=0x%x\n", cmd_to_str(op), op, ret,
	 (unsigned long long) in_param, in_modifier, op_modifier, err, status,
	 go_bit, t_bit, cmd->toggle);
	 }
	 mutex_unlock(&cmd->hcr_mutex);*/
	return ret;
}
/*
 static int mlx4_slave_cmd(struct mlx4_priv *priv, u64 in_param, u64 *out_param,
 int out_is_imm, u32 in_modifier, u8 op_modifier,
 u16 op, unsigned long timeout)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vhcr_cmd *vhcr = priv->mfunc.vhcr;
 int ret;

 mutex_lock(&priv->cmd.slave_cmd_mutex);

 vhcr->in_param = cpu_to_be64(in_param);
 vhcr->out_param = out_param ? cpu_to_be64(*out_param) : 0;
 vhcr->in_modifier = cpu_to_be32(in_modifier);
 vhcr->opcode = cpu_to_be16((((u16) op_modifier) << 12) | (op & 0xfff));
 vhcr->token = cpu_to_be16(CMD_POLL_TOKEN);
 vhcr->status = 0;
 vhcr->flags = !!(priv->cmd.use_events) << 6;

 if (mlx4_is_master(dev)) {
 ret = mlx4_master_process_vhcr(dev, dev->caps.function, vhcr);
 if (!ret) {
 if (out_is_imm) {
 if (out_param)
 *out_param =
 be64_to_cpu(vhcr->out_param);
 else {
 MLX4_DEBUG( "response expected while"
 "output mailbox is NULL for "
 "command 0x%x\n", op);
 vhcr->status = CMD_STAT_BAD_PARAM;
 }
 }
 ret = mlx4_status_to_errno(vhcr->status);
 }
 } else {
 ret = mlx4_comm_cmd(dev, MLX4_COMM_CMD_VHCR_POST, 0,
 MLX4_COMM_TIME + timeout);
 if (!ret) {
 if (out_is_imm) {
 if (out_param)
 *out_param =
 be64_to_cpu(vhcr->out_param);
 else {
 MLX4_DEBUG( "response expected while"
 "output mailbox is NULL for "
 "command 0x%x\n", op);
 vhcr->status = CMD_STAT_BAD_PARAM;
 }
 }
 ret = mlx4_status_to_errno(vhcr->status);
 } else
 MLX4_DEBUG( "failed execution of VHCR_POST command"
 "opcode %s (0x%x)\n", cmd_to_str(op), op);
 }

 mutex_unlock(&priv->cmd.slave_cmd_mutex);
 return ret;
 }
 */
static int mlx4_cmd_poll(struct mlx4_priv *priv, u64 in_param, u64 *out_param,
		int out_is_imm, u32 in_modifier, u8 op_modifier, u16 op,
		unsigned long timeout) {
	/*struct mlx4_priv *priv = mlx4_priv(dev);*/
	void *hcr = priv->cmd.hcr;
	int err = 0;
	unsigned long end = 0;
	u32 stat;

	/*down(&priv->cmd.poll_sem);

	 if (pci_channel_offline(dev->pdev)) {

	 *Device
	 is going
	 through error
	 recovery * and
	 cannot accept
	 commands.

	 err = -EIO;
	 goto out;
	 }
	 */
	err = mlx4_cmd_post(priv, NULL, in_param, out_param ? *out_param : 0,
			in_modifier, op_modifier, op, CMD_POLL_TOKEN, 0);
	if (err)
		goto out;

	/*MLX4_DEBUG("command %s (0x%x) \n", cmd_to_str(op), op);*/

	/*TODO: replace "end" with a barrelfish timestamp*/
	while (cmd_pending(priv) && end < 10000) {
		if (pci_channel_offline()) {
			/*
			 * Device is going through error recovery
			 * and cannot accept commands.
			 */
			err = -EIO;
			goto out;
		}
		barrelfish_usleep(1000);
		/*MLX4_DEBUG("command %lu\n", end);*/
		end++;
	}
	if (cmd_pending(priv)) {
		MLX4_DEBUG("command %s (0x%x) timed out (go bit not cleared)\n",
				cmd_to_str(op), op);
		err = -ETIMEDOUT;
		goto out;
	}

	if (out_is_imm)
	*out_param = (u64)
	be32_to_cpu(
			(__force __be32) __raw_readl(hcr + HCR_OUT_PARAM_OFFSET)) << 32 |
	(u64) be32_to_cpu((__force __be32)
			__raw_readl(hcr + HCR_OUT_PARAM_OFFSET + 4));
	stat = be32_to_cpu((__force __be32) __raw_readl(hcr + HCR_STATUS_OFFSET)) >> 24;
	err = mlx4_status_to_errno(stat);
	if (err)
		MLX4_DEBUG("command %s (0x%x) failed: fw status = 0x%x\n",
				cmd_to_str(op), op, stat);

	out: /*up(&priv->cmd.poll_sem);*/
	return err;
}

void mlx4_cmd_event(struct mlx4_priv *priv, u16 token, u8 status, u64 out_param) {
	struct mlx4_cmd_context *context = &priv->cmd.context[token
			& priv->cmd.token_mask];

	/*previously timed
	 out command
	 completing at
	 long last*/
	if (token != context->token)
		return;

	context->fw_status = status;
	context->result = mlx4_status_to_errno(status);
	context->out_param = out_param;

	got_irq = true;
}

static inline int mlx4_cmd_wait(struct mlx4_priv *priv, u64 in_param,
		u64 *out_param, int out_is_imm, u32 in_modifier, u8 op_modifier, u16 op,
		unsigned long timeout) {
	struct mlx4_cmd *cmd = &priv->cmd;
	struct mlx4_cmd_context *context;
	int err = 0;
	int go_bit = 0, t_bit = 0, stat_err;
	u32 status = 0;
	struct timespec ts1/*, ts2*/;
	/*ktime_t t1, t2, delta;
	 s64 ds;*/
	uint64_t current, ticks_per_msec;

	if (out_is_imm && !out_param)
		return -EINVAL;

	/*down(&cmd->event_sem);

	 spin_lock(&cmd->context_lock);
	 BUG_ON(cmd->free_head < 0);*/
	context = &cmd->context[cmd->free_head];
	context->token += cmd->token_mask + 1;
	cmd->free_head = context->next;
	/*spin_unlock(&cmd->context_lock);*/

	/*init_completion(&context->done);*/

	err = mlx4_cmd_post(priv, &ts1, in_param, out_param ? *out_param : 0,
			in_modifier, op_modifier, op, context->token, 1);
	if (err)
		goto out;

	MLX4_DEBUG("command %s (0x%x) \n", cmd_to_str(op), op);

	got_irq = false;
	current = rdtsc();
	sys_debug_get_tsc_per_ms(&ticks_per_msec);
	while (!got_irq) {
		if (rdtsc() >= current + timeout * ticks_per_msec) {
			stat_err = get_status(priv, &status, &go_bit, &t_bit);
			MLX4_DEBUG("command %s (0x%x) timed out: in_param=0x%llx, "
					"in_mod=0x%x, op_mod=0x%x, get_status err=%d, "
					"status_reg=0x%x, go_bit=%d, t_bit=%d, toggle=0x%x\n",
					cmd_to_str(op), op, (unsigned long long ) in_param,
					in_modifier, op_modifier, stat_err, status, go_bit, t_bit,
					priv->cmd.toggle);
			err = -EBUSY;
			goto out;
		}
		event_dispatch_non_block(get_default_waitset());
	}

	/*if (mlx4_debug_level & MLX4_DEBUG_MASK_CMD_TIME) {
	 ktime_get_ts(&ts2);
	 t1 = timespec_to_ktime(ts1);
	 t2 = timespec_to_ktime(ts2);
	 delta = ktime_sub(t2, t1);
	 ds = ktime_to_ns(delta);
	 pr_info("mlx4: fw exec time for %s is %lld nsec\n", cmd_to_str(op),
	 (long long) ds);
	 }*/

	err = context->result;
	if (err) {
		MLX4_DEBUG("command %s (0x%x) failed: in_param=0x%llx, "
				"in_mod=0x%x, op_mod=0x%x, fw status = 0x%x\n", cmd_to_str(op),
				op, (unsigned long long ) in_param, in_modifier, op_modifier,
				context->fw_status);

		switch (context->fw_status) {
		case CMD_STAT_BAD_PARAM:
			MLX4_DEBUG("Parameter is not supported, "
					"parameter is out of range\n");
			break;
		case CMD_STAT_EXCEED_LIM:
			MLX4_DEBUG("Required capability exceeded "
					"device limits\n");
			break;
		default:
			break;
		}
		goto out;
	}

	if (out_is_imm)
		*out_param = context->out_param;

	out: /*spin_lock(&cmd->context_lock);*/
	context->next = cmd->free_head;
	cmd->free_head = context - cmd->context;
	/*spin_unlock(&cmd->context_lock);*/

	/*up(&cmd->event_sem);*/
	return err;
}

int __mlx4_cmd(struct mlx4_dev *dev, uint64_t in_param, uint64_t *out_param,
		int out_is_imm, uint32_t in_modifier, uint32_t op_modifier, uint16_t op,
		unsigned long timeout, int native) {
	/*
	 if (pci_channel_offline(dev->pdev))
	 return -EIO;
	 */
	struct mlx4_priv *priv = mlx4_priv(dev);

	/*if (priv->cmd.use_events)
	 return mlx4_cmd_wait(priv, in_param, out_param, out_is_imm, in_modifier,
	 op_modifier, op, timeout);
	 else*/
	return mlx4_cmd_poll(priv, in_param, out_param, out_is_imm, in_modifier,
			op_modifier, op, timeout);
}
/*
 EXPORT_SYMBOL_GPL(__mlx4_cmd);


 static int mlx4_ARM_COMM_CHANNEL(struct mlx4_priv *priv)
 {
 return mlx4_cmd(dev, 0, 0, 0, MLX4_CMD_ARM_COMM_CHANNEL,
 MLX4_CMD_TIME_CLASS_B, MLX4_CMD_NATIVE);
 }

 static int mlx4_ACCESS_MEM(struct mlx4_priv *priv, u64 master_addr,
 int slave, u64 slave_addr,
 int size, int is_read)
 {
 u64 in_param;
 u64 out_param;

 if ((slave_addr & 0xfff) | (master_addr & 0xfff) |
 (slave & ~0x7f) | (size & 0xff)) {
 MLX4_DEBUG( "Bad access mem params - slave_addr:0x%llx "
 "master_addr:0x%llx slave_id:%d size:%d\n",
 (unsigned long long) slave_addr, (unsigned long long) master_addr, slave, size);
 return -EINVAL;
 }

 if (is_read) {
 in_param = (u64) slave | slave_addr;
 out_param = (u64) dev->caps.function | master_addr;
 } else {
 in_param = (u64) dev->caps.function | master_addr;
 out_param = (u64) slave | slave_addr;
 }

 return mlx4_cmd_imm(dev, in_param, &out_param, size, 0,
 MLX4_CMD_ACCESS_MEM,
 MLX4_CMD_TIME_CLASS_A, MLX4_CMD_NATIVE);
 }

 static int query_pkey_block(struct mlx4_priv *priv, u8 port, u16 index, u16 *pkey,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox)
 {
 struct ib_smp *in_mad = (struct ib_smp *)(inbox->buf);
 struct ib_smp *out_mad = (struct ib_smp *)(outbox->buf);
 int err;
 int i;

 if (index & 0x1f)
 return -EINVAL;

 in_mad->attr_mod = cpu_to_be32(index / 32);

 err = mlx4_cmd_box(dev, inbox->dma, outbox->dma, port, 3,
 MLX4_CMD_MAD_IFC, MLX4_CMD_TIME_CLASS_C,
 MLX4_CMD_NATIVE);
 if (err)
 return err;

 for (i = 0; i < 32; ++i)
 pkey[i] = be16_to_cpu(((__be16 *) out_mad->data)[i]);

 return err;
 }

 static int get_full_pkey_table(struct mlx4_priv *priv, u8 port, u16 *table,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox)
 {
 int i;
 int err;

 for (i = 0; i < dev->caps.pkey_table_len[port]; i += 32) {
 err = query_pkey_block(dev, port, i, table + i, inbox, outbox);
 if (err)
 return err;
 }

 return 0;
 }
 #define PORT_CAPABILITY_LOCATION_IN_SMP 20
 #define PORT_STATE_OFFSET 32

 static enum ib_port_state vf_port_state(struct mlx4_priv *priv, int port, int vf)
 {
 if (mlx4_get_slave_port_state(dev, vf, port) == SLAVE_PORT_UP)
 return IB_PORT_ACTIVE;
 else
 return IB_PORT_DOWN;
 }

 static int mlx4_MAD_IFC_wrapper(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd)
 {
 struct ib_smp *smp = inbox->buf;
 u32 index;
 u8 port;
 u16 *table;
 int err;
 int vidx, pidx;
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct ib_smp *outsmp = outbox->buf;
 __be16 *outtab = (__be16 *)(outsmp->data);
 __be32 slave_cap_mask;
 __be64 slave_node_guid;
 port = vhcr->in_modifier;

 if (smp->base_version == 1 &&
 smp->mgmt_class == IB_MGMT_CLASS_SUBN_LID_ROUTED &&
 smp->class_version == 1) {
 if (smp->method	== IB_MGMT_METHOD_GET) {
 if (smp->attr_id == IB_SMP_ATTR_PKEY_TABLE) {
 index = be32_to_cpu(smp->attr_mod);
 if (port < 1 || port > dev->caps.num_ports)
 return -EINVAL;
 table = kcalloc(dev->caps.pkey_table_len[port], sizeof *table, GFP_KERNEL);
 if (!table)
 return -ENOMEM;
 need to get the full pkey table because the paravirtualized
 * pkeys may be scattered among several pkey blocks.

 err = get_full_pkey_table(dev, port, table, inbox, outbox);
 if (!err) {
 for (vidx = index * 32; vidx < (index + 1) * 32; ++vidx) {
 pidx = priv->virt2phys_pkey[slave][port - 1][vidx];
 outtab[vidx % 32] = cpu_to_be16(table[pidx]);
 }
 }
 kfree(table);
 return err;
 }
 if (smp->attr_id == IB_SMP_ATTR_PORT_INFO) {
 get the slave specific caps:
 do the command
 err = mlx4_cmd_box(dev, inbox->dma, outbox->dma,
 vhcr->in_modifier, vhcr->op_modifier,
 vhcr->op, MLX4_CMD_TIME_CLASS_C, MLX4_CMD_NATIVE);
 modify the response for slaves
 if (!err && slave != mlx4_master_func_num(dev)) {
 u8 *state = outsmp->data + PORT_STATE_OFFSET;

 *state = (*state & 0xf0) | vf_port_state(dev, port, slave);
 slave_cap_mask = priv->mfunc.master.slave_state[slave].ib_cap_mask[port];
 memcpy(outsmp->data + PORT_CAPABILITY_LOCATION_IN_SMP, &slave_cap_mask, 4);
 }
 return err;
 }
 if (smp->attr_id == IB_SMP_ATTR_GUID_INFO) {
 compute slave's gid block
 smp->attr_mod = cpu_to_be32(slave / 8);
 execute cmd
 err = mlx4_cmd_box(dev, inbox->dma, outbox->dma,
 vhcr->in_modifier, vhcr->op_modifier,
 vhcr->op, MLX4_CMD_TIME_CLASS_C, MLX4_CMD_NATIVE);
 if (!err) {
 if needed, move slave gid to index 0
 if (slave % 8)
 memcpy(outsmp->data,
 outsmp->data + (slave % 8) * 8, 8);
 delete all other gids
 memset(outsmp->data + 8, 0, 56);
 }
 return err;
 }
 if (smp->attr_id == IB_SMP_ATTR_NODE_INFO) {
 err = mlx4_cmd_box(dev, inbox->dma, outbox->dma,
 vhcr->in_modifier, vhcr->op_modifier,
 vhcr->op, MLX4_CMD_TIME_CLASS_C, MLX4_CMD_NATIVE);
 if (!err) {
 slave_node_guid =  mlx4_get_slave_node_guid(dev, slave);
 memcpy(outsmp->data + 12, &slave_node_guid, 8);
 }
 return err;
 }
 }
 }
 if (slave != mlx4_master_func_num(dev) &&
 ((smp->mgmt_class == IB_MGMT_CLASS_SUBN_DIRECTED_ROUTE) ||
 (smp->mgmt_class == IB_MGMT_CLASS_SUBN_LID_ROUTED &&
 smp->method == IB_MGMT_METHOD_SET))) {
 MLX4_DEBUG( "slave %d is trying to execute a Subnet MGMT MAD, "
 "class 0x%x, method 0x%x for attr 0x%x. Rejecting\n",
 slave, smp->method, smp->mgmt_class,
 be16_to_cpu(smp->attr_id));
 return -EPERM;
 }
 default:
 return mlx4_cmd_box(dev, inbox->dma, outbox->dma,
 vhcr->in_modifier, vhcr->op_modifier,
 vhcr->op, MLX4_CMD_TIME_CLASS_C, MLX4_CMD_NATIVE);
 }

 static int MLX4_CMD_DIAG_RPRT_wrapper(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd)
 {
 return -EPERM;
 }

 static int MLX4_CMD_UPDATE_QP_wrapper(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd)
 {
 return -EPERM;
 }

 int mlx4_DMA_wrapper(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd)
 {
 u64 in_param;
 u64 out_param;
 int err;

 in_param = cmd->has_inbox ? (u64) inbox->dma : vhcr->in_param;
 out_param = cmd->has_outbox ? (u64) outbox->dma : vhcr->out_param;
 if (cmd->encode_slave_id) {
 in_param &= 0xffffffffffffff00ll;
 in_param |= slave;
 }

 err = __mlx4_cmd(dev, in_param, &out_param, cmd->out_is_imm,
 vhcr->in_modifier, vhcr->op_modifier, vhcr->op,
 MLX4_CMD_TIME_CLASS_A, MLX4_CMD_NATIVE);

 if (cmd->out_is_imm)
 vhcr->out_param = out_param;

 return err;
 }

 static struct mlx4_cmd_info cmd_info[] = {
 {
 .opcode = MLX4_CMD_QUERY_FW,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_FW_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_HCA,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_QUERY_DEV_CAP,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_DEV_CAP_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_FUNC_CAP,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_FUNC_CAP_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_ADAPTER,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_INIT_PORT,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_INIT_PORT_wrapper
 },
 {
 .opcode = MLX4_CMD_CLOSE_PORT,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm  = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_CLOSE_PORT_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_PORT,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_PORT_wrapper
 },
 {
 .opcode = MLX4_CMD_SET_PORT,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SET_PORT_wrapper
 },
 {
 .opcode = MLX4_CMD_MAP_EQ,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_MAP_EQ_wrapper
 },
 {
 .opcode = MLX4_CMD_SW2HW_EQ,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_SW2HW_EQ_wrapper
 },
 {
 .opcode = MLX4_CMD_HW_HEALTH_CHECK,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_DIAG_RPRT,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .skip_err_print = true,
 .verify = NULL,
 .wrapper = MLX4_CMD_DIAG_RPRT_wrapper
 },
 {
 .opcode = MLX4_CMD_NOP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_ALLOC_RES,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = true,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_ALLOC_RES_wrapper
 },
 {
 .opcode = MLX4_CMD_FREE_RES,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_FREE_RES_wrapper
 },
 {
 .opcode = MLX4_CMD_SW2HW_MPT,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_SW2HW_MPT_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_MPT,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_MPT_wrapper
 },
 {
 .opcode = MLX4_CMD_HW2SW_MPT,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_HW2SW_MPT_wrapper
 },
 {
 .opcode = MLX4_CMD_READ_MTT,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_WRITE_MTT,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_WRITE_MTT_wrapper
 },
 {
 .opcode = MLX4_CMD_SYNC_TPT,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_HW2SW_EQ,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_HW2SW_EQ_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_EQ,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_QUERY_EQ_wrapper
 },
 {
 .opcode = MLX4_CMD_SW2HW_CQ,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_SW2HW_CQ_wrapper
 },
 {
 .opcode = MLX4_CMD_HW2SW_CQ,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_HW2SW_CQ_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_CQ,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_CQ_wrapper
 },
 {
 .opcode = MLX4_CMD_MODIFY_CQ,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = true,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_MODIFY_CQ_wrapper
 },
 {
 .opcode = MLX4_CMD_SW2HW_SRQ,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_SW2HW_SRQ_wrapper
 },
 {
 .opcode = MLX4_CMD_HW2SW_SRQ,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_HW2SW_SRQ_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_SRQ,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_SRQ_wrapper
 },
 {
 .opcode = MLX4_CMD_ARM_SRQ,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_ARM_SRQ_wrapper
 },
 {
 .opcode = MLX4_CMD_RST2INIT_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = true,
 .verify = NULL,
 .wrapper = mlx4_RST2INIT_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_INIT2INIT_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_INIT2INIT_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_INIT2RTR_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_INIT2RTR_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_RTR2RTS_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_RTR2RTS_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_RTS2RTS_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_RTS2RTS_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_SQERR2RTS_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SQERR2RTS_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_2ERR_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_GEN_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_RTS2SQD_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_GEN_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_SQD2SQD_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SQD2SQD_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_SQD2RTS_QP,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SQD2RTS_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_2RST_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_2RST_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_QP,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_GEN_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_SUSPEND_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_GEN_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_UNSUSPEND_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_GEN_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_UPDATE_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .skip_err_print = true,
 .verify = NULL,
 .wrapper = MLX4_CMD_UPDATE_QP_wrapper
 },
 {
 .opcode = MLX4_CMD_CONF_SPECIAL_QP,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,  XXX verify: only demux can do this
 .wrapper = NULL
 },
 {
 .opcode = MLX4_CMD_MAD_IFC,
 .has_inbox = true,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_MAD_IFC_wrapper
 },
 {
 .opcode = MLX4_CMD_QUERY_IF_STAT,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QUERY_IF_STAT_wrapper
 },
 Native multicast commands are not available for guests
 {
 .opcode = MLX4_CMD_QP_ATTACH,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QP_ATTACH_wrapper
 },
 {
 .opcode = MLX4_CMD_PROMISC,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_PROMISC_wrapper
 },
 Ethernet specific commands
 {
 .opcode = MLX4_CMD_SET_VLAN_FLTR,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SET_VLAN_FLTR_wrapper
 },
 {
 .opcode = MLX4_CMD_SET_MCAST_FLTR,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_SET_MCAST_FLTR_wrapper
 },
 {
 .opcode = MLX4_CMD_DUMP_ETH_STATS,
 .has_inbox = false,
 .has_outbox = true,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_DUMP_ETH_STATS_wrapper
 },
 {
 .opcode = MLX4_CMD_INFORM_FLR_DONE,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = NULL
 },
 flow steering commands
 {
 .opcode = MLX4_QP_FLOW_STEERING_ATTACH,
 .has_inbox = true,
 .has_outbox = false,
 .out_is_imm = true,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QP_FLOW_STEERING_ATTACH_wrapper
 },
 {
 .opcode = MLX4_QP_FLOW_STEERING_DETACH,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .verify = NULL,
 .wrapper = mlx4_QP_FLOW_STEERING_DETACH_wrapper
 },
 wol commands
 {
 .opcode = MLX4_CMD_MOD_STAT_CFG,
 .has_inbox = false,
 .has_outbox = false,
 .out_is_imm = false,
 .encode_slave_id = false,
 .skip_err_print = true,
 .verify = NULL,
 .wrapper = mlx4_MOD_STAT_CFG_wrapper
 },
 };

 static int mlx4_master_process_vhcr(struct mlx4_priv *priv, int slave,
 struct mlx4_vhcr_cmd *in_vhcr)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_cmd_info *cmd = NULL;
 struct mlx4_vhcr_cmd *vhcr_cmd = in_vhcr ? in_vhcr : priv->mfunc.vhcr;
 struct mlx4_vhcr *vhcr;
 struct mlx4_cmd_mailbox *inbox = NULL;
 struct mlx4_cmd_mailbox *outbox = NULL;
 u64 in_param;
 u64 out_param;
 int ret = 0;
 int i;
 int err = 0;

 Create sw representation of Virtual HCR
 vhcr = kzalloc(sizeof(struct mlx4_vhcr), GFP_KERNEL);
 if (!vhcr)
 return -ENOMEM;

 DMA in the vHCR
 if (!in_vhcr) {
 ret = mlx4_ACCESS_MEM(dev, priv->mfunc.vhcr_dma, slave,
 priv->mfunc.master.slave_state[slave].vhcr_dma,
 ALIGN(sizeof(struct mlx4_vhcr_cmd),
 MLX4_ACCESS_MEM_ALIGN), 1);
 if (ret) {
 MLX4_DEBUG( "%s:Failed reading vhcr"
 "ret: 0x%x\n", __func__, ret);
 kfree(vhcr);
 return ret;
 }
 }

 Fill SW VHCR fields
 vhcr->in_param = be64_to_cpu(vhcr_cmd->in_param);
 vhcr->out_param = be64_to_cpu(vhcr_cmd->out_param);
 vhcr->in_modifier = be32_to_cpu(vhcr_cmd->in_modifier);
 vhcr->token = be16_to_cpu(vhcr_cmd->token);
 vhcr->op = be16_to_cpu(vhcr_cmd->opcode) & 0xfff;
 vhcr->op_modifier = (u8) (be16_to_cpu(vhcr_cmd->opcode) >> 12);
 vhcr->e_bit = vhcr_cmd->flags & (1 << 6);

 Lookup command
 for (i = 0; i < ARRAY_SIZE(cmd_info); ++i) {
 if (vhcr->op == cmd_info[i].opcode) {
 cmd = &cmd_info[i];
 break;
 }
 }
 if (!cmd) {
 MLX4_DEBUG( "unparavirt command: %s (0x%x) accepted from slave:%d\n",
 cmd_to_str(vhcr->op), vhcr->op, slave);
 vhcr_cmd->status = CMD_STAT_BAD_PARAM;
 goto out_status;
 }

 Read inbox
 if (cmd->has_inbox) {
 vhcr->in_param &= INBOX_MASK;
 inbox = mlx4_alloc_cmd_mailbox(dev);
 if (IS_ERR(inbox)) {
 vhcr_cmd->status = CMD_STAT_BAD_SIZE;
 inbox = NULL;
 goto out_status;
 }

 if (mlx4_ACCESS_MEM(dev, inbox->dma, slave,
 vhcr->in_param,
 MLX4_MAILBOX_SIZE, 1)) {
 MLX4_DEBUG( "%s: Failed reading inbox for cmd %s (0x%x)\n",
 __func__, cmd_to_str(cmd->opcode), cmd->opcode);
 vhcr_cmd->status = CMD_STAT_INTERNAL_ERR;
 goto out_status;
 }
 }

 Apply permission and bound checks if applicable
 if (cmd->verify && cmd->verify(dev, slave, vhcr, inbox)) {
 MLX4_DEBUG( "Command %s (0x%x) from slave: %d failed protection "
 "checks for resource_id: %d\n", cmd_to_str(vhcr->op),
 vhcr->op, slave, vhcr->in_modifier);
 vhcr_cmd->status = CMD_STAT_BAD_OP;
 goto out_status;
 }

 Allocate outbox
 if (cmd->has_outbox) {
 outbox = mlx4_alloc_cmd_mailbox(dev);
 if (IS_ERR(outbox)) {
 vhcr_cmd->status = CMD_STAT_BAD_SIZE;
 outbox = NULL;
 goto out_status;
 }
 }

 Execute the command!
 if (cmd->wrapper) {
 err = cmd->wrapper(dev, slave, vhcr, inbox, outbox,
 cmd);
 if (cmd->out_is_imm)
 vhcr_cmd->out_param = cpu_to_be64(vhcr->out_param);
 } else {
 in_param = cmd->has_inbox ? (u64) inbox->dma :
 vhcr->in_param;
 out_param = cmd->has_outbox ? (u64) outbox->dma :
 vhcr->out_param;
 err = __mlx4_cmd(dev, in_param, &out_param,
 cmd->out_is_imm, vhcr->in_modifier,
 vhcr->op_modifier, vhcr->op,
 MLX4_CMD_TIME_CLASS_A,
 MLX4_CMD_NATIVE);

 if (cmd->out_is_imm) {
 vhcr->out_param = out_param;
 vhcr_cmd->out_param = cpu_to_be64(vhcr->out_param);
 }
 }

 if (err) {
 if (!cmd->skip_err_print)
 MLX4_DEBUG( "vhcr command %s (0x%x) slave:%d "
 "in_param 0x%llx in_mod=0x%x, op_mod=0x%x "
 "failed with error:%d, status %d\n",
 cmd_to_str(vhcr->op), vhcr->op, slave,
 (unsigned long long) vhcr->in_param, vhcr->in_modifier,
 vhcr->op_modifier, vhcr->errno, err);
 vhcr_cmd->status = mlx4_errno_to_status(err);
 goto out_status;
 }


 Write outbox if command completed successfully
 if (cmd->has_outbox && !vhcr_cmd->status) {
 ret = mlx4_ACCESS_MEM(dev, outbox->dma, slave,
 vhcr->out_param,
 MLX4_MAILBOX_SIZE, MLX4_CMD_WRAPPED);
 if (ret) {
 If we failed to write back the outbox after the
 *command was successfully executed, we must fail this
 * slave, as it is now in undefined state
 MLX4_DEBUG( "%s: Failed writing outbox\n", __func__);
 goto out;
 }
 }

 out_status:
 DMA back vhcr result
 if (!in_vhcr) {
 ret = mlx4_ACCESS_MEM(dev, priv->mfunc.vhcr_dma, slave,
 priv->mfunc.master.slave_state[slave].vhcr_dma,
 ALIGN(sizeof(struct mlx4_vhcr),
 MLX4_ACCESS_MEM_ALIGN),
 MLX4_CMD_WRAPPED);
 if (ret)
 MLX4_DEBUG( "%s:Failed writing vhcr result\n",
 __func__);
 else if (vhcr->e_bit &&
 mlx4_GEN_EQE(dev, slave, &priv->mfunc.master.cmd_eqe))
 MLX4_DEBUG( "Failed to generate command completion "
 "eqe for slave %d\n", slave);
 }

 out:
 kfree(vhcr);
 mlx4_free_cmd_mailbox(dev, inbox);
 mlx4_free_cmd_mailbox(dev, outbox);
 return ret;
 }

 static int mlx4_master_immediate_activate_vlan_qos(struct mlx4_priv *priv,
 int slave, int port)
 {
 struct mlx4_vport_oper_state *vp_oper;
 struct mlx4_vport_state *vp_admin;
 struct mlx4_vf_immed_vlan_work *work;
 int err;
 int admin_vlan_ix = NO_INDX;

 vp_oper = &priv->mfunc.master.vf_oper[slave].vport[port];
 vp_admin = &priv->mfunc.master.vf_admin[slave].vport[port];

 if (vp_oper->state.default_vlan == vp_admin->default_vlan &&
 vp_oper->state.default_qos == vp_admin->default_qos)
 return 0;

 work = kzalloc(sizeof(*work), GFP_KERNEL);
 if (!work)
 return -ENOMEM;

 if (vp_oper->state.default_vlan != vp_admin->default_vlan) {
 if (MLX4_VGT != vp_admin->default_vlan) {
 err = __mlx4_register_vlan(&priv->dev, port,
 vp_admin->default_vlan,
 &admin_vlan_ix);
 if (err) {
 mlx4_warn((&priv->dev),
 "No vlan resources slave %d, port %d\n",
 slave, port);
 kfree(work);
 return err;
 }
 } else {
 admin_vlan_ix = NO_INDX;
 }
 work->flags |= MLX4_VF_IMMED_VLAN_FLAG_VLAN;
 mlx4_dbg((&(priv->dev)),
 "alloc vlan %d idx  %d slave %d port %d\n",
 (int)(vp_admin->default_vlan),
 admin_vlan_ix, slave, port);
 }

 save original vlan ix and vlan id
 work->orig_vlan_id = vp_oper->state.default_vlan;
 work->orig_vlan_ix = vp_oper->vlan_idx;

 handle new qos
 if (vp_oper->state.default_qos != vp_admin->default_qos)
 work->flags |= MLX4_VF_IMMED_VLAN_FLAG_QOS;

 if (work->flags & MLX4_VF_IMMED_VLAN_FLAG_VLAN)
 vp_oper->vlan_idx = admin_vlan_ix;

 vp_oper->state.default_vlan = vp_admin->default_vlan;
 vp_oper->state.default_qos = vp_admin->default_qos;

 iterate over QPs owned by this slave, using UPDATE_QP
 work->port = port;
 work->slave = slave;
 work->qos = vp_oper->state.default_qos;
 work->vlan_id = vp_oper->state.default_vlan;
 work->vlan_ix = vp_oper->vlan_idx;
 work->priv = priv;
 INIT_WORK(&work->work, mlx4_vf_immed_vlan_work_handler);
 queue_work(priv->mfunc.master.comm_wq, &work->work);

 return 0;
 }


 static int mlx4_master_activate_admin_state(struct mlx4_priv *priv, int slave)
 {
 int port, err;
 struct mlx4_vport_state *vp_admin;
 struct mlx4_vport_oper_state *vp_oper;

 for (port = 1; port <= MLX4_MAX_PORTS; port++) {
 vp_oper = &priv->mfunc.master.vf_oper[slave].vport[port];
 vp_admin = &priv->mfunc.master.vf_admin[slave].vport[port];
 vp_oper->state = *vp_admin;
 if (MLX4_VGT != vp_admin->default_vlan) {
 err = __mlx4_register_vlan(&priv->dev, port,
 vp_admin->default_vlan, &(vp_oper->vlan_idx));
 if (err) {
 vp_oper->vlan_idx = NO_INDX;
 mlx4_warn((&priv->dev),
 "No vlan resorces slave %d, port %d\n",
 slave, port);
 return err;
 }
 mlx4_dbg((&(priv->dev)), "alloc vlan %d idx  %d slave %d port %d\n",
 (int)(vp_oper->state.default_vlan),
 vp_oper->vlan_idx, slave, port);
 }
 if (vp_admin->spoofchk) {
 vp_oper->mac_idx = __mlx4_register_mac(&priv->dev,
 port,
 vp_admin->mac);
 if (0 > vp_oper->mac_idx) {
 err = vp_oper->mac_idx;
 vp_oper->mac_idx = NO_INDX;
 mlx4_warn((&priv->dev),
 "No mac resources slave %d, port %d\n",
 slave, port);
 return err;
 }
 mlx4_dbg((&(priv->dev)), "alloc mac %llx idx  %d slave %d port %d\n",
 (unsigned long long) vp_oper->state.mac, vp_oper->mac_idx, slave, port);
 }
 }
 return 0;
 }

 static void mlx4_master_deactivate_admin_state(struct mlx4_priv *priv, int slave)
 {
 int port;
 struct mlx4_vport_oper_state *vp_oper;

 for (port = 1; port <= MLX4_MAX_PORTS; port++) {
 vp_oper = &priv->mfunc.master.vf_oper[slave].vport[port];
 if (NO_INDX != vp_oper->vlan_idx) {
 __mlx4_unregister_vlan(&priv->dev,
 port, vp_oper->state.default_vlan);
 vp_oper->vlan_idx = NO_INDX;
 }
 if (NO_INDX != vp_oper->mac_idx) {
 __mlx4_unregister_mac(&priv->dev, port, vp_oper->state.mac);
 vp_oper->mac_idx = NO_INDX;
 }
 }
 return;
 }

 static void mlx4_master_do_cmd(struct mlx4_priv *priv, int slave, u8 cmd,
 u16 param, u8 toggle)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_slave_state *slave_state = priv->mfunc.master.slave_state;
 u32 reply;
 u8 is_going_down = 0;
 int i;
 unsigned long flags;

 slave_state[slave].comm_toggle ^= 1;
 reply = (u32) slave_state[slave].comm_toggle << 31;
 if (toggle != slave_state[slave].comm_toggle) {
 MLX4_DEBUG( "Incorrect toggle %d from slave %d. *** MASTER"
 "STATE COMPROMISIED ***\n", toggle, slave);
 goto reset_slave;
 }
 if (cmd == MLX4_COMM_CMD_RESET) {
 MLX4_DEBUG( "Received reset from slave:%d\n", slave);
 slave_state[slave].active = false;
 slave_state[slave].old_vlan_api = false;
 mlx4_master_deactivate_admin_state(priv, slave);
 for (i = 0; i < MLX4_EVENT_TYPES_NUM; ++i) {
 slave_state[slave].event_eq[i].eqn = -1;
 slave_state[slave].event_eq[i].token = 0;
 }
 check if we are in the middle of FLR process,
 if so return "retry" status to the slave
 if (MLX4_COMM_CMD_FLR == slave_state[slave].last_cmd)
 goto inform_slave_state;

 mlx4_dispatch_event(dev, MLX4_DEV_EVENT_SLAVE_SHUTDOWN, slave);

 write the version in the event field
 reply |= mlx4_comm_get_version();

 goto reset_slave;
 }
 command from slave in the middle of FLR
 if (cmd != MLX4_COMM_CMD_RESET &&
 MLX4_COMM_CMD_FLR == slave_state[slave].last_cmd) {
 MLX4_DEBUG( "slave:%d is Trying to run cmd (0x%x) "
 "in the middle of FLR\n", slave, cmd);
 return;
 }

 switch (cmd) {
 case MLX4_COMM_CMD_VHCR0:
 if (slave_state[slave].last_cmd != MLX4_COMM_CMD_RESET)
 goto reset_slave;
 slave_state[slave].vhcr_dma = ((u64) param) << 48;
 priv->mfunc.master.slave_state[slave].cookie = 0;
 break;
 case MLX4_COMM_CMD_VHCR1:
 if (slave_state[slave].last_cmd != MLX4_COMM_CMD_VHCR0)
 goto reset_slave;
 slave_state[slave].vhcr_dma |= ((u64) param) << 32;
 break;
 case MLX4_COMM_CMD_VHCR2:
 if (slave_state[slave].last_cmd != MLX4_COMM_CMD_VHCR1)
 goto reset_slave;
 slave_state[slave].vhcr_dma |= ((u64) param) << 16;
 break;
 case MLX4_COMM_CMD_VHCR_EN:
 if (slave_state[slave].last_cmd != MLX4_COMM_CMD_VHCR2)
 goto reset_slave;
 slave_state[slave].vhcr_dma |= param;
 if (mlx4_master_activate_admin_state(priv, slave))
 goto reset_slave;
 slave_state[slave].active = true;
 mlx4_dispatch_event(dev, MLX4_DEV_EVENT_SLAVE_INIT, slave);
 break;
 case MLX4_COMM_CMD_VHCR_POST:
 if ((slave_state[slave].last_cmd != MLX4_COMM_CMD_VHCR_EN) &&
 (slave_state[slave].last_cmd != MLX4_COMM_CMD_VHCR_POST))
 goto reset_slave;

 mutex_lock(&priv->cmd.slave_cmd_mutex);
 if (mlx4_master_process_vhcr(dev, slave, NULL)) {
 MLX4_DEBUG( "Failed processing vhcr for slave: %d,"
 " resetting slave.\n", slave);
 mutex_unlock(&priv->cmd.slave_cmd_mutex);
 goto reset_slave;
 }
 mutex_unlock(&priv->cmd.slave_cmd_mutex);
 break;
 default:
 MLX4_DEBUG( "Bad comm cmd: %d from slave: %d\n", cmd, slave);
 goto reset_slave;
 }
 spin_lock_irqsave(&priv->mfunc.master.slave_state_lock, flags);
 if (!slave_state[slave].is_slave_going_down)
 slave_state[slave].last_cmd = cmd;
 else
 is_going_down = 1;
 spin_unlock_irqrestore(&priv->mfunc.master.slave_state_lock, flags);
 if (is_going_down) {
 MLX4_DEBUG( "Slave is going down aborting command (%d)"
 " executing from slave: %d\n",
 cmd, slave);
 return;
 }
 __raw_writel((__force u32) cpu_to_be32(reply),
 &priv->mfunc.comm[slave].slave_read);
 mmiowb();

 return;

 reset_slave:
 cleanup any slave resources
 mlx4_delete_all_resources_for_slave(dev, slave);
 spin_lock_irqsave(&priv->mfunc.master.slave_state_lock, flags);
 if (!slave_state[slave].is_slave_going_down)
 slave_state[slave].last_cmd = MLX4_COMM_CMD_RESET;
 spin_unlock_irqrestore(&priv->mfunc.master.slave_state_lock, flags);
 with slave in the middle of flr, no need to clean resources again.
 inform_slave_state:
 __raw_writel((__force u32) cpu_to_be32(reply),
 &priv->mfunc.comm[slave].slave_read);
 wmb();
 }

 master command processing
 void mlx4_master_comm_channel(struct work_struct *work)
 {
 struct mlx4_mfunc_master_ctx *master =
 container_of(work,
 struct mlx4_mfunc_master_ctx,
 comm_work);
 struct mlx4_mfunc *mfunc =
 container_of(master, struct mlx4_mfunc, master);
 struct mlx4_priv *priv =
 container_of(mfunc, struct mlx4_priv, mfunc);
 struct mlx4_priv *priv = &priv->dev;
 __be32 *bit_vec;
 u32 comm_cmd;
 u32 vec;
 int i, j, slave;
 int toggle;
 int served = 0;
 int reported = 0;
 u32 slt;

 bit_vec = master->comm_arm_bit_vector;
 for (i = 0; i < COMM_CHANNEL_BIT_ARRAY_SIZE; i++) {
 vec = be32_to_cpu(bit_vec[i]);
 for (j = 0; j < 32; j++) {
 if (!(vec & (1 << j)))
 continue;
 ++reported;
 slave = (i * 32) + j;
 comm_cmd = swab32(readl(
 &mfunc->comm[slave].slave_write));
 slt = swab32(readl(&mfunc->comm[slave].slave_read))
 >> 31;
 toggle = comm_cmd >> 31;
 if (toggle != slt) {
 if (master->slave_state[slave].comm_toggle
 != slt) {
 MLX4_DEBUG( "slave %d out of sync."
 " read toggle %d, state toggle %d. "
 "Resynching.\n", slave, slt,
 master->slave_state[slave].comm_toggle);
 master->slave_state[slave].comm_toggle =
 slt;
 }
 mlx4_master_do_cmd(dev, slave,
 comm_cmd >> 16 & 0xff,
 comm_cmd & 0xffff, toggle);
 ++served;
 } else
 MLX4_DEBUG( "slave %d out of sync."
 " read toggle %d, write toggle %d.\n", slave, slt,
 toggle);
 }
 }

 if (reported && reported != served)
 MLX4_DEBUG( "Got command event with bitmask from %d slaves"
 " but %d were served\n",
 reported, served);
 }
 master command processing
 void mlx4_master_arm_comm_channel(struct work_struct *work)
 {
 struct mlx4_mfunc_master_ctx *master =
 container_of(work,
 struct mlx4_mfunc_master_ctx,
 arm_comm_work);
 struct mlx4_mfunc *mfunc =
 container_of(master, struct mlx4_mfunc, master);
 struct mlx4_priv *priv =
 container_of(mfunc, struct mlx4_priv, mfunc);
 struct mlx4_priv *priv = &priv->dev;

 if (mlx4_ARM_COMM_CHANNEL(dev))
 MLX4_DEBUG( "Failed to arm comm channel events\n");
 }

 static int sync_toggles(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 int wr_toggle;
 int rd_toggle;
 unsigned long end;

 wr_toggle = swab32(readl(&priv->mfunc.comm->slave_write)) >> 31;
 end = jiffies + msecs_to_jiffies(5000);

 while (time_before(jiffies, end)) {
 rd_toggle = swab32(readl(&priv->mfunc.comm->slave_read)) >> 31;
 if (rd_toggle == wr_toggle) {
 priv->cmd.comm_toggle = rd_toggle;
 return 0;
 }

 cond_resched();
 }


 * we could reach here if for example the previous VM using this
 * function misbehaved and left the channel with unsynced state. We
 * should fix this here and give this VM a chance to use a properly
 * synced channel

 MLX4_DEBUG( "recovering from previously mis-behaved VM\n");
 __raw_writel((__force u32) 0, &priv->mfunc.comm->slave_read);
 __raw_writel((__force u32) 0, &priv->mfunc.comm->slave_write);
 priv->cmd.comm_toggle = 0;

 return 0;
 }

 int mlx4_multi_func_init(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_slave_state *s_state;
 int i, j, err, port;

 if (mlx4_is_master(dev))
 priv->mfunc.comm =
 ioremap(pci_resource_start(dev->pdev, priv->fw.comm_bar) +
 priv->fw.comm_base, MLX4_COMM_PAGESIZE);
 else
 priv->mfunc.comm =
 ioremap(pci_resource_start(dev->pdev, 2) +
 MLX4_SLAVE_COMM_BASE, MLX4_COMM_PAGESIZE);
 if (!priv->mfunc.comm) {
 MLX4_DEBUG( "Couldn't map communication vector.\n");
 goto err_vhcr;
 }

 if (mlx4_is_master(dev)) {
 priv->mfunc.master.slave_state =
 kzalloc(dev->num_slaves *
 sizeof(struct mlx4_slave_state), GFP_KERNEL);
 if (!priv->mfunc.master.slave_state)
 goto err_comm;

 priv->mfunc.master.vf_admin =
 kzalloc(dev->num_slaves *
 sizeof(struct mlx4_vf_admin_state), GFP_KERNEL);
 if (!priv->mfunc.master.vf_admin)
 goto err_comm_admin;

 priv->mfunc.master.vf_oper =
 kzalloc(dev->num_slaves *
 sizeof(struct mlx4_vf_oper_state), GFP_KERNEL);
 if (!priv->mfunc.master.vf_oper)
 goto err_comm_oper;

 for (i = 0; i < dev->num_slaves; ++i) {
 s_state = &priv->mfunc.master.slave_state[i];
 s_state->last_cmd = MLX4_COMM_CMD_RESET;
 mutex_init(&priv->mfunc.master.gen_eqe_mutex[i]);
 for (j = 0; j < MLX4_EVENT_TYPES_NUM; ++j)
 s_state->event_eq[j].eqn = -1;
 __raw_writel((__force u32) 0,
 &priv->mfunc.comm[i].slave_write);
 __raw_writel((__force u32) 0,
 &priv->mfunc.comm[i].slave_read);
 mmiowb();
 for (port = 1; port <= MLX4_MAX_PORTS; port++) {
 s_state->vlan_filter[port] =
 kzalloc(sizeof(struct mlx4_vlan_fltr),
 GFP_KERNEL);
 if (!s_state->vlan_filter[port]) {
 if (--port)
 kfree(s_state->vlan_filter[port]);
 goto err_slaves;
 }
 INIT_LIST_HEAD(&s_state->mcast_filters[port]);
 priv->mfunc.master.vf_admin[i].vport[port].default_vlan = MLX4_VGT;
 priv->mfunc.master.vf_oper[i].vport[port].state.default_vlan = MLX4_VGT;
 priv->mfunc.master.vf_oper[i].vport[port].vlan_idx = NO_INDX;
 priv->mfunc.master.vf_oper[i].vport[port].mac_idx = NO_INDX;
 }
 spin_lock_init(&s_state->lock);
 }

 memset(&priv->mfunc.master.cmd_eqe, 0, dev->caps.eqe_size);
 priv->mfunc.master.cmd_eqe.type = MLX4_EVENT_TYPE_CMD;
 INIT_WORK(&priv->mfunc.master.comm_work,
 mlx4_master_comm_channel);
 INIT_WORK(&priv->mfunc.master.arm_comm_work,
 mlx4_master_arm_comm_channel);
 INIT_WORK(&priv->mfunc.master.slave_event_work,
 mlx4_gen_slave_eqe);
 INIT_WORK(&priv->mfunc.master.slave_flr_event_work,
 mlx4_master_handle_slave_flr);
 spin_lock_init(&priv->mfunc.master.slave_state_lock);
 spin_lock_init(&priv->mfunc.master.slave_eq.event_lock);
 priv->mfunc.master.comm_wq =
 create_singlethread_workqueue("mlx4_comm");
 if (!priv->mfunc.master.comm_wq)
 goto err_slaves;

 if (mlx4_init_resource_tracker(dev))
 goto err_thread;

 err = mlx4_ARM_COMM_CHANNEL(dev);
 if (err) {
 MLX4_DEBUG( " Failed to arm comm channel eq: %x\n",
 err);
 goto err_resource;
 }

 } else {
 err = sync_toggles(dev);
 if (err) {
 MLX4_DEBUG( "Couldn't sync toggles\n");
 goto err_comm;
 }
 }
 return 0;

 err_resource:
 mlx4_free_resource_tracker(dev, RES_TR_FREE_ALL);
 err_thread:
 flush_workqueue(priv->mfunc.master.comm_wq);
 destroy_workqueue(priv->mfunc.master.comm_wq);
 err_slaves:
 while (--i) {
 for (port = 1; port <= MLX4_MAX_PORTS; port++)
 kfree(priv->mfunc.master.slave_state[i].vlan_filter[port]);
 }
 kfree(priv->mfunc.master.vf_oper);
 err_comm_oper:
 kfree(priv->mfunc.master.vf_admin);
 err_comm_admin:
 kfree(priv->mfunc.master.slave_state);
 err_comm:
 iounmap(priv->mfunc.comm);
 err_vhcr:
 dma_free_coherent(&(dev->pdev->dev), PAGE_SIZE,
 priv->mfunc.vhcr,
 priv->mfunc.vhcr_dma);
 priv->mfunc.vhcr = NULL;
 return -ENOMEM;
 }
 */
int mlx4_cmd_init(struct mlx4_priv *priv) {

	/*mutex_init(&priv->cmd.hcr_mutex);
	 mutex_init(&priv->cmd.slave_cmd_mutex);
	 sema_init(&priv->cmd.poll_sem, 1);*/
	priv->cmd.use_events = 0;
	priv->cmd.toggle = 1;

	/*priv->mfunc.vhcr = NULL;*/

	/*if (!mlx4_is_slave(dev)) {*/

	priv->cmd.hcr = priv->dev.bar_info->vaddr + MLX4_HCR_BASE;
	/*priv->cmd.hcr = ioremap(pci_resource_start(dev->pdev, 0) + MLX4_HCR_BASE,
	 MLX4_HCR_SIZE);
	 if (!priv->cmd.hcr) {
	 MLX4_DEBUG( "Couldn't map command register.\n");
	 return -ENOMEM;
	 }*/
	/*}*/

	/*if (mlx4_is_mfunc(dev)) {
	 priv->mfunc.vhcr = dma_alloc_coherent(&(dev->pdev->dev), PAGE_SIZE,
	 &priv->mfunc.vhcr_dma, GFP_KERNEL);
	 if (!priv->mfunc.vhcr) {
	 MLX4_DEBUG( "Couldn't allocate VHCR.\n");
	 goto err_hcr;
	 }
	 }*/

	/*priv->cmd.pool = pci_pool_create("mlx4_cmd", dev->pdev, MLX4_MAILBOX_SIZE,
	 MLX4_MAILBOX_SIZE, 0);
	 if (!priv->cmd.pool)
	 goto err_vhcr;*/

	return 0;

	/*err_vhcr: if (mlx4_is_mfunc(dev))
	 dma_free_coherent(&(dev->pdev->dev), PAGE_SIZE, priv->mfunc.vhcr,
	 priv->mfunc.vhcr_dma);
	 priv->mfunc.vhcr = NULL;

	 err_hcr: if (!mlx4_is_slave(dev))
	 iounmap(priv->cmd.hcr);
	 return -ENOMEM;*/
}
/*
 void mlx4_multi_func_cleanup(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 int i, port;

 if (mlx4_is_master(dev)) {
 flush_workqueue(priv->mfunc.master.comm_wq);
 destroy_workqueue(priv->mfunc.master.comm_wq);
 for (i = 0; i < dev->num_slaves; i++) {
 for (port = 1; port <= MLX4_MAX_PORTS; port++)
 kfree(priv->mfunc.master.slave_state[i].vlan_filter[port]);
 }
 kfree(priv->mfunc.master.slave_state);
 kfree(priv->mfunc.master.vf_admin);
 kfree(priv->mfunc.master.vf_oper);
 }

 iounmap(priv->mfunc.comm);
 }

 void mlx4_cmd_cleanup(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);

 pci_pool_destroy(priv->cmd.pool);

 if (!mlx4_is_slave(dev))
 iounmap(priv->cmd.hcr);
 if (mlx4_is_mfunc(dev))
 dma_free_coherent(&(dev->pdev->dev), PAGE_SIZE,
 priv->mfunc.vhcr, priv->mfunc.vhcr_dma);
 priv->mfunc.vhcr = NULL;
 }


 * Switch to using events to issue FW commands (can only be called
 * after event queue for command events has been initialized).
 */
int mlx4_cmd_use_events(struct mlx4_priv *priv) {
	int i;
	int err = 0;

	priv->cmd.context = malloc(
			priv->cmd.max_cmds * sizeof(struct mlx4_cmd_context));
	if (!priv->cmd.context)
		return -ENOMEM;

	for (i = 0; i < priv->cmd.max_cmds; ++i) {
		priv->cmd.context[i].token = i;
		priv->cmd.context[i].next = i + 1;
	}

	priv->cmd.context[priv->cmd.max_cmds - 1].next = -1;
	priv->cmd.free_head = 0;

	/*sema_init(&priv->cmd.event_sem, priv->cmd.max_cmds);
	 spin_lock_init(&priv->cmd.context_lock);*/

	for (priv->cmd.token_mask = 1; priv->cmd.token_mask < priv->cmd.max_cmds;
			priv->cmd.token_mask <<= 1)
		;/*nothing*/
	--priv->cmd.token_mask;

	/*down(&priv->cmd.poll_sem);*/
	priv->cmd.use_events = 1;

	return err;
}

/*
 * Switch back to polling (used when shutting down the device)

 void mlx4_cmd_use_polling(struct mlx4_priv *priv)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 int i;

 priv->cmd.use_events = 0;

 for (i = 0; i < priv->cmd.max_cmds; ++i)
 down(&priv->cmd.event_sem);

 kfree(priv->cmd.context);

 up(&priv->cmd.poll_sem);
 }
 */
struct mlx4_cmd_mailbox *mlx4_alloc_cmd_mailbox(void) {
	errval_t err;
	struct mlx4_cmd_mailbox *mailbox;
	struct frame_identity fid;

	mailbox = calloc(1, sizeof *mailbox);

	err = frame_alloc(&mailbox->slot, MLX4_MAILBOX_SIZE, NULL);
	assert(err_is_ok(err));

	err = invoke_frame_identify(mailbox->slot, &fid);
	assert(err_is_ok(err));

	mailbox->dma = fid.base; //this is the physical address

	err = vspace_map_one_frame_attr(&mailbox->buf, MLX4_MAILBOX_SIZE,
			mailbox->slot,
			VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
			NULL);
	assert(err_is_ok(err));

	memset(mailbox->buf, 0, MLX4_MAILBOX_SIZE);

	return mailbox;
}
/*
 EXPORT_SYMBOL_GPL(mlx4_alloc_cmd_mailbox);
 */
void mlx4_free_cmd_mailbox(struct mlx4_cmd_mailbox *mailbox) {
	errval_t cleanup_err;

	/*int i;
	 uint32_t *data = (uint32_t *) mailbox->buf;*/

	if (!mailbox)
		return;

	/*for (i = 0; i < MLX4_MAILBOX_SIZE / 4; ++i) {
	 printf("mailbox: 0x%x\n", data[i]);
	 }*/

	cleanup_err = vspace_unmap(mailbox->buf);
	if (err_is_fail(cleanup_err)) {
		USER_PANIC_ERR(cleanup_err, "vspace_unmap failed");
	}

	cleanup_err = cap_destroy(mailbox->slot);
	if (err_is_fail(cleanup_err)) {
		USER_PANIC_ERR(cleanup_err, "cap_destroy failed");
	}

	/*pci_pool_free(mlx4_priv(dev)->cmd.pool, mailbox->buf, mailbox->dma);
	 kfree(mailbox);*/
}
/*
 EXPORT_SYMBOL_GPL(mlx4_free_cmd_mailbox);

 u32 mlx4_comm_get_version(void)
 {
 return ((u32) CMD_CHAN_IF_REV << 8) | (u32) CMD_CHAN_VER;
 }

 static int mlx4_get_slave_indx(struct mlx4_priv *priv, int vf)
 {
 if ((vf < 0) || (vf >= dev->num_vfs)) {
 MLX4_DEBUG( "Bad vf number:%d (number of activated vf: %d)\n", vf, dev->num_vfs);
 return -EINVAL;
 }
 return (vf+1);
 }

 int mlx4_set_vf_mac(struct mlx4_priv *priv, int port, int vf, u8 *mac)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vport_state *s_info;
 int slave;

 if (!mlx4_is_master(dev))
 return -EPROTONOSUPPORT;

 slave = mlx4_get_slave_indx(dev, vf);
 if (slave < 0)
 return -EINVAL;

 s_info = &priv->mfunc.master.vf_admin[slave].vport[port];
 s_info->mac = mlx4_mac_to_u64(mac);
 MLX4_DEBUG( "default mac on vf %d port %d to %llX will take afect only after vf restart\n",
 vf, port, (unsigned long long) s_info->mac);
 return 0;
 }
 EXPORT_SYMBOL_GPL(mlx4_set_vf_mac);

 int mlx4_set_vf_vlan(struct mlx4_priv *priv, int port, int vf, u16 vlan, u8 qos)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vport_oper_state *vf_oper;
 struct mlx4_vport_state *vf_admin;
 int slave;

 if ((!mlx4_is_master(dev)) ||
 !(dev->caps.flags2 & MLX4_DEV_CAP_FLAG2_VLAN_CONTROL))
 return -EPROTONOSUPPORT;

 if ((vlan > 4095) || (qos > 7))
 return -EINVAL;

 slave = mlx4_get_slave_indx(dev, vf);
 if (slave < 0)
 return -EINVAL;

 vf_admin = &priv->mfunc.master.vf_admin[slave].vport[port];
 vf_oper = &priv->mfunc.master.vf_oper[slave].vport[port];

 if ((0 == vlan) && (0 == qos))
 vf_admin->default_vlan = MLX4_VGT;
 else
 vf_admin->default_vlan = vlan;
 vf_admin->default_qos = qos;

 if (priv->mfunc.master.slave_state[slave].active &&
 dev->caps.flags2 & MLX4_DEV_CAP_FLAG2_UPDATE_QP) {
 MLX4_DEBUG( "updating vf %d port %d config params immediately\n",
 vf, port);
 mlx4_master_immediate_activate_vlan_qos(priv, slave, port);
 }
 return 0;
 }
 EXPORT_SYMBOL_GPL(mlx4_set_vf_vlan);

 mlx4_get_slave_default_vlan -
 * retrun true if VST ( default vlan)
 * if VST will fill vlan & qos (if not NULL)
 bool mlx4_get_slave_default_vlan(struct mlx4_priv *priv, int port, int slave, u16 *vlan, u8 *qos)
 {
 struct mlx4_vport_oper_state *vp_oper;
 struct mlx4_priv *priv;

 priv = mlx4_priv(dev);
 vp_oper = &priv->mfunc.master.vf_oper[slave].vport[port];

 if (MLX4_VGT != vp_oper->state.default_vlan) {
 if (vlan)
 *vlan = vp_oper->state.default_vlan;
 if (qos)
 *qos = vp_oper->state.default_qos;
 return true;
 }
 return false;
 }
 EXPORT_SYMBOL_GPL(mlx4_get_slave_default_vlan);

 int mlx4_set_vf_spoofchk(struct mlx4_priv *priv, int port, int vf, bool setting)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vport_state *s_info;
 int slave;

 if ((!mlx4_is_master(dev)) ||
 !(dev->caps.flags2 & MLX4_DEV_CAP_FLAG2_FSM))
 return -EPROTONOSUPPORT;

 slave = mlx4_get_slave_indx(dev, vf);
 if (slave < 0)
 return -EINVAL;

 s_info = &priv->mfunc.master.vf_admin[slave].vport[port];
 s_info->spoofchk = setting;

 return 0;
 }
 EXPORT_SYMBOL_GPL(mlx4_set_vf_spoofchk);

 int mlx4_set_vf_link_state(struct mlx4_priv *priv, int port, int vf, int link_state)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vport_state *s_info;
 struct mlx4_vport_oper_state *vp_oper;
 int slave;
 u8 link_stat_event;

 slave = mlx4_get_slave_indx(dev, vf);
 if (slave < 0)
 return -EINVAL;

 switch (link_state) {
 case IFLA_VF_LINK_STATE_AUTO:
 get link curent state
 if (!priv->sense.do_sense_port[port])
 link_stat_event = MLX4_PORT_CHANGE_SUBTYPE_ACTIVE;
 else
 link_stat_event = MLX4_PORT_CHANGE_SUBTYPE_DOWN;
 break;

 case IFLA_VF_LINK_STATE_ENABLE:
 link_stat_event = MLX4_PORT_CHANGE_SUBTYPE_ACTIVE;
 break;

 case IFLA_VF_LINK_STATE_DISABLE:
 link_stat_event = MLX4_PORT_CHANGE_SUBTYPE_DOWN;
 break;

 default:
 MLX4_DEBUG( "unknown value for link_state %02x on slave %d port %d\n",
 link_state, slave, port);
 return -EINVAL;
 };
 update the admin & oper state on the link state
 s_info = &priv->mfunc.master.vf_admin[slave].vport[port];
 vp_oper = &priv->mfunc.master.vf_oper[slave].vport[port];
 s_info->link_state = link_state;
 vp_oper->state.link_state = link_state;

 send event
 mlx4_gen_port_state_change_eqe(dev, slave, port, link_stat_event);
 return 0;
 }
 EXPORT_SYMBOL_GPL(mlx4_set_vf_link_state);

 int mlx4_get_vf_link_state(struct mlx4_priv *priv, int port, int vf)
 {
 struct mlx4_priv *priv = mlx4_priv(dev);
 struct mlx4_vport_state *s_info;
 int slave;

 if (!mlx4_is_master(dev))
 return -EPROTONOSUPPORT;

 slave = mlx4_get_slave_indx(dev, vf);
 if (slave < 0)
 return -EINVAL;

 s_info = &priv->mfunc.master.vf_admin[slave].vport[port];

 return s_info->link_state;
 }
 EXPORT_SYMBOL_GPL(mlx4_get_vf_link_state);*/
