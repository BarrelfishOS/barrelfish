/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

#include <pci/pci.h>

#include <linux/pci.h>
#include <linux/io.h>
#include <linux/gfp.h>
#include <linux/mm.h>
#include <linux/log2.h>

/*#include <pci/confspace/mackerelpci.h>*/

#include <debug.h>
#include <mlx4ib.h>

#include <mlx4en.h>

#include "fw.h"

#include <linux/mlx4/cmd.h>
#include <linux/mlx4/driver.h>

#include <asm/byteorder.h>
#include <asm/atomic.h>

#include <net_interfaces/flags.h>
#include <devif/backends/net/mlx4_devif.h>
#include "mlx4_devif_queue.h"
#include "mlx4_en.h"

/*****************************************************************
 * Local states:
 *****************************************************************/

static struct mlx4_priv *priv;
bool got_irq = false;
bool got_port_irq = false;
bool got_up_irq = false;

u64 dest_addr;
u32 dest_rkey;
u32 dest_qp_num;
u16 dest_lid;

enum {
	NUM_VFS, PROBE_VF, PORT_TYPE_ARRAY
};
enum {
	MLX4_NUM_ASYNC_EQE = 0x100,
	MLX4_NUM_SPARE_EQE = 0x80,
	MLX4_EQ_ENTRY_SIZE = 0x20
};
enum {
	MLX4_IF_STATE_BASIC, MLX4_IF_STATE_EXTENDED
};

/*arguments passed to the module*/
static int log_num_mac = 7;
static int fast_drop;
int mlx4_enable_64b_cqe_eqe = 1;
static int high_rate_steer;
int log_mtts_per_seg = 0; /*ilog2(1);*/
int mlx4_log_num_mgm_entry_size = MLX4_DEFAULT_MGM_LOG_ENTRY_SIZE;
static struct mlx4_profile mod_param_profile = { .num_qp = 19, .num_srq = 16,
		.rdmarc_per_qp = 4, .num_cq = 16, .num_mcg = 13, .num_mpt = 19,
		.num_mtt_segs = 0, /* max(20, 2*MTTs for host memory)) */
};

int mlx4_blck_lb = 1;
/*module_param_named(block_loopback, mlx4_blck_lb, int, 0644);
 MODULE_PARM_DESC(block_loopback, "Block multicast loopback packets if > 0 "
 "(default: 1)");*/

/*struct param_data {
 int id;
 struct mlx4_dbdf2val_lst dbdf2val;
 };
 static struct param_data num_vfs = { .id = NUM_VFS, .dbdf2val = { .name =
 "num_vfs param", .num_vals = 1, .def_val = { 0 }, .range = { 0,
 MLX4_MAX_NUM_VF } } };
 static struct param_data port_type_array = { .id = PORT_TYPE_ARRAY, .dbdf2val =
 { .name = "port_type_array param", .num_vals = 2, .def_val = {
 MLX4_PORT_TYPE_ETH, MLX4_PORT_TYPE_ETH }, .range = {
 MLX4_PORT_TYPE_IB, MLX4_PORT_TYPE_NA } } };*/
/*ENDOF: arguments passed to the module*/

#define MLX4_LOG_NUM_VLANS 7
#define MLX4_LOG_NUM_MTT 20
#define MLX4_MAX_LOG_NUM_MTT 30

#define PCIM_CMD_MEMEN	0x0002
#define PCIR_COMMAND    0x04

int uma_align_cache = 64 - 1;
#define	dma_get_cache_alignment()	uma_align_cache

inline uint16_t read16(void *base, int offset);
inline uint16_t read16(void *base, int offset) {
	volatile uint16_t *p = (volatile uint16_t *) (base + offset);
	return *p;
}

inline void write16(void *base, int offset, uint16_t v);
inline void write16(void *base, int offset, uint16_t v) {
	volatile uint16_t *p = (volatile uint16_t *) (base + offset);
	*p = v;
}

//inline uint32_t read32(void *base, int offset);
//inline uint32_t read32(void *base, int offset)
//{
//uint32_t data = -1;
//	uint64_t * va = PCIE_VADDR(base, (unsigned)offset, (int)81, (unsigned)0, (unsigned)0);

/* __asm("movl %1, %0" : "=a" (data)
 : "m" (*(volatile uint32_t *)va));*/
//	return *va;
//}
/*static int allocate_and_map_fw(void) {
 int err;
 struct page *page;
 struct mlx4_cmd_mailbox *mailbox;
 __be64 *pages;
 int order;
 int i;

 mailbox = mlx4_alloc_cmd_mailbox();
 pages = mailbox->buf;

 order = get_order(priv->fw.fw_pages * BASE_PAGE_SIZE);
 page = alloc_pages(order);
 if (!page) {
 MLX4_DEBUG("Couldn't allocate FW area, aborting.\n");
 return -ENOMEM;
 }

 pages[1] = cpu_to_be64(
 (page->phys_addr) | (page->bits - MLX4_ICM_PAGE_SHIFT));

 MLX4_DEBUG("Allocated %d pages for FW\n", 1 << order);

 //pages = page->virt_addr;
 //for (i = 0; i < BASE_PAGE_SIZE / 2; ++i) {
 //	MLX4_DEBUG("offset 0x%x: %"PRIx64"\n", i * 8, pages[i]);
 //}

 err = mlx4_cmd(&priv->dev, mailbox->dma, 1, 0, MLX4_CMD_MAP_FA,
 MLX4_CMD_TIME_CLASS_B, MLX4_CMD_NATIVE);

 for (i = 0; i < BASE_PAGE_SIZE / 2; ++i) {
 MLX4_DEBUG("offset 0x%x: %"PRIx64"\n", i * 8, pages[i]);
 }

 if (err) {
 MLX4_DEBUG("MAP_FA command failed, aborting.\n");
 goto err_free;
 } else
 goto err_ok;

 err_free: cap_destroy(page->slot);
 err_ok: mlx4_free_cmd_mailbox(mailbox);
 return err;

 }*/

#define MLX4_OWNER_BASE	0x8069c
static int mlx4_get_ownership(void) {
	void *owner;
	u32 ret;

	if (pci_channel_offline())
		return -EIO;

	owner = priv->dev.bar_info->vaddr + MLX4_OWNER_BASE;
	ret = __raw_readl(owner);

	return (int) !!ret;
}

static int mlx4_load_fw(void) {
	int err/*, unmap_flag = 0*/;
	/*struct mlx4_cmd_mailbox *mailbox;
	 mailbox = mlx4_alloc_cmd_mailbox();*/

	/*Tried to implement the mapping using scatterlist but it fails*/
	/*err = allocate_and_map_fw();
	 if (err) {
	 MLX4_DEBUG("MAP_FA command failed, aborting.\n");
	 return err;
	 }*/

	priv->fw.fw_icm = mlx4_alloc_icm(priv, priv->fw.fw_pages, 0);
	if (!priv->fw.fw_icm) {
		MLX4_DEBUG("Couldn't allocate FW area, aborting.\n");
		return -ENOMEM;
	}
	err = mlx4_MAP_FA(priv, priv->fw.fw_icm);
	if (err) {
		MLX4_DEBUG("MAP_FA command failed, aborting.\n");
		goto err_free;
	}

	err = mlx4_RUN_FW(priv);
	if (err) {
		MLX4_DEBUG("RUN_FW command failed, aborting.\n");
		goto err_unmap_fa;
	}

	/*err = mlx4_cmd_box(priv, 0, mailbox->dma, 0, 0, MLX4_CMD_QUERY_DEBUG_MSG,
	 MLX4_CMD_TIME_CLASS_A, MLX4_CMD_NATIVE);*/

	return 0;

	err_unmap_fa:
	/*TODO: implement UNMAP_FA*/
	/*unmap_flag = mlx4_UNMAP_FA(&priv->dev);
	 if (unmap_flag)
	 pr_warn("mlx4_core: mlx4_UNMAP_FA failed.\n");
	 */
	err_free:
	/*TODO*/
	/*if (!unmap_flag)
	 mlx4_free_icm(priv, priv->fw.fw_icm, 0);
	 return err;*/
	return err;
}

static int mlx4_dev_cap(struct mlx4_dev_cap *dev_cap) {
	int err;
	int i;

	err = mlx4_QUERY_DEV_CAP(priv, dev_cap);
	if (err) {
		MLX4_DEBUG("QUERY_DEV_CAP command failed, aborting.\n");
		return err;
	}

	if (dev_cap->min_page_sz > BASE_PAGE_SIZE) {
		MLX4_DEBUG("HCA minimum page size of %d bigger than "
				"kernel PAGE_SIZE of %d, aborting.\n", dev_cap->min_page_sz,
				(int ) BASE_PAGE_SIZE);
		return -ENODEV;
	}
	if (dev_cap->num_ports > MLX4_MAX_PORTS) {
		MLX4_DEBUG("HCA has %d ports, but we only support %d, "
				"aborting.\n", dev_cap->num_ports, MLX4_MAX_PORTS);
		return -ENODEV;
	}

	if (dev_cap->uar_size > priv->dev.bar_info[1].bytes) {
		MLX4_DEBUG("HCA reported UAR size of 0x%x bigger than "
				"PCI resource 2 size of 0x%llx, aborting.\n", dev_cap->uar_size,
				(unsigned long long ) priv->dev.bar_info[1].bytes);
		return -ENODEV;
	}

	priv->dev.caps.num_ports = dev_cap->num_ports;
	priv->dev.phys_caps.num_phys_eqs = MLX4_MAX_EQ_NUM;
	for (i = 1; i <= priv->dev.caps.num_ports; ++i) {
		priv->dev.caps.vl_cap[i] = dev_cap->max_vl[i];
		priv->dev.caps.ib_mtu_cap[i] = dev_cap->ib_mtu[i];
		priv->dev.phys_caps.gid_phys_table_len[i] = dev_cap->max_gids[i];
		priv->dev.phys_caps.pkey_phys_table_len[i] = dev_cap->max_pkeys[i];
		/* set gid and pkey table operating lengths by default
		 * to non-sriov values */
		priv->dev.caps.gid_table_len[i] = dev_cap->max_gids[i];
		priv->dev.caps.pkey_table_len[i] = dev_cap->max_pkeys[i];
		priv->dev.caps.port_width_cap[i] = dev_cap->max_port_width[i];
		priv->dev.caps.eth_mtu_cap[i] = dev_cap->eth_mtu[i];
		priv->dev.caps.def_mac[i] = dev_cap->def_mac[i];
		priv->dev.caps.supported_type[i] = dev_cap->supported_port_types[i];
		priv->dev.caps.suggested_type[i] = dev_cap->suggested_type[i];
		priv->dev.caps.default_sense[i] = dev_cap->default_sense[i];
		priv->dev.caps.trans_type[i] = dev_cap->trans_type[i];
		priv->dev.caps.vendor_oui[i] = dev_cap->vendor_oui[i];
		priv->dev.caps.wavelength[i] = dev_cap->wavelength[i];
		priv->dev.caps.trans_code[i] = dev_cap->trans_code[i];
	}

	priv->dev.caps.uar_page_size = BASE_PAGE_SIZE;
	priv->dev.caps.num_uars = dev_cap->uar_size / BASE_PAGE_SIZE;
	priv->dev.caps.local_ca_ack_delay = dev_cap->local_ca_ack_delay;
	priv->dev.caps.bf_reg_size = dev_cap->bf_reg_size;
	priv->dev.caps.bf_regs_per_page = dev_cap->bf_regs_per_page;
	priv->dev.caps.max_sq_sg = dev_cap->max_sq_sg;
	priv->dev.caps.max_rq_sg = dev_cap->max_rq_sg;
	priv->dev.caps.max_wqes = dev_cap->max_qp_sz;
	priv->dev.caps.max_qp_init_rdma = dev_cap->max_requester_per_qp;
	priv->dev.caps.max_srq_wqes = dev_cap->max_srq_sz;
	priv->dev.caps.max_srq_sge = dev_cap->max_rq_sg - 1;
	priv->dev.caps.reserved_srqs = dev_cap->reserved_srqs;
	priv->dev.caps.max_sq_desc_sz = dev_cap->max_sq_desc_sz;
	priv->dev.caps.max_rq_desc_sz = dev_cap->max_rq_desc_sz;

	/*
	 * Subtract 1 from the limit because we need to allocate a
	 * spare CQE to enable resizing the CQ
	 */

	priv->dev.caps.max_cqes = dev_cap->max_cq_sz - 1;
	priv->dev.caps.reserved_cqs = dev_cap->reserved_cqs;
	priv->dev.caps.reserved_eqs = dev_cap->reserved_eqs;
	priv->dev.caps.reserved_mtts = dev_cap->reserved_mtts;
	priv->dev.caps.reserved_mrws = dev_cap->reserved_mrws;

	/* The first 128 UARs are used for EQ doorbells */
	priv->dev.caps.reserved_uars = max_t(int, 128, dev_cap->reserved_uars);
	priv->dev.caps.reserved_pds = dev_cap->reserved_pds;
	priv->dev.caps.reserved_xrcds =
			(priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_XRC) ?
					dev_cap->reserved_xrcds : 0;
	priv->dev.caps.max_xrcds =
			(priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_XRC) ?
					dev_cap->max_xrcds : 0;
	priv->dev.caps.mtt_entry_sz = dev_cap->mtt_entry_sz;

	priv->dev.caps.max_msg_sz = dev_cap->max_msg_sz;
	priv->dev.caps.page_size_cap = ~(u32)(dev_cap->min_page_sz - 1);
	priv->dev.caps.flags = dev_cap->flags;
	priv->dev.caps.flags2 = dev_cap->flags2;
	priv->dev.caps.bmme_flags = dev_cap->bmme_flags;
	priv->dev.caps.reserved_lkey = dev_cap->reserved_lkey;
	priv->dev.caps.stat_rate_support = dev_cap->stat_rate_support;
	priv->dev.caps.cq_timestamp = dev_cap->timestamp_support;
	priv->dev.caps.max_gso_sz = dev_cap->max_gso_sz;
	priv->dev.caps.max_rss_tbl_sz = dev_cap->max_rss_tbl_sz;

	/* Sense port always allowed on supported devices for ConnectX-1 and -2 */
	if (priv->pci_dev_data & MLX4_PCI_DEV_FORCE_SENSE_PORT)
		priv->dev.caps.flags |= MLX4_DEV_CAP_FLAG_SENSE_SUPPORT;
	/* Don't do sense port on multifunction devices (for now at least) */
	if (mlx4_is_mfunc(&priv->dev))
		priv->dev.caps.flags &= ~MLX4_DEV_CAP_FLAG_SENSE_SUPPORT;

	priv->dev.caps.log_num_macs = log_num_mac;
	priv->dev.caps.log_num_vlans = MLX4_LOG_NUM_VLANS;

	priv->dev.caps.fast_drop =
			fast_drop ?
					!!(priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_FAST_DROP) : 0;

	for (i = 1; i <= priv->dev.caps.num_ports; ++i) {
		priv->dev.caps.port_type[i] = MLX4_PORT_TYPE_NONE;
		if (priv->dev.caps.supported_type[i]) {
			/* if only ETH is supported - assign ETH */
			if (priv->dev.caps.supported_type[i] == MLX4_PORT_TYPE_ETH)
				priv->dev.caps.port_type[i] = MLX4_PORT_TYPE_ETH;
			/* if only IB is supported, assign IB */
			else if (priv->dev.caps.supported_type[i] == MLX4_PORT_TYPE_IB)
				priv->dev.caps.port_type[i] = MLX4_PORT_TYPE_IB;
			else {
				/*
				 * if IB and ETH are supported, we set the port
				 * type according to user selection of port type;
				 * if there is no user selection, take the FW hint
				 */
				/*int pta;
				 mlx4_get_val(port_type_array.dbdf2val.tbl,
				 pci_physfn(priv->dev.pdev), i - 1,
				 &pta);
				 if (pta == MLX4_PORT_TYPE_NONE) {
				 priv->dev.caps.port_type[i] = priv->dev.caps.suggested_type[i] ?
				 MLX4_PORT_TYPE_ETH : MLX4_PORT_TYPE_IB;
				 } else if (pta == MLX4_PORT_TYPE_NA) {
				 MLX4_DEBUG( "Port %d is valid port. "
				 "It is not allowed to configure its type to N/A(%d)\n",
				 i, MLX4_PORT_TYPE_NA);
				 return -EINVAL;
				 } else {
				 priv->dev.caps.port_type[i] = pta;
				 }*/
				/*TODO: let the user select the port like above*/
				// priv->dev.caps.port_type[i] = MLX4_PORT_TYPE_IB;
				priv->dev.caps.port_type[i] = MLX4_PORT_TYPE_ETH;
			}
		}
        debug_printf("Port %d: %d\n", i, priv->dev.caps.port_type[i]);
		/*
		 * Link sensing is allowed on the port if 3 conditions are true:
		 * 1. Both protocols are supported on the port.
		 * 2. Different types are supported on the port
		 * 3. FW declared that it supports link sensing
		 */

		priv->sense.sense_allowed[i] = ((priv->dev.caps.supported_type[i]
				== MLX4_PORT_TYPE_AUTO)
				&& (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_DPDP)
				&& (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_SENSE_SUPPORT));

		/* Disablling auto sense for default Eth ports support */
		priv->sense.sense_allowed[i] = 0;

		/*
		 * If "default_sense" bit is set, we move the port to "AUTO" mode
		 * and perform sense_port FW command to try and set the correct
		 * port type from beginning
		 */

		/*TODO: implement port sensing*/
		/*if (priv->sense.sense_allowed[i]
		 && priv->dev.caps.default_sense[i]) {
		 enum mlx4_port_type sensed_port = MLX4_PORT_TYPE_NONE;
		 priv->dev.caps.possible_type[i] = MLX4_PORT_TYPE_AUTO;
		 mlx4_SENSE_PORT(priv, i, &sensed_port);
		 if (sensed_port != MLX4_PORT_TYPE_NONE)
		 priv->dev.caps.port_type[i] = sensed_port;
		 } else {*/
		priv->dev.caps.possible_type[i] = priv->dev.caps.port_type[i];
		/*}*/

		if (priv->dev.caps.log_num_macs > dev_cap->log_max_macs[i]) {
			priv->dev.caps.log_num_macs = dev_cap->log_max_macs[i];
			MLX4_DEBUG("Requested number of MACs is too much "
					"for port %d, reducing to %d.\n", i,
					1 << priv->dev.caps.log_num_macs);
		}
		if (priv->dev.caps.log_num_vlans > dev_cap->log_max_vlans[i]) {
			priv->dev.caps.log_num_vlans = dev_cap->log_max_vlans[i];
			MLX4_DEBUG("Requested number of VLANs is too much "
					"for port %d, reducing to %d.\n", i,
					1 << priv->dev.caps.log_num_vlans);
		}
	}

	priv->dev.caps.max_basic_counters = dev_cap->max_basic_counters;
	priv->dev.caps.max_extended_counters = dev_cap->max_extended_counters;
	/* support extended counters if available */
	if (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_COUNTERS_EXT)
		priv->dev.caps.max_counters = priv->dev.caps.max_extended_counters;
	else
		priv->dev.caps.max_counters = priv->dev.caps.max_basic_counters;

	priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW] = dev_cap->reserved_qps;
	priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_ETH_ADDR] =
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FC_ADDR] = (1
					<< priv->dev.caps.log_num_macs)
					* (1 << priv->dev.caps.log_num_vlans)
					* priv->dev.caps.num_ports;
	priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FC_EXCH] = MLX4_NUM_FEXCH;

	priv->dev.caps.reserved_qps =
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW]
					+ priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_ETH_ADDR]
					+ priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FC_ADDR]
					+ priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FC_EXCH];

	priv->dev.caps.sync_qp = dev_cap->sync_qp;

	if (priv->dev.pdev->device == 0x1003)
		priv->dev.caps.cq_flags |= MLX4_DEV_CAP_CQ_FLAG_IO;

	priv->dev.caps.sqp_demux =
			(mlx4_is_master(&priv->dev)) ? MLX4_MAX_NUM_SLAVES : 0;

	if (!mlx4_enable_64b_cqe_eqe && !mlx4_is_slave(&priv->dev)) {
		if (dev_cap->flags
				& (MLX4_DEV_CAP_FLAG_64B_CQE | MLX4_DEV_CAP_FLAG_64B_EQE)) {
			MLX4_DEBUG(
					"64B EQEs/CQEs supported by the device but not enabled\n");
			priv->dev.caps.flags &= ~MLX4_DEV_CAP_FLAG_64B_CQE;
			priv->dev.caps.flags &= ~MLX4_DEV_CAP_FLAG_64B_EQE;
		}
	}

	if ((priv->dev.caps.flags
			& (MLX4_DEV_CAP_FLAG_64B_CQE | MLX4_DEV_CAP_FLAG_64B_EQE))
			&& mlx4_is_master(&priv->dev))
		priv->dev.caps.function_caps |= MLX4_FUNC_CAP_64B_EQE_CQE;

	if (!mlx4_is_slave(&priv->dev)) {
		for (i = 0; i < priv->dev.caps.num_ports; ++i)
			priv->dev.caps.def_counter_index[i] = i << 1;
	}

	return 0;
}

static int choose_log_fs_mgm_entry_size(int qp_per_entry) {
	int i = MLX4_MIN_MGM_LOG_ENTRY_SIZE;

	for (i = MLX4_MIN_MGM_LOG_ENTRY_SIZE; i <= MLX4_MAX_MGM_LOG_ENTRY_SIZE;
			i++) {
		if (qp_per_entry <= 4 * ((1 << i) / 16 - 2))
			break;
	}

	return (i <= MLX4_MAX_MGM_LOG_ENTRY_SIZE) ? i : -1;
}

static void choose_steering_mode(struct mlx4_dev *dev,
		struct mlx4_dev_cap *dev_cap) {
	/*HARDWIRE*/
	int nvfs = 0;

	/*mlx4_get_val(num_vfs.dbdf2val.tbl, pci_physfn(priv->dev.pdev), 0, &nvfs);*/
	if (high_rate_steer && !mlx4_is_mfunc(&priv->dev)) {
		priv->dev.caps.flags &= ~(MLX4_DEV_CAP_FLAG_VEP_MC_STEER
				| MLX4_DEV_CAP_FLAG_VEP_UC_STEER);
		dev_cap->flags2 &= ~MLX4_DEV_CAP_FLAG2_FS_EN;
	}

	if (mlx4_log_num_mgm_entry_size == -1
			&& dev_cap->flags2 & MLX4_DEV_CAP_FLAG2_FS_EN
			&& (!mlx4_is_mfunc(&priv->dev)
					|| (dev_cap->fs_max_num_qp_per_entry >= (nvfs + 1)))
			&& choose_log_fs_mgm_entry_size(dev_cap->fs_max_num_qp_per_entry)
					>= MLX4_MIN_MGM_LOG_ENTRY_SIZE) {
		priv->dev.oper_log_mgm_entry_size = choose_log_fs_mgm_entry_size(
				dev_cap->fs_max_num_qp_per_entry);
		priv->dev.caps.steering_mode = MLX4_STEERING_MODE_DEVICE_MANAGED;
		priv->dev.caps.num_qp_per_mgm = dev_cap->fs_max_num_qp_per_entry;
	} else {
		if (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_VEP_UC_STEER
				&& priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_VEP_MC_STEER)
			priv->dev.caps.steering_mode = MLX4_STEERING_MODE_B0;
		else {
			priv->dev.caps.steering_mode = MLX4_STEERING_MODE_A0;

			if (priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_VEP_UC_STEER
					|| priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_VEP_MC_STEER)
				MLX4_DEBUG(
						"Must have both UC_STEER and MC_STEER flags "
								"set to use B0 steering. Falling back to A0 steering mode.\n");
		}
		priv->dev.oper_log_mgm_entry_size =
				mlx4_log_num_mgm_entry_size > 0 ?
						mlx4_log_num_mgm_entry_size :
						MLX4_DEFAULT_MGM_LOG_ENTRY_SIZE;
		priv->dev.caps.num_qp_per_mgm = mlx4_get_qp_per_mgm(&priv->dev);
	}
	MLX4_DEBUG("Steering mode is: %s, oper_log_mgm_entry_size = %d, "
			"log_num_mgm_entry_size = %d\n",
			mlx4_steering_mode_str(priv->dev.caps.steering_mode),
			priv->dev.oper_log_mgm_entry_size, mlx4_log_num_mgm_entry_size);
}

static void process_mod_param_profile(struct mlx4_profile *profile) {
	/*errval_t err;*/
	genpaddr_t /*available,*/hwphyssz;

	/*err = ram_available(&available, &hwphyssz);
	 assert(err_is_ok(err));*/
	/*TUNABLE_ULONG_FETCH("hw.realmem", (u_long *) &hwphyssz);*/

	/*the command in freebsd is broken so I hardwire*/
	hwphyssz = 0;

	profile->num_qp = 1 << mod_param_profile.num_qp;
	profile->num_srq = 1 << mod_param_profile.num_srq;
	profile->rdmarc_per_qp = 1 << mod_param_profile.rdmarc_per_qp;
	profile->num_cq = 1 << mod_param_profile.num_cq;
	profile->num_mcg = 1 << mod_param_profile.num_mcg;
	profile->num_mpt = 1 << mod_param_profile.num_mpt;
	/*
	 * We want to scale the number of MTTs with the size of the
	 * system memory, since it makes sense to register a lot of
	 * memory on a system with a lot of memory.  As a heuristic,
	 * make sure we have enough MTTs to register twice the system
	 * memory (with PAGE_SIZE entries).
	 *
	 * This number has to be a power of two and fit into 32 bits
	 * due to device limitations. We cap this at 2^30 as of bit map
	 * limitation to work with int instead of uint (mlx4_buddy_init -> bitmap_zero)
	 * That limits us to 4TB of memory registration per HCA with
	 * 4KB pages, which is probably OK for the next few months.
	 */
	if (mod_param_profile.num_mtt_segs)
		profile->num_mtt_segs = 1 << mod_param_profile.num_mtt_segs;
	else {
		profile->num_mtt_segs =
		roundup_pow_of_two(max_t(unsigned,
						1 << (MLX4_LOG_NUM_MTT - log_mtts_per_seg),
						min(1UL <<
								(MLX4_MAX_LOG_NUM_MTT -
										log_mtts_per_seg),
								(hwphyssz << 1)
								>> log_mtts_per_seg)));
		/* set the actual value, so it will be reflected to the user
		 using the sysfs */
		mod_param_profile.num_mtt_segs = ilog2(profile->num_mtt_segs);
	}
}

static int mlx4_init_cmpt_table(u64 cmpt_base, int cmpt_entry_sz) {
	int err;
	int num_eqs;

	err = mlx4_init_icm_table(priv, &priv->qp_table.cmpt_table,
			cmpt_base
					+ ((u64) (MLX4_CMPT_TYPE_QP * cmpt_entry_sz)
							<< MLX4_CMPT_SHIFT), cmpt_entry_sz,
			priv->dev.caps.num_qps,
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW], 0, 0);
	if (err)
		goto err;

	err = mlx4_init_icm_table(priv, &priv->srq_table.cmpt_table,
			cmpt_base
					+ ((u64) (MLX4_CMPT_TYPE_SRQ * cmpt_entry_sz)
							<< MLX4_CMPT_SHIFT), cmpt_entry_sz,
			priv->dev.caps.num_srqs, priv->dev.caps.reserved_srqs, 0, 0);
	if (err)
		goto err_qp;

	err = mlx4_init_icm_table(priv, &priv->cq_table.cmpt_table,
			cmpt_base
					+ ((u64) (MLX4_CMPT_TYPE_CQ * cmpt_entry_sz)
							<< MLX4_CMPT_SHIFT), cmpt_entry_sz,
			priv->dev.caps.num_cqs, priv->dev.caps.reserved_cqs, 0, 0);
	if (err)
		goto err_srq;

	num_eqs =
			(mlx4_is_master(&priv->dev)) ?
					priv->dev.phys_caps.num_phys_eqs : priv->dev.caps.num_eqs;
	err = mlx4_init_icm_table(priv, &priv->eq_table.cmpt_table,
			cmpt_base
					+ ((u64) (MLX4_CMPT_TYPE_EQ * cmpt_entry_sz)
							<< MLX4_CMPT_SHIFT), cmpt_entry_sz, num_eqs,
			num_eqs, 0, 0);
	if (err)
		goto err_cq;

	return 0;

	err_cq: /*mlx4_cleanup_icm_table(priv, &priv->cq_table.cmpt_table);*/

	err_srq: /*mlx4_cleanup_icm_table(priv, &priv->srq_table.cmpt_table);*/

	err_qp: /*mlx4_cleanup_icm_table(priv, &priv->qp_table.cmpt_table);*/

	err: return err;
}

static int mlx4_init_icm(struct mlx4_dev_cap *dev_cap,
		struct mlx4_init_hca_param *init_hca, u64 icm_size) {
	u64 aux_pages;
	int num_eqs;
	int err/*, unmap_flag = 0*/;

	err = mlx4_SET_ICM_SIZE(priv, icm_size, &aux_pages);
	if (err) {
		MLX4_DEBUG("SET_ICM_SIZE command failed, aborting.\n");
		return err;
	}

	MLX4_DEBUG("%lld KB of HCA context requires %lld KB aux memory.\n",
			(unsigned long long ) icm_size >> 10,
			(unsigned long long ) aux_pages << 2);

	priv->fw.aux_icm = mlx4_alloc_icm(priv, aux_pages, 0);
	if (!priv->fw.aux_icm) {
		MLX4_DEBUG("Couldn't allocate aux memory, aborting.\n");
		return -ENOMEM;
	}

	err = mlx4_MAP_ICM_AUX(priv, priv->fw.aux_icm);
	if (err) {
		MLX4_DEBUG("MAP_ICM_AUX command failed, aborting.\n");
		goto err_free_aux;
	}

	err = mlx4_init_cmpt_table(init_hca->cmpt_base, dev_cap->cmpt_entry_sz);
	if (err) {
		MLX4_DEBUG("Failed to map cMPT context memory, aborting.\n");
		goto err_unmap_aux;
	}

	num_eqs =
			(mlx4_is_master(&priv->dev)) ?
					priv->dev.phys_caps.num_phys_eqs : priv->dev.caps.num_eqs;
	err = mlx4_init_icm_table(priv, &priv->eq_table.table, init_hca->eqc_base,
			dev_cap->eqc_entry_sz, num_eqs, num_eqs, 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map EQ context memory, aborting.\n");
		goto err_unmap_cmpt;
	}

	/*
	 * Reserved MTT entries must be aligned up to a cacheline
	 * boundary, since the FW will write to them, while the driver
	 * writes to all other MTT entries. (The variable
	 * priv->dev.caps.mtt_entry_sz below is really the MTT segment
	 * size, not the raw entry size)
	 */
	priv->dev.caps.reserved_mtts = ALIGN(
			priv->dev.caps.reserved_mtts * priv->dev.caps.mtt_entry_sz,
			dma_get_cache_alignment()) / priv->dev.caps.mtt_entry_sz;

	err = mlx4_init_icm_table(priv, &priv->mr_table.mtt_table,
			init_hca->mtt_base, priv->dev.caps.mtt_entry_sz,
			priv->dev.caps.num_mtts, priv->dev.caps.reserved_mtts, 1, 0);
	if (err) {
		MLX4_DEBUG("Failed to map MTT context memory, aborting.\n");
		goto err_unmap_eq;
	}

	err = mlx4_init_icm_table(priv, &priv->mr_table.dmpt_table,
			init_hca->dmpt_base, dev_cap->dmpt_entry_sz,
			priv->dev.caps.num_mpts, priv->dev.caps.reserved_mrws, 1, 1);
	if (err) {
		MLX4_DEBUG("Failed to map dMPT context memory, aborting.\n");
		goto err_unmap_mtt;
	}

	err = mlx4_init_icm_table(priv, &priv->qp_table.qp_table,
			init_hca->qpc_base, dev_cap->qpc_entry_sz, priv->dev.caps.num_qps,
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW], 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map QP context memory, aborting.\n");
		goto err_unmap_dmpt;
	}

	err = mlx4_init_icm_table(priv, &priv->qp_table.auxc_table,
			init_hca->auxc_base, dev_cap->aux_entry_sz, priv->dev.caps.num_qps,
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW], 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map AUXC context memory, aborting.\n");
		goto err_unmap_qp;
	}

	err = mlx4_init_icm_table(priv, &priv->qp_table.altc_table,
			init_hca->altc_base, dev_cap->altc_entry_sz, priv->dev.caps.num_qps,
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW], 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map ALTC context memory, aborting.\n");
		goto err_unmap_auxc;
	}

	err = mlx4_init_icm_table(priv, &priv->qp_table.rdmarc_table,
			init_hca->rdmarc_base,
			dev_cap->rdmarc_entry_sz << priv->qp_table.rdmarc_shift,
			priv->dev.caps.num_qps,
			priv->dev.caps.reserved_qps_cnt[MLX4_QP_REGION_FW], 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map RDMARC context memory, aborting\n");
		goto err_unmap_altc;
	}

	err = mlx4_init_icm_table(priv, &priv->cq_table.table, init_hca->cqc_base,
			dev_cap->cqc_entry_sz, priv->dev.caps.num_cqs,
			priv->dev.caps.reserved_cqs, 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map CQ context memory, aborting.\n");
		goto err_unmap_rdmarc;
	}

	err = mlx4_init_icm_table(priv, &priv->srq_table.table, init_hca->srqc_base,
			dev_cap->srq_entry_sz, priv->dev.caps.num_srqs,
			priv->dev.caps.reserved_srqs, 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map SRQ context memory, aborting.\n");
		goto err_unmap_cq;
	}

	/*
	 * For flow steering device managed mode it is required to use
	 * mlx4_init_icm_table. For B0 steering mode it's not strictly
	 * required, but for simplicity just map the whole multicast
	 * group table now.  The table isn't very big and it's a lot
	 * easier than trying to track ref counts.
	 */
	err = mlx4_init_icm_table(priv, &priv->mcg_table.table, init_hca->mc_base,
			mlx4_get_mgm_entry_size(&priv->dev),
			priv->dev.caps.num_mgms + priv->dev.caps.num_amgms,
			priv->dev.caps.num_mgms + priv->dev.caps.num_amgms, 0, 0);
	if (err) {
		MLX4_DEBUG("Failed to map MCG context memory, aborting.\n");
		goto err_unmap_srq;
	}

	return 0;

	err_unmap_srq: /*mlx4_cleanup_icm_table(priv, &priv->srq_table.table);*/

	err_unmap_cq: /*mlx4_cleanup_icm_table(priv, &priv->cq_table.table);*/

	err_unmap_rdmarc: /*mlx4_cleanup_icm_table(priv, &priv->qp_table.rdmarc_table);*/

	err_unmap_altc: /*mlx4_cleanup_icm_table(priv, &priv->qp_table.altc_table);*/

	err_unmap_auxc: /*mlx4_cleanup_icm_table(priv, &priv->qp_table.auxc_table);*/

	err_unmap_qp: /*mlx4_cleanup_icm_table(priv, &priv->qp_table.qp_table);*/

	err_unmap_dmpt: /*mlx4_cleanup_icm_table(priv, &priv->mr_table.dmpt_table);*/

	err_unmap_mtt: /*mlx4_cleanup_icm_table(priv, &priv->mr_table.mtt_table);*/

	err_unmap_eq: /*mlx4_cleanup_icm_table(priv, &priv->eq_table.table);*/

	err_unmap_cmpt:
	/*mlx4_cleanup_icm_table(priv, &priv->eq_table.cmpt_table);
	 mlx4_cleanup_icm_table(priv, &priv->cq_table.cmpt_table);
	 mlx4_cleanup_icm_table(priv, &priv->srq_table.cmpt_table);
	 mlx4_cleanup_icm_table(priv, &priv->qp_table.cmpt_table);
	 */
	err_unmap_aux:
	/*unmap_flag = mlx4_UNMAP_ICM_AUX(&priv->dev);
	 if (unmap_flag)
	 pr_warn("mlx4_core: mlx4_UNMAP_ICM_AUX failed.\n");

	 */
	err_free_aux:
	/*if (!unmap_flag)
	 mlx4_free_icm(priv, priv->fw.aux_icm, 0);*/

	return err;
}

static int map_bf_area(void) {
	int err = 0;

	if (!priv->dev.caps.bf_reg_size)
		return -ENXIO;

	/*priv->bf_mapping = malloc(sizeof(struct io_mapping));*/

	priv->bf_mapping/*->base*/= priv->dev.bar_info[1].vaddr
			+ (priv->dev.caps.num_uars << PAGE_SHIFT);
	/*priv->bf_mapping->size = priv->dev.bar_info[1].bytes
	 - (priv->dev.caps.num_uars << PAGE_SHIFT);*/

	return err;
}

static void mlx4_set_port_mask(struct mlx4_dev *dev) {
	int i;

	for (i = 1; i <= priv->dev.caps.num_ports; ++i)
		priv->dev.caps.port_mask[i] = priv->dev.caps.port_type[i];
}

static int mlx4_init_hca(void) {
	struct mlx4_dev_cap *dev_cap = NULL;
	struct mlx4_adapter adapter;
	struct mlx4_mod_stat_cfg mlx4_cfg;
	struct mlx4_profile profile;
	struct mlx4_init_hca_param init_hca;
	u64 icm_size;
	int err;

	err = mlx4_QUERY_FW(priv);
	if (err) {
		if (err == -EACCES)
			MLX4_DEBUG("non-primary physical function, skipping.\n");
		else
			MLX4_DEBUG("QUERY_FW command failed, aborting.\n");
		return err;
	}

	err = mlx4_load_fw();
	if (err) {
		MLX4_DEBUG("Failed to start FW, aborting.\n");
		return err;
	}

	/*Modify the minimum system page size*/
	mlx4_cfg.log_pg_sz_m = 1;
	mlx4_cfg.log_pg_sz = 0;
	err = mlx4_MOD_STAT_CFG(priv, &mlx4_cfg);
	if (err)
		MLX4_DEBUG("Failed to override log_pg_sz parameter\n");

	dev_cap = malloc(sizeof *dev_cap);
	if (!dev_cap) {
		MLX4_DEBUG("Failed to allocate memory for dev_cap\n");
		err = -ENOMEM;
		goto err_stop_fw;
	}

	err = mlx4_dev_cap(dev_cap);
	if (err) {
		MLX4_DEBUG("QUERY_DEV_CAP command failed, aborting.\n");
		goto err_stop_fw;
	}

	choose_steering_mode(&priv->dev, dev_cap);

	/*if (mlx4_is_master(&priv->dev))
	 mlx4_parav_master_pf_caps(&priv->dev);*/

	process_mod_param_profile(&profile);
	if (priv->dev.caps.steering_mode == MLX4_STEERING_MODE_DEVICE_MANAGED)
		profile.num_mcg = MLX4_FS_NUM_MCG;

	icm_size = mlx4_make_profile(priv, &profile, dev_cap, &init_hca);
	if ((long long) icm_size < 0) {
		err = icm_size;
		goto err_stop_fw;
	}

	priv->dev.caps.max_fmr_maps = (1 << (32 - ilog2(priv->dev.caps.num_mpts)))
			- 1;

	init_hca.log_uar_sz = ilog2(priv->dev.caps.num_uars);
	init_hca.uar_page_sz = PAGE_SHIFT - 12;

	err = mlx4_init_icm(dev_cap, &init_hca, icm_size);
	if (err)
		goto err_stop_fw;

	init_hca.mw_enable = 1;

	err = mlx4_INIT_HCA(priv, &init_hca);
	if (err) {
		MLX4_DEBUG("INIT_HCA command failed, aborting.\n");
		goto err_free_icm;
	}

	/*
	 * Read HCA frequency by QUERY_HCA command
	 */
	if (priv->dev.caps.flags2 & MLX4_DEV_CAP_FLAG2_TS) {
		memset(&init_hca, 0, sizeof(init_hca));
		err = mlx4_QUERY_HCA(priv, &init_hca);
		if (err) {
			MLX4_DEBUG("QUERY_HCA command failed, disable timestamp.\n");
			priv->dev.caps.flags2 &= ~MLX4_DEV_CAP_FLAG2_TS;
		} else {
			priv->dev.caps.hca_core_clock = init_hca.hca_core_clock;
		}

		/* In case we got HCA frequency 0 - disable timestamping
		 * to avoid dividing by zero
		 */
		if (!priv->dev.caps.hca_core_clock) {
			priv->dev.caps.flags2 &= ~MLX4_DEV_CAP_FLAG2_TS;
			MLX4_DEBUG("HCA frequency is 0. Timestamping is not supported.");
		} /*else if (map_internal_clock(&priv->dev)) {
		 Map internal clock,
		 * in case of failure disable timestamping

		 priv->dev.caps.flags2 &= ~MLX4_DEV_CAP_FLAG2_TS;
		 MLX4_DEBUG("Failed to map internal clock. Timestamping is not supported.\n");
		 }*/
	}

	/*TODO: in the case of slave*/

	if (map_bf_area())
		MLX4_DEBUG("Failed to map blue flame area\n");

	/* Only the master set the ports, all the rest got it from it.*/
	if (!mlx4_is_slave(&priv->dev))
		mlx4_set_port_mask(&priv->dev);

	err = mlx4_QUERY_ADAPTER(priv, &adapter);
	if (err) {
		MLX4_DEBUG("QUERY_ADAPTER command failed, aborting.\n");
		goto unmap_bf;
	}

	priv->eq_table.inta_pin = adapter.inta_pin;
	memcpy(priv->dev.board_id, adapter.board_id, sizeof priv->dev.board_id);
	memcpy(priv->dev.vsd, adapter.vsd, sizeof(priv->dev.vsd));
	priv->dev.vsd_vendor_id = adapter.vsd_vendor_id;

	if (!mlx4_is_slave(&priv->dev))
		free(dev_cap);

	return 0;

	unmap_bf:
	/*TODO*/
	/*if (!mlx4_is_slave(&priv->dev))
	 unmap_internal_clock(&priv->dev);
	 unmap_bf_area(&priv->dev);

	 if (mlx4_is_slave(&priv->dev)) {
	 kfree(priv->dev.caps.qp0_tunnel);
	 kfree(priv->dev.caps.qp0_proxy);
	 kfree(priv->dev.caps.qp1_tunnel);
	 kfree(priv->dev.caps.qp1_proxy);
	 }

	 err_close: if (mlx4_is_slave(&priv->dev))
	 mlx4_slave_exit(&priv->dev);
	 else
	 mlx4_CLOSE_HCA(priv, 0);
	 */
	err_free_icm:
	/*if (!mlx4_is_slave(&priv->dev))
	 mlx4_free_icms(&priv->dev);
	 */
	err_stop_fw:
	/*
	 if (!mlx4_is_slave(&priv->dev)) {
	 if (!mlx4_UNMAP_FA(&priv->dev))
	 mlx4_free_icm(priv, priv->fw.fw_icm, 0);
	 else
	 pr_warn("mlx4_core: mlx4_UNMAP_FA failed.\n");
	 kfree(dev_cap);
	 }*/
	return err;
}

static void alloc_mlx4_priv(void) {

	priv = calloc(1, sizeof *priv);
	priv->dev.pdev = calloc(1, sizeof(struct pci_dev));
}

static void mlx4_enable_msi_x(void) {
	/*struct msix_entry *entries;*/

	/*int nreq = min_t(int, priv->dev.caps.num_ports *
	 min_t(int, num_possible_cpus() + 1, MAX_MSIX_P_PORT)
	 + MSIX_LEGACY_SZ, MAX_MSIX);*/
	/*int err;*/
	int i;

	/*TODO: enable MSI-X*/
	/*if (msi_x) {
	 nreq = min_t(int, priv->dev.caps.num_eqs - priv->dev.caps.reserved_eqs,
	 nreq);

	 if (msi_x > 1 && !mlx4_is_mfunc(&priv->dev))
	 nreq = min_t(int, nreq, msi_x);

	 entries = kcalloc(nreq, sizeof *entries, GFP_KERNEL);
	 if (!entries)
	 goto no_msi;

	 for (i = 0; i < nreq; ++i)
	 entries[i].entry = i;

	 retry: err = pci_enable_msix(priv->dev.pdev, entries, nreq);
	 if (err) {
	 Try again if at least 2 vectors are available
	 if (err > 1) {
	 MLX4_DEBUG( "Requested %d vectors, "
	 "but only %d MSI-X vectors available, "
	 "trying again\n", nreq, err);
	 nreq = err;
	 goto retry;
	 }
	 kfree(entries);
	 if error, or can't alloc even 1 IRQ
	 if (err < 0) {
	 MLX4_DEBUG( "No IRQs left, device can't "
	 "be started.\n");
	 goto no_irq;
	 }
	 goto no_msi;
	 }

	 if (nreq <
	 MSIX_LEGACY_SZ + priv->dev.caps.num_ports * MIN_MSIX_P_PORT) {
	 Working in legacy mode , all EQ's shared
	 priv->dev.caps.comp_pool = 0;
	 priv->dev.caps.num_comp_vectors = nreq - 1;
	 } else {
	 priv->dev.caps.comp_pool = nreq - MSIX_LEGACY_SZ;
	 priv->dev.caps.num_comp_vectors = MSIX_LEGACY_SZ - 1;
	 }
	 for (i = 0; i < nreq; ++i)
	 priv->eq_table.eq[i].irq = entries[i].vector;

	 priv->dev.flags |= MLX4_FLAG_MSI_X;

	 kfree(entries);
	 return;
	 }*/

	/*no_msi:*/priv->dev.caps.num_comp_vectors = 1;
	priv->dev.caps.comp_pool = 0;

	for (i = 0; i < 2; ++i)
		priv->eq_table.eq[i].irq = priv->dev.pdev->irq;
	return;
	/*no_irq: priv->dev.caps.num_comp_vectors = 0;
	 priv->dev.caps.comp_pool = 0;
	 return;*/
}

static int mlx4_init_steering(void) {
	int num_entries = priv->dev.caps.num_ports;
	int i, j;

	priv->steer = calloc(num_entries, sizeof(struct mlx4_steer));
	if (!priv->steer)
		return -ENOMEM;

	for (i = 0; i < num_entries; i++)
		for (j = 0; j < MLX4_NUM_STEERS; j++) {
			INIT_LIST_HEAD(&priv->steer[i].promisc_qps[j]);
			INIT_LIST_HEAD(&priv->steer[i].steer_entries[j]);
		}
	return 0;
}

static int mlx4_init_counters_table(void) {
	int nent_pow2, port_indx, vf_index, num_counters;
	int res, index = 0;
	struct counter_index *new_counter_index;

	if (!(priv->dev.caps.flags & MLX4_DEV_CAP_FLAG_COUNTERS))
		return -ENOENT;

	if (!mlx4_is_slave(&priv->dev)
			&& priv->dev.caps.max_counters
					== priv->dev.caps.max_extended_counters) {
		res = mlx4_cmd(&priv->dev, MLX4_IF_STATE_EXTENDED, 0, 0,
				MLX4_CMD_SET_IF_STAT, MLX4_CMD_TIME_CLASS_A, MLX4_CMD_NATIVE);
		if (res) {
			MLX4_DEBUG("Failed to set extended counters (err=%d)\n", res);
			return res;
		}
	}

	/*mutex_init(&priv->counters_table.mutex);*/

	if (mlx4_is_slave(&priv->dev)) {
		for (port_indx = 0; port_indx < priv->dev.caps.num_ports; port_indx++) {
			INIT_LIST_HEAD(&priv->counters_table.global_port_list[port_indx]);
			if (priv->dev.caps.def_counter_index[port_indx] != 0xFF) {
				new_counter_index = malloc(sizeof(struct counter_index));
				if (!new_counter_index)
					return -ENOMEM;
				new_counter_index->index =
						priv->dev.caps.def_counter_index[port_indx];
				list_add_tail(&new_counter_index->list,
						&priv->counters_table.global_port_list[port_indx]);
			}
		}
		MLX4_DEBUG("%s: slave allocated %d counters for %d ports\n", __func__,
				priv->dev.caps.num_ports, priv->dev.caps.num_ports);
		return 0;
	}

	nent_pow2 = roundup_pow_of_two(priv->dev.caps.max_counters);

	for (port_indx = 0; port_indx < priv->dev.caps.num_ports; port_indx++) {
		INIT_LIST_HEAD(&priv->counters_table.global_port_list[port_indx]);
		/* allocating 2 counters per port for PFs */
		/* For the PF, the ETH default counters are 0,2; */
		/* and the RoCE default counters are 1,3 */
		for (num_counters = 0; num_counters < 2; num_counters++, index++) {
			new_counter_index = malloc(sizeof(struct counter_index));
			if (!new_counter_index)
				return -ENOMEM;
			new_counter_index->index = index;
			list_add_tail(&new_counter_index->list,
					&priv->counters_table.global_port_list[port_indx]);
		}
	}

	if (mlx4_is_master(&priv->dev)) {
		for (vf_index = 0; vf_index < priv->dev.num_vfs; vf_index++) {
			for (port_indx = 0; port_indx < priv->dev.caps.num_ports;
					port_indx++) {
				INIT_LIST_HEAD(
						&priv->counters_table.vf_list[vf_index][port_indx]);
				new_counter_index = malloc(sizeof(struct counter_index));
				if (!new_counter_index)
					return -ENOMEM;
				if (index < nent_pow2 - 2) {
					new_counter_index->index = index;
					index++;
				} else {
					new_counter_index->index = MLX4_SINK_COUNTER_INDEX;
				}

				list_add_tail(&new_counter_index->list,
						&priv->counters_table.vf_list[vf_index][port_indx]);
			}
		}

		res = mlx4_bitmap_init(&priv->counters_table.bitmap, nent_pow2,
				nent_pow2 - 1, index, 1);
		MLX4_DEBUG("%s: master allocated %d counters for %d VFs\n", __func__,
				index, priv->dev.num_vfs);
	} else {
		res = mlx4_bitmap_init(&priv->counters_table.bitmap, nent_pow2,
				nent_pow2 - 1, index, 1);
		MLX4_DEBUG("%s: native allocated %d counters for %d ports\n", __func__,
				index, priv->dev.caps.num_ports);
	}

	return 0;

}

static int mlx4_setup_hca(void) {
	int err;
	int port;
	__be32 ib_port_default_caps;

	err = mlx4_init_uar_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"user access region table (err=%d), aborting.\n", err);
		return err;
	}

	err = mlx4_uar_alloc(&priv->dev, &priv->driver_uar);
	if (err) {
		MLX4_DEBUG("Failed to allocate driver access region "
				"(err=%d), aborting.\n", err);
		goto err_uar_table_free;
	}

	priv->kar = (void *) (priv->driver_uar.pfn << PAGE_SHIFT);
	if (!priv->kar) {
		MLX4_DEBUG("Couldn't map kernel access region, "
				"aborting.\n");
		err = -ENOMEM;
		goto err_uar_free;
	}

	err = mlx4_init_pd_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"protection domain table (err=%d), aborting.\n", err);
		goto err_kar_unmap;
	}

	err = mlx4_init_xrcd_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"reliable connection domain table (err=%d), "
				"aborting.\n", err);
		goto err_pd_table_free;
	}

	err = mlx4_init_mr_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"memory region table (err=%d), aborting.\n", err);
		goto err_xrcd_table_free;
	}

	if (!mlx4_is_slave(&priv->dev)) {
		err = mlx4_init_mcg_table(priv);
		if (err) {
			MLX4_DEBUG("Failed to initialize "
					"multicast group table (err=%d), aborting.\n", err);
			goto err_mr_table_free;
		}
	}

	err = mlx4_init_eq_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"event queue table (err=%d), aborting.\n", err);
		goto err_mcg_table_free;
	}

	err = mlx4_cmd_use_events(priv);
	if (err) {
		MLX4_DEBUG("Failed to switch to event-driven "
				"firmware commands (err=%d), aborting.\n", err);
		goto err_eq_table_free;
	}

	err = mlx4_NOP(priv);
	if (err) {
		if (priv->dev.flags & MLX4_FLAG_MSI_X) {
			MLX4_DEBUG("NOP command failed to generate MSI-X "
					"interrupt IRQ %d).\n",
					priv->eq_table.eq[priv->dev.caps.num_comp_vectors].irq);
			MLX4_DEBUG("Trying again without MSI-X.\n");
		} else {
			MLX4_DEBUG("NOP command failed to generate interrupt "
					"(IRQ %d), aborting.\n",
					priv->eq_table.eq[priv->dev.caps.num_comp_vectors].irq);
			MLX4_DEBUG("BIOS or ACPI interrupt routing problem?\n");
		}

		goto err_cmd_poll;
	}

	MLX4_DEBUG("NOP command IRQ test passed\n");

	err = mlx4_init_cq_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"completion queue table (err=%d), aborting.\n", err);
		goto err_cmd_poll;
	}

	err = mlx4_init_srq_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"shared receive queue table (err=%d), aborting.\n", err);
		goto err_cq_table_free;
	}

	err = mlx4_init_qp_table(priv);
	if (err) {
		MLX4_DEBUG("Failed to initialize "
				"queue pair table (err=%d), aborting.\n", err);
		goto err_srq_table_free;
	}

	err = mlx4_init_counters_table();
	if (err && err != -ENOENT) {
		MLX4_DEBUG("Failed to initialize counters table (err=%d), "
				"aborting.\n", err);
		goto err_qp_table_free;
	}

	if (!mlx4_is_slave(&priv->dev)) {
		for (port = 1; port <= priv->dev.caps.num_ports; port++) {
			ib_port_default_caps = 0;
			err = mlx4_get_port_ib_caps(priv, port, &ib_port_default_caps);
			if (err)
				MLX4_DEBUG("failed to get port %d default "
						"ib capabilities (%d). Continuing "
						"with caps = 0\n", port, err);
			priv->dev.caps.ib_port_def_cap[port] = ib_port_default_caps;

			/*initialize per
			 -slave
			 default ib port capabilities*/
			/*if (mlx4_is_master(&priv->dev)) {
			 int i;
			 for (i = 0; i < priv->dev.num_slaves; i++) {
			 if (i == mlx4_master_func_num(&priv->dev))
			 continue;
			 priv->mfunc.master.slave_state[i].ib_cap_mask[port] =
			 ib_port_default_caps;
			 }
			 }*/

			priv->dev.caps.port_ib_mtu[port] = IB_MTU_4096;

			err = mlx4_SET_PORT(priv, port,
					mlx4_is_master(&priv->dev) ?
							priv->dev.caps.pkey_table_len[port] : -1);
			if (err) {
				MLX4_DEBUG("Failed to set port %d (err=%d), "
						"aborting\n", port, err);
				goto err_counters_table_free;
			}
		}
	}

	return 0;

	err_counters_table_free: /*mlx4_cleanup_counters_table(&priv->dev);*/
	err_qp_table_free: /*mlx4_cleanup_qp_table(&priv->dev);*/
	err_srq_table_free: /*mlx4_cleanup_srq_table(&priv->dev);*/
	err_cq_table_free: /*mlx4_cleanup_cq_table(&priv->dev);*/
	err_cmd_poll: /*mlx4_cmd_use_polling(&priv->dev);*/
	err_eq_table_free: /* mlx4_cleanup_eq_table(&priv->dev);*/
	err_mcg_table_free: /*if (!mlx4_is_slave(&priv->dev))
	 mlx4_cleanup_mcg_table(&priv->dev);*/
	err_mr_table_free: /*mlx4_cleanup_mr_table(&priv->dev);*/
	err_xrcd_table_free: /*mlx4_cleanup_xrcd_table(&priv->dev);*/
	err_pd_table_free: /*mlx4_cleanup_pd_table(&priv->dev);*/
	err_kar_unmap: /*iounmap(priv->kar);*/
	err_uar_free: /*mlx4_uar_free(priv, &priv->driver_uar);*/
	err_uar_table_free: /*mlx4_cleanup_uar_table(&priv->dev);*/
	return err;
}

static void mlx4_init_hca_info(void) {
	struct mlx4_hca_info *info = &priv->hca_info;

	info->priv = priv;

	/*info->firmware_attr = (struct device_attribute) __ATTR(fw_ver, S_IRUGO,
	 show_firmware_version, NULL);
	 if (device_create_file(&priv->dev.pdev->dev, &info->firmware_attr))
	 mlx4_err(dev, "Failed to add file firmware version");

	 info->hca_attr = (struct device_attribute) __ATTR(hca, S_IRUGO, show_hca,
	 NULL);
	 if (device_create_file(&priv->dev.pdev->dev, &info->hca_attr))
	 mlx4_err(dev, "Failed to add file hca type");

	 info->board_attr = (struct device_attribute) __ATTR(board_id, S_IRUGO,
	 show_board, NULL);
	 if (device_create_file(&priv->dev.pdev->dev, &info->board_attr))
	 mlx4_err(dev, "Failed to add file board id type");*/
}

static int mlx4_init_port_info(int port) {
	struct mlx4_port_info *info = &priv->port[port];
	int err = 0;

	info->priv = priv;
	info->port = port;
	if (!mlx4_is_slave(&priv->dev)) {
		mlx4_init_mac_table(priv, &info->mac_table);
		mlx4_init_vlan_table(priv, &info->vlan_table);
		info->base_qpn = mlx4_get_base_qpn(&priv->dev, port);
	}

	sprintf(info->dev_name, "mlx4_port%d", port);
	/*
	 info->port_attr.attr.name = info->dev_name;
	 if (mlx4_is_mfunc(&priv->dev))
	 info->port_attr.attr.mode = S_IRUGO;
	 else {
	 info->port_attr.attr.mode = S_IRUGO | S_IWUSR;
	 info->port_attr.store = set_port_type;
	 }
	 info->port_attr.show = show_port_type;
	 sysfs_attr_init(&info->port_attr.attr);

	 err = device_create_file(&dev->pdev->dev, &info->port_attr);
	 if (err) {
	 mlx4_err(dev, "Failed to create file for port %d\n", port);
	 info->port = -1;
	 }
	 */
	sprintf(info->dev_mtu_name, "mlx4_port%d_mtu", port);
	/*
	 info->port_mtu_attr.attr.name = info->dev_mtu_name;
	 if (mlx4_is_mfunc(&priv->dev))
	 info->port_mtu_attr.attr.mode = S_IRUGO;
	 else {
	 info->port_mtu_attr.attr.mode = S_IRUGO | S_IWUSR;
	 info->port_mtu_attr.store = set_port_ib_mtu;
	 }
	 info->port_mtu_attr.show = show_port_ib_mtu;
	 sysfs_attr_init(&info->port_mtu_attr.attr);

	 err = device_create_file(&dev->pdev->dev, &info->port_mtu_attr);
	 if (err) {
	 mlx4_err(dev, "Failed to create mtu file for port %d\n", port);
	 device_remove_file(&info->dev->pdev->dev, &info->port_attr);
	 info->port = -1;
	 }*/

	return err;
}

static void mlx4_init_fn(void *user_state, struct device_mem *bar_info,
                         int nr_allocated_bars) {
    mlx4_queue_t *queue;
    // struct mlx4_priv *priv;
	errval_t err;
	int port;

    queue = user_state;
    // priv = device->priv;
	MLX4_DEBUG("Starting hardware initialization.\n");
	err = map_bars(bar_info, nr_allocated_bars);
	if (err_is_fail(err)) {
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to map bars: %s\n", err_getstring(err));
	}

	alloc_mlx4_priv();
	priv->dev.bar_info = bar_info;
	// priv->dev.pdev->device = deviceid;
	// priv->dev.pdev->vendor = vendor;

	INIT_LIST_HEAD(&priv->dev_list);
	INIT_LIST_HEAD(&priv->ctx_list);
	/*spin_lock_init(&priv->ctx_lock);*/

	/*mutex_init(&priv->port_mutex);*/

	INIT_LIST_HEAD(&priv->pgdir_list);
	/*mutex_init(&priv->pgdir_mutex);*/

	INIT_LIST_HEAD(&priv->bf_list);
	/*mutex_init(&priv->bf_mutex);*/

	/*TODO: Here we have to verify if the SR-IOV is enabled
	 * and how may virtual functions have to be provided;
	 * Also, we have to verify if virtual or physical function*/

	err = mlx4_get_ownership();
	if (err) {
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to get HCA ownership: %s\n", err_getstring(err));
	}

	err = mlx4_reset(priv);
	if (err_is_fail(err)) {
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to reset HCA: %s\n", err_getstring(err));
	}

	err = mlx4_cmd_init(priv);
	if (err) {
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"Failed to init command interface, aborting.\n");
	}

	err = mlx4_init_hca();
	if (err) {
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to init HCA: %s\n", err_getstring(err));
	}

	/*TODO In master functions, the communication channel must be initialized
	 * after obtaining its address from fw */

	err = mlx4_alloc_eq_table(priv);
	if (err)
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to allocate EQ table: %s\n", err_getstring(err));

	priv->msix_ctl.pool_bm = 0;
	/*mutex_init(&priv->msix_ctl.pool_lock);*/

	mlx4_enable_msi_x();

	/* no MSIX and no shared IRQ */
	if (!priv->dev.caps.num_comp_vectors && !priv->dev.caps.comp_pool) {
		err = -ENOSPC;
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"no MSIX and no shared IRQ: %s\n", err_getstring(err));
	}

	/*if ((mlx4_is_mfunc(&priv->dev)) && !(priv->dev.flags & MLX4_FLAG_MSI_X)) {
	 err = -ENOSYS;
	 MLX4_DEBUG( "INTx is not supported in multi-function mode."
	 " aborting.\n");
	 goto err_free_eq;
	 }*/

	if (!mlx4_is_slave(&priv->dev)) {
		err = mlx4_init_steering();
		if (err)
			user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
					"failed to init steering: %s\n", err_getstring(err));
	}

	err = mlx4_setup_hca();
	/*if (err == -EBUSY && (priv->dev.flags & MLX4_FLAG_MSI_X)
	 && !mlx4_is_mfunc(&priv->dev)) {
	 priv->dev.flags &= ~MLX4_FLAG_MSI_X;
	 priv->dev.caps.num_comp_vectors = 1;
	 priv->dev.caps.comp_pool = 0;
	 pci_disable_msix(pdev);
	 err = mlx4_setup_hca(&priv->dev);
	 }*/

	if (err)
		user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
				"failed to setup HCA: %s\n", err_getstring(err));

	mlx4_init_quotas(priv);
	mlx4_init_hca_info();

	for (port = 1; port <= priv->dev.caps.num_ports; port++) {
		err = mlx4_init_port_info(port);
		if (err)
			user_panic_fn(__FILE__, __FUNCTION__, __LINE__,
					"failed to init ports: %s\n", err_getstring(err));
	}
	/*err = mlx4_register_device(&priv->dev);
	 if (err)
	 goto err_port;*/

	/*mlx4_request_modules(&priv->dev);

	 mlx4_sense_init(&priv->dev);
	 mlx4_start_sense(&priv->dev);*/

	priv->pci_dev_data = 0;
	/*pci_set_drvdata(pdev, dev);*/

	/*TODO replace user_panic with proper cleanup*/
	/*err_port: for (--port; port >= 1; --port)
	 mlx4_cleanup_port_info(&priv->port[port]);

	 mlx4_cleanup_counters_table(dev);
	 mlx4_cleanup_qp_table(dev);
	 mlx4_cleanup_srq_table(dev);
	 mlx4_cleanup_cq_table(dev);
	 mlx4_cmd_use_polling(dev);
	 mlx4_cleanup_eq_table(dev);
	 mlx4_cleanup_mcg_table(dev);
	 mlx4_cleanup_mr_table(dev);
	 mlx4_cleanup_xrcd_table(dev);
	 mlx4_cleanup_pd_table(dev);
	 mlx4_cleanup_uar_table(dev);

	 err_steer: if (!mlx4_is_slave(dev))
	 mlx4_clear_steering(dev);

	 err_free_eq: mlx4_free_eq_table(dev);

	 err_master_mfunc: if (mlx4_is_master(dev)) {
	 mlx4_free_resource_tracker(dev, RES_TR_FREE_STRUCTS_ONLY);
	 mlx4_multi_func_cleanup(dev);
	 }

	 if (mlx4_is_slave(dev)) {
	 kfree(dev->caps.qp0_tunnel);
	 kfree(dev->caps.qp0_proxy);
	 kfree(dev->caps.qp1_tunnel);
	 kfree(dev->caps.qp1_proxy);
	 }

	 err_close: if (dev->flags & MLX4_FLAG_MSI_X)
	 pci_disable_msix(pdev);

	 mlx4_close_hca(dev);

	 err_mfunc: if (mlx4_is_slave(dev))
	 mlx4_multi_func_cleanup(dev);

	 err_cmd: mlx4_cmd_cleanup(dev);

	 err_sriov: if (dev->flags & MLX4_FLAG_SRIOV)
	 pci_disable_sriov(pdev);

	 if (!mlx4_is_slave(dev))
	 mlx4_free_ownership(dev);

	 err_free_dev: kfree(priv);

	 err_release_regions: pci_release_regions(pdev);

	 err_disable_pdev: pci_disable_device(pdev);
	 pci_set_drvdata(pdev, NULL);
	 return err;*/

	/*INFINIBAND--------------------------------------------------------------------*/
	// mlx4_ib_add(&priv->dev);

	/*ETHERNET----------------------------------------------------------------------*/
	mlx4_en_add(&priv->dev, queue);
}

int __mlx4_counter_alloc(struct mlx4_dev *dev, int slave, int port, int *idx) {
	struct counter_index *new_counter_index;

	if (!(dev->caps.flags & MLX4_DEV_CAP_FLAG_COUNTERS))
		return -ENOENT;

	if ((slave > MLX4_MAX_NUM_VF) || (slave < 0) || (port < 0)
			|| (port > MLX4_MAX_PORTS)) {
		MLX4_DEBUG("%s: invalid slave(%d) or port(%d) index\n", __func__, slave,
				port);
		return -EINVAL;
	}

	/* handle old guest request does not support request by port index */
	if (port == 0) {
		*idx = MLX4_SINK_COUNTER_INDEX;
		MLX4_DEBUG(
				"%s: allocated default counter index %d for slave %d port %d\n",
				__func__, *idx, slave, port);
		return 0;
	}

	/*mutex_lock(&priv->counters_table.mutex);*/

	*idx = mlx4_bitmap_alloc(&priv->counters_table.bitmap);
	/* if no resources return the default counter of the slave and port */
	if (*idx == -1) {
		if (slave == 0) { /* its the ethernet counter ?????? */
			new_counter_index = list_entry(
					priv->counters_table.global_port_list[port - 1].next,
					struct counter_index, list);
		} else {
			new_counter_index = list_entry(
					priv->counters_table.vf_list[slave - 1][port - 1].next,
					struct counter_index, list);
		}

		*idx = new_counter_index->index;
		MLX4_DEBUG(
				"%s: allocated defualt counter index %d for slave %d port %d\n",
				__func__, *idx, slave, port);
		goto out;
	}

	if (slave == 0) { /* native or master */
		new_counter_index = malloc(sizeof(struct counter_index));
		if (!new_counter_index)
			goto no_mem;
		new_counter_index->index = *idx;
		list_add_tail(&new_counter_index->list,
				&priv->counters_table.global_port_list[port - 1]);
	} else {
		new_counter_index = malloc(sizeof(struct counter_index));
		if (!new_counter_index)
			goto no_mem;
		new_counter_index->index = *idx;
		list_add_tail(&new_counter_index->list,
				&priv->counters_table.vf_list[slave - 1][port - 1]);
	}

	MLX4_DEBUG("%s: allocated counter index %d for slave %d port %d\n",
			__func__, *idx, slave, port);
	out: /*mutex_unlock(&priv->counters_table.mutex);*/
	return 0;

	no_mem: /*mlx4_bitmap_free(&priv->counters_table.bitmap, *idx, MLX4_USE_RR);*/
	/*mutex_unlock(&priv->counters_table.mutex);*/
	*idx = MLX4_SINK_COUNTER_INDEX;
	MLX4_DEBUG("%s: failed err (%d)\n", __func__, -ENOMEM);
	return -ENOMEM;
}

int mlx4_counter_alloc(struct mlx4_dev *dev, u8 port, int *idx) {
	u64 out_param;
	int err;
	struct counter_index *new_counter_index, *c_index;

	if (mlx4_is_mfunc(dev)) {
		err = mlx4_cmd_imm(&priv->dev, 0, &out_param,
				((u32) port) << 8 | (u32) RES_COUNTER, RES_OP_RESERVE,
				MLX4_CMD_ALLOC_RES, MLX4_CMD_TIME_CLASS_A, MLX4_CMD_WRAPPED);
		if (!err) {
			*idx = get_param_l(&out_param);
			if (*idx == MLX4_SINK_COUNTER_INDEX)
				return -ENOSPC;

			/*mutex_lock(&priv->counters_table.mutex);*/
			c_index = list_entry(
					priv->counters_table.global_port_list[port - 1].next,
					struct counter_index, list);
			/*mutex_unlock(&priv->counters_table.mutex);*/
			if (c_index->index == *idx)
				return -EEXIST;

			if (mlx4_is_slave(dev)) {
				new_counter_index = malloc(sizeof(struct counter_index));
				if (!new_counter_index) {
					/*TODO*/
					/*mlx4_counter_free(dev, port, *idx);*/
					return -ENOMEM;
				}
				new_counter_index->index = *idx;
				/*mutex_lock(&priv->counters_table.mutex);*/
				list_add_tail(&new_counter_index->list,
						&priv->counters_table.global_port_list[port - 1]);
				/*mutex_unlock(&priv->counters_table.mutex);*/
				MLX4_DEBUG("%s: allocated counter index %d for port %d\n",
						__func__, *idx, port);
			}
		}
		return err;
	}
	return __mlx4_counter_alloc(dev, 0, port, idx);
}

static struct mlx4_eqe *get_eqe(struct mlx4_eq *eq, u32 entry, u8 eqe_factor) {
	/* (entry & (eq->nent - 1)) gives us a cyclic array */
	unsigned long offset = (entry & (eq->nent - 1))
			* (MLX4_EQ_ENTRY_SIZE << eqe_factor);
	/* CX3 is capable of extending the EQE from 32 to 64 bytes.
	 * When this feature is enabled, the first (in the lower addresses)
	 * 32 bytes in the 64 byte EQE are reserved and the next 32 bytes
	 * contain the legacy EQE information.
	 */
	return eq->page_list[offset / BASE_PAGE_SIZE].buf
			+ (offset + (eqe_factor ? MLX4_EQ_ENTRY_SIZE : 0)) % BASE_PAGE_SIZE;
}

static struct mlx4_eqe *next_eqe_sw(struct mlx4_eq *eq, u8 eqe_factor) {
	struct mlx4_eqe *eqe = get_eqe(eq, eq->cons_index, eqe_factor);
	return !!(eqe->owner & 0x80) ^ !!(eq->cons_index & eq->nent) ?
	NULL :
																	eqe;
}

static void eq_set_ci(struct mlx4_eq *eq, int req_not) {
	__raw_writel((__force u32) cpu_to_be32((eq->cons_index & 0xffffff) |
					req_not << 31),
			eq->doorbell);
	/* We still want ordering, just not swabbing, so add a barrier */
	mb();
}

static int mlx4_eq_int(struct mlx4_eq *eq) {
	struct mlx4_eqe *eqe;
	int cqn;
	int eqes_found = 0;
	int set_ci = 0;
	/*int port;
	 int slave = 0;
	 int ret;*/
	u32 flr_slave;
	/*u8 update_slave_state;
	 int i;
	 enum slave_port_gen_event gen_event;
	 unsigned long flags;
	 struct mlx4_vport_state *s_info;*/

	/*int i;
	 uint64_t *data;*/

	while ((eqe = next_eqe_sw(eq, priv->dev.caps.eqe_factor))) {
		/*
		 * Make sure we read EQ entry contents after we've
		 * checked the ownership bit.
		 */
		rmb();

		switch (eqe->type) {
		case MLX4_EVENT_TYPE_COMP:
			// MLX4_DEBUG("MLX4_EVENT_TYPE_COMP\n");
			/*assert(!"not implemented!");*/
			cqn = be32_to_cpu(eqe->event.comp.cqn) & 0xffffff;
			mlx4_cq_completion(priv, cqn);

			break;

		case MLX4_EVENT_TYPE_PATH_MIG:
		case MLX4_EVENT_TYPE_COMM_EST:
		case MLX4_EVENT_TYPE_SQ_DRAINED:
		case MLX4_EVENT_TYPE_SRQ_QP_LAST_WQE:
		case MLX4_EVENT_TYPE_WQ_CATAS_ERROR:
		case MLX4_EVENT_TYPE_PATH_MIG_FAILED:
		case MLX4_EVENT_TYPE_WQ_INVAL_REQ_ERROR:
		case MLX4_EVENT_TYPE_WQ_ACCESS_ERROR:
			MLX4_DEBUG("event %d arrived\n", eqe->type);
			assert(!"not implemented!");
			/*if (mlx4_is_master(&priv->dev)) {
			 forward only to slave owning the QP
			 ret = mlx4_get_slave_from_resource_id(priv, RES_QP,
			 be32_to_cpu(eqe->event.qp.qpn) & 0xffffff, &slave);
			 if (ret && ret != -ENOENT) {
			 MLX4_DEBUG("QP event %02x(%02x) on "
			 "EQ %d at index %u: could "
			 "not get slave id (%d)\n", eqe->type, eqe->subtype,
			 eq->eqn, eq->cons_index, ret);
			 break;
			 }

			 if (!ret && slave != priv->dev.caps.function) {
			 mlx4_slave_event(priv, slave, eqe);
			 break;
			 }

			 }*/
			/*mlx4_qp_event(priv, be32_to_cpu(eqe->event.qp.qpn) & 0xffffff,
			 eqe->type);*/
			break;

		case MLX4_EVENT_TYPE_SRQ_LIMIT:
			MLX4_DEBUG("%s: MLX4_EVENT_TYPE_SRQ_LIMIT\n", __func__);
			/* fall through */
		case MLX4_EVENT_TYPE_SRQ_CATAS_ERROR:
			assert(!"not implemented!");
			/*if (mlx4_is_master(&priv->dev)) {
			 forward only to slave owning the SRQ
			 ret = mlx4_get_slave_from_resource_id(priv, RES_SRQ,
			 be32_to_cpu(eqe->event.srq.srqn) & 0xffffff, &slave);
			 if (ret && ret != -ENOENT) {
			 MLX4_DEBUG("SRQ event %02x(%02x) "
			 "on EQ %d at index %u: could"
			 " not get slave id (%d)\n", eqe->type, eqe->subtype,
			 eq->eqn, eq->cons_index, ret);
			 break;
			 }
			 MLX4_DEBUG("%s: slave:%d, srq_no:0x%x, event: %02x(%02x)\n",
			 __func__, slave, be32_to_cpu(eqe->event.srq.srqn),
			 eqe->type, eqe->subtype);

			 if (!ret && slave != priv->dev.caps.function) {
			 MLX4_DEBUG("%s: sending event %02x(%02x) to slave:%d\n",
			 __func__, eqe->type, eqe->subtype, slave);
			 mlx4_slave_event(priv, slave, eqe);
			 break;
			 }
			 }*/
			/*mlx4_srq_event(priv, be32_to_cpu(eqe->event.srq.srqn) & 0xffffff,
			 eqe->type);*/
			break;

		case MLX4_EVENT_TYPE_CMD:
			/*got_irq = true;*/
			MLX4_DEBUG("MLX4_EVENT_TYPE_CMD\n");
			/*assert(!"not implemented!");*/
			mlx4_cmd_event(priv, be16_to_cpu(eqe->event.cmd.token),
					eqe->event.cmd.status,
					be64_to_cpu(eqe->event.cmd.out_param));
			break;

		case MLX4_EVENT_TYPE_PORT_CHANGE:
			got_port_irq = true;
			/*assert(!"not implemented!");*/
			/*port = be32_to_cpu(eqe->event.port_change.port) >> 28;*/
			if (eqe->subtype == MLX4_PORT_CHANGE_SUBTYPE_DOWN) {
				MLX4_DEBUG("MLX4_PORT_CHANGE_SUBTYPE_DOWN\n");
				/*mlx4_dispatch_event(priv, MLX4_DEV_EVENT_PORT_DOWN, port);
				 mlx4_priv(&priv->dev)->sense.do_sense_port[port] = 1;
				 if (!mlx4_is_master(&priv->dev))
				 break;
				 for (i = 0; i < priv->dev.num_slaves; i++) {
				 if (priv->dev.caps.port_type[port] == MLX4_PORT_TYPE_ETH) {
				 if (i == mlx4_master_func_num(&priv->dev))
				 continue;
				 MLX4_DEBUG(
				 "%s: Sending MLX4_PORT_CHANGE_SUBTYPE_DOWN"
				 " to slave: %d, port:%d\n", __func__, i,
				 port);
				 s_info =
				 &priv->mfunc.master.vf_oper[slave].vport[port].state;
				 if (IFLA_VF_LINK_STATE_AUTO == s_info->link_state)
				 mlx4_slave_event(priv, i, eqe);
				 } else {  IB port
				 set_and_calc_slave_port_state(priv, i, port,
				 MLX4_PORT_STATE_DEV_EVENT_PORT_DOWN,
				 &gen_event);
				 we can be in pending state, then do not send port_down event
				 if (SLAVE_PORT_GEN_EVENT_DOWN == gen_event) {
				 if (i == mlx4_master_func_num(&priv->dev))
				 continue;
				 mlx4_slave_event(priv, i, eqe);
				 }
				 }
				 }*/
			} else {
				MLX4_DEBUG("MLX4_PORT_CHANGE_SUBTYPE_UP\n");
				got_up_irq = true;
				/*mlx4_dispatch_event(priv, MLX4_DEV_EVENT_PORT_UP, port);

				 mlx4_priv(&priv->dev)->sense.do_sense_port[port] = 0;

				 if (!mlx4_is_master(&priv->dev))
				 break;
				 if (priv->dev.caps.port_type[port] == MLX4_PORT_TYPE_ETH)
				 for (i = 0; i < priv->dev.num_slaves; i++) {
				 if (i == mlx4_master_func_num(&priv->dev))
				 continue;
				 s_info =
				 &priv->mfunc.master.vf_oper[slave].vport[port].state;
				 if (IFLA_VF_LINK_STATE_AUTO == s_info->link_state)
				 mlx4_slave_event(priv, i, eqe);
				 }
				 else
				 IB port
				 port-up event will be sent to a slave when the
				 * slave's alias-guid is set. This is done in alias_GUID.c

				 set_all_slave_state(priv, port, MLX4_DEV_EVENT_PORT_UP);*/
			}
			break;

		case MLX4_EVENT_TYPE_CQ_ERROR:
			MLX4_DEBUG("CQ %s on CQN %06x\n",
					eqe->event.cq_err.syndrome == 1 ?
							"overrun" : "access violation",
					be32_to_cpu(eqe->event.cq_err.cqn) & 0xffffff);
			assert(!"not implemented!");
			/*if (mlx4_is_master(&priv->dev)) {
			 ret = mlx4_get_slave_from_resource_id(priv, RES_CQ,
			 be32_to_cpu(eqe->event.cq_err.cqn) & 0xffffff, &slave);
			 if (ret && ret != -ENOENT) {
			 MLX4_DEBUG("CQ event %02x(%02x) on "
			 "EQ %d at index %u: could "
			 "not get slave id (%d)\n", eqe->type, eqe->subtype,
			 eq->eqn, eq->cons_index, ret);
			 break;
			 }

			 if (!ret && slave != priv->dev.caps.function) {
			 mlx4_slave_event(priv, slave, eqe);
			 break;
			 }
			 }
			 mlx4_cq_event(priv, be32_to_cpu(eqe->event.cq_err.cqn) & 0xffffff,
			 eqe->type);*/
			break;

		case MLX4_EVENT_TYPE_EQ_OVERFLOW:
			MLX4_DEBUG("EQ overrun on EQN %d\n", eq->eqn);
			break;

		case MLX4_EVENT_TYPE_OP_REQUIRED:
			assert(!"not implemented!");
			/*atomic_inc(&priv->opreq_count);
			 FW commands can't be executed from interrupt context
			 working in deferred task
			 queue_work(mlx4_wq, &priv->opreq_task);*/
			break;

		case MLX4_EVENT_TYPE_COMM_CHANNEL:
			if (!mlx4_is_master(&priv->dev)) {
				MLX4_DEBUG("Received comm channel event "
						"for non master device\n");
				break;
			}
			assert(!"not implemented!");
			/*memcpy(&priv->mfunc.master.comm_arm_bit_vector,
			 eqe->event.comm_channel_arm.bit_vec,
			 sizeof eqe->event.comm_channel_arm.bit_vec);

			 if (!queue_work(priv->mfunc.master.comm_wq,
			 &priv->mfunc.master.comm_work))
			 MLX4_DEBUG("Failed to queue comm channel work\n");

			 if (!queue_work(priv->mfunc.master.comm_wq,
			 &priv->mfunc.master.arm_comm_work))
			 MLX4_DEBUG("Failed to queue arm comm channel work\n");*/
			break;

		case MLX4_EVENT_TYPE_FLR_EVENT:
			flr_slave = be32_to_cpu(eqe->event.flr_event.slave_id);
			if (!mlx4_is_master(&priv->dev)) {
				MLX4_DEBUG("Non-master function received"
						"FLR event\n");
				break;
			}

			assert(!"not implemented!");

			/*MLX4_DEBUG("FLR event for slave: %d\n", flr_slave);

			 if (flr_slave >= priv->dev.num_slaves) {
			 MLX4_DEBUG("Got FLR for unknown function: %d\n", flr_slave);
			 update_slave_state = 0;
			 } else
			 update_slave_state = 1;

			 spin_lock_irqsave(&priv->mfunc.master.slave_state_lock, flags);
			 if (update_slave_state) {
			 priv->mfunc.master.slave_state[flr_slave].active = false;
			 priv->mfunc.master.slave_state[flr_slave].last_cmd =
			 MLX4_COMM_CMD_FLR;
			 priv->mfunc.master.slave_state[flr_slave].is_slave_going_down =
			 1;
			 }
			 spin_unlock_irqrestore(&priv->mfunc.master.slave_state_lock, flags);
			 queue_work(priv->mfunc.master.comm_wq,
			 &priv->mfunc.master.slave_flr_event_work);*/
			break;

		case MLX4_EVENT_TYPE_FATAL_WARNING:
			assert(!"not implemented!");
			/*if (eqe->subtype == MLX4_FATAL_WARNING_SUBTYPE_WARMING) {
			 if (mlx4_is_master(&priv->dev))
			 for (i = 0; i < priv->dev.num_slaves; i++) {
			 MLX4_DEBUG("%s: Sending "
			 "MLX4_FATAL_WARNING_SUBTYPE_WARMING"
			 " to slave: %d\n", __func__, i);
			 if (i == priv->dev.caps.function)
			 continue;
			 mlx4_slave_event(priv, i, eqe);
			 }
			 MLX4_DEBUG("Temperature Threshold was reached! "
			 "Threshold: %d celsius degrees; "
			 "Current Temperature: %d\n",
			 be16_to_cpu(eqe->event.warming.warning_threshold),
			 be16_to_cpu(eqe->event.warming.current_temperature));
			 } else
			 MLX4_DEBUG("Unhandled event FATAL WARNING (%02x), "
			 "subtype %02x on EQ %d at index %u. owner=%x, "
			 "nent=0x%x, slave=%x, ownership=%s\n", eqe->type,
			 eqe->subtype, eq->eqn, eq->cons_index, eqe->owner,
			 eq->nent, eqe->slave_id,
			 !!(eqe->owner & 0x80) ^ !!(eq->cons_index & eq->nent) ?
			 "HW" : "SW");*/

			break;

		case MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT:
			MLX4_DEBUG("MLX4_EVENT_TYPE_PORT_MNG_CHG_EVENT\n");
			/*assert(!"not implemented!");*/
			/*mlx4_dispatch_event(priv, MLX4_DEV_EVENT_PORT_MGMT_CHANGE,
			 (unsigned long) eqe);*/
			break;

		case MLX4_EVENT_TYPE_RECOVERABLE_ERROR_EVENT:
			assert(!"not implemented!");
			/*switch (eqe->subtype) {
			 case MLX4_RECOVERABLE_ERROR_EVENT_SUBTYPE_BAD_CABLE:
			 MLX4_DEBUG("Bad cable detected on port %u\n",
			 eqe->event.bad_cable.port);
			 break;
			 case MLX4_RECOVERABLE_ERROR_EVENT_SUBTYPE_UNSUPPORTED_CABLE:
			 MLX4_DEBUG("Unsupported cable detected\n");
			 break;
			 default:
			 MLX4_DEBUG("Unhandled recoverable error event "
			 "detected: %02x(%02x) on EQ %d at index %u. "
			 "owner=%x, nent=0x%x, ownership=%s\n", eqe->type,
			 eqe->subtype, eq->eqn, eq->cons_index, eqe->owner,
			 eq->nent,
			 !!(eqe->owner & 0x80) ^ !!(eq->cons_index & eq->nent) ?
			 "HW" : "SW");
			 break;
			 }*/
			break;

		case MLX4_EVENT_TYPE_EEC_CATAS_ERROR:
		case MLX4_EVENT_TYPE_ECC_DETECT:
		default:
			MLX4_DEBUG("Unhandled event %02x(%02x) on EQ %d at "
					"index %u. owner=%x, nent=0x%x, slave=%x, "
					"ownership=%s\n", eqe->type, eqe->subtype, eq->eqn,
					eq->cons_index, eqe->owner, eq->nent, eqe->slave_id,
					!!(eqe->owner & 0x80) ^ !!(eq->cons_index & eq->nent) ?
							"HW" : "SW");
			break;
		};

		++eq->cons_index;
		eqes_found = 1;
		++set_ci;

		/*
		 * The HCA will think the queue has overflowed if we
		 * don't tell it we've been processing events.  We
		 * create our EQs with MLX4_NUM_SPARE_EQE extra
		 * entries, so we must update our consumer index at
		 * least that often.
		 */
		if (set_ci >= MLX4_NUM_SPARE_EQE) {/*unlikely*/
			eq_set_ci(eq, 0);
			set_ci = 0;
		}
	}

	eq_set_ci(eq, 1);

	return eqes_found;
}

static void mlx4_interrupt_handler_fn(void *arg) {
    mlx4_queue_t *device = arg;
	int i;

    // debug_printf("%s.%d: %p\n", __func__, __LINE__, device->isr);
	__raw_writel(priv->eq_table.clr_mask, priv->eq_table.clr_int);

	for (i = 0; i < priv->dev.caps.num_comp_vectors + 1; ++i)
		mlx4_eq_int(&priv->eq_table.eq[i]);
    if (device->isr)
        device->isr(device);
}

static void mlx4_reregister_handler(void *arg) {
    mlx4_queue_t *device = arg;
	errval_t err;
	printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);
	err = pci_reregister_irq_for_device(PCI_CLASS_ETHERNET,
                                        PCI_DONT_CARE, PCI_DONT_CARE,
                                        device->pci_vendor, device->pci_deviceid,
                                        device->pci_bus, device->pci_device,
                                        device->pci_function,
                                        mlx4_interrupt_handler_fn, device,
                                        mlx4_reregister_handler, device);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "pci_reregister_irq_for_device");
	}

	return;
}

static errval_t mlx4_register(struct devq* q, struct capref cap,
                                  regionid_t rid)
{
    mlx4_queue_t *device = (mlx4_queue_t *)q;
    struct frame_identity id;
    errval_t err;
    
    err = invoke_frame_identify(cap, &id);
    assert(err_is_ok(err));
    assert(!device->region_id);
    device->region_id = rid;
    device->region_base = id.base;
    device->region_size = id.bytes;
    
    err = vspace_map_one_frame_attr(&device->region_mapped, id.bytes, cap, VREGION_FLAGS_READ_WRITE, NULL, NULL);
    assert(err_is_ok(err));
    return SYS_ERR_OK;
}

static errval_t mlx4_deregister(struct devq* q, regionid_t rid)
{
    return SYS_ERR_OK;
}


static errval_t mlx4_control(struct devq* q, uint64_t cmd, uint64_t value,
                                 uint64_t *result)
{
    mlx4_queue_t *device = (mlx4_queue_t *)q;
    *result = device->mac_address;
    return SYS_ERR_OK;
}


static errval_t mlx4_enqueue(struct devq* q, regionid_t rid,
                                 genoffset_t offset, genoffset_t length,
                                 genoffset_t valid_data, genoffset_t valid_length,
                                 uint64_t flags)
{
    mlx4_queue_t *device = (mlx4_queue_t *)q;
    errval_t err;
    
    // debug_printf("%s: %lx:%ld:%ld:%ld\n", __func__, offset, length, valid_data, valid_length);
    if (flags & NETIF_RXFLAG) {
        /* can not enqueue receive buffer larger than 2048 bytes */
        assert(length <= 2048);
        
        err = mlx4_en_enqueue_rx(device, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (flags & NETIF_TXFLAG) {
        assert(length <= BASE_PAGE_SIZE);
    
        err = mlx4_en_enqueue_tx(device, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        printf("Unknown buffer flags \n");
        return NIC_ERR_ENQUEUE;
    }

    return SYS_ERR_OK;
}

static errval_t mlx4_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                                 genoffset_t* length, genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    mlx4_queue_t *device = (mlx4_queue_t *)q;
    
    if (mlx4_en_dequeue_tx(device, rid, offset, length, valid_data, valid_length, flags) == SYS_ERR_OK)
        return SYS_ERR_OK;
    if (mlx4_en_dequeue_rx(device, rid, offset, length, valid_data, valid_length, flags) == SYS_ERR_OK)
        return SYS_ERR_OK;
    // debug_printf("%s:%s:\n", device->name, __func__);
    return DEVQ_ERR_QUEUE_EMPTY;
}

static errval_t mlx4_notify(struct devq* q)
{
    assert(0);
    return SYS_ERR_OK;
}

static errval_t mlx4_destroy(struct devq * queue)
{
    mlx4_queue_t* q = (mlx4_queue_t *) queue;
    free(q);
    // TODO rest of the cleanup
    return SYS_ERR_OK;
}


errval_t mlx4_queue_create(mlx4_queue_t ** q, uint32_t vendor, uint32_t deviceid,
    uint32_t bus, uint32_t pci_device, uint32_t function, unsigned interrupt_mode,
    void (*isr)(void *))
{
    errval_t err;
    mlx4_queue_t *device;
    
    device = malloc(sizeof(mlx4_queue_t));
    assert(device);

    device->pci_vendor = vendor;
    device->pci_deviceid = deviceid;
    device->pci_bus = bus;
    device->pci_device = pci_device;
    device->pci_function = function;
    device->name = malloc(128);
    snprintf(device->name, 128, "mlx4:%x:%x:%x:%x:%x", vendor, deviceid, bus,
        pci_device, function);

    device->region_id = 0;
    device->isr = isr;
    
	err = pci_client_connect();
	assert(err_is_ok(err));

    err = pci_register_driver_movable_irq(mlx4_init_fn, device, PCI_CLASS_ETHERNET,
                                          PCI_DONT_CARE, PCI_DONT_CARE,
                                          device->pci_vendor, device->pci_deviceid,
                                          device->pci_bus, device->pci_device,
                                          device->pci_function,
                                          mlx4_interrupt_handler_fn, device,
                                          mlx4_reregister_handler, device);
	assert(err_is_ok(err));

    err = devq_init(&device->q, false);
    assert(err_is_ok(err));
    
    device->q.f.enq = mlx4_enqueue;
    device->q.f.deq = mlx4_dequeue;
    device->q.f.reg = mlx4_register;
    device->q.f.dereg = mlx4_deregister;
    device->q.f.ctrl = mlx4_control;
    device->q.f.notify = mlx4_notify;
    device->q.f.destroy = mlx4_destroy;
    
    *q = device;

    return SYS_ERR_OK;
}
