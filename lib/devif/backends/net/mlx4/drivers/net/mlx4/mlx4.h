#define MLX4_PCI_VENDOR_ID 0x15b3

/*
 * Copyright (c) 2004, 2005 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2005, 2006, 2007 Cisco Systems.  All rights reserved.
 * Copyright (c) 2005, 2006, 2007, 2008, 2014 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2004 Voltaire, Inc. All rights reserved.
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
 */

#ifndef MLX4_H
#define MLX4_H

/*#include <linux/mutex.h>*/
/*
 #include <linux/radix-tree.h>
 #include <linux/timer.h>
 #include <linux/semaphore.h>
 */
/*#include <linux/workqueue.h>*/
#include <linux/device.h>
#include <linux/io-mapping.h>
#include <linux/mlx4/device.h>

#include <barrelfish/thread_sync.h>

#include <linux/rbtree.h>

#include <sys/_pthreadtypes.h>

#include <asm/atomic.h>
/*#include <linux/mlx4/driver.h>*/
/*
 #include <linux/mlx4/doorbell.h>
 #include <linux/mlx4/cmd.h>
 */

#define DRV_NAME	"mlx4_core"
#define PFX		DRV_NAME ": "
#define DRV_VERSION	"2.1.6"
#define DRV_RELDATE	__DATE__

#define DRV_STACK_NAME		"Linux-MLNX_OFED"
#define DRV_STACK_VERSION	"2.1"
#define DRV_NAME_FOR_FW		DRV_STACK_NAME","DRV_STACK_VERSION

#define MLX4_FS_UDP_UC_EN		(1 << 1)
#define MLX4_FS_TCP_UC_EN		(1 << 2)
#define MLX4_FS_NUM_OF_L2_ADDR		8
#define MLX4_FS_MGM_LOG_ENTRY_SIZE	7
#define MLX4_FS_NUM_MCG			(1 << 17)
/*
 struct mlx4_set_port_prio2tc_context {
 uint8_t prio2tc[4];
 };

 struct mlx4_port_scheduler_tc_cfg_be {
 __be16 pg;
 __be16 bw_precentage;
 __be16 max_bw_units;  3-100Mbps, 4-1Gbps, other values - reserved
 __be16 max_bw_value;
 };

 struct mlx4_set_port_scheduler_context {
 struct mlx4_port_scheduler_tc_cfg_be tc[MLX4_NUM_TC];
 };
 */

extern bool got_irq;

enum {
	MLX4_HCR_BASE = 0x80680,
	MLX4_HCR_SIZE = 0x0001c,
	MLX4_CLR_INT_SIZE = 0x00008,
	MLX4_SLAVE_COMM_BASE = 0x0,
	MLX4_COMM_PAGESIZE = 0x1000,
	MLX4_CLOCK_SIZE = 0x00008
};

enum {
	MLX4_DEFAULT_MGM_LOG_ENTRY_SIZE = 10,
	MLX4_MIN_MGM_LOG_ENTRY_SIZE = 7,
	MLX4_MAX_MGM_LOG_ENTRY_SIZE = 12,
	MLX4_MAX_QP_PER_MGM = 4 * ((1 << MLX4_MAX_MGM_LOG_ENTRY_SIZE) / 16 - 2),
};

enum {
	MLX4_NUM_PDS = 1 << 15
};

enum {
	MLX4_CMPT_TYPE_QP = 0,
	MLX4_CMPT_TYPE_SRQ = 1,
	MLX4_CMPT_TYPE_CQ = 2,
	MLX4_CMPT_TYPE_EQ = 3,
	MLX4_CMPT_NUM_TYPE
};

enum {
	MLX4_CMPT_SHIFT = 24, MLX4_NUM_CMPTS = MLX4_CMPT_NUM_TYPE << MLX4_CMPT_SHIFT
};

enum mlx4_mpt_state {
	MLX4_MPT_DISABLED = 0, MLX4_MPT_EN_HW, MLX4_MPT_EN_SW
};

#define MLX4_COMM_TIME		10000
enum {
	MLX4_COMM_CMD_RESET,
	MLX4_COMM_CMD_VHCR0,
	MLX4_COMM_CMD_VHCR1,
	MLX4_COMM_CMD_VHCR2,
	MLX4_COMM_CMD_VHCR_EN,
	MLX4_COMM_CMD_VHCR_POST,
	MLX4_COMM_CMD_FLR = 254
};

/*The flag indicates that the slave should delay the RESET cmd*/
#define MLX4_DELAY_RESET_SLAVE 0xbbbbbbb
/*indicates how many retries will be done if we are in the middle of FLR*/
#define NUM_OF_RESET_RETRIES	10
#define SLEEP_TIME_IN_RESET	(2 * 1000)

enum mlx4_resource {
	RES_QP,
	RES_CQ,
	RES_SRQ,
	RES_XRCD,
	RES_MPT,
	RES_MTT,
	RES_MAC,
	RES_VLAN,
	RES_NPORT_ID,
	RES_COUNTER,
	RES_FS_RULE,
	RES_EQ,
	MLX4_NUM_OF_RESOURCE_TYPE
};

enum mlx4_alloc_mode {
	RES_OP_RESERVE, RES_OP_RESERVE_AND_MAP, RES_OP_MAP_ICM,
};

enum mlx4_res_tracker_free_type {
	RES_TR_FREE_ALL, RES_TR_FREE_SLAVES_ONLY, RES_TR_FREE_STRUCTS_ONLY,
};

/*
 *Virtual HCR structures.
 * mlx4_vhcr is the sw representation, in machine endianess
 *
 * mlx4_vhcr_cmd is the formalized structure, the one that is passed
 * to FW to go through communication channel.
 * It is big endian, and has the same structure as the physical HCR
 * used by command interface

 struct mlx4_vhcr {
 uint64_t	in_param;
 uint64_t	out_param;
 uint32_t	in_modifier;
 uint32_t	errno;
 u16	op;
 u16	token;
 uint8_t	op_modifier;
 uint8_t	e_bit;
 };

 struct mlx4_vhcr_cmd {
 __be64 in_param;
 __be32 in_modifier;
 uint32_t reserved1;
 __be64 out_param;
 __be16 token;
 u16 reserved;
 uint8_t status;
 uint8_t flags;
 __be16 opcode;
 } __packed;

 struct mlx4_cmd_info {
 u16 opcode;
 bool has_inbox;
 bool has_outbox;
 bool out_is_imm;
 bool encode_slave_id;
 bool skip_err_print;
 int (*verify)(struct mlx4_dev *dev, int slave, struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox);
 int (*wrapper)(struct mlx4_dev *dev, int slave, struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 };

 enum {
 MLX4_DEBUG_MASK_CMD_TIME = 0x100,
 };

 #ifdef CONFIG_MLX4_DEBUG
 extern int mlx4_debug_level;
 #else  CONFIG_MLX4_DEBUG
 #define mlx4_debug_level	(0)
 #endif  CONFIG_MLX4_DEBUG

 #define mlx4_dbg(mdev, format, arg...)					\
do {									\
	if (mlx4_debug_level)						\
		dev_printk(KERN_DEBUG, &mdev->pdev->dev, format, ##arg); \
} while (0)

 #define mlx4_err(mdev, format, arg...) \
	dev_err(&mdev->pdev->dev, format, ##arg)
 #define mlx4_info(mdev, format, arg...) \
	dev_info(&mdev->pdev->dev, format, ##arg)
 #define mlx4_warn(mdev, format, arg...) \
	dev_warn(&mdev->pdev->dev, format, ##arg)

 extern int mlx4_log_num_mgm_entry_size;
 */
extern int log_mtts_per_seg;
extern int mlx4_blck_lb;
/*
 extern int mlx4_set_4k_mtu;
 */
#define MLX4_MAX_NUM_SLAVES	(MLX4_MAX_NUM_PF + MLX4_MAX_NUM_VF)
#define ALL_SLAVES 0xff

struct mlx4_bitmap {
	uint32_t last;
	uint32_t top;
	uint32_t max;
	uint32_t reserved_top;
	uint32_t mask;
	uint32_t avail;
	struct thread_mutex lock;
	unsigned long *table;
};

struct mlx4_buddy {
	unsigned long **bits;
	unsigned int *num_free;
	uint32_t max_order;
	spinlock_t lock;
};

struct mlx4_icm;

struct mlx4_icm_table {
	uint64_t virt;
	int num_icm;
	uint32_t num_obj;
	int obj_size;
	int lowmem;
	int coherent;
	struct thread_mutex mutex;
	struct mlx4_icm **icm;
};

#define MLX4_MPT_FLAG_SW_OWNS	    (0xfUL << 28)
#define MLX4_MPT_FLAG_FREE	    (0x3UL << 28)
#define MLX4_MPT_FLAG_MIO	    (1 << 17)
#define MLX4_MPT_FLAG_BIND_ENABLE   (1 << 15)
#define MLX4_MPT_FLAG_PHYSICAL	    (1 <<  9)
#define MLX4_MPT_FLAG_REGION	    (1 <<  8)

#define MLX4_MPT_PD_FLAG_FAST_REG   (1 << 27)
#define MLX4_MPT_PD_FLAG_RAE	    (1 << 28)
#define MLX4_MPT_PD_FLAG_EN_INV	    (3 << 24)

#define MLX4_MPT_QP_FLAG_BOUND_QP   (1 << 7)

#define MLX4_MPT_STATUS_SW		0xF0
#define MLX4_MPT_STATUS_HW		0x00

/** Must be packed because mtt_seg is 64 bits but only aligned to 32 bits.*/

struct mlx4_mpt_entry {
	__be32 flags;
	__be32 qpn;
	__be32 key;
	__be32 pd_flags;
	__be64 start;
	__be64 length;
	__be32 lkey;
	__be32 win_cnt;
	uint8_t reserved1[3];
	uint8_t mtt_rep;
	__be64 mtt_addr;
	__be32 mtt_sz;
	__be32 entity_size;
	__be32 first_byte_offset;
} __packed;

/** Must be packed because start is 64 bits but only aligned to 32 bits.*/

struct mlx4_eq_context {
	__be32 flags;
	u16 reserved1[3];
	__be16 page_offset;
	uint8_t log_eq_size;
	uint8_t reserved2[4];
	uint8_t eq_period;
	uint8_t reserved3;
	uint8_t eq_max_count;
	uint8_t reserved4[3];
	uint8_t intr;
	uint8_t log_page_size;
	uint8_t reserved5[2];
	uint8_t mtt_base_addr_h;
	__be32 mtt_base_addr_l;
	uint32_t reserved6[2];
	__be32 consumer_index;
	__be32 producer_index;
	uint32_t reserved7[4];
};

struct mlx4_cq_context {
	__be32 flags;
	u16 reserved1[3];
	__be16 page_offset;
	__be32 logsize_usrpage;
	__be16 cq_period;
	__be16 cq_max_count;
	uint8_t reserved2[3];
	uint8_t comp_eqn;
	uint8_t log_page_size;
	uint8_t reserved3[2];
	uint8_t mtt_base_addr_h;
	__be32 mtt_base_addr_l;
	__be32 last_notified_index;
	__be32 solicit_producer_index;
	__be32 consumer_index;
	__be32 producer_index;
	uint32_t reserved4[2];
	__be64 db_rec_addr;
};

struct mlx4_srq_context {
	__be32 state_logsize_srqn;
	uint8_t logstride;
	uint8_t reserved1;
	__be16 xrcd;
	__be32 pg_offset_cqn;
	uint32_t reserved2;
	uint8_t log_page_size;
	uint8_t reserved3[2];
	uint8_t mtt_base_addr_h;
	__be32 mtt_base_addr_l;
	__be32 pd;
	__be16 limit_watermark;
	__be16 wqe_cnt;
	u16 reserved4;
	__be16 wqe_counter;
	uint32_t reserved5;
	__be64 db_rec_addr;
};

struct mlx4_eq {
	struct mlx4_priv *priv;
	void /*__iomem*/*doorbell;
	int eqn;
	uint32_t cons_index;
	u16 irq;
	u16 have_irq;
	int nent;
	struct mlx4_buf_list *page_list;
	struct mlx4_mtt mtt;
};

struct mlx4_slave_eqe {
	uint8_t type;
	uint8_t port;
	uint32_t param;
};

struct mlx4_slave_event_eq_info {
	int eqn;
	u16 token;
};

struct mlx4_profile {
	int num_qp;
	int rdmarc_per_qp;
	int num_srq;
	int num_cq;
	int num_mcg;
	int num_mpt;
	unsigned num_mtt_segs;
};

struct mlx4_fw {
	uint64_t clr_int_base;
	uint64_t catas_offset;
	uint64_t comm_base;
	uint64_t clock_offset;
	struct mlx4_icm *fw_icm;
	struct mlx4_icm *aux_icm;
	uint32_t catas_size;
	u16 fw_pages;
	uint8_t clr_int_bar;
	uint8_t catas_bar;
	uint8_t comm_bar;
	uint8_t clock_bar;
};
/*
 struct mlx4_comm {
 uint32_t			slave_write;
 uint32_t			slave_read;
 };

 enum {
 MLX4_MCAST_CONFIG       = 0,
 MLX4_MCAST_DISABLE      = 1,
 MLX4_MCAST_ENABLE       = 2,
 };

 #define VLAN_FLTR_SIZE	128

 struct mlx4_vlan_fltr {
 __be32 entry[VLAN_FLTR_SIZE];
 };

 struct mlx4_mcast_entry {
 struct list_head list;
 uint64_t addr;
 };
 */
struct mlx4_promisc_qp {
	struct list_head list;
	uint32_t qpn;
};

struct mlx4_steer_index {
	struct list_head list;
	unsigned int index;
	struct list_head duplicates;
};
/*
 #define MLX4_EVENT_TYPES_NUM 64

 struct mlx4_slave_state {
 uint8_t comm_toggle;
 uint8_t last_cmd;
 uint8_t init_port_mask;
 bool active;
 bool old_vlan_api;
 uint8_t function;
 dma_addr_t vhcr_dma;
 u16 mtu[MLX4_MAX_PORTS + 1];
 __be32 ib_cap_mask[MLX4_MAX_PORTS + 1];
 struct mlx4_slave_eqe eq[MLX4_MFUNC_MAX_EQES];
 struct list_head mcast_filters[MLX4_MAX_PORTS + 1];
 struct mlx4_vlan_fltr *vlan_filter[MLX4_MAX_PORTS + 1];
 event type to eq number lookup
 struct mlx4_slave_event_eq_info event_eq[MLX4_EVENT_TYPES_NUM];
 u16 eq_pi;
 u16 eq_ci;
 spinlock_t lock;
 initialized via the kzalloc
 uint8_t is_slave_going_down;
 uint32_t cookie;
 enum slave_port_state port_state[MLX4_MAX_PORTS + 1];
 };

 #define MLX4_VGT 4095
 #define NO_INDX  (-1)


 struct mlx4_vport_state {
 uint64_t mac;
 u16 default_vlan;
 uint8_t  default_qos;
 uint32_t tx_rate;
 bool spoofchk;
 uint32_t link_state;
 };

 struct mlx4_vf_admin_state {
 struct mlx4_vport_state vport[MLX4_MAX_PORTS + 1];
 };

 struct mlx4_vport_oper_state {
 struct mlx4_vport_state state;
 int mac_idx;
 int vlan_idx;
 };
 struct mlx4_vf_oper_state {
 struct mlx4_vport_oper_state vport[MLX4_MAX_PORTS + 1];
 };

 struct slave_list {
 struct mutex mutex;
 struct list_head res_list[MLX4_NUM_OF_RESOURCE_TYPE];
 };

 struct resource_allocator {
 spinlock_t alloc_lock;
 union {
 int res_reserved;
 int res_port_rsvd[MLX4_MAX_PORTS];
 };
 union {
 int res_free;
 int res_port_free[MLX4_MAX_PORTS];
 };
 int *quota;
 int *allocated;
 int *guaranteed;
 };

 struct mlx4_resource_tracker {
 spinlock_t lock;
 tree for each resources
 struct rb_root res_tree[MLX4_NUM_OF_RESOURCE_TYPE];
 num_of_slave's lists, one per slave
 struct slave_list *slave_list;
 struct resource_allocator res_alloc[MLX4_NUM_OF_RESOURCE_TYPE];
 };

 #define SLAVE_EVENT_EQ_SIZE	128
 struct mlx4_slave_event_eq {
 uint32_t eqn;
 uint32_t cons;
 uint32_t prod;
 spinlock_t event_lock;
 struct mlx4_eqe event_eqe[SLAVE_EVENT_EQ_SIZE];
 };

 struct mlx4_master_qp0_state {
 int proxy_qp0_active;
 int qp0_active;
 int port_active;
 };

 struct mlx4_mfunc_master_ctx {
 struct mlx4_slave_state *slave_state;
 struct mlx4_vf_admin_state *vf_admin;
 struct mlx4_vf_oper_state *vf_oper;
 struct mlx4_master_qp0_state qp0_state[MLX4_MAX_PORTS + 1];
 int			init_port_ref[MLX4_MAX_PORTS + 1];
 u16			max_mtu[MLX4_MAX_PORTS + 1];
 int			disable_mcast_ref[MLX4_MAX_PORTS + 1];
 struct mlx4_resource_tracker res_tracker;
 struct workqueue_struct *comm_wq;
 struct work_struct	comm_work;
 struct work_struct	arm_comm_work;
 struct work_struct	slave_event_work;
 struct work_struct	slave_flr_event_work;
 spinlock_t		slave_state_lock;
 __be32			comm_arm_bit_vector[4];
 struct mlx4_eqe		cmd_eqe;
 struct mlx4_slave_event_eq slave_eq;
 struct mutex		gen_eqe_mutex[MLX4_MFUNC_MAX];
 };

 struct mlx4_mfunc {
 struct mlx4_comm		       *comm;
 struct mlx4_vhcr_cmd	       *vhcr;
 lpaddr_t						vhcr_dma;

 struct mlx4_mfunc_master_ctx	master;
 };
 */
#define MGM_QPN_MASK       0x00FFFFFF
#define MGM_BLCK_LB_BIT    30

struct mlx4_mgm {
	__be32 next_gid_index;
	__be32 members_count;
	uint32_t reserved[2];
	uint8_t gid[16];
	__be32 qp[MLX4_MAX_QP_PER_MGM];
};

struct mlx4_cmd {
	struct pci_pool *pool;
	void *hcr;
	struct thread_mutex hcr_mutex;
	struct thread_mutex slave_cmd_mutex;
	struct thread_sem poll_sem;
	struct thread_sem event_sem;
	int max_cmds;
	struct thread_mutex context_lock;
	int free_head;
	struct mlx4_cmd_context *context;
	u16 token_mask;
	uint8_t use_events;
	uint8_t toggle;
	uint8_t comm_toggle;
};
/*
 enum {
 MLX4_VF_IMMED_VLAN_FLAG_VLAN = 1 << 0,
 MLX4_VF_IMMED_VLAN_FLAG_QOS = 1 << 1,
 };
 struct mlx4_vf_immed_vlan_work {
 struct work_struct	work;
 struct mlx4_priv	*priv;
 int			flags;
 int			slave;
 int			vlan_ix;
 int			orig_vlan_ix;
 uint8_t			port;
 uint8_t			qos;
 u16			vlan_id;
 u16			orig_vlan_id;
 };

 */
struct mlx4_uar_table {
	struct mlx4_bitmap bitmap;
};

struct mlx4_mr_table {
	struct mlx4_bitmap mpt_bitmap;
	struct mlx4_buddy mtt_buddy;
	uint64_t mtt_base;
	uint64_t mpt_base;
	struct mlx4_icm_table mtt_table;
	struct mlx4_icm_table dmpt_table;
};

struct mlx4_cq_table {
	struct mlx4_bitmap bitmap;
	spinlock_t lock;
	pthread_rwlock_t cq_table_lock;
	struct radix_tree_root tree;
	struct mlx4_icm_table table;
	struct mlx4_icm_table cmpt_table;
};

struct mlx4_eq_table {
	struct mlx4_bitmap bitmap;
	char *irq_names;
	void *clr_int; /*__iomem*/
	void **uar_map; /*__iomem*/
	uint32_t clr_mask;
	struct mlx4_eq *eq;
	struct mlx4_icm_table table;
	struct mlx4_icm_table cmpt_table;
	int have_irq;
	uint8_t inta_pin;
};

struct mlx4_srq_table {
	struct mlx4_bitmap bitmap;
	spinlock_t lock;
	struct radix_tree_root tree;
	struct mlx4_icm_table table;
	struct mlx4_icm_table cmpt_table;
};

struct mlx4_qp_table {
	struct mlx4_bitmap bitmap;
	uint32_t rdmarc_base;
	int rdmarc_shift;
	spinlock_t lock;
	struct mlx4_icm_table qp_table;
	struct mlx4_icm_table auxc_table;
	struct mlx4_icm_table altc_table;
	struct mlx4_icm_table rdmarc_table;
	struct mlx4_icm_table cmpt_table;
};

struct mlx4_mcg_table {
	struct thread_mutex mutex;
	struct mlx4_bitmap bitmap;
	struct mlx4_icm_table table;
};
/*
 struct mlx4_catas_err {
 uint32_t __iomem	*map;
 struct timer_list	timer;
 struct list_head	list;
 };

 */
#define MLX4_MAX_MAC_NUM	128
#define MLX4_MAC_TABLE_SIZE	(MLX4_MAX_MAC_NUM << 3)

struct mlx4_mac_table {
	__be64 entries[MLX4_MAX_MAC_NUM];
	int refs[MLX4_MAX_MAC_NUM];
	struct thread_mutex mutex;
	int total;
	int max;
};

#define MLX4_MAX_VLAN_NUM	128
/*
 #define MLX4_VLAN_TABLE_SIZE	(MLX4_MAX_VLAN_NUM << 2)
 */
struct mlx4_vlan_table {
	__be32 entries[MLX4_MAX_VLAN_NUM];
	int refs[MLX4_MAX_VLAN_NUM];
	struct thread_mutex mutex;
	int total;
	int max;
};

#define SET_PORT_GEN_ALL_VALID		0x7
#define SET_PORT_PROMISC_SHIFT		31
#define SET_PORT_MC_PROMISC_SHIFT	30

enum {
	MCAST_DIRECT_ONLY = 0, MCAST_DIRECT = 1, MCAST_DEFAULT = 2
};

struct mlx4_set_port_general_context {
	uint8_t reserved[3];
	uint8_t flags;
	u16 reserved2;
	__be16 mtu;
	uint8_t pptx;
	uint8_t pfctx;
	u16 reserved3;
	uint8_t pprx;
	uint8_t pfcrx;
	u16 reserved4;
};

struct mlx4_set_port_rqp_calc_context {
	__be32 base_qpn;
	uint8_t rererved;
	uint8_t n_mac;
	uint8_t n_vlan;
	uint8_t n_prio;
	uint8_t reserved2[3];
	uint8_t mac_miss;
	uint8_t intra_no_vlan;
	uint8_t no_vlan;
	uint8_t intra_vlan_miss;
	uint8_t vlan_miss;
	uint8_t reserved3[3];
	uint8_t no_vlan_prio;
	__be32 promisc;
	__be32 mcast;
};

struct mlx4_hca_info {
	struct mlx4_priv *priv;
/*	struct device_attribute firmware_attr;
 struct device_attribute hca_attr;
 struct device_attribute board_attr;*/
};

struct mlx4_port_info {
	struct mlx4_priv *priv;
	int port;
	char dev_name[16];
	/*struct device_attribute port_attr;*/
	enum mlx4_port_type tmp_type;
	char dev_mtu_name[16];
	/*struct device_attribute port_mtu_attr;*/
	struct mlx4_mac_table mac_table;
	struct mlx4_vlan_table vlan_table;
	int base_qpn;
};

struct mlx4_sense {
	struct mlx4_priv *priv;
	uint8_t do_sense_port[MLX4_MAX_PORTS + 1];
	uint8_t sense_allowed[MLX4_MAX_PORTS + 1];
/*struct delayed_work	sense_poll;*/
};

struct mlx4_msix_ctl {
	uint64_t pool_bm;
	struct thread_mutex pool_lock;
};

struct mlx4_steer {
	struct list_head promisc_qps[MLX4_NUM_STEERS];
	struct list_head steer_entries[MLX4_NUM_STEERS];
};

enum {
	MLX4_PCI_DEV_IS_VF = 1 << 0, MLX4_PCI_DEV_FORCE_SENSE_PORT = 1 << 1,
};

struct mlx4_roce_gid_entry {
	uint8_t raw[16];
};

struct counter_index {
	struct list_head list;
	uint32_t index;
};

struct mlx4_counters {
	struct mlx4_bitmap bitmap;
	struct list_head global_port_list[MLX4_MAX_PORTS];
	struct list_head vf_list[MLX4_MAX_NUM_VF][MLX4_MAX_PORTS];
	struct thread_mutex mutex;
};
/*
 enum {
 MLX4_NO_RR	= 0,
 MLX4_USE_RR	= 1,
 };
 */
struct mlx4_priv {
	struct mlx4_dev dev;

	struct list_head dev_list;
	struct list_head ctx_list;
	spinlock_t ctx_lock;

	int pci_dev_data;

	struct list_head pgdir_list;
	struct thread_mutex pgdir_mutex;

	struct mlx4_fw fw;
	struct mlx4_cmd cmd;
	/*struct mlx4_mfunc	mfunc;*/

	struct mlx4_bitmap pd_bitmap;
	struct mlx4_bitmap xrcd_bitmap;
	struct mlx4_uar_table uar_table;
	struct mlx4_mr_table mr_table;
	struct mlx4_cq_table cq_table;
	struct mlx4_eq_table eq_table;
	struct mlx4_srq_table srq_table;
	struct mlx4_qp_table qp_table;
	struct mlx4_mcg_table mcg_table;
	struct mlx4_counters counters_table;

	/*struct mlx4_catas_err	catas_err;*/

	void *clr_base; /*__iomem*/

	struct mlx4_uar driver_uar;
	void *kar; /*__iomem*/
	struct mlx4_port_info port[MLX4_MAX_PORTS + 1];
	struct mlx4_hca_info hca_info;
	struct mlx4_sense sense;
	struct thread_mutex port_mutex;
	struct mlx4_msix_ctl msix_ctl;
	struct mlx4_steer *steer;
	struct list_head bf_list;
	struct thread_mutex bf_mutex;
	/*struct io_mapping*/void *bf_mapping;
	void *clock_mapping; /*__iomem*/
	int reserved_mtts;
	int fs_hash_mode;
	uint8_t virt2phys_pkey[MLX4_MFUNC_MAX][MLX4_MAX_PORTS][MLX4_MAX_PORT_PKEYS];
	__be64 slave_node_guids[MLX4_MFUNC_MAX];
	struct mlx4_roce_gid_entry roce_gids[MLX4_MAX_PORTS][128];
	atomic_t opreq_count;
/*struct work_struct	opreq_task;*/
};

static inline struct mlx4_priv *mlx4_priv(struct mlx4_dev *dev) {
	return container_of(dev, struct mlx4_priv, dev);
}
#define MLX4_SENSE_RANGE	(HZ * 3)
/*
 extern struct workqueue_struct *mlx4_wq;
 */
uint32_t mlx4_bitmap_alloc(struct mlx4_bitmap *bitmap);
/*
 void mlx4_bitmap_free(struct mlx4_bitmap *bitmap, uint32_t obj, int use_rr);
 */
uint32_t mlx4_bitmap_alloc_range(struct mlx4_bitmap *bitmap, int cnt, int align,
		uint32_t skip_mask);
void mlx4_bitmap_free_range(struct mlx4_bitmap *bitmap, uint32_t obj, int cnt,
		int use_rr);
uint32_t mlx4_bitmap_avail(struct mlx4_bitmap *bitmap);
int mlx4_bitmap_init(struct mlx4_bitmap *bitmap, uint32_t num, uint32_t mask,
		uint32_t reserved_bot, uint32_t resetrved_top);
void mlx4_bitmap_cleanup(struct mlx4_bitmap *bitmap);
int mlx4_reset(struct mlx4_priv *priv);
int mlx4_alloc_eq_table(struct mlx4_priv *priv);
/*
 void mlx4_free_eq_table(struct mlx4_priv *priv);
 */
int mlx4_init_pd_table(struct mlx4_priv *priv);
int mlx4_init_xrcd_table(struct mlx4_priv *priv);
int mlx4_init_uar_table(struct mlx4_priv *priv);
int mlx4_init_mr_table(struct mlx4_priv *priv);
int mlx4_init_eq_table(struct mlx4_priv *priv);
int mlx4_init_cq_table(struct mlx4_priv *priv);
int mlx4_init_qp_table(struct mlx4_priv *priv);
int mlx4_init_srq_table(struct mlx4_priv *priv);
int mlx4_init_mcg_table(struct mlx4_priv *priv);
/*
 void mlx4_cleanup_pd_table(struct mlx4_dev *dev);
 void mlx4_cleanup_xrcd_table(struct mlx4_dev *dev);
 void mlx4_cleanup_uar_table(struct mlx4_dev *dev);
 void mlx4_cleanup_mr_table(struct mlx4_dev *dev);
 void mlx4_cleanup_eq_table(struct mlx4_dev *dev);
 void mlx4_cleanup_cq_table(struct mlx4_dev *dev);
 void mlx4_cleanup_qp_table(struct mlx4_dev *dev);
 void mlx4_cleanup_srq_table(struct mlx4_dev *dev);
 void mlx4_cleanup_mcg_table(struct mlx4_dev *dev);
 */
int __mlx4_qp_alloc_icm(struct mlx4_dev *dev, int qpn);
/*
 void __mlx4_qp_free_icm(struct mlx4_dev *dev, int qpn);
 */
int __mlx4_cq_alloc_icm(struct mlx4_dev *dev, int *cqn);
/*
 void __mlx4_cq_free_icm(struct mlx4_dev *dev, int cqn);
 int __mlx4_srq_alloc_icm(struct mlx4_dev *dev, int *srqn);
 void __mlx4_srq_free_icm(struct mlx4_dev *dev, int srqn);
 */
int __mlx4_mpt_reserve(struct mlx4_priv *priv);
/*
 void __mlx4_mpt_release(struct mlx4_dev *dev, uint32_t index);
 */
int __mlx4_mpt_alloc_icm(struct mlx4_dev *dev, uint32_t index);
/*
 void __mlx4_mpt_free_icm(struct mlx4_dev *dev, uint32_t index);
 */
uint32_t __mlx4_alloc_mtt_range(struct mlx4_priv *priv, int order);
/*
 void __mlx4_free_mtt_range(struct mlx4_dev *dev, uint32_t first_seg, int order);

 int mlx4_WRITE_MTT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SYNC_TPT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SW2HW_MPT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_HW2SW_MPT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_MPT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SW2HW_EQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_DMA_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 */
int __mlx4_qp_reserve_range(struct mlx4_priv *priv, int cnt, int align,
		int *base, uint8_t flags);
/*
 void __mlx4_qp_release_range(struct mlx4_dev *dev, int base_qpn, int cnt);
 */
int __mlx4_register_mac(struct mlx4_dev *dev, uint8_t port, uint64_t mac);
/*
 void __mlx4_unregister_mac(struct mlx4_dev *dev, uint8_t port, uint64_t mac);
 */
int __mlx4_write_mtt(struct mlx4_priv *priv, struct mlx4_mtt *mtt,
		int start_index, int npages, uint64_t *page_list);
int __mlx4_counter_alloc(struct mlx4_dev *dev, int slave, int port, int *idx);
/*
 void __mlx4_counter_free(struct mlx4_dev *dev, int slave, int port, uint32_t idx);

 int __mlx4_slave_counters_free(struct mlx4_dev *dev, int slave);
 int __mlx4_clear_if_stat(struct mlx4_dev *dev,
 uint8_t counter_index);
 uint8_t mlx4_get_default_counter_index(struct mlx4_dev *dev, int slave, int port);

 int __mlx4_xrcd_alloc(struct mlx4_dev *dev, uint32_t *xrcdn);
 void __mlx4_xrcd_free(struct mlx4_dev *dev, uint32_t xrcdn);

 void mlx4_start_catas_poll(struct mlx4_dev *dev);
 void mlx4_stop_catas_poll(struct mlx4_dev *dev);
 void mlx4_catas_init(void);
 int mlx4_restart_one(struct pci_dev *pdev);
 int mlx4_register_device(struct mlx4_dev *dev);
 void mlx4_unregister_device(struct mlx4_dev *dev);
 void mlx4_dispatch_event(struct mlx4_dev *dev, enum mlx4_dev_event type,
 unsigned long param);
 */
struct mlx4_dev_cap;
struct mlx4_init_hca_param;

uint64_t mlx4_make_profile(struct mlx4_priv *priv, struct mlx4_profile *request,
		struct mlx4_dev_cap *dev_cap, struct mlx4_init_hca_param *init_hca);
/*
 void mlx4_master_comm_channel(struct work_struct *work);
 void mlx4_master_arm_comm_channel(struct work_struct *work);
 void mlx4_gen_slave_eqe(struct work_struct *work);
 void mlx4_master_handle_slave_flr(struct work_struct *work);

 int mlx4_ALLOC_RES_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_FREE_RES_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_MAP_EQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr, struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_COMM_INT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_HW2SW_EQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_EQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SW2HW_CQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_HW2SW_CQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_CQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_MODIFY_CQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SW2HW_SRQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_HW2SW_SRQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_SRQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_ARM_SRQ_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_GEN_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_RST2INIT_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_INIT2INIT_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_INIT2RTR_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_RTR2RTS_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_RTS2RTS_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SQERR2RTS_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_2ERR_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_RTS2SQD_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SQD2SQD_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SQD2RTS_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_2RST_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_QP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);

 int mlx4_GEN_EQE(struct mlx4_dev *dev, int slave, struct mlx4_eqe *eqe);
 */
int mlx4_cmd_init(struct mlx4_priv *priv);
/*
 void mlx4_cmd_cleanup(struct mlx4_dev *dev);
 int mlx4_multi_func_init(struct mlx4_dev *dev);
 void mlx4_multi_func_cleanup(struct mlx4_dev *dev);
 */
void mlx4_cmd_event(struct mlx4_priv *priv, u16 token, uint8_t status,
		uint64_t out_param);
int mlx4_cmd_use_events(struct mlx4_priv *priv);
/*
 void mlx4_cmd_use_polling(struct mlx4_dev *dev);

 int mlx4_comm_cmd(struct mlx4_dev *dev, uint8_t cmd, u16 param,
 unsigned long timeout);
 */
void mlx4_cq_completion(struct mlx4_priv *priv, uint32_t cqn);
/*
 void mlx4_cq_event(struct mlx4_dev *dev, uint32_t cqn, int event_type);
 */
void mlx4_qp_event(struct mlx4_priv *priv, uint32_t qpn, int event_type);
void mlx4_srq_event(struct mlx4_priv *priv, uint32_t srqn, int event_type);
/*
 void mlx4_handle_catas_err(struct mlx4_dev *dev);

 int mlx4_SENSE_PORT(struct mlx4_dev *dev, int port,
 enum mlx4_port_type *type);
 void mlx4_do_sense_ports(struct mlx4_dev *dev,
 enum mlx4_port_type *stype,
 enum mlx4_port_type *defaults);
 void mlx4_start_sense(struct mlx4_dev *dev);
 void mlx4_stop_sense(struct mlx4_dev *dev);
 void mlx4_sense_init(struct mlx4_dev *dev);
 int mlx4_check_port_params(struct mlx4_dev *dev,
 enum mlx4_port_type *port_type);
 int mlx4_change_port_types(struct mlx4_dev *dev,
 enum mlx4_port_type *port_types);
 */
void mlx4_init_mac_table(struct mlx4_priv *priv, struct mlx4_mac_table *table);
void mlx4_init_vlan_table(struct mlx4_priv *priv, struct mlx4_vlan_table *table);
/*
 void __mlx4_unregister_vlan(struct mlx4_dev *dev, uint8_t port, u16 vlan);
 int __mlx4_register_vlan(struct mlx4_dev *dev, uint8_t port, u16 vlan, int *index);
 */
int mlx4_SET_PORT(struct mlx4_priv *priv, uint8_t port, int pkey_tbl_sz);
/*
 resource tracker functions
 int mlx4_get_slave_from_resource_id(struct mlx4_dev *dev,
 enum mlx4_resource resource_type,
 uint64_t resource_id, int *slave);
 void mlx4_delete_all_resources_for_slave(struct mlx4_dev *dev, int slave_id);
 int mlx4_init_resource_tracker(struct mlx4_dev *dev);

 void mlx4_free_resource_tracker(struct mlx4_dev *dev,
 enum mlx4_res_tracker_free_type type);

 int mlx4_QUERY_FW_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SET_PORT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_INIT_PORT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_CLOSE_PORT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_DEV_CAP_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_PORT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 */
int mlx4_get_port_ib_caps(struct mlx4_priv *priv, uint8_t port, __be32 *caps);
/*
 int mlx4_get_slave_pkey_gid_tbl_len(struct mlx4_dev *dev, uint8_t port,
 int *gid_tbl_len, int *pkey_tbl_len);

 int mlx4_QP_ATTACH_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);

 int mlx4_PROMISC_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_qp_detach_common(struct mlx4_dev *dev, struct mlx4_qp *qp, uint8_t gid[16],
 enum mlx4_protocol prot, enum mlx4_steer_type steer);
 */
int mlx4_qp_attach_common(struct mlx4_dev *dev, struct mlx4_qp *qp,
		uint8_t gid[16], int block_mcast_loopback, enum mlx4_protocol prot,
		enum mlx4_steer_type steer);
int mlx4_trans_to_dmfs_attach(struct mlx4_dev *dev, struct mlx4_qp *qp,
		uint8_t gid[16], uint8_t port, int block_mcast_loopback,
		enum mlx4_protocol prot, uint64_t *reg_id);
/*
 int mlx4_SET_MCAST_FLTR(struct mlx4_dev *dev, uint8_t port, uint64_t mac, uint64_t clear, uint8_t mode);
 int mlx4_SET_MCAST_FLTR_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_SET_VLAN_FLTR_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_common_set_vlan_fltr(struct mlx4_dev *dev, int function,
 int port, void *buf);
 int mlx4_DUMP_ETH_STATS_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_PKEY_TABLE_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QUERY_IF_STAT_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QP_FLOW_STEERING_ATTACH_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_QP_FLOW_STEERING_DETACH_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 int mlx4_MOD_STAT_CFG_wrapper(struct mlx4_dev *dev, int slave,
 struct mlx4_vhcr *vhcr,
 struct mlx4_cmd_mailbox *inbox,
 struct mlx4_cmd_mailbox *outbox,
 struct mlx4_cmd_info *cmd);
 */
int mlx4_get_mgm_entry_size(struct mlx4_dev *dev);
int mlx4_get_qp_per_mgm(struct mlx4_dev *dev);

static inline void set_param_l(uint64_t *arg, uint32_t val) {
	*arg = (*arg & 0xffffffff00000000ULL) | (uint64_t) val;
}

static inline void set_param_h(uint64_t *arg, uint32_t val) {
	*arg = (*arg & 0xffffffff) | ((uint64_t) val << 32);
}

static inline uint32_t get_param_l(uint64_t *arg) {
	return (uint32_t) (*arg & 0xffffffff);
}
/*
 static inline uint32_t get_param_h(uint64_t *arg)
 {
 return (uint32_t)(*arg >> 32);
 }

 static inline spinlock_t *mlx4_tlock(struct mlx4_dev *dev)
 {
 return &mlx4_priv(dev)->mfunc.master.res_tracker.lock;
 }
 */
#define NOT_MASKED_PD_BITS 17
/*
 void sys_tune_init(void);
 void sys_tune_fini(void);
 */
void mlx4_init_quotas(struct mlx4_priv *priv);
/*
 int mlx4_get_slave_num_gids(struct mlx4_dev *dev, int slave);
 int mlx4_get_base_gid_ix(struct mlx4_dev *dev, int slave);
 void mlx4_vf_immed_vlan_work_handler(struct work_struct *_work);
 */

#endif  /*MLX4_H*/
