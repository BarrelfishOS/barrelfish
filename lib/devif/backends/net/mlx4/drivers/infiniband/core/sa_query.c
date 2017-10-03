/*
 * Copyright (c) 2004 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005 Voltaire, Inc.  All rights reserved.
 * Copyright (c) 2006 Intel Corporation.  All rights reserved.
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

#include <linux/module.h>
#include <linux/err.h>
#include <linux/random.h>
#include <linux/spinlock.h>
#include <linux/slab.h>
#include <linux/dma-mapping.h>
#include <linux/kref.h>
#include <linux/idr.h>
#include <linux/workqueue.h>

#include <rdma/ib_pack.h>
#include <rdma/ib_cache.h>
#include "sa.h"

MODULE_AUTHOR("Roland Dreier");
MODULE_DESCRIPTION("InfiniBand subnet administration query support");
MODULE_LICENSE("Dual BSD/GPL");

struct ib_sa_sm_ah {
	struct ib_ah        *ah;
	struct kref          ref;
	u16		     pkey_index;
	u8		     src_path_mask;
};

struct ib_sa_port {
	struct ib_mad_agent *agent;
	struct ib_mad_agent *notice_agent;
	struct ib_sa_sm_ah  *sm_ah;
	struct work_struct   update_task;
	spinlock_t           ah_lock;
	u8                   port_num;
	struct ib_device    *device;
};

struct ib_sa_device {
	int                     start_port, end_port;
	struct ib_event_handler event_handler;
	struct ib_sa_port port[0];
};

struct ib_sa_query {
	void (*callback)(struct ib_sa_query *, int, struct ib_sa_mad *);
	void (*release)(struct ib_sa_query *);
	struct ib_sa_client    *client;
	struct ib_sa_port      *port;
	struct ib_mad_send_buf *mad_buf;
	struct ib_sa_sm_ah     *sm_ah;
	int			id;
};

struct ib_sa_service_query {
	void (*callback)(int, struct ib_sa_service_rec *, void *);
	void *context;
	struct ib_sa_query sa_query;
};

struct ib_sa_path_query {
	void (*callback)(int, struct ib_sa_path_rec *, void *);
	void *context;
	struct ib_sa_query sa_query;
};

struct ib_sa_mcmember_query {
	void (*callback)(int, struct ib_sa_mcmember_rec *, void *);
	void *context;
	struct ib_sa_query sa_query;
};

struct ib_sa_inform_query {
	void (*callback)(int, struct ib_sa_inform *, void *);
	void *context;
	struct ib_sa_query sa_query;
};

static void ib_sa_add_one(struct ib_device *device);
static void ib_sa_remove_one(struct ib_device *device);

static struct ib_client sa_client = {
	.name   = "sa",
	.add    = ib_sa_add_one,
	.remove = ib_sa_remove_one
};

static spinlock_t idr_lock;
static DEFINE_IDR(query_idr);

static spinlock_t tid_lock;
static u32 tid;

#define PATH_REC_FIELD(field) \
	.struct_offset_bytes = offsetof(struct ib_sa_path_rec, field),		\
	.struct_size_bytes   = sizeof ((struct ib_sa_path_rec *) 0)->field,	\
	.field_name          = "sa_path_rec:" #field

static const struct ib_field path_rec_table[] = {
	{ PATH_REC_FIELD(service_id),
	  .offset_words = 0,
	  .offset_bits  = 0,
	  .size_bits    = 64 },
	{ PATH_REC_FIELD(dgid),
	  .offset_words = 2,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ PATH_REC_FIELD(sgid),
	  .offset_words = 6,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ PATH_REC_FIELD(dlid),
	  .offset_words = 10,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ PATH_REC_FIELD(slid),
	  .offset_words = 10,
	  .offset_bits  = 16,
	  .size_bits    = 16 },
	{ PATH_REC_FIELD(raw_traffic),
	  .offset_words = 11,
	  .offset_bits  = 0,
	  .size_bits    = 1 },
	{ RESERVED,
	  .offset_words = 11,
	  .offset_bits  = 1,
	  .size_bits    = 3 },
	{ PATH_REC_FIELD(flow_label),
	  .offset_words = 11,
	  .offset_bits  = 4,
	  .size_bits    = 20 },
	{ PATH_REC_FIELD(hop_limit),
	  .offset_words = 11,
	  .offset_bits  = 24,
	  .size_bits    = 8 },
	{ PATH_REC_FIELD(traffic_class),
	  .offset_words = 12,
	  .offset_bits  = 0,
	  .size_bits    = 8 },
	{ PATH_REC_FIELD(reversible),
	  .offset_words = 12,
	  .offset_bits  = 8,
	  .size_bits    = 1 },
	{ PATH_REC_FIELD(numb_path),
	  .offset_words = 12,
	  .offset_bits  = 9,
	  .size_bits    = 7 },
	{ PATH_REC_FIELD(pkey),
	  .offset_words = 12,
	  .offset_bits  = 16,
	  .size_bits    = 16 },
	{ PATH_REC_FIELD(qos_class),
	  .offset_words = 13,
	  .offset_bits  = 0,
	  .size_bits    = 12 },
	{ PATH_REC_FIELD(sl),
	  .offset_words = 13,
	  .offset_bits  = 12,
	  .size_bits    = 4 },
	{ PATH_REC_FIELD(mtu_selector),
	  .offset_words = 13,
	  .offset_bits  = 16,
	  .size_bits    = 2 },
	{ PATH_REC_FIELD(mtu),
	  .offset_words = 13,
	  .offset_bits  = 18,
	  .size_bits    = 6 },
	{ PATH_REC_FIELD(rate_selector),
	  .offset_words = 13,
	  .offset_bits  = 24,
	  .size_bits    = 2 },
	{ PATH_REC_FIELD(rate),
	  .offset_words = 13,
	  .offset_bits  = 26,
	  .size_bits    = 6 },
	{ PATH_REC_FIELD(packet_life_time_selector),
	  .offset_words = 14,
	  .offset_bits  = 0,
	  .size_bits    = 2 },
	{ PATH_REC_FIELD(packet_life_time),
	  .offset_words = 14,
	  .offset_bits  = 2,
	  .size_bits    = 6 },
	{ PATH_REC_FIELD(preference),
	  .offset_words = 14,
	  .offset_bits  = 8,
	  .size_bits    = 8 },
	{ RESERVED,
	  .offset_words = 14,
	  .offset_bits  = 16,
	  .size_bits    = 48 },
};

#define MCMEMBER_REC_FIELD(field) \
	.struct_offset_bytes = offsetof(struct ib_sa_mcmember_rec, field),	\
	.struct_size_bytes   = sizeof ((struct ib_sa_mcmember_rec *) 0)->field,	\
	.field_name          = "sa_mcmember_rec:" #field

static const struct ib_field mcmember_rec_table[] = {
	{ MCMEMBER_REC_FIELD(mgid),
	  .offset_words = 0,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ MCMEMBER_REC_FIELD(port_gid),
	  .offset_words = 4,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ MCMEMBER_REC_FIELD(qkey),
	  .offset_words = 8,
	  .offset_bits  = 0,
	  .size_bits    = 32 },
	{ MCMEMBER_REC_FIELD(mlid),
	  .offset_words = 9,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ MCMEMBER_REC_FIELD(mtu_selector),
	  .offset_words = 9,
	  .offset_bits  = 16,
	  .size_bits    = 2 },
	{ MCMEMBER_REC_FIELD(mtu),
	  .offset_words = 9,
	  .offset_bits  = 18,
	  .size_bits    = 6 },
	{ MCMEMBER_REC_FIELD(traffic_class),
	  .offset_words = 9,
	  .offset_bits  = 24,
	  .size_bits    = 8 },
	{ MCMEMBER_REC_FIELD(pkey),
	  .offset_words = 10,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ MCMEMBER_REC_FIELD(rate_selector),
	  .offset_words = 10,
	  .offset_bits  = 16,
	  .size_bits    = 2 },
	{ MCMEMBER_REC_FIELD(rate),
	  .offset_words = 10,
	  .offset_bits  = 18,
	  .size_bits    = 6 },
	{ MCMEMBER_REC_FIELD(packet_life_time_selector),
	  .offset_words = 10,
	  .offset_bits  = 24,
	  .size_bits    = 2 },
	{ MCMEMBER_REC_FIELD(packet_life_time),
	  .offset_words = 10,
	  .offset_bits  = 26,
	  .size_bits    = 6 },
	{ MCMEMBER_REC_FIELD(sl),
	  .offset_words = 11,
	  .offset_bits  = 0,
	  .size_bits    = 4 },
	{ MCMEMBER_REC_FIELD(flow_label),
	  .offset_words = 11,
	  .offset_bits  = 4,
	  .size_bits    = 20 },
	{ MCMEMBER_REC_FIELD(hop_limit),
	  .offset_words = 11,
	  .offset_bits  = 24,
	  .size_bits    = 8 },
	{ MCMEMBER_REC_FIELD(scope),
	  .offset_words = 12,
	  .offset_bits  = 0,
	  .size_bits    = 4 },
	{ MCMEMBER_REC_FIELD(join_state),
	  .offset_words = 12,
	  .offset_bits  = 4,
	  .size_bits    = 4 },
	{ MCMEMBER_REC_FIELD(proxy_join),
	  .offset_words = 12,
	  .offset_bits  = 8,
	  .size_bits    = 1 },
	{ RESERVED,
	  .offset_words = 12,
	  .offset_bits  = 9,
	  .size_bits    = 23 },
};

#define SERVICE_REC_FIELD(field) \
	.struct_offset_bytes = offsetof(struct ib_sa_service_rec, field),	\
	.struct_size_bytes   = sizeof ((struct ib_sa_service_rec *) 0)->field,	\
	.field_name          = "sa_service_rec:" #field

static const struct ib_field service_rec_table[] = {
	{ SERVICE_REC_FIELD(id),
	  .offset_words = 0,
	  .offset_bits  = 0,
	  .size_bits    = 64 },
	{ SERVICE_REC_FIELD(gid),
	  .offset_words = 2,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ SERVICE_REC_FIELD(pkey),
	  .offset_words = 6,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ SERVICE_REC_FIELD(lease),
	  .offset_words = 7,
	  .offset_bits  = 0,
	  .size_bits    = 32 },
	{ SERVICE_REC_FIELD(key),
	  .offset_words = 8,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ SERVICE_REC_FIELD(name),
	  .offset_words = 12,
	  .offset_bits  = 0,
	  .size_bits    = 64*8 },
	{ SERVICE_REC_FIELD(data8),
	  .offset_words = 28,
	  .offset_bits  = 0,
	  .size_bits    = 16*8 },
	{ SERVICE_REC_FIELD(data16),
	  .offset_words = 32,
	  .offset_bits  = 0,
	  .size_bits    = 8*16 },
	{ SERVICE_REC_FIELD(data32),
	  .offset_words = 36,
	  .offset_bits  = 0,
	  .size_bits    = 4*32 },
	{ SERVICE_REC_FIELD(data64),
	  .offset_words = 40,
	  .offset_bits  = 0,
	  .size_bits    = 2*64 },
};

#define INFORM_FIELD(field) \
	.struct_offset_bytes = offsetof(struct ib_sa_inform, field), \
	.struct_size_bytes   = sizeof ((struct ib_sa_inform *) 0)->field, \
	.field_name          = "sa_inform:" #field

static const struct ib_field inform_table[] = {
	{ INFORM_FIELD(gid),
	  .offset_words = 0,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
	{ INFORM_FIELD(lid_range_begin),
	  .offset_words = 4,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ INFORM_FIELD(lid_range_end),
	  .offset_words = 4,
	  .offset_bits  = 16,
	  .size_bits    = 16 },
	{ RESERVED,
	  .offset_words = 5,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ INFORM_FIELD(is_generic),
	  .offset_words = 5,
	  .offset_bits  = 16,
	  .size_bits    = 8 },
	{ INFORM_FIELD(subscribe),
	  .offset_words = 5,
	  .offset_bits  = 24,
	  .size_bits    = 8 },
	{ INFORM_FIELD(type),
	  .offset_words = 6,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ INFORM_FIELD(trap.generic.trap_num),
	  .offset_words = 6,
	  .offset_bits  = 16,
	  .size_bits    = 16 },
	{ INFORM_FIELD(trap.generic.qpn),
	  .offset_words = 7,
	  .offset_bits  = 0,
	  .size_bits    = 24 },
	{ RESERVED,
	  .offset_words = 7,
	  .offset_bits  = 24,
	  .size_bits    = 3 },
	{ INFORM_FIELD(trap.generic.resp_time),
	  .offset_words = 7,
	  .offset_bits  = 27,
	  .size_bits    = 5 },
	{ RESERVED,
	  .offset_words = 8,
	  .offset_bits  = 0,
	  .size_bits    = 8 },
	{ INFORM_FIELD(trap.generic.producer_type),
	  .offset_words = 8,
	  .offset_bits  = 8,
	  .size_bits    = 24 },
};

#define NOTICE_FIELD(field) \
	.struct_offset_bytes = offsetof(struct ib_sa_notice, field), \
	.struct_size_bytes   = sizeof ((struct ib_sa_notice *) 0)->field, \
	.field_name          = "sa_notice:" #field

static const struct ib_field notice_table[] = {
	{ NOTICE_FIELD(is_generic),
	  .offset_words = 0,
	  .offset_bits  = 0,
	  .size_bits    = 1 },
	{ NOTICE_FIELD(type),
	  .offset_words = 0,
	  .offset_bits  = 1,
	  .size_bits    = 7 },
	{ NOTICE_FIELD(trap.generic.producer_type),
	  .offset_words = 0,
	  .offset_bits  = 8,
	  .size_bits    = 24 },
	{ NOTICE_FIELD(trap.generic.trap_num),
	  .offset_words = 1,
	  .offset_bits  = 0,
	  .size_bits    = 16 },
	{ NOTICE_FIELD(issuer_lid),
	  .offset_words = 1,
	  .offset_bits  = 16,
	  .size_bits    = 16 },
	{ NOTICE_FIELD(notice_toggle),
	  .offset_words = 2,
	  .offset_bits  = 0,
	  .size_bits    = 1 },
	{ NOTICE_FIELD(notice_count),
	  .offset_words = 2,
	  .offset_bits  = 1,
	  .size_bits    = 15 },
	{ NOTICE_FIELD(data_details),
	  .offset_words = 2,
	  .offset_bits  = 16,
	  .size_bits    = 432 },
	{ NOTICE_FIELD(issuer_gid),
	  .offset_words = 16,
	  .offset_bits  = 0,
	  .size_bits    = 128 },
};

int ib_sa_check_selector(ib_sa_comp_mask comp_mask,
			 ib_sa_comp_mask selector_mask,
			 ib_sa_comp_mask value_mask,
			 u8 selector, u8 src_value, u8 dst_value)
{
	int err;

	if (!(comp_mask & selector_mask) || !(comp_mask & value_mask))
		return 0;

	switch (selector) {
	case IB_SA_GT:
		err = (src_value <= dst_value);
		break;
	case IB_SA_LT:
		err = (src_value >= dst_value);
		break;
	case IB_SA_EQ:
		err = (src_value != dst_value);
		break;
	default:
		err = 0;
		break;
	}

	return err;
}

int ib_sa_pack_attr(void *dst, void *src, int attr_id)
{
	switch (attr_id) {
	case IB_SA_ATTR_PATH_REC:
		ib_pack(path_rec_table, ARRAY_SIZE(path_rec_table), src, dst);
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

int ib_sa_unpack_attr(void *dst, void *src, int attr_id)
{
	switch (attr_id) {
	case IB_SA_ATTR_PATH_REC:
		ib_unpack(path_rec_table, ARRAY_SIZE(path_rec_table), src, dst);
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

static void free_sm_ah(struct kref *kref)
{
	struct ib_sa_sm_ah *sm_ah = container_of(kref, struct ib_sa_sm_ah, ref);

	ib_destroy_ah(sm_ah->ah);
	kfree(sm_ah);
}

static void update_sm_ah(struct work_struct *work)
{
	struct ib_sa_port *port =
		container_of(work, struct ib_sa_port, update_task);
	struct ib_sa_sm_ah *new_ah;
	struct ib_port_attr port_attr;
	struct ib_ah_attr   ah_attr;

	if (ib_query_port(port->agent->device, port->port_num, &port_attr)) {
		printk(KERN_WARNING "Couldn't query port\n");
		return;
	}

	new_ah = kmalloc(sizeof *new_ah, GFP_KERNEL);
	if (!new_ah) {
		printk(KERN_WARNING "Couldn't allocate new SM AH\n");
		return;
	}

	kref_init(&new_ah->ref);
	new_ah->src_path_mask = (1 << port_attr.lmc) - 1;

	new_ah->pkey_index = 0;
	if (ib_find_pkey(port->agent->device, port->port_num,
			 IB_DEFAULT_PKEY_FULL, &new_ah->pkey_index))
		printk(KERN_ERR "Couldn't find index for default PKey\n");

	memset(&ah_attr, 0, sizeof ah_attr);
	ah_attr.dlid     = port_attr.sm_lid;
	ah_attr.sl       = port_attr.sm_sl;
	ah_attr.port_num = port->port_num;

	new_ah->ah = ib_create_ah(port->agent->qp->pd, &ah_attr);
	if (IS_ERR(new_ah->ah)) {
		printk(KERN_WARNING "Couldn't create new SM AH\n");
		kfree(new_ah);
		return;
	}

	spin_lock_irq(&port->ah_lock);
	if (port->sm_ah)
		kref_put(&port->sm_ah->ref, free_sm_ah);
	port->sm_ah = new_ah;
	spin_unlock_irq(&port->ah_lock);

}

static void ib_sa_event(struct ib_event_handler *handler, struct ib_event *event)
{
	if (event->event == IB_EVENT_PORT_ERR    ||
	    event->event == IB_EVENT_PORT_ACTIVE ||
	    event->event == IB_EVENT_LID_CHANGE  ||
	    event->event == IB_EVENT_PKEY_CHANGE ||
	    event->event == IB_EVENT_SM_CHANGE   ||
	    event->event == IB_EVENT_CLIENT_REREGISTER) {
		unsigned long flags;
		struct ib_sa_device *sa_dev =
			container_of(handler, typeof(*sa_dev), event_handler);
		struct ib_sa_port *port =
			&sa_dev->port[event->element.port_num - sa_dev->start_port];

		if (rdma_port_get_link_layer(handler->device, port->port_num) != IB_LINK_LAYER_INFINIBAND)
			return;

		spin_lock_irqsave(&port->ah_lock, flags);
		if (port->sm_ah)
			kref_put(&port->sm_ah->ref, free_sm_ah);
		port->sm_ah = NULL;
		spin_unlock_irqrestore(&port->ah_lock, flags);

		schedule_work(&sa_dev->port[event->element.port_num -
					    sa_dev->start_port].update_task);
	}
}

void ib_sa_register_client(struct ib_sa_client *client)
{
	atomic_set(&client->users, 1);
	init_completion(&client->comp);
}
EXPORT_SYMBOL(ib_sa_register_client);

void ib_sa_unregister_client(struct ib_sa_client *client)
{
	ib_sa_client_put(client);
	wait_for_completion(&client->comp);
}
EXPORT_SYMBOL(ib_sa_unregister_client);

/**
 * ib_sa_cancel_query - try to cancel an SA query
 * @id:ID of query to cancel
 * @query:query pointer to cancel
 *
 * Try to cancel an SA query.  If the id and query don't match up or
 * the query has already completed, nothing is done.  Otherwise the
 * query is canceled and will complete with a status of -EINTR.
 */
void ib_sa_cancel_query(int id, struct ib_sa_query *query)
{
	unsigned long flags;
	struct ib_mad_agent *agent;
	struct ib_mad_send_buf *mad_buf;

	spin_lock_irqsave(&idr_lock, flags);
	if (idr_find(&query_idr, id) != query) {
		spin_unlock_irqrestore(&idr_lock, flags);
		return;
	}
	agent = query->port->agent;
	mad_buf = query->mad_buf;
	spin_unlock_irqrestore(&idr_lock, flags);

	ib_cancel_mad(agent, mad_buf);
}
EXPORT_SYMBOL(ib_sa_cancel_query);

static u8 get_src_path_mask(struct ib_device *device, u8 port_num)
{
	struct ib_sa_device *sa_dev;
	struct ib_sa_port   *port;
	unsigned long flags;
	u8 src_path_mask;

	sa_dev = ib_get_client_data(device, &sa_client);
	if (!sa_dev)
		return 0x7f;

	port  = &sa_dev->port[port_num - sa_dev->start_port];
	spin_lock_irqsave(&port->ah_lock, flags);
	src_path_mask = port->sm_ah ? port->sm_ah->src_path_mask : 0x7f;
	spin_unlock_irqrestore(&port->ah_lock, flags);

	return src_path_mask;
}

int ib_init_ah_from_path(struct ib_device *device, u8 port_num,
			 struct ib_sa_path_rec *rec, struct ib_ah_attr *ah_attr)
{
	int ret;
	u16 gid_index;
	int force_grh;

	memset(ah_attr, 0, sizeof *ah_attr);
	ah_attr->dlid = be16_to_cpu(rec->dlid);
	ah_attr->sl = rec->sl;
	ah_attr->src_path_bits = be16_to_cpu(rec->slid) &
				 get_src_path_mask(device, port_num);
	ah_attr->port_num = port_num;
	ah_attr->static_rate = rec->rate;

	force_grh = rdma_port_get_link_layer(device, port_num) == IB_LINK_LAYER_ETHERNET;

	if (rec->hop_limit > 1 || force_grh) {
		ah_attr->ah_flags = IB_AH_GRH;
		ah_attr->grh.dgid = rec->dgid;

		ret = ib_find_cached_gid(device, &rec->sgid, &port_num,
					 &gid_index);
		if (ret)
			return ret;

		ah_attr->grh.sgid_index    = gid_index;
		ah_attr->grh.flow_label    = be32_to_cpu(rec->flow_label);
		ah_attr->grh.hop_limit     = rec->hop_limit;
		ah_attr->grh.traffic_class = rec->traffic_class;
	}
	return 0;
}
EXPORT_SYMBOL(ib_init_ah_from_path);

static int alloc_mad(struct ib_sa_query *query, gfp_t gfp_mask)
{
	unsigned long flags;

	spin_lock_irqsave(&query->port->ah_lock, flags);
	if (!query->port->sm_ah) {
		spin_unlock_irqrestore(&query->port->ah_lock, flags);
		return -EAGAIN;
	}
	kref_get(&query->port->sm_ah->ref);
	query->sm_ah = query->port->sm_ah;
	spin_unlock_irqrestore(&query->port->ah_lock, flags);

	query->mad_buf = ib_create_send_mad(query->port->agent, 1,
					    query->sm_ah->pkey_index,
					    0, IB_MGMT_SA_HDR, IB_MGMT_SA_DATA,
					    gfp_mask);
	if (IS_ERR(query->mad_buf)) {
		kref_put(&query->sm_ah->ref, free_sm_ah);
		return -ENOMEM;
	}

	query->mad_buf->ah = query->sm_ah->ah;

	return 0;
}

static void free_mad(struct ib_sa_query *query)
{
	ib_free_send_mad(query->mad_buf);
	kref_put(&query->sm_ah->ref, free_sm_ah);
}

static void init_mad(struct ib_sa_mad *mad, struct ib_mad_agent *agent)
{
	unsigned long flags;

	memset(mad, 0, sizeof *mad);

	mad->mad_hdr.base_version  = IB_MGMT_BASE_VERSION;
	mad->mad_hdr.mgmt_class    = IB_MGMT_CLASS_SUBN_ADM;
	mad->mad_hdr.class_version = IB_SA_CLASS_VERSION;

	spin_lock_irqsave(&tid_lock, flags);
	mad->mad_hdr.tid           =
		cpu_to_be64(((u64) agent->hi_tid) << 32 | tid++);
	spin_unlock_irqrestore(&tid_lock, flags);
}

static int send_mad(struct ib_sa_query *query, int timeout_ms, gfp_t gfp_mask)
{
	unsigned long flags;
	int ret, id;

retry:
	if (!idr_pre_get(&query_idr, gfp_mask))
		return -ENOMEM;
	spin_lock_irqsave(&idr_lock, flags);
	ret = idr_get_new(&query_idr, query, &id);
	spin_unlock_irqrestore(&idr_lock, flags);
	if (ret == -EAGAIN)
		goto retry;
	if (ret)
		return ret;

	query->mad_buf->timeout_ms  = timeout_ms;
	query->mad_buf->context[0] = query;
	query->id = id;

	ret = ib_post_send_mad(query->mad_buf, NULL);
	if (ret) {
		spin_lock_irqsave(&idr_lock, flags);
		idr_remove(&query_idr, id);
		spin_unlock_irqrestore(&idr_lock, flags);
	}

	/*
	 * It's not safe to dereference query any more, because the
	 * send may already have completed and freed the query in
	 * another context.
	 */
	return ret ? ret : id;
}

void ib_sa_unpack_path(void *attribute, struct ib_sa_path_rec *rec)
{
	ib_unpack(path_rec_table, ARRAY_SIZE(path_rec_table), attribute, rec);
}
EXPORT_SYMBOL(ib_sa_unpack_path);

static void ib_sa_path_rec_callback(struct ib_sa_query *sa_query,
				    int status,
				    struct ib_sa_mad *mad)
{
	struct ib_sa_path_query *query =
		container_of(sa_query, struct ib_sa_path_query, sa_query);

	if (mad) {
		struct ib_sa_path_rec rec;

		ib_unpack(path_rec_table, ARRAY_SIZE(path_rec_table),
			  mad->data, &rec);
		query->callback(status, &rec, query->context);
	} else
		query->callback(status, NULL, query->context);
}

static void ib_sa_path_rec_release(struct ib_sa_query *sa_query)
{
	kfree(container_of(sa_query, struct ib_sa_path_query, sa_query));
}

int ib_sa_path_rec_query(struct ib_sa_client *client,
			 struct ib_device *device, u8 port_num,
			 struct ib_sa_path_rec *rec,
			 ib_sa_comp_mask comp_mask,
			 int timeout_ms, gfp_t gfp_mask,
			 void (*callback)(int status,
					  struct ib_sa_path_rec *resp,
					  void *context),
			 void *context,
			 struct ib_sa_query **sa_query)
{
	struct ib_sa_path_query *query;
	struct ib_sa_device *sa_dev = ib_get_client_data(device, &sa_client);
	struct ib_sa_port   *port;
	struct ib_mad_agent *agent;
	struct ib_sa_mad *mad;
	int ret;

	if (!sa_dev)
		return -ENODEV;

	port  = &sa_dev->port[port_num - sa_dev->start_port];
	agent = port->agent;

	query = kmalloc(sizeof *query, gfp_mask);
	if (!query)
		return -ENOMEM;

	query->sa_query.port     = port;
	ret = alloc_mad(&query->sa_query, gfp_mask);
	if (ret)
		goto err1;

	ib_sa_client_get(client);
	query->sa_query.client = client;
	query->callback        = callback;
	query->context         = context;

	mad = query->sa_query.mad_buf->mad;
	init_mad(mad, agent);

	query->sa_query.callback = callback ? ib_sa_path_rec_callback : NULL;
	query->sa_query.release  = ib_sa_path_rec_release;
	mad->mad_hdr.method	 = IB_MGMT_METHOD_GET;
	mad->mad_hdr.attr_id	 = cpu_to_be16(IB_SA_ATTR_PATH_REC);
	mad->sa_hdr.comp_mask	 = comp_mask;

	ib_pack(path_rec_table, ARRAY_SIZE(path_rec_table), rec, mad->data);

	*sa_query = &query->sa_query;

	ret = send_mad(&query->sa_query, timeout_ms, gfp_mask);
	if (ret < 0)
		goto err2;

	return ret;

err2:
	*sa_query = NULL;
	ib_sa_client_put(query->sa_query.client);
	free_mad(&query->sa_query);

err1:
	kfree(query);
	return ret;
}

static void ib_sa_service_rec_callback(struct ib_sa_query *sa_query,
				    int status,
				    struct ib_sa_mad *mad)
{
	struct ib_sa_service_query *query =
		container_of(sa_query, struct ib_sa_service_query, sa_query);

	if (mad) {
		struct ib_sa_service_rec rec;

		ib_unpack(service_rec_table, ARRAY_SIZE(service_rec_table),
			  mad->data, &rec);
		query->callback(status, &rec, query->context);
	} else
		query->callback(status, NULL, query->context);
}

static void ib_sa_service_rec_release(struct ib_sa_query *sa_query)
{
	kfree(container_of(sa_query, struct ib_sa_service_query, sa_query));
}

/**
 * ib_sa_service_rec_query - Start Service Record operation
 * @client:SA client
 * @device:device to send request on
 * @port_num: port number to send request on
 * @method:SA method - should be get, set, or delete
 * @rec:Service Record to send in request
 * @comp_mask:component mask to send in request
 * @timeout_ms:time to wait for response
 * @gfp_mask:GFP mask to use for internal allocations
 * @callback:function called when request completes, times out or is
 * canceled
 * @context:opaque user context passed to callback
 * @sa_query:request context, used to cancel request
 *
 * Send a Service Record set/get/delete to the SA to register,
 * unregister or query a service record.
 * The callback function will be called when the request completes (or
 * fails); status is 0 for a successful response, -EINTR if the query
 * is canceled, -ETIMEDOUT is the query timed out, or -EIO if an error
 * occurred sending the query.  The resp parameter of the callback is
 * only valid if status is 0.
 *
 * If the return value of ib_sa_service_rec_query() is negative, it is an
 * error code.  Otherwise it is a request ID that can be used to cancel
 * the query.
 */
int ib_sa_service_rec_query(struct ib_sa_client *client,
			    struct ib_device *device, u8 port_num, u8 method,
			    struct ib_sa_service_rec *rec,
			    ib_sa_comp_mask comp_mask,
			    int timeout_ms, gfp_t gfp_mask,
			    void (*callback)(int status,
					     struct ib_sa_service_rec *resp,
					     void *context),
			    void *context,
			    struct ib_sa_query **sa_query)
{
	struct ib_sa_service_query *query;
	struct ib_sa_device *sa_dev = ib_get_client_data(device, &sa_client);
	struct ib_sa_port   *port;
	struct ib_mad_agent *agent;
	struct ib_sa_mad *mad;
	int ret;

	if (!sa_dev)
		return -ENODEV;

	port  = &sa_dev->port[port_num - sa_dev->start_port];
	agent = port->agent;

	if (method != IB_MGMT_METHOD_GET &&
	    method != IB_MGMT_METHOD_SET &&
	    method != IB_SA_METHOD_DELETE)
		return -EINVAL;

	query = kmalloc(sizeof *query, gfp_mask);
	if (!query)
		return -ENOMEM;

	query->sa_query.port     = port;
	ret = alloc_mad(&query->sa_query, gfp_mask);
	if (ret)
		goto err1;

	ib_sa_client_get(client);
	query->sa_query.client = client;
	query->callback        = callback;
	query->context         = context;

	mad = query->sa_query.mad_buf->mad;
	init_mad(mad, agent);

	query->sa_query.callback = callback ? ib_sa_service_rec_callback : NULL;
	query->sa_query.release  = ib_sa_service_rec_release;
	mad->mad_hdr.method	 = method;
	mad->mad_hdr.attr_id	 = cpu_to_be16(IB_SA_ATTR_SERVICE_REC);
	mad->sa_hdr.comp_mask	 = comp_mask;

	ib_pack(service_rec_table, ARRAY_SIZE(service_rec_table),
		rec, mad->data);

	*sa_query = &query->sa_query;

	ret = send_mad(&query->sa_query, timeout_ms, gfp_mask);
	if (ret < 0)
		goto err2;

	return ret;

err2:
	*sa_query = NULL;
	ib_sa_client_put(query->sa_query.client);
	free_mad(&query->sa_query);

err1:
	kfree(query);
	return ret;
}
EXPORT_SYMBOL(ib_sa_service_rec_query);

static void ib_sa_mcmember_rec_callback(struct ib_sa_query *sa_query,
					int status,
					struct ib_sa_mad *mad)
{
	struct ib_sa_mcmember_query *query =
		container_of(sa_query, struct ib_sa_mcmember_query, sa_query);

	if (mad) {
		struct ib_sa_mcmember_rec rec;

		ib_unpack(mcmember_rec_table, ARRAY_SIZE(mcmember_rec_table),
			  mad->data, &rec);
		query->callback(status, &rec, query->context);
	} else
		query->callback(status, NULL, query->context);
}

static void ib_sa_mcmember_rec_release(struct ib_sa_query *sa_query)
{
	kfree(container_of(sa_query, struct ib_sa_mcmember_query, sa_query));
}

int ib_sa_mcmember_rec_query(struct ib_sa_client *client,
			     struct ib_device *device, u8 port_num,
			     u8 method,
			     struct ib_sa_mcmember_rec *rec,
			     ib_sa_comp_mask comp_mask,
			     int timeout_ms, gfp_t gfp_mask,
			     void (*callback)(int status,
					      struct ib_sa_mcmember_rec *resp,
					      void *context),
			     void *context,
			     struct ib_sa_query **sa_query)
{
	struct ib_sa_mcmember_query *query;
	struct ib_sa_device *sa_dev = ib_get_client_data(device, &sa_client);
	struct ib_sa_port   *port;
	struct ib_mad_agent *agent;
	struct ib_sa_mad *mad;
	int ret;

	if (!sa_dev)
		return -ENODEV;

	port  = &sa_dev->port[port_num - sa_dev->start_port];
	agent = port->agent;

	query = kmalloc(sizeof *query, gfp_mask);
	if (!query)
		return -ENOMEM;

	query->sa_query.port     = port;
	ret = alloc_mad(&query->sa_query, gfp_mask);
	if (ret)
		goto err1;

	ib_sa_client_get(client);
	query->sa_query.client = client;
	query->callback        = callback;
	query->context         = context;

	mad = query->sa_query.mad_buf->mad;
	init_mad(mad, agent);

	query->sa_query.callback = callback ? ib_sa_mcmember_rec_callback : NULL;
	query->sa_query.release  = ib_sa_mcmember_rec_release;
	mad->mad_hdr.method	 = method;
	mad->mad_hdr.attr_id	 = cpu_to_be16(IB_SA_ATTR_MC_MEMBER_REC);
	mad->sa_hdr.comp_mask	 = comp_mask;

	ib_pack(mcmember_rec_table, ARRAY_SIZE(mcmember_rec_table),
		rec, mad->data);

	*sa_query = &query->sa_query;

	ret = send_mad(&query->sa_query, timeout_ms, gfp_mask);
	if (ret < 0)
		goto err2;

	return ret;

err2:
	*sa_query = NULL;
	ib_sa_client_put(query->sa_query.client);
	free_mad(&query->sa_query);

err1:
	kfree(query);
	return ret;
}

static void ib_sa_inform_callback(struct ib_sa_query *sa_query,
				  int status,
				  struct ib_sa_mad *mad)
{
	struct ib_sa_inform_query *query =
		container_of(sa_query, struct ib_sa_inform_query, sa_query);

	if (mad) {
		struct ib_sa_inform rec;

		ib_unpack(inform_table, ARRAY_SIZE(inform_table),
			  mad->data, &rec);
		query->callback(status, &rec, query->context);
	} else
		query->callback(status, NULL, query->context);
}

static void ib_sa_inform_release(struct ib_sa_query *sa_query)
{
	kfree(container_of(sa_query, struct ib_sa_inform_query, sa_query));
}

int ib_sa_guid_info_rec_query(struct ib_sa_client *client,
			      struct ib_device *device, u8 port_num,
			      struct ib_sa_guidinfo_rec *rec,
			      ib_sa_comp_mask comp_mask, u8 method,
			      int timeout_ms, gfp_t gfp_mask,
			      void (*callback)(int status,
					       struct ib_sa_guidinfo_rec *resp,
					       void *context),
			      void *context,
			      struct ib_sa_query **sa_query)
{
	// stub function - 
        // called originally from mad.c under mlx4_ib_init_sriov()
        // which calls mlx4_ib_init_alias_guid_service() in alias_GUID.c
        // which goes down to this function

        printk("ERROR: function should be called only in SRIOV flow!!!");

	return 0;
}

/**
 * ib_sa_informinfo_query - Start an InformInfo registration.
 * @client:SA client
 * @device:device to send query on
 * @port_num: port number to send query on
 * @rec:Inform record to send in query
 * @timeout_ms:time to wait for response
 * @gfp_mask:GFP mask to use for internal allocations
 * @callback:function called when notice handler registration completes,
 * times out or is canceled
 * @context:opaque user context passed to callback
 * @sa_query:query context, used to cancel query
 *
 * This function sends inform info to register with SA to receive
 * in-service notice.
 * The callback function will be called when the query completes (or
 * fails); status is 0 for a successful response, -EINTR if the query
 * is canceled, -ETIMEDOUT is the query timed out, or -EIO if an error
 * occurred sending the query.  The resp parameter of the callback is
 * only valid if status is 0.
 *
 * If the return value of ib_sa_inform_query() is negative, it is an
 * error code.  Otherwise it is a query ID that can be used to cancel
 * the query.
 */
int ib_sa_informinfo_query(struct ib_sa_client *client,
			   struct ib_device *device, u8 port_num,
			   struct ib_sa_inform *rec,
			   int timeout_ms, gfp_t gfp_mask,
			   void (*callback)(int status,
					   struct ib_sa_inform *resp,
					   void *context),
			   void *context,
			   struct ib_sa_query **sa_query)
{
	struct ib_sa_inform_query *query;
	struct ib_sa_device *sa_dev = ib_get_client_data(device, &sa_client);
	struct ib_sa_port   *port;
	struct ib_mad_agent *agent;
	struct ib_sa_mad *mad;
	int ret;

	if (!sa_dev)
		return -ENODEV;

	port  = &sa_dev->port[port_num - sa_dev->start_port];
	agent = port->agent;

	query = kmalloc(sizeof *query, gfp_mask);
	if (!query)
		return -ENOMEM;

	query->sa_query.port     = port;
	ret = alloc_mad(&query->sa_query, gfp_mask);
	if (ret)
		goto err1;

	ib_sa_client_get(client);
	query->sa_query.client = client;
	query->callback = callback;
	query->context  = context;

	mad = query->sa_query.mad_buf->mad;
	init_mad(mad, agent);

	query->sa_query.callback = callback ? ib_sa_inform_callback : NULL;
	query->sa_query.release  = ib_sa_inform_release;
	query->sa_query.port     = port;
	mad->mad_hdr.method	 = IB_MGMT_METHOD_SET;
	mad->mad_hdr.attr_id	 = cpu_to_be16(IB_SA_ATTR_INFORM_INFO);

	ib_pack(inform_table, ARRAY_SIZE(inform_table), rec, mad->data);

	*sa_query = &query->sa_query;
	ret = send_mad(&query->sa_query, timeout_ms, gfp_mask);
	if (ret < 0)
		goto err2;

	return ret;

err2:
	*sa_query = NULL;
	ib_sa_client_put(query->sa_query.client);
	free_mad(&query->sa_query);
err1:
	kfree(query);
	return ret;
}

static void ib_sa_notice_resp(struct ib_sa_port *port,
			      struct ib_mad_recv_wc *mad_recv_wc)
{
	struct ib_mad_send_buf *mad_buf;
	struct ib_sa_mad *mad;
	int ret;
	unsigned long flags;

	mad_buf = ib_create_send_mad(port->notice_agent, 1, 0, 0,
				     IB_MGMT_SA_HDR, IB_MGMT_SA_DATA,
				     GFP_KERNEL);
	if (IS_ERR(mad_buf))
		return;

	mad = mad_buf->mad;
	memcpy(mad, mad_recv_wc->recv_buf.mad, sizeof *mad);
	mad->mad_hdr.method = IB_MGMT_METHOD_REPORT_RESP;

	spin_lock_irqsave(&port->ah_lock, flags);
	if (!port->sm_ah) {
		spin_unlock_irqrestore(&port->ah_lock, flags);
		ib_free_send_mad(mad_buf);
		return;
	}
	kref_get(&port->sm_ah->ref);
	mad_buf->context[0] = &port->sm_ah->ref;
	mad_buf->ah = port->sm_ah->ah;
	spin_unlock_irqrestore(&port->ah_lock, flags);

	ret = ib_post_send_mad(mad_buf, NULL);
	if (ret)
		goto err;

	return;
err:
	kref_put(mad_buf->context[0], free_sm_ah);
	ib_free_send_mad(mad_buf);
}

static void send_handler(struct ib_mad_agent *agent,
			 struct ib_mad_send_wc *mad_send_wc)
{
	struct ib_sa_query *query = mad_send_wc->send_buf->context[0];
	unsigned long flags;

	if (query->callback)
		switch (mad_send_wc->status) {
		case IB_WC_SUCCESS:
			/* No callback -- already got recv */
			break;
		case IB_WC_RESP_TIMEOUT_ERR:
			query->callback(query, -ETIMEDOUT, NULL);
			break;
		case IB_WC_WR_FLUSH_ERR:
			query->callback(query, -EINTR, NULL);
			break;
		default:
			query->callback(query, -EIO, NULL);
			break;
		}

	spin_lock_irqsave(&idr_lock, flags);
	idr_remove(&query_idr, query->id);
	spin_unlock_irqrestore(&idr_lock, flags);

	free_mad(query);
	ib_sa_client_put(query->client);
	query->release(query);
}

static void recv_handler(struct ib_mad_agent *mad_agent,
			 struct ib_mad_recv_wc *mad_recv_wc)
{
	struct ib_sa_query *query;
	struct ib_mad_send_buf *mad_buf;

	mad_buf = (void *) (unsigned long) mad_recv_wc->wc->wr_id;
	query = mad_buf->context[0];

	if (query->callback) {
		if (mad_recv_wc->wc->status == IB_WC_SUCCESS)
			query->callback(query,
					mad_recv_wc->recv_buf.mad->mad_hdr.status ?
					-EINVAL : 0,
					(struct ib_sa_mad *) mad_recv_wc->recv_buf.mad);
		else
			query->callback(query, -EIO, NULL);
	}

	ib_free_recv_mad(mad_recv_wc);
}

static void notice_resp_handler(struct ib_mad_agent *agent,
				struct ib_mad_send_wc *mad_send_wc)
{
	kref_put(mad_send_wc->send_buf->context[0], free_sm_ah);
	ib_free_send_mad(mad_send_wc->send_buf);
}

static void notice_handler(struct ib_mad_agent *mad_agent,
			   struct ib_mad_recv_wc *mad_recv_wc)
{
	struct ib_sa_port *port;
	struct ib_sa_mad *mad;
	struct ib_sa_notice notice;

	port = mad_agent->context;
	mad = (struct ib_sa_mad *) mad_recv_wc->recv_buf.mad;
	ib_unpack(notice_table, ARRAY_SIZE(notice_table), mad->data, &notice);

	if (!notice_dispatch(port->device, port->port_num, &notice))
		ib_sa_notice_resp(port, mad_recv_wc);
	ib_free_recv_mad(mad_recv_wc);
}

static void ib_sa_add_one(struct ib_device *device)
{
	struct ib_sa_device *sa_dev;
	struct ib_mad_reg_req reg_req = {
		.mgmt_class = IB_MGMT_CLASS_SUBN_ADM,
		.mgmt_class_version = 2
	};
	int s, e, i;

	if (rdma_node_get_transport(device->node_type) != RDMA_TRANSPORT_IB)
		return;

	if (device->node_type == RDMA_NODE_IB_SWITCH)
		s = e = 0;
	else {
		s = 1;
		e = device->phys_port_cnt;
	}

	sa_dev = kzalloc(sizeof *sa_dev +
			 (e - s + 1) * sizeof (struct ib_sa_port),
			 GFP_KERNEL);
	if (!sa_dev)
		return;

	sa_dev->start_port = s;
	sa_dev->end_port   = e;

	for (i = 0; i <= e - s; ++i) {
		spin_lock_init(&sa_dev->port[i].ah_lock);
		if (rdma_port_get_link_layer(device, i + 1) != IB_LINK_LAYER_INFINIBAND)
			continue;

		sa_dev->port[i].sm_ah    = NULL;
		sa_dev->port[i].port_num = i + s;

		sa_dev->port[i].agent =
			ib_register_mad_agent(device, i + s, IB_QPT_GSI,
					      NULL, 0, send_handler,
					      recv_handler, sa_dev);
		if (IS_ERR(sa_dev->port[i].agent))
			goto err;

		sa_dev->port[i].device = device;
		set_bit(IB_MGMT_METHOD_REPORT, reg_req.method_mask);
		sa_dev->port[i].notice_agent =
			ib_register_mad_agent(device, i + s, IB_QPT_GSI,
					      &reg_req, 0, notice_resp_handler,
					      notice_handler, &sa_dev->port[i]);

		if (IS_ERR(sa_dev->port[i].notice_agent))
			goto err;

		INIT_WORK(&sa_dev->port[i].update_task, update_sm_ah);
	}

	ib_set_client_data(device, &sa_client, sa_dev);

	/*
	 * We register our event handler after everything is set up,
	 * and then update our cached info after the event handler is
	 * registered to avoid any problems if a port changes state
	 * during our initialization.
	 */

	INIT_IB_EVENT_HANDLER(&sa_dev->event_handler, device, ib_sa_event);
	if (ib_register_event_handler(&sa_dev->event_handler))
		goto err;

	for (i = 0; i <= e - s; ++i)
		if (rdma_port_get_link_layer(device, i + 1) == IB_LINK_LAYER_INFINIBAND)
			update_sm_ah(&sa_dev->port[i].update_task);

	return;

err:
	while (--i >= 0)
		if (rdma_port_get_link_layer(device, i + 1) == IB_LINK_LAYER_INFINIBAND) {
			if (!IS_ERR(sa_dev->port[i].notice_agent))
				ib_unregister_mad_agent(sa_dev->port[i].notice_agent);
			if (!IS_ERR(sa_dev->port[i].agent))
				ib_unregister_mad_agent(sa_dev->port[i].agent);
		}

	kfree(sa_dev);

	return;
}

static void ib_sa_remove_one(struct ib_device *device)
{
	struct ib_sa_device *sa_dev = ib_get_client_data(device, &sa_client);
	int i;

	if (!sa_dev)
		return;

	ib_unregister_event_handler(&sa_dev->event_handler);

	flush_scheduled_work();

	for (i = 0; i <= sa_dev->end_port - sa_dev->start_port; ++i) {
		if (rdma_port_get_link_layer(device, i + 1) == IB_LINK_LAYER_INFINIBAND) {
			ib_unregister_mad_agent(sa_dev->port[i].notice_agent);
			ib_unregister_mad_agent(sa_dev->port[i].agent);
			if (sa_dev->port[i].sm_ah)
				kref_put(&sa_dev->port[i].sm_ah->ref, free_sm_ah);
		}

	}

	kfree(sa_dev);
}

static int __init ib_sa_init(void)
{
	int ret;

	spin_lock_init(&idr_lock);
	spin_lock_init(&tid_lock);

	get_random_bytes(&tid, sizeof tid);

	ret = ib_register_client(&sa_client);
	if (ret) {
		printk(KERN_ERR "Couldn't register ib_sa client\n");
		goto err1;
	}

	ret = mcast_init();
	if (ret) {
		printk(KERN_ERR "Couldn't initialize multicast handling\n");
		goto err2;
	}

	ret = notice_init();
	if (ret) {
		printk(KERN_ERR "Couldn't initialize notice handling\n");
		goto err3;
	}

	ret = sa_db_init();
	if (ret) {
		printk(KERN_ERR "Couldn't initialize local SA\n");
		goto err4;
	}

	return 0;
err4:
	notice_cleanup();
err3:
	mcast_cleanup();
err2:
	ib_unregister_client(&sa_client);
err1:
	return ret;
}

static void __exit ib_sa_cleanup(void)
{
	sa_db_cleanup();
	mcast_cleanup();
	notice_cleanup();
	ib_unregister_client(&sa_client);
	idr_destroy(&query_idr);
}

module_init_order(ib_sa_init, SI_ORDER_SECOND);
module_exit(ib_sa_cleanup);
