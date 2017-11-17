/*
 * Copyright (c) 2005 Topspin Communications.  All rights reserved.
 * Copyright (c) 2005, 2006 Cisco Systems.  All rights reserved.
 * Copyright (c) 2005 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2005 Voltaire, Inc. All rights reserved.
 * Copyright (c) 2005 PathScale, Inc. All rights reserved.
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
#include <linux/device.h>
#include <linux/err.h>
#include <linux/fs.h>
#include <linux/poll.h>
#include <linux/file.h>
#include <linux/cdev.h>

#include <asm/uaccess.h>

#include "uverbs.h"

MODULE_AUTHOR("Roland Dreier");
MODULE_DESCRIPTION("InfiniBand userspace verbs access");
MODULE_LICENSE("Dual BSD/GPL");

#define INFINIBANDEVENTFS_MAGIC	0x49426576	/* "IBev" */

enum {
	IB_UVERBS_MAJOR       = 231,
	IB_UVERBS_BASE_MINOR  = 192,
	IB_UVERBS_MAX_DEVICES = 32
};

#define IB_UVERBS_BASE_DEV	MKDEV(IB_UVERBS_MAJOR, IB_UVERBS_BASE_MINOR)

static struct class *uverbs_class;

DEFINE_SPINLOCK(ib_uverbs_idr_lock);
DEFINE_IDR(ib_uverbs_pd_idr);
DEFINE_IDR(ib_uverbs_mr_idr);
DEFINE_IDR(ib_uverbs_mw_idr);
DEFINE_IDR(ib_uverbs_ah_idr);
DEFINE_IDR(ib_uverbs_cq_idr);
DEFINE_IDR(ib_uverbs_qp_idr);
DEFINE_IDR(ib_uverbs_srq_idr);
DEFINE_IDR(ib_uverbs_xrc_domain_idr);

static spinlock_t map_lock;
static struct ib_uverbs_device *dev_table[IB_UVERBS_MAX_DEVICES];
static DECLARE_BITMAP(dev_map, IB_UVERBS_MAX_DEVICES);

static ssize_t (*uverbs_cmd_table[])(struct ib_uverbs_file *file,
				     const char __user *buf, int in_len,
				     int out_len) = {
	[IB_USER_VERBS_CMD_GET_CONTEXT]   	= ib_uverbs_get_context,
	[IB_USER_VERBS_CMD_QUERY_DEVICE]  	= ib_uverbs_query_device,
	[IB_USER_VERBS_CMD_QUERY_PORT]    	= ib_uverbs_query_port,
	[IB_USER_VERBS_CMD_ALLOC_PD]      	= ib_uverbs_alloc_pd,
	[IB_USER_VERBS_CMD_DEALLOC_PD]    	= ib_uverbs_dealloc_pd,
	[IB_USER_VERBS_CMD_REG_MR]        	= ib_uverbs_reg_mr,
	[IB_USER_VERBS_CMD_DEREG_MR]      	= ib_uverbs_dereg_mr,
	[IB_USER_VERBS_CMD_CREATE_COMP_CHANNEL] = ib_uverbs_create_comp_channel,
	[IB_USER_VERBS_CMD_CREATE_CQ]     	= ib_uverbs_create_cq,
	[IB_USER_VERBS_CMD_RESIZE_CQ]     	= ib_uverbs_resize_cq,
	[IB_USER_VERBS_CMD_POLL_CQ]     	= ib_uverbs_poll_cq,
	[IB_USER_VERBS_CMD_REQ_NOTIFY_CQ]     	= ib_uverbs_req_notify_cq,
	[IB_USER_VERBS_CMD_DESTROY_CQ]    	= ib_uverbs_destroy_cq,
	[IB_USER_VERBS_CMD_CREATE_QP]     	= ib_uverbs_create_qp,
	[IB_USER_VERBS_CMD_QUERY_QP]     	= ib_uverbs_query_qp,
	[IB_USER_VERBS_CMD_MODIFY_QP]     	= ib_uverbs_modify_qp,
	[IB_USER_VERBS_CMD_DESTROY_QP]    	= ib_uverbs_destroy_qp,
	[IB_USER_VERBS_CMD_POST_SEND]    	= ib_uverbs_post_send,
	[IB_USER_VERBS_CMD_POST_RECV]    	= ib_uverbs_post_recv,
	[IB_USER_VERBS_CMD_POST_SRQ_RECV]    	= ib_uverbs_post_srq_recv,
	[IB_USER_VERBS_CMD_CREATE_AH]    	= ib_uverbs_create_ah,
	[IB_USER_VERBS_CMD_DESTROY_AH]    	= ib_uverbs_destroy_ah,
	[IB_USER_VERBS_CMD_ATTACH_MCAST]  	= ib_uverbs_attach_mcast,
	[IB_USER_VERBS_CMD_DETACH_MCAST]  	= ib_uverbs_detach_mcast,
	[IB_USER_VERBS_CMD_CREATE_SRQ]    	= ib_uverbs_create_srq,
	[IB_USER_VERBS_CMD_MODIFY_SRQ]    	= ib_uverbs_modify_srq,
	[IB_USER_VERBS_CMD_QUERY_SRQ]     	= ib_uverbs_query_srq,
	[IB_USER_VERBS_CMD_DESTROY_SRQ]   	= ib_uverbs_destroy_srq,
	[IB_USER_VERBS_CMD_CREATE_XRC_SRQ]	= ib_uverbs_create_xrc_srq,
	[IB_USER_VERBS_CMD_OPEN_XRCD]	        = ib_uverbs_open_xrc_domain,
	[IB_USER_VERBS_CMD_CLOSE_XRCD]	        = ib_uverbs_close_xrc_domain,
	[IB_USER_VERBS_CMD_CREATE_XRC_RCV_QP]	= ib_uverbs_create_xrc_rcv_qp,
	[IB_USER_VERBS_CMD_MODIFY_XRC_RCV_QP]	= ib_uverbs_modify_xrc_rcv_qp,
	[IB_USER_VERBS_CMD_QUERY_XRC_RCV_QP]	= ib_uverbs_query_xrc_rcv_qp,
	[IB_USER_VERBS_CMD_REG_XRC_RCV_QP]	= ib_uverbs_reg_xrc_rcv_qp,
	[IB_USER_VERBS_CMD_UNREG_XRC_RCV_QP]	= ib_uverbs_unreg_xrc_rcv_qp,
};

#ifdef __linux__
/* BSD Does not require a fake mountpoint for all files. */
static struct vfsmount *uverbs_event_mnt;
#endif

static void ib_uverbs_add_one(struct ib_device *device);
static void ib_uverbs_remove_one(struct ib_device *device);

static void ib_uverbs_release_dev(struct kref *ref)
{
	struct ib_uverbs_device *dev =
		container_of(ref, struct ib_uverbs_device, ref);

	complete(&dev->comp);
}

static void ib_uverbs_release_event_file(struct kref *ref)
{
	struct ib_uverbs_event_file *file =
		container_of(ref, struct ib_uverbs_event_file, ref);

	kfree(file);
}

void ib_uverbs_release_ucq(struct ib_uverbs_file *file,
			  struct ib_uverbs_event_file *ev_file,
			  struct ib_ucq_object *uobj)
{
	struct ib_uverbs_event *evt, *tmp;

	if (ev_file) {
		spin_lock_irq(&ev_file->lock);
		list_for_each_entry_safe(evt, tmp, &uobj->comp_list, obj_list) {
			list_del(&evt->list);
			kfree(evt);
		}
		spin_unlock_irq(&ev_file->lock);

		kref_put(&ev_file->ref, ib_uverbs_release_event_file);
	}

	spin_lock_irq(&file->async_file->lock);
	list_for_each_entry_safe(evt, tmp, &uobj->async_list, obj_list) {
		list_del(&evt->list);
		kfree(evt);
	}
	spin_unlock_irq(&file->async_file->lock);
}

void ib_uverbs_release_uevent(struct ib_uverbs_file *file,
			      struct ib_uevent_object *uobj)
{
	struct ib_uverbs_event *evt, *tmp;

	spin_lock_irq(&file->async_file->lock);
	list_for_each_entry_safe(evt, tmp, &uobj->event_list, obj_list) {
		list_del(&evt->list);
		kfree(evt);
	}
	spin_unlock_irq(&file->async_file->lock);
}

static void ib_uverbs_detach_umcast(struct ib_qp *qp,
				    struct ib_uqp_object *uobj)
{
	struct ib_uverbs_mcast_entry *mcast, *tmp;

	list_for_each_entry_safe(mcast, tmp, &uobj->mcast_list, list) {
		ib_detach_mcast(qp, &mcast->gid, mcast->lid);
		list_del(&mcast->list);
		kfree(mcast);
	}
}

static int ib_uverbs_cleanup_ucontext(struct ib_uverbs_file *file,
				      struct ib_ucontext *context)
{
	struct ib_uobject *uobj, *tmp;

	if (!context)
		return 0;

	context->closing = 1;

	list_for_each_entry_safe(uobj, tmp, &context->ah_list, list) {
		struct ib_ah *ah = uobj->object;

		idr_remove_uobj(&ib_uverbs_ah_idr, uobj);
		ib_destroy_ah(ah);
		kfree(uobj);
	}

	list_for_each_entry_safe(uobj, tmp, &context->qp_list, list) {
		struct ib_qp *qp = uobj->object;
		struct ib_uqp_object *uqp =
			container_of(uobj, struct ib_uqp_object, uevent.uobject);

		idr_remove_uobj(&ib_uverbs_qp_idr, uobj);
		ib_uverbs_detach_umcast(qp, uqp);
		ib_destroy_qp(qp);
		ib_uverbs_release_uevent(file, &uqp->uevent);
		kfree(uqp);
	}


	list_for_each_entry_safe(uobj, tmp, &context->srq_list, list) {
		struct ib_srq *srq = uobj->object;
		struct ib_uevent_object *uevent =
			container_of(uobj, struct ib_uevent_object, uobject);

		idr_remove_uobj(&ib_uverbs_srq_idr, uobj);
		ib_destroy_srq(srq);
		ib_uverbs_release_uevent(file, uevent);
		kfree(uevent);
	}

	list_for_each_entry_safe(uobj, tmp, &context->cq_list, list) {
		struct ib_cq *cq = uobj->object;
		struct ib_uverbs_event_file *ev_file = cq->cq_context;
		struct ib_ucq_object *ucq =
			container_of(uobj, struct ib_ucq_object, uobject);

		idr_remove_uobj(&ib_uverbs_cq_idr, uobj);
		ib_destroy_cq(cq);
		ib_uverbs_release_ucq(file, ev_file, ucq);
		kfree(ucq);
	}

	/* XXX Free MWs */

	list_for_each_entry_safe(uobj, tmp, &context->mr_list, list) {
		struct ib_mr *mr = uobj->object;

		idr_remove_uobj(&ib_uverbs_mr_idr, uobj);
		ib_dereg_mr(mr);
		kfree(uobj);
	}

	mutex_lock(&file->device->ib_dev->xrcd_table_mutex);
	list_for_each_entry_safe(uobj, tmp, &context->xrcd_list, list) {
		struct ib_xrcd *xrcd = uobj->object;
		struct ib_uxrc_rcv_object *xrc_qp_obj, *tmp1;
		struct ib_uxrcd_object *xrcd_uobj =
			container_of(uobj, struct ib_uxrcd_object, uobject);

		list_for_each_entry_safe(xrc_qp_obj, tmp1,
					 &xrcd_uobj->xrc_reg_qp_list, list) {
			list_del(&xrc_qp_obj->list);
			ib_uverbs_cleanup_xrc_rcv_qp(file, xrcd,
						     xrc_qp_obj->qp_num);
			kfree(xrc_qp_obj);
		}

		idr_remove_uobj(&ib_uverbs_xrc_domain_idr, uobj);
		ib_uverbs_dealloc_xrcd(file->device->ib_dev, xrcd);
		kfree(uobj);
	}
	mutex_unlock(&file->device->ib_dev->xrcd_table_mutex);

	list_for_each_entry_safe(uobj, tmp, &context->pd_list, list) {
		struct ib_pd *pd = uobj->object;

		idr_remove_uobj(&ib_uverbs_pd_idr, uobj);
		ib_dealloc_pd(pd);
		kfree(uobj);
	}

	return context->device->dealloc_ucontext(context);
}

static void ib_uverbs_release_file(struct kref *ref)
{
	struct ib_uverbs_file *file =
		container_of(ref, struct ib_uverbs_file, ref);

	module_put(file->device->ib_dev->owner);
	kref_put(&file->device->ref, ib_uverbs_release_dev);

	kfree(file);
}

static ssize_t ib_uverbs_event_read(struct file *filp, char __user *buf,
				    size_t count, loff_t *pos)
{
	struct ib_uverbs_event_file *file = filp->private_data;
	struct ib_uverbs_event *event;
	int eventsz;
	int ret = 0;

	spin_lock_irq(&file->lock);

	while (list_empty(&file->event_list)) {
		spin_unlock_irq(&file->lock);

		if (filp->f_flags & O_NONBLOCK)
			return -EAGAIN;

		if (wait_event_interruptible(file->poll_wait,
					     !list_empty(&file->event_list)))
			return -ERESTARTSYS;

		spin_lock_irq(&file->lock);
	}

	event = list_entry(file->event_list.next, struct ib_uverbs_event, list);

	if (file->is_async)
		eventsz = sizeof (struct ib_uverbs_async_event_desc);
	else
		eventsz = sizeof (struct ib_uverbs_comp_event_desc);

	if (eventsz > count) {
		ret   = -EINVAL;
		event = NULL;
	} else {
		list_del(file->event_list.next);
		if (event->counter) {
			++(*event->counter);
			list_del(&event->obj_list);
		}
	}

	spin_unlock_irq(&file->lock);

	if (event) {
		if (copy_to_user(buf, event, eventsz))
			ret = -EFAULT;
		else
			ret = eventsz;
	}

	kfree(event);

	return ret;
}

static unsigned int ib_uverbs_event_poll(struct file *filp,
					 struct poll_table_struct *wait)
{
	unsigned int pollflags = 0;
	struct ib_uverbs_event_file *file = filp->private_data;

	file->filp = filp;
	poll_wait(filp, &file->poll_wait, wait);

	spin_lock_irq(&file->lock);
	if (!list_empty(&file->event_list))
		pollflags = POLLIN | POLLRDNORM;
	spin_unlock_irq(&file->lock);

	return pollflags;
}

static int ib_uverbs_event_fasync(int fd, struct file *filp, int on)
{
	struct ib_uverbs_event_file *file = filp->private_data;

	return fasync_helper(fd, filp, on, &file->async_queue);
}

static int ib_uverbs_event_close(struct inode *inode, struct file *filp)
{
	struct ib_uverbs_event_file *file = filp->private_data;
	struct ib_uverbs_event *entry, *tmp;

	spin_lock_irq(&file->lock);
	file->is_closed = 1;
	list_for_each_entry_safe(entry, tmp, &file->event_list, list) {
		if (entry->counter)
			list_del(&entry->obj_list);
		kfree(entry);
	}
	spin_unlock_irq(&file->lock);

	if (file->is_async) {
		ib_unregister_event_handler(&file->uverbs_file->event_handler);
		kref_put(&file->uverbs_file->ref, ib_uverbs_release_file);
	}
	kref_put(&file->ref, ib_uverbs_release_event_file);

	return 0;
}

static const struct file_operations uverbs_event_fops = {
	.owner	 = THIS_MODULE,
	.read 	 = ib_uverbs_event_read,
	.poll    = ib_uverbs_event_poll,
	.release = ib_uverbs_event_close,
	.fasync  = ib_uverbs_event_fasync
};

void ib_uverbs_comp_handler(struct ib_cq *cq, void *cq_context)
{
	struct ib_uverbs_event_file    *file = cq_context;
	struct ib_ucq_object	       *uobj;
	struct ib_uverbs_event	       *entry;
	unsigned long			flags;

	if (!file)
		return;

	spin_lock_irqsave(&file->lock, flags);
	if (file->is_closed) {
		spin_unlock_irqrestore(&file->lock, flags);
		return;
	}

	entry = kmalloc(sizeof *entry, GFP_ATOMIC);
	if (!entry) {
		spin_unlock_irqrestore(&file->lock, flags);
		return;
	}

	uobj = container_of(cq->uobject, struct ib_ucq_object, uobject);

	entry->desc.comp.cq_handle = cq->uobject->user_handle;
	entry->counter		   = &uobj->comp_events_reported;

	list_add_tail(&entry->list, &file->event_list);
	list_add_tail(&entry->obj_list, &uobj->comp_list);
	spin_unlock_irqrestore(&file->lock, flags);

	wake_up_interruptible(&file->poll_wait);
	if (file->filp)
		selwakeup(&file->filp->f_selinfo);
	kill_fasync(&file->async_queue, SIGIO, POLL_IN);
}

static void ib_uverbs_async_handler(struct ib_uverbs_file *file,
				    __u64 element, __u64 event,
				    struct list_head *obj_list,
				    u32 *counter)
{
	struct ib_uverbs_event *entry;
	unsigned long flags;

	spin_lock_irqsave(&file->async_file->lock, flags);
	if (file->async_file->is_closed) {
		spin_unlock_irqrestore(&file->async_file->lock, flags);
		return;
	}

	entry = kmalloc(sizeof *entry, GFP_ATOMIC);
	if (!entry) {
		spin_unlock_irqrestore(&file->async_file->lock, flags);
		return;
	}

	entry->desc.async.element    = element;
	entry->desc.async.event_type = event;
	entry->counter               = counter;

	list_add_tail(&entry->list, &file->async_file->event_list);
	if (obj_list)
		list_add_tail(&entry->obj_list, obj_list);
	spin_unlock_irqrestore(&file->async_file->lock, flags);

	wake_up_interruptible(&file->async_file->poll_wait);
	if (file->async_file->filp)
		selwakeup(&file->async_file->filp->f_selinfo);
	kill_fasync(&file->async_file->async_queue, SIGIO, POLL_IN);
}

void ib_uverbs_cq_event_handler(struct ib_event *event, void *context_ptr)
{
	struct ib_ucq_object *uobj = container_of(event->element.cq->uobject,
						  struct ib_ucq_object, uobject);

	ib_uverbs_async_handler(uobj->uverbs_file, uobj->uobject.user_handle,
				event->event, &uobj->async_list,
				&uobj->async_events_reported);
}

void ib_uverbs_qp_event_handler(struct ib_event *event, void *context_ptr)
{
	struct ib_uevent_object *uobj;

	uobj = container_of(event->element.qp->uobject,
			    struct ib_uevent_object, uobject);

	ib_uverbs_async_handler(context_ptr, uobj->uobject.user_handle,
				event->event, &uobj->event_list,
				&uobj->events_reported);
}

void ib_uverbs_srq_event_handler(struct ib_event *event, void *context_ptr)
{
	struct ib_uevent_object *uobj;

	uobj = container_of(event->element.srq->uobject,
			    struct ib_uevent_object, uobject);

	ib_uverbs_async_handler(context_ptr, uobj->uobject.user_handle,
				event->event, &uobj->event_list,
				&uobj->events_reported);
}

void ib_uverbs_event_handler(struct ib_event_handler *handler,
			     struct ib_event *event)
{
	struct ib_uverbs_file *file =
		container_of(handler, struct ib_uverbs_file, event_handler);

	ib_uverbs_async_handler(file, event->element.port_num, event->event,
				NULL, NULL);
}

void ib_uverbs_xrc_rcv_qp_event_handler(struct ib_event *event,
					void *context_ptr)
{
	ib_uverbs_async_handler(context_ptr, event->element.xrc_qp_num,
				event->event, NULL, NULL);
}

struct file *ib_uverbs_alloc_event_file(struct ib_uverbs_file *uverbs_file,
					int is_async, int *fd)
{
	struct ib_uverbs_event_file *ev_file;
	struct file *filp;
	int ret;

	ev_file = kmalloc(sizeof *ev_file, GFP_KERNEL);
	if (!ev_file)
		return ERR_PTR(-ENOMEM);

	kref_init(&ev_file->ref);
	spin_lock_init(&ev_file->lock);
	INIT_LIST_HEAD(&ev_file->event_list);
	init_waitqueue_head(&ev_file->poll_wait);
	ev_file->uverbs_file = uverbs_file;
	ev_file->async_queue = NULL;
	ev_file->is_async    = is_async;
	ev_file->is_closed   = 0;
	ev_file->filp	     = NULL;

	*fd = get_unused_fd();
	if (*fd < 0) {
		ret = *fd;
		goto err;
	}

	/*
	 * fops_get() can't fail here, because we're coming from a
	 * system call on a uverbs file, which will already have a
	 * module reference.
	 */
#ifdef __linux__
	filp = alloc_file(uverbs_event_mnt, dget(uverbs_event_mnt->mnt_root),
			  FMODE_READ, fops_get(&uverbs_event_fops));
#else
	filp = alloc_file(FMODE_READ, fops_get(&uverbs_event_fops));
#endif
	if (!filp) {
		ret = -ENFILE;
		goto err_fd;
	}

	filp->private_data = ev_file;

	return filp;

err_fd:
	put_unused_fd(*fd);

err:
	kfree(ev_file);
	return ERR_PTR(ret);
}

/*
 * Look up a completion event file by FD.  If lookup is successful,
 * takes a ref to the event file struct that it returns; if
 * unsuccessful, returns NULL.
 */
struct ib_uverbs_event_file *ib_uverbs_lookup_comp_file(int fd)
{
	struct ib_uverbs_event_file *ev_file = NULL;
	struct file *filp;

	filp = fget(fd);
	if (!filp)
		return NULL;

	if (filp->f_op != &uverbs_event_fops)
		goto out;

	ev_file = filp->private_data;
	if (ev_file->is_async) {
		ev_file = NULL;
		goto out;
	}

	kref_get(&ev_file->ref);

out:
	fput(filp);
	return ev_file;
}

static ssize_t ib_uverbs_write(struct file *filp, const char __user *buf,
			     size_t count, loff_t *pos)
{
	struct ib_uverbs_file *file = filp->private_data;
	struct ib_uverbs_cmd_hdr hdr;

	if (count < sizeof hdr)
		return -EINVAL;

	if (copy_from_user(&hdr, buf, sizeof hdr))
		return -EFAULT;

	if (hdr.in_words * 4 != count)
		return -EINVAL;

	if (hdr.command >= ARRAY_SIZE(uverbs_cmd_table) ||
	    !uverbs_cmd_table[hdr.command]		||
	    !(file->device->ib_dev->uverbs_cmd_mask & (1ull << hdr.command)))
		return -EINVAL;

	if (!file->ucontext &&
	    hdr.command != IB_USER_VERBS_CMD_GET_CONTEXT)
		return -EINVAL;

	return uverbs_cmd_table[hdr.command](file, buf + sizeof hdr,
					     hdr.in_words * 4, hdr.out_words * 4);
}

static int ib_uverbs_mmap(struct file *filp, struct vm_area_struct *vma)
{
	struct ib_uverbs_file *file = filp->private_data;

	if (!file->ucontext)
		return -ENODEV;
	else
		return file->device->ib_dev->mmap(file->ucontext, vma);
}

/*
 * ib_uverbs_open() does not need the BKL:
 *
 *  - dev_table[] accesses are protected by map_lock, the
 *    ib_uverbs_device structures are properly reference counted, and
 *    everything else is purely local to the file being created, so
 *    races against other open calls are not a problem;
 *  - there is no ioctl method to race against;
 *  - the device is added to dev_table[] as the last part of module
 *    initialization, the open method will either immediately run
 *    -ENXIO, or all required initialization will be done.
 */
static int ib_uverbs_open(struct inode *inode, struct file *filp)
{
	struct ib_uverbs_device *dev;
	struct ib_uverbs_file *file;
	int ret;

	spin_lock(&map_lock);
	dev = dev_table[iminor(inode) - IB_UVERBS_BASE_MINOR];
	if (dev)
		kref_get(&dev->ref);
	spin_unlock(&map_lock);

	if (!dev)
		return -ENXIO;

	if (!try_module_get(dev->ib_dev->owner)) {
		ret = -ENODEV;
		goto err;
	}

	file = kmalloc(sizeof *file, GFP_KERNEL);
	if (!file) {
		ret = -ENOMEM;
		goto err_module;
	}

	file->device	 = dev;
	file->ucontext	 = NULL;
	file->async_file = NULL;
	kref_init(&file->ref);
	mutex_init(&file->mutex);

	filp->private_data = file;

	return 0;

err_module:
	module_put(dev->ib_dev->owner);

err:
	kref_put(&dev->ref, ib_uverbs_release_dev);
	return ret;
}

static int ib_uverbs_close(struct inode *inode, struct file *filp)
{
	struct ib_uverbs_file *file = filp->private_data;

	ib_uverbs_cleanup_ucontext(file, file->ucontext);

	if (file->async_file)
		kref_put(&file->async_file->ref, ib_uverbs_release_event_file);

	kref_put(&file->ref, ib_uverbs_release_file);

	return 0;
}

static const struct file_operations uverbs_fops = {
	.owner 	 = THIS_MODULE,
	.write 	 = ib_uverbs_write,
	.open 	 = ib_uverbs_open,
	.release = ib_uverbs_close
};

static const struct file_operations uverbs_mmap_fops = {
	.owner 	 = THIS_MODULE,
	.write 	 = ib_uverbs_write,
	.mmap    = ib_uverbs_mmap,
	.open 	 = ib_uverbs_open,
	.release = ib_uverbs_close
};

static struct ib_client uverbs_client = {
	.name   = "uverbs",
	.add    = ib_uverbs_add_one,
	.remove = ib_uverbs_remove_one
};

static ssize_t show_ibdev(struct device *device, struct device_attribute *attr,
			  char *buf)
{
	struct ib_uverbs_device *dev = dev_get_drvdata(device);

	if (!dev)
		return -ENODEV;

	return sprintf(buf, "%s\n", dev->ib_dev->name);
}
static DEVICE_ATTR(ibdev, S_IRUGO, show_ibdev, NULL);

static ssize_t show_dev_abi_version(struct device *device,
				    struct device_attribute *attr, char *buf)
{
	struct ib_uverbs_device *dev = dev_get_drvdata(device);

	if (!dev)
		return -ENODEV;

	return sprintf(buf, "%d\n", dev->ib_dev->uverbs_abi_ver);
}
static DEVICE_ATTR(abi_version, S_IRUGO, show_dev_abi_version, NULL);

static ssize_t show_abi_version(struct class *class, struct class_attribute *attr, char *buf)
{
	return sprintf(buf, "%d\n", IB_USER_VERBS_ABI_VERSION);
}
static CLASS_ATTR(abi_version, S_IRUGO, show_abi_version, NULL);

#include <linux/pci.h>

static ssize_t
show_dev_device(struct device *device, struct device_attribute *attr, char *buf)
{
	struct ib_uverbs_device *dev = dev_get_drvdata(device);

	if (!dev)
		return -ENODEV;

	return sprintf(buf, "0x%04x\n",
	    ((struct pci_dev *)dev->ib_dev->dma_device)->device);
}
static DEVICE_ATTR(device, S_IRUGO, show_dev_device, NULL);

static ssize_t
show_dev_vendor(struct device *device, struct device_attribute *attr, char *buf)
{
	struct ib_uverbs_device *dev = dev_get_drvdata(device);

	if (!dev)
		return -ENODEV;

	return sprintf(buf, "0x%04x\n",
	    ((struct pci_dev *)dev->ib_dev->dma_device)->vendor);
}
static DEVICE_ATTR(vendor, S_IRUGO, show_dev_vendor, NULL);

struct attribute *device_attrs[] =
{
	&dev_attr_device.attr,
	&dev_attr_vendor.attr,
	NULL
};

static struct attribute_group device_group = {
        .name  = "device",
        .attrs  = device_attrs   
};

static void ib_uverbs_add_one(struct ib_device *device)
{
	struct ib_uverbs_device *uverbs_dev;

	if (!device->alloc_ucontext)
		return;

	uverbs_dev = kzalloc(sizeof *uverbs_dev, GFP_KERNEL);
	if (!uverbs_dev)
		return;

	kref_init(&uverbs_dev->ref);
	init_completion(&uverbs_dev->comp);

	spin_lock(&map_lock);
	uverbs_dev->devnum = find_first_zero_bit(dev_map, IB_UVERBS_MAX_DEVICES);
	if (uverbs_dev->devnum >= IB_UVERBS_MAX_DEVICES) {
		spin_unlock(&map_lock);
		goto err;
	}
	set_bit(uverbs_dev->devnum, dev_map);
	spin_unlock(&map_lock);

	uverbs_dev->ib_dev           = device;
	uverbs_dev->num_comp_vectors = device->num_comp_vectors;

	uverbs_dev->cdev = cdev_alloc();
	if (!uverbs_dev->cdev)
		goto err;
	uverbs_dev->cdev->owner = THIS_MODULE;
	uverbs_dev->cdev->ops = device->mmap ? &uverbs_mmap_fops : &uverbs_fops;
	kobject_set_name(&uverbs_dev->cdev->kobj, "uverbs%d", uverbs_dev->devnum);
	if (cdev_add(uverbs_dev->cdev, IB_UVERBS_BASE_DEV + uverbs_dev->devnum, 1))
		goto err_cdev;

	uverbs_dev->dev = device_create(uverbs_class, device->dma_device,
					uverbs_dev->cdev->dev, uverbs_dev,
					"uverbs%d", uverbs_dev->devnum);
	if (IS_ERR(uverbs_dev->dev))
		goto err_cdev;

	if (device_create_file(uverbs_dev->dev, &dev_attr_ibdev))
		goto err_class;
	if (device_create_file(uverbs_dev->dev, &dev_attr_abi_version))
		goto err_class;
	if (sysfs_create_group(&uverbs_dev->dev->kobj, &device_group))
		goto err_class;

	spin_lock(&map_lock);
	dev_table[uverbs_dev->devnum] = uverbs_dev;
	spin_unlock(&map_lock);

	ib_set_client_data(device, &uverbs_client, uverbs_dev);

	return;

err_class:
	device_destroy(uverbs_class, uverbs_dev->cdev->dev);

err_cdev:
	cdev_del(uverbs_dev->cdev);
	clear_bit(uverbs_dev->devnum, dev_map);

err:
	kref_put(&uverbs_dev->ref, ib_uverbs_release_dev);
	wait_for_completion(&uverbs_dev->comp);
	kfree(uverbs_dev);
	return;
}

static void ib_uverbs_remove_one(struct ib_device *device)
{
	struct ib_uverbs_device *uverbs_dev = ib_get_client_data(device, &uverbs_client);

	if (!uverbs_dev)
		return;

	sysfs_remove_group(&uverbs_dev->dev->kobj, &device_group);
	dev_set_drvdata(uverbs_dev->dev, NULL);
	device_destroy(uverbs_class, uverbs_dev->cdev->dev);
	cdev_del(uverbs_dev->cdev);

	spin_lock(&map_lock);
	dev_table[uverbs_dev->devnum] = NULL;
	spin_unlock(&map_lock);

	clear_bit(uverbs_dev->devnum, dev_map);

	kref_put(&uverbs_dev->ref, ib_uverbs_release_dev);
	wait_for_completion(&uverbs_dev->comp);
	kfree(uverbs_dev);
}
#ifdef __linux__
static int uverbs_event_get_sb(struct file_system_type *fs_type, int flags,
			       const char *dev_name, void *data,
			       struct vfsmount *mnt)
{
	return get_sb_pseudo(fs_type, "infinibandevent:", NULL,
			     INFINIBANDEVENTFS_MAGIC, mnt);
}

static struct file_system_type uverbs_event_fs = {
	/* No owner field so module can be unloaded */
	.name    = "infinibandeventfs",
	.get_sb  = uverbs_event_get_sb,
	.kill_sb = kill_litter_super
};
#endif

static int __init ib_uverbs_init(void)
{
	int ret;

	spin_lock_init(&map_lock);

	ret = register_chrdev_region(IB_UVERBS_BASE_DEV, IB_UVERBS_MAX_DEVICES,
				     "infiniband_verbs");
	if (ret) {
		printk(KERN_ERR "user_verbs: couldn't register device number\n");
		goto out;
	}

	uverbs_class = class_create(THIS_MODULE, "infiniband_verbs");
	if (IS_ERR(uverbs_class)) {
		ret = PTR_ERR(uverbs_class);
		printk(KERN_ERR "user_verbs: couldn't create class infiniband_verbs\n");
		goto out_chrdev;
	}

	ret = class_create_file(uverbs_class, &class_attr_abi_version);
	if (ret) {
		printk(KERN_ERR "user_verbs: couldn't create abi_version attribute\n");
		goto out_class;
	}

#ifdef __linux__
	ret = register_filesystem(&uverbs_event_fs);
	if (ret) {
		printk(KERN_ERR "user_verbs: couldn't register infinibandeventfs\n");
		goto out_class;
	}

	uverbs_event_mnt = kern_mount(&uverbs_event_fs);
	if (IS_ERR(uverbs_event_mnt)) {
		ret = PTR_ERR(uverbs_event_mnt);
		printk(KERN_ERR "user_verbs: couldn't mount infinibandeventfs\n");
		goto out_fs;
	}
#endif

	ret = ib_register_client(&uverbs_client);
	if (ret) {
		printk(KERN_ERR "user_verbs: couldn't register client\n");
		goto out_mnt;
	}

	return 0;

out_mnt:
#ifdef __linux__
	mntput(uverbs_event_mnt);

out_fs:
	unregister_filesystem(&uverbs_event_fs);
#endif

out_class:
	class_destroy(uverbs_class);

out_chrdev:
	unregister_chrdev_region(IB_UVERBS_BASE_DEV, IB_UVERBS_MAX_DEVICES);

out:
	return ret;
}

static void __exit ib_uverbs_cleanup(void)
{
	ib_unregister_client(&uverbs_client);
#ifdef __linux__
	mntput(uverbs_event_mnt);
	unregister_filesystem(&uverbs_event_fs);
#endif
	class_destroy(uverbs_class);
	unregister_chrdev_region(IB_UVERBS_BASE_DEV, IB_UVERBS_MAX_DEVICES);
	idr_destroy(&ib_uverbs_pd_idr);
	idr_destroy(&ib_uverbs_mr_idr);
	idr_destroy(&ib_uverbs_mw_idr);
	idr_destroy(&ib_uverbs_ah_idr);
	idr_destroy(&ib_uverbs_cq_idr);
	idr_destroy(&ib_uverbs_qp_idr);
	idr_destroy(&ib_uverbs_srq_idr);
}

module_init(ib_uverbs_init);
module_exit(ib_uverbs_cleanup);
