/*-
 * Copyright (c) 2010 Isilon Systems, Inc.
 * Copyright (c) 2010 iX Systems, Inc.
 * Copyright (c) 2010 Panasas, Inc.
 * Copyright (c) 2013-2015 Mellanox Technologies, Ltd.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice unmodified, this list of conditions, and the following
 *    disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef	_LINUX_INTERRUPT_H_
#define	_LINUX_INTERRUPT_H_

#include <linux/device.h>
#include <linux/pci.h>

#include <sys/bus.h>
#include <sys/rman.h>

typedef	irqreturn_t	(*irq_handler_t)(int, void *);

#define	IRQ_RETVAL(x)	((x) != IRQ_NONE)

#define	IRQF_SHARED	RF_SHAREABLE

struct irq_ent {
	struct list_head	links;
	struct device	*dev;
	struct resource	*res;
	void		*arg;
	irqreturn_t	(*handler)(int, void *);
	void		*tag;
	int		 irq;
};

static inline int
_irq_rid(struct device *dev, int irq)
{
	if (irq == dev->irq)
		return (0);
	return irq - dev->msix + 1;
}

static void
_irq_handler(void *ent)
{
	struct irq_ent *irqe;

	irqe = ent;
	irqe->handler(irqe->irq, irqe->arg);
}

static inline struct irq_ent *
_irq_ent(struct device *dev, int irq)
{
	struct irq_ent *irqe;

	list_for_each_entry(irqe, &dev->irqents, links)
		if (irqe->irq == irq)
			return (irqe);

	return (NULL);
}

static inline int
request_irq(unsigned int irq, irq_handler_t handler, unsigned long flags,
    const char *name, void *arg)
{
	struct resource *res;
	struct irq_ent *irqe;
	struct device *dev;
	int error;
	int rid;

	dev = _pci_find_irq_dev(irq);
	if (dev == NULL)
		return -ENXIO;
	rid = _irq_rid(dev, irq);
	res = bus_alloc_resource_any(dev->bsddev, SYS_RES_IRQ, &rid,
	    flags | RF_ACTIVE);
	if (res == NULL)
		return (-ENXIO);
	irqe = kmalloc(sizeof(*irqe), GFP_KERNEL);
	irqe->dev = dev;
	irqe->res = res;
	irqe->arg = arg;
	irqe->handler = handler;
	irqe->irq = irq;
	error = bus_setup_intr(dev->bsddev, res, INTR_TYPE_NET | INTR_MPSAFE,
	    NULL, _irq_handler, irqe, &irqe->tag);
	if (error) {
		bus_release_resource(dev->bsddev, SYS_RES_IRQ, rid, irqe->res);
		kfree(irqe);
		return (-error);
	}
	list_add(&irqe->links, &dev->irqents);

	return 0;
}

static inline int
bind_irq_to_cpu(unsigned int irq, int cpu_id)
{
	struct irq_ent *irqe;
	struct device *dev;

	dev = _pci_find_irq_dev(irq);
	if (dev == NULL)
		return (-ENOENT);

	irqe = _irq_ent(dev, irq);
	if (irqe == NULL)
		return (-ENOENT);

	return (-bus_bind_intr(dev->bsddev, irqe->res, cpu_id));
}

static inline void
free_irq(unsigned int irq, void *device)
{
	struct irq_ent *irqe;
	struct device *dev;
	int rid;

	dev = _pci_find_irq_dev(irq);
	if (dev == NULL)
		return;
	rid = _irq_rid(dev, irq);
	irqe = _irq_ent(dev, irq);
	if (irqe == NULL)
		return;
	bus_teardown_intr(dev->bsddev, irqe->res, irqe->tag);
	bus_release_resource(dev->bsddev, SYS_RES_IRQ, rid, irqe->res);
	list_del(&irqe->links);
	kfree(irqe);
}

#endif	/* _LINUX_INTERRUPT_H_ */
