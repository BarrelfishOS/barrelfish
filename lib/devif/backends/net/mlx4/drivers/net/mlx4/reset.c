/*

 * Copyright (c) 2006, 2007 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007, 2008, 2014 Mellanox Technologies. All rights reserved.
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


 #include <linux/errno.h>
 */
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <pci/pci.h>
#include <linux/pci.h>
#include <linux/io.h>
#include <asm/byteorder.h>
/*
 #include <linux/delay.h>
 #include <linux/slab.h>
 #include <linux/jiffies.h>
 */

#include "mlx4.h"
#include <debug.h>

int mlx4_reset(struct mlx4_priv *priv) {
	int err = 0;
	void *reset; /*__iomem*/
	u32 *hca_header = NULL;
	/*int pcie_cap;*/
	//u16 devctl;
	//u16 linkctl;
	u32 vendor;
	unsigned long end;
	u32 sem;
	int i;

#define PCI_COMMAND		0x04	/* 16 bits */
#define PCI_VENDOR_ID	0x00	/* 16 bits */

#define MLX4_RESET_BASE		0xf0000
#define MLX4_RESET_SIZE		  0x400
#define MLX4_SEM_OFFSET		  0x3fc
#define MLX4_RESET_OFFSET	   0x10
#define MLX4_RESET_VALUE	swab32(1)

#define MLX4_SEM_TIMEOUT_JIFFIES	(10 * HZ)
#define MLX4_RESET_TIMEOUT_JIFFIES	(2 * HZ)

	/*	 * Reset the chip.  This is somewhat ugly because we have to
	 * save off the PCI header before reset and then restore it
	 * after the chip reboots.  We skip config space offsets 22
	 * and 23 since those have a special meaning.


	 Do we need to save off the full 4K PCI Express header??*/
	hca_header = malloc(256);
	if (!hca_header) {
		err = -ENOMEM;
		MLX4_DEBUG("Couldn't allocate memory to save HCA "
				"PCI header, aborting.\n");
		goto out;
	}

	/*pcie_cap = pci_pcie_cap(pdev);*/

	for (i = 0; i < 64; ++i) {
		if (i == 22 || i == 23)
			continue;
		if (pci_read_conf_header(i * 4, hca_header + i)) {
			err = -ENODEV;
			MLX4_DEBUG("Couldn't save HCA "
					"PCI header, aborting.\n");
			goto out;
		}
		/*MLX4_DEBUG("%"PRIx32"\n", *(hca_header + i));*/
	}

	/*memset(hca_header, 0, 256);

	 for (i = 0; i < 64; ++i) {
	 if (i == 22 || i == 23)
	 continue;
	 if (pci_write_conf_header(i * 4, 0x00000000)) {
	 err = -ENODEV;
	 MLX4_DEBUG("Couldn't save HCA "
	 "PCI header, aborting.\n");
	 goto out;
	 }
	 MLX4_DEBUG("%"PRIx32"\n", *(hca_header + i));
	 }

	 for (i = 0; i < 64; ++i) {
	 if (i == 22 || i == 23)
	 continue;
	 if (pci_read_conf_header(i * 4, hca_header + i)) {
	 err = -ENODEV;
	 MLX4_DEBUG("Couldn't save HCA "
	 "PCI header, aborting.\n");
	 goto out;
	 }
	 MLX4_DEBUG("%"PRIx32"\n", *(hca_header + i));
	 }*/

	/*pci_read_conf_header(0x0C, &vendor);
	 MLX4_DEBUG("vendor1: %"PRIx32"\n", vendor);
	 vendor = 0x10000000;
	 pci_write_conf_header(0x0C, vendor);
	 MLX4_DEBUG("vendor11: %"PRIx32"\n", vendor);
	 vendor = 0x0;
	 pci_read_conf_header(0x0C, &vendor);
	 MLX4_DEBUG("vendor2: %"PRIx32"\n", vendor);*/

	reset = priv->dev.bar_info->vaddr + MLX4_RESET_BASE;
	/*if (!reset) {
	 err = -ENOMEM;
	 MLX4_DEBUG("Couldn't map HCA reset register, aborting.\n");
	 goto out;
	 }*/

	/* grab HW semaphore to lock out flash updates */
	/*TODO: replace with a timer*/
	end = 0; //jiffies + MLX4_SEM_TIMEOUT_JIFFIES;
	do {
		sem = __raw_readl(reset + MLX4_SEM_OFFSET);
		if (!sem)
			break;

		barrelfish_usleep(1000);
		end++;
	} while (end < 1000);

	if (sem) {
		MLX4_DEBUG("Failed to obtain HW semaphore, aborting\n");
		err = -EAGAIN;
		/*iounmap(reset);*/
		goto out;
	}

	/* actually hit reset */
	__raw_writel(MLX4_RESET_VALUE, reset + MLX4_RESET_OFFSET);
	/*iounmap(reset);*/

	/* wait half a second before accessing device */
	barrelfish_usleep(500 * 1000);

	end = 0; //jiffies + MLX4_RESET_TIMEOUT_JIFFIES;
	do {
		if (!pci_read_conf_header(PCI_VENDOR_ID, &vendor)
				&& (vendor & 0x0000ffff) != 0x0000ffff) {
			break;
		}
		barrelfish_usleep(1000);
		end++;
	} while (end < 200);

	if (vendor == 0xffff) {
		err = -ENODEV;
		MLX4_DEBUG("PCI device did not come back after reset, "
				"aborting.\n");
		goto out;
	}

	/*	for (i = 0; i < 64; ++i) {
	 if (i == 22 || i == 23)
	 continue;
	 if (pci_read_conf_header(i * 4, hca_header + i)) {
	 err = -ENODEV;
	 MLX4_DEBUG("Couldn't save HCA "
	 "PCI header, aborting.\n");
	 goto out;
	 }
	 MLX4_DEBUG("%"PRIx32"\n", *(hca_header + i));
	 }*/

	/*vendor = 0;
	 pci_read_conf_header(0x0C, &vendor);
	 MLX4_DEBUG("vendor3: %"PRIx32"\n", vendor);*/

	/* Now restore the PCI headers */
	/*if (pcie_cap) {
	 devctl = hca_header[(pcie_cap + PCI_EXP_DEVCTL) / 4];
	 if (pcie_capability_write_word(pdev, PCI_EXP_DEVCTL, devctl)) {
	 err = -ENODEV;
	 MLX4_DEBUG("Couldn't restore HCA PCI Express "
	 "Device Control register, aborting.\n");
	 goto out;
	 }
	 linkctl = hca_header[(pcie_cap + PCI_EXP_LNKCTL) / 4];
	 if (pcie_capability_write_word(pdev, PCI_EXP_LNKCTL, linkctl)) {
	 err = -ENODEV;
	 MLX4_DEBUG("Couldn't restore HCA PCI Express "
	 "Link control register, aborting.\n");
	 goto out;
	 }
	 }*/

	for (i = 0; i < 16; ++i) {
		if (i * 4 == PCI_COMMAND)
			continue;

		if (pci_write_conf_header(i * 4, hca_header[i])) {
			err = -ENODEV;
			MLX4_DEBUG("Couldn't restore HCA reg %x, "
					"aborting.\n", i);
			goto out;
		}
	}

	if (pci_write_conf_header(PCI_COMMAND, hca_header[PCI_COMMAND / 4])) {
		err = -ENODEV;
		MLX4_DEBUG("Couldn't restore HCA COMMAND, "
				"aborting.\n");
		goto out;
	}

	out: free(hca_header);

	return err;
}
