/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DEVICE_H_
#define IOAT_DEVICE_H_

#include <dma/dma_device.h>

enum device_type {
    IOAT_DEVICE_INVAL,
    IOAT_DEVICE_IVB,
    IOAT_DEVICE_HSW
};

/* CB device ID's */
#define IOAT_PCI_D5000       0x1A38
#define IOAT_PCI_DCNB        0x360B
#define IOAT_PCI_DSCNB       0x65FF
#define IOAT_PCI_DSNB        0x402F


/*
 * Intel I/O AT Device IDs for Ivy Bridge
 */
#define PCI_DEVICE_IOAT_IVB0   0x0e20
#define PCI_DEVICE_IOAT_IVB1   0x0e21
#define PCI_DEVICE_IOAT_IVB2   0x0e22
#define PCI_DEVICE_IOAT_IVB3   0x0e23
#define PCI_DEVICE_IOAT_IVB4   0x0e24
#define PCI_DEVICE_IOAT_IVB5   0x0e25
#define PCI_DEVICE_IOAT_IVB6   0x0e26
#define PCI_DEVICE_IOAT_IVB7   0x0e27
#define PCI_DEVICE_IOAT_IVB8   0x0e2e
#define PCI_DEVICE_IOAT_IVB9   0x0e2f
#define PCI_DEVICE_IOAT_IVB_CNT 10
/*
 * Intel I/O AT Device IDs for Haswell
 */
#define PCI_DEVICE_IOAT_HSW0   0x2f20
#define PCI_DEVICE_IOAT_HSW1   0x2f21
#define PCI_DEVICE_IOAT_HSW2   0x2f22
#define PCI_DEVICE_IOAT_HSW3   0x2f23
#define PCI_DEVICE_IOAT_HSW4   0x2f24
#define PCI_DEVICE_IOAT_HSW5   0x2f25
#define PCI_DEVICE_IOAT_HSW6   0x2f26
#define PCI_DEVICE_IOAT_HSW7   0x2f27
#define PCI_DEVICE_IOAT_HSW8   0x2f2e
#define PCI_DEVICE_IOAT_HSW9   0x2f2f
#define PCI_DEVICE_IOAT_HSW_CNT 10



#define IOAT_DMA_CHAN_COUNT 8

#define IOAT_DMA_BAR_COUNT 1

errval_t ioat_device_discovery(struct pci_addr addr,
                               enum device_type devtype,
                               uint8_t is_dev_mgr);

struct ioat_dma_device *ioat_device_get_next(void);

errval_t ioat_device_poll(void);


#endif /* IOAT_DEVICE_H_ */
