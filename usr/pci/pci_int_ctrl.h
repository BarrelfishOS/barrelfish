#ifndef PCI_INT_CTRL_H
#define PCI_INT_CTRL_H

#include <sys/cdefs.h>
#include <barrelfish/caddr.h>
#include <errors/errno.h>

errval_t pci_int_ctrl_init(void);

#endif
