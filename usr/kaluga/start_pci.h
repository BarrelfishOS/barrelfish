#ifndef START_PCI_H_
#define START_PCI_H_

#include <errors/errno.h>

errval_t watch_for_pci_root_bridge(void);
errval_t watch_for_pci_devices(void);
errval_t watch_for_int_controller(void);

#endif /* START_PCI_H_ */
