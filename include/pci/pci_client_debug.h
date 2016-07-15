#ifndef PCI_CLIENT_DEBUG_H
#define PCI_CLIENT_DEBUG_H

#define PCI_LIB_DEBUG 1

#if defined(PCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
#define PCI_CLIENT_DEBUG(x...) printf("pci_client: " x)
#else
#define PCI_CLIENT_DEBUG(x...) ((void)0)
#endif

#endif
