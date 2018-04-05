#ifndef PCI_CLIENT_DEBUG_H
#define PCI_CLIENT_DEBUG_H

//#define PCI_LIB_DEBUG

#if defined(PCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
#define PCI_CLIENT_DEBUG(x...) debug_printf("pci_client: " x)
#else
#define PCI_CLIENT_DEBUG(x...) ((void)0)
#endif

#endif
