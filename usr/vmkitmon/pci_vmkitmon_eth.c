
#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_vmkitmon_eth.h"
#include "guest.h"
#include <pci/devids.h>

#define PCI_ETHERNET_IRQ 11
#define INVALID         0xffffffff
#define PCI_CLASS_MISC 0xff
#define DEV_MEM_SIZE      0x2000      // 8KB

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{



}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
    struct pci_vmkitmon_eth *h = dev->state;

    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }

    if(addr.d.doubleword < 0x40) {
        *val = h->pci_header[addr.d.doubleword];
    } else {
        *val = INVALID;
    }
}


struct pci_device *pci_vmkitmon_eth(struct lpc *lpc, struct guest *g)
{
	struct pci_device *dev = calloc(1, sizeof(struct pci_device));
	struct pci_vmkitmon_eth *host = calloc(1, sizeof(struct pci_vmkitmon_eth));

	host->pci_device = dev;

	//initialize device
	dev->confspace_write = confspace_write;
	dev->confspace_read = confspace_read;
	dev->state = host;
	dev->irq = PCI_ETHERNET_IRQ;
	dev->lpc = lpc;

	pci_hdr0_mem_t *ph = &host->ph;
	pci_hdr0_mem_initialize(ph, (mackerel_addr_t)host->pci_header);

	// Fake a rtl8139
	pci_hdr0_mem_vendor_id_wr(ph, PCI_VENDOR_REALTEK);
	pci_hdr0_mem_device_id_wr(ph, 0x8139);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_ETHERNET);

	//Allocate device memory for mem mapped register
	host->mem_guest_paddr = 0x10000; //TODO: figure out a nice adress

	//alloc_guest_mem(g, host->mem_guest_paddr, DEV_MEM_SIZE); //check if we get a pagefault at the 0x10k

	//Write BAR0 into pci header
	pci_hdr0_mem_bars_wr(ph,0, host->mem_guest_paddr);

	return dev;

}
