
#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_vmio.h"

#define PCI_VMIO_IRQ 12
#define INVALID         0xffffffff
#define PCI_CLASS_MISC 0xff

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{



}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
    struct pci_vmio *h = dev->state;

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


struct pci_device *pci_ethernet_new(struct lpc *lpc, struct guest *g)
{
	struct pci_device *dev = calloc(1, sizeof(struct pci_device));
	struct pci_vmio *host = calloc(1, sizeof(struct pci_vmio));

	host->pci_device = dev;

	//initialize device
	dev->confspace_write = confspace_write;
	dev->confspace_read = confspace_read;
	dev->state = host;
	dev->irq = PCI_VMIO_IRQ;
	dev->lpc = lpc;

	pci_hdr0_mem_t *ph = &host->ph;
	pci_hdr0_mem_initialize(ph, (mackerel_addr_t)host->pci_header);

	// Fake a host bridge
	pci_hdr0_mem_vendor_id_wr(ph, 0x8086);
	pci_hdr0_mem_device_id_wr(ph, 1);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_MISC);

	return dev;

}
