
#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_vmkitmon_eth.h"
#include "guest.h"
#include "string.h"
#include <pci/devids.h>

#define PCI_ETHERNET_IRQ 11
#define INVALID         0xffffffff
#define PCI_HEADER_MEM_ROM_BASE_REGISTER 0xc

static uint64_t vmkitmon_eth_mac = 0xAABBCCDDEEFF;

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
	printf("pci_vmkitmon_eth confspace write. addr: 0x%x, val: 0x%x\n", addr.d.doubleword, val);
	struct pci_vmkitmon_eth *h = dev->state;
	if(4 <= addr.d.doubleword && addr.d.doubleword <= 9 && val == INVALID){
		//caller wants to figure out bar size
		val = h->pci_device->bars[addr.d.doubleword - 4].bytes;
		printf("pci_vmkitmon_eth writing bar %d size. 0x%x\n", addr.d.doubleword-4, val);
	}
	h->pci_header[addr.d.doubleword] = val;
}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
	printf("pci_vmkitmon_eth read addr: 0x%x \n",addr.d.doubleword);
    struct pci_vmkitmon_eth *h = dev->state;

    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
    } else {
    	if(addr.d.doubleword == PCI_HEADER_MEM_ROM_BASE_REGISTER) {
    		//we dont support a rom, return always 0
    		*val = 0;
    	} else if(addr.d.doubleword < 0x40) {
			*val = h->pci_header[addr.d.doubleword];
		} else {
			*val = INVALID;
		}
    }

    printf("Returning val: 0x%x\n", *val);
}

/** Callback to get card's MAC address */
static void get_mac_address_fn(uint8_t* mac)
{
    printf("Get MAC address\n");
    memcpy(mac, &vmkitmon_eth_mac, 6);
}


struct pci_device *pci_vmkitmon_eth_new(struct lpc *lpc, struct guest *g)
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
	pci_hdr0_mem_vendor_id_wr(ph, 0xdada);
	pci_hdr0_mem_device_id_wr(ph, 0x1000);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_ETHERNET);
	pci_hdr0_mem_int_line_wr(ph, PCI_ETHERNET_IRQ);

	//Allocate device memory for mem mapped register

	//TODO: Figure out a nice address, for the moment, make sure you dont go over 0xce000000
	// and stay close beyond (thats the point where the ixgbe is mapped).
	host->mem_guest_paddr = 0xcb000000;

	dev->bars[0].paddr = host->mem_guest_paddr;
	dev->bars[0].bytes = ~0x100000 + 1; //1 MB

	//alloc_guest_mem(g, host->mem_guest_paddr, DEV_MEM_SIZE); //check if we get a pagefault at the 0x10k
	if(0){
		get_mac_address_fn(0);
	}
	//Write BAR0 into pci header
	pci_hdr0_mem_bars_wr(ph,0, host->mem_guest_paddr);



	return dev;

}
