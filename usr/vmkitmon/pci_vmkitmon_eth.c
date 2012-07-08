#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_vmkitmon_eth.h"
#include "guest.h"
#include "string.h"
#include <pci/devids.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>

#define PCI_ETHERNET_IRQ 11
#define INVALID         0xffffffff
#define PCI_HEADER_MEM_ROM_BASE_REGISTER 0xc

static uint8_t vmkitmon_eth_mac[] = { 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF};

static uint64_t assumed_queue_id = 0;

static void generate_interrupt(struct pci_device *dev){
	struct pci_vmkitmon_eth * h = dev->state;
	h->mmio_register[PCI_VMKITMON_ETH_STATUS] |= PCI_VMKITMON_ETH_IRQST;
	lpc_pic_assert_irq(dev->lpc, dev->irq);
}

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
	printf("pci_vmkitmon_eth confspace_write addr: 0x%x, val: 0x%x\n", addr.d.doubleword, val);
	struct pci_vmkitmon_eth *h = dev->state;
	if(4 <= addr.d.doubleword && addr.d.doubleword <= 9 && val == INVALID){
		//caller wants to figure out bar size
		val = ~h->pci_device->bars[addr.d.doubleword - 4].bytes + 1; //~0x100000 + 1
		printf("pci_vmkitmon_eth writing bar %d size. 0x%x\n", addr.d.doubleword-4, val);
	}
	if(addr.d.doubleword == 1){
		//status register is clear by write 1
		val = (val & 0xffff) | (h->pci_header[addr.d.doubleword] & ~val)<<16;
	}
	h->pci_header[addr.d.doubleword] = val;
}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
	printf("pci_vmkitmon_eth confspace_read addr: 0x%x, ",addr.d.doubleword);
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

    printf(" val: 0x%x\n", *val);
}

/** Callback to get card's MAC address */
static void get_mac_address_fn(uint8_t* mac)
{
    printf("pci_vmkitmon_eth  get_mac_address_fn\n");
    memcpy(mac, &vmkitmon_eth_mac, 6);
}

static void dumpRegion(uint8_t *start){
	printf("-- dump starting from 0x%lx --\n", (uint64_t)start);
	for(int i=0; i<64;i++){
		printf("0x%4x: ", i*16);
		for(int j=0; j<16; j++){
			printf("%2x ", *( (start) + (i*16 + j)));
		}
		printf("\n");
	}
	printf("-- dump finished --\n");

}

static errval_t transmit_pbuf_list_fn(struct client_closure *cl) {
	printf("pci_vmkitmon_eth  transmit_pbuf_list_fn\n");
	int i;
	uint64_t paddr;
	//struct txbuf* buf;
	//    uint64_t client_data = 0;
	struct shared_pool_private *spp = cl->spp_ptr;
	struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
	uint64_t rtpbuf = sld->no_pbufs;

	struct buffer_descriptor *buffer = find_buffer(sld->buffer_id);

	// TODO: Make sure there is room in TX queue
	for (i = 0; i < rtpbuf; i++) {
		sld = &spp->sp->slot_list[cl->tx_index + i].d;
		assert(buffer->buffer_id == sld->buffer_id);
		paddr = (uint64_t) buffer->pa + sld->offset;
		printf("paddr: 0x%lx, len: 0x%lx\n", paddr, sld->len);
		dumpRegion(buffer->va + sld->offset);
		// Add info to free memory
		// TODO: Is this copy really necessary?
		//buf = txbufs + q[0]->tx_tail;
		//buf->eb = cl->app_connection;
		//buf->spp_index = cl->tx_index + i;

		//e10k_queue_add_txbuf(q[0], paddr, sld->len, buf, (i == rtpbuf - 1));
	}

	//e10k_queue_bump_txtail(q[0]);

	return SYS_ERR_OK;
}

static uint64_t find_tx_free_slot_count_fn(void) {
	printf("pci_vmkitmon_eth  find_tx_free_slot_count_fn\n");
	return 4096;
}

static bool handle_free_TX_slot_fn(void) {
	printf("pci_vmkitmon_eth  handle_free_TX_slot_fn\n");
//Is this a poll loop until the packets are transferred????
	return false;
}


static void mem_write(struct pci_device *dev, uint32_t addr, int bar, uint32_t val){
	struct pci_vmkitmon_eth *h = dev->state;
	printf("mem_write addr: 0x%x,  bar: %d, val: 0x%x, irq: %d\n",addr, bar, val, dev->irq );
	switch(addr) {
	case PCI_VMKITMON_ETH_STATUS:
		break;
	case PCI_VMKITMON_ETH_CONTROL:
		if( val & PCI_VMKITMON_ETH_RSTIRQ )
			h->mmio_register[PCI_VMKITMON_ETH_STATUS] &= ~PCI_VMKITMON_ETH_IRQST;
		if( val & PCI_VMKITMON_ETH_TXMIT )
			printf("Transmitting packet! guest-phys packet base address: 0x%x, packet-len: 0x%x\n",h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR], h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN]);
			// FIXME: ITERATE over txdescs
			//process_received_packet((void*)guest_to_host((lvaddr_t)h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR]), h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN]);
		break;
	default:
		h->mmio_register[addr] = val;
		break;
	}

	//For testing:
	if(0) generate_interrupt(dev);
}

static void mem_read(struct pci_device *dev, uint32_t addr, int bar, uint32_t *val){
	struct pci_vmkitmon_eth *h = (struct pci_vmkitmon_eth *) dev->state;
	if(addr != 0) printf("mem_read addr: 0x%x,  bar: %d, asserting irq: %d\n",addr, bar, dev->irq);
	switch(addr){
	case PCI_VMKITMON_ETH_MAC_LOW:
		memcpy(val, vmkitmon_eth_mac, 4);
		break;
	case PCI_VMKITMON_ETH_MAC_HIGH:
		memcpy(val, vmkitmon_eth_mac+4, 2);
		break;
	default:
		*val = h->mmio_register[addr];
	}
}


struct pci_device *pci_vmkitmon_eth_new(struct lpc *lpc, struct guest *g)
{
	struct pci_device *dev = calloc(1, sizeof(struct pci_device));
	struct pci_vmkitmon_eth *host = calloc(1, sizeof(struct pci_vmkitmon_eth));

	host->pci_device = dev;

	//initialize device
	dev->confspace_write = confspace_write;
	dev->confspace_read = confspace_read;
	dev->mem_read = mem_read;
	dev->mem_write = mem_write;
	dev->state = host;
	dev->irq = PCI_ETHERNET_IRQ;
	dev->lpc = lpc;

	pci_hdr0_mem_t *ph = &host->ph;
	pci_hdr0_mem_initialize(ph, (mackerel_addr_t) host->pci_header);

	// Fake a rtl8139
	pci_hdr0_mem_vendor_id_wr(ph, 0xdada);
	pci_hdr0_mem_device_id_wr(ph, 0x1000);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_ETHERNET);
	//pci_hdr0_mem_int_line_wr(ph, PCI_ETHERNET_IRQ);

	host->pci_header[0xf] = 1<<8 | PCI_ETHERNET_IRQ;


	//pci_hdr0_mem_int_pin_wr(ph, 0x3);

	//Allocate device memory for mem mapped register

	//TODO: Figure out a nice address, for the moment, make sure you dont go over 0xce000000
	// and stay close beyond (thats the point where the ixgbe is mapped).
	host->mem_guest_paddr = 0xcb000000;

	dev->bars[0].paddr = host->mem_guest_paddr;
	dev->bars[0].bytes = 0x100000; //1 MB

	//alloc_guest_mem(g, host->mem_guest_paddr, DEV_MEM_SIZE); //check if we get a pagefault at the 0x10k
	if (0) {
		get_mac_address_fn(0);
		handle_free_TX_slot_fn();
		find_tx_free_slot_count_fn();
		//transmit_pbuf_list_fn(NULL);
	}
	//Write BAR0 into pci header
	pci_hdr0_mem_bars_wr(ph, 0, host->mem_guest_paddr);

	// register to queue_manager

	ethersrv_init("vmkitmon_eth", assumed_queue_id, get_mac_address_fn,
			transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
			handle_free_TX_slot_fn);

	return dev;

}
