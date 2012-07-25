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

static uint8_t guest_mac[] = { 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF}; //The mac address presented to virt. linux
static uint8_t host_mac[] = { 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xBF}; //The mac address presented to barrelfish
static uint64_t assumed_queue_id = 0;
static struct pci_device *the_pci_vmkitmon_eth;
static uint64_t global_spp_index;
static struct net_queue_manager_binding *global_binding;

static void generate_interrupt(struct pci_device *dev){
	struct pci_vmkitmon_eth * h = dev->state;
	h->mmio_register[PCI_VMKITMON_ETH_STATUS] |= PCI_VMKITMON_ETH_IRQST;
	lpc_pic_assert_irq(dev->lpc, dev->irq);
}

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
	VMKITMON_ETH_DEBUG("pci_vmkitmon_eth confspace_write addr: 0x%x, val: 0x%x\n", addr.d.doubleword, val);
	struct pci_vmkitmon_eth *h = dev->state;
	if(4 <= addr.d.doubleword && addr.d.doubleword <= 9 && val == INVALID){
		//caller wants to figure out bar size
		val = ~h->pci_device->bars[addr.d.doubleword - 4].bytes + 1; //~0x100000 + 1
		VMKITMON_ETH_DEBUG("pci_vmkitmon_eth writing bar %d size. 0x%x\n", addr.d.doubleword-4, val);
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
	VMKITMON_ETH_DEBUG("confspace_read addr: 0x%x, ",addr.d.doubleword);
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

    VMKITMON_ETH_DEBUG(" val: 0x%x\n", *val);
}

static void get_mac_address_fn(uint8_t* mac)
{
    memcpy(mac, &host_mac, 6);
}

#if defined(VMKITMON_ETH_DEBUG_SWITCH)
static void dumpRegion(uint8_t *start){
	printf("-- dump starting from 0x%lx --\n", (uint64_t)start);
	for(int i=0; i<64;i++){
		printf("0x%04x: ", i*16);
		for(int j=0; j<16; j++){
			printf("%02x ", *( (start) + (i*16 + j)));
		}
		printf("\n");
	}
	printf("-- dump finished --\n");
}
#endif

static errval_t transmit_pbuf_list_fn(struct client_closure *cl) {
	struct pci_vmkitmon_eth *h = the_pci_vmkitmon_eth->state;
	int i;
	uint64_t paddr;
	struct shared_pool_private *spp = cl->spp_ptr;
	global_binding = cl->app_connection;
	struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
	uint64_t rtpbuf = sld->no_pbufs;

    VMKITMON_ETH_DEBUG("transmit_pbuf_list_fn, no_pbufs: 0x%lx\n", rtpbuf);

	struct buffer_descriptor *buffer = find_buffer(sld->buffer_id);

	struct pci_vmkitmon_eth_rxdesc * first_rx = (struct pci_vmkitmon_eth_rxdesc *) guest_to_host( h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR] );
	uint32_t rxdesc_len = h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR];
	int transmitted = 0;
	for (i = 0; i < rtpbuf; i++) {
		sld = &spp->sp->slot_list[cl->tx_index + i].d;
		assert(buffer->buffer_id == sld->buffer_id);
		paddr = (uint64_t) buffer->pa + sld->offset;
		VMKITMON_ETH_DEBUG("paddr: 0x%lx, len: 0x%lx\n", paddr, sld->len);
#if defined(VMKITMON_ETH_DEBUG_SWITCH)
		dumpRegion(buffer->va + sld->offset);
#endif

        global_spp_index = (cl->tx_index + i) % cl->spp_ptr->c_size;

		for(int j = 0; j <= rxdesc_len/sizeof(struct pci_vmkitmon_eth_rxdesc); j++){
			struct pci_vmkitmon_eth_rxdesc * cur_rx =first_rx + j;
			if(cur_rx->len == 0 && cur_rx->addr != 0){
				void *hv_addr = (void *)guest_to_host(cur_rx->addr);
				memcpy(hv_addr, buffer->va + sld->offset, sld->len);
				cur_rx->len = sld->len;
				VMKITMON_ETH_DEBUG("Used rxdesc %d to transmit\n", j);
				transmitted = 1;
				break;
			}
		}
	}

	if(transmitted){
		generate_interrupt(the_pci_vmkitmon_eth);
	}

	return SYS_ERR_OK;
}

static uint64_t find_tx_free_slot_count_fn(void) {
	struct pci_vmkitmon_eth *h = the_pci_vmkitmon_eth->state;
	VMKITMON_ETH_DEBUG("find_tx_free_slot_count_fn, ");
	struct pci_vmkitmon_eth_rxdesc * first_rx = (struct pci_vmkitmon_eth_rxdesc *) guest_to_host( h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR] );
	uint32_t rxdesc_len = h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR];
	int numFree = 0;
	for (int i = 0; i < rxdesc_len/sizeof(struct pci_vmkitmon_eth_rxdesc); i++) {
		struct pci_vmkitmon_eth_rxdesc * cur_rx =first_rx + i;
		if(cur_rx->len == 0 && cur_rx->addr != 0){
			numFree++;
		}
	}
	VMKITMON_ETH_DEBUG("returning: %d\n ",numFree);
	return numFree;
}

static bool handle_free_TX_slot_fn(void) {
	VMKITMON_ETH_DEBUG("handle_free_TX_slot_fn\n");
    handle_tx_done(global_binding, global_spp_index);
    netbench_record_event_simple(bm, RE_TX_DONE, rdtsc());
	return false;
}

static void transmit_pending_packets(struct pci_vmkitmon_eth * h){
	VMKITMON_ETH_DEBUG("transmit_pending_packets\n");
	uint32_t rxdesc_len = h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN];
	struct pci_vmkitmon_eth_txdesc * first_tx = (struct pci_vmkitmon_eth_txdesc *) guest_to_host( h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR] );
	for(int i=0; i <= rxdesc_len/sizeof(struct pci_vmkitmon_eth_rxdesc); i++){
		struct pci_vmkitmon_eth_txdesc * cur_tx =first_tx + i;
		if(cur_tx->len != 0 && cur_tx->addr != 0){
			void *hv_addr = (void *)guest_to_host(cur_tx->addr);
			VMKITMON_ETH_DEBUG("Sending packet at txdesc %d, addr: 0x%x, len: 0x%x\n", i, cur_tx->addr, cur_tx->len);
			process_received_packet((void*)hv_addr, cur_tx->len);
			cur_tx->len = 0;
		}
	}
}


static void mem_write(struct pci_device *dev, uint32_t addr, int bar, uint32_t val){
	struct pci_vmkitmon_eth *h = dev->state;
	VMKITMON_ETH_DEBUG("mem_write addr: 0x%x,  bar: %d, val: 0x%x, irq: %d\n",addr, bar, val, dev->irq );
	switch(addr) {
	case PCI_VMKITMON_ETH_STATUS:
		break;
	case PCI_VMKITMON_ETH_CONTROL:
		if( val & PCI_VMKITMON_ETH_RSTIRQ )
			h->mmio_register[PCI_VMKITMON_ETH_STATUS] &= ~PCI_VMKITMON_ETH_IRQST;
		if( val & PCI_VMKITMON_ETH_TXMIT ) {
			VMKITMON_ETH_DEBUG("Transmitting packet. guest-phys packet base address: 0x%x, packet-len: 0x%x\n",h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR], h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN]);
			transmit_pending_packets(h);
        }
		if( val & PCI_VMKITMON_ETH_IFUP) {
			VMKITMON_ETH_DEBUG("Interface up, registering\n");
			// register to queue_manager
			ethersrv_init("vmkitmon_eth", assumed_queue_id, get_mac_address_fn,
					transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
					handle_free_TX_slot_fn);

		}
		break;
	default:
		h->mmio_register[addr] = val;
		break;
	}
}

static void mem_read(struct pci_device *dev, uint32_t addr, int bar, uint32_t *val){
	struct pci_vmkitmon_eth *h = (struct pci_vmkitmon_eth *) dev->state;
	if(addr != 0) VMKITMON_ETH_DEBUG("mem_read addr: 0x%x,  bar: %d, asserting irq: %d\n",addr, bar, dev->irq);
	switch(addr){
	case PCI_VMKITMON_ETH_MAC_LOW:
		memcpy(val, guest_mac, 4);
		break;
	case PCI_VMKITMON_ETH_MAC_HIGH:
		memcpy(val, guest_mac+4, 2);
		break;
	default:
		*val = h->mmio_register[addr];
	}
}


struct pci_device *pci_vmkitmon_eth_new(struct lpc *lpc, struct guest *g) {
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

	pci_hdr0_mem_vendor_id_wr(ph, PCI_VENDOR_FISH);
	pci_hdr0_mem_device_id_wr(ph, PCI_VMKITMON_ETH_DEVID);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_ETHERNET);

	//The next line could be: pci_hdr0_mem_int_line_wr(ph, PCI_ETHERNET_IRQ);
	//but the register is defined read only...
	host->pci_header[0xf] = 1<<8 | PCI_ETHERNET_IRQ;


	//TODO: Figure out a nice address, for the moment, make sure you dont go over 0xce000000
	// and stay close beyond (thats the point where the ixgbe is mapped).
	host->mem_guest_paddr = 0xcb000000;
	dev->bars[0].paddr = host->mem_guest_paddr;
	dev->bars[0].bytes = 0x100000; //1 MB

	//Write BAR0 into pci header
	pci_hdr0_mem_bars_wr(ph, 0, host->mem_guest_paddr);

	the_pci_vmkitmon_eth = dev;
	return dev;
}
