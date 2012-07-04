/**
 * \file Fake PCI host bridge
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_ethernet.h"

#include "e1000_dev.h"


#include <pci/pci.h>
#include <arch/x86/barrelfish/iocap_arch.h>

#define INVALID         0xffffffff

#define PCI_CONFIG_ADDRESS_PORT 0x0cf8
#define PCI_CONFIG_DATA_PORT    0x0cfc

#define PCI_ETHERNET_IRQ 11


e1000_t d;  ///< Mackerel state

static struct guest *guest_info;
static struct pci_ethernet *pci_ethernet_unique;

// IXGBE START
#define EICR1_OFFSET 0x00800 //Interrupt Cause Read
#define EICR2_OFFSET 0x00804
#define EICS1_OFFSET 0x00808 //Interrupt Cause Set
#define EICS2_OFFSET 0x00812

#define TDT_OFFSET 0x6018
#define TDH_OFFSET 0x6010

#define TDBAL0_OFFSET 0x6000
#define TDBAH0_OFFSET 0x6004

#define RDBAL0_OFFSET 0x1000
#define RDBAH0_OFFSET 0x1004
#define RDH0_OFFSET 0x1010
#define RDT0_OFFSET 0x1018
#define RXDCTL0_OFFSET 0x1028
#define RDLEN_OFFSET 0x01008


static int register_needs_translation(uint64_t addr){
	return (
		(0x1000 <= addr && addr <= 0x1fc0 && (addr & 0x3f) == 0) ||  //RDBAL 1
		(0xd000 <= addr && addr <= 0xdfc0 && (addr & 0x3f) == 0) ||  //RDBAL 2
		(0x6000 <= addr && addr <= 0x7fc0 && (addr & 0x3f) == 0)     //TDBAL
	);
}
// IXGBE DONE

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
	//printf("pci_ixgbe confspace_write addr: 0x%x, val: 0x%x\n", addr.d.doubleword, val);
    if(addr.d.fnct_nr != 0) {
        return;
    }
    
    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_write_conf_header(addr.d.doubleword, val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r, "Writing conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Written to conf header at %u: %x\n", addr.d.doubleword, val);
        }
    }
}

struct pci_address {
    uint8_t bus;
    uint8_t device;
    uint8_t function;
};

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
	//printf("pci_ixgbe confspace_read addr: 0x%x ",addr.d.doubleword);
    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }

    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_read_conf_header(addr.d.doubleword,val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r,"Reading conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Read from conf header at %u: %x\n",addr.d.doubleword, *val);
        }
    } else {
        *val = INVALID;
    }
    //printf(" val: 0x%x\n", *val);
}

static genpaddr_t eth_base_paddr = 0;

static void e1000_init(void *bar_info, int nr_allocated_bars)
{
	printf("e1000_init. nr_allocated_bars: %d!\n", nr_allocated_bars);
    
    struct device_mem *bar = (struct device_mem *)bar_info;
    
    eth_base_paddr = bar[0].paddr;

    struct pci_ethernet * eth = pci_ethernet_unique;
    eth->phys_base_addr = bar[0].paddr;
    eth->bytes = bar[0].bytes;

    eth->pci_device->bars[0].bytes = bar[0].bytes;
    eth->pci_device->bars[0].vaddr = NULL; //Not mapped
    eth->pci_device->bars[0].paddr = bar[0].paddr;

    errval_t err = map_device(&bar[0]);
    if(err_is_fail(err)){
		printf("guest_vspace_map_wrapper failed\n");
	}
    printf("e1000_init: map_device successful. vaddr: 0x%lx, bytes: %d...\n", (uint64_t)bar[0].vaddr, (int)bar[0].bytes);
    eth->virt_base_addr = bar[0].vaddr;


    /* err = guest_vspace_map_wrapper(&guest_info->vspace, bar[0].paddr, bar[0].frame_cap[0], bar[0].bytes);
    if(err_is_fail(err)){
    	printf("guest_vspace_map_wrapper failed\n");
    } */

}

#define ICR_OFFSET 0xC0;

static void e1000_interrupt_handler(void *arg)
{
    struct pci_device *dev = (struct pci_device *)arg;
    lpc_pic_assert_irq(dev->lpc, dev->irq);
}

static uint64_t vaddr_to_paddr(uint64_t vaddr){
	uint64_t res = 0x100000000 + vaddr;
	VMKIT_PCI_DEBUG("Returning: 0x%lx\n", res);
	return res;
}

static uint32_t read_device_mem(struct pci_ethernet * eth, uint32_t offset){
	return *((uint32_t *)(((uint64_t)eth->virt_base_addr) + offset));
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

static void mem_write(struct pci_device *dev, uint32_t addr, int bar, uint32_t val){
	struct pci_ethernet * eth = (struct pci_ethernet *)dev->state;
	if(register_needs_translation(addr)){
		VMKIT_PCI_DEBUG("Write access to Pointer register 0x%08lx value 0x%08lx\n", fault_addr & ETH_MMIO_MASK(eth), val);
		if(val){
			val = vaddr_to_paddr(val);

		}
		VMKIT_PCI_DEBUG("Translated to value 0x%08lx\n", val);
	}
	if(TDBAH0_OFFSET == addr) {
		//!!HACK: OUR TRANSLATED ADDRESS GOES OVER 32BIT SPACE....
		VMKIT_PCI_DEBUG("TDBAH0 write detected. writing 1.\n");
		val = 1;
	}
	if(RDBAH0_OFFSET == addr) {
		//!!HACK: OUR TRANSLATED ADDRESS GOES OVER 32BIT SPACE....
		VMKIT_PCI_DEBUG("RDBAH0 write detected. writing 1.\n");
		val = 1;
	}
	if(TDT_OFFSET == addr){
		uint32_t tdt = val;
		uint32_t tdh = read_device_mem(eth,TDH_OFFSET);
		uint32_t tdbal = read_device_mem(eth,TDBAL0_OFFSET);
		uint32_t tdbah = read_device_mem(eth,TDBAH0_OFFSET);
		VMKIT_PCI_DEBUG("Wrote to TDT detected. TDT: %d, TDH: %d, TDBAL: 0x%08x, TDBAH: 0x%08x\n", tdt,tdh, tdbal, tdbah);
		if(tdt != tdh){
			lvaddr_t tdbal_monvirt = guest_to_host((lvaddr_t)tdbal);
			//dumpRegion((uint8_t*)tdbal_monvirt);

			uint32_t firstdesc_guestphys = *((uint32_t*)tdbal_monvirt);
			uint32_t * firstdesc_monvirt = (uint32_t *) guest_to_host( (lvaddr_t)(firstdesc_guestphys) );
			if(0) dumpRegion((uint8_t*)firstdesc_monvirt );

			uint32_t firstdesc_hostphys = (uint64_t) vaddr_to_paddr( (uint64_t) firstdesc_guestphys);
			*((uint32_t *)tdbal_monvirt) =  firstdesc_hostphys;
			// TODO FIXME hier ueber alle txdescs iterieren und das bit richtig setzen...
			//!!HACK: OUR TRANSLATED ADDRESS GOES OVER 32BIT SPACE....
			for(int j = tdh; j < tdt; j++){
				uint32_t * ptr = ((uint32_t *)tdbal_monvirt)+1 + 4*j;
				*ptr =  1;
			}

			//dumpRegion((uint8_t*)tdbal_monvirt);
		}

		//Inspect the contents of the RECEIVE descriptors
		uint32_t rdbal  = read_device_mem(eth, RDBAL0_OFFSET);
		uint32_t rdbah  = read_device_mem(eth, RDBAH0_OFFSET);
		uint32_t rdh    = read_device_mem(eth, RDH0_OFFSET);
		uint32_t rdt    = read_device_mem(eth, RDT0_OFFSET);
		uint32_t rdlen  = read_device_mem(eth, RDLEN_OFFSET);
		uint32_t rdxctl = read_device_mem(eth, RXDCTL0_OFFSET);

		if(0){
			printf("Inspecting ReceiveDescriptor. RDBAL: 0x%08x, RDBAH: 0x%08x, RDH: %d, RDT: %d, RDLEN: %d, RDXCTL: 0x%x\n",
					rdbal, rdbah, rdh, rdt, rdlen, rdxctl);
			printf("Wrote to TDT detected. TDT: %d, TDH: %d, TDBAL: 0x%08x, TDBAH: 0x%08x\n", tdt,tdh, tdbal, tdbah);
		}

		VMKIT_PCI_DEBUG("Inspecting ReceiveDescriptor. RDBAL: 0x%08x, RDBAH: 0x%08x, RDH: %d, RDT: %d, RDLEN: %d, RDXCTL: 0x%x\n",
				rdbal, rdbah, rdh, rdt, rdlen, rdxctl);
		lvaddr_t rdbal_monvirt = guest_to_host((lvaddr_t)rdbal);
		if(rdbal != 0){
			//dumpRegion((uint8_t*)rdbal_monvirt);
			//Patch region. RDLEN is in bytes. each descriptor needs 16 bytes
			for(int j = 0; j < rdlen/16; j++){
				uint32_t * ptr = ((uint32_t *)rdbal_monvirt)+1 + 4*j;
				//printf("j: %d, ptr: %p\n",j,ptr);
				*ptr =  1;
				*(ptr+2) = 1;
				//printf("written value is 0x%ux\n", *ptr);
			}
			//dumpRegion((uint8_t*)rdbal_monvirt);
		}
	}

	*((uint32_t *)(((uint64_t)(eth->virt_base_addr)) + addr)) = val;
}

static void mem_read(struct pci_device *dev, uint32_t addr, int bar, uint32_t *val){
	struct pci_ethernet * eth = (struct pci_ethernet *)dev->state;
	*val = *((uint32_t *)((uint64_t)(eth->virt_base_addr) + addr));
	if( register_needs_translation(addr) ){
		VMKIT_PCI_DEBUG("Read  access to Pointer register 0x%lx value 0x%lx\n", addr, val);
		if(*val)	{
			*val = host_to_guest((lvaddr_t)val); //dont translate null pointers
			//if(val) printf("At 0x%lx  is  0x%x\n", val, *((uint32_t *)val));
		}
		VMKIT_PCI_DEBUG("Translated to value 0x%08lx\n", val);
	}
	//VMKIT_PCI_DEBUG("Read from addr 0x%08lx val: 0x%08lx\n", fault_addr, val);


}



static uint32_t function = PCI_DONT_CARE;
//static uint32_t deviceid = PCI_DONT_CARE; //0x1079; //0x10fb; //PCI_DONT_CARE;
//static uint32_t deviceid = 0x1079; //intel e1000
static uint32_t deviceid = 0x10fb; //intel ixgbe

struct pci_device *pci_ethernet_new(struct lpc *lpc, struct guest *g)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_ethernet *host = calloc(1, sizeof(struct pci_ethernet));
    
    pci_ethernet_unique = host;
    host->pci_device = dev;
    guest_info = g;

    //initialize device
    dev->confspace_write = confspace_write;
    dev->confspace_read = confspace_read;
    dev->mem_read = mem_read;
    dev->mem_write = mem_write;
    dev->state = host;
    dev->irq = PCI_ETHERNET_IRQ;
    dev->lpc = lpc;
    
    //Connect to pci server
	errval_t r = pci_client_connect();
	assert(err_is_ok(r));
	printf("vmkitmon: connected to pci\n");
    
    //Register as driver

	r = pci_register_driver_irq((pci_driver_init_fn)e1000_init,
                                PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
								PCI_VENDOR_INTEL, deviceid,
								PCI_DONT_CARE, PCI_DONT_CARE, function,
								e1000_interrupt_handler, dev);

	if(err_is_fail(r)) {
		DEBUG_ERR(r, "ERROR: vmkitmon: pci_register_driver");
	}
	assert(err_is_ok(r));
	printf("vmkitmon: registered ethernet driver, waiting for init..\n");

	/*
	r = pci_register_driver_irq((pci_driver_init_fn)iommu_init,
									PCI_CLASS_SYSTEMPERIPHERAL,
									PCI_SUB_IOMMU, PCI_IF_IOMMU,
									PCI_VENDOR_AMD, deviceid,
									PCI_DONT_CARE, PCI_DONT_CARE, function,
									iommu_interrupt_handler, dev);

	printf("vmkitmon: registered iommu driver, waiting for init..\n");
	 */

    return dev;
}
