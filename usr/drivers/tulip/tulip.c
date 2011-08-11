/** \file
 * \brief DEC Tulip ethernet driver
 *
 * This file is a driver for the Tulip Ethernet controller
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include "tulip_dev.h"
#include <pci/pci.h>
//#include <ethernet/ethernet.h>

#include "tulip.h"

#define BAR_OFFSET 4
#define PCI_CONFIG_HDR_CFIT     0x3c

#define REGISTER_SIZE uint32_t
REGISTER_SIZE * volatile pciconfig;
// XXX Mackerel PCI doesn't work yet:  static struct pci_hdr0_t config;

static struct tulip_t csrs;
static uint8_t mac_address[6];

// Upcall interface to the IP stack
static InputPacket handler;
static void *handler_data;

#define BYTES_PER_DESCRIPTOR 16
#define BYTES_PER_FRAME      1536

// XXX PBAR FIXME!
// XXX PBAR Buffer allocation code doesn't deal with >1 page properly!
#define RX_FRAGMENTS  1
#define TX_FRAGMENTS  1

#define RX_BUFSIZE 4096 // (RX_FRAGMENTS * (BYTES_PER_DESCRIPTOR+BYTES_PER_FRAME))
#define TX_BUFSIZE 4096 // (TX_FRAGMENTS * (BYTES_PER_DESCRIPTOR+BYTES_PER_FRAME))

#define DESCRIPTOR_OFFSET(d) ((d)*BYTES_PER_DESCRIPTOR)
#define BUFFER_OFFSET(b,n) (((n)*BYTES_PER_DESCRIPTOR + (b)*BYTES_PER_FRAME))

struct capref rxcap, txcap;
static uint8_t *volatile rxbufs;
static uint8_t *volatile txbufs;

#ifdef TULIP_TU_DEBUG
static void print_csrs(void)
{
    printf("CSR0:%08x  CSR3:%08x  CSR4:%08x\n", 
        tulip_CSR0_rd(&csrs),tulip_CSR3_rd(&csrs),tulip_CSR4_rd(&csrs));
    printf("CSR5:%08x\n", tulip_CSR5_rd(&csrs));
    printf("CSR6:%08x  CSR7:%08x  CSR8:%08x\n", 
        tulip_CSR6_rd(&csrs),tulip_CSR7_rd(&csrs),tulip_CSR8_rd(&csrs));
    printf("CSR11:%08x  CSR12:%08x  CSR15:%08x\n", 
        tulip_CSR11_rd(&csrs),tulip_CSR12_rd(&csrs),tulip_CSR15_rd(&csrs));
}
static void dump_pkt(uint8_t *pkt, uint32_t len)
{
    for (int i = 0; i < len; i++) {
        printf("%02x", pkt[i]);
    }
    printf("\n");
}
#endif


//
// SROM related methods
//
static void delay(int kernel_ticks)
{
    // kernel ticks in units of 100ns
    // spin until delay elapsed
}

static uint8_t srom_read_preamble[] = {
    0x01, 0x31, 0x57, 0x57, 0x51, 0x31
};

static uint16_t srom_read16(uint32_t addr, uint32_t addrBits)
{
    int i;
    tulip_CSR9_t csr9 = {
        .DATA = 0,
        .SR   = 1,
        .RD   = 1
        
    };

    // This is taken from section 7-8 in 21140A reference manual. We
    // deliberately make all delays as 3 ticks since we don't have
    // sufficient resolution and don't care about 300ns vs 150ns for
    // the SROM accesses.

    //uint32_t srom_base_cmd = 0x4800;

    // Starting byte address to starting word address
    addr >>= 1;

    for (i = 0; i < sizeof(srom_read_preamble); i++)
    {
        uint8_t b = srom_read_preamble[i];
        csr9.DATA = b >> 4;
        tulip_CSR9_wr(&csrs, csr9); // srom_base_cmd | (uint32_t)(b >> 4));    delay(3);
        csr9.DATA = b & 0x0f;
        tulip_CSR9_wr(&csrs, csr9); // srom_base_cmd | (uint32_t)(b & 0x0f));  delay(3);
    }

    // Write address to be read
    for (i = (int)addrBits - 1; i >= 0; --i)
    {
        uint32_t bit = (addr >> i) & 0x01;
        bit <<= 2;
        
        csr9.DATA = bit | 0x01;
        tulip_CSR9_wr(&csrs, csr9); // srom_base_cmd | bit | 0x01);        
        delay(3);
        csr9.DATA = bit | 0x03;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | bit | 0x03);        
        delay(3);
        csr9.DATA = bit | 0x01;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | bit | 0x01);        
        delay(3);
    }

    // Get lsb
    uint32_t r = 0;
    for (i = 7; i >= 0; --i)
    {
        csr9.DATA = 0x03;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x03);              
        delay(3);
        r |= ((tulip_CSR9_rd(&csrs).DATA & 0x08) >> 3) << i;         
        delay(3);
        csr9.DATA = 0x01;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x01);              
        delay(3);
    }

    // Get msb
    for (i = 15; i >= 8; --i)
    {
        csr9.DATA = 0x03;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x03);              
        delay(3);
        r |= ((tulip_CSR9_rd(&csrs).DATA & 0x08) >> 3) << i;         
        delay(3);
        csr9.DATA = 0x01;
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x01);              
        delay(3);
    }

    // Finish up
    csr9.DATA = 0;
    tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd);                         
    delay(3);

    return (uint16_t)r;
}

static int get_srom_width(void)
{
    for (uint32_t i = 6; i < 13; i++)
    {
        uint16_t w = srom_read16(18u, i);
        if (w == 0)
        {
            return i - 1;
        }
    }
    return 6;
}


static uint8_t *read_srom(void)
{
    int srom_width = get_srom_width();
    TU_DEBUG("srom_width=%d\n", srom_width);
    
    int len = 2 << (int)srom_width;
    TU_DEBUG("malloc %d\n", len);
    //breakpoint();
    uint8_t *b = malloc(len);
    TU_DEBUG("done %p\n", b);
    for (int i = 0; i < len; i += 2)
    {
        uint16_t w = srom_read16(i, srom_width);
        b[i]     = (uint8_t) (w >> 8);
        b[i + 1] = (uint8_t) (w & 0xff);
        if (0) TU_DEBUG("%02x%02x", b[i], b[i+1]);
    }
    TU_DEBUG("\n");
    return b;
}

// XXX PBAR move into Mackerel
#define TDES1_TCH (1u<<24)
#define RDES0_OWN (1u<<31)
#define TDES0_OWN (1u << 31)
#define TDES1_LS  (1u << 30)
#define TDES1_FS  (1u << 29)

static void build_chain(uint8_t  *buffer,
                        paddr_t    pa,
                        uint32_t   bufferCount,
                        uint32_t   des0Flags,
                        uint32_t   des1Flags)
{
    uint32_t *des;
    int i;

    for (i = 0; i < bufferCount; i++) {
        des = (uint32_t*)(buffer + DESCRIPTOR_OFFSET(i));
        des[0] = des0Flags;
        des[1] = des1Flags | TDES1_TCH | (BYTES_PER_FRAME & 0x7ff);
        des[2] = pa + BUFFER_OFFSET(i, bufferCount);
        des[3] = pa + DESCRIPTOR_OFFSET((i + 1) % bufferCount);
    }
}

static void *contig_alloc(size_t bufsize, paddr_t *pa)
{
    struct capref frame;
    struct frame_identity phys = { .base = 0, .bits = 0 };

    // Allocate
    if (frame_alloc(&frame, bufsize, &bufsize) != 0) {
        return NULL;
    }

    int r = invoke_frame_identify(frame, &phys);
    assert(r == 0);
    *pa = phys.base;

    // Map it to vspace
    // XXX: simpeter: I suppose this has to be mapped non-cacheable??
    return vspace_map_attr(frame, 0, bufsize,
                           PTABLE_ACCESS_DEFAULT | PTABLE_CACHE_DISABLED,
                           NULL, NULL);
}

static void init_chains(int rxFragments, int txFragments)
{
  // struct capref frame_ci;
  // int rc;
    paddr_t pa = 0;

    TU_DEBUG("\nXXXXXXXXXXXXXXXXXXXXXXXX\n");
    TU_DEBUG("InitChains(rx = %d, tx = %d)\n",
                     rxFragments, txFragments);

    // Turn off receive and transmit
    tulip_CSR6_t mode = tulip_CSR6_rd(&csrs);
    mode.SR = 0; mode.ST = 0; //mode &= ~(CSR6.SR | CSR6.ST);
    tulip_CSR6_wr(&csrs, mode);

    // XXX PBAR This allocation code doesn't deal with >1 page properly!
    TU_DEBUG("RX_BUFSIZE %x\n", RX_BUFSIZE);
    rxbufs = contig_alloc(BASE_PAGE_SIZE /*RX_BUFSIZE*/, &pa);
    TU_DEBUG("RXBUFS @ %p\n", rxbufs);

    TU_DEBUG("PA=%lx\n", pa);

    build_chain((uint8_t*)rxbufs, pa, RX_FRAGMENTS, RDES0_OWN, 0);
    
    tulip_CSR3_wr(&csrs, (uint32_t)pa);
    tulip_CSR2_wr(&csrs, 1);   // Receive poll demand

    txbufs = contig_alloc(BASE_PAGE_SIZE /*TX_BUFSIZE*/, &pa);
    TU_DEBUG("TXBUFS @ %p\n", txbufs);

    TU_DEBUG("PA=%lx\n", pa);

    build_chain((uint8_t*)txbufs, pa, TX_FRAGMENTS, 0, TDES1_FS| TDES1_LS);
    tulip_CSR4_wr(&csrs, (uint32_t)pa);
    tulip_CSR1_wr(&csrs, 1);    // Transmit poll demand

}

static void start_io(void)
{
    tulip_CSR6_t csr6 = {
        .HBD = 1,
        .PS  = 1,
        .TR  = 3,
        .ST  = 1, // XXX NOT YET  1,
        .SR  = 1
    };

    TU_DEBUG("start_io\n");
    
    //csr6.Write32(CSR6.MBO | CSR6.HBD | CSR6.PS | (3u << CSR6.TR_ROLL) |
    //             CSR6.ST);
    
    //WriteStandardSetupFrame();

    // Write CSR6 to start receive and transmit processes
    //
    // This is taken from the state remaining after pxeboot.
    // == 100Mb/s MII/SYM (table 3-43 entry 1000)
    //csr6.Write32(CSR6.MBO | CSR6.HBD | CSR6.PS | (3u << CSR6.TR_ROLL) |
    //             CSR6.ST | CSR6.SR);
    tulip_CSR6_wr(&csrs, csr6);
    
    //TU_DEBUG("Programmed CSR values...\n");
    //print_csrs();

}


static void enable_irq(void)
{
    // Enable interrupts
    // csr7.Write32(CSR7.AI | CSR7.RI | CSR7.TI);
    TU_DEBUG("enable_irq\n");
    tulip_CSR7_wr_raw(&csrs, (1<<16) | (1<<6));

}

static void tulip_handle_interrupt(struct idc_msg *msg, struct capref cap)
{
    tulip_CSR5_t csr5 = tulip_CSR5_rd(&csrs);
    TU_DEBUG("tulip: interrupt\n");
    do {
        // Clear interrupts on card (interrupt bits are "write 1 to clear")
        tulip_CSR5_wr(&csrs, csr5);
        //print_csrs();
    
        if (csr5.RI) {
            TU_DEBUG("RI\n");
            uint32_t * volatile rdes0 = (uint32_t*)(rxbufs + DESCRIPTOR_OFFSET(0));
            if ((*rdes0 & RDES0_OWN) == 0) {
#if 0
                uint32_t len = *rdes0 >>16 & 0x3FFF;
                uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
                dump_pkt(rxpkt, len);
#endif            
                handler(handler_data); // Notify IP stack
                // Give the buffer back to the NIC
                //*rdes0 = RDES0_OWN;
            }
        }
        if (csr5.TI) {
            TU_DEBUG("TI\n");
        }
        
        csr5 = tulip_CSR5_rd(&csrs);
    } while(csr5.NIS);    
    
    //print_csrs();

}

static int register_irq(uint32_t irq)
{
    // Register interrupt handler
    uint64_t badge = idc_handler_register(tulip_handle_interrupt);
    struct capref ep;

    TU_DEBUG("tulip: register_irq %d\n", irq);
    if (endpoint_create(badge, &ep) != SYS_ERR_OK) {
        assert(!"async_endpoint_create failed");
        return -1;
    }
    if (irq_handle(irq, ep) != SYS_ERR_OK) {
        assert(!"Registering IRQ failed");
        return -1;
    }

    return 0;
}

/**
 * \brief Return MAC address.
 *
 * \param mac   Pointer to 6 byte array to hold MAC address.
 */
void ethernet_get_mac_address(uint8_t *mac)
{
    TU_DEBUG("tulip: get_mac_address\n");
    memcpy(mac, mac_address, 6);
}

void ethernet_register_handler(InputPacket input, void *data)
{
    TU_DEBUG("tulip: register_handler\n");
    handler = input;
    handler_data = data;
}

/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 * This is currently a very basic polled-mode transmit which waits until
 * the packet has been sent on the wire.
 *
 * \param packet        Pointer to packet buffer.
 * \param size          Size in bytes of packet.
 */
void ethernet_send_packet(struct pbuf *p)
{
    TU_DEBUG("tulip: send_packet %d called\n", p->tot_len);
    uint32_t * volatile tdes = (uint32_t*)(txbufs + DESCRIPTOR_OFFSET(0));
    uint8_t * volatile txpkt = txbufs + BUFFER_OFFSET(0, 1);
    for (; p != NULL; p = p->next) {
        memcpy(txpkt, p->payload, p->len);
        txpkt += p->len;
    }
    tdes[1] = /*TDES1_IC |*/ TDES1_LS | TDES1_TCH | (p->tot_len & 0x7ff);
    tdes[0] |= TDES0_OWN;
    tulip_CSR1_wr(&csrs, 1);    // Transmit poll demand
    while (tdes[0] & TDES0_OWN) {
        TU_DEBUG("tdes[0] = %x\n", tdes[0]);
        thread_yield();
    }
    TU_DEBUG("tulip: send complete\n");
}

/**
 * \brief Receive Ethernet packet.
 *
 */
uint16_t ethernet_get_next_packet(uint8_t *packet)
{
    uint32_t * volatile rdes0 = (uint32_t*)(rxbufs + DESCRIPTOR_OFFSET(0));
    uint32_t len = *rdes0 >>16 & 0x3FFF;
    uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
    TU_DEBUG("tulip: get_next_packet %d\n", len);
    //dump_pkt(rxpkt, len);
    memcpy(packet, rxpkt, len);
    // Give the buffer back to the NIC
    *rdes0 = RDES0_OWN;
    //tulip_CSR2_wr(&csrs, 1);    // receive poll demand (just in case)
    return len;
}
/**
 * \brief Initialize network interface.
 *
 */
int tulip_initialize_card(struct pci_address *net_card_address)
{
    uint32_t cbio, cfit;
    uint8_t * srom;
    uint32_t irq;
    
    TU_DEBUG("tulip: Initialize() called\n");

    // Set up IO base address 
    cbio = pci_read_conf_header(net_card_address, BAR_OFFSET);    
    TU_DEBUG("CBIO=%08x\n", cbio);
    pci_write_conf_header(net_card_address, BAR_OFFSET, PORTBASE | 0x1);    
    cbio = pci_read_conf_header(net_card_address, BAR_OFFSET);    
    TU_DEBUG("CBIO=%08x\n", cbio);

    cfit = pci_read_conf_header(net_card_address, PCI_CONFIG_HDR_CFIT/4);    
    TU_DEBUG("CFIT=%08x\n", cfit);
    irq = cfit & 0xff;
    TU_DEBUG("Tulip IRQ=%d\n", irq);
    
    tulip_initialize(&csrs, PORTBASE);
#ifdef TULIP_TU_DEBUG
    // Dump config registers
    TU_DEBUG("Initial CSR values...\n");
    print_csrs(void)();
#endif

    TU_DEBUG("Reset...\n");
    tulip_CSR0_t csr0 = tulip_CSR0_rd(&csrs);
    csr0.SWR = 1;
    tulip_CSR0_wr(&csrs, csr0);

#ifdef TULIP_TU_DEBUG
    TU_DEBUG("New CSR values...\n");
    print_csrs();
#endif

    TU_DEBUG("Read SROM...\n");
    srom = read_srom();
    memcpy(mac_address, srom+20, 6);
    TU_DEBUG("MAC Address: %02x:%02x:%02x:%02x:%02x:%02x\n",
        mac_address[0], mac_address[1], mac_address[2], 
        mac_address[3], mac_address[4], mac_address[5]);

    // Build the transmit and receive descriptors
    init_chains(1, 1);

    // Register the interrupt handler and enable IRQs
    register_irq(irq);
    enable_irq();

    // Clear interrupts
    tulip_CSR5_wr(&csrs, tulip_CSR5_rd(&csrs));

    // Start RX and TX DMA engines
    start_io();

#if 0
    // Poll for receive packets
    uint32_t * volatile rdes0 = (uint32_t*)(rxbufs + DESCRIPTOR_OFFSET(0));
    uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
    memset(rxpkt, 0xaa, BYTES_PER_FRAME);

    while(1) {
        //TU_DEBUG("%08x\n", *rdes0);
        if ((*rdes0 & RDES0_OWN) == 0) {
            uint32_t len = *rdes0 >>16 & 0x3FFF;
            TU_DEBUG("Got a packet len %d!\n", len);
            dump_pkt(rxpkt, len);
            print_csrs();
            // Give the buffer back to the NIC
            *rdes0 = RDES0_OWN;

            // Clear pending interrupts
            tulip_CSR5_wr(&csrs, tulip_CSR5_rd(&csrs));
            TU_DEBUG("CSR5=%x\n", tulip_CSR5_rd(&csrs));
        }
        thread_yield();
        
    }
#endif
    
    return 0;
}

