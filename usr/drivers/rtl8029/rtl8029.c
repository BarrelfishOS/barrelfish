/**
 * \file
 * \brief Realtek RTL8029(AS) driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <ethersrv/ethersrv.h>
#include "rtl8029.h"

/// The only instance of the RTL8029AS we're handling
static RTL8029AS_t      rtl;
/// This buffers the card's MAC address upon card reset
static uint8_t          rtl8029_mac[6];

/* driver will initially copy the pacet here. */
static uint8_t packetbuf[PACKET_SIZE];

/* the length of packet copied into packetbuf */
static uint16_t packet_length;


/*
 * The RTL8029(AS) has 32K of memory, starting at address 0x4000.
 * That is page 0x40, as each page is 256 bytes in size. Memory is
 * going up to address 0xc000.
 */


static uint8_t curr_page = READ_START_PAGE;


/**
 * \brief Yield ASIC memory address from page number.
 *
 * \param page  Page number.
 *
 * \return Corresponding memory address.
 */
static inline uint16_t page_to_mem(uint8_t page)
{
    return page << 8;
}

/**
 * \brief Select an RTL8029(AS) register page.
 *
 * This selects one of the 4 RTL8029(AS) register pages.
 *
 * \param page  Page to select.
 */
static void page_select(RTL8029AS_ps_t page)
{
    RTL8029AS_cr_wr(&rtl, (RTL8029AS_cr_t) {
            .sta = 1,
            .ps = page,
            .rd = RTL8029AS_acrdma
        });
}

/**
 * \brief Read from ASIC memory.
 *
 * \param dst           Pointer to buffer to copy data to.
 * \param src           Source address in ASIC memory to read from.
 * \param amount        Number of bytes to transfer.
 */
static void read_mem(uint8_t *dst, int src, int amount)
{
    int remain = amount % 4;
    uint32_t *d = (uint32_t *)dst;
    RTL8029AS_cr_t cr = {
        .sta = 1,
        .rd = RTL8029AS_rrd
    };
    int i;

    RTL8029AS_rbcr_wr(&rtl, amount);    // Amount of bytes to transfer
    RTL8029AS_rsar_wr(&rtl, src);       // Source in NIC mem
    RTL8029AS_cr_wr(&rtl, cr);          // Start read

    // Read PIO 32-bit
    for(i = 0; i < amount - remain; i += 4, d++) {
        *d = RTL8029AS_rdma32_rd(&rtl);
    }

    // Read remaining bytes
    for(; i < amount; i++) {
        dst[i] = RTL8029AS_rdma8_rd(&rtl);
    }

    // Stop read
    cr.rd = RTL8029AS_acrdma;
    RTL8029AS_cr_wr(&rtl, cr);
}


/**
 * \brief Write packet to memory at a particular page.
 *
 * \param page          Destination start page in ASIC memory.
 * \param p             Packet buffer chain to write.
 */
static inline void write_page(uint8_t page, struct client_closure *cl)
{
    RTL8029AS_cr_t cr = {
        .sta = 1,
        .rd = RTL8029AS_rwr
    };
    uint64_t pbuf_len = 0;
    uint16_t dst = page_to_mem(page);

    RTL8029_DEBUG("write page\n");
    RTL8029AS_rbcr_wr(&rtl, cl->len);// Number of bytes to transfer
    RTL8029AS_rsar_wr(&rtl, dst);       // Destination in NIC mem
    RTL8029AS_cr_wr(&rtl, cr);          // Start write


	for (int index = 0; index < cl->rtpbuf; index++) {
/*
		RTL8029_DEBUG("sending %dth rx_pbuf\n", index);

	    RTL8029_DEBUG("pa %p va %p offset %lu\n",
	    		(void *)cl->buffer_ptr->pa, cl->buffer_ptr->va, cl->pbuf[index].offset);
*/
#if defined(__i386__)
		uint8_t *src = (uint8_t *) ((uintptr_t)(cl->buffer_ptr->va + cl->pbuf[index].offset));
#else
        uint8_t *src = (uint8_t *) ((uint64_t)cl->buffer_ptr->va + cl->pbuf[index].offset);
#endif
        pbuf_len = cl->pbuf[index].len;

        uint32_t i = 0;

        /* write bytes until we reach word alignment in the card's memory */
        for (i = 0; dst % sizeof(uint32_t) != 0; i++, dst++) {
//        	RTL8029_DEBUG("sending byte %d\n", i);
            RTL8029AS_rdma8_wr(&rtl, src[i]);
        }
//        RTL8029_DEBUG("sending %d %u for len %lu\n", i, dst, pbuf_len);
        /* write 32-bit words until we don't have any whole words left */
        for (; pbuf_len - i > sizeof(uint32_t);
             i += sizeof(uint32_t), dst += sizeof(uint32_t)) {
/*            RTL8029_DEBUG("sending word %d %u loc %p, data%u\n",
            		i, dst, &src[i], src[i]);
            RTL8029_DEBUG("base is 0x%x\n", rtl.base);
*/
             RTL8029AS_rdma32_wr(&rtl, *(uint32_t*)&src[i]);
        }
//        RTL8029_DEBUG("done with loop 1\n");
        // Write remaining bytes
        for(; i < pbuf_len; i++, dst++) {
            RTL8029AS_rdma8_wr(&rtl, src[i]);
        }
    } /* end for : for each pbuf in the list (for single packet)*/

//    RTL8029_DEBUG("stopping the write\n");

    // Stop write
    cr.rd = RTL8029AS_acrdma;
    RTL8029AS_cr_wr(&rtl, cr);
    //RTL8029_DEBUG("finished writing page!\n");
}

/**
 * \brief Same as read_mem(), but with page granularity.
 *
 * \param dst           Pointer to buffer to copy data to.
 * \param src           Source start page in ASIC memory.
 * \param amount        Number of bytes to transfer.
 */
static inline void read_page(uint8_t *dst, uint8_t page, int amount)
{
    read_mem(dst, page_to_mem(page), amount);
}

/**
 * \brief Reset RTL8029(AS).
 */
static inline void rtl8029_reset(void)
{
    RTL8029AS_reset_rd(&rtl);
}

static inline void print_config(void)
{
    // Print configuration
    page_select(RTL8029AS_rtl8029as);
    char str[256];
    RTL8029AS_config2_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p0);
    RTL8029AS_cr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_isr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p2);
    RTL8029AS_imr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_dcr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_rcr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_tcr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p0);
    RTL8029AS_rsr_pr(str, sizeof(str), &rtl);
    printf("%s", str);
    RTL8029AS_tsr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_cntr0_pr(str, sizeof(str), &rtl);
    printf("%s", str);
    RTL8029AS_cntr1_pr(str, sizeof(str), &rtl);
    printf("%s", str);
    RTL8029AS_cntr2_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    RTL8029AS_crda_pr(str, sizeof(str), &rtl);
    printf("%s", str);
    RTL8029AS_clda_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p2);
    RTL8029AS_pstart_pr(str, sizeof(str), &rtl);
    printf("%s", str);
    RTL8029AS_pstop_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p1);
    RTL8029AS_curr_pr(str, sizeof(str), &rtl);
    printf("%s", str);

    page_select(RTL8029AS_ne2000p0);
    RTL8029AS_bnry_pr(str, sizeof(str), &rtl);
    printf("%s", str);
}


/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 */
static errval_t rtl8029_send_ethernet_packet_fn(struct client_closure *cl)
{
//	RTL8029_DEBUG("sending ethernet packet\n");
    assert(cl->len <= WRITE_BUF_SIZE);

    // Write packet to ASIC memory
    write_page(WRITE_PAGE, cl);
//    RTL8029_DEBUG("page written\n");

    // Set address & size
    RTL8029AS_tpsr_wr(&rtl, WRITE_PAGE);
    RTL8029AS_tbcr_wr(&rtl, cl->len);
//    RTL8029_DEBUG("address set\n");
    // Initiate send
    RTL8029AS_cr_t cr = {
        .sta = 1,
        .txp = 1,
        .rd = RTL8029AS_sp
    };
    RTL8029AS_cr_wr(&rtl, cr);

    // Wait until done...
    while(RTL8029AS_tsr_rd(&rtl).ptx == 0);

    // Tell the client we sent them!!!
    for (int i = 0; i < cl->rtpbuf; i++) {
        notify_client_free_tx(cl->app_connection, cl->pbuf[i].client_data);
    }

    return SYS_ERR_OK;
}


#if 0
uint16_t ethernet_get_next_packet(uint8_t *packet)
{
    memcpy(packet, packetbuf, packet_length);
    return packet_length;
}

void ethernet_register_handler(InputPacket input, void *data)
{
    handler = input;
    handler_data = data;
}

#endif // 0


static void read_ring_buffer(uint8_t *dst, int src, int amount)
{
    int stopaddr = READ_STOP_PAGE << 8;

    if(src + amount < stopaddr) {
        // No ring-buffer wrap-around
        read_mem(dst, src, amount);
    } else {
        int size = stopaddr - src;

        // Read everything up to end of buffer
        read_mem(dst, src, size);
        // Read rest from (wrapped-around) start of buffer
        read_mem(dst + size, page_to_mem(READ_START_PAGE), amount - size);
    }
}

/**
 * \brief Receive Ethernet packet.
 *
 * Reads latest new packet from ASIC packet ring-buffer and calls
 * higher-level receive function to process packet now in main
 * memory.
 *
 * Assumes card is at register page 0.
 */
static void rtl8029_receive_packet(void)
{
    struct {
        RTL8029AS_rsr_t rsr;
        uint8_t         next_page;
        uint16_t        length;
    } __attribute__ ((packed)) status;

    // Read packet status (first 4 bytes before Ethernet header)
    read_page((uint8_t *)&status, curr_page, sizeof(status));

    assert(status.rsr.prx == 1);
    assert(status.length <= PACKET_SIZE);
    int pos = curr_page + ((status.length + 4 + 255) >> 8);
    assert(status.next_page == (pos >= READ_STOP_PAGE ? pos - (READ_BUF_SIZE >> 8) : pos));

    // Read packet
    packet_length = status.length - sizeof(status);
    read_ring_buffer(packetbuf, page_to_mem(curr_page) + sizeof(status),
                     packet_length);

     RTL8029_DEBUG("................... Packet received (length = %d)\n",
                  status.length);

    // Update boundary
    curr_page = status.next_page;
    RTL8029AS_bnry_wr(&rtl, curr_page);
}

/**
 * \brief RTL8029AS IRQ handler
 *
 * This handler assumes the card is at page 0.
 *
 * The order of actions in this function is important. An interrupt
 * needs to be acknowledged to the card first, before reading packets
 * from the card. Otherwise a race between reading packets and newly
 * received packets arises and packet reception can be delayed
 * arbitrarily.
 */
static void rtl8029_handle_interrupt(void *arg)
{
//    thread_mutex_lock(&driver_lock);

    RTL8029AS_irq_t isr = RTL8029AS_isr_rd(&rtl);
    RTL8029_DEBUG("interrupt came in.\n");
    // 1. Acknowledge all interrupt causes
    RTL8029AS_irq_t nisr = {
        .prx = 1,
        .ptx = 1,
        .rxe = 1,
        .txe = 1,
        .ovw = 1,
        .cnt = 1,
        .rdc = 1,
        .rst = 1
    };
    RTL8029AS_isr_wr(&rtl, nisr);

    // 2. Get card's current packet pointer
    page_select(RTL8029AS_ne2000p1);
    uint8_t curr = RTL8029AS_curr_rd(&rtl);
    page_select(RTL8029AS_ne2000p0);

    // 3. Process current interrupt causes
    if(isr.prx) {
        // Read as many packets as possible
        while(curr_page != curr) {
            rtl8029_receive_packet();

        	assert(packet_length <= 1522);
        	assert(packet_length > 0);

        	/* Ensures that netd is up and running */
        	if(waiting_for_netd()){
        		RTL8029_DEBUG("still waiting for netd to register buffers\n");
        		return;
        	}
			/* FIXME: Not sure if its good idea to call
			 * process_received_packet in interrupt handler. */
        	/* FIXME: find out if this is real interrupt handler,
        	 * or just message generated for interrupt handler. */
            // Call handler if packet received
        	process_received_packet(packetbuf, packet_length);
        }
    }

//    thread_mutex_unlock(&driver_lock);
}


/**
 * \brief Initialize RTL8029(AS).
 *
 * \param net_card_address      Pointer to card's PCI configuration.
 *
 * \return 0 on success. Failure code otherwise.
 */
static int rtl8029_initialize_card(void)
{
//    thread_mutex_lock(&driver_lock);
    printf("Initializing RTL8029(AS)...\n");

//    uint32_t cbio = pci_read_conf_header(net_card_address, BAR_OFFSET);
/*     printf("My CBIO=0x%x\n", cbio); */
//    uint32_t cfit = pci_read_conf_header(net_card_address, PCI_CONFIG_HDR_CFIT/4);
/*     printf("My CFIT=0x%x\n", cfit & 0xf); */

    uint32_t cbio = RTL8029_IOBASE;
//    uint32_t cfit
    uint16_t portbase = cbio & ~0x1;
    RTL8029AS_initialize(&rtl, portbase);

    // Reset card
    RTL8029AS_reset_rd(&rtl);

    // Identify RTL8029AS
    char rtl8029id[2];

    rtl8029id[0] = RTL8029AS_rtl8029id0_rd(&rtl);
    rtl8029id[1] = RTL8029AS_rtl8029id1_rd(&rtl);

    if(rtl8029id[0] == 'P' && rtl8029id[1] == 'C') {
        printf("RTL8029AS identified\n");
    } else {
        printf("This is not an RTL8029AS!\n");
        return -1;
    }
    printf("RTL base is %d\n",rtl.base);

    // Read my MAC address
    page_select(RTL8029AS_ne2000p1);
    rtl8029_mac[0] = RTL8029AS_par0_rd(&rtl);
    rtl8029_mac[1] = RTL8029AS_par1_rd(&rtl);
    rtl8029_mac[2] = RTL8029AS_par2_rd(&rtl);
    rtl8029_mac[3] = RTL8029AS_par3_rd(&rtl);
    rtl8029_mac[4] = RTL8029AS_par4_rd(&rtl);
    rtl8029_mac[5] = RTL8029AS_par5_rd(&rtl);

    printf("My MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
           rtl8029_mac[0], rtl8029_mac[1], rtl8029_mac[2], rtl8029_mac[3],
           rtl8029_mac[4], rtl8029_mac[5]);

    // Start the card
    page_select(RTL8029AS_ne2000p0);
    RTL8029AS_cr_t cr = RTL8029AS_cr_rd(&rtl);
    cr.stp = 0;
    RTL8029AS_cr_wr(&rtl, cr);

    // Clear interrupt status register
    RTL8029AS_irq_t isr = {
        .prx = 1,
        .ptx = 1,
        .rxe = 1,
        .txe = 1,
        .ovw = 1,
        .cnt = 1,
        .rdc = 1,
        .rst = 1
    };
    RTL8029AS_isr_wr(&rtl, isr);

    /*
    // Register interrupt handler
    uint64_t badge = idc_handler_register(rtl8029_handle_interrupt);

    struct capref ep;
    if (endpoint_create(badge, &ep) != SYS_ERR_OK) {
        assert(!"endpoint_create failed");
        return -1;
    }
    if (irq_handle(cfit & 0xf, ep) != SYS_ERR_OK) {
        assert(!"Registering IRQ failed");
        return -1;
    }
*/
    // Set byte-wide PIO transfer
    RTL8029AS_dcr_t dcr = {
        .wts = 0
    };
    RTL8029AS_dcr_wr(&rtl, dcr);

    // Setup on-card receive ring-buffer
    page_select(RTL8029AS_ne2000p1);
    RTL8029AS_curr_wr(&rtl, READ_START_PAGE);
    page_select(RTL8029AS_ne2000p0);
    RTL8029AS_pstart_wr(&rtl, READ_START_PAGE);
    RTL8029AS_pstop_wr(&rtl, READ_STOP_PAGE);
    RTL8029AS_bnry_wr(&rtl, READ_START_PAGE);

    // Enable interrupts (IRQ handler assumes we're at page 0!)
    RTL8029AS_irq_t imr = {
        .prx = 1
/*         .ptx = 1, */
/*         .rdc = 1 */
    };
    RTL8029AS_imr_wr(&rtl, imr);

//    thread_mutex_unlock(&driver_lock);
    return 0;
}

static void get_mac_address_fn(uint8_t *mac)
{
    memcpy(mac, rtl8029_mac, 6);
}

static void rtl8029_init(void)
{
	/* FIXME: use correct name, and make apps and netd
	 * work with multiple service names for driver. */
	char *service_name = "rtl8029";
	RTL8029_DEBUG("starting hardware init\n");
	rtl8029_initialize_card();
	/* FIXME: do hardware init*/
	RTL8029_DEBUG("Done with hardware init\n");

	ethersrv_init(service_name, get_mac_address_fn,
			rtl8029_send_ethernet_packet_fn);
}

/**
 * \brief Initialize rtl8029 driver as legacy driver.
 *
 *
 */
static errval_t legacy_rtl8029_driver_init(void)
{
	/* FIXME: pci_client_connect returns int and not errval_t.
	 * So, change the pci_client_connect() */
    errval_t err = pci_client_connect();
    if (err_is_fail(err)) {
    	return err;
    }
    RTL8029_DEBUG("connected to pci\n");

    return pci_register_legacy_driver_irq(rtl8029_init, RTL8029_IOBASE,
						RTL8029_IOEND, RTL8029_IRQ,
						rtl8029_handle_interrupt, NULL);
}


//this functions polls all the client's channels as well as the transmit and
//receive descriptor rings
static void polling_loop(void)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
//        RTL8029_DEBUG("inside event dispatch\n");
/*        notify_client_next_free_tx();
*/
    }
}


int main(int argc, char *argv[])
{
	errval_t err;
    RTL8029_DEBUG("Starting rtl8029 standalone driver.....\n");
#ifdef CONFIG_QEMU_NETWORK
    printf("Starting RTL8029 for QEMU\n");
#else // CONFIG_QEMU_NETWORK
    printf("Starting RTL8029 for hardware\n");
#endif // CONFIG_QEMU_NETWORK
    // Initialize driver
    err = legacy_rtl8029_driver_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "legacy_rtl8029_driver_init\n");
    }
    RTL8029_DEBUG("registered driver\n");

    polling_loop(); //loop myself
}

