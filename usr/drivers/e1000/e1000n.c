/**
 * \file
 * \brief Intel e1000 driver
 *
 * This file is a driver for the PCI Express e1000 card
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ethersrv/ethersrv.h>
#include "e1000n.h"
#include <trace/trace.h>
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

/*****************************************************************
 * Data types:
 *****************************************************************/

static uint8_t macaddr[6]; ///< buffers the card's MAC address upon card reset
e1000_t d;  ///< Mackerel state
static bool user_macaddr; /// True iff the user specified the MAC address
static bool use_interrupt = true;

//transmit
static volatile struct tx_desc *transmit_ring;
static struct pbuf_desc tx_pbuf[TRANSMIT_BUFFERS]; //remember the tx pbufs in use

//receive
static volatile union rx_desc *receive_ring;

static void *internal_memory_pa = NULL;
static void *internal_memory_va = NULL;

static uint32_t ether_transmit_index = 0, ether_transmit_bufptr = 0;
static uint32_t receive_index = 0, receive_bufptr = 0; /* TODO: check if these variables are used */
static uint32_t receive_free = 0;
//remember the pbuf_id and the connection to the client which provided
//the pbuf at the same index in the receive_ring as here, so that we can notify
//the client in wich buffer there is new data.
static struct pbuf_desc local_pbuf[RECEIVE_BUFFERS];


/*****************************************************************
 * Local states:
 *****************************************************************/
uint64_t minbase = -1;
uint64_t maxbase = -1;

uint64_t bus = PCI_DONT_CARE;
uint64_t device = PCI_DONT_CARE;
uint32_t function = PCI_DONT_CARE;
uint32_t deviceid = PCI_DONT_CARE;

/* FIXME: most probably, I don't need this.  So, remove it.  */
char *global_service_name = 0;


/*****************************************************************
 * MAC address
 ****************************************************************/
/* NOTE: This function will get called from ethersrv.c */
static void get_mac_address_fn(uint8_t *mac)
{
    memcpy(mac, macaddr, sizeof(macaddr));
}

static bool parse_mac(uint8_t *mac, const char *str)
{
    for (int i = 0; i < 6; i++) {
        char *next = NULL;
        unsigned long val = strtoul(str, &next, 16);
        if (val > UINT8_MAX || next == NULL
            || (i == 5 && *next != '\0')
            || (i < 5 && (*next != ':' && *next != '-'))) {
            return false; // parse error
        }
        mac[i] = val;
        str = next + 1;
    }

    return true;
}

/*****************************************************************
 * Transmit logic
 ****************************************************************/
/* check if there are enough free buffers with driver,
 * so that packet can be sent
 * */
static bool can_transmit(int numbufs)
{
    uint64_t nr_free;
    assert(numbufs < TRANSMIT_BUFFERS);
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = TRANSMIT_BUFFERS -
            ((ether_transmit_index - ether_transmit_bufptr) %
                TRANSMIT_BUFFERS);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            TRANSMIT_BUFFERS;
    }
    return (nr_free > numbufs);
}

static uint64_t transmit_pbuf(lpaddr_t buffer_address,
                              size_t packet_len, uint64_t offset, bool last,
                              uint64_t client_data,
                              struct ether_binding *sr)
{

    struct tx_desc tdesc;

    tdesc.buffer_address = (uint64_t)buffer_address + offset;
    tdesc.ctrl.raw = 0;
    tdesc.ctrl.legacy.data_len = packet_len;
    tdesc.ctrl.legacy.cmd.d.rs = 1;
    tdesc.ctrl.legacy.cmd.d.ifcs = 1;
    tdesc.ctrl.legacy.cmd.d.eop = (last ? 1 : 0);

    /* FIXME: the packet should be copied into separate location, so that
     * application can't temper with it. */
    transmit_ring[ether_transmit_index] = tdesc;
    tx_pbuf[ether_transmit_index].paddr = (uint64_t)buffer_address + offset;
    tx_pbuf[ether_transmit_index].event_sent = false;
    tx_pbuf[ether_transmit_index].sr = sr;
    tx_pbuf[ether_transmit_index].client_data = client_data;
    tx_pbuf[ether_transmit_index].last = last;
    ether_transmit_index = (ether_transmit_index + 1) % TRANSMIT_BUFFERS;
    e1000_tdt_wr(&(d), 0, (e1000_dqval_t){ .val = ether_transmit_index });

    /* Actual place where packet is sent.  Adding trace_event here */
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NO_S,
    		(uint32_t)client_data);
#endif // TRACE_ETHERSRV_MODE

    return 0;
}


/* Send the buffer to device driver TX ring.
 * NOTE: This function will get called from ethersrv.c */
static errval_t transmit_pbuf_list_fn(struct client_closure *cl)
{
	errval_t r;
	if (!can_transmit(cl->rtpbuf)){
		return ETHSRV_ERR_CANT_TRANSMIT;
	}
	for (int i = 0; i < cl->rtpbuf; i++) {
		r = transmit_pbuf(cl->buffer_ptr->pa,
				cl->pbuf[i].len, cl->pbuf[i].offset, i
						== (cl->nr_transmit_pbufs - 1), //last?
				cl->pbuf[i].client_data, cl->app_connection);
		if(err_is_fail(r)) {
			//E1000N_DEBUG("ERROR:transmit_pbuf failed\n");
			printf("ERROR:transmit_pbuf failed\n");
			return r;
		}
	}
	return SYS_ERR_OK;
}

static uint64_t find_tx_free_slot_count_fn(void)
{
    uint64_t nr_free;
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = TRANSMIT_BUFFERS -
            ((ether_transmit_index - ether_transmit_bufptr) %
                TRANSMIT_BUFFERS);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            TRANSMIT_BUFFERS;
    }

    return nr_free;
} // end function: find_tx_queue_len

static bool check_for_free_TX_buffer(void)
{
    bool sent = false;
    volatile struct tx_desc *txd;
    if (ether_transmit_bufptr == ether_transmit_index) {
        return false;
    }

    txd = &transmit_ring[ether_transmit_bufptr];
    if (txd->ctrl.legacy.sta_rsv.d.dd == 1) {
        if (tx_pbuf[ether_transmit_bufptr].last == true) {

        	sent = notify_client_free_tx(tx_pbuf[ether_transmit_bufptr].sr,
        			tx_pbuf[ether_transmit_bufptr].client_data,
                                find_tx_free_slot_count_fn(), 0);
        }
        ether_transmit_bufptr = (ether_transmit_bufptr + 1) % TRANSMIT_BUFFERS;
    }
    return sent;
}


/*****************************************************************
 * Initialize internal memory for the device
 ****************************************************************/


static int add_desc(uint64_t paddr)
{
    union rx_desc r;
    r.raw[0] = r.raw[1] = 0;
    r.rx_read_format.buffer_address = paddr;

    if(receive_free == RECEIVE_BUFFERS) {
    	//E1000N_DEBUG("no space to add a new receive pbuf\n");
    	printf("no space to add a new receive pbuf\n");
    	/* FIXME: how can you return -1 as error here
    	 * when return type is unsigned?? */
    	return -1;
    }

    receive_ring[receive_index] = r;
    local_pbuf[receive_index].sr = NULL;
    local_pbuf[receive_index].pbuf_id = 0;
    local_pbuf[receive_index].paddr = paddr; //remember for later freeing
    local_pbuf[receive_index].len = RECEIVE_BUFFER_SIZE;
    local_pbuf[receive_index].event_sent = false;


    receive_index = (receive_index + 1) % RECEIVE_BUFFERS;
    e1000_rdt_wr(&d, 0, (e1000_dqval_t){ .val=receive_index } );
    receive_free++;
    return 0;
}

static void setup_internal_memory(void)
{
    struct capref frame;
    struct frame_identity frameid;
    lpaddr_t mem;
    errval_t r;
    uint32_t i;

    E1000N_DEBUG("Setting up internal memory for receive\n");
    r = frame_alloc(&frame,
	    (RECEIVE_BUFFERS - 1) * RECEIVE_BUFFER_SIZE, NULL);
    if(err_is_fail(r)) {
    	assert(!"frame_alloc failed");
    }

    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));
    mem = frameid.base;

    internal_memory_pa = (void*)mem;

    r = vspace_map_one_frame_attr(&internal_memory_va,
	    (RECEIVE_BUFFERS - 1)* RECEIVE_BUFFER_SIZE, frame,
	    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(r)) {
    	assert(!"vspace_map_one_frame failed");
    }

    for(i = 0; i < RECEIVE_BUFFERS - 1; i++) {
    	int ret = add_desc((mem + (i * RECEIVE_BUFFER_SIZE)));
    	assert(ret == 0);
    }
    assert(internal_memory_pa );
}


static bool handle_next_received_packet(void)
{
    volatile union rx_desc *rxd;
    void *data = NULL;
    void *buffer_address = NULL;
    struct buffer_descriptor *buffer = NULL;
    size_t len = 0;
    bool new_packet = false;

//    E1000N_DEBUG("Potential packet receive!\n");
    if (receive_bufptr == receive_index) { //no packets received
        return false;
    }

//    E1000N_DEBUG("Inside handle next packet 2\n");
    rxd = &receive_ring[receive_bufptr];

    if ((rxd->rx_read_format.info.status.dd) &&
    		(rxd->rx_read_format.info.status.eop) &&
    		(!local_pbuf[receive_bufptr].event_sent)) { /* valid packet received */

    	new_packet = true;

    	/* FIXME: following two conditions might be repeating, hence
    	 * extra.  Check it out. */
		if(internal_memory_pa == NULL || internal_memory_va == NULL) {
//        	    E1000N_DEBUG("no internal memory yet#####.\n");
			buffer = NULL;
			/* FIXME: control should go out of parent if block */
			goto end;
		}

		/* Ensures that netd is up and running */
		if(waiting_for_netd()){
			E1000N_DEBUG("still waiting for netd to register buffers\n");
			buffer = NULL;
			goto end;
		}


    	len = rxd->rx_read_format.info.length;
    	if (len < 0 || len > 1522) {
    		E1000N_DEBUG("ERROR: pkt with len %zu\n", len);
    		goto end;
    	}

        // E1000N_DEBUG("packet received of size %zu..\n", len);

    	buffer_address = (void*)rxd->rx_read_format.buffer_address;
		data = (buffer_address - internal_memory_pa)
			+ internal_memory_va;

		if (data == NULL || len == 0){
			printf("ERROR: Incorrect packet\n");
			// abort();
			/* FIXME: What should I do when such errors occur. */
			buffer = NULL;
			goto end;
		}
		process_received_packet(data, len);

    } /* end if: valid packet received */
    else {
    	/* false alarm. Something else happened, not packet arrival */
    	return false;
    }

    end:
	local_pbuf[receive_bufptr].event_sent = true;
	receive_free--;
	int ret = add_desc(rxd->rx_read_format.buffer_address);
	if(ret < 0){
		printf("ERROR: add_desc failed, so not able to re-use pbuf\n");
	}
	receive_bufptr = (receive_bufptr + 1) % RECEIVE_BUFFERS;

	return new_packet;
}


/*****************************************************************
 * Polling loop. Called by main and never left again
 ****************************************************************/
//this functions polls all the client's channels as well as the transmit and
//receive descriptor rings
static void polling_loop(void)
{
	printf("starting polling loop\n");
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
//    	printf("polling loop: waiting for event\n");
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
//        printf("polling loop: event handled\n");
        check_for_free_TX_buffer();
        if(!use_interrupt) {
            handle_next_received_packet();
//            printf("polling loop: handled pending packets\n");
        }
    }
}

/*****************************************************************
 * Interrupt handler
 ****************************************************************/
static void e1000_interrupt_handler(void *arg)
{
    // Read & acknowledge interrupt cause(s)
    e1000_intreg_t icr = e1000_icr_rd(&d);
//    printf("e1000n: packet interrupt\n");
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_I, 0);
#endif // TRACE_ETHERSRV_MODE


    if(!icr.rxt0) {
        printf("no packet\n");
        return;
    }
//    E1000N_DEBUG("#####Interrupt came in\n");
    if(waiting_for_netd()){
    	return;
    }

    while(handle_next_received_packet());
}

/*****************************************************************
 * Init callback
 ****************************************************************/
static void e1000_init(struct device_mem *bar_info, int nr_allocated_bars)
{
	E1000N_DEBUG("starting hardware init\n");
    e1000_hwinit(&d, bar_info, nr_allocated_bars, &transmit_ring, &receive_ring,
                 RECEIVE_BUFFERS, TRANSMIT_BUFFERS, macaddr, user_macaddr,
                 use_interrupt);
    E1000N_DEBUG("Done with hardware init\n");
    setup_internal_memory();
    ethersrv_init(global_service_name, get_mac_address_fn,
		  transmit_pbuf_list_fn, find_tx_free_slot_count_fn);
}


/*****************************************************************
 * Main:
 ****************************************************************/

int main(int argc, char **argv)
{
    char *service_name = 0;
    errval_t r;

    E1000N_DEBUG("e1000 standalone driver started.\n");

    E1000N_DEBUG("argc = %d\n", argc);
    for (int i = 0; i < argc; i++) {
        E1000N_DEBUG("arg %d = %s\n", i, argv[i]);
        if(strncmp(argv[i],"affinitymin=",strlen("affinitymin="))==0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            E1000N_DEBUG("minbase = %lu\n", minbase);
        }
        if(strncmp(argv[i],"affinitymax=",strlen("affinitymax=")-1)==0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            E1000N_DEBUG("maxbase = %lu\n", maxbase);
        }
        if(strncmp(argv[i],"servicename=",strlen("servicename=")-1)==0) {
            service_name = argv[i] + strlen("servicename=");
            E1000N_DEBUG("service name = %s\n", service_name);
        }
        if(strncmp(argv[i],"bus=",strlen("bus=")-1)==0) {
            bus = atol(argv[i] + strlen("bus="));
            E1000N_DEBUG("bus = %lu\n", bus);
        }
        if(strncmp(argv[i],"device=",strlen("device=")-1)==0) {
            device = atol(argv[i] + strlen("device="));
            E1000N_DEBUG("device = %lu\n", device);
        }
        if(strncmp(argv[i],"function=",strlen("function=")-1)==0) {
            function = atol(argv[i] + strlen("function="));
            E1000N_DEBUG("function = %u\n", function);
        }
        if(strncmp(argv[i],"deviceid=",strlen("deviceid=")-1)==0) {
            deviceid = strtoul(argv[i] + strlen("deviceid="), NULL, 0);
            E1000N_DEBUG("deviceid = %u\n", deviceid);
            printf("### deviceid = %u\n", deviceid);

        }
        if(strncmp(argv[i],"mac=",strlen("mac=")-1)==0) {
            if (parse_mac(macaddr, argv[i] + strlen("mac="))) {
                user_macaddr = true;
                E1000N_DEBUG("MAC = %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                             macaddr[0], macaddr[1], macaddr[2],
                             macaddr[3], macaddr[4], macaddr[5]);
            } else {
                fprintf(stderr, "%s: Error parsing MAC address '%s'\n", argv[0], argv[i]);
                return 1;
            }
        }
        if(strcmp(argv[i],"noirq")==0) {
            use_interrupt = false;
            E1000N_DEBUG("Polling mode\n");
            USER_PANIC("e1000n: Polling mode doesn't work currently, see Ticket #172\n");
        }
    }

    if ((minbase != -1) && (maxbase != -1)) {
        E1000N_DEBUG("set memory affinity [%lx, %lx]\n", minbase, maxbase);
        ram_set_affinity(minbase, maxbase);
    }
    if (service_name == 0) {
        service_name = (char *)malloc(sizeof("e1000") + 1);
        strncpy(service_name, "e1000", sizeof("e1000") + 1);
        E1000N_DEBUG("set the service name to %s\n", service_name);
    }

    global_service_name = service_name;

    // Register our device driver
    r = pci_client_connect();
    assert(err_is_ok(r));
    E1000N_DEBUG("connected to pci\n");

    if(use_interrupt) {
        printf("class %x: vendor %x, device %x, function %x\n",
                PCI_CLASS_ETHERNET, PCI_VENDOR_INTEL, deviceid,
                function);
        r = pci_register_driver_irq(e1000_init, PCI_CLASS_ETHERNET,
                                    PCI_DONT_CARE, PCI_DONT_CARE,
                                    PCI_VENDOR_INTEL, deviceid,
                                    PCI_DONT_CARE, PCI_DONT_CARE, function,
                                    e1000_interrupt_handler, NULL);
    } else {
        r = pci_register_driver_noirq(e1000_init, PCI_CLASS_ETHERNET, PCI_DONT_CARE,
                                      PCI_DONT_CARE, PCI_VENDOR_INTEL, deviceid,
                                      PCI_DONT_CARE, PCI_DONT_CARE, function);
    }
    if(err_is_fail(r)) {
        DEBUG_ERR(r, "pci_register_driver");
    }
    assert(err_is_ok(r));
    E1000N_DEBUG("registered driver\n");

    polling_loop(); //loop myself
}
