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
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include "e1000n.h"

#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define TRACE_N_BM 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK

//#define ENABLE_DEBUGGING_E1000 1
#ifdef ENABLE_DEBUGGING_E1000
static bool local_debug = true;
#define E1000N_DPRINT(x...) do{if(local_debug) printf("e1000n: " x); } while(0)
#else
#define E1000N_DPRINT(x...) ((void)0)
#endif // ENABLE_DEBUGGING_E1000

/*****************************************************************
 * Data types:
 *****************************************************************/

extern uint64_t interrupt_counter;
extern uint64_t total_rx_p_count;
extern uint64_t total_interrupt_time;
extern struct client_closure *g_cl;
extern uint64_t total_processing_time;
extern uint64_t total_rx_datasize;

static uint8_t macaddr[6]; ///< buffers the card's MAC address upon card reset
e1000_t d;  ///< Mackerel state
static bool user_macaddr; /// True iff the user specified the MAC address
static bool use_interrupt = true;

#define MAX_ALLOWED_PKT_PER_ITERATION    (0xff)  // working value
#define DRIVER_RECEIVE_BUFFERS   (1024 * 8) // Number of buffers with driver
#define RECEIVE_BUFFER_SIZE (1600) // MAX size of ethernet packet

#define DRIVER_TRANSMIT_BUFFER   (1024 * 8)

//transmit
static volatile struct tx_desc *transmit_ring;

// Data-structure to map sent buffer slots back to application slots
struct pbuf_desc {
    struct net_queue_manager_binding *sr; // which application binding
    uint64_t spp_index;  // which slot within spp
};
static struct pbuf_desc pbuf_list_tx[DRIVER_TRANSMIT_BUFFER];
//remember the tx pbufs in use

//receive
static volatile union rx_desc *receive_ring;

static void *internal_memory_pa = NULL;
static void *internal_memory_va = NULL;

static uint32_t ether_transmit_index = 0, ether_transmit_bufptr = 0;
/* TODO: check if these variables are used */
static uint32_t receive_index = 0, receive_bufptr = 0;

static uint32_t receive_free = 0;


/*****************************************************************
 * Local states:
 *****************************************************************/
static uint64_t minbase = -1;
static uint64_t maxbase = -1;

static uint64_t bus = PCI_DONT_CARE;
static uint64_t device = PCI_DONT_CARE;
static uint32_t function = PCI_DONT_CARE;
static uint32_t deviceid = PCI_DONT_CARE;

/* FIXME: most probably, I don't need this.  So, remove it.  */
static char *global_service_name = 0;
static uint64_t assumed_queue_id = 0;
static bool handle_free_TX_slot_fn(void);

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
    assert(numbufs < DRIVER_TRANSMIT_BUFFER);
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_index - ether_transmit_bufptr) %
                DRIVER_TRANSMIT_BUFFER);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            DRIVER_TRANSMIT_BUFFER;
    }
    return (nr_free > numbufs);
}

static uint64_t transmit_pbuf(lpaddr_t buffer_address,
                              size_t packet_len, uint64_t offset, bool last,
                              uint64_t client_data, uint64_t spp_index,
                              struct net_queue_manager_binding *sr)
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
    pbuf_list_tx[ether_transmit_index].sr = sr;
    pbuf_list_tx[ether_transmit_index].spp_index = spp_index;

    ether_transmit_index = (ether_transmit_index + 1) % DRIVER_TRANSMIT_BUFFER;
    e1000_tdt_wr(&(d), 0, (e1000_dqval_t){ .val = ether_transmit_index });

    E1000N_DEBUG("ether_transmit_index %"PRIu32"\n", ether_transmit_index);
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
    struct shared_pool_private *spp = cl->spp_ptr;
    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
    uint64_t rtpbuf = sld->no_pbufs;
    if (!can_transmit(rtpbuf)){
        while(handle_free_TX_slot_fn());
        if (!can_transmit(rtpbuf)){
            return ETHERSRV_ERR_CANT_TRANSMIT;
        }
    }
    struct buffer_descriptor *buffer = find_buffer(sld->buffer_id);
    struct buffer_descriptor *cached_buffer = buffer;
    if (buffer == NULL) {
        USER_PANIC("find_buffer failed\n");
        abort();
    }
    // FIXME: code should work with following assert enable.
    // Current problem is that, ARP request packets are reused to send the
    // response, and hence having different buffer_id than of tx buffer
    // assert(buffer == cl->buffer_ptr);

    for (int i = 0; i < rtpbuf; i++) {
        uint64_t slot_index = (cl->tx_index + i) % cl->spp_ptr->c_size;
        sld = &spp->sp->slot_list[slot_index].d;
        assert(sld != NULL);
        if (sld->buffer_id == 0) {
            printf("e1000n: TX malformed packet at %"PRIu64"\n",slot_index);
//            sp_print_metadata(cl->spp_ptr);
            return -1;
        }
        if (cached_buffer->buffer_id == sld->buffer_id) {
            buffer = cached_buffer;
        } else {
            buffer = find_buffer(sld->buffer_id);
        }
        assert(buffer->buffer_id == sld->buffer_id);

        r = transmit_pbuf(buffer->pa, sld->len, sld->offset,
                    i == (rtpbuf - 1), //last?
                    sld->client_data,
                    slot_index, cl->app_connection);
        if(err_is_fail(r)) {
            //E1000N_DEBUG("ERROR:transmit_pbuf failed\n");
            printf("ERROR:transmit_pbuf failed\n");
            return r;
        }
        E1000N_DEBUG("transmit_pbuf done for pbuf %"PRIx64", index %"PRIu64"\n",
            sld->client_data, slot_index);
    } // end for: for each pbuf
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET,  TRACE_EVENT_NNET_TXDRVADD,
        (uint32_t)0);
#endif // TRACE_ONLY_SUB_NNET

    return SYS_ERR_OK;
} // end function: transmit_pbuf_list_fn


static uint64_t find_tx_free_slot_count_fn(void)
{

    uint64_t nr_free;
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_index - ether_transmit_bufptr) %
                DRIVER_TRANSMIT_BUFFER);
    } else {
        nr_free = (ether_transmit_bufptr - ether_transmit_index) %
            DRIVER_TRANSMIT_BUFFER;
    }

    return nr_free;
} // end function: find_tx_queue_len

static bool handle_free_TX_slot_fn(void)
{
    uint64_t ts = rdtsc();
    bool sent = false;
    volatile struct tx_desc *txd;
    if (ether_transmit_bufptr == ether_transmit_index) {
        return false;
    }

    txd = &transmit_ring[ether_transmit_bufptr];
    if (txd->ctrl.legacy.sta_rsv.d.dd != 1) {
        return false;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVSEE,
                0);
#endif // TRACE_ONLY_SUB_NNET


    sent = handle_tx_done(pbuf_list_tx[ether_transmit_bufptr].sr,
            pbuf_list_tx[ether_transmit_bufptr].spp_index);

    ether_transmit_bufptr = (ether_transmit_bufptr + 1)%DRIVER_TRANSMIT_BUFFER;
    netbench_record_event_simple(bm, RE_TX_DONE, ts);
    return true;
}


/*****************************************************************
 * Initialize internal memory for the device
 ****************************************************************/


static int add_desc(uint64_t paddr)
{
    union rx_desc r;
    r.raw[0] = r.raw[1] = 0;
    r.rx_read_format.buffer_address = paddr;

    if(receive_free == DRIVER_RECEIVE_BUFFERS) {
    	//E1000N_DEBUG("no space to add a new receive pbuf\n");
    	printf("no space to add a new receive pbuf\n");
    	/* FIXME: how can you return -1 as error here
    	 * when return type is unsigned?? */
    	return -1;
    }


    receive_ring[receive_index] = r;

    receive_index = (receive_index + 1) % DRIVER_RECEIVE_BUFFERS;
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
	    (DRIVER_RECEIVE_BUFFERS - 1) * RECEIVE_BUFFER_SIZE, NULL);
    if(err_is_fail(r)) {
    	assert(!"frame_alloc failed");
    }

    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));
    mem = frameid.base;

    internal_memory_pa = (void*)mem;

    r = vspace_map_one_frame_attr(&internal_memory_va,
	    (DRIVER_RECEIVE_BUFFERS - 1)* RECEIVE_BUFFER_SIZE, frame,
//	    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
	    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(r)) {
    	assert(!"vspace_map_one_frame failed");
    }

    for(i = 0; i < DRIVER_RECEIVE_BUFFERS - 1; i++) {
    	int ret = add_desc((mem + (i * RECEIVE_BUFFER_SIZE)));
    	assert(ret == 0);
    }
    assert(internal_memory_pa );
}


static void print_rx_bm_stats(bool stop_trace)
{
    if (g_cl == NULL) {
        return;
    }

    if (g_cl->debug_state != 4) {
        return;
    }

    uint64_t cts = rdtsc();

    if (stop_trace) {

#if TRACE_N_BM
    // stopping the tracing
        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_STOP, 0);

        /*
        char *buf = malloc(4096*4096);
        trace_dump(buf, 4096*4096, NULL);
        printf("%s\n", buf);
        */
#endif // TRACE_N_BM

    } // end if: stop_trace

    uint64_t running_time = cts - g_cl->start_ts;
    printf("D:I:%u: RX speed = [%"PRIu64"] packets "
        "data(%"PRIu64") / time(%"PU") = [%f] MB/s ([%f]Mbps) = "
        " [%f]mpps, INT [%"PRIu64"]\n",
        disp_get_core_id(),
        total_rx_p_count, total_rx_datasize, in_seconds(running_time),
        ((total_rx_datasize/in_seconds(running_time))/(1024 * 1024)),
        (((total_rx_datasize * 8)/in_seconds(running_time))/(1024 * 1024)),
        ((total_rx_p_count/in_seconds(running_time))/(double)(1000000)),
        interrupt_counter
        );

    netbench_print_event_stat(bm, RE_COPY, "D: RX CP T", 1);
    netbench_print_event_stat(bm, RE_PROCESSING_ALL, "D: RX processing T", 1);
} // end function: print_rx_bm_stats

static char tmp_buf[2000];
static bool handle_next_received_packet(void)
{
    volatile union rx_desc *rxd;
    void *data = NULL;
    void *buffer_address = NULL;
    struct buffer_descriptor *buffer = NULL;
    size_t len = 0;
    bool new_packet = false;
    tmp_buf[0] = 0; // FIXME: to avoid the warning of not using this variable

    if (receive_bufptr == receive_index) { //no packets received
        return false;
    }

//    E1000N_DEBUG("Inside handle next packet 2\n");
    rxd = &receive_ring[receive_bufptr];

    if ((rxd->rx_read_format.info.status.dd) &&
            (rxd->rx_read_format.info.status.eop)
//            && (!local_pbuf[receive_bufptr].event_sent)
            ) {
        // valid packet received

    E1000N_DPRINT("Potential packet receive [%"PRIu32"]!\n",
            receive_bufptr);
        new_packet = true;

        // FIXME: following two conditions might be repeating, hence
        // extra.  Check it out.
        if(internal_memory_pa == NULL || internal_memory_va == NULL) {
        // E1000N_DEBUG("no internal memory yet#####.\n");
            buffer = NULL;
            // FIXME: control should go out of parent if block
            goto end;
        }

        // Ensures that netd is up and running
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
        total_rx_datasize += len;

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXDRVSEE,
                    (uint32_t) len);
#endif // TRACE_ONLY_SUB_NNET


        // E1000N_DEBUG("packet received of size %zu..\n", len);

        buffer_address = (void*)rxd->rx_read_format.buffer_address;
        data = (buffer_address - internal_memory_pa) + internal_memory_va;

        if (data == NULL || len == 0){
            printf("ERROR: Incorrect packet\n");
            // abort();
            // FIXME: What should I do when such errors occur.
            buffer = NULL;
            goto end;
        }

#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(data, len);
#endif // !defined(__scc__) && !defined(__i386__)

        process_received_packet(data, len);

#if 0
        // This code is useful for RX micro-benchmark
        // only to measures performance of accepting incoming packets
        if (g_cl != NULL) {
            if (g_cl->debug_state == 4) {

                uint64_t ts = rdtsc();

//                memcpy_fast(tmp_buf, data, len);
                process_received_packet(data, len);
                total_processing_time = total_processing_time +
                    (rdtsc() - ts);

            } else {
                process_received_packet(data, len);
            }
        } else {
            process_received_packet(data, len);
        }
#endif // 0

    } // end if: valid packet received
    else {
    	// false alarm. Something else happened, not packet arrival
    	return false;
    }

    end:
//    local_pbuf[receive_bufptr].event_sent = true;
    receive_free--;
    int ret = add_desc(rxd->rx_read_format.buffer_address);
    if(ret < 0){
        printf("ERROR: add_desc failed, so not able to re-use pbuf\n");
    }
    receive_bufptr = (receive_bufptr + 1) % DRIVER_RECEIVE_BUFFERS;
    return new_packet;
} // end function: handle_next_received_packet



static bool benchmark_complete = false;
static uint64_t pkt_size_limit = (1024 * 1024 * 1024); // 1GB
// static uint64_t pkt_limit = 810000;
// static uint64_t start_pkt_limit = 13000;

static uint64_t handle_multiple_packets(uint64_t upper_limit)
{
    uint64_t ts = rdtsc();
    uint8_t local_pkt_count;
    local_pkt_count = 0;

    while(handle_next_received_packet()) {
        ++total_rx_p_count;

#if TRACE_N_BM
        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_DRV_SEE,
                total_rx_p_count);
#endif // TRACE_N_BM


//        if (total_rx_p_count == pkt_limit) {
        if (total_rx_datasize > pkt_size_limit) {
            if (!benchmark_complete) {
                netbench_record_event_simple(bm, RE_PROCESSING_ALL, ts);
                benchmark_complete = true;
                print_rx_bm_stats(true);

                ts = rdtsc();
            }
        }

        ++local_pkt_count;
        if (local_pkt_count == upper_limit) {
            break;
        }
    } // end while:

    netbench_record_event_simple(bm, RE_PROCESSING_ALL, ts);
    return local_pkt_count;

} // end function: handle_multiple_packets


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

    ++interrupt_counter;

#if TRACE_N_BM
        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_DRV_INT,
                interrupt_counter);
#endif // TRACE_N_BM


    E1000N_DPRINT("interrupt msg [%"PRIu64"]!\n", interrupt_counter);

    if(!icr.rxt0) {
        //printf("no packet\n");
        return;
    }
    handle_multiple_packets(MAX_ALLOWED_PKT_PER_ITERATION);

} // end function: e1000_interrupt_handler

/*****************************************************************
 * Polling loop. Called by main and never left again
 ****************************************************************/
//this functions polls all the client's channels as well as the transmit and
//receive descriptor rings
static void polling_loop(void)
{
//	printf("starting polling loop\n");
    errval_t err;
    uint64_t ts;
    struct waitset *ws = get_default_waitset();
    uint64_t poll_count = 0;
    uint8_t jobless_iterations = 0;
    bool no_work = true;

    while (1) {
        no_work = true;
        ++poll_count;

        ts = rdtsc();
        do_pending_work_for_all();
        netbench_record_event_simple(bm, RE_PENDING_WORK, ts);

//        err = event_dispatch(ws); // blocking
        err = event_dispatch_non_block(ws); // nonblocking
        if (err != LIB_ERR_NO_EVENT) {
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch_non_block");
                break;
            } else {
                // Handled some event dispatch
                no_work = false;
            }
        }

#if TRACE_N_BM
        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_DRV_POLL,
                poll_count);
#endif // TRACE_N_BM


        if(handle_multiple_packets(MAX_ALLOWED_PKT_PER_ITERATION) > 0) {
            no_work = false;
        }

        if (no_work) {
            ++jobless_iterations;
            if (jobless_iterations == 10) {
                if (use_interrupt) {
                    thread_yield();
                }
            }
        } // end if: no work

    } // end while: 1
} // end function : polling_loop

/*****************************************************************
 * Init callback
 ****************************************************************/
static void e1000_init(struct device_mem *bar_info, int nr_allocated_bars)
{
    E1000N_DEBUG("starting hardware init\n");
    e1000_hwinit(&d, bar_info, nr_allocated_bars, &transmit_ring,
            &receive_ring, DRIVER_RECEIVE_BUFFERS, DRIVER_TRANSMIT_BUFFER,
            macaddr, user_macaddr, use_interrupt);
    E1000N_DEBUG("Done with hardware init\n");
    setup_internal_memory();

    ethersrv_init(global_service_name, assumed_queue_id, get_mac_address_fn,
		  transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
                  handle_free_TX_slot_fn);
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
                E1000N_DEBUG("MAC= %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                             macaddr[0], macaddr[1], macaddr[2],
                             macaddr[3], macaddr[4], macaddr[5]);
            } else {
                fprintf(stderr, "%s: Error parsing MAC address '%s'\n",
                        argv[0], argv[i]);
                return 1;
            }
        }
        if(strcmp(argv[i],"noirq")==0) {
            use_interrupt = false;
            printf("Driver working in polling mode\n");
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
        r = pci_register_driver_noirq(e1000_init, PCI_CLASS_ETHERNET,
                    PCI_DONT_CARE, PCI_DONT_CARE, PCI_VENDOR_INTEL, deviceid,
                    PCI_DONT_CARE, PCI_DONT_CARE, function);
    }
    if(err_is_fail(r)) {
        DEBUG_ERR(r, "pci_register_driver");
    }
    assert(err_is_ok(r));
    E1000N_DEBUG("registered driver\n");

    polling_loop(); //loop myself
} // end function: main

