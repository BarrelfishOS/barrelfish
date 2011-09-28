/**
 * \file
 * \brief Generic server part for most ethernet drivers.
 * Current drivers using this server code are
 * -- e1000n
 * -- rtl8029
 *
 * This file exports the PCI Express e1000 card as a service
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>

#include <stdio.h>
#include <string.h>
#include <trace/trace.h>
#include <ethersrv/ethersrv.h>
#include <bfdmuxvm/vm.h>
#include <if/ether_defs.h>
#include <if/ether_control_defs.h>
#include "ethersrv_debug.h"

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE
/*****************************************************************
 * Constants:
 *****************************************************************/

#define LAST_ACCESSED_BYTE_ARP 12
#define LAST_ACCESSED_BYTE_TRANSPORT 36



/* This is client_closure for filter management */
struct client_closure_FM {
	struct ether_control_binding *app_connection; /* FIXME: Do I need this? */
	struct cont_queue *q;
/* FIXME: this should contain the registered buffer ptr */
};

/* ethersrv specific debug state indicator */
static int debug_state = 0;

/* NETD connections */
#define NETD_BUF_NR 2
static struct ether_binding *netd[NETD_BUF_NR];

// Counter for dropped packets
static uint64_t dropped_pkt_count = 0;
/*****************************************************************
 * Prototypes
 *****************************************************************/

static void register_buffer(struct ether_binding *cc, struct capref cap);
static void register_pbuf(struct ether_binding *cc, uint64_t pbuf_id,
		uint64_t paddr, uint64_t len);
static void
		transmit_packet(struct ether_binding *cc, uint64_t nr_pbufs,
				uint64_t buffer_id, uint64_t len, uint64_t offset,
				uint64_t client_data);
static void get_mac_addr(struct ether_binding *cc);
static void print_statistics_handler(struct ether_binding *cc);
static void print_cardinfo_handler(struct ether_binding *cc);
static void debug_status(struct ether_binding *cc, uint8_t state);

static void register_filter_memory_request(struct ether_control_binding *cc,
		struct capref mem_cap);
static void register_filter(struct ether_control_binding *cc, uint64_t id,
            uint64_t len_rx, uint64_t len_tx, uint64_t buffer_id_rx,
                            uint64_t buffer_id_tx, uint64_t ftype, uint64_t paused);
static void register_arp_filter(struct ether_control_binding *cc, uint64_t id,
		uint64_t len_rx, uint64_t len_tx);
static void deregister_filter(struct ether_control_binding *cc,
        uint64_t filter_id);
static void re_register_filter(struct ether_control_binding *cc,
        uint64_t filter_id, uint64_t buffer_id_rx, uint64_t buffer_id_tx);
static void pause_filter(struct ether_control_binding *cc, uint64_t filter_id,
                         uint64_t buffer_id_rx, uint64_t buffer_id_tx);


/*****************************************************************
 * VTABLE
 *****************************************************************/

// Initialize service
static struct ether_rx_vtbl rx_ether_vtbl = {
		.register_buffer = register_buffer,
		.register_pbuf = register_pbuf,
		.transmit_packet = transmit_packet,
		.get_mac_address = get_mac_addr,
		.print_statistics = print_statistics_handler,
		.print_cardinfo = print_cardinfo_handler,
		.debug_status = debug_status,
    };

// Initialize interface for ether_control channel
static struct ether_control_rx_vtbl rx_ether_control_vtbl = {
		.register_filter_memory_request = register_filter_memory_request,
		.register_filter_request = register_filter,
		.re_register_filter_request = re_register_filter,
		.deregister_filter_request = deregister_filter,
		.register_arp_filter_request = register_arp_filter,
                .pause = pause_filter,
};

/*****************************************************************
 * Pointers to driver functionalities:
 *****************************************************************/
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
//static ether_can_transmit_t ether_can_transmit_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;

/*****************************************************************
 * Local states:
 *****************************************************************/

int client_no = 0;
static uint64_t buffer_id_counter = 0;
static uint64_t filter_id_counter = 0;
uint64_t netd_buffer_count = 0;
static char *my_service_name = NULL;

static struct buffer_descriptor *first_app_b = NULL;


/* Memory used for filter registration : ps */
// registered buffers:
static struct buffer_descriptor *buffers_list;
// filters state:
static struct filter *rx_filters;
static struct filter arp_filter_rx;
static struct filter arp_filter_tx;



/*****************************************************************
 * Message handlers:
 ****************************************************************/
static errval_t send_new_buffer_id(struct q_entry entry) {
	//    ETHERSRV_DEBUG("send_new_buffer_id -----\n");

	struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
	struct client_closure *ccl = (struct client_closure *) b->st;
	if (b->can_send(b)) {
		return b->tx_vtbl.new_buffer_id(b, MKCONT(cont_queue_callback, ccl->q),
				entry.plist[0], entry.plist[1]);
		/* entry.error,    entry.buffer_id */
	} else {
		ETHERSRV_DEBUG("send_new_buffer_id Flounder busy.. will retry\n");
		return FLOUNDER_ERR_TX_BUSY;
	}
}

static uint64_t metadata_size = sizeof(struct pbuf_desc) * RECEIVE_BUFFERS
		+ sizeof(struct tx_pbuf) * TRANSMIT_BUFFERS;

static void register_buffer(struct ether_binding *cc, struct capref cap)
{

	errval_t err;
	int i;
	struct buffer_descriptor *buffer = (struct buffer_descriptor *)
					((struct client_closure *)(cc->st))->buffer_ptr;

	/* FIXME: replace the name netd with control_channel */
	for (i = 0; i < NETD_BUF_NR; i++) {
		if (netd[i] == cc) {
			ETHERSRV_DEBUG("buffer registered with netd connection\n");
			netd_buffer_count++;
		}
	}

	buffer_id_counter++;

	buffer->buffer_id = buffer_id_counter;
//	printf("buffer gets id %lu\n", buffer->buffer_id);
	if (buffer->buffer_id == 3){
		first_app_b = buffer;
//		printf("setting up first app %lu\n", first_app_b->buffer_id);
	}

	/* Adding the buffer on the top of buffer list. */
	buffer->next = buffers_list;
	buffers_list = buffer;
	buffer->con = cc;
	buffer->cap = cap;
	//    buffer->type = type;
	struct frame_identity pa;
	err = invoke_frame_identify(cap, &pa);
	assert(err_is_ok(err));
	buffer->pa = pa.base;
	buffer->bits = pa.bits;

	err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
			VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

	if (err_is_fail(err)) {
		DEBUG_ERR(err, "vspace_map_one_frame failed");

		struct q_entry entry;
		memset(&entry, 0, sizeof(struct q_entry));
		entry.handler = send_new_buffer_id;
		entry.binding_ptr = (void *) cc;
		struct client_closure *ccl = (struct client_closure*) cc->st;

		entry.plist[0] = E1000_ERR_TOO_MANY_BUFFERS;
		/* FIXME: this is wrong error */
		entry.plist[1] = 0;
		/*   entry.plist[0], entry.plist[1]);
		 entry.error,    entry.buffer_id */

		enqueue_cont_q(ccl->q, &entry);

        return;
	}

	/* FIXME: cheat, driver is allocating some memory on behalf of client.
	 * but this memory should come from "register_metadata_mem"
	 * I need to implement that */

	buffer->pbuf_metadata_ds = malloc(metadata_size);
	if (buffer->pbuf_metadata_ds == NULL) {
		printf("CHEAT part: not enough internal memory to support buffer\n");
		abort();
	}

	memset(buffer->pbuf_metadata_ds, 0, metadata_size);
	buffer->pbuf_metadata_ds_tx = buffer->pbuf_metadata_ds
			+ sizeof(struct pbuf_desc) * RECEIVE_BUFFERS;

	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_new_buffer_id;
	entry.binding_ptr = (void *) cc;
	struct client_closure *ccl = (struct client_closure*) cc->st;

	entry.plist[0] = err;
	entry.plist[1] = buffer->buffer_id;
	/*   entry.plist[0], entry.plist[1]);
	 entry.error,    entry.buffer_id */

	enqueue_cont_q(ccl->q, &entry);
	ETHERSRV_DEBUG(
			"register_buffer:buff_id[%" PRIu64 "] pa[%" PRIuLPADDR "] va[%p] bits[%" PRIu64 "]#####\n",
			buffer->buffer_id, buffer->pa, buffer->va, buffer->bits);
}


static uint64_t add_receive_pbuf_app(uint64_t pbuf_id, uint64_t paddr,
		uint64_t vaddr, uint64_t len, struct ether_binding *sr)
{
    struct buffer_descriptor *buffer =
	(struct buffer_descriptor *)((struct client_closure *)(sr->st))->buffer_ptr;
    assert(buffer != NULL);

    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);
    uint32_t new_tail = (buffer->pbuf_tail + 1) % RECEIVE_BUFFERS;

    if(buffer->pbuf_metadata_ds == NULL) {
    	ETHERSRV_DEBUG("memory is yet not provided by the client "
    			"for pbuf management\n");
    	return -1;
    }

	/* check if there is a space in app ring before trying to
	 * insert the buffer */
    if(new_tail == buffer->pbuf_head_msg) {
    	ETHERSRV_DEBUG("no space to add a new receive pbuf\n");
    	printf("no space to add a new receive pbuf new_tail %u msg_hd %u\n",
    			new_tail, buffer->pbuf_head_msg);
    	return -1;
    }

    /* FIXME: following is precautionary call.  This flow of code is not
     * sending and data but only sending back the empty buffer,
     * so prepare_recv is not strictly necessary. */
    bulk_arch_prepare_recv((void *)(uintptr_t)vaddr, len);

    pbuf[buffer->pbuf_tail].sr = sr;
    pbuf[buffer->pbuf_tail].pbuf_id = pbuf_id;
    pbuf[buffer->pbuf_tail].paddr = paddr; // asq: remember for later freeing
    pbuf[buffer->pbuf_tail].vaddr = vaddr;
    pbuf[buffer->pbuf_tail].len = len;
    pbuf[buffer->pbuf_tail].event_sent = false;
    buffer->pbuf_tail = new_tail;
    /*
    ETHERSRV_DEBUG("pbuf added head %u msg_hd %u tail %u in buffer %lu\n",
    		buffer->pbuf_head, buffer->pbuf_head_msg, buffer->pbuf_tail,
    		buffer->buffer_id);
    */
    return 0;

}


static void register_pbuf(struct ether_binding *cc, uint64_t pbuf_id,
		uint64_t paddr, uint64_t len) {

#if TRACE_ETHERSRV_MODE
//	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_N_SPBUF, (uint32_t)pbuf_id);
#endif // TRACE_ETHERSRV_MODE

	errval_t r;
/*	ETHERSRV_DEBUG("ETHERSRV: register_pbuf: %"PRIx64" registering ++++++++\n",
	        pbuf_id);
*/
	struct buffer_descriptor *buffer =(struct buffer_descriptor *)
					((struct client_closure *) (cc->st))->buffer_ptr;

	/* Calculating the physical address of this pbuf. */
	uint64_t virtual_addr = (uint64_t)(uintptr_t)buffer->va + (paddr
			- (uint64_t) buffer->pa);
	/* NOTE: virtual address = virtual base + physical offset */
/*
	ETHERSRV_DEBUG("register_pbuf: pbuf id %"PRIx64" on buff_id %"PRIx64"\n",
	 pbuf_id, buffer->buffer_id);
*/

	r = add_receive_pbuf_app(pbuf_id, paddr, virtual_addr, len, cc);
	assert(err_is_ok(r));
}


static void transmit_packet(struct ether_binding *cc, uint64_t nr_pbufs,
		uint64_t buffer_id, uint64_t len, uint64_t offset, uint64_t client_data)
{

#if TRACE_ETHERSRV_MODE
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NO_A, (uint32_t) client_data);
#endif // TRACE_ETHERSRV_MODE

	errval_t r;
	struct client_closure *closure = (struct client_closure *) cc->st;
	assert(closure != NULL);

	if(closure->debug_state) ethersrv_debug_printf("ETHERSRV: transmit_packet: sending packet ------\n");
	if(closure->debug_state) ethersrv_debug_printf("packet from buf %lu, of size %lu from clie %d\n",
	 buffer_id, len, closure->cl_no);

	if(closure->debug_state) ethersrv_debug_printf("transmit_packet with buff_id %lu, on buffer %lu\n",
	        buffer_id, closure->buffer_ptr->buffer_id);

	assert (closure->buffer_ptr->buffer_id == buffer_id);
	assert(nr_pbufs <= MAX_NR_TRANSMIT_PBUFS);

	if (closure->nr_transmit_pbufs == 0) {
		closure->nr_transmit_pbufs = nr_pbufs;
		closure->len = 0;
	}
	closure->pbuf[closure->rtpbuf].buffer_id = buffer_id;
	closure->pbuf[closure->rtpbuf].sr = cc;
	closure->pbuf[closure->rtpbuf].cc = closure;
	closure->pbuf[closure->rtpbuf].len = len;
	closure->pbuf[closure->rtpbuf].offset = offset;
	closure->pbuf[closure->rtpbuf].client_data = client_data;
	/* WARN: Most of this code is on assumption that app will send one
	 * packet at one time, and there will be no packet pipelining. */
	closure->len = closure->len + len; /* total lengh of packet */

    /* making the buffer memory cache coherent. */
    bulk_arch_prepare_recv((void *)closure->buffer_ptr->pa + offset, len);

	closure->rtpbuf++;
	if (closure->rtpbuf < closure->nr_transmit_pbufs) {
		/* all pbufs are not arrived yet, waiting for more pbufs associated
		 * with this packet. */
		return;
	}


	/*we are done receiving all the pbufs from the application network
	 stack and can transmit them finally */

	/* FIXME: ideally, one should check if this sender is allowed to send
	 * this packet or not. */

	/* FIXME: this design expects more than one msg when packet does not
	 * fit into one pbuf, I feel that it is bad design */

	//in case we cannot transmit, discard the _whole_ packet (just don't
	//enqueue transmit descriptors in the network card's ring)
	r = ether_transmit_pbuf_list_ptr(closure);

	if(err_is_fail(r)){
                dropped_pkt_count++;
                printf("Transmit_packet dropping %"PRIu64"\n",
                        dropped_pkt_count);
		//if we have to drop the packet, we still need to send a tx_done
		//to make sure that lwip frees the pbuf. If we drop it, the driver
		//will never find it in the tx-ring from where the tx_dones
		//are usually sent

	    if(closure->debug_state)
                ethersrv_debug_printf(
                        "###transmit_packet dropping the packet"
                        " buff_id %" PRIu64 "\n",
				closure->buffer_ptr->buffer_id);
		/* BIG FIXME: This should free all the pbufs and not just pbuf[0].
		 * Also, is it certain that all packets start with pbuf[0]??
		 * Mostly this will lead to re-write of bulk-transfer mode.  */

                assert(closure->pbuf[0].sr);
		notify_client_free_tx(closure->pbuf[0].sr,
				closure->pbuf[0].client_data,
                                tx_free_slots_fn_ptr(), 1);
	}
	//reset to indicate that a new packet will start
	closure->nr_transmit_pbufs = 0;
	closure->rtpbuf = 0;
	closure->len = 0;

}


static errval_t send_mac_addr_response(struct q_entry entry) {
	//    ETHERSRV_DEBUG("send_mac_addr_response -----\n");
	struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
	struct client_closure *ccl = (struct client_closure*) b->st;
	if (b->can_send(b)) {
		return b->tx_vtbl.get_mac_address_response(b,
				MKCONT(cont_queue_callback, ccl->q), entry.plist[0]);
		/* entry.hwaddr */
	} else {
		ETHERSRV_DEBUG("send_mac_addr_response Flounder busy.. will retry\n");
		return FLOUNDER_ERR_TX_BUSY;
	}
}


static void get_mac_addr(struct ether_binding *cc) {
	union {
		uint8_t hwaddr[6];
		uint64_t hwasint;
	} u;
	u.hwasint = 0;
	ether_get_mac_address_ptr(u.hwaddr);

	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_mac_addr_response;
	entry.binding_ptr = (void *) cc;
	struct client_closure *ccl = (struct client_closure *) cc->st;
	entry.plist[0] = u.hwasint;
	/* entry.plist[0]);
	 entry.hwaddr */

	enqueue_cont_q(ccl->q, &entry);

}

static void print_statistics_handler(struct ether_binding *cc) {
	ETHERSRV_DEBUG("ETHERSRV: print_statistics_handler: called.\n");
//	print_statistics();
}

static void print_cardinfo_handler(struct ether_binding *cc) {

}

/*****************************************************************
 * Chips Handlers:
 ****************************************************************/
//static int client_conn_nr = 0;

static void export_ether_cb(void *st, errval_t err, iref_t iref) {
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "service [%s] export failed", my_service_name);
		abort();
	}

	ETHERSRV_DEBUG("service [%s] exported at iref %u\n", my_service_name, iref);

	// register this iref with the name service
	err = nameservice_register(my_service_name, iref);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "nameservice_register failed for [%s]", my_service_name);
		abort();
	}
}

static void export_ether_control_cb(void *st, errval_t err, iref_t iref) {
    char service_name[100];
    snprintf(service_name, sizeof(service_name), "%s%s", my_service_name,
            FILTER_SERVICE_SUFFIX);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "service[%s] export failed", service_name);
		abort();
	}

	ETHERSRV_DEBUG("service [%s] exported at iref %u\n", service_name, iref);

	// register this iref with the name service
	err = nameservice_register(service_name, iref);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "nameservice_register failed for [%s]", service_name);
		abort();
	}
}

static void error_handler(struct ether_binding *b, errval_t err)
{
	ETHERSRV_DEBUG("ether service error_handler: called\n");
	if (err == SYS_ERR_CAP_NOT_FOUND){
		struct client_closure *cc = b->st;
		assert(cc != NULL);
		struct buffer_descriptor *buffer = cc->buffer_ptr;
		assert(buffer != NULL);
		free(buffer);
		free(cc);
	}
	ETHERSRV_DEBUG("ether service error_handler: terminated\n");
}

static errval_t connect_ether_cb(void *st, struct ether_binding *b) {
	ETHERSRV_DEBUG("ether service got a connection!44\n");
	struct frame_identity pa;
	struct capref frame;
	errval_t err = SYS_ERR_OK;

	// copy my message receive handler vtable to the binding
	b->rx_vtbl = rx_ether_vtbl;
	b->error_handler = error_handler;

	struct client_closure *cc = (struct client_closure *) malloc(
			sizeof(struct client_closure));
	if (cc == NULL) {
		err = E1000_ERR_NOT_ENOUGH_MEM;
		ETHERSRV_DEBUG("Ether connection: out of memory\n");
		return err;
	}
	memset(cc, 0, sizeof(struct client_closure));

	struct buffer_descriptor *buffer = (struct buffer_descriptor *) malloc(
			sizeof(struct buffer_descriptor));
	if (buffer == NULL) {
		err = E1000_ERR_NOT_ENOUGH_MEM;
		ETHERSRV_DEBUG("connection_service_logic: out of memory\n");
		free(cc);
		return err;
	}
	memset(buffer, 0, sizeof(struct buffer_descriptor));

	b->st = cc;
	cc->buffer_ptr = buffer;
	cc->nr_transmit_pbufs = 0;
	cc->rtpbuf = 0;
	cc->debug_state = 0; // Default debug state : no debug
	cc->app_connection = b;

	char name[64];
	sprintf(name, "ether_c_%d", cc->cl_no);
	cc->q = create_cont_q(name);
	if (cc->q == NULL) {
		ETHERSRV_DEBUG("connection_service_logic: queue allocation failed\n");
		free(buffer);
		free(cc);
		return err;
	}

	err = frame_alloc(&frame, LAST_ACCESSED_BYTE_TRANSPORT * TRANSMIT_BUFFERS,
			NULL);
	if (err_is_fail(err)) {
		ETHERSRV_DEBUG("connection_service_logic: frame_alloc failed\n");
		free(buffer);
		free(cc);
		/* FIXME: free cc->q also */
		return err;
	}

	err = invoke_frame_identify(frame, &pa);
	assert(err_is_ok(err));
	cc->tx_private_mem_p = (pa.base);

	err = vspace_map_one_frame_attr((void*) (&(cc->tx_private_mem_v)),
			LAST_ACCESSED_BYTE_TRANSPORT * TRANSMIT_BUFFERS, frame,
			VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
	if (err_is_fail(err)) {
		assert(!"vspace_map_one_frame failed");
	}

	if (client_no < 2) {
		netd[client_no] = b;
	}
	cc->cl_no = client_no++;

	// accept the connection (we could return an error to refuse it)
	return SYS_ERR_OK;
} /* end function: connect_ether_cb */

static errval_t connect_ether_control_cb(void *st,
		struct ether_control_binding *b) {
	ETHERSRV_DEBUG("ether_netd service got a connection!55\n");

	// copy my message receive handler vtable to the binding
	b->rx_vtbl = rx_ether_control_vtbl;
	//b->error_handler = error_handler;

	struct client_closure_FM *ccfm = (struct client_closure_FM*) malloc(
			sizeof(struct client_closure_FM));

	b->st = ccfm;
	ccfm->q = create_cont_q("FILTER-MANAGER");
	ccfm->app_connection = b;

	/* FIXME: should I refuse more than one connections for FM services?
	 Currently, I am accepting them */

	// accept the connection (we could return an error to refuse it)
	return SYS_ERR_OK;
} /* end function: connect_ether_control_cb */


/*****************************************************************
 * ethersrv initialization wrapper:
 ****************************************************************/
void ethersrv_init(char *service_name,
		ether_get_mac_address_t get_mac_ptr,
		ether_transmit_pbuf_list_t transmit_ptr,
                ether_get_tx_free_slots tx_free_slots_ptr)
{
	errval_t err;

	ETHERSRV_DEBUG("in the server_init\n");
	assert(service_name != NULL);
	assert(get_mac_ptr != NULL);
	assert(transmit_ptr != NULL);
	assert(tx_free_slots_ptr != NULL);

	ether_get_mac_address_ptr = get_mac_ptr;
	ether_transmit_pbuf_list_ptr  = transmit_ptr;
        tx_free_slots_fn_ptr = tx_free_slots_ptr;

	my_service_name = service_name;

	buffers_list = NULL;
	netd[0] = NULL;
	netd[1] = NULL;
	buffer_id_counter = 0;
	filter_id_counter = 0;
	netd_buffer_count = 0;
	client_no = 0;

	/* FIXME: populate the receive ring of device driver with local pbufs */

	/* exporting ether interface */
	err = ether_export(NULL /* state pointer for connect/export callbacks */,
			export_ether_cb, connect_ether_cb, get_default_waitset(),
			IDC_EXPORT_FLAGS_DEFAULT);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "%s export failed", my_service_name);
		abort();
	}

	/* FIXME: do I need separate my_service_name for ether_netd services */
	/* exporting ether_netd interface */
	err = ether_control_export(NULL, export_ether_control_cb,
			connect_ether_control_cb, get_default_waitset(),
			IDC_EXPORT_FLAGS_DEFAULT);
	if (err_is_fail(err)) {
		DEBUG_ERR(err, "ethersrv_netd export failed");
		abort();
	}
}

/*****************************************************************
 *   filter registration
 *****************************************************************/

/* FIXME: provide proper handler here */
static errval_t send_resiger_filter_memory_response(struct q_entry entry) {
	//    ETHERSRV_DEBUG("send_resigered_netd_memory  -----\n");
	struct ether_control_binding *b =
			(struct ether_control_binding *) entry.binding_ptr;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;
	if (b->can_send(b)) {
		return b->tx_vtbl.register_filter_memory_response(b,
				MKCONT(cont_queue_callback, ccfm->q), entry.plist[0]);
		/* entry.error */
	} else {
		ETHERSRV_DEBUG("send_resigered_netd_memory Flounder bsy will retry\n");
		return FLOUNDER_ERR_TX_BUSY;
	}
}

static struct bulk_transfer_slave bt_filter_rx;

static void register_filter_memory_request(struct ether_control_binding *cc,
		struct capref mem_cap) {

	errval_t err = SYS_ERR_OK;

	struct frame_identity pa;
	err = invoke_frame_identify(mem_cap, &pa);
	assert(err_is_ok(err));

	ETHERSRV_DEBUG("register_netd_memory: attempt to register memory\n");
	// 2 is rx + tx
	if ((1L << pa.bits) < BASE_PAGE_SIZE * 2) {
		ETHERSRV_DEBUG("netd did not provided enough for filter transfer\n");
		err = FILTER_ERR_NOT_ENOUGH_MEMORY; /* ps: FIXME: enable this error */

	} /* end if: not enough memory */
	else { /* enough memory, try to map it */
		void *pool;
		err = vspace_map_one_frame_attr((void*) (&pool), BASE_PAGE_SIZE * 2,
				mem_cap, VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

		if (err_is_fail(err)) {
			DEBUG_ERR(err, "vspace_map_one_frame failed");
			//            abort();
		} /* end if: mapping failed */
		else {
			// Init receiver
			err = bulk_slave_init(pool, BASE_PAGE_SIZE * 2, &bt_filter_rx);
			//            assert(err_is_ok(err));

		} /* end else: mapping sucessful */

	} /* end else : */

	/* call registered_netd_memory with new IDC */
	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_resiger_filter_memory_response;
	entry.binding_ptr = (void *) cc;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

	entry.plist[0] = err;
	/* entry.plist[0]
	 entry.error */

	enqueue_cont_q(ccfm->q, &entry);
	ETHERSRV_DEBUG("register_netd_memory: sent IDC\n");

} /* end function : register_netd_memory */

/* Handler for sending response to register_filter */
static errval_t send_register_filter_response(struct q_entry e) {
	//    ETHERSRV_DEBUG("send_resigered_filter for ID %lu  -----\n", e.plist[0]);
	struct ether_control_binding *b =
			(struct ether_control_binding *) e.binding_ptr;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;
	if (b->can_send(b)) {
		return b->tx_vtbl.register_filter_response(b,
			MKCONT(cont_queue_callback, ccfm->q),
			e.plist[0], e.plist[1], e.plist[2], e.plist[3], e.plist[4], e.plist[5]);
	     /* e.id,       e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_tx, e.filter_type */

	} else {
		ETHERSRV_DEBUG(
				"send_resigered_filter: ID %" PRIu64 ": Flounder bsy will retry\n",
				e.plist[0]);
		return FLOUNDER_ERR_TX_BUSY;
	}
}

static void wrapper_send_filter_registered_msg(
		struct ether_control_binding *cc, uint64_t id, errval_t err,
		uint64_t filter_id, uint64_t buffer_id_rx, uint64_t buffer_id_tx,
		uint64_t ftype) {

	/* call registered_netd_memory with new IDC */

	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_register_filter_response;
	entry.binding_ptr = (void *) cc;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

	entry.plist[0] = id;
	entry.plist[1] = err;
	entry.plist[2] = filter_id;
	entry.plist[3] = buffer_id_rx;
	entry.plist[4] = buffer_id_tx;
	entry.plist[5] = ftype;
	/* e.plist[0], e.plist[1], e.plist[2],  e.plist[3],     e.plist[4],     e.plist[5]);
	   e.id,       e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_tx, e.filter_type */

	enqueue_cont_q(ccfm->q, &entry);

}

/**
 * \brief: Registers the filter with network driver
 */
static void register_filter(struct ether_control_binding *cc, uint64_t id,
			uint64_t len_rx, uint64_t len_tx, uint64_t buffer_id_rx,
                            uint64_t buffer_id_tx, uint64_t ftype, uint64_t paused)
{
	errval_t err = SYS_ERR_OK;
	ETHERSRV_DEBUG("Register_filter: ID:%" PRIu64 " of type[%" PRIu64 "] buffers RX[%" PRIu64 "] and TX[%" PRIu64 "]\n",
	        id, ftype, buffer_id_rx, buffer_id_tx);

	struct buffer_descriptor *buffer_rx = NULL;
	struct buffer_descriptor *buffer_tx = NULL;
	struct buffer_descriptor *tmp = buffers_list;

	while (tmp) {

		if (tmp->buffer_id == buffer_id_tx) {
			buffer_tx = tmp;
		}

		if (tmp->buffer_id == buffer_id_rx) {
			buffer_rx = tmp;
		}

		if (buffer_rx != NULL && buffer_tx != NULL) {
			break;
		}

		tmp = tmp->next;
	} /* end while : */

	if (buffer_rx == NULL || buffer_tx == NULL) {
		ETHERSRV_DEBUG("no buffer found for the provided buffer id\n");
		err = FILTER_ERR_BUFF_NOT_FOUND;

		wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
				buffer_id_tx, ftype);
		return;
	}

	if (len_rx > BASE_PAGE_SIZE) {
		len_rx = BASE_PAGE_SIZE;
	}

	if (len_tx > BASE_PAGE_SIZE) {
		len_tx = BASE_PAGE_SIZE;
	}

	/* using id to find the location of memory */
	void *buf = bulk_slave_buf_get_mem(&bt_filter_rx, id, NULL);
	if (buf == NULL) {
		ETHERSRV_DEBUG("no memory available for filter transfer\n");
		err = FILTER_ERR_NO_NETD_MEM;
		wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
				buffer_id_tx, ftype);
		return;
	}

	/* Create the filter data-structures */
	struct filter *new_filter_rx = (struct filter *) malloc(
			sizeof(struct filter));
	struct filter *new_filter_tx = (struct filter *) malloc(
			sizeof(struct filter));

	/* FIXME: use goto to deal with failure conditions and reduce the code */
	if (new_filter_rx == NULL || new_filter_tx == NULL) {
		ETHERSRV_DEBUG("out of memory for filter registration\n");
		err = E1000_ERR_NOT_ENOUGH_MEM;
		wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
				buffer_id_tx, ftype);

		if (new_filter_rx) {
			free(new_filter_rx);
		}

		if (new_filter_tx) {
			free(new_filter_tx);
		}
		return;
	}

	/* Zero out the filters */
	memset(new_filter_rx, 0, sizeof(struct filter));
	memset(new_filter_tx, 0, sizeof(struct filter));

	/* Allocate memory for holding the filter-data */
	new_filter_rx->data = (uint8_t *) malloc(len_rx);
	new_filter_tx->data = (uint8_t *) malloc(len_tx);

	if (new_filter_rx->data == NULL || new_filter_tx->data == NULL) {
		ETHERSRV_DEBUG("out of memory for filter data registration\n");
		err = E1000_ERR_NOT_ENOUGH_MEM;
		wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
				buffer_id_tx, ftype);

		if (new_filter_rx->data) {
			free(new_filter_rx->data);
		}

		if (new_filter_tx->data) {
			free(new_filter_tx->data);
		}

		free(new_filter_rx);
		free(new_filter_tx);

		return;
	}

	/* Zero-out the area of filter-data */
	memset(new_filter_rx->data, 0, len_rx);
	memset(new_filter_tx->data, 0, len_tx);

	filter_id_counter++;

	// rx filter
	memcpy(new_filter_rx->data, buf, len_rx);
	new_filter_rx->len = len_rx;
	new_filter_rx->filter_id = filter_id_counter;
	new_filter_rx->filter_type = ftype;
	new_filter_rx->buffer = buffer_rx;
	new_filter_rx->next = rx_filters;
        new_filter_rx->paused = paused ? true : false;
	rx_filters = new_filter_rx;
	ETHERSRV_DEBUG("filter registered with id %" PRIu64 " and len %d\n",
			new_filter_rx->filter_id, new_filter_rx->len);

	// tx filter
	void *bbuf_tx = buf + BASE_PAGE_SIZE;
	memcpy(new_filter_tx->data, bbuf_tx, len_tx);
	new_filter_tx->len = len_tx;
	new_filter_tx->filter_id = filter_id_counter;
	new_filter_tx->filter_type = ftype;
	new_filter_tx->buffer = buffer_tx; // we do not really need to set this
	/* FIXME: following linked list implementation looks buggy */
	new_filter_tx->next = buffer_tx->tx_filters;
	buffer_tx->tx_filters = new_filter_tx;
	/* FIXME: following looks buggy!!! */
	buffer_rx->tx_filters = new_filter_tx; // sometimes rx buffers transmit

	/* reporting back the success/failure */
	wrapper_send_filter_registered_msg(cc, id, err, filter_id_counter,
			buffer_id_rx, buffer_id_tx, ftype);

	ETHERSRV_DEBUG("Register_filter: ID %" PRIu64 ": type[%"PRIu64"] successful [%" PRIu64 "]\n",
			id, ftype, filter_id_counter);

} /* end function: register filter */


/* Handler for sending response to deregister_filter */
static errval_t send_deregister_filter_response(struct q_entry e) {
    //    ETHERSRV_DEBUG("send_deresigered_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
            (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;
    if (b->can_send(b)) {
        return b->tx_vtbl.deregister_filter_response(b,
                MKCONT(cont_queue_callback, ccfm->q),
                e.plist[0], e.plist[1]);
             /* e.error,    e.filter_id,  */

    } else {
        ETHERSRV_DEBUG(
                "send_deresiger_filter_response: Filter_ID %" PRIu64 ": Flounder bsy will retry\n",
                e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_send_filter_deregister_msg(
        struct ether_control_binding *cc, errval_t err, uint64_t filter_id) {

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_deregister_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = filter_id;
    /* e.plist[0], e.plist[1] );
       e.error,    e.filter_id */
    enqueue_cont_q(ccfm->q, &entry);
}

static struct filter *delete_from_filter_list(struct filter *head, uint64_t filter_id)
{
    struct filter *prev = NULL;
    while(head != NULL){
        if(head->filter_id == filter_id){
            if(prev == NULL) {
                rx_filters = head->next;
            } else {
                prev->next = head->next;
            }
            return head;
        } /* end if: filter_id found */
    } /* end while: for each element in list */
    return NULL; /* could not not find the id. */
}


/**
 * \brief: Deregisters the filter with network driver
 */
static void deregister_filter(struct ether_control_binding *cc,
        uint64_t filter_id)
{
    errval_t err = SYS_ERR_OK;
    ETHERSRV_DEBUG("DeRegister_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;
    struct filter *tx_filter = NULL;

    rx_filter = delete_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if(rx_filter == NULL  /*|| tx_filter == NULL*/) {
        ETHERSRV_DEBUG("Deregister_filter:requested filter_ID [%" PRIu64 "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
    }

    if(rx_filter) {
        free(rx_filter);
    }

    if(tx_filter) {
        free(tx_filter);
    }

    /* reporting back the success/failure */
    wrapper_send_filter_deregister_msg(cc, err, filter_id);

    ETHERSRV_DEBUG("Deregister_filter: ID %" PRIu64 ": Done\n", filter_id);

} /* end function: deregister_filter */



/* Handler for sending response to re register_filter */
static errval_t send_re_register_filter_response(struct q_entry e) {
    //    ETHERSRV_DEBUG("send_re_register_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
            (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;
    if (b->can_send(b)) {
        return b->tx_vtbl.re_register_filter_response(b,
                MKCONT(cont_queue_callback, ccfm->q),
                e.plist[0], e.plist[1], e.plist[2], e.plist[2]);
             /* e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        ETHERSRV_DEBUG(
                "send_re_register_filter_response: Filter_ID %" PRIu64 ": Flounder bsy will retry\n",
                e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
} /* end function: send_re_register_filter_response */

static errval_t send_pause_filter_response(struct q_entry e) {
    //    ETHERSRV_DEBUG("send_re_register_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
            (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.pause_response(b,
                MKCONT(cont_queue_callback, ccfm->q),
                e.plist[0], e.plist[1]);
             /* e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        ETHERSRV_DEBUG(
                "send_re_register_filter_response: Filter_ID %" PRIu64 ": Flounder bsy will retry\n",
                e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
} /* end function: send_re_register_filter_response */

static void wrapper_send_filter_re_register_msg(
        struct ether_control_binding *cc, errval_t err, uint64_t filter_id,
        uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_re_register_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = filter_id;
    entry.plist[2] = buffer_id_rx;
    entry.plist[3] = buffer_id_tx;
/*    e.plist[0], e.plist[1],  e.plist[2],     e.plist[2]
      e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */
    enqueue_cont_q(ccfm->q, &entry);
} /* end function: wrapper_send_filter_re_register_msg */

static void wrapper_send_filter_pause_msg(
        struct ether_control_binding *cc, errval_t err, uint64_t filter_id)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_pause_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = filter_id;
    entry.plist[1] = err;
/*    e.plist[0], e.plist[1],  e.plist[2],     e.plist[2]
      e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */
    enqueue_cont_q(ccfm->q, &entry);
} /* end function: wrapper_send_filter_re_register_msg */


static struct filter *find_from_filter_list(struct filter *head,
        uint64_t filter_id)
{
    while(head != NULL){
        if(head->filter_id == filter_id){
            return head;
        } /* end if: filter_id found */
        head = head->next;
    } /* end while: for each element in list */
    return NULL; /* could not not find the id. */
}

static struct buffer_descriptor *find_buffer(uint64_t buffer_id)
{
    struct buffer_descriptor *buffer = buffers_list;
    while(buffer) {
        if(buffer->buffer_id == buffer_id) {
            return buffer;
        }
        buffer = buffer->next;
    }
    return NULL;
} /* end function: buffer_descriptor */

/**
 * \brief: re-registers the filter with network driver
 */
static void re_register_filter(struct ether_control_binding *cc,
        uint64_t filter_id, uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;
    ETHERSRV_DEBUG("re_register_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;
//    struct filter *tx_filter = NULL;

    rx_filter = find_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if(rx_filter == NULL  /*|| tx_filter == NULL*/) {
        ETHERSRV_DEBUG("re_register_filter: requested filter_ID [%" PRIu64 "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
        wrapper_send_filter_re_register_msg(cc, err, filter_id,
                buffer_id_rx, buffer_id_tx);
        return;
    }
    /* Find the buffer with given buffer_id */
    struct buffer_descriptor *buffer_rx = find_buffer(buffer_id_rx);
    struct buffer_descriptor *buffer_tx = NULL;
    buffer_tx = find_buffer(buffer_id_tx);
    if(buffer_rx == NULL || buffer_rx == NULL) {
        ETHERSRV_DEBUG("re_register_filter: provided buffer id's not found\n");
        ETHERSRV_DEBUG("re_register_filter: rx=[[%" PRIu64 "] = %p], tx=[[%" PRIu64 "] = %p]\n",
                buffer_id_rx, buffer_rx, buffer_id_tx, buffer_tx);
        err =  FILTER_ERR_BUFFER_NOT_FOUND; /* set error value */
        wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx,
                buffer_id_tx);
    }
    rx_filter->buffer = buffer_rx;
    /* FIXME: Also, set the new buffer for tx_filters */
    /* reporting back the success/failure */
    wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx,
            buffer_id_tx);

    ETHERSRV_DEBUG("re_register_filter: ID %" PRIu64 ": Done\n", filter_id);

} /* end function: re_register_filter */

/**
 * \brief: pause the filter with network driver
 */
static void pause_filter(struct ether_control_binding *cc, uint64_t filter_id,
                         uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;
    ETHERSRV_DEBUG("(un)pause_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;
//    struct filter *tx_filter = NULL;

    rx_filter = find_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if(rx_filter == NULL  /*|| tx_filter == NULL*/) {
        ETHERSRV_DEBUG("pause_filter: requested filter_ID [%" PRIu64 "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
        assert(!"NYI");
        /* wrapper_send_filter_re_register_msg(cc, err, filter_id, */
        /*         buffer_id_rx, buffer_id_tx); */
        return;
    }

    /* Find the buffer with given buffer_id */
    struct buffer_descriptor *buffer_rx = find_buffer(buffer_id_rx);
    struct buffer_descriptor *buffer_tx = NULL;
    buffer_tx = find_buffer(buffer_id_tx);
    if(buffer_rx == NULL || buffer_rx == NULL) {
        ETHERSRV_DEBUG("re_register_filter: provided buffer id's not found\n");
        ETHERSRV_DEBUG("re_register_filter: rx=[[%" PRIu64 "] = %p], tx=[[%" PRIu64 "] = %p]\n",
                buffer_id_rx, buffer_rx, buffer_id_tx, buffer_tx);
        assert(!"NYI");
        /* err =  FILTER_ERR_BUFFER_NOT_FOUND; /\* set error value *\/ */
        /* wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx, */
        /*         buffer_id_tx); */
    }
    rx_filter->buffer = buffer_rx;
    /* FIXME: Also, set the new buffer for tx_filters */
    /* reporting back the success/failure */
    wrapper_send_filter_pause_msg(cc, err, filter_id);

    rx_filter->paused = false;
    if(rx_filter->pause_bufpos > 0) {
        for(int i = 0; i < rx_filter->pause_bufpos; i++) {
            struct bufdesc *bd = &rx_filter->pause_buffer[i];
            copy_packet_to_user(rx_filter->buffer, bd->pkt_data, bd->pkt_len);
        }
    }
    rx_filter->pause_bufpos = 0;

    ETHERSRV_DEBUG("(un)pause_filter: ID %" PRIu64 ": Done\n", filter_id);

} /* end function: re_register_filter */


/* Handler for sending response to register_filter */
static errval_t send_register_arp_filter_response(struct q_entry entry) {
	//    ETHERSRV_DEBUG("send_resigered_arp_filter  -----\n");
	struct ether_control_binding *b =
			(struct ether_control_binding *) entry.binding_ptr;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;
	if (b->can_send(b)) {
		return b->tx_vtbl.register_arp_filter_response(b,
				MKCONT(cont_queue_callback, ccfm->q), entry.plist[0],
				entry.plist[1]);
		/* e.id,        e.error */

	} else {
		ETHERSRV_DEBUG("send_resigered_arp_filter Flounder bsy will retry\n");
		return FLOUNDER_ERR_TX_BUSY;
	}
}

static void wrapper_send_arp_filter_registered_msg(
		struct ether_control_binding *cc, uint64_t id, errval_t err) {

	/* call registered_netd_memory with new IDC */

	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_register_arp_filter_response;
	entry.binding_ptr = (void *) cc;
	struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

	entry.plist[0] = id;
	entry.plist[1] = err;
	/* entry.plist[0], entry.plist[1]
	 id,             e.error */

	enqueue_cont_q(ccfm->q, &entry);
}

static void register_arp_filter(struct ether_control_binding *cc, uint64_t id,
		uint64_t len_rx, uint64_t len_tx) {

	errval_t err = SYS_ERR_OK;

	if (len_rx > BASE_PAGE_SIZE) {
		len_rx = BASE_PAGE_SIZE;
	}
	if (len_tx > BASE_PAGE_SIZE) {
		len_tx = BASE_PAGE_SIZE;
	}

	/* using id to find the location of memory */
	void *buf = bulk_slave_buf_get_mem(&bt_filter_rx, id, NULL);

	if (buf == NULL) {
		ETHERSRV_DEBUG("no memory available for arp_filter transfer\n");
		err = FILTER_ERR_NO_NETD_MEM;
		wrapper_send_arp_filter_registered_msg(cc, id, err);
		return;
	}

	arp_filter_rx.data = (uint8_t *) malloc(len_rx);
	assert(arp_filter_rx.data);
	memcpy(arp_filter_rx.data, buf, len_rx);
	arp_filter_rx.len = len_rx;
	ETHERSRV_DEBUG("#### The received arp RX filter is\n");
	//    show_binary_blob(arp_filter_rx.data, arp_filter_rx.len);

	void *bbuf_tx = buf + BASE_PAGE_SIZE;
	arp_filter_tx.data = (uint8_t *) malloc(len_tx);
	assert(arp_filter_tx.data);
	memcpy(arp_filter_tx.data, bbuf_tx, len_tx);
	arp_filter_tx.len = len_tx;
	ETHERSRV_DEBUG("#### The received arp RX filter is\n");
	//    show_binary_blob(arp_filter_tx.data, arp_filter_tx.len);

	wrapper_send_arp_filter_registered_msg(cc, id, err);
}



/****************** Functions copied from e1000n.c *************************/
static errval_t send_tx_done_handler(struct q_entry e)
{
//    ETHERSRV_DEBUG("send_tx_done_handler -----\n");
    struct ether_binding *b = (struct ether_binding *)e.binding_ptr;
    struct client_closure *ccl = (struct client_closure*)b->st;
    if (b->can_send(b)) {
        return b->tx_vtbl.tx_done(b, MKCONT(cont_queue_callback, ccl->q),
                          e.plist[0],    e.plist[1],   e.plist[2]);
                        //  client_data,   slots_left, dropped
    } else {
        ETHERSRV_DEBUG("send_tx_done_handler Flounder busy.. retry --------\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

bool notify_client_free_tx(struct ether_binding *b,
        uint64_t client_data, uint64_t slots_left, uint64_t dropped)
{
        assert(b != NULL);
	struct q_entry entry;
	memset(&entry, 0, sizeof(struct q_entry));
	entry.handler = send_tx_done_handler;
	entry.binding_ptr = (void *)b;
	struct client_closure *cc = (struct client_closure *)b->st;
	entry.plist[0] = client_data;
        // FIXME: Get the remaining slots value from the driver
        entry.plist[1] = slots_left;
        entry.plist[2] = dropped;
	enqueue_cont_q(cc->q, &entry);
	return true;
}

static errval_t send_received_packet_handler(struct q_entry entry)
{
/*	ETHERSRV_DEBUG("send_received_packet_handler id %lu pbuf %p\n",
			entry.plist[0], (void *)entry.plist[1]); */

    struct ether_binding *b = (struct ether_binding *)entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure*)b->st;
    if (b->can_send(b)) {
    	errval_t err;
    	if(entry.plist[2] == 0 || entry.plist[1] == 0) {
    		/* FIXME: handle this error in better way. */
    	    ETHERSRV_DEBUG("##ERROR: trying to send pbuf_id %"PRIx64" at %"PRIx64" "
    				"of size %"PRIx64" and len %"PRIx64"\n",
    				entry.plist[0], entry.plist[1],
    				entry.plist[2], entry.plist[3]);
    		assert(entry.plist[2] != 0);
    	}


#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_S,
    		(uint32_t)entry.plist[0]);
#endif // TRACE_ETHERSRV_MODE

        err = b->tx_vtbl.packet_received(b,
            MKCONT(cont_queue_callback, ccl->q),
               entry.plist[0], entry.plist[1], entry.plist[2], entry.plist[3]);
            /* entry.pbuf_id,  entry.paddr,    entry.len,      entry.length */

        /* FIXME: what is this? why is it here? */
        /* FIXME: I am assuming here that packet has been properly uploaded */
        ccl->buffer_ptr->pbuf_head_msg = (ccl->buffer_ptr->pbuf_head_msg + 1)
        										% RECEIVE_BUFFERS;

        /* FIXME: As I have sent the msg here, shouldn't I treat this pbuf
         * as un-initialzed ?*/
        return err;
    } else {
        ETHERSRV_DEBUG("send_received_packet_handler: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/* enqueues the pbuf from the top of the queue (buffer->head) */
static bool send_packet_received_notification(struct buffer_descriptor *buffer,
				struct pbuf_desc *rx_pbuf)
{

    if(buffer->pbuf_head == buffer->pbuf_head_msg) {
    	return false;
    }

    assert(rx_pbuf);

#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_P,
    		(uint32_t)rx_pbuf->pbuf_id);
#endif // TRACE_ETHERSRV_MODE

    bulk_arch_prepare_send((void *)(uintptr_t)rx_pbuf->vaddr, rx_pbuf->len);

    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_received_packet_handler;
    entry.binding_ptr = (void *)buffer->con;
    struct client_closure *ccl = (struct client_closure*)buffer->con->st;
    entry.plist[0] = rx_pbuf->pbuf_id;
    entry.plist[1] = rx_pbuf->paddr; /* the physical address of pbuf payload */
    entry.plist[2] = rx_pbuf->len;
    entry.plist[3] = rx_pbuf->packet_size;
	if(entry.plist[2] == 0 || entry.plist[1] == 0) {
	    ETHERSRV_DEBUG("## trying to enqueue pbuf_id %"PRIx64" at %"PRIx64" of size %"PRIx64" and len %"PRIx64"\n",
				entry.plist[0], entry.plist[1], entry.plist[2], entry.plist[3]);
		assert(entry.plist[2] != 0);
	}

    enqueue_cont_q(ccl->q, &entry);
    //    ETHERSRV_DEBUG("send_packet_received done\n");
    /* entry.plist[0], entry.plist[1], entry.plist[2], entry.plist[3]*/
    /* entry.pbuf_id,  entry.paddr,    entry.len,      entry.length */

    return true;
}


bool copy_packet_to_user(struct buffer_descriptor *buffer,
				void *data, uint64_t len)
{

    uint32_t phead, ptail;
    if (buffer == NULL) {
    	/* Invalid buffer */
    	printf("ERROR: copy_packet_to_user: Invalid buffer.\n");
    	return false;
    }
//    assert(buffer != NULL);
    struct pbuf_desc *pbuf_list = (struct pbuf_desc *)(buffer->pbuf_metadata_ds);

    if(len <= 0 || data == NULL) {
            	ETHERSRV_DEBUG("[%d]ERROR: copy_packet_to_user: Invalid packet of len %"PRIu64" and ptr [%p]\n",
               disp_get_core_id(), len, data);

//        abort();

    	return false;
    	/* This is just another error, don't abort, ignore the packet
    	 * and continue. */
    }

    phead = buffer->pbuf_head;
    ptail = buffer->pbuf_tail;

    ETHERSRV_DEBUG("Copy_packet_2_usr_buf [%" PRIu64 "]: phead[%u] ptail[%u]\n",
			buffer->buffer_id, buffer->pbuf_head, buffer->pbuf_tail);

    struct pbuf_desc *upbuf = &pbuf_list[phead];
//    assert(upbuf != NULL);
    if(upbuf == NULL) {
    	/* pbufs are not yet registered, so can't send packet to userspace. */
    	ETHERSRV_DEBUG("ERROR: copy_packet_to_user: No pbufs registered for selected buffer\n");
    	return false;
    }

    if(((phead + 1) % RECEIVE_BUFFERS ) == ptail) {

    	ETHERSRV_DEBUG("[%d]no space in userspace 2cp pkt buf [%" PRIu64 "]: phead[%u] ptail[%u]\n",
    		disp_get_domain_id(), buffer->buffer_id, buffer->pbuf_head, buffer->pbuf_tail);
    	return false;
    }

    void *dst = (void *)(uintptr_t)upbuf->vaddr;

    ETHERSRV_DEBUG("Copy packet pos %p %p %p\n", buffer->va ,dst,
    		(buffer->va + (1L << buffer->bits)));

    if((dst < (buffer->va + (1L << buffer->bits))) && (dst >= buffer->va)) {
        memcpy((void*)(uintptr_t)upbuf->vaddr, data, len);
   } else {
    	// k: naughty client does not deserve a packet :)
    	ETHERSRV_DEBUG("naughty client detected\n");
    	return false;
    }
    // add trace pkt cpy
#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t)((uintptr_t)data);
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_PKT_CPY, pkt_location);
#endif // TRACE_ETHERSRV_MODE

    upbuf->packet_size = len;
    phead = (phead + 1) % RECEIVE_BUFFERS;
    buffer->pbuf_head = phead;
    ETHERSRV_DEBUG("packet copied to userspace\n");
    return send_packet_received_notification(buffer, upbuf);
}


static void send_arp_to_all(void *data, uint64_t len)
{
    struct filter *head = rx_filters;
//    ETHERSRV_DEBUG("### Sending the ARP packet to all....\n");
    /* sending ARP packets to only those who have registered atleast one
     * filter with e1000n
     * */

    /* FIXME: this code will send two copies or ARP if there are two filters
     * registered, which is incorrect.  Fix it. */
    while(head) {
		copy_packet_to_user(head->buffer, data, len);
		head = head->next;
    }

    // Forwarding it to netd as well.
    struct buffer_descriptor *buffer = ((struct client_closure *)
	    (netd[RECEIVE_CONNECTION]->st))->buffer_ptr;


#if TRACE_ETHERSRV_MODE
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_ARP,
				(uint32_t)(uintptr_t)data);
#endif // TRACE_ETHERSRV_MODE

    copy_packet_to_user(buffer, data, len);
}


struct filter *execute_filters(void *data, size_t len)
{
	struct filter *head = rx_filters;
	int res, error;
//	ETHERSRV_DEBUG("Starting the filter matching....\n");
	// TODO: gracefully handle the error cases, although I think
	// it is not really necessary. since it could only mean we have
	// received a corrupted packet.
	while(head) {
	    res = execute_filter(head->data, head->len, (uint8_t *)data,
		    len, &error);
	    if(res) {
                // FIXME IK: we need some way of testing how precise a match is
                // and take the most precise match (ie with the least wildcards)
                // Currently we just take the most recently added filter as
                // reflected by the order in the list.
	        ETHERSRV_DEBUG("##### Filter_id [%" PRIu64"] type[%" PRIu64"] matched giving buff [%"PRIu64"]..\n",
					head->filter_id, head->filter_type, head->buffer->buffer_id);
                return head;
	    }
	    head = head->next;
	}
        return NULL;
}

#if 0
static bool only_one_user_app(void)
{
	if(buffer_id_counter == 3 || buffer_id_counter == 4){
		if(first_app_b != NULL){
			return true;
		}
	}
	return false;
}
#endif // 0

void process_received_packet(void *pkt_data, size_t pkt_len)
{
	struct buffer_descriptor *buffer = NULL;
#if TRACE_ETHERSRV_MODE
	uint32_t pkt_location = (uint32_t)((uintptr_t)pkt_data);
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_A, pkt_location);
#endif // TRACE_ETHERSRV_MODE

	/* check if there is only one application,
	 * then directly transfer the packet. */

#if 0
	if(only_one_user_app()){
/*
		printf("Taking single app path with buff id %lu\n",
				first_app_b->buffer_id);

		if(copy_packet_to_user(first_app_b, pkt_data, pkt_len) == false) {
			printf("SA: Copy packet to userspace failed\n");
		}
//		printf("Application packet arrived for buff %lu\n", buffer->buffer_id);
		return;
*/
	}
#endif // 0

	if(handle_fragmented_packet(pkt_data, pkt_len)) {
		ETHERSRV_DEBUG("fragmented packet..\n");
		return;
	}


#if TRACE_ETHERSRV_MODE
	pkt_location = (uint32_t)((uintptr_t)pkt_data);
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_FRAG, pkt_location);
#endif // TRACE_ETHERSRV_MODE

	//	printf("normal non-SA mode packet\n");
	/* buffer = execute_filters(pkt_data, pkt_len); */

	// executing filters to find the relevant buffer
        struct filter *filter;
	if ((filter = execute_filters(pkt_data, pkt_len)) != NULL){
/*             printf("multiple app path with buff id %lu\n",
                               buffer->buffer_id);
*/

            buffer = filter->buffer;

            if(filter->paused) {
                assert(filter->pause_bufpos < MAX_PAUSE_BUFFER);
                struct bufdesc *bd = &filter->pause_buffer[filter->pause_bufpos++];
                memcpy(bd->pkt_data, pkt_data, pkt_len);
                bd->pkt_len = pkt_len;
            } else {
		bool ret = copy_packet_to_user(buffer, pkt_data, pkt_len);
		if(ret == false) {
//			printf("A: Copy packet to userspace failed\n");
		}
            }
		//debug_printf("Application packet arrived for buff %lu\n", buffer->buffer_id);
            return;
	}

// add trace filter_execution_end_1
#if TRACE_ETHERSRV_MODE
	pkt_location = (uint32_t)((uintptr_t)pkt_data);
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_EX_1, pkt_location);
#endif // TRACE_ETHERSRV_MODE


	/* no filter is succeeded. So, two case could happen:
	   1: arp packets. in this case we copy the arp packet to
		   every lwip instance which has an active filter, including netd
	   2: any other packet which includes dhcp responses. we copy
		   these to the netd. */

	int32_t res = 0;
	int32_t error;
	if(arp_filter_rx.data != NULL) {
/*      ETHERSRV_DEBUG("ARP compare data[%p], len %d\n",
				data, (int)len);
		show_binary_blob(arp_filter_rx.data, arp_filter_rx.len);
*/
		res = execute_filter(arp_filter_rx.data, arp_filter_rx.len,
					(uint8_t*)pkt_data, pkt_len, &error);

	}

	if(res) { // we have an arp packet
//      ETHERSRV_DEBUG("ARP packet...\n");
		send_arp_to_all(pkt_data, pkt_len);
		return;
	}

	// add trace filter_execution_end_2
#if TRACE_ETHERSRV_MODE
	pkt_location = (uint32_t)((uintptr_t)pkt_data);
	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_EX_2, pkt_location);
#endif // TRACE_ETHERSRV_MODE

	/* assuming that it is netd */
//  ETHERSRV_DEBUG("No client wants, giving it to netd\n");
	buffer = ((struct client_closure *)
			(netd[RECEIVE_CONNECTION]->st))->buffer_ptr;

//    ETHERSRV_DEBUG("sending packet up.\n");
    /* copy the packet to userspace */
	if(copy_packet_to_user(buffer, pkt_data, pkt_len) == false) {
		ETHERSRV_DEBUG("Copy packet to userspace failed\n");
		/* AB: commented out printf here. If we're on a busy network
		 * and packets keep arriving, the rate of debug printing is
		 * slower than the packet arrival rate, and we get stuck in a
		 * loop printing out these messages that we are discarding packets.
		 */
		// printf("O:Copy packet to userspace failed\n");
	}



} /* end function: process_received_packet */

/* This function tells if netd is registered or not. */
bool waiting_for_netd(void)
{
	return ((netd[RECEIVE_CONNECTION] == NULL)
			 || (netd[TRANSMIT_CONNECTION] == NULL));
} /* end function: is_netd_registered */


static void debug_status(struct ether_binding *cc, uint8_t state)
{
    struct client_closure *cl = ((struct client_closure *)(cc->st));
    cl->debug_state = state;
    debug_state = state;
} // end function: debug_status


void ethersrv_debug_printf(const char *fmt, ...)
{
    uint8_t dbg = debug_state;
    if (dbg == 0) {
        return;
    }

    va_list argptr;
    char str[512];
    size_t len;

    len = snprintf(str, sizeof(str), "ETHERSRV:");
    if (len < sizeof(str)) {
        va_start(argptr, fmt);
        vsnprintf(str + len, sizeof(str) - len, fmt, argptr);
        va_end(argptr);
    }
    sys_print(str, sizeof(str));
}

