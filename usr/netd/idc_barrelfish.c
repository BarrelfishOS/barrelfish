/**
 * \file
 * \brief the main header file for the net "daemon"
 *
 * This file is part of the net "daemon"
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
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/deferred.h>


#include <if/net_queue_manager_defs.h>
#include <if/netd_defs.h>
#include <if/ether_control_defs.h>


#include <netif/etharp.h>
#include <lwip/netif.h>
//#include <timer/timer.h>
#include <lwip/dhcp.h>
#include <lwip/init.h>
#include <bfdmuxtools/tools.h>
#include <bfdmuxtools/codegen.h>
#include <contmng/contmng.h>
#include <procon/procon.h>

/* For ICMP benchmark */
#include "lwip/icmp.h"


#include "netd.h"
#include "netd_debug.h"
#include "idc_barrelfish.h"


#define NR_URPC_MESSAGES 2048

#define NORMAL_FILTER       (1)
#define REDIRECT_FILTER      (2)

/* client closure for connection between netd and ethernet driver */
struct client_closure_ND {
    struct cont_queue *q;
};

/**
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/

/* FIXME: This should not be needed,
    arrange functions so that this prototype is not needed. */
static void idc_register_filter(uint64_t id, uint64_t len_rx, uint64_t len_tx,
                                uint64_t buffer_id_rx, uint64_t buffer_id_tx,
                                uint8_t ftype, uint8_t paused);
static void idc_register_arp_filter(uint64_t id, uint64_t len_rx,
                                    uint64_t len_tx);
static void idc_deregister_filter(uint64_t filter_id);
static void register_netd_service(void);

static void share_common_memory_with_filter_manager(void);

/**
 * \brief Link to the network driver
 *
 * As mentionned above, the link to the network driver is compoed by
 * two Chips channels. We store the Flounder closure of these
 * connections in the following array.
 *
 */
static struct ether_control_binding *ether_control_connection;

/**
 * \brief Status of the connections to network driver
 *
 * To be operational, LWIP has to wait for both transmit and receive
 * channels to be up. Hence the following array, updated when a
 * channel gets connected.
 *
 */
static bool ether_control_connected = false;


/**
 * \brief Number of established connections
 */

static struct eth_addr mac;
static struct netif netif;

static struct periodic_event dhcp_fine_timer;   /* fine-grain timer for DHCP */
static struct periodic_event dhcp_coarse_timer; /* coarse-grain timer for DHCP */

static char *net_ctrl_service_name = NULL;      /* Name of service provided by netd */

#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define RUN_ICMP_BENCHMARK 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE

/****************************************************/
#if RUN_ICMP_BENCHMARK
#include <trace/trace.h>
#define MAX_ICMP_PKTS 22

static errval_t init_tracing(void)
{
    trace_reset_all();

    // Tell the trace system when to start and stop.  We can also
    // provide an overriding maximum duration (in cycles) as the last parameter.
    errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_NET,
                                             TRACE_EVENT_NET_START, 0),
                                 TRACE_EVENT(TRACE_SUBSYS_NET,
                                             TRACE_EVENT_NET_STOP, 0), 0);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_control failed");
    }
    return err;
}

static void dump_trace(void)
{
    // dump the trace on the output.  We can copy and paste it
    // to use in Aquarium.

    printf("the trace dump\n");

    char *buf = malloc(4096 * 4096);

    trace_dump(buf, 4096 * 4096);
    printf("%s\n", buf);
    printf("finished trace dump\n");
}


static void start_tracing(void)
{
    // start the trace going by providing the start event
    printf("Tracing started\n");
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_START, 0);
}

static void stop_tracing(void)
{
    // stop the trace by providing the stop event
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_STOP, 0);
    printf("Tracing stopped\n");
    dump_trace();
}


static uint64_t icmp_pkt_count = 0;
static void icmp_packet_notification(struct pbuf *p)
{
//      printf("netd: ICMP notification\n");
    if (icmp_pkt_count == 0) {
        /* Alternate place to start the tracing */
        start_tracing();
    }
    /* end if: icmp flow just started */
    ++icmp_pkt_count;
    if (icmp_pkt_count == MAX_ICMP_PKTS) {
        /* Stop the trace. */
        stop_tracing();
        /* Followin three lines will reset the tracing */
        trace_reset_all();
        icmp_pkt_count = 0;
        return;
    }
    /* end if: max count reached. */
}                               /* end function: icmp_packet_notification */

static void start_icmp_benchmark(void)
{
    record_icmp_upcall(icmp_packet_notification);
    init_tracing();
    /* possible place to start tracing */
//      start_tracing();
}                               /* end function: start_icmp_benchmark */

#endif                          // RUN_ICMP_BENCHMARK



static void timer_callback(void *data)
{

//    NETD_DEBUG("timer_callback: called\n");
    void (*lwip_callback) (void) = data;

    lwip_callback();

//    NETD_DEBUG("timer_callback: terminated\n");
}



static void setup_dhcp_timer(void)
{
    errval_t err;

    // Initialize Timer and LWIP
    NETD_DEBUG("setting up timeouts for lwip\n");

    /* DHCP fine timer */
    err = periodic_event_create(&dhcp_fine_timer, get_default_waitset(),
                                (DHCP_FINE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_fine_tmr));
    assert(err_is_ok(err));

    /* DHCP coarse timer */
    err = periodic_event_create(&dhcp_coarse_timer, get_default_waitset(),
                                (DHCP_COARSE_TIMER_MSECS * 1000),
                                MKCLOSURE(timer_callback, dhcp_coarse_tmr));
    assert(err_is_ok(err));
}

static void link_status_change(struct netif *nf)
{
    static bool subsequent_call;

    assert(nf == &netif);

    if (netif_is_up(nf)) {
        printf("netd: interface is now up\n");
    } else {
        printf("netd: interface is now down\n");
        return;
    }

    if (subsequent_call) {
        if (ip_addr_cmp(&local_ip, &nf->ip_addr) != 0) {
            printf
              ("netd: WARNING: IP has changed! Current address: %d.%d.%d.%d",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
        }
    } else {
        // warning: some regression tests depend upon the format of this message
        printf("##########################################\n");
        printf("Interface up! IP address %d.%d.%d.%d\n",
               ip4_addr1(&nf->ip_addr), ip4_addr2(&nf->ip_addr),
               ip4_addr3(&nf->ip_addr), ip4_addr4(&nf->ip_addr));
    }

    local_ip = nf->ip_addr;
    netif_set_default(nf);

    if (!subsequent_call) {
#if 0
        /* Now, the timers are not needed.  They should be closed. */
        /* I don't agree -- we need to keep renewing our lease -AB */
        periodic_event_cancel(&dhcp_fine_timer);
        periodic_event_cancel(&dhcp_coarse_timer);
#endif

        NETD_DEBUG("registering netd service\n");
        register_netd_service();
    }

    subsequent_call = true;
}

static void get_ip_from_dhcp(void)
{

    {
        struct ip_addr ipaddr, netmask, gw;

        ip_addr_set(&ipaddr, IP_ADDR_ANY);
        ip_addr_set(&netmask, IP_ADDR_ANY);
        ip_addr_set(&gw, IP_ADDR_ANY);
        struct netif *n = netif_add(&netif, &ipaddr, &netmask, &gw,
                                    NULL, bfeth_init, ethernet_input);

        assert(n != NULL);
    }

    netif_set_status_callback(&netif, link_status_change);
    NETD_DEBUG("get_ip_from_dhcp: starting dhcp\n");
    setup_dhcp_timer();
    err_t err = dhcp_start(&netif);

    assert(err == ERR_OK);
}


/* Filters to be shared with the network driver */
int32_t filter_len_rx, filter_len_tx;

//static uint8_t* netd_filter_mem_rx = NULL; /* memory area for rx filters */
//static uint8_t* netd_filter_mem_tx = NULL; /* memory area for tx filters */
/* NOTE: memory is allocated by the "share_common_memory_with_filter_manager" */

static struct bulk_transfer bt_filter_tx;

bool filter_mem_lock = false; // protects the above filter memory within netd


/************************************************************************/
/*                            netd interface function                     */
/************************************************************************/
static errval_t send_ip_info(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_ip_info_response(b,
                                               MKCONT(cont_queue_callback,
                                                      nu->q), e.plist[0],
                                               e.plist[1], e.plist[2]);
        /*  entry.ip,   entry.gw,   entry.mask */
    } else {
        NETD_DEBUG("send_assign_ip: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}


static void get_ip_info(struct netd_binding *cc)
{
    NETD_DEBUG("get_ip_info: client asking for ip\n");
    NETD_DEBUG("get_ip_info: sending back response\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_ip_info;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = netif.ip_addr.addr;
    entry.plist[1] = netif.gw.addr;
    entry.plist[2] = netif.netmask.addr;
/*		e.plist[0], e.plist[1], e.plist[2]);
		entry.ip,   entry.gw,   entry.mask */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("get_ip_info: terminating\n");
}


static errval_t send_mac_address(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_mac_address_response(b,
                                                   MKCONT(cont_queue_callback,
                                                          nu->q), e.plist[0]);
    } else {
        NETD_DEBUG("send_mac_address: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void get_mac_address(struct netd_binding *cc)
{
    NETD_DEBUG("get_mac_address: client asking for ip\n");
    NETD_DEBUG("get_mac_address: sending back response\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_mac_address;
    entry.binding_ptr = (void *) cc;

    union {
        uint64_t val;
        struct eth_addr mac;
    } u = {
    .mac = mac};

    entry.plist[0] = u.val;

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("get_mac_address: terminating\n");
}



static errval_t send_bound_port(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.bind_port_response(b,
                                             MKCONT(cont_queue_callback, nu->q),
                                             e.plist[0]);
        /* entry.err */
    } else {
        NETD_DEBUG("send_bound_port: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

/**
\brief : sending a bound_port msg back to the app which requested the port with
"bind_port" msg.
*/
static void idc_bound_port(struct netd_binding *cc, errval_t err)
{

    NETD_DEBUG("idc_bound_port: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_bound_port;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;

    /* plist[0]
     * entry.err
     */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("idc_bound_port: terminated\n");
}



static errval_t send_redirected(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.redirect_response(b,
                                            MKCONT(cont_queue_callback, nu->q),
                                            e.plist[0]);
        /* entry.err */
    } else {
        NETD_DEBUG("send_redirected: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

static errval_t send_paused(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.redirect_pause_response(b,
                                                  MKCONT(cont_queue_callback,
                                                         nu->q), e.plist[0]);
        /* entry.err */
    } else {
        NETD_DEBUG("send_redirected: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}


/**
\brief : sending a redirected msg back to the app which requested the
* redirection with "redirect" msg.
*/
static void idc_redirect_response(struct netd_binding *cc, errval_t err)
{

    NETD_DEBUG("idc_redirected: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_redirected;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;
    /* plist[0]
     * entry.err
     */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("idc_redirect_response: terminated\n");
}                               /* end function: idc_redirect_response */

/**
\brief : sending a redirected msg back to the app which requested the
* redirection with "redirect" msg.
*/
static void idc_redirect_pause_response(struct netd_binding *cc, errval_t err)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_paused;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;
    /* plist[0]
     * entry.err
     */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("idc_redirect_response: terminated\n");
}                               /* end function: idc_redirect_response */


static errval_t send_close_port_responce(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.close_port_response(b,
                                              MKCONT(cont_queue_callback,
                                                     nu->q), e.plist[0]);
        /*  e.err */
    } else {
        NETD_DEBUG("send_close_port_responce: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}                               /* end function: send_close_port_responce */


/**
\brief : sending a close_port_response msg back to the app which
 requested the port closing with "close_port" msg.
*/
static void idc_close_port_response(struct netd_binding *cc, errval_t err)
{

    NETD_DEBUG("idc_close_port_response: called\n");
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_close_port_responce;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;

    /*  e.plist[0]
       e.err    */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("idc_close_port_response: terminated\n");
}


static errval_t send_new_port(struct q_entry e)
{
    struct netd_binding *b = (struct netd_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_port_response(b,
                                            MKCONT(cont_queue_callback, nu->q),
                                            e.plist[0], e.plist[1]);
        /*  e.err, e.port_no */
    } else {
        NETD_DEBUG("send_new_port: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}


/**
\brief : sending a new_port msg back to the app which requested the port with
"get_port" msg.
*/
static void idc_new_port(struct netd_binding *cc, errval_t err,
                         uint16_t port_no)
{

    NETD_DEBUG("idc_new_port: called\n");
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_new_port;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;
    entry.plist[1] = port_no;

    /*      e.plist[0], e.plist[1]
       e.err,     e.port_no  */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NETD_DEBUG("idc_new_port: terminated\n");
}


static uint64_t populate_rx_tx_filter_mem(uint16_t port, netd_port_type_t type,
                                          int32_t * len_rx, int32_t * len_tx)
{

    struct bulk_buf *bb;

    /* get memory chunk from shared memory */
    do {
        bb = bulk_alloc(&bt_filter_tx);
        if (bb == NULL) {
            // dispatch one
            NETD_DEBUG("bulk_alloc is returning NULL!!!! for filter memory\n");
            event_dispatch(get_default_waitset());
        }
    } while (bb == NULL);
    void *bbuf = bulk_buf_get_mem(bb);


    uint8_t *filter_mem = NULL;
    char *filter;

    // rx filter
    if (type == netd_PORT_TCP) {
        filter = build_ether_dst_ipv4_tcp_filter(mac, BFDMUX_IP_ADDR_ANY,
                                                 htonl(local_ip.addr), PORT_ANY,
                                                 (port_t) port);
    } else {
        filter = build_ether_dst_ipv4_udp_filter(mac, BFDMUX_IP_ADDR_ANY,
                                                 htonl(local_ip.addr), PORT_ANY,
                                                 (port_t) port);
    }
    /* FIXME: shouldn't be above two ports be wrapped in htons(port)? */
    compile_filter(filter, &filter_mem, len_rx);
    assert(*len_rx < BASE_PAGE_SIZE);

    assert(filter_mem != NULL);
    memcpy(bbuf, filter_mem, *len_rx);
    free(filter);
    free(filter_mem);

    // tx filter
    if (type == netd_PORT_TCP) {
        filter = build_ether_src_ipv4_tcp_filter(mac, htonl(local_ip.addr),
                                                 BFDMUX_IP_ADDR_ANY,
                                                 (port_t) port, PORT_ANY);
    } else {
        filter = build_ether_src_ipv4_udp_filter(mac, htonl(local_ip.addr),
                                                 BFDMUX_IP_ADDR_ANY,
                                                 (port_t) port, PORT_ANY);
    }
    compile_filter(filter, &filter_mem, len_tx);
    assert(*len_tx < BASE_PAGE_SIZE);

    void *bbuf_tx = bbuf + BASE_PAGE_SIZE;

    memcpy(bbuf_tx, filter_mem, *len_tx);

    free(filter);
    free(filter_mem);
    uint64_t id = bulk_prepare_send(bb);

    return id;
}


/* Finds filter_id from the given ports list */
static struct buffer_port_translation *find_filter_id(struct
                                                      buffer_port_translation
                                                      *port_list,
                                                      uint16_t port_no,
                                                      uint64_t type)
{
    while (port_list) {
        if (port_list->local_port == port_no && type == port_list->type) {
            port_list->closing = true;
            NETD_DEBUG("find_filter_id: found, and has id %" PRIu64 "\n",
                       port_list->filter_id);
            return (port_list);
        }
        port_list = port_list->next;
    }                           /* end while */
    return NULL;
}                               /* end function: find_filter_id */

static struct buffer_port_translation *find_filter_id_ex(struct
                                                         buffer_port_translation
                                                         *port_list,
                                                         uint16_t local_port_no,
                                                         uint16_t
                                                         remote_port_no,
                                                         uint64_t type)
{
    while (port_list) {
        if (port_list->local_port == local_port_no &&
            port_list->remote_port == remote_port_no &&
            type == port_list->type) {
            port_list->closing = true;
            NETD_DEBUG("find_filter_id: found, and has id %" PRIu64 "\n",
                       port_list->filter_id);
            return (port_list);
        }
        port_list = port_list->next;
    }                           /* end while */
    return NULL;
}                               /* end function: find_filter_id */

/**
\brief: assigns new port
*/
static void close_port(struct netd_binding *cc, netd_port_type_t type,
                       uint16_t port_no)
{
    errval_t err = SYS_ERR_OK;

    NETD_DEBUG("close_port: called\n");
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    bp = find_filter_id(this_net_app->open_ports, port_no, type);
    if (bp != NULL) {
        idc_deregister_filter(bp->filter_id);
    } else {
        NETD_DEBUG("close_port: port not found\n");
        err = PORT_ERR_NOT_FOUND;
        idc_close_port_response(cc, err);
    }
    NETD_DEBUG("close_port: exiting\n");
}

/**
\brief: assigns new port
*/
static void get_port(struct netd_binding *cc, netd_port_type_t type,
                     uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{

    errval_t err = SYS_ERR_OK;
    uint64_t port;
    int32_t len_rx, len_tx;
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    NETD_DEBUG("get_port: called\n");

    /* NOTE: check if someone else is using the filter location */
    if (filter_mem_lock) {
        err = FILTER_ERR_FILTER_BUSY;
        /* FIXME: as there is only one registered location for filter
           transfer, only one filter registration can be done at one time. */
        NETD_DEBUG("netd is busy.\n");
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }

    /* Record the state that this port is allocated to this app */
    bp =
      (struct buffer_port_translation *)
      malloc(sizeof(struct buffer_port_translation));
    if (bp == NULL) {
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        NETD_DEBUG("netd is out of memory.\n");
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }
    memset(bp, 0, sizeof(struct buffer_port_translation));

    /* FIXME: get free port from portalloc system */
    if (type == netd_PORT_TCP) {
        port = alloc_tcp_port();
    } else {
        port = alloc_udp_port();
    }

    if (port == 0) {
        err = PORT_ERR_NO_MORE_PORT;
        NETD_DEBUG("all the ports for this user are allocated!\n");
        free(bp);
        /* send continuation msg about new port */
        idc_new_port(cc, err, 0);
        return;
    }

    /* FIXME: these things won't be present right now.. */
//    assert(local_ip.addr);
//    assert(ether_control_conn != NULL);

    /* add information about this session to the list of live sessions */
    bp->st = cc;
    bp->local_port = port;
    bp->type = type;
    bp->buffer_id_rx = buffer_id_rx;
    bp->buffer_id_tx = buffer_id_tx;
    bp->active = false;
    bp->bind = false;
    bp->closing = false;
    bp->redirected = false;
    bp->next = this_net_app->open_ports;
    this_net_app->open_ports = bp;


    /* create rx, tx filter around that port */
    filter_mem_lock = true;     /* NOTE: filter memory is in use
                                   till "registered_filter" is called by filter_manager */
    uint64_t id = populate_rx_tx_filter_mem(port, type, &len_rx, &len_tx);

    /* Register the filter with ether_control */
    NETD_DEBUG("get_port: trying to register the filter with id %" PRIu64 "\n",
               id);
    idc_register_filter(id, len_rx, len_tx, buffer_id_rx, buffer_id_tx,
                        NORMAL_FILTER, 0);

    NETD_DEBUG("get_port: exiting\n");

}                               /* end function: get_port */


/**
\brief: bind to the requested port
*/
static void bind_port(struct netd_binding *cc, netd_port_type_t type,
                      uint16_t port_no, uint64_t buffer_id_rx,
                      uint64_t buffer_id_tx)
{

    errval_t err = SYS_ERR_OK;
    uint64_t port;
    int32_t len_rx, len_tx;
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    NETD_DEBUG("bind_port: called for port %" PRIu16 " with RX[%" PRIu64
               "] and TX[%" PRIu64 "]\n", port_no, buffer_id_rx, buffer_id_tx);

    /* NOTE: check if someone else is using the filter location */
    if (filter_mem_lock) {
        err = FILTER_ERR_FILTER_BUSY;
        /* FIXME: as there is only one registered location for filter
           transfer, only one filter registration can be done at one time. */
        NETD_DEBUG("netd is busy.\n");
        /* send continuation msg about new port */
        idc_bound_port(cc, err);
        return;
    }

    /* Record the state that this port is allocated to this app */
    bp =
      (struct buffer_port_translation *)
      malloc(sizeof(struct buffer_port_translation));
    if (bp == NULL) {
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        NETD_DEBUG("netd is out of memory.\n");
        /* send continuation msg about new port */
        idc_bound_port(cc, err);
        return;
    }
    memset(bp, 0, sizeof(struct buffer_port_translation));

    port = (uint64_t) alloc_specific_port((uint16_t) port_no, type);

    if (port == 0) {
        err = PORT_ERR_IN_USE;
        NETD_DEBUG("Requested port is in use!\n");
        free(bp);
        /* send continuation msg about bound port */
        idc_bound_port(cc, err);
        return;
    }
    NETD_DEBUG("obtained port is %" PRIu64 "\n", port);
    /* FIXME: these things won't be present right now.. */
//    assert(local_ip.addr);
//    assert(ether_control_conn != NULL);

    /* add information about this session to the list of live sessions */
    bp->st = cc;
    bp->local_port = port;
    bp->type = type;
    bp->buffer_id_rx = buffer_id_rx;
    bp->buffer_id_tx = buffer_id_tx;
    bp->active = false;
    bp->bind = true;
    bp->closing = false;
    bp->next = this_net_app->open_ports;
    this_net_app->open_ports = bp;

    /* create rx, tx filter around that port */
    filter_mem_lock = true;     /* NOTE: filter memory is in use
                                   till "registered_filter" is called by filter_manager */

    uint64_t id = populate_rx_tx_filter_mem(port, type, &len_rx, &len_tx);

    /* Register the filter with ether_control */
    NETD_DEBUG("bind_port: trying to register the filter with id %" PRIu64 "\n",
               id);
    idc_register_filter(id, len_rx, len_tx, buffer_id_rx, buffer_id_tx,
                        NORMAL_FILTER, 0);

    NETD_DEBUG("bind_port: exiting\n");

}                               /* end function: bind_port */


#if 0
// find the port, and move it from it's current app to the dest argument
static struct buffer_port_translation *move_port(int port,
                                                 struct net_user *dest)
{
    struct net_user *app = registerd_app_list;
    struct buffer_port_translation *bp, **prev;

    while (app != NULL) {
        bp = app->open_ports;
        prev = &(app->open_ports);
        while (bp != NULL) {
            if (bp->port == port) {
                *prev = bp->next;
                bp->next = dest->open_ports;
                dest->open_ports = bp;
                return bp;
            }
            prev = &(bp->next);
            bp = bp->next;
        }
        app = app->next;
    }

    return NULL;
}
#endif                          // 0






static uint64_t populate_redirect_rx_tx_filter_mem(netd_ipv4addr_t
                                                   local_ip_addr,
                                                   uint16_t local_port,
                                                   netd_ipv4addr_t
                                                   remote_ip_addr,
                                                   uint16_t remote_port,
                                                   netd_port_type_t type,
                                                   int32_t * len_rx,
                                                   int32_t * len_tx)
{

    NETD_DEBUG
      ("populate_redirect_rx_tx_filter_mem: local_ip_addr %u local_port %u remote_ip_addr %u remote_port %u type %d len_rx %d len_tx %d\n",
       local_ip_addr, local_port, remote_ip_addr, remote_port, type, *len_rx,
       *len_tx);


    struct bulk_buf *bb;

    /* get memory chunk from shared memory */
    do {
        bb = bulk_alloc(&bt_filter_tx);
        if (bb == NULL) {
            // dispatch one
            NETD_DEBUG("bulk_alloc is returning NULL!!!! for filter memory\n");
            event_dispatch(get_default_waitset());
        }
    } while (bb == NULL);
    void *bbuf = bulk_buf_get_mem(bb);

    assert(bbuf != NULL);

    uint8_t *filter_mem = NULL;
    char *filter;

    // rx filter
    if (type == netd_PORT_TCP) {
        filter = build_ether_dst_ipv4_tcp_filter(mac, htonl(remote_ip_addr),
                                                 htonl(local_ip_addr),
                                                 (port_t) remote_port,
                                                 (port_t) local_port);
        NETD_DEBUG("dst filter: %s\n", filter);
    } else {
        // FIXME: we don't do UDP right now
        assert(!"NYI");
    }

    /* FIXME: shouldn't be above two ports be wrapped in htons(port)? */

    NETD_DEBUG("compile filter\n");
    compile_filter(filter, &filter_mem, len_rx);
    assert(*len_rx < BASE_PAGE_SIZE);
    assert(filter_mem != NULL);

    NETD_DEBUG("copy filter %p, %p, %d\n", bbuf, filter_mem, *len_rx);
    memcpy(bbuf, filter_mem, *len_rx);
    NETD_DEBUG("free filter\n");
    free(filter);
    NETD_DEBUG("copy filter\n");
    free(filter_mem);
    NETD_DEBUG("now, doing it for TX filter\n");

    // tx filter
    if (type == netd_PORT_TCP) {
        filter = build_ether_src_ipv4_tcp_filter(mac, htonl(local_ip_addr),
                                                 htonl(remote_ip_addr),
                                                 (port_t) local_port,
                                                 (port_t) remote_port);
        NETD_DEBUG("tx filter: %s\n", filter);
        //        debug_printf("src filter: %s\n", filter);
    } else {
        // FIXME: we don't do UDP right now
        assert(!"NYI");
    }
    NETD_DEBUG("compile filter\n");
    compile_filter(filter, &filter_mem, len_tx);
    assert(*len_tx < BASE_PAGE_SIZE);

    NETD_DEBUG("copy filter\n");
    void *bbuf_tx = bbuf + BASE_PAGE_SIZE;

    memcpy(bbuf_tx, filter_mem, *len_tx);

    NETD_DEBUG("free filter\n");
    free(filter);
    NETD_DEBUG("free filter\n");
    free(filter_mem);
    uint64_t id = bulk_prepare_send(bb);

    NETD_DEBUG("done with it\n");
    return id;
}

static errval_t send_filterID_for_pause(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.pause(b,
                                MKCONT(cont_queue_callback, ccnc->q),
                                e.plist[0], e.plist[1], e.plist[2]);
        /*  e.filterID, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        NETD_DEBUG("send_filterID_for_re_registration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/**
 * \brief sends the filterID for de-registration to network driver.
 *
 */
static void idc_unpause_filter(uint64_t filter_id, uint64_t buffer_id_rx,
                               uint64_t buffer_id_tx)
{
    NETD_DEBUG("idc_pause_filter: called for id %" PRIu64 "\n", filter_id);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filterID_for_pause;

    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = filter_id;
    entry.plist[1] = buffer_id_rx;
    entry.plist[2] = buffer_id_tx;
    /*    e.plist[0], e.plist[1],     e.plist[2]
       e.filterID, e.buffer_id_rx, e.buffer_id_rx */

    enqueue_cont_q(ccnc->q, &entry);

    NETD_DEBUG("idc_pause_filter: terminated for id %" PRIu64 "\n", filter_id);
}                               /* end function: idc_re_register_filter */

/**
\brief: redirect the requested connection
*  change the tx and rx buffers associated with a connection
*/
static void redirect(struct netd_binding *cc, netd_port_type_t type,
                     netd_ipv4addr_t local_ip_addr, uint16_t local_port,
                     netd_ipv4addr_t remote_ip_addr, uint16_t remote_port,
                     uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;

    NETD_DEBUG("redirect: called for local_port %" PRIu16 " remote_port %"
               PRIu16 " with RX[%" PRIu64 "] and TX[%" PRIu64 "], type = %d\n",
               local_port, remote_port, buffer_id_rx, buffer_id_tx, type);

    /* we only support migrating TCP ports at the moment */
    if (type != netd_PORT_TCP) {
        err = PORT_ERR_REDIRECT;
        NETD_DEBUG("redirect: netd requested redirect of non TCP port.\n");
        /* send continuation msg about new port */
        idc_redirect_response(cc, err);
        return;
    }

    /* NOTE: check if someone else is using the filter location */
    if (filter_mem_lock) {
        err = FILTER_ERR_FILTER_BUSY;
        /* FIXME: as there is only one registered location for filter
           transfer, only one filter registration can be done at one time. */
        NETD_DEBUG("redirect: netd is busy.\n");
        /* send continuation msg about new port */
        idc_redirect_response(cc, err);
        return;
    }

    /* TODO: Find the filter_id, which refers to this port/type combination. */
    struct buffer_port_translation *bp = NULL;

    for (struct net_user * this_net_app = registerd_app_list;
         this_net_app != NULL; this_net_app = this_net_app->next) {
        /* struct net_user *this_net_app = (struct net_user *)cc->st; */
        bp =
          find_filter_id_ex(this_net_app->open_ports, local_port, remote_port,
                            type);
        if (bp != NULL) {
            /* printf("found bp %p, filter id %lu\n", bp, bp->filter_id); */
            break;
        }
    }
    if (bp != NULL) {
        /* TODO: ask driver to change the buff_id, related to this filter id */
        idc_unpause_filter(bp->filter_id, buffer_id_rx, buffer_id_tx);
    } else {
        printf("redirect: port not found\n");
        err = PORT_ERR_NOT_FOUND;
    }
    idc_redirect_pause_response(cc, err);
}                               /* end function: redirect */

/**
\brief: redirect the requested connection
*  change the tx and rx buffers associated with a connection
*/
static void redirect_pause(struct netd_binding *cc, netd_port_type_t type,
                           netd_ipv4addr_t local_ip_addr, uint16_t local_port,
                           netd_ipv4addr_t remote_ip_addr, uint16_t remote_port,
                           uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;

    NETD_DEBUG("redirect_pause: called for local_port %" PRIu16 " remote_port %"
               PRIu16 " with RX[%" PRIu64 "] and TX[%" PRIu64 "]\n", local_port,
               remote_port, buffer_id_rx, buffer_id_tx);

    /* we only support migrating TCP ports at the moment */
    if (type != netd_PORT_TCP) {
        err = PORT_ERR_REDIRECT;
        printf("pause: netd requested redirect of non TCP port.\n");
        /* send continuation msg about new port */
        idc_redirect_pause_response(cc, err);
        return;
    }

    /* Record the state that this port is allocated to this app */
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    bp =
      (struct buffer_port_translation *)
      malloc(sizeof(struct buffer_port_translation));
    if (bp == NULL) {
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        NETD_DEBUG("netd is out of memory.\n");
        /* send continuation msg about new port */
        idc_redirect_response(cc, err);
        return;
    }
    memset(bp, 0, sizeof(struct buffer_port_translation));

    /* add information about this session to the list of live sessions */
    bp->st = cc;
    bp->local_port = local_port;
    bp->local_ip = local_ip_addr;
    bp->remote_port = remote_port;
    bp->remote_ip = remote_ip_addr;
    bp->type = type;
    bp->buffer_id_rx = buffer_id_rx;
    bp->buffer_id_tx = buffer_id_tx;
    //    bp->redirected = true;
    bp->active = false;
    bp->bind = false;
    bp->closing = false;
    bp->paused = true;
    bp->next = this_net_app->open_ports;
    this_net_app->open_ports = bp;

    int32_t len_rx = 0;
    int32_t len_tx = 0;
    uint64_t bulk_id = populate_redirect_rx_tx_filter_mem(local_ip_addr,
                                                          local_port,
                                                          remote_ip_addr,
                                                          remote_port, type,
                                                          &len_rx, &len_tx);

    idc_register_filter(bulk_id, len_rx, len_tx, buffer_id_rx, buffer_id_tx,
                        REDIRECT_FILTER, 1);
}                               /* end function: redirect */


static void register_arp_filter_response(struct ether_control_binding *st,
                                         uint64_t id, errval_t err)
{

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "register_arp_filter_response failed for ID %" PRIu64 "",
                  id);
        abort();
    }
    NETD_DEBUG("register_arp_filter_response: ARP filter ID %" PRIu64
               " registered\n", id);

    NETD_DEBUG("register_arp_filter_response: getting ip from dhcp now\n");
    /* free the memory in shared area */
    errval_t free_err = bulk_free(&bt_filter_tx, id);

    assert(err_is_ok(free_err));

    filter_mem_lock = false;
    assert(err == SYS_ERR_OK);
    /* call get_ip_from_dhcp */

    get_ip_from_dhcp();

    NETD_DEBUG("register_arp_filter_response: ID %" PRIu64 " Terminating\n",
               id);
}


static void req_arp_filter_registration(void)
{

    struct bulk_buf *bb;

    /* get memory chunk from shared memory */
    do {
        bb = bulk_alloc(&bt_filter_tx);
        if (bb == NULL) {
            // dispatch one
            NETD_DEBUG("bulk_alloc is returning NULL!!!! for filter memory\n");
            event_dispatch(get_default_waitset());
        }
    } while (bb == NULL);
    void *bbuf = bulk_buf_get_mem(bb);

    uint8_t *filter_mem = NULL;

    /* FIXME: Do I still need this following lock? */
    filter_mem_lock = true;
    NETD_DEBUG("req_arp_filter_registration: registering arp filter\n");
    char *filter = NULL;
    int32_t len_rx, len_tx;

    filter = build_generic_arp_reply_filter();
    compile_filter(filter, &filter_mem, &len_rx);
    assert(filter_mem != NULL);
    assert(len_rx < BASE_PAGE_SIZE);
    memcpy(bbuf, filter_mem, len_rx);
    NETD_DEBUG("#### The arp RX filter is\n");
//    show_binary_blob(filter_mem, len_rx);
    free(filter);
    free(filter_mem);
    filter = NULL;
    filter_mem = NULL;

    NETD_DEBUG("#### now building TX filter for mac [%s]\n", mac.addr);
    filter = build_arp_transmit_filter(mac);
    assert(filter != NULL);
    compile_filter(filter, &filter_mem, &len_tx);
    assert(filter_mem != NULL);
    assert(len_rx < BASE_PAGE_SIZE);
    void *bbuf_tx = bbuf + BASE_PAGE_SIZE;

    NETD_DEBUG("#### copying the TX filter now to %p from %p of size %d\n",
               bbuf_tx, filter_mem, len_tx);
    memcpy(bbuf_tx, filter_mem, len_tx);
    NETD_DEBUG("#### The arp TX filter is\n");
//    show_binary_blob(filter_mem, len_tx);

    free(filter);
    free(filter_mem);

    uint64_t id = bulk_prepare_send(bb);

    /* Register the arp_filter with ether_control */
    NETD_DEBUG("req_arp_filter_registration:ID[%" PRIu64 "] RX[%d] TX[%d]\n",
               id, len_rx, len_tx);
    idc_register_arp_filter(id, len_rx, len_tx);

    NETD_DEBUG("req_arp_filter_registration: exiting\n");

//    ether_control_conn->call_vtbl->register_arp_filter(ether_control_conn, len_rx,
//          len_tx);
}

/* FIXME: This function is defined in lib/lwip/src/barrelfish/idc_barrelfish.c
 * and is declared in lib/lwip/src/barrelfish/idc_barrelfish.h
 * I am redeclaring it here because I could not figure out a sane way to include
 * the declaration from that place. */
void idc_get_mac_address(uint8_t * mac);

/**
* \brief: Called by driver when memory is registered with driver.
*/
static void register_filter_memory_response(struct ether_control_binding *st,
                                            errval_t err)
{
    assert(err_is_ok(err));
    NETD_DEBUG("NETD: memory registration successful.\n");

    /* Filter memory is registered, now lets start the next step */
    /* call get_mac_address */
//    idc_get_mac_address_netd();

    memset(mac.addr, 0, 6);
    idc_get_mac_address((uint8_t *) & mac.addr);
    /* Check if you have correct mac address. */
    printf("##################################################\n");
    printf("MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
           mac.addr[0], mac.addr[1], mac.addr[2], mac.addr[3],
           mac.addr[4], mac.addr[5]);
    netif.hwaddr_len = 6;
    memcpy(netif.hwaddr, (uint8_t *) (&mac.addr), 6);
    /* mac address received, now lets start the next step */
    /*  The next step is to "register_arp_filter" */
    req_arp_filter_registration();
}



/**
* \brief: called by driver when filter is deregister_filter_response.
        Responsible for calling appropriate callback in app, reporting the
        status. It is some kind of IDC forwarding mechanism.
        (or a gateway for that matter)
*/
static void deregister_filter_response(struct ether_control_binding *st,
                                       errval_t err, uint64_t filter_id)
{
    if (err_is_ok(err)) {
        NETD_DEBUG("NETD: filter at id %" PRIu64 " deregistered.\n", filter_id);
    }

    struct net_user *one_app = registerd_app_list;
    struct buffer_port_translation *bp;
    struct buffer_port_translation *prev;

    /* We don't know yet what exactly the filter is registered for.
       So, find it out. */
    /* FIXME: modify the code to work without buffer_id_rx and buffer_id_tx.
     * Instead, use the id field to locate the request. */
    while (one_app) {
        bp = one_app->open_ports;
        prev = NULL;
        while (bp) {            /* It is two dimensional linked list. */
            if (bp->filter_id == filter_id) {

                free_port(bp->local_port, bp->type);
                NETD_DEBUG("port [%" PRIu16 "] of type [%" PRIu64
                           "] is closed\n", bp->local_port, bp->type);
                /* remove this port from list */
                if (prev == NULL) {
                    /* This is the first port in list */
                    one_app->open_ports = bp->next;
                } else {
                    /* this is somewhere in middle/last in list */
                    prev->next = bp->next;
                }
                idc_close_port_response(bp->st, SYS_ERR_OK);
                free(bp);
                return;
            }
            /* end if: match found */
            prev = bp;
            bp = bp->next;
        }                       /* end while : for each registration within app */

        one_app = one_app->next;
    }                           /* end while : for each app registered */
    NETD_DEBUG("ERROR: deregister_filter_response: filter id [%" PRIu64
               "] not found\n", filter_id);

}                               /* end function: registered_filter */

static void pause_response(struct ether_control_binding *st,
                           uint64_t filter_id, errval_t err)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pause_response");
    }
    assert(err_is_ok(err));
}                               /* end function: registered_filter */


/**
* \brief: called by driver when filter is registered.
        Responsible for calling appropriate callback in app, reporting the
        status. It is some kind of IDC forwarding mechanism.
        (or a gateway for that matter)
*/
static void register_filter_response(struct ether_control_binding *st,
                                     uint64_t id, errval_t err,
                                     uint64_t filter_id, uint64_t buffer_id_rx,
                                     uint64_t buffer_id_tx, uint64_t ftype)
{
    assert(err_is_ok(err));
    NETD_DEBUG("NETD: filter at id [%" PRIu64 "] type[%" PRIu64
               "] registered with filt_id %" PRIu64 ".\n", id, ftype,
               filter_id);

    /* Ensure that, after filter is successfully registered, callback
       will be called informing successful registration of the port using
       idc_new_port */

    struct net_user *one_app = registerd_app_list;
    struct buffer_port_translation *bp;
    struct buffer_port_translation *prev;

    /* free the memory in shared area */
    errval_t free_err = bulk_free(&bt_filter_tx, id);

    assert(err_is_ok(free_err));
    filter_mem_lock = false;    /* NOTE: filter memory can be used by others now */

    /* We don't know yet what exactly the filter is registered for.
       So, find it out. */
    /* FIXME: modify the code to work without buffer_id_rx and buffer_id_tx.
     * Instead, use the id field to locate the request. */
    while (one_app) {
        bp = one_app->open_ports;
        prev = NULL;
        while (bp) {            /* It is two dimensional linked list. */

            if (bp->buffer_id_rx == buffer_id_rx &&
                bp->buffer_id_tx == bp->buffer_id_tx) {
                /* this is the entry for which we got the response */
                if (err == SYS_ERR_OK) {
                    /* wanted case: filter registration successful */
                    bp->active = true;
                    bp->filter_id = filter_id;
                } else if (err == ETHERSRV_ERR_BUFFER_NOT_FOUND) {
                    NETD_DEBUG("no buffer found on the driver\n");
                    err = FILTER_ERR_BUFF_NOT_FOUND;
                }

                /* OK, we found the correct entry, now call the proper
                   function to inform app about registration */
                if (bp->paused) {
                    idc_redirect_pause_response(bp->st, err);
                } else if (bp->redirected) {
                    idc_redirect_response(bp->st, err);
                } else if (bp->bind) {
                    idc_bound_port(bp->st, err);
                } else {
                    idc_new_port(bp->st, err, bp->local_port);
                }

                // cleaning up
                /* when err is not SYS_ERR_OK, one should release the bp
                   as it seems that filter_registration is failed,
                   and user should retry. */
                if (!bp->active) {
                    if (prev == NULL) {
                        one_app->open_ports = bp->next;
                    } else {
                        prev->next = bp->next;
                    }
                    /* It is safe to release memory here,
                       as idc_*_port makes the copy of all the info needed. */
                    free(bp);
                }

                /* end if: not active */
                /* skipping remaining list as we found the match,
                   and triggered the callback. */
                return;

            }
            /* end if: match found */
            prev = bp;
            bp = bp->next;
        }                       /* end while : for each registration within app */

        one_app = one_app->next;
    }                           /* end while : for each app registered */
    NETD_DEBUG("client buffer_id not found");

}                               /* end function: registered_filter */


/************************************************************************/

static struct ether_control_rx_vtbl rx_vtbl = {
    .register_filter_memory_response = register_filter_memory_response,
    .register_filter_response = register_filter_response,
    .deregister_filter_response = deregister_filter_response,
    .register_arp_filter_response = register_arp_filter_response,
    .pause_response = pause_response,
};


// Initialize service  WORK_IN_PROGRESS ################################
static struct netd_rx_vtbl rx_netd_vtbl = {
    .get_ip_info_call = get_ip_info,
    .get_port_call = get_port,
    .get_mac_address_call = get_mac_address,
    .bind_port_call = bind_port,
    .redirect_call = redirect,
    .redirect_pause_call = redirect_pause,
    .close_port_call = close_port,
};



static void ether_control_bind_cb(void *st, errval_t err,
                                  struct ether_control_binding *enb)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
    NETD_DEBUG("NETD: ether_control_bind_cb: started\n");


    struct client_closure_ND *ccnd = (struct client_closure_ND *)
      malloc(sizeof(struct client_closure_ND));

    memset(ccnd, 0, sizeof(struct client_closure_ND));
    ccnd->q = create_cont_q("C_NETD");

    /* set client closure */
    enb->st = ccnd;
    // copy my message receive handler vtable to the binding
    enb->rx_vtbl = rx_vtbl;

    ether_control_connection = enb;
    ether_control_connected = true;
    NETD_DEBUG("NETD: ether_control_bind_cb: connection made,"
               " now registering memory \n");
    share_common_memory_with_filter_manager();
    NETD_DEBUG("NETD: ether_control_bind_cb: terminated\n");
}


/**
\brief : This function establishes single connection to the ethernet driver.
*/
static void single_connect_to_ether_filter_manager(char *cname)
{

    assert(cname != NULL);

    errval_t err;
    iref_t iref;

    NETD_DEBUG("connect_to_ether_driver: resolving driver %s\n", cname);

    err = nameservice_blocking_lookup(cname, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "netd: could not connect to the e1000 filter driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    NETD_DEBUG("connect_to_ether_driver: connecting\n");

//XXX: asq: it seems that we cannot pass a number of messages here.
//          The generated flounder code asserts that this value is 0.

    err =
      ether_control_bind(iref, ether_control_bind_cb, NULL,
                         get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));     // XXX

    NETD_DEBUG("connect_to_ether_driver: terminated\n");
}                               /* end function: connect_to_ether_driver */


/**
   \brief: Establishes two way connection with ether_control
*/
static void connect_to_ether_filter_manager(char *cname)
{

    NETD_DEBUG("connect_to_ether_filter_manager: start client\n");

    /* establish the connection with ethernet driver */
    single_connect_to_ether_filter_manager(cname);

    NETD_DEBUG("connect_to_ether_filter_manager: wait connection\n");
    while (!ether_control_connected) {
        messages_wait_and_handle_next();
    }

    NETD_DEBUG("connect_to_ether_filter_manager: terminated\n");
}                               /* end function: */

static errval_t send_filter_memory_cap(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_memory_request(b,
                                                         MKCONT
                                                         (cont_queue_callback,
                                                          ccnc->q), e.cap);
    } else {
        NETD_DEBUG("send_filter_memory_cap: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/**
 * \brief sends cap to the memory which is to be shared between filter_manager
 *   of network driver and netd.
 *
 */
static void idc_register_filter_memory(struct capref cap)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filter_memory_cap;
    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.cap = cap;
    enqueue_cont_q(ccnc->q, &entry);

    NETD_DEBUG("##### idc_register_filter_memory: terminated\n");
}


/**
* \brief: share the memory so that filter passing can be started.
*/
static void share_common_memory_with_filter_manager(void)
{
    errval_t err;
    struct capref frame;
    size_t size = BASE_PAGE_SIZE * 2;
    size_t total_size = size * 7;

    NETD_DEBUG("#### netd: SCMWFM: started\n");

    NETD_DEBUG("##### netd: SCMWFM: allocating %lu bytes of memory.\n", size);

#if defined(__scc__) && !defined(RCK_EMU)
    err = bulk_create(total_size, size, &frame, &bt_filter_tx, true);
#else
    err = bulk_create(total_size, size, &frame, &bt_filter_tx, false);
#endif                          // defined(__scc__) && !defined(RCK_EMU)

    assert(err_is_ok(err));

    NETD_DEBUG("###### SCMWFM: registering netd filter memory\n");
    idc_register_filter_memory(frame);
    NETD_DEBUG("##### SCMWFM: terminated\n");

}                               /* end function: share_common_memory_with_filter_manager */


/**
* \brief: Start the broker client by connecting to filter_manager (ether_control)
*       and sharing the memory with it.
*/
void init_controller_service(char *filter_controller, char *service_name)
{
    net_ctrl_service_name = service_name;
    connect_to_ether_filter_manager(filter_controller);
//    share_common_memory_with_filter_manager();
    /* it will be automatically called when connection is established */
}

/*********  Functionality for filter registration *********/

static errval_t send_filter_for_registration(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_request(b,
                                                  MKCONT(cont_queue_callback,
                                                         ccnc->q), e.plist[0],
                                                  e.plist[1], e.plist[2],
                                                  e.plist[3], e.plist[4],
                                                  e.plist[5], e.plist[6]);
        /*  e.id,       e.len_rx,   e.len_tx,   e.buffer_id_rx, e.buffer_id_rx, e.ftype */

    } else {
        NETD_DEBUG("send_filter_for_registration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/**
 * \brief sends the filter for registration to network driver.
 *
 */
static void idc_register_filter(uint64_t id, uint64_t len_rx,
                                uint64_t len_tx, uint64_t buffer_id_rx,
                                uint64_t buffer_id_tx, uint8_t ftype,
                                uint8_t paused)
{
    NETD_DEBUG("idc_register_filter: called for id %" PRIu64
               " and type %x, paused = %d\n", id, ftype, paused);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filter_for_registration;

    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = id;
    entry.plist[1] = len_rx;
    entry.plist[2] = len_tx;
    entry.plist[3] = buffer_id_rx;
    entry.plist[4] = buffer_id_tx;
    entry.plist[5] = ftype;
    entry.plist[6] = paused;
    /* e.plist[0], e.plist[1], e.plist[2], e.plist[3],     e.plist[4],     e.plist[4]);
       e.id,       e.len_rx,   e.len_tx,   e.buffer_id_rx, e.buffer_id_rx, ftype */

    enqueue_cont_q(ccnc->q, &entry);

    NETD_DEBUG("idc_register_filter: terminated for id %" PRIu64 "\n", id);
}

#if 0
static errval_t send_filterID_for_re_registration(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.re_register_filter_request(b,
                                                     MKCONT(cont_queue_callback,
                                                            ccnc->q),
                                                     e.plist[0], e.plist[1],
                                                     e.plist[2]);
        /*  e.filterID, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        NETD_DEBUG("send_filterID_for_re_registration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/**
 * \brief sends the filterID for de-registration to network driver.
 *
 */
static void idc_re_register_filter(uint64_t filter_id, uint64_t buffer_id_rx,
                                   uint64_t buffer_id_tx)
{
    NETD_DEBUG("idc_re_register_filter: called for id %" PRIu64 "\n", id);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filterID_for_re_registration;

    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = filter_id;
    entry.plist[1] = buffer_id_rx;
    entry.plist[2] = buffer_id_tx;
    /*    e.plist[0], e.plist[1],     e.plist[2]
       e.filterID, e.buffer_id_rx, e.buffer_id_rx */

    enqueue_cont_q(ccnc->q, &entry);

    NETD_DEBUG("idc_re_register_filter: terminated for id %" PRIu64 "\n",
               filter_id);
}                               /* end function: idc_re_register_filter */
#endif                          // 0

static errval_t send_filterID_for_deregistration(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.deregister_filter_request(b,
                                                    MKCONT(cont_queue_callback,
                                                           ccnc->q),
                                                    e.plist[0]);
        /*  e.filterID */

    } else {
        NETD_DEBUG("send_filterID_for_deregistration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}                               /* end function: send_filterID_for_deregistration */

/**
 * \brief sends the filterID for de-registration to network driver.
 *
 */
static void idc_deregister_filter(uint64_t filter_id)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filterID_for_deregistration;

    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = filter_id;
    /*    e.plist[0]
       e.filter_id */

    enqueue_cont_q(ccnc->q, &entry);;
}


static errval_t send_arp_filter_for_registration(struct q_entry e)
{
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_arp_filter_request(b,
                                                      MKCONT
                                                      (cont_queue_callback,
                                                       ccnc->q), e.plist[0],
                                                      e.plist[1], e.plist[2]);
        /*  id,         e.len_rx,   e.len_tx */

    } else {
        NETD_DEBUG("send_arp_filter_for_registration: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

/**
 * \brief sends the filter for registration to network driver.
 *
 */
static void idc_register_arp_filter(uint64_t id, uint64_t len_rx,
                                    uint64_t len_tx)
{
    NETD_DEBUG("idc_register_arp_filter: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_arp_filter_for_registration;

    struct ether_control_binding *b = ether_control_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = id;
    entry.plist[1] = len_rx;
    entry.plist[2] = len_tx;
    /*    e.plist[0], e.plist[1], e.plist[2]
       id,         e.len_rx,   e.len_tx   */

    enqueue_cont_q(ccnc->q, &entry);

    NETD_DEBUG("idc_register_arp_filter: terminated\n");
}

/*****************************************************/
/************************* netd code *****************/

static void export_netd_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "netd export failed");
        abort();
    }

    NETD_DEBUG("netd service exported at iref %u\n", iref);

    // register this iref with the name service
    err = nameservice_register(net_ctrl_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for netd");
        abort();
    }
    NETD_DEBUG("netd service-name [%s] registered\n", net_ctrl_service_name);
    /* Now, netd is done with all the startups. */

#if RUN_ICMP_BENCHMARK
    printf("netd: starting tracing for RUN_ICMP_BENCHMARK\n");
    /* start the ICMP benchmark */
    start_icmp_benchmark();
#endif                          // RUN_ICMP_BENCHMARK
}


static errval_t connect_netd_cb(void *st, struct netd_binding *b)
{
    errval_t err = SYS_ERR_OK;

    NETD_DEBUG("netd service got a connection!\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_netd_vtbl;

    /* using the b->st to store session specific data (net_user) */
    struct net_user *new_net_app = malloc(sizeof(struct net_user));

    if (new_net_app == NULL) {
        NETD_DEBUG("error: malloc failed...\n");
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        return err;
    }

    memset(new_net_app, 0, sizeof(struct net_user));
    new_net_app->next = registerd_app_list;
    registerd_app_list = new_net_app;
    b->st = (void *) new_net_app;

    new_net_app->q = create_cont_q("NETD2APP");

    /* FIXME: when these net_apps are removed from the list? */
    /* or, there is any callback when connection between to processes is
       terminated? */
    NETD_DEBUG("connect_netd: New app registered\n");

    // accept the connection (we could return an error to refuse it)
    return err;
}                               /* end function: connect_netd_cb */


static void register_netd_service(void)
{
/*    memset(buffer_table, 0, sizeof(struct
    buffer_descriptor)*BUFFER_TABLE_SIZE);
*/

    /* exporting the netd interface */
    errval_t err =
      netd_export(NULL /* state pointer for connect/export callbacks */ ,
                  export_netd_cb, connect_netd_cb, get_default_waitset(),
                  IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "netd export failed");
        abort();
    }
}
