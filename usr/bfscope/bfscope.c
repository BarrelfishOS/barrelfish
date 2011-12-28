/**
 * \file
 * \brief Barrelfish trace server
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/lmp_endpoints.h>
#include <trace/trace.h>

/* LWIP Network stack includes... */
#include <lwip/init.h>
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <netif/etharp.h>

#define BFSCOPE_TCP_PORT 666

#define BFSCOPE_BUFLEN (2<<20)

extern void idc_print_statistics(void);
extern void idc_print_cardinfo(void);
extern void network_polling_loop(void);

static char *trace_buf = NULL;
static size_t trace_length = 0;
static size_t trace_sent = 0;

static uint64_t timestamp_start = 0;

static struct trace_buffer *bfscope_save_tracebuf = NULL;
static bool bfscope_trace_acquired = false;

static struct tcp_pcb *bfscope_client = NULL;


#define DEBUG if (0) printf

/*
 * \brief Close the specified TCP connection
 */
static void bfscope_connection_close(struct tcp_pcb *tpcb)
{
    DEBUG("bfscope: close\n");
    trace_length = 0;
    tcp_arg(tpcb, NULL);
    tcp_close(tpcb);
    bfscope_client = NULL;
}

/*
 * \brief Error callback from lwip 
 */
static void error_cb(void *arg, err_t err)
{
    struct tcp_pcb *tpcb = (struct tcp_pcb *)arg;

    DEBUG("bfscope: TCP(%p) error %d\n", arg, err);

    if (tpcb) bfscope_connection_close(tpcb);
}

/*
 * \brief Send the next chunk of trace data down given TCP connection
 */
static void bfscope_trace_send(struct tcp_pcb *tpcb)
{
    char *bufptr;
    int len;

    //DEBUG("tcp_sndbuf=%d\n", tcp_sndbuf(tpcb));
    
    bufptr = trace_buf + trace_sent;
    len = trace_length - trace_sent;
    
    int more = 0;
    if (len > tcp_sndbuf(tpcb)) {
        len = tcp_sndbuf(tpcb);
        more = 1;
    }
    
    /* Give the data to LWIP until it runs out of buffer space */
    int r = tcp_write(tpcb, bufptr, len, 
                      TCP_WRITE_FLAG_COPY | (more ? TCP_WRITE_FLAG_MORE : 0)); 

    //DEBUG("%d %ld+%d\n", r, trace_sent, len);

    if (r == ERR_MEM) {
        printf("BAD!\n");
        return;
    }
    trace_sent += len;
    
    if (trace_sent >= trace_length) {
        /* No more events */
        uint64_t timestamp_stop = rdtsc();
        DEBUG("bfscope: done (%lu bytes) in %ld cycles\n", 
               trace_sent, timestamp_stop - timestamp_start);
        trace_length = 0;
        trace_sent = 0;
    }
}

/*
 * \brief Callback from LWIP when each chunk of data has been sent 
 */
static err_t send_cb(void *arg, struct tcp_pcb *tpcb, u16_t length)
{
    //printf("send_cb %d\n", length);

    /* If we haven't finished sending the trace, then send more data */
    if (trace_length) bfscope_trace_send(tpcb);

    return ERR_OK;
}


static void bfscope_trace_complete(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    int len;

    bfscope_trace_acquired = true;

    /* Re-enable tracing if necessary */
    if (disp->trace_buf == NULL &&
        bfscope_save_tracebuf != NULL) disp->trace_buf = bfscope_save_tracebuf;

    if (bfscope_client == NULL) return;


    /* Format the trace into global trace buffer */
    trace_length = trace_dump(trace_buf, BFSCOPE_BUFLEN);

    DEBUG("bfscope: trace length %lu\n", trace_length);

    /* Send length field */
    char tmpbuf[10];
    len = snprintf(tmpbuf, 9, "%08ld", trace_length);
    tcp_write(bfscope_client, tmpbuf, 8, TCP_WRITE_FLAG_COPY);

    /* Start to send the trace */
    timestamp_start = rdtsc();
    trace_sent = 0;

    bfscope_trace_send(bfscope_client);

    tcp_output(bfscope_client);
}

static void ipi_handler(void *arg)
{
    sys_print("!", 1);
    DEBUG("bfscope_ipi\n");
#if 0
    // consume IDC message
    struct idc_recv_msg msg;
    idc_endpoint_poll(arg, &msg, NULL);
#endif

    // Handle the trace completion
    bfscope_trace_complete();
}

/** 
 * \brief Wait for a trace completion IPI 
 */
static errval_t bfscope_trace_wait_ipi(void)
{
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
    struct trace_buffer *buf = disp->trace_buf;
    if (buf == NULL) return TRACE_ERR_NO_BUFFER;

    ((struct trace_buffer *)trace_buffer_master)->ipi_dest = disp_get_core_id();

    /* XXX Temporarily disable tracing for this process */
    bfscope_save_tracebuf = buf;
    disp->trace_buf = NULL;

    return SYS_ERR_OK;
}

/*
 * \brief Take a real trace and dump it ito the trace_buf
 */
static void trace_acquire(struct tcp_pcb *tpcb, 
                          uint64_t start_trigger, 
                          uint64_t stop_trigger)
{
    errval_t err;

    if (trace_buf == NULL) {
        trace_buf = malloc(BFSCOPE_BUFLEN);
    }
    assert(trace_buf);
    
    trace_reset_all();

    bfscope_trace_acquired = false;
    
    err = trace_control(start_trigger, stop_trigger, 10 * 30 * 2000000);

    err = bfscope_trace_wait_ipi();
}


/*
 * \brief Callback from LWIP when we receive TCP data 
 */
static err_t recv_cb(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                     err_t err)
{
    if (p == NULL) {
        // close the connection
        bfscope_connection_close(tpcb);
        return ERR_OK;
    }

    /* don't send an immediate ack here, do it later with the data */
    tpcb->flags |= TF_ACK_DELAY;
    
    assert(p->next == 0);

    if ((p->tot_len > 2) && (p->tot_len < 200)) {
        if (strncmp(p->payload, "stat", 4) == 0) {
            idc_print_statistics();
        }
        if (strncmp(p->payload, "cardinfo", 8) == 0) {
            idc_print_cardinfo();
        }
        if (strncmp(p->payload, "trace", strlen("trace")) == 0) {

            DEBUG("bfscope: trace request\n");
            
            if (trace_length == 0) {
                sys_print("T",1);
                trace_acquire((struct tcp_pcb *)arg,
                              TRACE_EVENT(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 1),
                              TRACE_EVENT(TRACE_SUBSYS_BENCH, TRACE_EVENT_PCBENCH, 0));
           } else {
                printf("trace already in progress\n");
                //sys_print("X",1);
                tcp_write(tpcb, "000000", 6, TCP_WRITE_FLAG_COPY);
            }
        }
    }

    /* Done with the incoming data */
    tcp_recved(tpcb, p->len);
    pbuf_free(p);

    return ERR_OK;
}

/*
 * \brief Callback from LWIP when a client connects to our TCP listen sock
 */
static err_t accept_cb(void *arg, struct tcp_pcb *tpcb, err_t err)
{
    printf("bfscope: connected\n");
    assert(err == ERR_OK);
    tcp_recv(tpcb, recv_cb);
    tcp_sent(tpcb, send_cb);
    tcp_err(tpcb, error_cb);
    tcp_arg(tpcb, (void*)tpcb);
    tcp_accepted(tpcb);

    bfscope_client = tpcb;

    return ERR_OK;
}

/*
 * \brief Start listening on the bfscope server port
 */
static int bfscope_server_init(void)
{
    err_t r;

    uint16_t bind_port = BFSCOPE_TCP_PORT;

    struct tcp_pcb *pcb = tcp_new();
    if (pcb == NULL) {
        return ERR_MEM;
    }

    r = tcp_bind(pcb, IP_ADDR_ANY, bind_port);
    if(r != ERR_OK) {
        return(r);
    }

    struct tcp_pcb *pcb2 = tcp_listen(pcb);
    assert(pcb2 != NULL);
    tcp_accept(pcb2, accept_cb);

    printf("bfscope: listening on port %d\n", BFSCOPE_TCP_PORT);

    return (0);
}

/*
 * \brief Bring up the LWIP stack and start DHCP..
 */
static void startlwip(char *card_name)
{
    lwip_init(card_name);

    int r = bfscope_server_init();
    assert(r == 0);
}



//******************************************************************************
// irq handling stuff
//******************************************************************************
static struct lmp_endpoint *idcep;

static void generic_interrupt_handler(void *arg)
{
    errval_t err;

    // consume message
    struct lmp_recv_buf buf = { .buflen = 0 };
    err = lmp_endpoint_recv(idcep, &buf, NULL);
    assert(err_is_ok(err));

    ipi_handler(NULL);

    // re-register
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = arg,
    };
    err = lmp_endpoint_register(idcep, get_default_waitset(), cl);
    assert(err_is_ok(err));
}

static errval_t register_interrupt(int irqvector)
{
    struct capref epcap;
    errval_t err;

    // use minimum-sized endpoint, because we don't need to buffer >1 interrupt
    err = endpoint_create(LMP_RECV_LENGTH, &epcap, &idcep);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_irqtable_set(cap_irq, irqvector, epcap);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_CAP_NOT_FOUND) {
            printf("bfscope: Error registering IRQ, check that bfscope " \
                   "is spawned by the monitor by having 'boot' as its " \
                   "first argument\n");
        } else {
            DEBUG_ERR(err, "bfscope, irq set");
        }
        return err;
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = generic_interrupt_handler,
        .arg = NULL,
    };
    err = lmp_endpoint_register(idcep, get_default_waitset(), cl);
    if (err_is_fail(err)) {
        lmp_endpoint_free(idcep);
        // TODO: release vector
        return err;
    }
    return SYS_ERR_OK;
}
//******************************************************************************



int main(int argc, char**argv)
{

#ifndef CONFIG_TRACE
    // bail - no tracing support
    printf("%.*s: Error, no tracing support, cannot start bfscope\n",
           DISP_NAME_LEN, disp_name());
    printf("%.*s: recompile with trace = TRUE in build/hake/Config.hs\n",
           DISP_NAME_LEN, disp_name());
    return -1;
#endif

    printf("%.*s running on core %d\n", DISP_NAME_LEN, disp_name(),
           disp_get_core_id());

    // Register IRQ
    errval_t err = register_interrupt(TRACE_COMPLETE_IPI_IRQ);
    assert(err_is_ok(err)); //XXX
    printf("Registered IRQ\n");

    /* Connect to e1000 driver */
    printf("%.*s: trying to connect to the e1000 driver...\n",
           DISP_NAME_LEN, disp_name());

    startlwip("e1000");

    network_polling_loop();

    return 0;
}

