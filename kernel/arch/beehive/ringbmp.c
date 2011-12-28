/**
 * \file
 * \brief Beehive message transport kernel component
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <hyper.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <capabilities.h> // cte
#include <dispatch.h> // dcb_current
#include <barrelfish_kpi/lmp.h> // LM_RECV_HEADER_LENGTH

#include <trace/trace.h>

#include "bmp.h"

/*
 * Understanding this kernel component transport: this is a connection
 * oriented word transport with credit based flow control between
 * cores.
 *
 * Message format: messages have a header word with two fields; one
 * contains the credit being sent, the other the channel number.
 *
 * Receive: the channel number identifies a state record (if invalid
 * the data is discarded) which describes the area of memory for
 * receipt of the words (base and size) which is used as a circular
 * buffer.  A Next index and a Limit index control the use of the
 * circular buffer.  How the Next and Limit indices are communicated
 * to the user mode process is TBD.
 *
 * Credits: there are two arrays of credits, one for transmission and
 * one for outgoing.  When a message arrives the incoming credit count
 * is added to the transmission credit for the remote core.  The size
 * of the message, including the hardware header word, is added to the
 * outgoing credit count for the remote core.
 *
 * Processing: when polling (e.g. from the idle loop) messages are
 * read and delivered until the incoming fifo is empty.  Then any
 * outgoing credit values greater than 2 are sent to the respective
 * cores.  Values <= 2 are not sent to avoid ACKing ACKs.
 *
 * Transmit: a message of n words (including headers) can be sent if
 * the amount of credit available is greater than or equal to n + 2.
 * The extra 2 is required to ensure that an ACK can be sent at a
 * later date without having to wait for one.
 *
 * MTU: The hardware supports 64 words, i.e. 62 plus our header plus
 * HW header, but this is also the flow control limit.  Since we might
 * have sent an ACK already which is not acked a common value of
 * credit is 62.  Since we have to keep enough credit to be able to
 * send an ACK subsequently the maximum sized message is 60 words,
 * i.e. 58 words of payload.
 */

#define MAX_PAYLOAD 58

#define BEEHIVE_RING_TYPE_BMP 2

#define NUM_ASSOCS 32

#define BEEHIVE_CORES 16

static int out_credits[BEEHIVE_CORES];
static int tx_credits[BEEHIVE_CORES];

static struct cte bmp_dispatch[NUM_ASSOCS];

void bmp_init(void)
{
    for(int i=0; i<BEEHIVE_CORES; i++) {
	out_credits[i] = 0;
	tx_credits[i] = 64;
    }
}


/**
 * \brief Deliver a BMP message to a dispatcher.
 *
 * \param ep     Endpoint capability to send to
 * \param len    Number of words to be transferred
 * \param msg    Message to be sent
 *
 * \return Error code
 */
errval_t bmp_deliver(struct capability *ep, size_t len,
                     uintptr_t *msg);

static inline void bmpack(int core, int credit);
static inline void bmptx(int core, int credit, int assoc, int size, uint32_t *ptr);

extern int message_recv(int *srce, int *type, uint32_t * buf);
extern void message_send(int dest, int type, uint32_t *buf, int len);
extern void message_send_with_header(int dest, int type, uint32_t *buf, int len, uint32_t header);

extern void halt_unless_timer_pending(void);


// Returns 0 if message queue was empty, 1 otherwise
static inline int bmp_process_one(void)
{
    static uint32_t buf[64];
    int core;
    int type;
    int len;

    for(;;) {
	len = message_recv(&core, &type, buf);
	if (len == 0)
	    return 0;

        if (core == 1) {
	    printf("bmpread1: message from hypervisor!\n");
	    continue;
        }
	trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_RX, len);
	//debug(SUBSYS_BMP, "bmpread1: len=%d, core=%d, type=%d\n", len, core, type);

	// Even if it is invalid message it counts for credit
	out_credits[core] += len + 1; // +1 for HW header word
	
	if (type != BEEHIVE_RING_TYPE_BMP) {
	    printf("bmpread1: bad type %d\n", type);
	    continue;
	}

	uint32_t header = buf[0];
	//debug(SUBSYS_BMP, "bmpread1: incoming credits: %u\n", header >> 16);
	tx_credits[core] += (header >> 16);
	// If no length then just an ack, no assoc
	if (len == 1)
	    continue;

	header &= 0xffff;
	if (header > NUM_ASSOCS) {
	    printf("bmpread1: bad assoc %d\n", header);
	    continue;
	}

	struct capability *cap = &bmp_dispatch[header].cap;
	if (cap->type == ObjType_Null) {
	    printf("bmpread1: unused assoc %d\n", header);
	    continue;
	}
	assert(cap->type == ObjType_EndPoint);
	// XXX Check incoming core?

	//debug(SUBSYS_BMP, "Delivering BMP message assoc %u len %u\n", header, len);
	trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_PRE_DELIVER, (uintptr_t)cap);
	errval_t err = bmp_deliver(cap, len -1, &buf[1]);
	trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_POST_DELIVER, (uintptr_t)cap);
	if (err_is_fail(err))
	    printf("bmpread1: bmp_deliver: error %u\n", err);
	return 1;
    }
} // bmpread1

static inline void bmpack(int core, int credit)
{
    uint32_t header = credit << 16;
    message_send_with_header(core, BEEHIVE_RING_TYPE_BMP, NULL, 0, header);
}

static inline void bmptx(int core, int credit, int assoc, int size, uint32_t *ptr)
{
    uint32_t header = (credit << 16) | assoc;
    message_send_with_header(core, BEEHIVE_RING_TYPE_BMP, ptr, size, header);
}


static inline void bmp_send_out_credits(void)
{
    for(int i=0; i<BEEHIVE_CORES; i++) {
	assert(out_credits[i] >= 0 && out_credits[i] <= 64);
	if (out_credits[i] > 2 && tx_credits[i] >= 2) {
	    bmpack(i, out_credits[i]);
	    out_credits[i] = 0;
	    tx_credits[i] -= 2;
	}
    }
}

void bmp_pump(void)
{
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_PUMP, 1);
    while (bmp_process_one() != 0);
    bmp_send_out_credits();
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_PUMP, 0);
}

// This returns when the messenger is empty and either a message
// arrived at some point, or a timer interrupt is pending.
void bmp_pump_until_timer(struct corearea *corearea)
{
    int something;
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_PUMP, 2);
    do {
	halt_unless_timer_pending();
	something = bmp_process_one();
	if (something) {
	    while (bmp_process_one() != 0);
	    bmp_send_out_credits();
	}
    } while(something == 0 && corearea->kernel_pending == 0);
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_PUMP, 0);
}

void bmp_send(int core, int assoc, int size, uint32_t *words)
{
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_SEND, size);

    assert(core > 1 && core < BEEHIVE_CORES);
    assert(assoc >= 0 && assoc < NUM_ASSOCS);
    assert(size > 0 && size <= MAX_PAYLOAD);
    assert(words != 0);

    //debug(SUBSYS_BMP, "bmp_send: core=%u assoc=%u size=%u tx_credits=%u\n",
    //  core, assoc, size, tx_credits[core]);

    if (tx_credits[core] >= size + 2 + 2)
	goto sendit;

    for(;;) {
	int idle = bmp_process_one();
	if (tx_credits[core] >= size + 2 + 2)
	    goto sendit;
	if (idle)
	    bmp_send_out_credits();
    }

  sendit:
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_BMP_SEND, 0);
    //debug(SUBSYS_BMP, "bmp_send: sendit\n");
    // Have enough to send it already
    bmptx(core, out_credits[core], assoc, size, words);
    tx_credits[core] -= size + 2;
    out_credits[core] = 0;
    
    bmp_pump();
}

/* --------------------------------------------------------------------- */
/* And now for the capability code which manages the ability to send and
 * receive on bmp associations
 */

errval_t bmp_deliver(struct capability *ep, size_t len,
                     uintptr_t *msg)
{
    assert(ep != NULL);
    assert(ep->type == ObjType_EndPoint);
    assert(msg != NULL);
    assert(len > 0 && len <= MAX_PAYLOAD);

    return lmp_deliver_payload(ep, NULL, msg, len, false);
}


struct sysret bmp_table_set(struct capability *to, struct idc_recv_msg *msg)
{
    errval_t err;
    unsigned int associd;
    capaddr_t cptr;
    struct cte *recv;

    associd = idc_msg_decode_word_or_zero(msg);
    cptr    = (capaddr_t)idc_msg_decode_word_or_zero(msg);

    err = caps_lookup_slot(&dcb_current->cspace.cap, cptr,
                           CPTR_BITS, &recv, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_BMP_LOOKUP));
    }

    assert(to->type == ObjType_BMPTable);
    assert(recv != NULL);

    // Return w/error if cap is not an endpoint
    if(recv->cap.type != ObjType_EndPoint) {
        return SYSRET(SYS_ERR_BMP_NOT_ENDPOINT);
    }

    // Return w/error if no listener on endpoint
    if(recv->cap.u.endpoint.listener == NULL) {
        return SYSRET(SYS_ERR_BMP_NO_LISTENER);
    }

    if(associd >= NUM_ASSOCS) {
	return SYSRET(SYS_ERR_BMP_INVALID);
    }

    // check that we don't overwrite someone else's handler
    if (bmp_dispatch[associd].cap.type != ObjType_Null) {
	printf("kernel: installing new handler for BMP associd %u\n", associd);
	// XXX TODO: Then what?
    }
    err = caps_copy_to_cte(&bmp_dispatch[associd], recv, false, 0, 0);
    return SYSRET(err);
}

struct sysret bmp_table_delete(struct capability *to, struct idc_recv_msg *msg)
{
    assert(to->type == ObjType_IRQTable);

    unsigned int associd = idc_msg_decode_word_or_zero(msg);

    if (associd >= NUM_ASSOCS) {
	return SYSRET(SYS_ERR_BMP_INVALID);
    }

    bmp_dispatch[associd].cap.type = ObjType_Null;
    return SYSRET(SYS_ERR_OK);
}

/* End */
