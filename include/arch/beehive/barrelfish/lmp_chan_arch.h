/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_LMP_CHAN_H
#define ARCH_BEEHIVE_BARRELFISH_LMP_CHAN_H

#include <barrelfish/syscall_arch.h> // for cap_invoke
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/lmp.h>

// Sanity check
#if LMP_MSG_LENGTH != 9
#error "LMP_MSG_LENGTH != 9"
#endif

// all this code relies massively on the compilers post-inlining optimiser

/**
 * \brief Send a message on the given LMP channel, if possible
 *
 * Non-blocking, may fail if there is no space in the receiver's endpoint.
 *
 * \param ep Remote endpoint cap
 * \param flags LMP send flags
 * \param send_cap (Optional) capability to send with the message
 * \param length_words Length of the message in words; payload beyond this
 *                      size will not be delivered
 * \param arg1..N Message payload
 */
static inline errval_t lmp_ep_send(struct capref ep, lmp_send_flags_t flags,
				   struct capref send_cap, uint8_t length_words,
				   uint32_t a, uint32_t b, uint32_t c, uint32_t d,
				   uint32_t e, uint32_t f, uint32_t g, uint32_t h,
				   uint32_t i)
{
    struct idc_send_msg msg;
    
    idc_msg_init(&msg);
    if (length_words > 0) idc_msg_encode_word(&msg, a);
    if (length_words > 1) idc_msg_encode_word(&msg, b);
    if (length_words > 2) idc_msg_encode_word(&msg, c);
    if (length_words > 3) idc_msg_encode_word(&msg, d);
    if (length_words > 4) idc_msg_encode_word(&msg, e);
    if (length_words > 5) idc_msg_encode_word(&msg, f);
    if (length_words > 6) idc_msg_encode_word(&msg, g);
    if (length_words > 7) idc_msg_encode_word(&msg, h);
    if (length_words > 8) idc_msg_encode_word(&msg, i);

    msg.u.x.header.x.flags.sync = ((flags & LMP_FLAG_SYNC) != 0) ? 1 : 0;
    msg.u.x.header.x.flags.yield = ((flags & LMP_FLAG_YIELD) != 0) ? 1 : 0;

    if (!capref_is_null(send_cap)) {
	uint8_t send_bits = get_cap_valid_bits(send_cap);
	msg.u.x.header.x.send_bits = send_bits;
	msg.u.x.header.x.send_cptr = get_cap_addr(send_cap) >> (CPTR_BITS - send_bits);
    }

    return cap_invoke(ep, &msg).error;
}

// all this code relies massively on the compilers post-inlining optimiser

#define lmp_ep_send4(_ep, _flags, _send_cap, _a, _b, _c, _d) \
    lmp_ep_send((_ep), (_flags), (_send_cap), 4, (_a), (_b), (_c), (_d), 5, 6, 7, 8, 9)
#define lmp_ep_send3(_ep, _flags, _send_cap, _a, _b, _c) \
    lmp_ep_send((_ep), (_flags), (_send_cap), 3, (_a), (_b), (_c), 4, 5, 6, 7, 8, 9)
#define lmp_ep_send2(_ep, _flags, _send_cap, _a, _b) \
    lmp_ep_send((_ep), (_flags), (_send_cap), 2, (_a), (_b), 3, 4, 5, 6, 7, 8, 9)
#define lmp_ep_send1(_ep, _flags, _send_cap, _a) \
    lmp_ep_send((_ep), (_flags), (_send_cap), 1, (_a), 2, 3, 4, 5, 6, 7, 8, 9)
#define lmp_ep_send0(_ep, _flags, _send_cap) \
    lmp_ep_send((_ep), (_flags), (_send_cap), 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)


#define lmp_chan_send(_lc, _flags, _send_cap, _len, _a, _b, _c, _d, _e, _f, _g, _h, _i) \
  lmp_ep_send((_lc)->remote_cap, (_flags), (_send_cap), (_len), \
		(_a), (_b), (_c), (_d), (_e), (_f), (_g), (_h), (_i))

// all this code relies massively on the compilers post-inlining optimiser

#define lmp_chan_send9(_lc, _flags, _send_cap, _a, _b, _c, _d, _e, _f, _g, _h, _i) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 9, (_a), (_b), (_c), (_d), (_e), (_f), (_g), (_h), (_i))
#define lmp_chan_send8(_lc, _flags, _send_cap, _a, _b, _c, _d, _e, _f, _g, _h) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 8, (_a), (_b), (_c), (_d), (_e), (_f), (_g), (_h), 9)
#define lmp_chan_send7(_lc, _flags, _send_cap, _a, _b, _c, _d, _e, _f, _g) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 7, (_a), (_b), (_c), (_d), (_e), (_f), (_g), 8, 9)
#define lmp_chan_send6(_lc, _flags, _send_cap, _a, _b, _c, _d, _e, _f) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 6, (_a), (_b), (_c), (_d), (_e), (_f), 7, 8, 9)
#define lmp_chan_send5(_lc, _flags, _send_cap, _a, _b, _c, _d, _e) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 5, (_a), (_b), (_c), (_d), (_e), 6, 7, 8, 9)
#define lmp_chan_send4(_lc, _flags, _send_cap, _a, _b, _c, _d) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 4, (_a), (_b), (_c), (_d), 5, 6, 7, 8, 9)
#define lmp_chan_send3(_lc, _flags, _send_cap, _a, _b, _c) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 3, (_a), (_b), (_c), 4, 5, 6, 7, 8, 9)
#define lmp_chan_send2(_lc, _flags, _send_cap, _a, _b) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 2, (_a), (_b), 3, 4, 5, 6, 7, 8, 9)
#define lmp_chan_send1(_lc, _flags, _send_cap, _a) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 1, (_a), 2, 3, 4, 5, 6, 7, 8, 9)
#define lmp_chan_send0(_lc, _flags, _send_cap) \
    lmp_chan_send((_lc), (_flags), (_send_cap), 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#endif // ARCH_BEEHIVE_BARRELFISH_LMP_CHAN_H
