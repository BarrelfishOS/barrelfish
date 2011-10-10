/*
 * Copyright (c) 2001-2004 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */
#ifndef __LWIP_INIT_H__
#define __LWIP_INIT_H__

//#include <barrelfish/barrelfish.h>
#include "lwip/opt.h"
#include "lwip/err.h"


#ifdef __cplusplus
extern "C" {
#endif

/** X.x.x: Major version of the stack */
#define LWIP_VERSION_MAJOR      1U
/** x.X.x: Minor version of the stack */
#define LWIP_VERSION_MINOR      3U
/** x.x.X: Revision of the stack */
#define LWIP_VERSION_REVISION   1U
/** For release candidates, this is set to 1..254
  * For official releases, this is set to 255 (LWIP_RC_RELEASE)
  * For development versions (CVS), this is set to 0 (LWIP_RC_DEVELOPMENT) */
#define LWIP_VERSION_RC         255U

/** LWIP_VERSION_RC is set to LWIP_RC_RELEASE for official releases */
#define LWIP_RC_RELEASE         255U
/** LWIP_VERSION_RC is set to LWIP_RC_DEVELOPMENT for CVS versions */
#define LWIP_RC_DEVELOPMENT     0U

#define LWIP_VERSION_IS_RELEASE     (LWIP_VERSION_RC == LWIP_RC_RELEASE)
#define LWIP_VERSION_IS_DEVELOPMENT (LWIP_VERSION_RC == LWIP_RC_DEVELOPMENT)
#define LWIP_VERSION_IS_RC          ((LWIP_VERSION_RC != LWIP_RC_RELEASE) && (LWIP_VERSION_RC != LWIP_RC_DEVELOPMENT))

/** Provides the version of the stack */
#define LWIP_VERSION   (LWIP_VERSION_MAJOR << 24   | LWIP_VERSION_MINOR << 16 | \
                        LWIP_VERSION_REVISION << 8 | LWIP_VERSION_RC)

    enum netd_port_type_t;
    void perform_ownership_housekeeping(uint16_t(*alloc_tcp_ptr) (void),
                                        uint16_t(*alloc_udp_ptr) (void),
                                        uint16_t(*bind_port_ptr) (uint16_t,
                                                                  enum
                                                                  netd_port_type_t),
                                        void (*close_port_ptr) (uint16_t,
                                                                enum
                                                                netd_port_type_t));

/* Modules initialization */
    struct waitset;
    struct thread_mutex;
    void owner_lwip_init(char *card_name);
    bool lwip_init_ex(const char *card_name, struct waitset *opt_waitset,
                      struct thread_mutex *opt_mutex);
    bool lwip_init(const char *card_name);
    bool lwip_init_auto_ex(struct waitset *opt_waitset,
                           struct thread_mutex *opt_mutex);
    bool lwip_init_auto(void);

    void lwip_start_net_debug(int connection_type, uint8_t state, uint64_t trigger);
    int is_lwip_loaded(void);
    uint64_t lwip_packet_drop_count(void);


// For recording statistics
#define MACHINE_CLK_UNIT    (1000000)

#if !defined(__scc__)
#define MACHINE_CLOCK_SPEED  (2800)
#else
#define MACHINE_CLOCK_SPEED  (533)
#endif // !defined(__scc__)
#define IN_SECONDS(x)   (((x)/(MACHINE_CLOCK_SPEED))/(MACHINE_CLK_UNIT))

#define CONVERT_TO_SEC



//#ifdef CONVERT_TO_SEC
#define PU "f"
float in_seconds(uint64_t cycles);
//#else
#if 0
#define PU PRIu64
uint64_t in_seconds(uint64_t cycles);
#endif // CONVERT_TO_SEC


enum Recorded_Events {
    RE_ALL = 0,
    RE_REG_PBUF = 1,
    RE_PBUF_REPLACE = 2,
    RE_PBUF_REPLACE_1 = 3,
    RE_PBUF_REPLACE_2 = 4,
    RE_PBUF_QUEUE = 5,
    RE_PKT_RCV_CS = 6,
    RE_PBUF_REPLACE_3 = 7,
    TX_SP = 8,
    TX_SP1 = 9,
    TX_SPP_FULL,
    TX_SN_WAIT,
    TX_SN_SEND,
    TX_A_SP_RN_CS,
    TX_A_SP_RN_T,
};
#define EVENT_LIST_SIZE  20

enum Recorded_DataTypes {
    RDT_COUNT = 0,
    RDT_SUM = 1,
    RDT_MAX = 2,
    RDT_MIN = 3,
};
#define RDT_LIST_SIZE   4

void lwip_reset_stats(void);
void lwip_record_event(uint8_t event_type, uint64_t delta);
void lwip_record_event_simple(uint8_t event_type, uint64_t ts);
void lwip_print_event_stat(uint8_t event_type, char *event_name, int type);
void lwip_print_interesting_stats(void);
void lwip_record_event_no_ts(uint8_t event_type);
#ifdef __cplusplus
}
#endif

#endif                          /* __LWIP_INIT_H__ */
