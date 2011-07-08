/** \file
 * \brief e1000 internal structures
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E1000_INTERNALS_H_
#define E1000_INTERNALS_H_

struct tx_desc {
        uint64_t buffer_address;
        union {
            uint64_t raw;
            struct {
                uint16_t data_len;
                uint8_t cso;
                union {
                    uint8_t raw;
                    struct {
                        uint8_t eop : 1;
                        uint8_t ifcs : 1;
                        uint8_t ic : 1;
                        uint8_t rs : 1;
                        uint8_t rsv : 1;
                        uint8_t dext : 1;
                        uint8_t vle : 1;
                        uint8_t ide : 1;
                    } __attribute__ ((packed)) d;
                } __attribute__ ((packed)) cmd;
                union {
                    uint8_t raw;
                    struct {
                        uint8_t dd : 1;
                        uint8_t ec : 1;
                        uint8_t lc : 1;
                        uint8_t res : 5;
                    } __attribute__ ((packed)) d;
                } __attribute__ ((packed)) sta_rsv;
                uint8_t css;
                uint16_t special;
            } __attribute__ ((packed)) legacy;
            struct {
                uint64_t data_len : 20;
                uint64_t dtype : 4;
                union {
                    uint8_t raw;
                    struct {
                        uint8_t eop : 1;
                        uint8_t ifcs : 1;
                        uint8_t tse : 1;
                        uint8_t rs : 1;
                        uint8_t rsv : 1;
                        uint8_t dext : 1;
                        uint8_t vle : 1;
                        uint8_t ide : 1;
                    } __attribute__ ((packed)) d;
                } __attribute__ ((packed)) dcmd;
                union {
                    uint8_t raw;
                    struct {
                        uint8_t dd : 1;
                        uint8_t res : 7;
                    } __attribute__ ((packed)) d;
                } __attribute__ ((packed)) sta_rsv;
                union {
                    uint8_t  raw;
                    struct {
                        uint8_t ixsm : 1;
                        uint8_t txsm : 1;
                        uint8_t  res : 6;
                    } __attribute__ ((packed)) d;
                } __attribute__ ((packed)) popts;
                uint16_t vlan;
            } __attribute__ ((packed)) extended_tcpip;
        } __attribute__ ((packed)) ctrl;
} __attribute__ ((packed));

#if 0 /* what is this? */
    union {
        uint64_t raw;
        struct {
            uint8_t ipcss;
            uint8_t ipcso;
            uint16_t ipcse;
            uint8_t tucss;
            uint8_t tucso;
            uint16_t tucse;
            struct {
                uint32_t paylen : 20;
                uint32_t dtype : 4;
            } __attribute__ ((packed)) pd;
            union {
                uint8_t raw;
                struct {
                    uint8_t tcp : 1;
                    uint8_t ip : 1;
                    uint8_t tse : 1;
                    uint8_t rs : 1;
                    uint8_t rsv: 1;
                    uint8_t dext : 1;
                    uint8_t snap : 1;
                    uint8_t ide : 1;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) tucmd;
            union {
                uint8_t raw;
                struct {
                    uint8_t dd : 1;
                    uint8_t res : 7;
                } __attribute__ ((packed)) d;
            } __attribute__ ((packed)) sta_rsv;
            uint8_t hdrlen;
            uint16_t mss;
        } __attribute__ ((packed)) d;
    } __attribute__ ((packed)) context_desc;
#endif

union rx_desc {
    uint64_t raw[2] __attribute__ ((packed));
    struct {
        uint64_t buffer_address;
        struct {
            uint16_t   length;
            uint16_t   checksum;
            struct {
                unsigned int   dd      :1;
                unsigned int   eop     :1;
                unsigned int   ixsm    :1;
                unsigned int   vp      :1;
                unsigned int   udpcs   :1;
                unsigned int   tcpcs   :1;
                unsigned int   ipcs    :1;
                unsigned int   pif     :1;
            } __attribute__ ((packed)) status;
            uint8_t    errors;
            uint16_t   vlan_tag;
        } __attribute__ ((packed)) info;
    } __attribute__ ((packed)) rx_read_format;
    struct {
        // XXX: What the hell is this??
        uint32_t mrq;
        uint16_t ip_id;
        uint16_t checksum;
        union {
            uint32_t raw;
            struct {
                uint32_t dd : 1;
                uint32_t eop : 1;
                uint32_t ixsm : 1;
                uint32_t vp : 1;
                uint32_t udpcs : 1;
                uint32_t tcpcs : 1;
                uint32_t ipcs : 1;
                uint32_t pif : 1;
                uint32_t res : 1;
                uint32_t ipidv : 1;
                uint32_t udpv : 1;
                uint32_t res2 : 4;
                uint32_t ack : 1;
                uint32_t res3 : 4;
                uint32_t res4 : 4;
                uint32_t ce : 1;
                uint32_t se : 1;
                uint32_t seq : 1;
                uint32_t res5 : 1;
                uint32_t  res6 : 1;
                uint32_t tcpe : 1;
                uint32_t ipe : 1;
                uint32_t rxe : 1;
            } __attribute__ ((packed)) d;
        } __attribute__ ((packed)) status_error;
        uint16_t length;
        uint16_t vlantag;
    } __attribute__ ((packed)) rx_write_format;
} __attribute__ ((packed));

#endif // E1000_INTERNALS_H_
