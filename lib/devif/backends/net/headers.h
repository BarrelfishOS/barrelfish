/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef HEADERS_IMPL_H
#define HEADERS_IMPL_H

#define ETH_HWADDR_LEN     6

#define PACK_STRUCT_FIELD(x) x
#define PACK_STRUCT_STRUCT __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END

PACK_STRUCT_BEGIN
/** Ethernet header */
struct eth_hdr {
  PACK_STRUCT_FIELD(struct eth_addr dest);
  PACK_STRUCT_FIELD(struct eth_addr src);
  PACK_STRUCT_FIELD(uint16_t type);
} PACK_STRUCT_STRUCT;
PACK_STRUCT_END

#define ETH_HLEN 14

#define ETHTYPE_IP        0x0800U

PACK_STRUCT_BEGIN
struct ip_hdr {
  /* version / header length */
  PACK_STRUCT_FIELD(uint8_t _v_hl);
  /* type of service */
  PACK_STRUCT_FIELD(uint8_t _tos);
  /* total length */
  PACK_STRUCT_FIELD(uint16_t _len);
  /* identification */
  PACK_STRUCT_FIELD(uint16_t _id);
  /* fragment offset field */
  PACK_STRUCT_FIELD(uint16_t _offset);
#define IP_RF 0x8000U        /* reserved fragment flag */
#define IP_DF 0x4000U        /* dont fragment flag */
#define IP_MF 0x2000U        /* more fragments flag */
#define IP_OFFMASK 0x1fffU   /* mask for fragmenting bits */
  /* time to live */
  PACK_STRUCT_FIELD(uint8_t _ttl);
  /* protocol*/
  PACK_STRUCT_FIELD(uint8_t _proto);
  /* checksum */
  PACK_STRUCT_FIELD(uint16_t _chksum);
  /* source and destination IP addresses */
  PACK_STRUCT_FIELD(uint32_t src);
  PACK_STRUCT_FIELD(uint32_t dest); 
} PACK_STRUCT_STRUCT;
PACK_STRUCT_END

#define IPH_V(hdr)  ((hdr)->_v_hl >> 4)
#define IPH_HL(hdr) ((hdr)->_v_hl & 0x0f)
#define IPH_TOS(hdr) ((hdr)->_tos)
#define IPH_LEN(hdr) ((hdr)->_len)
#define IPH_ID(hdr) ((hdr)->_id)
#define IPH_OFFSET(hdr) ((hdr)->_offset)
#define IPH_TTL(hdr) ((hdr)->_ttl)
#define IPH_PROTO(hdr) ((hdr)->_proto)
#define IPH_CHKSUM(hdr) ((hdr)->_chksum)

#define IPH_VHL_SET(hdr, v, hl) (hdr)->_v_hl = (((v) << 4) | (hl))
#define IPH_TOS_SET(hdr, tos) (hdr)->_tos = (tos)
#define IPH_LEN_SET(hdr, len) (hdr)->_len = (len)
#define IPH_ID_SET(hdr, id) (hdr)->_id = (id)
#define IPH_OFFSET_SET(hdr, off) (hdr)->_offset = (off)
#define IPH_TTL_SET(hdr, ttl) (hdr)->_ttl = (uint8_t)(ttl)
#define IPH_PROTO_SET(hdr, proto) (hdr)->_proto = (uint8_t)(proto)
#define IPH_CHKSUM_SET(hdr, chksum) (hdr)->_chksum = (chksum)

#define IP_HLEN 20

#define IP_PROTO_IP      0
#define IP_PROTO_UDP     17

#define UDP_HLEN 8

PACK_STRUCT_BEGIN
struct udp_hdr {
  PACK_STRUCT_FIELD(uint16_t src);
  PACK_STRUCT_FIELD(uint16_t dest);  /* src/dest UDP ports */
  PACK_STRUCT_FIELD(uint16_t len);
  PACK_STRUCT_FIELD(uint16_t chksum);
} PACK_STRUCT_STRUCT;
PACK_STRUCT_END

#endif
