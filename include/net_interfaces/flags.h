/*
 * Copyright (c) 2007-2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NET_INTERFACES_FLAGS_H_
#define NET_INTERFACES_FLAGS_H_

#define NETIF_TXFLAG_IPCHECKSUM (1UL << 0)
#define NETIF_TXFLAG_TCPCHECKSUM (1UL << 1)
#define NETIF_TXFLAG_UDPCHECKSUM (1UL << 2)
#define NETIF_TXFLAG_TCPHDRLEN_SHIFT (3UL)
#define NETIF_TXFLAG_TCPHDRLEN_MASK (0xfUL << NETIF_TXFLAG_TCPHDRLEN_SHIFT)

#define NETIF_TXFLAG (1UL << 31)
#define NETIF_TXFLAG_LAST (1UL << 30)
#define NETIF_TXFLAG_FIRST (1UL << 29)


#define NETIF_RXFLAG (1UL << 28)
#define NETIF_RXFLAG_TYPE_IPV4 (1UL << 0)
#define NETIF_RXFLAG_TYPE_UDP (1UL << 1)
#define NETIF_RXFLAG_TYPE_TCP (1UL << 2)

#define NETIF_RXFLAG_IPCHECKSUM (1UL << 3)
#define NETIF_RXFLAG_IPCHECKSUM_GOOD (1UL << 4)
#define NETIF_RXFLAG_L4CHECKSUM (1UL << 5)
#define NETIF_RXFLAG_L4CHECKSUM_GOOD (1UL << 6)

#endif // ndef NET_INTERFACES_FLAGS_H_

