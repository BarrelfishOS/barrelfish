/**
 * \file
 * \brief the main header file for the net "daemon"
 *
 * This file is part of the net "daemon"
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: NetOS Group.
 */

#ifndef _NETD_H_
#define _NETD_H_

struct netd_state;

/**
 * @brief initializes LWIP. not a lot left after I changed the subsystems
 *
 * @param card_name the of the card.
 */

errval_t netd_init(struct netd_state **state, char *card_name, uint64_t queueid,
    bool do_dhcp, char *ip_addr_str, char *netmask_str, char *gateway_str);

#endif // _NETD_H_
