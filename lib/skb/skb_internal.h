/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SKB_INTERNAL_H_
#define SKB_INTERNAL_H_

#include <barrelfish/debug.h>
#include <skb/skb.h>

int skb_vsnprintf (char *str, size_t count, const char *fmt, va_list args);
int skb_sscanf(const char *ibuf, const char *fmt, ...);
int skb_vsscanf(const char *str, const char *format, va_list args);
int skb_snprintf(char *str, size_t count, const char *fmt, ...);


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(SKB_CLIENT_DEBUG) || defined(GLOBAL_DEBUG)
#define SKB_DEBUG(x...) printf("skb_client: " x)
#else
#define SKB_DEBUG(x...) ((void)0)
#endif

#endif // SKB_INTERNAL_H_
