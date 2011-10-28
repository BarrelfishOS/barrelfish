/** \file
 * \brief SKB server prototypes
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef SKB_SERVER_H_
#define SKB_SERVER_H_

#define POST_EXECUTE 1

void skb_server_init(void);
void post_and_execute_string(void);

void test_function_call_ez(void);
void test_function_call(char* what);
int p_string_to_list(void);

#endif
