/** \file
 *  \brief IDC system test code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



extern void bb_test_start_client(void);
extern void bb_test_start_server(void);

extern void bb_pingpong_start_server(void);
extern void bb_pingpong_start_client(void);

extern void icachetest_setup(void);
extern cycles_t icachetest_run(int count);
