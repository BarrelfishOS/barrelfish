/**
 * \file
 * \brief Arch specific LMP declarations
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_KPI_LMP_H
#define ARCH_BEEHIVE_BARRELFISH_KPI_LMP_H

#include <barrelfish_kpi/legacy_idc_buffer.h>

/**
 * \brief Maximum total length of LMP and LRPC messages (payload)
 *
 * Determined by the size of idc_send_msg.  LMP is done by marshalling
 * into an struct idc_send_msg in lmp_chan_arch.h and then invoking
 * the endpoint capability passing the idc_send_msg.  This passes
 * through the syscal call to handle_invocation in the kernel and
 * thence lmp_deliver in the kernel.
 */

// Warning: this is also known to tools/flounder/Args.hs

#define LMP_MSG_LENGTH          IDC_MSG_LENGTH

// TODO: XXX Size of LRPC
#define LRPC_MSG_LENGTH         0

#endif // ARCH_BEEHIVE_BARRELFISH_KPI_LMP_H
