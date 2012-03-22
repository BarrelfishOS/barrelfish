/**
 * \file
 * \brief Intel e1000 driver: Debug functionality
 *
 * This file is a driver for the PCI Express e1000 card
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "e1000n.h"
#include <net_queue_manager/net_queue_manager.h>
extern e1000_t d;

/*
 * Handy-dandy shorthand for printing registers: make sure you have a
 * suitable buffer declared.
 */
#define PR_REG(t) e1000_##t##_pr(pb, PRTBUF_SZ, &d); printf("%s\n",pb)
#define PR_REGZ(t) e1000_##t##_pri(pb, PRTBUF_SZ, &d, 0); printf("%s\n",pb)
#define PRTBUF_SZ 4000


void print_statistics(void)
{
    char pb[PRTBUF_SZ];

    PR_REG(status);

    // Transmit registers
    PR_REGZ(tdbal);
    PR_REGZ(tdbah);
    PR_REGZ(tdlen);
    PR_REG(tctl);
    PR_REGZ(txdctl);
    PR_REGZ(tdh);
    PR_REGZ(tdt);

    // Receive registers
    PR_REGZ(rdbal);
    PR_REGZ(rdbah);
    PR_REGZ(rdh);
    PR_REGZ(rdt);

    // Statistics registers
    PR_REG(crcerrs);
    PR_REG(algnerrc);
    PR_REG(symerrs);
    PR_REG(rxerrc);
    PR_REG(mpc);
    PR_REG(scc);
    PR_REG(ecol);
    PR_REG(mcc);
    PR_REG(latecol);
    PR_REG(colc);
    PR_REG(dc);
    PR_REG(tncrs);
    PR_REG(sec);
    PR_REG(cexterr);
    PR_REG(rlec);
    PR_REG(xonrxc);
    PR_REG(xontxc);
    PR_REG(xoffrxc);
    PR_REG(xofftxc);
    PR_REG(fcurc);
    PR_REG(prc64);
    PR_REG(prc127);
    PR_REG(prc255);
    PR_REG(prc511);
    PR_REG(prc1023);
    PR_REG(prc1522);
    PR_REG(gprc);
    PR_REG(bprc);
    PR_REG(mcprc);
    PR_REG(gptc);
    PR_REG(gorcl);
    PR_REG(gorch);
    PR_REG(gotcl);
    PR_REG(gotch);
    PR_REG(rnbc);
    PR_REG(ruc);
    PR_REG(rfc);
    PR_REG(roc);
    PR_REG(rjc);
    PR_REG(mprc);
    PR_REG(mpdc);
    PR_REG(mptc);
    PR_REG(torl);
    PR_REG(torh);
    PR_REG(totl);
    PR_REG(toth);
    PR_REG(tpr);
    PR_REG(tpt);
    PR_REG(ptc64);
    PR_REG(ptc127);
    PR_REG(ptc255);
    PR_REG(ptc511);
    PR_REG(ptc1023);
    PR_REG(ptc1522);
    PR_REG(mcptc);
    PR_REG(bptc);
    PR_REG(tsctc);
    PR_REG(tsctfc);
    PR_REG(iac);
    PR_REG(icrxptc);
    PR_REG(icrxatc);
    PR_REG(ictxptc);
    PR_REG(ictxatc);
    PR_REG(ictxqec);
    PR_REG(ictxdltc);
    PR_REG(icrxdmtc);
    PR_REG(icrxoc);
}

