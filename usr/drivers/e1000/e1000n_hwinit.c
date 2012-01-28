/**
 * \file
 * \brief Intel e1000 driver: Initialize the hardware
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "e1000_dev.h"
#include "e1000n_desc.h"
#include "e1000n.h"

#include "e1000n_debug.h"

#define TIPG_VALUE 0x00702008



/*****************************************************************
* initialize the card
 *****************************************************************/

void e1000_hwinit(e1000_t *d, struct device_mem *bar_info,
                  int nr_allocated_bars,
                  volatile struct tx_desc **transmit_ring,
                  volatile union rx_desc **receive_ring,
                  int receive_buffers, int transmit_buffers,
                  uint8_t *macaddr, bool user_macaddr, bool use_interrupt)
{
    struct capref frame;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    errval_t r;

    E1000N_DEBUG("e1000: initializing network device\n");

    //we need at least the register mapping (the first BAR)
    assert(nr_allocated_bars >= 1);
    E1000N_DEBUG("e1000: Mapping BAR[0]...\n");
    map_device(&bar_info[0]);
    E1000N_DEBUG("e1000: BAR[0] mapped.\n");

    e1000_initialize(d, (void *)(bar_info[0].vaddr));
    E1000N_DEBUG("e1000: accessing conf regs starting at %p, %p\n", (*d).base,
            (void *)(bar_info[0].vaddr));
    E1000N_DEBUG("e1000: physical address of conf regs: %p\n",
            (void *)(bar_info[0].paddr));

    //clear interrupts
    e1000_imc_wr_raw(d, 0xffffffff);

    //disable receiver and transmitter
    e1000_rctl_wr_raw(d, 0);
    e1000_tctl_wr_raw(d, 0);

    //global reset
    // XXX should e1000_ctl_rst_wr(&d,1);
    {
        e1000_ctrl_t c = e1000_ctrl_rd(d);
        c.rst = 1;
        e1000_ctrl_wr(d, c);
    }

    //clear interrupts again
    e1000_imc_wr_raw(d, 0xffffffff);

    //deassert PHY_RESET
    {
        e1000_ctrl_t c = e1000_ctrl_rd(d);
        c.phy_rst = 0;
        e1000_ctrl_wr(d, c);
        e1000_status_t s = e1000_status_rd(d);
        s.phyra = 0;
        e1000_status_wr(d, s);
    }

    //set up link between MAC and PHY
    {
        e1000_ctrl_t c = e1000_ctrl_rd(d);
        c.slu = 1;
        e1000_ctrl_wr(d, c);
    }

    //set phy mode (instead of SerDes)
    {
        e1000_ctrlext_t c = e1000_ctrlext_rd(d);
        c.link_mode = e1000_glci;
        e1000_ctrlext_wr(d, c);
    }

    //set full-duplex
    {
        e1000_ctrl_t c = e1000_ctrl_rd(d);
        c.fd = 1;
        e1000_ctrl_wr(d, c);
    }

    //set SPEED in CTRL according to SPEED in STATUS
    {
        e1000_ctrl_t c = e1000_ctrl_rd(d);
        c.speed = e1000_status_rd(d).speed;
        e1000_ctrl_wr(d, c);
    }

    e1000_fcal_wr(d, 0);
    e1000_fcah_wr(d, 0);
    e1000_fct_wr(d, 0);


    //init statistic counters
    for (int i = 0; i < e1000_statsregs_length; i++) {
        e1000_statsregs_rd(d, i);
    }

    /* --------------------- MAC address setup --------------------- */

    // is a valid MAC already present?
    bool mac_present = e1000_rah_rd(d, 0).av;

    if (user_macaddr || !mac_present) {
        uint16_t mac_word0, mac_word1, mac_word2;
        if (user_macaddr) {
            mac_word0 = (((uint16_t)macaddr[1]) << 16) | macaddr[0];
            mac_word1 = (((uint16_t)macaddr[3]) << 16) | macaddr[2];
            mac_word2 = (((uint16_t)macaddr[5]) << 16) | macaddr[4];
        } else {
            // read MAC from EEPROM
            mac_word0 = read_eeprom(d, 0);
            mac_word1 = read_eeprom(d, 1);
            mac_word2 = read_eeprom(d, 2);

            // test LAN ID to see if we need to modify the MAC from EEPROM
            e1000_status_t s = e1000_status_rd(d);
            if (s.lan_id == e1000_lan_b) {
                mac_word2 ^= e1000_lan_b_mask;
            }
        }

        // program card's address with MAC
        e1000_rah_wr(d, 0, (e1000_rah_t){ .av = 0 });
        e1000_ral_wr(d, 0, ( mac_word0 | ((mac_word1)<<16)));
        e1000_rah_wr(d, 0, (e1000_rah_t){ .rah = mac_word2, .av = 1 });
    }

    // cache MAC for stack to see
    uint64_t machi = e1000_rah_rd(d, 0).rah;
    uint64_t mac = e1000_ral_rd(d, 0) + (machi << 32);
    macaddr[0] = mac & 0xff;
    macaddr[1] = (mac >> 8) & 0xff;
    macaddr[2] = (mac >> 16) & 0xff;
    macaddr[3] = (mac >> 24) & 0xff;
    macaddr[4] = (mac >> 32) & 0xff;
    macaddr[5] = (mac >> 40) & 0xff;

    E1000N_DEBUG("e1000: my MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
           macaddr[0], macaddr[1], macaddr[2], macaddr[3],
           macaddr[4], macaddr[5]);

    //clear all other filers (clear high-to-low (13.4.3))
    for (int i = 1; i < e1000_ral_length; i++) {
        e1000_rah_wr(d, i, (e1000_rah_t){ .av=0 });
        e1000_ral_wr(d, i, 0);
    }

    //clear MTA table
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(d, i, 0);
    }


    /* --------------------- receive setup --------------------- */
    //receive descriptor control
    e1000_rxdctl_wr(d, 0, (e1000_rxdctl_t){ .gran=1, .wthresh=1 } );
    {
        e1000_rfctl_t c = e1000_rfctl_rd(d);
        c.exsten = 0;
        e1000_rfctl_wr(d, c);
    }


    // Allocate and map frame for receive ring
    *receive_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                              sizeof(union rx_desc) * receive_buffers, &frame);
    assert(*receive_ring != NULL);
    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));

    // tell card where receive ring is
    e1000_rdbal_wr(d, 0, frameid.base & 0xffffffff);
    e1000_rdbah_wr(d, 0, (frameid.base >> 32) & 0xffffffff);

    // Len is in fact the number of descriptors divided by 8 (see .dev
    // file and data sheet
    // So equiv to:
    //     e1000_rdlen_wr_raw(&d, 0, recieve_buffers * sizeof(rx_desc));
    e1000_rdlen_wr(d, 0, (e1000_dqlen_t){ .len = (receive_buffers / 8) });

    // initialise receive head and tail pointers
    e1000_rdh_wr(d, 0, (e1000_dqval_t){.val=0});
    e1000_rdt_wr(d, 0, (e1000_dqval_t){.val=0});

    //enable receive unit
    {
        e1000_rctl_t c = e1000_rctl_rd(d);
        c.en = 1;
        c.bam = 1;
        e1000_rctl_wr(d, c);
    }


    /* --------------------- transmit setup --------------------- */
    e1000_txdctl_wr(d, 0, (e1000_txdctl_t){ .gran=1, .wthresh=0 });

    // alloc and map frame for transmit ring
    *transmit_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                              sizeof(struct tx_desc) * transmit_buffers, &frame);
    assert(*transmit_ring != NULL);

    // tell card about our tx ring
    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));
    e1000_tdbal_wr(d, 0, frameid.base & 0xffffffff);
    e1000_tdbah_wr(d, 0, frameid.base >> 32);
    e1000_tdlen_wr(d, 0, (e1000_dqlen_t){ .len = (transmit_buffers / 8) });
    e1000_tdh_wr(d, 0, (e1000_dqval_t){.val=0});
    e1000_tdt_wr(d, 0,  (e1000_dqval_t){.val=0});

    //there is a recommended value (see 13.3.59)
    e1000_tipg_wr_raw(d, TIPG_VALUE);

    // enable transmit
    e1000_tctl_wr(d, (e1000_tctl_t){ .ct=1, .cold=0xf, .psp=1, .en=1 });


    /* ------------------------- enable interrupts ------------------------ */
    /* Commenting interrupt throttling rate as it improves the latency
     * performance by around 50 times. -- PS */

    //enable interrupt throttling rate
    {
        /*
        e1000_itr_t c = e1000_itr_rd(d);
        c.interval = 0xff;
        e1000_itr_wr(d, c);
        */
    }

    if (use_interrupt) {
        //enable interrupts
        //    memory[IMS] = RXT | RXO | RXDMT | RXSEQ | LSC;
        e1000_ims_wr(d, (e1000_intreg_t) {
                .rxt0 = 1,
            });
    }
}



