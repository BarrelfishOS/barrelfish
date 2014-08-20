/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MESSAGING_H_
#define XEON_PHI_MESSAGING_H_

///
#define XEON_PHI_MSG_SIZE

#define XEON_PHI_MSG_CHAN 63

enum {
  XEON_PHI_CHAN_TYPE_VIRTIO,
  XEON_PHI_CHAN_TYPE_UMP,
  XEON_PHI_CHAN_TYPE_OTHER
};

enum {
  XEON_PHI_CHAN_OWNER_SELF,
  XEON_PHI_CHAN_OWNER_OTHER
};

enum {

};

struct xeon_phi_msg_chan {
    lpaddr_t base;
    lvaddr_t vbase;
    uint64_t size;
    uint64_t _pad[5];
};

struct xeon_phi_msg_meta {
    uint8_t meta_changed;
    uint8_t chan_changed[XEON_PHI_MSG_CHAN];
    struct xeon_phi_msg_chan chan[XEON_PHI_MSG_CHAN];
};



#endif // XEON_PHI_MESSAGING_H_
