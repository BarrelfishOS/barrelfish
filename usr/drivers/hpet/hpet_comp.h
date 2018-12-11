#ifndef HPET_COMP_H
#define HPET_COMP_H

#include <stdint.h>
#include <barrelfish/barrelfish.h>

struct hpet_comp_st {
    struct hpet_t *hpet_dev;
    int index;
};

errval_t hpet_comp_enable_fsb_int(struct hpet_comp_st *driver_state,
                                  uint32_t msg_addr, uint32_t msg_data);

errval_t hpet_comp_enable_ioapic_int(struct hpet_comp_st *driver_state,
                                     uint32_t gsi);
                                     

#endif
