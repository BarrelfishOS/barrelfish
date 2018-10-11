#ifndef HPET_INT_CTRL_H
#define HPET_INT_CTRL_H

#include <barrelfish/caddr.h>
#include <errors/errno.h>
#include <hpet.h>
#include <sys/cdefs.h>

errval_t init_hpet_int_controller(struct hpet_driver_state *ds, lvaddr_t vaddr);

#endif
