#ifndef HPET_INT_CTRL_H
#define HPET_INT_CTRL_H

#include <barrelfish/caddr.h>
#include <errors/errno.h>
#include <hpet.h>
#include <hpet_comp.h>
#include <sys/cdefs.h>

errval_t init_hpet_int_controller(struct hpet_comp_st *ds, const char *lbl);

#endif
