#ifndef HPET_INT_CTRL_H
#define HPET_INT_CTRL_H

#include <sys/cdefs.h>
#include <barrelfish/caddr.h>
#include <errors/errno.h>
#include <hpet.h>


errval_t init_hpet_int_controller(struct hpet_driver_state * ds  , lvaddr_t vaddr);


#endif