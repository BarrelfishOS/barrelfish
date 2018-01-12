#ifndef INT_MODEL_H_
#define INT_MODEL_H_

#include "stdint.h"
#include <errors/errno.h>

/* The interrupt models a driver might use.
 * device_db.pl contains the information what driver wants to use which model.
 */
enum int_model {
    INT_MODEL_NONE = 0,
    INT_MODEL_LEGACY = 1,
    INT_MODEL_MSI = 2,
    INT_MODEL_MSIX = 3
};

/*
 * This is the struct that contains information regarding the int routing that
 * must be passed on to the driver
 */
struct int_startup_argument {
    enum int_model model;
    uint64_t int_range_start;
    uint64_t int_range_end;
    char msix_ctrl_name[128]; 
};

errval_t int_startup_argument_init(struct int_startup_argument * arg);
errval_t int_startup_argument_to_string(struct int_startup_argument * arg, char ** out);
errval_t int_startup_argument_parse(char * in, struct int_startup_argument * out);


#endif /*INT_MODEL_H_ */
