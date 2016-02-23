/*
 * Copyright (c) 2012, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <errno.h>
#include <unistd.h>
#include <vfs/fdtab.h> /* For MAX_FD */
#include <octopus/octopus.h>

#include "posixcompat.h"

/**
 * \brief Get configurable system variables.
 */
long sysconf(int name)
{
    switch(name) {
    case _SC_OPEN_MAX:
        return MAX_FD;

    case _SC_NPROCESSORS_ONLN:
        {
            oct_init();

            static char* local_apics = "r'hw\\.processor\\.[0-9]+' { enabled: 1 }";
            char** names;
            size_t count;
            errval_t err = oct_get_names(&names, &count, local_apics);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Can not get core count");
                return 1;
            }
            oct_free_names(names, count);

            return count;
        }

    default:
        debug_printf("sysconf(%d): No implementation for this "
                     "configuration information.\n", name);
        errno = EINVAL;
        return -1;
    }
}
