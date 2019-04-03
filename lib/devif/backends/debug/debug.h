/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEBUG_DEBUG_H_
#define DEBUG_DEBUG_H_ 1


//#define DEBUG_ENABLED 1

/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(DEBUG_ENABLED) 
#define DEBUG(x...) do { printf("DEBUG: %s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DEBUG(x...) ((void)0)
#endif 

#endif /* DEBUG_DEBUG_H_ */
