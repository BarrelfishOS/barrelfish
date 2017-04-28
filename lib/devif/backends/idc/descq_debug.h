/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DESCQ_DEBUG_H_
#define DESCQ_DEBUG_H_ 1


//#define DESCQ_DEBUG_ENABLED 1

/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(DESCQ_DEBUG_ENABLED) 
#define DESCQ_DEBUG(x...) do { printf("DQI_REGION:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DESCQ_DEBUG(x...) ((void)0)
#endif 

#endif /* DESCQ_DEBUG_H_ */
