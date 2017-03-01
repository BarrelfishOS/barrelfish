/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __DEBUG_H__
#define __DEBUG_H__

//#define DEBUG_E10K_VF 1
//#define DEBUG_E10K_QUEUE 1 

/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(DEBUG_E10K_VF) 
#define DEBUG_VF(x...) do { printf("e10k_vf:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DEBUG_VF(x...) ((void)0)
#endif 

#if defined(DEBUG_E10K_QUEUE) 
#define DEBUG_QUEUE(x...) do { printf("e10k_queue:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)
#else
#define DEBUG_QUEUE(x...) ((void)0)
#endif 

#endif
