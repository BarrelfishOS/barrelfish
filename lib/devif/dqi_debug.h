/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DQI_DEBUG_H_
#define DQI_DEBUG_H_ 1


//#define DQI_DEBUG_ENABLED 1
//#define DQI_REGION_DEBUG_ENABLED 1 
//#define DQI_DEBUG_QUEUE_ENABLED 1

/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(DQI_DEBUG_ENABLED) 
#define DQI_DEBUG(x...) do { printf("DQI:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DQI_DEBUG(x...) ((void)0)
#endif 

#if defined(DQI_REGION_DEBUG_ENABLED) 
#define DQI_DEBUG_REGION(x...) do { printf("DQI_REGION:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DQI_DEBUG_REGION(x...) ((void)0)
#endif 


#if defined(DQI_DEBUG_QUEUE_ENABLED) 
#define DQI_DEBUG_QUEUE(x...) do { printf("DQI_REGION:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define DQI_DEBUG_QUEUE(x...) ((void)0)
#endif 

#endif /* DQI_DEBUG_H_ */
