/*
 * \file
 * \brief Solarflare sfn5122f debug function
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SFN5122F_DEBUG_H_
#define SFN5122F_DEBUG_H_

//#define DEBUG_SFN
//#define DEBUG_BUFTBL

#ifdef DEBUG_SFN
    #define DEBUG_QUEUE(x...) printf("sfn5122f_q : " x)
#else
    #define DEBUG_QUEUE(x...) do {} while (0)
#endif

#ifdef DEBUG_SFN
    #define DEBUG(x...) printf("sfn5122f: " x)
#else
    #define DEBUG(x...) do {} while (0)
#endif


#ifdef DEBUG_BUFTBL
    #define DEBUG_BUF(x...) printf("buftbl: " x)
#else
    #define DEBUG_BUF(x...) do {} while (0)
#endif

#endif /* SFN5122F_DEBUG_H_ */
