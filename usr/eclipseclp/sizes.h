/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#if !defined(ECLIPSECLP_SIZES_H_)
#define ECLIPSECLP_SIZES_H_ 1

/*
 * Eclipse needs to know the exact sizes of various types at compile time. Thus,
 * we cannot just use sizeof but have to define the sizes here.
 */

/* Type sizes.  */
// #undef SIZEOF_INT
// #undef SIZEOF_LONG
// #undef SIZEOF_CHAR_P
// #undef SIZEOF_LONG_P
#if defined(__x86_64__)
#   define SIZEOF_INT 4
#   define SIZEOF_LONG 8
#   define SIZEOF_CHAR_P 8
#   define SIZEOF_LONG_P 8
#   if !defined(_FPU_SETCW)
#       define _FPU_SETCW
#   endif
#   if !defined(_FPU_RC_DOWN)
#       define _FPU_RC_DOWN
#   endif
#   if !defined(__x86_64)
#       define __x86_64
#   endif
#   if !defined(__SSE_MATH__)
#       define __SSE_MATH__
#   endif
#elif defined(__x86__)
#   define SIZEOF_INT 4
#   define SIZEOF_LONG 4
#   define SIZEOF_CHAR_P 4
#   define SIZEOF_LONG_P 4
#   if !defined(_FPU_RC_DOWN)
#       define _FPU_RC_DOWN
#   endif
#elif defined(__arm__)
#   define SIZEOF_INT 4
#   define SIZEOF_LONG 4
#   define SIZEOF_CHAR_P 4
#   define SIZEOF_LONG_P 4
#elif defined(__aarch64__)
#   define SIZEOF_INT 4
#   define SIZEOF_LONG 8
#   define SIZEOF_CHAR_P 8
#   define SIZEOF_LONG_P 8
#else
#   error Unknown architecture
#endif

#endif
