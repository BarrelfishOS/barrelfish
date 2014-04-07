/*
 * Australian Public Licence B (OZPLB)
 * 
 * Version 1-0
 * 
 * Copyright (c) 2004 University of New South Wales
 * 
 * All rights reserved. 
 * 
 * Developed by: Operating Systems and Distributed Systems Group (DiSy)
 *               University of New South Wales
 *               http://www.disy.cse.unsw.edu.au
 * 
 * Permission is granted by University of New South Wales, free of charge, to
 * any person obtaining a copy of this software and any associated
 * documentation files (the "Software") to deal with the Software without
 * restriction, including (without limitation) the rights to use, copy,
 * modify, adapt, merge, publish, distribute, communicate to the public,
 * sublicense, and/or sell, lend or rent out copies of the Software, and
 * to permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimers.
 * 
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimers in the documentation and/or other materials provided
 *       with the distribution.
 * 
 *     * Neither the name of University of New South Wales, nor the names of its
 *       contributors, may be used to endorse or promote products derived
 *       from this Software without specific prior written permission.
 * 
 * EXCEPT AS EXPRESSLY STATED IN THIS LICENCE AND TO THE FULL EXTENT
 * PERMITTED BY APPLICABLE LAW, THE SOFTWARE IS PROVIDED "AS-IS", AND
 * NATIONAL ICT AUSTRALIA AND ITS CONTRIBUTORS MAKE NO REPRESENTATIONS,
 * WARRANTIES OR CONDITIONS OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO ANY REPRESENTATIONS, WARRANTIES OR CONDITIONS
 * REGARDING THE CONTENTS OR ACCURACY OF THE SOFTWARE, OR OF TITLE,
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NONINFRINGEMENT,
 * THE ABSENCE OF LATENT OR OTHER DEFECTS, OR THE PRESENCE OR ABSENCE OF
 * ERRORS, WHETHER OR NOT DISCOVERABLE.
 * 
 * TO THE FULL EXTENT PERMITTED BY APPLICABLE LAW, IN NO EVENT SHALL
 * NATIONAL ICT AUSTRALIA OR ITS CONTRIBUTORS BE LIABLE ON ANY LEGAL
 * THEORY (INCLUDING, WITHOUT LIMITATION, IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHERWISE) FOR ANY CLAIM, LOSS, DAMAGES OR OTHER
 * LIABILITY, INCLUDING (WITHOUT LIMITATION) LOSS OF PRODUCTION OR
 * OPERATION TIME, LOSS, DAMAGE OR CORRUPTION OF DATA OR RECORDS; OR LOSS
 * OF ANTICIPATED SAVINGS, OPPORTUNITY, REVENUE, PROFIT OR GOODWILL, OR
 * OTHER ECONOMIC LOSS; OR ANY SPECIAL, INCIDENTAL, INDIRECT,
 * CONSEQUENTIAL, PUNITIVE OR EXEMPLARY DAMAGES, ARISING OUT OF OR IN
 * CONNECTION WITH THIS LICENCE, THE SOFTWARE OR THE USE OF OR OTHER
 * DEALINGS WITH THE SOFTWARE, EVEN IF NATIONAL ICT AUSTRALIA OR ITS
 * CONTRIBUTORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH CLAIM, LOSS,
 * DAMAGES OR OTHER LIABILITY.
 * 
 * If applicable legislation implies representations, warranties, or
 * conditions, or imposes obligations or liability on University of New South
 * Wales or one of its contributors in respect of the Software that
 * cannot be wholly or partly excluded, restricted or modified, the
 * liability of University of New South Wales or the contributor is limited, to
 * the full extent permitted by the applicable legislation, at its
 * option, to:
 * a.  in the case of goods, any one or more of the following:
 * i.  the replacement of the goods or the supply of equivalent goods;
 * ii.  the repair of the goods;
 * iii. the payment of the cost of replacing the goods or of acquiring
 *  equivalent goods;
 * iv.  the payment of the cost of having the goods repaired; or
 * b.  in the case of services:
 * i.  the supplying of the services again; or
 * ii.  the payment of the cost of having the services supplied again.
 * 
 * The construction, validity and performance of this licence is governed
 * by the laws in force in New South Wales, Australia.
 */
/*
 Author: Carl van Schaik
*/

#include <kernel.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish_kpi/asm_inlines_arch.h>
#include <barrelfish_kpi/paging_arch.h>
#include <x86.h>
#include <fpu.h>
#include <dispatch.h>
#include <memset.h>
/*
 * Fill memory at s with (n) * byte value 'c'
 */
void *
memset(void *s, int c, size_t n)
{
	uintptr_t num, align, pattern, *p, x;
	unsigned char *mem = s;

	x = (unsigned char)c;
	align = sizeof(uintptr_t)-1;

	if ((uintptr_t)s & align)
	{
		num = n > align ? (sizeof(uintptr_t) - ((uintptr_t)s & align)) : n;
		n -= num;
		while (num--)
			*mem++ = x;
	}

	num = (uintptr_t)n / sizeof(uintptr_t);

	p = (uintptr_t*)mem;

#if UINTPTR_MAX == UINT32_MAX
	pattern = x | x << 8 | x << 16 | x << 24;
#elif UINTPTR_MAX == UINT64_MAX
	pattern = x | x << 8 | x << 16 | x << 24 |
		x << 32 | x << 40 | x << 48 | x << 56;
#else
#error UINTPTR_MAX not valid
#endif
	while (num) {
		/* simple hand unrolled loop */
		if ((num & 3)==0) {
			num -= 4;
			*p++ = pattern;
			*p++ = pattern;
			*p++ = pattern;
			*p++ = pattern;
		} else {
			num--;
			*p++ = pattern;
		}
	}
	mem = (unsigned char*)p;

	num = n & align;
	while (num--)
		*mem++ = x;

	return s;
}

/**
 * Fill memory at s with (n) * BASE_PAGE_SIZE bytes of value 'c'
 * uses SSE2 instructions (if available)
 */
void *memset_pages(void *s, int c, size_t n)
{
#ifdef __x86_64__
    if ((uintptr_t)s&0x7F) {
        // goto naive impl if not aligned to 128b
        return memset(s, c, n*BASE_PAGE_SIZE);
    }

    bool trap = fpu_trap_get();
    fpu_trap_off();

    if (fpu_dcb && fpu_dcb == dcb_current) {
        // need to account for fpu state by application
        struct dispatcher_shared_generic *dst =
            get_dispatcher_shared_generic(fpu_dcb->disp);

        if(fpu_dcb->disabled) {
            fpu_save(dispatcher_get_disabled_fpu_save_area(fpu_dcb->disp));
	    dst->fpu_used = 1;
        } else {
            assert(!fpu_dcb->disabled);
            fpu_save(dispatcher_get_enabled_fpu_save_area(fpu_dcb->disp));
	    dst->fpu_used = 2;
        }
    }

    // call 128b sse2-enabled memset loop
    memset_128b_sse2_(s,c,n);

    if (trap) {
        fpu_trap_on();
    }

    return s;
#else
    // fall back to standard memset for other archs
    return memset(s, c, n*BASE_PAGE_SIZE);
#endif
}
