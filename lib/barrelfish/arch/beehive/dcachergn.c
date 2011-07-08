/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <dcache.h>

extern void bee_dcache_flush_lines(size_t line, size_t countless1);
extern void bee_dcache_empty_lines(size_t line, size_t countless1);

void bee_dcache_flush_rgn (void * addr, size_t n);
void bee_dcache_empty_rgn (void * addr, size_t n);

void bee_dcache_flush_rgn (void * addr, size_t n)
{
    if (n == 0) return;

    unsigned int start = (unsigned int)addr;
    unsigned int after = start + n;

    unsigned int firstline = start >> 5;
    unsigned int lastline = (after - 1) >> 5;

    unsigned int countlessone = lastline - firstline;

    if (countlessone >= 127) {
	/*
	 * This command will affect the entire cache,
	 * so just perform the whole cache version.
	 */
	
	bee_dcache_flush_all();
	return;
    }

    unsigned int firstalias = firstline & 127;

    /*
     * According to Chuck, a dcache command in which
     * first + count wraps around the cache is illegal.
     * So for such a case, we have to break it up into
     * two dcache commands.
     */
    
    if (firstalias + countlessone > 127) {
	/*
	 * At this point we must have ( firstalias > 0 )
	 * because we know that ( countlessone < 127 )
	 */
	
	unsigned int countlessonexx = 127 - firstalias;
	bee_dcache_flush_lines(firstalias,countlessonexx);
	
	/*
	 * Note that ( countlessone > 127 - firstalias )
	 * therefore ( countlessone > countlessonexx )
	 */
	
	firstalias = 0;
	countlessone -= countlessonexx + 1;
    }

    bee_dcache_flush_lines(firstalias,countlessone);
}   


void bee_dcache_empty_rgn (void * addr, size_t n)
{
    if (n == 0) return;

    unsigned int start = (unsigned int)addr;
    unsigned int after = start + n;

    unsigned int firstline = start >> 5;
    unsigned int lastline = (after - 1) >> 5;

    unsigned int countlessone = lastline - firstline;

    if (countlessone >= 127) {
	/*
	 * This command will affect the entire cache,
	 * so just perform the whole cache version.
	 */
	
	bee_dcache_empty_all();
	return;
    }

    unsigned int firstalias = firstline & 127;

    /*
     * According to Chuck, a dcache command in which
     * first + count wraps around the cache is illegal.
     * So for such a case, we have to break it up into
     * two dcache commands.
     */
    
    if (firstalias + countlessone > 127) {
	/*
	 * At this point we must have ( firstalias > 0 )
	 * because we know that ( countlessone < 127 )
	 */
	
	unsigned int countlessonexx = 127 - firstalias;
	bee_dcache_empty_lines(firstalias,countlessonexx);
	
	/*
	 * Note that ( countlessone > 127 - firstalias )
	 * therefore ( countlessone > countlessonexx )
	 */
	
	firstalias = 0;
	countlessone -= countlessonexx + 1;
    }

    bee_dcache_empty_lines(firstalias,countlessone);
}   
