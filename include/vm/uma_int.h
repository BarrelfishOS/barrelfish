/*-
 * Copyright (c) 2002-2005, 2009, 2013 Jeffrey Roberson <jeff@FreeBSD.org>
 * Copyright (c) 2004, 2005 Bosko Milekic <bmilekic@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice unmodified, this list of conditions, and the following
 *    disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $FreeBSD$
 *
 */

#include <sys/_task.h>

/* 
 * This file includes definitions, structures, prototypes, and inlines that
 * should not be used outside of the actual implementation of UMA.
 */

/* 
 * Here's a quick description of the relationship between the objects:
 *
 * Kegs contain lists of slabs which are stored in either the full bin, empty
 * bin, or partially allocated bin, to reduce fragmentation.  They also contain
 * the user supplied value for size, which is adjusted for alignment purposes
 * and rsize is the result of that.  The Keg also stores information for
 * managing a hash of page addresses that maps pages to uma_slab_t structures
 * for pages that don't have embedded uma_slab_t's.
 *  
 * The uma_slab_t may be embedded in a UMA_SLAB_SIZE chunk of memory or it may
 * be allocated off the page from a special slab zone.  The free list within a
 * slab is managed with a bitmask.  For item sizes that would yield more than
 * 10% memory waste we potentially allocate a separate uma_slab_t if this will
 * improve the number of items per slab that will fit.  
 *
 * The only really gross cases, with regards to memory waste, are for those
 * items that are just over half the page size.   You can get nearly 50% waste,
 * so you fall back to the memory footprint of the power of two allocator. I
 * have looked at memory allocation sizes on many of the machines available to
 * me, and there does not seem to be an abundance of allocations at this range
 * so at this time it may not make sense to optimize for it.  This can, of 
 * course, be solved with dynamic slab sizes.
 *
 * Kegs may serve multiple Zones but by far most of the time they only serve
 * one.  When a Zone is created, a Keg is allocated and setup for it.  While
 * the backing Keg stores slabs, the Zone caches Buckets of items allocated
 * from the slabs.  Each Zone is equipped with an init/fini and ctor/dtor
 * pair, as well as with its own set of small per-CPU caches, layered above
 * the Zone's general Bucket cache.
 *
 * The PCPU caches are protected by critical sections, and may be accessed
 * safely only from their associated CPU, while the Zones backed by the same
 * Keg all share a common Keg lock (to coalesce contention on the backing
 * slabs).  The backing Keg typically only serves one Zone but in the case of
 * multiple Zones, one of the Zones is considered the Master Zone and all
 * Zone-related stats from the Keg are done in the Master Zone.  For an
 * example of a Multi-Zone setup, refer to the Mbuf allocation code.
 */

/*
 *	This is the representation for normal (Non OFFPAGE slab)
 *
 *	i == item
 *	s == slab pointer
 *
 *	<----------------  Page (UMA_SLAB_SIZE) ------------------>
 *	___________________________________________________________
 *     | _  _  _  _  _  _  _  _  _  _  _  _  _  _  _   ___________ |
 *     ||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i| |slab header||
 *     ||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_| |___________|| 
 *     |___________________________________________________________|
 *
 *
 *	This is an OFFPAGE slab. These can be larger than UMA_SLAB_SIZE.
 *
 *	___________________________________________________________
 *     | _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _   |
 *     ||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i||i|  |
 *     ||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_||_|  |
 *     |___________________________________________________________|
 *       ___________    ^
 *	|slab header|   |
 *	|___________|---*
 *
 */

#ifndef VM_UMA_INT_H
#define VM_UMA_INT_H

#define UMA_SLAB_SIZE	PAGE_SIZE	/* How big are our slabs? */
#define UMA_SLAB_MASK	(PAGE_SIZE - 1)	/* Mask to get back to the page */
#define UMA_SLAB_SHIFT	PAGE_SHIFT	/* Number of bits PAGE_MASK */

#define UMA_BOOT_PAGES		64	/* Pages allocated for startup */
#define UMA_BOOT_PAGES_ZONES	32	/* Multiplier for pages to reserve */
					/* if uma_zone > PAGE_SIZE */

/* Max waste percentage before going to off page slab management */
#define UMA_MAX_WASTE	10

/*
 * I doubt there will be many cases where this is exceeded. This is the initial
 * size of the hash table for uma_slabs that are managed off page. This hash
 * does expand by powers of two.  Currently it doesn't get smaller.
 */
#define UMA_HASH_SIZE_INIT	32		

/* 
 * I should investigate other hashing algorithms.  This should yield a low
 * number of collisions if the pages are relatively contiguous.
 */

#define UMA_HASH(h, s) ((((uintptr_t)s) >> UMA_SLAB_SHIFT) & (h)->uh_hashmask)

#define UMA_HASH_INSERT(h, s, mem)					\
		SLIST_INSERT_HEAD(&(h)->uh_slab_hash[UMA_HASH((h),	\
		    (mem))], (s), us_hlink)
#define UMA_HASH_REMOVE(h, s, mem)					\
		SLIST_REMOVE(&(h)->uh_slab_hash[UMA_HASH((h),		\
		    (mem))], (s), uma_slab, us_hlink)

/* Hash table for freed address -> slab translation */

SLIST_HEAD(slabhead, uma_slab);

struct uma_hash {
	struct slabhead	*uh_slab_hash;	/* Hash table for slabs */
	int		uh_hashsize;	/* Current size of the hash table */
	int		uh_hashmask;	/* Mask used during hashing */
};

/*
 * align field or structure to cache line
 */
#if defined(__amd64__)
#define UMA_ALIGN	__aligned(CACHE_LINE_SIZE)
#else
#define UMA_ALIGN
#endif

/*
 * Structures for per cpu queues.
 */

struct uma_bucket {
	LIST_ENTRY(uma_bucket)	ub_link;	/* Link into the zone */
	int16_t	ub_cnt;				/* Count of free items. */
	int16_t	ub_entries;			/* Max items. */
	void	*ub_bucket[];			/* actual allocation storage */
};

typedef struct uma_bucket * uma_bucket_t;

struct uma_cache {
	uma_bucket_t	uc_freebucket;	/* Bucket we're freeing to */
	uma_bucket_t	uc_allocbucket;	/* Bucket to allocate from */
	uint64_t	uc_allocs;	/* Count of allocations */
	uint64_t	uc_frees;	/* Count of frees */
} UMA_ALIGN;

typedef struct uma_cache * uma_cache_t;

/*
 * Keg management structure
 *
 * TODO: Optimize for cache line size
 *
 */
struct uma_keg {
	struct mtx_padalign	uk_lock;	/* Lock for the keg */
	struct uma_hash	uk_hash;

	LIST_HEAD(,uma_zone)	uk_zones;	/* Keg's zones */
	LIST_HEAD(,uma_slab)	uk_part_slab;	/* partially allocated slabs */
	LIST_HEAD(,uma_slab)	uk_free_slab;	/* empty slab list */
	LIST_HEAD(,uma_slab)	uk_full_slab;	/* full slabs */

	uint32_t	uk_align;	/* Alignment mask */
	uint32_t	uk_pages;	/* Total page count */
	uint32_t	uk_free;	/* Count of items free in slabs */
	uint32_t	uk_reserve;	/* Number of reserved items. */
	uint32_t	uk_size;	/* Requested size of each item */
	uint32_t	uk_rsize;	/* Real size of each item */
	uint32_t	uk_maxpages;	/* Maximum number of pages to alloc */

	uma_init	uk_init;	/* Keg's init routine */
	uma_fini	uk_fini;	/* Keg's fini routine */
	uma_alloc	uk_allocf;	/* Allocation function */
	uma_free	uk_freef;	/* Free routine */

	u_long		uk_offset;	/* Next free offset from base KVA */
	vm_offset_t	uk_kva;		/* Zone base KVA */
	uma_zone_t	uk_slabzone;	/* Slab zone backing us, if OFFPAGE */

	uint16_t	uk_slabsize;	/* Slab size for this keg */
	uint16_t	uk_pgoff;	/* Offset to uma_slab struct */
	uint16_t	uk_ppera;	/* pages per allocation from backend */
	uint16_t	uk_ipers;	/* Items per slab */
	uint32_t	uk_flags;	/* Internal flags */

	/* Least used fields go to the last cache line. */
	const char	*uk_name;		/* Name of creating zone. */
	LIST_ENTRY(uma_keg)	uk_link;	/* List of all kegs */
};
typedef struct uma_keg	* uma_keg_t;

/*
 * Free bits per-slab.
 */
#define	SLAB_SETSIZE	(PAGE_SIZE / UMA_SMALLEST_UNIT)
BITSET_DEFINE(slabbits, SLAB_SETSIZE);

/*
 * The slab structure manages a single contiguous allocation from backing
 * store and subdivides it into individually allocatable items.
 */
struct uma_slab {
	uma_keg_t	us_keg;			/* Keg we live in */
	union {
		LIST_ENTRY(uma_slab)	_us_link;	/* slabs in zone */
		unsigned long	_us_size;	/* Size of allocation */
	} us_type;
	SLIST_ENTRY(uma_slab)	us_hlink;	/* Link for hash table */
	uint8_t		*us_data;		/* First item */
	struct slabbits	us_free;		/* Free bitmask. */
#ifdef INVARIANTS
	struct slabbits	us_debugfree;		/* Debug bitmask. */
#endif
	uint16_t	us_freecount;		/* How many are free? */
	uint8_t		us_flags;		/* Page flags see uma.h */
	uint8_t		us_pad;			/* Pad to 32bits, unused. */
};

#define	us_link	us_type._us_link
#define	us_size	us_type._us_size

typedef struct uma_slab * uma_slab_t;
typedef uma_slab_t (*uma_slaballoc)(uma_zone_t, uma_keg_t, int);

struct uma_klink {
	LIST_ENTRY(uma_klink)	kl_link;
	uma_keg_t		kl_keg;
};
typedef struct uma_klink *uma_klink_t;

/*
 * Zone management structure 
 *
 * TODO: Optimize for cache line size
 *
 */
struct uma_zone {
	struct mtx_padalign	uz_lock;	/* Lock for the zone */
	struct mtx_padalign	*uz_lockptr;
	const char		*uz_name;	/* Text name of the zone */

	LIST_ENTRY(uma_zone)	uz_link;	/* List of all zones in keg */
	LIST_HEAD(,uma_bucket)	uz_buckets;	/* full buckets */

	LIST_HEAD(,uma_klink)	uz_kegs;	/* List of kegs. */
	struct uma_klink	uz_klink;	/* klink for first keg. */

	uma_slaballoc	uz_slab;	/* Allocate a slab from the backend. */
	uma_ctor	uz_ctor;	/* Constructor for each allocation */
	uma_dtor	uz_dtor;	/* Destructor */
	uma_init	uz_init;	/* Initializer for each item */
	uma_fini	uz_fini;	/* Finalizer for each item. */
	uma_import	uz_import;	/* Import new memory to cache. */
	uma_release	uz_release;	/* Release memory from cache. */
	void		*uz_arg;	/* Import/release argument. */

	uint32_t	uz_flags;	/* Flags inherited from kegs */
	uint32_t	uz_size;	/* Size inherited from kegs */

	volatile u_long	uz_allocs UMA_ALIGN; /* Total number of allocations */
	volatile u_long	uz_fails;	/* Total number of alloc failures */
	volatile u_long	uz_frees;	/* Total number of frees */
	uint64_t	uz_sleeps;	/* Total number of alloc sleeps */
	uint16_t	uz_count;	/* Amount of items in full bucket */
	uint16_t	uz_count_min;	/* Minimal amount of items there */

	/* The next two fields are used to print a rate-limited warnings. */
	const char	*uz_warning;	/* Warning to print on failure */
	struct timeval	uz_ratecheck;	/* Warnings rate-limiting */

	struct task	uz_maxaction;	/* Task to run when at limit */

	/*
	 * This HAS to be the last item because we adjust the zone size
	 * based on NCPU and then allocate the space for the zones.
	 */
	struct uma_cache	uz_cpu[1]; /* Per cpu caches */
};

/*
 * These flags must not overlap with the UMA_ZONE flags specified in uma.h.
 */
#define	UMA_ZFLAG_MULTI		0x04000000	/* Multiple kegs in the zone. */
#define	UMA_ZFLAG_DRAINING	0x08000000	/* Running zone_drain. */
#define	UMA_ZFLAG_BUCKET	0x10000000	/* Bucket zone. */
#define UMA_ZFLAG_INTERNAL	0x20000000	/* No offpage no PCPU. */
#define UMA_ZFLAG_FULL		0x40000000	/* Reached uz_maxpages */
#define UMA_ZFLAG_CACHEONLY	0x80000000	/* Don't ask VM for buckets. */

#define	UMA_ZFLAG_INHERIT						\
    (UMA_ZFLAG_INTERNAL | UMA_ZFLAG_CACHEONLY | UMA_ZFLAG_BUCKET)

static inline uma_keg_t
zone_first_keg(uma_zone_t zone)
{
	uma_klink_t klink;

	klink = LIST_FIRST(&zone->uz_kegs);
	return (klink != NULL) ? klink->kl_keg : NULL;
}

#undef UMA_ALIGN

#ifdef _KERNEL
/* Internal prototypes */
static __inline uma_slab_t hash_sfind(struct uma_hash *hash, uint8_t *data);
void *uma_large_malloc(vm_size_t size, int wait);
void uma_large_free(uma_slab_t slab);

/* Lock Macros */

#define	KEG_LOCK_INIT(k, lc)					\
	do {							\
		if ((lc))					\
			mtx_init(&(k)->uk_lock, (k)->uk_name,	\
			    (k)->uk_name, MTX_DEF | MTX_DUPOK);	\
		else						\
			mtx_init(&(k)->uk_lock, (k)->uk_name,	\
			    "UMA zone", MTX_DEF | MTX_DUPOK);	\
	} while (0)

#define	KEG_LOCK_FINI(k)	mtx_destroy(&(k)->uk_lock)
#define	KEG_LOCK(k)	mtx_lock(&(k)->uk_lock)
#define	KEG_UNLOCK(k)	mtx_unlock(&(k)->uk_lock)

#define	ZONE_LOCK_INIT(z, lc)					\
	do {							\
		if ((lc))					\
			mtx_init(&(z)->uz_lock, (z)->uz_name,	\
			    (z)->uz_name, MTX_DEF | MTX_DUPOK);	\
		else						\
			mtx_init(&(z)->uz_lock, (z)->uz_name,	\
			    "UMA zone", MTX_DEF | MTX_DUPOK);	\
	} while (0)
	    
#define	ZONE_LOCK(z)	mtx_lock((z)->uz_lockptr)
#define	ZONE_TRYLOCK(z)	mtx_trylock((z)->uz_lockptr)
#define	ZONE_UNLOCK(z)	mtx_unlock((z)->uz_lockptr)
#define	ZONE_LOCK_FINI(z)	mtx_destroy(&(z)->uz_lock)

/*
 * Find a slab within a hash table.  This is used for OFFPAGE zones to lookup
 * the slab structure.
 *
 * Arguments:
 *	hash  The hash table to search.
 *	data  The base page of the item.
 *
 * Returns:
 *	A pointer to a slab if successful, else NULL.
 */
static __inline uma_slab_t
hash_sfind(struct uma_hash *hash, uint8_t *data)
{
        uma_slab_t slab;
        int hval;

        hval = UMA_HASH(hash, data);

        SLIST_FOREACH(slab, &hash->uh_slab_hash[hval], us_hlink) {
                if ((uint8_t *)slab->us_data == data)
                        return (slab);
        }
        return (NULL);
}

static __inline uma_slab_t
vtoslab(vm_offset_t va)
{
	vm_page_t p;

	p = PHYS_TO_VM_PAGE(pmap_kextract(va));
	return ((uma_slab_t)p->plinks.s.pv);
}

static __inline void
vsetslab(vm_offset_t va, uma_slab_t slab)
{
	vm_page_t p;

	p = PHYS_TO_VM_PAGE(pmap_kextract(va));
	p->plinks.s.pv = slab;
}

/*
 * The following two functions may be defined by architecture specific code
 * if they can provide more efficient allocation functions.  This is useful
 * for using direct mapped addresses.
 */
void *uma_small_alloc(uma_zone_t zone, vm_size_t bytes, uint8_t *pflag,
    int wait);
void uma_small_free(void *mem, vm_size_t size, uint8_t flags);
#endif /* _KERNEL */

#endif /* VM_UMA_INT_H */
