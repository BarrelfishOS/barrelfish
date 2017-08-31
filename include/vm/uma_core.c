/*-
 * Copyright (c) 2002-2005, 2009, 2013 Jeffrey Roberson <jeff@FreeBSD.org>
 * Copyright (c) 2004, 2005 Bosko Milekic <bmilekic@FreeBSD.org>
 * Copyright (c) 2004-2006 Robert N. M. Watson
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
 */

/*
 * uma_core.c  Implementation of the Universal Memory allocator
 *
 * This allocator is intended to replace the multitude of similar object caches
 * in the standard FreeBSD kernel.  The intent is to be flexible as well as
 * efficient.  A primary design goal is to return unused memory to the rest of
 * the system.  This will make the system as a whole more flexible due to the
 * ability to move memory to subsystems which most need it instead of leaving
 * pools of reserved memory unused.
 *
 * The basic ideas stem from similar slab/zone based allocators whose algorithms
 * are well known.
 *
 */

/*
 * TODO:
 *	- Improve memory usage for large allocations
 *	- Investigate cache size adjustments
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

/* I should really use ktr.. */
/*
#define UMA_DEBUG 1
#define UMA_DEBUG_ALLOC 1
#define UMA_DEBUG_ALLOC_1 1
*/

#include "opt_ddb.h"
#include "opt_param.h"
#include "opt_vm.h"

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/bitset.h>
#include <sys/kernel.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <sys/malloc.h>
#include <sys/ktr.h>
#include <sys/lock.h>
#include <sys/sysctl.h>
#include <sys/mutex.h>
#include <sys/proc.h>
#include <sys/random.h>
#include <sys/rwlock.h>
#include <sys/sbuf.h>
#include <sys/sched.h>
#include <sys/smp.h>
#include <sys/taskqueue.h>
#include <sys/vmmeter.h>

#include <vm/vm.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <vm/vm_param.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <vm/vm_extern.h>
#include <vm/uma.h>
#include <vm/uma_int.h>
#include <vm/uma_dbg.h>

#include <ddb/ddb.h>

#ifdef DEBUG_MEMGUARD
#include <vm/memguard.h>
#endif

/*
 * This is the zone and keg from which all zones are spawned.  The idea is that
 * even the zone & keg heads are allocated from the allocator, so we use the
 * bss section to bootstrap us.
 */
static struct uma_keg masterkeg;
static struct uma_zone masterzone_k;
static struct uma_zone masterzone_z;
static uma_zone_t kegs = &masterzone_k;
static uma_zone_t zones = &masterzone_z;

/* This is the zone from which all of uma_slab_t's are allocated. */
static uma_zone_t slabzone;

/*
 * The initial hash tables come out of this zone so they can be allocated
 * prior to malloc coming up.
 */
static uma_zone_t hashzone;

/* The boot-time adjusted value for cache line alignment. */
int uma_align_cache = 64 - 1;

static MALLOC_DEFINE(M_UMAHASH, "UMAHash", "UMA Hash Buckets");

/*
 * Are we allowed to allocate buckets?
 */
static int bucketdisable = 1;

/* Linked list of all kegs in the system */
static LIST_HEAD(,uma_keg) uma_kegs = LIST_HEAD_INITIALIZER(uma_kegs);

/* Linked list of all cache-only zones in the system */
static LIST_HEAD(,uma_zone) uma_cachezones =
    LIST_HEAD_INITIALIZER(uma_cachezones);

/* This RW lock protects the keg list */
static struct rwlock_padalign uma_rwlock;

/* Linked list of boot time pages */
static LIST_HEAD(,uma_slab) uma_boot_pages =
    LIST_HEAD_INITIALIZER(uma_boot_pages);

/* This mutex protects the boot time pages list */
static struct mtx_padalign uma_boot_pages_mtx;

static struct sx uma_drain_lock;

/* Is the VM done starting up? */
static int booted = 0;
#define	UMA_STARTUP	1
#define	UMA_STARTUP2	2

/*
 * This is the handle used to schedule events that need to happen
 * outside of the allocation fast path.
 */
static struct callout uma_callout;
#define	UMA_TIMEOUT	20		/* Seconds for callout interval. */

/*
 * This structure is passed as the zone ctor arg so that I don't have to create
 * a special allocation function just for zones.
 */
struct uma_zctor_args {
	const char *name;
	size_t size;
	uma_ctor ctor;
	uma_dtor dtor;
	uma_init uminit;
	uma_fini fini;
	uma_import import;
	uma_release release;
	void *arg;
	uma_keg_t keg;
	int align;
	uint32_t flags;
};

struct uma_kctor_args {
	uma_zone_t zone;
	size_t size;
	uma_init uminit;
	uma_fini fini;
	int align;
	uint32_t flags;
};

struct uma_bucket_zone {
	uma_zone_t	ubz_zone;
	char		*ubz_name;
	int		ubz_entries;	/* Number of items it can hold. */
	int		ubz_maxsize;	/* Maximum allocation size per-item. */
};

/*
 * Compute the actual number of bucket entries to pack them in power
 * of two sizes for more efficient space utilization.
 */
#define	BUCKET_SIZE(n)						\
    (((sizeof(void *) * (n)) - sizeof(struct uma_bucket)) / sizeof(void *))

#define	BUCKET_MAX	BUCKET_SIZE(256)

struct uma_bucket_zone bucket_zones[] = {
	{ NULL, "4 Bucket", BUCKET_SIZE(4), 4096 },
	{ NULL, "6 Bucket", BUCKET_SIZE(6), 3072 },
	{ NULL, "8 Bucket", BUCKET_SIZE(8), 2048 },
	{ NULL, "12 Bucket", BUCKET_SIZE(12), 1536 },
	{ NULL, "16 Bucket", BUCKET_SIZE(16), 1024 },
	{ NULL, "32 Bucket", BUCKET_SIZE(32), 512 },
	{ NULL, "64 Bucket", BUCKET_SIZE(64), 256 },
	{ NULL, "128 Bucket", BUCKET_SIZE(128), 128 },
	{ NULL, "256 Bucket", BUCKET_SIZE(256), 64 },
	{ NULL, NULL, 0}
};

/*
 * Flags and enumerations to be passed to internal functions.
 */
enum zfreeskip { SKIP_NONE = 0, SKIP_DTOR, SKIP_FINI };

/* Prototypes.. */

static void *noobj_alloc(uma_zone_t, vm_size_t, uint8_t *, int);
static void *page_alloc(uma_zone_t, vm_size_t, uint8_t *, int);
static void *startup_alloc(uma_zone_t, vm_size_t, uint8_t *, int);
static void page_free(void *, vm_size_t, uint8_t);
static uma_slab_t keg_alloc_slab(uma_keg_t, uma_zone_t, int);
static void cache_drain(uma_zone_t);
static void bucket_drain(uma_zone_t, uma_bucket_t);
static void bucket_cache_drain(uma_zone_t zone);
static int keg_ctor(void *, int, void *, int);
static void keg_dtor(void *, int, void *);
static int zone_ctor(void *, int, void *, int);
static void zone_dtor(void *, int, void *);
static int zero_init(void *, int, int);
static void keg_small_init(uma_keg_t keg);
static void keg_large_init(uma_keg_t keg);
static void zone_foreach(void (*zfunc)(uma_zone_t));
static void zone_timeout(uma_zone_t zone);
static int hash_alloc(struct uma_hash *);
static int hash_expand(struct uma_hash *, struct uma_hash *);
static void hash_free(struct uma_hash *hash);
static void uma_timeout(void *);
static void uma_startup3(void);
static void *zone_alloc_item(uma_zone_t, void *, int);
static void zone_free_item(uma_zone_t, void *, void *, enum zfreeskip);
static void bucket_enable(void);
static void bucket_init(void);
static uma_bucket_t bucket_alloc(uma_zone_t zone, void *, int);
static void bucket_free(uma_zone_t zone, uma_bucket_t, void *);
static void bucket_zone_drain(void);
static uma_bucket_t zone_alloc_bucket(uma_zone_t zone, void *, int flags);
static uma_slab_t zone_fetch_slab(uma_zone_t zone, uma_keg_t last, int flags);
static uma_slab_t zone_fetch_slab_multi(uma_zone_t zone, uma_keg_t last, int flags);
static void *slab_alloc_item(uma_keg_t keg, uma_slab_t slab);
static void slab_free_item(uma_keg_t keg, uma_slab_t slab, void *item);
static uma_keg_t uma_kcreate(uma_zone_t zone, size_t size, uma_init uminit,
    uma_fini fini, int align, uint32_t flags);
static int zone_import(uma_zone_t zone, void **bucket, int max, int flags);
static void zone_release(uma_zone_t zone, void **bucket, int cnt);
static void uma_zero_item(void *item, uma_zone_t zone);

void uma_print_zone(uma_zone_t);
void uma_print_stats(void);
static int sysctl_vm_zone_count(SYSCTL_HANDLER_ARGS);
static int sysctl_vm_zone_stats(SYSCTL_HANDLER_ARGS);

#ifdef INVARIANTS
static void uma_dbg_free(uma_zone_t zone, uma_slab_t slab, void *item);
static void uma_dbg_alloc(uma_zone_t zone, uma_slab_t slab, void *item);
#endif

SYSINIT(uma_startup3, SI_SUB_VM_CONF, SI_ORDER_SECOND, uma_startup3, NULL);

SYSCTL_PROC(_vm, OID_AUTO, zone_count, CTLFLAG_RD|CTLTYPE_INT,
    0, 0, sysctl_vm_zone_count, "I", "Number of UMA zones");

SYSCTL_PROC(_vm, OID_AUTO, zone_stats, CTLFLAG_RD|CTLTYPE_STRUCT,
    0, 0, sysctl_vm_zone_stats, "s,struct uma_type_header", "Zone Stats");

static int zone_warnings = 1;
SYSCTL_INT(_vm, OID_AUTO, zone_warnings, CTLFLAG_RWTUN, &zone_warnings, 0,
    "Warn when UMA zones becomes full");

/*
 * This routine checks to see whether or not it's safe to enable buckets.
 */
static void
bucket_enable(void)
{
	bucketdisable = vm_page_count_min();
}

/*
 * Initialize bucket_zones, the array of zones of buckets of various sizes.
 *
 * For each zone, calculate the memory required for each bucket, consisting
 * of the header and an array of pointers.
 */
static void
bucket_init(void)
{
	struct uma_bucket_zone *ubz;
	int size;

	for (ubz = &bucket_zones[0]; ubz->ubz_entries != 0; ubz++) {
		size = roundup(sizeof(struct uma_bucket), sizeof(void *));
		size += sizeof(void *) * ubz->ubz_entries;
		ubz->ubz_zone = uma_zcreate(ubz->ubz_name, size,
		    NULL, NULL, NULL, NULL, UMA_ALIGN_PTR,
		    UMA_ZONE_MTXCLASS | UMA_ZFLAG_BUCKET);
	}
}

/*
 * Given a desired number of entries for a bucket, return the zone from which
 * to allocate the bucket.
 */
static struct uma_bucket_zone *
bucket_zone_lookup(int entries)
{
	struct uma_bucket_zone *ubz;

	for (ubz = &bucket_zones[0]; ubz->ubz_entries != 0; ubz++)
		if (ubz->ubz_entries >= entries)
			return (ubz);
	ubz--;
	return (ubz);
}

static int
bucket_select(int size)
{
	struct uma_bucket_zone *ubz;

	ubz = &bucket_zones[0];
	if (size > ubz->ubz_maxsize)
		return MAX((ubz->ubz_maxsize * ubz->ubz_entries) / size, 1);

	for (; ubz->ubz_entries != 0; ubz++)
		if (ubz->ubz_maxsize < size)
			break;
	ubz--;
	return (ubz->ubz_entries);
}

static uma_bucket_t
bucket_alloc(uma_zone_t zone, void *udata, int flags)
{
	struct uma_bucket_zone *ubz;
	uma_bucket_t bucket;

	/*
	 * This is to stop us from allocating per cpu buckets while we're
	 * running out of vm.boot_pages.  Otherwise, we would exhaust the
	 * boot pages.  This also prevents us from allocating buckets in
	 * low memory situations.
	 */
	if (bucketdisable)
		return (NULL);
	/*
	 * To limit bucket recursion we store the original zone flags
	 * in a cookie passed via zalloc_arg/zfree_arg.  This allows the
	 * NOVM flag to persist even through deep recursions.  We also
	 * store ZFLAG_BUCKET once we have recursed attempting to allocate
	 * a bucket for a bucket zone so we do not allow infinite bucket
	 * recursion.  This cookie will even persist to frees of unused
	 * buckets via the allocation path or bucket allocations in the
	 * free path.
	 */
	if ((zone->uz_flags & UMA_ZFLAG_BUCKET) == 0)
		udata = (void *)(uintptr_t)zone->uz_flags;
	else {
		if ((uintptr_t)udata & UMA_ZFLAG_BUCKET)
			return (NULL);
		udata = (void *)((uintptr_t)udata | UMA_ZFLAG_BUCKET);
	}
	if ((uintptr_t)udata & UMA_ZFLAG_CACHEONLY)
		flags |= M_NOVM;
	ubz = bucket_zone_lookup(zone->uz_count);
	if (ubz->ubz_zone == zone && (ubz + 1)->ubz_entries != 0)
		ubz++;
	bucket = uma_zalloc_arg(ubz->ubz_zone, udata, flags);
	if (bucket) {
#ifdef INVARIANTS
		bzero(bucket->ub_bucket, sizeof(void *) * ubz->ubz_entries);
#endif
		bucket->ub_cnt = 0;
		bucket->ub_entries = ubz->ubz_entries;
	}

	return (bucket);
}

static void
bucket_free(uma_zone_t zone, uma_bucket_t bucket, void *udata)
{
	struct uma_bucket_zone *ubz;

	KASSERT(bucket->ub_cnt == 0,
	    ("bucket_free: Freeing a non free bucket."));
	if ((zone->uz_flags & UMA_ZFLAG_BUCKET) == 0)
		udata = (void *)(uintptr_t)zone->uz_flags;
	ubz = bucket_zone_lookup(bucket->ub_entries);
	uma_zfree_arg(ubz->ubz_zone, bucket, udata);
}

static void
bucket_zone_drain(void)
{
	struct uma_bucket_zone *ubz;

	for (ubz = &bucket_zones[0]; ubz->ubz_entries != 0; ubz++)
		zone_drain(ubz->ubz_zone);
}

static void
zone_log_warning(uma_zone_t zone)
{
	static const struct timeval warninterval = { 300, 0 };

	if (!zone_warnings || zone->uz_warning == NULL)
		return;

	if (ratecheck(&zone->uz_ratecheck, &warninterval))
		printf("[zone: %s] %s\n", zone->uz_name, zone->uz_warning);
}

static inline void
zone_maxaction(uma_zone_t zone)
{

	if (zone->uz_maxaction.ta_func != NULL)
		taskqueue_enqueue(taskqueue_thread, &zone->uz_maxaction);
}

static void
zone_foreach_keg(uma_zone_t zone, void (*kegfn)(uma_keg_t))
{
	uma_klink_t klink;

	LIST_FOREACH(klink, &zone->uz_kegs, kl_link)
		kegfn(klink->kl_keg);
}

/*
 * Routine called by timeout which is used to fire off some time interval
 * based calculations.  (stats, hash size, etc.)
 *
 * Arguments:
 *	arg   Unused
 *
 * Returns:
 *	Nothing
 */
static void
uma_timeout(void *unused)
{
	bucket_enable();
	zone_foreach(zone_timeout);

	/* Reschedule this event */
	callout_reset(&uma_callout, UMA_TIMEOUT * hz, uma_timeout, NULL);
}

/*
 * Routine to perform timeout driven calculations.  This expands the
 * hashes and does per cpu statistics aggregation.
 *
 *  Returns nothing.
 */
static void
keg_timeout(uma_keg_t keg)
{

	KEG_LOCK(keg);
	/*
	 * Expand the keg hash table.
	 *
	 * This is done if the number of slabs is larger than the hash size.
	 * What I'm trying to do here is completely reduce collisions.  This
	 * may be a little aggressive.  Should I allow for two collisions max?
	 */
	if (keg->uk_flags & UMA_ZONE_HASH &&
	    keg->uk_pages / keg->uk_ppera >= keg->uk_hash.uh_hashsize) {
		struct uma_hash newhash;
		struct uma_hash oldhash;
		int ret;

		/*
		 * This is so involved because allocating and freeing
		 * while the keg lock is held will lead to deadlock.
		 * I have to do everything in stages and check for
		 * races.
		 */
		newhash = keg->uk_hash;
		KEG_UNLOCK(keg);
		ret = hash_alloc(&newhash);
		KEG_LOCK(keg);
		if (ret) {
			if (hash_expand(&keg->uk_hash, &newhash)) {
				oldhash = keg->uk_hash;
				keg->uk_hash = newhash;
			} else
				oldhash = newhash;

			KEG_UNLOCK(keg);
			hash_free(&oldhash);
			return;
		}
	}
	KEG_UNLOCK(keg);
}

static void
zone_timeout(uma_zone_t zone)
{

	zone_foreach_keg(zone, &keg_timeout);
}

/*
 * Allocate and zero fill the next sized hash table from the appropriate
 * backing store.
 *
 * Arguments:
 *	hash  A new hash structure with the old hash size in uh_hashsize
 *
 * Returns:
 *	1 on success and 0 on failure.
 */
static int
hash_alloc(struct uma_hash *hash)
{
	int oldsize;
	int alloc;

	oldsize = hash->uh_hashsize;

	/* We're just going to go to a power of two greater */
	if (oldsize)  {
		hash->uh_hashsize = oldsize * 2;
		alloc = sizeof(hash->uh_slab_hash[0]) * hash->uh_hashsize;
		hash->uh_slab_hash = (struct slabhead *)malloc(alloc,
		    M_UMAHASH, M_NOWAIT);
	} else {
		alloc = sizeof(hash->uh_slab_hash[0]) * UMA_HASH_SIZE_INIT;
		hash->uh_slab_hash = zone_alloc_item(hashzone, NULL,
		    M_WAITOK);
		hash->uh_hashsize = UMA_HASH_SIZE_INIT;
	}
	if (hash->uh_slab_hash) {
		bzero(hash->uh_slab_hash, alloc);
		hash->uh_hashmask = hash->uh_hashsize - 1;
		return (1);
	}

	return (0);
}

/*
 * Expands the hash table for HASH zones.  This is done from zone_timeout
 * to reduce collisions.  This must not be done in the regular allocation
 * path, otherwise, we can recurse on the vm while allocating pages.
 *
 * Arguments:
 *	oldhash  The hash you want to expand
 *	newhash  The hash structure for the new table
 *
 * Returns:
 *	Nothing
 *
 * Discussion:
 */
static int
hash_expand(struct uma_hash *oldhash, struct uma_hash *newhash)
{
	uma_slab_t slab;
	int hval;
	int i;

	if (!newhash->uh_slab_hash)
		return (0);

	if (oldhash->uh_hashsize >= newhash->uh_hashsize)
		return (0);

	/*
	 * I need to investigate hash algorithms for resizing without a
	 * full rehash.
	 */

	for (i = 0; i < oldhash->uh_hashsize; i++)
		while (!SLIST_EMPTY(&oldhash->uh_slab_hash[i])) {
			slab = SLIST_FIRST(&oldhash->uh_slab_hash[i]);
			SLIST_REMOVE_HEAD(&oldhash->uh_slab_hash[i], us_hlink);
			hval = UMA_HASH(newhash, slab->us_data);
			SLIST_INSERT_HEAD(&newhash->uh_slab_hash[hval],
			    slab, us_hlink);
		}

	return (1);
}

/*
 * Free the hash bucket to the appropriate backing store.
 *
 * Arguments:
 *	slab_hash  The hash bucket we're freeing
 *	hashsize   The number of entries in that hash bucket
 *
 * Returns:
 *	Nothing
 */
static void
hash_free(struct uma_hash *hash)
{
	if (hash->uh_slab_hash == NULL)
		return;
	if (hash->uh_hashsize == UMA_HASH_SIZE_INIT)
		zone_free_item(hashzone, hash->uh_slab_hash, NULL, SKIP_NONE);
	else
		free(hash->uh_slab_hash, M_UMAHASH);
}

/*
 * Frees all outstanding items in a bucket
 *
 * Arguments:
 *	zone   The zone to free to, must be unlocked.
 *	bucket The free/alloc bucket with items, cpu queue must be locked.
 *
 * Returns:
 *	Nothing
 */

static void
bucket_drain(uma_zone_t zone, uma_bucket_t bucket)
{
	int i;

	if (bucket == NULL)
		return;

	if (zone->uz_fini)
		for (i = 0; i < bucket->ub_cnt; i++) 
			zone->uz_fini(bucket->ub_bucket[i], zone->uz_size);
	zone->uz_release(zone->uz_arg, bucket->ub_bucket, bucket->ub_cnt);
	bucket->ub_cnt = 0;
}

/*
 * Drains the per cpu caches for a zone.
 *
 * NOTE: This may only be called while the zone is being turn down, and not
 * during normal operation.  This is necessary in order that we do not have
 * to migrate CPUs to drain the per-CPU caches.
 *
 * Arguments:
 *	zone     The zone to drain, must be unlocked.
 *
 * Returns:
 *	Nothing
 */
static void
cache_drain(uma_zone_t zone)
{
	uma_cache_t cache;
	int cpu;

	/*
	 * XXX: It is safe to not lock the per-CPU caches, because we're
	 * tearing down the zone anyway.  I.e., there will be no further use
	 * of the caches at this point.
	 *
	 * XXX: It would good to be able to assert that the zone is being
	 * torn down to prevent improper use of cache_drain().
	 *
	 * XXX: We lock the zone before passing into bucket_cache_drain() as
	 * it is used elsewhere.  Should the tear-down path be made special
	 * there in some form?
	 */
	CPU_FOREACH(cpu) {
		cache = &zone->uz_cpu[cpu];
		bucket_drain(zone, cache->uc_allocbucket);
		bucket_drain(zone, cache->uc_freebucket);
		if (cache->uc_allocbucket != NULL)
			bucket_free(zone, cache->uc_allocbucket, NULL);
		if (cache->uc_freebucket != NULL)
			bucket_free(zone, cache->uc_freebucket, NULL);
		cache->uc_allocbucket = cache->uc_freebucket = NULL;
	}
	ZONE_LOCK(zone);
	bucket_cache_drain(zone);
	ZONE_UNLOCK(zone);
}

static void
cache_shrink(uma_zone_t zone)
{

	if (zone->uz_flags & UMA_ZFLAG_INTERNAL)
		return;

	ZONE_LOCK(zone);
	zone->uz_count = (zone->uz_count_min + zone->uz_count) / 2;
	ZONE_UNLOCK(zone);
}

static void
cache_drain_safe_cpu(uma_zone_t zone)
{
	uma_cache_t cache;
	uma_bucket_t b1, b2;

	if (zone->uz_flags & UMA_ZFLAG_INTERNAL)
		return;

	b1 = b2 = NULL;
	ZONE_LOCK(zone);
	critical_enter();
	cache = &zone->uz_cpu[curcpu];
	if (cache->uc_allocbucket) {
		if (cache->uc_allocbucket->ub_cnt != 0)
			LIST_INSERT_HEAD(&zone->uz_buckets,
			    cache->uc_allocbucket, ub_link);
		else
			b1 = cache->uc_allocbucket;
		cache->uc_allocbucket = NULL;
	}
	if (cache->uc_freebucket) {
		if (cache->uc_freebucket->ub_cnt != 0)
			LIST_INSERT_HEAD(&zone->uz_buckets,
			    cache->uc_freebucket, ub_link);
		else
			b2 = cache->uc_freebucket;
		cache->uc_freebucket = NULL;
	}
	critical_exit();
	ZONE_UNLOCK(zone);
	if (b1)
		bucket_free(zone, b1, NULL);
	if (b2)
		bucket_free(zone, b2, NULL);
}

/*
 * Safely drain per-CPU caches of a zone(s) to alloc bucket.
 * This is an expensive call because it needs to bind to all CPUs
 * one by one and enter a critical section on each of them in order
 * to safely access their cache buckets.
 * Zone lock must not be held on call this function.
 */
static void
cache_drain_safe(uma_zone_t zone)
{
	int cpu;

	/*
	 * Polite bucket sizes shrinking was not enouth, shrink aggressively.
	 */
	if (zone)
		cache_shrink(zone);
	else
		zone_foreach(cache_shrink);

	CPU_FOREACH(cpu) {
		thread_lock(curthread);
		sched_bind(curthread, cpu);
		thread_unlock(curthread);

		if (zone)
			cache_drain_safe_cpu(zone);
		else
			zone_foreach(cache_drain_safe_cpu);
	}
	thread_lock(curthread);
	sched_unbind(curthread);
	thread_unlock(curthread);
}

/*
 * Drain the cached buckets from a zone.  Expects a locked zone on entry.
 */
static void
bucket_cache_drain(uma_zone_t zone)
{
	uma_bucket_t bucket;

	/*
	 * Drain the bucket queues and free the buckets, we just keep two per
	 * cpu (alloc/free).
	 */
	while ((bucket = LIST_FIRST(&zone->uz_buckets)) != NULL) {
		LIST_REMOVE(bucket, ub_link);
		ZONE_UNLOCK(zone);
		bucket_drain(zone, bucket);
		bucket_free(zone, bucket, NULL);
		ZONE_LOCK(zone);
	}

	/*
	 * Shrink further bucket sizes.  Price of single zone lock collision
	 * is probably lower then price of global cache drain.
	 */
	if (zone->uz_count > zone->uz_count_min)
		zone->uz_count--;
}

static void
keg_free_slab(uma_keg_t keg, uma_slab_t slab, int start)
{
	uint8_t *mem;
	int i;
	uint8_t flags;

	mem = slab->us_data;
	flags = slab->us_flags;
	i = start;
	if (keg->uk_fini != NULL) {
		for (i--; i > -1; i--)
			keg->uk_fini(slab->us_data + (keg->uk_rsize * i),
			    keg->uk_size);
	}
	if (keg->uk_flags & UMA_ZONE_OFFPAGE)
		zone_free_item(keg->uk_slabzone, slab, NULL, SKIP_NONE);
#ifdef UMA_DEBUG
	printf("%s: Returning %d bytes.\n", keg->uk_name,
	    PAGE_SIZE * keg->uk_ppera);
#endif
	keg->uk_freef(mem, PAGE_SIZE * keg->uk_ppera, flags);
}

/*
 * Frees pages from a keg back to the system.  This is done on demand from
 * the pageout daemon.
 *
 * Returns nothing.
 */
static void
keg_drain(uma_keg_t keg)
{
	struct slabhead freeslabs = { 0 };
	uma_slab_t slab;
	uma_slab_t n;

	/*
	 * We don't want to take pages from statically allocated kegs at this
	 * time
	 */
	if (keg->uk_flags & UMA_ZONE_NOFREE || keg->uk_freef == NULL)
		return;

#ifdef UMA_DEBUG
	printf("%s free items: %u\n", keg->uk_name, keg->uk_free);
#endif
	KEG_LOCK(keg);
	if (keg->uk_free == 0)
		goto finished;

	slab = LIST_FIRST(&keg->uk_free_slab);
	while (slab) {
		n = LIST_NEXT(slab, us_link);

		/* We have no where to free these to */
		if (slab->us_flags & UMA_SLAB_BOOT) {
			slab = n;
			continue;
		}

		LIST_REMOVE(slab, us_link);
		keg->uk_pages -= keg->uk_ppera;
		keg->uk_free -= keg->uk_ipers;

		if (keg->uk_flags & UMA_ZONE_HASH)
			UMA_HASH_REMOVE(&keg->uk_hash, slab, slab->us_data);

		SLIST_INSERT_HEAD(&freeslabs, slab, us_hlink);

		slab = n;
	}
finished:
	KEG_UNLOCK(keg);

	while ((slab = SLIST_FIRST(&freeslabs)) != NULL) {
		SLIST_REMOVE(&freeslabs, slab, uma_slab, us_hlink);
		keg_free_slab(keg, slab, keg->uk_ipers);
	}
}

static void
zone_drain_wait(uma_zone_t zone, int waitok)
{

	/*
	 * Set draining to interlock with zone_dtor() so we can release our
	 * locks as we go.  Only dtor() should do a WAITOK call since it
	 * is the only call that knows the structure will still be available
	 * when it wakes up.
	 */
	ZONE_LOCK(zone);
	while (zone->uz_flags & UMA_ZFLAG_DRAINING) {
		if (waitok == M_NOWAIT)
			goto out;
		msleep(zone, zone->uz_lockptr, PVM, "zonedrain", 1);
	}
	zone->uz_flags |= UMA_ZFLAG_DRAINING;
	bucket_cache_drain(zone);
	ZONE_UNLOCK(zone);
	/*
	 * The DRAINING flag protects us from being freed while
	 * we're running.  Normally the uma_rwlock would protect us but we
	 * must be able to release and acquire the right lock for each keg.
	 */
	zone_foreach_keg(zone, &keg_drain);
	ZONE_LOCK(zone);
	zone->uz_flags &= ~UMA_ZFLAG_DRAINING;
	wakeup(zone);
out:
	ZONE_UNLOCK(zone);
}

void
zone_drain(uma_zone_t zone)
{

	zone_drain_wait(zone, M_NOWAIT);
}

/*
 * Allocate a new slab for a keg.  This does not insert the slab onto a list.
 *
 * Arguments:
 *	wait  Shall we wait?
 *
 * Returns:
 *	The slab that was allocated or NULL if there is no memory and the
 *	caller specified M_NOWAIT.
 */
static uma_slab_t
keg_alloc_slab(uma_keg_t keg, uma_zone_t zone, int wait)
{
	uma_alloc allocf;
	uma_slab_t slab;
	uint8_t *mem;
	uint8_t flags;
	int i;

	mtx_assert(&keg->uk_lock, MA_OWNED);
	slab = NULL;
	mem = NULL;

#ifdef UMA_DEBUG
	printf("alloc_slab:  Allocating a new slab for %s\n", keg->uk_name);
#endif
	allocf = keg->uk_allocf;
	KEG_UNLOCK(keg);

	if (keg->uk_flags & UMA_ZONE_OFFPAGE) {
		slab = zone_alloc_item(keg->uk_slabzone, NULL, wait);
		if (slab == NULL)
			goto out;
	}

	/*
	 * This reproduces the old vm_zone behavior of zero filling pages the
	 * first time they are added to a zone.
	 *
	 * Malloced items are zeroed in uma_zalloc.
	 */

	if ((keg->uk_flags & UMA_ZONE_MALLOC) == 0)
		wait |= M_ZERO;
	else
		wait &= ~M_ZERO;

	if (keg->uk_flags & UMA_ZONE_NODUMP)
		wait |= M_NODUMP;

	/* zone is passed for legacy reasons. */
	mem = allocf(zone, keg->uk_ppera * PAGE_SIZE, &flags, wait);
	if (mem == NULL) {
		if (keg->uk_flags & UMA_ZONE_OFFPAGE)
			zone_free_item(keg->uk_slabzone, slab, NULL, SKIP_NONE);
		slab = NULL;
		goto out;
	}

	/* Point the slab into the allocated memory */
	if (!(keg->uk_flags & UMA_ZONE_OFFPAGE))
		slab = (uma_slab_t )(mem + keg->uk_pgoff);

	if (keg->uk_flags & UMA_ZONE_VTOSLAB)
		for (i = 0; i < keg->uk_ppera; i++)
			vsetslab((vm_offset_t)mem + (i * PAGE_SIZE), slab);

	slab->us_keg = keg;
	slab->us_data = mem;
	slab->us_freecount = keg->uk_ipers;
	slab->us_flags = flags;
	BIT_FILL(SLAB_SETSIZE, &slab->us_free);
#ifdef INVARIANTS
	BIT_ZERO(SLAB_SETSIZE, &slab->us_debugfree);
#endif

	if (keg->uk_init != NULL) {
		for (i = 0; i < keg->uk_ipers; i++)
			if (keg->uk_init(slab->us_data + (keg->uk_rsize * i),
			    keg->uk_size, wait) != 0)
				break;
		if (i != keg->uk_ipers) {
			keg_free_slab(keg, slab, i);
			slab = NULL;
			goto out;
		}
	}
out:
	KEG_LOCK(keg);

	if (slab != NULL) {
		if (keg->uk_flags & UMA_ZONE_HASH)
			UMA_HASH_INSERT(&keg->uk_hash, slab, mem);

		keg->uk_pages += keg->uk_ppera;
		keg->uk_free += keg->uk_ipers;
	}

	return (slab);
}

/*
 * This function is intended to be used early on in place of page_alloc() so
 * that we may use the boot time page cache to satisfy allocations before
 * the VM is ready.
 */
static void *
startup_alloc(uma_zone_t zone, vm_size_t bytes, uint8_t *pflag, int wait)
{
	uma_keg_t keg;
	uma_slab_t tmps;
	int pages, check_pages;

	keg = zone_first_keg(zone);
	pages = howmany(bytes, PAGE_SIZE);
	check_pages = pages - 1;
	KASSERT(pages > 0, ("startup_alloc can't reserve 0 pages\n"));

	/*
	 * Check our small startup cache to see if it has pages remaining.
	 */
	mtx_lock(&uma_boot_pages_mtx);

	/* First check if we have enough room. */
	tmps = LIST_FIRST(&uma_boot_pages);
	while (tmps != NULL && check_pages-- > 0)
		tmps = LIST_NEXT(tmps, us_link);
	if (tmps != NULL) {
		/*
		 * It's ok to lose tmps references.  The last one will
		 * have tmps->us_data pointing to the start address of
		 * "pages" contiguous pages of memory.
		 */
		while (pages-- > 0) {
			tmps = LIST_FIRST(&uma_boot_pages);
			LIST_REMOVE(tmps, us_link);
		}
		mtx_unlock(&uma_boot_pages_mtx);
		*pflag = tmps->us_flags;
		return (tmps->us_data);
	}
	mtx_unlock(&uma_boot_pages_mtx);
	if (booted < UMA_STARTUP2)
		panic("UMA: Increase vm.boot_pages");
	/*
	 * Now that we've booted reset these users to their real allocator.
	 */
#ifdef UMA_MD_SMALL_ALLOC
	keg->uk_allocf = (keg->uk_ppera > 1) ? page_alloc : uma_small_alloc;
#else
	keg->uk_allocf = page_alloc;
#endif
	return keg->uk_allocf(zone, bytes, pflag, wait);
}

/*
 * Allocates a number of pages from the system
 *
 * Arguments:
 *	bytes  The number of bytes requested
 *	wait  Shall we wait?
 *
 * Returns:
 *	A pointer to the alloced memory or possibly
 *	NULL if M_NOWAIT is set.
 */
static void *
page_alloc(uma_zone_t zone, vm_size_t bytes, uint8_t *pflag, int wait)
{
	void *p;	/* Returned page */

	*pflag = UMA_SLAB_KMEM;
	p = (void *) kmem_malloc(kmem_arena, bytes, wait);

	return (p);
}

/*
 * Allocates a number of pages from within an object
 *
 * Arguments:
 *	bytes  The number of bytes requested
 *	wait   Shall we wait?
 *
 * Returns:
 *	A pointer to the alloced memory or possibly
 *	NULL if M_NOWAIT is set.
 */
static void *
noobj_alloc(uma_zone_t zone, vm_size_t bytes, uint8_t *flags, int wait)
{
	TAILQ_HEAD(, vm_page) alloctail;
	u_long npages;
	vm_offset_t retkva, zkva;
	vm_page_t p, p_next;
	uma_keg_t keg;

	TAILQ_INIT(&alloctail);
	keg = zone_first_keg(zone);

	npages = howmany(bytes, PAGE_SIZE);
	while (npages > 0) {
		p = vm_page_alloc(NULL, 0, VM_ALLOC_INTERRUPT |
		    VM_ALLOC_WIRED | VM_ALLOC_NOOBJ);
		if (p != NULL) {
			/*
			 * Since the page does not belong to an object, its
			 * listq is unused.
			 */
			TAILQ_INSERT_TAIL(&alloctail, p, listq);
			npages--;
			continue;
		}
		if (wait & M_WAITOK) {
			VM_WAIT;
			continue;
		}

		/*
		 * Page allocation failed, free intermediate pages and
		 * exit.
		 */
		TAILQ_FOREACH_SAFE(p, &alloctail, listq, p_next) {
			vm_page_unwire(p, PQ_NONE);
			vm_page_free(p); 
		}
		return (NULL);
	}
	*flags = UMA_SLAB_PRIV;
	zkva = keg->uk_kva +
	    atomic_fetchadd_long(&keg->uk_offset, round_page(bytes));
	retkva = zkva;
	TAILQ_FOREACH(p, &alloctail, listq) {
		pmap_qenter(zkva, &p, 1);
		zkva += PAGE_SIZE;
	}

	return ((void *)retkva);
}

/*
 * Frees a number of pages to the system
 *
 * Arguments:
 *	mem   A pointer to the memory to be freed
 *	size  The size of the memory being freed
 *	flags The original p->us_flags field
 *
 * Returns:
 *	Nothing
 */
static void
page_free(void *mem, vm_size_t size, uint8_t flags)
{
	struct vmem *vmem;

	if (flags & UMA_SLAB_KMEM)
		vmem = kmem_arena;
	else if (flags & UMA_SLAB_KERNEL)
		vmem = kernel_arena;
	else
		panic("UMA: page_free used with invalid flags %d", flags);

	kmem_free(vmem, (vm_offset_t)mem, size);
}

/*
 * Zero fill initializer
 *
 * Arguments/Returns follow uma_init specifications
 */
static int
zero_init(void *mem, int size, int flags)
{
	bzero(mem, size);
	return (0);
}

/*
 * Finish creating a small uma keg.  This calculates ipers, and the keg size.
 *
 * Arguments
 *	keg  The zone we should initialize
 *
 * Returns
 *	Nothing
 */
static void
keg_small_init(uma_keg_t keg)
{
	u_int rsize;
	u_int memused;
	u_int wastedspace;
	u_int shsize;

	if (keg->uk_flags & UMA_ZONE_PCPU) {
		u_int ncpus = (mp_maxid + 1) ? (mp_maxid + 1) : MAXCPU;

		keg->uk_slabsize = sizeof(struct pcpu);
		keg->uk_ppera = howmany(ncpus * sizeof(struct pcpu),
		    PAGE_SIZE);
	} else {
		keg->uk_slabsize = UMA_SLAB_SIZE;
		keg->uk_ppera = 1;
	}

	/*
	 * Calculate the size of each allocation (rsize) according to
	 * alignment.  If the requested size is smaller than we have
	 * allocation bits for we round it up.
	 */
	rsize = keg->uk_size;
	if (rsize < keg->uk_slabsize / SLAB_SETSIZE)
		rsize = keg->uk_slabsize / SLAB_SETSIZE;
	if (rsize & keg->uk_align)
		rsize = (rsize & ~keg->uk_align) + (keg->uk_align + 1);
	keg->uk_rsize = rsize;

	KASSERT((keg->uk_flags & UMA_ZONE_PCPU) == 0 ||
	    keg->uk_rsize < sizeof(struct pcpu),
	    ("%s: size %u too large", __func__, keg->uk_rsize));

	if (keg->uk_flags & UMA_ZONE_OFFPAGE)
		shsize = 0;
	else 
		shsize = sizeof(struct uma_slab);

	keg->uk_ipers = (keg->uk_slabsize - shsize) / rsize;
	KASSERT(keg->uk_ipers > 0 && keg->uk_ipers <= SLAB_SETSIZE,
	    ("%s: keg->uk_ipers %u", __func__, keg->uk_ipers));

	memused = keg->uk_ipers * rsize + shsize;
	wastedspace = keg->uk_slabsize - memused;

	/*
	 * We can't do OFFPAGE if we're internal or if we've been
	 * asked to not go to the VM for buckets.  If we do this we
	 * may end up going to the VM  for slabs which we do not
	 * want to do if we're UMA_ZFLAG_CACHEONLY as a result
	 * of UMA_ZONE_VM, which clearly forbids it.
	 */
	if ((keg->uk_flags & UMA_ZFLAG_INTERNAL) ||
	    (keg->uk_flags & UMA_ZFLAG_CACHEONLY))
		return;

	/*
	 * See if using an OFFPAGE slab will limit our waste.  Only do
	 * this if it permits more items per-slab.
	 *
	 * XXX We could try growing slabsize to limit max waste as well.
	 * Historically this was not done because the VM could not
	 * efficiently handle contiguous allocations.
	 */
	if ((wastedspace >= keg->uk_slabsize / UMA_MAX_WASTE) &&
	    (keg->uk_ipers < (keg->uk_slabsize / keg->uk_rsize))) {
		keg->uk_ipers = keg->uk_slabsize / keg->uk_rsize;
		KASSERT(keg->uk_ipers > 0 && keg->uk_ipers <= SLAB_SETSIZE,
		    ("%s: keg->uk_ipers %u", __func__, keg->uk_ipers));
#ifdef UMA_DEBUG
		printf("UMA decided we need offpage slab headers for "
		    "keg: %s, calculated wastedspace = %d, "
		    "maximum wasted space allowed = %d, "
		    "calculated ipers = %d, "
		    "new wasted space = %d\n", keg->uk_name, wastedspace,
		    keg->uk_slabsize / UMA_MAX_WASTE, keg->uk_ipers,
		    keg->uk_slabsize - keg->uk_ipers * keg->uk_rsize);
#endif
		keg->uk_flags |= UMA_ZONE_OFFPAGE;
	}

	if ((keg->uk_flags & UMA_ZONE_OFFPAGE) &&
	    (keg->uk_flags & UMA_ZONE_VTOSLAB) == 0)
		keg->uk_flags |= UMA_ZONE_HASH;
}

/*
 * Finish creating a large (> UMA_SLAB_SIZE) uma kegs.  Just give in and do
 * OFFPAGE for now.  When I can allow for more dynamic slab sizes this will be
 * more complicated.
 *
 * Arguments
 *	keg  The keg we should initialize
 *
 * Returns
 *	Nothing
 */
static void
keg_large_init(uma_keg_t keg)
{
	u_int shsize;

	KASSERT(keg != NULL, ("Keg is null in keg_large_init"));
	KASSERT((keg->uk_flags & UMA_ZFLAG_CACHEONLY) == 0,
	    ("keg_large_init: Cannot large-init a UMA_ZFLAG_CACHEONLY keg"));
	KASSERT((keg->uk_flags & UMA_ZONE_PCPU) == 0,
	    ("%s: Cannot large-init a UMA_ZONE_PCPU keg", __func__));

	keg->uk_ppera = howmany(keg->uk_size, PAGE_SIZE);
	keg->uk_slabsize = keg->uk_ppera * PAGE_SIZE;
	keg->uk_ipers = 1;
	keg->uk_rsize = keg->uk_size;

	/* We can't do OFFPAGE if we're internal, bail out here. */
	if (keg->uk_flags & UMA_ZFLAG_INTERNAL)
		return;

	/* Check whether we have enough space to not do OFFPAGE. */
	if ((keg->uk_flags & UMA_ZONE_OFFPAGE) == 0) {
		shsize = sizeof(struct uma_slab);
		if (shsize & UMA_ALIGN_PTR)
			shsize = (shsize & ~UMA_ALIGN_PTR) +
			    (UMA_ALIGN_PTR + 1);

		if ((PAGE_SIZE * keg->uk_ppera) - keg->uk_rsize < shsize)
			keg->uk_flags |= UMA_ZONE_OFFPAGE;
	}

	if ((keg->uk_flags & UMA_ZONE_OFFPAGE) &&
	    (keg->uk_flags & UMA_ZONE_VTOSLAB) == 0)
		keg->uk_flags |= UMA_ZONE_HASH;
}

static void
keg_cachespread_init(uma_keg_t keg)
{
	int alignsize;
	int trailer;
	int pages;
	int rsize;

	KASSERT((keg->uk_flags & UMA_ZONE_PCPU) == 0,
	    ("%s: Cannot cachespread-init a UMA_ZONE_PCPU keg", __func__));

	alignsize = keg->uk_align + 1;
	rsize = keg->uk_size;
	/*
	 * We want one item to start on every align boundary in a page.  To
	 * do this we will span pages.  We will also extend the item by the
	 * size of align if it is an even multiple of align.  Otherwise, it
	 * would fall on the same boundary every time.
	 */
	if (rsize & keg->uk_align)
		rsize = (rsize & ~keg->uk_align) + alignsize;
	if ((rsize & alignsize) == 0)
		rsize += alignsize;
	trailer = rsize - keg->uk_size;
	pages = (rsize * (PAGE_SIZE / alignsize)) / PAGE_SIZE;
	pages = MIN(pages, (128 * 1024) / PAGE_SIZE);
	keg->uk_rsize = rsize;
	keg->uk_ppera = pages;
	keg->uk_slabsize = UMA_SLAB_SIZE;
	keg->uk_ipers = ((pages * PAGE_SIZE) + trailer) / rsize;
	keg->uk_flags |= UMA_ZONE_OFFPAGE | UMA_ZONE_VTOSLAB;
	KASSERT(keg->uk_ipers <= SLAB_SETSIZE,
	    ("%s: keg->uk_ipers too high(%d) increase max_ipers", __func__,
	    keg->uk_ipers));
}

/*
 * Keg header ctor.  This initializes all fields, locks, etc.  And inserts
 * the keg onto the global keg list.
 *
 * Arguments/Returns follow uma_ctor specifications
 *	udata  Actually uma_kctor_args
 */
static int
keg_ctor(void *mem, int size, void *udata, int flags)
{
	struct uma_kctor_args *arg = udata;
	uma_keg_t keg = mem;
	uma_zone_t zone;

	bzero(keg, size);
	keg->uk_size = arg->size;
	keg->uk_init = arg->uminit;
	keg->uk_fini = arg->fini;
	keg->uk_align = arg->align;
	keg->uk_free = 0;
	keg->uk_reserve = 0;
	keg->uk_pages = 0;
	keg->uk_flags = arg->flags;
	keg->uk_allocf = page_alloc;
	keg->uk_freef = page_free;
	keg->uk_slabzone = NULL;

	/*
	 * The master zone is passed to us at keg-creation time.
	 */
	zone = arg->zone;
	keg->uk_name = zone->uz_name;

	if (arg->flags & UMA_ZONE_VM)
		keg->uk_flags |= UMA_ZFLAG_CACHEONLY;

	if (arg->flags & UMA_ZONE_ZINIT)
		keg->uk_init = zero_init;

	if (arg->flags & UMA_ZONE_MALLOC)
		keg->uk_flags |= UMA_ZONE_VTOSLAB;

	if (arg->flags & UMA_ZONE_PCPU)
#ifdef SMP
		keg->uk_flags |= UMA_ZONE_OFFPAGE;
#else
		keg->uk_flags &= ~UMA_ZONE_PCPU;
#endif

	if (keg->uk_flags & UMA_ZONE_CACHESPREAD) {
		keg_cachespread_init(keg);
	} else {
		if (keg->uk_size > (UMA_SLAB_SIZE - sizeof(struct uma_slab)))
			keg_large_init(keg);
		else
			keg_small_init(keg);
	}

	if (keg->uk_flags & UMA_ZONE_OFFPAGE)
		keg->uk_slabzone = slabzone;

	/*
	 * If we haven't booted yet we need allocations to go through the
	 * startup cache until the vm is ready.
	 */
	if (keg->uk_ppera == 1) {
#ifdef UMA_MD_SMALL_ALLOC
		keg->uk_allocf = uma_small_alloc;
		keg->uk_freef = uma_small_free;

		if (booted < UMA_STARTUP)
			keg->uk_allocf = startup_alloc;
#else
		if (booted < UMA_STARTUP2)
			keg->uk_allocf = startup_alloc;
#endif
	} else if (booted < UMA_STARTUP2 &&
	    (keg->uk_flags & UMA_ZFLAG_INTERNAL))
		keg->uk_allocf = startup_alloc;

	/*
	 * Initialize keg's lock
	 */
	KEG_LOCK_INIT(keg, (arg->flags & UMA_ZONE_MTXCLASS));

	/*
	 * If we're putting the slab header in the actual page we need to
	 * figure out where in each page it goes.  This calculates a right
	 * justified offset into the memory on an ALIGN_PTR boundary.
	 */
	if (!(keg->uk_flags & UMA_ZONE_OFFPAGE)) {
		u_int totsize;

		/* Size of the slab struct and free list */
		totsize = sizeof(struct uma_slab);

		if (totsize & UMA_ALIGN_PTR)
			totsize = (totsize & ~UMA_ALIGN_PTR) +
			    (UMA_ALIGN_PTR + 1);
		keg->uk_pgoff = (PAGE_SIZE * keg->uk_ppera) - totsize;

		/*
		 * The only way the following is possible is if with our
		 * UMA_ALIGN_PTR adjustments we are now bigger than
		 * UMA_SLAB_SIZE.  I haven't checked whether this is
		 * mathematically possible for all cases, so we make
		 * sure here anyway.
		 */
		totsize = keg->uk_pgoff + sizeof(struct uma_slab);
		if (totsize > PAGE_SIZE * keg->uk_ppera) {
			printf("zone %s ipers %d rsize %d size %d\n",
			    zone->uz_name, keg->uk_ipers, keg->uk_rsize,
			    keg->uk_size);
			panic("UMA slab won't fit.");
		}
	}

	if (keg->uk_flags & UMA_ZONE_HASH)
		hash_alloc(&keg->uk_hash);

#ifdef UMA_DEBUG
	printf("UMA: %s(%p) size %d(%d) flags %#x ipers %d ppera %d out %d free %d\n",
	    zone->uz_name, zone, keg->uk_size, keg->uk_rsize, keg->uk_flags,
	    keg->uk_ipers, keg->uk_ppera,
	    (keg->uk_ipers * keg->uk_pages) - keg->uk_free, keg->uk_free);
#endif

	LIST_INSERT_HEAD(&keg->uk_zones, zone, uz_link);

	rw_wlock(&uma_rwlock);
	LIST_INSERT_HEAD(&uma_kegs, keg, uk_link);
	rw_wunlock(&uma_rwlock);
	return (0);
}

/*
 * Zone header ctor.  This initializes all fields, locks, etc.
 *
 * Arguments/Returns follow uma_ctor specifications
 *	udata  Actually uma_zctor_args
 */
static int
zone_ctor(void *mem, int size, void *udata, int flags)
{
	struct uma_zctor_args *arg = udata;
	uma_zone_t zone = mem;
	uma_zone_t z;
	uma_keg_t keg;

	bzero(zone, size);
	zone->uz_name = arg->name;
	zone->uz_ctor = arg->ctor;
	zone->uz_dtor = arg->dtor;
	zone->uz_slab = zone_fetch_slab;
	zone->uz_init = NULL;
	zone->uz_fini = NULL;
	zone->uz_allocs = 0;
	zone->uz_frees = 0;
	zone->uz_fails = 0;
	zone->uz_sleeps = 0;
	zone->uz_count = 0;
	zone->uz_count_min = 0;
	zone->uz_flags = 0;
	zone->uz_warning = NULL;
	timevalclear(&zone->uz_ratecheck);
	keg = arg->keg;

	ZONE_LOCK_INIT(zone, (arg->flags & UMA_ZONE_MTXCLASS));

	/*
	 * This is a pure cache zone, no kegs.
	 */
	if (arg->import) {
		if (arg->flags & UMA_ZONE_VM)
			arg->flags |= UMA_ZFLAG_CACHEONLY;
		zone->uz_flags = arg->flags;
		zone->uz_size = arg->size;
		zone->uz_import = arg->import;
		zone->uz_release = arg->release;
		zone->uz_arg = arg->arg;
		zone->uz_lockptr = &zone->uz_lock;
		rw_wlock(&uma_rwlock);
		LIST_INSERT_HEAD(&uma_cachezones, zone, uz_link);
		rw_wunlock(&uma_rwlock);
		goto out;
	}

	/*
	 * Use the regular zone/keg/slab allocator.
	 */
	zone->uz_import = (uma_import)zone_import;
	zone->uz_release = (uma_release)zone_release;
	zone->uz_arg = zone; 

	if (arg->flags & UMA_ZONE_SECONDARY) {
		KASSERT(arg->keg != NULL, ("Secondary zone on zero'd keg"));
		zone->uz_init = arg->uminit;
		zone->uz_fini = arg->fini;
		zone->uz_lockptr = &keg->uk_lock;
		zone->uz_flags |= UMA_ZONE_SECONDARY;
		rw_wlock(&uma_rwlock);
		ZONE_LOCK(zone);
		LIST_FOREACH(z, &keg->uk_zones, uz_link) {
			if (LIST_NEXT(z, uz_link) == NULL) {
				LIST_INSERT_AFTER(z, zone, uz_link);
				break;
			}
		}
		ZONE_UNLOCK(zone);
		rw_wunlock(&uma_rwlock);
	} else if (keg == NULL) {
		if ((keg = uma_kcreate(zone, arg->size, arg->uminit, arg->fini,
		    arg->align, arg->flags)) == NULL)
			return (ENOMEM);
	} else {
		struct uma_kctor_args karg;
		int error;

		/* We should only be here from uma_startup() */
		karg.size = arg->size;
		karg.uminit = arg->uminit;
		karg.fini = arg->fini;
		karg.align = arg->align;
		karg.flags = arg->flags;
		karg.zone = zone;
		error = keg_ctor(arg->keg, sizeof(struct uma_keg), &karg,
		    flags);
		if (error)
			return (error);
	}

	/*
	 * Link in the first keg.
	 */
	zone->uz_klink.kl_keg = keg;
	LIST_INSERT_HEAD(&zone->uz_kegs, &zone->uz_klink, kl_link);
	zone->uz_lockptr = &keg->uk_lock;
	zone->uz_size = keg->uk_size;
	zone->uz_flags |= (keg->uk_flags &
	    (UMA_ZONE_INHERIT | UMA_ZFLAG_INHERIT));

	/*
	 * Some internal zones don't have room allocated for the per cpu
	 * caches.  If we're internal, bail out here.
	 */
	if (keg->uk_flags & UMA_ZFLAG_INTERNAL) {
		KASSERT((zone->uz_flags & UMA_ZONE_SECONDARY) == 0,
		    ("Secondary zone requested UMA_ZFLAG_INTERNAL"));
		return (0);
	}

out:
	if ((arg->flags & UMA_ZONE_MAXBUCKET) == 0)
		zone->uz_count = bucket_select(zone->uz_size);
	else
		zone->uz_count = BUCKET_MAX;
	zone->uz_count_min = zone->uz_count;

	return (0);
}

/*
 * Keg header dtor.  This frees all data, destroys locks, frees the hash
 * table and removes the keg from the global list.
 *
 * Arguments/Returns follow uma_dtor specifications
 *	udata  unused
 */
static void
keg_dtor(void *arg, int size, void *udata)
{
	uma_keg_t keg;

	keg = (uma_keg_t)arg;
	KEG_LOCK(keg);
	if (keg->uk_free != 0) {
		printf("Freed UMA keg (%s) was not empty (%d items). "
		    " Lost %d pages of memory.\n",
		    keg->uk_name ? keg->uk_name : "",
		    keg->uk_free, keg->uk_pages);
	}
	KEG_UNLOCK(keg);

	hash_free(&keg->uk_hash);

	KEG_LOCK_FINI(keg);
}

/*
 * Zone header dtor.
 *
 * Arguments/Returns follow uma_dtor specifications
 *	udata  unused
 */
static void
zone_dtor(void *arg, int size, void *udata)
{
	uma_klink_t klink;
	uma_zone_t zone;
	uma_keg_t keg;

	zone = (uma_zone_t)arg;
	keg = zone_first_keg(zone);

	if (!(zone->uz_flags & UMA_ZFLAG_INTERNAL))
		cache_drain(zone);

	rw_wlock(&uma_rwlock);
	LIST_REMOVE(zone, uz_link);
	rw_wunlock(&uma_rwlock);
	/*
	 * XXX there are some races here where
	 * the zone can be drained but zone lock
	 * released and then refilled before we
	 * remove it... we dont care for now
	 */
	zone_drain_wait(zone, M_WAITOK);
	/*
	 * Unlink all of our kegs.
	 */
	while ((klink = LIST_FIRST(&zone->uz_kegs)) != NULL) {
		klink->kl_keg = NULL;
		LIST_REMOVE(klink, kl_link);
		if (klink == &zone->uz_klink)
			continue;
		free(klink, M_TEMP);
	}
	/*
	 * We only destroy kegs from non secondary zones.
	 */
	if (keg != NULL && (zone->uz_flags & UMA_ZONE_SECONDARY) == 0)  {
		rw_wlock(&uma_rwlock);
		LIST_REMOVE(keg, uk_link);
		rw_wunlock(&uma_rwlock);
		zone_free_item(kegs, keg, NULL, SKIP_NONE);
	}
	ZONE_LOCK_FINI(zone);
}

/*
 * Traverses every zone in the system and calls a callback
 *
 * Arguments:
 *	zfunc  A pointer to a function which accepts a zone
 *		as an argument.
 *
 * Returns:
 *	Nothing
 */
static void
zone_foreach(void (*zfunc)(uma_zone_t))
{
	uma_keg_t keg;
	uma_zone_t zone;

	rw_rlock(&uma_rwlock);
	LIST_FOREACH(keg, &uma_kegs, uk_link) {
		LIST_FOREACH(zone, &keg->uk_zones, uz_link)
			zfunc(zone);
	}
	rw_runlock(&uma_rwlock);
}

/* Public functions */
/* See uma.h */
void
uma_startup(void *bootmem, int boot_pages)
{
	struct uma_zctor_args args;
	uma_slab_t slab;
	int i;

#ifdef UMA_DEBUG
	printf("Creating uma keg headers zone and keg.\n");
#endif
	rw_init(&uma_rwlock, "UMA lock");

	/* "manually" create the initial zone */
	memset(&args, 0, sizeof(args));
	args.name = "UMA Kegs";
	args.size = sizeof(struct uma_keg);
	args.ctor = keg_ctor;
	args.dtor = keg_dtor;
	args.uminit = zero_init;
	args.fini = NULL;
	args.keg = &masterkeg;
	args.align = 32 - 1;
	args.flags = UMA_ZFLAG_INTERNAL;
	/* The initial zone has no Per cpu queues so it's smaller */
	zone_ctor(kegs, sizeof(struct uma_zone), &args, M_WAITOK);

#ifdef UMA_DEBUG
	printf("Filling boot free list.\n");
#endif
	for (i = 0; i < boot_pages; i++) {
		slab = (uma_slab_t)((uint8_t *)bootmem + (i * UMA_SLAB_SIZE));
		slab->us_data = (uint8_t *)slab;
		slab->us_flags = UMA_SLAB_BOOT;
		LIST_INSERT_HEAD(&uma_boot_pages, slab, us_link);
	}
	mtx_init(&uma_boot_pages_mtx, "UMA boot pages", NULL, MTX_DEF);

#ifdef UMA_DEBUG
	printf("Creating uma zone headers zone and keg.\n");
#endif
	args.name = "UMA Zones";
	args.size = sizeof(struct uma_zone) +
	    (sizeof(struct uma_cache) * (mp_maxid + 1));
	args.ctor = zone_ctor;
	args.dtor = zone_dtor;
	args.uminit = zero_init;
	args.fini = NULL;
	args.keg = NULL;
	args.align = 32 - 1;
	args.flags = UMA_ZFLAG_INTERNAL;
	/* The initial zone has no Per cpu queues so it's smaller */
	zone_ctor(zones, sizeof(struct uma_zone), &args, M_WAITOK);

#ifdef UMA_DEBUG
	printf("Creating slab and hash zones.\n");
#endif

	/* Now make a zone for slab headers */
	slabzone = uma_zcreate("UMA Slabs",
				sizeof(struct uma_slab),
				NULL, NULL, NULL, NULL,
				UMA_ALIGN_PTR, UMA_ZFLAG_INTERNAL);

	hashzone = uma_zcreate("UMA Hash",
	    sizeof(struct slabhead *) * UMA_HASH_SIZE_INIT,
	    NULL, NULL, NULL, NULL,
	    UMA_ALIGN_PTR, UMA_ZFLAG_INTERNAL);

	bucket_init();

	booted = UMA_STARTUP;

#ifdef UMA_DEBUG
	printf("UMA startup complete.\n");
#endif
}

/* see uma.h */
void
uma_startup2(void)
{
	booted = UMA_STARTUP2;
	bucket_enable();
	sx_init(&uma_drain_lock, "umadrain");
#ifdef UMA_DEBUG
	printf("UMA startup2 complete.\n");
#endif
}

/*
 * Initialize our callout handle
 *
 */

static void
uma_startup3(void)
{
#ifdef UMA_DEBUG
	printf("Starting callout.\n");
#endif
	callout_init(&uma_callout, 1);
	callout_reset(&uma_callout, UMA_TIMEOUT * hz, uma_timeout, NULL);
#ifdef UMA_DEBUG
	printf("UMA startup3 complete.\n");
#endif
}

static uma_keg_t
uma_kcreate(uma_zone_t zone, size_t size, uma_init uminit, uma_fini fini,
		int align, uint32_t flags)
{
	struct uma_kctor_args args;

	args.size = size;
	args.uminit = uminit;
	args.fini = fini;
	args.align = (align == UMA_ALIGN_CACHE) ? uma_align_cache : align;
	args.flags = flags;
	args.zone = zone;
	return (zone_alloc_item(kegs, &args, M_WAITOK));
}

/* See uma.h */
void
uma_set_align(int align)
{

	if (align != UMA_ALIGN_CACHE)
		uma_align_cache = align;
}

/* See uma.h */
uma_zone_t
uma_zcreate(const char *name, size_t size, uma_ctor ctor, uma_dtor dtor,
		uma_init uminit, uma_fini fini, int align, uint32_t flags)

{
	struct uma_zctor_args args;
	uma_zone_t res;
	bool locked;

	/* This stuff is essential for the zone ctor */
	memset(&args, 0, sizeof(args));
	args.name = name;
	args.size = size;
	args.ctor = ctor;
	args.dtor = dtor;
	args.uminit = uminit;
	args.fini = fini;
#ifdef  INVARIANTS
	/*
	 * If a zone is being created with an empty constructor and
	 * destructor, pass UMA constructor/destructor which checks for
	 * memory use after free.
	 */
	if ((!(flags & (UMA_ZONE_ZINIT | UMA_ZONE_NOFREE))) &&
	    ctor == NULL && dtor == NULL && uminit == NULL && fini == NULL) {
		args.ctor = trash_ctor;
		args.dtor = trash_dtor;
		args.uminit = trash_init;
		args.fini = trash_fini;
	}
#endif
	args.align = align;
	args.flags = flags;
	args.keg = NULL;

	if (booted < UMA_STARTUP2) {
		locked = false;
	} else {
		sx_slock(&uma_drain_lock);
		locked = true;
	}
	res = zone_alloc_item(zones, &args, M_WAITOK);
	if (locked)
		sx_sunlock(&uma_drain_lock);
	return (res);
}

/* See uma.h */
uma_zone_t
uma_zsecond_create(char *name, uma_ctor ctor, uma_dtor dtor,
		    uma_init zinit, uma_fini zfini, uma_zone_t master)
{
	struct uma_zctor_args args;
	uma_keg_t keg;
	uma_zone_t res;
	bool locked;

	keg = zone_first_keg(master);
	memset(&args, 0, sizeof(args));
	args.name = name;
	args.size = keg->uk_size;
	args.ctor = ctor;
	args.dtor = dtor;
	args.uminit = zinit;
	args.fini = zfini;
	args.align = keg->uk_align;
	args.flags = keg->uk_flags | UMA_ZONE_SECONDARY;
	args.keg = keg;

	if (booted < UMA_STARTUP2) {
		locked = false;
	} else {
		sx_slock(&uma_drain_lock);
		locked = true;
	}
	/* XXX Attaches only one keg of potentially many. */
	res = zone_alloc_item(zones, &args, M_WAITOK);
	if (locked)
		sx_sunlock(&uma_drain_lock);
	return (res);
}

/* See uma.h */
uma_zone_t
uma_zcache_create(char *name, int size, uma_ctor ctor, uma_dtor dtor,
		    uma_init zinit, uma_fini zfini, uma_import zimport,
		    uma_release zrelease, void *arg, int flags)
{
	struct uma_zctor_args args;

	memset(&args, 0, sizeof(args));
	args.name = name;
	args.size = size;
	args.ctor = ctor;
	args.dtor = dtor;
	args.uminit = zinit;
	args.fini = zfini;
	args.import = zimport;
	args.release = zrelease;
	args.arg = arg;
	args.align = 0;
	args.flags = flags;

	return (zone_alloc_item(zones, &args, M_WAITOK));
}

static void
zone_lock_pair(uma_zone_t a, uma_zone_t b)
{
	if (a < b) {
		ZONE_LOCK(a);
		mtx_lock_flags(b->uz_lockptr, MTX_DUPOK);
	} else {
		ZONE_LOCK(b);
		mtx_lock_flags(a->uz_lockptr, MTX_DUPOK);
	}
}

static void
zone_unlock_pair(uma_zone_t a, uma_zone_t b)
{

	ZONE_UNLOCK(a);
	ZONE_UNLOCK(b);
}

int
uma_zsecond_add(uma_zone_t zone, uma_zone_t master)
{
	uma_klink_t klink;
	uma_klink_t kl;
	int error;

	error = 0;
	klink = malloc(sizeof(*klink), M_TEMP, M_WAITOK | M_ZERO);

	zone_lock_pair(zone, master);
	/*
	 * zone must use vtoslab() to resolve objects and must already be
	 * a secondary.
	 */
	if ((zone->uz_flags & (UMA_ZONE_VTOSLAB | UMA_ZONE_SECONDARY))
	    != (UMA_ZONE_VTOSLAB | UMA_ZONE_SECONDARY)) {
		error = EINVAL;
		goto out;
	}
	/*
	 * The new master must also use vtoslab().
	 */
	if ((zone->uz_flags & UMA_ZONE_VTOSLAB) != UMA_ZONE_VTOSLAB) {
		error = EINVAL;
		goto out;
	}

	/*
	 * The underlying object must be the same size.  rsize
	 * may be different.
	 */
	if (master->uz_size != zone->uz_size) {
		error = E2BIG;
		goto out;
	}
	/*
	 * Put it at the end of the list.
	 */
	klink->kl_keg = zone_first_keg(master);
	LIST_FOREACH(kl, &zone->uz_kegs, kl_link) {
		if (LIST_NEXT(kl, kl_link) == NULL) {
			LIST_INSERT_AFTER(kl, klink, kl_link);
			break;
		}
	}
	klink = NULL;
	zone->uz_flags |= UMA_ZFLAG_MULTI;
	zone->uz_slab = zone_fetch_slab_multi;

out:
	zone_unlock_pair(zone, master);
	if (klink != NULL)
		free(klink, M_TEMP);

	return (error);
}


/* See uma.h */
void
uma_zdestroy(uma_zone_t zone)
{

	sx_slock(&uma_drain_lock);
	zone_free_item(zones, zone, NULL, SKIP_NONE);
	sx_sunlock(&uma_drain_lock);
}

/* See uma.h */
void *
uma_zalloc_arg(uma_zone_t zone, void *udata, int flags)
{
	void *item;
	uma_cache_t cache;
	uma_bucket_t bucket;
	int lockfail;
	int cpu;

	/* Enable entropy collection for RANDOM_ENABLE_UMA kernel option */
	random_harvest_fast_uma(&zone, sizeof(zone), 1, RANDOM_UMA);

	/* This is the fast path allocation */
#ifdef UMA_DEBUG_ALLOC_1
	printf("Allocating one item from %s(%p)\n", zone->uz_name, zone);
#endif
	CTR3(KTR_UMA, "uma_zalloc_arg thread %x zone %s flags %d", curthread,
	    zone->uz_name, flags);

	if (flags & M_WAITOK) {
		WITNESS_WARN(WARN_GIANTOK | WARN_SLEEPOK, NULL,
		    "uma_zalloc_arg: zone \"%s\"", zone->uz_name);
	}
	KASSERT(curthread->td_critnest == 0 || SCHEDULER_STOPPED(),
	    ("uma_zalloc_arg: called with spinlock or critical section held"));

#ifdef DEBUG_MEMGUARD
	if (memguard_cmp_zone(zone)) {
		item = memguard_alloc(zone->uz_size, flags);
		if (item != NULL) {
			if (zone->uz_init != NULL &&
			    zone->uz_init(item, zone->uz_size, flags) != 0)
				return (NULL);
			if (zone->uz_ctor != NULL &&
			    zone->uz_ctor(item, zone->uz_size, udata,
			    flags) != 0) {
			    	zone->uz_fini(item, zone->uz_size);
				return (NULL);
			}
			return (item);
		}
		/* This is unfortunate but should not be fatal. */
	}
#endif
	/*
	 * If possible, allocate from the per-CPU cache.  There are two
	 * requirements for safe access to the per-CPU cache: (1) the thread
	 * accessing the cache must not be preempted or yield during access,
	 * and (2) the thread must not migrate CPUs without switching which
	 * cache it accesses.  We rely on a critical section to prevent
	 * preemption and migration.  We release the critical section in
	 * order to acquire the zone mutex if we are unable to allocate from
	 * the current cache; when we re-acquire the critical section, we
	 * must detect and handle migration if it has occurred.
	 */
	critical_enter();
	cpu = curcpu;
	cache = &zone->uz_cpu[cpu];

zalloc_start:
	bucket = cache->uc_allocbucket;
	if (bucket != NULL && bucket->ub_cnt > 0) {
		bucket->ub_cnt--;
		item = bucket->ub_bucket[bucket->ub_cnt];
#ifdef INVARIANTS
		bucket->ub_bucket[bucket->ub_cnt] = NULL;
#endif
		KASSERT(item != NULL, ("uma_zalloc: Bucket pointer mangled."));
		cache->uc_allocs++;
		critical_exit();
		if (zone->uz_ctor != NULL &&
		    zone->uz_ctor(item, zone->uz_size, udata, flags) != 0) {
			atomic_add_long(&zone->uz_fails, 1);
			zone_free_item(zone, item, udata, SKIP_DTOR);
			return (NULL);
		}
#ifdef INVARIANTS
		uma_dbg_alloc(zone, NULL, item);
#endif
		if (flags & M_ZERO)
			uma_zero_item(item, zone);
		return (item);
	}

	/*
	 * We have run out of items in our alloc bucket.
	 * See if we can switch with our free bucket.
	 */
	bucket = cache->uc_freebucket;
	if (bucket != NULL && bucket->ub_cnt > 0) {
#ifdef UMA_DEBUG_ALLOC
		printf("uma_zalloc: Swapping empty with alloc.\n");
#endif
		cache->uc_freebucket = cache->uc_allocbucket;
		cache->uc_allocbucket = bucket;
		goto zalloc_start;
	}

	/*
	 * Discard any empty allocation bucket while we hold no locks.
	 */
	bucket = cache->uc_allocbucket;
	cache->uc_allocbucket = NULL;
	critical_exit();
	if (bucket != NULL)
		bucket_free(zone, bucket, udata);

	/* Short-circuit for zones without buckets and low memory. */
	if (zone->uz_count == 0 || bucketdisable)
		goto zalloc_item;

	/*
	 * Attempt to retrieve the item from the per-CPU cache has failed, so
	 * we must go back to the zone.  This requires the zone lock, so we
	 * must drop the critical section, then re-acquire it when we go back
	 * to the cache.  Since the critical section is released, we may be
	 * preempted or migrate.  As such, make sure not to maintain any
	 * thread-local state specific to the cache from prior to releasing
	 * the critical section.
	 */
	lockfail = 0;
	if (ZONE_TRYLOCK(zone) == 0) {
		/* Record contention to size the buckets. */
		ZONE_LOCK(zone);
		lockfail = 1;
	}
	critical_enter();
	cpu = curcpu;
	cache = &zone->uz_cpu[cpu];

	/*
	 * Since we have locked the zone we may as well send back our stats.
	 */
	atomic_add_long(&zone->uz_allocs, cache->uc_allocs);
	atomic_add_long(&zone->uz_frees, cache->uc_frees);
	cache->uc_allocs = 0;
	cache->uc_frees = 0;

	/* See if we lost the race to fill the cache. */
	if (cache->uc_allocbucket != NULL) {
		ZONE_UNLOCK(zone);
		goto zalloc_start;
	}

	/*
	 * Check the zone's cache of buckets.
	 */
	if ((bucket = LIST_FIRST(&zone->uz_buckets)) != NULL) {
		KASSERT(bucket->ub_cnt != 0,
		    ("uma_zalloc_arg: Returning an empty bucket."));

		LIST_REMOVE(bucket, ub_link);
		cache->uc_allocbucket = bucket;
		ZONE_UNLOCK(zone);
		goto zalloc_start;
	}
	/* We are no longer associated with this CPU. */
	critical_exit();

	/*
	 * We bump the uz count when the cache size is insufficient to
	 * handle the working set.
	 */
	if (lockfail && zone->uz_count < BUCKET_MAX)
		zone->uz_count++;
	ZONE_UNLOCK(zone);

	/*
	 * Now lets just fill a bucket and put it on the free list.  If that
	 * works we'll restart the allocation from the beginning and it
	 * will use the just filled bucket.
	 */
	bucket = zone_alloc_bucket(zone, udata, flags);
	if (bucket != NULL) {
		ZONE_LOCK(zone);
		critical_enter();
		cpu = curcpu;
		cache = &zone->uz_cpu[cpu];
		/*
		 * See if we lost the race or were migrated.  Cache the
		 * initialized bucket to make this less likely or claim
		 * the memory directly.
		 */
		if (cache->uc_allocbucket == NULL)
			cache->uc_allocbucket = bucket;
		else
			LIST_INSERT_HEAD(&zone->uz_buckets, bucket, ub_link);
		ZONE_UNLOCK(zone);
		goto zalloc_start;
	}

	/*
	 * We may not be able to get a bucket so return an actual item.
	 */
#ifdef UMA_DEBUG
	printf("uma_zalloc_arg: Bucketzone returned NULL\n");
#endif

zalloc_item:
	item = zone_alloc_item(zone, udata, flags);

	return (item);
}

static uma_slab_t
keg_fetch_slab(uma_keg_t keg, uma_zone_t zone, int flags)
{
	uma_slab_t slab;
	int reserve;

	mtx_assert(&keg->uk_lock, MA_OWNED);
	slab = NULL;
	reserve = 0;
	if ((flags & M_USE_RESERVE) == 0)
		reserve = keg->uk_reserve;

	for (;;) {
		/*
		 * Find a slab with some space.  Prefer slabs that are partially
		 * used over those that are totally full.  This helps to reduce
		 * fragmentation.
		 */
		if (keg->uk_free > reserve) {
			if (!LIST_EMPTY(&keg->uk_part_slab)) {
				slab = LIST_FIRST(&keg->uk_part_slab);
			} else {
				slab = LIST_FIRST(&keg->uk_free_slab);
				LIST_REMOVE(slab, us_link);
				LIST_INSERT_HEAD(&keg->uk_part_slab, slab,
				    us_link);
			}
			MPASS(slab->us_keg == keg);
			return (slab);
		}

		/*
		 * M_NOVM means don't ask at all!
		 */
		if (flags & M_NOVM)
			break;

		if (keg->uk_maxpages && keg->uk_pages >= keg->uk_maxpages) {
			keg->uk_flags |= UMA_ZFLAG_FULL;
			/*
			 * If this is not a multi-zone, set the FULL bit.
			 * Otherwise slab_multi() takes care of it.
			 */
			if ((zone->uz_flags & UMA_ZFLAG_MULTI) == 0) {
				zone->uz_flags |= UMA_ZFLAG_FULL;
				zone_log_warning(zone);
				zone_maxaction(zone);
			}
			if (flags & M_NOWAIT)
				break;
			zone->uz_sleeps++;
			msleep(keg, &keg->uk_lock, PVM, "keglimit", 0);
			continue;
		}
		slab = keg_alloc_slab(keg, zone, flags);
		/*
		 * If we got a slab here it's safe to mark it partially used
		 * and return.  We assume that the caller is going to remove
		 * at least one item.
		 */
		if (slab) {
			MPASS(slab->us_keg == keg);
			LIST_INSERT_HEAD(&keg->uk_part_slab, slab, us_link);
			return (slab);
		}
		/*
		 * We might not have been able to get a slab but another cpu
		 * could have while we were unlocked.  Check again before we
		 * fail.
		 */
		flags |= M_NOVM;
	}
	return (slab);
}

static uma_slab_t
zone_fetch_slab(uma_zone_t zone, uma_keg_t keg, int flags)
{
	uma_slab_t slab;

	if (keg == NULL) {
		keg = zone_first_keg(zone);
		KEG_LOCK(keg);
	}

	for (;;) {
		slab = keg_fetch_slab(keg, zone, flags);
		if (slab)
			return (slab);
		if (flags & (M_NOWAIT | M_NOVM))
			break;
	}
	KEG_UNLOCK(keg);
	return (NULL);
}

/*
 * uma_zone_fetch_slab_multi:  Fetches a slab from one available keg.  Returns
 * with the keg locked.  On NULL no lock is held.
 *
 * The last pointer is used to seed the search.  It is not required.
 */
static uma_slab_t
zone_fetch_slab_multi(uma_zone_t zone, uma_keg_t last, int rflags)
{
	uma_klink_t klink;
	uma_slab_t slab;
	uma_keg_t keg;
	int flags;
	int empty;
	int full;

	/*
	 * Don't wait on the first pass.  This will skip limit tests
	 * as well.  We don't want to block if we can find a provider
	 * without blocking.
	 */
	flags = (rflags & ~M_WAITOK) | M_NOWAIT;
	/*
	 * Use the last slab allocated as a hint for where to start
	 * the search.
	 */
	if (last != NULL) {
		slab = keg_fetch_slab(last, zone, flags);
		if (slab)
			return (slab);
		KEG_UNLOCK(last);
	}
	/*
	 * Loop until we have a slab incase of transient failures
	 * while M_WAITOK is specified.  I'm not sure this is 100%
	 * required but we've done it for so long now.
	 */
	for (;;) {
		empty = 0;
		full = 0;
		/*
		 * Search the available kegs for slabs.  Be careful to hold the
		 * correct lock while calling into the keg layer.
		 */
		LIST_FOREACH(klink, &zone->uz_kegs, kl_link) {
			keg = klink->kl_keg;
			KEG_LOCK(keg);
			if ((keg->uk_flags & UMA_ZFLAG_FULL) == 0) {
				slab = keg_fetch_slab(keg, zone, flags);
				if (slab)
					return (slab);
			}
			if (keg->uk_flags & UMA_ZFLAG_FULL)
				full++;
			else
				empty++;
			KEG_UNLOCK(keg);
		}
		if (rflags & (M_NOWAIT | M_NOVM))
			break;
		flags = rflags;
		/*
		 * All kegs are full.  XXX We can't atomically check all kegs
		 * and sleep so just sleep for a short period and retry.
		 */
		if (full && !empty) {
			ZONE_LOCK(zone);
			zone->uz_flags |= UMA_ZFLAG_FULL;
			zone->uz_sleeps++;
			zone_log_warning(zone);
			zone_maxaction(zone);
			msleep(zone, zone->uz_lockptr, PVM,
			    "zonelimit", hz/100);
			zone->uz_flags &= ~UMA_ZFLAG_FULL;
			ZONE_UNLOCK(zone);
			continue;
		}
	}
	return (NULL);
}

static void *
slab_alloc_item(uma_keg_t keg, uma_slab_t slab)
{
	void *item;
	uint8_t freei;

	MPASS(keg == slab->us_keg);
	mtx_assert(&keg->uk_lock, MA_OWNED);

	freei = BIT_FFS(SLAB_SETSIZE, &slab->us_free) - 1;
	BIT_CLR(SLAB_SETSIZE, freei, &slab->us_free);
	item = slab->us_data + (keg->uk_rsize * freei);
	slab->us_freecount--;
	keg->uk_free--;

	/* Move this slab to the full list */
	if (slab->us_freecount == 0) {
		LIST_REMOVE(slab, us_link);
		LIST_INSERT_HEAD(&keg->uk_full_slab, slab, us_link);
	}

	return (item);
}

static int
zone_import(uma_zone_t zone, void **bucket, int max, int flags)
{
	uma_slab_t slab;
	uma_keg_t keg;
	int i;

	slab = NULL;
	keg = NULL;
	/* Try to keep the buckets totally full */
	for (i = 0; i < max; ) {
		if ((slab = zone->uz_slab(zone, keg, flags)) == NULL)
			break;
		keg = slab->us_keg;
		while (slab->us_freecount && i < max) { 
			bucket[i++] = slab_alloc_item(keg, slab);
			if (keg->uk_free <= keg->uk_reserve)
				break;
		}
		/* Don't grab more than one slab at a time. */
		flags &= ~M_WAITOK;
		flags |= M_NOWAIT;
	}
	if (slab != NULL)
		KEG_UNLOCK(keg);

	return i;
}

static uma_bucket_t
zone_alloc_bucket(uma_zone_t zone, void *udata, int flags)
{
	uma_bucket_t bucket;
	int max;

	/* Don't wait for buckets, preserve caller's NOVM setting. */
	bucket = bucket_alloc(zone, udata, M_NOWAIT | (flags & M_NOVM));
	if (bucket == NULL)
		return (NULL);

	max = MIN(bucket->ub_entries, zone->uz_count);
	bucket->ub_cnt = zone->uz_import(zone->uz_arg, bucket->ub_bucket,
	    max, flags);

	/*
	 * Initialize the memory if necessary.
	 */
	if (bucket->ub_cnt != 0 && zone->uz_init != NULL) {
		int i;

		for (i = 0; i < bucket->ub_cnt; i++)
			if (zone->uz_init(bucket->ub_bucket[i], zone->uz_size,
			    flags) != 0)
				break;
		/*
		 * If we couldn't initialize the whole bucket, put the
		 * rest back onto the freelist.
		 */
		if (i != bucket->ub_cnt) {
			zone->uz_release(zone->uz_arg, &bucket->ub_bucket[i],
			    bucket->ub_cnt - i);
#ifdef INVARIANTS
			bzero(&bucket->ub_bucket[i],
			    sizeof(void *) * (bucket->ub_cnt - i));
#endif
			bucket->ub_cnt = i;
		}
	}

	if (bucket->ub_cnt == 0) {
		bucket_free(zone, bucket, udata);
		atomic_add_long(&zone->uz_fails, 1);
		return (NULL);
	}

	return (bucket);
}

/*
 * Allocates a single item from a zone.
 *
 * Arguments
 *	zone   The zone to alloc for.
 *	udata  The data to be passed to the constructor.
 *	flags  M_WAITOK, M_NOWAIT, M_ZERO.
 *
 * Returns
 *	NULL if there is no memory and M_NOWAIT is set
 *	An item if successful
 */

static void *
zone_alloc_item(uma_zone_t zone, void *udata, int flags)
{
	void *item;

	item = NULL;

#ifdef UMA_DEBUG_ALLOC
	printf("INTERNAL: Allocating one item from %s(%p)\n", zone->uz_name, zone);
#endif
	if (zone->uz_import(zone->uz_arg, &item, 1, flags) != 1)
		goto fail;
	atomic_add_long(&zone->uz_allocs, 1);

	/*
	 * We have to call both the zone's init (not the keg's init)
	 * and the zone's ctor.  This is because the item is going from
	 * a keg slab directly to the user, and the user is expecting it
	 * to be both zone-init'd as well as zone-ctor'd.
	 */
	if (zone->uz_init != NULL) {
		if (zone->uz_init(item, zone->uz_size, flags) != 0) {
			zone_free_item(zone, item, udata, SKIP_FINI);
			goto fail;
		}
	}
	if (zone->uz_ctor != NULL) {
		if (zone->uz_ctor(item, zone->uz_size, udata, flags) != 0) {
			zone_free_item(zone, item, udata, SKIP_DTOR);
			goto fail;
		}
	}
#ifdef INVARIANTS
	uma_dbg_alloc(zone, NULL, item);
#endif
	if (flags & M_ZERO)
		uma_zero_item(item, zone);

	return (item);

fail:
	atomic_add_long(&zone->uz_fails, 1);
	return (NULL);
}

/* See uma.h */
void
uma_zfree_arg(uma_zone_t zone, void *item, void *udata)
{
	uma_cache_t cache;
	uma_bucket_t bucket;
	int lockfail;
	int cpu;

	/* Enable entropy collection for RANDOM_ENABLE_UMA kernel option */
	random_harvest_fast_uma(&zone, sizeof(zone), 1, RANDOM_UMA);

#ifdef UMA_DEBUG_ALLOC_1
	printf("Freeing item %p to %s(%p)\n", item, zone->uz_name, zone);
#endif
	CTR2(KTR_UMA, "uma_zfree_arg thread %x zone %s", curthread,
	    zone->uz_name);

	KASSERT(curthread->td_critnest == 0 || SCHEDULER_STOPPED(),
	    ("uma_zfree_arg: called with spinlock or critical section held"));

        /* uma_zfree(..., NULL) does nothing, to match free(9). */
        if (item == NULL)
                return;
#ifdef DEBUG_MEMGUARD
	if (is_memguard_addr(item)) {
		if (zone->uz_dtor != NULL)
			zone->uz_dtor(item, zone->uz_size, udata);
		if (zone->uz_fini != NULL)
			zone->uz_fini(item, zone->uz_size);
		memguard_free(item);
		return;
	}
#endif
#ifdef INVARIANTS
	if (zone->uz_flags & UMA_ZONE_MALLOC)
		uma_dbg_free(zone, udata, item);
	else
		uma_dbg_free(zone, NULL, item);
#endif
	if (zone->uz_dtor != NULL)
		zone->uz_dtor(item, zone->uz_size, udata);

	/*
	 * The race here is acceptable.  If we miss it we'll just have to wait
	 * a little longer for the limits to be reset.
	 */
	if (zone->uz_flags & UMA_ZFLAG_FULL)
		goto zfree_item;

	/*
	 * If possible, free to the per-CPU cache.  There are two
	 * requirements for safe access to the per-CPU cache: (1) the thread
	 * accessing the cache must not be preempted or yield during access,
	 * and (2) the thread must not migrate CPUs without switching which
	 * cache it accesses.  We rely on a critical section to prevent
	 * preemption and migration.  We release the critical section in
	 * order to acquire the zone mutex if we are unable to free to the
	 * current cache; when we re-acquire the critical section, we must
	 * detect and handle migration if it has occurred.
	 */
zfree_restart:
	critical_enter();
	cpu = curcpu;
	cache = &zone->uz_cpu[cpu];

zfree_start:
	/*
	 * Try to free into the allocbucket first to give LIFO ordering
	 * for cache-hot datastructures.  Spill over into the freebucket
	 * if necessary.  Alloc will swap them if one runs dry.
	 */
	bucket = cache->uc_allocbucket;
	if (bucket == NULL || bucket->ub_cnt >= bucket->ub_entries)
		bucket = cache->uc_freebucket;
	if (bucket != NULL && bucket->ub_cnt < bucket->ub_entries) {
		KASSERT(bucket->ub_bucket[bucket->ub_cnt] == NULL,
		    ("uma_zfree: Freeing to non free bucket index."));
		bucket->ub_bucket[bucket->ub_cnt] = item;
		bucket->ub_cnt++;
		cache->uc_frees++;
		critical_exit();
		return;
	}

	/*
	 * We must go back the zone, which requires acquiring the zone lock,
	 * which in turn means we must release and re-acquire the critical
	 * section.  Since the critical section is released, we may be
	 * preempted or migrate.  As such, make sure not to maintain any
	 * thread-local state specific to the cache from prior to releasing
	 * the critical section.
	 */
	critical_exit();
	if (zone->uz_count == 0 || bucketdisable)
		goto zfree_item;

	lockfail = 0;
	if (ZONE_TRYLOCK(zone) == 0) {
		/* Record contention to size the buckets. */
		ZONE_LOCK(zone);
		lockfail = 1;
	}
	critical_enter();
	cpu = curcpu;
	cache = &zone->uz_cpu[cpu];

	/*
	 * Since we have locked the zone we may as well send back our stats.
	 */
	atomic_add_long(&zone->uz_allocs, cache->uc_allocs);
	atomic_add_long(&zone->uz_frees, cache->uc_frees);
	cache->uc_allocs = 0;
	cache->uc_frees = 0;

	bucket = cache->uc_freebucket;
	if (bucket != NULL && bucket->ub_cnt < bucket->ub_entries) {
		ZONE_UNLOCK(zone);
		goto zfree_start;
	}
	cache->uc_freebucket = NULL;
	/* We are no longer associated with this CPU. */
	critical_exit();

	/* Can we throw this on the zone full list? */
	if (bucket != NULL) {
#ifdef UMA_DEBUG_ALLOC
		printf("uma_zfree: Putting old bucket on the free list.\n");
#endif
		/* ub_cnt is pointing to the last free item */
		KASSERT(bucket->ub_cnt != 0,
		    ("uma_zfree: Attempting to insert an empty bucket onto the full list.\n"));
		LIST_INSERT_HEAD(&zone->uz_buckets, bucket, ub_link);
	}

	/*
	 * We bump the uz count when the cache size is insufficient to
	 * handle the working set.
	 */
	if (lockfail && zone->uz_count < BUCKET_MAX)
		zone->uz_count++;
	ZONE_UNLOCK(zone);

#ifdef UMA_DEBUG_ALLOC
	printf("uma_zfree: Allocating new free bucket.\n");
#endif
	bucket = bucket_alloc(zone, udata, M_NOWAIT);
	if (bucket) {
		critical_enter();
		cpu = curcpu;
		cache = &zone->uz_cpu[cpu];
		if (cache->uc_freebucket == NULL) {
			cache->uc_freebucket = bucket;
			goto zfree_start;
		}
		/*
		 * We lost the race, start over.  We have to drop our
		 * critical section to free the bucket.
		 */
		critical_exit();
		bucket_free(zone, bucket, udata);
		goto zfree_restart;
	}

	/*
	 * If nothing else caught this, we'll just do an internal free.
	 */
zfree_item:
	zone_free_item(zone, item, udata, SKIP_DTOR);

	return;
}

static void
slab_free_item(uma_keg_t keg, uma_slab_t slab, void *item)
{
	uint8_t freei;

	mtx_assert(&keg->uk_lock, MA_OWNED);
	MPASS(keg == slab->us_keg);

	/* Do we need to remove from any lists? */
	if (slab->us_freecount+1 == keg->uk_ipers) {
		LIST_REMOVE(slab, us_link);
		LIST_INSERT_HEAD(&keg->uk_free_slab, slab, us_link);
	} else if (slab->us_freecount == 0) {
		LIST_REMOVE(slab, us_link);
		LIST_INSERT_HEAD(&keg->uk_part_slab, slab, us_link);
	}

	/* Slab management. */
	freei = ((uintptr_t)item - (uintptr_t)slab->us_data) / keg->uk_rsize;
	BIT_SET(SLAB_SETSIZE, freei, &slab->us_free);
	slab->us_freecount++;

	/* Keg statistics. */
	keg->uk_free++;
}

static void
zone_release(uma_zone_t zone, void **bucket, int cnt)
{
	void *item;
	uma_slab_t slab;
	uma_keg_t keg;
	uint8_t *mem;
	int clearfull;
	int i;

	clearfull = 0;
	keg = zone_first_keg(zone);
	KEG_LOCK(keg);
	for (i = 0; i < cnt; i++) {
		item = bucket[i];
		if (!(zone->uz_flags & UMA_ZONE_VTOSLAB)) {
			mem = (uint8_t *)((uintptr_t)item & (~UMA_SLAB_MASK));
			if (zone->uz_flags & UMA_ZONE_HASH) {
				slab = hash_sfind(&keg->uk_hash, mem);
			} else {
				mem += keg->uk_pgoff;
				slab = (uma_slab_t)mem;
			}
		} else {
			slab = vtoslab((vm_offset_t)item);
			if (slab->us_keg != keg) {
				KEG_UNLOCK(keg);
				keg = slab->us_keg;
				KEG_LOCK(keg);
			}
		}
		slab_free_item(keg, slab, item);
		if (keg->uk_flags & UMA_ZFLAG_FULL) {
			if (keg->uk_pages < keg->uk_maxpages) {
				keg->uk_flags &= ~UMA_ZFLAG_FULL;
				clearfull = 1;
			}

			/* 
			 * We can handle one more allocation. Since we're
			 * clearing ZFLAG_FULL, wake up all procs blocked
			 * on pages. This should be uncommon, so keeping this
			 * simple for now (rather than adding count of blocked 
			 * threads etc).
			 */
			wakeup(keg);
		}
	}
	KEG_UNLOCK(keg);
	if (clearfull) {
		ZONE_LOCK(zone);
		zone->uz_flags &= ~UMA_ZFLAG_FULL;
		wakeup(zone);
		ZONE_UNLOCK(zone);
	}

}

/*
 * Frees a single item to any zone.
 *
 * Arguments:
 *	zone   The zone to free to
 *	item   The item we're freeing
 *	udata  User supplied data for the dtor
 *	skip   Skip dtors and finis
 */
static void
zone_free_item(uma_zone_t zone, void *item, void *udata, enum zfreeskip skip)
{

#ifdef INVARIANTS
	if (skip == SKIP_NONE) {
		if (zone->uz_flags & UMA_ZONE_MALLOC)
			uma_dbg_free(zone, udata, item);
		else
			uma_dbg_free(zone, NULL, item);
	}
#endif
	if (skip < SKIP_DTOR && zone->uz_dtor)
		zone->uz_dtor(item, zone->uz_size, udata);

	if (skip < SKIP_FINI && zone->uz_fini)
		zone->uz_fini(item, zone->uz_size);

	atomic_add_long(&zone->uz_frees, 1);
	zone->uz_release(zone->uz_arg, &item, 1);
}

/* See uma.h */
int
uma_zone_set_max(uma_zone_t zone, int nitems)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	if (keg == NULL)
		return (0);
	KEG_LOCK(keg);
	keg->uk_maxpages = (nitems / keg->uk_ipers) * keg->uk_ppera;
	if (keg->uk_maxpages * keg->uk_ipers < nitems)
		keg->uk_maxpages += keg->uk_ppera;
	nitems = keg->uk_maxpages * keg->uk_ipers;
	KEG_UNLOCK(keg);

	return (nitems);
}

/* See uma.h */
int
uma_zone_get_max(uma_zone_t zone)
{
	int nitems;
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	if (keg == NULL)
		return (0);
	KEG_LOCK(keg);
	nitems = keg->uk_maxpages * keg->uk_ipers;
	KEG_UNLOCK(keg);

	return (nitems);
}

/* See uma.h */
void
uma_zone_set_warning(uma_zone_t zone, const char *warning)
{

	ZONE_LOCK(zone);
	zone->uz_warning = warning;
	ZONE_UNLOCK(zone);
}

/* See uma.h */
void
uma_zone_set_maxaction(uma_zone_t zone, uma_maxaction_t maxaction)
{

	ZONE_LOCK(zone);
	TASK_INIT(&zone->uz_maxaction, 0, (task_fn_t *)maxaction, zone);
	ZONE_UNLOCK(zone);
}

/* See uma.h */
int
uma_zone_get_cur(uma_zone_t zone)
{
	int64_t nitems;
	u_int i;

	ZONE_LOCK(zone);
	nitems = zone->uz_allocs - zone->uz_frees;
	CPU_FOREACH(i) {
		/*
		 * See the comment in sysctl_vm_zone_stats() regarding the
		 * safety of accessing the per-cpu caches. With the zone lock
		 * held, it is safe, but can potentially result in stale data.
		 */
		nitems += zone->uz_cpu[i].uc_allocs -
		    zone->uz_cpu[i].uc_frees;
	}
	ZONE_UNLOCK(zone);

	return (nitems < 0 ? 0 : nitems);
}

/* See uma.h */
void
uma_zone_set_init(uma_zone_t zone, uma_init uminit)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	KASSERT(keg != NULL, ("uma_zone_set_init: Invalid zone type"));
	KEG_LOCK(keg);
	KASSERT(keg->uk_pages == 0,
	    ("uma_zone_set_init on non-empty keg"));
	keg->uk_init = uminit;
	KEG_UNLOCK(keg);
}

/* See uma.h */
void
uma_zone_set_fini(uma_zone_t zone, uma_fini fini)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	KASSERT(keg != NULL, ("uma_zone_set_fini: Invalid zone type"));
	KEG_LOCK(keg);
	KASSERT(keg->uk_pages == 0,
	    ("uma_zone_set_fini on non-empty keg"));
	keg->uk_fini = fini;
	KEG_UNLOCK(keg);
}

/* See uma.h */
void
uma_zone_set_zinit(uma_zone_t zone, uma_init zinit)
{

	ZONE_LOCK(zone);
	KASSERT(zone_first_keg(zone)->uk_pages == 0,
	    ("uma_zone_set_zinit on non-empty keg"));
	zone->uz_init = zinit;
	ZONE_UNLOCK(zone);
}

/* See uma.h */
void
uma_zone_set_zfini(uma_zone_t zone, uma_fini zfini)
{

	ZONE_LOCK(zone);
	KASSERT(zone_first_keg(zone)->uk_pages == 0,
	    ("uma_zone_set_zfini on non-empty keg"));
	zone->uz_fini = zfini;
	ZONE_UNLOCK(zone);
}

/* See uma.h */
/* XXX uk_freef is not actually used with the zone locked */
void
uma_zone_set_freef(uma_zone_t zone, uma_free freef)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	KASSERT(keg != NULL, ("uma_zone_set_freef: Invalid zone type"));
	KEG_LOCK(keg);
	keg->uk_freef = freef;
	KEG_UNLOCK(keg);
}

/* See uma.h */
/* XXX uk_allocf is not actually used with the zone locked */
void
uma_zone_set_allocf(uma_zone_t zone, uma_alloc allocf)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	KEG_LOCK(keg);
	keg->uk_allocf = allocf;
	KEG_UNLOCK(keg);
}

/* See uma.h */
void
uma_zone_reserve(uma_zone_t zone, int items)
{
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	if (keg == NULL)
		return;
	KEG_LOCK(keg);
	keg->uk_reserve = items;
	KEG_UNLOCK(keg);

	return;
}

/* See uma.h */
int
uma_zone_reserve_kva(uma_zone_t zone, int count)
{
	uma_keg_t keg;
	vm_offset_t kva;
	u_int pages;

	keg = zone_first_keg(zone);
	if (keg == NULL)
		return (0);
	pages = count / keg->uk_ipers;

	if (pages * keg->uk_ipers < count)
		pages++;

#ifdef UMA_MD_SMALL_ALLOC
	if (keg->uk_ppera > 1) {
#else
	if (1) {
#endif
		kva = kva_alloc((vm_size_t)pages * UMA_SLAB_SIZE);
		if (kva == 0)
			return (0);
	} else
		kva = 0;
	KEG_LOCK(keg);
	keg->uk_kva = kva;
	keg->uk_offset = 0;
	keg->uk_maxpages = pages;
#ifdef UMA_MD_SMALL_ALLOC
	keg->uk_allocf = (keg->uk_ppera > 1) ? noobj_alloc : uma_small_alloc;
#else
	keg->uk_allocf = noobj_alloc;
#endif
	keg->uk_flags |= UMA_ZONE_NOFREE;
	KEG_UNLOCK(keg);

	return (1);
}

/* See uma.h */
void
uma_prealloc(uma_zone_t zone, int items)
{
	int slabs;
	uma_slab_t slab;
	uma_keg_t keg;

	keg = zone_first_keg(zone);
	if (keg == NULL)
		return;
	KEG_LOCK(keg);
	slabs = items / keg->uk_ipers;
	if (slabs * keg->uk_ipers < items)
		slabs++;
	while (slabs > 0) {
		slab = keg_alloc_slab(keg, zone, M_WAITOK);
		if (slab == NULL)
			break;
		MPASS(slab->us_keg == keg);
		LIST_INSERT_HEAD(&keg->uk_free_slab, slab, us_link);
		slabs--;
	}
	KEG_UNLOCK(keg);
}

/* See uma.h */
static void
uma_reclaim_locked(bool kmem_danger)
{

#ifdef UMA_DEBUG
	printf("UMA: vm asked us to release pages!\n");
#endif
	sx_assert(&uma_drain_lock, SA_XLOCKED);
	bucket_enable();
	zone_foreach(zone_drain);
	if (vm_page_count_min() || kmem_danger) {
		cache_drain_safe(NULL);
		zone_foreach(zone_drain);
	}
	/*
	 * Some slabs may have been freed but this zone will be visited early
	 * we visit again so that we can free pages that are empty once other
	 * zones are drained.  We have to do the same for buckets.
	 */
	zone_drain(slabzone);
	bucket_zone_drain();
}

void
uma_reclaim(void)
{

	sx_xlock(&uma_drain_lock);
	uma_reclaim_locked(false);
	sx_xunlock(&uma_drain_lock);
}

static int uma_reclaim_needed;

void
uma_reclaim_wakeup(void)
{

	uma_reclaim_needed = 1;
	wakeup(&uma_reclaim_needed);
}

void
uma_reclaim_worker(void *arg __unused)
{

	sx_xlock(&uma_drain_lock);
	for (;;) {
		sx_sleep(&uma_reclaim_needed, &uma_drain_lock, PVM,
		    "umarcl", 0);
		if (uma_reclaim_needed) {
			uma_reclaim_needed = 0;
			uma_reclaim_locked(true);
		}
	}
}

/* See uma.h */
int
uma_zone_exhausted(uma_zone_t zone)
{
	int full;

	ZONE_LOCK(zone);
	full = (zone->uz_flags & UMA_ZFLAG_FULL);
	ZONE_UNLOCK(zone);
	return (full);	
}

int
uma_zone_exhausted_nolock(uma_zone_t zone)
{
	return (zone->uz_flags & UMA_ZFLAG_FULL);
}

void *
uma_large_malloc(vm_size_t size, int wait)
{
	void *mem;
	uma_slab_t slab;
	uint8_t flags;

	slab = zone_alloc_item(slabzone, NULL, wait);
	if (slab == NULL)
		return (NULL);
	mem = page_alloc(NULL, size, &flags, wait);
	if (mem) {
		vsetslab((vm_offset_t)mem, slab);
		slab->us_data = mem;
		slab->us_flags = flags | UMA_SLAB_MALLOC;
		slab->us_size = size;
	} else {
		zone_free_item(slabzone, slab, NULL, SKIP_NONE);
	}

	return (mem);
}

void
uma_large_free(uma_slab_t slab)
{

	page_free(slab->us_data, slab->us_size, slab->us_flags);
	zone_free_item(slabzone, slab, NULL, SKIP_NONE);
}

static void
uma_zero_item(void *item, uma_zone_t zone)
{
	int i;

	if (zone->uz_flags & UMA_ZONE_PCPU) {
		CPU_FOREACH(i)
			bzero(zpcpu_get_cpu(item, i), zone->uz_size);
	} else
		bzero(item, zone->uz_size);
}

void
uma_print_stats(void)
{
	zone_foreach(uma_print_zone);
}

static void
slab_print(uma_slab_t slab)
{
	printf("slab: keg %p, data %p, freecount %d\n",
		slab->us_keg, slab->us_data, slab->us_freecount);
}

static void
cache_print(uma_cache_t cache)
{
	printf("alloc: %p(%d), free: %p(%d)\n",
		cache->uc_allocbucket,
		cache->uc_allocbucket?cache->uc_allocbucket->ub_cnt:0,
		cache->uc_freebucket,
		cache->uc_freebucket?cache->uc_freebucket->ub_cnt:0);
}

static void
uma_print_keg(uma_keg_t keg)
{
	uma_slab_t slab;

	printf("keg: %s(%p) size %d(%d) flags %#x ipers %d ppera %d "
	    "out %d free %d limit %d\n",
	    keg->uk_name, keg, keg->uk_size, keg->uk_rsize, keg->uk_flags,
	    keg->uk_ipers, keg->uk_ppera,
	    (keg->uk_ipers * keg->uk_pages) - keg->uk_free, keg->uk_free,
	    (keg->uk_maxpages / keg->uk_ppera) * keg->uk_ipers);
	printf("Part slabs:\n");
	LIST_FOREACH(slab, &keg->uk_part_slab, us_link)
		slab_print(slab);
	printf("Free slabs:\n");
	LIST_FOREACH(slab, &keg->uk_free_slab, us_link)
		slab_print(slab);
	printf("Full slabs:\n");
	LIST_FOREACH(slab, &keg->uk_full_slab, us_link)
		slab_print(slab);
}

void
uma_print_zone(uma_zone_t zone)
{
	uma_cache_t cache;
	uma_klink_t kl;
	int i;

	printf("zone: %s(%p) size %d flags %#x\n",
	    zone->uz_name, zone, zone->uz_size, zone->uz_flags);
	LIST_FOREACH(kl, &zone->uz_kegs, kl_link)
		uma_print_keg(kl->kl_keg);
	CPU_FOREACH(i) {
		cache = &zone->uz_cpu[i];
		printf("CPU %d Cache:\n", i);
		cache_print(cache);
	}
}

#ifdef DDB
/*
 * Generate statistics across both the zone and its per-cpu cache's.  Return
 * desired statistics if the pointer is non-NULL for that statistic.
 *
 * Note: does not update the zone statistics, as it can't safely clear the
 * per-CPU cache statistic.
 *
 * XXXRW: Following the uc_allocbucket and uc_freebucket pointers here isn't
 * safe from off-CPU; we should modify the caches to track this information
 * directly so that we don't have to.
 */
static void
uma_zone_sumstat(uma_zone_t z, int *cachefreep, uint64_t *allocsp,
    uint64_t *freesp, uint64_t *sleepsp)
{
	uma_cache_t cache;
	uint64_t allocs, frees, sleeps;
	int cachefree, cpu;

	allocs = frees = sleeps = 0;
	cachefree = 0;
	CPU_FOREACH(cpu) {
		cache = &z->uz_cpu[cpu];
		if (cache->uc_allocbucket != NULL)
			cachefree += cache->uc_allocbucket->ub_cnt;
		if (cache->uc_freebucket != NULL)
			cachefree += cache->uc_freebucket->ub_cnt;
		allocs += cache->uc_allocs;
		frees += cache->uc_frees;
	}
	allocs += z->uz_allocs;
	frees += z->uz_frees;
	sleeps += z->uz_sleeps;
	if (cachefreep != NULL)
		*cachefreep = cachefree;
	if (allocsp != NULL)
		*allocsp = allocs;
	if (freesp != NULL)
		*freesp = frees;
	if (sleepsp != NULL)
		*sleepsp = sleeps;
}
#endif /* DDB */

static int
sysctl_vm_zone_count(SYSCTL_HANDLER_ARGS)
{
	uma_keg_t kz;
	uma_zone_t z;
	int count;

	count = 0;
	rw_rlock(&uma_rwlock);
	LIST_FOREACH(kz, &uma_kegs, uk_link) {
		LIST_FOREACH(z, &kz->uk_zones, uz_link)
			count++;
	}
	rw_runlock(&uma_rwlock);
	return (sysctl_handle_int(oidp, &count, 0, req));
}

static int
sysctl_vm_zone_stats(SYSCTL_HANDLER_ARGS)
{
	struct uma_stream_header ush;
	struct uma_type_header uth;
	struct uma_percpu_stat ups;
	uma_bucket_t bucket;
	struct sbuf sbuf;
	uma_cache_t cache;
	uma_klink_t kl;
	uma_keg_t kz;
	uma_zone_t z;
	uma_keg_t k;
	int count, error, i;

	error = sysctl_wire_old_buffer(req, 0);
	if (error != 0)
		return (error);
	sbuf_new_for_sysctl(&sbuf, NULL, 128, req);
	sbuf_clear_flags(&sbuf, SBUF_INCLUDENUL);

	count = 0;
	rw_rlock(&uma_rwlock);
	LIST_FOREACH(kz, &uma_kegs, uk_link) {
		LIST_FOREACH(z, &kz->uk_zones, uz_link)
			count++;
	}

	/*
	 * Insert stream header.
	 */
	bzero(&ush, sizeof(ush));
	ush.ush_version = UMA_STREAM_VERSION;
	ush.ush_maxcpus = (mp_maxid + 1);
	ush.ush_count = count;
	(void)sbuf_bcat(&sbuf, &ush, sizeof(ush));

	LIST_FOREACH(kz, &uma_kegs, uk_link) {
		LIST_FOREACH(z, &kz->uk_zones, uz_link) {
			bzero(&uth, sizeof(uth));
			ZONE_LOCK(z);
			strlcpy(uth.uth_name, z->uz_name, UTH_MAX_NAME);
			uth.uth_align = kz->uk_align;
			uth.uth_size = kz->uk_size;
			uth.uth_rsize = kz->uk_rsize;
			LIST_FOREACH(kl, &z->uz_kegs, kl_link) {
				k = kl->kl_keg;
				uth.uth_maxpages += k->uk_maxpages;
				uth.uth_pages += k->uk_pages;
				uth.uth_keg_free += k->uk_free;
				uth.uth_limit = (k->uk_maxpages / k->uk_ppera)
				    * k->uk_ipers;
			}

			/*
			 * A zone is secondary is it is not the first entry
			 * on the keg's zone list.
			 */
			if ((z->uz_flags & UMA_ZONE_SECONDARY) &&
			    (LIST_FIRST(&kz->uk_zones) != z))
				uth.uth_zone_flags = UTH_ZONE_SECONDARY;

			LIST_FOREACH(bucket, &z->uz_buckets, ub_link)
				uth.uth_zone_free += bucket->ub_cnt;
			uth.uth_allocs = z->uz_allocs;
			uth.uth_frees = z->uz_frees;
			uth.uth_fails = z->uz_fails;
			uth.uth_sleeps = z->uz_sleeps;
			(void)sbuf_bcat(&sbuf, &uth, sizeof(uth));
			/*
			 * While it is not normally safe to access the cache
			 * bucket pointers while not on the CPU that owns the
			 * cache, we only allow the pointers to be exchanged
			 * without the zone lock held, not invalidated, so
			 * accept the possible race associated with bucket
			 * exchange during monitoring.
			 */
			for (i = 0; i < (mp_maxid + 1); i++) {
				bzero(&ups, sizeof(ups));
				if (kz->uk_flags & UMA_ZFLAG_INTERNAL)
					goto skip;
				if (CPU_ABSENT(i))
					goto skip;
				cache = &z->uz_cpu[i];
				if (cache->uc_allocbucket != NULL)
					ups.ups_cache_free +=
					    cache->uc_allocbucket->ub_cnt;
				if (cache->uc_freebucket != NULL)
					ups.ups_cache_free +=
					    cache->uc_freebucket->ub_cnt;
				ups.ups_allocs = cache->uc_allocs;
				ups.ups_frees = cache->uc_frees;
skip:
				(void)sbuf_bcat(&sbuf, &ups, sizeof(ups));
			}
			ZONE_UNLOCK(z);
		}
	}
	rw_runlock(&uma_rwlock);
	error = sbuf_finish(&sbuf);
	sbuf_delete(&sbuf);
	return (error);
}

int
sysctl_handle_uma_zone_max(SYSCTL_HANDLER_ARGS)
{
	uma_zone_t zone = *(uma_zone_t *)arg1;
	int error, max;

	max = uma_zone_get_max(zone);
	error = sysctl_handle_int(oidp, &max, 0, req);
	if (error || !req->newptr)
		return (error);

	uma_zone_set_max(zone, max);

	return (0);
}

int
sysctl_handle_uma_zone_cur(SYSCTL_HANDLER_ARGS)
{
	uma_zone_t zone = *(uma_zone_t *)arg1;
	int cur;

	cur = uma_zone_get_cur(zone);
	return (sysctl_handle_int(oidp, &cur, 0, req));
}

#ifdef INVARIANTS
static uma_slab_t
uma_dbg_getslab(uma_zone_t zone, void *item)
{
	uma_slab_t slab;
	uma_keg_t keg;
	uint8_t *mem;

	mem = (uint8_t *)((uintptr_t)item & (~UMA_SLAB_MASK));
	if (zone->uz_flags & UMA_ZONE_VTOSLAB) {
		slab = vtoslab((vm_offset_t)mem);
	} else {
		/*
		 * It is safe to return the slab here even though the
		 * zone is unlocked because the item's allocation state
		 * essentially holds a reference.
		 */
		ZONE_LOCK(zone);
		keg = LIST_FIRST(&zone->uz_kegs)->kl_keg;
		if (keg->uk_flags & UMA_ZONE_HASH)
			slab = hash_sfind(&keg->uk_hash, mem);
		else
			slab = (uma_slab_t)(mem + keg->uk_pgoff);
		ZONE_UNLOCK(zone);
	}

	return (slab);
}

/*
 * Set up the slab's freei data such that uma_dbg_free can function.
 *
 */
static void
uma_dbg_alloc(uma_zone_t zone, uma_slab_t slab, void *item)
{
	uma_keg_t keg;
	int freei;

	if (zone_first_keg(zone) == NULL)
		return;
	if (slab == NULL) {
		slab = uma_dbg_getslab(zone, item);
		if (slab == NULL) 
			panic("uma: item %p did not belong to zone %s\n",
			    item, zone->uz_name);
	}
	keg = slab->us_keg;
	freei = ((uintptr_t)item - (uintptr_t)slab->us_data) / keg->uk_rsize;

	if (BIT_ISSET(SLAB_SETSIZE, freei, &slab->us_debugfree))
		panic("Duplicate alloc of %p from zone %p(%s) slab %p(%d)\n",
		    item, zone, zone->uz_name, slab, freei);
	BIT_SET_ATOMIC(SLAB_SETSIZE, freei, &slab->us_debugfree);

	return;
}

/*
 * Verifies freed addresses.  Checks for alignment, valid slab membership
 * and duplicate frees.
 *
 */
static void
uma_dbg_free(uma_zone_t zone, uma_slab_t slab, void *item)
{
	uma_keg_t keg;
	int freei;

	if (zone_first_keg(zone) == NULL)
		return;
	if (slab == NULL) {
		slab = uma_dbg_getslab(zone, item);
		if (slab == NULL) 
			panic("uma: Freed item %p did not belong to zone %s\n",
			    item, zone->uz_name);
	}
	keg = slab->us_keg;
	freei = ((uintptr_t)item - (uintptr_t)slab->us_data) / keg->uk_rsize;

	if (freei >= keg->uk_ipers)
		panic("Invalid free of %p from zone %p(%s) slab %p(%d)\n",
		    item, zone, zone->uz_name, slab, freei);

	if (((freei * keg->uk_rsize) + slab->us_data) != item) 
		panic("Unaligned free of %p from zone %p(%s) slab %p(%d)\n",
		    item, zone, zone->uz_name, slab, freei);

	if (!BIT_ISSET(SLAB_SETSIZE, freei, &slab->us_debugfree))
		panic("Duplicate free of %p from zone %p(%s) slab %p(%d)\n",
		    item, zone, zone->uz_name, slab, freei);

	BIT_CLR_ATOMIC(SLAB_SETSIZE, freei, &slab->us_debugfree);
}
#endif /* INVARIANTS */

#ifdef DDB
DB_SHOW_COMMAND(uma, db_show_uma)
{
	uint64_t allocs, frees, sleeps;
	uma_bucket_t bucket;
	uma_keg_t kz;
	uma_zone_t z;
	int cachefree;

	db_printf("%18s %8s %8s %8s %12s %8s %8s\n", "Zone", "Size", "Used",
	    "Free", "Requests", "Sleeps", "Bucket");
	LIST_FOREACH(kz, &uma_kegs, uk_link) {
		LIST_FOREACH(z, &kz->uk_zones, uz_link) {
			if (kz->uk_flags & UMA_ZFLAG_INTERNAL) {
				allocs = z->uz_allocs;
				frees = z->uz_frees;
				sleeps = z->uz_sleeps;
				cachefree = 0;
			} else
				uma_zone_sumstat(z, &cachefree, &allocs,
				    &frees, &sleeps);
			if (!((z->uz_flags & UMA_ZONE_SECONDARY) &&
			    (LIST_FIRST(&kz->uk_zones) != z)))
				cachefree += kz->uk_free;
			LIST_FOREACH(bucket, &z->uz_buckets, ub_link)
				cachefree += bucket->ub_cnt;
			db_printf("%18s %8ju %8jd %8d %12ju %8ju %8u\n",
			    z->uz_name, (uintmax_t)kz->uk_size,
			    (intmax_t)(allocs - frees), cachefree,
			    (uintmax_t)allocs, sleeps, z->uz_count);
			if (db_pager_quit)
				return;
		}
	}
}

DB_SHOW_COMMAND(umacache, db_show_umacache)
{
	uint64_t allocs, frees;
	uma_bucket_t bucket;
	uma_zone_t z;
	int cachefree;

	db_printf("%18s %8s %8s %8s %12s %8s\n", "Zone", "Size", "Used", "Free",
	    "Requests", "Bucket");
	LIST_FOREACH(z, &uma_cachezones, uz_link) {
		uma_zone_sumstat(z, &cachefree, &allocs, &frees, NULL);
		LIST_FOREACH(bucket, &z->uz_buckets, ub_link)
			cachefree += bucket->ub_cnt;
		db_printf("%18s %8ju %8jd %8d %12ju %8u\n",
		    z->uz_name, (uintmax_t)z->uz_size,
		    (intmax_t)(allocs - frees), cachefree,
		    (uintmax_t)allocs, z->uz_count);
		if (db_pager_quit)
			return;
	}
}
#endif	/* DDB */
