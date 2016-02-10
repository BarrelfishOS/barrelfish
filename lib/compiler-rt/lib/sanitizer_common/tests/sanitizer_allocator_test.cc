//===-- sanitizer_allocator_test.cc ---------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is a part of ThreadSanitizer/AddressSanitizer runtime.
// Tests for sanitizer_allocator.h.
//
//===----------------------------------------------------------------------===//
#include "sanitizer_common/sanitizer_allocator.h"
#include "sanitizer_common/sanitizer_allocator_internal.h"
#include "sanitizer_common/sanitizer_common.h"

#include "sanitizer_test_utils.h"
#include "sanitizer_pthread_wrappers.h"

#include "gtest/gtest.h"

#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <set>

// Too slow for debug build
#if !SANITIZER_DEBUG

#if SANITIZER_CAN_USE_ALLOCATOR64
static const uptr kAllocatorSpace = 0x700000000000ULL;
static const uptr kAllocatorSize  = 0x010000000000ULL;  // 1T.
static const u64 kAddressSpaceSize = 1ULL << 47;

typedef SizeClassAllocator64<
  kAllocatorSpace, kAllocatorSize, 16, DefaultSizeClassMap> Allocator64;

typedef SizeClassAllocator64<
  kAllocatorSpace, kAllocatorSize, 16, CompactSizeClassMap> Allocator64Compact;
#elif defined(__mips64)
static const u64 kAddressSpaceSize = 1ULL << 40;
#elif defined(__aarch64__)
static const u64 kAddressSpaceSize = 1ULL << 39;
#else
static const u64 kAddressSpaceSize = 1ULL << 32;
#endif

static const uptr kRegionSizeLog = FIRST_32_SECOND_64(20, 24);
static const uptr kFlatByteMapSize = kAddressSpaceSize >> kRegionSizeLog;

typedef SizeClassAllocator32<
  0, kAddressSpaceSize,
  /*kMetadataSize*/16,
  CompactSizeClassMap,
  kRegionSizeLog,
  FlatByteMap<kFlatByteMapSize> >
  Allocator32Compact;

template <class SizeClassMap>
void TestSizeClassMap() {
  typedef SizeClassMap SCMap;
  // SCMap::Print();
  SCMap::Validate();
}

TEST(SanitizerCommon, DefaultSizeClassMap) {
  TestSizeClassMap<DefaultSizeClassMap>();
}

TEST(SanitizerCommon, CompactSizeClassMap) {
  TestSizeClassMap<CompactSizeClassMap>();
}

TEST(SanitizerCommon, InternalSizeClassMap) {
  TestSizeClassMap<InternalSizeClassMap>();
}

template <class Allocator>
void TestSizeClassAllocator() {
  Allocator *a = new Allocator;
  a->Init();
  SizeClassAllocatorLocalCache<Allocator> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  static const uptr sizes[] = {1, 16, 30, 40, 100, 1000, 10000,
    50000, 60000, 100000, 120000, 300000, 500000, 1000000, 2000000};

  std::vector<void *> allocated;

  uptr last_total_allocated = 0;
  for (int i = 0; i < 3; i++) {
    // Allocate a bunch of chunks.
    for (uptr s = 0; s < ARRAY_SIZE(sizes); s++) {
      uptr size = sizes[s];
      if (!a->CanAllocate(size, 1)) continue;
      // printf("s = %ld\n", size);
      uptr n_iter = std::max((uptr)6, 4000000 / size);
      // fprintf(stderr, "size: %ld iter: %ld\n", size, n_iter);
      for (uptr i = 0; i < n_iter; i++) {
        uptr class_id0 = Allocator::SizeClassMapT::ClassID(size);
        char *x = (char*)cache.Allocate(a, class_id0);
        x[0] = 0;
        x[size - 1] = 0;
        x[size / 2] = 0;
        allocated.push_back(x);
        CHECK_EQ(x, a->GetBlockBegin(x));
        CHECK_EQ(x, a->GetBlockBegin(x + size - 1));
        CHECK(a->PointerIsMine(x));
        CHECK(a->PointerIsMine(x + size - 1));
        CHECK(a->PointerIsMine(x + size / 2));
        CHECK_GE(a->GetActuallyAllocatedSize(x), size);
        uptr class_id = a->GetSizeClass(x);
        CHECK_EQ(class_id, Allocator::SizeClassMapT::ClassID(size));
        uptr *metadata = reinterpret_cast<uptr*>(a->GetMetaData(x));
        metadata[0] = reinterpret_cast<uptr>(x) + 1;
        metadata[1] = 0xABCD;
      }
    }
    // Deallocate all.
    for (uptr i = 0; i < allocated.size(); i++) {
      void *x = allocated[i];
      uptr *metadata = reinterpret_cast<uptr*>(a->GetMetaData(x));
      CHECK_EQ(metadata[0], reinterpret_cast<uptr>(x) + 1);
      CHECK_EQ(metadata[1], 0xABCD);
      cache.Deallocate(a, a->GetSizeClass(x), x);
    }
    allocated.clear();
    uptr total_allocated = a->TotalMemoryUsed();
    if (last_total_allocated == 0)
      last_total_allocated = total_allocated;
    CHECK_EQ(last_total_allocated, total_allocated);
  }

  // Check that GetBlockBegin never crashes.
  for (uptr x = 0, step = kAddressSpaceSize / 100000;
       x < kAddressSpaceSize - step; x += step)
    if (a->PointerIsMine(reinterpret_cast<void *>(x)))
      Ident(a->GetBlockBegin(reinterpret_cast<void *>(x)));

  a->TestOnlyUnmap();
  delete a;
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64) {
  TestSizeClassAllocator<Allocator64>();
}

TEST(SanitizerCommon, SizeClassAllocator64Compact) {
  TestSizeClassAllocator<Allocator64Compact>();
}
#endif

TEST(SanitizerCommon, SizeClassAllocator32Compact) {
  TestSizeClassAllocator<Allocator32Compact>();
}

template <class Allocator>
void SizeClassAllocatorMetadataStress() {
  Allocator *a = new Allocator;
  a->Init();
  SizeClassAllocatorLocalCache<Allocator> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  const uptr kNumAllocs = 1 << 13;
  void *allocated[kNumAllocs];
  void *meta[kNumAllocs];
  for (uptr i = 0; i < kNumAllocs; i++) {
    void *x = cache.Allocate(a, 1 + i % 50);
    allocated[i] = x;
    meta[i] = a->GetMetaData(x);
  }
  // Get Metadata kNumAllocs^2 times.
  for (uptr i = 0; i < kNumAllocs * kNumAllocs; i++) {
    uptr idx = i % kNumAllocs;
    void *m = a->GetMetaData(allocated[idx]);
    EXPECT_EQ(m, meta[idx]);
  }
  for (uptr i = 0; i < kNumAllocs; i++) {
    cache.Deallocate(a, 1 + i % 50, allocated[i]);
  }

  a->TestOnlyUnmap();
  delete a;
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64MetadataStress) {
  SizeClassAllocatorMetadataStress<Allocator64>();
}

TEST(SanitizerCommon, SizeClassAllocator64CompactMetadataStress) {
  SizeClassAllocatorMetadataStress<Allocator64Compact>();
}
#endif  // SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator32CompactMetadataStress) {
  SizeClassAllocatorMetadataStress<Allocator32Compact>();
}

template <class Allocator>
void SizeClassAllocatorGetBlockBeginStress() {
  Allocator *a = new Allocator;
  a->Init();
  SizeClassAllocatorLocalCache<Allocator> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  uptr max_size_class = Allocator::kNumClasses - 1;
  uptr size = Allocator::SizeClassMapT::Size(max_size_class);
  u64 G8 = 1ULL << 33;
  // Make sure we correctly compute GetBlockBegin() w/o overflow.
  for (size_t i = 0; i <= G8 / size; i++) {
    void *x = cache.Allocate(a, max_size_class);
    void *beg = a->GetBlockBegin(x);
    // if ((i & (i - 1)) == 0)
    //   fprintf(stderr, "[%zd] %p %p\n", i, x, beg);
    EXPECT_EQ(x, beg);
  }

  a->TestOnlyUnmap();
  delete a;
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64GetBlockBegin) {
  SizeClassAllocatorGetBlockBeginStress<Allocator64>();
}
TEST(SanitizerCommon, SizeClassAllocator64CompactGetBlockBegin) {
  SizeClassAllocatorGetBlockBeginStress<Allocator64Compact>();
}
TEST(SanitizerCommon, SizeClassAllocator32CompactGetBlockBegin) {
  SizeClassAllocatorGetBlockBeginStress<Allocator32Compact>();
}
#endif  // SANITIZER_CAN_USE_ALLOCATOR64

struct TestMapUnmapCallback {
  static int map_count, unmap_count;
  void OnMap(uptr p, uptr size) const { map_count++; }
  void OnUnmap(uptr p, uptr size) const { unmap_count++; }
};
int TestMapUnmapCallback::map_count;
int TestMapUnmapCallback::unmap_count;

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64MapUnmapCallback) {
  TestMapUnmapCallback::map_count = 0;
  TestMapUnmapCallback::unmap_count = 0;
  typedef SizeClassAllocator64<
      kAllocatorSpace, kAllocatorSize, 16, DefaultSizeClassMap,
      TestMapUnmapCallback> Allocator64WithCallBack;
  Allocator64WithCallBack *a = new Allocator64WithCallBack;
  a->Init();
  EXPECT_EQ(TestMapUnmapCallback::map_count, 1);  // Allocator state.
  SizeClassAllocatorLocalCache<Allocator64WithCallBack> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);
  AllocatorStats stats;
  stats.Init();
  a->AllocateBatch(&stats, &cache, 32);
  EXPECT_EQ(TestMapUnmapCallback::map_count, 3);  // State + alloc + metadata.
  a->TestOnlyUnmap();
  EXPECT_EQ(TestMapUnmapCallback::unmap_count, 1);  // The whole thing.
  delete a;
}
#endif

TEST(SanitizerCommon, SizeClassAllocator32MapUnmapCallback) {
  TestMapUnmapCallback::map_count = 0;
  TestMapUnmapCallback::unmap_count = 0;
  typedef SizeClassAllocator32<
      0, kAddressSpaceSize,
      /*kMetadataSize*/16,
      CompactSizeClassMap,
      kRegionSizeLog,
      FlatByteMap<kFlatByteMapSize>,
      TestMapUnmapCallback>
    Allocator32WithCallBack;
  Allocator32WithCallBack *a = new Allocator32WithCallBack;
  a->Init();
  EXPECT_EQ(TestMapUnmapCallback::map_count, 0);
  SizeClassAllocatorLocalCache<Allocator32WithCallBack>  cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);
  AllocatorStats stats;
  stats.Init();
  a->AllocateBatch(&stats, &cache, 32);
  EXPECT_EQ(TestMapUnmapCallback::map_count, 1);
  a->TestOnlyUnmap();
  EXPECT_EQ(TestMapUnmapCallback::unmap_count, 1);
  delete a;
  // fprintf(stderr, "Map: %d Unmap: %d\n",
  //         TestMapUnmapCallback::map_count,
  //         TestMapUnmapCallback::unmap_count);
}

TEST(SanitizerCommon, LargeMmapAllocatorMapUnmapCallback) {
  TestMapUnmapCallback::map_count = 0;
  TestMapUnmapCallback::unmap_count = 0;
  LargeMmapAllocator<TestMapUnmapCallback> a;
  a.Init(/* may_return_null */ false);
  AllocatorStats stats;
  stats.Init();
  void *x = a.Allocate(&stats, 1 << 20, 1);
  EXPECT_EQ(TestMapUnmapCallback::map_count, 1);
  a.Deallocate(&stats, x);
  EXPECT_EQ(TestMapUnmapCallback::unmap_count, 1);
}

template<class Allocator>
void FailInAssertionOnOOM() {
  Allocator a;
  a.Init();
  SizeClassAllocatorLocalCache<Allocator> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);
  AllocatorStats stats;
  stats.Init();
  for (int i = 0; i < 1000000; i++) {
    a.AllocateBatch(&stats, &cache, 52);
  }

  a.TestOnlyUnmap();
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64Overflow) {
  EXPECT_DEATH(FailInAssertionOnOOM<Allocator64>(), "Out of memory");
}
#endif

#if !defined(_WIN32)  // FIXME: This currently fails on Windows.
TEST(SanitizerCommon, LargeMmapAllocator) {
  LargeMmapAllocator<> a;
  a.Init(/* may_return_null */ false);
  AllocatorStats stats;
  stats.Init();

  static const int kNumAllocs = 1000;
  char *allocated[kNumAllocs];
  static const uptr size = 4000;
  // Allocate some.
  for (int i = 0; i < kNumAllocs; i++) {
    allocated[i] = (char *)a.Allocate(&stats, size, 1);
    CHECK(a.PointerIsMine(allocated[i]));
  }
  // Deallocate all.
  CHECK_GT(a.TotalMemoryUsed(), size * kNumAllocs);
  for (int i = 0; i < kNumAllocs; i++) {
    char *p = allocated[i];
    CHECK(a.PointerIsMine(p));
    a.Deallocate(&stats, p);
  }
  // Check that non left.
  CHECK_EQ(a.TotalMemoryUsed(), 0);

  // Allocate some more, also add metadata.
  for (int i = 0; i < kNumAllocs; i++) {
    char *x = (char *)a.Allocate(&stats, size, 1);
    CHECK_GE(a.GetActuallyAllocatedSize(x), size);
    uptr *meta = reinterpret_cast<uptr*>(a.GetMetaData(x));
    *meta = i;
    allocated[i] = x;
  }
  for (int i = 0; i < kNumAllocs * kNumAllocs; i++) {
    char *p = allocated[i % kNumAllocs];
    CHECK(a.PointerIsMine(p));
    CHECK(a.PointerIsMine(p + 2000));
  }
  CHECK_GT(a.TotalMemoryUsed(), size * kNumAllocs);
  // Deallocate all in reverse order.
  for (int i = 0; i < kNumAllocs; i++) {
    int idx = kNumAllocs - i - 1;
    char *p = allocated[idx];
    uptr *meta = reinterpret_cast<uptr*>(a.GetMetaData(p));
    CHECK_EQ(*meta, idx);
    CHECK(a.PointerIsMine(p));
    a.Deallocate(&stats, p);
  }
  CHECK_EQ(a.TotalMemoryUsed(), 0);

  // Test alignments.
  uptr max_alignment = SANITIZER_WORDSIZE == 64 ? (1 << 28) : (1 << 24);
  for (uptr alignment = 8; alignment <= max_alignment; alignment *= 2) {
    const uptr kNumAlignedAllocs = 100;
    for (uptr i = 0; i < kNumAlignedAllocs; i++) {
      uptr size = ((i % 10) + 1) * 4096;
      char *p = allocated[i] = (char *)a.Allocate(&stats, size, alignment);
      CHECK_EQ(p, a.GetBlockBegin(p));
      CHECK_EQ(p, a.GetBlockBegin(p + size - 1));
      CHECK_EQ(p, a.GetBlockBegin(p + size / 2));
      CHECK_EQ(0, (uptr)allocated[i] % alignment);
      p[0] = p[size - 1] = 0;
    }
    for (uptr i = 0; i < kNumAlignedAllocs; i++) {
      a.Deallocate(&stats, allocated[i]);
    }
  }

  // Regression test for boundary condition in GetBlockBegin().
  uptr page_size = GetPageSizeCached();
  char *p = (char *)a.Allocate(&stats, page_size, 1);
  CHECK_EQ(p, a.GetBlockBegin(p));
  CHECK_EQ(p, (char *)a.GetBlockBegin(p + page_size - 1));
  CHECK_NE(p, (char *)a.GetBlockBegin(p + page_size));
  a.Deallocate(&stats, p);
}
#endif

template
<class PrimaryAllocator, class SecondaryAllocator, class AllocatorCache>
void TestCombinedAllocator() {
  typedef
      CombinedAllocator<PrimaryAllocator, AllocatorCache, SecondaryAllocator>
      Allocator;
  Allocator *a = new Allocator;
  a->Init(/* may_return_null */ true);

  AllocatorCache cache;
  memset(&cache, 0, sizeof(cache));
  a->InitCache(&cache);

  EXPECT_EQ(a->Allocate(&cache, -1, 1), (void*)0);
  EXPECT_EQ(a->Allocate(&cache, -1, 1024), (void*)0);
  EXPECT_EQ(a->Allocate(&cache, (uptr)-1 - 1024, 1), (void*)0);
  EXPECT_EQ(a->Allocate(&cache, (uptr)-1 - 1024, 1024), (void*)0);
  EXPECT_EQ(a->Allocate(&cache, (uptr)-1 - 1023, 1024), (void*)0);

  // Set to false
  a->SetMayReturnNull(false);
  EXPECT_DEATH(a->Allocate(&cache, -1, 1),
               "allocator is terminating the process");

  const uptr kNumAllocs = 100000;
  const uptr kNumIter = 10;
  for (uptr iter = 0; iter < kNumIter; iter++) {
    std::vector<void*> allocated;
    for (uptr i = 0; i < kNumAllocs; i++) {
      uptr size = (i % (1 << 14)) + 1;
      if ((i % 1024) == 0)
        size = 1 << (10 + (i % 14));
      void *x = a->Allocate(&cache, size, 1);
      uptr *meta = reinterpret_cast<uptr*>(a->GetMetaData(x));
      CHECK_EQ(*meta, 0);
      *meta = size;
      allocated.push_back(x);
    }

    random_shuffle(allocated.begin(), allocated.end());

    for (uptr i = 0; i < kNumAllocs; i++) {
      void *x = allocated[i];
      uptr *meta = reinterpret_cast<uptr*>(a->GetMetaData(x));
      CHECK_NE(*meta, 0);
      CHECK(a->PointerIsMine(x));
      *meta = 0;
      a->Deallocate(&cache, x);
    }
    allocated.clear();
    a->SwallowCache(&cache);
  }
  a->DestroyCache(&cache);
  a->TestOnlyUnmap();
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, CombinedAllocator64) {
  TestCombinedAllocator<Allocator64,
      LargeMmapAllocator<>,
      SizeClassAllocatorLocalCache<Allocator64> > ();
}

TEST(SanitizerCommon, CombinedAllocator64Compact) {
  TestCombinedAllocator<Allocator64Compact,
      LargeMmapAllocator<>,
      SizeClassAllocatorLocalCache<Allocator64Compact> > ();
}
#endif

#if !defined(_WIN32)  // FIXME: This currently fails on Windows.
TEST(SanitizerCommon, CombinedAllocator32Compact) {
  TestCombinedAllocator<Allocator32Compact,
      LargeMmapAllocator<>,
      SizeClassAllocatorLocalCache<Allocator32Compact> > ();
}
#endif

template <class AllocatorCache>
void TestSizeClassAllocatorLocalCache() {
  AllocatorCache cache;
  typedef typename AllocatorCache::Allocator Allocator;
  Allocator *a = new Allocator();

  a->Init();
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  const uptr kNumAllocs = 10000;
  const int kNumIter = 100;
  uptr saved_total = 0;
  for (int class_id = 1; class_id <= 5; class_id++) {
    for (int it = 0; it < kNumIter; it++) {
      void *allocated[kNumAllocs];
      for (uptr i = 0; i < kNumAllocs; i++) {
        allocated[i] = cache.Allocate(a, class_id);
      }
      for (uptr i = 0; i < kNumAllocs; i++) {
        cache.Deallocate(a, class_id, allocated[i]);
      }
      cache.Drain(a);
      uptr total_allocated = a->TotalMemoryUsed();
      if (it)
        CHECK_EQ(saved_total, total_allocated);
      saved_total = total_allocated;
    }
  }

  a->TestOnlyUnmap();
  delete a;
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64LocalCache) {
  TestSizeClassAllocatorLocalCache<
      SizeClassAllocatorLocalCache<Allocator64> >();
}

TEST(SanitizerCommon, SizeClassAllocator64CompactLocalCache) {
  TestSizeClassAllocatorLocalCache<
      SizeClassAllocatorLocalCache<Allocator64Compact> >();
}
#endif

TEST(SanitizerCommon, SizeClassAllocator32CompactLocalCache) {
  TestSizeClassAllocatorLocalCache<
      SizeClassAllocatorLocalCache<Allocator32Compact> >();
}

#if SANITIZER_CAN_USE_ALLOCATOR64
typedef SizeClassAllocatorLocalCache<Allocator64> AllocatorCache;
static AllocatorCache static_allocator_cache;

void *AllocatorLeakTestWorker(void *arg) {
  typedef AllocatorCache::Allocator Allocator;
  Allocator *a = (Allocator*)(arg);
  static_allocator_cache.Allocate(a, 10);
  static_allocator_cache.Drain(a);
  return 0;
}

TEST(SanitizerCommon, AllocatorLeakTest) {
  typedef AllocatorCache::Allocator Allocator;
  Allocator a;
  a.Init();
  uptr total_used_memory = 0;
  for (int i = 0; i < 100; i++) {
    pthread_t t;
    PTHREAD_CREATE(&t, 0, AllocatorLeakTestWorker, &a);
    PTHREAD_JOIN(t, 0);
    if (i == 0)
      total_used_memory = a.TotalMemoryUsed();
    EXPECT_EQ(a.TotalMemoryUsed(), total_used_memory);
  }

  a.TestOnlyUnmap();
}

// Struct which is allocated to pass info to new threads.  The new thread frees
// it.
struct NewThreadParams {
  AllocatorCache *thread_cache;
  AllocatorCache::Allocator *allocator;
  uptr class_id;
};

// Called in a new thread.  Just frees its argument.
static void *DeallocNewThreadWorker(void *arg) {
  NewThreadParams *params = reinterpret_cast<NewThreadParams*>(arg);
  params->thread_cache->Deallocate(params->allocator, params->class_id, params);
  return NULL;
}

// The allocator cache is supposed to be POD and zero initialized.  We should be
// able to call Deallocate on a zeroed cache, and it will self-initialize.
TEST(Allocator, AllocatorCacheDeallocNewThread) {
  AllocatorCache::Allocator allocator;
  allocator.Init();
  AllocatorCache main_cache;
  AllocatorCache child_cache;
  memset(&main_cache, 0, sizeof(main_cache));
  memset(&child_cache, 0, sizeof(child_cache));

  uptr class_id = DefaultSizeClassMap::ClassID(sizeof(NewThreadParams));
  NewThreadParams *params = reinterpret_cast<NewThreadParams*>(
      main_cache.Allocate(&allocator, class_id));
  params->thread_cache = &child_cache;
  params->allocator = &allocator;
  params->class_id = class_id;
  pthread_t t;
  PTHREAD_CREATE(&t, 0, DeallocNewThreadWorker, params);
  PTHREAD_JOIN(t, 0);
}
#endif

TEST(Allocator, Basic) {
  char *p = (char*)InternalAlloc(10);
  EXPECT_NE(p, (char*)0);
  char *p2 = (char*)InternalAlloc(20);
  EXPECT_NE(p2, (char*)0);
  EXPECT_NE(p2, p);
  InternalFree(p);
  InternalFree(p2);
}

TEST(Allocator, Stress) {
  const int kCount = 1000;
  char *ptrs[kCount];
  unsigned rnd = 42;
  for (int i = 0; i < kCount; i++) {
    uptr sz = my_rand_r(&rnd) % 1000;
    char *p = (char*)InternalAlloc(sz);
    EXPECT_NE(p, (char*)0);
    ptrs[i] = p;
  }
  for (int i = 0; i < kCount; i++) {
    InternalFree(ptrs[i]);
  }
}

TEST(Allocator, LargeAlloc) {
  void *p = InternalAlloc(10 << 20);
  InternalFree(p);
}

TEST(Allocator, ScopedBuffer) {
  const int kSize = 512;
  {
    InternalScopedBuffer<int> int_buf(kSize);
    EXPECT_EQ(sizeof(int) * kSize, int_buf.size());  // NOLINT
  }
  InternalScopedBuffer<char> char_buf(kSize);
  EXPECT_EQ(sizeof(char) * kSize, char_buf.size());  // NOLINT
  internal_memset(char_buf.data(), 'c', kSize);
  for (int i = 0; i < kSize; i++) {
    EXPECT_EQ('c', char_buf[i]);
  }
}

void IterationTestCallback(uptr chunk, void *arg) {
  reinterpret_cast<std::set<uptr> *>(arg)->insert(chunk);
}

template <class Allocator>
void TestSizeClassAllocatorIteration() {
  Allocator *a = new Allocator;
  a->Init();
  SizeClassAllocatorLocalCache<Allocator> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  static const uptr sizes[] = {1, 16, 30, 40, 100, 1000, 10000,
    50000, 60000, 100000, 120000, 300000, 500000, 1000000, 2000000};

  std::vector<void *> allocated;

  // Allocate a bunch of chunks.
  for (uptr s = 0; s < ARRAY_SIZE(sizes); s++) {
    uptr size = sizes[s];
    if (!a->CanAllocate(size, 1)) continue;
    // printf("s = %ld\n", size);
    uptr n_iter = std::max((uptr)6, 80000 / size);
    // fprintf(stderr, "size: %ld iter: %ld\n", size, n_iter);
    for (uptr j = 0; j < n_iter; j++) {
      uptr class_id0 = Allocator::SizeClassMapT::ClassID(size);
      void *x = cache.Allocate(a, class_id0);
      allocated.push_back(x);
    }
  }

  std::set<uptr> reported_chunks;
  a->ForceLock();
  a->ForEachChunk(IterationTestCallback, &reported_chunks);
  a->ForceUnlock();

  for (uptr i = 0; i < allocated.size(); i++) {
    // Don't use EXPECT_NE. Reporting the first mismatch is enough.
    ASSERT_NE(reported_chunks.find(reinterpret_cast<uptr>(allocated[i])),
              reported_chunks.end());
  }

  a->TestOnlyUnmap();
  delete a;
}

#if SANITIZER_CAN_USE_ALLOCATOR64
TEST(SanitizerCommon, SizeClassAllocator64Iteration) {
  TestSizeClassAllocatorIteration<Allocator64>();
}
#endif

TEST(SanitizerCommon, SizeClassAllocator32Iteration) {
  TestSizeClassAllocatorIteration<Allocator32Compact>();
}

TEST(SanitizerCommon, LargeMmapAllocatorIteration) {
  LargeMmapAllocator<> a;
  a.Init(/* may_return_null */ false);
  AllocatorStats stats;
  stats.Init();

  static const uptr kNumAllocs = 1000;
  char *allocated[kNumAllocs];
  static const uptr size = 40;
  // Allocate some.
  for (uptr i = 0; i < kNumAllocs; i++)
    allocated[i] = (char *)a.Allocate(&stats, size, 1);

  std::set<uptr> reported_chunks;
  a.ForceLock();
  a.ForEachChunk(IterationTestCallback, &reported_chunks);
  a.ForceUnlock();

  for (uptr i = 0; i < kNumAllocs; i++) {
    // Don't use EXPECT_NE. Reporting the first mismatch is enough.
    ASSERT_NE(reported_chunks.find(reinterpret_cast<uptr>(allocated[i])),
              reported_chunks.end());
  }
  for (uptr i = 0; i < kNumAllocs; i++)
    a.Deallocate(&stats, allocated[i]);
}

TEST(SanitizerCommon, LargeMmapAllocatorBlockBegin) {
  LargeMmapAllocator<> a;
  a.Init(/* may_return_null */ false);
  AllocatorStats stats;
  stats.Init();

  static const uptr kNumAllocs = 1024;
  static const uptr kNumExpectedFalseLookups = 10000000;
  char *allocated[kNumAllocs];
  static const uptr size = 4096;
  // Allocate some.
  for (uptr i = 0; i < kNumAllocs; i++) {
    allocated[i] = (char *)a.Allocate(&stats, size, 1);
  }

  a.ForceLock();
  for (uptr i = 0; i < kNumAllocs  * kNumAllocs; i++) {
    // if ((i & (i - 1)) == 0) fprintf(stderr, "[%zd]\n", i);
    char *p1 = allocated[i % kNumAllocs];
    EXPECT_EQ(p1, a.GetBlockBeginFastLocked(p1));
    EXPECT_EQ(p1, a.GetBlockBeginFastLocked(p1 + size / 2));
    EXPECT_EQ(p1, a.GetBlockBeginFastLocked(p1 + size - 1));
    EXPECT_EQ(p1, a.GetBlockBeginFastLocked(p1 - 100));
  }

  for (uptr i = 0; i < kNumExpectedFalseLookups; i++) {
    void *p = reinterpret_cast<void *>(i % 1024);
    EXPECT_EQ((void *)0, a.GetBlockBeginFastLocked(p));
    p = reinterpret_cast<void *>(~0L - (i % 1024));
    EXPECT_EQ((void *)0, a.GetBlockBeginFastLocked(p));
  }
  a.ForceUnlock();

  for (uptr i = 0; i < kNumAllocs; i++)
    a.Deallocate(&stats, allocated[i]);
}


#if SANITIZER_CAN_USE_ALLOCATOR64
// Regression test for out-of-memory condition in PopulateFreeList().
TEST(SanitizerCommon, SizeClassAllocator64PopulateFreeListOOM) {
  // In a world where regions are small and chunks are huge...
  typedef SizeClassMap<63, 128, 16> SpecialSizeClassMap;
  typedef SizeClassAllocator64<kAllocatorSpace, kAllocatorSize, 0,
                               SpecialSizeClassMap> SpecialAllocator64;
  const uptr kRegionSize =
      kAllocatorSize / SpecialSizeClassMap::kNumClassesRounded;
  SpecialAllocator64 *a = new SpecialAllocator64;
  a->Init();
  SizeClassAllocatorLocalCache<SpecialAllocator64> cache;
  memset(&cache, 0, sizeof(cache));
  cache.Init(0);

  // ...one man is on a mission to overflow a region with a series of
  // successive allocations.
  const uptr kClassID = 107;
  const uptr kAllocationSize = DefaultSizeClassMap::Size(kClassID);
  ASSERT_LT(2 * kAllocationSize, kRegionSize);
  ASSERT_GT(3 * kAllocationSize, kRegionSize);
  cache.Allocate(a, kClassID);
  EXPECT_DEATH(cache.Allocate(a, kClassID) && cache.Allocate(a, kClassID),
               "The process has exhausted");
  a->TestOnlyUnmap();
  delete a;
}
#endif

TEST(SanitizerCommon, TwoLevelByteMap) {
  const u64 kSize1 = 1 << 6, kSize2 = 1 << 12;
  const u64 n = kSize1 * kSize2;
  TwoLevelByteMap<kSize1, kSize2> m;
  m.TestOnlyInit();
  for (u64 i = 0; i < n; i += 7) {
    m.set(i, (i % 100) + 1);
  }
  for (u64 j = 0; j < n; j++) {
    if (j % 7)
      EXPECT_EQ(m[j], 0);
    else
      EXPECT_EQ(m[j], (j % 100) + 1);
  }

  m.TestOnlyUnmap();
}


typedef TwoLevelByteMap<1 << 12, 1 << 13, TestMapUnmapCallback> TestByteMap;

struct TestByteMapParam {
  TestByteMap *m;
  size_t shard;
  size_t num_shards;
};

void *TwoLevelByteMapUserThread(void *param) {
  TestByteMapParam *p = (TestByteMapParam*)param;
  for (size_t i = p->shard; i < p->m->size(); i += p->num_shards) {
    size_t val = (i % 100) + 1;
    p->m->set(i, val);
    EXPECT_EQ((*p->m)[i], val);
  }
  return 0;
}

TEST(SanitizerCommon, ThreadedTwoLevelByteMap) {
  TestByteMap m;
  m.TestOnlyInit();
  TestMapUnmapCallback::map_count = 0;
  TestMapUnmapCallback::unmap_count = 0;
  static const int kNumThreads = 4;
  pthread_t t[kNumThreads];
  TestByteMapParam p[kNumThreads];
  for (int i = 0; i < kNumThreads; i++) {
    p[i].m = &m;
    p[i].shard = i;
    p[i].num_shards = kNumThreads;
    PTHREAD_CREATE(&t[i], 0, TwoLevelByteMapUserThread, &p[i]);
  }
  for (int i = 0; i < kNumThreads; i++) {
    PTHREAD_JOIN(t[i], 0);
  }
  EXPECT_EQ((uptr)TestMapUnmapCallback::map_count, m.size1());
  EXPECT_EQ((uptr)TestMapUnmapCallback::unmap_count, 0UL);
  m.TestOnlyUnmap();
  EXPECT_EQ((uptr)TestMapUnmapCallback::map_count, m.size1());
  EXPECT_EQ((uptr)TestMapUnmapCallback::unmap_count, m.size1());
}

#endif  // #if !SANITIZER_DEBUG
