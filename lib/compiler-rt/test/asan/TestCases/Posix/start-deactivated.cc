// Test for ASAN_OPTIONS=start_deactivated=1 mode.
// Main executable is uninstrumented, but linked to ASan runtime. The shared
// library is instrumented. Memory errors before dlopen are not detected.

// RUN: %clangxx_asan -O0 -DSHARED_LIB %s -fPIC -shared -o %t-so.so
// RUN: %clangxx -O0 %s -c -o %t.o
// RUN: %clangxx_asan -O0 %t.o %libdl -o %t
// RUN: %env_asan_opts=start_deactivated=1,allocator_may_return_null=0 \
// RUN:   ASAN_ACTIVATION_OPTIONS=allocator_may_return_null=1 not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %env_asan_opts=start_deactivated=1 \
// RUN:   ASAN_ACTIVATION_OPTIONS=help=1 not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK-HELP
// RUN: %env_asan_opts=start_deactivated=1,verbosity=1 \
// RUN:   ASAN_ACTIVATION_OPTIONS=help=1,handle_segv=0 not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK-UNSUPPORTED
// RUN: %env_asan_opts=start_deactivated=1 \
// RUN:   ASAN_ACTIVATION_OPTIONS=help=1,handle_segv=0 not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK-UNSUPPORTED-V0

// Check that verbosity=1 in activation flags affects reporting of unrecognized activation flags.
// RUN: %env_asan_opts=start_deactivated=1 \
// RUN:   ASAN_ACTIVATION_OPTIONS=help=1,handle_segv=0,verbosity=1 not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK-UNSUPPORTED

// XFAIL: arm-linux-gnueabi

#if !defined(SHARED_LIB)
#include <assert.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <string>

#include "sanitizer/asan_interface.h"

void test_malloc_shadow() {
  char *p = (char *)malloc(100);
  char *q = (char *)__asan_region_is_poisoned(p + 95, 8);
  fprintf(stderr, "=%zd=\n", q ? q - (p + 95) : -1);
  free(p);
}

typedef void (*Fn)();

int main(int argc, char *argv[]) {
  test_malloc_shadow();
  // CHECK: =-1=

  std::string path = std::string(argv[0]) + "-so.so";
  void *dso = dlopen(path.c_str(), RTLD_NOW);
  if (!dso) {
    fprintf(stderr, "dlopen failed: %s\n", dlerror());
    return 1;
  }

  test_malloc_shadow();
  // CHECK: =5=

  // After this line ASan is activated and starts detecting errors.
  void *fn = dlsym(dso, "do_another_bad_thing");
  if (!fn) {
    fprintf(stderr, "dlsym failed: %s\n", dlerror());
    return 1;
  }

  // Test that ASAN_ACTIVATION_OPTIONS=allocator_may_return_null=1 has effect.
  void *p = malloc((unsigned long)-2);
  assert(!p);
  // CHECK: WARNING: AddressSanitizer failed to allocate 0xfff{{.*}} bytes

  ((Fn)fn)();
  // CHECK: AddressSanitizer: heap-buffer-overflow
  // CHECK: READ of size 1
  // CHECK: {{#0 .* in do_another_bad_thing}}
  // CHECK: is located 5 bytes to the right of 100-byte region
  // CHECK: in do_another_bad_thing

  return 0;
}
#else  // SHARED_LIB
#include <stdio.h>
#include <stdlib.h>

extern "C" void do_another_bad_thing() {
  char *volatile p = (char *)malloc(100);
  printf("%hhx\n", p[105]);
}
#endif  // SHARED_LIB

// help=1 in activation flags lists only flags are are supported at activation
// CHECK-HELP: Available flags for {{.*}}Sanitizer:
// CHECK-HELP-NOT: handle_segv
// CHECK-HELP: max_redzone
// CHECK-HELP-NOT: handle_segv

// unsupported activation flags produce a warning ...
// CHECK-UNSUPPORTED: WARNING: found 1 unrecognized
// CHECK-UNSUPPORTED:   handle_segv

// ... but not at verbosity=0
// CHECK-UNSUPPORTED-V0-NOT: WARNING: found {{.*}} unrecognized
