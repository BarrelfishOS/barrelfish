// RUN: %clangxx_asan -O2 %s -o %t
// RUN: %run %t 2>&1 | FileCheck %s

const char *kAsanDefaultOptions="verbosity=1 help=1";

extern "C"
__attribute__((no_sanitize_address))
const char *__asan_default_options() {
  // CHECK: Available flags for AddressSanitizer:
  return kAsanDefaultOptions;
}

int main() {
  return 0;
}
