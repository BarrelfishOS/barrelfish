// RUN: %clangxx_asan -O0 -fsanitize=use-after-scope %s -o %t && \
// RUN: not %run %t 2>&1 | FileCheck %s
// RUN: %env_asan_opts=detect_stack_use_after_return=1 not %run %t 2>&1 | FileCheck %s
// XFAIL: *

int main() {
  int *p = 0;
  {
    int x = 0;
    p = &x;
  }
  return *p;  // BOOM
  // CHECK: ERROR: AddressSanitizer: stack-use-after-scope
  // CHECK:  #0 0x{{.*}} in main {{.*}}use-after-scope.cc:[[@LINE-2]]
  // CHECK: Address 0x{{.*}} is located in stack of thread T{{.*}} at offset [[OFFSET:[^ ]+]] in frame
  // {{\[}}[[OFFSET]], {{[0-9]+}}) 'x'
}
