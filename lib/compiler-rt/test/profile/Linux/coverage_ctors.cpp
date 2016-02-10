// RUN: %clang_profgen -x c++  -std=c++11 -fuse-ld=gold -fcoverage-mapping -o %t %s
// RUN: env LLVM_PROFILE_FILE=%t.profraw %run %t
// RUN: llvm-profdata merge -o %t.profdata %t.profraw
// RUN: llvm-cov show %t -instr-profile %t.profdata -filename-equivalence 2>&1 | FileCheck %s

struct Base {
  int B;
  Base() : B(0) {}
  Base(const Base &b2) {
    B = b2.B + 5;
  }
  Base(Base &&b2) {
    B = b2.B + 10;
  }
};

struct Derived : public Base {
  Derived(const Derived &) = default; // CHECK:  2| [[@LINE]]|  Derived(const Derived &) = default;
  Derived(Derived &&) = default;      // CHECK:  1| [[@LINE]]|  Derived(Derived &&) = default;
  Derived() = default;                // CHECK:  1| [[@LINE]]|  Derived() = default
};

Derived dd;
int main() {
  Derived dd2(dd);
  Derived dd3(dd2);
  Derived dd4(static_cast<Derived &&>(dd3));

  if (dd.B != 0 || dd2.B != 5 || dd3.B != 10 || dd4.B != 20)
    return 1;                         // CHECK: 0| [[@LINE]]|     return 1;
  return 0;
}
