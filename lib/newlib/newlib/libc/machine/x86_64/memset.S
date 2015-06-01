/*
 * ====================================================
 * Copyright (C) 2007 by Ellips BV. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

  #include "x86_64mach.h"

  .global SYM (memset)
  SOTYPE_FUNCTION(memset)

SYM (memset):
  movq    rdi, r9                 /* Save return value */
  movq    rsi, rax
  movq    rdx, rcx
  cmpq    $16, rdx
  jb      byte_set

  movq    rdi, r8                 /* Align on quad word boundary */
  andq    $7, r8
  jz      quadword_aligned
  movq    $8, rcx
  subq    r8, rcx
  subq    rcx, rdx
  rep     stosb
  movq    rdx, rcx

quadword_aligned:
  movabs  $0x0101010101010101, r8
  movzbl  sil, eax
  imul    r8, rax
  cmpq    $256, rdx
  jb      quadword_set

  shrq    $7, rcx                 /* Store 128 bytes at a time with minimum cache polution */

  .p2align 4
loop:
#ifdef __k1om__
  /*
   * XXX: K1OM does not support movntiq instructions, using normal move instead
   * There may be a better way for doing this.
   */
  movq rax,     (rdi)
  movq rax,   8 (rdi)
  movq rax,  16 (rdi)
  movq rax,  24 (rdi)
  movq rax,  32 (rdi)
  movq rax,  40 (rdi)
  movq rax,  48 (rdi)
  movq rax,  56 (rdi)
  movq rax,  64 (rdi)
  movq rax,  72 (rdi)
  movq rax,  80 (rdi)
  movq rax,  88 (rdi)
  movq rax,  96 (rdi)
  movq rax, 104 (rdi)
  movq rax, 112 (rdi)
  movq rax, 120 (rdi)
#else
  movntiq rax,     (rdi)
  movntiq rax,   8 (rdi)
  movntiq rax,  16 (rdi)
  movntiq rax,  24 (rdi)
  movntiq rax,  32 (rdi)
  movntiq rax,  40 (rdi)
  movntiq rax,  48 (rdi)
  movntiq rax,  56 (rdi)
  movntiq rax,  64 (rdi)
  movntiq rax,  72 (rdi)
  movntiq rax,  80 (rdi)
  movntiq rax,  88 (rdi)
  movntiq rax,  96 (rdi)
  movntiq rax, 104 (rdi)
  movntiq rax, 112 (rdi)
  movntiq rax, 120 (rdi)
#endif
  leaq    128 (rdi), rdi

  dec     rcx
  jnz     loop
#ifdef __k1om__
  /*
   * XXX:
   * Though CPUID can be used as the fencing operation, better options are
   * "LOCK ADD [RSP],0" (a dummy atomic add) or XCHG (which combines a store
   *  and a fence)
   */
  lock add  $0, (rsp)
#else
  sfence
#endif
  movq    rdx, rcx
  andq    $127, rcx
  rep     stosb
  movq    r9, rax
  ret


byte_set:
  rep     stosb
  movq    r9, rax
  ret


quadword_set:
  shrq    $3, rcx
  .p2align 4
  rep     stosq
  movq    rdx, rcx
  andq    $7, rcx
  rep     stosb                   /* Store the remaining bytes */
  movq    r9, rax
  ret

