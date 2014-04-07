/*
 * Based on snippets from
 * https://github.com/macmade/LibC-String-Optimisations/blob/master/source/memset.64.s
 */
.text
.globl memset_128b_sse2_, memset_128b_

/*
 * we get the base address in rdi, the character in rsi, and the #pages in rdx
 */
memset_128b_sse2_:
    /* calculate the number of iterations necessary:
     * rdx * BASE_PAGE_SIZE / 256 = (rdx << 12) >> 78= rdx << 4
     */
    shlq $4, %rdx
    pxor %xmm0, %xmm0
    test %rsi, %rsi
    jz memset_128b_sse2_loop_
    /* create pattern, if rsi!=0 */
    movq %rsi, %rax
    shlq $8, %rsi
    orq  %rsi, %rax
    movq %rax, %rsi
    shlq $16, %rsi
    orq  %rax, %rsi
    /* move pattern to xmm0 */
    movd %rsi, %xmm0
    shufps  $0, %xmm0, %xmm0
    /* memset */
memset_128b_sse2_loop_:
    movdqa %xmm0, (%rdi)
    movdqa %xmm0, 0x10(%rdi)
    movdqa %xmm0, 0x20(%rdi)
    movdqa %xmm0, 0x30(%rdi)
    movdqa %xmm0, 0x40(%rdi)
    movdqa %xmm0, 0x50(%rdi)
    movdqa %xmm0, 0x60(%rdi)
    movdqa %xmm0, 0x70(%rdi)
    movdqa %xmm0, 0x80(%rdi)
    movdqa %xmm0, 0x90(%rdi)
    movdqa %xmm0, 0xA0(%rdi)
    movdqa %xmm0, 0xB0(%rdi)
    movdqa %xmm0, 0xC0(%rdi)
    movdqa %xmm0, 0xD0(%rdi)
    movdqa %xmm0, 0xE0(%rdi)
    movdqa %xmm0, 0xF0(%rdi)
    leaq 256(%rdi), %rdi
    dec %rdx
    jnz memset_128b_sse2_loop_

    ret

memset_128b_:
    ret
