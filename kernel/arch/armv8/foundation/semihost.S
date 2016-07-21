.global semihost_syscall

/* On call, we have:
   x0 - operation
   x1 - pointer to parameters
   We pass these directly to to semihosting syscall handler. */
semihost_syscall:
    str x30, [sp,#-16]!
    hlt #0xf000
    /* The return value, if any, is in x0 already. */
    ldr x30, [sp], #-16
    ret
