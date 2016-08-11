.section .text.boot
.globl _start

_start:
		/* obtain address of where _start was loaded */
		adr r7, _start
		adr r8, size
		ldr r8, [r8]

		/* image end (source) */
		add r7, r8, r7

		/* image start and end (destination) */
		ldr r8, =_start
		ldr r9, =__bss_end

copy:
		ldr r6, [r7], #-4
		str r6, [r9], #-4
		cmp r9, r8
		bne copy

		ldr r8, =aboot
		bx r8
		b .

size:
		.word __bss_end - _start

.global enable_irqs
.global disable_irqs

enable_irqs:
	cpsie I
	bx lr

disable_irqs:
	cpsid I
	bx lr
