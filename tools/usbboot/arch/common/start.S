.text
.globl _start

_start:
	adr sp, _start

	mov r7, #0
	ldr r8, =__bss_start
	ldr r9, =__bss_end

copy:
	cmp r8, r9
	strne r7, [r8], #4
	bne copy
	
	bl start
	b .
