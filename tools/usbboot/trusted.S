.global call_trusted
.arch_extension sec

call_trusted:
	stmfd sp!, {r4-r12,lr}
	mov r6, #0xff
	mov r12, #0
	dsb
	isb
	dmb
	smc 1
	ldmfd sp!, {r4-r12,lr}
	bx lr

