/*-
 * Copyright (c) 2004 Doug Rabson
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	$FreeBSD$
 */

/*
 * Define stubs for TLS internals so that programs and libraries can
 * link. These functions will be replaced by functional versions at
 * runtime from ld-elf.so.1.
 */

#include <sys/cdefs.h>
#include <sys/param.h>
#include <stdlib.h>
#include <string.h>
#include <elf.h>

#include "libc_private.h"

/* Provided by jemalloc to avoid bootstrapping issues. */
void	*__je_bootstrap_malloc(size_t size);
void	*__je_bootstrap_calloc(size_t num, size_t size);
void	__je_bootstrap_free(void *ptr);

__weak_reference(__libc_allocate_tls, _rtld_allocate_tls);
__weak_reference(__libc_free_tls, _rtld_free_tls);

#ifdef __i386__

__weak_reference(___libc_tls_get_addr, ___tls_get_addr);
__attribute__((__regparm__(1))) void * ___libc_tls_get_addr(void *);

#endif

void * __libc_tls_get_addr(void *);
__weak_reference(__libc_tls_get_addr, __tls_get_addr);

void *_rtld_allocate_tls(void *oldtls, size_t tcbsize, size_t tcbalign);
void _rtld_free_tls(void *tls, size_t tcbsize, size_t tcbalign);
void *__libc_allocate_tls(void *oldtls, size_t tcbsize, size_t tcbalign);
void __libc_free_tls(void *tls, size_t tcbsize, size_t tcbalign);

#if defined(__amd64__)
#define TLS_TCB_ALIGN 16
#elif defined(__aarch64__) || defined(__arm__) || defined(__i386__) || \
    defined(__mips__) || defined(__powerpc__) || defined(__riscv__) || \
    defined(__sparc64__)
#define TLS_TCB_ALIGN sizeof(void *)
#else
#error TLS_TCB_ALIGN undefined for target architecture
#endif

#if defined(__aarch64__) || defined(__arm__) || defined(__mips__) || \
    defined(__powerpc__) || defined(__riscv__)
#define TLS_VARIANT_I
#endif
#if defined(__i386__) || defined(__amd64__) || defined(__sparc64__)
#define TLS_VARIANT_II
#endif

#ifndef PIC

static size_t tls_static_space;
static size_t tls_init_size;
static void *tls_init;
#endif

#ifdef __i386__

/* GNU ABI */

__attribute__((__regparm__(1)))
void *
___libc_tls_get_addr(void *ti __unused)
{
	return (0);
}

#endif

void *
__libc_tls_get_addr(void *ti __unused)
{
	return (0);
}

#ifndef PIC

#ifdef TLS_VARIANT_I

#define	TLS_TCB_SIZE	(2 * sizeof(void *))

/*
 * Free Static TLS using the Variant I method.
 */
void
__libc_free_tls(void *tcb, size_t tcbsize, size_t tcbalign __unused)
{
	Elf_Addr *dtv;
	Elf_Addr **tls;

	tls = (Elf_Addr **)((Elf_Addr)tcb + tcbsize - TLS_TCB_SIZE);
	dtv = tls[0];
	__je_bootstrap_free(dtv);
	__je_bootstrap_free(tcb);
}

/*
 * Allocate Static TLS using the Variant I method.
 */
void *
__libc_allocate_tls(void *oldtcb, size_t tcbsize, size_t tcbalign __unused)
{
	Elf_Addr *dtv;
	Elf_Addr **tls;
	char *tcb;

	if (oldtcb != NULL && tcbsize == TLS_TCB_SIZE)
		return (oldtcb);

	tcb = __je_bootstrap_calloc(1, tls_static_space + tcbsize - TLS_TCB_SIZE);
	tls = (Elf_Addr **)(tcb + tcbsize - TLS_TCB_SIZE);

	if (oldtcb != NULL) {
		memcpy(tls, oldtcb, tls_static_space);
		__je_bootstrap_free(oldtcb);

		/* Adjust the DTV. */
		dtv = tls[0];
		dtv[2] = (Elf_Addr)tls + TLS_TCB_SIZE;
	} else {
		dtv = __je_bootstrap_malloc(3 * sizeof(Elf_Addr));
		tls[0] = dtv;
		dtv[0] = 1;
		dtv[1] = 1;
		dtv[2] = (Elf_Addr)tls + TLS_TCB_SIZE;

		if (tls_init_size > 0)
			memcpy((void*)dtv[2], tls_init, tls_init_size);
		if (tls_static_space > tls_init_size)
			memset((void*)(dtv[2] + tls_init_size), 0,
			    tls_static_space - tls_init_size);
	}

	return(tcb); 
}

#endif

#ifdef TLS_VARIANT_II

#define	TLS_TCB_SIZE	(3 * sizeof(Elf_Addr))

/*
 * Free Static TLS using the Variant II method.
 */
void
__libc_free_tls(void *tcb, size_t tcbsize __unused, size_t tcbalign)
{
	size_t size;
	Elf_Addr* dtv;
	Elf_Addr tlsstart, tlsend;

	/*
	 * Figure out the size of the initial TLS block so that we can
	 * find stuff which ___tls_get_addr() allocated dynamically.
	 */
	size = roundup2(tls_static_space, tcbalign);

	dtv = ((Elf_Addr**)tcb)[1];
	tlsend = (Elf_Addr) tcb;
	tlsstart = tlsend - size;
	__je_bootstrap_free((void*) tlsstart);
	__je_bootstrap_free(dtv);
}

/*
 * Allocate Static TLS using the Variant II method.
 */
void *
__libc_allocate_tls(void *oldtls, size_t tcbsize, size_t tcbalign)
{
	size_t size;
	char *tls;
	Elf_Addr *dtv;
	Elf_Addr segbase, oldsegbase;

	size = roundup2(tls_static_space, tcbalign);

	if (tcbsize < 2 * sizeof(Elf_Addr))
		tcbsize = 2 * sizeof(Elf_Addr);
	tls = __je_bootstrap_calloc(1, size + tcbsize);
	dtv = __je_bootstrap_malloc(3 * sizeof(Elf_Addr));

	segbase = (Elf_Addr)(tls + size);
	((Elf_Addr*)segbase)[0] = segbase;
	((Elf_Addr*)segbase)[1] = (Elf_Addr) dtv;

	dtv[0] = 1;
	dtv[1] = 1;
	dtv[2] = segbase - tls_static_space;

	if (oldtls) {
		/*
		 * Copy the static TLS block over whole.
		 */
		oldsegbase = (Elf_Addr) oldtls;
		memcpy((void *)(segbase - tls_static_space),
		    (const void *)(oldsegbase - tls_static_space),
		    tls_static_space);

		/*
		 * We assume that this block was the one we created with
		 * allocate_initial_tls().
		 */
		_rtld_free_tls(oldtls, 2*sizeof(Elf_Addr), sizeof(Elf_Addr));
	} else {
		memcpy((void *)(segbase - tls_static_space),
		    tls_init, tls_init_size);
		memset((void *)(segbase - tls_static_space + tls_init_size),
		    0, tls_static_space - tls_init_size);
	}

	return (void*) segbase;
}

#endif /* TLS_VARIANT_II */

#else

void *
__libc_allocate_tls(void *oldtls __unused, size_t tcbsize __unused,
	size_t tcbalign __unused)
{
	return (0);
}

void
__libc_free_tls(void *tcb __unused, size_t tcbsize __unused,
	size_t tcbalign __unused)
{
}

#endif /* PIC */

extern char **environ;

void
_init_tls(void)
{
#ifndef PIC
	Elf_Addr *sp;
	Elf_Auxinfo *aux, *auxp;
	Elf_Phdr *phdr;
	size_t phent, phnum;
	int i;
	void *tls;

	sp = (Elf_Addr *) environ;
	while (*sp++ != 0)
		;
	aux = (Elf_Auxinfo *) sp;
	phdr = NULL;
	phent = phnum = 0;
	for (auxp = aux; auxp->a_type != AT_NULL; auxp++) {
		switch (auxp->a_type) {
		case AT_PHDR:
			phdr = auxp->a_un.a_ptr;
			break;

		case AT_PHENT:
			phent = auxp->a_un.a_val;
			break;

		case AT_PHNUM:
			phnum = auxp->a_un.a_val;
			break;
		}
	}
	if (phdr == NULL || phent != sizeof(Elf_Phdr) || phnum == 0)
		return;

	for (i = 0; (unsigned) i < phnum; i++) {
		if (phdr[i].p_type == PT_TLS) {
			tls_static_space = roundup2(phdr[i].p_memsz,
			    phdr[i].p_align);
			tls_init_size = phdr[i].p_filesz;
			tls_init = (void*) phdr[i].p_vaddr;
		}
	}

#ifdef TLS_VARIANT_I
	/*
	 * tls_static_space should include space for TLS structure
	 */
	tls_static_space += TLS_TCB_SIZE;
#endif

	tls = _rtld_allocate_tls(NULL, TLS_TCB_SIZE, TLS_TCB_ALIGN);

	_set_tp(tls);
#endif
}
