/*
 * Copyright (c) 2011 Konstantin Belousov <kib@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sys/cdefs.h>
__FBSDID("$FreeBSD$");

#include <sys/types.h>
#include <sys/ucontext.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <machine/cpufunc.h>
#include <machine/fpu.h>
#include <machine/specialreg.h>
#include <machine/sysarch.h>

static int xstate_sz = -1;

int
__getcontextx_size(void)
{
	u_int p[4];

	if (xstate_sz == -1) {
		do_cpuid(1, p);
		if ((p[2] & CPUID2_OSXSAVE) != 0) {
			cpuid_count(0xd, 0x0, p);
			xstate_sz = p[1] - sizeof(struct savefpu);
		} else
			xstate_sz = 0;
	}

	return (sizeof(ucontext_t) + xstate_sz);
}

int
__fillcontextx2(char *ctx)
{
	struct amd64_get_xfpustate xfpu;
	ucontext_t *ucp;

	ucp = (ucontext_t *)ctx;
	if (xstate_sz != 0) {
		xfpu.addr = (char *)(ucp + 1);
		xfpu.len = xstate_sz;
		if (sysarch(AMD64_GET_XFPUSTATE, &xfpu) == -1)
			return (-1);
		ucp->uc_mcontext.mc_xfpustate = (__register_t)xfpu.addr;
		ucp->uc_mcontext.mc_xfpustate_len = xstate_sz;
		ucp->uc_mcontext.mc_flags |= _MC_HASFPXSTATE;
	} else {
		ucp->uc_mcontext.mc_xfpustate = 0;
		ucp->uc_mcontext.mc_xfpustate_len = 0;
	}
	return (0);
}

int
__fillcontextx(char *ctx)
{
	ucontext_t *ucp;

	ucp = (ucontext_t *)ctx;
	if (getcontext(ucp) == -1)
		return (-1);
	__fillcontextx2(ctx);
	return (0);
}

__weak_reference(__getcontextx, getcontextx);

ucontext_t *
__getcontextx(void)
{
	char *ctx;
	int error;

	ctx = malloc(__getcontextx_size());
	if (ctx == NULL)
		return (NULL);
	if (__fillcontextx(ctx) == -1) {
		error = errno;
		free(ctx);
		errno = error;
		return (NULL);
	}
	return ((ucontext_t *)ctx);
}
