/*
 * Copyright (C) 2010 The Android Open Source Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *  * Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the 
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef _ABOOT_H_
#define _ABOOT_H_

#include <aboot/types.h>
#include <stdarg.h>

void serial_init(void);
void serial_putc(char c);
void serial_puts(const char *s);

void board_mux_init(void);
void board_ddr_init(void);

int printf(const char *fmt, ...);
int snprintf(char *str, size_t len, const char *fmt, ...);
int vsprintf(char *str, const char *fmt, va_list ap);
int vsnprintf(char *str, size_t len, const char *fmt, va_list ap);

int strlen(const char *s);
void memset(void *p, unsigned char c, unsigned len);
void *memcpy(void *_dst, const void *_src, unsigned count);

void enable_irqs(void);
void disable_irqs(void);

/* funky TI-style stuff */
void sr32(u32 addr, u32 start_bit, u32 num_bits, u32 value);
u32 wait_on_value(u32 read_bit_mask, u32 match_value, u32 read_addr, u32 bound);
void sdelay(unsigned long loops);

/* global configuration, changable by board file */
extern unsigned cfg_machine_type;
extern unsigned cfg_uart_base;

/* some extern functions */
extern void scale_vcores(void);
extern void prcm_init(void);
extern void gpmc_init(void);
extern void board_late_init(void);
extern void prcm_init(void);
extern void configure_core_dpll_no_lock(void);
extern void lock_core_dpll_shadow(void);

/* rev-id stuff */
typedef enum {
	OMAP_REV_INVALID,
	OMAP_4430_ES1_DOT_0,
	OMAP_4430_ES2_DOT_0,
	OMAP_4430_ES2_DOT_1,
	OMAP_4430_ES2_DOT_2,
	OMAP_4430_ES2_DOT_3,
	OMAP_4460_ES1_DOT_0,
	OMAP_4460_ES1_DOT_1,
}omap_rev;

extern omap_rev get_omap_rev(void);

#endif
