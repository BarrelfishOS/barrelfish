/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __CP15_H__
#define __CP15_H__

#include <barrelfish_kpi/types.h>
#include <stdio.h>

/**
 * \brief Read domain access control register
 */
static inline uint32_t cp15_read_dacr(void)
{
    uint32_t dacr;
    __asm volatile("mrc   p15, 0, %[dacr], c3, c0, 0" : [dacr] "=r" (dacr));
    return dacr;
}

/**
 * \brief Write domain access control register
 */
static inline void cp15_write_dacr(uint32_t dacr)
{
    __asm volatile("mcr   p15, 0, %[dacr], c3, c0, 0" : : [dacr] "r" (dacr));
}

/**
 * \brief Read instruction fault status register.
 */
static inline uint32_t cp15_read_ifsr(void)
{
    uint32_t ifsr;
    __asm volatile("mrc   p15, 0, %[ifsr], c5, c0, 1" : [ifsr] "=r" (ifsr));
    return ifsr;
}

/**
 * \brief Read data fault status register.
 */
static inline uint32_t cp15_read_dfsr(void)
{
    uint32_t dfsr;
    __asm volatile("mrc   p15, 0, %[dfsr], c5, c0, 0" : [dfsr] "=r" (dfsr));
    return dfsr;
}

/**
 * \brief Read fault address register.
 */
static inline uint32_t cp15_read_far(void)
{
    uint32_t addr;
    __asm volatile(" mrc  p15, 0, %[addr], c6, c0, 0" : [addr] "=r" (addr));
    return addr;
}

static inline lpaddr_t cp15_read_ttbr0(void)
{
    lpaddr_t ttbr;
    __asm volatile(" mrc  p15, 0, %[ttbr], c2, c0, 0" : [ttbr] "=r" (ttbr));
    return ttbr;
}

static inline lpaddr_t cp15_read_ttbr1(void)
{
    lpaddr_t ttbr;
    __asm volatile(" mrc  p15, 0, %[ttbr], c2, c0, 1" : [ttbr] "=r" (ttbr));
    return ttbr;
}

static inline void cp15_write_ttbr0(lpaddr_t ttbr)
{
    __asm volatile(" mcr  p15, 0, %[ttbr], c2, c0, 0" :: [ttbr] "r" (ttbr));
}

static inline void cp15_write_ttbr1(lpaddr_t ttbr)
{
    __asm volatile(" mcr  p15, 0, %[ttbr], c2, c0, 1" :: [ttbr] "r" (ttbr));
}

static inline uint32_t cp15_read_ttbcr(void)
{
	uint32_t ttbcr;
	__asm volatile ("mrc p15, 0, %[ttbcr], c2, c0, 2" : [ttbcr] "=r" (ttbcr));
	return ttbcr;
}

static inline void cp15_write_ttbcr(uint32_t ttbcr)
{
	__asm volatile ("mcr p15, 0, %[ttbcr], c2, c0, 2" :: [ttbcr] "r" (ttbcr));
}

static inline uint32_t cp15_read_cache_status(void){
    uint32_t cache;
    __asm volatile("mrc   p15, 0, %[cache], c1, c0, 0" : [cache] "=r" (cache));
    return cache;
}

static inline uint8_t cp15_get_cpu_id(void) {
	uint8_t cpu_id;
	__asm volatile(
			"mrc 	p15, 0, %[cpu_id], c0, c0, 5\n\t" // get the MPIDR register
			"and	%[cpu_id], %[cpu_id], #0xF\n\t"
			:[cpu_id] "=r" (cpu_id)
		);

	return cpu_id;
}

/*
 * Get the configuration base address
 * This is described in the Cortex A9 TRM, 4.2.32
 */
static inline uint32_t cp15_read_cbar(void)
{
  uint32_t cbar;
  __asm volatile ("mrc p15, 4, %[cbar], c15, c0, 0" : [cbar] "=r" (cbar));
  return cbar & ~0x1FFF; // Only [31:13] is valid
}

static inline void cp15_write_contextidr(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c13, c0, 1" :: [x] "r" (x));
}

static inline uint32_t cp15_read_sctlr(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c1, c0, 0" : [x] "=r" (x));
  return x;
}

static inline void cp15_write_sctlr(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c1, c0, 0" :: [x] "r" (x));
}

static inline void cp15_write_vbar(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c12, c0, 0" :: [x] "r" (x));
}

/* CPUID registers. */
static inline uint32_t cp15_read_id_pfr0(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c1, 0" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_id_pfr1(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c1, 1" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_midr(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c0, 0" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_ctr(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c0, 1" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_id_dfr0(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c1, 2" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_id_afr0(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c0, c1, 3" : [x] "=r" (x));
  return x;
}

static inline uint32_t cp15_read_tpidruro(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 0, %[x], c13, c0, 3" : [x] "=r" (x));
  return x;
}

static inline void cp15_write_tpidruro(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c13, c0, 3" :: [x] "r" (x));
}

static inline uint32_t cp15_read_clidr(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 1, %[x], c0, c0, 1" : [x] "=r" (x));
  return x;
}

static inline void cp15_write_csselr(uint32_t x)
{
	__asm volatile ("mcr p15, 2, %[x], c0, c0, 0" :: [x] "r" (x));
}

static inline uint32_t cp15_read_ccsidr(void)
{
  uint32_t x;
  __asm volatile ("mrc p15, 1, %[x], c0, c0, 0" : [x] "=r" (x));
  return x;
}

static inline void cp15_write_dcisw(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c6, 2" :: [x] "r" (x));
}

static inline void cp15_write_dccisw(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c14, 2" :: [x] "r" (x));
}

static inline void cp15_write_iciallu(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c5, 0" :: [x] "r" (x));
}

static inline void cp15_write_tlbiall(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c8, c7, 0" :: [x] "r" (x));
}

static inline void cp15_write_dccmvau(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c11, 1" :: [x] "r" (x));
}

static inline void cp15_write_dccmvac(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c10, 1" :: [x] "r" (x));
}

static inline void cp15_write_dcimvac(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c6, 1" :: [x] "r" (x));
}

static inline void cp15_write_dccimvac(uint32_t x)
{
	__asm volatile ("mcr p15, 0, %[x], c7, c14, 1" :: [x] "r" (x));
}

static inline void dsb(void) { __asm volatile ("dsb"); }
static inline void dmb(void) { __asm volatile ("dmb"); }
static inline void isb(void) { __asm volatile ("isb"); }
static inline void sev(void) { __asm volatile ("sev"); }
static inline void wfe(void) { __asm volatile ("wfe"); }

#endif // __CP15_H__
