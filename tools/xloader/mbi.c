#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>
#include "mbi.h"
#include "../kernel/include/multiboot.h"
static struct multiboot_modinfo mbi_mods[2];
static struct multiboot_mmap mbi_mmaps[0];
static struct multiboot_info mbi;

struct multiboot_info *get_multiboot(void) {
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;
  mbi.cmdline = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi.mods_count = 2;
  mbi.mods_addr = (uint32_t)(uint64_t) mbi_mods;
  mbi_mods[0].mod_start = (uint32_t) 0x0;
  mbi_mods[0].mod_end = (uint32_t) 0xf2f09;
  mbi_mods[0].string = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi_mods[1].mod_start = (uint32_t) 0xf3000;
  mbi_mods[1].mod_end = (uint32_t) 0x608c13;
  mbi_mods[1].string = (uint32_t)(uint64_t) "/k1om/sbin/init_null ";
  mbi.mmap_length = sizeof(mbi_mmaps);
  mbi.mmap_addr = (uint32_t)(uint64_t) mbi_mmaps;
  return &mbi;
}

