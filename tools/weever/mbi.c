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
static uint64_t mbi_mmap0[] = {0x0, 0xfee00000, 1};
static uint64_t mbi_mmap1[] = {0xfee00000, 0x120000, 3};
static uint64_t mbi_mmap2[] = {0x100000000, 0x80000000, 1};
static struct multiboot_modinfo mbi_mods[9];
static struct multiboot_mmap mbi_mmaps[3];
static struct multiboot_info mbi;

struct multiboot_info *get_multiboot(void) {
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;
  mbi.cmdline = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi.mods_count = 9;
  mbi.mods_addr = (uint32_t)(uint64_t) mbi_mods;
  mbi_mods[0].mod_start = (uint32_t) 0x0;
  mbi_mods[0].mod_end = (uint32_t) 0xf9506;
  mbi_mods[0].string = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi_mods[1].mod_start = (uint32_t) 0xfa000;
  mbi_mods[1].mod_end = (uint32_t) 0x6cf4dc;
  mbi_mods[1].string = (uint32_t)(uint64_t) "/k1om/sbin/init ";
  mbi_mods[2].mod_start = (uint32_t) 0x6d0000;
  mbi_mods[2].mod_end = (uint32_t) 0xc4cef9;
  mbi_mods[2].string = (uint32_t)(uint64_t) "/k1om/sbin/mem_serv ";
  mbi_mods[3].mod_start = (uint32_t) 0xc4d000;
  mbi_mods[3].mod_end = (uint32_t) 0x1420874;
  mbi_mods[3].string = (uint32_t)(uint64_t) "/k1om/sbin/monitor ";
  mbi_mods[4].mod_start = (uint32_t) 0x1421000;
  mbi_mods[4].mod_end = (uint32_t) 0x1a54e8f;
  mbi_mods[4].string = (uint32_t)(uint64_t) "/k1om/sbin/ramfsd boot";
  mbi_mods[5].mod_start = (uint32_t) 0x1a55000;
  mbi_mods[5].mod_end = (uint32_t) 0x2066023;
  mbi_mods[5].string = (uint32_t)(uint64_t) "/k1om/sbin/skb boot";
  mbi_mods[6].mod_start = (uint32_t) 0x2067000;
  mbi_mods[6].mod_end = (uint32_t) 0x25e130b;
  mbi_mods[6].string = (uint32_t)(uint64_t) "/k1om/sbin/xeon_phi_mgr boot";
  mbi_mods[7].mod_start = (uint32_t) 0x25e2000;
  mbi_mods[7].mod_end = (uint32_t) 0x2f8f54f;
  mbi_mods[7].string = (uint32_t)(uint64_t) "/k1om/sbin/spawnd boot bootk1om=0-3";
  mbi_mods[8].mod_start = (uint32_t) 0x2f90000;
  mbi_mods[8].mod_end = (uint32_t) 0x38b07e4;
  mbi_mods[8].string = (uint32_t)(uint64_t) "/k1om/sbin/startd boot";
  mbi.mmap_length = sizeof(mbi_mmaps);
  mbi.mmap_addr = (uint32_t)(uint64_t) mbi_mmaps;
  mbi_mmaps[0].size = sizeof(struct multiboot_mmap);
  mbi_mmaps[0].base_addr = mbi_mmap0[0];
  mbi_mmaps[0].length = mbi_mmap0[1];
  mbi_mmaps[0].type = (int)mbi_mmap0[2];
  mbi_mmaps[1].size = sizeof(struct multiboot_mmap);
  mbi_mmaps[1].base_addr = mbi_mmap1[0];
  mbi_mmaps[1].length = mbi_mmap1[1];
  mbi_mmaps[1].type = (int)mbi_mmap1[2];
  mbi_mmaps[2].size = sizeof(struct multiboot_mmap);
  mbi_mmaps[2].base_addr = mbi_mmap2[0];
  mbi_mmaps[2].length = mbi_mmap2[1];
  mbi_mmaps[2].type = (int)mbi_mmap2[2];
  return &mbi;
}

