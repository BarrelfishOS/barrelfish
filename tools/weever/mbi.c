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
static struct multiboot_modinfo mbi_mods[11];
static struct multiboot_mmap mbi_mmaps[3];
static struct multiboot_info mbi;

struct multiboot_info *get_multiboot(void) {
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_CMDLINE;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MODS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;
  mbi.flags |= MULTIBOOT_INFO_FLAG_HAS_MMAP;
  mbi.cmdline = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi.mods_count = 11;
  mbi.mods_addr = (uint32_t)(uint64_t) mbi_mods;
  mbi_mods[0].mod_start = (uint32_t) 0x0;
  mbi_mods[0].mod_end = (uint32_t) 0xfc858;
  mbi_mods[0].string = (uint32_t)(uint64_t) "/k1om/sbin/cpu loglevel=4";
  mbi_mods[1].mod_start = (uint32_t) 0xfd000;
  mbi_mods[1].mod_end = (uint32_t) 0x6f6b65;
  mbi_mods[1].string = (uint32_t)(uint64_t) "/k1om/sbin/init ";
  mbi_mods[2].mod_start = (uint32_t) 0x6f7000;
  mbi_mods[2].mod_end = (uint32_t) 0xc97072;
  mbi_mods[2].string = (uint32_t)(uint64_t) "/k1om/sbin/mem_serv ";
  mbi_mods[3].mod_start = (uint32_t) 0xc98000;
  mbi_mods[3].mod_end = (uint32_t) 0x1489bdf;
  mbi_mods[3].string = (uint32_t)(uint64_t) "/k1om/sbin/monitor ";
  mbi_mods[4].mod_start = (uint32_t) 0x148a000;
  mbi_mods[4].mod_end = (uint32_t) 0x1ae322f;
  mbi_mods[4].string = (uint32_t)(uint64_t) "/k1om/sbin/ramfsd boot";
  mbi_mods[5].mod_start = (uint32_t) 0x1ae4000;
  mbi_mods[5].mod_end = (uint32_t) 0x211ac3e;
  mbi_mods[5].string = (uint32_t)(uint64_t) "/k1om/sbin/skb boot";
  mbi_mods[6].mod_start = (uint32_t) 0x211b000;
  mbi_mods[6].mod_end = (uint32_t) 0x276fc3d;
  mbi_mods[6].string = (uint32_t)(uint64_t) "/k1om/sbin/xeon_phi boot";
  mbi_mods[7].mod_start = (uint32_t) 0x2770000;
  mbi_mods[7].mod_end = (uint32_t) 0x315609a;
  mbi_mods[7].string = (uint32_t)(uint64_t) "/k1om/sbin/spawnd boot bootk1om=0,4,8,12,16";
  mbi_mods[8].mod_start = (uint32_t) 0x3157000;
  mbi_mods[8].mod_end = (uint32_t) 0x3aacf07;
  mbi_mods[8].string = (uint32_t)(uint64_t) "/k1om/sbin/startd boot";
  mbi_mods[9].mod_start = (uint32_t) 0x3aad000;
  mbi_mods[9].mod_end = (uint32_t) 0x40cba74;
  mbi_mods[9].string = (uint32_t)(uint64_t) "/k1om/sbin/xeon_phi_test ";
  mbi_mods[10].mod_start = (uint32_t) 0x40cc000;
  mbi_mods[10].mod_end = (uint32_t) 0x46eb226;
  mbi_mods[10].string = (uint32_t)(uint64_t) "/k1om/sbin/xeon_phi_inter ";
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

