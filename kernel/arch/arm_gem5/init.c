/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <init.h>
#include <exceptions.h>
#include <exec.h>
#include <offsets.h>
#include <paging_kernel_arch.h>
#include <phys_mmap.h>
#include <serial.h>
#include <stdio.h>
#include <arm_hal.h>
#include <cpiobin.h>
#include <getopt/getopt.h>
#include <romfs_size.h>
#include <cp15.h>
#include <elf/elf.h>
#include <arm_core_data.h>
#include <startup_arch.h>
#include <kernel_multiboot.h>

#define GEM5_RAM_SIZE	0x2000000

extern errval_t early_serial_init(uint8_t port_no);


/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

/**
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
// XXX: This won't work if this kernel is not relocated from a pristine image!
//static uint32_t addr_global;

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)];

/**
 * Boot-up L1 page table for addresses up to 2GB (translated by TTBR0)
 */
static union arm_l1_entry boot_l1_low[ARM_L1_MAX_ENTRIES]
__attribute__ ((aligned(ARM_L1_ALIGN)));
/**
 * Boot-up L1 page table for addresses >=2GB (translated by TTBR1)
 */
static union arm_l1_entry boot_l1_high[ARM_L1_MAX_ENTRIES]
__attribute__ ((aligned(ARM_L1_ALIGN)));

//
// ATAG boot header declarations
//
// See: http://www.simtec.co.uk/products/SWLINUX/files/booting_article.html
//

static const uint32_t ATAG_NONE      = 0;
static const uint32_t ATAG_CORE      = 0x54410001;
static const uint32_t ATAG_MEM       = 0x54410002;
static const uint32_t ATAG_VIDEOTEXT = 0x54410003;
static const uint32_t ATAG_RAMDISK   = 0x54410004;
static const uint32_t ATAG_INITRD2   = 0x54420005;
static const uint32_t ATAG_SERIAL    = 0x54410006;
static const uint32_t ATAG_REVISION  = 0x54410007;
static const uint32_t ATAG_VIDEOLFB  = 0x54410008;
static const uint32_t ATAG_CMDLINE   = 0x54410009;

struct atag_header {
    uint32_t size;              // Size of header plus payload in 32-bit words
    uint32_t tag;               // Payload identifier
};

struct atag_core {
    uint32_t flags;             // bit 0 = r/o
    uint32_t page_bytes;
    uint32_t root_device;
};

struct atag_mem {
    uint32_t bytes;
    uint32_t start;
};

struct atag_videotext {
    uint8_t  width;
    uint8_t  height;
    uint16_t video_page;
    uint8_t  video_mode;
    uint8_t  video_cols;
    uint16_t video_ega_bx;
    uint8_t  video_lines;
    uint8_t  video_isvga;
    uint16_t video_points;
};

struct atag_ramdisk {
    uint32_t flags;             // Bit 0 = load, bit 1 = prompt
    uint32_t bytes;             // Decompressed size
    uint32_t start;       		// Starting block of RAM disk image
};

struct atag_initrd2 {
    uint32_t start;             // Physical start address
    uint32_t bytes;             // Copmressed disk image in bytes
};

struct atag_serial {
    uint32_t low;               // Lower order bits of board serial number
    uint32_t high;              // Upper order bits of board serial number
};

struct atag_revision {
    uint32_t board_revision;
};

struct atag_videolfb
{
    uint16_t lfb_width;
    uint16_t lfb_height;
    uint16_t lfb_depth;
    uint16_t lfb_linelength;
    uint32_t lfb_base;
    uint32_t lfb_size;
    uint8_t  red_size;
    uint8_t  red_pos;
    uint8_t  green_size;
    uint8_t  green_pos;
    uint8_t  bluint_te_size;
    uint8_t  bluint_te_pos;
    uint8_t  rsvd_size;
    uint8_t  rsvd_pos;
};

struct atag_cmdline
{
    char cmdline[1];
};

struct atag {
    struct atag_header header;
    union {
        struct atag_core         core;
        struct atag_mem          mem;
        struct atag_videotext    videotext;
        struct atag_ramdisk      ramdisk;
        struct atag_initrd2      initrd2;
        struct atag_serial       serial;
        struct atag_revision     revision;
        struct atag_videolfb     videolfb;
        struct atag_cmdline      cmdline;
    } u;
};


#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define CONSTRAIN(x, a, b) MIN(MAX(x, a), b)

//
// Kernel command line variables and binding options
//

static int timeslice		     = 5; //interval in ms in which the scheduler gets called
static int serial_console_port       = 2;
static int serial_debug_port         = 2;

static struct cmdarg cmdargs[] = {
    { "consolePort",    ArgType_Int, { .integer = &serial_console_port}},
    { "debugPort",      ArgType_Int, { .integer = &serial_debug_port}},
    { "loglevel",       ArgType_Int, { .integer = &kernel_loglevel }},
    { "logmask",        ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    { "timeslice",      ArgType_Int, { .integer = &timeslice }},
    {NULL, 0, {NULL}}
};

static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
	__asm volatile (
			"add	sp, sp, %[offset]\n\t" :: [offset] "r" (offset)
		);
}

static inline void __attribute__ ((always_inline))
relocate_got_base(lvaddr_t offset)
{
	__asm volatile (
			"add	r10, r10, %[offset]\n\t" :: [offset] "r" (offset)
		);
}

void enable_mmu(void);
void enable_mmu(void)
{
	__asm volatile (
			"ldr    r0, =0x55555555\n\t"       // Initial domain permissions
			"mcr    p15, 0, r0, c3, c0, 0\n\t"
			"ldr	r1, =0x1007\n\t" // Enable: D-Cache, I-Cache, Alignment, MMU 0x007 0x003 --> works
			"mrc	p15, 0, r0, c1, c0, 0\n\t"	// read out system configuration register
			"orr	r0, r0, r1\n\t"
			"mcr	p15, 0, r0, c1, c0, 0\n\t"	// enable MMU
		);
}

void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa);

static void paging_init(void)
{


	// configure system to use TTBR1 for VAs >= 2GB
	uint32_t ttbcr;
	ttbcr = cp15_read_ttbcr();
	ttbcr |= 1;
	cp15_write_ttbcr(ttbcr);

	lvaddr_t vbase = MEMORY_OFFSET, base =  0; // PHYS_MEMORY_START;

	for(size_t i=0; i < ARM_L1_MAX_ENTRIES/2; i++,
		base += ARM_L1_SECTION_BYTES, vbase += ARM_L1_SECTION_BYTES)
	{
		// create 1:1 mapping
		paging_map_device_section((uintptr_t)boot_l1_low, base, base);

		// Alias the same region at MEMORY_OFFSET (gem5 code)
		// create 1:1 mapping for pandaboard
		paging_map_device_section((uintptr_t)boot_l1_high, vbase, vbase);
	}

	// Activate new page tables
	cp15_write_ttbr1((lpaddr_t)&boot_l1_high[0]);
	//cp15_write_ttbr0((lpaddr_t)&boot_l1_high[0]);
        cp15_write_ttbr0((lpaddr_t)&boot_l1_low[0]);
}

void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
    timeslice = CONSTRAIN(timeslice, 1, 20);
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It sets up exception handling,
 * initializes devices and enables interrupts. After that it
 * calls arm_kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void  __attribute__ ((noinline,noreturn)) text_init(void)
{
	errval_t errval;
	// Relocate glbl_core_data to "memory"

    // Map-out low memory
    if(glbl_core_data->multiboot_flags & MULTIBOOT_INFO_FLAG_HAS_MMAP)
    {
    	struct arm_coredata_mmap *mmap = (struct arm_coredata_mmap *)
    			local_phys_to_mem(glbl_core_data->mmap_addr);

//        printf("v2 mmap_base %x\n", mmap->base_addr);
//        printf("v2 mmap_len %x\n", mmap->length);
//        printf("paging_arm_reset for %x of length %x!\n",
//                mmap->base_addr, mmap->length);
    	paging_arm_reset(mmap->base_addr, mmap->length);
        printf("paging_arm_reset v1 done for %x of length %x !\n",
                mmap->base_addr, mmap->length);
        printf("paging_arm_reset v2 done for %x of length %x !\n",
                mmap->base_addr, mmap->length);
        printf("paging_arm_reset v3 done for %x of length %x !\n",
                mmap->base_addr, mmap->length);
    }
    else
    {
        panic("need multiboot MMAP\n");
    }

	exceptions_init();
        printf("exceptions_init done!\n");

	kernel_startup_early();
        printf("kernel_startup_early done!\n");

	//initialize console
	 serial_console_init(serial_console_port);

	 // do not remove/change this printf: needed by regression harness
	 printf("Barrelfish CPU driver starting on ARMv7 Board id 0x%08"PRIx32"\n", hal_get_board_id());
	 printf("The address of paging_map_kernel_section is %p\n", paging_map_kernel_section);

	 errval = serial_debug_init(serial_debug_port);
	 if (err_is_fail(errval))
	 {
		 printf("Failed to initialize debug port: %d", serial_debug_port);
	 }

	 my_core_id = hal_get_cpu_id();
         printf("cpu id %d\n", my_core_id);


	 pic_init();
	 printf("pic_init done\n");

	 pit_init(timeslice, 0);
	 printf("pit_init 1 done\n");
	 pit_init(timeslice, 1);
	 printf("pic_init 2 done\n");
	 tsc_init();
	 printf("tsc_init done\n");
	 arm_kernel_startup();
}

/**
 * Entry point called from boot.S for bootstrap processor.
 * if is_bsp == true, then pointer points to multiboot_info
 * else pointer points to a global struct
 */

void arch_init(void *pointer)
{
    struct arm_coredata_elf *elf = NULL;
    early_serial_init(serial_console_port);

    // XXX: print kernel address for debugging with gdb
    printf("Kernel starting at address 0x%"PRIxLVADDR"\n", local_phys_to_mem((uint32_t)&kernel_first_byte));

    if(hal_cpu_is_bsp())
    {
        struct multiboot_info *mb = (struct multiboot_info *)pointer;
        elf = (struct arm_coredata_elf *)&mb->syms.elf;
    	memset(glbl_core_data, 0, sizeof(struct arm_core_data));
    	glbl_core_data->start_free_ram =
    	                ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&kernel_final_byte),
    	                         BASE_PAGE_SIZE);

        glbl_core_data->mods_addr = mb->mods_addr;
        glbl_core_data->mods_count = mb->mods_count;
        glbl_core_data->cmdline = mb->cmdline;
        glbl_core_data->mmap_length = mb->mmap_length;
        glbl_core_data->mmap_addr = mb->mmap_addr;
        glbl_core_data->multiboot_flags = mb->flags;
//        printf("mmap_base %x\n", mb->mmap_addr);
//        printf("mmap_len %x\n", mb->mmap_length);
    }
    /*
    if(hal_cpu_is_bsp()) {
        // Construct the global structure and store its address to retrive it
        // across relocation
        memset(&global->locks, 0, sizeof(global->locks));
        addr_global            = (uint32_t)global;
    }
*/

//	 pic_init();

    printf("At paging init\n");

    paging_init();

    printf("At MMU init\n");

    enable_mmu();

    printf("Relocating kernel to virtual memory\n");

    //align kernel dest to 16KB
    // Call aliased text_init() function and continue initialization
    text_init();
}
