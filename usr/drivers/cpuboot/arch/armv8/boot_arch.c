/**
 * \file
 * \brief Boot driver arch specific parts for ARM CPUs
 */
/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */



#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/platform.h>
#include <barrelfish/syscall_arch.h>
#include <target/aarch64/barrelfish_kpi/arm_core_data.h>
#include <offsets.h>
#include <barrelfish/deferred.h>
#include <acpi_client/acpi_client.h>
#include <if/acpi_defs.h>

#include <hw_records_arch.h>

#include <skb/skb.h>


#include "../../coreboot.h"

extern coreid_t my_arch_id;
extern struct capref ipi_cap;

/// XXX: make this configurable...
#define ARMV8_KERNEL_STACK_SIZE (16 * 1024)

struct armv8_parking_page
{
    uint32_t processor_id;
    uint32_t reserved;
    genpaddr_t jump_address;
    genpaddr_t context;
    uint8_t reserved_os[2048 - 24];
    uint8_t reserved_firmware[2048];
};
STATIC_ASSERT_SIZEOF(struct armv8_parking_page, 4096);


static void parking_write_mailbox(struct armv8_parking_page *mailbox,
                                  uint32_t procid, genpaddr_t entry,
                                  genpaddr_t context)
{
    mailbox->context = context;

    /* Change the Processor ID to all ones */
    mailbox->processor_id = 0xffffffff;

    /* Execute a data synchronization barrier */
    __asm volatile("dsb   sy\n"
                   "dmb   sy\n"
                   "isb     \n");

    /* Change the jump address to the required value */
    mailbox ->jump_address = entry;

    /* Execute a data synchronization barrier */
    __asm volatile("dsb   sy\n"
                   "dmb   sy\n"
                   "isb     \n");


    /* Program the correct Processor ID in the mailbox */
    mailbox->processor_id = procid;
}




static inline errval_t
invoke_monitor_spawn_core(hwid_t core_id, enum cpu_type cpu_type,
                          genpaddr_t entry, genpaddr_t context)
{
    return cap_invoke5(ipi_cap, IPICmd_Send_Start, core_id, cpu_type,
                       entry, context).error;
}

struct arch_config
{
    genpaddr_t arch_page_size;
    size_t stack_size;
    char boot_driver_binary[256];
    char boot_driver_entry[256];
    char cpu_driver_binary[256];
    char monitor_binary[256];
};

static errval_t get_arch_config(hwid_t hwid, struct arch_config * config)
{
    errval_t err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connect to SKB.");
    }

    /* Query the SKB for the CPU driver to use. */
    err= skb_execute_query("cpu_driver(S), write(res(S)).");
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", config->cpu_driver_binary);
    if (err_is_fail(err)) return err;

    /* Query the SKB for the monitor binary to use. */
    err= skb_execute_query("monitor(S), write(res(S)).");
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", config->monitor_binary);
    if (err_is_fail(err)) return err;

    err = skb_execute_query("boot_driver_entry(%"PRIu64",T), entry_symbol(T,S),"
                            " write(res(S)).", hwid);
    if (err_is_fail(err)) {
        printf("error: \n %s\n", skb_get_error_output());
        return err;
    }

    err= skb_read_output("res(%255[^)])", config->boot_driver_entry);
    if (err_is_fail(err)) {
        return err;
    }


    err = skb_execute_query("boot_driver(S), write(res(S)).");
    if (err_is_fail(err)) {
        printf("error: \n %s\n", skb_get_error_output());
        return err;
    }

    err= skb_read_output("res(%255[^)])", config->boot_driver_binary);
    if (err_is_fail(err)) {
        return err;
    }

    config->arch_page_size= BASE_PAGE_SIZE;
    config->stack_size = ARMV8_KERNEL_STACK_SIZE;

    return SYS_ERR_OK;
}


struct mem_info {
    size_t                size;
    struct capref         cap;
    void                  *buf;
    struct frame_identity frameid;
};

static errval_t mem_alloc(size_t size, bool map, struct mem_info *mem_info)
{
    errval_t err;

    DEBUG("mem_alloc=%zu bytes\n", size);

    memset(mem_info, 0, sizeof(*mem_info));

    err = frame_alloc(&mem_info->cap, size, &mem_info->size);
    if (err_is_fail(err)) {
        return err;
    }

    err = invoke_frame_identify(mem_info->cap, &mem_info->frameid);
    if (err_is_fail(err)) {
        err =  err_push(err, LIB_ERR_FRAME_IDENTIFY);
        goto out_err;
    }

    if (map) {
        err = vspace_map_one_frame(&mem_info->buf, mem_info->size, mem_info->cap,
                                   NULL, NULL);
        if (err_is_fail(err)) {
            err =  err_push(err, LIB_ERR_VSPACE_MAP);
            goto out_err;
        }
    }

    // Mark memory as remote
    err = cap_mark_remote(mem_info->cap);
    if (err_is_fail(err)) {
        vspace_unmap(mem_info->buf);
        goto out_err;
    }

    return SYS_ERR_OK;

out_err:
    cap_delete(mem_info->cap);
    memset(mem_info, 0, sizeof(*mem_info));
    return err;
}

static errval_t mem_free(struct mem_info *mem_info)
{
    errval_t err;

    if (mem_info->buf) {
        err = vspace_unmap(mem_info->buf);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to unmap\n");
        }
    }
    if (!capref_is_null(mem_info->cap)) {
        err = cap_destroy(mem_info->cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to cap destory");
        }
    }

    return SYS_ERR_OK;
}


static errval_t cpu_memory_alloc(size_t size, struct mem_info *mem_info)
{
    return mem_alloc(size, true, mem_info);
}

static errval_t app_memory_alloc(size_t size, struct mem_info *mem_info)
{
    return mem_alloc(size, false, mem_info);
}


struct module_blob {
    size_t             size;    ///< size of the binary in memory
    lvaddr_t           vaddr;   ///< virtual address of the binary in memory
    genpaddr_t         paddr;   ///< physical address of the memory
    struct capref      frame;
    struct mem_region *mem_region;
};

static errval_t
get_module_info(const char *name, struct module_blob *blob)
{
    errval_t err;

    DEBUG("getting module %s\n", name);

    err = lookup_module(name, &blob->vaddr,
                        &blob->paddr, &blob->size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err_push(err, SPAWN_ERR_FIND_MODULE);
    }

    return SYS_ERR_OK;
}


static errval_t
relocate_elf(struct module_blob *binary, struct mem_info *mem,
            lvaddr_t kernel_offset) {

    DEBUG("Relocating kernel image.\n");

    struct Elf64_Ehdr *ehdr = (struct Elf64_Ehdr *)binary->vaddr;

    size_t shnum  = ehdr->e_shnum;
    struct Elf64_Phdr *phdr = (struct Elf64_Phdr *)(binary->vaddr + ehdr->e_phoff);
    struct Elf64_Shdr *shead = (struct Elf64_Shdr *)(binary->vaddr + (uintptr_t)ehdr->e_shoff);

    /* Search for relocaton sections. */
    for(size_t i= 0; i < shnum; i++) {

        struct Elf64_Shdr *shdr=  &shead[i];
        if(shdr->sh_type == SHT_REL || shdr->sh_type == SHT_RELA) {
            if(shdr->sh_info != 0) {
                DEBUG("I expected global relocations, but got"
                              " section-specific ones.\n");
                return ELF_ERR_HEADER;
            }


            uint64_t segment_elf_base= phdr[0].p_vaddr;
            uint64_t segment_load_base=mem->frameid.base;
            uint64_t segment_delta= segment_load_base - segment_elf_base;
            uint64_t segment_vdelta= (uintptr_t)mem->buf - segment_elf_base;

            size_t rsize;
            if(shdr->sh_type == SHT_REL){
                rsize= sizeof(struct Elf64_Rel);
            } else {
                rsize= sizeof(struct Elf64_Rela);
            }

            assert(rsize == shdr->sh_entsize);
            size_t nrel= shdr->sh_size / rsize;

            void * reldata = (void*)(binary->vaddr + shdr->sh_offset);

            /* Iterate through the relocations. */
            for(size_t ii= 0; ii < nrel; ii++) {
                void *reladdr= reldata + ii *rsize;

                switch(shdr->sh_type) {
                    case SHT_REL:
                        DEBUG("SHT_REL unimplemented.\n");
                        return ELF_ERR_PROGHDR;
                    case SHT_RELA:
                    {
                        struct Elf64_Rela *rel= reladdr;

                        uint64_t offset= rel->r_offset;
                        uint64_t sym= ELF64_R_SYM(rel->r_info);
                        uint64_t type= ELF64_R_TYPE(rel->r_info);
                        uint64_t addend= rel->r_addend;

                        uint64_t *rel_target= (void *)offset + segment_vdelta;

                        switch(type) {
                            case R_AARCH64_RELATIVE:
                                if(sym != 0) {
                                    DEBUG("Relocation references a"
                                                 " dynamic symbol, which is"
                                                 " unsupported.\n");
                                    return ELF_ERR_PROGHDR;
                                }

                                /* Delta(S) + A */
                                *rel_target= addend + segment_delta + kernel_offset;
                                break;

                            default:
                                DEBUG("Unsupported relocation type %d\n",
                                             type);
                                return ELF_ERR_PROGHDR;
                        }
                    }
                    break;
                    default:
                        DEBUG("Unexpected type\n");
                        break;

                }
            }
        }
    }

    return SYS_ERR_OK;
}

static errval_t elf_check_header(lvaddr_t addr, size_t size)
{
    struct Elf64_Ehdr   *ehdr = (struct Elf64_Ehdr *)addr;

    // Check for valid file size
    if (size < sizeof(struct Elf64_Ehdr)) {
        return ELF_ERR_FILESZ;
    }

    if(ehdr->e_ident[EI_CLASS] != ELFCLASS64 || ehdr->e_ident[EI_DATA] != ELFDATA2LSB) {
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_ident[EI_OSABI] != ELFOSABI_STANDALONE
        && ehdr->e_ident[EI_OSABI] != ELFOSABI_NONE) {
        DEBUG("Warning: Compiled for OS ABI %d.  Wrong compiler?\n",
                     ehdr->e_ident[EI_OSABI]);
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_machine != EM_AARCH64) {
        DEBUG( "Error: Not AArch64\n");
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_type != ET_EXEC) {
        DEBUG("Warning: CPU driver isn't executable! Continuing anyway.\n");
    }

    // More sanity checks
    if (ehdr->e_phoff + ehdr->e_phentsize * ehdr->e_phnum > size
        || ehdr->e_phentsize != sizeof(struct Elf64_Phdr)) {
        return ELF_ERR_PROGHDR;
    }

    return SYS_ERR_OK;
}

static errval_t load_elf_binary(struct module_blob *binary, struct mem_info *mem,
                         genvaddr_t entry_point, genvaddr_t *reloc_entry_point)

{

    struct Elf64_Ehdr *ehdr = (struct Elf64_Ehdr *)binary->vaddr;

    /* Load the CPU driver from its ELF image. */
    bool found_entry_point= 0;
    bool loaded = 0;

    struct Elf64_Phdr *phdr = (struct Elf64_Phdr *)(binary->vaddr + ehdr->e_phoff);
    for(size_t i= 0; i < ehdr->e_phnum; i++) {
        if(phdr[i].p_type != PT_LOAD) {
            DEBUG("Segment %d load address 0x% "PRIx64 ", file size %" PRIu64
                  ", memory size 0x%" PRIx64 " SKIP\n", i, phdr[i].p_vaddr,
                  phdr[i].p_filesz, phdr[i].p_memsz);
            continue;
        }

        DEBUG("Segment %d load address 0x% "PRIx64 ", file size %" PRIu64
              ", memory size 0x%" PRIx64 " LOAD\n", i, phdr[i].p_vaddr,
              phdr[i].p_filesz, phdr[i].p_memsz);


        if (loaded) {
            USER_PANIC("Expected one load able segment!\n");
        }
        loaded = 1;

        void *dest = mem->buf;
        lpaddr_t dest_phys = mem->frameid.base;

        assert(phdr[i].p_offset + phdr[i].p_memsz <= mem->frameid.bytes);

        /* copy loadable part */
        memcpy(dest, (void *)(binary->vaddr + phdr[i].p_offset), phdr[i].p_filesz);

        /* zero out BSS section */
        memset(dest + phdr[i].p_filesz, 0, phdr[i].p_memsz - phdr[i].p_filesz);

        if (!found_entry_point) {
            if(entry_point >= phdr[i].p_vaddr
                 && entry_point - phdr[i].p_vaddr < phdr[i].p_memsz) {
               *reloc_entry_point= (dest_phys + (entry_point - phdr[i].p_vaddr));
               found_entry_point= 1;
            }
        }
    }

    if (!found_entry_point) {
        USER_PANIC("No entry point loaded\n");
    }

    return SYS_ERR_OK;
}


static errval_t elf_find_entry(struct module_blob *binary, const char *sym,
                               genvaddr_t *ret_entry)
{
    if (sym && strlen(sym) > 0) {
        DEBUG("Looking for entry: '%s'\n", sym);
        struct Elf64_Sym *entry;
        entry = elf64_find_symbol_by_name(binary->vaddr, binary->size, sym, 0,
                                          STT_FUNC, 0);
        if (!entry) {
            DEBUG("Entry '%s' not found\n", sym);
            return ELF_ERR_PROGHDR;
        }
        *ret_entry = entry->st_value;
    } else {
        *ret_entry = ((struct Elf64_Ehdr *)binary->vaddr)->e_entry;
    }

    return SYS_ERR_OK;
}

static errval_t load_boot_and_cpu_driver(struct arch_config *cfg,
                                         struct module_blob *boot_driver,
                                         struct mem_info *boot_mem,
                                         struct module_blob *cpu_driver,
                                         struct mem_info *cpu_mem,
                                         genvaddr_t *ret_boot_entry,
                                         genvaddr_t *ret_cpu_entry) {

    errval_t err;

    err = elf_check_header(boot_driver->vaddr, boot_driver->size);
    if (err_is_fail(err)) {
        return err;
    }

    err = elf_check_header(boot_driver->vaddr, cpu_driver->size);
    if (err_is_fail(err)) {
        return err;
    }

    genvaddr_t boot_entry_point = 0;
    err = elf_find_entry(boot_driver, cfg->boot_driver_entry, &boot_entry_point);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("Unrelocated entry point in bootdriver: '%s' @ %" PRIxGENVADDR "\n",
                 cfg->boot_driver_entry, boot_entry_point);

    genvaddr_t cpu_entry_point = 0;
    err = elf_find_entry(cpu_driver, "arch_init", &cpu_entry_point);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("Unrelocated entry point in cpu driver: '%s' @ %" PRIxGENVADDR "\n",
                 "arch_init", cpu_entry_point);


    err = load_elf_binary(boot_driver, boot_mem, boot_entry_point, &boot_entry_point);
    if (err_is_fail(err)) {
        return err;
    }

    err = load_elf_binary(cpu_driver, cpu_mem, cpu_entry_point, &cpu_entry_point);
    if (err_is_fail(err)) {
        return err;
    }

    err = relocate_elf(boot_driver, boot_mem, 0);
    if (err_is_fail(err)) {
        return err;
    }

    err = relocate_elf(cpu_driver, cpu_mem, KERNEL_OFFSET);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("Relocated boot driver point is %p\n",boot_entry_point);
    DEBUG("Relocated cpu driver point is %p\n", cpu_entry_point);

    *ret_boot_entry = boot_entry_point;
    *ret_cpu_entry = cpu_entry_point + KERNEL_OFFSET;

    return SYS_ERR_OK;
}


static errval_t get_boot_protocol(coreid_t core_id, uint32_t *parking_version,
                                  struct mem_info *parking_page)
{
    errval_t err;

    char* record = NULL;
    err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }

    uint64_t parked_address, _parking_version;
    err = oct_read(record, "_ { parkingVersion: %d, parkedAddress: %d}",
            &_parking_version, &parked_address);
    if (err_is_fail(err)) {
        goto out;
    }

    *parking_version = _parking_version;

    debug_printf("Parking Version: %u, ParkedAddress= 0x%lx\n", *parking_version,
                  parked_address);

    if (*parking_version) {
        struct acpi_binding* acl = get_acpi_binding();

        err = slot_alloc(&parking_page->cap);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "slot_alloc for mm_realloc_range_proxy");
        }
        errval_t error_code;
        err = acl->rpc_tx_vtbl.mm_realloc_range_proxy(acl, 12, parked_address,
                                                      &parking_page->cap,
                                                      &error_code);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "mm_alloc_range_proxy failed.");
            goto out;
        }
        if (err_is_fail(error_code)) {
            DEBUG_ERR(error_code, "mm_alloc_range_proxy return failed.");
            err = error_code;
            goto out;
        }

        err = vspace_map_one_frame(&parking_page->buf, 4096, parking_page->cap,
                                   NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to map parking page\n");
            goto out;
        }

        parking_page->size = 4096;
        parking_page->frameid.base = parked_address;
        parking_page->frameid.bytes = 4096;

    } else {
        memset(parking_page, 0, sizeof(*parking_page));
        *parking_version = 0;
    }

    out:
    if (record) {
        free(record);
    }
    return err;

}


errval_t spawn_xcore_monitor(coreid_t coreid, hwid_t hwid,
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{

    DEBUG("Booting: %" PRIuCOREID ", hwid=%" PRIxHWID "\n", coreid, hwid);

    struct arch_config config;

    errval_t err;

    if(cpu_type != CPU_ARM8) {
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    err = get_arch_config(hwid, &config);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to obtain architecture configuration");
        return err;
    }

    struct mem_info parking_mem;
    uint32_t parking_version = 0;
    err = get_boot_protocol(coreid, &parking_version, &parking_mem);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("boot driver: %s\n", config.cpu_driver_binary);
    DEBUG("boot driver entry: %s\n", config.boot_driver_entry);
    DEBUG("cpu_driver: %s\n", config.cpu_driver_binary);
    DEBUG("monitor: %s\n", config.monitor_binary);


    // compute size of frame needed and allocate it
    DEBUG("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    DEBUG("%s:%s:%d: urpc_frame_id.size=0x%" PRIuGENSIZE "\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bytes);


    // XXX: Caching these for now, until we have unmap

    struct module_blob boot_binary;
    err = get_module_info(config.boot_driver_binary, &boot_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    struct module_blob cpu_binary;
    err = get_module_info(config.cpu_driver_binary, &cpu_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    struct module_blob monitor_binary;
    err = get_module_info(config.monitor_binary, &monitor_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    size_t elf_size = ROUND_UP(elf_virtual_size(boot_binary.vaddr),
                               config.arch_page_size);

    struct mem_info boot_mem;
    err = cpu_memory_alloc(elf_size, &boot_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("BOOTMEM: %lx, %zu kb\n", boot_mem.frameid.base, boot_mem.size >> 10);

    /* */

    elf_size = ROUND_UP(elf_virtual_size(cpu_binary.vaddr), config.arch_page_size);

    struct mem_info cpu_mem;
    err = cpu_memory_alloc(elf_size, &cpu_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("CPUMEM: %lx, %zu kb\n", cpu_mem.frameid.base, cpu_mem.size >> 10);

    elf_size = ROUND_UP(elf_virtual_size(monitor_binary.vaddr), config.arch_page_size);

    struct mem_info monitor_mem;
    err = app_memory_alloc(elf_size + ARMV8_CORE_DATA_PAGES * config.arch_page_size,
                           &monitor_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("DATAMEM: %lx, %zu kb\n", monitor_mem.frameid.base,
                 monitor_mem.frameid.bytes >> 10);

    /*
     * The layout is:
     *  [ARMv8 CORE DATA]
     *  [KERNEL STACK]
     */
    struct mem_info stack_mem;
    err = cpu_memory_alloc(config.stack_size + config.arch_page_size , &stack_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("STACKMEM: %lx, %zu kb\n", stack_mem.frameid.base,
            stack_mem.frameid.bytes >> 10);


    /* Load cpu */
    genvaddr_t boot_entry, cpu_driver_entry;
    err =  load_boot_and_cpu_driver(&config, &boot_binary, &boot_mem, &cpu_binary,
                                    &cpu_mem, &boot_entry, &cpu_driver_entry);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not load kernel .");
        return err;
    }

    DEBUG("Writing core data structure...\n");
    struct armv8_core_data *core_data = (struct armv8_core_data *)stack_mem.buf;

    core_data->boot_magic = ARMV8_BOOTMAGIC_PSCI;
    core_data->cpu_driver_stack = stack_mem.frameid.base + stack_mem.frameid.bytes - 16;
    core_data->cpu_driver_stack_limit = stack_mem.frameid.base + BASE_PAGE_SIZE;

    DEBUG("kernel stack: 0x%" PRIxLPADDR"..0x%" PRIxLPADDR "\n",
            core_data->cpu_driver_stack_limit,
            core_data->cpu_driver_stack);

    core_data->cpu_driver_entry = cpu_driver_entry;


    core_data->memory.base = monitor_mem.frameid.base;
    core_data->memory.length = monitor_mem.frameid.bytes;

    core_data->urpc_frame.base = urpc_frame_id.base;
    core_data->urpc_frame.length = urpc_frame_id.bytes;

    core_data->monitor_binary.base   = monitor_binary.paddr;
    core_data->monitor_binary.length = monitor_binary.size;

    core_data->src_core_id       = disp_get_core_id();
    core_data->src_arch_id       = my_arch_id;
    core_data->dst_core_id       = coreid;
    core_data->dst_arch_id       = hwid;

    struct frame_identity fid;
    err = invoke_frame_identify(kcb, &fid);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Invoke frame identity for KCB failed. "
                            "Did you add the syscall handler for that architecture?");
    }

    DEBUG("%s:%s:%d: fid.base is 0x%"PRIxGENPADDR"\n",
           __FILE__, __FUNCTION__, __LINE__, fid.base);
    core_data->kcb = (genpaddr_t) fid.base;
#ifdef CONFIG_FLOUNDER_BACKEND_UMP_IPI
    core_data->chan_id           = chanid;
#endif


    if (cmdline != NULL) {
        // copy as much of command line as will fit
        snprintf(core_data->cpu_driver_cmdline, sizeof(core_data->cpu_driver_cmdline),
                "%s %s", config.cpu_driver_binary, cmdline);
        // ensure termination
        core_data->cpu_driver_cmdline[sizeof(core_data->cpu_driver_cmdline) - 1] = '\0';

        DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, core_data->kernel_cmdline);
    }


    if (parking_version) {
        assert(parking_mem.buf);
        parking_write_mailbox(parking_mem.buf, hwid, boot_entry,
                              stack_mem.frameid.base);
    }

    __asm volatile("dsb   sy\n"
                   "dmb   sy\n"
                   "isb     \n");

    /* start */

    DEBUG("invoking boot start hwid=%lx entry=%lx context=%lx\n",
                 hwid, boot_entry, stack_mem.frameid.base);

    err = invoke_monitor_spawn_core(hwid, cpu_type, boot_entry, stack_mem.frameid.base);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to spawn the cpu\n");
    }

    if (parking_version) {
        debug_printf("WAITING FOR ACK!\n");

        debug_printf("ACKNOWLEDGED!\n");
    }

    err = mem_free(&stack_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = mem_free(&cpu_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = mem_free(&monitor_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = mem_free(&boot_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }

    err = mem_free(&parking_mem);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap_destroy failed");
    }
    return SYS_ERR_OK;
}

errval_t get_core_info(coreid_t core_id, hwid_t* hw_id, enum cpu_type* cpu_type)
{
    char* record = NULL;
    errval_t err = oct_get(&record, "hw.processor.%"PRIuCOREID"", core_id);
    if (err_is_fail(err)) {
        goto out;
    }


    uint64_t enabled, type, barrelfish_id;
    err = oct_read(record, "_ { " HW_PROCESSOR_GENERIC_FIELDS " }",
                   &enabled, &barrelfish_id, hw_id, &type);
    if (err_is_fail(err)) {
        goto out;
    }

    if (!enabled) {
        /* XXX: better error code */
        err = SYS_ERR_CORE_NOT_FOUND;
    }


    *cpu_type = (enum cpu_type) type;
out:
    if (record) {
        free(record);
    }
    return err;
}
