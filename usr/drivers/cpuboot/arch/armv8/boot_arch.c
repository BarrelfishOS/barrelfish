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

#include <hw_records_arch.h>

#include <skb/skb.h>


#include "../../coreboot.h"

extern coreid_t my_arch_id;


/// XXX: make this configurable...
#define ARMV8_KERNEL_STACK_SIZE (16 * 1024)

static errval_t get_arch_config(hwid_t hwid,
                                genpaddr_t *arch_page_size,
                                size_t *stack_size,
                                const char *monitor_binary,
                                const char *cpu_binary)
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
    err= skb_read_output("res(%255[^)])", cpu_binary);
    if (err_is_fail(err)) return err;

    /* Query the SKB for the monitor binary to use. */
    err= skb_execute_query("monitor(S), write(res(S)).");
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "skb_execute_query");
        return err;
    }
    err= skb_read_output("res(%255[^)])", monitor_binary);
    if (err_is_fail(err)) return err;

    *arch_page_size= BASE_PAGE_SIZE;
    *stack_size = ARMV8_KERNEL_STACK_SIZE;

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


#include <barrelfish_kpi/sys_debug.h>

static errval_t sys_debug_invoke_psci(uintptr_t target, lpaddr_t entry, lpaddr_t context)
{
    struct sysret sr = syscall5(SYSCALL_DEBUG, DEBUG_PSCI_CPU_ON, target, entry, context);
    return sr.error;
}




static errval_t
relocate_elf(struct mem_info *cpumem, lvaddr_t base, size_t arch_page_size,
             struct Elf64_Phdr *phdr, size_t phnum, size_t shnum) {

    struct Elf64_Ehdr   *ehdr = (struct Elf64_Ehdr *)base;

    DEBUG("Relocating kernel image.\n");

    struct Elf64_Shdr *shead = (struct Elf64_Shdr *)(base + (uintptr_t)ehdr->e_shoff);

    /* Search for relocaton sections. */
    for(size_t i= 0; i < shnum; i++) {

        struct Elf64_Shdr *shdr=  &shead[i];

        if(shdr->sh_type == SHT_REL ||
           shdr->sh_type == SHT_RELA) {
            if(shdr->sh_info != 0) {
                debug_printf("I expected global relocations, but got"
                              " section-specific ones.\n");
                return ELF_ERR_HEADER;
            }


            uint64_t segment_elf_base= phdr[0].p_vaddr;
            uint64_t segment_load_base=cpumem->frameid.base + arch_page_size;
            uint64_t segment_delta= segment_load_base - segment_elf_base;
            uint64_t segment_vdelta= (uintptr_t)cpumem->buf+arch_page_size - segment_elf_base;

            size_t rsize;
            if(shdr->sh_type == SHT_REL){
                rsize= sizeof(struct Elf64_Rel);
            } else {
                rsize= sizeof(struct Elf64_Rela);
            }

            assert(rsize == shdr->sh_entsize);
            size_t nrel= shdr->sh_size / rsize;

            void * reldata = (void*)(base + shdr->sh_offset);

            /* Iterate through the relocations. */
            for(size_t ii= 0; ii < nrel; ii++) {
                void *reladdr= reldata + ii *rsize;

                switch(shdr->sh_type) {
                    case SHT_REL:
                        debug_printf("SHT_REL unimplemented.\n");
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
                                    debug_printf("Relocation references a"
                                                 " dynamic symbol, which is"
                                                 " unsupported.\n");
                                    return ELF_ERR_PROGHDR;
                                }

                                /* Delta(S) + A */
                                *rel_target= addend + segment_delta + KERNEL_OFFSET;
                                break;

                            default:
                                debug_printf("Unsupported relocation type %d\n",
                                             type);
                                return ELF_ERR_PROGHDR;
                        }
                    }
                    break;
                    default:
                        debug_printf("Unexpected type\n");
                        break;

                }
            }
        }
    }

    return SYS_ERR_OK;
}

static errval_t load_cpudriver(uint16_t em_machine, struct mem_info *cpumem,
                               lvaddr_t base, size_t size, size_t arch_page_size,
                               genvaddr_t *retentry){


    errval_t err;
    struct Elf64_Ehdr   *ehdr = (struct Elf64_Ehdr *)base;

    // Check for valid file size
    if (size < sizeof(struct Elf64_Ehdr)) {
        return ELF_ERR_FILESZ;
    }


    if(ehdr->e_ident[EI_CLASS] != ELFCLASS64 || ehdr->e_ident[EI_DATA] != ELFDATA2LSB) {
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_ident[EI_OSABI] != ELFOSABI_STANDALONE
        && ehdr->e_ident[EI_OSABI] != ELFOSABI_NONE) {
        debug_printf("Warning: Compiled for OS ABI %d.  Wrong compiler?\n",
                     ehdr->e_ident[EI_OSABI]);
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_machine != EM_AARCH64) {
        debug_printf( "Error: Not AArch64\n");
        return ELF_ERR_HEADER;
    }

    if(ehdr->e_type != ET_EXEC) {
        debug_printf("Warning: CPU driver isn't executable! Continuing anyway.\n");
    }

    DEBUG("Unrelocated kernel entry point is %x\n", ehdr->e_entry);

    // More sanity checks
    if (ehdr->e_phoff + ehdr->e_phentsize * ehdr->e_phnum > size
        || ehdr->e_phentsize != sizeof(struct Elf64_Phdr)) {
        return ELF_ERR_PROGHDR;
    }

    DEBUG("Found %d program header(s)\n", ehdr->e_phnum);

    /* Load the CPU driver from its ELF image. */
    bool found_entry_point= 0;
    bool loaded = 0;
    lpaddr_t entry_point = 0;
    struct Elf64_Phdr *phdr = (struct Elf64_Phdr *)(base + ehdr->e_phoff);
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

        void *dest = cpumem->buf + arch_page_size;
        lpaddr_t dest_phys = cpumem->frameid.base + arch_page_size;

        /* copy loadable part */
        memcpy(dest, (void *)(base + phdr[i].p_offset), phdr[i].p_filesz);

        /* zero out BSS section */
        memset(dest + phdr[i].p_filesz, 0, phdr[i].p_memsz - phdr[i].p_filesz);

        if(ehdr->e_entry >= phdr[i].p_vaddr &&
           ehdr->e_entry - phdr[i].p_vaddr < phdr[i].p_memsz) {
            entry_point= (dest_phys + (ehdr->e_entry - phdr[i].p_vaddr));
            found_entry_point= 1;
        }
    }

    err = relocate_elf(cpumem, base, arch_page_size, phdr, ehdr->e_phnum, ehdr->e_shnum);
    if (err_is_fail(err)) {
        return err;
    }

    if(!found_entry_point) {
        debug_printf("Kernel entry point wasn't in any loaded segment.\n");
        return ELF_ERR_HEADER;
    }

    DEBUG("Relocated entry point is %p\n",entry_point);

    *retentry = (genvaddr_t)entry_point;

    return SYS_ERR_OK;
}



errval_t spawn_xcore_monitor(coreid_t coreid, hwid_t hwid,
                             enum cpu_type cpu_type,
                             const char *cmdline,
                             struct frame_identity urpc_frame_id,
                             struct capref kcb)
{

    DEBUG("Booting: %" PRIuCOREID ", hwid=%" PRIxHWID "\n", coreid, hwid);

    if (coreid != 47) {
        return SYS_ERR_OK;
    }

    static char cpuname[256], monitorname[256];
    genpaddr_t arch_page_size;
    size_t stack_size;
    errval_t err;

    if(cpu_type != CPU_ARM8) {
        return SPAWN_ERR_UNKNOWN_TARGET_ARCH;
    }

    err = get_arch_config(hwid, &arch_page_size, &stack_size, monitorname, cpuname);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to obtain architecture configuration");
    }

    DEBUG("loading kernel: %s\n", cpuname);
    DEBUG("loading 1st app: %s\n", monitorname);

    // compute size of frame needed and allocate it
    DEBUG("%s:%s:%d: urpc_frame_id.base=%"PRIxGENPADDR"\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.base);
    DEBUG("%s:%s:%d: urpc_frame_id.size=0x%" PRIuGENSIZE "\n",
          __FILE__, __FUNCTION__, __LINE__, urpc_frame_id.bytes);


    // XXX: Caching these for now, until we have unmap

    struct module_blob cpu_binary;
    err = get_module_info(cpuname, &cpu_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    struct module_blob monitor_binary;
    err = get_module_info(monitorname, &monitor_binary);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not lookup module");
        return err;
    }

    /* */
    struct mem_info cpu_mem;
    err = cpu_memory_alloc(cpu_binary.size, &cpu_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("CPUMEM: %lx, %zu kb\n", cpu_mem.frameid.base, cpu_mem.size >> 10);

    size_t monitor_size = ROUND_UP(elf_virtual_size(monitor_binary.vaddr),
                                   arch_page_size);

    debug_printf("Monitor binary: %zu\n", monitor_size);

    struct mem_info monitor_mem;
    err = app_memory_alloc(monitor_size + ARMV8_CORE_DATA_PAGES * arch_page_size, &monitor_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    DEBUG("DATAMEM: %lx, %zu kb\n", monitor_mem.frameid.base,
                 monitor_mem.frameid.bytes >> 10);


    struct mem_info stack_mem;
    err = app_memory_alloc(stack_size, &stack_mem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not allocate space for new app kernel.");
        return err;
    }

    /* Load cpu */
    struct elf_allocate_state state;
    state.vbase = (char *)cpu_mem.buf + arch_page_size;
    assert(sizeof(struct armv8_core_data) <= arch_page_size);
    state.elfbase = elf_virtual_base(cpu_binary.vaddr);

    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)cpu_binary.vaddr;
    genvaddr_t cpu_entry;

    err = load_cpudriver(cpu_head->e_machine, &cpu_mem, cpu_binary.vaddr,
                         cpu_binary.size, arch_page_size, &cpu_entry);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can not load kernel .");
        return err;
    }

    struct armv8_core_data *core_data = (struct armv8_core_data *)cpu_mem.buf;

    /* set the stack */
    core_data->kernel_stack = stack_mem.frameid.base + stack_mem.frameid.bytes - 16;
    core_data->boot_magic = ARMV8_BOOTMAGIC_PSCI;

    core_data->elf.size = sizeof(struct Elf64_Shdr);
    core_data->elf.addr = cpu_binary.paddr + (uintptr_t)cpu_head->e_shoff;
    core_data->elf.num  = cpu_head->e_shnum;

    core_data->module_start = cpu_binary.paddr;
    core_data->module_end   = cpu_binary.paddr + cpu_binary.size;
    core_data->urpc_frame_base = urpc_frame_id.base;
    core_data->urpc_frame_size = urpc_frame_id.bytes;
    core_data->monitor_binary   = monitor_binary.paddr;
    core_data->monitor_binary_size = monitor_binary.size;
    core_data->memory_base_start = monitor_mem.frameid.base;
    core_data->memory_size       = monitor_mem.frameid.bytes;
    core_data->src_core_id       = disp_get_core_id();
    core_data->src_arch_id       = my_arch_id;
    core_data->dst_core_id       = coreid;

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
        snprintf(core_data->kernel_cmdline, sizeof(core_data->kernel_cmdline),
                "%s %s", cpuname, cmdline);
        // ensure termination
        core_data->kernel_cmdline[sizeof(core_data->kernel_cmdline) - 1] = '\0';

        DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, core_data->kernel_cmdline);
    }

    __asm volatile("dsb   sy\n"
                   "dmb   sy\n"
                   "isb     \n");

    /* start */

    debug_printf("invoking PSCI_START hwid=%lx entry=%lx context=%lx\n",
                 hwid, cpu_entry, cpu_mem.frameid.base);
    err = sys_debug_invoke_psci(hwid, cpu_entry, cpu_mem.frameid.base);
    DEBUG_ERR(err, "sys_debug_invoke_psci");

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
