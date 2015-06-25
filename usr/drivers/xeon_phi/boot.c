/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include <vfs/vfs.h>
#include <elf/elf.h>

#include <xeon_phi/xeon_phi.h>
#include <tftp/tftp.h>

#include <../kernel/include/multiboot.h>

#include <dev/xeon_phi/xeon_phi_boot_dev.h>
#include <dev/xeon_phi/xeon_phi_apic_dev.h>

struct bootinfo *bi = NULL;

#include "xeon_phi_internal.h"
#include "interphi.h"
#include "sleep.h"

#define TFTP_BUF_SIZE (1<<20)
#define TFTP_BUF_SIZE_MODULES (1<<30)

typedef errval_t (*loadfile_fn_t)(char *path, void *buf, size_t buflen, size_t *ret_size);

#define BOOT_TIMEOUT 3000
#define BOOT_COUNTER 0xFFFFF

/*
 * TODO: Verify these values if they are really needed
 */
#define MEMORY_RESERVE_PERCENT 50
#define UOS_RESERVE_SIZE_MIN    ((128) * 1024 * 1024)
#define UOS_RESERVE_SIZE_MAX    (((4) * 1024 * 1024 * 1024ULL) - ((4) * 1024))

/*
 * Helper macros
 */
#define MAX(a, b)   ( ((a) > (b)) ? (a) : (b) )
#define MIN(a, b)   ( ((a) < (b)) ? (a) : (b) )
#define ALIGN(x) ((x + BASE_PAGE_SIZE-1) & ~(BASE_PAGE_SIZE-1))

static xeon_phi_boot_t boot_registers;
static xeon_phi_apic_t apic_registers;

/**
 * \brief   get the load offset to where to place the bootloader
 *
 * The bootstrap on the card will write the offset into the SBOX_SCRATCH2
 * register once the bootstrap is finished
 */
static inline lvaddr_t get_load_offset(struct xeon_phi *phi)
{
    return ((lvaddr_t) xeon_phi_boot_download_offset_rdf(&boot_registers)) << 12;
}

static uint64_t get_adapter_memsize(void)
{
    xeon_phi_boot_meminfo_t meminfo = xeon_phi_boot_meminfo_rd(&boot_registers);

    uint64_t memsize = xeon_phi_boot_meminfo_size_kb_extract(meminfo);
    memsize *= 1024;

    switch (xeon_phi_boot_meminfo_usage_extract(meminfo)) {
        case xeon_phi_boot_mem_all:
            return memsize;
        case xeon_phi_boot_mem_half:
            return (memsize / 2);
        case xeon_phi_boot_mem_third:
            return (memsize / 3);
            break;
        case xeon_phi_boot_mem_fourth:
            return (memsize / 4);
        default:
            return memsize;
    }
}

/**
 * \brief   generates the cmdline supplied to the card kernel
 *
 * \param   phi         the card information structure
 * \param   load_offset offset where to load the cmdline
 * \param   ret_size    size of the cmdline in bytes
 */
static errval_t load_cmdline(struct xeon_phi *phi, lvaddr_t load_offset)
{
    uint32_t cmdlen = 0;

    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);

    void *buf = (void *) (phi->apt.vbase + load_offset);

    if (phi->cmdline) {
        cmdlen += sprintf(buf + cmdlen, "%s", phi->cmdline);
    }

    cmdlen += sprintf(buf + cmdlen, "card_id=%i", phi->id);

    /*
     * id
     */
    /*
     * TODO: Add multihop / communication information here..
     */

    XBOOT_DEBUG("cmdline @ 0x%" PRIx32 " '%s'\n", (uint32_t)load_offset, (char*)buf);

    phi->cmdline = buf;
    phi->cmdlen = cmdlen;


    bp->cmdline_ptr = (uint32_t)(load_offset);
    bp->cmdline_size = (uint32_t)cmdlen;

    return SYS_ERR_OK;
}

static errval_t bootstrap_notify(struct xeon_phi *phi)
{
    // set the bootimage size to tell the bootloader
    xeon_phi_boot_os_size_rawwr(&boot_registers, phi->os_size);

    uint64_t memsize = get_adapter_memsize();

    uint64_t reserved = (memsize * MEMORY_RESERVE_PERCENT / 100);

    // Keep in mind maximum uos reserve size is uint32_t, so we never overflow
    reserved = MIN(reserved, UOS_RESERVE_SIZE_MAX);
    reserved = MAX(reserved, UOS_RESERVE_SIZE_MIN);

    // Always align uos reserve size to a page
    reserved = (reserved & ~(BASE_PAGE_SIZE - 1));

    xeon_phi_boot_res_size_rawwr(&boot_registers, (uint32_t) reserved);

    // sending the bootstrap interrupt
    xeon_phi_apic_icr_lo_t icr_lo = xeon_phi_apic_icr_lo_default;
    icr_lo = xeon_phi_apic_icr_lo_vector_insert(icr_lo, xeon_phi_apic_vec_bsp);
    icr_lo = xeon_phi_apic_icr_lo_boot_notify_insert(icr_lo, 0x1);

    assert(icr_lo == (229 | (1 << 13)));

    xeon_phi_apic_icr_hi_wr(&apic_registers, xeon_phi_apic_bootstrap, phi->apicid);

    xeon_phi_apic_icr_lo_wr(&apic_registers, xeon_phi_apic_bootstrap, icr_lo);

    return SYS_ERR_OK;
}

/*
 * -------------------------------------------------------------------------------
 * VFS helper function
 * -------------------------------------------------------------------------------
 */

static vfs_handle_t file_open(char *file, uint8_t nfs)
{
    errval_t err;

    vfs_handle_t fh;
    char *path = file;
    if (nfs) {
        size_t path_size = strlen(file) + strlen(XEON_PHI_NFS_MNT) + 2;
        path = malloc(path_size);
        if (path == NULL) {
            return NULL;
        }
        if (file[0] == '/') {
            snprintf(path, path_size, "%s%s", XEON_PHI_NFS_MNT, file);
        } else {
            snprintf(path, path_size, "%s/%s", XEON_PHI_NFS_MNT, file);
        }
    }

    err = vfs_open(path, &fh);
    if (nfs) {
        free(path);
    }
    switch(err_no(err)) {
        case SYS_ERR_OK :
            return fh;
            break;
        case FS_ERR_NOTFOUND :
            err = vfs_open(file, &fh);
            if (err_is_ok(err)) {
                return fh;
            }
            return NULL;
            break;
        default:
            return NULL;
    }

    return NULL;
}

static errval_t file_load(vfs_handle_t fh, void *buf, size_t length)
{
    errval_t err;
    size_t pos = 0, readlen = 0;
    do {
        err = vfs_read(fh, buf+pos, length - pos, &readlen);
        if (err_is_fail(err)) {
            return err;
        } else if (readlen == 0) {
            return SPAWN_ERR_LOAD; // XXX
        } else {
            pos += readlen;
        }
    } while (err_is_ok(err) && readlen > 0 && pos < length);

    return SYS_ERR_OK;
}


static errval_t download_file_vfs(char *file, void *card_buffer, size_t use_nfs,
                                  size_t *bytes)
{
    errval_t err;

    vfs_handle_t fh = file_open(file, use_nfs);
    if (fh == NULL) {
        return FS_ERR_INVALID_FH;
    }

    struct vfs_fileinfo info;
    err = vfs_stat(fh, &info);
    if (err_is_fail(err)) {
        goto out;
    }

    if (bytes) {
        *bytes = info.size;
    }

    assert(info.type == VFS_FILE);
    assert(info.size < 4UL<<30);

    err = file_load(fh, card_buffer, info.size);
    out:
    vfs_close(fh);
    return err;
}

/*
 * -------------------------------------------------------------------------------
 * boot loader
 * -------------------------------------------------------------------------------
 */

#define SETUP_SECTORS 2
#define SECTOR_SIZE 512
#define HEADER_SIZE (SETUP_SECTORS*SECTOR_SIZE)

static errval_t download_bootloader_generic(struct xeon_phi *phi, char *bootloader,
                                            loadfile_fn_t loadfn, size_t args)
{
    errval_t err;

    lvaddr_t loadoffset = get_load_offset(phi);
    size_t imgsize;

    char *buf = (void *) (phi->apt.vbase + loadoffset);

    /* fill in the header */
    memset(buf, 0, HEADER_SIZE);

    /*
     * This is the signature. Without this the kernel does not boot.
     * Signature is reads "HdrS"
     */
    buf[514] = 0x48;
    buf[515] = 0x64;
    buf[516] = 0x72;
    buf[517] = 0x53;
    buf[0x1f1] = SETUP_SECTORS-1;

    err = loadfn(bootloader, buf + HEADER_SIZE, args, &imgsize);
    if (err_is_fail(err)) {
        return err;
    }

    size_t sys_size = (imgsize + 15) / 16;
    buf[0x1f4] = sys_size;
    buf[0x1f5] = sys_size >> 8;
    buf[0x1f6] = sys_size >> 16;
    buf[0x1f7] = sys_size >> 24;

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    phi->os_offset = loadoffset;
    phi->os_size = imgsize;

    XBOOT_DEBUG("Xeon Phi bootloader %s loaded @ 0x%lx size %lu kB\n", bootloader,
                loadoffset, imgsize >> 10);

    return SYS_ERR_OK;
}


static inline errval_t download_bootloader_vfs(struct xeon_phi *phi, char *bootloader,
                                               uint8_t use_nfs)
{
   return download_bootloader_generic(phi, bootloader, download_file_vfs, use_nfs);
}

static inline errval_t download_bootloader_tftp(struct xeon_phi *phi, char *bootloader)
{
    return download_bootloader_generic(phi, bootloader, tftp_client_read_file,
                                       TFTP_BUF_SIZE_MODULES);
}


/*
 * -------------------------------------------------------------------------------
 * multiboot modules
 * -------------------------------------------------------------------------------
 */

static uint32_t prepare_multiboot_strings(void *strings, char **mods,
                                          uint32_t num_mods)
{
    uint32_t bytes = 0;
    for (uint32_t i = 0; i < num_mods; ++i) {
        bytes += snprintf(strings+bytes, 1<<20, "%s", mods[i]) + 1;
    }
    return bytes;
}

static inline char *get_module_path(char *cmdline)
{
    while(*cmdline) {
        if (isspace((int)*cmdline)) {
            return cmdline;
        }
        cmdline++;
    }
    return cmdline;
}

static uint32_t prepare_multiboot_info(void *aptvbase, lpaddr_t offset,
                                       char **mmaps, uint32_t num_mods,
                                       uint32_t num_mmaps)
{
    void *mbibuf = aptvbase + offset;
    /*
     * Layout of multi boot information on card:
     * [multiboot_info]
     * [n x multiboot_modinfo]
     * [m x multiboot_mmap]
     * [strings]
     */

    /* set the host virtual pointers of the multiboot structures */
    struct multiboot_info *mbi = mbibuf;
    struct multiboot_modinfo *mbi_mods = (struct multiboot_modinfo *)(mbi + 1);
    struct multiboot_mmap *mbi_mmaps = (struct multiboot_mmap *)(mbi_mods + num_mods);

    /* card physical modules array */
    offset += (uint32_t)sizeof(struct multiboot_info);
    mbi->mods_count = num_mods;
    mbi->mods_addr = offset;

    /* card physical mmap array */
    offset += num_mods * (uint32_t)sizeof(struct multiboot_modinfo);
    mbi->mmap_addr = offset;
    mbi->mmap_length = num_mmaps * sizeof(struct multiboot_mmap);

    offset += num_mmaps * (uint32_t)sizeof(struct multiboot_mmap);

    /* set the multiboot flags */
    mbi->flags = MULTIBOOT_INFO_FLAG_HAS_CMDLINE | MULTIBOOT_INFO_FLAG_HAS_MODS
                    | MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS| MULTIBOOT_INFO_FLAG_HAS_MMAP;

    for (uint32_t i = 0; i < num_mmaps; ++i) {
        uint32_t parsed = sscanf(mmaps[i],"map%*[ \n\t]%" SCNx64
                                 "%*[ \n\t]%" SCNx64 "%*[ \n\t]%" SCNu32,
                                 &mbi_mmaps[i].base_addr,
                                 &mbi_mmaps[i].length,
                                 &mbi_mmaps[i].type);
        if (parsed !=3) {
            debug_printf("INVALID mmap: {%s}\n", mmaps[i]);
            mbi_mmaps[i].size = 0;
            continue;
        }
        mbi_mmaps[i].size = sizeof(struct multiboot_mmap);
    }

    return sizeof(*mbi) + num_mods * sizeof(*mbi_mods) + num_mmaps * sizeof(*mbi_mmaps);
}

static errval_t download_modules_generic(struct xeon_phi *phi, size_t offset,
                                         char **mods, uint32_t num_mods,
                                         uint32_t num_mmaps, loadfile_fn_t loadfn,
                                         size_t args)
{
    errval_t err;

    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);

    struct multiboot_info *mbi = (void *)phi->apt.vbase + offset;
    struct multiboot_modinfo *mbi_mods = (struct multiboot_modinfo *)(mbi + 1);

    bp->ramdisk_image = offset;
    bp->mbi = offset;

    offset += prepare_multiboot_info((void *)phi->apt.vbase, offset, mods + num_mods,
                                     num_mods, num_mmaps);

    lpaddr_t strings_offset = offset;
    offset += prepare_multiboot_strings((void *)phi->apt.vbase + strings_offset,
                                        mods, num_mods);

    offset = ALIGN(offset);

    for (uint32_t i = 0; i < num_mods; ++i) {
        char *strings = (void *)phi->apt.vbase + strings_offset;
        size_t cmdlength = strlen(strings);
        size_t imgsize = 0;
        mbi_mods[i].mod_start = offset;
        mbi_mods[i].string = strings_offset;

        char *delim = get_module_path(mods[i]);
        *delim = 0;

        err = loadfn(mods[i], (void *)phi->apt.vbase + offset, args, &imgsize);
        if (err_is_fail(err)) {
            return err;
        }
        mbi_mods[i].mod_end = mbi_mods[i].mod_start + imgsize;

        offset = ALIGN(offset + imgsize);

        XBOOT_DEBUG("module %35s @ 0x08%lx  size %lu kB\n",
                    (char *)phi->apt.vbase + strings_offset, offset, imgsize >> 10);

        strings_offset += (cmdlength + 1);
    }

    bp->ramdisk_size = offset - bp->ramdisk_image;

    return SYS_ERR_OK;
}


static errval_t download_modules_vfs(struct xeon_phi *phi, size_t offset,
                                     char **mods, uint32_t num_mods,
                                     uint32_t num_mmaps, uint8_t use_nfs)
{
    return download_modules_generic(phi, offset, mods, num_mods, num_mmaps,
                                    download_file_vfs, use_nfs);
}

static errval_t download_modules_tftp(struct xeon_phi *phi, lpaddr_t offset,
                                      char **mods, uint32_t num_mods,
                                      uint32_t num_mmaps)
{
    return download_modules_generic(phi, offset, mods, num_mods, num_mmaps,
                                    tftp_client_read_file, TFTP_BUF_SIZE_MODULES);
}

/*
 * -------------------------------------------------------------------------------
 * Parsing modules
 * -------------------------------------------------------------------------------
 */

static inline char *discard_leading_white_spaces(char *string)
{
    while(*string) {
        if (!isspace((int)*string)) {
            break;
        }
        string++;
    }
    return string;
}


static errval_t parse_mod_list(char *modules, uint32_t *mods, uint32_t *mmaps,
                               uint8_t *kernel, char ***parsed_modules)
{
    uint32_t num_mod = 0, num_mmap = 0;
    uint8_t has_kernel = 0;

    char *line = modules;

    /* how many modules we have */
    while (line != NULL)
    {
        if (*line == '\n') {
            line++;
        }
        if (strncmp(line, "module", 6)==0) {
            num_mod ++;
        } else if (strncmp(line, "kernel", 6) == 0) {
            assert(has_kernel == 0);
            has_kernel = 1;
        } else if (strncmp(line, "mmap", 4) == 0) {
            num_mmap++;
        }
        line=strchr(line+1,'\n');
    }


    /* allocate parsed array */
    char **parsed = calloc(num_mod + num_mmap + 1, sizeof(char *));
    if (parsed == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    uint32_t mod_idx = 1;
    uint32_t mmap_idx = num_mod + num_mmap;
    line = modules;
    while (line != NULL)
    {
        if (*line == '\n') {
            *line = 0;
            line++;
        }
        if (strncmp(line, "module", 6)==0) {
            parsed[mod_idx++] = discard_leading_white_spaces(line + 6);
        } else if (strncmp(line, "kernel", 6) == 0) {
            parsed[0] = discard_leading_white_spaces(line + 6);
        } else if (strncmp(line, "mmap", 4) == 0) {
            parsed[mmap_idx--] = discard_leading_white_spaces(line + 4);
        }
        line=strchr(line+1,'\n');
    }

    if (parsed_modules) {
        *parsed_modules = parsed;
    } else  {
        free(parsed_modules);
    }

    if (mods) {
        *mods = num_mod;
    }

    if (mmaps) {
        *mmaps = num_mmap;
    }

    if (kernel) {
        *kernel = has_kernel;
    }

    XBOOT_DEBUG("parsing modules found: %u kernel, %u modules, %u mmaps\n",
                has_kernel, num_mod, num_mmap);

    return SYS_ERR_OK;
}


static errval_t load_mod_list_tftp(char *mod_list, void **modules, size_t *size)
{
    errval_t err;

    void *buf = calloc(1, TFTP_BUF_SIZE);

    XBOOT_DEBUG("loading modules list %s over TFTP\n", mod_list);

    err = tftp_client_read_file(mod_list, buf, TFTP_BUF_SIZE, size);
    if (err_is_fail(err)) {
        USER_PANIC("reading tftp file");
    }

    if (modules) {
        *modules = buf;
    }

    return SYS_ERR_OK;
}

static errval_t load_mod_list_vfs(char *mod_list, uint8_t use_nfs,
                                  void **modules, size_t *size)
{
    errval_t err;

    XBOOT_DEBUG("loading modules list %s %s\n", mod_list,
                (use_nfs==1 ? "over NFS" : "from ramfs"));

    /* load the menu lst file */
    vfs_handle_t fh = file_open(mod_list, use_nfs);
    if (fh == NULL) {
        return SPAWN_ERR_LOAD;
    }

    struct vfs_fileinfo info;
    err = vfs_stat(fh, &info);
    if (err_is_fail(err)) {
        vfs_close(fh);
        return err_push(err, SPAWN_ERR_LOAD);
    }

    assert(info.type == VFS_FILE);

    char *menulst = calloc(info.size + 1, 1);
    if (menulst == NULL) {
        vfs_close(fh);
        return LIB_ERR_MALLOC_FAIL;
    }

    err = file_load(fh, menulst, info.size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "file loading failed.\n");
        vfs_close(fh);
        free(menulst);
        return err;
    }

    if (modules) {
        *modules = menulst;
    }

    if (size) {
        *size = info.size;
    }

    return SYS_ERR_OK;
}

/**
 * \brief boots the card with the given loader and multiboot image
 *
 * \param phi           pointer to the card information
 * \param mod_uri       name of to the modules location uri
 * \param mod_list      name of the modules list
 */
errval_t xeon_phi_boot(struct xeon_phi *phi,
                       char *mod_uri,
                       char *mod_list)
{
    errval_t err;
    lvaddr_t offset;

    xeon_phi_boot_initialize(&boot_registers,
                             XEON_PHI_MMIO_TO_SBOX(phi),
                             XEON_PHI_MMIO_TO_DBOX(phi));
    xeon_phi_apic_initialize(&apic_registers, XEON_PHI_MMIO_TO_SBOX(phi));

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    void *modules = NULL;
    size_t modules_size = 0;
    uint8_t use_nfs = 0, use_tftp = 0;
    if (strncmp(mod_uri, "nfs://", 6) == 0) {
        XBOOT_DEBUG("using nfs share: %s\n", mod_uri);
        use_nfs = 1;
        err = load_mod_list_vfs(mod_list, 1, &modules, &modules_size);
    } else if (strncmp(mod_uri, "tftp://", 7) == 0) {
        use_tftp = 1;
        char *del = strchr(mod_uri+7, ':');\
        uint16_t port = 69; // default tftp port
        if (del != NULL) {
            port = atoi(del + 1);
            *del = 0;
        }

        XBOOT_DEBUG("using tftp server: %s @ port %u\n", mod_uri + 7, port);

        err = tftp_client_connect(mod_uri + 7, port);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Could not connect to the tftp service");
        }
        err = load_mod_list_tftp(mod_list, &modules, &modules_size);
    } else  {
        err = load_mod_list_vfs(mod_list, 0, &modules, &modules_size);
    }

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to load modules list");
    }

    char **modules_parsed;
    uint32_t num_mods, num_mmaps;
    uint8_t has_kernel;
    err = parse_mod_list(modules, &num_mods, &num_mmaps, &has_kernel, &modules_parsed);
    if (err_is_fail(err)) {
        return err;
    }

    if (!has_kernel) {
        return SPAWN_ERR_FIND_MODULE;
    }

    // load the coprocessor OS (boot loader)
    if (use_tftp) {
        err = download_bootloader_tftp(phi, modules_parsed[0]);
    } else {
        err = download_bootloader_vfs(phi, modules_parsed[0], use_nfs);
    }
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load bootloader image");
    }

    // round to next page
    offset = ALIGN(phi->os_offset + phi->os_size);

    // load cmdline
    err = load_cmdline(phi, offset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    // round to next page
    offset = ALIGN(offset+phi->cmdlen);

    if (use_tftp) {
        err = download_modules_tftp(phi, offset, modules_parsed + 1, num_mods, num_mmaps);
    } else {
        err = download_modules_vfs(phi, offset, modules_parsed + 1, num_mods, num_mmaps, use_nfs);
    }
    if (err_is_fail(err)) {
         USER_PANIC_ERR(err, "Could not load multiboot image");
    }

    free(modules_parsed);

    if (use_tftp) {
        tftp_client_disconnect();
    }

    struct xeon_phi_boot_params *bp;
    bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);
    bp->xeon_phi_id = 0xFF00;
    bp->xeon_phi_id += phi->id;

    err = interphi_init(phi, NULL_CAP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not initialize messaging");
    }

    xeon_phi_boot_download_status_wrf(&boot_registers, 0x0);

    phi->apicid = xeon_phi_boot_download_apicid_rdf(&boot_registers);

    // notify the bootstrap
    bootstrap_notify(phi);

    xeon_phi_boot_postcode_t postcode;
    xeon_phi_boot_postcodes_t pc, pc_prev = 0;
    uint32_t counter = BOOT_COUNTER;
    while (--counter) {
        postcode = xeon_phi_boot_postcode_rd(&boot_registers);
        pc = xeon_phi_boot_postcode_code_extract(postcode);
        if (pc_prev != pc) {
            debug_printf("Xeon Phi Booting: %s\n",
                         xeon_phi_boot_postcodes_describe(pc));
        }
        if (postcode == xeon_phi_boot_postcode_done) {
            break;
        }
        pc_prev = pc;
    }

    XBOOT_DEBUG("Bootstrap has finished execution. Waiting for Firmware...\n");

    uint32_t time = 0, time_steps = 0;
    while (time < BOOT_TIMEOUT) {
        /* read all the pending messages */
        xeon_phi_serial_handle_recv();
        milli_sleep(100);
        if (xeon_phi_boot_download_status_rdf(&boot_registers)) {
            XBOOT_DEBUG("Firmware signaled with ready bit. \n");
            break;
        }
        if (!(time % 50)) {
            debug_printf("Xeon Phi Booting: Waiting for ready signal %u\n",
                         time_steps);
            time_steps += 5;
        }
        time++;
    }

    if (!xeon_phi_boot_download_status_rdf(&boot_registers)) {
        USER_PANIC("Firmware not responding with ready bit");
        // TODO return error code
    }

    // we don't need the aperture mapped anymore so unmap it
    err = xeon_phi_unmap_aperture(phi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to map aperture range");
    }

    return SYS_ERR_OK;
}
