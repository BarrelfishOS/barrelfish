/**
 * \file
 * \brief Architecture independent code
 */
/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "coreboot.h"

extern bool done;
extern coreid_t core_count;
extern coreid_t core_max;
extern struct capref kcb;

char* get_binary_path(char* fmt, char* binary_name)
{
    assert (binary_name != NULL);
    assert (fmt != NULL);

    int length = snprintf(NULL, 0, fmt, binary_name);
    char* binary = malloc(length+1); // TODO(gz): Free this
    snprintf(binary, length+1, fmt, binary_name);

    return binary;
}

errval_t elfload_allocate(void *state, genvaddr_t base,
                          size_t size, uint32_t flags,
                          void **retbase)
{
    struct elf_allocate_state *s = state;

    *retbase = (char *)s->vbase + base - s->elfbase;
    return SYS_ERR_OK;
}

void boot_core_reply(struct monitor_binding *st, errval_t msgerr)
{
    if (err_is_fail(msgerr)) {
        USER_PANIC_ERR(msgerr, "msgerr in boot_core_reply, exiting\n");
    }
    DEBUG("%s:%d: got boot_core_reply.\n", __FILE__, __LINE__);
    core_count++;
    if (core_count == core_max) {
        done = true;
    }
}

static errval_t add_kcb_record(uint32_t kcb_id, coreid_t core_id, char* kcb_key)
{
    errval_t err = oct_set("kcb.%d { kcb_id: %d, barrelfish_id: %"PRIuCOREID", cap_key: '%s' }",
                            kcb_id, kcb_id, core_id, kcb_key);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_set");
        return err;
    }

    return err;
}

errval_t create_or_get_kcb_cap(coreid_t coreid, struct capref* the_kcb)
{
    errval_t err;
    struct capref kcb_mem;

    DEBUG("%s:%s:%d: get capability\n",
          __FILE__, __FUNCTION__, __LINE__);

    int length = snprintf(NULL, 0, "kcb_id_%d", coreid) + 1; // +1 for \0
    char *kcb_key = (char *)malloc(length);
    assert (kcb_key != NULL);
    snprintf(kcb_key, length + 1, "kcb_id_%d", coreid);

    DEBUG("%s:%s:%d: oct_get_capability for key = %s\n",
          __FILE__, __FUNCTION__, __LINE__, kcb_key);

    if (!new_kcb_flag) {
        err = oct_get_capability(kcb_key, the_kcb);
        if (err_is_ok(err)) {
            DEBUG("%s:%s:%d: kcb cap was cached\n",
                  __FILE__, __FUNCTION__, __LINE__);
            return err;
        } else if (err_no(err) != OCT_ERR_CAP_NAME_UNKNOWN) {
            DEBUG("%s:%s:%d: did not find the kcb in cap storage\n",
                  __FILE__, __FUNCTION__, __LINE__);
            return err;
        }
    }
    DEBUG("%s:%s:%d: Create a new kcb (new_kcb_flag = %d)\n",
          __FILE__, __FUNCTION__, __LINE__, new_kcb_flag);

    err = ram_alloc(&kcb_mem, OBJBITS_KCB);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "frame alloc");
        return err;
    }

    err = slot_alloc(the_kcb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failure in slot_alloc.");
        return err;
    }

    err = cap_retype(*the_kcb, kcb_mem,
                     ObjType_KernelControlBlock,
                     OBJBITS_KCB);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failure in cap_retype.");
    }

    // HACK don't store KCB right now will leak with -nm flags!
    if (!new_kcb_flag) {
        DEBUG("%s:%s:%d: Store the kcb.\n",
              __FILE__, __FUNCTION__, __LINE__);
        err = oct_put_capability(kcb_key, *the_kcb);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "can not save the capability.");
            return err;
        }

        err = add_kcb_record(coreid, coreid, kcb_key);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "add_kcb_record failed.");
            return err;
        }
    }

    return err;
}

errval_t give_kcb_to_new_core(coreid_t destination_id, struct capref new_kcb)
{
    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    DEBUG("%s:%s:%d: Send KCB to local monitor for forwarding to destination_id = %"PRIuCOREID"\n",
          __FILE__, __FUNCTION__, __LINE__, destination_id);

    errval_t ret_err;
    errval_t err = mc->vtbl.forward_kcb_request(mc, destination_id, new_kcb,
                   &ret_err);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "forward_kcb_request failed.");
    }
    if (err_is_fail(ret_err)) {
        USER_PANIC_ERR(ret_err, "forward_kcb_request failed.");
    }

    DEBUG("%s:%s:%d: KCB forwarded\n", __FILE__, __FUNCTION__, __LINE__);
    return SYS_ERR_OK;
}

errval_t cap_mark_remote(struct capref cap)
{
    errval_t err, msgerr;

    struct monitor_blocking_rpc_client *mc = get_monitor_blocking_rpc_client();
    err = mc->vtbl.cap_set_remote(mc, cap, true, &msgerr);
    if (err_is_fail(err)) {
        debug_printf("cap_set_remote RPC transmission failed\n");
        return err;
    }

    return msgerr;
}

/**
 * \brief Same as frame_alloc but also identify the capability.
 */
errval_t frame_alloc_identify(struct capref *dest, size_t bytes,
                              size_t *retbytes, struct frame_identity *id)
{
    errval_t err = frame_alloc(dest, bytes, retbytes);
    if (err_is_fail(err)) {
        if (err_no(err) != LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS){
            DEBUG_ERR(err, "frame_alloc failed.");
        }
        return err;
    }

    if (id != NULL) {
        err = invoke_frame_identify(*dest, id);
    }

    return err;
}

static errval_t cache_module(const char *module_name, struct capref binary_image_cap) 
{
    return oct_put_capability(module_name, binary_image_cap);
}

static errval_t lookup_module_cache(const char *module_name, struct capref *binary_image_cap) 
{
    return oct_get_capability(module_name, binary_image_cap);
}

errval_t lookup_module(const char *module_name, lvaddr_t *binary_virt,
                       genpaddr_t *binary_phys, size_t *binary_size)
{
    vfs_handle_t handle;
    struct vfs_fileinfo info;
    struct capref binary_image_cap;    
    struct frame_identity id;

    DEBUG("Trying to find binary %s in file system\n", module_name);
    errval_t err = vfs_open(module_name, &handle);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_open could not open module?");
        return err;
    }

    err = vfs_stat(handle, &info);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_stat for binary failed.");
        return err;
    }
    *binary_size = info.size;

    DEBUG("Trying to find module in cache\n");
    err = lookup_module_cache(module_name, &binary_image_cap);
    if (err_is_ok(err)) {
        err = invoke_frame_identify(binary_image_cap, &id);
        *binary_phys = id.base;
        DEBUG("%s:%d: id.base=0x%"PRIxGENPADDR"\n", __FILE__, __LINE__, id.base);
        err = vspace_map_one_frame((void **)binary_virt, info.size, binary_image_cap,
                                    NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Could identify frame from cap storage");
            return err;
        }
    }
    else {
        err = frame_alloc_identify(&binary_image_cap, info.size, NULL, &id);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Could not allocate space for binary");
            return err;
        }
        *binary_phys = id.base;
        DEBUG("%s:%d: id.base=0x%"PRIxGENPADDR"\n", __FILE__, __LINE__, id.base);
        err = vspace_map_one_frame((void **)binary_virt, info.size, binary_image_cap,
                                   NULL, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Could not map frame");
            return err;
        }

        size_t bytes_read = 0;
        err = vfs_read(handle, (void *)*binary_virt, info.size, &bytes_read);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not read binary from vfs");
            return err;
        }
        assert(bytes_read ==
               info.size); // TODO(gz): If this fails, need to loop vfs_read

        err = cache_module(module_name, binary_image_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Can not read binary from vfs");
            return err;
        }
    }

    return SYS_ERR_OK;
}
