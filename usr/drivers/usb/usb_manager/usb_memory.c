/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */






static struct usb_page map_new_frame(int num, uint8_t flag)
{
    int r = 0;
    struct capref frame;
    struct frame_identity frame_id = { .base = 0, .bits = 0 };
    struct usb_page map;

    if (flag == USB_NEAR_EHCI && ehci_core_id != -1)
        set_range_to_ehci();
    else if (flag == USB_NEAR_SELF && self_core_id != -1)
        set_range(self_core_id);

    // In case DONOT_CARE...we just fall through

    // XXX: IMPORTANT - Assuming that num pages will be contigious
    //      otherwise ...:-( !!
    int total_size = BASE_PAGE_SIZE * num;
    r = frame_alloc(&frame, total_size, NULL);
    assert(r == 0);
    r = invoke_frame_identify(frame, &frame_id);
    assert(r == 0);
    void *va;
    r = vspace_map_one_frame_attr(&va, total_size, frame,
                                  VREGION_FLAGS_READ_WRITE_NOCACHE,
                                  NULL, NULL);
    assert(r == 0);
    map.va = va;
    map.frame = frame;
    map.frame_id = frame_id;
    map.valid = 1;
    map.pa = (void *)frame_id.base;

    insert_page(map, num);
    usb_pages += num;

    dprintf("\n EHCI: A new frame is allocated   PADDR %lx     VADDR %lx ",
            (uint64_t) map.frame_id.base, (uint64_t) map.va);

    return map;
}




/*
 * \brief Maps a given capability into caller's domain.
 */

void *map_cap(struct capref cap, uint32_t sz)
{
    void *retval;
    errval_t err = vspace_map_one_frame(&retval, sz, cap, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return NULL;
    }
    return retval;
}

/*
 * \brief Allocates an I/O buffer of size sz.
 *
 * \param sz   size of the I/O buffer.
 * \param flag for NUMA aware allocation
 */

usb_mem malloc_iobuff(uint32_t sz, uint8_t flag)
{
    // FIXME: This is very poor memory managment
    // code for EHCI. Even for a small io buffer request
    // it allocates a whole new frame.
    int no_frames = (sz / BASE_PAGE_SIZE);
    if (sz % BASE_PAGE_SIZE != 0)       //spilled data into next frame
        no_frames++;

    usb_page map = map_new_frame(no_frames, flag);
    //reset the range for neq requests
    set_range(self_core_id);

    usb_mem mem;
    mem.va = map.va;
    mem.pa = map.pa;
    mem.type = EHCI_MEM_TYPE_IO;
    mem.free = 1;
    mem.size = sz;
    mem.cap = map.frame;

    return mem;
}

