/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "usb_memory.h"

static struct usb_page *free_pages = NULL;

static struct usb_dma_page *free_dma_buffers = NULL;

#define USB_PAGE_SIZE 0x1000 // 4k


/**
 * \brief   allocates a chunk of memory from a given usb_page with the given
 *          size and alignment constraints.
 *
 * \param size      the minimum size of the block
 * \param align     the alignment requirement of the block (physical memory)
 * \param page      the given usb_page to allocate from
 * \param ret_mem   the filled usb_mem_block structure with all the information
 *
 * \return  size of the requested block
 */
uint32_t usb_mem_next_block(uint32_t size, uint32_t align,
        struct usb_page *page, struct usb_memory_block *ret_mem)
{
    // check if there is enough free space on this usb page
    struct usb_memory_block *free = &page->free;

    uint32_t size_req = size;

    uint32_t offset = free->phys_addr % align;

    // calculate the required size
    if (offset) {
        size_req += (align - offset);
    }

    // check if we have enough free space, otherwise return
    if (free->size < size_req) {
        ret_mem->buffer = 0;
        ret_mem->phys_addr = 0;
        ret_mem->size = 0;
        return 0;
    }

    ret_mem->buffer = free->buffer + offset;
    ret_mem->phys_addr = free->phys_addr + offset;
    ret_mem->size = size;

    // update free memory in page
    free->buffer += size_req;
    free->phys_addr += size_req;
    free->size -= size_req;

    assert(free->size >= 0);

    return size;
}

/*
 * \brief   allocates a fresh usb_page for hardware descriptors
 *
 * \return  pointer to struct usb_page or NULL
 */
struct usb_page *usb_mem_page_alloc()
{
    struct usb_page *ret;

    // check if we have a free page left
    if (free_pages != NULL) {
        ret = free_pages;
        free_pages = free_pages->next;
        ret->next = NULL;
        return ret;
    }

    ret = (struct usb_page *) malloc(sizeof(struct usb_page));
    memset(ret, 0, sizeof(struct usb_page));

    uint32_t ret_size;
    errval_t err = frame_alloc(&ret->cap, USB_PAGE_SIZE, &ret_size);

    if (err) {
        return NULL;
    }

    err = invoke_frame_identify(ret->cap, &ret->frame_id);

    if (err) {
        return NULL;
    }

    err = vspace_map_one_frame_attr(&ret->page->buffer, USB_PAGE_SIZE, ret->cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err) {
        return NULL;
    }
    ret->page.phys_addr = ret->frame_id.base;
    ret->page.size = USB_PAGE_SIZE;
    ret->free.size = USB_PAGE_SIZE;
    ret->free.buffer = ret->page.buffer;

    return ret;
}

void usb_mem_page_free(struct usb_page *mem)
{
    if (free_pages != NULL) {
        mem->next = free_pages;
    } else {
        mem->next = NULL;
    }
    free_pages = mem;
}

struct usb_dma_page *usb_mem_dma_alloc(uint32_t size, uint32_t align)
{
    struct usb_dma_page *ret;

    // check if we have a free page left
    if (free_dma_buffers != NULL) {
        ret = free_dma_buffers;
        free_dma_buffers = free_dma_buffers->next;
        ret->next = NULL;
        return ret;
    }

    ret = (struct usb_dma_page *) malloc(sizeof(struct usb_dma_page));
    memset(ret, 0, sizeof(struct usb_dma_page));

    uint32_t ret_size;
    errval_t err = frame_alloc(&ret->cap, USB_PAGE_SIZE, &ret_size);

    if (err) {
        return NULL;
    }

    err = invoke_frame_identify(ret->cap, &ret->frame_id);

    if (err) {
        return NULL;
    }

    err = vspace_map_one_frame_attr(&ret->buffer, USB_PAGE_SIZE, ret->cap,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);

    if (err) {
        return NULL;
    }
    ret->phys_addr = ret->frame_id.base;
    ret->size = USB_PAGE_SIZE;

    return ret;
}

void usb_mem_dma_free(struct usb_dma_page *page)
{
    if (free_dma_buffers != NULL) {
        page->next = free_dma_buffers;
    } else {
        page->next = NULL;
    }
    free_dma_buffers = page;
}




