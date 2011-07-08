/**
 * \file
 * \brief Simple in-memory kernel log buffer for RCK console I/O.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <conio.h>
#include <paging_kernel_arch.h>

#define VIDEO_LEN   7000                ///< Number of chars in buffer
#define FEED_WIDTH  80                  ///< Number of characters to feed when buffer overflows

// Cursor position
static int xpos = 0;
// Pointer to video memory
static uint8_t *video = (uint8_t *)VIDEO_MEM;

/**
 * \brief Clear the screen and reset cursor.
 */
void conio_cls(void)
{
    memset(video, 0, VIDEO_LEN);
    xpos = 0;
}

void klog_init(void)
{
    video = (void *)paging_map_device(VIDEO_MEM, VIDEO_LEN);
}

/**
 * \brief Relocate video memory to new location.
 *
 * This function relocates the start of the video memory to the
 * virtual address 'newaddr'.
 *
 * \param newaddr       Address of start of new video memory.
 */
void conio_relocate_vidmem(lvaddr_t newaddr)
{
    video = (uint8_t *)newaddr;
}

/**
 * \brief Feed the screen page by one line.
 */
#if 1
static void page_feed(void)
{
    for(int i = FEED_WIDTH; i < VIDEO_LEN; i++) {
        video[i - FEED_WIDTH] = video[i];
    }
    for(int i = VIDEO_LEN - FEED_WIDTH; i < VIDEO_LEN; i++) {
        video[i] = 0;
    }
    /* memmove(video, video + FEED_WIDTH, VIDEO_LEN - FEED_WIDTH); */
    /* memset(video + VIDEO_LEN - FEED_WIDTH, 0, FEED_WIDTH); */
}
#endif

/**
 * \brief Put the character 'c' on the screen.
 *
 * \param c     Character to print on the screen.
 */
void conio_putchar(char c)
{
    uint8_t *vidptr;

    vidptr = video + xpos;
    *vidptr = c & 0xff;

    xpos++;
    if (xpos >= VIDEO_LEN) {
        page_feed();
        xpos -= FEED_WIDTH;
        /* conio_cls(); */
    }
}
