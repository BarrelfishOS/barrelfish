/**
 * \file
 * \brief The world's simplest serial driver.
 *
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <serial.h>
#include <hyper.h>
#include <kputchar.h>
#include <beej7.h>
#include "beekernel.h"

static uint32_t msg[63];
static char *kputbuf = 0;

#define KPBUFSZ (61*4)
static int kcount = 0;

extern int message_recv(int *srce, int *type, uint32_t * buf);
extern void message_send(int dest, int type, uint32_t *buf, int len);

extern int lockunit_read(int n);
extern void lockunit_write(int n);


static  void breakpoint(void) 
{
    // Sanity check that breakpoint code doesn't trash registers!
    //__asm volatile ("simctrl 4");
    __asm volatile ("j7	6");
    // __asm volatile ("simctrl 4");
}

static inline int running(void)
{
    int result;
    __asm("j7 5; ld %[result], link"
	  : [result]"=r" (result) :: "link", "cc");
    return result;
}


static void kflush(void)
{
    if (kcount > 0) {
        msg[0] = HYPER_SERIAL_OUT;
        msg[1] = kcount;
        int words = 2 + ((kcount+3)>>2);

	// Wait while any previous message is unfinished
	// indicated by us still having the lock/semaphore
	int mycore = arch_get_core_id();
	int lockval;
	do {
	    lockval = lockunit_read(mycore);
	    if (lockval != 1 && lockval != 2)
		breakpoint();
	} while(lockval == 2);

        message_send(1, HYPER_MSG_TYPE, msg, words);
        //breakpoint();

        kcount = 0;
    }
}

void kprintf_begin(void)
{
    if (!running())
	breakpoint();
    kcount = 0;
    kputbuf = (char*)&msg[2];
}

int kputchar(int c)
{
    if (c == '\n') {
	if (kcount == KPBUFSZ)
	    kflush();
	kputbuf[kcount++] = '\r';
    }
    if (kcount == KPBUFSZ)
	kflush();
    kputbuf[kcount++] = c;
    return c;
}

void kprintf_end(void)
{
    kflush();
}

// End
