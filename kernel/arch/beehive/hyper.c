/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/* 
 * Core 1 runs a small 'hypervisor' which acts as a timer device, 
 * serial device and interrupt controller. 
 *
 * It interacts with other cores via DEBUG messages.  
 *
 */

#include <stdarg.h>
#include <kernel.h>
#include <hyper.h>
#include <serial.h>
#include <bexec.h>
#include <simctrl.h>
#include <corearea.h>
#include <dcache.h>
#include <stopcode.h>


#if 0
#define HYPERDBG(x) serial_console_putchar(x)
#else
#define HYPERDBG(x) (void)sizeof(x)
#endif

#define ROUNDUP(x,y) (((x)+(y)-1) & ~((y)-1))
#define BASE_PAGE_SIZE 0x1000


static void halt(void) __attribute__((noreturn));
static void serial_dec_word(uint32_t word);
static void serial_hex_word(uint32_t word);

void bmain(uint32_t a, uint32_t b, uint32_t c, 
	   uint32_t d, uint32_t e, uint32_t f)
    __attribute__((noreturn));

#define HYPERFAIL(format, ...) hyperfail(__LINE__, format, ## __VA_ARGS__)

/*
 * Stop the system if running in the simulator
 */
static void halt(void)
{
    for(;;)
	BEE_SIMCTRL(BEE_SIMCTRL_TERMINATE);
}

/*
 * Do a register dump if running in the simulator
 */
static inline void dumpregs(void)
{
    BEE_SIMCTRL(BEE_SIMCTRL_REGISTERS);
}

/*
 * Read the ID register to get the core number 
 */
static inline uint8_t arch_get_core_id(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
 
    return (val >> 10) & 0xf;
}

static inline uint8_t arch_get_max_core_id(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
    return (val >> 14) & 0xf;
}


static inline unsigned int cyclecounter(void)
{
    return *(volatile unsigned int *)(0x22);
}


/*
 * Hardware messages are up to 63 32-bit words
 */
typedef unsigned int msg_t[63];

/*
 * Send a message to core number "dest", using "len" words at "buf".
 *
 * Note that message lengths are measured in words, not bytes.
 */
void message_send(unsigned int dest, unsigned int type,
		  msg_t *buf, unsigned int len);

/*
 * If there's a message available to receive, place its details and
 * contents in (srce, type, buf) and return its length.
 * Otherwise return 0.
 */
unsigned int message_recv(unsigned int *srce, unsigned int *type,
			  msg_t * buf);


extern int lockunit_read(int n);
extern void lockunit_write(int n);


int hyperfail(int line, const char *fmt, ...) __attribute__ ((format(printf, 2, 3)))
    __attribute__((noreturn));

int vprintf(const char *fmt, va_list ap)
{
    int percent = 0;
    int ch;
    while ((ch = *fmt++) != 0) {
	if (percent) {
	    if (ch == 'x' || ch == 'X' || ch == 'p')
		serial_hex_word(va_arg(ap, unsigned));
	    else if (ch == '%')
		serial_console_putchar('%');
	    else if (ch == 'c')
		serial_console_putchar(va_arg(ap, int));
	    else
		serial_dec_word(va_arg(ap, unsigned));
	    percent = 0;
	}
	else if (ch == '%')
	    percent = 1;
	else if (ch == '\n') {
	    serial_console_putchar('\r');
	    serial_console_putchar(ch);
	}
	else
	    serial_console_putchar(ch);
    }
    va_end(ap);
    return 0;
}

int printf(const char *fmt, ...)
{
    int result;
    va_list ap;
    va_start(ap, fmt);
    result = vprintf(fmt, ap);
    va_end(ap);
    return result;
}

int hyperfail(int line, const char *fmt, ...)
{
    va_list ap;
    printf("\nhypervisor failure at line %d\n", line);
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
    printf("\n");
    halt();
}

/// Different handler for cap operations performed by the monitor
static errval_t arch_spawn_core(coreid_t core_id, genvaddr_t entry)
{
    // Setup registers in save area
    struct corearea *savearea = COREAREA_FOR_CORE(core_id);

    for (unsigned i = 1; i < 32; i++) {
        savearea->regs[i] = 0xBAD000 | i;
    }
    // Count
    savearea->regs[0]  = 0;

    // Arg Registers
    savearea->regs[3]  = 0xA1;
    savearea->regs[4]  = 0xA2;
    savearea->regs[5]  = 0xA3; 
    savearea->regs[6]  = 0xA4;
    savearea->regs[7]  = 0xA5;
    savearea->regs[8]  = 0xA6;

    // SavedLink
    savearea->regs[30] = 0xBAD;
 
    // SavedPC
    savearea->regs[31] = entry;

    // Mark kernel covering whole memory so timer interrupts are deferred
    savearea->kernel_begins = 0;
    savearea->kernel_ends = 0x7fffffff;

    //bee_dcache_flush_all();
    bee_dcache_empty_all();
   //bee_dcache_flush_rgn(savearea, 512);

    // Send a START message
    HYPERDBG('>');
    message_send(core_id, 0, NULL, 0);

    return SYS_ERR_OK;
}

static void serial_hex_nibble(uint32_t nibble);
static void serial_hex_nibble(uint32_t nibble)
{
    nibble &= 0xf;
    if (nibble > 9) 
	nibble += '@' - '0';
    serial_console_putchar(nibble + '0');
}
	
static void serial_hex_word(uint32_t word)
{
    serial_console_putchar('0');
    serial_console_putchar('x');
    serial_hex_nibble((word>>28));
    serial_hex_nibble((word>>24));
    serial_hex_nibble((word>>20));
    serial_hex_nibble((word>>16));
    serial_hex_nibble((word>>12));
    serial_hex_nibble((word>>8));
    serial_hex_nibble((word>>4));
    serial_hex_nibble((word));
}

static void serial_dec_word(uint32_t word)
{
    if (word > 9) {
	uint32_t tens = word / 10;
	word = word - (10 * tens);
	serial_dec_word(tens);
    }
    serial_console_putchar('0'+word);
}

/* Reads a single character from the default serial port.
 * Returns -1 if there isnt one without delaying
 */
static int serial_poll_getchar(void)
{
    // The RS232 is in IO position 0, register 0 XXX Should have a
    // MACRO in an arch header file to build these type of constants
    volatile int *const rs232 = (int *)0x02;

    int reg = *rs232;
    if ((reg & 0x100) == 0)
	return -1;
    // Indicate that we noticed it
    *rs232 = 0x100;
    return (reg & 0xff);
}



static inline int arch_is_simulator(void)
{
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;

    printf("identity=%x\n", val);

    val >>= 18;
    val &= 0x7f;
    return val == 2 ? 1 : 0;
}

/**
 * \brief Expect a break message from all cores and panic if not
 *
 */
static inline void expect_breaks(uint32_t pc, unsigned wait);
static inline void expect_breaks(uint32_t pc, unsigned wait)
{
    int expectbreak[16];
    for (unsigned i = 0; i < 16; i++)
	expectbreak[i] = 0;
    int ncores = arch_get_max_core_id();
    for(unsigned i=2; i<ncores; i++)
	expectbreak[i] = 1;
    
    msg_t msg; 
    unsigned int srce = 0;
    unsigned int type = 0;
    unsigned int len;
    if (wait != 0)
	wait = ncores - 2;
    while ((len = message_recv(&srce, &type, &msg)) != 0 || (wait != 0)) {
	serial_console_putchar('0'+len);
	serial_console_putchar('A'+wait);
	if (len == 1 && type == 1 && expectbreak[srce] == 1 && msg[0] == pc) {
	    expectbreak[srce] = 0;
	    if (wait > 0)
		wait--;
	}
	else if (len != 0)
	    HYPERFAIL("not break len=%d srce=%d type=%d", len, srce, type);
    }
    for(unsigned i=2; i<ncores; i++)
	if (expectbreak[i] != 0)
	    HYPERFAIL("no expected break core %d", i);
} // expect_breaks


#define STEP(_s) \
    serial_console_putchar('0'+(_s))

void change_slaves(void);
void change_slaves(void)
{
    const uint32_t jplus8 = 0xf800220c;
    const uint32_t jlink = 0xf000030c;
    const uint32_t nop = 0x000000c5;
    uint32_t *const iflushDataAddr = (uint32_t *)0x7ffff000;
    
    printf("change_slaves: ");
    STEP(0);
    // Step 0: new slaves dont build cache flush table so just
    // in case our new slave is already installed in hardware
    // we must build the cache flush table before we poke them
    for(int i=0; i<1024-8; i+=8)
	iflushDataAddr[i] = jplus8;
    iflushDataAddr[1024-8] = jlink;
    
    iflushDataAddr[1024-7] = nop;
    iflushDataAddr[1024-6] = nop;
    iflushDataAddr[1024-5] = jlink;
    bee_dcache_flush_all();

    STEP(1);
    // Step 1: set the resume context for all cores to execute
    // the iflush code and then return to the return (loops)
    unsigned ncores = arch_get_max_core_id();
    for(unsigned i=2; i < ncores; i++) {
	struct corearea *corearea = COREAREA_FOR_CORE(i);
	corearea->regs[0] = 0;
	corearea->regs[30] = ((uint32_t)(iflushDataAddr+1024-7)) >> 2;
	corearea->regs[31] = ((uint32_t)iflushDataAddr) >> 2;
    }
    bee_dcache_flush_all();

    STEP(2);
    // Step 2: cause the old slaves to start running the
    // cache flush and then loop
    for(unsigned i=2; i<ncores; i++) {
	message_send(i, 0, NULL, 0);
    }

    STEP(3);
    // Step 3: wait long enough that they must have finished iflush
    // and are now looping.  They have to notice the start; load
    // context execute the iflush, finish loading the context and then
    // execute the iflush all over again (the second one we expect to
    // be mostly cached).  This should be shorter than the time it takes
    // us to read the same memory three times (with *empty* in between)
    for(unsigned i=0; i<3; i++) {
	bee_dcache_empty_all();
	for (unsigned j=0; j<1024; j++)
	    ((volatile uint32_t *)iflushDataAddr)[j];
    }
    
    STEP(4);
    // Step 4: copy the new slave into place
    // and zero all the core areas. except tell the new slave that we
    // expect it to stop.
    extern void slaveStart(void);
    extern void slaveAfter(void);
    uint32_t *from = (uint32_t*)(((uint32_t)slaveStart) << 2);
    uint32_t *to = (uint32_t*)0;
    uint32_t count = slaveAfter - slaveStart;
    while(count--)
	*to++ = *from++;

    for(int i=2; i<ncores; i++) {
	struct corearea *corearea = COREAREA_FOR_CORE(i);
	for(unsigned j=0; j<sizeof(*corearea)/sizeof(uint32_t); j++)
	    ((uint32_t*)corearea)[j] = 0;
	corearea->master_stops = 1;
    }
    bee_dcache_flush_all();

    STEP(5);
    // Step 5: send the cores a stop
    // TODO: XXX use the signal word to explain to them...
    for (unsigned i=2; i<ncores; i++) {
	message_send(i, 1, NULL, 0);
    }

    // Step 6: Wait for them to sync
    STEP(6);
    expect_breaks(STOPCODE_STOP, 1);
    printf("done\n");
} // change_slaves


static inline void backtrace(uintptr_t fp)
{
    int i=0;
    printf("Backtrace follows:\n");
    printf("   ---Frame-- -Address-\n");
    while ((fp != 0) && ((fp & (sizeof(fp)-1)) == 0)) {
	printf("%d %x %x\n", i, fp, ((uintptr_t *)fp)[1]);
	fp = *(uintptr_t *)fp;
	i++;
    }
    printf("%d %x\n", i, fp);
}

#define CORESTATE_STOPPED        1
#define CORESTATE_STOP_SENT      2
#define CORESTATE_RUN_SENT       4
#define CORESTATE_SEND_STOP      8
#define CORESTATE_SEND_KILL      16
#define CORESTATE_EXPECT_BREAKPOINT 32

int two = 2; // Strange but true; needed to avoid a compiler bug

void bmain(uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e, uint32_t f)
{
    uint8_t core;

    int corestate[16];
    int expectstoptype[16];

    HYPERDBG('*');

    core = arch_get_core_id();

    if (core != 1) {
	HYPERFAIL("core=%d", core);
    }

    int simulator = arch_is_simulator();

    extern void _data_iafter(void);
    uintptr_t codelast = (uintptr_t)&_data_iafter;

    bexec_t *bexec = (bexec_t*)ROUNDUP(codelast<<2, BASE_PAGE_SIZE);


    if ((uintptr_t)bexec  != 0x2000<<2) {
	HYPERFAIL("bexec=%p", bexec);
    }
    if (bexec->bmagic != BEXEC_BMAGIC) {
	HYPERFAIL("bmagic=%x", bexec->bmagic);
    }

#if 0
    // Print the debug stub code for checking
    if (!simulator) {
	printf("Debug stub:\n");
	for (int i = 0; i < 0x300; i+=4) {
	    serial_hex_word(*(unsigned *)i);
	    serial_console_putchar('\r');
	    serial_console_putchar('\n');
	}
    }
#endif

    for (int i = 0; i < 16; i++)
	corestate[i] = expectstoptype[i] = 0;

    if (simulator) {
	// The other cores should have run the slave code which should
	// give initial power-on breakpoint.
	expect_breaks(0, 0);
    }

    change_slaves();

    // Start first real kernel on core 2 (XXX hardwired address for now!)
    arch_spawn_core(2, 0x2000);

    unsigned int alarmdelta = (simulator ? 20*1000*1000 : 100*1000*1000);
    unsigned int alarm = cyclecounter() + alarmdelta;
    unsigned ncores = arch_get_max_core_id();

    corestate[2] = CORESTATE_RUN_SENT;
    for(unsigned i=3; i<ncores; i++)
	corestate[i] = CORESTATE_STOPPED;

    printf("hyper: alarm=%d\n", alarm);

    // Handle messages forever
    while (1) {
        msg_t msg; 
        unsigned int srce = 0;
        unsigned int type = 0;
        unsigned int len;

	do {
	    int ch = serial_poll_getchar();
	    if (ch != -1) {
		printf("hyper: ch=%c\n", ch);
	    }
	    if (ch == 'K' || ch == 'k' || ch == 'S' || ch =='s') { // Stop or Kill
		// Send breaks to all cores
		for (unsigned i=2; i<ncores; i++) {
		    struct corearea *corearea = COREAREA_FOR_CORE(i);
		    corearea->master_stops++;
		}
		bee_dcache_flush_all();
		for (unsigned i=2; i<ncores; i++) {
		    message_send(i, ch == 'K' ? 2 : 1, NULL, 0);
		}
	    }
	    unsigned int cycles = cyclecounter();
	    if (((int)(cycles - alarm)) > 0) {
		// Send timer to all cores
              //		printf("hyper: timer at %d\n", cycles);
		for (unsigned i=2; i<ncores; i++) {
		    struct corearea *corearea = COREAREA_FOR_CORE(i);
		    if (corestate[i] == CORESTATE_RUN_SENT) {
			corearea->master_ticks++;
			corestate[i] = CORESTATE_SEND_STOP;
			expectstoptype[i] = STOPCODE_TIMER;
		    } else if (corestate[i] == CORESTATE_STOP_SENT) {
			corearea->master_stops++;
			printf("hyper: %d not responding!\n", i);
			corestate[i] = CORESTATE_SEND_KILL;
			expectstoptype[i] = STOPCODE_STOP;
		    } else if ((corestate[i] & CORESTATE_EXPECT_BREAKPOINT) != 0) {
		    } else if (corestate[i] != CORESTATE_STOPPED)
			HYPERFAIL("corestate[%d]=%d", i, corestate[i]);
		}
		bee_dcache_flush_all(); // after decisions before sends
		for (unsigned i=2; i<ncores; i++) {
		    if (corestate[i] == CORESTATE_SEND_STOP) {
			message_send(i, 1, NULL, 0);
			corestate[i] = CORESTATE_STOP_SENT;
		    } else if (corestate[i] == CORESTATE_SEND_KILL) {
			message_send(i, 2, NULL, 0);
			corestate[i] = CORESTATE_STOP_SENT;
		    }
		}
		alarm = cyclecounter() + alarmdelta;
                //		printf("hyper: alarm=%d\n", alarm);
	    }
	    //BEE_SIMCTRL(BEE_SIMCTRL_HALT);
	    len = message_recv(&srce, &type, &msg);
	} while(len == 0);

	HYPERDBG('<');
	HYPERDBG('0'+srce);
	HYPERDBG('T');
	HYPERDBG('0'+type);
	HYPERDBG('L');
	HYPERDBG('0'+len);
	
	if (type == HYPER_MSG_TYPE) {
	    uint32_t opcode = msg[0];
                
	    switch(opcode) {
	    case HYPER_START_CORE:
		HYPERDBG('S');
		HYPERDBG('0'+msg[1]);
		HYPERDBG('>');
		if (corestate[msg[1]] != CORESTATE_STOPPED)
		    HYPERFAIL("start core %d state %d", msg[1], corestate[msg[1]]);
		message_send(msg[1], 0, NULL, 0);
		corestate[msg[1]] = CORESTATE_RUN_SENT;
		break;

	    case HYPER_SERIAL_OUT:
		if (1) {
#if 0
		    const char *hex = "0123456789ABCDEF";
		    serial_console_putchar(hex[srce]);
		    serial_console_putchar(':');
#endif
		    int outlen = msg[1];
		    char *buf = (char*)&msg[2];
		    // Synchronous output for now...
		    while (outlen) {
			serial_console_putchar(*buf++);
			outlen--;
		    }
		}
		// // XXX Core will breakpoint next ... 
		// // restart once serial output is complete
		//corestate[srce] |= CORESTATE_EXPECT_BREAKPOINT;
		//expectstoptype[srce] = STOPCODE_BREAK;
		// clear lock so core knows it can resume (or
		// send next printf if printf if async)
		lockunit_write(srce);
		break;
	    default:
		HYPERDBG('?');
		break;
	    }
	} else if (type == 1 && len == 1) {
	    // STOPACK TYPE:1 LEN:1 MSG[0]:STOPCODE
	    uint32_t stopcode = msg[0];
	    if (corestate[srce] == CORESTATE_STOP_SENT
		&& stopcode == STOPCODE_TIMER
		&& expectstoptype[srce] == STOPCODE_TIMER) {
		// just get it running again asap
              //		printf("hyper: timer ack %d\n", srce);
		message_send(srce, 0, NULL, 0);
		corestate[srce] = CORESTATE_RUN_SENT;
	    }
	    else if (stopcode == STOPCODE_BREAK
		     && ((corestate[srce] & CORESTATE_EXPECT_BREAKPOINT) != 0)
		     && expectstoptype[srce] == STOPCODE_BREAK) {
		/* Restart the core: dont need to skip over breakpoint
		 * as that is now done in the slave */
		//bee_dcache_empty_all();
		//struct corearea *corearea = COREAREA_FOR_CORE(srce);
		//corearea->regs[31]++;
		//bee_dcache_empty_all();
		message_send(srce, 0, NULL, 0);
		corestate[srce] &= ~CORESTATE_EXPECT_BREAKPOINT;
	    }
	    else {
		printf("hyper: Unexpected stop %d on core %d state %d expect %d\n",
		       stopcode, srce, corestate[srce], expectstoptype[srce]);

		// XXX should really be invalidate of savearea
		bee_dcache_empty_all();

		// Dump registers in save area
		struct corearea *savearea = COREAREA_FOR_CORE(srce);

		for (unsigned i = 0; i < 32; i++) {
		    if ((i & 3) == 0) {
			if (i < 10) serial_console_putchar(' ');
			serial_dec_word(i);
		    }
		    serial_console_putchar(' ');
		    serial_hex_word(savearea->regs[i]);
		    if ((i & 3) == 3)
			printf("\n");
		}
		backtrace(savearea->regs[23]);

		for (unsigned p = savearea->regs[31] - 8;
		     p < savearea->regs[31] + 8;
		     p++) {
		    printf("word %x = %x\n", p, *((unsigned *)(p << 2)));
		}

		corestate[srce] = CORESTATE_STOPPED;

		int allstopped = 1;
		for(unsigned i=two; i<ncores; i++) {
		    if (corestate[i] != CORESTATE_STOPPED) {
			allstopped = 0;
			break;
		    }
		}
		if (allstopped)
		    HYPERFAIL("all stopped");
	    }
	} else {
	    printf("hyper: Unexpected type %x len %d: ", type, len);
	    for (unsigned i=0; i<len; i++) {
		printf("%x\n", msg[i]);
	    }
	}
    } // while(1)
}
