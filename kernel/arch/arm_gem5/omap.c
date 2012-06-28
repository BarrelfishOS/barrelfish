/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/omap_uart_dev.h>
#include <dev/cortex_a9_pit_dev.h>
#include <dev/a9scu_dev.h>

#include <omap_uart.h>
#include <serial.h>
#include <arm_hal.h>
#include <cp15.h>

//hardcoded bc gem5 doesn't set board id in ID_Register
#define VEXPRESS_ELT_BOARD_ID		0x8e0
uint32_t hal_get_board_id(void)
{
    return VEXPRESS_ELT_BOARD_ID;
}

uint8_t hal_get_cpu_id(void)
{
    return cp15_get_cpu_id();
}

//Gem5 ensures that cpu0 is always BSP, this probably also holds in general
bool hal_cpu_is_bsp(void)
{
    return cp15_get_cpu_id() == 0;
}

// clock rate hardcoded to 2GHz
static uint32_t tsc_hz = 2000000000;

//
// Interrupt controller
//

#define PIC_BASE 	0xE0200000
#define DIST_OFFSET     0x1000
#define CPU_OFFSET 	0x100

void pic_init(void)
{
    assert(!"NYI");
}

void pic_disable_all_irqs(void)
{
    assert(!"NYI");
}


void pic_enable_interrupt(uint32_t int_id, uint8_t cpu_targets, uint16_t prio,
						  uint8_t edge_triggered, uint8_t one_to_n)
{
    assert(!"NYI");
}

uint32_t pic_get_active_irq(void)
{
    assert(!"NYI");
}

void pic_raise_softirq(uint8_t cpumask, uint8_t irq)
{
    assert(!"NYI");
}

void pic_ack_irq(uint32_t irq)
{
    assert(!"NYI");
}

//
// Kernel timer and tsc
//

#define PIT_BASE 	0xE0000000
#define PIT0_OFFSET	0x11000
#define PIT_DIFF	0x1000

#define PIT0_IRQ	36
#define PIT1_IRQ	37

#define PIT0_ID		0
#define PIT1_ID		1

/* static lvaddr_t pit_map_resources(void) */
/* { */
/*     assert(!"NYI"); */
/* } */

void pit_init(uint32_t timeslice, uint8_t pit_id)
{
    assert(!"NYI");
}

void pit_start(uint8_t pit_id)
{
    assert(!"NYI");
}

bool pit_handle_irq(uint32_t irq)
{
    assert(!"NYI");
}

void pit_mask_irq(bool masked, uint8_t pit_id)
{
    assert(!"NYI");
}

//
// TSC uses cpu private timer
//

#define TSC_BASE 	0xE0200000
#define TSC_OFFSET	0x600

static cortex_a9_pit_t tsc;

void tsc_init(void)
{
    lvaddr_t timer_base = paging_map_device(TSC_BASE, ARM_L1_SECTION_BYTES);

    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)timer_base+TSC_OFFSET);

    // write load
    uint32_t load = ~0ul;
    cortex_a9_pit_TimerLoad_wr(&tsc, load);

    //configure tsc
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 0);
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);

}

uint32_t tsc_read(void)
{
    // Timers count down so invert it.
    return ~cortex_a9_pit_TimerCounter_rd(&tsc);
}

uint32_t tsc_get_hz(void)
{
    return tsc_hz;
}


//
// Snoop Control Unit
//

#define SCU_BASE	0xE0200000

static a9scu_t scu;

void scu_enable(void)
{
    lvaddr_t scu_base = paging_map_device(TSC_BASE, ARM_L1_SECTION_BYTES);

    a9scu_initialize(&scu, (mackerel_addr_t)scu_base);

    //enable SCU
    a9scu_SCUControl_t ctrl_reg = a9scu_SCUControl_rd(&scu);
    ctrl_reg |= 0x1;
    a9scu_SCUControl_wr(&scu, ctrl_reg);
    //(should invalidate d-cache here)
}

int scu_get_core_count(void)
{
	return a9scu_SCUConfig_cpu_number_rdf(&scu);
}

//
// Serial console and debugger interfaces
//

#define CONSOLE_PORT 2
#define DEBUG_PORT   2

#define UART3_VBASE			0x48020000
#define UART_DEVICE_BYTES		0x4c
#define UART_MAPPING_DIFF		0x1000

static omap_uart_t ports[4];

static errval_t serial_init(uint8_t index, uint8_t port_no)
{
    assert(!"NYI");

    if (port_no < 4) {
        //assert(ports[port_no].base == 0);

        lvaddr_t base = paging_map_device(UART3_VBASE + port_no * UART_MAPPING_DIFF,
                                          UART_DEVICE_BYTES);
        omap_uart_init(&ports[index], base + port_no * UART_MAPPING_DIFF);
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}

errval_t early_serial_init(uint8_t port_no);
errval_t early_serial_init(uint8_t port_no)
{
    if (port_no < 4) {
        assert(ports[port_no].base == 0);
        omap_uart_init(&ports[CONSOLE_PORT], UART3_VBASE);
        return SYS_ERR_OK;
    }
    else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}

errval_t serial_console_init(uint8_t port_ordinal)
{
    return serial_init(CONSOLE_PORT, port_ordinal);
}

void serial_console_putchar(char c)
{
    if (c == '\n') {
        omap_putchar(&ports[CONSOLE_PORT], '\r');
    }

    omap_putchar(&ports[CONSOLE_PORT], c);
}

char serial_console_getchar(void)
{
    return omap_getchar(&ports[CONSOLE_PORT]);
}

errval_t serial_debug_init(uint8_t port_ordinal)
{
    return serial_init(DEBUG_PORT, port_ordinal);
}

void serial_debug_putchar(char c)
{
    if (c == '\n') {
        omap_putchar(&ports[DEBUG_PORT], '\r');
    }
    omap_putchar(&ports[DEBUG_PORT], c);
}

char serial_debug_getchar(void)
{
    return omap_getchar(&ports[DEBUG_PORT]);
}
