#include <kernel.h>
#include <serial.h>
#include <dev/apm88xxxx/apm88xxxx_pc16550_dev.h>

#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"

// base addrs
#define UART0_BASE ((mackerel_addr_t)0x1C020000)
#define UART1_BASE ((mackerel_addr_t)0x1C021000)
#define UART2_BASE ((mackerel_addr_t)0x1C022000)
#define UART3_BASE ((mackerel_addr_t)0x1C023000)

// #ports
#define NUM_PORTS 4

unsigned serial_console_port = 0;
unsigned serial_debug_port   = 0;

// port base addresses for lookup by port no
static const mackerel_addr_t portbases[NUM_PORTS] =
    { UART0_BASE, UART1_BASE, UART2_BASE, UART3_BASE };
// the mackerel devices
static apm88xxxx_pc16550_t ports[NUM_PORTS];

errval_t serial_init(unsigned port, bool initialize_hw)
{
    if (port >= NUM_PORTS) {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }

    if (ports[port].base == portbases[port]) {
        return SYS_ERR_OK;
    }

    apm88xxxx_pc16550_t *uart = &ports[port];
    apm88xxxx_pc16550_initialize(uart, portbases[port]);

    if (!initialize_hw) {
        // hw initialized, this is for non-bsp cores, where hw has been
        // initialized by bsp core and we come through here just to setup our
        // local apm88xxxx_pc16550 struct for the port.
        return SYS_ERR_OK;
    }

    panic("device init NYI");
    return SYS_ERR_OK;
}

errval_t serial_early_init(unsigned port)
{
    // XXX: early init we don't need to do anything?
    // NOTE: adapted from x86/serial.c
    return SYS_ERR_OK;
}

/**
 * \brief Prints a single character to a serial port. 
 */
void serial_putchar(unsigned port, char c)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    // Wait until FIFO can hold more characters
    while(!apm88xxxx_pc16550_LSR_thre_rdf(&ports[port]));
    // Write character
    apm88xxxx_pc16550_THR_thr_wrf(&ports[port], c);
}

/** 
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);

    // Wait until character available
    while(!apm88xxxx_pc16550_LSR_dr_rdf(&ports[port]));
    // Read a character from FIFO
    return apm88xxxx_pc16550_RBR_rbr_rdf(&ports[port]);
}
