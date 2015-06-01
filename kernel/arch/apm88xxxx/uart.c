#include <kernel.h>
#include <serial.h>

#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"

// base addrs
#define UART0_BASE 0x1C020000
#define UART1_BASE 0x1C021000
#define UART2_BASE 0x1C022000
#define UART3_BASE 0x1C023000

unsigned serial_console_port, serial_debug_port;

errval_t serial_init(unsigned port, bool initialize_hw)
{

    panic("NYI");
}

errval_t serial_early_init(unsigned port)
{

    panic("NYI");
}

/**
 * \brief Prints a single character to a serial port. 
 */
void serial_putchar(unsigned port, char c)
{

    panic("NYI");
}

/** 
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port)
{
    panic("NYI");
}
