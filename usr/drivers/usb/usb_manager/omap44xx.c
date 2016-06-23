/*
 * =========================================================================
 * ARM and PandaBoard specific functions
 * =========================================================================
 */

#include <barrelfish/barrelfish.h>

#include <usb/usb.h>

#include "platform.h"
// the offset into the supplied capability
#define USB_CAPABILITY_OFFSET (0x00000C00)

// the EHCI interrupt number on the pandaboard
#define USB_ARM_EHCI_IRQ 109

/**
 * \brief this is a quick check function if everything is all right
 *
 * NOTE: there is just one specific setting possible on the PandaBoard
 *       EHCI controller, with the specific capability and the specific offset
 *       This function checks if these values match to ensure functionality
 *       If you change something with the startup of the USB manager domain
 *       you may need to change the values in this function!
 */
usb_error_t platform_checkup(uintptr_t base, int argc, char *argv[])
{
    USB_DEBUG("performing pandaboard integrity check.\n");

    /* checking the host controller type */
    if (strcmp(argv[0], "ehci")) {
        debug_printf("wrong host controller type: %s\n", argv[0]);
        return (USB_ERR_INVAL);
    }

    /* checking the memory offset */
    if (strtoul(argv[1], NULL, 10) != ((uint32_t) USB_CAPABILITY_OFFSET)) {
        debug_printf("wrong offset!: %x (%s)\n", strtoul(argv[1], NULL, 10),
                argv[1]);
        return (USB_ERR_INVAL);
    }

    /* checking the IRQ number */
    if (strtoul(argv[2], NULL, 10) != USB_ARM_EHCI_IRQ) {
        debug_printf("wrong interrupt number: %s, %x", argv[2],
                strtoul(argv[2], NULL, 10));
        return (USB_ERR_INVAL);
    }

    /*
     * here we read some values from the ULPI register of the PandaBoards
     * additional ULPI interface on the EHCI controller.
     *
     * The request are forwarded to the external ULPI receiver on the
     * PandaBoard.
     *
     * NOTE: Not every EHCI controller has those register!
     */

    uintptr_t tmp = USB_CAPABILITY_OFFSET + (uintptr_t) base;
    volatile uint32_t *ulpi_debug_reg = (volatile uint32_t*) (tmp + 0x00A4);
    /*
     * This request reads the debug register of the ULPI receiver. The values
     * returned are the line state. If the returned value is 0x1 this means
     * there is connection i.e. the USB hub on the PandaBoard is reachable.
     */

    printf("check whether USB hub is reachable... ");
    *ulpi_debug_reg = (uint32_t) ((0x15 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));

    /* wait till the request is done */
    while ((*ulpi_debug_reg) & (1UL << 31)) {
        thread_yield();
    }

    // XXX: reset register here?
    ulpi_debug_reg = (volatile uint32_t*) (tmp + 0x00A4);
    /* compare the result */
    if (!((*ulpi_debug_reg) & 0x1)) {
        return (USB_ERR_INVAL);
    }

    printf("ok\nCheck vendor ID of ULPI receiver... ");
    /*
     * This request reads out the low part of the vendor id from the ULPI
     * receiver on the PandaBoard. This should be 0x24.
     *
     * XXX: Assuming that all the Pandaboards have the same ULPI receiver
     */
    *ulpi_debug_reg = (uint32_t) ((0x00 << 16)
            | (0x3 << 22) | (0x1 << 24) | (0x1 << 31));

    /* wait till request is done */
    while ((*ulpi_debug_reg) & (1 << 31)) {
    }

    /* compare the values */
    if (0x24 != ((*ulpi_debug_reg) & 0xFF)) {
        return (USB_ERR_INVAL);
    }
    printf("ok\n");

    return (USB_ERR_OK);
}
