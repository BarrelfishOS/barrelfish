##########################################################################
# Copyright (c) 2019 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# Tools for handling the boards
#
##########################################################################

OTG_PREFIX="/dev/usbOtgColibri"
UART_PREFIX="/dev/usbSerialColibri"

get_usb_dev_path() {
    DEVATTRS=$(udevadm info -a -n $1 2> /dev/null)
    if [ $? -ne 0 ]; then
        echo "Device \`$1\` not found." >&2
        return 1
    fi
    BUSNUM=$(sed -En 's/[[:space:]]*ATTR\{busnum\}=="([0-9])"/\1/p' <<< ${DEVATTRS})
    DEVPATH=$(sed -En 's/[[:space:]]*ATTR\{devpath\}=="([0-9](\.[0-9])*)"/\1/p' <<< ${DEVATTRS})
    echo "${BUSNUM}:$DEVPATH"
}

get_uart_dev_path() {
    get_usb_dev_path "${UART_PREFIX}$1"
}

get_otg_dev_path() {
    get_usb_dev_path "${OTG_PREFIX}$1"
}

get_otg_path_uuu() {
    # UUU wants the USB dev path in the format
    # <bus:devpath> where devpath doesn't contain the separating
    # '.' between the ports
    DEVPATH=$(get_otg_dev_path $1)
    if [ $? -ne 0 ]; then
        return 1
    fi
    echo $(sed -n s/\\.//gp <<< ${DEVPATH})
}

get_uart_serial_number() { 
    DEVATTRS=$(udevadm info -a -n ${UART_PREFIX}$1 2> /dev/null)
    if [ $? -ne 0 ]; then
        echo "Device \`${UART_PREFIX}$1\` not found." >&2
        return 1
    fi
    echo $(sed -rn 's/[[:space:]]*ATTR\{serial\}=="([0-9A-Z]{8})"/\1/p' <<< ${DEVATTRS})
}
