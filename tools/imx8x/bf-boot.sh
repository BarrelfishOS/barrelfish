#!/bin/bash
############################################################################
# Copyright (c) 2019 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# Shell script for booting Barrelfish on SoCs with U-Boot using SDP and UUU
#
############################################################################

set -e 

SCRIPT_DIR=$(dirname $0)

usage() { 
    echo "Usage: $0 <options>"
    echo "   --bf: Barrelfish image (required)"
    echo "   --board: Select board"
    echo "   --no-reset: Don't reset the board"
    echo ""
    echo "The script assumes that uuu is either on your"
    echo "search path or the UUU environment variables is set."
    exit 1;
}

while [ $# -ne 0 ]; do
    case $1 in
	"--bf") 
		BF_IMAGE=$2
        shift 
	    ;;
    "--board")
        BOARD=$2
        shift
        ;;
    "--no-reset") 
		NO_RESET=1
	    ;;
    "-h"|"--help")
        usage
        ;;
	*) 
	    usage
	    ;;
    esac
    shift
done

UUU=${UUU:-"uuu"}
if !(which $UUU > /dev/null); then
    echo "\`${UUU}\` not found or not executable. Add to PATH or set UUU variable." >&2
    exit 1
fi

if [ -z "$BF_IMAGE" ]; then
    echo "No Barrelfish image specified." >&2
    exit 1;
fi

source "${SCRIPT_DIR}/board-tools.sh"
BOARD_CTRL="${SCRIPT_DIR}/board_ctrl.py"

if [ ! -z "$BOARD" ]; then
    BOARD_SERIAL=$(get_uart_serial_number $BOARD)
    if [ $? -ne 0 ]; then
        exit 1
    fi
    BOARD_CTRL="${BOARD_CTRL} --board ${BOARD_SERIAL}"
fi

if [ ${NO_RESET:-0} -eq 0 ]; then
    echo ""
    echo "Resetting board"
    $BOARD_CTRL reset
fi

echo ""
echo "Creating temp dir"
TEMPDIR=$(mktemp -d)

echo ""
echo "Copying auxiliary files to ${TEMPDIR}"
cp "${SCRIPT_DIR}/aux-bf-boot/boot.scr" "${TEMPDIR}/"
cp "${SCRIPT_DIR}/aux-bf-boot/uuu.auto" "${TEMPDIR}/"

echo ""
echo "Copying Barrelfish image to ${TEMPDIR}"
cp $BF_IMAGE "${TEMPDIR}/image.efi"

# Wait for fastboot to be started, and udev rules have been applied...
sleep 1
if [ ! -z "$BOARD" ]; then
    DEVPATH=$(get_otg_path_uuu $BOARD)
    if [ $? -ne 0 ]; then
        exit 1
    fi
    UUU="${UUU} -m ${DEVPATH}"
fi

echo ""
echo "Flashing board"
$UUU ${TEMPDIR}

#Clean up
rm -rf $TEMPDIR
