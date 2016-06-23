#!/bin/bash
##########################################################################
# Copyright (c) 2016, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# Rudimentary wrapper script to boot ARM GEM5 with a Barrelfish image.

if [ "$#" -lt 2 ] ; then
    echo "*** Usage: $0 <machine-type> <boot-image-file> [<m5-path> <port>]"
    exit 1
fi

export MACHINE="$1" 
export KERNEL=$(realpath $2)

PORT=

if [ "$#" -gt 2 ] ; then
    if [ "$#" -lt 4 ] ; then
        echo "*** Usage: $0 <machine-type> <boot-image-file> [<m5-path> <port>]"
        exit 1
    fi

    export M5_PATH=$(realpath $3)
    PORT="--console-port=$4"
fi

if [ -z "$M5_PATH" ]; then
    echo "*** Error: M5_PATH variable not set."
    echo "    Please set M5_PATH to be the pathname of the GEM5 build directory"
    echo "    (i.e. the directory containing LICENSE, build, READ, etc.)"
    exit 1
fi

export M5_DIR="$M5_PATH"
export M5=$M5_DIR/build/ARM/gem5.fast

exec "$M5" "$M5_DIR/configs/example/fs.py" \
    --bare-metal \
    --kernel="$KERNEL" \
    --machine-type="$MACHINE" \
    --disk-image="$M5_DIR/disks/linux-aarch32-ael.img" \
    --mem-type=SimpleMemory \
    --mem-size=512MB \
    $PORT
