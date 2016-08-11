#!/bin/sh

OBJDUMP=$1
IMAGE_FILE=$2
BOOT_DRIVER=$3
CPU_DRIVER=$4
OUT_FILE=$5

BOOTADDR="0x$($OBJDUMP $IMAGE_FILE -p -w | grep LOAD | \
              sed "s/ \+/\t/g" | cut -f 6 | sed s/0x//)"
CPUADDR="0x$($OBJDUMP $IMAGE_FILE -h -w -j .cpudriver | tail -n 1 | \
             sed "s/ \+/\t/g" | cut -f 5 | sed s/0x//)"

cat >$OUT_FILE <<EOF
add-symbol-file $BOOT_DRIVER $BOOTADDR
add-symbol-file $CPU_DRIVER $CPUADDR
EOF
