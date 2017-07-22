#!/bin/sh
##########################################################################
# Copyright (c) 2009-2015 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# Shell script for running Qemu with a Barrelfish image
#
##########################################################################

HDFILE=hd.img
EFI_FLASH0=flash0.img
EFI_FLASH1=flash1.img
HAGFISH_LOCATION="/home/netos/tftpboot/Hagfish.efi"
MENUFILE=""
ARCH=""
DEBUG_SCRIPT=""
# Grab SMP from env, if unset default to 2
SMP=${SMP:-2}
# Grab NIC_MODEL from env, if unset default to e1000
NIC_MODEL="${NIC_MODEL:-e1000}"


usage () {
    echo "Usage: $0 --menu <file> --arch <arch>  [options]"
    echo "  where:"
    echo "    'arch' is one of: x86_64, a15ve, armv8, zynq7"
    echo "    'file' is a menu.lst format file to read module list from"
    echo "  and options can be:"
    echo "    --debug <script>   (run under the specified GDB script)"
    echo "    --hdfile <file>    (hard disk image to be build for AHCI, defaults to $HDFILE"
    echo "    --kernel <file>    (kernel binary, if no menu.lst given)"
    echo "    --initrd <file>    (initial RAM disk, if no menu.lst given)"
    echo "    --image  <file>    (prebaked boot image, instead of kernel/initrd)"
    echo "    --args <args>      (kernel command-line args, if no menu.lst given)"
    echo "    --smp <cores>      (number of cores to use, defaults to $SMP)"
    echo "    --nic-model <name> (nic model to use, defaults to $NIC_MODEL)"
    echo "    --hagfish <file>   (Hagfish boot loader, defaults to $HAGFISH_LOCATION)"
    echo "  "
    echo "  The following environment variables are considered:"
    echo "    QEMU_PATH         (Path for qemu-system-* binary)"
    echo "    NIC_MODEL         (Same as --nic-model)"
    echo "    SMP               (Same as --smp)"
    exit 1
}


if test $# = 0; then usage ; fi
while test $# != 0; do
    case $1 in
    "--help"|"-h")
        usage
        exit 0
        ;;
    "--menu")
        shift; MENUFILE="$1"
        ;;
    "--arch")
        shift; ARCH="$1"
        ;;
    "--hdfile")
        shift; HDFILE="$1"
        ;;
    "--debug")
        shift; DEBUG_SCRIPT="$1"
        ;;
    "--initrd")
        shift; INITRD="$1"
        ;;
    "--kernel")
        shift; KERNEL="$1"
        ;;
    "--image")
        shift; IMAGE="$1"
        ;;
    "--args")
        shift; KERNEL_CMDS="$1"
        ;;
    "--smp")
        shift; SMP="$1"
        ;;
    "--hagfish")
        shift; HAGFISH_LOCATION="$1"
        ;;
    "--nic-model")
        shift; NIC_MODEL="$1"
        ;;
    *)
        echo "Unknown option $1 (try: --help)" >&2
        exit 1
        ;;
    esac
    shift
done

if test -z "$IMAGE"; then
    if test -z "$MENUFILE"; then
        echo "No menu.lst file specified."
        if test -z "$KERNEL"; then
        echo "ERROR: No initial kernel given and no menu.lst file." >&2; exit 1
        fi
        if test -z "$INITRD"; then
        echo "ERROR: No initial RAM disk given and no menu.lst file." >&2; exit 1
        fi
    else
        echo "Using menu file $MENUFILE"
        ROOT=`sed -rne 's,^root[ \t]*([^ ]*).*,\1,p' "$MENUFILE"`
        if test "$ROOT" != "(nd)"; then
            echo "Root: $ROOT"
        fi
        KERNEL=`sed -rne 's,^kernel[ \t]*/([^ ]*).*,\1,p' "$MENUFILE"`
        if test "$ROOT" != "(nd)"; then
            KERNEL="$ROOT/$KERNEL"
        fi
        if test -z "$KERNEL"; then
        echo "ERROR: No initial kernel specified in menu.lst file." >&2; exit 1
        fi
        KERNEL_CMDS=`sed -rne 's,^kernel[ \t]*[^ ]*[ \t]*(.*),\1,p' "$MENUFILE"`
        if test "$ROOT" != "(nd)"; then
            AWKSCRIPT='{ if (NR == 1) printf(root "/" $$0); else printf("," root "/" $$0) }'
            AWKARGS="-v root=$ROOT"
        else
            AWKSCRIPT='{ if (NR == 1) printf($$0); else printf("," $$0) }'
        fi
        INITRD=`sed -rne 's,^module(nounzip)?[ \t]*/(.*),\2,p' "$MENUFILE" | awk $AWKARGS "$AWKSCRIPT"`
        if test -z "$INITRD"; then
        echo "ERROR: No initial ram disk modules specified in menu.lst file." >&2; exit 1
        fi
    fi
    echo "Initial kernel file: $KERNEL"
    echo "Initial RAM disk contents: $INITRD"
else
    echo "Booting image: $IMAGE"
fi

echo "Kernel command line arguments: $KERNEL_CMDS"
echo "Requested architecture is $ARCH."

case "$ARCH" in
    "x86_64")
    QEMU_CMD="${QEMU_PATH}qemu-system-x86_64 \
        -machine type=q35 \
        -smp $SMP \
        -m 1024 \
        -net nic,model=$NIC_MODEL \
        -net user \
        -device ahci,id=ahci \
        -device ide-drive,drive=disk,bus=ahci.0 \
        -drive id=disk,file="$HDFILE",if=none"
    QEMU_NONDEBUG=-nographic
    GDB=gdb-multiarch
    echo "Creating hard disk image $HDFILE"
    qemu-img create "$HDFILE" 10M
    ;;
    "a15ve")
        QEMU_CMD="${QEMU_PATH}qemu-system-arm \
        -m 1024 \
        -smp $SMP \
        -machine vexpress-a15"
    GDB=gdb
    QEMU_NONDEBUG=-nographic
    ;;
    "armv8")
       #
       # The next steps create the EFI directory for QEMU.
       #    qemu-efi/
       #        - Hagfish.efi   the UEFI bootloader
       #        - Hagfish.cfg   'menu.lst' specifying what Hagfish should boot
       #        - startup.nsh   script executed by the EFI shell
       #        - ...           remaining files in the disk
       #
       # NOTE: Qemu is only able to support FAT16, hence the directory specified
       #       by -drive must be smaller than 500M.
       #       We currently copy the created binary into this directory
       #
       mkdir -p qemu-efi/armv8/sbin
       # create the startup script
       echo "\\Hagfish.efi Hagfish.cfg" > qemu-efi/startup.nsh
       chmod +x qemu-efi/startup.nsh
       # setup hagfish location
       cp $HAGFISH_LOCATION qemu-efi/Hagfish.efi
       cp platforms/arm/menu.lst.armv8_a57v qemu-efi/Hagfish.cfg
       # copy install files
       cp *.gz qemu-efi
       cp -r armv8/sbin/* qemu-efi/armv8/sbin/
       QEMU_CMD="${QEMU_PATH}qemu-system-aarch64 \
                -m 1024 \
                -cpu cortex-a57 \
                -M virt \
                -smp $SMP \
                -pflash $EFI_FLASH0 \
                -pflash $EFI_FLASH1 \
                -drive if=none,file=fat:rw:qemu-efi,id=drv \
                -device virtio-blk-device,drive=drv"
       GDB=gdb-multiarch
       QEMU_NONDEBUG=-nographic
       # Now you'll need to create pflash volumes for UEFI. Two volumes are required,
       # one static one for the UEFI firmware, and another dynamic one to store variables.
       # Both need to be exactly 64M in size. //https://wiki.ubuntu.com/ARM64/QEMU
       if ! [ -e $EFI_FLASH0 ] ; then
            dd if=/dev/zero of="$EFI_FLASH0" bs=1M count=64
            dd if=/usr/share/qemu-efi/QEMU_EFI.fd of="$EFI_FLASH0" conv=notrunc
       fi
       [ -e $EFI_FLASH1 ] || dd if=/dev/zero of="$EFI_FLASH1" bs=1M count=64
       EFI=1
       ;;
    "zynq7")
        QEMU_CMD="${QEMU_PATH}qemu-system-arm \
        -machine xilinx-zynq-a9 \
        -m 1024 \
        -serial /dev/null \
        -serial mon:stdio"
    GDB=gdb
    QEMU_NONDEBUG=-nographic
    ;;
    *)
    echo "No QEmu environment defined for architecture=$ARCH." >&2
    exit 1
    ;;
esac

export QEMU_AUDIO_DRV=none

if test "$DEBUG_SCRIPT" = ""; then
    echo "OK: about to run the follow qemu command:"
    if test -z "$EFI"; then
        if test -z "$IMAGE"; then
            echo "$QEMU_CMD $QEMU_NONDEBUG -kernel $KERNEL -append '$KERNEL_CMDS' -initrd $INITRD"
            exec $QEMU_CMD $QEMU_NONDEBUG -kernel $KERNEL -append '$KERNEL_CMDS'  -initrd "$INITRD"
        else
            echo "$QEMU_CMD $QEMU_NONDEBUG -kernel $IMAGE"
            exec $QEMU_CMD $QEMU_NONDEBUG -kernel "$IMAGE"
        fi
    else
        echo $QEMU_CMD $QEMU_NONDEBUG
        exec $QEMU_CMD $QEMU_NONDEBUG
    fi
fi


# Now we run the debugger instead
GDB_ARGS="-x $DEBUG_SCRIPT"
SERIAL_OUTPUT=file:/dev/stdout
PORT=$((10000 + UID))

if test "${SERIAL_OUTPUT}" = ""; then
    # Assuming session is interactive. Use terminal for serial output because
    # stdout does not work for daemonized qemu and output is lost. This likely
    # only matters on ARM where there is no video driver at time of writing.
    SERIAL_OUTPUT=`tty`
fi

PIDFILE=/tmp/qemu_debugsim_${USER}_${PORT}.pid
if test -f $PIDFILE; then
    if ps `cat $PIDFILE` >/dev/null; then
    echo "Another QEMU already running (PID: `cat $PIDFILE` PIDFILE: $PIDFILE)"
    exit 1
    else
    echo "Deleting stale lockfile $PIDFILE"
    rm -f $PIDFILE
    fi
fi

echo args = $GDB_ARGS

cat > barrelfish_debug.gdb <<EOF
# Connect to QEMU instance
target remote localhost:$PORT
EOF

if test -z "$EFI"; then
    if test -z "$IMAGE"; then
        QEMU_INVOCATION="${QEMU_CMD} \
            -kernel \"$KERNEL\" \
            -append \"$KERNEL_CMDS\" \
            -initrd \"$INITRD\" \
            -serial $SERIAL_OUTPUT \
            -gdb tcp::$PORT \
            -S \
            -display none \
            -daemonize \
            -pidfile $PIDFILE"
    else
        QEMU_INVOCATION="${QEMU_CMD} \
            -kernel \"$IMAGE\" \
            -append \"$KERNEL_CMDS\" \
            -serial $SERIAL_OUTPUT \
            -gdb tcp::$PORT \
            -S \
            -display none \
            -daemonize \
            -pidfile $PIDFILE"
    fi
else
    QEMU_INVOCATION="${QEMU_CMD} \
        -serial $SERIAL_OUTPUT \
        -gdb tcp::$PORT \
        -S \
        -display none \
        -daemonize \
        -pidfile $PIDFILE"
fi

echo $QEMU_INVOCATION
set -x

eval $QEMU_INVOCATION

if test $? -eq 0; then
    stty sane
    trap '' INT
    ${GDB} -x barrelfish_debug.gdb ${GDB_ARGS}
    PID=`cat ${PIDFILE}`
    kill ${PID} > /dev/null || true
    rm -f $PIDFILE
else
    echo Failed to launch qemu with:
    echo "   ${QEMU_INVOCATION}"
fi
