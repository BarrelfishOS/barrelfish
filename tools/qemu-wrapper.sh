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
BIOS="/home/netos/tftpboot/QEMU_EFI.fd"
# Grab SMP from env, if unset default to 1
SMP=${SMP:-4}
# Grab the MEMORY from the enf
MEMORY=${MEMORY:-4G}
# Grab the KVM fenable from the env
KVM=${KVM:-"-enable-kvm"}

# Grab NIC_MODEL from env, if unset default to e1000
NIC_MODEL="${NIC_MODEL:-e1000}"
# U-Boot options
UBOOT=false
UBOOT_IMAGE=/home/netos/tftpboot/u-boot.bin


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
    echo "    --bios <file>      (ARMv8 QEMU bios,  defaults to $BIOS)"
    echo "    --uboot            (boot U-Boot instead of EFI on ARMv8)"
    echo "    --uboot-img <file> (U-Boot binary, defaults to $UBOOT_IMAGE)"
    echo "  "
    echo "  The following environment variables are considered:"
    echo "    QEMU_PATH         (Path for qemu-system-* binary)"
    echo "    NIC_MODEL         (Same as --nic-model)"
    echo "    SMP               (Same as --smp)"
    exit 1
}

# Result in $?
qemu_supports_device() {
    ${QEMU_PATH}qemu-system-x86_64 -device help 2>&1 | grep \"$1\" > /dev/null
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
    "--bios")
        shift; BIOS="$1"
        ;;
    "--nic-model")
        shift; NIC_MODEL="$1"
        ;;
    "--uboot")
        UBOOT=true
        ;;
    "--uboot-img")
        UBOOT=true
	shift; UBOOT_IMAGE="$1"
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
    qemu_supports_device $NIC_MODEL
    if [ $? = 1 ] ; then
        echo "$NIC_MODEL not supported. Fall back to e1000"
        NIC_MODEL=e1000 ;
    fi

    # Two NIC qemu conf
    #QEMU_CMD="${QEMU_PATH}qemu-system-x86_64 \
    #    -machine type=q35 \
    #    -smp $SMP \
    #    -enable-kvm \
    #    -m 1024 \
    #    -netdev user,id=network0 \
    #    -netdev user,id=network1 \
    #    -device $NIC_MODEL,netdev=network0
    #    -device $NIC_MODEL,netdev=network1
    #    -device ahci,id=ahci \
    #    -device ide-drive,drive=disk,bus=ahci.0 \
    #    -drive id=disk,file="$HDFILE",if=none"

    QEMU_CMD="${QEMU_PATH}qemu-system-x86_64 \
        -machine type=q35 \
        -smp ${SMP} \
        -m ${MEMORY} \
        ${KVM} \
        -netdev user,id=network0 \
        -device $NIC_MODEL,netdev=network0 \
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
        -m 2G \
        -smp $SMP \
        -machine vexpress-a15"
    GDB=gdb
    QEMU_NONDEBUG=-nographic
    ;;
    "armv8")
       QEMU_CMD="${QEMU_PATH}qemu-system-aarch64 \
                 -m ${MEMORY} \
                 -cpu cortex-a57 \
                 -M virt -d guest_errors \
                 -M gic_version=3 \
                 -smp ${SMP}"
       if $UBOOT; then
          QEMU_CMD="$QEMU_CMD -bios $UBOOT_IMAGE \
                    -device loader,addr=0x50000000,file=$IMAGE"
       else
           QEMU_CMD="$QEMU_CMD -bios $BIOS \
                    -device virtio-blk-device,drive=image \
                    -drive if=none,id=image,file=$IMAGE,format=raw"
       fi
       GDB=gdb-multiarch
       QEMU_NONDEBUG=-nographic
       EFI=1
       ;;
    "zynq7")
        QEMU_CMD="${QEMU_PATH}qemu-system-arm \
        -machine xilinx-zynq-a9 \
        -m 2G \
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
