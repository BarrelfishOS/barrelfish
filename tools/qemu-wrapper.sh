#!/bin/bash
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
MENUFILE=""
ARCH=""
DEBUG_SCRIPT=""
SMP=2

usage () {
    echo "Usage: $0 --menu <file> --arch <arch>  [options]"
    echo "  where:"
    echo "    'arch' is one of: x86_64, x86_32, armv5"
    echo "    'file' is a menu.lst format file to read module list from"
    echo "  and options can be:"
    echo "    --debug <script>  (run under the specified GDB script)"
    echo "    --hdfile <file>   (hard disk image to be build for AHCI, defaults to $HDFILE"
    echo "    --kernel <file>   (kernel binary, if no menu.lst given)"
    echo "    --initrd <file>   (initial RAM disk, if no menu.lst given)"
    echo "    --args <args>     (kernel command-line args, if no menu.lst given)"
    echo "    --smp <cores>     (number of cores to use, defaults to $SMP)"
    exit 1
}


if [ $# == 0 ]; then usage ; fi
while [ $# != 0 ]; do
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
	"--args")
	    shift; KERNEL_CMDS="$1"
	    ;;
	"--smp")
	    shift; SMP="$1"
	    ;;
	*)
	    echo "Unknown option $1 (try: --help)" >&2
	    exit 1
	    ;;
    esac
    shift
done

if [ -z "$MENUFILE" ]; then
    echo "No menu.lst file specified." 
    if [ -z "$KERNEL" ]; then 
	echo "ERROR: No initial kernel given and no menu.lst file." >&2; exit 1
    fi
    if [ -z "$INITRD" ]; then 
	echo "ERROR: No initial RAM disk given and no menu.lst file." >&2; exit 1
    fi
else
    echo "Using menu file $MENUFILE"
    ROOT=`sed -rne 's,^root[ \t]*([^ ]*).*,\1,p' "$MENUFILE"`
    if [ "$ROOT" != "(nd)" ]; then
        echo "Root: $ROOT"
    fi
    KERNEL=`sed -rne 's,^kernel[ \t]*/([^ ]*).*,\1,p' "$MENUFILE"`
    if [ "$ROOT" != "(nd)" ]; then
        KERNEL="$ROOT/$KERNEL"
    fi
    if [ -z "$KERNEL" ]; then
	echo "ERROR: No initial kernel specified in menu.lst file." >&2; exit 1
    fi
    KERNEL_CMDS=`sed -rne 's,^kernel[ \t]*[^ ]*[ \t]*(.*),\1,p' "$MENUFILE"`
    if [ "$ROOT" != "(nd)" ]; then
        AWKSCRIPT='{ if (NR == 1) printf(root "/" $$0); else printf("," root "/" $$0) }'
        AWKARGS="-v root=$ROOT"
    else
        AWKSCRIPT='{ if (NR == 1) printf($$0); else printf("," $$0) }'
    fi
    INITRD=`sed -rne 's,^module(nounzip)?[ \t]*/(.*),\2,p' "$MENUFILE" | awk $AWKARGS "$AWKSCRIPT"`
    if [ -z "$INITRD" ]; then
	echo "ERROR: No initial ram disk modules specified in menu.lst file." >&2; exit 1
    fi
fi

echo "Initial kernel file: $KERNEL"
echo "Initial RAM disk contents: $INITRD"
echo "Kernel command line arguments: $KERNEL_CMDS"
echo "Requested architecture is $ARCH."

case "$ARCH" in
    "x86_64")
	QEMU_CMD="qemu-system-x86_64 \
	    -smp $SMP \
	    -m 1024 \
	    -net nic,model=e1000 \
	    -net user \
	    -device ahci,id=ahci \
	    -device ide-drive,drive=disk,bus=ahci.0 \
	    -drive id=disk,file="$HDFILE",if=none"
	QEMU_NONDEBUG=-nographic
	GDB=gdb
	echo "Creating hard disk image $HDFILE"
	qemu-img create "$HDFILE" 10M
	;;
    "x86_32")
        QEMU_CMD="qemu-system-i386 \
	    -no-kvm \
	    -smp 2 \
	    -m 1024 \
	    -net nic,model=ne2k_pci \
	    -net user \
	    -device ahci,id=ahci \
	    -device ide-drive,drive=disk,bus=ahci.0 \
	    -drive id=disk,file="$HDFILE",if=none"
	GDB=gdb
	QEMU_NONDEBUG=-nographic
	echo "Creating hard disk image $HDFILE"
	qemu-img create "$HDFILE" 10M
	;;
    "armv5")
	QEMU_CMD="qemu-system-arm \
	    -machine integratorcp \
	    -kernel armv5/sbin/cpu.bin \
	    -nographic \
	    -no-reboot \
	    -m 256 \
	    -initrd armv5/romfs.cpio"
	GDB=arm-linux-gnueabi-gdb
	;;
    "arm11mp")
	QEMU_CMD="qemu-system-arm \
	    -cpu mpcore \
	    -machine realview-eb-mpcore \
	    -kernel arm11mp/sbin/cpu.bin"
	GDB=arm-linux-gnueabi-gdb
	;;
    *)
	echo "No Qemu environment defined for architecture=$ARCH." >&2
	exit 1
	;;
esac

if [ "$DEBUG_SCRIPT" = "" ] ; then
    echo "OK: about to run the follow qemu command:"
    echo "$QEMU_CMD $QEMU_NONDEBUG -kernel $KERNEL -append $KERNEL_CMDS -initrd $INITRD"
    exec $QEMU_CMD $QEMU_NONDEBUG -kernel "$KERNEL" -append "$KERNEL_CMDS" -initrd "$INITRD"
fi


# Now we run the debugger instead
GDB_ARGS="-x $DEBUG_SCRIPT"
SERIAL_OUTPUT=file:/dev/stdout
PORT=$((10000 + UID))

if [ "${SERIAL_OUTPUT}" = "" ] ; then
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
eval $QEMU_INVOCATION

if [ $? -eq 0 ] ; then 
    stty sane
    trap '' SIGINT
    ${GDB} -x barrelfish_debug.gdb ${GDB_ARGS}
    PID=`cat ${PIDFILE}`
    kill ${PID} > /dev/null || true
    rm -f $PIDFILE
else
    echo Failed to launch qemu with:
    echo "   ${QEMU_INVOCATION}"
fi
