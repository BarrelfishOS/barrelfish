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
# Shell script for constructing a multiboot image for Pandaboard
#
##########################################################################

SRC_DIR=
BUILD_DIR=
ARM_GCC=$(which arm-linux-gnueabi-gcc)
ARM_OBJCOPY=$(which arm-linux-gnueabi-objcopy)
MENU_LST=""
DEFINES=""
EXTRAS=""
BASE_ADDR=""
ARCH=armv7-a
IMAGE="pandaboard_image"
EXTRACFLAGS=""

usage () { 
    echo "Usage: $0 --srcdir <dir> --builddir <dir> --menu <menu.lst> [option].."
    echo " where options are:"
    echo "   --arch <ARM architecture>       (default: $ARCH)"
    echo "   --gcc <gcc binary>              (default: $ARM_GCC)"
    echo "   --objcopy <objcopy binary>      (default: $ARM_OBJCOPY)"
    echo "   -D <define>"
    echo "   --extra <additional file>"
    echo "   --image <output image file>     (default: $IMAGE)"
    echo "   --cflags <flags to pass to cc>  (default: -march=$ARCH $EXTRACFLAGS)"
    exit 0
}

if [ $# == 0 ]; then usage ; fi

while [ $# != 0 ]; do
    case $1 in
	"--help")
	    usage
	    exit 0
	    ;;
	"--srcdir")
	    shift; SRC_DIR="$1"
	    ;;
	"--builddir")
	    shift; BUILD_DIR="$1"
	    ;;
	"--baseaddr")
	    shift; BASE_ADDR="$1"
	    ;;
	"--arch")
	    shift; ARCH="$1"
	    ;;
	"--gcc")
	    shift; ARM_GCC="$1"
	    ;;
	"--objcopy")
	    shift; ARM_OBJCOPY="$1"
	    ;;
	"--menu")
	    shift; MENU_LST="$1"
	    ;;
	"-D")
	    shift; DEFINES="$DEFINES -D$1"
	    ;;
	"--extra")
	    shift; EXTRAS="$EXTRAS $1"
	    ;;
	"--image")
	    shift; IMAGE="$1"
	    ;;
	"--cflags")
	    shift; EXTRACFLAGS="$1"
	    ;;
	*)
	    echo "Unknown option $1 (try: --help)" >&2
	    exit 1
	    ;;
    esac
    shift
done

CFLAGS="-march=$ARCH $EXTRACFLAGS"
BASEARCH=$(echo $ARCH | sed s/-.*//)
if [ "$BASEARCH" == "armv8" ]
then
    BFDNAME="elf64-littleaarch64"
    BFDARCH="aarch64"
    BW=64
    KARCH="aarch64"
else
    BFDNAME="elf32-littlearm"
    BFDARCH="arm"
    BW=32
    KARCH="arm"
fi

if [ -z "$SRC_DIR" ] ; then
    echo "No source directory defined." >&2; exit 1
elif [ -z "$BUILD_DIR" ] ; then
    echo "No build directory defined." >&2; exit 1
elif [ -z "$ARM_GCC" ]; then
    echo "No ARM GCC compiler defined." >&2; exit 1
elif [ -z "$ARM_OBJCOPY" ]; then
    echo "No ARM objcopy utility." >&2; exit 1
elif [ -z "$MENU_LST" ]; then
    echo "No boot menu list defined." >&2; exit 1
elif [ -z "$BASE_ADDR" ]; then
    echo "No kernel base address defined." >&2; exit 1
fi

TMP_DIR="$BUILD_DIR/molly_tmp_"`basename $IMAGE`

# Dependencies:
#  - tools/bin/arm_molly must be built
#  - all modules mentioned in $MENU_LST must exist in $ARCH/sbin/
#  - the menu.list file
#
# Outputs: 
#  - $IMAGE : the image to be built

TMP_LDSCRIPT="$TMP_DIR/molly_ld_script"
TMP_MBIC="$TMP_DIR/panda_mbi.c"

# Prefix prepended to each output file within the directory
# $OUTPUT_PREFIX (for safety, this means we can clean the directory
# by removing everything with this prefix)
TMP_PREFIX=tmp_molly

echo "Cleaning temporary directory $TMP_DIR"
rm -rf "$TMP_DIR"
mkdir -p "$TMP_DIR"

echo "Generating the list of of binaries to translate"
BINS=$(awk '/^kernel/ || /^module/ {print $2}' $MENU_LST | uniq)
# For each binary generate an object file in the output directory.
# The flags to objcopy cause it to place the binary image of the input
# file into an .rodataIDX section in the generated object file where
# IDX is a counter incremented for each binary.  
echo "Translating data files ($BFDNAME,$BFDARCH)"
IDX=1
for BIN in $BINS; do
  UNDERSCORED=${BIN//-/_}
  SLASH=${UNDERSCORED////_}
  BIN_OUT="$TMP_DIR/${TMP_PREFIX}_$SLASH"
  echo ' ' $BIN '->' $BIN_OUT
  $ARM_OBJCOPY -I binary -O $BFDNAME -B $BFDARCH \
    --rename-section .data=.rodata$IDX,alloc,load,readonly,data,contents \
    .$BIN $BIN_OUT
  IDX=$(($IDX+1))
  if [ $IDX = 20 ]; then
      echo Error: linker script cannot handle $IDX modules
      exit 1
  fi
done

echo "Creating appropriate linker script"
cpp -P -DBASE_ADDR=$BASE_ADDR \
       "$SRC_DIR/tools/arm_molly/molly_ld_script${BW}.in" \
       "$TMP_LDSCRIPT"

echo "Building a C file to link into a single image for the 2nd-stage bootloader"
"$BUILD_DIR/tools/bin/arm_molly" "$MENU_LST" "$TMP_MBIC" "-$BW"

echo "Compiling the complete boot image into a single executable"
$ARM_GCC -std=c99 -g -fPIC -pie -Wl,-N -fno-builtin \
	-nostdlib $CFLAGS -fno-unwind-tables \
	-T$TMP_LDSCRIPT \
	-I$SRC_DIR/include \
	-I$SRC_DIR/include/arch/$KARCH \
	-I./$BASEARCH/include \
	-I$SRC_DIR/include/oldc \
	-I$SRC_DIR/include/c \
	$SRC_DIR/tools/arm_molly/molly_boot${BW}.S \
	$SRC_DIR/tools/arm_molly/molly_init${BW}.c \
	$SRC_DIR/tools/arm_molly/lib.c \
	$TMP_MBIC \
	$SRC_DIR/lib/elf/elf${BW}.c \
	$TMP_DIR/${TMP_PREFIX}* \
    $EXTRAS \
	-o $IMAGE

echo "OK - boot image $IMAGE is built."
echo "If this is a pandaboard image, you can now:"
echo "usbboot $IMAGE"
