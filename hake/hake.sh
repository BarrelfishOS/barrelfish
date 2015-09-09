#!/bin/bash

##########################################################################
# Copyright (c) 2009, 2011, 2013, 2015, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitätstasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

DFLTARCHS="\"x86_64\""
RUN_HAKE="Yes"
HAKEDIR=$(dirname $0)
DEFAULT_JOBS=4
JOBS="$DEFAULT_JOBS"

# Don't override the default toolchain unless asked to.
ARM_TOOLSPEC=Nothing
THUMB_TOOLSPEC=Nothing
ARMEB_TOOLSPEC=Nothing
X86_TOOLSPEC=Nothing
K1OM_TOOLSPEC=Nothing

usage() { 
    echo "Usage: $0 <options>"
    echo "   -s|--source-dir: path to source tree (required)"
    echo "   -i|--install-dir: path to install directory (defaults to \`pwd\`)"
    echo "   -a|--architecture: specify archtitecture to build for (can be"
    echo "       given multiple times, default architectures are"
    echo "       $DFLTARCHS"
    echo "   -n|--no-hake: just rebuild hake itself, don't run it (only useful"
    echo "       for debugging hake)"
    echo "   -t|--toolchain <arch> <toolchain>: use <toolchain> to build for"
    echo "       <arch>."
    echo "   -j|--jobs: Number of parallel jobs to run (default $DEFAULT_JOBS)."
    echo ""
    echo "  The way you use this script is to create a new directory for your"
    echo "  build tree, cd into it, and run this script with the --source-dir"
    echo "  argument specifying the top of the source tree."
    echo ""
    echo "  Known architectures may include: "
    echo "     x86_64 x86_32 armv5 xscale armv7 armv7-m k10m"
    exit 1;
}

#
# Legacy compatibility to avoid breaking the harness...
#
if [ $# -eq 1 ]; then
    echo "WARNING: old usage of hake.sh (sole argument gives the source directory) is"
    echo "deprecated: please use --source-dir instead."
    SRCDIR="$1"
    shift
fi

#
# Parse args
#
while [ $# -ne 0 ]; do
    case $1 in
	"-a"|"--architecture") 
	    if [ -z "$NEWARCHS" ] ; then
		    NEWARCHS="\"$2\""
	    else
		    NEWARCHS="$NEWARCHS, \"$2\""
	    fi
        shift 
	    ;;
	"-i"|"--install-dir")
	    INSTALLDIR="$2"
        shift 
	    ;;
	"-s"|"--source-dir")
	    SRCDIR="$2"
        shift 
	    ;;
	"-n"|"--no-hake")
	    RUN_HAKE="No"
	    ;;
    "-t"|"--toolchain")
        TC_ARCH="$2"
        shift
        TOOLSPEC="$2"
        shift
        case ${TC_ARCH} in
        "arm")
            ARM_TOOLSPEC="Just Tools.$TOOLSPEC"
            ;;
        "thumb")
            THUMB_TOOLSPEC="Just Tools.$TOOLSPEC"
            ;;
        "armeb")
            ARMEB_TOOLSPEC="Just Tools.$TOOLSPEC"
            ;;
        "x86")
            X86_TOOLSPEC="Just Tools.$TOOLSPEC"
            ;;
        "k1om")
            K1OM_TOOLSPEC="Just Tools.$TOOLSPEC"
            ;;
	    *) 
            echo "Unknown toolchain architecture: $TC_ARCH"
            exit 1
            ;;
        esac
        ;;
	"-j"|"--jobs")
	    JOBS="$2"
        shift 
        ;;
	*) 
	    usage
	    ;;
    esac
    shift
done

if [ -z "$INSTALLDIR" ] ; then
    echo "Install directory defaulting to '.'"
    INSTALLDIR="."
else
    echo "Install directory is $INSTALLDIR"
fi
cd $INSTALLDIR

if [ -z "$SRCDIR" ] ; then
    usage
fi

if [ ! -f "$SRCDIR"/hake/Main.hs ] ; then
    echo "Can't find Hake in the source directory $SRCDIR."
    echo "Did you specify the source directory correctly?"
    usage
fi
echo "Source directory is $SRCDIR"

if [ ! -z "$NEWARCHS" ]; then
    ARCHS="$NEWARCHS"
else 
    ARCHS="$DFLTARCHS"
fi
echo "Architectures to build: $ARCHS"

if [ ! -d hake ] ; then
    echo "Creating a local hake directory..."
    mkdir -p hake
    touch hake/.marker
fi

echo "Setting up hake build directory..."
if [ ! -f hake/Config.hs ]; then
    echo "Bootstrapping Config.hs"
    cp $SRCDIR/hake/Config.hs.template hake/Config.hs
    cat >> hake/Config.hs <<EOF

-- Automatically added by hake.sh. Do NOT copy these definitions to the defaults
source_dir = "$SRCDIR"
architectures = [ $ARCHS ]
install_dir = "$INSTALLDIR"
arm_toolspec   = $ARM_TOOLSPEC
thumb_toolspec = $THUMB_TOOLSPEC
armeb_toolspec = $ARMEB_TOOLSPEC
x86_toolspec   = $X86_TOOLSPEC
k1om_toolspec  = $K1OM_TOOLSPEC
EOF
else
    echo "You already have Config.hs, leaving it as-is."
fi

if [ ! -f ./symbolic_targets.mk ]; then
    echo "Creating new symbolic_targets.mk file."
    cp "$SRCDIR/hake/symbolic_targets.mk" . 
else
    echo "You already have symbolic_targets.mk, leaving it as-is."
fi

# FIXME: do we really need this; doesn't ghc get the dependencies right? -AB
#rm -f hake/*.hi hake/*.o 

# RTS parameters for hake.  Tuned to perform well for the mid-2015 Barrelfish
# tree, building over NFS.  Maximum heap size sits around 64MB, or a little
# more, so 128MB minimises collections.  Parallelism is a balancing act
# between performance in the tree walk on NFS systems (which benefits heavily
# from parallelising the IO operations), and performance in Hakefile
# evaluation, which generally gets *slower* with more threads, as the GHC
# garbage collector ends up thrashing a lot.
HAKE_RTSOPTS="-H128M -A4M -N4"

echo "Building hake..."
ghc -O --make \
    -XDeriveDataTypeable \
    -XStandaloneDeriving \
    -XScopedTypeVariables \
    -package ghc \
    -package ghc-mtl \
    -package ghc-paths \
    -package bytestring-trie \
    -o hake/hake \
    -outputdir hake \
    -i$SRCDIR/hake \
    -ihake \
    -rtsopts=all \
    -with-rtsopts="$HAKE_RTSOPTS" \
    -threaded \
    $SRCDIR/hake/Main.hs $LDFLAGS || exit 1

    # -eventlog \

if [ "$RUN_HAKE" == "No" ] ; then
    echo "Not running hake as per your request."
    exit
fi

echo "Running hake..."
./hake/hake --output-filename Makefile --source-dir "$SRCDIR/" || exit

echo "Now running initial make to build dependencies."
echo "Running $JOBS jobs at once (-j N to change this)."
make -j "$JOBS" help
