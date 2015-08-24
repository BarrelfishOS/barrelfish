#!/bin/bash

##########################################################################
# Copyright (c) 2009, 2011, 2013, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitätstasse 6, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

DFLTARCHS="\"x86_64\""
RUN_HAKE="Yes"
TOOLCHAIN="ubuntu"
HAKEDIR=$(dirname $0)

usage() { 
    echo "Usage: $0 <options>"
    echo "   -s|--source-dir: path to source tree (required)"
    echo "   -i|--install-dir: path to install directory (defaults to \`pwd\`)"
    echo "   -a|--architecture: specify archtitecture to build for (can be"
    echo "       given multiple times, default architectures are"
    echo "       $DFLTARCHS"
    echo "   -n|--no-hake: just rebuild hake itself, don't run it (only useful"
    echo "       for debugging hake)"
    echo "   -t|--toolchain: toolchain to use when bootstrapping a new"
    echo "       build tree (-t list for available options)."
    echo ""
    echo "  The way you use this script is to create a new directory for your"
    echo "  build tree, cd into it, and run this script with the --source-dir"
    echo "  argument specifying the top of the source tree."
    echo ""
    echo "  Known architectures may include: "
    echo "     x86_64 x86_32 armv5 arm11mp xscale armv7 armv7-m k10m"
    exit 1;
}

toolchains() {
    TOOLCHAINS=$(ls ${HAKEDIR}/Config.hs.* | sed s/.*Config\.hs\.//)
    echo "Available toolchains:"
    for tc in ${TOOLCHAINS}; do
        echo "   $tc"
    done
    exit 1
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
        TOOLCHAIN="$2"
        shift
        if [ "${TOOLCHAIN}" == "list" ]; then
            toolchains
        fi
        if [ ! -f ${HAKEDIR}/Config.hs.${TOOLCHAIN} ]; then
            echo "Unknown toolchain \"${TOOLCHAIN}\""
            exit 1
        fi
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
    echo "Bootstrapping with toolchain \"${TOOLCHAIN}\""
    cp $SRCDIR/hake/Config.hs.${TOOLCHAIN} hake/Config.hs
    cat >> hake/Config.hs <<EOF

-- Automatically added by hake.sh. Do NOT copy these definitions to the defaults
source_dir = "$SRCDIR"
architectures = [ $ARCHS ]
install_dir = "$INSTALLDIR"
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
./hake/hake --output-filename Makefile --source-dir "$SRCDIR" || exit

echo "Now running initial make to build dependencies."
echo " (remove the '-j 4' if your system has trouble handling this" 
make -j 4 help
