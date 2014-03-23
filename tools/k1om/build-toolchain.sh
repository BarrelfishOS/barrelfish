#!/usr/bin/env bash

# C/C++ toolchain build script for Barrelfish.
# http://redmine.aluzina.org/projects/barrelfish/wiki/Toolchain_from_sources
# http://wiki.barrelfish.org/CrossCompiler
#
# In order to build a toolchain you will need to install the following packages:
#   $ sudo apt-get install gcc g++ make patch bison flex texinfo
#
# These are also required but will be downloaded automatically during the build:
#   libgmp-dev, libmpc-dev, libmpfr-dev
#
# Optional (for Graphite optimizations): libcloog-ppl-dev.

set -e  # Die if any command fails.
set -x  # Trace each command before execution.

BASEDIR=$(pwd)
BASEDIR=~/eth/barrelfish


export TARGET=x86_64-k1om-barrelfish

# Path of your Barrelfish source and build tree.
BARRELFISH_SOURCE=$BASEDIR/barrelfish
BARRELFISH_BUILD=$BASEDIR/build

# Modify these versions to match the corresponding patch.
BINUTILS=binutils-2.24
GCC=gcc-4.8.2


# Directory this shell script is stored in.
# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Paths to patches.
BINUTILS_PATCH="${SCRIPT_DIR}/${BINUTILS}-barrelfish.patch"
GCC_PATCH="${SCRIPT_DIR}/${GCC}-barrelfish.patch"

#-------------------------------------------------------------------------------

# Where the toolchain will be built and installed.
# Note: the toolchain is specific to the Barrelfish tree mentioned above.
TOOLCHAIN_PREFIX=${BARRELFISH_SOURCE}/toolchain



#-------------------------------------------------------------------------------

# Sanity checks.
exit_with_error() { echo "error: $1" && exit 1; }
[[ ! -d "${BARRELFISH_SOURCE}" ]] && \
    exit_with_error "Barrelfish source not found (${BARRELFISH_SOURCE})."
[[ ! -d "${BARRELFISH_BUILD}" ]] && \
    exit_with_error "Barrelfish tree not found (${BARRELFISH_BUILD})."
[[   -d "${TOOLCHAIN_PREFIX}" ]] && \
    exit_with_error "toolchain already built in ${TOOLCHAIN_PREFIX}."
[[ ! -f "${BINUTILS_PATCH}" ]] && \
    exit_with_error "binutils patch not found (${BINUTILS_PATCH})."
[[ ! -f "${GCC_PATCH}" ]] && \
    exit_with_error "GCC patch not found (${GCC_PATCH})."


# Build the toolchain.
export PATH=${PATH}:${TOOLCHAIN_PREFIX}/bin
export PREFIX=${TOOLCHAIN_PREFIX}


TOOLCHAIN_BUILD="$(mktemp -d --tmpdir barrelfish-toolchain-build.XXXXXXXXXX)"


# Build parallelism
MAKE_JOBS=
if [[ -z "${MAKE_JOBS}" ]]; then
  # Guess a sensible value - default: #cores + 2.
  MAKE_JOBS=$(($(grep "^core id" /proc/cpuinfo | sort -u | wc -l) + 2))
fi


pushd "${TOOLCHAIN_BUILD}"

# 1. binutils - GNU Binary Utilities
curl -L -O "http://ftp.gnu.org/gnu/binutils/${BINUTILS}.tar.gz"
tar xzvf ${BINUTILS}.tar.gz
pushd ${BINUTILS}/
patch -p1 < "${BINUTILS_PATCH}"
popd  # ${BINUTILS}/
mkdir -p ${BINUTILS}-build/
pushd ${BINUTILS}-build/
../${BINUTILS}/configure \
    --prefix="${TOOLCHAIN_PREFIX}" \
    --target="${TARGET}" \
    --enable-gold=default \
    --enable-threads \
    --enable-lto \
    --enable-plugins \
    --disable-nls
make -j${MAKE_JOBS}
make install-strip
popd  # ${BINUTILS}-build/


# 2. GCC - GNU Compiler Collection
curl -L -O "ftp://ftp.fu-berlin.de/unix/languages/gcc/releases/${GCC}/${GCC}.tar.bz2"
tar xjvf ${GCC}.tar.bz2
pushd ${GCC}/
source ./contrib/download_prerequisites
# http://stackoverflow.com/questions/407523/escape-a-string-for-sed-search-pattern
BF_SOURCE_ESCAPED=$(echo "${BARRELFISH_SOURCE}" | sed -e 's/[\/&]/\\&/g')
BF_BUILD_ESCAPED=$(echo "${BARRELFISH_BUILD}" | sed -e 's/[\/&]/\\&/g')
sed -r -e "s/\{\{BF_SRC\}\}/${BF_SOURCE_ESCAPED}/g" \
       -e "s/\{\{BF_BUILD\}\}/${BF_BUILD_ESCAPED}/g" \
       "${GCC_PATCH}" | patch -p1
popd  # ${GCC}/

mkdir -p ${GCC}-build/
pushd ${GCC}-build/
../${GCC}/configure \
    --prefix="${TOOLCHAIN_PREFIX}" \
    --target="${TARGET}" \
    --enable-languages=c,c++ \
    --enable-initfini-array \
    --disable-nls \
    --disable-libssp \
    --with-newlib \
    --disable-multilib # from xeon phi inside cluster
make -j$MAKE_JOBS
make install-strip

popd  # ${GCC}-build/

popd  # ${TOOLCHAIN_BUILD}

rm -rf "${TOOLCHAIN_BUILD}"
