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

#-------------------------------------------------------------------------------

# Modify these versions to match the corresponding patch.
BINUTILS=binutils-2.22+mpss3.2
GCC=gcc-4.7.0+mpss3.2

# Path of your Barrelfish source and build tree.
BARRELFISH_SOURCE=/home/acreto/barrelfish.xeon-phi
BARRELFISH_BUILD=${BARRELFISH_SOURCE}/build

# Where the toolchain will be built and installed.
# Note: the toolchain is specific to the Barrelfish tree mentioned above.
TOOLCHAIN_PREFIX=${BARRELFISH_SOURCE}/toolchain

# Cross compiler target.
#TARGET=x86_64-pc-barrelfish
TARGET=k1om-pc-barrelfish
#TARGET=i586-pc-barrelfish
#TARGET=i586-scc-barrelfish

# Directory this shell script is stored in.
# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Paths to patches.
BINUTILS_PATCH="${SCRIPT_DIR}/${BINUTILS}-barrelfish.patch"
GCC_PATCH="${SCRIPT_DIR}/${GCC}-barrelfish.patch"

# Build parallelism
MAKE_JOBS=
if [[ -z "${MAKE_JOBS}" ]]; then
  # Guess a sensible value - default: #cores + 2.
  MAKE_JOBS=$(($(grep "^core id" /proc/cpuinfo | sort -u | wc -l) + 2))
fi

#-------------------------------------------------------------------------------

TOOLCHAIN_BUILD="$(mktemp -d --tmpdir barrelfish-toolchain-build.XXXXXXXXXX)"

# Build the toolchain.
export PATH=${PATH}:${TOOLCHAIN_PREFIX}/bin

pushd "${TOOLCHAIN_BUILD}"

# 2. GCC - GNU Compiler Collection
cp "${SCRIPT_DIR}/${GCC}.tar.bz2" "${TOOLCHAIN_BUILD}/${GCC}.tar.bz2"
tar xjvf "${TOOLCHAIN_BUILD}/${GCC}.tar.bz2"
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
CC=gcc-4.7 ../${GCC}/configure \
    --prefix="${TOOLCHAIN_PREFIX}" \
    --target="${TARGET}" \
    --enable-languages=c \
    --enable-initfini-array \
    --disable-nls \
    --disable-multilib \
    --disable-libssp \
    --with-newlib \
   # --with-cpu-64=k1om \
   # --with-arch-64=k1om \
   # --with-tune-64=k1om \
    MAKEINFO=missing
make MAKEINFO=true -j$MAKE_JOBS
make MAKEINFO=true install-strip
popd  # ${GCC}-build/

popd  # ${TOOLCHAIN_BUILD}

rm -rf "${TOOLCHAIN_BUILD}"
rm -rf "${TOOLCHAIN_BUILD}"
