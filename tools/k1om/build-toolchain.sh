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
BINUTILS=binutils-2.24
GCC=gcc-4.8.2


# Cross compiler target.
TARGET=x86_64-k1om-barrelfish

# Directory this shell script is stored in.
# http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Paths to patches.
BINUTILS_PATCH="${SCRIPT_DIR}/${BINUTILS}-barrelfish.patch"
GCC_PATCH="${SCRIPT_DIR}/${GCC}-barrelfish.patch"


# 1. binutils - GNU Binary Utilities
#curl -L -O "http://ftp.gnu.org/gnu/binutils/${BINUTILS}.tar.gz"
tar xzvf ${BINUTILS}.tar.gz
pushd ${BINUTILS}/
patch -p1 < "${BINUTILS_PATCH}"
popd  # ${BINUTILS}/

# 2. GCC - GNU Compiler Collection
#curl -L -O "ftp://ftp.fu-berlin.de/unix/languages/gcc/releases/${GCC}/${GCC}.tar.bz2"
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


