# Options which may be overriden for platforms, etc.
#
# This list of such variables should be kept up to date with AvailableOptions in
# 'make/lib_info.mk'.

# The compiler to use.
CC := gcc

# The compiler flags to use.
CFLAGS := -Wall -Werror

# The list of functions to include in the library.
FUNCTIONS :=

# Whether optimized function implementations should be used.
OPTIMIZED := 1

# Whether function definitions should use hidden visibility. This adds the
# -fvisibility=hidden compiler option and uses .private_extern annotations in
# assembly files.
#
# FIXME: Make this more portable. When that is done, it should probably be the
# default.
VISIBILITY_HIDDEN := 0

# Whether the library is being built for kernel use.
KERNEL_USE := 0

# Whether the library should be built as a shared object.
SHARED_LIBRARY := 0

# Miscellaneous tools.

AR := ar
# FIXME: Remove these pipes once ranlib errors are fixed.
ARFLAGS := cru 2> /dev/null

LDFLAGS :=

RANLIB := ranlib
# FIXME: Remove these pipes once ranlib errors are fixed.
RANLIBFLAGS := 2> /dev/null

STRIP := strip
LIPO := lipo
DSYMUTIL := dsymutil

SHARED_LIBRARY_SUFFIX := so
