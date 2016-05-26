#
# This makefile builds the necessary files for a binary release
#
# The environment variable ARCH must be set to indicate the machine
# architecture
#

CP = /bin/cp
RM = /bin/rm -f
CHMOD = /bin/chmod
INCLUDE_DIR = $(ALOG_DIR)/include
LIB_DIR = $(ALOG_DIR)/lib/$(ARCH)
BIN_DIR = $(ALOG_DIR)/bin/$(ARCH)


install: global_install

local_install:
	cd $(ARCH) && ./configure && $(MAKE) clean
	cd $(ARCH) && $(MAKE) install

global_install: global_install_lib global_install_include

global_install_lib: local_install
	-(cd ../sys_lib/$(ARCH) && $(RM) `/bin/ls $(LIB_DIR)`)
	$(CP) lib/$(ARCH)/* ../sys_lib/$(ARCH)
	cd ../sys_lib/$(ARCH) && $(CHMOD) 664 `/bin/ls $(LIB_DIR)`

global_install_include: local_install
	-(cd ../sys_include && $(RM) `/bin/ls $(INCLUDE_DIR)`)
	$(CP) include/* ../sys_include
	cd ../sys_include && $(CHMOD) 664 `/bin/ls $(INCLUDE_DIR)`

global_install_bin: local_install
	-(cd ../sys_bin/$(ARCH) && $(RM) `/bin/ls $(BIN_DIR)`)
	$(CP) bin/$(ARCH)/* ../sys_bin/$(ARCH)
	cd ../sys_bin/$(ARCH) && $(CHMOD) 664 `/bin/ls $(BIN_DIR)`

