#
# This makefile builds the necessary files for a binary release
#
# The environment variable ARCH must be set to indicate the machine
# architecture
#

CP = /bin/cp
RM = /bin/rm -f
CHMOD = /bin/chmod
INCLUDE_DIR = $(USC_DIR)/include
LIB_DIR = $(USC_DIR)/lib/$(ARCH)
#INCLUDE_DIR = ../$(USC_DIR)/include
#LIB_DIR = ../../$(USC_DIR)/lib/$(ARCH)


install: global_install

local_install:
	cd $(ARCH) && ./configure && $(MAKE) clean
	cd $(ARCH) && $(MAKE) install

global_install: local_install
	-(cd  ../sys_include && $(RM) `/bin/ls $(INCLUDE_DIR)`)
	$(CP) include/* ../sys_include
	cd ../sys_include && $(CHMOD) 664 `/bin/ls $(INCLUDE_DIR)`
	-(cd ../sys_lib/$(ARCH) && $(RM) `/bin/ls $(LIB_DIR)`)
	$(CP) lib/$(ARCH)/* ../sys_lib/$(ARCH)
	cd ../sys_lib/$(ARCH) && $(CHMOD) 664 `/bin/ls $(LIB_DIR)`

