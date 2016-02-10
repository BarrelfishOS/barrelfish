# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  The ECLiPSe Constraint Logic Programming System. 
# The Initial Developer of the Original Code is  Cisco Systems, Inc. 
# Portions created by the Initial Developer are
# Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): ___________________________________. 
# 
# END LICENSE BLOCK
#
# This makefile builds the necessary files for a binary release
#
# The environment variable ARCH must be set to indicate the machine
# architecture
#

CP = /bin/cp
RM = /bin/rm -f
CHMOD = /bin/chmod
INCLUDE_DIR = $(PDS_DIR)/include
LIB_DIR = $(PDS_DIR)/lib/$(ARCH)
BIN_DIR = $(PDS_DIR)/bin/$(ARCH)


install: global_install

local_install:
	cd $(ARCH) && ./configure && $(MAKE) clean
	cd $(ARCH) && $(MAKE) install_all

global_install: global_install_lib global_install_include

global_install_lib: local_install
	-(cd ../sys_lib/$(ARCH) && $(RM) `ls $(LIB_DIR)`)
	$(CP) lib/$(ARCH)/* ../sys_lib/$(ARCH)
	cd ../sys_lib/$(ARCH) && $(CHMOD) 664 `ls $(LIB_DIR)`

global_install_include: local_install
	-(cd ../sys_include && $(RM) `ls $(INCLUDE_DIR)`)
	$(CP) include/* ../sys_include
	cd ../sys_include && $(CHMOD) 664 `ls $(INCLUDE_DIR)`

global_install_bin: local_install
	-(cd ../sys_bin/$(ARCH) && $(RM) `ls $(BIN_DIR)`)
	$(CP) bin/$(ARCH)/* ../sys_bin/$(ARCH)
	cd ../sys_bin/$(ARCH) && $(CHMOD) 664 `ls $(BIN_DIR)`
	-(cd ../boot/$(ARCH) && $(RM) `ls $(BIN_DIR)`)
	$(CP) bin/$(ARCH)/* ../boot/$(ARCH)
	cd ../boot/$(ARCH) && $(CHMOD) 664 `ls $(BIN_DIR)`

