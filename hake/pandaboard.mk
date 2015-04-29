##########################################################################
# Copyright (c) 2009-2015 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
##########################################################################

#######################################################################
#
# Pandaboard builds
#
#######################################################################

PANDABOARD_MODULES=\
	armv7/sbin/cpu_omap44xx \
	armv7/sbin/init \
	armv7/sbin/mem_serv \
	armv7/sbin/monitor \
	armv7/sbin/ramfsd \
	armv7/sbin/spawnd \
	armv7/sbin/startd \
	armv7/sbin/skb \
	armv7/sbin/memtest \
	armv7/sbin/kaluga \
	armv7/sbin/fish \
	armv7/sbin/sdma \
	armv7/sbin/sdmatest \
	armv7/sbin/sdma_bench \
	armv7/sbin/bulk_sdma \
	armv7/sbin/usb_manager \
	armv7/sbin/usb_keyboard \
	armv7/sbin/serial \
	armv7/sbin/angler \
	armv7/sbin/corectrl \


menu.lst.pandaboard: $(SRCDIR)/hake/menu.lst.pandaboard
	cp $< $@

pandaboard_image: $(PANDABOARD_MODULES) \
		tools/bin/arm_molly \
		menu.lst.pandaboard
	# Translate each of the binary files we need
	$(SRCDIR)/tools/arm_molly/build_data_files.sh menu.lst.pandaboard molly_panda
	# Generate appropriate linker script
	cpp -P -DBASE_ADDR=0x82001000 $(SRCDIR)/tools/arm_molly/molly_ld_script.in \
		molly_panda/molly_ld_script
	# Build a C file to link into a single image for the 2nd-stage
	# bootloader
	tools/bin/arm_molly menu.lst.pandaboard panda_mbi.c
	# Compile the complete boot image into a single executable
	$(ARM_GCC) -std=c99 -g -fPIC -pie -Wl,-N -fno-builtin \
		-nostdlib -march=armv7-a -mapcs -fno-unwind-tables \
		-Tmolly_panda/molly_ld_script \
		-I$(SRCDIR)/include \
		-I$(SRCDIR)/include/arch/arm \
		-I./armv7/include \
		-I$(SRCDIR)/include/oldc \
		-I$(SRCDIR)/include/c \
		-imacros $(SRCDIR)/include/deputy/nodeputy.h \
		$(SRCDIR)/tools/arm_molly/molly_boot.S \
		$(SRCDIR)/tools/arm_molly/molly_init.c \
		$(SRCDIR)/tools/arm_molly/lib.c \
		./panda_mbi.c \
		$(SRCDIR)/lib/elf/elf32.c \
		./molly_panda/* \
		-o pandaboard_image
	@echo "OK - pandaboard boot image is built."
	@echo "If your boot environment is correctly set up, you can now:"
	@echo "$ usbboot ./pandaboard_image"

#######################################################################
#
# Pandaboard build for the armv7-M slave image (to be used in conjunction with a master image)
# (basically a normal pandaboard_image, but compiled for the cortex-m3)
#
#######################################################################

HETEROPANDA_SLAVE_MODULES=\
	armv7-m/sbin/cpu_omap44xx \
	armv7-m/sbin/init \
	armv7-m/sbin/mem_serv \
	armv7-m/sbin/monitor \
	armv7-m/sbin/ramfsd \
	armv7-m/sbin/spawnd \
	armv7-m/sbin/startd \
	armv7-m/sbin/skb \
	armv7-m/sbin/memtest

menu.lst.armv7-m: $(SRCDIR)/hake/menu.lst.armv7-m
	cp $< $@

heteropanda_slave: $(HETEROPANDA_SLAVE_MODULES) \
		tools/bin/arm_molly \
		menu.lst.armv7-m
	# Translate each of the binary files we need
	$(SRCDIR)/tools/arm_molly/build_data_files.sh menu.lst.armv7-m molly_panda_slave
	# Generate appropriate linker script
	cpp -P -DBASE_ADDR=0x0 $(SRCDIR)/tools/arm_molly/molly_ld_script.in \
		molly_panda_slave/molly_ld_script
	# Build a C file to link into a single image for the 2nd-stage
	# bootloader
	tools/bin/arm_molly menu.lst.armv7-m panda_mbi_slave.c
	# Compile the complete boot image into a single executable
	$(ARM_GCC) -std=c99 -g -fPIC -pie -Wl,-N -fno-builtin \
		-nostdlib -march=armv7-m -mcpu=cortex-m3 -mthumb -mapcs -fno-unwind-tables \
		-Tmolly_panda_slave/molly_ld_script \
		-I$(SRCDIR)/include \
		-I$(SRCDIR)/include/arch/arm \
		-I./armv7-m/include \
		-I$(SRCDIR)/include/oldc \
		-I$(SRCDIR)/include/c \
		-imacros $(SRCDIR)/include/deputy/nodeputy.h \
		$(SRCDIR)/tools/arm_molly/molly_boot.S \
		$(SRCDIR)/tools/arm_molly/molly_init.c \
		$(SRCDIR)/tools/arm_molly/lib.c \
		./panda_mbi_slave.c \
		$(SRCDIR)/lib/elf/elf32.c \
		./molly_panda_slave/* \
		-o heteropanda_slave
	@echo "OK - heteropanda slave image is built."
	@echo "you can now use this image to link into a regular pandaboard image"




#######################################################################
#
# Pandaboard build for the heteropanda_master:
# basically a regular pandaboard_image, except that it contains
# a heteropanda_slave image, and arm_molly is called with -DHETEROPANDA
#
#######################################################################

menu.lst.heteropanda_master: $(SRCDIR)/hake/menu.lst.heteropanda_master
	cp $< $@

heteropanda_master_image: $(PANDABOARD_MODULES) \
		tools/bin/arm_molly \
		menu.lst.heteropanda_master \
		heteropanda_slave \
		$(SRCDIR)/tools/arm_molly/molly_ld_script.in
	# Translate each of the binary files we need
	$(SRCDIR)/tools/arm_molly/build_data_files.sh menu.lst.heteropanda_master molly_panda
	# Generate appropriate linker script
	cpp -P -DBASE_ADDR=0x82001000 $(SRCDIR)/tools/arm_molly/molly_ld_script.in \
		molly_panda/molly_ld_script

	# HETEROPANDA: convert slave image into a form we can insert in our image
	$(ARM_OBJCOPY) -I binary -O elf32-littlearm -B arm --rename-section \
	    .data=.rodata_thumb,alloc,load,readonly,data,contents heteropanda_slave \
	    molly_panda/heteropanda_slave

	# Build a C file to link into a single image for the 2nd-stage
	# bootloader
	tools/bin/arm_molly menu.lst.heteropanda_master panda_mbi.c
	# Compile the complete boot image into a single executable
	$(ARM_GCC) -std=c99 -g -fPIC -pie -Wl,-N -fno-builtin \
		-nostdlib -march=armv7-a -mcpu=cortex-a9 -mapcs -fno-unwind-tables \
		-Tmolly_panda/molly_ld_script \
		-I$(SRCDIR)/include \
		-I$(SRCDIR)/include/arch/arm \
		-I./armv7/include \
		-I$(SRCDIR)/include/oldc \
		-I$(SRCDIR)/include/c \
		-imacros $(SRCDIR)/include/deputy/nodeputy.h \
		$(SRCDIR)/tools/arm_molly/molly_boot.S \
		$(SRCDIR)/tools/arm_molly/molly_init.c \
		$(SRCDIR)/tools/arm_molly/lib.c \
		./panda_mbi.c \
		$(SRCDIR)/lib/elf/elf32.c \
		./molly_panda/* \
		-DHETEROPANDA \
		-o heteropanda_master_image
	@echo "OK - heteropanda_master_image is built."
	@echo "If your boot environment is correctly set up, you can now:"
	@echo "$ usbboot ./heteropanda_master_image"
