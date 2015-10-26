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

########################################################################
#
# GEM5 build
#
########################################################################

menu.lst.arm_gem5: $(SRCDIR)/hake/menu.lst.arm_gem5
	cp $< $@

menu.lst.arm_gem5_mc: $(SRCDIR)/hake/menu.lst.arm_gem5_mc
	cp $< $@

GEM5_MODULES=\
	armv7/sbin/cpu_arm_gem5 \
	armv7/sbin/init \
	armv7/sbin/mem_serv \
	armv7/sbin/monitor \
	armv7/sbin/ramfsd \
	armv7/sbin/spawnd \
	armv7/sbin/startd \
	armv7/sbin/corectrl \
	armv7/sbin/skb \
	armv7/sbin/memtest


arm_gem5_image: $(GEM5_MODULES) \
		tools/bin/arm_molly \
		menu.lst.arm_gem5_mc
	# Translate each of the binary files we need
	$(SRCDIR)/tools/arm_molly/build_data_files.sh menu.lst.arm_gem5_mc molly_gem5
	# Generate appropriate linker script
	cpp -P -DBASE_ADDR=0x00100000 $(SRCDIR)/tools/arm_molly/molly_ld_script.in \
		molly_gem5/molly_ld_script
	# Build a C file to link into a single image for the 2nd-stage
	# bootloader
	tools/bin/arm_molly menu.lst.arm_gem5_mc arm_mbi.c
	# Compile the complete boot image into a single executable
	$(ARM_GCC) -std=c99 -g -fPIC -pie -Wl,-N -fno-builtin \
		-nostdlib -march=armv7-a -mapcs -fno-unwind-tables \
		-Tmolly_gem5/molly_ld_script \
		-I$(SRCDIR)/include \
		-I$(SRCDIR)/include/arch/arm \
		-I./armv7/include \
		-I$(SRCDIR)/include/oldc \
		-I$(SRCDIR)/include/c \
		-imacros $(SRCDIR)/include/deputy/nodeputy.h \
		$(SRCDIR)/tools/arm_molly/molly_boot.S \
		$(SRCDIR)/tools/arm_molly/molly_init.c \
		$(SRCDIR)/tools/arm_molly/lib.c \
		./arm_mbi.c \
		$(SRCDIR)/lib/elf/elf32.c \
		./molly_gem5/* \
		-o arm_gem5_image

# ARM GEM5 Simulation Targets
ARM_FLAGS=$(SRCDIR)/tools/arm_gem5/gem5script.py --caches --l2cache --n=2 --kernel=arm_gem5_image

arm_gem5: arm_gem5_image $(SRCDIR)/tools/arm_gem5/gem5script.py
	gem5.fast $(ARM_FLAGS)

arm_gem5_detailed: arm_gem5_image $(SRCDIR)/tools/arm_gem5/gem5script.py
	gem5.fast $(ARM_FLAGS) --cpu-type=arm_detailed

.PHONY: arm_gem5 arm_gem5_detailed
