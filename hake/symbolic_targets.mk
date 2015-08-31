##########################################################################
# Copyright (c) 2009-2014 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
# Attn: Systems Group.
#
# This file defines symbolic (i.e. non-file) targets for the Makefile
# generated by Hake.  Edit this to add your own symbolic targets.
#
##########################################################################

# Disable built-in implicit rules. GNU make adds environment's MAKEFLAGS too.
MAKEFLAGS=r

# Explicitly disable the flex and bison implicit rules
%.c : %.y

%.c : %.l

# Set default architecture to the first specified by Hake in generated Makefile.
ARCH ?= $(word 1, $(HAKE_ARCHS))

# Generic help text: this is the default target.
.PHONY: help
help:: 
	@echo "------------------------------------------------------------------"
	@echo "This is the 'help' target for the Barrelfish Makefile.  This Makefile"
	@echo "has been generated by hake in the top-level directory of your build tree."
	@echo "See the Hake documentation for more information."
	@echo ""
	@echo "This Makefile contains build targets for the following architectures:"
	@echo ""		
	@echo "$(HAKE_ARCHS)"
	@echo ""	
	@echo "To change configuration options, edit the Config.hs file in the hake"
	@echo "subdirectory of this directory and run 'make rehake'."
	@echo ""
	@echo "To change the set of symbolic make targets available (for example, to"
	@echo "build a different set of modules or architectures for your boot image),"
	@echo "edit the local copy of the symbolic_targets.mk in this directory."
	@echo ""	
	@echo "Useful make targets:"
	@echo " - <file>:           any file which can be built in the Barrelfish tree"	
	@echo " - 'help':           show this help text"
	@echo " - 'help-platforms': show the platforms (file collections) which can be built"
	@echo " - 'help-boot':      show the boot sequences which can be initiated"
	@echo " - 'clean':          remove most generated files" 
	@echo " - 'realclean':      remove all generated files (clears the build tree)"
	@echo " - 'rehake':         attempt to re-run hake"
	@echo ""

# Print the list of defined platforms.  Most of these will be
# generated by Hake; this is just the preamble:
.PHONY: help-platforms
help-platforms:: 
	@echo "------------------------------------------------------------------"
	@echo "Platforms supported by this Makefile.  Use 'make <platform name>':"
	@echo " (these are the platforms available with your architecture choices)"
	@echo ""

# Print the list of defined boots.  Most of these will be
# generated by Hake; this is just the preamble:
.PHONY: help-boot
help-boot:: 
	@echo "------------------------------------------------------------------"
	@echo "Boot instructions supported by this Makefile.  Use 'make <boot name>':"
	@echo " (these are the targets available with your architecture choices)"
	@echo ""

# All binaries of the RCCE LU benchmark
BIN_RCCE_LU= \
	sbin/rcce_lu_A1 \
	sbin/rcce_lu_A2 \
	sbin/rcce_lu_A4 \
	sbin/rcce_lu_A8 \
	sbin/rcce_lu_A16 \
	sbin/rcce_lu_A32 \
	sbin/rcce_lu_A64

# All binaries of the RCCE BT benchmark
BIN_RCCE_BT= \
	sbin/rcce_bt_A1 \
	sbin/rcce_bt_A4 \
	sbin/rcce_bt_A9  \
	sbin/rcce_bt_A16 \
	sbin/rcce_bt_A25 \
	sbin/rcce_bt_A36

# All test domains
TESTS_COMMON= \
	sbin/hellotest \
	sbin/idctest \
	sbin/memtest \
	sbin/schedtest \
	sbin/testerror \
	sbin/yield_test

TESTS_x86= \
	sbin/tests/luatest \
	sbin/tests/numatest

TESTS_x86_64= \
	$(TESTS_x86) \
	sbin/arrakis_hellotest \
	sbin/ata_rw28_test \
	sbin/bomp_cpu_bound \
	sbin/bomp_cpu_bound_progress \
	sbin/bomp_sync \
	sbin/bomp_sync_progress \
	sbin/bomp_test \
	sbin/bulk_shm \
	sbin/cryptotest \
	sbin/fputest \
	sbin/fread_test \
	sbin/fscanf_test \
	sbin/mdbtest_addr_zero \
	sbin/mdbtest_range_query \
	sbin/mem_affinity \
	sbin/multihoptest \
	sbin/net-test \
	sbin/net_openport_test \
	sbin/perfmontest \
	sbin/phoenix_kmeans \
	sbin/socketpipetest \
	sbin/spantest \
	sbin/spin \
	sbin/testconcurrent \
	sbin/testdesc \
	sbin/testdesc-child \
	sbin/tests/cxxtest \
	sbin/tests/dma_test \
	sbin/tests/xphi_nameservice_test \
	sbin/thcidctest \
	sbin/thcminitest \
	sbin/thctest \
	sbin/timer_test \
	sbin/tlstest \
	sbin/tweedtest \
	sbin/xcorecap \
	sbin/xcorecapserv

TESTS_k1om= \
	$(TESTS_x86) \
	sbin/tests/dma_test \
	sbin/tests/xeon_phi_inter \
	sbin/tests/xeon_phi_test \
	sbin/tests/xphi_nameservice_test

# All benchmark domains
BENCH_COMMON= \
	sbin/channel_cost_bench \
	sbin/flounder_stubs_buffer_bench \
	sbin/flounder_stubs_empty_bench \
	sbin/flounder_stubs_payload_bench \
	sbin/xcorecapbench

BENCH_x86= \
	sbin/multihop_latency_bench \
	sbin/net_openport_test \
	sbin/perfmontest \
	sbin/thc_v_flounder_empty \
	sbin/timer_test \
	sbin/udp_throughput \
	sbin/ump_exchange \
	sbin/ump_latency \
	sbin/ump_latency_cache \
	sbin/ump_receive \
	sbin/ump_send \
	sbin/ump_throughput

BENCH_x86_64= \
	$(BENCH_x86) \
	$(BIN_RCCE_BT) \
	$(BIN_RCCE_LU) \
	sbin/ahci_bench \
	sbin/apicdrift_bench \
	sbin/benchmarks/bomp_mm \
	sbin/benchmarks/dma_bench \
	sbin/benchmarks/xomp_share \
	sbin/benchmarks/xomp_spawn \
	sbin/benchmarks/xomp_work \
	sbin/benchmarks/xphi_ump_bench \
	sbin/bomp_benchmark_cg \
	sbin/bomp_benchmark_ft \
	sbin/bomp_benchmark_is \
	sbin/bulk_transfer_passthrough \
	sbin/bulkbench \
	sbin/bulkbench_micro_echo \
	sbin/bulkbench_micro_rtt \
	sbin/bulkbench_micro_throughput \
	sbin/elb_app \
	sbin/elb_app_tcp \
	sbin/lrpc_bench \
	sbin/mdb_bench \
	sbin/mdb_bench_old \
	sbin/netthroughput \
	sbin/phases_bench \
	sbin/phases_scale_bench \
	sbin/placement_bench \
	sbin/rcce_pingpong \
	sbin/shared_mem_clock_bench \
	sbin/tsc_bench

BENCH_k1om=\
	$(BENCH_x86) \
	sbin/benchmarks/bomp_mm \
	sbin/benchmarks/dma_bench \
	sbin/benchmarks/xomp_share \
	sbin/benchmarks/xomp_spawn \
	sbin/benchmarks/xomp_work \
	sbin/benchmarks/xphi_ump_bench \
	sbin/benchmarks/xphi_xump_bench 

# Default list of modules to build/install for all enabled architectures
MODULES_COMMON= \
	sbin/init \
	sbin/chips \
	sbin/skb \
	sbin/spawnd \
	sbin/startd \
	sbin/mem_serv \
	sbin/monitor \
	sbin/ramfsd

# List of modules that are arch-independent and always built
MODULES_GENERIC= \
	skb_ramfs.cpio.gz \
	sshd_ramfs.cpio.gz

# x86_64-specific modules to build by default
# this should shrink as targets are ported and move into the generic list above
MODULES_x86_64= \
	sbin/elver \
	sbin/cpu \
	sbin/arrakismon \
	sbin/bench \
	sbin/bfscope \
	sbin/boot_perfmon \
	sbin/datagatherer \
	sbin/ahcid \
	sbin/e1000n \
	sbin/NGD_mng \
	sbin/e10k \
	sbin/sfxge \
	sbin/e10k_queue \
	sbin/rtl8029 \
	sbin/netd \
	sbin/echoserver \
	sbin/fbdemo \
	sbin/fish \
	sbin/hpet \
	sbin/lpc_kbd \
	sbin/lpc_timer \
	sbin/mem_serv_dist \
	sbin/lo_queue \
	sbin/pci \
	sbin/acpi \
	sbin/kaluga \
	sbin/serial \
	sbin/angler \
	sbin/sshd \
	sbin/lshw \
	sbin/sif \
	sbin/slideshow \
	sbin/vbe \
	sbin/vmkitmon \
	sbin/vnode_map_test \
	sbin/webserver \
	sbin/routing_setup \
	sbin/bcached \
	sbin/xeon_phi_mgr \
	sbin/xeon_phi \
	sbin/dma_mgr \
	sbin/ioat_dma \
	sbin/virtio_blk_host \
	sbin/virtio_blk \
	sbin/block_server \
	sbin/block_server_client \
	sbin/bs_user \
	sbin/bulk_shm \
	sbin/corectrl \
	sbin/megaraid \
	lib/libmegaraid.a

MODULES_k1om= \
	sbin/weever \
	sbin/cpu \
	sbin/xeon_phi \
	sbin/corectrl \
	xeon_phi_multiboot

# the following are broken in the newidc system
MODULES_x86_64_broken= \
	sbin/barriers \
	sbin/ipi_bench \
	sbin/ring_barriers \
	sbin/ssf_bcast \
	sbin/lamport_bcast

# x86-32-specific module to build by default
MODULES_x86_32=\
	sbin/cpu \
	sbin/lpc_kbd \
	sbin/serial \
	$(BIN_RCCE_BT) \
	$(BIN_RCCE_LU) \
	sbin/rcce_pingpong \
	sbin/bench \
	sbin/fbdemo \
	sbin/fish \
	sbin/fputest \
	sbin/pci \
	sbin/acpi \
	sbin/kaluga \
	sbin/slideshow \
	sbin/thc_v_flounder_empty \
	sbin/thcidctest \
	sbin/thcminitest \
	sbin/thctest \
	sbin/vbe \
	sbin/mem_serv_dist \
	sbin/routing_setup \
	sbin/multihoptest \
	sbin/multihop_latency_bench \
	sbin/angler \
	sbin/sshd \
	sbin/corectrl

# ARM-specific modules to build by default
MODULES_armv5=\
	sbin/cpu \
	sbin/cpu.bin

# XScale-specific modules to build by default
MODULES_xscale=\
	sbin/cpu_ixp2800 \
	sbin/cpu_ixp2800.bin

# ARMv7-specific modules to build by default
# XXX: figure out armv7 default
MODULES_armv7=\
	sbin/cpu_omap44xx \
	sbin/usb_manager \
	sbin/usb_keyboard \
	sbin/kaluga \
	sbin/fish \
	sbin/corectrl

# construct list of all modules to be built (arch-specific and common for each arch)
MODULES=$(foreach a,$(HAKE_ARCHS),$(foreach m,$(MODULES_$(a)),$(a)/$(m)) \
                                  $(foreach m,$(MODULES_COMMON),$(a)/$(m))) \
		$(foreach a,$(HAKE_ARCHS),$(foreach m,$(TESTS_$(a)),$(a)/$(m)) \
		                          $(foreach m,$(TESTS_COMMON),$(a)/$(m))) \
		$(foreach a,$(HAKE_ARCHS),$(foreach m,$(BENCH_$(a)),$(a)/$(m)) \
                                  $(foreach m,$(BENCH_COMMON),$(a)/$(m))) \
        $(MODULES_GENERIC)

# XXX: this should be overridden in some local settings file?
INSTALL_PREFIX ?= /home/netos/tftpboot/$(USER)

# upload Xeon Phi images to nfs share (leave blank to cancel)
BARRELFISH_NFS_DIR ?="emmentaler.ethz.ch:/mnt/local/nfs/barrelfish/xeon_phi"

# Only install a binary if it doesn't exist in INSTALL_PREFIX or the modification timestamp differs

install: $(MODULES)
	@echo ""; \
	echo "Installing modules..." ; \
	for m in ${MODULES}; do \
	  if [ ! -f ${INSTALL_PREFIX}/$$m ] || \
	      [ $$(stat -c%Y $$m) -ne $$(stat -c%Y ${INSTALL_PREFIX}/$$m) ]; then \
	         do_update=1; \
	      	 echo "  > Installing $$m" ; \
	    	 mkdir -p ${INSTALL_PREFIX}/$$(dirname $$m); \
	    	 install -p $$m ${INSTALL_PREFIX}/$$m; \
	  fi; \
	done; \
	if [ ! $$do_update ]; then \
		echo "  > All up to date" ; \
	fi; \
	echo ""; \
	echo "done." ; \

.PHONY : install


install_headers:
	echo "Installing header files..." ; \
	for a in ${HAKE_ARCHS}; do \
	  mkdir -p "$$a" ; \
	  cp -rv "${SRCDIR}/include" "$$a/" ; \
	done; \
	echo "done." ; \

.PHONY : install_headers

# Source indexing targets
cscope.files:
	find $(abspath .) $(abspath $(SRCDIR)) -name '*.[ch]' -type f -print | sort | uniq > $@
.PHONY: cscope.files

cscope.out: cscope.files
	cscope -k -b -i$<

TAGS: cscope.files
	etags - < $< # for emacs
	cat $< | xargs ctags -o TAGS_VI # for vim

# force rebuild of the Makefile
rehake: ./hake/hake
	./hake/hake --source-dir $(SRCDIR) --install-dir . \
	            --output-filename Makefile
.PHONY: rehake

clean::
	$(RM) -r tools docs $(HAKE_ARCHS)
.PHONY: clean

realclean:: clean
	$(RM) hake/*.o hake/*.hi hake/hake Hakefiles.hs cscope.*
.PHONY: realclean

# Scheduler simulator test cases
RUNTIME = 1000
TESTS = $(addsuffix .txt,$(basename $(wildcard $(SRCDIR)/tools/schedsim/*.cfg)))

schedsim-regen: $(TESTS)

$(TESTS): %.txt: %.cfg tools/bin/simulator
	tools/bin/simulator $< $(RUNTIME) > $@

schedsim-check: $(wildcard $(SRCDIR)/tools/schedsim/*.cfg)
	for f in $^; do tools/bin/simulator $$f $(RUNTIME) | diff -q - `dirname $$f`/`basename $$f .cfg`.txt || exit 1; done


######################################################################
#
# Documentation
#
######################################################################

# pretend to be CMake's CONFIGURE_FILE command
# TODO: clean this up
Doxyfile: $(SRCDIR)/doc/Doxyfile.cmake
	sed -r 's#@CMAKE_SOURCE_DIR@#$(SRCDIR)#g' $< > $@

doxygen: Doxyfile
	doxygen $<
.PHONY: doxygen


######################################################################
#
# Intel Xeon Phi Builds
#
######################################################################

#k1om/sbin/weever: k1om/sbin/weever_elf
#	$(K1OM_OBJCOPY) -O binary -R .note -R .comment -S k1om/sbin/weever_elf ./k1om/sbin/weever

################################################################################
#
# Backwards compatibility: remove soon
# 
################################################################################

all: 
	@echo "Type 'make help' for information on available targets."
.PHONY : all

sim: simulate
.PHONY : sim

simulate: 
	@echo "The targets 'sim' and 'simulate' have removed."
	@echo "Type 'make help-boot' for a list of simulation targets."
.PHONY : simulate

# XXX: Horrid hack to hardcode size of romfs CPIO image into ARM kernel
# This works in several recursive make steps:
# 1. Create a dummy romfs_size.h header file
# 2. Compile everything
# 3. Create the romfs CPIO image
# 4. Determine its size and write to romfs_size.h
# 5. Re-compile kernel (but not the romfs) with correct size information
# 6. Install romfs to installation location

$(ARCH)/menu.lst: $(SRCDIR)/hake/menu.lst.$(ARCH)
	cp $< $@

$(ARCH)/romfs.cpio: $(SRCDIR)/tools/arm-mkbootcpio.sh $(MODULES) $(ARCH)/menu.lst
	$(SRCDIR)/tools/arm-mkbootcpio.sh $(ARCH)/menu.lst $@

# Location of hardcoded size of romfs CPIO image
arm_romfs_cpio = "$(ARCH)/include/romfs_size.h"

arm:
	$(MAKE)
	$(MAKE) $(ARCH)/romfs.cpio
	echo "//Autogenerated size of romfs.cpio because the bootloader cannot calculate it" > $(arm_romfs_cpio)
	echo "size_t romfs_cpio_archive_size = `ls -asl $(ARCH)/romfs.cpio | sed -e 's/ /\n/g' | head -6 | tail -1`;" >> $(arm_romfs_cpio)
	$(MAKE)
.PHONY: arm

# Builds a dummy romfs_size.h
$(ARCH)/include/romfs_size.h:
	mkdir -p $(shell dirname $@)
	echo "size_t romfs_cpio_archive_size = 0; //should not see this" > $@

arminstall:
	$(MAKE) arm
	$(MAKE) install
	install -p $(ARCH)/romfs.cpio ${INSTALL_PREFIX}/$(ARCH)/romfs.cpio
.PHONY: arminstall

