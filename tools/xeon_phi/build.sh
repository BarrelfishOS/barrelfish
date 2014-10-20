#!/bin/sh

##########################################################################
# Copyright (c) 2014 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
#########################################################################



#
# NOTE: You need to put this script into your mpss-modules source directory
#       Make sure you have the requirements as stated in the Intel
#	MPSS manual fulfilled.
#

patch host/uos_download.c < mpss.patch

make MIC_CARD_ARCH=k1om -j 10

has_mic=$(lsmod|grep mic)

if [[ "$has_mic" != "" ]]; then
	modprobe -r mic	
fi

cp mic.ko /lib/modules/2.6.32-431.el6.x86_64/extra/mic_mod.ko

depmod -a

modprobe mic_mod
