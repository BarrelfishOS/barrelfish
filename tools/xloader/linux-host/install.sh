#!/bin/bash


#
# \file
# \brief Boot tool for the Intel Xeon Phi (Linux Host)
#
# This program creates a multiboot informaton structure based on the pre-
# processed menu.lst file
#
#
# Copyright (c) 2014 ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
#

# stof on error
set -e

# preparing the upload
mv multiboot/mbimg ./xeon_phi_multiboot
rm -rf xeon_phi_bootloader.gz xeon_phi_multiboot.gz
gzip xeon_phi_bootloader xeon_phi_multiboot

# uploading...
scp *.gz emmentaler.ethz.ch:

# Issuing commands to emmentaler
ssh emmentaler.ethz.ch "gunzip -f *.gz"
ssh emmentaler.ethz.ch "mv xeon_phi_bootloader /home/netos/tftpboot/acreto"
ssh emmentaler.ethz.ch "mv xeon_phi_multiboot /home/netos/tftpboot/acreto"

