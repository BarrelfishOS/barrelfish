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


# where the script is located
WD=/root/barrelfish

# reset the card if not already done
$WD/reset-mic.sh


#source the MPSS environemnt configuration
. /opt/mpss/3.2/environment-setup-k1om-mpss-linux


#set the object copy
OBJCOPY=k1om-mpss-linux-objcopy

#remove existing files
rm -rf $WD/vmBf.bin
rm -rf $WD/xloader
rm -rf $WD/mbimg

#unzip the the images (for faster transfer over the internet)
gunzip $WD/xloader.gz
gunzip $WD/mbimg.gz

# transfor the bootloader into binary format
$OBJCOPY  -O binary -R .note -R .comment -S $WD/xloader $WD/vmBf.bin

# create the bzBarrelfihsh Image
$WD/build $WD/setup.bin $WD/vmBf.bin CURRENT > $WD/bzBarrelfish

state=$(cat /sys/class/mic/mic0/state)
while [[ "'$state'" == "ready" ]]; do
  sleep 1;
  state=$(cat /sys/class/mic/mic0/state)
done

# boot the mic with the bootloader and the multiboot image
echo "boot:linux:$WD/bzBarrelfish:$WD/mbimg" > /sys/class/mic/mic0/state


