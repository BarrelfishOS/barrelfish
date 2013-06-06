##########################################################################
# Copyright (c) 2007, 2008, 2009, ETH Zurich.
# All rights reserved.
#
# This file is distributed under the terms in the attached LICENSE file.
# If you do not find this file, copies can be found by writing to:
# ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
##########################################################################

# Command to debug a kernel on hardware connected through conserver
# $arg0 is the name of the relevant console
define debug_hw
  target remote | console -f $arg0
end

# Command to debug kernel at an arbitrary location. $arg0 specifies
# kernel binary file name. $arg1 specifies position of kernel in
# memory (kernel prints this at startup).
define debug_kernel_at
  set $rodata_offset = 0x0000000000121000 - 0x100000
  set $data_offset = 0x0000000000127638 - 0x100000
  set $data_rel_offset = 0x0000000000127840 - 0x100000
  set $data_rel_local_offset = 0x0000000000127828 - 0x100000
  set $bss_offset = 0x0000000000128a68 - 0x100000

  set $text_addr = $arg1
  set $rodata_addr = $arg1 + $rodata_offset
  set $data_addr = $arg1 + $data_offset
  set $data_rel_addr = $arg1 + $data_rel_offset
  set $data_rel_local_addr = $arg1 + $data_rel_local_offset
  set $bss_addr = $arg1 + $bss_offset

  file $arg0
  symbol-file
  add-symbol-file $arg0 $text_addr -s .rodata $rodata_addr -s .data $data_addr -s .data.rel $data_rel_addr -s .data.rel.local $data_rel_local_addr -s .bss $bss_addr
end
