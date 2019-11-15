%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Statically-initialised hardware facts for the ThunderX CN88xx SoC
boot_driver("/armv8/sbin/boot_armv8_generic").
cpu_driver("/armv8/sbin/cpu_imx8x").
monitor("/armv8/sbin/monitor").


% boot dirver entry points
entry_symbol(armBootBSP, "boot_entry_bsp").
entry_symbol(armBootPSCI, "boot_entry_psci").
entry_symbol(armBootParking, "boot_entry_parking").

% Core boot information
arm_mpid(0).
arm_mpid(1).
%arm_mpid(2).
%arm_mpid(3).
boot_driver_entry(_, armBootPSCI).
psci_use_hvc(0).

