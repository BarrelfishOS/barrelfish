%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2017, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Statically-initialised hardware facts for the X-GENE APM88xxxx SoC

boot_driver("/armv8/sbin/boot_armv8_generic").
cpu_driver("/armv8/sbin/cpu_apm88xxxx").
monitor("/armv8/sbin/monitor").


% boot dirver entry points
entry_symbol(armBootBSP, "boot_entry_bsp").
entry_symbol(armBootPSCI, "boot_entry_psci").
entry_symbol(armBootParking, "boot_entry_parking").

% ACPI quirks
acpi_quirk(AcpiInitializeObjects, 16'20).
