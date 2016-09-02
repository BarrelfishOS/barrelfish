%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2016, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Statically-initialised hardware facts for the big.LITTLE Fixed Virtual
% Platform with 4 A15 cores and 4 A7

cpu_driver(cortexA15, "/armv7/sbin/cpu_a15ve").
% The A7 runs fine with the A15 CPU driver.
cpu_driver(cortexA7, "/armv7/sbin/cpu_a15ve").

monitor(cortexA15, "/armv7/sbin/monitor").
monitor(cortexA7, "/armv7/sbin/monitor").

% One cluster of four Cortex A15s
arm_core(16'000000,cortexA15).
arm_core(16'000001,cortexA15).
arm_core(16'000002,cortexA15).
arm_core(16'000003,cortexA15).

% A second cluster of four Cortex A5s
arm_core(16'000100,cortexA7).
arm_core(16'000101,cortexA7).
arm_core(16'000102,cortexA7).
arm_core(16'000103,cortexA7).
