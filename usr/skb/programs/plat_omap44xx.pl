%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2016, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local struct(dn_cpu_driver(
    binary,
    supported_cores,
    kernel_memory,
    kernel_devices,
    platforms
)).

:- local struct(dn_monitor(
    binary,
    supported_cores,
    platforms
)).

:- local struct(dn_driver(
    module,            % Name of driver module
    supported_devices, % Supported devices
    device_regions,    % Device regions the driver needs access to
    device_interrupts,    % Device interrupts the driver needs access to
    driver_deps,       % Dependencies on other drivers
    start_on_discovery,% Whether to start this driver eagerly or only if dependency
    platforms          % Supported platforms
)).

% Decoding net related stuff
decoding_net("sockeyefacts/omap44xx").
decoding_net_meta("sockeyefacts/omap44xx_meta").
decoding_net_irq("sockeyefacts/omap44xx-int").
decoding_net_irq_meta("sockeyefacts/omap44xx-int_meta").

% Drivers
dn_cpu_driver{
    binary: "/armv7/sbin/cpu_omap44xx",
    supported_cores: ['CORTEXA9_1', 'CORTEXA9_2'],
    kernel_memory: ['SDRAM'],
    kernel_devices: ['UART3','SCU','GIC_PROC','GIC_DIST','Global_Timer','Private_Timers','CKGEN_PRM','CKGEN_CM1','Spinlock'],
    platforms: ['omap44xx']
}.

dn_monitor{
    binary: "/armv7/sbin/monitor",
    supported_cores: ['CORTEXA9_1', 'CORTEXA9_2'],
    platforms: ['omap44xx']
}.

dn_driver{
    module: "fdif",
    supported_devices: ['FDIF'],
    device_regions: ['CAM_CM2', 'DEVICE_PRM', 'CAM_PRM', 'FDIF'],
    device_interrupts: ['FDIF_1', 'FDIF_3'],
    driver_deps: [],
    start_on_discovery: 1,
    platforms: ['omap44xx']
}.

dn_driver{
    module: "sdma",
    supported_devices: ['SDMA'],
    device_regions: ['SDMA'],
    device_interrupts: ['SDMA'],
    driver_deps: [],
    start_on_discovery: 1,
    platforms: ['omap44xx']
}.

dn_driver{
    module: "mmchs",
    supported_devices: ['HSMMC1'],
    device_regions: ['SYSCTRL_PADCONF_CORE', 'HSMMC1'],
    driver_deps: ['L3INIT_CM2', 'I2C1'],
    start_on_discovery: 0,
    platforms: ['omap44xx']
}.

dn_driver{
    module: "cm2",
    supported_devices: ['L3INIT_CM2'],
    device_regions: ['L3INIT_CM2'],
    driver_deps: [],
    start_on_discovery: 0,
    platforms: ['omap44xx']
}.

dn_driver{
    module: "twl6030",
    supported_devices: ['I2C1'],
    device_regions: ['I2C1'],
    driver_deps: [],
    start_on_discovery: 0,
    platforms: ['omap44xx']
}.

find_dn_cpu_driver(CoreID, DriverInfo) :-
    meta{
        name:Core,
        key:hw_id,
        value:CoreID
    },
    dn_cpu_driver{
        binary: Binary,
        supported_cores: Cores
    },
    member(Core, Cores),
    DriverInfo = driver(Binary).

find_dn_monitor(CoreID, MonitorInfo) :-
    meta{
        name:Core,
        key:hw_id,
        value:CoreID
    },
    dn_monitor{
        binary: Binary,
        supported_cores: Cores
    },
    member(Core, Cores),
    MonitorInfo = monitor(Binary).

find_dn_driver(Device, DriverInfo) :-
    dn_driver{
        module: Module,
        supported_devices: Devices,
        driver_deps: Deps,
        start_on_discovery: Mode
    },
    member(Device, Devices),
    findall(drv_dep(Dep),member(Dep,Deps),DepInfo),
    DriverInfo = driver(Module, Mode, DepInfo).

