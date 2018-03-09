:- local struct(pci_driver(
    binary,             % Name of driver binary
    supported_cards,    % List of cards this binary supports
    core_hint,          % Preferred core to start the driver
    core_offset,        % Core offset where to start the drivers (multi instance)
    multi_instance,     % Allow multi instances of the driver
    interrupt_load,     % Expected Interrupt load
    interrupt_model,    % List of supported int models. legacy,msi,msix
    platforms,          % List of architectures the driver runs on
    priority            % When more than one driver matches, the higher prio gets started
)).

:- dynamic(pci_driver/9).

:- local struct(cpu_driver(
    binary,             % Name of driver binary
    platforms           % List of architectures the driver runs on
)).

:- local struct(bus_driver(
    binary,             % Name of driver binary
    core_hint,          % Preferred core to start the driver
    platforms           % List of architectures the driver runs on
)).

:- local struct(pci_card(
    vendor,             % PCI Vendor ID
    device,             % PCI Device ID
    function,
    subvendor,
    subdevice
)).


%
% Driver entries
%

pci_driver{
    %binary: "net_sockets_server",
    binary: "e1000n",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'1521, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107d, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107e, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107f, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10b9, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1096, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'100e, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10c9, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10a7, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10d3, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1079, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1533, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'15b8, function: _, subvendor: _, subdevice: _ }
    ],
    core_hint: 1,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.75,
    interrupt_model: ['legacy'],
    platforms: ['x86_64', 'x86_32']
}.


pci_driver{
    binary: "mlx4",
    supported_cards:
    [ pci_card{ vendor: 16'15b3, device: 16'1003, function: _, subvendor: _, subdevice: _ }],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.75,
    interrupt_model: ['legacy'],
    platforms: ['x86_64', 'x86_32']
}.


pci_driver{
    binary: "sfn5122f",
    supported_cards:
    [ pci_card{ vendor: 16'1924, device: 16'0803, function: _, subvendor: _, subdevice: _ }],
    core_hint: 0,
    core_offset: 1,
    multi_instance: 0,
    interrupt_load: 0.75,
    interrupt_model: ['legacy'],
    platforms: ['x86_64']
}.


pci_driver{
    binary: "e10k",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'10fb, function: _, subvendor: _, subdevice: _ }],
    core_hint: 0,
    core_offset: 1,
    multi_instance: 0,
    interrupt_load: 0.5,
    interrupt_model: ['legacy'],
    platforms: ['x86_64']
}.

pci_driver{
    binary: "rtl8029",
    supported_cards:
    [ pci_card{ vendor: 16'10ec, device: 16'8029, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.5,
    platforms: ['x86_64', 'x86_32']
}.

pci_driver{
    binary: "mxl4_core",
    supported_cards:
    [ pci_card{ vendor: 16'15b3, device: 16'0050, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.5,
    platforms: ['x86_64' ]
}.

pci_driver{
    binary: "xeon_phi",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'225e, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 2,
    core_offset: 10,
    multi_instance: 1,
    interrupt_load: 0.5,
    platforms: ['x86_64']
}.


pci_driver{
    binary: "ioat_dma",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'0e20, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'2f20, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 4,
    core_offset: 20,
    multi_instance: 1,
    interrupt_load: 0.5,
    platforms: ['x86_64']
}.

pci_driver{
    binary: "ahcid",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'2922, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'3a22, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'1002, device: 16'4390, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.5,
    platforms: ['x86_64', 'x86_32']
}.

cpu_driver{
    binary: "cpu",
    platforms: ['x86_64', 'x86_32']
}.

bus_driver{
    binary: "ioapic",
    core_hint: 0,
    platforms: ['x86_64', 'x86_32']
}.



%
% Driver selection logic
%

int_model_enum(none, 0).
int_model_enum(legacy, 1).
int_model_enum(msi, 2).
int_model_enum(msix, 3).

% Picks from a list of IntModels one that is feasible on this system
% Currently, we use the the highest entry in interrupt_model
get_interrupt_model(IntModels, Model) :-
    ((var(IntModels) -> Model = 0);
    (maplist(int_model_enum,IntModels, Mpd), sort(0,>,Mpd,[Model | _]))).

find_pci_driver(PciInfo, DriverInfo) :-
    PciInfo = pci_card{vendor:VId, device: DId, function: Fun, subvendor: SVId,
                        subdevice: SDId},
    pci_driver{binary: Binary, supported_cards: _, core_hint: Core,
        core_offset: Offset, multi_instance: Multi,
        interrupt_load: IRQLoad, platforms: Platforms, interrupt_model: IntModels},

    % We find the highest priority matching driver.
    % TODO: binary name-intmodel is used as an identifier. Thus, multiple entries with the same
    % intmodel-binary are not supported
    findall((Prio,X, IM), (pci_driver{ supported_cards: Cards, binary: X,
        interrupt_model: IM, priority: Prio },
        member(PciInfo, Cards)), LiU),
    sort(0,>,LiU, [(_,Binary,BinIM)|_]),
    get_interrupt_model(BinIM, IntModel),
    DriverInfo = driver(Core, Multi, Offset, Binary, IntModel).

find_cpu_driver(ApicId, DriverInfo) :-
    cpu_driver{binary: Binary, platforms: Platforms},
    % TODO: In future use ApicId to select cpu driver that has listed the correct
    % platform
    DriverInfo = driver(Binary).

find_ioapic_driver(IOApicId, DriverInfo) :-
    bus_driver{binary: Binary, core_hint: Core, platforms: Platforms},
    % TODO: Select appropriate Core based on core_hint, platform, ioapic id
    DriverInfo = driver(Core, Binary).
