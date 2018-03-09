% Load the irqtest program with a higher priority than the e1000 driver
pci_driver{
    binary: "e1000n_irqtest",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'1521, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107d, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1076, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1079, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107e, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'107f, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10b9, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1096, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'100e, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10c9, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1533, function: _, subvendor: _, subdevice: _ }
    ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 1,
    interrupt_load: 0.75,
    interrupt_model: [legacy],
    platforms: ['x86_64', 'x86_32'],
    priority: 1000
}.

pci_driver{
    binary: "e1000n_irqtest",
    supported_cards:
    [ 
      pci_card{ vendor: 16'8086, device: 16'10a7, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'10d3, function: _, subvendor: _, subdevice: _ }
    ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 1,
    interrupt_load: 0.75,
    interrupt_model: [legacy],
    platforms: ['x86_64', 'x86_32'],
    priority: 1001
}.

