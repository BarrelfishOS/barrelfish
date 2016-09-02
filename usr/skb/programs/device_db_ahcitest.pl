% Load the ahci_test program with a higher priority than ahcid 

pci_driver{
    binary: "ahci_test",
    supported_cards:
    [ pci_card{ vendor: 16'8086, device: 16'2922, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'3a22, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'8c02, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'a102, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'8086, device: 16'1d02, function: _, subvendor: _, subdevice: _ },
      pci_card{ vendor: 16'1002, device: 16'4390, function: _, subvendor: _, subdevice: _ } ],
    core_hint: 0,
    core_offset: 0,
    multi_instance: 0,
    interrupt_load: 0.5,
    priority: 1000,
    platforms: ['x86_64', 'x86_32']
}.
