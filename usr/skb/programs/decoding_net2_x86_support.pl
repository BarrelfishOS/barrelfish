% This file assumes the .soc file x86_iommu loaded

init :-
    SYS_ID = ["SYS"],
    add_SYSTEM(SYS_ID).


% Make ID argument if we want to add multiple.
add_pci :-
    ID = ["PCI0"],
    PCIBUS_ID = ["PCIBUS", "SYS"],
    PCIIN_ID = ["IN" | ID],
    PCIOUT_ID = ["OUT" | ID],
    add_PCI_IOMMU(ID),
    % connect the output to the systems pci bus
    assert(node_overlay(PCIOUT_ID, PCIBUS_ID)),
    % Now insert the BAR into the PCI bus address space
    assert(node_translate(PCIBUS_ID, [block{base:1024,limit:2048}], PCIIN_ID, [block{base:1024,limit:2048}])).

% Make ID argument if we want to add multiple.
add_process :-
    ID = ["PROC0"],
    DRAM_ID = ["DRAM", "SYS"],
    add_PROC_MMU(ID),
    assert(node_overlay(["OUT" | ID], DRAM_ID)).

remove_process_mapping :-
    MMU_ID = ["MMU","PROC0"],
    IN_ID = ["IN" | MMU_ID],
    OUT_ID = ["OUT" | MMU_ID],
    retract(node_translate(IN_ID, _, _, _)).

add_process_mapping :-
    MMU_ID = ["MMU","PROC0"],
    IN_ID = ["IN" | MMU_ID],
    OUT_ID = ["OUT" | MMU_ID],
    assert(node_translate(IN_ID, [block{base:1024,limit:2048}], OUT_ID, [block{base:1024,limit:2048}])).
