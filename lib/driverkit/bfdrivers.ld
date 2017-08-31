SECTIONS
{
    .rodata.bfdrivers : {
        PROVIDE(bfdrivers_start = .);
        KEEP( *(.bfdrivers) )
        PROVIDE(bfdrivers_end = .);
    }
}
INSERT AFTER .text;
