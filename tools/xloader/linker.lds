/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

OUTPUT_FORMAT("elf64-k1om") 
OUTPUT_ARCH(k1om) 



ENTRY(startup_32)
SECTIONS
{
 /* Be careful parts of head_64.S assume startup_32 is at
           * address 0.
           */
 . = 0 ;
 .head.text  : {
  _head = . ;
  *(.head.text)
  _ehead = . ;
 }
  
 .head64.text 0x300 : {
  _head64 = . ;
  *(.head64.text)
  _ehead64 = . ;
 }
  . = ALIGN(4k);
  
 .text : ALIGN(4k) {
  _start_kernel = .; /* Text */
  *(.text)
  *(.text.*)
  _end_kernel_text = . ;
 }
 . = ALIGN(4k);
 .rodata : {
  _rodata = . ;
  *(.rodata) /* read-only data */
  *(.rodata.*)
  _erodata = . ;
 }
 
 .got : {
  _got = .;
  KEEP(*(.got.plt))
  KEEP(*(.got))
  _egot = .;
 }
  
 .bss : ALIGN(4k) {
  _bss = . ;
  *(.bss)
  *(.bss.*)
  *(COMMON)
  . = ALIGN(8); /* For convenience during zeroing */
  _ebss = .;
 }
 . = ALIGN((1 << 12));
 _end_kernel = .;
. = ALIGN((1 << 12));
.pgtable : {
  _pgtable = . ;
  *(.pgtable)
  _epgtable = . ;
 }
_end_bootloader = .;

 
 /***** These sections get discarded *****/
        /DISCARD/ :
        {
                /* Discard exception handler frames and headers -- we don't use em */
                *(.eh_frame);
                *(.eh_frame_hdr);
                *(.note.gnu.build-id);
                *(.interp);
/*              *(.dynsym); */
/*              *(.dynstr); */
/*              *(.hash); */
/*              *(.gnu.hash); */
                *(.dynamic);
        }
}