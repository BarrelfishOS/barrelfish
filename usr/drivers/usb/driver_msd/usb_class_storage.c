/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include "usb_class_storage.h"
#include <usb/utility.h>

#define CMD_SIGNATURE     0x43425355
#define STATUS_SIGNATURE  0x53425355

uint32_t get_new_tag(void)
{
    static uint32_t tag = 0xABCDABCD;
    return tag++;
}


/*
 * \brief Generates a new command block to be sent over the USB mass 
 *        storage protocol. 
 *
 * \param tag Debugging tag
 * \param len Size of I/O transfer  
 * \param flags I/O direction flags
 * \param cmd_len Length of SCSI command block
 * \param cmd Command itself
 * \param cmd_wrapper The output of function, SCSI command wrapper in 
 *                     USB command block wrapper.
 */

void init_new_cbw(uint32_t tag, uint32_t len, uint8_t flags,
                  uint8_t cmd_len, uint8_t * cmd, cbw_t * cmd_wrapper)
{
    uint8_t *ptr = (uint8_t *) ((void *)cmd);
    uint8_t i, j = cmd_len;
    cmd_wrapper->dCBWSignature = CMD_SIGNATURE;
    cmd_wrapper->dCBWTag = tag;
    cmd_wrapper->dCBWDataTransferLength = len;
    cmd_wrapper->bmCBWFlags = flags;
    cmd_wrapper->bCBWLUN = 0x0; // 7-4 are RSVD and 0-3 are zero LUN
    cmd_wrapper->bCBWCBLength = cmd_len;

    // Starting from MSB copy the command

    i = 0;
    j = 0;
    while (j < cmd_len) {
        cmd_wrapper->CBWCB[i] = ptr[j];
        i++;
        j++;
    }

    // Sanity check 
    assert(cmd_wrapper->CBWCB[0] == cmd[0]);
}

/*
 * \brief Generates a new clean Command Status Block, which is 
 *        populated later. 
 */

void init_new_csw(uint32_t tag, csw_t * status)
{
    status->dCSWSignature = 0x0;        //STATUS_SIGNATURE;
    status->dCSWTag = 0x0;      //tag;
    status->dCSWDataResidue = 0;
    status->bCSWStatus = 0;
}

/*
 * \brief Prints the USB command block 
 *
 * \param cbw Command block to be printed
 */

void print_cbw(cbw_t cbw)
{
    int i;
    printf("\n\n------- DUMP of CBW -------------");
    printf("\n Signature %x", cbw.dCBWSignature);
    printf("\n Tag       %x", cbw.dCBWTag);
    printf("\n Transfer Length %x", cbw.dCBWDataTransferLength);
    printf("\n Flags  %x", cbw.bmCBWFlags);
    printf("\n LUN %x", cbw.bCBWLUN);
    printf("\n CBWCBLength %x", cbw.bCBWCBLength);
    for (i = 0; i < 16; i++)
        printf("\n CBWCB[%d] : %x", i, cbw.CBWCB[i]);

    printf("\n----------End of dum of CBW -------");
}

/*
 * \brief Prints the USB status block 
 *
 * \param cbw Status block to be printed
 */

void print_csw(csw_t csw)
{
    printf("\n\n------- DUMP of CSW -------------");
    printf("\n Signature %x", csw.dCSWSignature);
    printf("\n Tag       %x", csw.dCSWTag);
    printf("\n Data Residue %x", csw.dCSWDataResidue);
    printf("\n Status  %x", csw.bCSWStatus);
    printf("\n----------End of dum of CSW -------");
}
