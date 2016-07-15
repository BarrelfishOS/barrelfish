/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <errors/errno.h>
#include "sata_fis.h"

void sata_h2d_fis_new(struct sata_fis_reg_h2d* fis, uint8_t command, uint64_t lba, uint16_t sectors)
{
    sata_h2d_fis_init(fis);
    sata_h2d_set_command(fis, command);
    sata_h2d_set_lba48(fis, lba);
    sata_h2d_set_count(fis, sectors);
}

void sata_h2d_fis_init(struct sata_fis_reg_h2d* fis)
{
    fis->type = SATA_FIS_TYPE_H2D;

    /* Device Shadow Register layout (see: [1])
     * [  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  ]
     * [  -  |  L  |  -  | DEV | HS3 | HS2 | HS1 | HS0 ]
     *
     * L is the address mode, cleared implies CHS addressing, set, LBA addressing
     * DEV device select, cleared and set imply Device 0 and 1 resp.
     *   for SATA this should always be cleared (see: [2])
     * HS3-HS0 are bits 28-25 of the LBA 28 (not used for LBA 48, see [3])
     *
     * [1] Serial ATA NSSD Rev. 1.0 (Sept 2008), section 6.3.1
     * [2] Serial ATA Rev. 2.6 (15-February-2007), section 13.1, paragraph 2
     * [3] ATA8-ACS Rev. 3f (December 11, 2006), section 7.1.5.2
     */
    fis->device |= (1 << 6);
}

void sata_h2d_set_command(struct sata_fis_reg_h2d* fis, uint8_t command)
{
    fis->command = command;
    /* set bit to indicate update of command register (see [1])
     *
     * [1]: SATA Rev. 2.6 (15-February-2007), section 10.3.4
     */
    fis->specialstuff |= (1 << 7);
}

void sata_h2d_set_feature(struct sata_fis_reg_h2d* fis, uint8_t feature)
{
    fis->feature = feature;
}

void sata_h2d_set_lba28(struct sata_fis_reg_h2d* fis, uint32_t lba)
{
    fis->lba0 = lba & 0xFF;
    fis->lba1 = (lba >> 8) & 0xFF;
    fis->lba2 = (lba >> 16) & 0xFF;
    fis->device = (fis->device & ~0x0F) | ((lba >> 24) & 0x0F);
}

void sata_h2d_set_lba48(struct sata_fis_reg_h2d* fis, uint64_t lba)
{
    fis->lba0 = lba & 0xFF;
    fis->lba1 = (lba >> 8) & 0xFF;
    fis->lba2 = (lba >> 16) & 0xFF;
    fis->device &= 0xF0; // clear bits otherwise used by lba28

    fis->lba3 = (lba >> 24) & 0xFF;
    fis->lba4 = (lba >> 32) & 0xFF;
    fis->lba5 = (lba >> 40) & 0xFF;
}

void sata_h2d_set_count(struct sata_fis_reg_h2d* fis, uint16_t count)
{
    fis->countl = count & 0xFF;
    fis->counth = (count >> 8) & 0xFF;
}
