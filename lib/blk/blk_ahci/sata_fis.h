/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_SATA_FIS_H
#define _AHCI_SATA_FIS_H

#include <stdint.h>

#define SATA_FIS_TYPE_H2D	0x27 // Register FIS - Host to Device
#define SATA_FIS_TYPE_D2H	0x34 // Register FIS - Device to Host
#define SATA_FIS_TYPE_DMAA	0x39 // DMA Activate FIS - Device to Host
#define SATA_FIS_TYPE_DMAS	0x41 // DMA Setup FIS - Bi-directional
#define SATA_FIS_TYPE_DATA	0x46 // Data FIS - Bi-directional
#define SATA_FIS_TYPE_BIST	0x58 // BIST Activate FIS - Bi-directional
#define SATA_FIS_TYPE_PIO	0x5F // PIO Setup FIS - Device to Host
#define SATA_FIS_TYPE_SDB	0xA1 // Set Device Bits FIS - Device to Host

#define ATA_CMD_READ_PIO          0x20
#define ATA_CMD_READ_PIO_EXT      0x24
#define ATA_CMD_READ_DMA          0xC8
#define ATA_CMD_READ_DMA_EXT      0x25
#define ATA_CMD_WRITE_PIO         0x30
#define ATA_CMD_WRITE_PIO_EXT     0x34
#define ATA_CMD_WRITE_DMA         0xCA
#define ATA_CMD_WRITE_DMA_EXT     0x35
#define ATA_CMD_CACHE_FLUSH       0xE7
#define ATA_CMD_CACHE_FLUSH_EXT   0xEA
#define ATA_CMD_PACKET            0xA0
#define ATA_CMD_IDENTIFY_PACKET   0xA1
#define ATA_CMD_IDENTIFY          0xEC

struct sata_fis_reg_h2d {
	unsigned char type;
	unsigned char specialstuff;
	unsigned char command;
	unsigned char feature;

	unsigned char lba0;
	unsigned char lba1;
	unsigned char lba2;
	unsigned char device;

	unsigned char lba3;
	unsigned char lba4;
	unsigned char lba5;
	unsigned char featureh;

	unsigned char countl;
	unsigned char counth;
	unsigned char icc;
	unsigned char control;

	unsigned char reserved[4];
};

struct sata_fis_reg_d2h {
	unsigned char type;
	unsigned char specialstuff;
	unsigned char status;
	unsigned char error;

	unsigned char lba0;
	unsigned char lba1;
	unsigned char lba2;
	unsigned char device;

	unsigned char lba3;
	unsigned char lba4;
	unsigned char lba5;
	unsigned char reserved;

	unsigned char countl;
	unsigned char counth;
	unsigned char reserved2[2];

	unsigned char reserved3[4];
};


void sata_h2d_fis_new(struct sata_fis_reg_h2d* fis, uint8_t command, uint64_t lba, uint16_t sectors);
void sata_h2d_fis_init(struct sata_fis_reg_h2d* fis);
void sata_h2d_set_command(struct sata_fis_reg_h2d* fis, uint8_t command);
void sata_h2d_set_feature(struct sata_fis_reg_h2d* fis, uint8_t feature);
void sata_h2d_set_lba28(struct sata_fis_reg_h2d* fis, uint32_t lba);
void sata_h2d_set_lba48(struct sata_fis_reg_h2d* fis, uint64_t lba);
void sata_h2d_set_count(struct sata_fis_reg_h2d* fis, uint16_t count);


#endif // _AHCI_SATA_FIS_H
