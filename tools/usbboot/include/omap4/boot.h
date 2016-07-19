#ifndef __OMAP44XX_BOOT
#define __OMAP44XX_BOOT

#define OMAP44xx_bootmsg_periphboot 0xF0030002
#define OMAP44xx_bootmsg_getid      0xF0030003
#define OMAP44xx_subblock_id        0x01
#define OMAP44xx_subblock_checksum  0x15
#define OMAP44xx_ch_enabled         0x07
#define OMAP44xx_ch_disabled        0x17
#define OMAP44xx_vid                0x0451
#define OMAP44xx_pid                0xd010
#define OMAP44xx_bulk_out           0x01
#define OMAP44xx_bulk_in            0x81

struct omap44xx_subblock_header {
    uint8_t subblock_id;
    uint8_t subblock_size;
    uint8_t fixed;
} __attribute__((packed));

struct omap44xx_subblock_id {
    uint8_t subblock_id;
    uint8_t subblock_size;
    uint8_t fixed;
    uint8_t device[2];
    uint8_t ch;
    uint8_t rom_revision;
} __attribute__((packed));

struct omap44xx_subblock_checksum {
    uint8_t subblock_id;
    uint8_t subblock_size;
    uint8_t fixed;
    uint8_t rom_crc[4];
    uint8_t unused[4];
} __attribute__((packed));

struct omap44xx_id {
    uint8_t items;

    struct omap44xx_subblock_id id;

    uint8_t reserved1[4];
    uint8_t reserved2[23];
    uint8_t reserved3[35];

    struct omap44xx_subblock_checksum checksum;
} __attribute__((packed));

#endif /* __OMAP44XX_BOOT */
