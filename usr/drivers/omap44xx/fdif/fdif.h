#ifndef FDIF_H_
#define FDIF_H_

#include <barrelfish/barrelfish.h>

void play_with_fdif(void);

struct gimage {
  uint32_t       width;
  uint32_t       height;
  uint32_t       bytes_per_pixel; /* 2:RGB16, 3:RGB, 4:RGBA */ 
  uint8_t    pixel_data[320 * 240];
};

errval_t map_device_register(lpaddr_t address, size_t size, lvaddr_t** return_address) ;
errval_t init_memory_manager(void);

#define FDIF_DEBUG(x...) printf("fdif: " x)

#endif // FDIF_H_