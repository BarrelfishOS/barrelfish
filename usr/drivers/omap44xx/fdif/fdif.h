#include <barrelfish/barrelfish.h>

void play_with_fdif(void);

struct gimage {
  uint32_t       width;
  uint32_t       height;
  uint32_t       bytes_per_pixel; /* 2:RGB16, 3:RGB, 4:RGBA */ 
  uint8_t    pixel_data[320 * 240];
};
