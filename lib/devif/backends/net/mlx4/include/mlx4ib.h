#ifndef MLX4IB_H
#define MLX4IB_H

#include <rdma/ib_verbs.h>
#include <linux/mlx4/device.h>

extern u64 dest_addr;
extern u32 dest_rkey;
extern u32 dest_qp_num;
extern u16 dest_lid;

void *mlx4_ib_add(struct mlx4_dev *dev);
void test_ib(struct ib_device *device);

#endif /* MLX4IB_H */
