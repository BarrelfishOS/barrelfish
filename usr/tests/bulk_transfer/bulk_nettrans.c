#include <stdio.h>

#include<bulk_transfer/bulk_transfer.h>
#include<bulk_transfer/bulk_allocator.h>
#include<bulk_transfer/bulk_local.h>

/******************************************************************************/
/* Test control */

void bulk_e10k_transport_test(void);

int main(int argc, char *argv[])
{
    struct waitset *ws = get_default_waitset();
    bulk_e10k_transport_test();
    while (true) {
        event_dispatch(ws);
    }
    return 0;
}


