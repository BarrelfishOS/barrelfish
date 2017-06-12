#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <barrelfish/nameservice_client.h>

int main(int argc, char** argv)
{
    size_t drivers = 0;
    struct bfdriver* cur = NULL;
    driverkit_list(&cur, &drivers);
    for (size_t i=0; i<drivers; i++) {
        printf("%s:%s:%d: Found device driver class = %s\n", __FILE__, __FUNCTION__, __LINE__, cur->name);
        cur += 1;
    }

    iref_t dev, ctrl;
    driverkit_create_driver("fdif", "fdif_inst", NULL, 0, NULL, 0, 0, &dev, &ctrl);

    messages_handler_loop();
    return 0;
}
