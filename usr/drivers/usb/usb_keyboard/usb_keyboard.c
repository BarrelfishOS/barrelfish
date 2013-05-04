#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <usb/usb.h>
#include <usb/usb_driver.h>


int main(int argc, char *argv[])
{
    printf(
            "\n***********************\n USB KEYBOARD \n ***************");


    usb_driver_init();

    errval_t err;
    struct waitset *ws = get_default_waitset();
        while (1) {
            err = event_dispatch(ws);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch");
                break;
            }
        }
// ff
}
