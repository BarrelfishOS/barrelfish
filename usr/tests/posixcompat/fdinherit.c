#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <posixcompat.h>

int main(int argc, char *argv[])
{
    debug_printf("calling posixcompat_unpack_fds\n");
    errval_t err = posixcompat_unpack_fds();
    DEBUG_ERR(err, "posixcompat_unpack_fds");
    return 0;
}
