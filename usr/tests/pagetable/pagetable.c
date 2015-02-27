/**
 * Program that maps its own page-table into
 * it's virtual address space.
 **/
#include <stdio.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>

int main(int argc, char *argv[])
{

    printf("%s:%s:%d: Hi, lets map our own ptable!\n", 
           __FILE__, __FUNCTION__, __LINE__);

    return EXIT_SUCCESS;
}
