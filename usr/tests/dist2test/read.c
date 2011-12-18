#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>

#include <skb/skb.h>
#include <dist2/dist2.h>

#include "common.h"

int main(int argc, char *argv[])
{
    errval_t err = SYS_ERR_OK;
    dist_init();

    char* name = NULL;
    char* attr1 = NULL;
    double d;
    uint64_t i;

    err = dist_read("record", "%s", &name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    err = dist_read("record {}", "%s {}", &name);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(name, "record");
    free(name);

    err = dist_read("record { attr1: 'Test String 123' }", "%s { attr1: %s }", &name, &attr1);
    ASSERT_STRING(name, "record");
    ASSERT_STRING(attr1, "Test String 123");
    ASSERT_ERR_OK(err);
    free(name);
    free(attr1);

    err = dist_read("record2 { str: 'string', float: 12.0, integer: 1212}",
    		"_ { float: %f, str: %s, integer: %d }", &d, &attr1, &i);
    ASSERT_ERR_OK(err);
    ASSERT_STRING(attr1, "string");
    assert(d == 12.0);
    assert(i == 1212);




    return EXIT_SUCCESS;
}
