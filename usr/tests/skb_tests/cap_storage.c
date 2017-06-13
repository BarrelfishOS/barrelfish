#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <octopus/octopus.h>
#include <octopus/capability_storage.h>
#include <skb/skb.h>

#pragma GCC diagnostic ignored "-Wformat"
#pragma GCC diagnostic ignored "-Wformat-extra-args"

int main(int argc, char** argv)
{
    errval_t err = oct_init();
    assert(err_is_ok(err));
    err = skb_client_connect();
    assert(err_is_ok(err));

    // Old things should work as before
    err = skb_add_fact("y(x(%s, %d)).", "test", 1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    err = skb_execute_query("y(L),write(L).");
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }

    char retbuf[8] = {'\0'};
    skb_read_output("x(%s, 1)", retbuf);
    if (strcmp(retbuf, "test,") != 0) {
        USER_PANIC("Failed to store knowledge in SKB");
    }

    // Allocate some caps:
    struct capref c1;
    err = frame_alloc(&c1, 4096, NULL);
    assert(err_is_ok(err));

    struct frame_identity f1;
    err = invoke_frame_identify(c1, &f1);
    assert(err_is_ok(err));

    struct capref c2;
    err = frame_alloc(&c2, 4096, NULL);
    assert(err_is_ok(err));

    struct frame_identity f2;
    err = invoke_frame_identify(c2, &f2);
    assert(err_is_ok(err));
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);

    // Store them in the SKB
    err = skb_add_fact("cap(frame(%s, p(%zu, %Q))).", "cap1", f1.base, c1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Cap storage in SKB failed.");
    }
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);

    err = skb_add_fact("cap(frame(%s, p(%zu, %Q))).", "cap2", f2.base, c2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Cap storage in SKB failed.");
    }
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);

    // Try and get them back:
    err = skb_execute_query("cap(frame(cap1, A)),writeln(A).");
    uint64_t base;
    struct capref rc1;
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);

    err = skb_read_output("p(%lu, %Q)", &base, &rc1);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    if (f1.base != base) {
        USER_PANIC_ERR(err, "Parsing frame address from fact failed.");
    }

    struct frame_identity rcf1;
    err = invoke_frame_identify(rc1, &rcf1);
    if (f1.base != rcf1.base) {
        USER_PANIC_ERR(err, "Address doesn't match. We got the wrong cap?");
    }

    err = skb_execute_query("cap(frame(cap2, A)),write(A)");
    uint64_t base2;
    struct capref rc2;
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);

    err = skb_read_output("p(%lu, %Q)", &base2, &rc2);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    if (f2.base != base2) {
        USER_PANIC_ERR(err, "Parsing frame address from fact failed.");
    }

    struct frame_identity rcf2;
    err = invoke_frame_identify(rc2, &rcf2);
    if (f2.base != rcf2.base) {
        USER_PANIC_ERR(err, "Address doesn't match. We got the wrong cap?");
    }

    printf("SUCCESS\n");
    return 0;
}
