/**
 * \file
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <if/dist_defs.h>
#include <if/dist_thc.h>
#include <thc/thc.h>


int main(int argc, char *argv[])
{
    printf("hello world!\n");
    errval_t err;

    struct dist_thc_client_binding_t cl;
    struct dist_binding* b = NULL;
    err = dist_thc_connect_by_name("dist2_rpc",
                                   get_default_waitset(),
                                   IDC_BIND_FLAGS_DEFAULT,
                                   &b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect failed!");
        abort();
    }

    err = dist_thc_init_client(&cl, b, b);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "init failed");
        abort();
    }

    for(int i=0; i<2; i++) {
		DO_FINISH_(cb1, {
			thc_sem_t sem;
			thc_sem_init(&sem, 0);

			char* record = NULL;
			char* trigger_record = NULL;
			errval_t error = SYS_ERR_OK;
			uint64_t mode;

			ASYNC({
				int my_i = i;
				err = cl.recv_x.trigger(&cl, &mode, &trigger_record, &error);
				if(err_no(err) == THC_CANCELED) {
					printf("thc trigger=%d: cancelled recv trigger\n", my_i);
					printf("thc trigger=%d: receive anyway but ignore result\n", my_i);
				} else {
					debug_printf("thc trigger=%d: record = %s, err = %s\n",
							     my_i, trigger_record, err_getstring(err));
					free(trigger_record);
					thc_sem_v(&sem);
				}
			});

			char buf[100];
			snprintf(buf, 100, "test%d { attr: 'bla%d' }", i, i);
			printf("before setting test=%d\n", i);
			err = cl.call_seq.set(&cl, buf, 0, true, &record, &error);
			assert(err_is_ok(err));
			debug_printf("thc returned: record = %s, err = %s\n", record, err_getstring(err));
			free(record);

			if(i > 0) {
				printf("go again!\n");
				thc_sem_p(&sem);
			}
			else {
				printf("Cancel here\n");
				//cl.send.abort_trigger();
				CANCEL(cb1);
			}

		});
    }

	/*
	ASYNC({
		err = cl.recv.trigger(&cl, &watch_id, &mode, &trigger_record, &error);
		debug_printf("thc trigger: watch_id: %lu record = %s, err = %s\n", watch_id, trigger_record, err_getstring(err));
		free(trigger_record);
	});

	printf("before setting test2\n");
	err = cl.call_fifo.set(&cl, "test2 { attr: 'bla2' }", 0, true, &record, &error);
	assert(err_is_ok(err));
	debug_printf("thc returned: record = %s, err = %s\n", record, err_getstring(err));
	free(record);*/


    return EXIT_SUCCESS;
}
