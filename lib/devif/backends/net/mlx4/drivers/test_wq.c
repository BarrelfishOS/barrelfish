#include <linux/workqueue.h>
#include <barrelfish/waitset.h>

struct test {
	struct work_struct work;
};

static void test_fn(struct work_struct *work) {
	printf("1\n");
}

static void polling_loop(void) {
	errval_t err;
	struct waitset *ws = get_default_waitset();
	while (1) {
		err = event_dispatch(ws);
		if (err_is_fail(err)) {
			DEBUG_ERR(err, "in event_dispatch");
			break;
		}
	}
}

int main(int argc, char ** argv) {
	struct workqueue_struct *wq;
	struct test *test1 = malloc(sizeof *test1);
	struct test *test2 = malloc(sizeof *test2);

	printf("init\n");
	wq = create_singlethread_workqueue("name");
	INIT_WORK(&test1->work, test_fn);
	INIT_WORK(&test2->work, test_fn);
	queue_work(wq, &test1->work);
	queue_work(wq, &test2->work);
	polling_loop();
}
