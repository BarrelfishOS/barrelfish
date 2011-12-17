size_t incoming_messages = 0;

static void subhandler(subscription_t id1, char* object, void* st)
{
    assert(object != NULL);

    switch(id1) {
        case 0:
            printf("object: %s\n", object);
            assert(strcmp(object, "publishIt { weight: 10, height: 20, depth: 30 }") == 0);
        break;

        case 1:
            assert(strcmp(object, "publishIt { age: 10 }") == 0);
        break;
    }

    printf("subhandler(%lu): id:%lu obj:%s\n", ++incoming_messages, id1, object);
    free(object);
}


static errval_t pub_sub_test(void)
{
    errval_t err = SYS_ERR_OK;

    subscription_t id1 = 0;
    err = dist_subscribe(subhandler, NULL, &id1, "_ { weight: %d }", 10);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_get failed!");
        return err;
    }
    printf("subscription done with id: %lu\n", id1);

    subscription_t id2 = 0;
    err = dist_subscribe(subhandler, NULL, &id2, "_ { age: > %d }", 9);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_get failed!");
        return err;
    }
    printf("subscription done with id: %lu\n", id2);

    err = dist_publish("publishIt { weight: %d, height: %d, depth: %d }", 10, 20, 30);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_publish failed!");
        return err;
    }

    err = dist_unsubscribe(id);

    // Should not deliver
    err = dist_publish("publishIt { weight: %d, height: %d }", 10, 9999);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_publish failed!");
        return err;
    }

    // Should not deliver
    err = dist_publish("publishIt { age: %d }", 9);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_publish failed!");
        return err;
    }

    err = dist_publish("publishIt { age: %d }", 10);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_publish failed!");
        return err;
    }

    return err;
}


static void subhandler2(subscription_t id1, char* object, void* st)
{
    assert(object != NULL);
    debug_printf("subhandler2(%lu): id:%lu obj:%s\n", ++incoming_messages, id1, object);
    free(object);
}

static void main_subscriber(void) {
    errval_t err;

    subscription_t id1 = 0;
    err = dist_subscribe(subhandler2, NULL, &id1, "_ { weight: %d }", 10);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_subscribe failed!");
        abort();
    }
    printf("subscription done with id: %lu\n", id1);

    subscription_t id2 = 0;
    err = dist_subscribe(subhandler2, NULL, &id2, "_ { age: > %d }", 9);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "dist_subscribe failed!");
        abort();
    }
    printf("subscription done with id: %lu\n", id2);

    messages_handler_loop();
}


