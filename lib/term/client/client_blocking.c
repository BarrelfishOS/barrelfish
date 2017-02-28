/**
 * \file
 * \brief Blocking I/O API for terminal client library.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/waitset.h>
#include <collections/list.h>
#include <if/monitor_defs.h>
#include <if/octopus_defs.h>
#include <if/octopus_defs.h>
#include <if/terminal_defs.h>
#include <if/terminal_config_defs.h>
#include <octopus/getset.h>
#include <octopus/trigger.h>
#include <term/client/client_blocking.h>
#include <term/client/default_filters.h>
#include <term/client/default_triggers.h>

#include "term_debug.h"
#include "filter_priv.h"
#include "trigger_priv.h"

#include <assert.h>
#include <string.h>

/* internal functions */
static errval_t get_irefs(struct capref sessionid, iref_t *in_iref,
                          iref_t *out_iref, iref_t *conf_iref);
static void struct_term_client_init(struct term_client *client);
static void in_bind_cb(void *st, errval_t err, struct terminal_binding *b);
static void out_bind_cb(void *st, errval_t err, struct terminal_binding *b);
static void conf_bind_cb(void *st, errval_t err,
                         struct terminal_config_binding *b);
static errval_t handle_echo(struct term_client *client, char *data,
                            size_t length);
static void handle_triggers(struct term_client *client, char *data,
                            size_t length);
static void exit_cb(void *st);

/**
 * \brief Initialize a connection to a terminal server and block until
 *        connection is established.
 *
 * \param client     Terminal client state, initialized by function to default
 *                   values.
 * \param session_id The session the domain is part of.
 *
 * Dispatches the monitor waitset until all the bindings to the terminal server
 * are established.
 */
errval_t term_client_blocking_init(struct term_client *client,
                                   struct capref session_id)
{
    errval_t err;
    iref_t in_iref= 0;
    iref_t out_iref= 0;
    iref_t conf_iref= 0;

    /* Initialize client state to default values. */
    struct_term_client_init(client);

    /* Get the interface references from octopus. */
    err = get_irefs(session_id, &in_iref, &out_iref, &conf_iref);
    if (err_is_fail(err)) {
        return err;
    }

    /* Bind to interface for incoming characters. */
    err = terminal_bind(in_iref, in_bind_cb, client, client->read_ws,
                        IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_BIND_IN_INTERFACE);
    }
    TERM_DEBUG("Binding to terminal interface for incoming characters.\n");

    /* Bind to interface for outgoing characters. */
    err = terminal_bind(out_iref, out_bind_cb, client, client->write_ws,
                        IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_BIND_OUT_INTERFACE);
    }
    TERM_DEBUG("Binding to terminal interface for outgoing characters.\n");

    /* Bind to interface for incoming characters. */
    err = terminal_config_bind(conf_iref, conf_bind_cb, client,
                               client->conf_ws, IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_BIND_CONF_INTERFACE);
    }
    TERM_DEBUG("Binding to terminal configuration interface for configuration "
               "messages.\n");

    /*
     * Dispatch on the monitor binding until the bind completes. Otherwise, we
     * would have to check before every term_client_blocking_read and
     * term_client_blocking_write if we're already connected.
     */
    struct monitor_binding *monitor_b = get_monitor_binding();
    struct waitset *monitor_ws = monitor_b->waitset;
    while (!client->connected) {
        err = event_dispatch(monitor_ws);
        if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Error dispatching events.");
        }
    }
    TERM_DEBUG("Connection to terminal server successfully established.\n");

    return SYS_ERR_OK;
}

/**
 * \brief Tear down connection to terminal server.
 *
 * \param client Terminal client state.
 *
 * Dispatches the control waitset until the message is sent.
 */
void term_client_blocking_exit(struct term_client *client)
{
    errval_t err;

    TERM_DEBUG("Sending disconnect message to terminal device.\n");

    /* Inform terminal device (server), that domain terminated. */
    err = client->conf_binding->tx_vtbl.disconnect(client->conf_binding,
                                                   MKCONT(exit_cb, client));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error sending disconnect to terminal device.\n");
    }

    /* Wait until message is sent. Necessary to ensure that message is sent
     * before we terminate. */
    TERM_DEBUG("Waiting until disconnect message is sent.\n");
    while (client->connected) {
        err = event_dispatch(client->conf_ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Error dispatching events.");
        }
    }
}

/**
 * \brief Blocking read from a terminal.
 *
 * \param client  Terminal client state.
 * \param data    Buffer to hold read characters.
 * \param length  The number of characters to read.
 * \param read    Number of characters read. This might be less than length if
 *                line_mode is enabled and the end of line was reached or if an
 *                error occurred.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_IO if an I/O error occurred.
 *
 * Dispatches the read if no data is available.
 */
errval_t term_client_blocking_read(struct term_client *client, char *data,
                                   size_t length, size_t *read)
{
    errval_t err;
    bool eol_reached = false;

    assert(data != NULL);
    assert(length > 0);
    assert(read != NULL);

    /*
     * Copy as many characters to the user buffer as he requested but stop if
     * line mode is enabled and the end of line is reached.
     */
    while ((*read < length) && !(client->line_mode && eol_reached)) {

        if (client->readbuf == NULL) {

            /*
             * Dispatch events on the incoming interface until characters
             * arrive.
             */
            while (client->readbuf == NULL) {
                err = event_dispatch(client->read_ws);
                if (err_is_fail(err)) {
                    return err_push(err, TERM_ERR_IO);
                }
            }

            /* handle echo */
            if (client->echo) {
                err = handle_echo(client, client->readbuf, client->readbuf_len);
                if (err_is_fail(err)) {
                    return err_push(err, TERM_ERR_IO);
                }
            }

            /* handle triggers */
            handle_triggers(client, client->readbuf, client->readbuf_len);

            /* filter input */
            term_filter_apply(client->input_filters, &client->readbuf,
                              &client->readbuf_len);
        }

        /* copy data to user supplied buffer */
        char *end = client->readbuf + client->readbuf_len;
        while ((client->readbuf_pos < end) && (*read < length) &&
               !(client->line_mode && eol_reached)) {
            data[(*read)++] = *client->readbuf_pos;
            if (client->line_mode &&
                (*client->readbuf_pos == TERM_CLIENT_EOL_CHAR)) {
                eol_reached = true;
            }
            client->readbuf_pos++;
        }

        /* free readbuf */
        if (client->readbuf_pos == end) {
            free(client->readbuf);
            client->readbuf = NULL;
            client->readbuf_pos = NULL;
            client->readbuf_len = 0;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Blocking write to a terminal.
 *
 * \param client  Terminal client state.
 * \param data    Buffer holding characters to write.
 * \param length  The number of characters to write.
 * \param written Number of characters written. This might be less than length
 *                if an error occurred.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_IO if an I/O error occurred.
 *
 * Dispatches the write waitset until data is sent.
 */
errval_t term_client_blocking_write(struct term_client *client,
                                    const char *data, size_t length,
                                    size_t *written)
{
    errval_t err;
    char *outdata = NULL;

    assert(data != NULL);
    assert(length > 0);
    assert(written != NULL);

    /* Dispatch the outgoing waitset until we can send characters. */
    while (!client->out_binding->can_send(client->out_binding)) {
        err = event_dispatch(client->write_ws);
        if (err_is_fail(err)) {
            return err_push(err, TERM_ERR_IO);
        }
    }

    /* Make a copy of characters, since the output filters might modify them. */
    outdata = memdup(data, length);

    /* tell user how much we've written (before applying filters) */
    *written = length;

    /* apply output filters */
    term_filter_apply(client->output_filters, &outdata, &length);

    /* send characters */
    err = client->out_binding->tx_vtbl.characters(client->out_binding, NOP_CONT,
                                                  outdata, length);
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_IO);
        goto out;
    }

    /* Wait until characters are sent. */
    while (!client->out_binding->can_send(client->out_binding)) {
        err = event_dispatch(client->write_ws);
        if (err_is_fail(err)) {
            err = err_push(err, TERM_ERR_IO);
            goto out;
        }
    }

 out:
    /* reset amount written if error */
    if (err_is_fail(err)) {
        *written = 0;
    }
    /* free data */
    free(outdata);
    return err;
}

/**
 * \brief Send a configuration command to the terminal server.
 *
 * \param client Terminal client state.
 * \param opt    Configuration option.
 * \param arg    Optional argument.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_UNKNOWN_CONFIG_OPT if opt is unknown.
 *
 * Dispatches the config waitset until configuration message is sent.
 */
errval_t term_client_blocking_config(struct term_client *client,
                                     enum TerminalConfig opt, size_t arg)
{
    switch(opt) {
        case TerminalConfig_ECHO:
            client->echo = arg > 0;
            return SYS_ERR_OK;
        break;

        case TerminalConfig_ICRNL:
        {
            if (arg == false && client->cr2lf_id > 0) {
                errval_t err = term_client_remove_input_filter(client, client->cr2lf_id);
                if (err_is_ok(err)) {
                    client->cr2lf_id = 0;
                }
                return err;
            }
            else if(arg == true && client->cr2lf_id == 0) {
                term_filter_id_t id = term_client_add_input_filter(client, term_filter_cr2lf);
                client->cr2lf_id = id;
            }

            return SYS_ERR_OK;
        }
        break;

        case TerminalConfig_CTRLC:
        {
            if (arg == false && client->ctrlc_id > 0) {
                errval_t err = term_client_remove_trigger(client, client->ctrlc_id);
                if (err_is_ok(err)) {
                    client->ctrlc_id = 0;
                }
                return err;
            }
            else if(arg == true && client->ctrlc_id == 0) {
                client->ctrlc_id = term_client_add_trigger_type(client, term_trigger_int,
                                                                TERM_TRIGGER_TYPE_USER);
            }
            return SYS_ERR_OK;
        }
        break;


        default:
            return TERM_ERR_UNKNOWN_CONFIG_OPT;
    }


}

errval_t term_client_blocking_tcgetattr(struct term_client *client,
                                        struct termios* t)
{
    if (client->cr2lf_id > 0) {
        t->c_iflag = ICRNL;
    }
    if (client->echo) {
        t->c_lflag = ECHO;
    }

    return SYS_ERR_OK;
}

errval_t term_client_blocking_tcsetattr(struct term_client *client,
                                        const struct termios* t)
{
    errval_t err = term_client_blocking_config(client, TerminalConfig_ECHO, (t->c_lflag & ECHO) > 0);
    assert(err_is_ok(err));
    err = term_client_blocking_config(client, TerminalConfig_ICRNL, (t->c_iflag & ICRNL) > 0);
    assert(err_is_ok(err));

    return err;
}



/**
 * \privatesection
 * Internal function follow.
 */

/**
 * \brief Default asynchronous error handler.
 */
static void default_err_handler(void *st, errval_t err)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error in libterm_client.");
    }
}

/**
 * \brief Initialize client state with default values.
 */
static void struct_term_client_init(struct term_client *client)
{
    client->read_ws = malloc(sizeof(struct waitset));
    assert(client->read_ws != NULL);
    waitset_init(client->read_ws);
    client->write_ws = malloc(sizeof(struct waitset));
    assert(client->write_ws != NULL);
    waitset_init(client->write_ws);
    client->conf_ws = malloc(sizeof(struct waitset));
    assert(client->conf_ws);
    waitset_init(client->conf_ws);
    client->connected = false;
    client->echo = true;
    client->line_mode = true;
    client->non_blocking_read = false;
    client->chars_cb = NULL;
    client->err_cb = default_err_handler,
    client->in_binding = NULL;
    client->out_binding = NULL;
    client->conf_binding = NULL;
    client->readbuf = NULL;
    collections_list_create(&client->input_filters, term_filter_free);
    collections_list_create(&client->output_filters, term_filter_free);
    collections_list_create(&client->echo_filters, term_filter_free);
    client->max_input_filter_id = 0;
    client->max_output_filter_id = 0;
    client->max_echo_filter_id = 0;
    collections_list_create(&client->triggers, term_trigger_free);
    client->max_trigger_id = 0;

    /* add default input filters */
    client->cr2lf_id = term_client_add_input_filter(client, term_filter_cr2lf);

    /* add default output filters */
    client->lf2crlf_id = term_client_add_output_filter(client, term_filter_lf2crlf);

    /* add default echo filters */
    client->ctrlhat_id = term_client_add_echo_filter(client, term_filter_ctrlhat);

    /* add default triggers */
    term_client_add_trigger_type(client, term_trigger_kill,
                                 TERM_TRIGGER_TYPE_BUILT_IN);
    client->ctrlc_id = term_client_add_trigger_type(client, term_trigger_int,
                                                    TERM_TRIGGER_TYPE_USER);
}

/**
 * \brief Retrieve interface references for incoming and outgoing characters
 *        as well as for configuration messages from octopus.
 */
static errval_t get_irefs(struct capref session_id, iref_t *in_iref,
                          iref_t *out_iref, iref_t *conf_iref)
{
    errval_t err;

    struct octopus_binding *r = get_octopus_binding();
    assert(r != NULL);

    struct octopus_get_with_idcap_response__rx_args reply;

    err = r->rpc_tx_vtbl.get_with_idcap(r, session_id, NOP_TRIGGER, reply.output, &reply.tid,
                                 &reply.error_code);
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_LOOKUP_SESSION_RECORD);
        goto out;
    }
    err = reply.error_code;
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_LOOKUP_SESSION_RECORD);
        goto out;
    }

    TERM_DEBUG("Record retrieved from octopus: %s\n", record);

    int64_t session_oct;
    int64_t in_oct;
    int64_t out_oct;
    int64_t conf_oct;
    // oct_read can only parse 64-bit values, we need to parse the irefs as 64bit
    // then cast to 32bit
    err = oct_read(reply.output, "_ { session_iref: %d, in_iref: %d, out_iref: %d, "
                   "conf_iref: %d }", &session_oct, &in_oct, &out_oct,
                    &conf_oct);
    //iref_t session_iref = (iref_t)session_oct;
    *in_iref = (iref_t)in_oct;
    *out_iref = (iref_t)out_oct;
    *conf_iref = (iref_t)conf_oct;
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_PARSE_SESSION_RECORD);
        goto out;
    }
    if ((*in_iref == NULL_IREF) || (*out_iref == NULL_IREF) ||
        (*conf_iref == NULL_IREF)) {
        err = TERM_ERR_PARSE_SESSION_RECORD;
        goto out;
    }

    TERM_DEBUG("Retrieved interface references from octopus. in_iref: %"
               PRIuIREF ", out_iref: %" PRIuIREF ", conf_iref: %" PRIuIREF
               "\n", *in_iref, *out_iref, *conf_iref);

out:
    return err;
}

static void check_connection_established(struct term_client *client)
{
    if (client->in_binding == NULL || client->out_binding == NULL ||
        client->conf_binding == NULL) {
        /* Not all three connections are already established. */
        return;
    }

    client->connected = true;
}

static errval_t handle_echo(struct term_client *client, char *data,
                            size_t length)
{
    errval_t err = SYS_ERR_OK;
    char *echodata = NULL;

    assert(client != NULL);
    assert(data != NULL);
    assert(length > 0);

    /* Dispatch the outgoing waitset until we can send characters */
    while (!client->out_binding->can_send(client->out_binding)) {
        err = event_dispatch(client->write_ws);
        if (err_is_fail(err)) {
            return err;
        }
    }

    /*
     * Make a copy of the data, since the echo filters might modify it and the
     * modification should not be seen by the application.
     */
    echodata = memdup(data, length);

    /* apply echo filters */
    term_filter_apply(client->echo_filters, &echodata, &length);

    /* echo characters */
    err = client->out_binding->tx_vtbl.characters(client->out_binding, NOP_CONT,
                                                  echodata, length);
    if (err_is_fail(err)) {
        goto out;
    }

    /* Wait until characters echoed. */
    while (!client->out_binding->can_send(client->out_binding)) {
        err = event_dispatch(client->write_ws);
        if (err_is_fail(err)) {
            goto out;
        }
    }

out:
    /* free data*/
    free(echodata);
    return err;
}

static void handle_triggers(struct term_client *client, char *data,
                            size_t length)
{
    struct term_trigger *trigger = NULL;

    collections_list_traverse_start(client->triggers);

    while ((trigger = collections_list_traverse_next(client->triggers)) != NULL)
    {
        for (int i = 0; i < length; i++) {
            if (data[i] == trigger->trigger_character) {
                /* call closure associated with trigger */
                trigger->closure.handler(trigger->closure.arg);
            }
        }
    }

    collections_list_traverse_end(client->triggers);
}

static void in_characters_handler(struct terminal_binding *b, const char *data,
                                  size_t length)
{
    struct term_client *client = b->st;


    char *my_data = memdup(data, length);

    if (client->non_blocking_read) {
        assert(client->chars_cb != NULL);

        /* handle triggers */
        handle_triggers(client, my_data, length);

        /* filter input */
        term_filter_apply(client->input_filters, &my_data, &length);

        /* call user supplied chars_cb */
        client->chars_cb(client->st, my_data, length);
    } else {
        assert(client->readbuf == NULL);

        client->readbuf = my_data;
        client->readbuf_pos = my_data;
        client->readbuf_len = length;
    }
}

static void in_bind_cb(void *st, errval_t err, struct terminal_binding *b)
{
    struct term_client *client = st;

    if (err_is_fail(err)) {
        /* call async error callback */
        err = err_push(err, TERM_ERR_BIND_IN_INTERFACE);
        client->err_cb(client->st, err);
        return;
    }

    client->in_binding = b;
    b->st = client;
    b->rx_vtbl.characters = in_characters_handler;

    /* Check if all connections are already established. */
    check_connection_established(client);
}

static void out_characters_handler(struct terminal_binding *b, const char *data,
                                   size_t length)
{
    struct term_client *client = b->st;

    /*
     * It is an error if characters arrive at the interface for outgoing
     * characters.
     */
    client->err_cb(client->st, TERM_ERR_RECV_CHARS);
}

static void out_bind_cb(void *st, errval_t err, struct terminal_binding *b)
{
    struct term_client *client = st;

    if (err_is_fail(err)) {
        /* call async error callback */
        err = err_push(err, TERM_ERR_BIND_OUT_INTERFACE);
        client->err_cb(client->st, err);
        return;
    }

    client->out_binding = b;
    b->st = client;
    b->rx_vtbl.characters = out_characters_handler;

    /* Check if all connections are already established. */
    check_connection_established(client);
}

static void conf_configuration_handler(struct terminal_config_binding *b,
                                       terminal_config_option_t opt,
                                       const char *arguments)
{
    struct term_client *client = b->st;

    /*
     * Configuration messages only flow from the client to the server. It is
     * an error if the server send a configuration message to the client.
     */
    client->err_cb(client->st, TERM_ERR_RECV_CONFIGURATION);
}

static void conf_bind_cb(void *st, errval_t err,
                         struct terminal_config_binding *b)
{
    struct term_client *client = st;

    if (err_is_fail(err)) {
        /* call async error callback */
        err = err_push(err, TERM_ERR_BIND_CONF_INTERFACE);
        client->err_cb(client->st, err);
        return;
    }

    client->conf_binding = b;
    b->st = client;
    b->rx_vtbl.configuration = conf_configuration_handler;

    /* Check if all connections are already established. */
    check_connection_established(client);
}

static void exit_cb(void *arg)
{
    struct term_client *client = arg;

    client->connected = false;
    TERM_DEBUG("Disconnect message sucessfully sent to terminal server.\n");
}
