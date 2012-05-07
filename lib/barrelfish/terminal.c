/**
 * \file
 * \brief Terminal emulator.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/terminal.h>
#include <barrelfish/nameservice_client.h>
#include <if/serial_defs.h>
#include <if/keyboard_defs.h>
#include <string.h>

#define BUFSIZE         256
#define MAX_INPUT_HANDLERS  10

struct input_handler {
    terminal_input_handler      f;
    void                        *user_data;
};

struct terminal_state {
    bool want_stdin, serial_bound, kbd_bound;
    struct thread_mutex mutex;
    struct serial_binding *serial;
    struct keyboard_binding *kbd;
    struct waitset waitset;

    /* input */
    char stdin_buf[BUFSIZE];
    unsigned int produced;
    unsigned int consumed;
};

// this table maps keyboard scan codes to input characters
// XXX: this is completely bogus. we need termcap etc.
static char scancode_map[] = {
    [0x1E] = 'a',
    [0x30] = 'b',
    [0x2E] = 'c',
    [0x20] = 'd',
    [0x12] = 'e',
    [0x21] = 'f',
    [0x22] = 'g',
    [0x23] = 'h',
    [0x17] = 'i',
    [0x24] = 'j',
    [0x25] = 'k',
    [0x26] = 'l',
    [0x32] = 'm',
    [0x31] = 'n',
    [0x18] = 'o',
    [0x19] = 'p',
    [0x10] = 'q',
    [0x13] = 'r',
    [0x1F] = 's',
    [0x14] = 't',
    [0x16] = 'u',
    [0x2F] = 'v',
    [0x11] = 'w',
    [0x2D] = 'x',
    [0x15] = 'y',
    [0x2C] = 'z',
    [0x02] = '1',
    [0x03] = '2',
    [0x04] = '3',
    [0x05] = '4',
    [0x06] = '5',
    [0x07] = '6',
    [0x08] = '7',
    [0x09] = '8',
    [0x0a] = '9',
    [0x0b] = '0',
    [0x0c] = '_', // XXX: really -, but this is more useful :)
    [0x0d] = '=',
    [0x1a] = '[',
    [0x1b] = ']',
    [0x2b] = '\\',
    [0x27] = ';',
    [0x28] = '\'',
    [0x29] = '`',
    [0x33] = ',',
    [0x34] = '.',
    [0x35] = '/',

    // control characters
    [0x0e] = 0x08,    // back-space / delete?
    [0x1c] = '\r',    // enter
    [0x01] = 0x1b,    // escape
    [0x39] = ' ',     // space
    [0x0f] = '\t',    // tab1
};

static struct input_handler input_handlers[MAX_INPUT_HANDLERS];

static void handle_stdin(struct serial_binding *b, char *data, size_t len)
{
    terminal_input(data, len);
    free(data);
}

static struct serial_rx_vtbl serial_rx_vtbl = {
    .input = handle_stdin,
};

static void handle_key_event(struct keyboard_binding *b, uint8_t scancode,
                             bool extended)
{
    char sc = scancode < sizeof(scancode_map) ? scancode_map[scancode] : 0;
    if (sc) {
        terminal_input(&sc, 1);
    }
}

static struct keyboard_rx_vtbl keyboard_rx_vtbl = {
    .key_event = handle_key_event,
};

/// Filter output for serial driver
// just converts \n to \r\n for the moment
static void filter_output(char *outbuf, size_t outbuflen,
                          const char *in, size_t inlen)
{
    while (inlen--) {
        char c = *in++;
        if (c == '\n') {
            assert(outbuflen--);
            *outbuf++ = '\r';
        }
        assert(outbuflen--);
        *outbuf++ = c;
    }
}

/// Returns length of buffer required by filter_output()
static size_t filter_output_len(const char *in, size_t inlen)
{
    size_t outlen = 0;
    while (inlen--) {
        outlen++;
        if (*in++ == '\n') {
            outlen++;
        }
    }
    return outlen;
}

static void serial_write(struct terminal_state *st,
                         const char *data, size_t inlen)
{
    assert(st != NULL);
    assert(st->serial != NULL);
    errval_t err;

    size_t outlen = filter_output_len(data, inlen);
    // check sane size for buffer on stack; after all, this is the terminal!
    assert(outlen <= 4096);
    char outbuf[outlen];
    filter_output(outbuf, outlen, data, inlen);

    // try to send
    assert(st->serial->can_send(st->serial));
    err = st->serial->tx_vtbl.output(st->serial, NOP_CONT, outbuf, outlen);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending terminal output to serial driver");
    }

    // block on output completion.
    // this is necessary to maintain libc buffering semantics, and prevent
    // output being lost if a dispatcher exit()s after a printf
    while (!st->serial->can_send(st->serial)) {
        err = event_dispatch(&st->waitset);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch on terminal waitset");
        }
    }
}

size_t terminal_write(const char *data, size_t length)
{
    struct terminal_state *state = get_terminal_state();

    if (length > 0) {
        assert(data != NULL);

        if (state != NULL && state->serial != NULL) {
            thread_mutex_lock(&state->mutex);
            serial_write(state, data, length);
            thread_mutex_unlock(&state->mutex);
        } else {
            sys_print(data, length);
        }
    }

    return length;
}

size_t terminal_read(char *data, size_t count)
{
    struct terminal_state *state = get_terminal_state();
    errval_t err;
    size_t i;

    thread_mutex_lock(&state->mutex);

    for(i = 0; i < count; i++) {
        while(state->consumed == state->produced) {
            err = event_dispatch(&state->waitset);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "error in event_dispatch on terminal waitset");
            }
        }

        data[i] = state->stdin_buf[(state->consumed++) % BUFSIZE];
    }

    thread_mutex_unlock(&state->mutex);

    return count;
}

void terminal_input(char *data, size_t length)
{
    struct terminal_state *state = get_terminal_state();

    for (size_t i = 0; i < length; i++) {
        /* XXX: translate \r to \n in place */
        if (data[i] == '\r') {
            data[i] = '\n';
        }

        state->stdin_buf[(state->produced++) % BUFSIZE] = data[i];
        assert(state->produced != state->consumed); // FIXME!
    }

    for (int i = 0; i < MAX_INPUT_HANDLERS; i++) {
        if (input_handlers[i].f != NULL) {
            input_handlers[i].f(input_handlers[i].user_data, data, length);
        }
    }
}

static void serial_bind_cb(void *st, errval_t err, struct serial_binding *b)
{
    struct terminal_state *state = st;

    if (err_is_ok(err)) {
        b->rx_vtbl = serial_rx_vtbl;
        b->st = state;
        state->serial = b;
        if (state->want_stdin) {
            err = b->tx_vtbl.associate_stdin(b, NOP_CONT);
            assert(err_is_ok(err)); // XXX: can fail!!
        }
    } else {
        state->serial_bound = false;
        USER_PANIC_ERR(err, "error binding to serial driver");
    }
}

static void keyboard_bind_cb(void *st, errval_t err, struct keyboard_binding *b)
{
    struct terminal_state *state = st;

    if (err_is_ok(err)) {
        b->rx_vtbl = keyboard_rx_vtbl;
        b->st = state;
        state->kbd = b;
    } else {
        state->kbd_bound = false;
        USER_PANIC_ERR(err, "error binding to keyboard driver");
    }
}

errval_t terminal_init(void)
{
    memset(input_handlers, 0, sizeof(input_handlers));

    /* Allocate and initialize dispatcher-specific state */
    struct terminal_state *state = malloc(sizeof(struct terminal_state));
    if (!state) {
        return LIB_ERR_MALLOC_FAIL;
    }
    set_terminal_state(state);
    thread_mutex_init(&state->mutex);
    state->want_stdin = false;
    state->serial_bound = false;
    state->kbd_bound = false;
    state->produced = state->consumed = 0;
    state->serial = NULL;
    waitset_init(&state->waitset);

    iref_t iref;
    errval_t err;

    /* Connect to serial driver if possible */
    err = nameservice_lookup("serial", &iref);
    if (err_is_fail(err)) {
        if (err_no(err) == LIB_ERR_NAMESERVICE_UNKNOWN_NAME) {
            // serial not present, ignore it and continue
            return SYS_ERR_OK;
        } else {
            return err;
        }
    }

    err = serial_bind(iref, serial_bind_cb, state, &state->waitset,
                      IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SERIAL_BIND);
    }

    state->serial_bound = true;

    return SYS_ERR_OK;
}

errval_t terminal_want_stdin(unsigned sources)
{
    struct terminal_state *state = get_terminal_state();
    assert(state != NULL);

    iref_t iref;
    errval_t err;

    thread_mutex_lock(&state->mutex);

    state->want_stdin = true;

    if ((sources & TERMINAL_SOURCE_SERIAL) && !state->serial_bound) {
        // didn't connect at init time, try again, blocking on the lookup
        err = nameservice_blocking_lookup("serial", &iref);
        if (err_is_fail(err)) {
            goto out;
        }

        err = serial_bind(iref, serial_bind_cb, state, &state->waitset,
                          IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            err = err_push(err, LIB_ERR_SERIAL_BIND);
            goto out;
        }
        state->serial_bound = true;
    }

    // connect to keyboard driver if desired
    if ((sources & TERMINAL_SOURCE_KEYBOARD) && !state->kbd_bound) {
        err = nameservice_blocking_lookup("keyboard", &iref);
        if (err_is_fail(err)) {
            goto out;
        }

        err = keyboard_bind(iref, keyboard_bind_cb, state,
                            &state->waitset, IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            err = err_push(err, LIB_ERR_KBD_BIND);
            goto out;
        }
        state->kbd_bound = true;
    }

    // XXX: I don't believe this waiting is correct. It changes this from a 
    // non-blocking to a blocking API call. The caller should dispatch
    // the default waitset, and the bind will eventually complete. -AB
    while ((sources & TERMINAL_SOURCE_SERIAL) && state->serial == NULL) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "event_dispatch on default waitset failed.");
        }
    }

    if (sources & TERMINAL_SOURCE_SERIAL && state->serial != NULL) {
        err = state->serial->tx_vtbl.associate_stdin(state->serial, NOP_CONT);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "sending associate_stdin failed");
        }
    }

    err = SYS_ERR_OK;

 out:
    thread_mutex_unlock(&state->mutex);

    return err;
}

/**
 * \brief Register a handler to be called when input arrives at the terminal.
 */
errval_t terminal_register_input_handler (terminal_input_handler handler,
                                          void * user_data)
{
    for (int i = 0; i < MAX_INPUT_HANDLERS; i++) {
        if (input_handlers[i].f == NULL) {
            input_handlers[i].f = handler;
            input_handlers[i].user_data = user_data;
            return SYS_ERR_OK;
        }
    }

    return TERM_ERR_REGISTER_HANDLER;
}

/**
 * \brief Unregister a previously registered input handler.
 */
void terminal_unregister_input_handler (terminal_input_handler handler)
{
    for (int i = 0; i < MAX_INPUT_HANDLERS; i++) {
        if (input_handlers[i].f == handler) {
            input_handlers[i].f = NULL;
            input_handlers[i].user_data = NULL;
        }
    }
}
