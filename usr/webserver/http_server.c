/**
 * \file
 * \brief HTTP server
 *
 * \bug All calls to tcp_write currently use TCP_WRITE_FLAG_COPY, causing
 *   the data to be copied to LWIP's internal memory pool. This is necessary,
 *   because we lack the VM support necessary to do a reverse mapping for
 *   arbitrary memory regions.
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <sys/param.h>
#include <barrelfish/barrelfish.h>
#include <netinet/in.h>
#include <net_sockets/net_sockets.h>
#include <netbench/netbench.h>
#include <debug_log/debug_log.h>

#define LWIP_IPV4
#include <lwip/ip_addr.h>

#include "http_cache.h"
#include "webserver_network.h"
#include "webserver_debug.h"
#include "webserver_session.h"

#define HTTP_PORT       80

#define CRLF "\r\n"
#define HTTP_HEADER_COMMON "Server: Barrelfish" CRLF
#define HTTP_HEADER_200 "HTTP/1.0 200 OK" CRLF HTTP_HEADER_COMMON
#define HTTP_HEADER_404 "HTTP/1.0 404 Not Found" CRLF HTTP_HEADER_COMMON
#define HTTP_HEADER_500 "HTTP/1.0 500 Internal Server Error" CRLF HTTP_HEADER_COMMON

#define HTTP_MIME_HTML  "Content-type: text/html; charset=utf-8" CRLF
#define HTTP_MIME_GIF   "Content-type: image/gif" CRLF
#define HTTP_MIME_JPG   "Content-type: image/jpeg" CRLF
#define HTTP_MIME_PDF   "Content-type: application/pdf" CRLF
#define HTTP_MIME_TAR   "Content-type: application/x-tar" CRLF
#define HTTP_MIME_GZIP  "Content-type: application/x-gzip" CRLF
#define HTTP_MIME_BZIP2 "Content-type: application/x-bzip2" CRLF
#define HTTP_MIME_OCTET "Content-type: application/octet-stream" CRLF


static const char notfound_reply[] =
    HTTP_HEADER_404 HTTP_MIME_HTML CRLF
    "<html>" CRLF
    "<body>" CRLF
    "<h1>404 Not Found</h1>" CRLF
    "<p>The requested URL was not found.</p>" CRLF
    "</body>" CRLF
    "</html>" CRLF;

static const char error_reply[] =
    HTTP_HEADER_500 HTTP_MIME_HTML CRLF
    "<html>" CRLF
    "<body>" CRLF
    "<h1>500 Internal Server Error</h1>" CRLF
    "<p>Bad stuff happened. Damn.</p>" CRLF
    "</body>" CRLF
    "</html>" CRLF;

static const char header_html[] = HTTP_HEADER_200 HTTP_MIME_HTML CRLF;
static const char header_gif[] = HTTP_HEADER_200 HTTP_MIME_GIF CRLF;
static const char header_jpg[] = HTTP_HEADER_200 HTTP_MIME_JPG CRLF;
static const char header_pdf[] = HTTP_HEADER_200 HTTP_MIME_PDF CRLF;
static const char header_bz2[] = HTTP_HEADER_200 HTTP_MIME_BZIP2 CRLF;
static const char header_gz[] = HTTP_HEADER_200 HTTP_MIME_GZIP CRLF;
static const char header_octet[] = HTTP_HEADER_200 HTTP_MIME_OCTET CRLF;

#if 0

#define MAX_DURATIONS   1000

#define INST_BEGIN \
    static uint64_t _dursum = 0, _dur = 0;      \
    uint64_t _begin = rdtsc();

#define INST_END \
    _dursum += rdtsc() - _begin;                \
    _dur++;                                                     \
    if(_dur == MAX_DURATIONS) {                                 \
        DEBUGPRINT("%s: %lu\n", __func__, _dursum / MAX_DURATIONS);     \
        _dur = 0; _dursum = 0;                                      \
    }

#else

#define INST_BEGIN
#define INST_END

#endif

/* GLOBAL STATE */
static int parallel_connections = 0; /* number of connections alive at moment */
static int request_counter = 0;  /* Total no. of requests received till now */
/* above both are for debugging purpose only */



static struct http_conn *http_conn_new(void)
{
    struct http_conn *newconn = malloc(sizeof(struct http_conn));
    assert (newconn != NULL);
    memset (newconn, 0, sizeof(struct http_conn));

    newconn->state = HTTP_STATE_NEW;
    newconn->request_no = request_counter++;

    DEBUGPRINT ("%d: http_conn created [p %d] %lu %lu\n", newconn->request_no,
        parallel_connections, newconn->header_pos, newconn->header_length);
    ++parallel_connections;
    return newconn;
}


static void http_conn_free(struct http_conn *conn)
{
    DEBUGPRINT ("%d: http_conn_free freeing [p %d]\n", conn->request_no,
        parallel_connections);

    if(conn->request != NULL) {
        free(conn->request);
    }
    /* decrementing the reference to buff_holder */
    decrement_buff_holder_ref (conn->hbuff);
    free(conn);
    --parallel_connections;
}

/* increments the reference counter and returns the incremented value */
long increment_http_conn_reference (struct http_conn *cs)
{
    ++cs->ref_count;
    return (cs->ref_count);
} /* end function: increment_http_conn_reference  */

/* This function decrements the references to http_conn
 * and if references reach 0, the memory of struct is released. */
long decrement_http_conn_reference (struct http_conn *cs)
{
    --cs->ref_count;
    if (cs->mark_invalid) {
        /* connection is no longer valid */
        if (cs->ref_count <= 0) {
            /* no one is using the connection */
            /* so, free up the the memory */
            http_conn_free(cs);
            return 0;
        }
    } /* end if : invalid http_conn */
    return cs->ref_count;
} /* end function: decrement_reference */


static void http_conn_invalidate (struct http_conn *conn)
{
    DEBUGPRINT ("%d: http_conn_invalidate\n", conn->request_no);
    conn->mark_invalid = 1;
    decrement_http_conn_reference (conn);
}


// static void http_server_err(void *arg, errval_t err)
// {
//     struct http_conn *conn = arg;
//
//     DEBUGPRINT("http_server_err! %p %d\n", arg, err);
//     if(conn != NULL) {
//         DEBUGPRINT("%d: http_server_err! %p %d\n", conn->request_no, arg, err);
//         http_conn_invalidate (conn);
//     } else {
//         DEBUGPRINT("http_server_err! %p %d\n", arg, err);
//     }
// }
//
//
static void http_server_close(struct net_socket *socket, struct http_conn *cs)
{
/*
    printf("%s %s %s %hu.%hu.%hu.%hu in %"PU"\n",
            cs->hbuff->data ? "200" : "404", cs->request, cs->filename,
           ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
           ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip),
            in_seconds(get_time_delta(&cs->start_ts)));
*/
    // debug_printf_to_log("%s(%d): %p", __func__, socket->descriptor, __builtin_return_address(0));
    DEBUGPRINT("%d: http_server_close freeing the connection\n",
        cs->request_no);

    assert(cs);
    // debug_printf("%s(%d):\n", __func__, socket->descriptor);
    net_close(socket);
}

static void http_server_closed(void *arg, struct net_socket *socket)
{
    // debug_printf("%s(%d):\n", __func__, socket->descriptor);
    struct http_conn *cs = arg;
    http_conn_invalidate(cs);
}

static errval_t trysend(struct net_socket *socket, const void *data, size_t *len, bool
more)
{
    size_t sendlen;
    errval_t err;

    for (sendlen = 0; sendlen < *len;) {
        void *buffer;
        size_t s = *len - sendlen;
        s = s > 16000 ? 16000: s;

        buffer = net_alloc(s);
        if (!buffer)
            break;
        memcpy(buffer, data + sendlen, s);
        err = net_send(socket, buffer, s);
        assert(err_is_ok(err));
        sendlen += s;
    }
    *len = sendlen;
    return SYS_ERR_OK;
}

static void http_send_data(struct net_socket *socket, struct http_conn *conn)
{
    errval_t err;
    const void *data;
    size_t len;

// debug_printf_to_log("%s(%d): %p %d", __func__, socket->descriptor, conn, conn->state);
    switch (conn->state) {
    case HTTP_STATE_SENDHEADER:
        DEBUGPRINT ("%d: http_send_data: header_pos %lu < header_len %lu\n",
            conn->request_no, conn->header_pos, conn->header_length);
        assert(conn->header_pos < conn->header_length);
        data = &conn->header[conn->header_pos];
        len = conn->header_length - conn->header_pos;
 // debug_printf_to_log("%s(%d): header %zd:%zd", __func__, socket->descriptor, conn->header_pos, len);
        err = trysend(socket, data, &len, (conn->hbuff->data != NULL));
        if (err != SYS_ERR_OK) {
            DEBUGPRINT("http_send_data(): Error %d sending header\n", err);
            return; // will retry
        }

        conn->header_pos += len;
        DEBUGPRINT ("%d: http_send_data incr: hdr_pos %lu < hdr_len %lu\n",
                conn->request_no, conn->header_pos, conn->header_length);
        if (conn->header_pos == conn->header_length) {
            conn->state = HTTP_STATE_SENDFILE; // fall through below
            conn->reply_pos = 0;
            conn->reply_sent = 0;
        } else {
            break;
        }

    case HTTP_STATE_SENDFILE:
        if (conn->hbuff->data == NULL) {
            conn->state = HTTP_STATE_CLOSING;
            break;
        }
        data = conn->hbuff->data + conn->reply_pos; /* pointer arithmatic */
        len = conn->hbuff->len - conn->reply_pos;
        size_t maxlen = 16000 - (conn->reply_pos - conn->reply_sent);
        // debug_printf("%s: %zd %zd\n", __func__, len, maxlen);
        if (len > maxlen)
            len = maxlen;
 // debug_printf_to_log("%s(%d): file %zd:%zd", __func__, socket->descriptor, conn->reply_pos, len);
        err = trysend(socket, data, &len, false);
        if (err != SYS_ERR_OK) {
            DEBUGPRINT("http_send_data(): Error %d sending payload\n", err);
            return; // will retry
        }
        conn->reply_pos += len;
// debug_printf_to_log("%s(%d): %zd %zd\n", __func__, socket->descriptor, conn->reply_pos, conn->hbuff->len);
        if (conn->reply_pos == conn->hbuff->len) {
            conn->state = HTTP_STATE_CLOSING;
            http_server_close(socket, conn);
        }
        break;

    default:
        DEBUGPRINT ("http_send_data(): Wrong state! (%d)\n", conn->state);
        break;
    }
}

/* This function is called periodically from TCP.
 * and is also responsible for taking care of stale connections.
**/
// static errval_t http_poll(void *arg, struct net_socket *socket)
// {
//     struct http_conn *conn = arg;
//
//     if (conn == NULL && socket->state == ESTABLISHED) {
//         tcp_abort(socket);
//         return ERR_ABRT;
//     } else if (conn != NULL && (conn->state == HTTP_STATE_SENDHEADER
//                                 || conn->state == HTTP_STATE_SENDFILE)) {
//         if (++conn->retries == 4) {
//             DEBUGPRINT ("connection closed, tried too hard\n");
//             http_conn_invalidate (conn);
//             net_delete_socket(socket);
//             return ERR_ABRT;
//         }
//         http_send_data(socket, conn);
//         if (conn->state == HTTP_STATE_CLOSING) {
//             DEBUGPRINT ("%d: http_poll closing the connection\n",
//                     conn->request_no);
//             http_server_close(socket, conn);
//         } else {
//             // tcp_output(socket);
//         }
//     } else if (conn != NULL && (conn->state == HTTP_STATE_NEW
//                                 || conn->state == HTTP_STATE_REQUEST)) {
//         /* abort connections that sit open for too long without sending a
// request */
//         if (++conn->retries == 60) {
//             DEBUGPRINT("connection in state %d too long, aborted\n",
//                          conn->state);
//             DEBUGPRINT("connection in state %d too long, aborted\n",
//                         conn->state);
//
//             http_conn_invalidate (conn);
//             net_delete_socket(socket);
//             return ERR_ABRT;
//         }
//     }
//     return SYS_ERR_OK;
// } /* end function: http_poll */

/* called when data is successfully sent */
static void http_server_sent(void *arg, struct net_socket *socket, void *buffer, size_t size)
{
    struct http_conn *conn = arg;

    // debug_printf("%s(%d): %d\n", __func__, socket->descriptor, conn->state == HTTP_STATE_CLOSING);
    // debug_printf_to_log("%s(%d):", __func__, socket->descriptor);
    assert(conn);
    net_free(buffer);

    // debug_printf("%s: %zd  %zd:%zd  %zd:%zd\n", __func__, size, conn->header_pos, conn->header_sent, conn->reply_pos, conn->reply_sent);
    if (conn->header_sent < conn->header_pos)
        conn->header_sent += size;
    else
        conn->reply_sent += size;

    switch(conn->state) {
    case HTTP_STATE_SENDHEADER:
    case HTTP_STATE_SENDFILE:
        // Need to send more data?
        http_send_data(socket, conn);
        break;
    case HTTP_STATE_CLOSING:
        DEBUGPRINT("%d: http_server_sent closing the connection\n",
                    conn->request_no);
// debug_printf_to_log("%s(%d): %ld:%ld  %ld:%ld", __func__, socket->descriptor, conn->header_pos, conn->header_sent, conn->reply_pos, conn->reply_sent);
        // if (conn->header_pos == conn->header_sent && conn->reply_pos == conn->reply_sent) {
// debug_printf("%s.%d: %zd\n", __func__, __LINE__, size);
            // http_server_close(socket, conn);
        // }
        break;

    default:
        break;
    }
}

static const void *make_header(const char *uri, size_t *retlen)
{
    /* FIXME: hack to guess MIME type */
    size_t urilen = strlen(uri);
    if (strcmp(uri + urilen - 5, ".html") == 0) {
        *retlen = sizeof(header_html) - 1; // -1 for '\0'
        return header_html;
    } else if (strcmp(uri + urilen - 4, ".gif") == 0) {
        *retlen = sizeof(header_gif) - 1;
        return header_gif;
    } else if (strcmp(uri + urilen - 4, ".jpg") == 0) {
        *retlen = sizeof(header_jpg) - 1;
        return header_jpg;
    } else if (strcmp(uri + urilen - 4, ".pdf") == 0) {
        *retlen = sizeof(header_pdf) - 1;
        return header_pdf;
    } else if (strcmp(uri + urilen - 4, ".bz2") == 0) {
        *retlen = sizeof(header_bz2) - 1;
        return header_bz2;
    } else if (strcmp(uri + urilen - 3, ".gz") == 0) {
        *retlen = sizeof(header_gz) - 1;
        return header_gz;
    } else {
        *retlen = sizeof(header_octet) - 1;
        return header_octet;
    }
}

/* callback function to fetch file
    This function is responsible for sending the fetched file */
static void send_response(struct http_conn *cs)
{

    if (cs->error) {
        DEBUGPRINT ("%d: BIGERROR Sending the response back of size %lu\n",
					cs->request_no, cs->reply_pos);
        DEBUGPRINT("%s %s %s %hu.%hu.%hu.%hu\n", "500",
               cs->request, cs->filename,
               ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
               ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip));

        cs->header = error_reply;
        cs->header_length = sizeof(error_reply) - 1;
        cs->header_sent = 0;
    } else {
        DEBUGPRINT ("%d: Sending the response back of size %lu\n",
                cs->request_no, cs->reply_pos);
        DEBUGPRINT("%s %s %s %hu.%hu.%hu.%hu\n", cs->hbuff->data ?
                "200" : "404", cs->request, cs->filename,
               ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
               ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip));

        if (cs->hbuff->data == NULL) {
            /* not found, send 404 */
            DEBUGPRINT ("%d: making 404 case\n",cs->request_no);
            DEBUGPRINT ("witness: header_pos %lu < header_len %lu\n",
                cs->header_pos, cs->header_length);

            cs->header = notfound_reply;
            cs->header_length = sizeof(notfound_reply) - 1;
            cs->header_sent = 0;
        } else {
            /* found, send static header */
            cs->header = make_header(cs->filename, &cs->header_length);
            cs->header_sent = 0;
        }
    } /* end else: internal error */

    /* send data */
    cs->state = HTTP_STATE_SENDHEADER;
    cs->retries = 0;
    http_send_data(cs->pcb, cs);

    /* did we send the whole page? */
    // if (cs->state == HTTP_STATE_CLOSING) {
    //     DEBUGPRINT("%d: send_response closing the connection\n",
    //             cs->request_no);
// debug_printf("%s.%d:\n", __func__, __LINE__);
                                // http_server_close(cs->pcb, cs);
    // } else {
        // tcp_output(cs->pcb);
    // }
} /* end function: send_response */

// static errval_t http_server_recv(void *arg, struct net_socket *socket, struct pbuf *p,
//                               errval_t err);
//
static void http_server_recv(void *arg, struct net_socket *socket, void *data, size_t size, struct in_addr ip_address, uint16_t port)
{
    struct http_conn *conn = arg;

    DEBUGPRINT("%d, http_server_recv called\n", conn->request_no);
    // debug_printf_to_log("%s(%d): %ld %d\n", __func__, socket->descriptor, size, conn->state);

    // check if connection closed
    assert(conn);
    if (size == 0) {
        DEBUGPRINT("%d, closing from http_server_recv\n", conn->request_no);
// debug_printf("%s.%d:\n", __func__, __LINE__);
        conn->state = HTTP_STATE_CLOSING;
        http_server_close(socket, conn);
        return;
    }

    switch(conn->state) {
    case HTTP_STATE_NEW:
        conn->state = HTTP_STATE_REQUEST;
        // Fall through...

    case HTTP_STATE_REQUEST:
        /* don't send an immediate ack here, do it later with the data */
        // socket->flags &= ~(TF_ACK_DELAY | TF_ACK_NOW);

        /* accumulate the request data */
        conn->request_length += size;
        conn->request = realloc(conn->request, conn->request_length + 1);
        char *d = conn->request + conn->request_length - size;
        memcpy(d, data, size);
        d += size;
        *d = '\0';

        // pbuf_free(p);

        // have we seen the end of the request yet?
        if (strstr(conn->request, CRLF CRLF) == NULL) {
            break;
        }

        // ignore everything after the first line
        char *cp = strstr(conn->request, CRLF);
        assert(cp != NULL);
        *cp = '\0';

        // Parse request: break into method and URI
        cp = strchr(conn->request, ' ');
        if (cp == NULL) {
            goto invalid;
        }
        *cp = '\0';
        const char *uri = cp + 1;
        cp = strrchr(uri, ' ');
        if (cp == NULL) {
            goto invalid;
        }
        *cp = '\0';

        if (strcmp(conn->request, "GET") != 0) {
            goto invalid;
        }

        // drop a leading /
        if (uri[0] == '/') {
            uri++;
        }

        // if URI is now empty, look for index.html
        if (uri[0] == '\0') {
            uri = "index.html";
        }

        conn->filename = (char *)uri;
        conn->callback = send_response;
        conn->pcb = socket;
        conn->start_ts = rdtsc();
        /* for callback execution */
        errval_t e = http_cache_lookup(uri, conn);
        if (e != SYS_ERR_OK) {
            conn->error = 1;
            send_response(conn);
        }
        break;

    default:
        DEBUGPRINT("http_server_recv(): data received in wrong state (%d)!\n",
                     conn->state);
        conn->error = 1;
        send_response(conn);
        break;
    }
    return;

invalid:
    DEBUGPRINT("invalid request: %s\n", conn->request);
    DEBUGPRINT("%d: invalid request: %s\n",conn->request_no, conn->request);
    conn->state = HTTP_STATE_CLOSING;
    http_server_close(socket, conn);
    return;
}

static void http_server_accept(void *arg, struct net_socket *socket)
{
// #if TCP_LISTEN_BACKLOG
//     /* Decrease the listen backlog counter */
//     struct tcp_pcb_listen *lpcb = (struct tcp_pcb_listen*)arg;
// #endif
    // debug_printf_to_log("%s(%d):", __func__, socket->descriptor);
    struct http_conn *conn = http_conn_new();
    DEBUGPRINT("accpet called: %s\n", conn->request);
    increment_http_conn_reference (conn);
    /* NOTE: This initial increment marks the basic assess and it will be
        decremented by http_server_invalidate */

    net_set_user_state(socket, conn);
    net_set_on_received(socket, http_server_recv);
    net_set_on_sent(socket, http_server_sent);
    net_set_on_closed(socket, http_server_closed);

    // tcp_err(socket, http_server_err);
    // tcp_poll(socket, http_poll, 4);
}


static void realinit(void)
{
    uint64_t ts = rdtsc();
    struct net_socket *pcb = net_tcp_socket();
//    err_t e = tcp_bind(pcb, IP_ADDR_ANY, (HTTP_PORT + disp_get_core_id()));
    errval_t e = net_bind(pcb, (struct in_addr){(INADDR_ANY)}, HTTP_PORT);
    assert(e == SYS_ERR_OK);

    e = net_listen(pcb, 100);
    assert(e == SYS_ERR_OK);

    net_set_on_accepted(pcb, http_server_accept);
    printf("HTTP setup time %"PU"\n", in_seconds(get_time_delta(&ts)));
    printf("#######################################################\n");
    printf("Starting webserver\n");
    printf("#######################################################\n");

}

void http_server_init(struct in_addr server, const char *path)
{
    http_cache_init(server, path, realinit);
}


uint64_t get_time_delta(uint64_t *l_ts)
{
    uint64_t ct = rdtsc();
    uint64_t delta = ct - *l_ts;
    *l_ts = ct;
    return delta;
    //  return delta / (2800 * 1000);
} // end function: get_time_delta
