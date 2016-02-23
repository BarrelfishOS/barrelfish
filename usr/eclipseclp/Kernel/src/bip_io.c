/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * VERSION	$Id: bip_io.c,v 1.22 2015/01/14 01:31:09 jschimpf Exp $
 */

/****************************************************************************
 *
 *		SEPIA Built-in Predicates: I/O
 *
 *
 *****************************************************************************/

#include 	"config.h"
#include 	"os_support.h"

#include	<errno.h>
#include	<stdio.h>
//#include	<memory.h>
#include	<sys/types.h>
#include 	<sys/stat.h>

#ifdef HAVE_SYS_PARAM_H
#include 	<sys/param.h>
#endif

#if defined(BARRELFISH)
#include <barrelfish/debug.h>
#endif

#if defined(HAVE_UNISTD_H)
#include	<unistd.h>
#endif

#if defined(HAVE_SYS_SELECT_H)
#include	<sys/select.h>
#endif

#ifdef HAVE_VFORK_H
#include	<vfork.h>
#endif

#if HAVE_STRING_H
#  include <string.h>
#  ifdef MEMCPY_STRING
#    define bcopy(s1, s2, n)	(void) memcpy((void *)(s2),(void *)(s1), n)
#  endif
#endif
#ifdef MEMCPY_MEMORY
#  define bcopy(s1, s2, n)	(void) memcpy((char *)(s2), (char *)(s1), n)
extern char	*strcpy(),
		*strncpy(),
		*strcat(),
		*strerror();
#endif

#ifdef _WIN32
#include	<windows.h>
#include	<process.h>
#else
#include <sys/wait.h>
#endif

#ifdef SOCKETS
#ifdef _WIN32

#define StreamCanSignal(nst)	IsSocket(nst)

typedef SOCKET socket_t;

#else

#define StreamCanSignal(nst)	(IsSocket(nst) || IsPipeStream(nst))

#define INVALID_SOCKET (-1)
typedef int socket_t;
#include	<sys/socket.h>
#include	<sys/time.h>
#ifdef HAVE_AF_UNIX
#include	<sys/un.h>
#endif
#include	<netinet/in.h>
#include	<netdb.h>

#endif	/*_WIN32*/

#elif defined(BARRELFISH)
#define StreamCanSignal(x) 0
#else	/*SOCKETS*/
#undef S_ISSOCK
#define S_ISSOCK(m)	0
#endif	/*SOCKETS*/

#if defined(S_IFSOCK) && !defined (S_ISSOCK)
#  define S_ISSOCK(m)	(((m)&S_IFMT) == S_IFSOCK)
#  define S_ISFIFO(m)	(((m)&S_IFMT) == S_IFIFO)
#endif

#include	<fcntl.h>

/* directory access (see autoconf manual) */

#if HAVE_DIRENT_H
#  include	<dirent.h>
#  define	HAVE_READDIR
#else
#  if HAVE_SYS_NDIR_H
#    include	<sys/ndir.h>
#    define	HAVE_READDIR
#  endif
#  if HAVE_SYS_DIR_H
#    include	<sys/dir.h>
#    define	HAVE_READDIR
#  endif
#  if HAVE_NDIR_H
#    include	<ndir.h>
#    define	HAVE_READDIR
#  endif
#  if !defined(dirent)
#    define dirent	direct
#  endif
#endif


#include        "sepia.h"
#include        "types.h"
#include	"embed.h"
#include        "mem.h"
#include        "error.h"
#include        "ec_io.h"
#include	"dict.h"
#include	"lex.h"
#include	"emu_export.h"
#include	"property.h"

/* constants which are the same everywhere, but whose symbolic names vary */
#define ACCESS_OK	0

#define StreamCanRaiseEvent(nst) (IsQueueStream(nst) || StreamCanSignal(nst))

#define GetStreamProperty(functor)	\
    get_property(functor, STREAM_PROP)

#define Bind_Stream(v, t, s)				\
	if (IsAtom(t) || IsNil(t)) {			\
	    int _res;					\
	    if ((_res = set_stream(IsNil(t) ? d_.nil : (v).did, s)) < 0)	\
		{ Bip_Error(_res); }			\
	} else {					\
	    pword hstream = StreamHandle(s);		\
	    Bind_Var(v, t, hstream.val.all, hstream.tag.kernel);	\
	}

#define MAX_ARGS	30

struct pipe_desc {
    int		fd[2];
    int		fd_orig;	/* needed for Windows (no fork) */
    pword	pw;
    int		flags;
};

#define MAX_PIPES	32
#define EXEC_PIPE_CON	 1		/* connect it?			*/
#define EXEC_PIPE_SIG	 2		/* make it a SIGIO stream	*/
#define EXEC_PIPE_IN	 4		/* input			*/
#define EXEC_PIPE_OUT	 8		/* output			*/
#define EXEC_PIPE_LAST	 16		/* end marker, last fd used	*/


#ifdef _WIN32
/*
 * On Windows, maintain a list of child process handles to prevent the
 * processes from disappearing before they have been waited for
 * (Windows doesn't have zombies)
 */
typedef struct child_desc {
    struct child_desc	*next;
    struct child_desc	**prev_next;
    int			pid;
    HANDLE		hProcess;
} t_child_desc;

static t_child_desc	*child_processes = 0;

#define Child_Unlink(pd) { \
	if (pd) { \
	    *pd->prev_next = pd->next; \
	    hp_free_size(pd, sizeof(t_child_desc)); \
	} \
}
#endif


extern pword		*empty_string;
extern t_ext_type	heap_event_tid;
extern int		ec_sigio;

static dident		d_pipe,
			d_fd,
			d_fd1,
			d_false,
			d_force1,
			d_dup1,
			d_sigio,
			d_in,
			d_out,
			d_at,
			d_not,
			d_past,
			d_eof_code,
			d_socket,
			d_queue,
			d_queue1,
			d_unix,
			d_internet,
			d_stream,
			d_datagram,
			d_end_of_line,
			d_lf,
			d_crlf,
			d_when_lost,
			d_when_closed,
			d_reprompt1,
			d_block;

static dident		modes[SMODEBITS + 1];
static dident		stream_types[STYPE_NUM];
static dident		stream_encodings[SENC_NUM];

#ifdef __STDC__

static int		_check_stream(value, type, pword *, int),
			_check_streams(value, type, struct pipe_desc *),
			_match(char *, char *);
static void		_get_args(char *cmd, char *argv[]);

#else /* __STDC__ */

static int		_check_stream(),
			_check_streams(),
			_match();
static void		_get_args();

#endif /* __STDC__ */

static int		_open_pipes(struct pipe_desc *pipes);
static void		_close_pipes(struct pipe_desc *pipes);
#ifndef _WIN32
static void		_connect_pipes(struct pipe_desc *pipes);
#endif

static int     		p_nl(value vs, type ts), 
			p_open(value vfile, type tfile, value vmode, type tmode, value vstr, type tstr), 
			p_erase_stream_property(value v, type t),
			p_close(value v, type t),
			p_close2(value v, type t, value vopt, type topt),
			p_tyo(value vs, type ts, value v, type t),
			p_tyi(value vs, type ts, value v, type t), 
			p_delete(value v, type t), 
			p_mkdir(value v, type t), 
			p_rename(value vo, type to, value vd, type td), 
			p_get_prompt(value iv, type it, value pv, type pt, value ov, type ot),
			p_set_prompt(value iv, type it, value pv, type pt, value ov, type ot),
			p_is_open_stream(value vc, type tc),
			p_check_valid_stream(value v, type t),
			p_check_stream_spec(value v, type t),
			p_set_stream(value ov, type ot, value nv, type nt),
			p_read_string(value vs, type ts, value vdel, type tdel, value vl, type tl, value val, type tag),
			p_read_string5(value vs, type ts, value vdel, type tdel, value vpad, type tpad, value vsep, type tsep, value val, type tag),
			p_at(value vs, type ts, value vp, type tp),
			p_get_char(value vs, type ts, value val, type tag),
			p_get(value vs, type ts, value val, type tag),
			p_get1(value val, type tag),
			p_put_char(value vs, type ts, value val, type tag),
			p_put(value vstr, type tstr, value v, type t),
			p_put1(value v, type t),
			p_getw(value vs, type ts, value val, type tag),
			p_unget(value vs, type ts),
			p_flush(value sv, type st),
			p_at_eof(value vs, type ts),
			p_read_dir(value vdir, type tdir, value vpat, type tpat, value vsubdirs, type tsubdirs, value vfiles, type tfiles),
			p_socket(value vdom, type tdom, value vtp, type ttp, value vs, type ts),
			p_bind(value v, type t, value vaddr, type taddr),
			p_connect(value v, type t, value vaddr, type taddr),
			p_accept(value v, type t, value vaddr, type taddr, value vs, type ts),
			p_listen(value v, type t, value vn, type tn),
			p_select(value vin, type tin, value vtime, type ttime, value vout, type tout),
			p_pipe(value valr, type tagr, value valw, type tagw),
			p_exec(value vc, type tc, value vstr, type tstr, value vp, type tp, value vpr, type tpr),
			p_wait(value pv, type pt, value sv, type st, value vmode, type tmode),
#if defined(HAVE_READLINE)
			p_readline(),
#endif
			p_stream_number(value val1, type tag1),
			p_get_stream(value vi, type ti, value vs, type ts),
			p_next_open_stream(value v1, type t1, value v2, type t2),
			p_seek(value vs, type ts, value vp, type tp),
			p_stream_truncate(value vs, type ts),
			p_stream_info_(value vs, type ts, value vi, type ti, value v, type t),
			p_set_stream_prop_(value vs, type ts, value vi, type ti, value v, type t);


void
bip_io_init(int flags)
{
    d_fd = in_dict("fd", 0);
    d_fd1 = in_dict("fd", 1);
    d_false = in_dict("false", 0);
    d_force1 = in_dict("force", 1);
    d_dup1 = in_dict("dup", 1);
    d_sigio = in_dict("sigio", 1);
    d_in = in_dict("in", 1);
    d_out = in_dict("out", 1);
    d_at = in_dict("at", 0);
    d_not = in_dict("not", 0);
    d_past = in_dict("past", 0);
    d_eof_code = in_dict("eof_code", 0);
    d_queue1 = in_dict("queue", 1);
    d_unix = in_dict("unix", 0);
    d_internet = in_dict("internet", 0);
    d_stream = in_dict("stream", 0);
    d_datagram = in_dict("datagram", 0);
    d_reprompt1 = in_dict("reprompt", 1);
    d_block = in_dict("block", 0);
    d_end_of_line = in_dict("end_of_line", 0);
    d_lf = in_dict("lf", 0);
    d_crlf = in_dict("crlf", 0);
    d_when_lost = in_dict("when_lost", 0);
    d_when_closed = in_dict("when_closed", 0);

    modes[SCLOSED] = in_dict("closed",0);
    modes[SREAD] = d_.read;
    modes[SWRITE] = d_.write;
    modes[SRDWR] = d_.update;
    modes[SAPPEND|SCLOSED] = in_dict("invalid",0);
    modes[SAPPEND|SREAD] =  in_dict("invalid",0);
    modes[SAPPEND|SWRITE] = d_.append;
    modes[SAPPEND|SRDWR] =  in_dict("invalid",0);

    stream_types[SFILE>>STYPE_SHIFT] = in_dict("file", 0);
    stream_types[SSTRING>>STYPE_SHIFT] = d_.string0;
    stream_types[SPIPE>>STYPE_SHIFT] = d_pipe = in_dict("pipe", 0);
    stream_types[SQUEUE>>STYPE_SHIFT] = d_queue = in_dict("queue", 0);
    stream_types[SNULL>>STYPE_SHIFT] = d_.null;
    stream_types[SSOCKET>>STYPE_SHIFT] = d_socket = in_dict("socket", 0);
    stream_types[STTY>>STYPE_SHIFT] = in_dict("tty", 0);

    stream_encodings[SENC_OCTET] = in_dict("octet", 0);
    stream_encodings[SENC_ASCII] = in_dict("ascii", 0);
    stream_encodings[SENC_LATIN1] = in_dict("iso_latin_1", 0);

#ifdef _WIN32
    if (flags & INIT_PRIVATE)
    {
	child_processes = NULL;
    }
#endif

    if (flags & INIT_SHARED)
    {
	(void) built_in(in_dict("nl", 1),	p_nl, B_SAFE);
	(void) built_in(in_dict("open", 3),	p_open, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("close", 1),	p_close, B_SAFE);
	(void) built_in(in_dict("close", 2),	p_close2, B_SAFE);
	(void) built_in(in_dict("tyo", 2),	p_tyo, B_SAFE);
	(void) built_in(in_dict("tyi", 2),	p_tyi, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("delete", 1),	p_delete, B_SAFE);
	(void) built_in(in_dict("mkdir", 1),	p_mkdir, B_SAFE);
	(void) built_in(in_dict("rename", 2),	p_rename, B_SAFE);
	built_in(in_dict("get_prompt", 3),	p_get_prompt, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT);
	(void) built_in(in_dict("set_prompt", 3),	p_set_prompt, B_UNSAFE);
	(void) local_built_in(in_dict("is_open_stream", 1),
			p_is_open_stream, B_SAFE);
	(void) local_built_in(in_dict("check_valid_stream", 1),
			p_check_valid_stream, B_SAFE);
	(void) local_built_in(in_dict("check_stream_spec", 1),
			p_check_stream_spec, B_SAFE);
	(void) built_in(in_dict("get_stream",2),	p_get_stream, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("set_stream",2),	p_set_stream, B_SAFE);
	(void) built_in(in_dict("seek",2),	p_seek, B_SAFE);
	(void) built_in(in_dict("stream_truncate",1),	p_stream_truncate, B_SAFE);
	(void) built_in(in_dict("at",2),	p_at, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("get_char",2),	p_get_char, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("get", 2),	p_get,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("get", 1),	p_get1,	B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("unget",1),	p_unget, B_SAFE);
	(void) built_in(in_dict("put_char",2),	p_put_char, B_SAFE);
	(void) built_in(in_dict("put", 2),	p_put, B_SAFE);
	(void) built_in(in_dict("put", 1),	p_put1, B_SAFE);
	(void) exported_built_in(in_dict("getw", 2),	p_getw, B_UNSAFE|U_SIMPLE);
	(void) built_in(in_dict("at_eof",1),	p_at_eof, B_SAFE);
	(void) built_in(in_dict("flush", 1),		p_flush,	B_SAFE);
	(void) local_built_in(in_dict("stream_number", 1),
			p_stream_number, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("stream_info_", 3), p_stream_info_, B_UNSAFE|U_SIMPLE);
	(void) local_built_in(in_dict("set_stream_prop_", 3), p_set_stream_prop_, B_SAFE);
	(void) local_built_in(in_dict("erase_stream_property", 1),
			p_erase_stream_property, B_SAFE);
	built_in(in_dict("pipe", 2),	p_pipe,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(1, CONSTANT) | BoundArg(2, CONSTANT);
	local_built_in(in_dict("exec", 4),	p_exec,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(3, CONSTANT) | BoundArg(4, CONSTANT);
	built_in(in_dict("read_string", 4),	p_read_string,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(3, CONSTANT) | BoundArg(4, CONSTANT);
	built_in(in_dict("read_string", 5),	p_read_string5,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(4, CONSTANT) | BoundArg(5, CONSTANT);
	built_in(in_dict("read_directory", 4),	p_read_dir,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(3, GROUND) | BoundArg(4, GROUND);
	(void) built_in(in_dict("socket", 3),	p_socket,	B_UNSAFE|U_SIMPLE);
	built_in(in_dict("bind", 2),		p_bind,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, GROUND);
	built_in(in_dict("connect", 2),		p_connect,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, GROUND);
	(void) built_in(in_dict("listen", 2),	p_listen,	B_UNSAFE);
	(void) built_in(in_dict("accept", 3),	p_accept,	B_UNSAFE|U_SIMPLE);
	built_in(in_dict("stream_select", 3),	p_select,	B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(3, GROUND);
	local_built_in(in_dict("next_open_stream", 2),
			p_next_open_stream, B_UNSAFE|U_GROUND)
	    -> mode = BoundArg(2, CONSTANT);
	b_built_in(in_dict("wait", 3), 		p_wait, 	d_.kernel_sepia)
	    -> mode = BoundArg(1, CONSTANT) | BoundArg(2, CONSTANT) | BoundArg(3, CONSTANT);
#if defined(HAVE_READLINE)
	(void) exported_built_in(in_dict("readline", 1),		p_readline,	B_SAFE);
#endif
    }
}


/* METHODS */
static void _lose_stream(stream_id nst);
static stream_id _copy_stream(stream_id nst);
static void _mark_stream(stream_id nst);
static int _tostr_stream(stream_id nst, char *buf, int quoted);
static int _strsz_stream(stream_id nst, int quoted);


static void
_lose_stream(stream_id nst)		/* nst != NULL */
{
    assert(nst);
    assert(nst->nref > 0);
    if (--nst->nref == 0)
    {
	if (IsOpened(nst) && !(StreamMode(nst) & SSYSTEM)
                          && !(StreamMode(nst) & SNUMBERUSED))
	{
	    /*
	    p_fprintf(current_output_, "lose_stream(%d)\n", StreamNr(nst));
	    ec_flush(current_output_);
	    */
	    int res = ec_close_stream(nst, CLOSE_FORCE|CLOSE_LOST);
	    if (res != PSUCCEED)
	    {
		p_fprintf(current_err_, "\nError %d during auto-close of stream_%d\n", -res, StreamNr(nst));
		ec_flush(current_err_);
	    }
	}
	/* once we get rid of the array: */
	/* hg_free_size(nst, sizeof(stream_desc)); */
    }
}

static stream_id
_copy_stream(stream_id nst)		/* nst != NULL */
{
    ++nst->nref;
    return nst;
}

static void
_mark_stream(stream_id nst)		/* nst != NULL */
{
    if ((IsOpened(nst) || StreamNref(nst) > 0))
    {
	if (StreamPrompt(nst) != D_UNKNOWN)	/* == SocketUnix */
	    Mark_Did(StreamPrompt(nst));
	if (StreamName(nst) != D_UNKNOWN)
	    Mark_Did(StreamName(nst));
	if (StreamPath(nst) != D_UNKNOWN)
	    Mark_Did(StreamPath(nst));
	mark_dids_from_pwords(&StreamEvent(nst), &StreamEvent(nst)+1);
    }
}


static int
_tostr_stream(stream_id nst, char *buf, int quoted)	/* nst != NULL */
{
#define STRSZ_STREAM 30
    sprintf(buf, "$&(stream(%d))", (int) StreamNr(nst));
    return strlen(buf);
}


static int
_strsz_stream(stream_id nst, int quoted)	/* nst != NULL */
{
    return STRSZ_STREAM;
}


/* CLASS DESCRIPTOR (method table) */

t_ext_type stream_tid = {
    (void (*)(t_ext_ptr)) _lose_stream,
    (t_ext_ptr (*)(t_ext_ptr)) _copy_stream,
    (void (*)(t_ext_ptr)) _mark_stream,
    (int (*)(t_ext_ptr,int)) _strsz_stream,
    (int (*)(t_ext_ptr,char *,int)) _tostr_stream,
    0,	/* equal */
    (t_ext_ptr (*)(t_ext_ptr)) _copy_stream,
    0,	/* get */
    0	/* set */
};


/*
 * FUNCTION NAME:	get_stream_id()
 *
 * PARAMETERS:		v, t	- value and tag of a prolog word which
 *				  specifies the stream
 *			mode	- whether the stream should be input or output
 *				  or none (used only for the 'user' stream)
 *
 * DESCRIPTION:		
 * An auxiliary function.
 * if (v, t) is a number which is in the range [0, NbStreams], it returns the
 *    corresponding stream_id,
 * else if it is an atom which denotes a stream, i.e. whose stream property
 *	is defined, it returns the corresponding stream_id.
 * In all other cases, it returns a (negative) prolog error code.
 * If the specified stream is 'user', it returns either input, output, or
 * INCORRECT_USER.
 */
stream_id
get_stream_id(value v, type t, int mode, int *err)
{
    pword	*stream_prop;
    stream_id	nst;

    if (IsRef(t))
    {
	*err = INSTANTIATION_FAULT;
	return NO_STREAM;
    }
    switch(TagType(t))
    {
    case TNIL:
	v.did = d_.nil;
	/* fall through */
    case TDICT:
	if ((stream_prop = GetStreamProperty(v.did)) == (pword *) NULL)
	{
	    if (v.did == d_.user)
	    {
		if (mode == SREAD)
		    nst = (stream_id) GetStreamProperty(d_.stdin0)->val.wptr;
		else if (mode == SWRITE)
		    nst = (stream_id) GetStreamProperty(d_.stdout0)->val.wptr;
		else
		{
		    *err = INCORRECT_USER;
		    return NO_STREAM;
		}
	    }
	    else
	    {
		*err = STREAM_SPEC;
		return NO_STREAM;
	    }
	}
	else
	    nst = (stream_id) stream_prop->val.wptr;
	break;

    case TINT:
	/* backward compatibility: allow number iff it was obtained previously */
	if (v.nint < 0 || v.nint >= NbStreams
		|| !(StreamMode(StreamId(v.nint)) & SNUMBERUSED))
	{
	    *err = STREAM_SPEC;
	    return NO_STREAM;
	}
	nst = StreamId(v.nint);
	break;

    case THANDLE:
    {
	int res;
	pword hstream;
	hstream.val.all = v.all;
	hstream.tag.all = t.all;
	res = ec_get_handle(hstream, &stream_tid, (t_ext_ptr*) &nst);
	if (res != PSUCCEED) {
	    *err = res==STALE_HANDLE ? STREAM_SPEC : res;
	    return NO_STREAM;
	}
	break;
    }

    default:
	*err = TYPE_ERROR;
	return NO_STREAM;
    }

    if (IsSocket(nst)) {
	if (IsInvalidSocket(nst)) {
	    *err = STREAM_SPEC;
	    return NO_STREAM;
	}
	else if (mode & SREAD)
	    return SocketInputStream(nst);
    }
    return nst;
}


int Winapi
ec_get_stream(const pword pw, stream_id* nst)
{
    int err;
    if ((*nst = get_stream_id(pw.val, pw.tag, 0, &err)) == NO_STREAM)
	return err;
    return PSUCCEED;
}


/*
 * next_open_stream(+Stream, -Stream)
 * Auxiliary for enumerating streams, should start with stdin>
 * 
 */
static int
p_next_open_stream(value v1, type t1, value v2, type t2)
{
    int err, i;
    pword hstream;
    stream_id nst = get_stream_id(v1, t1, 0, &err);
    if (nst == NO_STREAM) { Bip_Error(err); }
    i = StreamNr(nst);
    do {
	if (++i >= NbStreams) { Fail_; }
	nst = StreamId(i);
    } while (!IsOpened(nst) || IsInvalidSocket(nst));
    hstream = StreamHandle(nst);
    Return_Unify_Pw(v2, t2, hstream.val, hstream.tag);
}


static int
p_set_stream(value ov, type ot, value nv, type nt)
{
    stream_id	nst;
    int		err;

    Check_Atom_Or_Nil(ov, ot);		/* must not be an integer	*/
    nst = get_stream_id(nv, nt, 0, &err);
    if (nst == NO_STREAM)
    {
	if (!IsRef(nt) && IsAtom(nt) && nv.did == d_.user)
	{
	    if (ov.did == d_.input)
	    {
		nst = (stream_id) GetStreamProperty(d_.stdin0)->val.wptr;
	    }
	    else if (
		ov.did == d_.output ||
		ov.did == d_.warning_output ||
		ov.did == d_.log_output ||
		ov.did == d_.err)
	    {
		nst = (stream_id) GetStreamProperty(d_.stdout0)->val.wptr;
	    }
	    else
	    {
		Bip_Error(INCORRECT_USER);
	    }
	}
	else
	{
	    Bip_Error(err);
	}
    }
    return set_stream(ov.did, nst);
}


static int
p_get_stream(value vi, type ti, value vs, type ts)
{
    stream_id	nst;
    stream_id	onst;
    int		res;

    nst = get_stream_id(vi, ti, 0, &res);
    if (nst == NO_STREAM)
    {
	Bip_Error(res);
    }
    if (!IsOpened(nst))
    {
	Bip_Error(STREAM_SPEC);
    }
    if (IsRef(ts))
    {
	pword hstream;
	if (IsHandle(ti)) {
	    hstream.val.all = vi.all;	/* reuse old anchor */
	    hstream.tag.all = ti.all;
	} else {
	    hstream = StreamHandle(nst);
	}
	Return_Unify_Pw(vs, ts, hstream.val, hstream.tag);
    }
    if ((onst = get_stream_id(vs, ts, 0, &res)) != NO_STREAM)
    {
	Succeed_If(nst == onst);
    }
    if (IsAtom(ts) && vs.did == d_.user)
    {
	if ((StreamMode(nst) & (SREAD | SWRITE)) == SREAD)
	{
	    Succeed_If(nst == (stream_id) GetStreamProperty(d_.stdin0)->val.wptr);
	}
	else if ((StreamMode(nst) & (SREAD | SWRITE)) == SWRITE)
	{
	    Succeed_If(nst == (stream_id) GetStreamProperty(d_.stdout0)->val.wptr);
	}
	else
	{
	    Bip_Error(INCORRECT_USER);
	}
    }
    Bip_Error(res);
}

int Winapi
ec_stream_nr(const char *name)
{
    stream_id	nst;
    int		res;
    value	v;
    v.did = enter_dict((char*) name, 0);
    nst = get_stream_id(v, tdict, 0, &res);
    if (nst == NO_STREAM  ||  !IsOpened(nst))
	return -1;
    StreamMode(nst) |= SNUMBERUSED;
    return StreamNr(nst);	/*DEPRECATE*/
}

stream_id Winapi
ec_stream_id(int nr)
{
    return StreamId(nr);	/*DEPRECATE*/
}


/*
	p_get_char() 	get_char/2	(standard)
		Same as get, but the character is taken as a one element
		string
*/
static int
p_get_char(value vs, type ts, value val, type tag)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, SREAD, &res);
    char *c;

    Check_Output_String(tag);
    if(IsString(tag) && (*(StringStart(val)) == 0 || *(StringStart(val) + 1) != 0))
    {
	Bip_Error(TYPE_ERROR)
    }
    if (nst == NO_STREAM) {
	Bip_Error(res)
    }
    if (!IsTextStream(nst)) {
	Bip_Error(STREAM_MODE)
    }
    Lock_Stream(nst);
    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;
    /* ec_getch checks for mode errors */
    if ((res = ec_getch(nst)) < 0) {
	Unlock_Stream(nst);
	Bip_Error(res)
    }
    Unlock_Stream(nst);
    {
	value v;
	Make_Stack_String(1, v, c)
	c[0] = res;
	c[1] = 0;
	Return_Unify_String(val, tag, v.ptr);
    }
}


/*
	p_put_char() 	put_char/2	(standard)
*/
static int
p_put_char(value vs, type ts, value val, type tag)
{
    int		res, len;
    char	*s;
    stream_id	nst = get_stream_id(vs, ts, SWRITE, &res);

    if(nst == NO_STREAM) {
	Bip_Error(res)
    }

    if (IsAtom(tag)) {
    	len = DidLength(val.did);
    	s = DidName(val.did);
    } else if (IsString(tag)) {
    	len = StringLength(val);
    	s = StringStart(val);
    } else if (IsRef(tag)) {
	Bip_Error(INSTANTIATION_FAULT)
    } else {
	Bip_Error(TYPE_ERROR)
    }
    if (len != 1) {
	Bip_Error(TYPE_ERROR)
    }
    if (!IsTextStream(nst)) {
	Bip_Error(STREAM_MODE)
    }
    Lock_Stream(nst);
    if((res = ec_outfc(nst, *s)) < 0) {
	Unlock_Stream(nst);
	Bip_Error(res)
    }
    Unlock_Stream(nst);
    Succeed_;
}

/* p_nl()	nl/1	outputs a newline on the given stream.
 *			
 */
static int 
p_nl(value vs, type ts)
{
    int		res;
    stream_id	nst = get_stream_id(vs,ts, SWRITE, &res);

    if(nst == NO_STREAM) {
	Bip_Error(res)
    }

    Lock_Stream(nst);
    res = ec_newline(nst);
    Unlock_Stream(nst);
    return res;
}


/*
 * p_open()	open(+Spec, +Mode, ?Stream)
 *
 * +Spec:
 *	- File name as atom or string
 *	- string(?InitString) for string streams
 *	- queue(?InitString) for queue streams
 *	- fd(+FileDesc) to open (a duplicate of) an existing UNIX fd
 * +Mode:
 *	atom read, write, append, update
 *
 * ?Stream:
 *	a variable which will be bound to a stream number or an atom
 *	which specifies the symbolic name of the stream.
 *
 * Obsolete forms still supported:
 *
 *	open(?InitString, string, S)
 *	open(?InitString, string(+Size), S)
 *	open(_, queue, S)
 *	open(event, queue, S)
 *	open(dup(FD), M, S) is the same as open(fd(FD), M, S)
 */

#define SFD	SPIPE

static int
p_open(value vfile, type tfile, value vmode, type tmode, value vstr, type tstr)
{
    char		*namefile;
    dident		d_event = D_UNKNOWN;
    pword		*init_string = 0;
    pword		init_string_pw;
    short		mode;
    int			kind = SFILE;
    stream_id		nst;
    int			res;
    int			size = 1024;
    int			fd = NO_UNIT;
    Prepare_Requests;

    Check_Output_Atom_Or_Nil(vstr, tstr);
    Error_If_Ref(tmode);
    if (IsAtom(tmode))
    {
	if(vmode.did == d_.read)
	    mode = SREAD;
	else if (vmode.did == d_.write)
	    mode = SWRITE;
	else if (vmode.did == d_.update)
	    mode = SRDWR;
	else if (vmode.did == d_.append)
	    mode = SAPPEND|SWRITE;
	else if (vmode.did == d_queue)		/* obsolete */
	{
	    kind = SQUEUE;
	    mode = SRDWR;
	    if (IsRef(tfile))
		d_event = D_UNKNOWN;
	    else if (IsAtom(tfile))
		d_event = vfile.did;
	    else
		{ Bip_Error(TYPE_ERROR); }
	}
	else if (vmode.did == d_.string0)	/* obsolete */
	{
	    kind = SSTRING;
	    mode = SRDWR|MREAD;
	    Check_Output_String(tfile);
	    init_string_pw.tag.all = tfile.all;
	    init_string_pw.val.all = vfile.all;
	    init_string = &init_string_pw;
	}
	else
	{
	    Bip_Error(STREAM_MODE)
	}
    }
    else if (IsStructure(tmode) && vmode.ptr->val.did == d_.string) /* obsolete */
    {
	if (!IsRef(vmode.ptr[1].tag) && IsInteger(vmode.ptr[1].tag))
	    size = vmode.ptr[1].val.nint;
	else
	{
		Bip_Error(TYPE_ERROR);
	}
	if (size <= 0)
	{
		Bip_Error(RANGE_ERROR);
	}
	if (!IsRef(tfile))	/* size specified for a given string */
	{
		Bip_Error(TYPE_ERROR);
	}
	kind = SSTRING;
	mode = SRDWR|MREAD;
    }
    else
    {
	Bip_Error(TYPE_ERROR)
    }
    if (kind == SFILE)
    {
	/* New interpretation of 1st argument:
	 *	Filename	atom or string
	 *	string(InitStr)
	 *	queue(InitStr)
	 *	fd(Integer)
	 */
	if (IsRef(tfile))
	{
	    Bip_Error(INSTANTIATION_FAULT);
	}
	else if (IsStructure(tfile))
	{
	    if (vfile.ptr->val.did == d_.string)
	    {
		/* the stream is always MREAD to mark that
		 * the contents of the buffer is always significant
		 */
	    	kind = SSTRING;
	    	mode |= MREAD;
		init_string = vfile.ptr + 1;
		Dereference_(init_string);
		Check_String(init_string->tag);
	    }
	    else if (vfile.ptr->val.did == d_queue1)
	    {
	    	kind = SQUEUE;
		init_string = vfile.ptr + 1;
		Dereference_(init_string);
		Check_String(init_string->tag);
	    }
	    else if (vfile.ptr->val.did == d_fd1 || vfile.ptr->val.did == d_dup1)
	    {
		pword *pw = vfile.ptr + 1;
		Dereference_(pw);
		Check_Integer(pw->tag);
		fd = dup((int) pw->val.nint);
		if (fd == -1)
		{
		    Set_Errno
		    Bip_Error(SYS_ERROR)
		}
	    	kind = SFD;		/* preliminary */
	    }
	    else { Bip_Error(RANGE_ERROR); }
	}
	else if (!IsString(tfile) && !IsAtom(tfile))
	{
	    Bip_Error(TYPE_ERROR);
	}
    }

    /* At this point: kind, mode, size are set.
     * init_string is NULL or checked for Output_String.
     */
    if (init_string && IsString(init_string->tag) &&
	    size < StringLength(init_string->val))
    {
	size = StringLength(init_string->val);
    }

    if (kind == SSTRING || kind == SQUEUE)
    {
	nst = find_free_stream();
	init_stream(nst, NO_UNIT, mode|kind,
		kind == SSTRING? d_.string0: d_queue,
		NO_PROMPT, NO_STREAM, size);
    }
    else if (kind == SFD)	/* connect to an existing fd */
    {
	struct_stat fs;

	if (fstat(fd, &fs))
	{
	    Set_Errno
	    Bip_Error(SYS_ERROR)
	}
	if (isatty(fd))
	    kind = STTY;
#ifndef _WIN32
	else if (S_ISSOCK(fs.st_mode) || S_ISFIFO(fs.st_mode))
#else
	else if (S_ISFIFO(fs.st_mode))
#endif
	    kind = SPIPE;
	else
	    kind = SFILE;

	nst = find_free_stream();
	init_stream(nst, fd, mode|kind, d_fd, NO_PROMPT, NO_STREAM, 0);
    }
    else			/* open by name	*/
    {
	Get_Name(vfile, tfile, namefile);
	nst = ec_open_file(namefile, mode, &res);
	if (nst == NO_STREAM)
	{
	    Bip_Error(res);
	}
    }

    if (init_string)		/* init buffer if needed */
    {
	if (IsRef(init_string->tag))	/* obsolete */
	{
	    Request_Unify_String(init_string->val, init_string->tag, empty_string);
	}
	else if (StringLength(init_string->val) > 0)
	{
	    StreamLastWritten(nst) = StringStart(init_string->val)[StringLength(init_string->val)-1];
	    StreamMethods(nst).outf(nst, StringStart(init_string->val), StringLength(init_string->val));
	    if (IsStringStream(nst))
	    {
		if (!(mode & SAPPEND))
		    StreamMethods(nst).seek(nst, 0, LSEEK_SET);
	    }
	}
    }
    if (d_event == D_UNKNOWN || d_event == d_.nil) {
	Make_Nil(&StreamEvent(nst));
    } else {
	Make_Atom(&StreamEvent(nst), d_event);
    }

    if (StreamNref(nst) != 0)
    {
	ec_panic("New stream has refs", "p_open()");
    }
    if (IsRef(tstr))
    {
	pword hstream = ec_handle(&stream_tid, (t_ext_ptr) nst);
	++StreamNref(nst);
	Request_Unify_Pw(vstr, tstr, hstream.val, hstream.tag);
    }
    else if ((res = set_stream(vstr.did, nst)) < 0)
    {
	(void) ec_close_stream(nst, 0);
	Bip_Error(res);
    }
    Return_Unify;
}


/* p_close()	close/1
 * one argument: a stream id
 * return an error code if something is wrong. Never fails.
 * Note: "user" cannot be closed.
 */

static int
p_close2(value v, type t, value vopt, type topt)
{
    stream_id	nst;
    int		res;
    int		close_options = 0;

    /* process the options list */
    while (IsList(topt))
    {
	pword *car = vopt.ptr;
	pword *cdr = car + 1;

	Dereference_(car);
	Error_If_Ref(car->tag);
	if (!IsStructure(car->tag)) {
	    Bip_Error(RANGE_ERROR);	/* not TYPE_ERROR (ISO) */
	}
	car = car->val.ptr;
	if (car->val.did == d_force1) {
	    Check_Atom(car[1].tag);
	    if (car[1].val.did == d_.true0) close_options |= CLOSE_FORCE;
	    else if (car[1].val.did == d_false) close_options &= ~CLOSE_FORCE;
	    else { Bip_Error(RANGE_ERROR); }
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	Dereference_(cdr);
	topt = cdr->tag;
	vopt = cdr->val;
    }
    Check_Nil(topt);

    Error_If_Ref(t);

    nst = get_stream_id(v,t, 0, &res);
    if (nst == NO_STREAM)
    {
	switch (res) {
	    case STREAM_SPEC:
	    case STALE_HANDLE:
		if (close_options&CLOSE_FORCE) { Succeed_; }
		/* fall through */
	    default:
		Bip_Error(res);
	}
    }

    if (SystemStream(nst) || (StreamMode(nst) & SSYSTEM))
    {
        /* It is (or is pointing to) one of the predefined streams.
         * Let the close_handler take care of the details.
         */
	Bip_Error(SYSTEM_STREAM);
    }

    /* close the stream, reporting errors only if necessary */
    Lock_Stream(nst);
    res = ec_close_stream(nst, close_options);
    Unlock_Stream(nst);
    if ((res < 0)  &&  !(close_options & CLOSE_FORCE)  &&
    	!(res==STREAM_SPEC && (IsAtom(t)||IsNil(t))))
    {
	Bip_Error(res)
    }

    /* free handle or property */
    if (IsNil(t))
	v.did = d_.nil;
    if ((IsAtom(t) || IsNil(t)) && !IsOpened(nst))
    {
	(void) erase_property(v.did, STREAM_PROP);
	stream_tid.free((t_ext_ptr) nst);
    }
    else if (IsHandle(t))
    {
	pword hstream;
	hstream.val.all = v.all;
	hstream.tag.all = t.all;
	res = ec_free_handle(hstream, &stream_tid);
	return (close_options & CLOSE_FORCE) ? PSUCCEED : res;
    }
    Succeed_;
}


static int
p_close(value v, type t)
{
    pword nil;
    Make_Nil(&nil);
    return p_close2(v, t, nil.val, nil.tag);
}


static int
p_erase_stream_property(value v, type t)
{
    int		res;
    stream_id	nst;

    Check_Atom_Or_Nil(v, t);
    if ((nst = get_stream_id(v,t, 0, &res)) != NO_STREAM)
    {
	(void) erase_property(v.did, STREAM_PROP);
	StreamNref(nst)--;
    }
    Succeed_;
}

static int
p_tyi(value vs, type ts, value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(vs,ts, SREAD, &res);

    if (nst == NO_STREAM) {
	Bip_Error(res);
    }
    if( !IsRef(t) && !IsInteger(t) ) {
        Bip_Error(TYPE_ERROR);
    }
    Lock_Stream(nst);
    res = ec_tty_in(nst);
    Unlock_Stream(nst);
    if (res < 0) {
	    Bip_Error(res)
    }
    Return_Unify_Integer(v,t,res);
}

static int
p_tyo(value vs, type ts, value v, type t)
{
    int		res;
    stream_id nst = get_stream_id(vs,ts, SWRITE, &res);

    if (nst == NO_STREAM) {
	Bip_Error(res);
    }

    Check_Integer(t)
    Lock_Stream(nst);
    res = ec_tty_out(nst, v.nint);
    Unlock_Stream(nst);
    if (res < 0) {
	Bip_Error(res)
    }
    Succeed_;
}


static int
p_delete(value v, type t)
{
    int	   err;
    char   *name;
    char   fullname[MAX_PATH_LEN];
    struct_stat	file_stat;

    Get_Name(v,t,name)
    name = expand_filename(name, fullname, EXPAND_STANDARD);

    if (ec_stat(name, &file_stat) < 0)
    {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    if ((file_stat.st_mode & S_IFMT) == S_IFDIR)	/* it's a directory */
	err = ec_rmdir(name);
    else
	err = ec_unlink(name);
    if (err < 0)
    {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    Succeed_;
}

static int
p_mkdir(value v, type t)
{
    char   *name;
    char   fullname[MAX_PATH_LEN];

    Get_Name(v,t,name)
    name = expand_filename(name, fullname, EXPAND_STANDARD);

    if (ec_mkdir(name, 0777) < 0)
    {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    Succeed_;
}

#ifdef HAVE_RENAME

static int
p_rename(value vo, type to, value vd, type td)
{
    char   *old, *new;
    char   fullold[MAX_PATH_LEN];
    char   fullnew[MAX_PATH_LEN];
    Get_Name(vo,to,old)
    Get_Name(vd,td,new)
    old = expand_filename(old, fullold, EXPAND_STANDARD);
    new = expand_filename(new, fullnew, EXPAND_STANDARD);
    if (ec_rename(old, new) < 0) {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
    Succeed_;
}

#else /*rename*/

static int
p_rename(value vo, type to, value vd, type td)
{
    char   *nameo;
    char   *named;
    char   buf[2*MAX_PATH_LEN + 5];

    Get_Name(vo,to,nameo)
    Get_Name(vd,td,named)
    (void) strcpy(buf, "mv ");
    (void) expand_filename(nameo, &buf[3], EXPAND_STANDARD);
    (void) strcat(buf, " ");
    (void) expand_filename(named, &buf[strlen(buf)], EXPAND_STANDARD);
#ifdef NO_SYSTEM_RETURN
    (void) system(buf);
#else
    if(system(buf) < 0) {
	Set_Errno
	Bip_Error(SYS_ERROR)
    }
#endif /* no system return code check */
    Succeed_;
}

#endif

/*
 * get_prompt(InputStream, Prompt, OutputStream)
 */
static int
p_get_prompt(value iv, type it, value pv, type pt, value ov, type ot)
{
    stream_id	nst;
    stream_id	onst;
    stream_id	ps;
    int		res;
    dident	pr;
    Prepare_Requests;

    nst = get_stream_id(iv, it, SREAD, &res);
    if (nst == NO_STREAM)
    {
	Bip_Error(res);
    }
    if(!(IsReadStream(nst)))
    {
	Bip_Error(STREAM_MODE)
    }
    pr = StreamPrompt(nst);
    if (pr == NO_PROMPT)
    	pr = in_dict("",0);
    ps = StreamPromptStream(nst);
    if (ps == NO_STREAM)
    	ps = null_;

    if (IsRef(pt) || IsString(pt)) {
	Request_Unify_String(pv, pt, DidString(pr));
    }
    else if (IsAtom(pt) || IsNil(pt)) {
	Request_Unify_Atom(pv, pt, pr);
    }
    else {
	Bip_Error(TYPE_ERROR);
    }
    if (IsRef(ot))
    {
	pword hstream = StreamHandle(ps);
	Return_Unify_Pw(ov, ot, hstream.val, hstream.tag);
    }
    else if ((onst = get_stream_id(ov, ot, SWRITE, &res)) == NO_STREAM)
    {		/* stream checking */
	Bip_Error(res);
    }
    else if (onst != ps)
    {
	Fail_;
    }
    Return_Unify;
}

#define Get_String_Did(v,t,d)						\
	if (IsRef(t)) { Bip_Error(INSTANTIATION_FAULT) }		\
	if (IsAtom(t)) {						\
	    d = v.did;							\
	} else if (IsString(t)) {					\
	    d = enter_dict_n(StringStart(v), StringLength(v), 0);	\
	} else if IsNil(t) {						\
	    d = d_.nil;							\
	} else { Bip_Error(TYPE_ERROR) }


/*
 * set_prompt(InputStream, Prompt, OutputStream)
 */
static int
p_set_prompt(value iv, type it, value pv, type pt, value ov, type ot)
{
    stream_id	nst;
    stream_id	onst;
    int		res;
    dident	d;

    if ((nst = get_stream_id(iv, it, SREAD, &res)) == NO_STREAM)
    {
	Bip_Error(res);
    }
    if(!(IsReadStream(nst)))
    {
	Bip_Error(STREAM_MODE)
    }
    if ((onst = get_stream_id(ov, ot, SWRITE, &res)) == NO_STREAM)
    {
	Bip_Error(res);
    }
    if (!(IsWriteStream(onst)))
    {
	Bip_Error(STREAM_MODE)
    }
    if (IsStructure(pt) && pv.ptr->val.did == d_reprompt1)
    {
	pt.all = pv.ptr[1].tag.all;
	pv.all = pv.ptr[1].val.all;
	StreamMode(nst) |= REPROMPT_ONLY;
    }
    else
    {
	StreamMode(nst) &= ~REPROMPT_ONLY;
    }
    Get_String_Did(pv, pt, d);
    StreamPrompt(nst) = d;
    if (StreamPromptStream(nst) != NO_STREAM)
        _lose_stream(StreamPromptStream(nst));
    StreamPromptStream(nst) = (onst == null_) ? NO_STREAM :
        _copy_stream(onst);
    Succeed_;
}


/*
 * Succeed if the given stream is open. System-use only.
 */
static int
p_is_open_stream(value vc, type tc)
{
    int		res;
    stream_id nst;

    nst = get_stream_id(vc,tc, 0, &res);
    if (nst == NO_STREAM) {
	Fail_;
    }
    else if (!(IsOpened(nst))) {
	Fail_;
    }
    Succeed_;
}


/*
 * stream_info_(Stream, Info, Value)
 *	Stream must be instantiated to an open stream,
 *	does not backtrack, system-use only
 */
static int
p_stream_info_(value vs, type ts, value vi, type ti, value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, 0, &res);
    pword	result;

    Check_Integer(ti);
    if (nst == NO_STREAM) {
	Bip_Error(res)
    }

    switch(vi.nint)
    {
    case 0:	/* name */
	if (IsStringStream(nst) || IsQueueStream(nst))
	{
	    char	*buf;
	    int inbuf = StreamMethods(nst).size(nst);
	    Make_Stack_String(inbuf, result.val, buf);
	    if (StreamMethods(nst).content(nst, buf) != inbuf)
	    {
		p_fprintf(current_err_, "queue_avail/read inconsistency\n");
		ec_flush(current_err_);
	    }
	    buf[inbuf] = '\0';
	    result.tag.kernel = TSTRG;
	}
	else
	{
	    if ((result.val.did = StreamName(nst)) == d_.nil)
		result.tag.kernel = TNIL;
	    else
		result.tag.kernel = TDICT;
	}
	break;
    case 1:	/* prompt */
	if (IsReadStream(nst) && StreamPromptStream(nst) != NO_STREAM)
	{
	    dident pr = StreamPrompt(nst);
	    if (pr == NO_PROMPT)
		pr = in_dict("",0);
	    result.val.ptr = DidString(pr);
	    result.tag.kernel = TSTRG;
	}
	else { Fail_; }
	break;
    case 2:	/* old style mode, not very clean, for backward compatibility */
	if (IsStringStream(nst))
	    result.val.did = d_.string0;
	else if (IsQueueStream(nst))
	    result.val.did = d_queue;
	else if (IsSocket(nst))
	    result.val.did = d_socket;
	else
	    result.val.did = modes[StreamMode(nst) & SMODEBITS];
	result.tag.kernel = TDICT;
	break;
    case 3:	/* aliases */
	result.val.nint = StreamNref(nst);
	result.tag.kernel = TINT;
	break;
    case 4:	/* physical_stream - deprecated */
	StreamMode(nst) |= SNUMBERUSED;
	result.val.nint = StreamNr(nst);
	result.tag.kernel = TINT;
	break;
    case 5:	/* line */
	if (IsSocket(nst))
	    nst = SocketInputStream(nst);
	if (IsReadStream(nst))
	{
	    result.val.nint = StreamLine(nst);
	    result.tag.kernel = TINT;
	}
	else { Fail_; }
	break;
    case 6:	/* offset */
    {
	long offset;
	res = ec_stream_at(nst, &offset);
	if (res != PSUCCEED)
	    { Bip_Error(res); }
	result.val.nint = offset;
	result.tag.kernel = TINT;
	break;
    }
    case 7:	/* system_use */
	if (SystemStream(nst))
	    result.val.did = d_.on;
	else
	    result.val.did = d_.off;
	result.tag.kernel = TDICT;
	break;
    case 8:	/* prompt_stream */
	if (!IsReadStream(nst) || StreamPromptStream(nst) == NO_STREAM)
	    { Fail_; }
	result = StreamHandle(StreamPromptStream(nst));
	break;
    case 9:	/* fd */
	if (StreamUnit(nst) == NO_UNIT)
	    { Fail_; }
	result.tag.kernel = TINT;
	result.val.nint = StreamUnit(nst);
	break;
#ifdef SOCKETS
    case 10:	/* socket port */
	if (IsSocket(nst) && !SocketUnix(nst))
	{
	    struct sockaddr_in	name;
	    int			length = sizeof(name);

	    memset(&name, 0, length);

	    if (getsockname(StreamUnit(nst), (struct sockaddr *) &name, &length) < 0) {
		Set_Errno;
		Bip_Error(SYS_ERROR);
	    }
	    result.tag.kernel = TINT;
	    result.val.nint = ntohs(name.sin_port);
	}
	else { Fail_; }
	break;
    case 11:	/* connection */
	if (IsSocket(nst) && SocketConnection(nst))
	{
	    result.tag.kernel = TDICT;
	    result.val.did = (dident) SocketConnection(nst);
	}
	else { Fail_; }
	break;
#endif
    case 12:	/* reprompt_only */
	if (IsReadStream(nst) && StreamPromptStream(nst) != NO_STREAM)
	{
	    if (StreamMode(nst) & REPROMPT_ONLY)
		result.val.did = d_.on;
	    else
		result.val.did = d_.off;
	    result.tag.kernel = TDICT;
	}
	else { Fail_; }
	break;

    case 13:	/* device */
	Make_Atom(&result, stream_types[StreamType(nst)>>STYPE_SHIFT]);
	break;

    case 14:		/* smallest offset in the buffer - system only */
	if (IsSocket(nst))
	    nst = SocketInputStream(nst);
	if (IsTty(nst) && !(StreamMode(nst) & MREAD)) {
	    Fail_
	}
	result.tag.kernel = TINT;
	result.val.nint = StreamOffset(nst);
	break;

    case 15:	/* mode */
	Make_Atom(&result, modes[StreamMode(nst) & SMODEBITS]);
	break;

    case 16:		/* buffer size - system only */
	result.tag.kernel = TINT;
	result.val.nint = StreamSize(nst);
	break;

    case 17:		/* event name, if any */
	if (IsNil(StreamEvent(nst).tag)) {
	    Fail_;
	} else if (IsTag(StreamEvent(nst).tag.kernel, TPTR)) {
	    result = ec_handle(&heap_event_tid,
		(t_ext_ptr) heap_event_tid.copy(StreamEvent(nst).val.wptr));
	} else {
	    result = StreamEvent(nst);
	}
	break;

    case 18:	/* get flush mode */
	if (!IsWriteStream(nst))
	    { Fail_; }
	if (StreamMode(nst) & SFLUSHEOL) {
	    Make_Atom(&result, d_end_of_line)
	} else {
	    Make_Atom(&result, d_.flush)
	}
	break;

    case 19:		/* get yield */
	if (!IsQueueStream(nst))
	    { Fail_; }
	result.val.did = StreamMode(nst) & SYIELD ? d_.on : d_.off;
	result.tag.kernel = TDICT;
    	break;

    case 20:	/* get end_of_line mode */
	if (IsWriteStream(nst)) {
	    if (StreamMode(nst) & SEOLCR) {
	       Make_Atom(&result, d_crlf)
	    } else {
	       Make_Atom(&result, d_lf)
	    }
	} else {
            Fail_;
	}
	break;

    case 21:		/* get scramble mode */
	if (!(StreamMode(nst) & SSCRAMBLE))
	    { Fail_; }
	Make_Atom(&result, d_.on);
	break;

    case 22:		/* get sigio flag */
	if (!ec_is_sigio_stream(nst, SREAD))
	    { Fail_; }
	Make_Atom(&result, d_.on);
	break;

    case 23:            /* `usable' */
	if (g_emu_.nesting_level > 1 && IsQueueStream(nst) && (StreamMode(nst) & SYIELD)) {
	    Make_Atom(&result, d_.off);
	} else {
	    Make_Atom(&result, d_.on);
	}
	break;

    case 24:            /* macro_expansion */
	if (!IsReadStream(nst))
	    { Fail_; }
	result.val.did = StreamMode(nst) & SNOMACROEXP ? d_.off : d_.on;
	result.tag.kernel = TDICT;
	break;

    case 25:            /* output_options */
	if (!IsWriteStream(nst))
	    { Fail_; }
	Make_Integer(&result, StreamOutputMode(nst));
    	break;

    case 26:            /* print_depth */
	if (!IsWriteStream(nst))
	    { Fail_; }
	Make_Integer(&result, StreamPrintDepth(nst));
    	break;

    case 27:            /* compress */
	if (!IsWriteStream(nst))
	    { Fail_; }
	result.val.did = StreamMode(nst) & SCOMPRESS ? d_.on : d_.off;
	result.tag.kernel = TDICT;
    	break;

    case 28:            /* last_written */
	if (!IsWriteStream(nst) || StreamLastWritten(nst) == -1)
	    { Fail_; }
	Make_Integer(&result, StreamLastWritten(nst));
    	break;

    case 29:		/* handle */
	result = StreamHandle(nst);
	break;

    case 30:		/* delete_file */
	if (!IsFileStream(nst))
	    { Fail_; }
	result.val.did =
	    StreamMode(nst) & SDELETELOST ? d_when_lost :
	    StreamMode(nst) & SDELETECLOSED ? d_when_closed : d_.off;
	result.tag.kernel = TDICT;
	break;

    case 31:		/* full file path */
	if ((StreamPath(nst)) == D_UNKNOWN)
	    { Fail_; }
	if ((result.val.did = StreamPath(nst)) == d_.nil)
	    result.tag.kernel = TNIL;
	else
	    result.tag.kernel = TDICT;
	break;

    case 32:		/* reposition */
	Make_Atom(&result, StreamMode(nst) & SREPOSITION ? d_.true0 : d_false);
	break;

    case 33:		/* encoding */
	Make_Atom(&result, stream_encodings[StreamEncoding(nst)]);
	break;

    case 34:		/* input */
	Make_Atom(&result, StreamMode(nst) & SREAD ? d_.true0 : d_false);
	break;

    case 35:		/* output */
	Make_Atom(&result, StreamMode(nst) & SWRITE ? d_.true0 : d_false);
	break;

    case 36:		/* end_of_stream */
	if (!IsReadStream(nst))
	    { Fail_; }
	/* Only a SoftEofStream can recover from being "past" eof */
	result.val.did =
	    ( IsSoftEofStream(nst) ?
		(StreamMethods(nst).at_eof(nst) == PSUCCEED ?
		    (StreamPastEof(nst) ? d_past : d_at)
		: d_not)
	    : StreamPastEof(nst) ? d_past
	    : StreamMethods(nst).at_eof(nst) == PSUCCEED ? d_at
	    : d_not
	    );
	result.tag.kernel = TDICT;
	break;

    case 37:		/* eof_action */
	if (!IsReadStream(nst))
	    { Fail_; }
	result.val.did =
	    (StreamMode(nst) & SEOF_ACTION) == SEOF_ERROR ? d_.err :
	    (StreamMode(nst) & SEOF_ACTION) == SEOF_RESET ? d_.reset : d_eof_code;
	result.tag.kernel = TDICT;
	break;

    default:
	Fail_;
    }
    Return_Unify_Pw(v, t, result.val, result.tag);
}


#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

static int
p_set_stream_prop_(value vs, type ts, value vi, type ti, value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, 0, &res);
    stream_id	onst;
    dident	d;

    Check_Integer(ti);
    if (nst == NO_STREAM) {
	Bip_Error(res)
    }

    switch(vi.nint)
    {
    case 1:	/* prompt */
	if (!IsReadStream(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	Get_String_Did(v, t, d);
	StreamPrompt(nst) = d;
	break;

    case 5:		/* set line */
	Check_Integer(t)
	StreamLine(nst) = v.nint;
	Succeed_;

    case 6:	/* offset */
	res = p_seek(vs, ts, v, t);
	if (res != PSUCCEED)
	{
	    Bip_Error(res);
	}
	break;

    case 8:	/* prompt_stream */
	if(!(IsReadStream(nst)))
	{
	    Bip_Error(STREAM_MODE)
	}
	if ((onst = get_stream_id(v, t, SWRITE, &res)) == NO_STREAM)
	{
	    Bip_Error(res);
	}
	if (!(IsWriteStream(onst)))
	{
	    Bip_Error(STREAM_MODE)
	}
        if (StreamPromptStream(nst) != NO_STREAM)
            _lose_stream(StreamPromptStream(nst));
        StreamPromptStream(nst) = (onst == null_) ? NO_STREAM :
            _copy_stream(onst);
	break;

    case 12:	/* reprompt_only */
	Check_Atom(t);
	if (v.did == d_.on) {
	    StreamMode(nst) |= REPROMPT_ONLY;
	} else if (v.did == d_.off) {
	    StreamMode(nst) &= ~REPROMPT_ONLY;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 15:	/* mode */
	Check_Atom(t);
	if( !IsOpened(nst) || !(IsQueueStream(nst) || IsStringStream(nst)))
	{
	    Bip_Error(STREAM_MODE)
	}
	if (v.did == d_.update) {
	    StreamMode(nst) |= SRDWR;
	} else if (v.did == d_.read) {
	    StreamMode(nst) = (StreamMode(nst) & ~SWRITE) | SREAD;
	} else if (v.did == d_.write) {
	    StreamMode(nst) = (StreamMode(nst) & ~SREAD) | SWRITE;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	if (StreamMode(nst) & SREAD  &&  StreamLexAux(nst) == NO_BUF)
	{
	    /* read streams need a lex_aux buffer */
	    StreamLexAux(nst) = (unsigned char *) hg_alloc(BUFSIZE);
	    StreamLexSize(nst) = BUFSIZE;
	}
	break;

    case 17:		/* set event name */
	if (!StreamCanRaiseEvent(nst)) {
	    Bip_Error(UNIMPLEMENTED);
	}
	if (IsNil(t)) {
	    if (IsTag(StreamEvent(nst).tag.kernel, TPTR)) {
		heap_event_tid.free(StreamEvent(nst).val.wptr);
	    }
	    Make_Nil(&StreamEvent(nst));
	    if (StreamCanSignal(nst))
	    {
		res = ec_stream_reset_sigio(nst, SREAD);
		Return_If_Error(res);
	    }
	} else {
	    if (IsAtom(t)) {
		Make_Atom(&StreamEvent(nst), v.did);
	    } else {
		t_heap_event *event;
		Get_Typed_Object(v, t, &heap_event_tid, event);
		StreamEvent(nst).tag.kernel = TPTR;
		StreamEvent(nst).val.wptr = heap_event_tid.copy(event);
	    }
	    if (StreamCanSignal(nst))
	    {
		res = ec_stream_set_sigio(nst, SREAD);
		Return_If_Error(res);
	    }
	}
	break;

    case 18:	/* set flush mode */
	Check_Atom(t);
	if (v.did == d_end_of_line) {
	    StreamMode(nst) |= SFLUSHEOL;
	} else if (v.did == d_.flush) {
	    StreamMode(nst) &= ~SFLUSHEOL;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 19:		/* set yield */
	Check_Atom(t);
	if (v.did == d_.on) {
	    StreamMode(nst) |= SYIELD;
	}
	else if (v.did == d_.off) {
	    StreamMode(nst) &= ~SYIELD;
	}
	else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 20:	/* set end_of_line mode */
	Check_Atom(t);
	if (v.did == d_crlf) {
	    StreamMode(nst) |= SEOLCR;
	} else if (v.did == d_lf) {
	    StreamMode(nst) &= ~SEOLCR;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 21:		/* set scramble key */
	Check_Integer(t);
	if ((StreamType(nst) != SFILE) || IsReadWriteStream(nst)) {
	    Bip_Error(STREAM_MODE)
	}
	/* the constant in the next line is arbitrary, just for confusion */
	StreamRand(nst) = (uword) v.nint ^ 0x9bc33c86;
	StreamMode(nst) |= SSCRAMBLE;
	break;

    case 22:		/* set sigio */
	Check_Atom(t);
	if (!StreamCanSignal(nst)) {
	    Bip_Error(UNIMPLEMENTED);
	}
	if (v.did == d_.on) {
	    ec_stream_set_sigio(nst, SREAD);
	} else if (v.did == d_.off) {
	    ec_stream_reset_sigio(nst, SREAD);
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 24:		/* macro_expansion */
	if (!IsReadStream(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	Check_Atom(t);
	if (v.did == d_.on) {
	    StreamMode(nst) &= ~SNOMACROEXP;
	} else if (v.did == d_.off) {
	    StreamMode(nst) |= SNOMACROEXP;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 25:		/* output_options */
	Check_Integer(t);
	if (!IsWriteStream(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	StreamOutputMode(nst) = (int) v.nint;
	break;

    case 26:		/* print_depth */
	Check_Integer(t);
	if (!IsWriteStream(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	StreamPrintDepth(nst) = (int) v.nint;
	break;

    case 27:		/* compress */
	if (!IsWriteStream(nst))
	{
	    Bip_Error(STREAM_MODE);
	}
	Check_Atom(t);
	if (v.did == d_.off) {
	    StreamMode(nst) &= ~SCOMPRESS;
	} else if (v.did == d_.on) {
	    StreamMode(nst) |= SCOMPRESS;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 30:		/* delete_file {off|when_closed|when_lost} */
	Check_Atom(t);
	if (v.did == d_when_lost) {
	    StreamMode(nst) &= ~(SDELETELOST|SDELETECLOSED);
	    StreamMode(nst) |= SDELETELOST;
	}
	else if (v.did == d_when_closed) {
	    StreamMode(nst) &= ~(SDELETELOST|SDELETECLOSED);
	    StreamMode(nst) |= SDELETECLOSED;
	}
	else if (v.did == d_.off) {
	    StreamMode(nst) &= ~(SDELETELOST|SDELETECLOSED);
	}
	else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    case 33:		/* encoding */
	Check_Atom(t);
	{
	    int i;
	    for (i=0; i<SENC_NUM; ++i) {
		if (v.did == stream_encodings[i]) {
		    StreamEncoding(nst) = i;
		    Succeed_;
		}
	    }
	}
	Bip_Error(RANGE_ERROR);

    case 37:		/* eof_action */
	if (!IsReadStream(nst)) {
	    Bip_Error(STREAM_MODE);
	}
	Check_Atom(t);
	StreamMode(nst) &= ~SEOF_ACTION;
	if (v.did == d_.err) {
	    StreamMode(nst) |= SEOF_ERROR;
	} else if (v.did == d_.reset) {
	    StreamMode(nst) |= SEOF_RESET;
	} else if (v.did == d_eof_code) {
	    StreamMode(nst) |= SEOF_CODE;
	} else {
	    Bip_Error(RANGE_ERROR);
	}
	break;

    default:
	Bip_Error(RANGE_ERROR);
    }
    Succeed_;
}

#undef Bip_Error
#define Bip_Error(N) return(N);


static int
p_at(value vs, type ts, value vp, type tp)
{
    int		res;
    stream_id	nst = get_stream_id(vs,ts, 0, &res);
    long	pos;

    Check_Output_Integer(tp);
    if (nst == NO_STREAM)
    {
	if (res == INCORRECT_USER)
	    res = STREAM_MODE;
	Bip_Error(res)
    }
    if (!IsOpened(nst))
    {
	Bip_Error(STREAM_MODE);
    }
    res = ec_stream_at(nst, &pos);
    if (res != PSUCCEED)
    {
	Bip_Error(res);
    }
    Return_Unify_Integer(vp, tp, pos);
}


static int
p_seek(value vs, type ts, value vp, type tp)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, 0, &res);

    Error_If_Ref(tp);
    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    /* no seek on scrambled files: synchronisation gets lost */
    /* no seek on append files: always at eof */
    else if(!IsOpened(nst) || (StreamMode(nst) & (SSCRAMBLE|SAPPEND)))
    {
	Bip_Error(STREAM_MODE);
    }
    if (IsAtom(tp) && vp.did == d_.eof)
    {
	return ec_seek_stream(nst, 0, LSEEK_END);
    }
    Check_Integer(tp);
    return ec_seek_stream(nst, vp.nint, LSEEK_SET);
}


static int
p_stream_truncate(value vs, type ts)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, 0, &res);

    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    if (!IsWriteStream(nst))
    {
	Bip_Error(STREAM_MODE);
    }
    return StreamMethods(nst).truncate(nst);
}


static int
p_get(value vs, type ts, value val, type tag)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, SREAD, &res);

    Check_Output_Integer(tag);
    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    Lock_Stream(nst);
    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;
    if ((res = ec_getch(nst)) < 0)
    {
	Unlock_Stream(nst);
	Bip_Error(res)
    }
    Unlock_Stream(nst);
    Return_Unify_Integer(val, tag, res);
}

static int
p_unget(value vs, type ts)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, SREAD, &res);

    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    Lock_Stream(nst);
    res = ec_ungetch(nst);
    Unlock_Stream(nst);
    return res;
}

static int
p_getw(value vs, type ts, value val, type tag)
{
    int			res;
    register char	*p;
    word		l;
    word		w;
    char		*pw;
    int			i;
    stream_id		nst = get_stream_id(vs, ts, SREAD, &res);

    Check_Output_Integer(tag);
    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }
    Lock_Stream(nst);
    p = ec_getstring(nst, sizeof(word), &l);
    Unlock_Stream(nst);
    if (p == 0)
    {
	Bip_Error((int)l)
    }
    else if (l < sizeof(word))
    {
	Bip_Error(PEOF)
    }
    /* cope with p possibly not aligned */
    pw = (char *) &w;
    for (i = 0; i < sizeof(word); i++)
	*pw++ = *p++;
    Return_Unify_Integer(val, tag, w);
}

static int
p_get1(value val, type tag)
{
    int		res;

    Check_Output_Integer(tag);
    Lock_Stream(current_input_);
    if (StreamMode(current_input_) & REPROMPT_ONLY)
	StreamMode(current_input_) |= DONT_PROMPT;
    if ((res = ec_getch(current_input_)) < 0)
    {
	Unlock_Stream(current_input_);
	Bip_Error(res)
    }
    Unlock_Stream(current_input_);
    Return_Unify_Integer(val, tag, res);
}


/*
 *	p_put() 	put/2
 *	similar to put_char/2, 
 *	but takes a number.
 */
static int
p_put(value vstr, type tstr, value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(vstr, tstr, SWRITE, &res);

    if (nst == NO_STREAM)
    {
	Bip_Error(res)
    }

    Check_Integer(t);
    Lock_Stream(nst);
    if ((res = ec_outfc(nst, (char) v.nint)) < 0)
    {
	Unlock_Stream(nst);
	Bip_Error(res);
    }
    Unlock_Stream(nst);
    Succeed_;
}

/*
 *	p_put1() 	put/1
 */
static int
p_put1(value v, type t)
{
    int		res;

    Check_Integer(t);
    Lock_Stream(current_output_);
    if ((res = ec_outfc(current_output_, (char) v.nint)) < 0)
    {
	Unlock_Stream(current_output_);
	Bip_Error(res);
    }
    Unlock_Stream(current_output_);
    Succeed_;
}

static int
p_at_eof(value vs, type ts)
{
    int		res;
    stream_id	nst = get_stream_id(vs, ts, 0, &res);

    if (nst == NO_STREAM)
    {
	Bip_Error(res);
    }
    /* SoftEofStream can recover from being "past" eof, and needs extra check */
    Succeed_If((StreamPastEof(nst) && !IsSoftEofStream(nst))
    	|| (StreamMethods(nst).at_eof(nst) == PSUCCEED));
}


/*
 * Flush the specified (output) stream.
 */
static int
p_flush(value sv, type st)
{
    int		res;
    stream_id	nst;

    if ((nst = get_stream_id(sv, st, SWRITE, &res)) == NO_STREAM)
    {
	Bip_Error(res)
    }
    Lock_Stream(nst);
    res = ec_flush(nst);
    Unlock_Stream(nst);
    return res;
}

static int
p_stream_number(value val1, type tag1)
{
	Check_Output_Integer(tag1);
	Return_Unify_Integer(val1, tag1, NbStreams - 1);
}

static int
p_pipe(value valr, type tagr, value valw, type tagw)
{
#if defined(HAVE_PIPE)
	int		pd[2];
	stream_id	nr, nw;
	int		res;
	int		sigio = 0;
	pword		in_s;
	pword		out_s;

	res = _check_stream(valr, tagr, &in_s, 0);
	if (res < 0) {
	    Bip_Error(res)
	}
	else if (res & EXEC_PIPE_SIG)
	    sigio = 1;
	res = _check_stream(valw, tagw, &out_s, 0);
	if (res < 0) {
	    Bip_Error(res)
	}
	else if (res & EXEC_PIPE_SIG)
	    sigio = 1;
	if (in_s.val.did == out_s.val.did) {
	    Bip_Error(STREAM_SPEC)
	}

	if (pipe(pd) == -1)
	{
		Set_Errno;
		Bip_Error(SYS_ERROR);
	}
	nr = find_free_stream();
	init_stream(nr, pd[0], SREAD | SPIPE, d_pipe, NO_PROMPT, NO_STREAM, 0);
	nw = find_free_stream();
	init_stream(nw, pd[1], SWRITE | SPIPE, d_pipe, NO_PROMPT, NO_STREAM, 0);
	if (sigio) {
	    if ((res = ec_stream_set_sigio(nr, SREAD)) < 0) {
		Bip_Error(res)
	    }
	}
	Bind_Stream(in_s.val, in_s.tag, nr);
	Bind_Stream(out_s.val, out_s.tag, nw);
	Succeed_;
#else
	Bip_Error(NOT_AVAILABLE);
#endif
}



/*
	p_read_string() 	read_string/4
*/
static int
p_read_string(value vs, type ts, value vdel, type tdel, value vl, type tl, value val, type tag)
{
    stream_id		nst;
    int			isref, status;
    int			res;
    char		*c, *d, *delim;
    long		ndelim, dellength, length = 0;
    pword		*pw;
    static char *	nl = "\n";
    Prepare_Requests

    if (IsRef(tdel))
    	{ Bip_Error(INSTANTIATION_FAULT); }
    else if (IsString(tdel))
    {
	ndelim = StringLength(vdel);
	delim = StringStart(vdel);
    }
    else if (IsAtom(tdel))
    {
	if (vdel.did == d_end_of_line)
	{
	    ndelim = 1; delim = nl;
	}
	else if (vdel.did == d_.eof)
	{
	    ndelim = 0; delim = "";
	}
	else { Bip_Error(RANGE_ERROR); }
    }
    else { Bip_Error(TYPE_ERROR); }

    Check_Output_Integer(tl);
    Check_Output_String(tag);
    isref = IsRef(tl);
    nst = get_stream_id(vs, ts, SREAD, &status);
    if (nst == NO_STREAM)
    {
	Bip_Error(status)
    }
    Lock_Stream(nst);
    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;
    pw = TG;
    Push_Buffer(1);			/* first make a minimal buffer */
    c = (char *) BufferStart(pw);
    while(isref || length < vl.nint)
    {
    	if ((res = ec_getch(nst)) == PEOF)	/* ec_getch checks for end of file	*/
	{
	    if (!length) {
		Unlock_Stream(nst);
		TG = pw;		/* pop the unfinished string	*/
		Bip_Error(PEOF)
	    } else {			/* consider EOF as delimiter	*/
		/* clear the mark, because PEOF is not raised */
		StreamMode(nst) &= ~MEOF;
		break;
	    }
	}
    	if (res < 0)			/*  checks for mode errors	*/
	{
	    Unlock_Stream(nst);
	    TG = pw;		/* pop the unfinished string	*/
	    Bip_Error(res)
        }
	dellength = ndelim;	/* check if we have hit a delimiter */
	d = delim;
	while(dellength--)
	{
	    if (res == *d++)
	    {
		dellength = 0;
		break;
	    }
	}
	if (!dellength)
	    break;
	length++;			/* add the character to the string */
	*c++ = res;
	if (c == (char *) TG)	/* get a new memory word, if needed */
	{
	    TG += 1;
	    Check_Gc;
	}
    }
    Unlock_Stream(nst);
    /* remove CR if we had a CR-LF end-of-line sequence */
    if (delim == nl  &&  length > 0  &&  *(c-1) == '\r')
    {
	--length;
	--c;
    }
    *c = 0;
    Trim_Buffer(pw, length+1);
    Request_Unify_String(val, tag, pw);
    if (isref)
    {
	Request_Unify_Integer(vl, tl, length);
    }
    Return_Unify;
}


/*
 * read_string(+Stream, +SepChars, +PadChars, -ActualSep, -String)
 *
 * SepChars and PadChars are strings.
 * SepChars can also be atom 'end_of_line' (meaning "\n or \r\n"),
 * or 'end_of_file' (equivalent to "").
 * ActualSep is the delimiter that actually occurred, or -1 for EOF.
 * String is the read string with padding removed.
 * Once ActualSep=-1 has been returned, the next call gives READ_PAST_EOF.
 * The "multi-separator" functionality of split_string/4 is not supported,
 * as this could require blocking reads.
 */

#define CheckSetMember(ch,nset,pset,match) \
    for(match=nset;match;--match) { \
	if ((ch) == pset[match-1]) \
	    break; \
    }

static int
p_read_string5(value vs, type ts, value vdel, type tdel,
	value vpad, type tpad, value vsep, type tsep, value val, type tag)
{
    stream_id		nst;
    int			res;
    char		*c, *start, *ipad;
    char		*delim, *pad;
    long		ndelim, npad, match;
    pword		*pw;
    static char *	nl = "\n";
    Prepare_Requests

    if (IsString(tdel)) {
	ndelim = StringLength(vdel);
	delim = StringStart(vdel);
    } else {
	Check_Atom_Or_Nil(vdel, tdel);
	if (vdel.did == d_end_of_line) {
	    ndelim = 1; delim = nl;
	} else if (vdel.did == d_.eof) {
	    ndelim = 0; delim = "";
	} else {
	    Bip_Error(RANGE_ERROR);
	}
    }
    if (IsString(tpad)) {
	npad = StringLength(vpad);
	pad = StringStart(vpad);
    } else {
	Check_Atom_Or_Nil(vpad, tpad);
	/* no padding symbols yet */ 
	Bip_Error(RANGE_ERROR);
    }
    /* Check_Output_Integer(tl); */
    /* Check_Output_String(tag); */

    nst = get_stream_id(vs, ts, SREAD, &res);
    if (nst == NO_STREAM) {
	Bip_Error(res)
    }
    Lock_Stream(nst);
    if (StreamMode(nst) & REPROMPT_ONLY)
	StreamMode(nst) |= DONT_PROMPT;
    pw = TG;
    Push_Buffer(1);			/* first make a minimal buffer */
    start = c = (char *) BufferStart(pw);

_before_:
    res = ec_getch(nst);
    if (res < 0) goto _eof_err_;
    CheckSetMember(res,npad,pad,match);
    if (match) goto _before_;
    CheckSetMember(res,ndelim,delim,match);
    if (match) goto _end_;

_within_:
    *c++ = res;
    if (c == (char *) TG) {	/* get a new memory word, if needed */
	TG += 1;
	Check_Gc;
    }
    res = ec_getch(nst);
    if (res < 0) goto _eof_err_;
    CheckSetMember(res,ndelim,delim,match);
    if (match) {
	if (delim==nl && *(c-1)=='\r')
	    --c;		/* forget CR (delimiter was end_of_line) */
	goto _end_;
    }
    CheckSetMember(res,npad,pad,match);
    if (!match) goto _within_;
    ipad = c;

_after_:
    *c++ = res;
    if (c == (char *) TG) {	/* get a new memory word, if needed */
	TG += 1;
	Check_Gc;
    }
    res = ec_getch(nst);
    if (res < 0) {
	c = ipad;		/* forget trailing padding */
	goto _eof_err_;
    }
    CheckSetMember(res,npad,pad,match);
    if (match) {
	if (delim==nl && *(c-1)=='\r')
	    ipad = c;
	goto _after_;
    }
    CheckSetMember(res,ndelim,delim,match);
    if (match) {
	c = ipad;
	goto _end_;
    }
    if (res != '\r')
	goto _within_;

_after_cr_:
    *c++ = res;
    if (c == (char *) TG) {	/* get a new memory word, if needed */
	TG += 1;
	Check_Gc;
    }
    res = ec_getch(nst);
    if (res < 0) {
	goto _eof_err_;		/* end after lone CR */
    }
    CheckSetMember(res,npad,pad,match);
    if (match) {
	ipad = c;		/* restart padding */
	goto _after_;
    }
    CheckSetMember(res,ndelim,delim,match);
    if (!match) goto _within_;
    if (delim==nl)
	c = ipad;		/* Pad+CR+LF forget all */

_end_:	/* here: res == delimiter char */
    Unlock_Stream(nst);
    *c++ = 0;
    Trim_Buffer(pw, c-start);
    Request_Unify_String(val, tag, pw);
    Request_Unify_Integer(vsep, tsep, res);
    Return_Unify;

_eof_err_:
    if (res == PEOF) {
	res = -1;
	goto _end_;
    }
    Unlock_Stream(nst);
    TG = pw;			/* pop the unfinished string	*/
    Bip_Error(res)
}


/*
 * read_directory(+Directory, +Pattern, ?FileList, ?DirList)
 */

#ifdef _WIN32

static int
p_read_dir(value vdir, type tdir, value vpat, type tpat, value vsubdirs, type tsubdirs, value vfiles, type tfiles)
{
    char		*name, *pattern;
    char		exp_name[MAX_PATH_LEN];
    char		full_name[MAX_PATH_LEN];
    HANDLE		dirp;
    WIN32_FIND_DATA	dent;
    DWORD		err;
    pword		file_list, dir_list;
    register pword	*file_last = &file_list;
    register pword	*dir_last = &dir_list;
    Prepare_Requests;

    Get_Name(vdir, tdir, name);			/* check arguments	*/
    Get_Name(vpat, tpat, pattern);
    Check_Output_List(tsubdirs);
    Check_Output_List(tfiles);

    name = expand_filename(name, exp_name, EXPAND_STANDARD);
    name = strcat(os_filename(name, full_name), "/*.*");

    dirp = FindFirstFile(name, &dent);
    if (dirp == INVALID_HANDLE_VALUE)
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }

    do
    {
	pword	*elem = TG;

	if (dent.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) 
	{
	    if (!strcmp(dent.cFileName, ".") || !strcmp(dent.cFileName, ".."))
		continue;
	    Make_List(dir_last, elem);		/* append the new element */
	    dir_last = elem + 1;
	}
	else					/* it's a simple file */
	{
	    if (!_match(pattern, dent.cFileName))
		continue;
	    Make_List(file_last, elem);		/* append the new element */
	    file_last = elem + 1;
	}

	Push_List_Frame();			/* make a list element */
	Make_String(elem, dent.cFileName);	/* value is the name string */

    } while (FindNextFile(dirp, &dent));

    if ((err = GetLastError()) != ERROR_NO_MORE_FILES)
    {
	Set_Sys_Errno(err,ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }

    if (!FindClose(dirp))
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
    }

    Make_Nil(file_last);			/* terminate the lists */
    Make_Nil(dir_last);

    Request_Unify_Pw(vfiles, tfiles, file_list.val, file_list.tag);
    Request_Unify_Pw(vsubdirs, tsubdirs, dir_list.val, dir_list.tag);
    Return_Unify;
}

#else
#if defined(HAVE_READDIR)

static int
p_read_dir(value vdir, type tdir, value vpat, type tpat, value vsubdirs, type tsubdirs, value vfiles, type tfiles)
{
    char		*name, *pattern;
    char		exp_name[MAX_PATH_LEN];
    char		full_name[MAXNAMLEN];	/* for stat() */
    DIR			 *dirp;
    struct dirent	*dent;
    pword		file_list, dir_list;
    register pword	*file_last = &file_list;
    register pword	*dir_last = &dir_list;
    struct_stat		file_stat;
    Prepare_Requests;

    Get_Name(vdir, tdir, name);			/* check arguments	*/
    Get_Name(vpat, tpat, pattern);
    Check_Output_List(tsubdirs);
    Check_Output_List(tfiles);

    name = expand_filename(name, exp_name, EXPAND_STANDARD);
    name = os_filename(name, full_name);
    if ((dirp = opendir(name)) == NULL)		/* try to open the directory */
    {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }

    for (name = full_name; *name; name++)	/* prepare the name buffer */
	;
    *name++ = '/';

    while ((dent = readdir(dirp)))		/* loop through the entries */
    {
	register pword	*elem = Gbl_Tg;

	(void) strcpy(name, dent->d_name);	/* get the file's status */
	if (ec_stat(full_name, &file_stat))
	{
	    errno = 0;				/* just ignore the file */
	    continue;
	}

	if ((file_stat.st_mode & S_IFMT) == S_IFDIR)	/* it's a directory */
	{
	    if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, ".."))
		continue;
	    dir_last->tag.kernel = TLIST;	/* append the new element */
	    dir_last->val.ptr = elem;
	    dir_last = elem + 1;
	}
	else					/* it's a simple file */
	{
	    if (!_match(pattern, name))
		continue;
	    file_last->tag.kernel = TLIST;	/* append the new element */
	    file_last->val.ptr = elem;
	    file_last = elem + 1;
	}

	Gbl_Tg += 2;				/* make a list element */
	elem->tag.kernel = TSTRG;		/* value is the name string */
	Cstring_To_Prolog(dent->d_name, elem->val);
    }

    (void) closedir(dirp);
    file_last->tag.kernel = TNIL;		/* terminate the lists */
    dir_last->tag.kernel = TNIL;
    errno = 0;					/* just to be sure .. */

    Request_Unify_Pw(vfiles, tfiles, file_list.val, file_list.tag);
    Request_Unify_Pw(vsubdirs, tsubdirs, dir_list.val, dir_list.tag);
    Return_Unify;
}

#else

static int
p_read_dir(value vdir, type tdir, value vpat, type tpat, value vsubdirs, type tsubdirs, value vfiles, type tfiles) {
    USER_PANIC("Not available\n");
    Bip_Error(NOT_AVAILABLE);
}

#endif
#endif

#ifdef SOCKETS

static int
p_socket(value vdom, type tdom, value vtp, type ttp, value vs, type ts)
{
    int		sdomain;
    int		stype;
    socket_t	s;
    stream_id	onst, inst;
    int		res;
    int		sigio = 0;
    pword	p;

    Check_Atom(tdom);
    Check_Atom(ttp);
    res = _check_stream(vs, ts, &p, 0);
    if (res < 0) {
	Bip_Error(res)
    }
    else if (res & EXEC_PIPE_SIG)
	sigio = 1;
    if (vdom.did == d_unix)
	sdomain = AF_UNIX;
    else if (vdom.did == d_internet)
	sdomain = AF_INET;
    else {
	Bip_Error(RANGE_ERROR);
    }
    if (vtp.did == d_stream)
	stype = SOCK_STREAM;
    else if (vtp.did == d_datagram)
	stype = SOCK_DGRAM;
    else {
	Bip_Error(RANGE_ERROR);
    }
    s = socket(sdomain, stype, 0);
    if (s == INVALID_SOCKET) {
	Set_Socket_Errno();
	Bip_Error(SYS_ERROR);
    }
    inst = find_free_stream();
    init_stream(inst, s, SREAD | SSOCKET, d_socket, NO_PROMPT, NO_STREAM, 0);
    onst = find_free_stream();
    init_stream(onst, s, SWRITE | SSOCKET, d_socket, NO_PROMPT, _copy_stream(inst), 0);
    if (sdomain == AF_UNIX)
	SocketUnix(onst) = in_dict("",0);	/* to mark AF_UNIX */
    SocketConnection(onst) = 0;
    if (sigio) {
	if ((res = ec_stream_set_sigio(onst, SWRITE)) < 0) {
	    Bip_Error(res)
	}
    }
    SocketType(onst) = stype;
    Bind_Stream(p.val, p.tag, onst);
    Succeed_;
}

static int
socket_bind(stream_id nst, value vaddr, type taddr)
{
    if (SocketUnix(nst))
    {
#ifdef HAVE_AF_UNIX
	struct sockaddr_un	name;

	Check_Atom_Or_Nil(vaddr, taddr);
	name.sun_family = AF_UNIX;
	(void) strcpy(name.sun_path, DidName(vaddr.did));
	if (bind(StreamUnit(nst), (struct sockaddr *) &name,
	    strlen(name.sun_path) + sizeof(name.sun_family)) < 0) {
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	StreamName(nst) = vaddr.did;
	SocketUnix(nst) = vaddr.did;
	Succeed_;
#else
	Bip_Error(SYS_ERROR);
#endif
    }
    else
    {
	struct sockaddr_in	name;
	struct hostent		*host;
	pword			*addr;
	pword			*port;
	int			length = sizeof(name);
	dident			hdid;
	Prepare_Requests;

	memset(&name, 0, length);

	if (IsStructure(taddr) && vaddr.ptr->val.did == d_.quotient)
	{						
		addr = vaddr.ptr + 1;
		Dereference_(addr);
		Check_Output_Atom_Or_Nil(addr->val, addr->tag);
		port = vaddr.ptr + 2;
		Dereference_(port);
		Check_Output_Integer(port->tag);
	}						
	else if (!IsRef(taddr))
	    { Bip_Error(TYPE_ERROR); }
	name.sin_family = AF_INET;
	if (IsRef(taddr) || IsRef(addr->tag))
	{
	    int		hlen;
	    char	buf[257];

	    name.sin_addr.s_addr = htonl(INADDR_ANY);

	    hlen = ec_gethostname(buf, 257);
	    if (hlen < 0) {
		Bip_Error(SYS_ERROR);
	    }
	    hdid = enter_dict_n(buf, hlen, 0);
	}
	else
	{
	    host = gethostbyname(DidName(addr->val.did));
	    if (!host) {
		Fail_;
	    }
	    hdid = addr->val.did;
	    bcopy((char *) host->h_addr, (char *) &name.sin_addr, host->h_length);
	}
	if (!IsRef(taddr) && IsInteger(port->tag))
	    name.sin_port = htons((short) port->val.nint);
	else
	    name.sin_port = htons(0);
	if (bind((socket_t) StreamUnit(nst), (struct sockaddr *) &name, sizeof(name)) != 0) {
	    Set_Socket_Errno();
	    Bip_Error(SYS_ERROR);
	}
	StreamName(nst) = hdid;
	if (getsockname((socket_t) StreamUnit(nst), (struct sockaddr *) &name, &length) != 0) {
	    Set_Socket_Errno();
	    Bip_Error(SYS_ERROR);
	}
	if (IsRef(taddr))
	{
	    pword		*pw = Gbl_Tg;

	    Gbl_Tg += 3;
	    pw[0].tag.kernel = TDICT;
	    pw[0].val.did = d_.quotient;
	    pw[1].val.did = hdid;
	    pw[1].tag.kernel = TDICT;
	    pw[2].val.nint = ntohs(name.sin_port);
	    pw[2].tag.kernel = TINT;
	    Return_Unify_Structure(vaddr, taddr, pw);
	}
	if (IsRef(port->tag)) {
	    Request_Unify_Integer(port->val, port->tag, ntohs(name.sin_port));
	}
	if (IsRef(addr->tag)) {
	    Request_Unify_Atom(addr->val, addr->tag, hdid);
	}
	Return_Unify;
    }
}

static int
p_bind(value v, type t, value vaddr, type taddr)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, 0, &res);

    if (nst == NO_STREAM)
	{ Bip_Error(res); }

    if (IsOpened(nst)) {
        return RemoteStream(nst) ? io_rpc(nst, IO_BIND):
				socket_bind(nst, vaddr, taddr);
    } else 
        { Bip_Error(STREAM_SPEC); }

}

static int
socket_connect(stream_id nst, value vaddr, type taddr)
{
    if (SocketUnix(nst))
    {
#ifdef HAVE_AF_UNIX
	char			*file;
	struct sockaddr_un	name;

	if (IsInteger(taddr))
	{
	    if (vaddr.nint == 0)
		/* null address does not work everywhere, so take a non-socket
		   file */
		(void) strcpy(name.sun_path, "/");
	    else
		{ Bip_Error(RANGE_ERROR); }
	}
	else {
	    Get_Name(vaddr, taddr, file);
	    (void) strcpy(name.sun_path, file);
	}
	name.sun_family = AF_UNIX;
	if (connect(StreamUnit(nst), (struct sockaddr *) &name,
	    strlen(name.sun_path) + sizeof(name.sun_family)) < 0
	    && !(IsInteger(taddr) && errno == ENOTSOCK))
	{
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	if (IsInteger(taddr))
	    SocketConnection(nst) = 0;
	else
	    SocketConnection(nst) = (unsigned char *) (vaddr.did);
	Succeed_;
#else
	Bip_Error(SYS_ERROR);
#endif
    }
    else
    {
	struct sockaddr_in	name;
	struct hostent		*host;
	long			haddr = 0;
	pword			*addr;
	pword			*port;
	dident			hostname_did;

	memset(&name, 0, sizeof(name));

	Error_If_Ref(taddr);				
	if (!IsStructure(taddr) || vaddr.ptr->val.did != d_.quotient)
	    { Bip_Error(TYPE_ERROR); }

	addr = vaddr.ptr + 1;
	Dereference_(addr);
	Error_If_Ref(addr->tag);
	if (IsInteger(addr->tag)) {
	    if (addr->val.nint != 0)
		{ Bip_Error(RANGE_ERROR); }
	    host = 0;
	    haddr = addr->val.nint;
	    hostname_did = (dident) 0;
	}
	else
	{
	    if (IsString(addr->tag)) {
	    	hostname_did = Did(StringStart(addr->val),0);
	    } else if (IsAtom(addr->tag)) {
	    	hostname_did = addr->val.did;
	    } else if (IsNil(addr->tag)) {
	    	hostname_did = d_.nil;
	    } else {
		Bip_Error(TYPE_ERROR);
	    }
	    host = gethostbyname(DidName(hostname_did));
	}
	port = vaddr.ptr + 2;
	Dereference_(port);
	Check_Integer(port->tag);
	name.sin_port = htons((short) port->val.nint);
	if (!host)
	    name.sin_addr.s_addr = htonl(haddr);
	else
	    bcopy((char *) host->h_addr, (char *) &name.sin_addr, host->h_length);
	name.sin_family = AF_INET;
	if (connect((socket_t) StreamUnit(nst), (struct sockaddr *) &name, sizeof(name)) != 0)
	{
	    Set_Socket_Errno();
#ifdef EADDRNOTAVAIL
	    if (!(host == 0 && haddr == 0 && ec_os_errno_ == EADDRNOTAVAIL))
#endif
	    { 
	      /* if connect returns with error, then the socket is closed
                 (some OSs can leave the socket in a funny state if
		 connection refused)
	      */
	        Lock_Stream(nst);
	        ec_close_stream(nst, CLOSE_FORCE);
		Unlock_Stream(nst);
	        Bip_Error(SYS_ERROR); 
	    }
	}
	if (!host)
	    SocketConnection(nst) = 0;
	else
	    SocketConnection(nst) = (unsigned char *) hostname_did;
	Succeed_;
    }
}

static int
p_connect(value v, type t, value vaddr, type taddr)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, 0, &res);

    if (nst == NO_STREAM)
	{ Bip_Error(res); }
    if (IsOpened(nst))
    {
        return RemoteStream(nst) ? io_rpc(nst, IO_CONNECT):
				socket_connect(nst, vaddr, taddr);
    } else
        { Bip_Error(STREAM_SPEC); }

}

static int
socket_listen(stream_id nst, value vn, type tn)
{
    Check_Integer(tn);
    if (listen((socket_t) StreamUnit(nst), (int) vn.nint) != 0) {
	Set_Socket_Errno();
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}

static int
p_listen(value v, type t, value vn, type tn)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, 0, &res);

    if (nst == NO_STREAM)
	{ Bip_Error(res); }
    if (IsOpened(nst)) {
      return RemoteStream(nst) ? io_rpc(nst, IO_LISTEN):
				socket_listen(nst, vn, tn);
    } else
        { Bip_Error(STREAM_SPEC); }


}

static int
socket_accept(stream_id nst, value vaddr, type taddr, pword p, int sigio)
{
    socket_t	res;
    stream_id	inst, onst;
    int		stype;
    int		length, err;
    dident	wn;
    Prepare_Requests;

    if (SocketUnix(nst))
    {
#ifdef HAVE_AF_UNIX
	struct sockaddr_un	name;

	Check_Output_Atom_Or_Nil(vaddr, taddr);
	length = sizeof(name);
	res = accept(StreamUnit(nst), (struct sockaddr *) &name, &length);
	if (res == INVALID_SOCKET) {
	    Set_Socket_Errno();
	    Bip_Error(SYS_ERROR);
	}
	wn = enter_dict_n(name.sun_path, length-sizeof(name.sun_family), 0);
	Request_Unify_Atom(vaddr, taddr, wn);
#else
	Bip_Error(SYS_ERROR);
#endif
    }
    else
    {
	struct sockaddr_in	name;
	struct hostent		*host;
	pword			*pw = Gbl_Tg;

	Check_Output_Structure(taddr);				

	length = sizeof(name);
	memset(&name, 0, length);

	res = accept((socket_t) StreamUnit(nst), (struct sockaddr *) &name, &length);
	if (res == INVALID_SOCKET) {
	    Set_Socket_Errno();
	    Bip_Error(SYS_ERROR);
	}
	host = gethostbyaddr ((char *) &name.sin_addr, sizeof(name.sin_addr), AF_INET);
	Gbl_Tg += 3;
	pw[0].tag.kernel = TDICT;
	pw[0].val.did = d_.quotient;
	if (host) {
	    pw[1].val.did = wn = enter_dict(host->h_name, 0);
	    pw[1].tag.kernel = TDICT;
	}
	else {
	    pw[1].val.ptr = pw + 1;
	    pw[1].tag.kernel = TREF;
	    wn = d_socket;
	}
	pw[2].val.nint = ntohs(name.sin_port);
	pw[2].tag.kernel = TINT;
	Request_Unify_Structure(vaddr, taddr, pw);
    }
    inst = find_free_stream();
    init_stream(inst, (uword) res, SREAD | SSOCKET, wn, NO_PROMPT, NO_STREAM, 0);
    onst = find_free_stream();
    init_stream(onst, (uword) res, SWRITE | SSOCKET, wn, NO_PROMPT, _copy_stream(inst), 0);
    if (SocketUnix(nst))
	SocketUnix(onst) = in_dict("",0);
    if (sigio) {
	if ((err = ec_stream_set_sigio(onst, SWRITE)) < 0) {
	    Bip_Error(err)
	}
    }
#ifdef SO_TYPE
    length = sizeof(stype);
    (void) getsockopt(res, SOL_SOCKET, SO_TYPE, &stype, &length);
    SocketType(onst) = stype;
#else
    /* copy the socket type from that of the accept socket */
    SocketType(onst) = SocketType(nst);
    SocketType(inst) = SocketType(nst);
#endif
    Bind_Stream(p.val, p.tag, onst);
    Return_Unify;
}

static int
p_accept(value v, type t, value vaddr, type taddr, value vs, type ts)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, 0, &res);
    pword	p;
    int		sigio = 0;

    if (nst == NO_STREAM)
	{ Bip_Error(res); }
    res = _check_stream(vs, ts, &p, 0);
    if (res < 0) {
	Bip_Error(res)
    }
    else if (res & EXEC_PIPE_SIG)
	sigio = 1;
    if (IsOpened(nst)) {
        return RemoteStream(nst) ? io_rpc(nst, IO_ACCEPT):
				socket_accept(nst, vaddr, taddr, p, sigio);
    } else
        { Bip_Error(STREAM_SPEC); }

}

int
ec_write_socket(uword fd, char *buf, int n)	/* returns eclipse status */
{
    int		cnt = 0;

    for (;;)
    {
	cnt = send((int) fd, buf, n, 0);
	if (cnt == n)
	    return PSUCCEED;
	else if (cnt < 0 )
	{
	    Set_Socket_Errno();
#ifdef EINTR
	    if (ec_os_errno_ == EINTR)
	    	continue;	/* an interrupted call, try again */
#endif
#ifdef _WIN32
	    if (ec_os_errno_ == WSAEINTR)
	    	continue;	/* an interrupted call, try again */
#endif
	    return OUT_ERROR;
	}
	else
	{
	    n -= cnt;
	    buf += cnt;
	}
    }
}

int
ec_read_socket(uword fd, char *buf, int n)	/* returns count, sets ec_os_errno_ if -1 */
{
    int count;

    for (;;)
    {
	count = recv((int) fd, buf, n, 0);
	if (count < 0)
	{
	    Set_Socket_Errno();
#ifdef EINTR
	    if (ec_os_errno_ == EINTR)
	    	continue;	/* an interrupted call, try again */
#endif
#ifdef _WIN32
	    if (ec_os_errno_ == WSAEINTR)
	    	continue;	/* an interrupted call, try again */
#endif
	}
	return count;
    }
}

int
ec_close_socket(uword fd)		/* returns eclipse status */
{
#ifdef _WIN32
    if (closesocket(fd) != 0)
#else
    if (close(fd) != 0)
#endif
    {
	Set_Socket_Errno();
	return SYS_ERROR;
    }
    return PSUCCEED;
}


#ifdef _WIN32

/***********************************************************************
 * Signalling streams (like SIGIO on Unix)
 *
 * Mechanism for faking SIGIO signals, mainly intended for Windows:
 * When signaling is requested for a stream (which must support
 * select()), we associate with it a thread, and let the thread
 * post a pseudo-SIGIO integer event whenever data arrives on the
 * empty stream. After posting, the thread is stopped. It is reenabled
 * when a read operation finds that there is no more data available.
 ***********************************************************************/

/*
 * Event-posting thread: do a blocking select on the given socket,
 * when data is available, post a pseudo-sigio integer event.
 */

static int
_sigio_thread_function(stream_id nst)
{
    fd_set dread;
    int res;
    socket_t sock = StreamUnit(nst);

    for(;;)
    {
	if (!(StreamMode(nst) & SSIGIO))
	    return 1;				/* stop thread, ok */

	FD_ZERO(&dread);
	FD_SET(sock, &dread);
	res = select(sock + 1, &dread, NULL, NULL, NULL);	/* block */

	if (res > 0)
	{
	    if (StreamMode(nst) & SSIGIO)	/* still enabled? */
		ec_post_event_int(ec_sigio);
	    return 1;				/* stop thread, ok */
	}
	else if (res < 0)
	{
	    Set_Socket_Errno();
	    switch (ec_os_errno_) {
	    case WSAEINTR:
	    case WSAEINPROGRESS:		/* ? */
	    case WSAENETDOWN:			/* ? */
		continue;			/* ignore and select again */

	    default:
		return 0;			/* stop thread, error */
	    }
	}
    }
}


/* Initial setup of the signaling mechanism for the stream */

int
ec_setup_stream_sigio_thread(stream_id nst)
{
    int res;

    /* setup a thread for this socket */
    if (!nst->signal_thread)
    {
	nst->signal_thread = ec_make_thread();
	if (!nst->signal_thread)
	    return SYS_ERROR;
    }
    else if (!ec_thread_stopped(nst->signal_thread, &res))
    {
	return RANGE_ERROR;
    }
    if (!ec_start_thread(nst->signal_thread, (int(*) ARGS((void*)))_sigio_thread_function, nst))
	return SYS_ERROR;
    return PSUCCEED;
}


int
ec_reenable_sigio(stream_id nst, int bytes_wanted, int bytes_read)
{
    int res;

    /* If we just read less than we asked for, we know the stream is empty.
     * Otherwise, do a select to find out if there is more data waiting.
     */
    if (bytes_read >= bytes_wanted)
    {
	struct timeval to;
	fd_set dread;
	to.tv_sec = 0;
	to.tv_usec = 0;
	FD_ZERO(&dread);
	FD_SET(StreamUnit(nst), &dread);
	res = select(StreamUnit(nst) + 1, &dread, NULL, NULL, &to);
	if (res > 0) {
	    return PSUCCEED;	/* there is more data */
	} else if (res < 0) {
	    Set_Socket_Errno();
	    return SYS_ERROR;
	}
    }

    /* nothing to read, reenable SIGIO thread */
    if (ec_thread_stopped(nst->signal_thread, &res))
    {
	if (!ec_start_thread(nst->signal_thread, (int(*) ARGS((void*)))_sigio_thread_function, nst))
	    return SYS_ERROR;
    }
    return PSUCCEED;
}

#else

int
ec_setup_stream_sigio_thread(stream_id nst)
{}

int
ec_reenable_sigio(stream_id nst, int bytes_wanted, int bytes_read)
{}

#endif

#else
static int p_socket(value vdom, type tdom, value vtp, type ttp, value vs, type ts)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
static int p_bind(value v, type t, value vaddr, type taddr)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
static int p_connect(value v, type t, value vaddr, type taddr)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
static int p_accept(value v, type t, value vaddr, type taddr, value vs, type ts)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
static int p_listen(value v, type t, value vn, type tn)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

int
ec_setup_stream_sigio_thread(stream_id nst)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

int
ec_reenable_sigio(stream_id nst, int bytes_wanted, int bytes_read)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

int
ec_close_socket(uword fd)               /* returns eclipse status */
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

int
ec_read_socket(uword fd, char *buf, int n)      /* returns count, sets ec_os_errno_ if -1 */
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

int
ec_write_socket(uword fd, char *buf, int n)     /* returns eclipse status */
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}

#endif /* SOCKETS */

#if defined(HAVE_SELECT)


/*
 * select/3 succeeds if
 *
 *	null	r(w)	never
 *	string	r(w)	something in buffer
 *	queue	r(w)	something in buffer
 *	pipe	r	something in buffer, or select(fd)
 *	pipe	w	select(fd)
 *	file	r	something in buffer, or select(fd)
 *	file	w	select(fd)
 *	socket	r	something in buffer, or select(fd)
 *	socket	w
 *	tty	rw	something in buffer, or select(fd)
 */


static int
p_select(value vin, type tin, value vtime, type ttime, value vout, type tout)
{
    fd_set		dread;
    fd_set		dwrite;
    pword		*list;
    pword		*pw;
    pword		*pl;
    pword		*p;
    int			res;
    int			buffer_input = 0;
    int			need_select = 0;
#ifdef _WIN32
    int			need_kbhit = 0;
    int			need_peek = 0;
#endif
    stream_id		nst;
    struct timeval	to;
    struct timeval	*pto = &to;
    uword		max = 0;
    double		dtime;

    if (IsNil(tin))
	list = 0;
    else 
    {
	Check_List(tin);
	list = vin.ptr;
    }
    Error_If_Ref(ttime);
    if (IsInteger(ttime))
    {
	if ((int) vtime.nint < 0 || (int) vtime.nint > 100000000)
	    { Bip_Error(RANGE_ERROR); }
	to.tv_sec = vtime.nint;
	to.tv_usec = 0;
    }
    else if (IsDouble(ttime))
    {
	dtime = Dbl(vtime);
	if (dtime < 0.0 || dtime > 1e8)
	    { Bip_Error(RANGE_ERROR); }
	to.tv_sec = (int) dtime;
	to.tv_usec = (int) ((dtime - (int) dtime) * 1000000.0);
    }
    else
    {
	if (!IsAtom(ttime))
	    { Bip_Error(TYPE_ERROR); }
	else if (vtime.did != d_block)
	    { Bip_Error(RANGE_ERROR); }
	pto = (struct timeval *) 0;
    }
    if (!IsNil(tout)) {
	Check_Output_List(tout)
    }
    if (!list)
    {
	Return_Unify_Nil(vout, tout);
    }

    FD_ZERO(&dread);
    FD_ZERO(&dwrite);
    pl = list;
    while (pl)
    {
	pw = pl++;
	Dereference_(pw);		/* get the list element	*/
	nst = get_stream_id(pw->val, pw->tag, 0, &res);
	if (nst == NO_STREAM)
	    { Bip_Error(res); }
	if (!IsOpened(nst))
	    { Bip_Error(STREAM_SPEC); }
	if (IsSocket(nst))	/* We don't wait for writes in sockets... */
	    nst = SocketInputStream(nst);

	if (StreamMode(nst) & SSELECTABLE)
	{
	    if (IsReadStream(nst) && StreamMethods(nst).buffer_nonempty(nst))
	    {
		buffer_input = 1;	/* we can read from buffer */
	    }
	    else if (StreamUnit(nst) != NO_UNIT)
	    {
		need_select = 1;
		if (IsReadStream(nst))
		{
		    FD_SET((socket_t) StreamUnit(nst), &dread);
		}
		else if (IsWriteStream(nst))
		{
		    FD_SET((socket_t) StreamUnit(nst), &dwrite);
		}
		if ((socket_t) StreamUnit(nst) > max)
		    max = StreamUnit(nst);
	    }
	    /* else: stream definitely not ready */
	}
#ifdef _WIN32
	else if (IsTty(nst) && IsReadStream(nst) && pto && pto->tv_sec==0 && pto->tv_usec==0)
	{
	    /* allow pseudo-select on Windows console with zero timeout */
	    need_kbhit = 1;
	}
	else if (IsPipeStream(nst) && IsReadStream(nst) && pto && pto->tv_sec==0 && pto->tv_usec==0)
	{
	    /* allow pseudo-select on Windows pipe with zero timeout */
	    need_peek = 1;
	}
#endif
	else
	{
	    Bip_Error(UNIMPLEMENTED);
	}

	Dereference_(pl);		/* get the list tail	*/
	if (IsRef(pl->tag))
	    { Bip_Error(INSTANTIATION_FAULT); }
	else if (IsList(pl->tag))
	    pl = pl->val.ptr;
	else if (IsNil(pl->tag))
	    pl = 0;
	else
	    { Bip_Error(TYPE_ERROR); }
    }

    if (need_select)
    {
	if (buffer_input)	/* we don't need to wait, there is something */
	{
	    to.tv_sec = 0;
	    to.tv_usec = 0;
	    pto = &to;
	}
	if (select(max + 1, &dread, &dwrite, (fd_set *) 0, pto) < 0)
	{
	    Set_Socket_Errno();
	    Bip_Error(SYS_ERROR);
	}
    }
#ifdef _WIN32
    if (need_kbhit && _kbhit())
    {
	FD_SET(StreamUnit(nst), &dread);
    }
    if (need_peek)
    {
	DWORD avail;
	if (!PeekNamedPipe((HANDLE)_get_osfhandle(StreamUnit(nst)),
				NULL, 0, NULL, &avail, NULL))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    Bip_Error(SYS_ERROR);
	}
	if (avail > 0)
	{
	    FD_SET(StreamUnit(nst), &dread);
	}
    }
#endif

    pl = list;
    list = p = Gbl_Tg;
    while (pl)
    {
	pw = pl++;
	Dereference_(pw);		/* get the list element	*/
	nst = get_stream_id(pw->val, pw->tag, 0, &res);
	if (IsSocket(nst))
	    nst = SocketInputStream(nst);

	if ((IsReadStream(nst) && StreamMethods(nst).buffer_nonempty(nst))
	     || ((StreamUnit(nst) != NO_UNIT) &&
		( FD_ISSET((socket_t) StreamUnit(nst), &dread)
		  || FD_ISSET((socket_t) StreamUnit(nst), &dwrite))))
	{
	    Gbl_Tg += 2;
	    Check_Gc;
	    *p++ = *pw;
	    p->val.ptr = p + 1;
	    p++->tag.kernel = TLIST;
	}

	Dereference_(pl);		/* get the list tail	*/
	if (IsList(pl->tag))
	    pl = pl->val.ptr;
	else
	    pl = 0;
    }
    if (list == p) {
	Return_Unify_Nil(vout, tout);
    }
    else
    {
	(p - 1)->tag.kernel = TNIL;
	Return_Unify_List(vout, tout, list);
    }
}
#else
static int p_select(value vin, type tin, value vtime, type ttime, value vout, type tout)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
#endif /* SELECT */


/* shell-like filename matching routine
 */
static int
_match(register char *pattern, register char *name)
{
    register int pc, nc;
    int flag, found;

    do
    {
	nc = *name++;
	switch (pc = *pattern++)
	{
	case '[':
	    if (!nc) return 0;
	    found = flag = 0;
	    if (*pattern == '^')
	    {
		pattern++;
		flag = 1;
	    }
	    for(;;)
	    {
		switch (pc = *pattern++)
		{
		case '-':	if (nc >= *(pattern-2)  &&  nc <= *pattern)
				    found = 1;
				continue;
		default:	if (pc == nc)
				    found = 1;
				continue;
		case 0:
		case ']':	break;
		}
		break;
	    }
	    if (found == flag) return 0;
	    break;

	case '*':
	    name -= 1;
	    do
		if (_match(pattern, name))
		    return 1;
	    while (*name++);
	    return 0;

	case '?':
	    if (!nc) return 0;
	    break;

	default:
	    if (pc != nc) return 0;
	    break;
	}
    }
    while (nc);
    return 1;
}


#if defined(HAVE_READLINE)
static int
p_readline(value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, SREAD, &res);

    if (nst == NO_STREAM)
	{ Bip_Error(res); }
    if (!IsTty(nst)) {
	Bip_Error(STREAM_MODE)
    }
    res = set_readline(nst);
    if (res != PSUCCEED) {
	Set_Errno;
	Bip_Error(SYS_ERROR);
    }
    Succeed_;
}
#endif


#ifdef _WIN32
/*
 * Surround a string with double quotes and double internal quotes.
 * This is the best method I have found for Windows to pass the string as
 * precisely as possible. Experiments with backslash-escaping were unsuccessful
 * since windows sometimes doubles them internally, probably assuming they are
 * path separators.
 * The only character that cannot be passed with this method is \n because
 * Windows insists in converting it to \r\n...
 *
 * The result string is allocated on the global stack.
 */
char *
_quoted_string(char *s, int len)
{
    pword *pw = TG;
    char *buf;
    int i;
    Push_Buffer(2*len+3);	/* worst case: N chars, N escapes, 2 quotes, 1 nul */
    buf = (char *) BufferStart(pw);
    *buf++ = '"';
    for(i=0; i<len; i++)
    {
	int c = s[i];
	if (c == '"')		/* escape quotes by doubling */
	    *buf++ = '"';
	*buf++ = c;
    }
    *buf++ = '"';
    *buf++ = 0;
    Trim_Buffer(pw, buf - ((char *) BufferStart(pw)));	/* adjust length */
    return (char *) BufferStart(pw);
}

char *
_new_os_filename(char *s)
{
    pword *pw = TG;
    Push_Buffer(MAX_PATH_LEN);
    s = os_filename(s, (char *) BufferStart(pw));
    Trim_Buffer(pw, strlen(s)+1);
    return (char *) BufferStart(pw);
}
#endif


/*
 * set up an argv[] array from a string or lists of strings/atoms
 */

static int
_build_argv(value vc,
	type tc,
	char **argv,	/* the constructed argument vector */
	char **cmd)	/* usually the same as argv[0], but not on Windows */
{
    if (IsList(tc))
    {
	pword *cdr = vc.ptr;
	int i = 0;

	while (i < MAX_ARGS)
	{
	    pword *car = cdr++;
	    Dereference_(car);
	    if (IsNumber(car->tag))
	    {
		pword auxpw;
		int len;
		len = tag_desc[TagType(car->tag)].string_size(car->val, car->tag, 0);
		Make_Stack_String(len, auxpw.val, argv[i]); /* maybe too long */
		len = tag_desc[TagType(car->tag)].to_string(car->val, car->tag, argv[i], 0);
		Trim_Buffer(auxpw.val.ptr, len+1);	/* adjust length */
	    }
	    else
	    {
#ifdef _WIN32
		char *s;
		int len;

		if (IsAtom(car->tag)) {
		    s = DidName(car->val.did);
		    len = DidLength(car->val.did);
		} else if (IsString(car->tag)) {
		    s = StringStart(car->val);
		    len = StringLength(car->val);
		} else if (IsNil(car->tag)) {
		    s = DidName(d_.nil);
		    len = DidLength(d_.nil);
		} else if (IsRef(car->tag)) {
		    Bip_Error(INSTANTIATION_FAULT);
		} else {
		    Bip_Error(TYPE_ERROR);
		}

		/* apply filename conversion to the command name only */
		if (i == 0)
		{
		    *cmd = s = _new_os_filename(s);
		    len = strlen(s);
		}

		/* quote the arguments argv[], but not cmd! */
		argv[i] = _quoted_string(s, len);

#else
		Get_Name(car->val, car->tag, argv[i]);
		if (i == 0)
		    *cmd = argv[0];
#endif
	    }
	    Dereference_(cdr);
	    ++i;
	    if (IsNil(cdr->tag)) {
	    	break;
	    } else if (!IsList(cdr->tag)) {
		Bip_Error(TYPE_ERROR);
	    }
	    if (i >= MAX_ARGS) {
		Set_Sys_Errno(E2BIG, ERRNO_UNIX);
		Bip_Error(SYS_ERROR);
	    }
	    cdr = cdr->val.ptr;
	}
	argv[i] = 0;
    }
    else	/* atoms and strings (backward compatibility) */
    {
	char *command;
	pword copy;
	Get_Name(vc, tc, command);
	Make_String(&copy, command);
	_get_args(StringStart(copy.val), argv);	/* parse the string */
	*cmd = argv[0];
    }
    Succeed_;
}


#undef Bip_Error
#define Bip_Error(N) Bip_Error_Fail(N)

static int
p_check_valid_stream(value v, type t)
{
    int		res;
    stream_id	nst = get_stream_id(v, t, 0, &res);

    if (nst == NO_STREAM)
	{ Bip_Error(res); }
    if (!IsOpened(nst))
	{ Bip_Error(STREAM_SPEC); }
    Succeed_;
}

static int
p_check_stream_spec(value v, type t)
{
    if (IsRef(t)) {
	Bip_Error(INSTANTIATION_FAULT);
    }
    switch(TagType(t))
    {
    case TNIL:
    case TDICT:
	    break;

    case TINT:
    case TBIG:
	/* backward compatibility: allow number iff it was obtained previously */
	break;

    case THANDLE:
	Check_Typed_Object_Handle(v, t, &stream_tid);
	break;

    default:
	Bip_Error(TYPE_ERROR);
    }
    Succeed_;
}


#ifdef _WIN32

/* The CreateProcess() doc says the command line can be 32k,
 * except for Win2000, where it's limited to MAX_PATH */
#define MAX_WIN_CMD_LINE	(32*1024)

static int
p_exec(value vc, type tc, value vstr, type tstr, value vp, type tp, value vpr, type tpr)
{
    char		*argv[MAX_ARGS+1];
    struct pipe_desc	pipes[MAX_PIPES + 1];
    struct pipe_desc	*p;
    int			pid;
    stream_id		id;
    int			i, err;
    char		*cmd;
    pword		*old_tg = TG;
    STARTUPINFO		si;
    PROCESS_INFORMATION	pi;
    DWORD		dwInfo, dwCreationFlags;


    Check_Ref(tp);
    Check_Integer(tpr);

    err = _build_argv(vc, tc, argv, &cmd);
    if (err < 0) {
	Bip_Error(err)
    }

    err = _check_streams(vstr, tstr, pipes);
    if (err < 0) {
	Bip_Error(err)
    }

    err = _open_pipes(pipes);
    if (err < 0) {
	Bip_Error(err)
    }

    /* Prepare arguments for CreateProcess() */
    dwCreationFlags = (vpr.nint==1 ? CREATE_NEW_PROCESS_GROUP : 0);

    ZeroMemory( &pi, sizeof(pi) );
    ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);

    /* By default, inherit the parent's standard I/O */
    si.dwFlags |= STARTF_USESTDHANDLES;
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    /* If there are pipes, make sure the correct end gets inherited
     * by the child, and the other does not.
     */
    for(i=0; !(pipes[i].flags & EXEC_PIPE_LAST); ++i)
    {
	HANDLE hParent, hChild;
	if (!pipes[i].flags)
	    continue;

	/* don't create a window if there is any I/O redirection */
	dwCreationFlags |= CREATE_NO_WINDOW;

	switch(i) {
	case 0:
	    hParent = (HANDLE) _get_osfhandle(pipes[i].fd[1]);
	    hChild = (HANDLE) _get_osfhandle(pipes[i].fd[0]);
	    si.hStdInput = hChild;
	    break;
	case 1:
	    hParent = (HANDLE) _get_osfhandle(pipes[i].fd[0]);
	    hChild = (HANDLE) _get_osfhandle(pipes[i].fd[1]);
	    si.hStdOutput = hChild;
	    break;
	case 2:
	    hParent = (HANDLE) _get_osfhandle(pipes[i].fd[0]);
	    hChild = (HANDLE) _get_osfhandle(pipes[i].fd[1]);
	    si.hStdError = hChild;
	    break;
	default:	/* TODO: can we inherit the other handles? */
	    Bip_Error(UNIMPLEMENTED);
	}
	if (hParent == INVALID_HANDLE_VALUE || hChild == INVALID_HANDLE_VALUE)
	{
	    Set_Errno;
	    Bip_Error(SYS_ERROR);
	}
	if (!SetHandleInformation(hChild, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT)
	 || !SetHandleInformation(hParent, HANDLE_FLAG_INHERIT, 0))
	{
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    Bip_Error(SYS_ERROR);
	}
    }

    /* Concat the arguments into a command line again. Thanks, Microsoft! */
    {
	char *s;
	int len = 0;
	pword *pw_s = TG;
	for (i=0; argv[i]; ++i)
	{
	    len += strlen(argv[i]) + 1;
	}
	if (len > MAX_WIN_CMD_LINE)
	{
	    Set_Sys_Errno(E2BIG, ERRNO_UNIX);
	    Bip_Error(SYS_ERROR);
	}
	Push_Buffer(len);
	cmd = s = (char *) BufferStart(pw_s);
	for (i=0; argv[i]; ++i)
	{
	    char *t = argv[i];
	    while((*s++ = *t++))
		;
	    *(s-1) = ' ';
	}
	*(s-1) = 0;
    }

    /* Start the child process */
    if (!CreateProcess(
	NULL,	    /* If we specify this, PATH won't be searched */
	(LPTSTR) cmd,   /* Command line as string */
	NULL,           /* Process handle not inheritable */
	NULL,           /* Thread handle not inheritable */
	TRUE,           /* inherit handles */
	dwCreationFlags,	/* process group, window, ... */
	NULL,           /* Use parent's environment block */
	NULL,           /* Use parent's starting directory  */
	&si,            /* Pointer to STARTUPINFO structure */
	&pi))           /* Pointer to PROCESS_INFORMATION structure */
    {
	Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	Bip_Error(SYS_ERROR);
	_close_pipes(pipes);
	Bip_Error(SYS_ERROR);
    } 

    /* Pop all the temporary strings */
    TG = old_tg;

    /* Close the (now inherited) child ends of the pipes in the parent */
    for(i=0; !(pipes[i].flags & EXEC_PIPE_LAST); ++i)
    {
	if (pipes[i].flags) {
	    switch(i) {
	    case 0:
		close(pipes[i].fd[0]);
		break;
	    case 1:
	    case 2:
		close(pipes[i].fd[1]);
		break;
	    default:	/* TODO: can we inherit the other handles? */
		Bip_Error(UNIMPLEMENTED);
	    }
	}
    }
    pid = pi.dwProcessId;
    CloseHandle(pi.hThread);
    
    /* Remember the process handle in a list which is used by p_wait().
     * Otherwise the process can disappear before they have been waited for */
    {
	t_child_desc *pd = (t_child_desc *) hp_alloc_size(sizeof(t_child_desc));
	pd->pid = pid;
	pd->hProcess = pi.hProcess;
	pd->next = child_processes;
	pd->prev_next = &child_processes;
	child_processes = pd;
    }

    /* Now create the Eclipse streams for the pipes */
    p = &pipes[0];
    while (!(p->flags & EXEC_PIPE_LAST))
    {
	if (p->flags & EXEC_PIPE_IN) {
	    id = find_free_stream();
	    init_stream(id, p->fd[1], SWRITE | SPIPE, d_pipe, NO_PROMPT,
		NO_STREAM, 0);
	} else if (p->flags & EXEC_PIPE_OUT) {
	    id = find_free_stream();
	    init_stream(id, p->fd[0], SREAD | SPIPE, d_pipe, NO_PROMPT,
		NO_STREAM, 0);
	    if (p->flags & EXEC_PIPE_SIG) {
		if ((err = ec_stream_set_sigio(id, SREAD)) < 0) {
		    Bip_Error(err);
		}
	    }
	}
	if (p->flags & (EXEC_PIPE_IN|EXEC_PIPE_OUT)) {
	    Bind_Stream(p->pw.val, p->pw.tag, id);
	}
	p++;
    }

    Return_Unify_Integer(vp, tp, pid);
}

#elif defined(BARRELFISH)
static int
p_exec(value vc, type tc, value vstr, type tstr, value vp, type tp, value vpr, type tpr)
{
    USER_PANIC("\nNOT available\n");
    Bip_Error(NOT_AVAILABLE);
}
#else

static int
p_exec(value vc, type tc, value vstr, type tstr, value vp, type tp, value vpr, type tpr)
{
    char		*argv[MAX_ARGS+1];
    struct pipe_desc	pipes[MAX_PIPES + 1];
    struct pipe_desc	*p;
    int			pid;
    stream_id		id;
    int			err;
    char		*cmd;

    Check_Ref(tp);
    Check_Integer(tpr);

    err = _build_argv(vc, tc, argv, &cmd);
    if (err < 0) {
	Bip_Error(err)
    }

    err = _check_streams(vstr, tstr, pipes);
    if (err < 0) {
	Bip_Error(err)
    }

    err = _open_pipes(pipes);
    if (err < 0) {
	Bip_Error(err)
    }

    switch (pid = vfork())
    {
    case -1:
	_close_pipes(pipes);
	Set_Errno;
	Bip_Error(SYS_ERROR);

    case 0:			/* child */
	_connect_pipes(pipes);
	if (vpr.nint == 1)	/* wants to set process group ID */
#ifdef HAVE_SETSID
	    (void) setsid();
#else
	    (void) setpgrp(0, getpid());
#endif
	errno = 0;
	(void)  execvp(cmd, argv);
	{
	    /* Explicitly send error to child's error stream. If
	     * we send to current_err_ on most architectures the
	     * error goes to the parent's error stream. On alpha Linux
	     * current_err_ is attached to the ether so the error isn't 
	     * seen at all. This has the benefit that the error can now be read
	     * correctly from the child's stream, but in tkeclipse it no
	     * longer appears as an error in the output window.
	     * This would appear to be determined by the architecture's
	     * vfork() implementation.
	     */
	    if (vpr.nint < 2 && strerror(errno)) {
		fprintf(stderr, "system interface error: %s in exec(%s, ..., ...)\n",
		    strerror(errno), cmd);
		fflush(stderr);
	    }
	    /* buggy behaviour in some cases mean errno may not be set with
               an error, reutrn a fake errno
	    */
	    if (errno == 0) errno = ENOEXEC;
	    _exit(errno + 128);  /* not exit() inside vfork, as per man page */
	}

    default:			/* parent */
	p = &pipes[0];
	while (!(p->flags & EXEC_PIPE_LAST))
	{
	    if (p->flags & EXEC_PIPE_IN) {
		(void) close(p->fd[0]);
		id = find_free_stream();
		init_stream(id, p->fd[1], SWRITE | SPIPE, d_pipe, NO_PROMPT,
		    NO_STREAM, 0);
	    } else if (p->flags & EXEC_PIPE_OUT) {
		(void) close(p->fd[1]);
		id = find_free_stream();
		init_stream(id, p->fd[0], SREAD | SPIPE, d_pipe, NO_PROMPT,
		    NO_STREAM, 0);
		if (p->flags & EXEC_PIPE_SIG) {
		    if ((err = ec_stream_set_sigio(id, SREAD)) < 0) {
			Bip_Error(err);
		    }
		}
	    }
	    if (p->flags & (EXEC_PIPE_IN|EXEC_PIPE_OUT)) {
		Bind_Stream(p->pw.val, p->pw.tag, id);
	    }
	    p++;
	}
	Return_Unify_Integer(vp, tp, pid);
    }
}
#endif

#undef Bip_Error
#define Bip_Error(N) return(N);

/*
 * Break up a string into an array of tokens which can be used for
 * an execv call.
 */
static void
_get_args(char *command, char **argv)
{
    int			i;
    register int	c;
    register int	sep;
    char		*cp;

    for (i = 0; i < MAX_ARGS; )
    {
	if (!command)
	    break;

	while ((c = *command))
	{
	    if (c != ' ' && c != '\t')
		break;
	    command++;
	}

	if (c == '\0')
	    break;

	switch (*command)
	{
	case '\'':
	    sep = '\'';
	    command++;
	    break;

	case '"':
	    sep = '"';
	    command++;
	    break;

#ifndef _WIN32
	case '\\':
	    command++;
	    /* fall into */
#endif
	default:
	    sep = 0;
	}
	argv[i++] = command;
	cp = command + 1;
	while ((c = *++command))
	    if (sep)
	    {
		if (c == sep)
		    break;
		*cp++ = c;
	    }
#ifndef _WIN32
	    /* take care of escaped chars */
	    else if (c == '\\')
	    {
		if ((c = *++command) == '\0')
		    break;
		else
		   *cp++ = c;
	    }
#endif
	    else if (c == ' ' || c == '\t')
		break;
	    else
		*cp++ = c;

	*cp++ = '\0';
	if (c == '\0')
	    break;
	else
	    command = cp;
    }
    argv[i] = 0;
}

static int
_check_streams(value vstr, type tstr, struct pipe_desc *pipes)
{
    int		i = 0;
    int		res;
    int		io;
    pword	*p;
    pword	*l;

    if (IsList(tstr))
    {
	l = vstr.ptr;
	for (;;)
	{
	    p = l;
	    Dereference_(p);
	    switch (i) {
	    case 0:
		io = EXEC_PIPE_IN;
		break;

	    case 1:
	    case 2:
		io = EXEC_PIPE_OUT;
		break;

	    default:
		io = EXEC_PIPE_IN | EXEC_PIPE_OUT;
	    }
	    res = _check_stream(p->val, p->tag, &pipes[i].pw, io);
	    if (res < 0)
		return res;
	    if (i <= 2 && res)			/* we know if input or output */
		res |= io;
	    else if (res && !(res & io))	/* must be specified */
		return STREAM_MODE;
	    pipes[i].flags = res;
	    l++;
	    i++;
	    Dereference_(l);
	    if (IsNil(l->tag))
		break;
	    if (!IsList(l->tag))
		return IsRef(l->tag) ? INSTANTIATION_FAULT : TYPE_ERROR;
	    l = l->val.ptr;
	    if (i >= MAX_PIPES)
		return RANGE_ERROR;
	}
    }
    else if (!IsNil(tstr))
    {
	return IsRef(tstr) ? INSTANTIATION_FAULT : TYPE_ERROR;
    }
    pipes[i].flags |= EXEC_PIPE_LAST;
    return 0;
}


/*
 * Check the stream argument for exec/3.
 * For error, return negative error code
 *
 * For null return 0
 * if atom or variable set EXEC_PIPE_CON
 * if sigio(Atom_Or_Var) also set EXEC_PIPE_SIG
 * Also set s to the proper stream argument.
 *
 * If io is nonzero, allow in(S), out(S), or either, depending on io.
 * if in(Atom_Or_Var) also set EXEC_PIPE_IN in return code
 * if out(Atom_Or_Var) also set EXEC_PIPE_OUT in return code
 */
static int
_check_stream(value v, type t, pword *s, int io)
{
    int		res = EXEC_PIPE_CON;
    int		where;

    if (IsAtom(t))
    {
	if (v.did == d_.null)
	    res = 0;
    }
#if defined(SIGIO_FASYNC) || defined(SIGIO_SETSIG) || defined(SIGIO_FIOASYNC)
    else if (IsStructure(t) && v.ptr->val.did == d_sigio)
    {
	(v.ptr)++;
	Dereference_(v.ptr);
	if ((res = _check_stream(v.ptr->val, v.ptr->tag, s, io)) < 0)
	    return res;
	return res | EXEC_PIPE_SIG;
    }
#endif
    else if (IsStructure(t) &&
	((v.ptr->val.did == d_in && (where = EXEC_PIPE_IN)) ||
	(v.ptr->val.did == d_out && (where = EXEC_PIPE_OUT))))
    {
	if (!(io & where))
	    return STREAM_MODE;
	(v.ptr)++;
	Dereference_(v.ptr);
	if ((res = _check_stream(v.ptr->val, v.ptr->tag, s, 0)) < 0)
	    return res;
	return res | where;
    }
    else if (IsNil(t))
	v.did = d_.nil;
    else if (!IsRef(t))
	return TYPE_ERROR;
    s->val = v;
    s->tag = t;
    return res;
}


static void
_close_pipes(struct pipe_desc *pipes)
{
    while (!(pipes->flags & EXEC_PIPE_LAST)) {
	if (pipes->flags) {
	    (void) close(pipes->fd[0]);
	    (void) close(pipes->fd[1]);
	}
	pipes++;
    }
}

#ifndef _WIN32

static void
_connect_pipes(struct pipe_desc *pipes)
{
    int		i = 0;

    while (!(pipes->flags & EXEC_PIPE_LAST)) {
	if (pipes->flags & EXEC_PIPE_IN) {
	    if (dup2(pipes->fd[0], i) == -1 ||
		close(pipes->fd[1]) == -1 ||
		close(pipes->fd[0]) == -1)
	    {
		ec_bad_exit(strerror(errno));
	    }
	    if ((pipes->flags & EXEC_PIPE_SIG) && set_sigio(i) < 0) {
		ec_bad_exit(strerror(errno));
	    }
	} else if (pipes->flags & EXEC_PIPE_OUT) {
	    if (dup2(pipes->fd[1], i) == -1) {
		ec_bad_exit(strerror(errno));
	    }
	    (void) close(pipes->fd[0]);
	    (void) close(pipes->fd[1]);
	}
	pipes++;
	i++;
    }
}

#endif

static int
_open_pipes(struct pipe_desc *allpipes)
{
    struct pipe_desc *pipes = allpipes;
    while (!(pipes->flags & EXEC_PIPE_LAST)) {
	if (pipes->flags) {
	    if (pipe(pipes->fd) == -1) {
		Set_Errno;
		pipes->flags |= EXEC_PIPE_LAST;
		_close_pipes(allpipes);
		return SYS_ERROR;
	    }
	}
	pipes++;
    }
    return 0;
}


static int
p_wait(value pv, type pt, value sv, type st, value vmode, type tmode)
{
    int		statusp;
    int		pid, res;
    Prepare_Requests;

    Check_Atom(tmode)
    Check_Output_Integer(st);
    if (IsInteger(pt))
    {
#ifdef _WIN32
	HANDLE phandle;
	DWORD dwstatus;
	t_child_desc *pd;

	Cut_External;

	/* First try to find the PID in our list of children */
	for(pd = child_processes; pd; pd = pd->next)
	{
	    if (pv.nint == pd->pid)
		break;
	}
	if (pd)	/* We know the process and still have a handle */
	{
	    phandle = pd->hProcess;
	}
	else	/* Unknown process, try to open a temporary handle */
	{
	    phandle = OpenProcess(SYNCHRONIZE|PROCESS_QUERY_INFORMATION, FALSE, pv.nint);
	    if (!phandle)
	    {
		if (GetLastError() == ERROR_INVALID_PARAMETER)
		{
		    Fail_;
		}
		Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
		Bip_Error(SYS_ERROR);
	    }
	}

        if (vmode.did == d_.hang) {
            res = WaitForSingleObject(phandle, INFINITE);
        } else if(vmode.did == d_.nohang) {
            res = WaitForSingleObject(phandle, 0);
        } else {
            Bip_Error(RANGE_ERROR);
        }
        if (res == WAIT_OBJECT_0)
	{
	    /* handle is signaled, i.e. process terminated */
	    if (!GetExitCodeProcess(phandle, &dwstatus))
		goto _wait_cleanup_error_;
            pid = pv.nint;
	    statusp = dwstatus;
	    Child_Unlink(pd);
	    CloseHandle(phandle);
        }
	else if (res == WAIT_TIMEOUT)
	{
	    /* make it fail, but keep the handle if was in the list */
	    if (!pd)
	    {
		CloseHandle(phandle);
	    }
	    Fail_;
	}
	else /* WAIT_FAILED */
	{
_wait_cleanup_error_:
	    Child_Unlink(pd);
	    CloseHandle(phandle);
	    Set_Sys_Errno(GetLastError(),ERRNO_WIN32);
	    Bip_Error(SYS_ERROR);
        }
#else
	Cut_External;
        if (vmode.did == d_.hang) {
	    pid = waitpid((pid_t) pv.nint, &statusp, 0);
        } else if(vmode.did == d_.nohang) {
	    pid = waitpid((pid_t) pv.nint, &statusp, WNOHANG);
            if (pid == 0) { /* Child not yet exited */
		Fail_;
            }
        } else {
            Bip_Error(RANGE_ERROR);
        }
#endif
    }
    else if (IsRef(pt))
    {
#ifdef _WIN32
	Bip_Error(UNIMPLEMENTED);
#else
	pid = waitpid((pid_t) (-1), &statusp, 0);
	if (pid >= 0) {
	    Request_Unify_Integer(pv, pt, pid);
	}
#endif
    }
    else
    {
	Bip_Error(TYPE_ERROR);
    }
    if (pid == -1) {
	Cut_External;
	if (errno == ECHILD) {
	    Fail_;
	}
	Set_Errno;
	Bip_Error(SYS_ERROR)
    }
    Request_Unify_Integer(sv, st, statusp);
    Return_Unify;
}
