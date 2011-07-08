/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BF_UNISTD_H
#define __BF_UNISTD_H

#include <stddef.h>
#include <sys/types.h> // for pid_t

#define	R_OK 4
#define	W_OK 2
#define	X_OK 1
#define	F_OK 0

#define	STDIN_FILENO  0
#define	STDOUT_FILENO 1
#define	STDERR_FILENO 2

#define O_RDONLY   00000
#define O_WRONLY   00001
#define O_RDWR     00002
#define O_CREAT    00100
#define O_EXCL     00200
#define O_NOCTTY   00400
#define O_TRUNC    01000	
#define O_APPEND   02000
#define O_NONBLOCK 04000
#define O_SYNC    010000
#define O_FSYNC	  O_SYNC
#define O_ASYNC	  020000

struct stat;
extern char **environ;

void _exit(int status);
int open(const char*pathname, int flags, ...);
int creat (char * file, int mode);
int read(int fd, void *buf, int len);
int write(int fd, const void *buf, int len);
int close(int fd);
int lseek(int fd, int offset, int whence);
int stat(const char *pathname, struct stat *buf);
int fstat(int fd, struct stat*buf);
int ftruncate(int fd, int length);
int access(const char*pathname,int mode);
int chdir(const char*pathname);
int mkdir(const char*pathname,int mode);
int rmdir(const char*pathname);
int unlink(const char*pathname);
int dup(int oldfd);
int isatty(int fd);
int dup2(int oldfd, int newfd);
int pipe(int pipefd[2]);
char *getcwd(char *buf, size_t size);
pid_t getpid(void);
void *sbrk(intptr_t increment);
long gethostid(void);

#endif // __BF_UNISTD_H
