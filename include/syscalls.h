#ifndef SYSCALLS_H
#define SYSCALLS_H

#include <lwip/sockets.h>

int ftruncate(int fd, int length);
int mkdir(const char *pathname, int mode);
long gethostid(void);
int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout);
int kill(pid_t pid, int sig);

#endif
