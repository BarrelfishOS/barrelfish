#ifndef __PAGER_H
#define __PAGER_H
#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>

errval_t pager_install_handler(char *ex_stack, size_t stack_size);

#endif // __PAGER_H
