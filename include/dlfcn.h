/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DLFCN_H_
#define DLFCN_H_

#define RTLD_NOW 1
#define RTLD_LAZY 2


struct function_entry{
    char name[40];
    void *f;
};



void dlopen_set_params(struct function_entry (*fk)[], int nrk);
void *dlopen(const char *filename, int flags);
void *dlsym(void *handle, const char *symbol);
char *dlerror(void);
int dlclose(void *handle);

#endif // DLFCN_H_
