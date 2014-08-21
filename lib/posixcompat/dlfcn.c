/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include "posixcompat.h"

static struct function_entry *ftable = 0;
static int nr_entries = 0;

void dlopen_set_params(struct function_entry *fk, int nrk)
{
    ftable = fk;
    nr_entries = nrk;
}

void *dlopen(const char *filename, int flags)
{
    POSIXCOMPAT_DEBUG("dlopen(%s)\n", filename);
    return (dlopen);
}


void *dlsym(void *handle, const char *symbol)
{
    int i;
    if (ftable == 0) {
        return (0);
    }
    for (i = 0; i < nr_entries; i++) {
        if (strcmp(symbol, ftable[i].name) == 0) {
            return (ftable[i].f);
        }
    }
    return (0);
}


char *dlerror(void)
{
    return("ok");
}

int dlclose(void *handle)
{
    return (0);
}

int dladdr (const void *address, Dl_info *info)
{
    printf("WARNING: dladdr is not implemented.\n");
    abort();
#if 0
  const ElfW(Addr) addr = DL_LOOKUP_ADDRESS (address);
  int result = 0;

  /* Protect against concurrent loads and unloads.  */
  __rtld_lock_lock_recursive (GL(dl_load_lock));

  struct link_map *l = _dl_find_dso_for_object (addr);

  if (l)
    {
      determine_info (addr, l, info, mapp, symbolp);
      result = 1;
    }

  __rtld_lock_unlock_recursive (GL(dl_load_lock));

  return result;
#endif
  return 0;
}
