/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <pwd.h>
#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include "posixcompat.h"

static struct passwd dummyuser = {
    .pw_name = "user",
    .pw_passwd = "abcd",
    .pw_uid = 1000,
    .pw_gid = 100,
    .pw_dir = "/",
};

uid_t geteuid(void)
{
    POSIXCOMPAT_DEBUG("geteuid(): returning %d\n", dummyuser.pw_uid);
    return dummyuser.pw_uid;
}

uid_t getuid(void)
{
    POSIXCOMPAT_DEBUG("getuid(): returning %d\n", dummyuser.pw_uid);
    return dummyuser.pw_uid;
}

struct passwd *getpwuid(uid_t uid)
{
    POSIXCOMPAT_DEBUG("getpwuid(%d): returning dummy user \"%s\"\n",
                      uid, dummyuser.pw_name);
    return &dummyuser;
}

struct passwd *getpwnam(const char *name)
{
    assert(!"NYI");
    return NULL;
}
