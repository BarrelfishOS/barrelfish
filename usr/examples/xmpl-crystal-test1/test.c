/** \file
 *  \brief Hello World application
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>
#include <barrelfish/barrelfish.h>

//static char *cwd;
int main(int argc, char *argv[]) 
{
	printf("Now, I am going to create a directory.....\n");
	//char *path = vfs_path_mkabsolute(cwd, argv[1]);
	errval_t err = vfs_mkdir(argv[1]);
	printf("Finished! The path is %s.....\n", argv[1]);
	//free(path);
	//free(cwd);
    if (err_is_fail(err)) {
        printf("%s\n", err_getstring(err));
        return EXIT_FAILURE;
    } else {
    	printf("Successed!\n");
        return EXIT_SUCCESS;
    }
}
