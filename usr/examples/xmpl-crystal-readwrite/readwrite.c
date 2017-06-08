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

int main(int argc, char *argv[]) 
{
	vfs_init();
	FILE * fp;
	char text[] = "Here is the example of read and write files";
	char buffer[100];
	char buffer2[100];
	printf("open the file\n");
	fp = fopen("/x86_64/sbin/examples/test.txt", "w+");
	if(fp!=NULL)
	{
		printf("write something into the file\n");
		fwrite(text, strlen(text)+1, 1, fp);
		printf("we will read something from this file\n");
		fseek(fp, 2, SEEK_SET);
		fread(buffer2, 50, 1, fp);
		printf("reading after seek, the result is %s\n", buffer2);
		fclose(fp);
	}
	else
	{
		printf("fp is NULL\n");
		return EXIT_SUCCESS;
	}

	FILE * fp2;
	printf("Now, we will reopen this file and read again\n");
	fp2 = fopen("/x86_64/sbin/examples/test.txt", "r");
	fread(buffer, 50, 1, fp2);
	printf("reading after open again, the result is %s\n", buffer);
	fclose(fp2);

    return EXIT_SUCCESS;
}

