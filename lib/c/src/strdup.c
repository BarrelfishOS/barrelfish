#define _USE_XOPEN
#include <string.h>
#include <stdlib.h>

char *
strdup(const char *s)
{
	size_t len = strlen(s);
	char *d;
	d = malloc(len + 1);
	if (d == NULL)
		return NULL;
	strcpy(d, s);
	return d;
}
