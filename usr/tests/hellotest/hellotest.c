#include <stdio.h>
#include <barrelfish/barrelfish.h>

int main(int argc, char *argv[])
{
  debug_printf("Hello world (debug_printf)\n");
  printf("Hello world (normal printf)\n");
  for (int i = 0;i < argc; i ++) {
    printf("arg[%d] = %s\n", i, argv[i]);
  }
  return EXIT_SUCCESS;
}
