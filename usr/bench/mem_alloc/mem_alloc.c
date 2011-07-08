#include <stdio.h>
#include <rcce/RCCE.h>
#include <barrelfish/barrelfish.h>

#define ALLOC_BYTES     0x100000
#define MAX_ROUNDS      1000

enum state {
    State_Idle,
    State_Inflate,
    State_Deflate
};

int RCCE_APP(int argc, char **argv)
{
  int ME;
  size_t total = 0;
  enum state state = State_Idle;
  static void *mem[MAX_ROUNDS];
  int j;
  void *ret;

  RCCE_init(&argc, &argv);
  //  RCCE_debug_set(RCCE_DEBUG_ALL);

  ME = RCCE_ue();
  printf("Core %d passed RCCE_init\n", ME);

  for(int i = 0; i < MAX_ROUNDS; i++) {
      mem[i] = NULL;
  }

  for(int i = 0; i < RCCE_num_ues() * 9; i++) {
      switch(state) {
      case State_Idle:
          printf("Idling\n");
          if(i % RCCE_num_ues() == ME) {
              state = State_Inflate;
          }
          break;

      case State_Inflate:
          printf("Inflating\n");
          total = 0;
          j = 0;
          do {
              ret = malloc(ALLOC_BYTES);
              if(ret == NULL) {
                  printf("%d: Out of memory! total %u bytes after %d allocations\n",
                         ME, total, j);
                  /* printf("highest memory at %p\n", mem[j - 1]); */
              } else {
                  assert(j < MAX_ROUNDS);
                  mem[j] = ret;
                  total += ALLOC_BYTES;
                  j++;

                  /* printf("got memory at %p, run %d\n", ret, j); */

                  bool dirty = false;
                  for(int x = 0; x < ALLOC_BYTES; x++) {
                      char *c = ret;

                      if(c[x] != 0) {
                          dirty = true;
                      }
                  }

                  if(dirty) {
                      printf("Memory dirty\n");
                  /* } else { */
                  /*     printf("Memory clean\n"); */
                  }
              }
          } while(ret != NULL && j < 125);

          printf("%d: Total %u bytes after %d allocations\n", ME, total, j);

          state = State_Deflate;
          break;

      case State_Deflate:
          printf("Deflating\n");
          for(j = 0; mem[j] != NULL; j++) {
              /* printf("freeing %d\n", j); */
              free(mem[j]);
              mem[j] = NULL;
          }
          state = State_Idle;
          break;
      }

      RCCE_barrier(&RCCE_COMM_WORLD);
  }

  printf("%d: done\n", ME);
  return 0;
}
