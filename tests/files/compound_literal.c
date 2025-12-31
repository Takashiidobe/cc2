#include "chibicc/test.h"

int main() {
  ASSERT(1, (int){1});
  ASSERT(2, ((int[]){0, 1, 2})[2]);
  ASSERT('a', ((struct { char a; int b; }){'a', 3}).a);
  ASSERT(3, ({ int x = 3; (int){x}; }));
  int *p = &(int){3};
  *p = 5;
  ASSERT(5, *p);

  printf("OK\n");
  return 0;
}
