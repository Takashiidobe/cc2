#include "test.h"

int use(void *p) { return p != 0; }

int main() {
  int v = 1 + use(alloca(8));
  ASSERT(2, v);

  printf("OK\n");
  return 0;
}
