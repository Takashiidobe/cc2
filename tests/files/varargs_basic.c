#include <stdarg.h>

int sum_twice(int n, ...) {
  va_list ap;
  va_start(ap, n);

  va_list ap2;
  va_copy(ap2, ap);

  int total = 0;
  for (int i = 0; i < n; i++) {
    total += va_arg(ap, int);
  }
  for (int i = 0; i < n; i++) {
    total += va_arg(ap2, int);
  }

  va_end(ap);
  va_end(ap2);
  return total;
}

int main() {
  return sum_twice(3, 1, 2, 3) == 12 ? 0 : 1;
}
