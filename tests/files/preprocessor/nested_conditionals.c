// Test nested conditionals
#define LEVEL 2
#define DEBUG 1

int main() {
    int result = 0;

#if LEVEL > 0
    result = result + 10;

  #if LEVEL > 1
    result = result + 20;

    #ifdef DEBUG
      result = result + 5;
    #endif
  #endif
#endif

    return result;
}
