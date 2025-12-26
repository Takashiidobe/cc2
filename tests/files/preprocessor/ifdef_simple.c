// Test #ifdef and #ifndef
#define FEATURE_A

int main() {
    int result = 0;

#ifdef FEATURE_A
    result = result + 10;
#endif

#ifdef FEATURE_B
    result = result + 20;
#endif

#ifndef FEATURE_B
    result = result + 5;
#endif

    return result;
}
