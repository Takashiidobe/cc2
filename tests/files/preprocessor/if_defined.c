// Test defined() operator
#define FEATURE_X
#define FEATURE_Y

int main() {
    int result = 0;

#if defined(FEATURE_X)
    result = result + 10;
#endif

#if defined(FEATURE_Y) && defined(FEATURE_X)
    result = result + 20;
#endif

#if defined(FEATURE_Z)
    result = result + 100;
#endif

#if !defined(FEATURE_Z)
    result = result + 5;
#endif

    return result;
}
