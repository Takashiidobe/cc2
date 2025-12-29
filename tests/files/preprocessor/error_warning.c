// Test #error and #warning directives

#define FEATURE_ENABLED

// #warning should trigger
#ifdef FEATURE_ENABLED
#warning Feature is enabled
#endif

// #error should NOT trigger (in false branch)
#ifndef FEATURE_ENABLED
#error Feature is required
#endif

int main() {
    return 0;
}
