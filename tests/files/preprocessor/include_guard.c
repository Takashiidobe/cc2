// Test include guards pattern
#include "guarded.h"
#include "guarded.h"  // Should only include once

int main() {
    return get_value();
}
