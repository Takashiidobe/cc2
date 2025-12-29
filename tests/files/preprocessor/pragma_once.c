// Test #pragma once for include guards

#include "pragma_once_header.h"
#include "pragma_once_header.h"  // Should be skipped due to #pragma once
#include "pragma_once_header.h"  // Should also be skipped

int main() {
    // If pragma once didn't work, we'd get a redefinition error
    // or the value would be wrong
    return PRAGMA_ONCE_VALUE;  // Should return 42
}
