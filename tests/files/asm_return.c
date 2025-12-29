// Test inline assembly that sets return value
int main() {
    asm("movq $7, %rax");
    return 0;  // This should be overridden by the asm
}
