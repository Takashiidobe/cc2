// Test simple inline assembly
int main() {
    int result = 42;
    asm("movq $10, %rax");
    return 0;
}
