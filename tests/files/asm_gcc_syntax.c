// Test GCC __asm__ syntax
int main() {
    __asm__("movq $5, %rax");
    return 0;
}
