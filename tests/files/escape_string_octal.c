// Test: Octal escape in string literals

int main() {
    char *str = "\110\145\154\154\157";  // "Hello" in octal

    // H=110(oct)=72, e=145(oct)=101, l=154(oct)=108, l=154, o=157(oct)=111
    return str[0];  // Should be 'H' = 72
}
