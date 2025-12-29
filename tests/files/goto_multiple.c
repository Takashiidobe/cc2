// Test multiple labels and gotos
int main() {
    int x = 0;
    goto second;
first:
    x = x + 10;
    goto end;
second:
    x = x + 5;
    goto first;
end:
    return x;
}
