// Define a global variable
int global_value;

// Declare it as extern (redundant but valid)
extern int global_value;

int main() {
    global_value = 55;
    return global_value;
}
